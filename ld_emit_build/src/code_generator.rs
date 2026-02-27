use crate::code_utils::{to_snake_case, to_pascal_case, is_reserved, dedup_method_name};
use crate::error::LDBuildError;
use crate::models::*;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use std::collections::HashMap;

pub struct CodeGenerator;

/// Info about a context derived from its name
struct ContextInfo {
    /// PascalCase type name (e.g., `ActivityStreams`)
    type_name: Ident,
    /// snake_case module name (e.g., `activity_streams`)
    module_name: Ident,
    /// Marker trait name (e.g., `HasActivityStreams`)
    marker_trait: Ident,
    /// Extension trait name (e.g., `ActivityStreamsExt`)
    ext_trait: Ident,
}

impl ContextInfo {
    fn from_name(name: &str) -> Self {
        let pascal = to_pascal_case(name);
        let snake = to_snake_case(name);
        let type_name = format_ident!("{}", pascal);
        let module_name = format_ident!("{}", snake);
        let marker_trait = format_ident!("Has{}", pascal);
        let ext_trait = format_ident!("{}Ext", pascal);
        ContextInfo {
            type_name,
            module_name,
            marker_trait,
            ext_trait,
        }
    }
}

impl CodeGenerator {
    pub fn generate(
        contexts: &[(String, ParsedContext)],
        expose_values: &[ExposeValueDirective],
        renames: &[RenameDirective],
    ) -> Result<TokenStream, LDBuildError> {
        let infos: Vec<ContextInfo> = contexts
            .iter()
            .map(|(name, _)| ContextInfo::from_name(name))
            .collect();

        // Validate @expose_value directives match at least one SimpleTerm
        Self::validate_expose_values(contexts, expose_values)?;

        // Token-based structural generation
        let context_types = Self::generate_context_types(&infos);
        let marker_traits = Self::generate_marker_traits(&infos);
        let cross_forwarding = Self::generate_cross_forwarding(&infos);
        let type_alias = Self::generate_type_alias(&infos);
        let unit_type_constants = Self::generate_unit_type_constants(contexts, &infos);
        let iri_constants = Self::generate_iri_constants(contexts, &infos);
        let extension_traits = Self::generate_extension_traits(contexts, &infos, expose_values, renames)?;

        // Token-based generation for context serializer impls
        let context_serializer_impls = Self::generate_context_serializer_impls(contexts, &infos);

        // Wrap all generated items in a module with warning suppression,
        // then re-export everything. This works with include!() contexts.
        let tokens = quote! {
            #[allow(dead_code)]
            #[allow(unused_imports)]
            #[allow(clippy::all)]
            mod __ld_emit_inner {
                #context_types
                #marker_traits
                #cross_forwarding
                #unit_type_constants
                #iri_constants
                #context_serializer_impls
                #extension_traits
                #type_alias
            }
            pub use __ld_emit_inner::*;
        };

        Ok(tokens)
    }

    // === 4.2: Context types ===

    fn generate_context_types(infos: &[ContextInfo]) -> TokenStream {
        let structs = infos.iter().map(|info| {
            let name = &info.type_name;
            quote! {
                pub struct #name<S = ()>(::core::marker::PhantomData<S>);
            }
        });
        quote! { #(#structs)* }
    }

    fn generate_marker_traits(infos: &[ContextInfo]) -> TokenStream {
        let traits = infos.iter().map(|info| {
            let marker = &info.marker_trait;
            let type_name = &info.type_name;
            quote! {
                pub trait #marker {}
                impl<S> #marker for #type_name<S> {}
            }
        });
        quote! { #(#traits)* }
    }

    fn generate_cross_forwarding(infos: &[ContextInfo]) -> TokenStream {
        if infos.len() < 2 {
            return TokenStream::new();
        }
        let impls = infos.iter().flat_map(|outer| {
            infos.iter().filter_map(move |inner| {
                if outer.type_name == inner.type_name {
                    return None;
                }
                let marker = &inner.marker_trait;
                let outer_type = &outer.type_name;
                Some(quote! {
                    impl<S: #marker> #marker for #outer_type<S> {}
                })
            })
        });
        quote! { #(#impls)* }
    }

    fn generate_type_alias(infos: &[ContextInfo]) -> TokenStream {
        if infos.is_empty() {
            return TokenStream::new();
        }
        // Build nested type: A<B<C>> from [A, B, C]
        let names: Vec<&Ident> = infos.iter().map(|i| &i.type_name).collect();
        let doc = {
            let name_strs: Vec<String> = names.iter().map(|n| n.to_string()).collect();
            format!("Composed context type: {}", name_strs.join(" + "))
        };
        // Build the nested type tokens recursively from the end
        let mut alias: TokenStream = quote! { #(#names)<*> };
        // quote's repetition with <*> doesn't nest, so build manually
        alias = Self::build_nested_type(&names);
        quote! {
            #[doc = #doc]
            pub type Context = #alias;
        }
    }

    /// Build nested generic type `A<B<C>>` from `[A, B, C]`.
    fn build_nested_type(names: &[&Ident]) -> TokenStream {
        match names.len() {
            0 => TokenStream::new(),
            1 => {
                let name = names[0];
                quote! { #name }
            }
            _ => {
                let first = names[0];
                let rest = Self::build_nested_type(&names[1..]);
                quote! { #first<#rest> }
            }
        }
    }

    // === 3.1: Unit type constants (token-based) ===

    fn generate_unit_type_constants(
        contexts: &[(String, ParsedContext)],
        infos: &[ContextInfo],
    ) -> TokenStream {
        let modules = contexts.iter().enumerate().map(|(i, (_, parsed))| {
            let info = &infos[i];
            let module_name = &info.module_name;

            let consts: Vec<TokenStream> = parsed.terms.iter().filter_map(|term| {
                if let TermKind::SimpleTerm { iri } = &term.kind {
                    let const_name = format_ident!("{}", to_snake_case(&term.name).to_uppercase());
                    let term_name = term.name.as_str();
                    let doc = format!("JSON-LD type constant: `{}`", iri);
                    Some(quote! {
                        #[doc = #doc]
                        pub const #const_name: ld_emit::TypeConstant = ld_emit::TypeConstant {
                            iri: #iri,
                            term_name: #term_name,
                        };
                    })
                } else {
                    None
                }
            }).collect();

            quote! {
                pub mod #module_name {
                    #(#consts)*
                }
            }
        });

        quote! { #(#modules)* }
    }

    // === 3.2: IRI constants (token-based with dedup) ===

    fn generate_iri_constants(
        contexts: &[(String, ParsedContext)],
        _infos: &[ContextInfo],
    ) -> TokenStream {
        // Step 1: Resolve intra-context collisions.
        // Within a single context, if two terms produce the same SCREAMING_SNAKE_CASE name
        // (e.g. "Image" (SimpleTerm) and "image" (ExtendedTerm) both → "IMAGE"),
        // keep the SimpleTerm and skip the ExtendedTerm.
        let mut skip_set: std::collections::HashSet<(usize, usize)> = std::collections::HashSet::new();

        for (ctx_idx, (_, parsed)) in contexts.iter().enumerate() {
            let mut intra_names: HashMap<String, Vec<usize>> = HashMap::new();
            for (term_idx, term) in parsed.terms.iter().enumerate() {
                let has_iri = matches!(
                    &term.kind,
                    TermKind::SimpleTerm { .. } | TermKind::ExtendedTerm { .. }
                );
                if has_iri {
                    let base_name = to_snake_case(&term.name).to_uppercase();
                    intra_names.entry(base_name).or_default().push(term_idx);
                }
            }

            for indices in intra_names.values() {
                if indices.len() > 1 {
                    let simple_idx = indices.iter().find(|&&i| {
                        matches!(&parsed.terms[i].kind, TermKind::SimpleTerm { .. })
                    });
                    if let Some(&winner) = simple_idx {
                        for &idx in indices {
                            if idx != winner {
                                skip_set.insert((ctx_idx, idx));
                            }
                        }
                    } else {
                        for &idx in &indices[1..] {
                            skip_set.insert((ctx_idx, idx));
                        }
                    }
                }
            }
        }

        // Step 2: Count occurrences of each base constant name across all contexts
        // (only counting non-skipped terms)
        let mut name_counts: HashMap<String, usize> = HashMap::new();
        for (ctx_idx, (_, parsed)) in contexts.iter().enumerate() {
            for (term_idx, term) in parsed.terms.iter().enumerate() {
                if skip_set.contains(&(ctx_idx, term_idx)) {
                    continue;
                }
                let has_iri = matches!(
                    &term.kind,
                    TermKind::SimpleTerm { .. } | TermKind::ExtendedTerm { .. }
                );
                if has_iri {
                    let base_name = to_snake_case(&term.name).to_uppercase();
                    *name_counts.entry(base_name).or_insert(0) += 1;
                }
            }
        }

        // Step 3: Generate constants with dedup prefixes where needed
        let mut constants: Vec<TokenStream> = Vec::new();
        for (ctx_idx, (ctx_name, parsed)) in contexts.iter().enumerate() {
            for (term_idx, term) in parsed.terms.iter().enumerate() {
                if skip_set.contains(&(ctx_idx, term_idx)) {
                    continue;
                }
                let iri = match &term.kind {
                    TermKind::SimpleTerm { iri } => Some(iri.as_str()),
                    TermKind::ExtendedTerm { id, .. } => Some(id.as_str()),
                    _ => None,
                };
                if let Some(iri) = iri {
                    let base_name = to_snake_case(&term.name).to_uppercase();
                    let const_name = if name_counts.get(&base_name).copied().unwrap_or(0) > 1 {
                        let prefix = to_snake_case(ctx_name).to_uppercase();
                        format!("{}_{}", prefix, base_name)
                    } else {
                        base_name
                    };
                    let const_ident = format_ident!("{}", const_name);
                    constants.push(quote! {
                        pub const #const_ident: &str = #iri;
                    });
                }
            }
        }

        quote! {
            pub mod iri {
                #(#constants)*
            }
        }
    }

    // === 4.1/4.2: Extension traits and property setters (token-based) ===

    fn generate_extension_traits(
        contexts: &[(String, ParsedContext)],
        infos: &[ContextInfo],
        expose_values: &[ExposeValueDirective],
        renames: &[RenameDirective],
    ) -> Result<TokenStream, LDBuildError> {
        // Build rename lookup: original term name -> target method name
        let rename_map: HashMap<&str, &str> = renames
            .iter()
            .map(|r| (r.from.as_str(), r.to.as_str()))
            .collect();

        // Phase 1: Resolve base method names for all property terms
        // and count for dedup detection.
        // Each entry per context: Vec<(term_index, resolved_base_name)>
        let mut per_context_resolved: Vec<Vec<(usize, String)>> = Vec::new();
        let mut method_name_counts: HashMap<String, usize> = HashMap::new();

        for (_, parsed) in contexts {
            let mut ctx_resolved = Vec::new();
            for (term_idx, term) in parsed.terms.iter().enumerate() {
                if !Self::is_property_term(term, expose_values) {
                    continue;
                }
                let base_name = to_snake_case(&term.name);
                let resolved = if is_reserved(&base_name) {
                    match rename_map.get(term.name.as_str()) {
                        Some(&target) => {
                            if is_reserved(target) {
                                return Err(LDBuildError::CodeGen {
                                    term: term.name.clone(),
                                    message: format!(
                                        "Rename target '{}' for term '{}' is also a Rust reserved word.",
                                        target, term.name
                                    ),
                                });
                            }
                            target.to_string()
                        }
                        None => {
                            // KeywordAlias terms automatically use raw identifiers (e.g., r#type)
                            if matches!(term.kind, TermKind::KeywordAlias { .. }) {
                                base_name
                            } else {
                                return Err(LDBuildError::CodeGen {
                                    term: term.name.clone(),
                                    message: format!(
                                        "Term '{}' maps to Rust reserved word '{}'. Use @rename {{ \"{}\" -> \"<alternative>\" }} to specify an alternative name.",
                                        term.name, base_name, term.name
                                    ),
                                });
                            }
                        }
                    }
                } else {
                    base_name
                };
                *method_name_counts.entry(resolved.clone()).or_insert(0) += 1;
                ctx_resolved.push((term_idx, resolved));
            }
            per_context_resolved.push(ctx_resolved);
        }

        // Phase 2: Generate token stream for each context
        let mut result = TokenStream::new();

        for (ctx_idx, (ctx_name, parsed)) in contexts.iter().enumerate() {
            let info = &infos[ctx_idx];
            let ext_trait = &info.ext_trait;
            let marker = &info.marker_trait;
            let trait_doc = format!("Extension trait for {} context properties.", ctx_name);

            let mut trait_methods = Vec::new();
            let mut impl_methods = Vec::new();

            for &(term_idx, ref resolved_name) in &per_context_resolved[ctx_idx] {
                let term = &parsed.terms[term_idx];

                // Apply dedup prefix if needed
                let final_name = if method_name_counts.get(resolved_name).copied().unwrap_or(0) > 1 {
                    dedup_method_name(ctx_name, resolved_name)
                } else {
                    resolved_name.clone()
                };

                // Use raw identifier for reserved words (e.g., r#type for KeywordAlias "type")
                let method_ident = if is_reserved(&final_name) {
                    Ident::new_raw(&final_name, Span::call_site())
                } else {
                    format_ident!("{}", final_name)
                };

                match &term.kind {
                    TermKind::KeywordAlias { keyword } if keyword == "@type" => {
                        let json_key = term.name.as_str();
                        let doc = format!("Set the `{}` type property (`{}`).", json_key, keyword);

                        trait_methods.push(quote! {
                            #[doc = #doc]
                            fn #method_ident(&mut self, types: &[&ld_emit::TypeConstant]) -> &mut Self;
                        });
                        impl_methods.push(quote! {
                            fn #method_ident(&mut self, types: &[&ld_emit::TypeConstant]) -> &mut Self {
                                self.type_def_aliased(#json_key, types)
                            }
                        });
                    }
                    TermKind::KeywordAlias { keyword } => {
                        let json_key = term.name.as_str();
                        let doc = format!("Set the `{}` property (`{}`).", json_key, keyword);

                        trait_methods.push(quote! {
                            #[doc = #doc]
                            fn #method_ident(&mut self, value: impl ld_emit::serde::Serialize) -> &mut Self;
                        });
                        impl_methods.push(quote! {
                            fn #method_ident(&mut self, value: impl ld_emit::serde::Serialize) -> &mut Self {
                                self.field(#json_key, value)
                            }
                        });
                    }
                    TermKind::ExtendedTerm { id, type_coercion, .. } => {
                        let json_key = term.name.as_str();
                        let doc = format!("Set the `{}` property (`{}`).", json_key, id);

                        trait_methods.push(quote! {
                            #[doc = #doc]
                            fn #method_ident(&mut self, value: impl ld_emit::serde::Serialize) -> &mut Self;
                        });
                        impl_methods.push(quote! {
                            fn #method_ident(&mut self, value: impl ld_emit::serde::Serialize) -> &mut Self {
                                self.field(#json_key, value)
                            }
                        });

                        if matches!(type_coercion, Some(TypeCoercion::Id)) {
                            let nested_ident = format_ident!("{}_object", final_name);
                            let nested_doc = format!("Set the `{}` property as a nested object (`{}`).", json_key, id);

                            trait_methods.push(quote! {
                                #[doc = #nested_doc]
                                fn #nested_ident<F: FnOnce(&mut Self)>(&mut self, f: F) -> &mut Self;
                            });
                            impl_methods.push(quote! {
                                fn #nested_ident<F: FnOnce(&mut Self)>(&mut self, f: F) -> &mut Self {
                                    self.nested_object(#json_key, f)
                                }
                            });
                        }
                    }
                    TermKind::SimpleTerm { iri } => {
                        if expose_values.iter().any(|ev| ev.expanded_iri == *iri) {
                            let with_ident = format_ident!("{}_with", final_name);
                            let json_key = term.name.as_str();
                            let doc = format!("Set the `{}` property with a direct value (`{}`).", json_key, iri);

                            trait_methods.push(quote! {
                                #[doc = #doc]
                                fn #with_ident(&mut self, value: impl ld_emit::serde::Serialize) -> &mut Self;
                            });
                            impl_methods.push(quote! {
                                fn #with_ident(&mut self, value: impl ld_emit::serde::Serialize) -> &mut Self {
                                    self.field(#json_key, value)
                                }
                            });
                        }
                    }
                    _ => {}
                }
            }

            result.extend(quote! {
                #[doc = #trait_doc]
                pub trait #ext_trait {
                    #(#trait_methods)*
                }
                impl<Ctx: #marker + ld_emit::ContextSerializer> #ext_trait for ld_emit::ObjectSerializer<Ctx> {
                    #(#impl_methods)*
                }
            });
        }

        Ok(result)
    }

    fn is_property_term(term: &TermDefinition, expose_values: &[ExposeValueDirective]) -> bool {
        match &term.kind {
            TermKind::ExtendedTerm { .. } => true,
            TermKind::KeywordAlias { .. } => true,
            TermKind::SimpleTerm { iri } => {
                expose_values.iter().any(|ev| ev.expanded_iri == *iri)
            }
            _ => false,
        }
    }

    // === 4.5: ContextSerializer impls ===

    fn generate_context_serializer_impls(
        contexts: &[(String, ParsedContext)],
        infos: &[ContextInfo],
    ) -> TokenStream {
        let impls = contexts.iter().enumerate().map(|(i, (_, parsed))| {
            let type_name = &infos[i].type_name;
            let ctx_value_expr = Self::json_value_to_tokens(&parsed.original_json);
            quote! {
                impl<S: ld_emit::ContextSerializer> ld_emit::ContextSerializer for #type_name<S> {
                    fn context_json() -> ld_emit::serde_json::Value {
                        let mut arr = match S::context_json() {
                            ld_emit::serde_json::Value::Array(a) => a,
                            other => vec![other],
                        };
                        let ctx_value: ld_emit::serde_json::Value = #ctx_value_expr;
                        arr.push(ctx_value);
                        ld_emit::serde_json::Value::Array(arr)
                    }
                }
            }
        });
        quote! { #(#impls)* }
    }

    /// Recursively convert a `serde_json::Value` into a `TokenStream` that constructs
    /// the equivalent `ld_emit::serde_json::Value` at compile time, without runtime parsing.
    fn json_value_to_tokens(value: &serde_json::Value) -> TokenStream {
        match value {
            serde_json::Value::Null => {
                quote! { ld_emit::serde_json::Value::Null }
            }
            serde_json::Value::Bool(b) => {
                quote! { ld_emit::serde_json::Value::Bool(#b) }
            }
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    quote! {
                        ld_emit::serde_json::Value::Number(
                            ld_emit::serde_json::Number::from(#i)
                        )
                    }
                } else if let Some(u) = n.as_u64() {
                    quote! {
                        ld_emit::serde_json::Value::Number(
                            ld_emit::serde_json::Number::from(#u)
                        )
                    }
                } else if let Some(f) = n.as_f64() {
                    quote! {
                        ld_emit::serde_json::Value::Number(
                            ld_emit::serde_json::Number::from_f64(#f).unwrap()
                        )
                    }
                } else {
                    quote! { ld_emit::serde_json::Value::Null }
                }
            }
            serde_json::Value::String(s) => {
                quote! { ld_emit::serde_json::Value::String(#s.to_string()) }
            }
            serde_json::Value::Array(arr) => {
                let elements: Vec<TokenStream> = arr.iter()
                    .map(Self::json_value_to_tokens)
                    .collect();
                quote! { ld_emit::serde_json::Value::Array(vec![#(#elements),*]) }
            }
            serde_json::Value::Object(map) => {
                let inserts: Vec<TokenStream> = map.iter().map(|(k, v)| {
                    let val_tokens = Self::json_value_to_tokens(v);
                    quote! {
                        __map.insert(#k.to_string(), #val_tokens);
                    }
                }).collect();
                quote! {
                    {
                        let mut __map = ld_emit::serde_json::Map::new();
                        #(#inserts)*
                        ld_emit::serde_json::Value::Object(__map)
                    }
                }
            }
        }
    }

    // === Validation ===

    fn validate_expose_values(
        contexts: &[(String, ParsedContext)],
        expose_values: &[ExposeValueDirective],
    ) -> Result<(), LDBuildError> {
        for ev in expose_values {
            let found = contexts.iter().any(|(_, parsed)| {
                parsed.terms.iter().any(|term| {
                    matches!(&term.kind, TermKind::SimpleTerm { iri } if *iri == ev.expanded_iri)
                })
            });
            if !found {
                return Err(LDBuildError::CodeGen {
                    term: ev.expanded_iri.clone(),
                    message: format!(
                        "@expose_value IRI '{}' does not match any SimpleTerm in the provided contexts",
                        ev.expanded_iri
                    ),
                });
            }
        }
        Ok(())
    }
}

// noinspection DuplicatedCode
#[cfg(test)]
mod tests {
    use super::*;

    /// Convert generated TokenStream to formatted Rust code via prettyplease.
    fn format_tokens(tokens: TokenStream) -> String {
        let file: syn::File = syn::parse2(tokens)
            .expect("Generated code should be valid Rust");
        prettyplease::unparse(&file)
    }

    fn make_simple_context() -> Vec<(String, ParsedContext)> {
        vec![(
            "activity_streams".to_string(),
            ParsedContext {
                source: ContextSource::Url("https://www.w3.org/ns/activitystreams".to_string()),
                terms: vec![
                    TermDefinition {
                        name: "Note".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://www.w3.org/ns/activitystreams#Note".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "Create".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://www.w3.org/ns/activitystreams#Create".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "actor".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://www.w3.org/ns/activitystreams#actor".to_string(),
                            type_coercion: Some(TypeCoercion::Id),
                            container: None,
                        },
                    },
                    TermDefinition {
                        name: "id".to_string(),
                        kind: TermKind::KeywordAlias {
                            keyword: "@id".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "content".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://www.w3.org/ns/activitystreams#content".to_string(),
                        },
                    },
                ],
                original_json: serde_json::json!("https://www.w3.org/ns/activitystreams"),
            },
        )]
    }

    fn make_multi_context() -> Vec<(String, ParsedContext)> {
        vec![
            (
                "activity_streams".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://www.w3.org/ns/activitystreams".to_string()),
                    terms: vec![
                        TermDefinition {
                            name: "Note".to_string(),
                            kind: TermKind::SimpleTerm {
                                iri: "https://www.w3.org/ns/activitystreams#Note".to_string(),
                            },
                        },
                        TermDefinition {
                            name: "actor".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://www.w3.org/ns/activitystreams#actor".to_string(),
                                type_coercion: Some(TypeCoercion::Id),
                                container: None,
                            },
                        },
                    ],
                    original_json: serde_json::json!("https://www.w3.org/ns/activitystreams"),
                },
            ),
            (
                "security_v1".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://w3id.org/security/v1".to_string()),
                    terms: vec![
                        TermDefinition {
                            name: "CryptographicKey".to_string(),
                            kind: TermKind::SimpleTerm {
                                iri: "https://w3id.org/security#Key".to_string(),
                            },
                        },
                        TermDefinition {
                            name: "publicKey".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://w3id.org/security#publicKey".to_string(),
                                type_coercion: Some(TypeCoercion::Id),
                                container: None,
                            },
                        },
                    ],
                    original_json: serde_json::json!("https://w3id.org/security/v1"),
                },
            ),
        ]
    }

    // === 4.2: Context types ===

    #[test]
    fn generates_context_struct() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub struct ActivityStreams<S = ()>(::core::marker::PhantomData<S>);"));
    }

    #[test]
    fn generates_marker_trait() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub trait HasActivityStreams {}"));
        assert!(code.contains("impl<S> HasActivityStreams for ActivityStreams<S> {}"));
    }

    #[test]
    fn generates_cross_forwarding_for_multi_context() {
        let contexts = make_multi_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // ActivityStreams forwards HasSecurityV1
        assert!(code.contains("impl<S: HasSecurityV1> HasSecurityV1 for ActivityStreams<S> {}"));
        // SecurityV1 forwards HasActivityStreams
        assert!(code.contains("impl<S: HasActivityStreams> HasActivityStreams for SecurityV1<S> {}"));
    }

    #[test]
    fn generates_composed_type_alias() {
        let contexts = make_multi_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub type Context = ActivityStreams<SecurityV1>;"));
    }

    #[test]
    fn generates_single_context_type_alias() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub type Context = ActivityStreams;"));
    }

    // === 4.3: Unit type constants ===

    #[test]
    fn generates_unit_type_for_simple_term() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub mod activity_streams"));
        assert!(code.contains("pub const NOTE: ld_emit::TypeConstant"), "Code:\n{}", code);
        assert!(code.contains("https://www.w3.org/ns/activitystreams#Note"));
    }

    #[test]
    fn generates_iri_constants() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub mod iri"));
        assert!(code.contains("NOTE"));
        assert!(code.contains("https://www.w3.org/ns/activitystreams#Note"));
        assert!(code.contains("ACTOR"));
        assert!(code.contains("https://www.w3.org/ns/activitystreams#actor"));
    }

    // === 4.3: @expose_value ===

    #[test]
    fn expose_value_generates_with_method() {
        let contexts = make_simple_context();
        let expose = vec![ExposeValueDirective {
            expanded_iri: "https://www.w3.org/ns/activitystreams#content".to_string(),
        }];
        let tokens = CodeGenerator::generate(&contexts, &expose, &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("fn content_with("));
    }

    #[test]
    fn expose_value_unmatched_iri_returns_error() {
        let contexts = make_simple_context();
        let expose = vec![ExposeValueDirective {
            expanded_iri: "https://nonexistent.example.com#term".to_string(),
        }];
        let result = CodeGenerator::generate(&contexts, &expose, &[]);
        assert!(result.is_err());
        match result.unwrap_err() {
            LDBuildError::CodeGen { term, .. } => {
                assert!(term.contains("nonexistent"));
            }
            other => panic!("Expected CodeGen error, got {:?}", other),
        }
    }

    // === 4.4: Extension traits ===

    #[test]
    fn generates_extension_trait_for_extended_term() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub trait ActivityStreamsExt"));
        assert!(code.contains("fn actor("));
    }

    #[test]
    fn generates_nested_object_for_id_type() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("fn actor_object"));
    }

    #[test]
    fn generates_keyword_alias_setter() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("fn id("));
    }

    #[test]
    fn generates_impl_with_marker_bound() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("HasActivityStreams"));
        assert!(code.contains("ActivityStreamsExt"));
        assert!(code.contains("ld_emit::ObjectSerializer"));
    }

    // === 4.5: ContextSerializer impls ===

    #[test]
    fn generates_context_serializer_impl() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("ld_emit::ContextSerializer"));
        assert!(code.contains("for ActivityStreams<S>"));
        assert!(code.contains("fn context_json()"));
    }

    #[test]
    fn generates_context_serializer_with_original_json() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("https://www.w3.org/ns/activitystreams"));
    }

    // === Task 5: Context serializer optimization tests ===

    #[test]
    fn context_serializer_does_not_use_from_str() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            !code.contains("from_str"),
            "Generated code should not use serde_json::from_str for runtime parsing"
        );
    }

    #[test]
    fn context_serializer_url_source_generates_value_string() {
        // URL source should generate Value::String("url".to_string())
        let contexts = make_simple_context(); // uses ContextSource::Url
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("ld_emit::serde_json::Value::String"),
            "URL source should generate Value::String construction, got:\n{}",
            code
        );
    }

    #[test]
    fn context_serializer_inline_source_generates_value_object() {
        // Inline source should generate Value::Object + Map construction
        let contexts = vec![(
            "inline_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({
                    "name": "https://schema.org/name",
                    "url": {"@id": "https://schema.org/url", "@type": "@id"}
                })),
                terms: vec![
                    TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://schema.org/name".to_string(),
                        },
                    },
                ],
                original_json: serde_json::json!({
                    "name": "https://schema.org/name",
                    "url": {"@id": "https://schema.org/url", "@type": "@id"}
                }),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("ld_emit::serde_json::Map"),
            "Inline source should generate Map construction, got:\n{}",
            code
        );
        assert!(
            code.contains("ld_emit::serde_json::Value::Object"),
            "Inline source should generate Value::Object construction"
        );
        assert!(
            !code.contains("from_str"),
            "Inline source should not use from_str"
        );
    }

    #[test]
    fn context_serializer_does_not_use_json_macro() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            !code.contains("serde_json::json!"),
            "Generated code should not use serde_json::json! macro"
        );
    }

    // === Multi-context tests ===

    #[test]
    fn multi_context_generates_separate_modules() {
        let contexts = make_multi_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub mod activity_streams"));
        assert!(code.contains("pub mod security_v1"));
    }

    #[test]
    fn multi_context_generates_separate_extension_traits() {
        let contexts = make_multi_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub trait ActivityStreamsExt"));
        assert!(code.contains("pub trait SecurityV1Ext"));
    }

    // === Syntax validation ===

    #[test]
    fn generated_single_context_is_valid_rust_syntax() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let _code = format_tokens(tokens); // format_tokens asserts valid syntax
    }

    #[test]
    fn generated_multi_context_is_valid_rust_syntax() {
        let contexts = make_multi_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let _code = format_tokens(tokens);
    }

    #[test]
    fn generated_expose_value_is_valid_rust_syntax() {
        let contexts = make_simple_context();
        let expose = vec![ExposeValueDirective {
            expanded_iri: "https://www.w3.org/ns/activitystreams#content".to_string(),
        }];
        let tokens = CodeGenerator::generate(&contexts, &expose, &[]).unwrap();
        let _code = format_tokens(tokens);
    }

    #[test]
    fn generated_all_term_kinds_is_valid_rust_syntax() {
        let contexts = vec![(
            "mixed".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![
                    TermDefinition {
                        name: "as".to_string(),
                        kind: TermKind::Prefix {
                            uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "id".to_string(),
                        kind: TermKind::KeywordAlias {
                            keyword: "@id".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "type".to_string(),
                        kind: TermKind::KeywordAlias {
                            keyword: "@type".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "Note".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://www.w3.org/ns/activitystreams#Note".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "actor".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://www.w3.org/ns/activitystreams#actor".to_string(),
                            type_coercion: Some(TypeCoercion::Id),
                            container: None,
                        },
                    },
                    TermDefinition {
                        name: "contentMap".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://www.w3.org/ns/activitystreams#content".to_string(),
                            type_coercion: None,
                            container: Some(Container::Language),
                        },
                    },
                ],
                original_json: serde_json::json!({}),
            },
        )];
        let renames = vec![
            RenameDirective { from: "type".to_string(), to: "kind".to_string() },
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &renames).unwrap();
        let _code = format_tokens(tokens);
    }

    #[test]
    fn generated_three_contexts_with_expose_value_is_valid_rust_syntax() {
        let contexts = vec![
            (
                "activity_streams".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://www.w3.org/ns/activitystreams".to_string()),
                    terms: vec![
                        TermDefinition {
                            name: "id".to_string(),
                            kind: TermKind::KeywordAlias { keyword: "@id".to_string() },
                        },
                        TermDefinition {
                            name: "type".to_string(),
                            kind: TermKind::KeywordAlias { keyword: "@type".to_string() },
                        },
                        TermDefinition {
                            name: "Note".to_string(),
                            kind: TermKind::SimpleTerm {
                                iri: "https://www.w3.org/ns/activitystreams#Note".to_string(),
                            },
                        },
                        TermDefinition {
                            name: "Person".to_string(),
                            kind: TermKind::SimpleTerm {
                                iri: "https://www.w3.org/ns/activitystreams#Person".to_string(),
                            },
                        },
                        TermDefinition {
                            name: "content".to_string(),
                            kind: TermKind::SimpleTerm {
                                iri: "https://www.w3.org/ns/activitystreams#content".to_string(),
                            },
                        },
                        TermDefinition {
                            name: "actor".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://www.w3.org/ns/activitystreams#actor".to_string(),
                                type_coercion: Some(TypeCoercion::Id),
                                container: None,
                            },
                        },
                        TermDefinition {
                            name: "name".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://www.w3.org/ns/activitystreams#name".to_string(),
                                type_coercion: None,
                                container: None,
                            },
                        },
                        TermDefinition {
                            name: "summary".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://www.w3.org/ns/activitystreams#summary".to_string(),
                                type_coercion: None,
                                container: None,
                            },
                        },
                    ],
                    original_json: serde_json::json!("https://www.w3.org/ns/activitystreams"),
                },
            ),
            (
                "security_v1".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://w3id.org/security/v1".to_string()),
                    terms: vec![
                        TermDefinition {
                            name: "CryptographicKey".to_string(),
                            kind: TermKind::SimpleTerm {
                                iri: "https://w3id.org/security#Key".to_string(),
                            },
                        },
                        TermDefinition {
                            name: "publicKey".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://w3id.org/security#publicKey".to_string(),
                                type_coercion: Some(TypeCoercion::Id),
                                container: None,
                            },
                        },
                        TermDefinition {
                            name: "publicKeyPem".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://w3id.org/security#publicKeyPem".to_string(),
                                type_coercion: None,
                                container: None,
                            },
                        },
                    ],
                    original_json: serde_json::json!("https://w3id.org/security/v1"),
                },
            ),
            (
                "toot_ext".to_string(),
                ParsedContext {
                    source: ContextSource::Inline(serde_json::json!({
                        "toot": "http://joinmastodon.org/ns#",
                        "discoverable": "toot:discoverable"
                    })),
                    terms: vec![
                        TermDefinition {
                            name: "discoverable".to_string(),
                            kind: TermKind::SimpleTerm {
                                iri: "http://joinmastodon.org/ns#discoverable".to_string(),
                            },
                        },
                    ],
                    original_json: serde_json::json!({
                        "toot": "http://joinmastodon.org/ns#",
                        "discoverable": "toot:discoverable"
                    }),
                },
            ),
        ];
        let expose = vec![
            ExposeValueDirective {
                expanded_iri: "https://www.w3.org/ns/activitystreams#content".to_string(),
            },
            ExposeValueDirective {
                expanded_iri: "http://joinmastodon.org/ns#discoverable".to_string(),
            },
        ];
        let renames = vec![
            RenameDirective { from: "type".to_string(), to: "kind".to_string() },
        ];
        let tokens = CodeGenerator::generate(&contexts, &expose, &renames).unwrap();
        let _code = format_tokens(tokens);
    }

    #[test]
    fn generated_empty_context_is_valid_rust_syntax() {
        let contexts: Vec<(String, ParsedContext)> = vec![];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let _code = format_tokens(tokens);
    }

    // === Warning suppression preamble ===

    #[test]
    fn generated_code_contains_allow_attributes() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("#[allow(dead_code)]"));
        assert!(code.contains("#[allow(unused_imports)]"));
        assert!(code.contains("#[allow(clippy::all)]"));
    }

    // === 3.1: Unit type constants with doc attributes ===

    #[test]
    fn unit_type_constant_has_doc_attribute_with_iri() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // prettyplease renders #[doc = "..."] as /// comments
        assert!(
            code.contains("///JSON-LD type constant: `https://www.w3.org/ns/activitystreams#Note`"),
            "Note should have doc attribute with IRI. Code:\n{}", code
        );
        assert!(
            code.contains("///JSON-LD type constant: `https://www.w3.org/ns/activitystreams#Create`"),
            "Create should have doc attribute with IRI. Code:\n{}", code
        );
    }

    #[test]
    fn unit_type_constant_doc_for_multi_context() {
        let contexts = make_multi_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("///JSON-LD type constant: `https://www.w3.org/ns/activitystreams#Note`"),
            "Note should have doc attribute. Code:\n{}", code
        );
        assert!(
            code.contains("///JSON-LD type constant: `https://w3id.org/security#Key`"),
            "CryptographicKey should have doc attribute. Code:\n{}", code
        );
    }

    #[test]
    fn unit_type_constant_includes_term_name() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("term_name: \"Note\""),
            "Note should have term_name: \"Note\". Code:\n{}", code
        );
        assert!(
            code.contains("term_name: \"Create\""),
            "Create should have term_name: \"Create\". Code:\n{}", code
        );
    }

    // === 3.2: IRI constant dedup ===

    #[test]
    fn iri_constants_dedup_when_same_name_across_contexts() {
        // Two contexts both define a term "name" that maps to IRI constants
        let contexts = vec![
            (
                "ctx_a".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/a".to_string()),
                    terms: vec![
                        TermDefinition {
                            name: "name".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://example.com/a#name".to_string(),
                                type_coercion: None,
                                container: None,
                            },
                        },
                    ],
                    original_json: serde_json::json!("https://example.com/a"),
                },
            ),
            (
                "ctx_b".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/b".to_string()),
                    terms: vec![
                        TermDefinition {
                            name: "name".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://example.com/b#name".to_string(),
                                type_coercion: None,
                                container: None,
                            },
                        },
                    ],
                    original_json: serde_json::json!("https://example.com/b"),
                },
            ),
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // Both should have prefixed constant names
        assert!(
            code.contains("CTX_A_NAME"),
            "Should have CTX_A_NAME for dedup. Code:\n{}", code
        );
        assert!(
            code.contains("CTX_B_NAME"),
            "Should have CTX_B_NAME for dedup. Code:\n{}", code
        );
        // Should NOT have unprefixed NAME
        // (Check that there's no standalone `NAME` without a prefix)
        assert!(
            !code.contains("pub const NAME"),
            "Should not have unprefixed NAME. Code:\n{}", code
        );
    }

    #[test]
    fn iri_constants_no_dedup_when_unique_names() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // Single context, all names unique → no prefix
        assert!(
            code.contains("pub const NOTE"),
            "Should have unprefixed NOTE. Code:\n{}", code
        );
        assert!(
            code.contains("pub const ACTOR"),
            "Should have unprefixed ACTOR. Code:\n{}", code
        );
        // Should NOT have prefixed versions
        assert!(
            !code.contains("ACTIVITY_STREAMS_NOTE"),
            "Should not have prefixed ACTIVITY_STREAMS_NOTE. Code:\n{}", code
        );
    }

    // === Type alias doc attribute ===

    #[test]
    fn type_alias_has_doc_attribute() {
        let contexts = make_multi_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("Composed context type:"));
        assert!(code.contains("ActivityStreams"));
        assert!(code.contains("SecurityV1"));
    }

    // === 4.1: Extension trait doc attributes ===

    #[test]
    fn extension_trait_has_doc_attribute() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("Extension trait for activity_streams context properties."),
            "Extension trait should have doc attribute. Code:\n{}", code
        );
    }

    #[test]
    fn property_setter_has_doc_with_iri() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("Set the `actor` property (`https://www.w3.org/ns/activitystreams#actor`)."),
            "Property setter should have doc with IRI. Code:\n{}", code
        );
    }

    #[test]
    fn keyword_alias_setter_has_doc() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("Set the `id` property (`@id`)."),
            "Keyword alias setter should have doc with alias name and keyword. Code:\n{}", code
        );
    }

    #[test]
    fn nested_object_setter_has_doc() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("Set the `actor` property as a nested object"),
            "Nested object setter should have doc. Code:\n{}", code
        );
    }

    #[test]
    fn expose_value_setter_has_doc() {
        let contexts = make_simple_context();
        let expose = vec![ExposeValueDirective {
            expanded_iri: "https://www.w3.org/ns/activitystreams#content".to_string(),
        }];
        let tokens = CodeGenerator::generate(&contexts, &expose, &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("Set the `content` property with a direct value"),
            "Expose value setter should have doc. Code:\n{}", code
        );
    }

    // === 4.1: Method name dedup across contexts ===

    #[test]
    fn method_name_dedup_across_contexts() {
        let contexts = vec![
            (
                "ctx_a".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/a".to_string()),
                    terms: vec![TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/a#name".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/a"),
                },
            ),
            (
                "ctx_b".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/b".to_string()),
                    terms: vec![TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/b#name".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/b"),
                },
            ),
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("fn ctx_a_name("),
            "Should have prefixed method ctx_a_name. Code:\n{}", code
        );
        assert!(
            code.contains("fn ctx_b_name("),
            "Should have prefixed method ctx_b_name. Code:\n{}", code
        );
    }

    #[test]
    fn method_name_no_dedup_when_unique() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // Single context → no prefixes needed
        assert!(
            code.contains("fn actor("),
            "Should have unprefixed actor method. Code:\n{}", code
        );
        assert!(
            !code.contains("fn activity_streams_actor("),
            "Should not have prefixed method. Code:\n{}", code
        );
    }

    // === 4.2: @rename directive tests ===

    #[test]
    fn rename_replaces_reserved_word_method() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![TermDefinition {
                    name: "type".to_string(),
                    kind: TermKind::KeywordAlias { keyword: "@type".to_string() },
                }],
                original_json: serde_json::json!({}),
            },
        )];
        let renames = vec![RenameDirective {
            from: "type".to_string(),
            to: "kind".to_string(),
        }];
        let tokens = CodeGenerator::generate(&contexts, &[], &renames).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("fn kind("),
            "Should use renamed method. Code:\n{}", code
        );
        assert!(
            !code.contains("fn r#type("),
            "Should not use r#type. Code:\n{}", code
        );
        // JSON key should be the alias name "type" (not the primitive keyword "@type")
        assert!(
            code.contains("\"type\""),
            "JSON key should be alias name. Code:\n{}", code
        );
        // @type alias should use TypeConstant signature, not impl Serialize
        assert!(
            code.contains("TypeConstant"),
            "@type alias method should use TypeConstant signature. Code:\n{}", code
        );
        assert!(
            code.contains("type_def_aliased"),
            "@type alias method should delegate to type_def_aliased. Code:\n{}", code
        );
    }

    #[test]
    fn keyword_alias_reserved_word_auto_resolves_raw_ident() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![TermDefinition {
                    name: "type".to_string(),
                    kind: TermKind::KeywordAlias { keyword: "@type".to_string() },
                }],
                original_json: serde_json::json!({}),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("fn r#type("),
            "KeywordAlias 'type' should auto-resolve to r#type. Code:\n{}", code
        );
        assert!(
            code.contains("ld_emit::TypeConstant") && !code.contains("dyn ld_emit::TypeConstant"),
            "@type alias should use ld_emit::TypeConstant without dyn. Code:\n{}", code
        );
        assert!(
            code.contains("type_def_aliased"),
            "@type alias should delegate to type_def_aliased. Code:\n{}", code
        );
    }

    #[test]
    fn rename_target_reserved_word_returns_error() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![TermDefinition {
                    name: "type".to_string(),
                    kind: TermKind::KeywordAlias { keyword: "@type".to_string() },
                }],
                original_json: serde_json::json!({}),
            },
        )];
        let renames = vec![RenameDirective {
            from: "type".to_string(),
            to: "struct".to_string(),
        }];
        let result = CodeGenerator::generate(&contexts, &[], &renames);
        assert!(result.is_err());
        match result.unwrap_err() {
            LDBuildError::CodeGen { term, message } => {
                assert_eq!(term, "type");
                assert!(message.contains("also a Rust reserved word"), "Error message: {}", message);
            }
            other => panic!("Expected CodeGen error, got {:?}", other),
        }
    }

    #[test]
    fn prefix_term_with_reserved_name_no_error() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![TermDefinition {
                    name: "as".to_string(),
                    kind: TermKind::Prefix {
                        uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                    },
                }],
                original_json: serde_json::json!({}),
            },
        )];
        // No @rename needed — Prefix terms don't generate methods
        let result = CodeGenerator::generate(&contexts, &[], &[]);
        assert!(result.is_ok(), "Prefix with reserved name should not error");
    }

    #[test]
    fn rename_preserves_json_key_for_extended_term() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![TermDefinition {
                    name: "type".to_string(),
                    kind: TermKind::ExtendedTerm {
                        id: "https://example.com#type".to_string(),
                        type_coercion: None,
                        container: None,
                    },
                }],
                original_json: serde_json::json!({}),
            },
        )];
        let renames = vec![RenameDirective {
            from: "type".to_string(),
            to: "kind".to_string(),
        }];
        let tokens = CodeGenerator::generate(&contexts, &[], &renames).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("fn kind("),
            "Should use renamed method. Code:\n{}", code
        );
        // JSON key should still be "type" (original term name)
        assert!(
            code.contains("\"type\""),
            "JSON key should be original term name. Code:\n{}", code
        );
    }

    #[test]
    fn rename_with_dedup_uses_prefixed_renamed_method() {
        // Two contexts both define "type" as ExtendedTerm — both need @rename
        // and both should get dedup prefix
        let contexts = vec![
            (
                "ctx_a".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/a".to_string()),
                    terms: vec![TermDefinition {
                        name: "type".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/a#type".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/a"),
                },
            ),
            (
                "ctx_b".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/b".to_string()),
                    terms: vec![TermDefinition {
                        name: "type".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/b#type".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/b"),
                },
            ),
        ];
        let renames = vec![RenameDirective {
            from: "type".to_string(),
            to: "kind".to_string(),
        }];
        let tokens = CodeGenerator::generate(&contexts, &[], &renames).unwrap();
        let code = format_tokens(tokens);
        // Both should be deduped with context prefix applied to the renamed name
        assert!(
            code.contains("fn ctx_a_kind("),
            "Should have prefixed renamed method. Code:\n{}", code
        );
        assert!(
            code.contains("fn ctx_b_kind("),
            "Should have prefixed renamed method. Code:\n{}", code
        );
    }

    // ========================================================================
    // Task 7.2: Edge case & comprehensive coverage tests
    // ========================================================================

    // --- Method name dedup: 3+ contexts ---

    #[test]
    fn method_name_dedup_three_contexts_same_property() {
        let contexts = vec![
            (
                "ctx_a".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/a".to_string()),
                    terms: vec![TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/a#name".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/a"),
                },
            ),
            (
                "ctx_b".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/b".to_string()),
                    terms: vec![TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/b#name".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/b"),
                },
            ),
            (
                "ctx_c".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/c".to_string()),
                    terms: vec![TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/c#name".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/c"),
                },
            ),
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("fn ctx_a_name("), "Code:\n{}", code);
        assert!(code.contains("fn ctx_b_name("), "Code:\n{}", code);
        assert!(code.contains("fn ctx_c_name("), "Code:\n{}", code);
        // No unprefixed "name" method
        assert!(!code.contains("fn name("), "Should not have unprefixed name. Code:\n{}", code);
    }

    #[test]
    fn method_name_dedup_only_duplicates_get_prefixed() {
        // ctx_a has "name" and "title"; ctx_b has "name" only.
        // "name" should be prefixed (duplicate), "title" should not.
        let contexts = vec![
            (
                "ctx_a".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/a".to_string()),
                    terms: vec![
                        TermDefinition {
                            name: "name".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://example.com/a#name".to_string(),
                                type_coercion: None,
                                container: None,
                            },
                        },
                        TermDefinition {
                            name: "title".to_string(),
                            kind: TermKind::ExtendedTerm {
                                id: "https://example.com/a#title".to_string(),
                                type_coercion: None,
                                container: None,
                            },
                        },
                    ],
                    original_json: serde_json::json!("https://example.com/a"),
                },
            ),
            (
                "ctx_b".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/b".to_string()),
                    terms: vec![TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/b#name".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/b"),
                },
            ),
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // "name" is duplicated → prefixed
        assert!(code.contains("fn ctx_a_name("), "Code:\n{}", code);
        assert!(code.contains("fn ctx_b_name("), "Code:\n{}", code);
        // "title" is unique → not prefixed
        assert!(code.contains("fn title("), "Code:\n{}", code);
        assert!(!code.contains("fn ctx_a_title("), "Code:\n{}", code);
    }

    // --- IRI constant dedup: 3 contexts ---

    #[test]
    fn iri_constants_dedup_three_contexts_same_name() {
        let contexts = vec![
            (
                "ctx_a".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/a".to_string()),
                    terms: vec![TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/a#name".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/a"),
                },
            ),
            (
                "ctx_b".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/b".to_string()),
                    terms: vec![TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/b#name".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/b"),
                },
            ),
            (
                "ctx_c".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/c".to_string()),
                    terms: vec![TermDefinition {
                        name: "name".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://example.com/c#name".to_string(),
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/c"),
                },
            ),
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("CTX_A_NAME"), "Code:\n{}", code);
        assert!(code.contains("CTX_B_NAME"), "Code:\n{}", code);
        assert!(code.contains("CTX_C_NAME"), "Code:\n{}", code);
        assert!(!code.contains("pub const NAME: &str"), "Should not have unprefixed NAME IRI constant. Code:\n{}", code);
    }

    #[test]
    fn iri_constants_dedup_mixed_simple_and_extended_terms() {
        // SimpleTerm and ExtendedTerm with same name both produce IRI constants
        let contexts = vec![
            (
                "ctx_a".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/a".to_string()),
                    terms: vec![TermDefinition {
                        name: "status".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://example.com/a#status".to_string(),
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/a"),
                },
            ),
            (
                "ctx_b".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/b".to_string()),
                    terms: vec![TermDefinition {
                        name: "status".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com/b#status".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    }],
                    original_json: serde_json::json!("https://example.com/b"),
                },
            ),
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("CTX_A_STATUS"), "Code:\n{}", code);
        assert!(code.contains("CTX_B_STATUS"), "Code:\n{}", code);
    }

    // --- @rename: multiple directives ---

    #[test]
    fn multiple_rename_directives() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![
                    TermDefinition {
                        name: "type".to_string(),
                        kind: TermKind::KeywordAlias { keyword: "@type".to_string() },
                    },
                    TermDefinition {
                        name: "as".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com#as".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    },
                ],
                original_json: serde_json::json!({}),
            },
        )];
        let renames = vec![
            RenameDirective { from: "type".to_string(), to: "kind".to_string() },
            RenameDirective { from: "as".to_string(), to: "as_value".to_string() },
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &renames).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("fn kind("), "Should rename 'type' to 'kind'. Code:\n{}", code);
        assert!(code.contains("fn as_value("), "Should rename 'as' to 'as_value'. Code:\n{}", code);
        // JSON keys use alias names (not primitive keywords)
        assert!(code.contains("\"type\""), "JSON key should be alias name 'type'. Code:\n{}", code);
        assert!(code.contains("\"as\""), "JSON key should be 'as'. Code:\n{}", code);
    }

    #[test]
    fn rename_extended_term_with_id_type_generates_renamed_object_method() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![TermDefinition {
                    name: "type".to_string(),
                    kind: TermKind::ExtendedTerm {
                        id: "https://example.com#type".to_string(),
                        type_coercion: Some(TypeCoercion::Id),
                        container: None,
                    },
                }],
                original_json: serde_json::json!({}),
            },
        )];
        let renames = vec![RenameDirective {
            from: "type".to_string(),
            to: "kind".to_string(),
        }];
        let tokens = CodeGenerator::generate(&contexts, &[], &renames).unwrap();
        let code = format_tokens(tokens);
        // Both value setter and nested object setter should use renamed name
        assert!(code.contains("fn kind("), "Code:\n{}", code);
        assert!(code.contains("fn kind_object"), "Should generate kind_object method. Code:\n{}", code);
        assert!(!code.contains("fn type("), "Should not have 'type' method. Code:\n{}", code);
    }

    #[test]
    fn rename_for_nonexistent_term_is_silently_ignored() {
        // @rename for a term that doesn't exist in any context should be a no-op
        let contexts = make_simple_context();
        let renames = vec![RenameDirective {
            from: "nonexistent".to_string(),
            to: "something".to_string(),
        }];
        let result = CodeGenerator::generate(&contexts, &[], &renames);
        assert!(result.is_ok(), "Unknown @rename should not cause error");
    }

    #[test]
    fn reserved_word_as_extended_term_without_rename_errors() {
        // "fn" is a reserved word, as an ExtendedTerm it should require @rename
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![TermDefinition {
                    name: "fn".to_string(),
                    kind: TermKind::ExtendedTerm {
                        id: "https://example.com#fn".to_string(),
                        type_coercion: None,
                        container: None,
                    },
                }],
                original_json: serde_json::json!({}),
            },
        )];
        let result = CodeGenerator::generate(&contexts, &[], &[]);
        assert!(result.is_err());
        match result.unwrap_err() {
            LDBuildError::CodeGen { term, message } => {
                assert_eq!(term, "fn");
                assert!(message.contains("reserved word"), "Message: {}", message);
            }
            other => panic!("Expected CodeGen error, got {:?}", other),
        }
    }

    // --- @context JSON optimization edge cases ---

    #[test]
    fn context_serializer_inline_with_nested_arrays() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![],
                original_json: serde_json::json!({
                    "items": ["a", "b", ["nested"]]
                }),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("ld_emit::serde_json::Value::Array"), "Code:\n{}", code);
        assert!(!code.contains("from_str"), "Should not use from_str. Code:\n{}", code);
    }

    #[test]
    fn context_serializer_inline_with_null_and_bool() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![],
                original_json: serde_json::json!({
                    "flag": true,
                    "empty": null,
                    "off": false
                }),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("ld_emit::serde_json::Value::Bool(true)"), "Code:\n{}", code);
        assert!(code.contains("ld_emit::serde_json::Value::Bool(false)"), "Code:\n{}", code);
        assert!(code.contains("ld_emit::serde_json::Value::Null"), "Code:\n{}", code);
    }

    #[test]
    fn context_serializer_inline_with_number_values() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![],
                original_json: serde_json::json!({
                    "@version": 1.1
                }),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("ld_emit::serde_json::Value::Number"), "Code:\n{}", code);
        assert!(!code.contains("from_str"), "Code:\n{}", code);
    }

    // --- doc attribute: detailed content verification ---

    #[test]
    fn doc_attribute_for_context_type_alias_lists_all_contexts() {
        let contexts = vec![
            (
                "activity_streams".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/as".to_string()),
                    terms: vec![],
                    original_json: serde_json::json!("https://example.com/as"),
                },
            ),
            (
                "security_v1".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/sec".to_string()),
                    terms: vec![],
                    original_json: serde_json::json!("https://example.com/sec"),
                },
            ),
            (
                "toot_ext".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/toot".to_string()),
                    terms: vec![],
                    original_json: serde_json::json!("https://example.com/toot"),
                },
            ),
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // Doc should list all three context names
        assert!(
            code.contains("ActivityStreams + SecurityV1 + TootExt"),
            "Context alias doc should list all context types. Code:\n{}", code
        );
    }

    #[test]
    fn doc_attribute_for_expose_value_setter() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Url("https://example.com".to_string()),
                terms: vec![TermDefinition {
                    name: "myProp".to_string(),
                    kind: TermKind::SimpleTerm {
                        iri: "https://example.com#myProp".to_string(),
                    },
                }],
                original_json: serde_json::json!("https://example.com"),
            },
        )];
        let expose = vec![ExposeValueDirective {
            expanded_iri: "https://example.com#myProp".to_string(),
        }];
        let tokens = CodeGenerator::generate(&contexts, &expose, &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            code.contains("Set the `myProp` property with a direct value"),
            "Expose value should have doc. Code:\n{}", code
        );
    }

    // --- allow attributes: module wrapper structure ---

    #[test]
    fn allow_attributes_wrap_inner_module() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // The allow attributes should be on the wrapper module
        assert!(code.contains("#[allow(dead_code)]"), "Code:\n{}", code);
        assert!(code.contains("#[allow(unused_imports)]"), "Code:\n{}", code);
        assert!(code.contains("#[allow(clippy::all)]"), "Code:\n{}", code);
        // The inner module and re-export pattern
        assert!(code.contains("mod __ld_emit_inner"), "Code:\n{}", code);
        assert!(code.contains("pub use __ld_emit_inner::*"), "Code:\n{}", code);
    }

    // --- Context with only Prefix terms (no methods, no IRI constants) ---

    #[test]
    fn context_with_only_prefix_terms_generates_no_methods() {
        let contexts = vec![(
            "prefix_only".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![
                    TermDefinition {
                        name: "as".to_string(),
                        kind: TermKind::Prefix {
                            uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "sec".to_string(),
                        kind: TermKind::Prefix {
                            uri: "https://w3id.org/security#".to_string(),
                        },
                    },
                ],
                original_json: serde_json::json!({}),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // Should still generate the struct, marker trait, ext trait (empty body)
        assert!(code.contains("pub struct PrefixOnly"), "Code:\n{}", code);
        assert!(code.contains("pub trait HasPrefixOnly"), "Code:\n{}", code);
        assert!(code.contains("pub trait PrefixOnlyExt"), "Code:\n{}", code);
        // IRI module should exist but be empty (Prefix terms don't produce IRI constants)
        assert!(code.contains("pub mod iri"), "Code:\n{}", code);
    }

    // --- Context with empty terms ---

    #[test]
    fn context_with_empty_terms() {
        let contexts = vec![(
            "empty_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Url("https://example.com".to_string()),
                terms: vec![],
                original_json: serde_json::json!("https://example.com"),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("pub struct EmptyCtx"), "Code:\n{}", code);
        assert!(code.contains("pub type Context = EmptyCtx"), "Code:\n{}", code);
        // Valid Rust syntax (already asserted by format_tokens)
    }

    // --- Prefix type reserved word combined with property reserved word ---

    #[test]
    fn prefix_reserved_and_property_reserved_in_same_context() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Inline(serde_json::json!({})),
                terms: vec![
                    TermDefinition {
                        name: "as".to_string(),
                        kind: TermKind::Prefix {
                            uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "type".to_string(),
                        kind: TermKind::KeywordAlias { keyword: "@type".to_string() },
                    },
                ],
                original_json: serde_json::json!({}),
            },
        )];
        let renames = vec![RenameDirective {
            from: "type".to_string(),
            to: "kind".to_string(),
        }];
        let tokens = CodeGenerator::generate(&contexts, &[], &renames).unwrap();
        let code = format_tokens(tokens);
        // Prefix "as" → no error, no method
        // "type" → renamed to "kind"
        assert!(code.contains("fn kind("), "Code:\n{}", code);
    }

    // --- Verify no r#ident in generated output ---

    #[test]
    fn generated_code_never_contains_raw_identifiers() {
        let contexts = make_multi_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(
            !code.contains("r#"),
            "Generated code should not contain raw identifiers. Code:\n{}", code
        );
    }

    // --- camelCase property names produce correct snake_case methods ---

    #[test]
    fn camel_case_property_generates_snake_case_method() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Url("https://example.com".to_string()),
                terms: vec![
                    TermDefinition {
                        name: "publicKeyPem".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com#publicKeyPem".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    },
                    TermDefinition {
                        name: "manuallyApprovesFollowers".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com#manuallyApprovesFollowers".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    },
                ],
                original_json: serde_json::json!("https://example.com"),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("fn public_key_pem("), "Code:\n{}", code);
        assert!(code.contains("fn manually_approves_followers("), "Code:\n{}", code);
        // JSON keys should remain camelCase
        assert!(code.contains("\"publicKeyPem\""), "Code:\n{}", code);
        assert!(code.contains("\"manuallyApprovesFollowers\""), "Code:\n{}", code);
    }

    // --- PascalCase type constants from camelCase terms ---

    #[test]
    fn screaming_snake_case_type_constants_from_various_inputs() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Url("https://example.com".to_string()),
                terms: vec![
                    TermDefinition {
                        name: "CryptographicKey".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://example.com#CryptographicKey".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "publicKey".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://example.com#publicKey".to_string(),
                        },
                    },
                ],
                original_json: serde_json::json!("https://example.com"),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // PascalCase → SCREAMING_SNAKE_CASE
        assert!(code.contains("pub const CRYPTOGRAPHIC_KEY: ld_emit::TypeConstant"), "Code:\n{}", code);
        // camelCase → SCREAMING_SNAKE_CASE
        assert!(code.contains("pub const PUBLIC_KEY: ld_emit::TypeConstant"), "Code:\n{}", code);
    }

    // --- Multiple expose_value directives ---

    #[test]
    fn multiple_expose_values_generate_multiple_with_methods() {
        let contexts = vec![(
            "test_ctx".to_string(),
            ParsedContext {
                source: ContextSource::Url("https://example.com".to_string()),
                terms: vec![
                    TermDefinition {
                        name: "content".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://example.com#content".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "discoverable".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://example.com#discoverable".to_string(),
                        },
                    },
                ],
                original_json: serde_json::json!("https://example.com"),
            },
        )];
        let expose = vec![
            ExposeValueDirective { expanded_iri: "https://example.com#content".to_string() },
            ExposeValueDirective { expanded_iri: "https://example.com#discoverable".to_string() },
        ];
        let tokens = CodeGenerator::generate(&contexts, &expose, &[]).unwrap();
        let code = format_tokens(tokens);
        assert!(code.contains("fn content_with("), "Code:\n{}", code);
        assert!(code.contains("fn discoverable_with("), "Code:\n{}", code);
    }

    // --- ContextSerializer impl generates correct code per source type ---

    #[test]
    fn context_serializer_multi_context_each_has_impl() {
        let contexts = make_multi_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // Both contexts should have ContextSerializer impls
        // prettyplease may wrap long impl lines, so check for key parts
        assert!(code.contains("ld_emit::ContextSerializer"), "Code:\n{}", code);
        assert!(code.contains("for ActivityStreams<S>"), "Code:\n{}", code);
        assert!(code.contains("for SecurityV1<S>"), "Code:\n{}", code);
        assert!(code.contains("fn context_json()"), "Code:\n{}", code);
    }

    // --- Cross-forwarding: 3 contexts ---

    #[test]
    fn cross_forwarding_three_contexts() {
        let contexts = vec![
            (
                "ctx_a".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/a".to_string()),
                    terms: vec![],
                    original_json: serde_json::json!("https://example.com/a"),
                },
            ),
            (
                "ctx_b".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/b".to_string()),
                    terms: vec![],
                    original_json: serde_json::json!("https://example.com/b"),
                },
            ),
            (
                "ctx_c".to_string(),
                ParsedContext {
                    source: ContextSource::Url("https://example.com/c".to_string()),
                    terms: vec![],
                    original_json: serde_json::json!("https://example.com/c"),
                },
            ),
        ];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // Each context should forward all other marker traits
        // CtxA forwards HasCtxB and HasCtxC
        assert!(code.contains("impl<S: HasCtxB> HasCtxB for CtxA<S>"), "Code:\n{}", code);
        assert!(code.contains("impl<S: HasCtxC> HasCtxC for CtxA<S>"), "Code:\n{}", code);
        // CtxB forwards HasCtxA and HasCtxC
        assert!(code.contains("impl<S: HasCtxA> HasCtxA for CtxB<S>"), "Code:\n{}", code);
        assert!(code.contains("impl<S: HasCtxC> HasCtxC for CtxB<S>"), "Code:\n{}", code);
        // Type alias: CtxA<CtxB<CtxC>>
        assert!(code.contains("pub type Context = CtxA<CtxB<CtxC>>"), "Code:\n{}", code);
    }

    // --- Single context: no cross-forwarding ---

    #[test]
    fn single_context_no_cross_forwarding() {
        let contexts = make_simple_context();
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // Should not contain cross-forwarding impls
        // Only the direct marker impl: impl<S> HasActivityStreams for ActivityStreams<S>
        let cross_fwd_count = code.matches("impl<S: Has").count();
        assert_eq!(cross_fwd_count, 0, "Single context should have no cross-forwarding. Code:\n{}", code);
    }

    #[test]
    fn iri_constants_intra_context_collision_simple_term_wins() {
        // Within a single context, "Image" (SimpleTerm) and "image" (ExtendedTerm)
        // both produce SCREAMING_SNAKE_CASE "IMAGE". SimpleTerm should win.
        let contexts = vec![(
            "ctx".to_string(),
            ParsedContext {
                source: ContextSource::Url("https://example.com/ctx".to_string()),
                terms: vec![
                    TermDefinition {
                        name: "Image".to_string(),
                        kind: TermKind::SimpleTerm {
                            iri: "https://example.com#Image".to_string(),
                        },
                    },
                    TermDefinition {
                        name: "image".to_string(),
                        kind: TermKind::ExtendedTerm {
                            id: "https://example.com#image".to_string(),
                            type_coercion: None,
                            container: None,
                        },
                    },
                ],
                original_json: serde_json::json!("https://example.com/ctx"),
            },
        )];
        let tokens = CodeGenerator::generate(&contexts, &[], &[]).unwrap();
        let code = format_tokens(tokens);
        // SimpleTerm "Image" should produce the constant with the SimpleTerm IRI
        assert!(
            code.contains(r#"pub const IMAGE: &str = "https://example.com#Image""#),
            "SimpleTerm should win. Code:\n{}", code
        );
        // ExtendedTerm "image" should NOT produce a separate constant
        assert!(
            !code.contains(r#""https://example.com#image""#),
            "ExtendedTerm IRI should be skipped. Code:\n{}", code
        );
    }
}
