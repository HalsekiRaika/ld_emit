mod code_generator;
pub mod code_utils;
mod context_builder;
mod context_fetcher;
mod context_parser;
mod error;
mod models;
mod prefix_resolver;

pub use code_generator::CodeGenerator;
pub use context_builder::ContextBuilder;
pub use context_fetcher::ContextFetcher;
pub use context_parser::ContextParser;
pub use error::LDBuildError;
pub use models::*;
pub use prefix_resolver::PrefixResolver;

// Re-export serde_json for use in ld_context! macro
pub use serde_json;

/// Macro to declare JSON-LD contexts and generate serializer code.
///
/// # Basic Usage
///
/// In `build.rs`:
/// ```ignore
/// ld_emit_build::ld_context! {
///     @serializer_name "activity_pub",
///     @export_dir "src/generated",
///     activity_streams: {
///         "id": "@id",
///         "type": "@type",
///         "as": "https://www.w3.org/ns/activitystreams#",
///         "Note": "as:Note",
///         "name": {"@id": "as:name"},
///     },
///     @expose_value {
///         "https://www.w3.org/ns/activitystreams#content",
///     }
/// }
/// ```
///
/// In your crate's source:
/// ```ignore
/// pub mod generated {
///     ld_emit::include_ld!("activity_pub");
/// }
/// pub use self::generated::*;
/// ```
///
/// # Directives
///
/// | Directive | Description |
/// |-----------|-------------|
/// | `@serializer_name "name"` | **Required.** Output filename stem (`{name}.rs` in `OUT_DIR`) |
/// | `name: "url"` | URL context (fetched at build time) |
/// | `name: { ... }` | Inline JSON-LD context object |
/// | `@expose_value { "iri", ... }` | Generate `_with` setter methods for the specified IRIs |
/// | `@rename { "from" -> "to", ... }` | Rename terms to alternative Rust identifiers (see below) |
/// | `@export_dir "dir"` | Also write the generated file to a source-tree directory (see below) |
///
/// # Reserved Word Handling
///
/// When a JSON-LD keyword alias (e.g., `"type": "@type"`, `"id": "@id"`) maps to a
/// Rust reserved word, the generated method automatically uses a raw identifier (`r#type`,
/// `r#as`, etc.). No `@rename` directive is needed:
///
/// ```ignore
/// // In build.rs — no @rename needed for keyword aliases
/// activity_streams: {
///     "type": "@type",  // generates fn r#type(...)
///     "id": "@id",      // "id" is not reserved, generates fn id(...)
///     // ...
/// }
/// ```
///
/// ```ignore
/// // In your serializer implementation
/// ser.r#type(&[&activity_streams::Person])
///    .id(&self.id);
/// ```
///
/// For non-keyword terms (`ExtendedTerm`) that collide with reserved words, `@rename`
/// is still required:
///
/// ```ignore
/// @rename {
///     "match" -> "match_value"
/// }
/// ```
///
/// # Multiple `ld_context!` Invocations
///
/// Each `ld_context!` call must have a unique `@serializer_name`. This allows multiple
/// independent invocations in the same `build.rs` without overwriting each other:
///
/// ```ignore
/// ld_emit_build::ld_context! {
///     @serializer_name "activity_pub",
///     activity_streams: { ... },
/// }
/// ld_emit_build::ld_context! {
///     @serializer_name "activitypub_gen",
///     activitystream: "https://www.w3.org/ns/activitystreams#",
/// }
/// ```
///
/// # IDE Compatibility
///
/// ## rust-analyzer
///
/// `ld_emit::include_ld!` works out of the box with rust-analyzer. No additional
/// configuration is needed.
///
/// ## JetBrains IDE (RustRover / IntelliJ Rust)
///
/// `ld_emit::include_ld!` internally expands to `include!(concat!(env!("OUT_DIR"), ...))`.
/// IntelliJ Rust plugin may not resolve `env!("OUT_DIR")` during static analysis,
/// which can prevent code completion and navigation for the generated symbols.
/// This is a known limitation of the IntelliJ Rust plugin
/// (see [intellij-rust#1908](https://github.com/intellij-rust/intellij-rust/issues/1908),
/// [intellij-rust#8780](https://github.com/intellij-rust/intellij-rust/issues/8780)).
///
/// To work around this, use `@export_dir` to also write the generated file into your
/// source tree, then use `#[path]` so the IDE can resolve it directly:
///
/// In `build.rs`:
/// ```ignore
/// ld_emit_build::ld_context! {
///     @serializer_name "activity_pub",
///     @export_dir "src/generated",
///     activity_streams: { ... },
///     // ...
/// }
/// ```
///
/// In your crate's source (e.g. `src/main.rs` or `src/lib.rs`):
/// ```ignore
/// #[path = "generated/activity_pub.rs"]
/// pub mod generated;
/// pub use self::generated::*;
/// ```
///
/// The `@export_dir` path is relative to `CARGO_MANIFEST_DIR`. The output filename is
/// derived from `@serializer_name`. Remember to add the generated directory to
/// `.gitignore` (e.g. `src/generated/`).
#[macro_export]
macro_rules! ld_context {
    ( $( $item:tt )* ) => {{
        let out_dir = ::std::env::var_os("OUT_DIR").unwrap();
        let mut builder = $crate::ContextBuilder::new(&out_dir);
        $crate::__process_context_items!(builder, $($item)*);
        builder.generate().expect("ld_emit_build: code generation failed");
    }};
}

/// Internal helper macro to process individual items in ld_context!.
/// Not part of the public API.
#[macro_export]
#[doc(hidden)]
macro_rules! __process_context_items {
    // Base case: no more items
    ($builder:ident,) => {};

    // @serializer_name "name" with trailing comma
    ($builder:ident, @serializer_name $name:literal , $( $rest:tt )*) => {
        $builder.set_serializer_name($name);
        $crate::__process_context_items!($builder, $($rest)*);
    };

    // @serializer_name "name" as last item
    ($builder:ident, @serializer_name $name:literal) => {
        $builder.set_serializer_name($name);
    };

    // @export_dir "path" with trailing comma
    ($builder:ident, @export_dir $path:literal , $( $rest:tt )*) => {
        $builder.set_export_dir($path);
        $crate::__process_context_items!($builder, $($rest)*);
    };

    // @export_dir "path" as last item
    ($builder:ident, @export_dir $path:literal) => {
        $builder.set_export_dir($path);
    };

    // @expose_value { "iri1", "iri2", ... } with trailing comma
    ($builder:ident, @expose_value { $( $iri:literal ),* $(,)? } , $( $rest:tt )*) => {
        $(
            $builder.add_expose_value($iri);
        )*
        $crate::__process_context_items!($builder, $($rest)*);
    };

    // @expose_value { "iri1", "iri2", ... } as last item or without trailing comma
    ($builder:ident, @expose_value { $( $iri:literal ),* $(,)? } $( $rest:tt )*) => {
        $(
            $builder.add_expose_value($iri);
        )*
        $crate::__process_context_items!($builder, $($rest)*);
    };

    // @rename { "from" -> "to", ... } with trailing comma
    ($builder:ident, @rename { $( $from:literal -> $to:literal ),* $(,)? } , $( $rest:tt )*) => {
        $(
            $builder.add_rename($from, $to);
        )*
        $crate::__process_context_items!($builder, $($rest)*);
    };

    // @rename { "from" -> "to", ... } as last item or without trailing comma
    ($builder:ident, @rename { $( $from:literal -> $to:literal ),* $(,)? } $( $rest:tt )*) => {
        $(
            $builder.add_rename($from, $to);
        )*
        $crate::__process_context_items!($builder, $($rest)*);
    };

    // name: { ... }, (inline context object)
    ($builder:ident, $name:ident : { $( $json:tt )* } , $( $rest:tt )*) => {
        $builder.add_context(
            stringify!($name),
            $crate::ContextSource::Inline($crate::serde_json::json!({ $($json)* })),
        );
        $crate::__process_context_items!($builder, $($rest)*);
    };

    // name: { ... } (inline context object, last item without trailing comma)
    ($builder:ident, $name:ident : { $( $json:tt )* }) => {
        $builder.add_context(
            stringify!($name),
            $crate::ContextSource::Inline($crate::serde_json::json!({ $($json)* })),
        );
    };

    // name: "url", (URL context)
    ($builder:ident, $name:ident : $url:literal , $( $rest:tt )*) => {
        $builder.add_context(
            stringify!($name),
            $crate::ContextSource::Url($url.to_string()),
        );
        $crate::__process_context_items!($builder, $($rest)*);
    };

    // name: "url" (URL context, last item without trailing comma)
    ($builder:ident, $name:ident : $url:literal) => {
        $builder.add_context(
            stringify!($name),
            $crate::ContextSource::Url($url.to_string()),
        );
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    // === Data Model Tests ===

    #[test]
    fn term_kind_prefix() {
        let kind = TermKind::Prefix {
            uri: "https://www.w3.org/ns/activitystreams#".to_string(),
        };
        match &kind {
            TermKind::Prefix { uri } => {
                assert!(uri.ends_with('#') || uri.ends_with('/'));
            }
            _ => panic!("Expected Prefix"),
        }
    }

    #[test]
    fn term_kind_keyword_alias() {
        let kind = TermKind::KeywordAlias {
            keyword: "@id".to_string(),
        };
        match &kind {
            TermKind::KeywordAlias { keyword } => assert_eq!(keyword, "@id"),
            _ => panic!("Expected KeywordAlias"),
        }
    }

    #[test]
    fn term_kind_simple_term() {
        let kind = TermKind::SimpleTerm {
            iri: "https://www.w3.org/ns/activitystreams#Note".to_string(),
        };
        match &kind {
            TermKind::SimpleTerm { iri } => {
                assert_eq!(iri, "https://www.w3.org/ns/activitystreams#Note");
            }
            _ => panic!("Expected SimpleTerm"),
        }
    }

    #[test]
    fn term_kind_extended_term() {
        let kind = TermKind::ExtendedTerm {
            id: "https://www.w3.org/ns/activitystreams#actor".to_string(),
            type_coercion: Some(TypeCoercion::Id),
            container: None,
        };
        match &kind {
            TermKind::ExtendedTerm {
                id,
                type_coercion,
                container,
            } => {
                assert_eq!(id, "https://www.w3.org/ns/activitystreams#actor");
                assert!(matches!(type_coercion, Some(TypeCoercion::Id)));
                assert!(container.is_none());
            }
            _ => panic!("Expected ExtendedTerm"),
        }
    }

    #[test]
    fn term_definition_creation() {
        let td = TermDefinition {
            name: "Note".to_string(),
            kind: TermKind::SimpleTerm {
                iri: "https://www.w3.org/ns/activitystreams#Note".to_string(),
            },
        };
        assert_eq!(td.name, "Note");
    }

    #[test]
    fn parsed_context_creation() {
        let pc = ParsedContext {
            source: ContextSource::Url("https://www.w3.org/ns/activitystreams".to_string()),
            terms: vec![],
            original_json: serde_json::json!("https://www.w3.org/ns/activitystreams"),
        };
        assert!(matches!(pc.source, ContextSource::Url(_)));
        assert!(pc.terms.is_empty());
    }

    #[test]
    fn context_source_variants() {
        let url = ContextSource::Url("https://example.com".to_string());
        assert!(matches!(url, ContextSource::Url(_)));

        let inline = ContextSource::Inline(serde_json::json!({"key": "value"}));
        assert!(matches!(inline, ContextSource::Inline(_)));
    }

    #[test]
    fn expose_value_directive_creation() {
        let d = ExposeValueDirective {
            expanded_iri: "https://www.w3.org/ns/activitystreams#content".to_string(),
        };
        assert_eq!(
            d.expanded_iri,
            "https://www.w3.org/ns/activitystreams#content"
        );
    }

    #[test]
    fn rename_directive_creation() {
        let r = RenameDirective {
            from: "type".to_string(),
            to: "type_value".to_string(),
        };
        assert_eq!(r.from, "type");
        assert_eq!(r.to, "type_value");
    }

    #[test]
    fn rename_directive_clone() {
        let r = RenameDirective {
            from: "as".to_string(),
            to: "as_value".to_string(),
        };
        let cloned = r.clone();
        assert_eq!(cloned.from, "as");
        assert_eq!(cloned.to, "as_value");
    }

    #[test]
    fn type_coercion_variants() {
        let _id = TypeCoercion::Id;
        let _vocab = TypeCoercion::Vocab;
        let _xsd = TypeCoercion::Xsd("xsd:dateTime".to_string());
    }

    #[test]
    fn container_variants() {
        let _lang = Container::Language;
        let _list = Container::List;
        let _set = Container::Set;
        let _graph = Container::Graph;
    }

    // === LDBuildError Tests ===

    #[test]
    fn ld_build_error_display() {
        let err = LDBuildError::Parse {
            context: "test".to_string(),
            message: "unexpected token".to_string(),
            position: Some(42),
        };
        let msg = format!("{}", err);
        assert!(msg.contains("unexpected token"));
        assert!(msg.contains("42"));
    }

    #[test]
    fn ld_build_error_io() {
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let err = LDBuildError::Io(io_err);
        let msg = format!("{}", err);
        assert!(msg.contains("file not found"));
    }

    #[test]
    fn ld_build_error_implements_std_error() {
        let err = LDBuildError::CodeGen {
            term: "test".to_string(),
            message: "failed".to_string(),
        };
        let _e: &dyn std::error::Error = &err;
    }

    #[test]
    fn ld_build_error_from_io() {
        let io_err = std::io::Error::new(std::io::ErrorKind::Other, "test");
        let _err: LDBuildError = io_err.into();
    }

    // === PrefixResolver Tests ===

    #[test]
    fn prefix_resolver_basic() {
        let mut r = PrefixResolver::new();
        r.add_prefix("as", "https://www.w3.org/ns/activitystreams#");
        assert_eq!(
            r.resolve("as:Note"),
            Some("https://www.w3.org/ns/activitystreams#Note".to_string())
        );
    }

    #[test]
    fn prefix_resolver_toot_namespace() {
        let mut r = PrefixResolver::new();
        r.add_prefix("toot", "http://joinmastodon.org/ns#");
        assert_eq!(
            r.resolve("toot:discoverable"),
            Some("http://joinmastodon.org/ns#discoverable".to_string())
        );
    }

    #[test]
    fn prefix_resolver_slash_terminated() {
        let mut r = PrefixResolver::new();
        r.add_prefix("ldp", "http://www.w3.org/ns/ldp#");
        assert_eq!(
            r.resolve("ldp:inbox"),
            Some("http://www.w3.org/ns/ldp#inbox".to_string())
        );
    }

    #[test]
    fn prefix_resolver_unknown_prefix() {
        let r = PrefixResolver::new();
        assert_eq!(r.resolve("unknown:term"), None);
    }

    #[test]
    fn prefix_resolver_no_colon() {
        let r = PrefixResolver::new();
        assert_eq!(r.resolve("noprefix"), None);
    }

    #[test]
    fn prefix_resolver_full_iri_passthrough() {
        let r = PrefixResolver::new();
        // Full IRIs contain "://" so split_once(':') returns ("https", "//...")
        // which won't match any prefix - correct behavior
        assert_eq!(r.resolve("https://example.com/term"), None);
    }

    #[test]
    fn prefix_resolver_multiple_prefixes() {
        let mut r = PrefixResolver::new();
        r.add_prefix("as", "https://www.w3.org/ns/activitystreams#");
        r.add_prefix("sec", "https://w3id.org/security#");
        r.add_prefix("toot", "http://joinmastodon.org/ns#");

        assert_eq!(
            r.resolve("as:Note"),
            Some("https://www.w3.org/ns/activitystreams#Note".to_string())
        );
        assert_eq!(
            r.resolve("sec:publicKey"),
            Some("https://w3id.org/security#publicKey".to_string())
        );
        assert_eq!(
            r.resolve("toot:featured"),
            Some("http://joinmastodon.org/ns#featured".to_string())
        );
    }

    #[test]
    fn prefix_resolver_overwrite() {
        let mut r = PrefixResolver::new();
        r.add_prefix("as", "https://old.example.com#");
        r.add_prefix("as", "https://www.w3.org/ns/activitystreams#");
        assert_eq!(
            r.resolve("as:Note"),
            Some("https://www.w3.org/ns/activitystreams#Note".to_string())
        );
    }

    // === ContextParser Tests ===

    // Helper to find a term by name in parsed results
    fn find_term<'a>(terms: &'a [TermDefinition], name: &str) -> Option<&'a TermDefinition> {
        terms.iter().find(|t| t.name == name)
    }

    // Form 1: Prefix declaration ("as": "https://...#")
    #[test]
    fn parse_prefix_declaration() {
        let json = r#"{"as": "https://www.w3.org/ns/activitystreams#"}"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "as").unwrap();
        match &term.kind {
            TermKind::Prefix { uri } => {
                assert_eq!(uri, "https://www.w3.org/ns/activitystreams#");
            }
            other => panic!("Expected Prefix, got {:?}", other),
        }
    }

    // Prefix with trailing slash
    #[test]
    fn parse_prefix_slash_terminated() {
        let json = r#"{"schema": "http://schema.org/"}"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "schema").unwrap();
        assert!(matches!(&term.kind, TermKind::Prefix { uri } if uri == "http://schema.org/"));
    }

    // Form 2: Keyword alias ("id": "@id")
    #[test]
    fn parse_keyword_alias() {
        let json = r#"{"id": "@id", "type": "@type"}"#;
        let result = ContextParser::parse(json).unwrap();

        let id_term = find_term(&result.terms, "id").unwrap();
        assert!(matches!(&id_term.kind, TermKind::KeywordAlias { keyword } if keyword == "@id"));

        let type_term = find_term(&result.terms, "type").unwrap();
        assert!(
            matches!(&type_term.kind, TermKind::KeywordAlias { keyword } if keyword == "@type")
        );
    }

    // Form 3: @vocab declaration - skipped (context-level directive, not a term)
    #[test]
    fn parse_vocab_declaration_skipped() {
        let json = r#"{"@vocab": "_:", "id": "@id"}"#;
        let result = ContextParser::parse(json).unwrap();
        // @vocab should not produce a TermDefinition (like @version)
        assert!(find_term(&result.terms, "@vocab").is_none());
        // But other terms should still be parsed
        assert!(find_term(&result.terms, "id").is_some());
    }

    // Form 4: Simple term → compact IRI (class-like: "Note": "as:Note")
    #[test]
    fn parse_simple_term_class() {
        let json = r#"{
            "as": "https://www.w3.org/ns/activitystreams#",
            "Note": "as:Note"
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "Note").unwrap();
        match &term.kind {
            TermKind::SimpleTerm { iri } => {
                assert_eq!(iri, "https://www.w3.org/ns/activitystreams#Note");
            }
            other => panic!("Expected SimpleTerm, got {:?}", other),
        }
    }

    // Form 5: Simple term → compact IRI (property-like: "content": "as:content")
    #[test]
    fn parse_simple_term_property() {
        let json = r#"{
            "as": "https://www.w3.org/ns/activitystreams#",
            "content": "as:content"
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "content").unwrap();
        match &term.kind {
            TermKind::SimpleTerm { iri } => {
                assert_eq!(iri, "https://www.w3.org/ns/activitystreams#content");
            }
            other => panic!("Expected SimpleTerm, got {:?}", other),
        }
    }

    // Form 6: @id + @type: "@id" (IRI reference property)
    #[test]
    fn parse_extended_term_type_id() {
        let json = r#"{
            "as": "https://www.w3.org/ns/activitystreams#",
            "actor": {"@id": "as:actor", "@type": "@id"}
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "actor").unwrap();
        match &term.kind {
            TermKind::ExtendedTerm {
                id,
                type_coercion,
                container,
            } => {
                assert_eq!(id, "https://www.w3.org/ns/activitystreams#actor");
                assert!(matches!(type_coercion, Some(TypeCoercion::Id)));
                assert!(container.is_none());
            }
            other => panic!("Expected ExtendedTerm, got {:?}", other),
        }
    }

    // Form 7: @id + @type: "xsd:dateTime"
    #[test]
    fn parse_extended_term_type_xsd() {
        let json = r#"{
            "as": "https://www.w3.org/ns/activitystreams#",
            "xsd": "http://www.w3.org/2001/XMLSchema#",
            "published": {"@id": "as:published", "@type": "xsd:dateTime"}
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "published").unwrap();
        match &term.kind {
            TermKind::ExtendedTerm {
                id, type_coercion, ..
            } => {
                assert_eq!(id, "https://www.w3.org/ns/activitystreams#published");
                match type_coercion {
                    Some(TypeCoercion::Xsd(xsd_type)) => {
                        assert_eq!(xsd_type, "http://www.w3.org/2001/XMLSchema#dateTime");
                    }
                    other => panic!("Expected Xsd, got {:?}", other),
                }
            }
            other => panic!("Expected ExtendedTerm, got {:?}", other),
        }
    }

    // Form 8: @container: "@language"
    #[test]
    fn parse_extended_term_container_language() {
        let json = r#"{
            "as": "https://www.w3.org/ns/activitystreams#",
            "contentMap": {"@id": "as:content", "@container": "@language"}
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "contentMap").unwrap();
        match &term.kind {
            TermKind::ExtendedTerm { container, .. } => {
                assert!(matches!(container, Some(Container::Language)));
            }
            other => panic!("Expected ExtendedTerm, got {:?}", other),
        }
    }

    // Form 9: @container: "@list"
    #[test]
    fn parse_extended_term_container_list() {
        let json = r#"{
            "as": "https://www.w3.org/ns/activitystreams#",
            "orderedItems": {"@id": "as:items", "@type": "@id", "@container": "@list"}
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "orderedItems").unwrap();
        match &term.kind {
            TermKind::ExtendedTerm {
                id,
                type_coercion,
                container,
            } => {
                assert_eq!(id, "https://www.w3.org/ns/activitystreams#items");
                assert!(matches!(type_coercion, Some(TypeCoercion::Id)));
                assert!(matches!(container, Some(Container::List)));
            }
            other => panic!("Expected ExtendedTerm, got {:?}", other),
        }
    }

    // Form 10: @container: "@set"
    #[test]
    fn parse_extended_term_container_set() {
        let json = r#"{
            "sec": "https://w3id.org/security#",
            "assertionMethod": {"@id": "sec:assertionMethod", "@container": "@set"}
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "assertionMethod").unwrap();
        match &term.kind {
            TermKind::ExtendedTerm { container, .. } => {
                assert!(matches!(container, Some(Container::Set)));
            }
            other => panic!("Expected ExtendedTerm, got {:?}", other),
        }
    }

    // Form 11: @container: "@graph"
    #[test]
    fn parse_extended_term_container_graph() {
        let json = r#"{
            "sec": "https://w3id.org/security#",
            "proof": {"@id": "sec:proof", "@container": "@graph"}
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "proof").unwrap();
        match &term.kind {
            TermKind::ExtendedTerm { container, .. } => {
                assert!(matches!(container, Some(Container::Graph)));
            }
            other => panic!("Expected ExtendedTerm, got {:?}", other),
        }
    }

    // Form 12: @type: "@vocab"
    #[test]
    fn parse_extended_term_type_vocab() {
        let json = r#"{
            "sec": "https://w3id.org/security#",
            "proofPurpose": {"@id": "sec:proofPurpose", "@type": "@vocab"}
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "proofPurpose").unwrap();
        match &term.kind {
            TermKind::ExtendedTerm { type_coercion, .. } => {
                assert!(matches!(type_coercion, Some(TypeCoercion::Vocab)));
            }
            other => panic!("Expected ExtendedTerm, got {:?}", other),
        }
    }

    // Form 13: @id only object form
    #[test]
    fn parse_extended_term_id_only() {
        let json = r#"{
            "sec": "https://w3id.org/security#",
            "kmsModule": {"@id": "sec:kmsModule"}
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "kmsModule").unwrap();
        match &term.kind {
            TermKind::ExtendedTerm {
                id,
                type_coercion,
                container,
            } => {
                assert_eq!(id, "https://w3id.org/security#kmsModule");
                assert!(type_coercion.is_none());
                assert!(container.is_none());
            }
            other => panic!("Expected ExtendedTerm, got {:?}", other),
        }
    }

    // Form 14: @version declaration - skipped (not a term)
    #[test]
    fn parse_version_declaration_skipped() {
        let json = r#"{"@version": 1.1, "id": "@id"}"#;
        let result = ContextParser::parse(json).unwrap();
        // @version should not produce a TermDefinition
        assert!(find_term(&result.terms, "@version").is_none());
        // But other terms should still be parsed
        assert!(find_term(&result.terms, "id").is_some());
    }

    // @context unwrapping
    #[test]
    fn parse_unwraps_context_key() {
        let json = r#"{"@context": {"id": "@id", "type": "@type"}}"#;
        let result = ContextParser::parse(json).unwrap();
        assert!(find_term(&result.terms, "id").is_some());
        assert!(find_term(&result.terms, "type").is_some());
    }

    // Parse error
    #[test]
    fn parse_invalid_json_returns_error() {
        let result = ContextParser::parse("not valid json");
        assert!(result.is_err());
    }

    // Cross-namespace reference: term name ≠ IRI local name
    #[test]
    fn parse_cross_namespace_term() {
        let json = r#"{
            "sec": "https://w3id.org/security#",
            "CryptographicKey": "sec:Key"
        }"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "CryptographicKey").unwrap();
        match &term.kind {
            TermKind::SimpleTerm { iri } => {
                assert_eq!(iri, "https://w3id.org/security#Key");
            }
            other => panic!("Expected SimpleTerm, got {:?}", other),
        }
    }

    // Simple term with full IRI (no prefix expansion needed)
    #[test]
    fn parse_simple_term_full_iri() {
        let json = r#"{"manuallyApprovesFollowers": "http://joinmastodon.org/ns#manuallyApprovesFollowers"}"#;
        let result = ContextParser::parse(json).unwrap();
        let term = find_term(&result.terms, "manuallyApprovesFollowers").unwrap();
        match &term.kind {
            TermKind::SimpleTerm { iri } => {
                assert_eq!(iri, "http://joinmastodon.org/ns#manuallyApprovesFollowers");
            }
            other => panic!("Expected SimpleTerm, got {:?}", other),
        }
    }

    // original_json is preserved
    #[test]
    fn parse_preserves_original_json() {
        let json = r#"{"id": "@id"}"#;
        let result = ContextParser::parse(json).unwrap();
        assert_eq!(result.original_json, serde_json::json!({"id": "@id"}));
    }
}
