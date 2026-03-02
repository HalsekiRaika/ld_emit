use std::ffi::OsStr;
use std::fs;
use std::path::PathBuf;

use crate::code_generator::CodeGenerator;
use crate::context_fetcher::ContextFetcher;
use crate::context_parser::ContextParser;
use crate::error::LDBuildError;
use crate::models::*;

pub struct ContextBuilder {
    out_dir: PathBuf,
    contexts: Vec<(String, ContextSource)>,
    expose_values: Vec<ExposeValueDirective>,
    renames: Vec<RenameDirective>,
    serializer_name: Option<String>,
    export_dir: Option<PathBuf>,
}

impl ContextBuilder {
    pub fn new(out_dir: &OsStr) -> Self {
        ContextBuilder {
            out_dir: PathBuf::from(out_dir),
            contexts: Vec::new(),
            expose_values: Vec::new(),
            renames: Vec::new(),
            serializer_name: None,
            export_dir: None,
        }
    }

    pub fn add_context(&mut self, name: &str, source: ContextSource) {
        self.contexts.push((name.to_string(), source));
    }

    pub fn add_expose_value(&mut self, expanded_iri: &str) {
        self.expose_values.push(ExposeValueDirective {
            expanded_iri: expanded_iri.to_string(),
        });
    }

    /// Set the `@serializer_name` for the generated output file.
    ///
    /// This determines the output filename: `{serializer_name}.rs` in `OUT_DIR`.
    /// Required — `generate()` will return an error if this is not set.
    pub fn set_serializer_name(&mut self, name: impl Into<String>) -> &mut Self {
        self.serializer_name = Some(name.into());
        self
    }

    /// Set the `@export_dir` directory for additional output relative to `CARGO_MANIFEST_DIR`.
    ///
    /// When set, the generated code will also be written to
    /// `{CARGO_MANIFEST_DIR}/{export_dir}/{serializer_name}.rs`, enabling JetBrains IDE
    /// completion via `#[path]` or standard `include!`.
    pub fn set_export_dir(&mut self, path: impl Into<PathBuf>) -> &mut Self {
        self.export_dir = Some(path.into());
        self
    }

    /// Add a @rename directive to map a JSON-LD term name to an alternative Rust method name.
    pub fn add_rename(&mut self, from: impl Into<String>, to: impl Into<String>) -> &mut Self {
        self.renames.push(RenameDirective {
            from: from.into(),
            to: to.into(),
        });
        self
    }

    pub fn generate(&self) -> Result<(), LDBuildError> {
        let serializer_name =
            self.serializer_name
                .as_ref()
                .ok_or_else(|| LDBuildError::CodeGen {
                    term: "ContextBuilder".to_string(),
                    message: "@serializer_name is required but was not set".to_string(),
                })?;

        let cache_dir = self.out_dir.join(".ld_emit_cache");
        let fetcher = ContextFetcher::new(cache_dir);

        // Parse all contexts
        let mut parsed_contexts: Vec<(String, ParsedContext)> = Vec::new();
        for (name, source) in &self.contexts {
            let parsed = match source {
                ContextSource::Url(url) => {
                    let json = fetcher.fetch(url)?;
                    let mut parsed = ContextParser::parse(&json)?;
                    // @context にはURL文字列のみを出力する（フェッチ内容は展開しない）
                    parsed.original_json = serde_json::Value::String(url.clone());
                    parsed
                }
                ContextSource::Inherit { url, overrides } => {
                    // Fetch the remote context
                    let json = fetcher.fetch(url)?;
                    let fetched_value: serde_json::Value =
                        serde_json::from_str(&json).map_err(|e| LDBuildError::Parse {
                            context: name.clone(),
                            message: format!("Failed to parse fetched context: {}", e),
                            position: None,
                        })?;

                    // Unwrap @context key and merge overrides
                    let context_value = Self::unwrap_context_value(&fetched_value);
                    let merged = Self::merge_context_json(&context_value, overrides);

                    // Parse the merged JSON
                    let merged_str =
                        serde_json::to_string(&merged).map_err(|e| LDBuildError::Parse {
                            context: name.clone(),
                            message: format!("Failed to serialize merged context: {}", e),
                            position: None,
                        })?;
                    let mut parsed = ContextParser::parse(&merged_str)?;

                    // Override original_json:
                    // - If overrides is empty object, use just the URL string
                    // - Otherwise, use [url, overrides] array format
                    let is_empty = overrides
                        .as_object()
                        .map(|o| o.is_empty())
                        .unwrap_or(false);
                    if is_empty {
                        parsed.original_json = serde_json::Value::String(url.clone());
                    } else {
                        parsed.original_json = serde_json::json!([url, overrides]);
                    }

                    parsed
                }
                ContextSource::WithAlias { uri, alias, terms } => {
                    // Build JSON: {alias: uri, ...terms} — no remote fetch
                    let constructed = Self::build_with_alias_json(uri, alias, terms);

                    let json_str =
                        serde_json::to_string(&constructed).map_err(|e| LDBuildError::Parse {
                            context: name.clone(),
                            message: format!("Failed to serialize WithAlias context: {}", e),
                            position: None,
                        })?;
                    ContextParser::parse(&json_str)?
                }
            };
            parsed_contexts.push((name.clone(), parsed));
        }

        // Generate code as TokenStream
        let tokens = CodeGenerator::generate(&parsed_contexts, &self.expose_values, &self.renames)?;

        // Format with prettyplease
        let syntax_tree: syn::File = syn::parse2(tokens).map_err(|e| LDBuildError::CodeGen {
            term: "generated code".to_string(),
            message: format!("Generated code is not valid Rust: {}", e),
        })?;
        let formatted = prettyplease::unparse(&syntax_tree);

        // Write to OUT_DIR using serializer_name
        let output_filename = format!("{}.rs", serializer_name);
        let output_path = self.out_dir.join(&output_filename);
        fs::write(&output_path, &formatted)?;

        // Emit cargo rerun-if-changed for the generated file
        println!("cargo::rerun-if-changed={}", output_path.display());

        // Write to export_dir path (relative to CARGO_MANIFEST_DIR) if set
        if let Some(ref export_dir) = self.export_dir {
            let manifest_dir = PathBuf::from(
                std::env::var_os("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set"),
            );
            let include_path = manifest_dir.join(export_dir).join(&output_filename);
            if let Some(parent) = include_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&include_path, &formatted)?;
        }

        Ok(())
    }

    /// Unwraps the `@context` key from a JSON-LD document.
    /// If the value has an `@context` key, returns its value.
    /// Otherwise, returns the value itself.
    fn unwrap_context_value(value: &serde_json::Value) -> serde_json::Value {
        value
            .as_object()
            .and_then(|o| o.get("@context"))
            .cloned()
            .unwrap_or_else(|| value.clone())
    }

    /// Merges override entries into a fetched context JSON.
    /// - If fetched is an Object: shallow merge (overrides keys overwrite)
    /// - If fetched is an Array: append overrides as a new Object element
    /// - If overrides is empty, returns fetched unchanged
    fn merge_context_json(
        fetched_context: &serde_json::Value,
        overrides: &serde_json::Value,
    ) -> serde_json::Value {
        let override_obj = match overrides.as_object() {
            Some(o) if o.is_empty() => return fetched_context.clone(),
            Some(o) => o,
            None => return fetched_context.clone(),
        };

        match fetched_context {
            serde_json::Value::Object(fetched_obj) => {
                let mut merged = fetched_obj.clone();
                for (key, val) in override_obj {
                    merged.insert(key.clone(), val.clone());
                }
                serde_json::Value::Object(merged)
            }
            serde_json::Value::Array(arr) => {
                let mut new_arr = arr.clone();
                new_arr.push(serde_json::Value::Object(override_obj.clone()));
                serde_json::Value::Array(new_arr)
            }
            _ => fetched_context.clone(),
        }
    }

    /// Builds a JSON object for WithAlias mode: `{alias: uri, ...terms}`
    fn build_with_alias_json(
        uri: &str,
        alias: &str,
        terms: &serde_json::Value,
    ) -> serde_json::Value {
        // Namespace URI must end with a separator for correct compact IRI expansion.
        // Auto-append '#' if missing (e.g., "https://example.org/ns" → "https://example.org/ns#").
        let normalized_uri = if uri.ends_with('#') || uri.ends_with('/') {
            uri.to_string()
        } else {
            format!("{}#", uri)
        };
        let mut map = serde_json::Map::new();
        map.insert(alias.to_string(), serde_json::Value::String(normalized_uri));
        if let Some(term_obj) = terms.as_object() {
            for (key, val) in term_obj {
                map.insert(key.clone(), val.clone());
            }
        }
        serde_json::Value::Object(map)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsString;

    #[test]
    fn context_builder_new() {
        let dir = OsString::from("/tmp/test_out");
        let builder = ContextBuilder::new(&dir);
        assert!(builder.contexts.is_empty());
        assert!(builder.expose_values.is_empty());
    }

    #[test]
    fn context_builder_add_context_url() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        builder.add_context(
            "activity_streams",
            ContextSource::Url("https://www.w3.org/ns/activitystreams".to_string()),
        );
        assert_eq!(builder.contexts.len(), 1);
        assert_eq!(builder.contexts[0].0, "activity_streams");
        assert!(
            matches!(&builder.contexts[0].1, ContextSource::Url(u) if u == "https://www.w3.org/ns/activitystreams")
        );
    }

    #[test]
    fn context_builder_add_context_with_alias() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        builder.add_context(
            "toot_ext",
            ContextSource::WithAlias {
                uri: "http://joinmastodon.org/ns#".to_string(),
                alias: "toot".to_string(),
                terms: serde_json::json!({
                    "discoverable": "toot:discoverable"
                }),
            },
        );
        assert_eq!(builder.contexts.len(), 1);
        assert_eq!(builder.contexts[0].0, "toot_ext");
        assert!(matches!(&builder.contexts[0].1, ContextSource::WithAlias { .. }));
    }

    #[test]
    fn context_builder_add_expose_value() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        builder.add_expose_value("https://www.w3.org/ns/activitystreams#content");
        assert_eq!(builder.expose_values.len(), 1);
        assert_eq!(
            builder.expose_values[0].expanded_iri,
            "https://www.w3.org/ns/activitystreams#content"
        );
    }

    #[test]
    fn context_builder_add_multiple_contexts() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        builder.add_context(
            "activity_streams",
            ContextSource::Url("https://www.w3.org/ns/activitystreams".to_string()),
        );
        builder.add_context(
            "security_v1",
            ContextSource::Url("https://w3id.org/security/v1".to_string()),
        );
        builder.add_context(
            "toot_ext",
            ContextSource::WithAlias {
                uri: "http://joinmastodon.org/ns#".to_string(),
                alias: "toot".to_string(),
                terms: serde_json::json!({
                    "discoverable": "toot:discoverable"
                }),
            },
        );
        assert_eq!(builder.contexts.len(), 3);
        // Order is preserved
        assert_eq!(builder.contexts[0].0, "activity_streams");
        assert_eq!(builder.contexts[1].0, "security_v1");
        assert_eq!(builder.contexts[2].0, "toot_ext");
    }

    #[test]
    fn context_builder_generate_inline_writes_file() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_generate");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                alias: "as".to_string(),
                terms: serde_json::json!({
                    "id": "@id",
                    "Note": "as:Note"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let output_path = tmp_dir.join("test_output.rs");
        assert!(output_path.exists());
        let content = fs::read_to_string(&output_path).unwrap();
        assert!(content.contains("pub struct TestCtx"));
        assert!(content.contains("pub trait HasTestCtx"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn context_builder_generate_preserves_context_order() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_order");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "first",
            ContextSource::WithAlias {
                uri: "https://example.com/first#".to_string(),
                alias: "ex1".to_string(),
                terms: serde_json::json!({"id": "@id"}),
            },
        );
        builder.add_context(
            "second",
            ContextSource::WithAlias {
                uri: "https://example.com/second#".to_string(),
                alias: "ex2".to_string(),
                terms: serde_json::json!({"value": "@value"}),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        // Type alias should be First<Second>
        assert!(content.contains("pub type Context = First<Second>;"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    // === 6.2: Integration tests ===

    #[test]
    fn integration_multi_context_end_to_end() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_integration_multi");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "activity_streams",
            ContextSource::WithAlias {
                uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                alias: "as".to_string(),
                terms: serde_json::json!({
                    "id": "@id",
                    "Note": "as:Note",
                    "Person": "as:Person",
                    "name": {"@id": "as:name"},
                    "actor": {"@id": "as:actor", "@type": "@id"},
                    "content": "as:content"
                }),
            },
        );
        builder.add_context(
            "security_v1",
            ContextSource::WithAlias {
                uri: "https://w3id.org/security#".to_string(),
                alias: "sec".to_string(),
                terms: serde_json::json!({
                    "CryptographicKey": "sec:Key",
                    "publicKey": {"@id": "sec:publicKey", "@type": "@id"}
                }),
            },
        );
        builder.add_context(
            "toot_ext",
            ContextSource::WithAlias {
                uri: "http://joinmastodon.org/ns#".to_string(),
                alias: "toot".to_string(),
                terms: serde_json::json!({
                    "discoverable": "toot:discoverable"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();

        // Syntax validation with syn
        syn::parse_file(&content).expect("Multi-context generated code should be valid Rust");

        // 3-context composed type alias
        assert!(content.contains("pub type Context = ActivityStreams<SecurityV1<TootExt>>;"));

        // All context types present
        assert!(content.contains("pub struct ActivityStreams<S = ()>"));
        assert!(content.contains("pub struct SecurityV1<S = ()>"));
        assert!(content.contains("pub struct TootExt<S = ()>"));

        // All marker traits present
        assert!(content.contains("pub trait HasActivityStreams {}"));
        assert!(content.contains("pub trait HasSecurityV1 {}"));
        assert!(content.contains("pub trait HasTootExt {}"));

        // Cross-forwarding exists
        assert!(content.contains("HasSecurityV1 for ActivityStreams<S>"));
        assert!(content.contains("HasTootExt for ActivityStreams<S>"));
        assert!(content.contains("HasActivityStreams for SecurityV1<S>"));

        // All extension traits present
        assert!(content.contains("pub trait ActivityStreamsExt {"));
        assert!(content.contains("pub trait SecurityV1Ext {"));
        assert!(content.contains("pub trait TootExtExt {"));

        // All modules present
        assert!(content.contains("pub mod activity_streams {"));
        assert!(content.contains("pub mod security_v1 {"));
        assert!(content.contains("pub mod toot_ext {"));

        // Const type constants generated for SimpleTerms
        assert!(content.contains("pub const NOTE"));
        assert!(content.contains("pub const PERSON"));
        assert!(content.contains("pub const CRYPTOGRAPHIC_KEY"));
        assert!(content.contains("pub const DISCOVERABLE"));

        // ContextSerializer impls
        assert!(content.contains("for ActivityStreams<S>"));
        assert!(content.contains("for SecurityV1<S>"));
        assert!(content.contains("for TootExt<S>"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_expose_value_end_to_end() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_integration_expose");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                alias: "as".to_string(),
                terms: serde_json::json!({
                    "content": "as:content",
                    "name": {"@id": "as:name"}
                }),
            },
        );
        builder.add_expose_value("https://www.w3.org/ns/activitystreams#content");
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();

        // Syntax validation
        syn::parse_file(&content).expect("Code with @expose_value should be valid Rust");

        // _with method generated for exposed SimpleTerm
        assert!(content.contains("content_with"));

        // Regular ExtendedTerm setter also present
        assert!(content.contains("fn name("));

        // Const type for the SimpleTerm
        assert!(content.contains("pub const CONTENT"));
        assert!(content.contains("ld_emit::TypeConstant"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_expose_value_mismatch_returns_error() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_integration_expose_err");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                alias: "as".to_string(),
                terms: serde_json::json!({
                    "Note": "as:Note"
                }),
            },
        );
        builder.add_expose_value("https://nonexistent.example.com#term");
        builder.set_serializer_name("test_output");

        let result = builder.generate();
        assert!(result.is_err());

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_keyword_alias_methods() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_integration_keywords");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://example.com/ns#".to_string(),
                alias: "ex".to_string(),
                terms: serde_json::json!({
                    "id": "@id"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();

        // Syntax validation
        syn::parse_file(&content).expect("Code with keyword aliases should be valid Rust");

        // Keyword alias methods with correct JSON keys
        assert!(content.contains("fn id("));
        assert!(content.contains(r#""@id""#));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_keyword_alias_reserved_word_auto_resolves() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_integration_reserved");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://example.com/ns#".to_string(),
                alias: "ex".to_string(),
                terms: serde_json::json!({
                    "type": "@type"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        let result = builder.generate();
        assert!(
            result.is_ok(),
            "KeywordAlias 'type' should auto-resolve without @rename"
        );
        let output_path = tmp_dir.join("test_output.rs");
        let code = fs::read_to_string(&output_path).unwrap();
        assert!(
            code.contains("r#type"),
            "Generated code should contain r#type. Code:\n{}",
            code
        );

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_extended_term_reserved_word_without_rename_returns_error() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_integration_reserved_ext");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://example.com/ns#".to_string(),
                alias: "ex".to_string(),
                terms: serde_json::json!({
                    "fn": {"@id": "https://example.com#fn"}
                }),
            },
        );
        builder.set_serializer_name("test_output");
        let result = builder.generate();
        assert!(
            result.is_err(),
            "ExtendedTerm 'fn' without @rename should error"
        );

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_nested_object_methods_for_id_type() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_integration_nested");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://w3id.org/security#".to_string(),
                alias: "sec".to_string(),
                terms: serde_json::json!({
                    "publicKey": {"@id": "sec:publicKey", "@type": "@id"},
                    "owner": {"@id": "sec:owner", "@type": "@id"}
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();

        // Syntax validation
        syn::parse_file(&content).expect("Code with nested object methods should be valid Rust");

        // Both value setter and nested object setter for @type: "@id" terms
        assert!(content.contains("fn public_key("));
        assert!(content.contains("fn public_key_object"));
        assert!(content.contains("fn owner("));
        assert!(content.contains("fn owner_object"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    // === 6.2 specific: prettyplease formatting and pipeline verification ===

    #[test]
    fn integration_output_is_prettyplease_formatted() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_prettyplease_format");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                alias: "as".to_string(),
                terms: serde_json::json!({
                    "id": "@id",
                    "Note": "as:Note",
                    "name": {"@id": "as:name"}
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();

        // Re-parse and re-format: prettyplease output should be idempotent
        let syntax_tree = syn::parse_file(&content).expect("Generated code should be valid Rust");
        let reformatted = prettyplease::unparse(&syntax_tree);

        assert_eq!(
            content, reformatted,
            "Output should be idempotent after prettyplease formatting"
        );

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_output_has_readable_formatting() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_readable_format");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://www.w3.org/ns/activitystreams#".to_string(),
                alias: "as".to_string(),
                terms: serde_json::json!({
                    "id": "@id",
                    "Note": "as:Note"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();

        // prettyplease uses 4-space indentation
        assert!(
            content.contains("    pub"),
            "Output should have 4-space indented pub items (prettyplease style)"
        );

        // Should not contain excessive blank lines or unformatted token runs
        assert!(
            !content.contains("  \n  \n  \n"),
            "Output should not contain excessive blank lines"
        );
    }

    #[test]
    fn integration_rerun_if_changed_directive() {
        // Verify that the generate method produces cargo rerun-if-changed
        // by checking the output file path is correct
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_rerun");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://example.com/ns#".to_string(),
                alias: "ex".to_string(),
                terms: serde_json::json!({"id": "@id"}),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        // The output file should exist at the expected path
        let output_path = tmp_dir.join("test_output.rs");
        assert!(
            output_path.exists(),
            "Generated file should exist at OUT_DIR/test_output.rs"
        );

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn generate_without_serializer_name_returns_error() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_no_serializer_name");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::WithAlias {
                uri: "https://example.com/ns#".to_string(),
                alias: "ex".to_string(),
                terms: serde_json::json!({"id": "@id"}),
            },
        );
        // Do NOT set serializer_name
        let result = builder.generate();
        assert!(result.is_err(), "@serializer_name is required");

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    // === Task 3.1: Inherit mode helper tests ===

    #[test]
    fn unwrap_context_value_with_context_key() {
        let value = serde_json::json!({
            "@context": {
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note"
            }
        });
        let result = ContextBuilder::unwrap_context_value(&value);
        assert!(result.is_object());
        assert_eq!(result["as"], "https://www.w3.org/ns/activitystreams#");
        assert_eq!(result["Note"], "as:Note");
    }

    #[test]
    fn unwrap_context_value_without_context_key() {
        let value = serde_json::json!({
            "as": "https://www.w3.org/ns/activitystreams#",
            "Note": "as:Note"
        });
        let result = ContextBuilder::unwrap_context_value(&value);
        assert!(result.is_object());
        assert_eq!(result["as"], "https://www.w3.org/ns/activitystreams#");
    }

    #[test]
    fn merge_context_json_object_merge() {
        let fetched = serde_json::json!({
            "as": "https://www.w3.org/ns/activitystreams#",
            "Note": "as:Note"
        });
        let overrides = serde_json::json!({
            "manuallyApprovesFollowers": "as:manuallyApprovesFollowers"
        });
        let result = ContextBuilder::merge_context_json(&fetched, &overrides);
        assert!(result.is_object());
        let obj = result.as_object().unwrap();
        assert!(obj.contains_key("as"));
        assert!(obj.contains_key("Note"));
        assert!(obj.contains_key("manuallyApprovesFollowers"));
    }

    #[test]
    fn merge_context_json_override_existing_key() {
        let fetched = serde_json::json!({
            "as": "https://www.w3.org/ns/activitystreams#",
            "Note": "as:Note"
        });
        let overrides = serde_json::json!({
            "Note": "as:CustomNote"
        });
        let result = ContextBuilder::merge_context_json(&fetched, &overrides);
        assert_eq!(result["Note"], "as:CustomNote");
    }

    #[test]
    fn merge_context_json_array_appends_overrides() {
        let fetched = serde_json::json!([
            "https://www.w3.org/ns/activitystreams",
            {"as": "https://www.w3.org/ns/activitystreams#"}
        ]);
        let overrides = serde_json::json!({
            "custom": "as:custom"
        });
        let result = ContextBuilder::merge_context_json(&fetched, &overrides);
        assert!(result.is_array());
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 3);
        assert_eq!(arr[0], "https://www.w3.org/ns/activitystreams");
        assert!(arr[2].is_object());
        assert_eq!(arr[2]["custom"], "as:custom");
    }

    #[test]
    fn merge_context_json_empty_overrides() {
        let fetched = serde_json::json!({
            "as": "https://www.w3.org/ns/activitystreams#",
            "Note": "as:Note"
        });
        let overrides = serde_json::json!({});
        let result = ContextBuilder::merge_context_json(&fetched, &overrides);
        assert_eq!(result, fetched);
    }

    fn make_cache_file(tmp_dir: &std::path::Path, url: &str, content: &serde_json::Value) {
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;

        let cache_dir = tmp_dir.join(".ld_emit_cache");
        fs::create_dir_all(&cache_dir).unwrap();

        let mut hasher = DefaultHasher::new();
        url.hash(&mut hasher);
        let hash = hasher.finish();
        let cache_file = cache_dir.join(format!("{:016x}.json", hash));
        fs::write(&cache_file, serde_json::to_string(content).unwrap()).unwrap();
    }

    #[test]
    fn inherit_mode_original_json_is_array_when_overrides_present() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_inherit_original_json");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let url = "https://example.com/test-context";
        let context_json = serde_json::json!({
            "@context": {
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note",
                "Person": "as:Person"
            }
        });
        make_cache_file(&tmp_dir, url, &context_json);

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::Inherit {
                url: url.to_string(),
                overrides: serde_json::json!({
                    "custom": "as:custom"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        syn::parse_file(&content).expect("Inherit mode generated code should be valid Rust");

        // Should have the context types
        assert!(content.contains("pub struct TestCtx"));
        assert!(content.contains("pub trait HasTestCtx"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn inherit_mode_empty_overrides_produces_url_string() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_inherit_empty_overrides");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let url = "https://example.com/test-context2";
        let context_json = serde_json::json!({
            "@context": {
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note"
            }
        });
        make_cache_file(&tmp_dir, url, &context_json);

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::Inherit {
                url: url.to_string(),
                overrides: serde_json::json!({}),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        syn::parse_file(&content).expect("Inherit mode with empty overrides should be valid Rust");

        // With empty overrides, original_json should be just the URL string
        assert!(
            content.contains("https://example.com/test-context2"),
            "Empty overrides inherit should include URL. Code:\n{}",
            content
        );

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    // === Task 3.2: WithAlias mode tests ===

    #[test]
    fn build_with_alias_json_creates_correct_structure() {
        let terms = serde_json::json!({
            "discoverable": "toot:discoverable",
            "featured": {"@id": "toot:featured", "@type": "@id"}
        });
        let result = ContextBuilder::build_with_alias_json(
            "http://joinmastodon.org/ns#",
            "toot",
            &terms,
        );
        assert!(result.is_object());
        let obj = result.as_object().unwrap();
        assert_eq!(obj["toot"], "http://joinmastodon.org/ns#");
        assert_eq!(obj["discoverable"], "toot:discoverable");
        assert!(obj["featured"].is_object());
    }

    #[test]
    fn build_with_alias_json_normalizes_uri_without_fragment() {
        let terms = serde_json::json!({"Note": "as:Note"});
        let result = ContextBuilder::build_with_alias_json(
            "https://www.w3.org/ns/activitystreams",
            "as",
            &terms,
        );
        let obj = result.as_object().unwrap();
        // URI should have # appended automatically
        assert_eq!(obj["as"], "https://www.w3.org/ns/activitystreams#");
        assert_eq!(obj["Note"], "as:Note");
    }

    #[test]
    fn build_with_alias_json_preserves_uri_with_slash() {
        let terms = serde_json::json!({"name": "schema:name"});
        let result = ContextBuilder::build_with_alias_json(
            "https://schema.org/",
            "schema",
            &terms,
        );
        let obj = result.as_object().unwrap();
        assert_eq!(obj["schema"], "https://schema.org/");
    }

    #[test]
    fn with_alias_mode_generates_valid_code() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_with_alias");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "toot_ext",
            ContextSource::WithAlias {
                uri: "http://joinmastodon.org/ns#".to_string(),
                alias: "toot".to_string(),
                terms: serde_json::json!({
                    "discoverable": "toot:discoverable"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        syn::parse_file(&content).expect("WithAlias mode generated code should be valid Rust");

        assert!(content.contains("pub struct TootExt"));
        assert!(content.contains("pub trait HasTootExt"));
        assert!(content.contains("pub const DISCOVERABLE"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn with_alias_mode_name_differs_from_alias() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_with_alias_diff_name");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "mastodon_ext",
            ContextSource::WithAlias {
                uri: "http://joinmastodon.org/ns#".to_string(),
                alias: "toot".to_string(),
                terms: serde_json::json!({
                    "discoverable": "toot:discoverable"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        syn::parse_file(&content).expect("WithAlias with different name should be valid Rust");

        // Module name comes from context name, not alias
        assert!(content.contains("pub struct MastodonExt"));
        assert!(content.contains("pub trait HasMastodonExt"));
        // But JSON-LD prefix comes from alias
        assert!(content.contains("http://joinmastodon.org/ns#"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn with_alias_mode_same_name_and_alias() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_with_alias_same");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "toot",
            ContextSource::WithAlias {
                uri: "http://joinmastodon.org/ns#".to_string(),
                alias: "toot".to_string(),
                terms: serde_json::json!({
                    "discoverable": "toot:discoverable"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        syn::parse_file(&content).expect("WithAlias with same name and alias should be valid Rust");

        assert!(content.contains("pub struct Toot"));
        assert!(content.contains("pub trait HasToot"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    // === Task 4: Macro new syntax pattern tests ===

    #[test]
    fn macro_fetch_only_syntax() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        // Simulate: activity_streams = "https://www.w3.org/ns/activitystreams"
        crate::__process_context_items!(builder,
            activity_streams = "https://www.w3.org/ns/activitystreams"
        );
        assert_eq!(builder.contexts.len(), 1);
        assert_eq!(builder.contexts[0].0, "activity_streams");
        assert!(matches!(
            &builder.contexts[0].1,
            ContextSource::Url(u) if u == "https://www.w3.org/ns/activitystreams"
        ));
    }

    #[test]
    fn macro_inherit_syntax() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        // Simulate: activity_streams = "https://www.w3.org/ns/activitystreams" inherit: { "custom": "as:custom" }
        crate::__process_context_items!(builder,
            activity_streams = "https://www.w3.org/ns/activitystreams" inherit: {
                "custom": "as:custom"
            }
        );
        assert_eq!(builder.contexts.len(), 1);
        assert_eq!(builder.contexts[0].0, "activity_streams");
        assert!(matches!(&builder.contexts[0].1, ContextSource::Inherit { .. }));
        if let ContextSource::Inherit { url, overrides } = &builder.contexts[0].1 {
            assert_eq!(url, "https://www.w3.org/ns/activitystreams");
            assert!(overrides.is_object());
            assert_eq!(overrides["custom"], "as:custom");
        }
    }

    #[test]
    fn macro_with_alias_syntax() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        // Simulate: toot_ext = "http://joinmastodon.org/ns#" with toot: { "discoverable": "toot:discoverable" }
        crate::__process_context_items!(builder,
            toot_ext = "http://joinmastodon.org/ns#" with toot: {
                "discoverable": "toot:discoverable"
            }
        );
        assert_eq!(builder.contexts.len(), 1);
        assert_eq!(builder.contexts[0].0, "toot_ext");
        assert!(matches!(&builder.contexts[0].1, ContextSource::WithAlias { .. }));
        if let ContextSource::WithAlias { uri, alias, terms } = &builder.contexts[0].1 {
            assert_eq!(uri, "http://joinmastodon.org/ns#");
            assert_eq!(alias, "toot");
            assert!(terms.is_object());
            assert_eq!(terms["discoverable"], "toot:discoverable");
        }
    }

    #[test]
    fn macro_mixed_syntax_with_directives() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        crate::__process_context_items!(builder,
            @serializer_name "test_output",
            activity_streams = "https://www.w3.org/ns/activitystreams",
            toot_ext = "http://joinmastodon.org/ns#" with toot: {
                "discoverable": "toot:discoverable"
            },
            @expose_value {
                "https://www.w3.org/ns/activitystreams#content",
            }
        );
        assert_eq!(builder.contexts.len(), 2);
        assert_eq!(builder.contexts[0].0, "activity_streams");
        assert!(matches!(&builder.contexts[0].1, ContextSource::Url(_)));
        assert_eq!(builder.contexts[1].0, "toot_ext");
        assert!(matches!(&builder.contexts[1].1, ContextSource::WithAlias { .. }));
        assert_eq!(builder.expose_values.len(), 1);
    }

    #[test]
    fn macro_inherit_with_trailing_comma() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        crate::__process_context_items!(builder,
            ctx1 = "https://example.com/ctx1" inherit: {
                "a": "b"
            },
            ctx2 = "https://example.com/ctx2"
        );
        assert_eq!(builder.contexts.len(), 2);
        assert!(matches!(&builder.contexts[0].1, ContextSource::Inherit { .. }));
        assert!(matches!(&builder.contexts[1].1, ContextSource::Url(_)));
    }

    #[test]
    fn macro_with_alias_trailing_comma() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        crate::__process_context_items!(builder,
            ctx1 = "http://example.com/ns#" with ex: {
                "term": "ex:term"
            },
            ctx2 = "https://example.com/ctx2"
        );
        assert_eq!(builder.contexts.len(), 2);
        assert!(matches!(&builder.contexts[0].1, ContextSource::WithAlias { .. }));
        assert!(matches!(&builder.contexts[1].1, ContextSource::Url(_)));
    }

    // === Task 5.2: Integration tests ===

    #[test]
    fn integration_inherit_e2e_pipeline() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_inherit_e2e");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let url = "https://example.com/as-context";
        let context_json = serde_json::json!({
            "@context": {
                "as": "https://www.w3.org/ns/activitystreams#",
                "id": "@id",
                "type": "@type",
                "Note": "as:Note",
                "Person": "as:Person",
                "name": {"@id": "as:name"},
                "actor": {"@id": "as:actor", "@type": "@id"},
                "content": "as:content"
            }
        });
        make_cache_file(&tmp_dir, url, &context_json);

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "activity_streams",
            ContextSource::Inherit {
                url: url.to_string(),
                overrides: serde_json::json!({
                    "manuallyApprovesFollowers": "as:manuallyApprovesFollowers"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        syn::parse_file(&content).expect("Inherit E2E should generate valid Rust");

        // Correct struct/trait generation
        assert!(content.contains("pub struct ActivityStreams<S = ()>"));
        assert!(content.contains("pub trait HasActivityStreams {}"));
        assert!(content.contains("pub trait ActivityStreamsExt {"));

        // Terms from fetched context
        assert!(content.contains("pub const NOTE"));
        assert!(content.contains("pub const PERSON"));
        assert!(content.contains("fn name("));
        assert!(content.contains("fn actor("));

        // Custom term from overrides merged into terms
        assert!(content.contains("pub const MANUALLY_APPROVES_FOLLOWERS"));

        // ContextSerializer impl present
        assert!(
            content.contains("for ActivityStreams<S>"),
            "Should contain ContextSerializer impl for ActivityStreams"
        );

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_with_e2e_pipeline() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_with_e2e");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "toot_ext",
            ContextSource::WithAlias {
                uri: "http://joinmastodon.org/ns#".to_string(),
                alias: "toot".to_string(),
                terms: serde_json::json!({
                    "discoverable": "toot:discoverable",
                    "featured": {"@id": "toot:featured", "@type": "@id"}
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        syn::parse_file(&content).expect("WithAlias E2E should generate valid Rust");

        // Struct/trait
        assert!(content.contains("pub struct TootExt<S = ()>"));
        assert!(content.contains("pub trait HasTootExt {}"));

        // SimpleTerm from compact IRI
        assert!(content.contains("pub const DISCOVERABLE"));

        // ExtendedTerm with @type: @id generates both setter and _object variant
        assert!(content.contains("fn featured("));
        assert!(content.contains("fn featured_object"));

        // original_json includes the prefix in ContextSerializer
        assert!(content.contains("http://joinmastodon.org/ns#"));

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_inherit_empty_block_equals_fetch_only() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_inherit_empty_eq_fetch");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let url = "https://example.com/empty-inherit-ctx";
        let context_json = serde_json::json!({
            "@context": {
                "ex": "https://example.com/ns#",
                "Term": "ex:Term"
            }
        });
        make_cache_file(&tmp_dir, url, &context_json);

        // inherit: {} (empty)
        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::Inherit {
                url: url.to_string(),
                overrides: serde_json::json!({}),
            },
        );
        builder.set_serializer_name("test_inherit_empty");
        builder.generate().unwrap();
        let inherit_content = fs::read_to_string(tmp_dir.join("test_inherit_empty.rs")).unwrap();

        // Url (fetch-only)
        let mut builder2 = ContextBuilder::new(tmp_dir.as_os_str());
        builder2.add_context(
            "test_ctx",
            ContextSource::Url(url.to_string()),
        );
        builder2.set_serializer_name("test_fetch_only");
        builder2.generate().unwrap();
        let fetch_content = fs::read_to_string(tmp_dir.join("test_fetch_only.rs")).unwrap();

        // Both should generate the same struct/trait structure
        assert!(inherit_content.contains("pub struct TestCtx"));
        assert!(fetch_content.contains("pub struct TestCtx"));
        assert!(inherit_content.contains("pub const TERM"));
        assert!(fetch_content.contains("pub const TERM"));

        // Both should have valid Rust
        syn::parse_file(&inherit_content).expect("inherit empty should be valid Rust");
        syn::parse_file(&fetch_content).expect("fetch-only should be valid Rust");

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_multi_context_inherit_with_fetch_only() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_multi_mode_mix");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let as_url = "https://example.com/as-multi";
        let as_json = serde_json::json!({
            "@context": {
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note",
                "name": {"@id": "as:name"}
            }
        });
        make_cache_file(&tmp_dir, as_url, &as_json);

        let sec_url = "https://example.com/sec-multi";
        let sec_json = serde_json::json!({
            "@context": {
                "sec": "https://w3id.org/security#",
                "publicKey": {"@id": "sec:publicKey", "@type": "@id"}
            }
        });
        make_cache_file(&tmp_dir, sec_url, &sec_json);

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        // inherit mode
        builder.add_context(
            "activity_streams",
            ContextSource::Inherit {
                url: as_url.to_string(),
                overrides: serde_json::json!({
                    "custom": "as:custom"
                }),
            },
        );
        // fetch-only mode
        builder.add_context(
            "security_v1",
            ContextSource::Url(sec_url.to_string()),
        );
        // with mode
        builder.add_context(
            "toot_ext",
            ContextSource::WithAlias {
                uri: "http://joinmastodon.org/ns#".to_string(),
                alias: "toot".to_string(),
                terms: serde_json::json!({
                    "discoverable": "toot:discoverable"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        syn::parse_file(&content).expect("Mixed multi-context should be valid Rust");

        // All 3 context types present
        assert!(content.contains("pub struct ActivityStreams<S = ()>"));
        assert!(content.contains("pub struct SecurityV1<S = ()>"));
        assert!(content.contains("pub struct TootExt<S = ()>"));

        // Composed type
        assert!(content.contains("pub type Context = ActivityStreams<SecurityV1<TootExt>>;"));

        // Cross-forwarding
        assert!(content.contains("HasSecurityV1 for ActivityStreams<S>"));
        assert!(content.contains("HasTootExt for ActivityStreams<S>"));

        // Terms from all modes
        assert!(content.contains("pub const NOTE"));         // inherit (fetched)
        assert!(content.contains("pub const CUSTOM"));       // inherit (override)
        assert!(content.contains("fn public_key("));          // fetch-only
        assert!(content.contains("pub const DISCOVERABLE")); // with

        let _ = fs::remove_dir_all(&tmp_dir);
    }

    #[test]
    fn integration_inherit_array_expansion_in_context_serializer() {
        let tmp_dir = std::env::temp_dir().join("ld_emit_test_inherit_array_expand");
        let _ = fs::remove_dir_all(&tmp_dir);
        fs::create_dir_all(&tmp_dir).unwrap();

        let url = "https://example.com/array-expand-ctx";
        let context_json = serde_json::json!({
            "@context": {
                "ex": "https://example.com/ns#",
                "Thing": "ex:Thing"
            }
        });
        make_cache_file(&tmp_dir, url, &context_json);

        let mut builder = ContextBuilder::new(tmp_dir.as_os_str());
        builder.add_context(
            "test_ctx",
            ContextSource::Inherit {
                url: url.to_string(),
                overrides: serde_json::json!({
                    "custom": "ex:custom"
                }),
            },
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();
        syn::parse_file(&content).expect("Array expansion should produce valid Rust");

        // The generated ContextSerializer should use extend for Array original_json
        // (inherit with non-empty overrides produces ["url", {overrides}] array)
        assert!(
            content.contains("extend"),
            "inherit mode with overrides should generate extend in ContextSerializer. Code:\n{}",
            content
        );

        let _ = fs::remove_dir_all(&tmp_dir);
    }
}
