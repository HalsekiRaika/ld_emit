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
        let serializer_name = self.serializer_name.as_ref().ok_or_else(|| LDBuildError::CodeGen {
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
                    ContextParser::parse(&json)?
                }
                ContextSource::Inline(value) => {
                    let json_str = serde_json::to_string(value)
                        .map_err(|e| LDBuildError::Parse {
                            context: name.clone(),
                            message: format!("Failed to serialize inline context: {}", e),
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
        let syntax_tree: syn::File = syn::parse2(tokens)
            .map_err(|e| LDBuildError::CodeGen {
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
                std::env::var_os("CARGO_MANIFEST_DIR")
                    .expect("CARGO_MANIFEST_DIR not set"),
            );
            let include_path = manifest_dir.join(export_dir).join(&output_filename);
            if let Some(parent) = include_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&include_path, &formatted)?;
        }

        Ok(())
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
        assert!(matches!(&builder.contexts[0].1, ContextSource::Url(u) if u == "https://www.w3.org/ns/activitystreams"));
    }

    #[test]
    fn context_builder_add_context_inline() {
        let dir = OsString::from("/tmp/test_out");
        let mut builder = ContextBuilder::new(&dir);
        let inline = serde_json::json!({
            "toot": "http://joinmastodon.org/ns#",
            "discoverable": "toot:discoverable"
        });
        builder.add_context("toot_ext", ContextSource::Inline(inline));
        assert_eq!(builder.contexts.len(), 1);
        assert_eq!(builder.contexts[0].0, "toot_ext");
        assert!(matches!(&builder.contexts[0].1, ContextSource::Inline(_)));
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
            ContextSource::Inline(serde_json::json!({
                "toot": "http://joinmastodon.org/ns#",
                "discoverable": "toot:discoverable"
            })),
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
            ContextSource::Inline(serde_json::json!({
                "id": "@id",
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note"
            })),
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
            ContextSource::Inline(serde_json::json!({"id": "@id"})),
        );
        builder.add_context(
            "second",
            ContextSource::Inline(serde_json::json!({"value": "@value"})),
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
            ContextSource::Inline(serde_json::json!({
                "id": "@id",
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note",
                "Person": "as:Person",
                "name": {"@id": "as:name"},
                "actor": {"@id": "as:actor", "@type": "@id"},
                "content": "as:content"
            })),
        );
        builder.add_context(
            "security_v1",
            ContextSource::Inline(serde_json::json!({
                "sec": "https://w3id.org/security#",
                "CryptographicKey": "sec:Key",
                "publicKey": {"@id": "sec:publicKey", "@type": "@id"}
            })),
        );
        builder.add_context(
            "toot_ext",
            ContextSource::Inline(serde_json::json!({
                "toot": "http://joinmastodon.org/ns#",
                "discoverable": "toot:discoverable"
            })),
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
            ContextSource::Inline(serde_json::json!({
                "as": "https://www.w3.org/ns/activitystreams#",
                "content": "as:content",
                "name": {"@id": "as:name"}
            })),
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
            ContextSource::Inline(serde_json::json!({
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note"
            })),
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
            ContextSource::Inline(serde_json::json!({
                "id": "@id"
            })),
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
            ContextSource::Inline(serde_json::json!({
                "type": "@type"
            })),
        );
        builder.set_serializer_name("test_output");
        let result = builder.generate();
        assert!(result.is_ok(), "KeywordAlias 'type' should auto-resolve without @rename");
        let output_path = tmp_dir.join("test_output.rs");
        let code = fs::read_to_string(&output_path).unwrap();
        assert!(code.contains("r#type"), "Generated code should contain r#type. Code:\n{}", code);

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
            ContextSource::Inline(serde_json::json!({
                "fn": {"@id": "https://example.com#fn"}
            })),
        );
        builder.set_serializer_name("test_output");
        let result = builder.generate();
        assert!(result.is_err(), "ExtendedTerm 'fn' without @rename should error");

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
            ContextSource::Inline(serde_json::json!({
                "sec": "https://w3id.org/security#",
                "publicKey": {"@id": "sec:publicKey", "@type": "@id"},
                "owner": {"@id": "sec:owner", "@type": "@id"}
            })),
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
            ContextSource::Inline(serde_json::json!({
                "id": "@id",
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note",
                "name": {"@id": "as:name"}
            })),
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        let content = fs::read_to_string(tmp_dir.join("test_output.rs")).unwrap();

        // Re-parse and re-format: prettyplease output should be idempotent
        let syntax_tree = syn::parse_file(&content)
            .expect("Generated code should be valid Rust");
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
            ContextSource::Inline(serde_json::json!({
                "id": "@id",
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note"
            })),
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
            ContextSource::Inline(serde_json::json!({"id": "@id"})),
        );
        builder.set_serializer_name("test_output");
        builder.generate().unwrap();

        // The output file should exist at the expected path
        let output_path = tmp_dir.join("test_output.rs");
        assert!(output_path.exists(), "Generated file should exist at OUT_DIR/test_output.rs");

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
            ContextSource::Inline(serde_json::json!({"id": "@id"})),
        );
        // Do NOT set serializer_name
        let result = builder.generate();
        assert!(result.is_err(), "@serializer_name is required");

        let _ = fs::remove_dir_all(&tmp_dir);
    }
}
