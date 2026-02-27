use crate::error::LDBuildError;
use crate::models::*;
use crate::prefix_resolver::PrefixResolver;

pub struct ContextParser;

impl ContextParser {
    pub fn parse(json: &str) -> Result<ParsedContext, LDBuildError> {
        let value: serde_json::Value =
            serde_json::from_str(json).map_err(|e| LDBuildError::Parse {
                context: String::new(),
                message: e.to_string(),
                position: None,
            })?;

        let original_json = value.clone();

        // Unwrap @context key if present
        let context_value = value
            .as_object()
            .and_then(|o| o.get("@context"))
            .cloned()
            .unwrap_or(value);

        let mut resolver = PrefixResolver::new();
        let mut terms = Vec::new();

        // Parse the context value
        Self::parse_context_value(&context_value, &mut resolver, &mut terms)?;

        Ok(ParsedContext {
            source: ContextSource::Inline(original_json),
            terms,
            original_json: context_value,
        })
    }

    fn parse_context_value(
        value: &serde_json::Value,
        resolver: &mut PrefixResolver,
        terms: &mut Vec<TermDefinition>,
    ) -> Result<(), LDBuildError> {
        match value {
            serde_json::Value::Object(map) => {
                // First pass: collect all prefixes
                for (key, val) in map {
                    if let Some(uri) = val.as_str()
                        && Self::is_prefix_uri(uri)
                        && !key.starts_with('@')
                    {
                        resolver.add_prefix(key, uri);
                    }
                }

                // Second pass: classify all entries
                for (key, val) in map {
                    if let Some(td) = Self::classify_entry(key, val, resolver) {
                        terms.push(td);
                    }
                }
            }
            serde_json::Value::Array(arr) => {
                for item in arr {
                    Self::parse_context_value(item, resolver, terms)?;
                }
            }
            serde_json::Value::String(_) => {
                // A bare URL string in a context - nothing to extract as terms
            }
            _ => {}
        }
        Ok(())
    }

    fn is_prefix_uri(uri: &str) -> bool {
        uri.ends_with('#') || uri.ends_with('/')
    }

    fn classify_entry(
        key: &str,
        value: &serde_json::Value,
        resolver: &PrefixResolver,
    ) -> Option<TermDefinition> {
        // Skip @version declarations
        if key == "@version" {
            return None;
        }

        // Skip @vocab declarations (context-level default vocabulary directive).
        // The value is preserved in original_json for context_json() output.
        if key == "@vocab" {
            return None;
        }

        match value {
            serde_json::Value::String(s) => Self::classify_string_entry(key, s, resolver),
            serde_json::Value::Object(obj) => Self::classify_object_entry(key, obj, resolver),
            _ => None,
        }
    }

    fn classify_string_entry(
        key: &str,
        value: &str,
        resolver: &PrefixResolver,
    ) -> Option<TermDefinition> {
        // Keyword alias: value starts with @
        if value.starts_with('@') {
            return Some(TermDefinition {
                name: key.to_string(),
                kind: TermKind::KeywordAlias {
                    keyword: value.to_string(),
                },
            });
        }

        // Prefix declaration: value ends with # or /
        if Self::is_prefix_uri(value) {
            return Some(TermDefinition {
                name: key.to_string(),
                kind: TermKind::Prefix {
                    uri: value.to_string(),
                },
            });
        }

        // Simple term: resolve compact IRI or use as-is
        let iri = Self::resolve_iri(value, resolver);
        Some(TermDefinition {
            name: key.to_string(),
            kind: TermKind::SimpleTerm { iri },
        })
    }

    fn classify_object_entry(
        key: &str,
        obj: &serde_json::Map<String, serde_json::Value>,
        resolver: &PrefixResolver,
    ) -> Option<TermDefinition> {
        // Must have @id to be a valid extended term definition
        let id_value = obj.get("@id")?.as_str()?;
        let id = Self::resolve_iri(id_value, resolver);

        let type_coercion = obj.get("@type").and_then(|t| {
            let t_str = t.as_str()?;
            match t_str {
                "@id" => Some(TypeCoercion::Id),
                "@vocab" => Some(TypeCoercion::Vocab),
                _ => {
                    // xsd:* or other typed literal
                    let resolved = Self::resolve_iri(t_str, resolver);
                    Some(TypeCoercion::Xsd(resolved))
                }
            }
        });

        let container = obj.get("@container").and_then(|c| {
            let c_str = c.as_str()?;
            match c_str {
                "@language" => Some(Container::Language),
                "@list" => Some(Container::List),
                "@set" => Some(Container::Set),
                "@graph" => Some(Container::Graph),
                _ => None,
            }
        });

        Some(TermDefinition {
            name: key.to_string(),
            kind: TermKind::ExtendedTerm {
                id,
                type_coercion,
                container,
            },
        })
    }

    fn resolve_iri(value: &str, resolver: &PrefixResolver) -> String {
        // Try prefix resolution first
        if let Some(resolved) = resolver.resolve(value) {
            return resolved;
        }
        // Return as-is (already a full IRI or unresolvable)
        value.to_string()
    }
}
