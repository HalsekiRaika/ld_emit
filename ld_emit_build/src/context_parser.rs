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
                // First pass: collect obvious prefixes (URI ends with # or /)
                for (key, val) in map {
                    if let Some(uri) = val.as_str()
                        && Self::is_prefix_uri(uri)
                        && !key.starts_with('@')
                    {
                        resolver.add_prefix(key, uri);
                    }
                }

                // Second pass: detect implicit prefixes via compact IRI usage
                Self::detect_implicit_prefixes(map, resolver);

                // Third pass: classify all entries
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

    /// Detect prefixes that don't end with `#` or `/` but are referenced
    /// by compact IRIs (e.g., `"as:Note"`) elsewhere in the context.
    fn detect_implicit_prefixes(
        map: &serde_json::Map<String, serde_json::Value>,
        resolver: &mut PrefixResolver,
    ) {
        let mut used_prefixes = std::collections::HashSet::new();

        for (_key, val) in map {
            match val {
                serde_json::Value::String(s) => {
                    if let Some(prefix) = Self::extract_compact_iri_prefix(s) {
                        used_prefixes.insert(prefix.to_string());
                    }
                }
                serde_json::Value::Object(obj) => {
                    if let Some(serde_json::Value::String(id)) = obj.get("@id")
                        &&let Some(prefix) = Self::extract_compact_iri_prefix(id)
                    {
                        used_prefixes.insert(prefix.to_string());
                    }
                    if let Some(serde_json::Value::String(t)) = obj.get("@type")
                        && let Some(prefix) = Self::extract_compact_iri_prefix(t)
                    {
                        used_prefixes.insert(prefix.to_string());
                    }
                }
                _ => {}
            }
        }

        for prefix in &used_prefixes {
            if resolver.has_prefix(prefix) {
                continue;
            }
            if let Some(val) = map.get(prefix.as_str())
                && let Some(uri) = val.as_str()
                && (uri.starts_with("http://") || uri.starts_with("https://"))
            {
                resolver.add_prefix(prefix, uri);
            }
        }
    }

    /// Extract the prefix part from a compact IRI (e.g., `"as"` from `"as:Note"`).
    /// Returns `None` for full URIs (`http://...`) and keyword references (`@id`).
    fn extract_compact_iri_prefix(value: &str) -> Option<&str> {
        let (prefix, local) = value.split_once(':')?;
        if local.starts_with("//") || prefix.starts_with('@') {
            return None;
        }
        Some(prefix)
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

        // Prefix declaration: URI ends with # or /, or detected as prefix via compact IRI usage
        if Self::is_prefix_uri(value) || resolver.has_prefix(key) {
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
