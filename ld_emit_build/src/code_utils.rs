//! Rust identifier conversion utilities for JSON-LD term names.
//!
//! Converts camelCase term names to snake_case, handles Rust reserved words,
//! and resolves name collisions across contexts.

/// Convert a camelCase or PascalCase string to snake_case.
pub fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() {
            if i > 0 {
                let prev = s.as_bytes()[i - 1] as char;
                if prev.is_lowercase() || prev.is_ascii_digit() {
                    result.push('_');
                } else if prev.is_uppercase() {
                    // Check if next char is lowercase (e.g., "URL" -> keep together, "URLParser" -> "url_parser")
                    if let Some(&next) = s.as_bytes().get(i + 1)
                        && (next as char).is_lowercase() {
                            result.push('_');
                        }
                    
                }
            }
            result.push(ch.to_lowercase().next().unwrap());
        } else {
            result.push(ch);
        }
    }
    result
}

/// Convert a camelCase, PascalCase, snake_case, or other string to PascalCase.
/// Normalizes through snake_case first to correctly handle word boundaries.
pub fn to_pascal_case(s: &str) -> String {
    let snake = to_snake_case(s);
    snake
        .split('_')
        .filter(|part| !part.is_empty())
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                Some(first) => {
                    let upper: String = first.to_uppercase().collect();
                    upper + chars.as_str()
                }
                None => String::new(),
            }
        })
        .collect()
}

const RUST_RESERVED_WORDS: &[&str] = &[
    "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum",
    "extern", "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move",
    "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true",
    "type", "unsafe", "use", "where", "while", "yield",
];

/// Check if a name is a Rust reserved word.
pub fn is_reserved(name: &str) -> bool {
    RUST_RESERVED_WORDS.contains(&name)
}

/// Escape a snake_case identifier if it collides with a Rust reserved word.
/// Returns `r#name` for reserved words, otherwise returns as-is.
pub fn escape_reserved(name: &str) -> String {
    if is_reserved(name) {
        format!("r#{}", name)
    } else {
        name.to_string()
    }
}

/// Convert a JSON-LD term name to a safe Rust snake_case identifier.
pub fn term_to_method_name(term: &str) -> String {
    let snake = to_snake_case(term);
    escape_reserved(&snake)
}

/// Deduplicate a method name by prepending a context prefix.
/// Used when multiple contexts define terms with the same snake_case name.
pub fn dedup_method_name(context_name: &str, method_name: &str) -> String {
    let prefix = to_snake_case(context_name);
    format!("{}_{}", prefix, method_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    // === to_snake_case Tests ===

    #[test]
    fn snake_case_simple_camel() {
        assert_eq!(to_snake_case("orderedItems"), "ordered_items");
    }

    #[test]
    fn snake_case_pascal() {
        assert_eq!(to_snake_case("ActivityStreams"), "activity_streams");
    }

    #[test]
    fn snake_case_already_snake() {
        assert_eq!(to_snake_case("already_snake"), "already_snake");
    }

    #[test]
    fn snake_case_single_word_lower() {
        assert_eq!(to_snake_case("actor"), "actor");
    }

    #[test]
    fn snake_case_single_word_upper() {
        assert_eq!(to_snake_case("Note"), "note");
    }

    #[test]
    fn snake_case_consecutive_uppercase() {
        assert_eq!(to_snake_case("publicKeyPem"), "public_key_pem");
    }

    #[test]
    fn snake_case_all_uppercase() {
        assert_eq!(to_snake_case("URL"), "url");
    }

    #[test]
    fn snake_case_acronym_followed_by_word() {
        assert_eq!(to_snake_case("manuallyApprovesFollowers"), "manually_approves_followers");
    }

    // === to_pascal_case Tests ===

    #[test]
    fn pascal_case_from_snake() {
        assert_eq!(to_pascal_case("activity_streams"), "ActivityStreams");
    }

    #[test]
    fn pascal_case_from_single_word() {
        assert_eq!(to_pascal_case("security"), "Security");
    }

    #[test]
    fn pascal_case_from_already_pascal() {
        assert_eq!(to_pascal_case("ActivityStreams"), "ActivityStreams");
    }

    #[test]
    fn pascal_case_from_already_pascal_preserved() {
        assert_eq!(to_pascal_case("CryptographicKey"), "CryptographicKey");
    }

    #[test]
    fn pascal_case_from_camel_case() {
        assert_eq!(to_pascal_case("publicKeyPem"), "PublicKeyPem");
    }

    #[test]
    fn pascal_case_from_single_lowercase() {
        assert_eq!(to_pascal_case("note"), "Note");
    }

    #[test]
    fn pascal_case_from_acronym() {
        assert_eq!(to_pascal_case("URLParser"), "UrlParser");
    }

    #[test]
    fn pascal_case_from_multi_segment() {
        assert_eq!(to_pascal_case("toot_ext"), "TootExt");
    }

    // === escape_reserved Tests ===

    #[test]
    fn escape_reserved_type() {
        assert_eq!(escape_reserved("type"), "r#type");
    }

    #[test]
    fn escape_reserved_as() {
        assert_eq!(escape_reserved("as"), "r#as");
    }

    #[test]
    fn escape_not_reserved() {
        assert_eq!(escape_reserved("actor"), "actor");
    }

    // === term_to_method_name Tests ===

    #[test]
    fn method_name_camel_case() {
        assert_eq!(term_to_method_name("orderedItems"), "ordered_items");
    }

    #[test]
    fn method_name_reserved_word() {
        assert_eq!(term_to_method_name("type"), "r#type");
    }

    #[test]
    fn method_name_simple() {
        assert_eq!(term_to_method_name("actor"), "actor");
    }

    #[test]
    fn method_name_pascal_case_term() {
        assert_eq!(term_to_method_name("Note"), "note");
    }

    #[test]
    fn method_name_public_key_pem() {
        assert_eq!(term_to_method_name("publicKeyPem"), "public_key_pem");
    }

    // === dedup_method_name Tests ===

    #[test]
    fn dedup_with_context_prefix() {
        assert_eq!(
            dedup_method_name("activity_streams", "name"),
            "activity_streams_name"
        );
    }

    #[test]
    fn dedup_pascal_context() {
        assert_eq!(
            dedup_method_name("SecurityV1", "id"),
            "security_v1_id"
        );
    }

    // === is_reserved Tests ===

    #[test]
    fn is_reserved_type() {
        assert!(is_reserved("type"));
    }

    #[test]
    fn is_reserved_as() {
        assert!(is_reserved("as"));
    }

    #[test]
    fn is_reserved_name_is_not() {
        assert!(!is_reserved("name"));
    }

    #[test]
    fn is_reserved_fn() {
        assert!(is_reserved("fn"));
    }

    #[test]
    fn is_reserved_actor_is_not() {
        assert!(!is_reserved("actor"));
    }
}
