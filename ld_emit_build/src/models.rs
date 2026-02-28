#[derive(Debug, Clone)]
pub enum TypeCoercion {
    Id,
    Vocab,
    Xsd(String),
}

#[derive(Debug, Clone)]
pub enum Container {
    Language,
    List,
    Set,
    Graph,
}

#[derive(Debug, Clone)]
pub enum TermKind {
    Prefix {
        uri: String,
    },
    KeywordAlias {
        keyword: String,
    },
    SimpleTerm {
        iri: String,
    },
    ExtendedTerm {
        id: String,
        type_coercion: Option<TypeCoercion>,
        container: Option<Container>,
    },
}

#[derive(Debug, Clone)]
pub struct TermDefinition {
    pub name: String,
    pub kind: TermKind,
}

#[derive(Debug, Clone)]
pub enum ContextSource {
    /// フェッチ専用モード: `name = "url"`
    Url(String),

    /// 継承モード: `name = "url" inherit: { ... }`
    Inherit {
        url: String,
        overrides: serde_json::Value,
    },

    /// エイリアス付き名前空間定義: `name = "uri" with alias: { ... }`
    WithAlias {
        uri: String,
        alias: String,
        terms: serde_json::Value,
    },
}

#[derive(Debug)]
pub struct ParsedContext {
    pub terms: Vec<TermDefinition>,
    pub original_json: serde_json::Value,
}

#[derive(Debug, Clone)]
pub struct ExposeValueDirective {
    pub expanded_iri: String,
}

/// @rename directive: renames a JSON-LD term to a different Rust method name.
#[derive(Debug, Clone)]
pub struct RenameDirective {
    /// Original JSON-LD term name (e.g., "type")
    pub from: String,
    /// Replacement Rust method name (e.g., "type_value")
    pub to: String,
}
