use std::collections::HashMap;

pub struct PrefixResolver {
    prefixes: HashMap<String, String>,
}

impl Default for PrefixResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl PrefixResolver {
    pub fn new() -> Self {
        PrefixResolver {
            prefixes: HashMap::new(),
        }
    }

    pub fn add_prefix(&mut self, name: &str, uri: &str) {
        self.prefixes.insert(name.to_string(), uri.to_string());
    }

    pub fn has_prefix(&self, name: &str) -> bool {
        self.prefixes.contains_key(name)
    }

    pub fn resolve(&self, compact_iri: &str) -> Option<String> {
        let (prefix, local) = compact_iri.split_once(':')?;
        let base_uri = self.prefixes.get(prefix)?;
        Some(format!("{}{}", base_uri, local))
    }
}
