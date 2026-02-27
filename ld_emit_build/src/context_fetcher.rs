use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use crate::error::LDBuildError;

pub struct ContextFetcher {
    cache_dir: PathBuf,
}

impl ContextFetcher {
    pub fn new(cache_dir: PathBuf) -> Self {
        ContextFetcher { cache_dir }
    }

    pub fn fetch(&self, url: &str) -> Result<String, LDBuildError> {
        let cache_path = self.cache_path(url);

        // Try network fetch first
        match self.fetch_from_network(url) {
            Ok(json) => {
                // Write to cache
                if let Err(e) = Self::write_cache(&cache_path, &json) {
                    eprintln!(
                        "ld_emit_build: warning: failed to write cache for {}: {}",
                        url, e
                    );
                }
                // Register cache file for cargo rerun-if-changed
                println!("cargo::rerun-if-changed={}", cache_path.display());
                Ok(json)
            }
            Err(network_err) => {
                // Try cache fallback
                if cache_path.exists() {
                    eprintln!(
                        "ld_emit_build: warning: network fetch failed for {}, using cached version",
                        url
                    );
                    println!("cargo::rerun-if-changed={}", cache_path.display());
                    fs::read_to_string(&cache_path).map_err(|e| LDBuildError::Network {
                        url: url.to_string(),
                        source: Box::new(e),
                    })
                } else {
                    Err(network_err)
                }
            }
        }
    }

    /// Returns the cache file path for a given URL.
    /// Uses a hash of the URL as the filename.
    fn cache_path(&self, url: &str) -> PathBuf {
        let mut hasher = DefaultHasher::new();
        url.hash(&mut hasher);
        let hash = hasher.finish();
        self.cache_dir.join(format!("{:016x}.json", hash))
    }

    fn fetch_from_network(&self, url: &str) -> Result<String, LDBuildError> {
        let body = ureq::get(url)
            .header("Accept", "application/ld+json")
            .call()
            .map_err(|e| LDBuildError::Network {
                url: url.to_string(),
                source: Box::new(e),
            })?
            .body_mut()
            .read_to_string()
            .map_err(|e| LDBuildError::Network {
                url: url.to_string(),
                source: Box::new(e),
            })?;

        Ok(body)
    }

    fn write_cache(path: &Path, content: &str) -> Result<(), std::io::Error> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temp_cache_dir(suffix: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "ld_emit_test_{}_{suffix}",
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn cleanup(dir: &Path) {
        let _ = fs::remove_dir_all(dir);
    }

    // === cache_path tests ===

    #[test]
    fn cache_path_is_deterministic() {
        let dir = temp_cache_dir("deterministic");
        let fetcher = ContextFetcher::new(dir.clone());
        let url = "https://www.w3.org/ns/activitystreams";
        let path1 = fetcher.cache_path(url);
        let path2 = fetcher.cache_path(url);
        assert_eq!(path1, path2);
        cleanup(&dir);
    }

    #[test]
    fn cache_path_differs_for_different_urls() {
        let dir = temp_cache_dir("differs");
        let fetcher = ContextFetcher::new(dir.clone());
        let path1 = fetcher.cache_path("https://www.w3.org/ns/activitystreams");
        let path2 = fetcher.cache_path("https://w3id.org/security/v1");
        assert_ne!(path1, path2);
        cleanup(&dir);
    }

    #[test]
    fn cache_path_has_json_extension() {
        let dir = temp_cache_dir("extension");
        let fetcher = ContextFetcher::new(dir.clone());
        let path = fetcher.cache_path("https://example.com");
        assert_eq!(path.extension().unwrap(), "json");
        cleanup(&dir);
    }

    #[test]
    fn cache_path_is_inside_cache_dir() {
        let dir = temp_cache_dir("inside");
        let fetcher = ContextFetcher::new(dir.clone());
        let path = fetcher.cache_path("https://example.com");
        assert!(path.starts_with(&dir));
        cleanup(&dir);
    }

    // === write_cache tests ===

    #[test]
    fn write_cache_creates_file() {
        let dir = temp_cache_dir("write");
        let path = dir.join("test.json");
        ContextFetcher::write_cache(&path, r#"{"test": true}"#).unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), r#"{"test": true}"#);
        cleanup(&dir);
    }

    #[test]
    fn write_cache_creates_parent_directories() {
        let dir = temp_cache_dir("write_nested");
        let path = dir.join("sub").join("dir").join("test.json");
        ContextFetcher::write_cache(&path, "content").unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "content");
        cleanup(&dir);
    }

    // === fetch with cache fallback tests ===

    #[test]
    fn fetch_uses_cache_on_network_failure() {
        let dir = temp_cache_dir("cache_fallback");
        let fetcher = ContextFetcher::new(dir.clone());
        let url = "https://nonexistent.invalid/context";

        // Pre-populate cache
        let cache_path = fetcher.cache_path(url);
        fs::create_dir_all(cache_path.parent().unwrap()).unwrap();
        fs::write(&cache_path, r#"{"@context": {"id": "@id"}}"#).unwrap();

        // Fetch should succeed from cache
        let result = fetcher.fetch(url);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), r#"{"@context": {"id": "@id"}}"#);
        cleanup(&dir);
    }

    #[test]
    fn fetch_returns_error_when_no_cache_and_network_fails() {
        let dir = temp_cache_dir("no_cache");
        let fetcher = ContextFetcher::new(dir.clone());
        let url = "https://nonexistent.invalid/context";

        let result = fetcher.fetch(url);
        assert!(result.is_err());
        match result.unwrap_err() {
            LDBuildError::Network { url: err_url, .. } => {
                assert_eq!(err_url, url);
            }
            other => panic!("Expected Network error, got: {:?}", other),
        }
        cleanup(&dir);
    }

    // === Network integration tests (require internet access) ===

    #[test]
    #[ignore]
    fn fetch_activitystreams_context() {
        let dir = temp_cache_dir("as_ctx");
        let fetcher = ContextFetcher::new(dir.clone());
        let result = fetcher.fetch("https://www.w3.org/ns/activitystreams");
        assert!(result.is_ok());
        let json = result.unwrap();
        assert!(json.contains("@context"));
        cleanup(&dir);
    }

    #[test]
    #[ignore]
    fn fetch_security_v1_context_with_redirect() {
        let dir = temp_cache_dir("sec_ctx");
        let fetcher = ContextFetcher::new(dir.clone());
        let result = fetcher.fetch("https://w3id.org/security/v1");
        assert!(result.is_ok());
        let json = result.unwrap();
        assert!(json.contains("@context"));
        cleanup(&dir);
    }

    #[test]
    #[ignore]
    fn fetch_writes_cache_file_on_success() {
        let dir = temp_cache_dir("write_cache");
        let fetcher = ContextFetcher::new(dir.clone());
        let url = "https://www.w3.org/ns/activitystreams";
        let cache_path = fetcher.cache_path(url);

        // Ensure no cache exists
        let _ = fs::remove_file(&cache_path);

        // Fetch should succeed
        let json = fetcher.fetch(url).unwrap();

        // Cache file should now exist with same content
        assert!(cache_path.exists());
        let cached = fs::read_to_string(&cache_path).unwrap();
        assert_eq!(cached, json);
        cleanup(&dir);
    }
}
