pub use serde;
pub use serde_json;

use std::fmt;
use std::marker::PhantomData;

// === Error Types ===

#[derive(Debug)]
pub enum LDError {
    Serialization(serde_json::Error),
}

impl fmt::Display for LDError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LDError::Serialization(e) => write!(f, "JSON-LD serialization error: {}", e),
        }
    }
}

impl std::error::Error for LDError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            LDError::Serialization(e) => Some(e),
        }
    }
}

impl From<serde_json::Error> for LDError {
    fn from(e: serde_json::Error) -> Self {
        LDError::Serialization(e)
    }
}

// === TypeConstant ===

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeConstant {
    pub iri: &'static str,
    pub term_name: &'static str,
}

// === ContextSerializer Trait ===

pub trait ContextSerializer {
    fn context_json() -> serde_json::Value;
}

impl ContextSerializer for () {
    fn context_json() -> serde_json::Value {
        serde_json::Value::Array(vec![])
    }
}

// === ObjectSerializer (minimal struct for LDSerializable reference) ===

pub struct ObjectSerializer<Ctx> {
    map: serde_json::Map<String, serde_json::Value>,
    _ctx: PhantomData<Ctx>,
}

impl<Ctx: ContextSerializer> ObjectSerializer<Ctx> {
    pub(crate) fn new() -> Self {
        ObjectSerializer {
            map: serde_json::Map::new(),
            _ctx: PhantomData,
        }
    }

    pub fn field(&mut self, key: &str, value: impl serde::Serialize) -> &mut Self {
        if let Ok(v) = serde_json::to_value(value) {
            self.map.insert(key.to_string(), v);
        }
        self
    }

    pub fn nested_object<F>(&mut self, key: &str, f: F) -> &mut Self
    where
        F: FnOnce(&mut ObjectSerializer<Ctx>),
    {
        let mut child = ObjectSerializer::<Ctx>::new();
        f(&mut child);
        self.map
            .insert(key.to_string(), serde_json::Value::Object(child.map));
        self
    }

    pub fn type_def(&mut self, types: &[&TypeConstant]) -> &mut Self {
        let value = match types {
            [single] => serde_json::Value::String(single.iri.to_string()),
            multiple => serde_json::Value::Array(
                multiple
                    .iter()
                    .map(|t| serde_json::Value::String(t.iri.to_string()))
                    .collect(),
            ),
        };
        self.map.insert("@type".to_string(), value);
        self
    }

    /// Set the `@type` field using an alias key and term names.
    ///
    /// Unlike `type_def()` which uses `"@type"` as the key and full IRIs as values,
    /// this method uses the provided alias key (e.g., `"type"`) and `term_name`
    /// for compact output compatible with ActivityPub and similar contexts.
    pub fn type_def_aliased(&mut self, key: &str, types: &[&TypeConstant]) -> &mut Self {
        let value = match types {
            [single] => serde_json::Value::String(single.term_name.to_string()),
            multiple => serde_json::Value::Array(
                multiple
                    .iter()
                    .map(|t| serde_json::Value::String(t.term_name.to_string()))
                    .collect(),
            ),
        };
        self.map.insert(key.to_string(), value);
        self
    }

    pub(crate) fn into_map(self) -> serde_json::Map<String, serde_json::Value> {
        self.map
    }
}

// === JSON-LD Keyword Types ===

pub struct LangString {
    pub value: String,
    pub language: String,
}

impl serde::Serialize for LangString {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(Some(2))?;
        map.serialize_entry("@value", &self.value)?;
        map.serialize_entry("@language", &self.language)?;
        map.end()
    }
}

pub struct LDList<T: serde::Serialize>(pub Vec<T>);

impl<T: serde::Serialize> serde::Serialize for LDList<T> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("@list", &self.0)?;
        map.end()
    }
}

// === LDSerializable Trait ===

pub trait LDSerializable {
    type Context: ContextSerializer;
    fn ld_serialize(&self, serializer: &mut ObjectSerializer<Self::Context>)
    -> Result<(), LDError>;
}

// === Serialization Functions ===

pub fn to_string<T: LDSerializable>(value: &T) -> Result<String, LDError> {
    let map = serialize_to_map(value)?;
    serde_json::to_string(&serde_json::Value::Object(map)).map_err(LDError::Serialization)
}

pub fn to_string_pretty<T: LDSerializable>(value: &T) -> Result<String, LDError> {
    let map = serialize_to_map(value)?;
    serde_json::to_string_pretty(&serde_json::Value::Object(map)).map_err(LDError::Serialization)
}

pub fn to_value<T: LDSerializable>(value: &T) -> Result<serde_json::Value, LDError> {
    let map = serialize_to_map(value)?;
    Ok(serde_json::Value::Object(map))
}

fn serialize_to_map<T: LDSerializable>(
    value: &T,
) -> Result<serde_json::Map<String, serde_json::Value>, LDError> {
    let mut serializer = ObjectSerializer::<T::Context>::new();
    serializer.field("@context", flatten_context_array(T::Context::context_json()));
    value.ld_serialize(&mut serializer)?;
    Ok(serializer.into_map())
}

/// Flatten a `@context` array by merging all Object entries into a single Object.
///
/// String entries (URL references) are kept as separate array elements.
/// Multiple Object entries are merged into one to reduce payload size.
fn flatten_context_array(value: serde_json::Value) -> serde_json::Value {
    let arr = match value {
        serde_json::Value::Array(a) => a,
        other => return other,
    };

    let mut non_objects: Vec<serde_json::Value> = Vec::new();
    let mut merged_obj = serde_json::Map::new();

    for item in arr {
        match item {
            serde_json::Value::Object(map) => {
                for (k, v) in map {
                    merged_obj.insert(k, v);
                }
            }
            other => non_objects.push(other),
        }
    }

    let mut result = non_objects;
    if !merged_obj.is_empty() {
        result.push(serde_json::Value::Object(merged_obj));
    }

    serde_json::Value::Array(result)
}

// === include_ld! Macro ===

/// Include generated ld_emit code from `OUT_DIR`.
///
/// The argument must match the `@serializer_name` used in `ld_context!`.
///
/// ```ignore
/// mod generated {
///     ld_emit::include_ld!("activity_pub");
/// }
/// ```
///
/// **Note:** JetBrains IDE (RustRover) cannot resolve `env!("OUT_DIR")` during
/// static analysis. Use `@export_dir` in `ld_context!` and `#[path]` instead.
/// See [`ld_context!`](ld_emit_build::ld_context) for details.
#[macro_export]
macro_rules! include_ld {
    ($package: tt) => {
        include!(concat!(env!("OUT_DIR"), concat!("/", $package, ".rs")));
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    // === LDError Tests ===

    #[test]
    fn ld_error_from_serde_json_error() {
        // serde_json::Error から LDError::Serialization への変換
        let json_err = serde_json::from_str::<serde_json::Value>("invalid").unwrap_err();
        let ld_err: LDError = LDError::Serialization(json_err);
        // Display が実装されていること
        let msg = format!("{}", ld_err);
        assert!(!msg.is_empty());
    }

    #[test]
    fn ld_error_implements_std_error() {
        let json_err = serde_json::from_str::<serde_json::Value>("invalid").unwrap_err();
        let ld_err: LDError = LDError::Serialization(json_err);
        // std::error::Error トレイトを実装していること
        let _err: &dyn std::error::Error = &ld_err;
    }

    #[test]
    fn ld_error_from_conversion() {
        let json_err = serde_json::from_str::<serde_json::Value>("invalid").unwrap_err();
        let _ld_err: LDError = json_err.into();
    }

    // === TypeConstant Tests ===

    #[test]
    fn type_constant_returns_iri() {
        const TEST_TYPE: TypeConstant = TypeConstant {
            iri: "https://www.w3.org/ns/activitystreams#Note",
            term_name: "Note",
        };

        assert_eq!(TEST_TYPE.iri, "https://www.w3.org/ns/activitystreams#Note");
    }

    #[test]
    fn type_constant_as_struct_references() {
        const NOTE_TYPE: TypeConstant = TypeConstant {
            iri: "https://www.w3.org/ns/activitystreams#Note",
            term_name: "Note",
        };

        const CREATE_TYPE: TypeConstant = TypeConstant {
            iri: "https://www.w3.org/ns/activitystreams#Create",
            term_name: "Create",
        };

        let types: Vec<&TypeConstant> = vec![&NOTE_TYPE, &CREATE_TYPE];
        assert_eq!(types[0].iri, "https://www.w3.org/ns/activitystreams#Note");
        assert_eq!(types[1].iri, "https://www.w3.org/ns/activitystreams#Create");
    }

    // === ContextSerializer Tests ===

    #[test]
    fn unit_context_serializer_returns_empty_array() {
        let value = <() as ContextSerializer>::context_json();
        assert_eq!(value, serde_json::Value::Array(vec![]));
    }

    #[test]
    fn custom_context_serializer() {
        struct TestContext;
        impl ContextSerializer for TestContext {
            fn context_json() -> serde_json::Value {
                serde_json::json!(["https://www.w3.org/ns/activitystreams"])
            }
        }

        let value = TestContext::context_json();
        assert_eq!(
            value,
            serde_json::json!(["https://www.w3.org/ns/activitystreams"])
        );
    }

    #[test]
    fn composed_context_serializer() {
        // 内側のコンテキスト
        struct Inner;
        impl ContextSerializer for Inner {
            fn context_json() -> serde_json::Value {
                serde_json::json!(["https://w3id.org/security/v1"])
            }
        }

        // 外側のコンテキストが内側を合成する
        struct Outer<S>(std::marker::PhantomData<S>);
        impl<S: ContextSerializer> ContextSerializer for Outer<S> {
            fn context_json() -> serde_json::Value {
                let mut arr = match S::context_json() {
                    serde_json::Value::Array(a) => a,
                    other => vec![other],
                };
                arr.push(serde_json::json!("https://www.w3.org/ns/activitystreams"));
                serde_json::Value::Array(arr)
            }
        }

        let value = <Outer<Inner>>::context_json();
        assert_eq!(
            value,
            serde_json::json!([
                "https://w3id.org/security/v1",
                "https://www.w3.org/ns/activitystreams"
            ])
        );
    }

    // === ObjectSerializer Tests ===

    #[test]
    fn object_serializer_field_inserts_value() {
        let mut ser = ObjectSerializer::<()>::new();
        ser.field("name", "Alice");
        let map = ser.into_map();
        assert_eq!(map.get("name").unwrap(), &serde_json::json!("Alice"));
    }

    #[test]
    fn object_serializer_field_chaining() {
        let mut ser = ObjectSerializer::<()>::new();
        ser.field("name", "Alice")
            .field("age", 30)
            .field("active", true);
        let map = ser.into_map();
        assert_eq!(map.len(), 3);
        assert_eq!(map.get("name").unwrap(), &serde_json::json!("Alice"));
        assert_eq!(map.get("age").unwrap(), &serde_json::json!(30));
        assert_eq!(map.get("active").unwrap(), &serde_json::json!(true));
    }

    #[test]
    fn object_serializer_nested_object() {
        let mut ser = ObjectSerializer::<()>::new();
        ser.nested_object("address", |child| {
            child.field("city", "Tokyo").field("country", "JP");
        });
        let map = ser.into_map();
        let addr = map.get("address").unwrap();
        assert_eq!(addr["city"], serde_json::json!("Tokyo"));
        assert_eq!(addr["country"], serde_json::json!("JP"));
    }

    #[test]
    fn object_serializer_nested_object_chaining() {
        let mut ser = ObjectSerializer::<()>::new();
        ser.field("name", "Alice")
            .nested_object("address", |child| {
                child.field("city", "Tokyo");
            })
            .field("active", true);
        let map = ser.into_map();
        assert_eq!(map.len(), 3);
    }

    #[test]
    fn object_serializer_type_def_single() {
        const NOTE_TYPE: TypeConstant = TypeConstant {
            iri: "https://www.w3.org/ns/activitystreams#Note",
            term_name: "Note",
        };

        let mut ser = ObjectSerializer::<()>::new();
        ser.type_def(&[&NOTE_TYPE]);
        let map = ser.into_map();
        assert_eq!(
            map.get("@type").unwrap(),
            &serde_json::json!("https://www.w3.org/ns/activitystreams#Note")
        );
    }

    #[test]
    fn object_serializer_type_def_multiple() {
        const NOTE_TYPE: TypeConstant = TypeConstant {
            iri: "https://www.w3.org/ns/activitystreams#Note",
            term_name: "Note",
        };
        const CREATE_TYPE: TypeConstant = TypeConstant {
            iri: "https://www.w3.org/ns/activitystreams#Create",
            term_name: "Create",
        };

        let mut ser = ObjectSerializer::<()>::new();
        ser.type_def(&[&NOTE_TYPE, &CREATE_TYPE]);
        let map = ser.into_map();
        assert_eq!(
            map.get("@type").unwrap(),
            &serde_json::json!([
                "https://www.w3.org/ns/activitystreams#Note",
                "https://www.w3.org/ns/activitystreams#Create"
            ])
        );
    }

    #[test]
    fn object_serializer_type_def_chaining() {
        const NOTE_TYPE: TypeConstant = TypeConstant {
            iri: "https://www.w3.org/ns/activitystreams#Note",
            term_name: "Note",
        };

        let mut ser = ObjectSerializer::<()>::new();
        ser.type_def(&[&NOTE_TYPE]).field("name", "test");
        let map = ser.into_map();
        assert_eq!(map.len(), 2);
    }

    // === LDSerializable Tests ===

    #[test]
    fn ld_serializable_can_be_implemented() {
        struct MyType {
            name: String,
        }

        impl LDSerializable for MyType {
            type Context = ();
            fn ld_serialize(
                &self,
                serializer: &mut ObjectSerializer<Self::Context>,
            ) -> Result<(), LDError> {
                serializer.field("name", &self.name);
                Ok(())
            }
        }

        let value = MyType {
            name: "test".to_string(),
        };
        let mut ser = ObjectSerializer::<()>::new();
        value.ld_serialize(&mut ser).unwrap();
    }

    // === LangString Tests ===

    #[test]
    fn lang_string_serializes_to_json_ld_format() {
        let ls = LangString {
            value: "Hello".to_string(),
            language: "en".to_string(),
        };
        let json = serde_json::to_value(&ls).unwrap();
        assert_eq!(
            json,
            serde_json::json!({"@value": "Hello", "@language": "en"})
        );
    }

    #[test]
    fn lang_string_in_object_serializer() {
        let ls = LangString {
            value: "こんにちは".to_string(),
            language: "ja".to_string(),
        };
        let mut ser = ObjectSerializer::<()>::new();
        ser.field("name", &ls);
        let map = ser.into_map();
        assert_eq!(
            map.get("name").unwrap(),
            &serde_json::json!({"@value": "こんにちは", "@language": "ja"})
        );
    }

    // === LDList Tests ===

    #[test]
    fn ld_list_serializes_to_json_ld_format() {
        let list = LDList(vec!["a", "b", "c"]);
        let json = serde_json::to_value(&list).unwrap();
        assert_eq!(json, serde_json::json!({"@list": ["a", "b", "c"]}));
    }

    #[test]
    fn ld_list_empty() {
        let list: LDList<String> = LDList(vec![]);
        let json = serde_json::to_value(&list).unwrap();
        assert_eq!(json, serde_json::json!({"@list": []}));
    }

    #[test]
    fn ld_list_with_integers() {
        let list = LDList(vec![1, 2, 3]);
        let json = serde_json::to_value(&list).unwrap();
        assert_eq!(json, serde_json::json!({"@list": [1, 2, 3]}));
    }

    #[test]
    fn ld_list_in_object_serializer() {
        let list = LDList(vec!["item1", "item2"]);
        let mut ser = ObjectSerializer::<()>::new();
        ser.field("items", &list);
        let map = ser.into_map();
        assert_eq!(
            map.get("items").unwrap(),
            &serde_json::json!({"@list": ["item1", "item2"]})
        );
    }

    // === to_string / to_string_pretty Tests ===

    #[test]
    fn to_string_produces_valid_json_ld() {
        struct TestCtx;
        impl ContextSerializer for TestCtx {
            fn context_json() -> serde_json::Value {
                serde_json::json!("https://www.w3.org/ns/activitystreams")
            }
        }

        struct MyNote;
        impl LDSerializable for MyNote {
            type Context = TestCtx;
            fn ld_serialize(
                &self,
                serializer: &mut ObjectSerializer<Self::Context>,
            ) -> Result<(), LDError> {
                serializer.field("name", "Test Note");
                Ok(())
            }
        }

        let result = to_string(&MyNote).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(
            parsed["@context"],
            serde_json::json!("https://www.w3.org/ns/activitystreams")
        );
        assert_eq!(parsed["name"], serde_json::json!("Test Note"));
    }

    #[test]
    fn to_string_with_empty_context() {
        struct MyType;
        impl LDSerializable for MyType {
            type Context = ();
            fn ld_serialize(
                &self,
                serializer: &mut ObjectSerializer<Self::Context>,
            ) -> Result<(), LDError> {
                serializer.field("value", 42);
                Ok(())
            }
        }

        let result = to_string(&MyType).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed["@context"], serde_json::json!([]));
        assert_eq!(parsed["value"], serde_json::json!(42));
    }

    #[test]
    fn to_string_pretty_produces_formatted_output() {
        struct MyType;
        impl LDSerializable for MyType {
            type Context = ();
            fn ld_serialize(
                &self,
                serializer: &mut ObjectSerializer<Self::Context>,
            ) -> Result<(), LDError> {
                serializer.field("name", "test");
                Ok(())
            }
        }

        let result = to_string_pretty(&MyType).unwrap();
        // Pretty output contains newlines
        assert!(result.contains('\n'));
        // Still valid JSON
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed["name"], serde_json::json!("test"));
    }

    #[test]
    fn to_string_context_inserted_first() {
        struct TestCtx;
        impl ContextSerializer for TestCtx {
            fn context_json() -> serde_json::Value {
                serde_json::json!(["https://example.com/ctx"])
            }
        }

        struct MyType;
        impl LDSerializable for MyType {
            type Context = TestCtx;
            fn ld_serialize(
                &self,
                serializer: &mut ObjectSerializer<Self::Context>,
            ) -> Result<(), LDError> {
                serializer.field("id", "123");
                Ok(())
            }
        }

        let result = to_string(&MyType).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(
            parsed["@context"],
            serde_json::json!(["https://example.com/ctx"])
        );
        assert_eq!(parsed["id"], serde_json::json!("123"));
    }

    #[test]
    fn to_string_with_type_def_and_nested() {
        const NOTE_TYPE: TypeConstant = TypeConstant {
            iri: "https://www.w3.org/ns/activitystreams#Note",
            term_name: "Note",
        };

        struct TestCtx;
        impl ContextSerializer for TestCtx {
            fn context_json() -> serde_json::Value {
                serde_json::json!("https://www.w3.org/ns/activitystreams")
            }
        }

        struct MyNote;
        impl LDSerializable for MyNote {
            type Context = TestCtx;
            fn ld_serialize(
                &self,
                serializer: &mut ObjectSerializer<Self::Context>,
            ) -> Result<(), LDError> {
                serializer
                    .type_def(&[&NOTE_TYPE])
                    .field("content", "Hello World")
                    .nested_object("author", |child| {
                        child.field("name", "Alice");
                    });
                Ok(())
            }
        }

        let result = to_string(&MyNote).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(
            parsed["@type"],
            serde_json::json!("https://www.w3.org/ns/activitystreams#Note")
        );
        assert_eq!(parsed["content"], serde_json::json!("Hello World"));
        assert_eq!(parsed["author"]["name"], serde_json::json!("Alice"));
    }

    // === flatten_context_array Tests ===

    // === to_value Tests ===

    #[test]
    fn to_value_returns_json_ld_value() {
        struct TestCtx;
        impl ContextSerializer for TestCtx {
            fn context_json() -> serde_json::Value {
                serde_json::json!(["https://www.w3.org/ns/activitystreams"])
            }
        }

        struct MyNote;
        impl LDSerializable for MyNote {
            type Context = TestCtx;
            fn ld_serialize(
                &self,
                serializer: &mut ObjectSerializer<Self::Context>,
            ) -> Result<(), LDError> {
                serializer.field("name", "Test Note");
                Ok(())
            }
        }

        let result = to_value(&MyNote).unwrap();
        assert!(result.is_object());
        assert_eq!(
            result["@context"],
            serde_json::json!(["https://www.w3.org/ns/activitystreams"])
        );
        assert_eq!(result["name"], serde_json::json!("Test Note"));
    }

    // === flatten_context_array Tests ===

    #[test]
    fn flatten_context_array_merges_multiple_objects() {
        let input = serde_json::json!([
            {"as": "https://www.w3.org/ns/activitystreams#", "Note": "as:Note"},
            {"sec": "https://w3id.org/security#", "Key": "sec:Key"},
            {"toot": "http://joinmastodon.org/ns#", "discoverable": "toot:discoverable"}
        ]);
        let result = flatten_context_array(input);
        assert_eq!(
            result,
            serde_json::json!([{
                "as": "https://www.w3.org/ns/activitystreams#",
                "Note": "as:Note",
                "sec": "https://w3id.org/security#",
                "Key": "sec:Key",
                "toot": "http://joinmastodon.org/ns#",
                "discoverable": "toot:discoverable"
            }])
        );
    }

    #[test]
    fn flatten_context_array_keeps_strings_separate() {
        let input = serde_json::json!([
            "https://www.w3.org/ns/activitystreams",
            {"sec": "https://w3id.org/security#", "Key": "sec:Key"},
            {"toot": "http://joinmastodon.org/ns#"}
        ]);
        let result = flatten_context_array(input);
        assert_eq!(
            result,
            serde_json::json!([
                "https://www.w3.org/ns/activitystreams",
                {
                    "sec": "https://w3id.org/security#",
                    "Key": "sec:Key",
                    "toot": "http://joinmastodon.org/ns#"
                }
            ])
        );
    }

    #[test]
    fn flatten_context_array_single_object_keeps_array() {
        let input = serde_json::json!([{"as": "https://www.w3.org/ns/activitystreams#"}]);
        let result = flatten_context_array(input);
        assert_eq!(
            result,
            serde_json::json!([{"as": "https://www.w3.org/ns/activitystreams#"}])
        );
    }

    #[test]
    fn flatten_context_array_single_string_keeps_array() {
        let input = serde_json::json!(["https://www.w3.org/ns/activitystreams"]);
        let result = flatten_context_array(input);
        assert_eq!(
            result,
            serde_json::json!(["https://www.w3.org/ns/activitystreams"])
        );
    }

    #[test]
    fn flatten_context_array_non_array_passthrough() {
        let input = serde_json::json!("https://www.w3.org/ns/activitystreams");
        let result = flatten_context_array(input);
        assert_eq!(
            result,
            serde_json::json!("https://www.w3.org/ns/activitystreams")
        );
    }

    #[test]
    fn flatten_context_array_empty_stays_empty() {
        let input = serde_json::json!([]);
        let result = flatten_context_array(input);
        assert_eq!(result, serde_json::json!([]));
    }

    #[test]
    fn flatten_context_array_mixed_strings_and_objects() {
        // inherit mode: ["url", {overrides}] + with mode objects
        let input = serde_json::json!([
            "https://example.com/context",
            {"custom": "value"},
            "https://other.example.com/context",
            {"sec": "https://w3id.org/security#"}
        ]);
        let result = flatten_context_array(input);
        assert_eq!(
            result,
            serde_json::json!([
                "https://example.com/context",
                "https://other.example.com/context",
                {"custom": "value", "sec": "https://w3id.org/security#"}
            ])
        );
    }
}
