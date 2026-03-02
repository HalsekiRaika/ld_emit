fn main() {
    ld_emit_build::ld_context! {
        @serializer_name "ld_emit_generated",
        @export_dir "src/generated",
        activity_streams = "https://www.w3.org/ns/activitystreams" with as: {
            "id": "@id",
            "type": "@type",
            "Note": "as:Note",
            "Person": "as:Person",
            "Create": "as:Create",
            "name": {"@id": "as:name"},
            "summary": {"@id": "as:summary"},
            "content": "as:content",
            "actor": {"@id": "as:actor", "@type": "@id"},
            "published": {"@id": "as:published"},
            "url": {"@id": "as:url", "@type": "@id"},
            "preferredUsername": {"@id": "as:preferredUsername"},
            "inbox": {"@id": "http://www.w3.org/ns/ldp#inbox", "@type": "@id"},
            "outbox": {"@id": "as:outbox", "@type": "@id"}
        },
        security_v1 = "https://w3id.org/security" with sec: {
            "Key": "sec:Key",
            "publicKey": {"@id": "sec:publicKey", "@type": "@id"},
            "publicKeyPem": {"@id": "sec:publicKeyPem"},
            "owner": {"@id": "sec:owner", "@type": "@id"}
        },
        toot_ext = "http://joinmastodon.org/ns" with toot: {
            "discoverable": "toot:discoverable",
            "featured": {"@id": "toot:featured", "@type": "@id"}
        },
        @expose_value {
            "https://www.w3.org/ns/activitystreams#content",
            "http://joinmastodon.org/ns#discoverable",
        },
        @rename {
            "type" -> "kind"
        }
    }
    ld_emit_build::ld_context! {
        @serializer_name "activitypub_gen",
        @export_dir "src/generated",
        activity_streams = "https://www.w3.org/ns/activitystreams",
        security_v1 = "https://w3id.org/security" with sec: {
            "Key": "sec:Key",
            "publicKey": {"@id": "sec:publicKey", "@type": "@id"},
            "publicKeyPem": {"@id": "sec:publicKeyPem"},
            "owner": {"@id": "sec:owner", "@type": "@id"}
        },
        @expose_value {
            "https://www.w3.org/ns/activitystreams#content",
            "https://www.w3.org/ns/activitystreams#name",
        }
        @rename {
            "type" -> "kind"
        }
    }
}
