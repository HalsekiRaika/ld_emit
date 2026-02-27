fn main() {
    ld_emit_build::ld_context! {
        @serializer_name "ld_emit_generated",
        @export_dir "src/generated",
        activity_streams: {
            "id": "@id",
            "type": "@type",
            "as": "https://www.w3.org/ns/activitystreams#",
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
        security_v1: {
            "sec": "https://w3id.org/security#",
            "Key": "sec:Key",
            "publicKey": {"@id": "sec:publicKey", "@type": "@id"},
            "publicKeyPem": {"@id": "sec:publicKeyPem"},
            "owner": {"@id": "sec:owner", "@type": "@id"}
        },
        toot_ext: {
            "toot": "http://joinmastodon.org/ns#",
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
        activitystream: "https://www.w3.org/ns/activitystreams#",
        w3idsecurty: "https://w3id.org/security#",
        @expose_value {
            "https://www.w3.org/ns/activitystreams#content",
            "https://www.w3.org/ns/activitystreams#name",
        }
    }
}
