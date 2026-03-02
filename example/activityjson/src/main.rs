use ld_emit::{LDError, LDSerializable, ObjectSerializer};

// Include generated code from build.rs
// Uses @include_file to write to src/generated/ for JetBrains IDE compatibility
#[path = "generated/activitypub_gen.rs"]
pub mod generated;

pub use self::generated::*;

// pub mod _another_import {
//     ld_emit::include_ld!("ld_emit_generated");
// }
//
// pub use _another_import::*;


/// A Mastodon-style ActivityPub Person actor.
struct Person {
    id: String,
    name: String,
    preferred_username: String,
    summary: String,
    inbox: String,
    outbox: String,
    discoverable: bool,
    public_key_id: String,
    public_key_pem: String,
}

impl LDSerializable for Person {
    type Context = Context;

    fn ld_serialize(
        &self,
        ser: &mut ObjectSerializer<Self::Context>,
    ) -> Result<(), LDError> {
        ser
            .kind(&[&activity_streams::PERSON])
            .id(&self.id)
            .name_with(&self.name)
            // .preferred_username(&self.preferred_username)
            // .summary(&self.summary)
            .inbox(&self.inbox)
            .outbox(&self.outbox)
            // .discoverable_with(self.discoverable)
            .public_key_object(|key| {
                key
                    .kind(&[&security_v1::KEY])
                    .id(&self.public_key_id)
                    .owner(&self.id)
                    .public_key_pem(&self.public_key_pem);
            });
        Ok(())
    }
}

/// A simple ActivityStreams Note object.
struct Note {
    id: String,
    content: String,
    attributed_to: String,
    published: String,
}

impl LDSerializable for Note {
    type Context = Context;

    fn ld_serialize(
        &self,
        ser: &mut ObjectSerializer<Self::Context>,
    ) -> Result<(), LDError> {
        ser
            .kind(&[&activity_streams::NOTE])
            .id(&self.id)
            .content_with(&self.content)
            .actor(&self.attributed_to)
            .published(&self.published);
        Ok(())
    }
}

fn main() {
    // Serialize a Person actor
    let person = Person {
        id: "https://mastodon.example/users/alice".to_string(),
        name: "Alice".to_string(),
        preferred_username: "alice".to_string(),
        summary: "A test user for ld_emit".to_string(),
        inbox: "https://mastodon.example/users/alice/inbox".to_string(),
        outbox: "https://mastodon.example/users/alice/outbox".to_string(),
        discoverable: true,
        public_key_id: "https://mastodon.example/users/alice#main-key".to_string(),
        public_key_pem: "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhki...\n-----END PUBLIC KEY-----\n".to_string(),
    };

    println!("=== Person Actor ===");
    let json = ld_emit::to_string_pretty(&person).unwrap();
    println!("{}", json);

    // Serialize a Note
    let note = Note {
        id: "https://mastodon.example/users/alice/statuses/1".to_string(),
        content: "<p>Hello, ActivityPub world!</p>".to_string(),
        attributed_to: "https://mastodon.example/users/alice".to_string(),
        published: "2026-02-23T12:00:00Z".to_string(),
    };

    println!("\n=== Note ===");
    let json = ld_emit::to_string_pretty(&note).unwrap();
    println!("{}", json);
}
