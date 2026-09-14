//! The content digest, and the fingerprint that finishes into one.

use {super::*, std::hash::Hasher};

/// A fingerprint is the adapter between a producer that writes into a `Hasher` and a key that is spelled as a digest, so what was written has to decide what comes out.
#[test]
fn a_fingerprint_answers_for_what_was_written_into_it() {
    let fold = |bytes: &[u8]| {
        let mut fingerprint = Fingerprint::new();
        fingerprint.write(bytes);

        fingerprint.hex()
    };

    assert_eq!(fold(b"one"), fold(b"one"), "and it is a function");
    assert_ne!(fold(b"one"), fold(b"two"));
    assert_eq!(
        fold(b"one"),
        digest(b"one"),
        "and it is the same digest a record is checked with, since both are SHA-256 over what they were given"
    );
}

/// A frame is what separates one part from the next, so the same bytes split differently are two keys.
#[test]
fn the_same_bytes_split_into_different_parts_are_different_keys() {
    let fold = |parts: &[&str]| {
        let mut fingerprint = Fingerprint::new();
        for part in parts {
            fingerprint.feed(part);
        }

        fingerprint.hex()
    };

    assert_ne!(fold(&["ab", "c"]), fold(&["a", "bc"]));
    assert_ne!(fold(&["abc"]), fold(&["ab", "c"]));
    assert_eq!(
        fold(&["ab", "c"]),
        fold(&["ab", "c"]),
        "and it is a function"
    );
}
