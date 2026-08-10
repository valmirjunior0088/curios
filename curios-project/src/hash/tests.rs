use super::*;

/// A well-formed `c1:` digest, round-tripping through the spelling it was written as.
#[test]
fn a_c1_hash_parses() {
    let spelling = format!("c1:{}", "0123456789abcdef".repeat(4));
    let hash = TreeHash::parse(&spelling).expect("a well-formed c1 hash");

    assert_eq!(hash.to_string(), spelling);
}

#[test]
fn an_unknown_scheme_is_refused() {
    let refusal = TreeHash::parse(&format!("c2:{}", "a".repeat(64)))
        .expect_err("a scheme this compiler does not know");

    assert!(refusal.contains("names no hash scheme"), "{refusal}");
}

/// A bare digest with no scheme is the same refusal: the prefix is what lets two schemes verify during a transition, so it is never optional.
#[test]
fn a_scheme_less_digest_is_refused() {
    let refusal = TreeHash::parse(&"a".repeat(64)).expect_err("a digest with no scheme");

    assert!(refusal.contains("names no hash scheme"), "{refusal}");
}

#[test]
fn a_short_digest_is_refused() {
    let refusal = TreeHash::parse("c1:abc").expect_err("a digest of the wrong length");

    assert!(refusal.contains("64 lowercase hex digits"), "{refusal}");
}

/// Uppercase is refused rather than folded, because the hash is a store key and two spellings of one digest would be two directories.
#[test]
fn an_uppercase_digest_is_refused() {
    let refusal =
        TreeHash::parse(&format!("c1:{}", "A".repeat(64))).expect_err("an uppercase digest");

    assert!(refusal.contains("64 lowercase hex digits"), "{refusal}");
}
