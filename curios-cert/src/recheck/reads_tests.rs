//! What each judged definition's record says it read of other items.

use {super::test_support::*, curios_core::Reads, std::collections::BTreeSet};

/// `alias : Type = Nat`, `three : alias = 3`, `identity : (Nat) -> Nat = (x) => x`, `five : Nat = identity(5)`, and `four : Nat = 4`.
const READING: &str = r#"
let /alias: Type =
    #NatType;

let /three: /alias =
    3;

let /identity: (x: #NatType) -> #NatType =
    (x: #NatType) => x;

let /five: #NatType =
    /identity(5);

let /four: #NatType =
    4;
"#;

/// A definition's record holds what judging it read, of the kind it read, and nothing another judgment did. `five` applies `identity`, which types the head — its signature — and unfolds nothing. `three`'s declared type is `alias`, which is typed as written, so `alias`'s signature is read, and checking `3` against it unfolds it, so its body is read too. `four` names nothing, and `alias` reads nothing of its own.
#[test]
fn a_definition_reads_the_signature_it_types_and_the_body_it_unfolds() {
    let rechecked = text_certified(READING);
    assert_eq!(rechecked.refusals(), Vec::new());
    let reads = |symbol: &str| {
        rechecked
            .certification()
            .reads(&global(symbol))
            .cloned()
            .expect("the record covers every judged definition")
    };

    assert_eq!(
        reads("five"),
        Reads {
            signatures: BTreeSet::from([global("identity")]),
            bodies: BTreeSet::new(),
        },
    );
    assert_eq!(
        reads("three"),
        Reads {
            signatures: BTreeSet::from([global("alias")]),
            bodies: BTreeSet::from([global("alias")]),
        },
    );
    assert_eq!(reads("four"), Reads::default());
    assert_eq!(reads("alias"), Reads::default());
}
