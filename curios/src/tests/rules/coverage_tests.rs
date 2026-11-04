//! Coverage: an elimination enumerates its constructors, and a clash is told from an index that may still meet.

use {super::test_support::*, curios_elab::Error};

// Coverage. A missing arm leaves an elimination undefined at that constructor, which is a proof of the motive at an index nothing established.
#[test]
fn an_elimination_must_enumerate_its_constructors() {
    refused(AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS).by_elaborator_in("/f", |error| {
        matches!(error, Error::MatrixUncovered { indexed: false, .. })
    });
}

// An index pair is impossible only where no value meets it: a truncated difference is zero wherever its subtrahend is at least its minuend, so `x - y` against `0` is no clash.
#[test]
fn a_truncated_difference_against_zero_is_no_clash() {
    refused(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{False};

        let absurd(x: Nat, y: Nat, p: Eq()(x - y, 0)) -> False = match p end;

        /std/print(Nat/to_str(0))
        "#,
    )
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The same over the free monoid: a concatenation of chunks is empty wherever every chunk is, so `[..xs, ..xs]` against `[]` is no clash.
#[test]
fn a_concatenation_of_chunks_against_the_empty_list_is_no_clash() {
    refused(
        r#"
        use /std/{Nat, Eq, List};
        use /std/Bool/{False};

        let absurd(xs: List(Nat), p: Eq()([..xs, ..xs], [])) -> False = match p end;

        /std/print(Nat/to_str(0))
        "#,
    )
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}
