//! Coverage, strict positivity behind a record, and the foreign wire contract.
//!
//! Its kernel counterpart is `curios-cert/src/kernel/infer/eliminate/coverage_tests.rs`.

use {super::test_support::*, curios_core::Polarity, curios_elab::Error};

// Coverage. A missing arm leaves an elimination undefined at that constructor, which is a proof of the motive at an index nothing established.
#[test]
fn an_elimination_must_enumerate_its_constructors() {
    refused(AN_ELIMINATION_MUST_ENUMERATE_ITS_CONSTRUCTORS).by_elaborator_in("/f", |error| {
        matches!(error, Error::MatrixUncovered { indexed: false, .. })
    });
}

// The foreign wire contract. The embedder supplies these values, so a `foreign` admitted at an arbitrary type would let the host hand back an inhabitant of a proposition that nothing ever checked.
#[test]
fn a_foreign_declaration_is_confined_to_wire_types() {
    rejected_by(
        A_FOREIGN_DECLARATION_IS_CONFINED_TO_WIRE_TYPES,
        "expected a wire type",
    );
}

// The other support the argument names, at a shape positivity's own probes do not spell: they run the negative and the double negative bare, through an `induct` parameter, through a `struct` parameter, through a type alias, under `List`, behind a type-level `match`, and at a higher-kinded parameter — never behind an anonymous Σ, which is the construct this row is about.
//
// The diagnostic is what makes this more than a repeat. It reads *positively, but not strictly* rather than *negatively*, which is the same verdict the bare spelling gets: the polarity lattice is computed through the tuple component rather than the component being answered opaquely, since an opaque answer would join to `Mixed` and refuse with the other message. Refusing a merely-`Pos` diagonal is precisely what keeps `℘℘` out, a family that is its own double function space having no model as a set, so this is the pairing the row rests on, checked where the row lives.
#[test]
fn a_non_strict_occurrence_behind_a_record_is_still_refused() {
    refused(A_NON_STRICT_OCCURRENCE_BEHIND_A_RECORD_IS_STILL_REFUSED).by_elaborator_in(
        "/Bad",
        |error| {
            matches!(
                error,
                Error::NotStrictlyPositive {
                    polarity: Polarity::Pos,
                    ..
                }
            )
        },
    );
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
