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

// `j + j` is `0` at `j = 0` and a successor everywhere else, so neither case's target clashes with it and each arm is written. `curios_cert`'s `recheck::case_tests` puts the module to the kernel under this name, with `j + 1` as the control a clash excuses.
#[test]
fn an_arm_absent_where_a_sum_may_meet_its_target_is_refused() {
    let refusals = refused(
        r#"
        use /std/{Nat};

        induct Ix: (i: Nat) -> Type
        | zero(): (0)
        | succ(k: Nat): (k + 1)
        end

        pub let only_zero(j: Nat, v: Ix(j + j)) -> Nat = match v | zero() => 0 end;
        pub let only_succ(j: Nat, v: Ix(j + j)) -> Nat = match v | succ(k) => k end;
        pub let excused(j: Nat, v: Ix(j + 1)) -> Nat = match v | succ(k) => k end;

        /std/print("unreachable")
        "#,
    );

    for at in ["/only_zero", "/only_succ"] {
        refusals.by_elaborator_in(at, |error| {
            matches!(error, Error::MatrixUncovered { indexed: true, .. })
        });
    }
}
