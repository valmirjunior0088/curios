//! `unused-binder` once elaboration has credited it: a binder only a proof the elaborator writes reads is used wherever that proof is written, and a credit names the declaration whose binder was read and no other.

use super::test_support::{unit_of, unused_binders};

/// The bound of `/` in the result type is discharged from `ok`, which nothing names: the proof is written while the type elaborates, under the type's own binder.
#[test]
fn a_hypothesis_only_a_proof_in_the_result_type_reads_is_used() {
    let unit = unit_of(
        "use /std/{Nat, Eq};

pub let halves(n: Nat, ok: Holds(0 < n)) -> Eq()(10 / n, 10 / n) = Eq/refl();
",
    );

    assert_eq!(unused_binders(&unit), Vec::<String>::new());
}

/// `idle` and `ok` sit at one place among their own declarations' written binders, so a credit keyed by the wrong member would spare the first and report the second.
#[test]
fn a_proof_in_a_group_members_type_credits_that_member() {
    let unit = unit_of(
        "use /std/{Nat, Eq};

pub let first(n: Nat, idle: Nat) -> Nat = n
pub and second(n: Nat, ok: Holds(0 < n)) -> Eq()(10 / n, 10 / n) = Eq/refl();
",
    );

    assert_eq!(
        unused_binders(&unit),
        ["unused binder `idle`; name it `_idle` to keep it"]
    );
}

/// `holder` binds a local at `lemma`'s type, whose hypothesis sits at the place `idle` has in `holder`: a type that travels carries no credit with it.
#[test]
fn a_declaration_holding_anothers_type_is_credited_nothing_by_it() {
    let unit = unit_of(
        "use /std/{Nat, Eq};

pub let lemma(n: Nat, ok: Holds(0 < n)) -> Eq()(10 / n, 10 / n) = Eq/refl();

pub let holder(m: Nat, idle: Nat) -> Nat =
    let _ = lemma;
    m;
",
    );

    assert_eq!(
        unused_binders(&unit),
        ["unused binder `idle`; name it `_idle` to keep it"]
    );
}
