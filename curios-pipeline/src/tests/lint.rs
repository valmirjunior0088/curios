//! `unused-binder` and `unused-let` once elaboration has credited them: a binder only a proof the elaborator writes reads is used wherever that proof is written, `_` among them, and a credit names the declaration whose binder was read and no other.

use super::test_support::*;

/// The bound of `/` in the result type is discharged from `ok`, which nothing names: the proof is written while the type elaborates, under the type's own binder.
#[test]
fn a_hypothesis_only_a_proof_in_the_result_type_reads_is_used() {
    let unit = unit_of(
        "use /std/{Nat, Eq};
use /std/Bool/{Holds};

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
use /std/Bool/{Holds};

pub let first(n: Nat, idle: Nat) -> Nat =
    let _ = second;
    n
pub and second(n: Nat, ok: Holds(0 < n)) -> {Nat, Eq()(10 / n, 10 / n)} = (first(n, n), Eq/refl());
",
    );

    assert_eq!(
        unused_binders(&unit),
        ["unused binder\n  `idle` is never used\n  write `_` in its name's place"]
    );
}

/// `holder` binds a local at `lemma`'s type, whose hypothesis sits at the place `idle` has in `holder`: a type that travels carries no credit with it.
#[test]
fn a_declaration_holding_anothers_type_is_credited_nothing_by_it() {
    let unit = unit_of(
        "use /std/{Nat, Eq};
use /std/Bool/{Holds};

pub let lemma(n: Nat, ok: Holds(0 < n)) -> Eq()(10 / n, 10 / n) = Eq/refl();

pub let holder(m: Nat, idle: Nat) -> Nat =
    let _ = lemma;
    m;
",
    );

    assert_eq!(
        unused_binders(&unit),
        ["unused binder\n  `idle` is never used\n  write `_` in its name's place"]
    );
}

/// `below(t)` is bound to `_` for the fact it states, which nothing names: the proof `proved` stands for reads it, so the `let` does something, and no lint stands.
#[test]
fn a_wildcard_a_proof_reads_is_used() {
    let unit = unit_of(
        "use /std/{Bytes, Nat, proved};
use /std/Bool/{Holds};

pub let bounded(t: Bytes) -> Holds(Bytes/to_nat(t) < Nat/pow(256, Bytes/len(t))) =
    let _ = Bytes/to_nat/below(t);
    proved();
",
    );

    assert_eq!(lints_of(&unit), Vec::<String>::new());
}

/// An action bound to `_` and never run is a description nothing performs: the `let` does nothing.
#[test]
fn an_action_bound_and_never_run_is_reported() {
    let unit = unit_of(
        "use /std/{Io, print};

pub let greet() -> Io({}) =
    let _ = print(\"never printed\\n\");
    print(\"ran\\n\");
",
    );

    assert_eq!(
        lints_of(&unit),
        [
            "unused-let: unused let\n  nothing reads this `let`, and its value runs no action\n  delete it"
        ]
    );
}

/// A `Test` bound and never run is reported as an `Io` is: no list of types decides it, and an assertion dropped this way would let its test pass.
#[test]
fn a_test_bound_and_never_run_is_reported() {
    let unit = unit_of(
        "use /std/{Test};

pub let dropped() -> Test({}) =
    let failed = Test/assert(false);
    Test/pure(());
",
    );

    assert_eq!(
        lints_of(&unit),
        [
            "unused-let: unused let\n  `failed` is never used, and its value runs no action\n  delete it"
        ]
    );
}
