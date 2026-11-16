//! `unused-binder` and `unused-let` once elaboration has credited them: a binder only a proof the elaborator writes reads is used wherever that proof is written, `_` among them, and a credit names the declaration whose binder was read and no other.

use {super::test_support::*, curios_lint::LintKind};

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

/// An action bound to `_` and never run is a description nothing performs: the `let` does nothing, and running it is as likely the way out as deleting it.
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
            "unused-let: unused let\n  this `_` is an action this program never runs\n  run it with `!`, or delete it"
        ]
    );
}

/// A `Test` bound and never run is reported as an `Io` is: no list of types decides it, and an assertion dropped this way would let its test pass. It is an action of the region it stands in, whose monad `Test` is.
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
            "unused-let: unused let\n  `failed` is an action this program never runs\n  run it with `!`, or delete it"
        ]
    );
}

/// A fallible operation answers a `Try` over `Io`, a struct of one field holding an action, and is one.
#[test]
fn a_fallible_action_bound_and_never_run_is_an_action() {
    let unit = unit_of(
        "use /std/{Io, File, Path, print};

pub let greet() -> Io({}) =
    let read = File/read_all_text(Path/of_str(\"notes.txt\"));
    print(\"ran\\n\");
",
    );

    assert_eq!(
        lints_of(&unit),
        [
            "unused-let: unused let\n  `read` is an action this program never runs\n  run it with `!`, or delete it"
        ]
    );
}

/// A value of no action's type is deleted: it runs nothing whatever it is.
#[test]
fn a_value_bound_and_never_read_is_deleted() {
    let unit = unit_of(
        "use /std/{Nat};

pub let count() -> Nat =
    let unread = [1, 2, 3];
    3;
",
    );

    assert_eq!(
        lints_of(&unit),
        [
            "unused-let: unused let\n  `unread` is never used, and its value runs no action\n  delete it"
        ]
    );
}

/// A binder of `{}` nothing reads gives its name up to `()`, which checks the value is `{}`, rather than to `_`, which takes whatever a result becomes.
#[test]
fn a_binder_of_unit_nothing_reads_is_told_to_write_unit() {
    let unit = unit_of(
        "use /std/{Nat};

pub let one(done: {}) -> Nat = 1;
",
    );

    assert_eq!(
        lints_of(&unit),
        ["unused-binder: unused binder\n  `done` is never used\n  write `()` in its name's place"]
    );
}

/// A `_` of `{}` is told to be `()`, which refuses a value that stops being `{}`, wherever it is written: a `let` that runs an action, a part of a `let`'s pattern, a lambda's and a definition's parameter, a variant's payload and a match tuple's component.
#[test]
fn a_wildcard_of_unit_is_told_to_write_unit_wherever_it_stands() {
    let unit = unit_of(
        "use /std/{Io, Nat, Result, Str, print};

pub let run() -> Io({}) =
    let _ = print(\"one\\n\")!;
    let (line, _) = (\"two\\n\", ());
    Io/bind(print(line), (_) => print(\"three\\n\"));

pub let one(_: {}) -> Nat = 1;

pub let succeeded(r: Result(Str, {})) -> Nat =
    match r
    | success(_) => 1
    | failure(_) => 0
    end;

pub let first(p: {Nat, {}}) -> Nat =
    match p
    | (k, _) => k
    end;
",
    );

    assert_eq!(
        lints_of(&unit),
        vec![
            "unit-wildcard: unit wildcard\n  this `_` binds `{}`\n  write `()`, which refuses a value that stops being `{}`";
            6
        ]
    );
    assert_eq!(
        placed_lints(&unit, LintKind::UnitWildcard),
        [(4, 9), (5, 16), (6, 27), (8, 13), (12, 15), (18, 11)]
    );
}

/// A `_` of anything but `{}` is the slot it should be, and an arm's whole `_` takes what the arms before it left rather than a part of a value, whatever its type: neither is reported.
#[test]
fn a_wildcard_of_anything_else_is_never_reported() {
    let unit = unit_of(
        "use /std/{Nat, Option};

pub let second(p: {Nat, Nat}) -> Nat =
    let (_, b) = p;
    b;

pub let fallback(o: Option(Nat)) -> Nat =
    match o
    | some(_) => 1
    | none() => 0
    end;

pub let whole(u: {}) -> Nat =
    match u
    | _ => 1
    end;
",
    );

    assert_eq!(lints_of(&unit), Vec::<String>::new());
}
