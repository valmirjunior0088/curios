//! `unused-binder` and `unused-let` once elaboration has credited them: a binder only a proof the elaborator writes reads is used wherever that proof is written, `_` among them, and a credit names the declaration whose binder was read and no other.

use {super::test_support::*, crate::*, curios_core::LintKind};

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

/// A program checked again over the unit kept of it, refused for `_other`, keeps `_halves` as it was elaborated, and the proof of `/`'s bound that reads `ok` stands in the record kept with it: what the refusal reports reads that proof as the accepted check did, and `_other`'s own `n` is the one binder nothing reads.
///
/// Mutation-checked: with the refusal handed out before the kept records' credits join, `ok` is reported.
#[test]
fn a_kept_declarations_proof_reads_its_hypothesis_beside_a_refusal() {
    let accepted = "use /std/{Nat, Eq};
use /std/Bool/{Holds};

let _halves(n: Nat, ok: Holds(0 < n)) -> Eq()(10 / n, 10 / n) = Eq/refl();

let _other(n: Nat) -> Nat = n;

/std/print(\"x\")
";
    let refused = accepted.replace("-> Nat = n;", "-> Nat = \"s\";");
    let cache = Keeping::default();

    assert!(checked_through(accepted, &cache).verdict.is_ok());
    assert!(cache.kept.borrow().is_some(), "the accepted unit was kept");
    let Checked { entry, verdict } = checked_through(&refused, &cache);

    assert!(verdict.is_err(), "`_other` is refused");
    assert_eq!(
        entry
            .lints
            .iter()
            .filter(|lint| lint.kind == LintKind::UnusedBinder)
            .map(|lint| lint.report(&[&entry.anchors]).message())
            .collect::<Vec<_>>(),
        ["unused binder\n  `n` is never used\n  write `_` in its name's place"]
    );
}

/// A program refused for `_bad` reports what the record of each declaration it filed holds, worded as an accepted check words it: `_one`'s `_` binds `{}`, which its record's class says.
#[test]
fn a_refused_program_words_its_declarations_sites_by_what_they_bind() {
    let source = "use /std/{Nat};

let _one(_: {}) -> Nat = 1;

let _bad: Nat = \"s\";

/std/print(\"x\")
";
    let Checked { entry, verdict } = checked_through(source, &Keeping::default());

    assert!(verdict.is_err(), "`_bad` is refused");
    assert_eq!(
        entry
            .lints
            .iter()
            .filter(|lint| lint.kind == LintKind::UnitWildcard)
            .map(|lint| lint.report(&[&entry.anchors]).message())
            .collect::<Vec<_>>(),
        [
            "unit wildcard\n  this `_` binds `{}`\n  write `()`, which refuses a value that stops being `{}`"
        ]
    );
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
    let failed = Test/assert(\"it fails\", false);
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

/// A default renaming the name its match's head is, `| m =>` under `match n`, is reported where it is written, by the head it renames, and so it is checked again over the unit the first check kept, from the record its declaration was filed with: the lowering decides it, and the declaration's record holds it. A default under a head that is an expression is the one name of the value it binds, and no rename.
#[test]
fn a_default_renaming_its_head_is_reported_and_one_naming_a_value_is_not() {
    let source = "use /std/{Nat};

let _renamed(n: Nat) -> Nat =
    match n
    | 0 => 1
    | m => m
    end;

let _named(n: Nat) -> Nat =
    match n + 1
    | 0 => 1
    | m => m
    end;

/std/print(\"x\")
";
    let cache = Keeping::default();
    for round in ["first", "over the kept unit"] {
        let Checked { entry, verdict } = checked_through(source, &cache);

        assert!(verdict.is_ok(), "{round}");
        assert_eq!(
            entry
                .lints
                .iter()
                .filter(|lint| lint.kind == LintKind::RenamingBinder)
                .map(|lint| lint.report(&[&entry.anchors]).message())
                .collect::<Vec<_>>(),
            [
                "renaming binder\n  `m` renames `n`, which this match takes apart\n  read `n`, or write `_`"
            ],
            "{round}"
        );
    }
}
