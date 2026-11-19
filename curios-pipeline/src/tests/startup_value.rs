//! `startup-value`: a value every program its unit is in scope of runs as it starts, whether or not it reads it, reported where it is declared, and no value a program leaves out where nothing reads it.

use {super::test_support::*, curios_core::LintKind, curios_unit::Unit};

/// What a unit reports as `startup-value`, rendered.
fn startup_values(unit: &Unit) -> Vec<String> {
    unit.lints()
        .iter()
        .filter(|lint| lint.kind == LintKind::StartupValue)
        .map(|lint| lint.report(&[]).message())
        .collect()
}

/// A loop no measure bounds, and a count to a bound by a definition the certifier does not judge total, both given up on where their unit is erased.
const PARTIAL: &str = "use /std/{Nat};

let forever(n: Nat) -> Nat = forever(n + 1);

let up(i: Nat, n: Nat) -> Nat =
    match i < n
    | true => up(i + 1, n)
    | false => i
    end;

pub let hidden: Nat = forever(0);

pub let climbed: Nat = up(0, 10000000);
";

/// A value whose computation is not known to end, and which its unit's compile could not finish, is reported at the name it was declared under, worded with the two ways out.
#[test]
fn a_value_not_known_to_end_is_reported_at_its_name() {
    let unit = unit_of(PARTIAL);

    assert_eq!(
        placed_lints(&unit, LintKind::StartupValue),
        [(11, 9), (13, 9)]
    );
    assert_eq!(
        startup_values(&unit)[0],
        "value computed at every start\n  `hidden` is computed as each program in its unit's scope starts, whether or not the program reads it: what it computes is not known to end\n  make it a function of none, which runs where it is called, or give what it computes a definition the certifier judges total, which a program leaves out where nothing reads it"
    );
}

/// A witness whose value runs such a computation is reported at its declaration, worded as the witness it is.
#[test]
fn a_witness_that_runs_one_is_reported() {
    let unit = unit_of(
        "use /std/{Nat};

pub concept Seed(A: Type): pub Type {
    seed: A,
}

let up(i: Nat, n: Nat) -> Nat =
    match i < n
    | true => up(i + 1, n)
    | false => i
    end;

pub struct Meters: pub Type { Nat }

satisfy Seed(Meters) {
    seed = Meters { up(0, 10000000) },
}
",
    );

    assert_eq!(placed_lints(&unit, LintKind::StartupValue).len(), 1);
    assert!(
        startup_values(&unit)[0].contains("this witness is computed"),
        "{:?}",
        startup_values(&unit)
    );
}

/// What every program leaves out where nothing reads it is not reported: a count the certifier judges total, which the evaluator gives up on; a description; a match over a constant; a command line declared as a value; a function of none that would not end; and values finished where the unit is compiled.
#[test]
fn what_a_program_leaves_out_unread_is_not_reported() {
    let unit = unit_of(
        "use /std/{Nat, Option, Cli, Io};

let count(n: Nat, acc: Nat) -> Nat =
    match n
    | 0 => acc
    | p + 1 => count(p, acc + 1)
    end;

let forever(n: Nat) -> Nat = forever(n + 1);

pub let five: Nat = 5;

pub let counted: Nat = count(10000000, 0);

pub let greeting: Io({}) = /std/print(\"hi\");

pub let picked: Nat =
    match five
    | 0 => 1
    | _ => 2
    end;

pub let tool: Cli =
    Cli/declare(
        Cli/leaf(\"tool\", \"A tool\", [Cli/flag(\"dry\", \"Change nothing\")], Cli/positionals([])),
        Option/some(\"1.0.0\"));

pub let later() -> Nat = forever(0);

pub let summed: Nat = count(100, 0);
",
    );

    assert_eq!(startup_values(&unit), Vec::<String>::new());
}

/// A unit compiled over a baseline keeps the lint of an item it keeps: `extra` is added, `hidden` is kept as it was erased, and still reported where it stands.
#[test]
fn a_recompile_keeps_the_lint_of_a_kept_value() {
    let baseline = unit_of(PARTIAL);
    let edited = format!("{PARTIAL}\npub let extra: Nat = 1;\n");
    let unit = recompile_over(&edited, &baseline).expect("the unit compiles over its baseline");

    assert_eq!(
        placed_lints(&unit, LintKind::StartupValue),
        [(11, 9), (13, 9)]
    );
}
