//! What a declaration costs is its own: a unit accepts it at every budget it is accepted at alone, and what a scope costs to bring in is spent on no declaration's.
//!
//! Each case finds the smallest budget one declaration is accepted under and puts many to it, so nothing here states a figure the library's growth would move.

use {
    super::test_support::{entrypoint_of, mounted},
    crate::{EntryTail, check_entrypoint, compile_units, elaborate_and_zonk},
    curios_core::Zonked,
    curios_elab::{Erasures, erase_program},
    curios_prelude::with_prelude,
    curios_text::{RootSource, SYNTAX, UnitSource},
    curios_unit::{Predecessors, Unit},
    std::fmt::Write,
};

/// The smallest budget `accepted` takes, to within a sixteenth: doubled until it is taken, then halved in four times between the last refused and the first taken.
fn smallest(accepted: impl Fn(u64) -> bool) -> u64 {
    let mut taken = 1_024;
    while !accepted(taken) {
        taken *= 2;
        assert!(taken < 1 << 32, "accepted under no budget");
    }
    let mut refused = taken / 2;
    for _ in 0..4 {
        let middle = refused + (taken - refused) / 2;
        match accepted(middle) {
            true => taken = middle,
            false => refused = middle,
        }
    }

    taken
}

/// Twelve aliases, each applying the one before it twice, so reading through the last costs four thousand unfoldings.
fn aliases() -> String {
    let mut source = String::from("let W1(A: Type) -> Type = A;\n");
    for index in 2..=12 {
        let below = index - 1;
        writeln!(
            source,
            "let W{index}(A: Type) -> Type = W{below}(W{below}(A));"
        )
        .unwrap();
    }

    source
}

/// `count` families that name nothing of one another, each with one payload read through the aliases.
fn families(count: usize) -> String {
    let mut source = aliases();
    for index in 1..=count {
        writeln!(
            source,
            "induct T{index}(A: Type): Type\n| leaf{index}()\n| node{index}(W12(T{index}(A)))\nend"
        )
        .unwrap();
    }
    source.push_str("/std/Io/pure(())\n");

    source
}

/// `count` values that name nothing of one another, each at a type of its own read through the aliases: a family apiece, so every value costs what the others do and no reading of one type answers for another.
fn values(count: usize) -> String {
    let mut source = aliases();
    for index in 1..=count {
        writeln!(
            source,
            "induct B{index}: Type\n| b{index}()\nend\nlet value{index}: W12(B{index}) = B{index}/b{index}();"
        )
        .unwrap();
    }
    source.push_str("/std/Io/pure(())\n");

    source
}

/// Whether `source` is taken under `budget` over `scope` by everything that judges it: elaborated, certified by the kernel and erased.
fn checks(source: &str, budget: u64, scope: &[&Unit]) -> bool {
    let entrypoint = entrypoint_of(source);

    check_entrypoint(
        budget,
        Predecessors::over(scope),
        &SYNTAX,
        &entrypoint,
        &RootSource::none(),
        EntryTail::Authored,
    )
    .is_ok()
}

/// Whether `source`, elaborated under the default budget, erases under `budget`.
fn erases(source: &str, budget: u64) -> bool {
    let entrypoint = entrypoint_of(source);

    with_prelude(|prelude| {
        let predecessors = Predecessors::over(prelude);
        let (program, _foreigns, _records, _anchors) = elaborate_and_zonk(
            crate::DEFAULT_STEP_BUDGET,
            predecessors,
            &SYNTAX,
            &entrypoint,
            &RootSource::none(),
            EntryTail::Authored,
            &mut |_| {},
        )
        .expect("the program elaborates");
        let program = Zonked::project(&program).expect("the elaborated program is zonked");

        erase_program(
            budget,
            SYNTAX,
            Erasures::of(&predecessors.cores(), &predecessors.erased()),
            &program,
        )
        .is_ok()
    })
}

/// Sixteen families that name nothing of one another are taken under the budget one of them is: in either checker, what decides a family's vectors, its sizing and its obligations spends that family's budget, not one the unit shares or one its scope's families draw on.
#[test]
fn a_unit_takes_its_families_under_the_budget_one_of_them_takes() {
    with_prelude(|prelude| {
        let one = smallest(|budget| checks(&families(1), budget, prelude));

        assert!(
            checks(&families(16), one, prelude),
            "sixteen independent families are refused under {one}, which one of them is taken under"
        );
    });
}

/// A program is taken under the budget it takes whatever else its scope holds: a unit of families and witnesses it never asks for is brought in on no declaration's budget, and judged by no walk but its own.
#[test]
fn a_program_is_taken_under_the_same_budget_whatever_its_scope_holds() {
    let mut library = String::from("use /std/{Show, Str};\n");
    for index in 1..=128 {
        writeln!(
            library,
            "pub induct B{index}: pub Type\n| b{index}()\nend\nsatisfy Show(B{index}) {{\n    show(_) = \"b\",\n}}"
        )
        .unwrap();
    }
    let program = "/std/print(\"Hello, Curios!\\n\")\n";

    with_prelude(|prelude| {
        let modules = mounted("lib", &library);
        let library = compile_units(
            crate::DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &[(UnitSource::mounted(&modules), None)],
            None,
            |_| {},
        )
        .map_err(String::from)
        .expect("the library compiles")
        .pop()
        .expect("one unit was compiled");
        let mut wider = prelude.to_vec();
        wider.push(&library);

        let alone = smallest(|budget| checks(program, budget, prelude));

        assert!(
            checks(program, alone, &wider),
            "a program taken under {alone} is refused under it once its scope holds a unit it does not read"
        );
    });
}

/// Sixteen values that name nothing of one another erase under the budget one of them does.
#[test]
fn a_unit_erases_its_values_under_the_budget_one_of_them_takes() {
    let one = smallest(|budget| erases(&values(1), budget));

    assert!(
        erases(&values(16), one),
        "sixteen independent values are refused by erasure under {one}, which one of them erases under"
    );
}
