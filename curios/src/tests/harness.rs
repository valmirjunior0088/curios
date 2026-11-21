//! The test harness across the stages: the `Test` description, its combinators, `Test/main`'s dispatch, the `Spell` witnesses the reports rely on, the `test` declaration, and the synthesized tail a unit's test program runs.

mod resource;
mod sequence;

use {
    super::test_support::*,
    crate::{FailedCheck, Outcome, test_support::*},
    curios_common::{Qualifier, RootKind},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{ForeignBindings, TestEnd, test_support::*},
    curios_pipeline::Fold,
    curios_surface::Entrypoint,
};

#[test]
fn the_description_schedules_every_rung() {
    // The end-to-end scheduler, through the library: each declared test run by its index, in declaration order. Two rungs, since a description is an outcome or an action — a proof is a `let` the certifier checks and never reaches a runner.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, Io, Test};
        let double(n: Nat) -> Nat = n * 2;
        test a_bool = Test/assert("one and one are two", 1 + 1 == 2);
        test doubling = Test/equal(double(21), 42);
        test a_failing_equal = Test/equal(double(2), 5);
        test an_action =
            let s = Io/pure("x")!;
            Test/equal(s, "x");
        test a_failing_action =
            let _ = Io/pure(())!;
            Test/assert("the action's check fails", false);
        ()
        "#
        ),
        b"/a_bool: passed\n/doubling: passed\n/a_failing_equal: failed\n  not equal\n    obtained: `4`\n    expected: `5`\n/an_action: passed\n/a_failing_action: failed\n  the action's check fails\n"
    );
}

#[test]
fn try_runs_a_fallible_action_and_fails_with_the_error_shown() {
    // `try`'s thunk is a `Try` region over `Io`: a fallible operation sequences bare, a success yields the inner test, and a raise is a failure carrying the error's `Show`.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, Io, Try, Path, fs, Test};
        let exists() -> Try(Io, Io/Error, Test({})) =
            let here = fs/exists(Path/of_str("data"))!;
            Try/pure(Test/assert("the file is absent", here == false));
        let missing() -> Try(Io, Io/Error, Test({})) =
            let m = fs/stat(Path/of_str("nope"))!;
            Try/pure(Test/assert("the stat answered", true));
        test an_absent_file = Test/try(exists);
        test a_missing_file = Test/try(missing);
        ()
        "#
        ),
        b"/an_absent_file: passed\n/a_missing_file: failed\n  not_found\n"
    );
}

#[test]
fn an_index_argument_selects_one_test() {
    // The runner's protocol: argv[1] names the test to run, which answers what it found and writes nothing of it, since whoever selected it names it, says how it ended and presents what it found. An index the program does not hold answers no verdict, which the runner reports as its own fault.
    let (module, foreigns) = test_program(
        r#"
        use /std/{Nat, Test};
        test first = Test/assert("the first test holds", true);
        test second = Test/equal(1 + 1, 3);
        ()
        "#,
    )
    .compiled
    .expect("a test program scheduling a test is built");
    let compiled = to_compiled(&module, foreigns).expect("the test program compiles");
    let selected = |index: &str| {
        let (system, io) = MockHost::builder().args(["prog", index]).build();
        let Ok(TestEnd::Answered(answer)) = compiled.run_test(system, ForeignBindings::empty())
        else {
            panic!("the test at {index} answers");
        };
        assert_eq!(io.output(), b"", "the test at {index} writes nothing");

        Outcome::read(&answer)
    };

    assert!(selected("0").is_ok_and(|outcome| outcome.held()));
    assert_eq!(
        selected("1").map(|outcome| outcome.failures),
        Ok(vec![FailedCheck {
            steps: vec![],
            statement: "not equal".to_string(),
            facts: vec![
                ("obtained".to_string(), "2".to_string()),
                ("expected".to_string(), "3".to_string()),
            ],
        }])
    );
    assert!(selected("2").is_err());
}

#[test]
fn spelled_strings_are_quoted_and_escaped() {
    // `Show(Str)` is the identity, which made string reports ambiguous; `Spell` quotes and escapes, so `"a"` and `"a\n"` are distinct report text.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Str, Test};
        test newline = Test/equal("a\n", "a");
        ()
        "#
        ),
        b"/newline: failed\n  not equal\n    obtained: `\"a\\n\"`\n    expected: `\"a\"`\n"
    );
}

#[test]
fn a_conjunction_takes_the_weakest_rung_and_reports_every_failure() {
    // `Test/all` at every rung: passing outcomes conjoin to a passing outcome, the empty conjunction included; every failing case is reported, each by its position; an action performs before what follows it is consulted, and a case after a failing one still runs, its action with it.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, Io, Test};
        let counter() -> Test({}) =
            let _ = /std/print("performed\n")!;
            Test/assert("the counter ran", true);
        test passing = Test/all([Test/equal(1, 1), Test/equal(2, 2)]);
        test empty = Test/all([]);
        test first_failure = Test/all([Test/assert("the first case holds", true), Test/equal(2, 3), Test/equal(4, 5)]);
        test action_then_verdict = Test/all([counter(), Test/assert("the verdict after the action holds", true)]);
        test failure_before_action = Test/all([Test/assert("the first case fails", false), counter()]);
        ()
        "#
        ),
        b"/passing: passed\n/empty: passed\n/first_failure: failed\n  case 1: not equal\n    obtained: `2`\n    expected: `3`\n  case 2: not equal\n    obtained: `4`\n    expected: `5`\nperformed\n/action_then_verdict: passed\nperformed\n/failure_before_action: failed\n  case 0: the first case fails\n"
    );
}

#[test]
fn a_failure_is_reported_under_every_step_that_reached_it() {
    // `Test/step` names a part of a test, its label leading each failure the part reports, ahead of the steps within it, a table's case among them.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Test};
        test nested =
            Test/step("parsing", Test/step("headers", Test/all([Test/equal(1, 1), Test/equal(2, 3)])));
        ()
        "#
        ),
        b"/nested: failed\n  parsing: headers: case 1: not equal\n    obtained: `2`\n    expected: `3`\n"
    );
}

#[test]
fn a_declared_test_applied_to_a_table_is_one_test() {
    // An open claim is an ordinary definition: it takes its arguments, is not scheduled, and owes no proof, because it is not a claim about anything until it is instantiated. The test is the table that applies it to the author's own cases, and a case's failure carries both its position and the inner report.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, List, Io, Test};
        let add_commutes(n: Nat, m: Nat) -> Test({}) =
            Test/assert("addition commutes", n + m == m + n);
        let small(n: Nat) -> Test({}) =
            Test/assert("the case is small", n < 3);
        let cases: List({Nat, Nat}) = [(1, 2), (3, 4)];
        test table =
            Test/all(List/map(cases, ((a, b)) => add_commutes(a, b)));
        test failing_table =
            Test/all(List/map([0, 1, 5], small));
        /std/print("ran\n")
        "#
        ),
        b"/table: passed\n/failing_table: failed\n  case 2: the case is small\n"
    );
}

#[test]
fn a_description_is_matched_only_by_its_own_module() {
    // `Test`'s representation is private to `/std/Test`: a consumer builds descriptions through the combinators and cannot eliminate one.
    let report = error(
        r#"
        use /std/{Io, Test};
        let t: Test({}) = Test/assert("it holds", true);
        match t | success(_) => /std/print("leaked\n") | _ => /std/print("hidden\n") end
        "#,
    );
    assert!(
        report.contains("representation of `Test` is private"),
        "{report}"
    );
}

#[test]
fn numerals_and_booleans_spell_as_their_literals() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Int, Bool, Str, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let i: Int = 3;
        let entry() -> /std/Io({}) =
            let _ = line(Spell/spell(42))!;
            let _ = line(Spell/spell(-7))!;
            let _ = line(Spell/spell(i))!;
            let _ = line(Spell/spell(true))!;
            Io/pure(());
        entry()
        "#),
        b"42\n-7\n+3\ntrue\n"
    );
}

/// Every one of the four cases, because the witness decides them.
///
/// The three non-finites are decided from the value rather than by string-matching `Flt/to_str`'s output, where a rendering change could silently send one down the numeral path and produce text that is no `Flt` literal; both infinities are here so neither can drift alone. The last row is an overflowing multiply, which is how a non-finite reaches a report without anyone spelling one.
#[test]
fn floats_and_their_non_finite_values_spell_as_literals() {
    assert_eq!(
        run(r#"
        use /std/{Flt, Str, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let entry() -> /std/Io({}) =
            let _ = line(Spell/spell(2.5))!;
            let _ = line(Spell/spell(-2.5))!;
            let _ = line(Spell/spell(100.0))!;
            let _ = line(Spell/spell(-0.0))!;
            let _ = line(Spell/spell(+nan.0))!;
            let _ = line(Spell/spell(+inf.0))!;
            let _ = line(Spell/spell(-inf.0))!;
            let _ = line(Spell/spell(Flt/mul(1.0e200, 1.0e200)))!;
            Io/pure(());
        entry()
        "#),
        concat!(
            "2.5\n-2.5\n100.0\n-0.0\n",
            "+nan.0\n+inf.0\n-inf.0\n+inf.0\n"
        )
        .as_bytes()
    );
}

#[test]
fn text_spells_quoted_and_escaped() {
    assert_eq!(
        run(r#"
        use /std/{Char, Str, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let entry() -> /std/Io({}) =
            let _ = line(Spell/spell('c'))!;
            let _ = line(Spell/spell('\n'))!;
            let _ = line(Spell/spell("a\"b\n"))!;
            Io/pure(());
        entry()
        "#),
        b"'c'\n'\\n'\n\"a\\\"b\\n\"\n"
    );
}

#[test]
fn sequences_spell_as_their_bracketed_literals() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Bytes, List, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let entry() -> /std/Io({}) =
            let _ = line(Spell/spell(x[72, 255]))!;
            let _ = line(Spell/spell([1, 2, 3]))!;
            Io/pure(());
        entry()
        "#),
        b"x[72, 255]\n[1, 2, 3]\n"
    );
}

#[test]
fn structural_values_spell_as_type_qualified_constructor_paths() {
    // The declaration lives at `/sys/Option`, while its derived spelling is `Option/some`, through the public facade.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Option, Result, Ordering, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let entry() -> /std/Io({}) =
            let _ = line(Spell/spell(Option/some(3)))!;
            let _ = line(Spell/spell(Option/none(@Nat)))!;
            let _ = line(Spell/spell(Result/failure(@Str, @Nat, "why")))!;
            let _ = line(Spell/spell(Ordering/lt()))!;
            Io/pure(());
        entry()
        "#),
        b"Option/some(3)\nOption/none()\nResult/failure(\"why\")\nOrdering/lt()\n"
    );
}

#[test]
fn a_structural_fixture_compares_and_spells() {
    // `Option`, `Result` and `Ordering` carry `Eql` and `Spell`, so `Test/equal` works on them out of the box.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Option, Test};
        test options = Test/equal(Option/some(3), Option/some(4));
        ()
        "#
        ),
        b"/options: failed\n  not equal\n    obtained: `Option/some(3)`\n    expected: `Option/some(4)`\n"
    );
}

#[test]
fn a_test_declaration_compiles_beside_the_entry() {
    // A `test` declaration is an ordinary definition of kind `Test` — the program's authored entry still runs, and the unreferenced test rides through elaboration, certification and erasure without disturbing it.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Test};
        test the_answer_holds =
            Test/assert("the answer holds", 42 == 42);
        /std/print("ran\n")
        "#),
        b"ran\n"
    );
}

#[test]
fn a_test_body_is_checked_against_the_description_type() {
    // The declared type is `() -> /std/Test({})` whatever the body: a `Nat` body is a type error at the declaration, not a value the runner later chokes on.
    let error = tests_error(
        r#"
        use /std/{Nat, Str, Io, Test};
        test nope =
            42;
        /std/print("ran\n")
        "#,
    );
    assert!(error.contains("Test"), "unexpected error: {error}");
}

// A test program is judged as any program is, the tests its synthesized tail schedules with it, and a build holds no test to judge: the body is refused where the test program is compiled and nowhere else. It is one the certifier alone once refused — a bound filled through a recursive member's own body — and the elaborator refuses it now; no term only the certifier refuses is known to put in its place.
#[test]
fn a_test_program_is_judged_and_a_build_holds_none_of_its_tests() {
    let source = r#"
        use /std/{Nat, List, Test};
        test judged =
            let r(n: Nat) -> Nat =
                match n
                | 0 => 0
                | _ + 1 => List/get([1, 2, 3], r(0))
                end;
            Test/assert("the recursion answers one", r(2) == 1);
        /std/print("built\n")
        "#;

    let report = tests_error(source);
    assert!(
        report.contains("List/get([1, 2, 3], r(0))")
            && report
                .contains("nothing discharged `/std/Bool/Holds(r(0) < List/len(@Nat, [1, 2, 3]))`"),
        "{report}"
    );
    assert_eq!(run(source), b"built\n");
}

#[test]
fn a_test_body_sequences_assertions_and_lifted_io() {
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Io, Test, print};
        test t =
            let n = Test/pure(21)!;
            let _ = Test/assert("twenty-one doubles to forty-two", n * 2 == 42)!;
            let _ = print("before|")!;
            let _ = Io/yield()!;
            Test/assert("the action answers true", Io/pure(true)!);
        ()
        "#,
        ),
        b"before|/t: passed\n"
    );
}

/// Whether any optimized function's debug name starts with `needle` — presence of a definition in the arena, by the name erasure stamped.
fn survives(module: &curios_anf::Module, needle: &str) -> bool {
    module.functions().iter().flatten().any(|function| {
        function
            .debug_name
            .as_deref()
            .is_some_and(|name| name.starts_with(needle))
    })
}

#[test]
fn declared_tests_schedule_through_the_synthesized_tail() {
    // The unit compiled as its own test program runs every declared test module by module in declaration order — a private `mod`'s included — under the synthesized `Test/main` tail, and the authored entry does not run.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, Io, Eq, Test};
        mod checks
            use /std/{Nat, Test};
            test addition_holds = Test/assert("one and one are two", 1 + 1 == 2);
        end
        test the_answer_holds = Test/assert("twenty-one doubled is forty-two", 21 * 2 == 42);
        /std/print("ran\n")
        "#
        ),
        b"/checks/addition_holds: passed\n/the_answer_holds: passed\n"
    );
}

#[test]
fn a_unit_with_no_tests_builds_no_program() {
    // `Test/main([])` replaces the authored tail and is judged, and a program that would run nothing is built no further.
    let program = test_program(r#"/std/print("ran\n")"#);
    assert!(program.modules.is_empty());
    assert!(program.compiled.is_none());
}

#[test]
fn a_selection_schedules_the_tests_at_or_under_any_of_its_prefixes() {
    // A whole segment at a time: `/Map` selects `/Map` and what is under it, and never `/MapBuilder`. What the tail does not schedule is still declared, and each prefix that selects nothing is named, for a selection meeting nothing to be refused with the paths nearest it.
    let entrypoint = r#"
        use /std/{Test};
        test Map = Test/assert("it holds", true);
        mod Map
            use /std/{Test};
            test inner = Test/assert("it holds", true);
        end
        mod MapBuilder
            use /std/{Test};
            test beside = Test/assert("it holds", true);
        end
        mod Set
            use /std/{Test};
            test member = Test/assert("it holds", true);
        end
        ()
        "#
    .parse::<Entrypoint>()
    .expect("fixture parses");
    let selected = |under: &[&[&str]]| {
        let program = Fold::new(DEFAULT_BUDGET, &[], None)
            .tests(
                &entrypoint,
                &RootSource::none().tested(),
                under
                    .iter()
                    .map(|prefix| Qualifier::from(prefix.iter().copied()))
                    .collect(),
                |_| {},
                |_| {},
            )
            .expect("fixture compiles as a test program");
        assert_eq!(
            program
                .modules
                .iter()
                .flat_map(|module| {
                    module
                        .scheduled
                        .iter()
                        .map(|test| test.path)
                        .chain(module.skipped.iter().copied())
                })
                .map(|path| path.join())
                .collect::<Vec<_>>(),
            ["/Map", "/Map/inner", "/MapBuilder/beside", "/Set/member"]
        );

        let scheduled = program
            .modules
            .iter()
            .flat_map(|module| &module.scheduled)
            .map(|test| test.path.join())
            .collect::<Vec<_>>();
        let unmatched = program
            .unmatched
            .iter()
            .map(Qualifier::join)
            .collect::<Vec<_>>();

        (scheduled, unmatched)
    };

    let (scheduled, unmatched) = selected(&[&["Map"]]);
    assert_eq!(scheduled, ["/Map", "/Map/inner"]);
    assert!(unmatched.is_empty());
    let (scheduled, unmatched) = selected(&[&["Map", "inner"], &["Set"]]);
    assert_eq!(scheduled, ["/Map/inner", "/Set/member"]);
    assert!(unmatched.is_empty());
    let (scheduled, unmatched) = selected(&[&["Set"], &["Nothing"]]);
    assert_eq!(scheduled, ["/Set/member"]);
    assert_eq!(unmatched, ["/Nothing"]);
}

#[test]
fn the_ordinary_program_holds_no_test_the_test_program_keeps() {
    // The same unit compiles to two programs: the ordinary one is built and holds none of its tests, and the test program's is tested and its tail references every one, which is what keeps them.
    let source = r#"
        use /std/{Nat, Str, Io, Test};
        test the_answer_holds = Test/assert("the answer holds", 42 == 42);
        /std/print("ran\n")
        "#;
    assert!(!survives(&anf_optm(source), "/the_answer_holds"));
    assert!(survives(&anf_optm_tests(source), "/the_answer_holds"));
}

/// A test program schedules the tests of the one unit compiled tested and of none it stands on, wherever the fold places that unit: a unit built for the ones after it holds no test, so a dependency's never reach a dependent's schedule.
#[test]
fn a_test_program_schedules_only_the_tested_units_tests() {
    let unit = |prefix: &str, test: &str| {
        let mut unit = RootSource::supplied();
        unit.insert_root(
            prefix,
            RootKind::Ordinary,
            format!("use /std/{{Test}};\n\ntest {test} =\n    Test/assert(\"it holds\", true);\n")
                .parse()
                .expect("the unit parses"),
        );
        unit
    };
    let scheduled = |units: &[RootSource], entrypoint: &Entrypoint, loader: &RootSource| {
        let modules = Fold::new(DEFAULT_BUDGET, units, None)
            .tests(entrypoint, loader, vec![Qualifier::empty()], |_| {}, |_| {})
            .expect("the test program compiles")
            .modules;

        modules
            .iter()
            .flat_map(|module| &module.scheduled)
            .map(|test| test.path.join())
            .collect::<Vec<_>>()
    };

    let library = [
        unit("dependency", "the_dependency_holds"),
        unit("library", "the_library_holds").tested(),
    ];
    assert_eq!(
        scheduled(&library, &Entrypoint::trivial(), &RootSource::none()),
        ["/library/the_library_holds"]
    );

    let program = "use /std/{Test};\ntest the_program_holds = Test/assert(\"it holds\", true);\n()"
        .parse::<Entrypoint>()
        .expect("the program parses");
    let built = [
        unit("dependency", "the_dependency_holds"),
        unit("library", "the_library_holds"),
    ];
    assert_eq!(
        scheduled(&built, &program, &RootSource::none().tested()),
        ["/the_program_holds"]
    );
}
