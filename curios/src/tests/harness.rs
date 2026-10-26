//! The test harness across the stages: the `Test` description, its combinators, `Test/main`'s dispatch, the `Spell` witnesses the reports rely on, the `test` declaration, and the synthesized tail a unit's test program runs.

mod resource_tests;
mod sequence_tests;

use {
    super::{
        error, ersd_optm, ersd_optm_tests, run, run_tests_program, run_text, test_program,
        tests_error,
    },
    curios_native::MockHost,
    curios_pipeline::{DEFAULT_BUDGET, Fold},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::{Qualifier, RootKind},
};

#[test]
fn the_description_schedules_every_rung() {
    // The end-to-end scheduler, through the library: no index argument runs every test in declaration order. Two rungs, since a description is a verdict or an action — a proof is a `let` the kernel checks and never reaches a runner.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Eq, Test};
        let double(n: Nat) -> Nat = n * 2;
        let an_action() -> Io(Test({})) =
            let s = Io/pure("x")!;
            Io/pure(Test/equal(s, "x"));
        let entry() -> /std/Io({}) =
            let _ = Test/main([
                ("/tests/a_bool", () => Test/assert(1 + 1 == 2)),
                ("/tests/doubling", () => Test/equal(double(21), 42)),
                ("/tests/a_failing_equal", () => Test/equal(double(2), 5)),
                ("/tests/an_action", () => Test/perform(an_action)),
                ("/tests/a_failing_action", () => Test/perform(() => Io/pure(Test/assert(false)))),
            ])!;
            Io/pure(());
        entry()
        "#),
        b"/tests/a_bool: passed\n/tests/doubling: passed\n/tests/a_failing_equal: failed\n  expected 5 but got 4\n/tests/an_action: passed\n/tests/a_failing_action: failed\n  the condition was false\n"
    );
}

#[test]
fn try_runs_a_fallible_action_and_fails_with_the_error_shown() {
    // `try`'s thunk is a `Try` region over `Io`: a fallible operation sequences bare, a success yields the inner test, and a raise is a failure carrying the error's `Show`.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Try, Path, fs, Test};
        let exists() -> Try(Io, Io/Error, Test({})) =
            let here = fs/exists(Path/of_str("data"))!;
            Try/pure(Test/assert(here == false));
        let missing() -> Try(Io, Io/Error, Test({})) =
            let m = fs/stat(Path/of_str("nope"))!;
            Try/pure(Test/assert(true));
        let entry() -> /std/Io({}) =
            let _ = Test/main([
                ("/tests/exists", () => Test/try(exists)),
                ("/tests/missing", () => Test/try(missing)),
            ])!;
            Io/pure(());
        entry()
        "#),
        b"/tests/exists: passed\n/tests/missing: failed\n  not_found\n"
    );
}

#[test]
fn an_index_argument_selects_one_test() {
    // The runner's protocol: argv[1] names the test to run, and only its line is printed.
    let (system, io) = MockHost::builder().args(["prog", "1"]).build();
    run_text(
        r#"
        use /std/{Nat, Io, Test};
        let entry() -> /std/Io({}) =
            let _ = Test/main([
                ("/tests/first", () => Test/assert(true)),
                ("/tests/second", () => Test/assert(2 == 2)),
            ])!;
            Io/pure(());
        entry()
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"/tests/second: passed\n");
}

#[test]
fn spelled_strings_are_quoted_and_escaped() {
    // `Show(Str)` is the identity, which made string reports ambiguous; `Spell` quotes and escapes, so `"a"` and `"a\n"` are distinct report text.
    assert_eq!(
        run(r#"
        use /std/{Str, Io, Test};
        let entry() -> /std/Io({}) =
            let _ = Test/main([("/tests/newline", () => Test/equal("a\n", "a"))])!;
            Io/pure(());
        entry()
        "#),
        b"/tests/newline: failed\n  expected \"a\" but got \"a\\n\"\n"
    );
}

#[test]
fn a_conjunction_takes_the_weakest_rung_and_names_the_first_failure() {
    // `Test/all` at every rung: passing verdicts conjoin to a passing verdict, the empty conjunction included; the first failure is the report, positioned; an action performs before what follows it is consulted, and a failure before an action leaves the action unrun.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Eq, Test};
        let counter() -> Io(Test({})) =
            let _ = /std/print("performed\n")!;
            Io/pure(Test/assert(true));
        let entry() -> /std/Io({}) =
            let _ = Test/main([
                ("/tests/passing", () => Test/all([Test/equal(1, 1), Test/equal(2, 2)])),
                ("/tests/empty", () => Test/all([])),
                ("/tests/first_failure", () => Test/all([Test/assert(true), Test/equal(2, 3), Test/equal(4, 5)])),
                ("/tests/action_then_verdict", () => Test/all([Test/perform(counter), Test/assert(true)])),
                ("/tests/failure_before_action", () => Test/all([Test/assert(false), Test/perform(counter)])),
            ])!;
            Io/pure(());
        entry()
        "#),
        b"/tests/passing: passed\n/tests/empty: passed\n/tests/first_failure: failed\n  case 1: expected 3 but got 2\nperformed\n/tests/action_then_verdict: passed\n/tests/failure_before_action: failed\n  case 0: the condition was false\n"
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
            Test/assert(n + m == m + n);
        let small(n: Nat) -> Test({}) =
            Test/assert(n < 3);
        let cases: List({Nat, Nat}) = [(1, 2), (3, 4)];
        test table =
            Test/all(List/map(cases, ((a, b)) => add_commutes(a, b)));
        test failing_table =
            Test/all(List/map([0, 1, 5], small));
        /std/print("ran\n")
        "#
        ),
        b"/table: passed\n/failing_table: failed\n  case 2: the condition was false\n"
    );
}

#[test]
fn a_description_is_matched_only_by_its_own_module() {
    // `Test`'s representation is private to `/std/Test`: a consumer builds descriptions through the combinators and cannot eliminate one.
    let report = error(
        r#"
        use /std/{Io, Test};
        let t: Test({}) = Test/assert(true);
        match t | success(_) => /std/print("leaked\n") | _ => /std/print("hidden\n") end
        "#,
    );
    assert!(
        report.contains("representation of type 'Test' is private"),
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
        run(r#"
        use /std/{Nat, Io, Option, Test};
        let entry() -> /std/Io({}) =
            let _ = Test/main([("/tests/options", () => Test/equal(Option/some(3), Option/some(4)))])!;
            Io/pure(());
        entry()
        "#),
        b"/tests/options: failed\n  expected Option/some(4) but got Option/some(3)\n"
    );
}

#[test]
fn a_test_declaration_compiles_beside_the_entry() {
    // A `test` declaration is an ordinary definition of kind `Test` — the program's authored entry still runs, and the unreferenced test rides through elaboration, certification and erasure without disturbing it.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Test};
        test the_answer_holds =
            Test/assert(42 == 42);
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

// A test program is judged as any program is, the tests its synthesized tail schedules with it, and a build holds no test to judge: the body is refused where the test program is compiled and nowhere else. It is one the kernel alone once refused — a bound filled through a recursive member's own body — and the elaborator refuses it now; no term only the kernel refuses is known to put in its place.
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
            Test/assert(r(2) == 1);
        /std/print("built\n")
        "#;

    let report = tests_error(source);
    assert!(
        report.contains("while elaborating /judged")
            && report
                .contains("nothing discharged /std/Bool/Holds(r(0) < List/len(@Nat, [1, 2, 3]))"),
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
            let _ = Test/assert(n * 2 == 42)!;
            let _ = print("before|")!;
            let _ = Io/yield()!;
            Test/assert(Io/pure(true)!);
        ()
        "#,
        ),
        b"before|/t: passed\n"
    );
}

/// Whether any optimized function's debug name starts with `needle` — presence of a definition in the arena, by the name erasure stamped.
fn survives(module: &curios_ersd::Module, needle: &str) -> bool {
    module.functions().iter().flatten().any(|function| {
        function
            .debug_name
            .as_deref()
            .is_some_and(|name| name.starts_with(needle))
    })
}

#[test]
fn declared_tests_schedule_through_the_synthesized_tail() {
    // The unit compiled as its own test program runs every declared test in declaration order — a private `mod`'s included — under the synthesized `Test/main` tail, and the authored entry does not run.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, Io, Eq, Test};
        mod checks
            use /std/{Nat, Test};
            test addition_holds = Test/assert(1 + 1 == 2);
        end
        test the_answer_holds = Test/assert(21 * 2 == 42);
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
    assert!(program.records.is_empty() && program.declared.is_empty());
    assert!(program.compiled.is_none());
}

#[test]
fn a_selection_schedules_the_tests_at_or_under_any_of_its_prefixes() {
    // A whole segment at a time: `/Map` selects `/Map` and what is under it, and never `/MapBuilder`. What the tail does not schedule is still declared, for a selection meeting nothing to be answered with.
    let entrypoint = r#"
        use /std/{Test};
        test Map = Test/assert(true);
        mod Map
            use /std/{Test};
            test inner = Test/assert(true);
        end
        mod MapBuilder
            use /std/{Test};
            test beside = Test/assert(true);
        end
        mod Set
            use /std/{Test};
            test member = Test/assert(true);
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
                .declared
                .iter()
                .map(Qualifier::join)
                .collect::<Vec<_>>(),
            ["/Map", "/Map/inner", "/MapBuilder/beside", "/Set/member"]
        );

        program
            .records
            .into_iter()
            .map(|record| record.path)
            .collect::<Vec<_>>()
    };

    assert_eq!(selected(&[&["Map"]]), ["/Map", "/Map/inner"]);
    assert_eq!(
        selected(&[&["Map", "inner"], &["Set"]]),
        ["/Map/inner", "/Set/member"]
    );
    assert!(selected(&[&["Nothing"]]).is_empty());
}

#[test]
fn the_ordinary_program_holds_no_test_the_test_program_keeps() {
    // The same unit compiles to two programs: the ordinary one is built and holds none of its tests, and the test program's is tested and its tail references every one, which is what keeps them.
    let source = r#"
        use /std/{Nat, Str, Io, Test};
        test the_answer_holds = Test/assert(42 == 42);
        /std/print("ran\n")
        "#;
    assert!(!survives(&ersd_optm(source), "/the_answer_holds"));
    assert!(survives(&ersd_optm_tests(source), "/the_answer_holds"));
}

#[test]
fn a_body_written_as_a_whole_term_form_is_recorded() {
    // The runner slices a test's whole body from the generated thunk's extent, independently of the narrower action extent its interior may carry for a sequencing diagnostic.
    let entrypoint = r#"
        use /std/{Nat, Str, Bool, Io, Test};
        test application =
            Test/assert(1 + 1 == 2);
        test matching =
            match 1 + 1 == 2 : (_) => Test({}) | true => Test/assert(true) | false => Test/assert(false) end;
        test binding =
            let x = 1 + 1;
            Test/assert(x == 2);
        test sequencing =
            Test/assert(Io/pure(true)!);
        test binding_actions =
            let x = Test/pure(2)!;
            Test/assert(x == 2);
        /std/print("ran\n")
        "#
    .parse::<Entrypoint>()
    .expect("fixture parses");
    let records = Fold::new(DEFAULT_BUDGET, &[], None)
        .tests(
            &entrypoint,
            &RootSource::none().tested(),
            vec![Qualifier::empty()],
            |_| {},
            |_| {},
        )
        .expect("fixture compiles as a test program")
        .records;

    assert_eq!(
        records
            .iter()
            .map(|record| record.body.as_str())
            .collect::<Vec<_>>(),
        [
            "Test/assert(1 + 1 == 2)",
            "match 1 + 1 == 2 : (_) => Test({}) | true => Test/assert(true) | false => Test/assert(false) end",
            "let x = 1 + 1;\n            Test/assert(x == 2)",
            "Test/assert(Io/pure(true)!)",
            "let x = Test/pure(2)!;\n            Test/assert(x == 2)",
        ]
    );
}

/// A test program schedules the tests of the one unit compiled tested and of none it stands on, wherever the fold places that unit: a unit built for the ones after it holds no test, so a dependency's never reach a dependent's schedule.
#[test]
fn a_test_program_schedules_only_the_tested_units_tests() {
    let unit = |prefix: &str, test: &str| {
        let mut unit = RootSource::supplied();
        unit.insert_root(
            prefix,
            RootKind::Ordinary,
            format!("use /std/{{Test}};\n\ntest {test} =\n    Test/assert(true);\n")
                .parse()
                .expect("the unit parses"),
        );
        unit
    };
    let scheduled = |units: &[RootSource], entrypoint: &Entrypoint, loader: &RootSource| {
        let records = Fold::new(DEFAULT_BUDGET, units, None)
            .tests(entrypoint, loader, vec![Qualifier::empty()], |_| {}, |_| {})
            .expect("the test program compiles")
            .records;

        records
            .into_iter()
            .map(|record| record.path)
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

    let program = "use /std/{Test};\ntest the_program_holds = Test/assert(true);\n()"
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
