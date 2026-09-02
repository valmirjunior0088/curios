//! The test harness across the stages: the `Test` description, its combinators, `Test/main`'s dispatch, the `Spell` witnesses the reports rely on, the `test` declaration, and the synthesized tail a unit's test program runs.

use {
    super::{error, ersd_optm, ersd_optm_tests, run, run_tests_program, run_text},
    curios_runtime::MockHost,
};

#[test]
fn the_description_schedules_every_rung() {
    // The end-to-end scheduler probed on 2026-08-31, now through the library: no index argument runs every test in declaration order.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Eq, Test};
        let double(n: Nat) -> Nat = n * 2;
        let an_action() -> Io(Test) =
            let s = Io/pure("x")!;
            Io/pure(Test/equal(s, "x"));
        Test/main([
            ("/tests/doubling", () => Test/refl(double(21), 42, Eq/refl())),
            ("/tests/a_bool", () => Test/check(1 + 1 == 2)),
            ("/tests/a_failing_equal", () => Test/equal(double(2), 5)),
            ("/tests/an_action", () => Test/perform(an_action)),
            ("/tests/a_failing_action", () => Test/perform(() => Io/pure(Test/check(false)))),
        ])
        "#),
        b"/tests/doubling: proved\n/tests/a_bool: passed\n/tests/a_failing_equal: failed\n  expected 5 but got 4\n/tests/an_action: passed\n/tests/a_failing_action: failed\n  the condition was false\n"
    );
}

#[test]
fn an_index_argument_selects_one_test() {
    // The runner's protocol: argv[1] names the test to run, and only its line is printed.
    let (system, io) = MockHost::builder().args(["prog", "1"]).build();
    run_text(
        r#"
        use /std/{Nat, Io, Test};
        Test/main([
            ("/tests/first", () => Test/check(true)),
            ("/tests/second", () => Test/check(2 == 2)),
        ])
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
        Test/main([("/tests/newline", () => Test/equal("a\n", "a"))])
        "#),
        b"/tests/newline: failed\n  expected \"a\" but got \"a\\n\"\n"
    );
}

#[test]
fn a_conjunction_takes_the_weakest_rung_and_names_the_first_failure() {
    // `Test/all` at every rung: theorems alone stay a theorem, the empty conjunction included; a verdict among theorems is the verdict; the first failure is the report, positioned; an action performs before what follows it is consulted, and a failure before an action leaves the action unrun.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Eq, Test};
        let counter() -> Io(Test) =
            let _ = /std/print("performed\n")!;
            Io/pure(Test/check(true));
        Test/main([
            ("/tests/theorems", () => Test/all([Test/refl(1, 1, Eq/refl()), Test/refl(2, 2, Eq/refl())])),
            ("/tests/empty", () => Test/all([])),
            ("/tests/mixed", () => Test/all([Test/refl(1, 1, Eq/refl()), Test/check(true)])),
            ("/tests/first_failure", () => Test/all([Test/check(true), Test/equal(2, 3), Test/equal(4, 5)])),
            ("/tests/action_then_verdict", () => Test/all([Test/perform(counter), Test/check(true)])),
            ("/tests/failure_before_action", () => Test/all([Test/check(false), Test/perform(counter)])),
        ])
        "#),
        b"/tests/theorems: proved\n/tests/empty: proved\n/tests/mixed: passed\n/tests/first_failure: failed\n  case 1: expected 3 but got 2\nperformed\n/tests/action_then_verdict: passed\n/tests/failure_before_action: failed\n  case 0: the condition was false\n"
    );
}

#[test]
fn a_declared_test_applied_to_a_table_is_one_test() {
    // A test is callable by name, so a table of cases is `Test/all` over its applications — the author-supplied domain beside the drawn one — and a case's failure carries both its position and the inner report.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, List, Io, Test};
        test add_commutes(n: Nat, m: Nat) =
            Test/check(n + m == m + n);
        let small(n: Nat) -> Test =
            Test/check(n < 3);
        let cases: List({Nat, Nat}) = [(1, 2), (3, 4)];
        test table() =
            Test/all(List/map(cases, ((a, b)) => add_commutes(a, b)));
        test failing_table() =
            Test/all(List/map([0, 1, 5], small));
        /std/print("ran\n")
        "#
        ),
        b"/add_commutes: passed\n/table: passed\n/failing_table: failed\n  case 2: the condition was false\n"
    );
}

#[test]
fn a_description_is_matched_only_by_its_own_module() {
    // `Test`'s representation is private to `/syn/Test`: a consumer builds descriptions through the combinators and cannot eliminate one.
    let report = error(
        r#"
        use /std/{Io, Test};
        let t: Test = Test/check(true);
        match t | verdict(v) => /std/print("leaked\n") | _ => /std/print("hidden\n") end
        "#,
    );
    assert!(report.contains("Test"), "{report}");
}

#[test]
fn numerals_and_booleans_spell_as_their_literals() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Int, Bool, Str, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let i: Int = 3;
        let _ = line(Spell/spell(42))!;
        let _ = line(Spell/spell(-7))!;
        let _ = line(Spell/spell(i))!;
        let _ = line(Spell/spell(true))!;
        Io/pure(())
        "#),
        b"42\n-7\n+3\ntrue\n"
    );
}

#[test]
fn floats_spell_as_literals_and_non_finites_by_name() {
    assert_eq!(
        run(r#"
        use /std/{Flt, Str, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let _ = line(Spell/spell(2.5))!;
        let _ = line(Spell/spell(Flt/nan))!;
        Io/pure(())
        "#),
        b"2.5\n/std/Flt/nan\n"
    );
}

#[test]
fn text_spells_quoted_and_escaped() {
    assert_eq!(
        run(r#"
        use /std/{Char, Str, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let _ = line(Spell/spell('c'))!;
        let _ = line(Spell/spell('\n'))!;
        let _ = line(Spell/spell("a\"b\n"))!;
        Io/pure(())
        "#),
        b"'c'\n'\\n'\n\"a\\\"b\\n\"\n"
    );
}

#[test]
fn sequences_spell_as_their_bracketed_literals() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Bits, Bytes, List, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let _ = line(Spell/spell(b[0, 1]))!;
        let _ = line(Spell/spell(x[72, 255]))!;
        let _ = line(Spell/spell([1, 2, 3]))!;
        Io/pure(())
        "#),
        b"b[0, 1]\nx[72, 255]\n[1, 2, 3]\n"
    );
}

#[test]
fn structural_values_spell_as_absolute_constructor_paths() {
    // The declaration's own path — `/std/Option/Option/some`, the induct `Option` inside module `/std/Option` — rather than the facade's `/std/Option/some` the written witnesses once spelled: both re-parse, and the derived witness spells the one it reads off the registry.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Option, Result, Ordering, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let _ = line(Spell/spell(Option/some(3)))!;
        let _ = line(Spell/spell(Option/none(@Nat)))!;
        let _ = line(Spell/spell(Result/failure(@Nat, "why")))!;
        let _ = line(Spell/spell(Ordering/lt()))!;
        Io/pure(())
        "#),
        b"/std/Option/Option/some(3)\n/std/Option/Option/none()\n/std/Result/Result/failure(\"why\")\n/std/Ordering/Ordering/lt()\n"
    );
}

#[test]
fn a_structural_fixture_compares_and_spells() {
    // The `/std` gap closed: `Option`, `Result` and `Ordering` carry `Equal` and `Spell`, so `Test/equal` works on them out of the box.
    assert_eq!(
        run(r#"
        use /std/{Nat, Io, Option, Test};
        Test/main([("/tests/options", () => Test/equal(Option/some(3), Option/some(4)))])
        "#),
        b"/tests/options: failed\n  expected /std/Option/Option/some(4) but got /std/Option/Option/some(3)\n"
    );
}

#[test]
fn a_test_declaration_compiles_beside_the_entry() {
    // Step 3's acceptance: a `test` declaration is an ordinary definition of kind `Test` — the program's authored entry still runs, and the unreferenced test rides through elaboration, certification and erasure without disturbing it.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Test};
        test the_answer_holds() =
            Test/check(42 == 42);
        /std/print("ran\n")
        "#),
        b"ran\n"
    );
}

#[test]
fn a_test_body_is_checked_against_the_description_type() {
    // The declared type is `() -> /syn/Test` whatever the body: a `Nat` body is a type error at the declaration, not a value the runner later chokes on.
    let error = error(
        r#"
        use /std/{Nat, Str, Io, Test};
        test nope() =
            42;
        /std/print("ran\n")
        "#,
    );
    assert!(error.contains("Test"), "unexpected error: {error}");
}

#[test]
fn a_bare_bang_in_a_test_body_is_refused() {
    // A test body's region is the description type, and `Test` is no monad: effects enter only through `perform`'s thunk, so a bare `!` is refused where it is written.
    let error = error(
        r#"
        use /std/{Nat, Str, Io, Test};
        test t() =
            Test/check(Io/pure(true)!);
        /std/print("ran\n")
        "#,
    );
    assert!(error.contains("Monad"), "unexpected error: {error}");
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
    // Step 4's acceptance: the unit compiled as its own test program runs every declared test in declaration order — a private `mod`'s included — under the synthesized `Test/main` tail, and the authored entry does not run.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, Io, Eq, Test};
        mod checks
            use /std/{Nat, Test};
            test addition_holds() = Test/check(1 + 1 == 2);
        end
        test the_answer_holds() = Test/refl(21 * 2, 42, Eq/refl());
        /std/print("ran\n")
        "#
        ),
        b"/checks/addition_holds: passed\n/the_answer_holds: proved\n"
    );
}

#[test]
fn a_unit_with_no_tests_runs_nothing() {
    // `Test/main([])`: the authored tail is replaced, nothing is scheduled, nothing is written, and the program exits cleanly.
    assert_eq!(run_tests_program(r#"/std/print("ran\n")"#), b"");
}

#[test]
fn the_ordinary_program_prunes_what_the_test_program_keeps() {
    // The same unit compiles to two programs: the ordinary one neither reaches nor runs its tests, so the prune drops them; the test program's tail references every one, which is what keeps them.
    let source = r#"
        use /std/{Nat, Str, Io, Test};
        test the_answer_holds() = Test/check(42 == 42);
        /std/print("ran\n")
        "#;
    assert!(!survives(&ersd_optm(source), "/the_answer_holds"));
    assert!(survives(&ersd_optm_tests(source), "/the_answer_holds"));
}
