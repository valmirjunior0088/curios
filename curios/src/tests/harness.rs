//! The test-harness library half: the `Test` description, its combinators, `Test/main`'s dispatch, and the `Spell` witnesses the reports rely on.

use {
    super::{error, run, run_text},
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
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Option, Result, Order, Io, Spell, print};
        let line(s: Str) -> Io({}) = print(Str/concat(s, "\n"));
        let _ = line(Spell/spell(Option/some(3)))!;
        let _ = line(Spell/spell(Option/none(@Nat)))!;
        let _ = line(Spell/spell(Result/failure(@Nat, "why")))!;
        let _ = line(Spell/spell(Order/lt()))!;
        Io/pure(())
        "#),
        b"/std/Option/some(3)\n/std/Option/none()\n/std/Result/failure(\"why\")\n/std/Order/lt()\n"
    );
}

#[test]
fn a_structural_fixture_compares_and_spells() {
    // The `/std` gap closed: `Option`, `Result` and `Order` carry `Eql` and `Spell`, so `Test/equal` works on them out of the box.
    assert_eq!(
        run(r#"
        use /std/{Nat, Io, Option, Test};
        Test/main([("/tests/options", () => Test/equal(Option/some(3), Option/some(4)))])
        "#),
        b"/tests/options: failed\n  expected /std/Option/some(4) but got /std/Option/some(3)\n"
    );
}
