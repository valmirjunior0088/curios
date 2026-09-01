//! Property-based testing across the stages: the pure splittable `Seed`, the `Draw` roster and the tuple `Spell` witnesses a counterexample renders, `Property` and `Test/property`, and the parameterized `test` declaration the synthesized tail closes through `Test/property`.

use {
    super::{error, run, run_tests_program},
    curios_pipeline::{DEFAULT_STEP_BUDGET, EntryTail, compile_tests_with_units},
    curios_text::{Entrypoint, RootSource},
};

#[test]
fn a_drawn_sequence_is_a_function_of_its_seed() {
    // Pinned from the generator's first run: the same seed yields the same draws run after run, the size bounds a drawn number, and an `Int` spells with its sign as every `Spell` does.
    assert_eq!(
        run(r#"
        use /std/{Nat, Int, Str, List, Spell, Io};
        use /std/Test/{Seed, Draw};
        let draws(@A: Type, use Draw(A), use Spell(A), seed: Seed, size: Nat, count: Nat) -> List(Str) =
            match count
            | 0 => []
            | more + 1 =>
                let (value, rest) = Draw/draw(@A, seed, size);
                [Spell/spell(value), ..draws(@A, rest, size, more)]
            end;
        let seed: Seed = Seed/make(42);
        /std/print(
            Str/join(
                "\n",
                [
                    Str/join(", ", draws(@Nat, seed, 10, 8)),
                    Str/join(", ", draws(@Int, seed, 10, 8)),
                    Str/join(", ", draws(@Str, seed, 6, 4)),
                    ""]))
        "#),
        b"9, 7, 7, 5, 8, 7, 10, 8\n-9, -7, +8, +10, +3, -4, +10, +5\n\"r 61]\", \"\", \"T:v#v\", \"o!1\"\n"
    );
}

#[test]
fn tuple_shapes_draw_and_spell_through_their_premises() {
    // A positional shape composes its fields' witnesses, and `Spell` renders the literal the shape is written as — the one-field literal with the trailing comma that separates it from a parenthesized term.
    assert_eq!(
        run(r#"
        use /std/{Nat, Bool, Char, Str, List, Option, Spell, Io};
        use /std/Test/{Seed, Draw};
        let draws(@A: Type, use Draw(A), use Spell(A), seed: Seed, size: Nat, count: Nat) -> List(Str) =
            match count
            | 0 => []
            | more + 1 =>
                let (value, rest) = Draw/draw(@A, seed, size);
                [Spell/spell(value), ..draws(@A, rest, size, more)]
            end;
        let seed: Seed = Seed/make(42);
        /std/print(
            Str/join(
                "\n",
                [
                    Str/join(", ", draws(@{}, seed, 4, 2)),
                    Str/join(", ", draws(@{Nat}, seed, 4, 3)),
                    Str/join(", ", draws(@{Nat, Bool}, seed, 4, 4)),
                    Str/join(", ", draws(@{Nat, Char, Option(Bool)}, seed, 4, 3)),
                    Spell/spell((1, "a", ('x', true))),
                    ""]))
        "#),
        b"(), ()\n(0,), (2,), (0,)\n(0, true), (0, true), (2, false), (4, false)\n(0, 'r', /std/Option/none()), (2, '1', /std/Option/none()), (4, ':', /std/Option/some(false))\n(1, \"a\", ('x', true))\n"
    );
}

#[test]
fn the_generator_never_leaves_the_envelope() {
    // `Nat` rides an i31 and a result past it traps, so a generator whose intermediates could exceed 2^30 would trap somewhere in a long enough walk; this one masks before every left shift, and a walk of thousands of steps — `next` and both `split` children alike — finishes. The three first draws after a split are also pairwise distinct, which is what makes the children independent cases rather than a replay.
    assert_eq!(
        run(r#"
        use /std/{Nat, Bool, Str, Io};
        use /std/Test/{Seed};
        let walk(seed: Seed, steps: Nat) -> Seed =
            match steps
            | 0 => seed
            | more + 1 =>
                let (_, stepped) = Seed/next(seed);
                let (left, right) = Seed/split(stepped);
                let (_, again) = Seed/next(left);
                walk(match Nat/and(more, 1) == 0 | true => again | false => right end, more)
            end;
        let seed: Seed = walk(Seed/make(7), 5000);
        let (a, _) = Seed/next(seed);
        let (left, right) = Seed/split(seed);
        let (b, _) = Seed/next(left);
        let (c, _) = Seed/next(right);
        /std/print(Str/concat(Bool/to_str(a != b && b != c && a != c), "\n"))
        "#),
        b"true\n"
    );
}

#[test]
fn a_property_is_probed_over_drawn_arguments() {
    // `Test/property` applied by hand, through the harness's own scheduler: a law holds over every drawn case, a claim that fails reports the first counterexample — small, since the size is the case index — spelled in parameter order before the inner report, and a curried spelling resolves through the arity-1 witness twice.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Test};
        let commutes(n: Nat, m: Nat) -> Test = Test/check(n + m == m + n);
        let bounded(n: Nat, m: Nat) -> Test = Test/check(n + m < 7);
        let curried: (Nat) -> (Nat) -> Test = (a) => (b) => Test/equal(a * b, b * a);
        Test/main([
            ("/commutes", () => Test/property(commutes)),
            ("/bounded", () => Test/property(bounded)),
            ("/curried", () => Test/property(curried)),
        ])
        "#),
        b"/commutes: passed\n/bounded: failed\n  for 6, 6: the condition was false\n/curried: passed\n"
    );
}

#[test]
fn a_theorem_bodied_property_passes_without_a_case_failing() {
    // The kernel settled `n + 0 = n` over the open binder once, at elaboration, so the runner meets `theorem()` at every case and has nothing left to decide — and still reports `passed` rather than `proved`, because a property is a verdict decided by running it, whatever each case turned out to be.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Eq, Test};
        let plus_zero(n: Nat) -> Test = Test/refl(n + 0, n, Eq/refl());
        Test/main([("/plus_zero", () => Test/property(plus_zero))])
        "#),
        b"/plus_zero: passed\n"
    );
}

#[test]
fn a_nullary_description_is_its_own_property() {
    // `Property(Test)` is the base of every function-shape witness and stands on its own: a bare description probes as the verdict it carries, and an action is refused rather than performed by the pure runner.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, Test};
        Test/main([
            ("/verdict", () => Test/property(Test/check(2 == 2))),
            ("/action", () => Test/property(Test/perform(() => Io/pure(Test/check(true))))),
        ])
        "#),
        b"/verdict: passed\n/action: failed\n  an action cannot be probed\n"
    );
}

#[test]
fn a_parameter_the_roster_cannot_draw_reports_the_missing_witness() {
    // A dependent telescope never unifies with a function-shape witness, and a parameter type with no `Draw` fails the premise: either way the report names the `Property` goal at the application.
    let error = error(
        r#"
        use /std/{Nat, Str, Io, Test};
        let bounded(n: Nat, p: Nat/Lt(n, 100)) -> Test = Test/check(true);
        Test/main([("/bounded", () => Test/property(bounded))])
        "#,
    );
    assert!(error.contains("Property"), "unexpected error: {error}");
}

#[test]
fn a_parameterized_test_is_probed_through_the_synthesized_tail() {
    // The decision's mission program, and the seam it opens: a `test` with a telescope is a property the tail closes through `Test/property`, reported by the harness's ordinary lines with the counterexample spelled in parameter order — and the nullary test beside it schedules exactly as it did before the seam existed.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, Io, Test};
        test add_commutes(n: Nat, m: Nat) =
            Test/check(n + m == m + n);
        test sum_is_small(n: Nat, m: Nat) =
            Test/check(n + m < 7);
        test the_answer_holds() =
            Test/check(21 * 2 == 42);
        /std/print("ran\n")
        "#
        ),
        b"/add_commutes: passed\n/sum_is_small: failed\n  for 6, 6: the condition was false\n/the_answer_holds: passed\n"
    );
}

#[test]
fn a_property_run_is_a_function_of_the_sources() {
    // Determinism: the same unit compiled and run twice writes the same bytes, since no clock or host entropy reaches a draw — the seed and the case count are constants of the library.
    let source = r#"
        use /std/{Nat, Str, Option, Io, Test};
        test an_option_is_drawn(o: Option(Nat)) =
            Test/check(true);
        test sums_stay_small(n: Nat, m: Nat, k: Nat) =
            Test/check(n + m + k < 12);
        /std/print("ran\n")
        "#;
    let first = String::from_utf8(run_tests_program(source)).expect("the report is text");
    assert_eq!(first, String::from_utf8_lossy(&run_tests_program(source)));
    assert_eq!(
        first,
        "/an_option_is_drawn: passed\n/sums_stay_small: failed\n  for 6, 6, 4: the condition was false\n"
    );
}

#[test]
fn a_property_over_a_sequence_parameter_is_probed() {
    // The four draws that recurse — `Str`, `Bits`, `Bytes`, `List` — once tripped `curios-cont`'s round-boundary verify with a call out of scope: the SCC known-argument fixpoint forwarded the archived draw's function reference into a recursive member that could not name it, and the inliner, which heals that for a non-recursive callee, declines a recursive one. `known_values` now forwards a function reference only within the member's lexical scope, so these compile and run like every other parameter type.
    assert_eq!(
        run_tests_program(
            r#"
        use /std/{Nat, Str, Bits, Bytes, List, Io, Test};
        test concatenating_nothing_keeps_a_string(s: Str) =
            Test/equal(Str/concat(s, ""), s);
        test spreading_bits_keeps_them(b: Bits) =
            Test/equal(b[..b], b);
        test spreading_bytes_keeps_them(b: Bytes) =
            Test/equal(x[..b], b);
        test flattening_keeps_a_lone_list(l: List(Nat)) =
            Test/equal(List/flatten([l, []]), l);
        /std/print("ran\n")
        "#
        ),
        b"/concatenating_nothing_keeps_a_string: passed\n/spreading_bits_keeps_them: passed\n/spreading_bytes_keeps_them: passed\n/flattening_keeps_a_lone_list: passed\n"
    );
}

#[test]
fn a_parameterized_record_carries_its_body_as_written() {
    // The runner prints a failing test's body from the record the compile hands back; a property's body is the lambda's interior under the telescope, sliced from the authored span exactly as a nullary test's is.
    let entrypoint = r#"
        use /std/{Nat, Str, Io, Test};
        test add_commutes(n: Nat, m: Nat) =
            Test/check(n + m == m + n);
        /std/print("ran\n")
        "#
    .parse::<Entrypoint>()
    .expect("fixture parses");
    let (_module, _foreigns, records) = compile_tests_with_units(
        DEFAULT_STEP_BUDGET,
        &[],
        &entrypoint,
        &RootSource::none(),
        None,
        EntryTail::Tests,
        |_| {},
        |_| {},
    )
    .expect("fixture compiles as a test program");
    assert_eq!(records.len(), 1);
    assert_eq!(records[0].path, "/add_commutes");
    assert_eq!(records[0].body, "Test/check(n + m == m + n)");
}

#[test]
fn a_parameter_without_a_draw_is_reported_at_its_declaration() {
    // A dependent telescope unifies with no function-shape witness — outside the roster by design — so the tail's `Property` goal fails, and it is reported at the test as written rather than in a tail the author never sees.
    let entrypoint = r#"
        use /std/{Nat, Str, Io, Test};
        test bounded(n: Nat, p: Nat/Lt(n, 100)) =
            Test/check(n < 100);
        /std/print("ran\n")
        "#
    .parse::<Entrypoint>()
    .expect("fixture parses");
    let error = match compile_tests_with_units(
        DEFAULT_STEP_BUDGET,
        &[],
        &entrypoint,
        &RootSource::none(),
        None,
        EntryTail::Tests,
        |_| {},
        |_| {},
    ) {
        Ok(_) => panic!("the test program compiled"),
        Err(error) => error.to_string(),
    };
    assert!(error.contains("Property"), "unexpected error: {error}");
    assert!(
        error.contains("test bounded(n: Nat, p: Nat/Lt(n, 100))")
            || error.contains("Test/check(n < 100)"),
        "the report does not point at the declaration: {error}"
    );
}
