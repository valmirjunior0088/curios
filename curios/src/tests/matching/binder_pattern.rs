//! A constructor written where a binder stands: held to its one case as a match's lone arm is, refining what follows, and refused where its value's type has another case the facts in scope do not refute.

use crate::test_support::*;

// A constructor pattern binds its payloads at a `let`, at a lambda's parameter and at a function's, and `refl()` refines what follows it as an arm of it would: `Eq/refl()` is checked at `Eq()(y, x)` once the case has solved `y` to `x`.
#[test]
fn a_constructor_pattern_binds_its_payloads_and_refines_what_follows() {
    let source = r#"
        use /std/{Nat, Eq};
        induct Pair: Type | pair(Nat, Nat) end
        let sum(p: Pair) -> Nat =
            let pair(a, b) = p;
            a + b;
        let first(pair(a, _): Pair) -> Nat = a;
        let flipped(@x: Nat, @y: Nat, e: Eq()(x, y)) -> Eq()(y, x) =
            let refl() = e;
            Eq/refl();
        let applied(f: (Pair) -> Nat) -> Nat = f(Pair/pair(2, 3));
        /std/print(Nat/to_str(sum(Pair/pair(1, 2)) + first(Pair/pair(4, 0)) + applied((pair(a, b)) => a * b)))
        "#;
    assert_eq!(run(source), b"13");
}

// A constructor whose value's type has another case the facts in scope do not refute is refused as a match missing that arm is, naming the case.
#[test]
fn a_constructor_pattern_whose_value_has_another_case_is_refused_naming_it() {
    let source = r#"
        use /std/{Nat, Option};
        let got(o: Option(Nat)) -> Nat =
            let some(x) = o;
            x;
        /std/print(Nat/to_str(got(Option/some(1))))
        "#;
    let report = error(source);
    assert!(report.contains("no arm matches `none()`"), "{report}");
}

// A constructor pattern is a match's lone arm, so another case the facts in scope refute is closed as that match's would be: under `Holds(Option/is_some(o))`, `let some(x) = o;` binds. The refusal beside it is `a_constructor_pattern_whose_value_has_another_case_is_refused_naming_it`, where no fact refutes `none()`.
#[test]
fn a_constructor_pattern_whose_other_case_the_facts_refute_binds() {
    let source = r#"
        use /std/{Nat, Option};
        use /std/Bool/{Holds};
        let got(o: Option(Nat), @q: Holds(Option/is_some(o))) -> Nat =
            let some(x) = o;
            x;
        /std/print(Nat/to_str(got(Option/some(1))))
        "#;
    assert_eq!(run(source), b"1");
}

// The `->` a function states its result with is what tells a local function from a constructor pattern: written, `one()` is a function; left out where its parameters are typed, the definition is refused for it, as before patterns took constructors.
#[test]
fn a_let_that_states_its_result_defines_a_function_and_one_that_omits_it_is_refused() {
    let defined = r#"
        use /std/{Nat};
        let main() -> /std/Io({}) =
            let one() -> Nat = 1;
            /std/print(Nat/to_str(one()));
        main()
        "#;
    assert_eq!(run(defined), b"1");

    let omitted = r#"
        use /std/{Nat};
        let main() -> /std/Io({}) =
            let double(n: Nat) = n + n;
            /std/print(Nat/to_str(double(1)));
        main()
        "#;
    let report = error(omitted);
    assert!(
        report.contains("a definition with parameters states its result"),
        "{report}"
    );
}
