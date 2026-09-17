//! What a refusal at an argument tells the reader about the call it sits in.

use crate::tests::error;

// A lambda handed in where the callee takes its list — the argument order every other language uses for `map` — is refused as a lambda against a non-function type, which is true and not what the reader needs. The report names the parameter the lambda filled and the parameter the callee takes a function as.
#[test]
fn a_lambda_in_the_wrong_position_names_the_parameter_it_filled_and_the_one_it_fits() {
    let report = error(
        r#"
        use /std/{Nat, List};
        let bump(xs: List(Nat)) -> List(Nat) = List/map((x) => x + 1, xs);
        /std/print("ok")
        "#,
    );
    assert!(
        report.contains("checked as `a`, the 1st argument of '/sys/List/map'")
            && report.contains("'/sys/List/map' takes a function as `f`, its 2nd argument"),
        "unexpected report:\n{report}"
    );
}

// A written `@` argument is named among the `@` arguments, which is how it was aligned with its slot. Counting it among the plain ones underflowed when none came before it. The hint about a later function parameter is for swapped plain arguments, and an author who wrote `@` chose the hidden slot on purpose.
#[test]
fn an_implicit_lambda_before_any_plain_argument_is_named_among_the_implicit_ones() {
    let report = error(
        r#"
        use /std/{Nat};
        let f(@n: Nat, k: (Nat) -> Nat) -> Nat = k(n);
        let g: Nat = f(@(x) => x, (y) => y);
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("checked as `n`, the 1st '@' argument of '/f'")
            && !report.contains("takes a function"),
        "unexpected report:\n{report}"
    );
}

#[test]
fn an_implicit_lambda_after_a_plain_argument_is_not_named_by_the_plain_ones_position() {
    let report = error(
        r#"
        use /std/{Nat};
        let f(m: Nat, @n: Nat) -> Nat = m + n;
        let g: Nat = f(1, @(x) => x);
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("checked as `n`, the 1st '@' argument of '/f'"),
        "unexpected report:\n{report}"
    );
}

// The site is worked out for every refused argument, though only a misplaced lambda keeps it, so a plain mismatch at an `@` argument took the same path to the underflow.
#[test]
fn a_mismatched_implicit_argument_before_any_plain_one_is_refused_as_a_mismatch() {
    let report = error(
        r#"
        use /std/{Nat};
        let f(@n: Nat) -> Nat = n;
        let g: Nat = f(@true);
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("type mismatch"),
        "unexpected report:\n{report}"
    );
}

// A `use` parameter is anonymous, so the report names its position alone.
#[test]
fn a_use_lambda_is_named_among_the_use_arguments_without_a_parameter_name() {
    let report = error(
        r#"
        use /std/{Nat, Show};
        let f(use Show(Nat), m: Nat) -> Nat = m;
        let g: Nat = f(use (x) => x, 1);
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("checked as the 1st 'use' argument of '/f'"),
        "unexpected report:\n{report}"
    );
}

// A method wrapper's telescope holds no explicit slot, so its hidden arguments are checked by the saturating walk rather than the main one. The wrapper names its `use` binder `w`, which appears in no program and must not surface.
#[test]
fn a_use_lambda_in_a_leading_hidden_telescope_is_named_too() {
    let report = error(
        r#"
        use /std/{Nat, Show, Str};
        let s: Str = Show/show(use (x) => x, 5);
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("checked as the 1st 'use' argument of '/std/Show/Show/show'")
            && !report.contains("`w`"),
        "unexpected report:\n{report}"
    );
}

// A premise is named by its position among every `use` slot, written or not: supplying the first does not make the second the first.
#[test]
fn a_missing_witness_after_a_written_one_is_named_by_its_own_position() {
    let report = error(
        r#"
        use /std/{Nat, Show, Str};
        struct Foo: Type { Nat }
        let f(use Show(Nat), use Show(Foo)) -> Nat = 0;
        let mine: Show(Nat) = Show { show(_n) = "n" };
        let g: Nat = f(use mine);
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("needed by '/f' for its 2nd 'use' premise"),
        "unexpected report:\n{report}"
    );
}

// An operator has no argument list, so its bound cannot be supplied where it is written. The report names the operator rather than its minted binder, spells the bound as the method it projects rather than the witness it projects from, and names the two ways that do establish it — the method call with its type argument first, since a leading `@` fills that slot.
#[test]
fn an_operators_undischarged_bound_names_the_operator_and_what_establishes_it() {
    let report = error(
        r#"
        use /std/{Nat};
        let halve(n: Nat) -> Nat = 7 / n;
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains(
            "the bound of the '/' operator was not discharged\n  nothing discharged Div/Ok(n)"
        ) && report.contains(
            "decide the bound with a guard before the operation, or call Div/div(@T, a, b, @proof)"
        ) && !report.contains("/(@")
            && !report.contains("witness@"),
        "unexpected report:\n{report}"
    );
}

// A witness is never called, so a parameter of its telescope nothing discharged is named by the witness's concept and head, and the report suggests no call.
#[test]
fn a_witness_telescopes_undischarged_bound_names_the_witness() {
    let report = error(
        r#"
        use /std/{Nat, Bool, Str, Show};
        struct Small(n: Nat): pub Type { Nat }
        satisfy (@n: Nat, @ok: Bool/Holds(n < 2)) => Show(Small(n)) { show(_s) = "small" }
        let s: Str = Show/show(Small(3) { 0 });
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains(
            "the implicit parameter 'ok' of the witness of 'Show' for head 'Small' was not inferred"
        ) && report.contains("a witness is never called")
            && !report.contains("supply it explicitly")
            && !report.contains("witness@"),
        "unexpected report:\n{report}"
    );
}
