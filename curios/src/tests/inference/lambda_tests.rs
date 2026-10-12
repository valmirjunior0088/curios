//! A lambda whose type its term has yet to learn is checked where that term completes, at the type its annotations state.
//!
//! A lambda read at a type nothing has given waits for a function type, since only an expectation can supply a hidden binder. Where the term that awaits its type has read its parts none is coming, and the lambda is checked at the type its annotations and body state, one whose every domain is annotated ahead of one with a domain left open. So a lambda bound through a call has its type on the next line, a lambda beside it types it whichever is read first, and a numeral in its body that its domain reaches waits with the domain. Each program that compiles is run, so the kernel has judged what the elaborator admitted.

use crate::tests::{error, run};

/// `body` over the declarations these programs share, its last line the text printed.
fn program(body: &str) -> String {
    format!(
        r#"
        use /std/{{Nat, Int, Byte, Bool, List, Str, Show, print}};
        let id(@A: Type, a: A) -> A = a;
        let first(@A: Type, x: A, pin: A) -> A = x;
        let second(@A: Type, pin: A, x: A) -> A = x;
        let b: Byte = 7;
        let c: Bool = true;
        let and(s: Str, t: Str) -> Str = Str/concat(s, Str/concat(" ", t));
        {body}
        "#
    )
}

// The call is read with no expectation, so it takes every choice it owns before the next line: the lambda has a function type there, and a domain nothing decided is left for the line that applies it.
#[test]
fn a_lambda_bound_through_a_call_has_its_type_on_the_next_line() {
    let source = program(
        r#"
        let z = id((x: Nat) => x + 1);
        let open = id((x) => x);
        let closed: Byte = open(b);
        print(and(Nat/to_str(z(3)), Show/show(closed)))
        "#,
    );

    assert_eq!(run(&source), b"4 7");
}

// The type a lambda's annotations and body state is its own, a domain that names an earlier binder and a codomain that names one among them.
#[test]
fn a_lambda_bound_through_a_call_keeps_a_type_that_names_its_binders() {
    let source = program(
        r#"
        let poly = id((A: Type, x: A) => x);
        let through: Byte = poly(Byte, b);
        print(and(Nat/to_str(poly(Nat, 3)), Show/show(through)))
        "#,
    );

    assert_eq!(run(&source), b"3 7");
}

// The lambda with a domain left open is checked after the annotated one beside it, whichever is written first, and the numeral its domain reaches is realized at the type that one gives.
#[test]
fn a_numeral_in_a_lambdas_body_takes_the_type_a_lambda_beside_it_gives() {
    let source = program(
        r#"
        let left = first((y) => first(y, 5), (x: Byte) => x);
        let right = second((x: Byte) => x, (y) => first(y, 5));
        print(and(Show/show(left(b)), Show/show(right(b))))
        "#,
    );

    assert_eq!(run(&source), b"7 7");
}

// Under two calls of its own the lambda is still the outer call's to settle: the annotated one beside it is read first, however deep the other stands.
#[test]
fn a_lambda_under_nested_calls_is_typed_by_the_lambda_beside_the_outermost() {
    let source = program(
        r#"
        let left = first(id(id((y) => first(y, 5))), (x: Byte) => x);
        let right = second((x: Byte) => x, id(id((y) => first(y, 5))));
        print(and(Show/show(left(b)), Show/show(right(b))))
        "#,
    );

    assert_eq!(run(&source), b"7 7");
}

// A list's elements and a match's arms wait on one type as a call's arguments do.
#[test]
fn lambdas_in_a_list_and_in_a_match_type_each_other() {
    let source = program(
        r#"
        let listed = [(y) => first(y, 5), (x: Byte) => x];
        let chosen = match c | true => (y) => first(y, 5) | false => (x: Byte) => x end;
        let head(fs: List((Byte) -> Byte)) -> Str = match fs | [] => "none" | [f, ..t] => Show/show(f(b)) end;
        print(and(head(listed), Show/show(chosen(b))))
        "#,
    );

    assert_eq!(run(&source), b"7 7");
}

// A body that projects its parameter needs the parameter's shape, which the annotated lambda beside it states.
#[test]
fn a_lambda_that_projects_its_parameter_is_typed_by_the_annotated_one_beside_it() {
    let source = program(
        r#"
        let left = first((p) => p.0, (q: {Nat, Nat}) => q.1);
        let right = second((q: {Nat, Nat}) => q.1, (p) => p.0);
        print(and(Nat/to_str(left((1, 2))), Nat/to_str(right((1, 2)))))
        "#,
    );

    assert_eq!(run(&source), b"1 1");
}

// Only an expectation can supply a hidden binder, and a lambda bound by a `let` has none whether or not a call stands around it: a function type with one, handed to it by a later line, is refused after either binding.
#[test]
fn a_function_type_with_a_hidden_binder_is_refused_after_either_binding() {
    for binding in ["(x: Nat) => x", "id((x: Nat) => x)"] {
        let report = error(&program(&format!(
            r#"
            let z = {binding};
            let h: (@n: Nat, Nat) -> Nat = z;
            print("unreachable")
            "#
        )));

        assert!(
            report.contains("inferred: (x: Nat) -> Nat")
                && report.contains("expected: (@n: Nat, Nat) -> Nat"),
            "unexpected report for `{binding}`:\n{report}"
        );
    }
}

// Inference is local to a statement: the numeral the lambda's domain reaches takes its default where the call completes, and the line after reads a function over `Nat`.
#[test]
fn a_numeral_in_a_lambda_bound_through_a_call_is_its_default_on_the_next_line() {
    let report = error(&program(
        r#"
        let z = id((x) => first(x, 1));
        let h: (Byte) -> Byte = z;
        print("unreachable")
        "#,
    ));

    assert!(
        report.contains("inferred: (x: Nat) -> Nat") && report.contains("expected: (Byte) -> Byte"),
        "unexpected report:\n{report}"
    );
}

// A metavariable may outlive its statement where no choice is taken for it: a domain nothing decided is given by the line that uses the lambda.
#[test]
fn a_domain_nothing_decided_is_given_by_a_later_line() {
    let source = program(
        r#"
        let z = id((x) => x);
        let h: (Byte) -> Byte = z;
        print(Show/show(h(b)))
        "#,
    );

    assert_eq!(run(&source), b"7");
}

// And one nothing ever gives is reported at its parameter, as it is for a lambda no call stands around.
#[test]
fn a_domain_nothing_ever_gives_is_reported_at_its_parameter() {
    let report = error(&program(
        r#"
        let z = id((x) => x);
        print("unreachable")
        "#,
    ));

    assert!(
        report.contains("the type of parameter 'x' was never determined"),
        "unexpected report:\n{report}"
    );
}
