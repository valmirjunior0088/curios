//! Recursion is implicit and a group is declared: a definition may mention itself, at the top level and locally, and definitions that mention one another are declared as one `let … and …;` group.
//!
//! The refusals carry the rule's weight. A cycle the source did not declare is refused by name with the way out; a value that reads itself while it is being computed is refused where the erased program is verified, since only there can a read be told from a knot forced by need; and a local binding that names itself must be a plain, typed name — which is what turns the shadowing idiom `let n = n + 1;` into an error that says so, since the new binding is now in scope of its own value.

use crate::tests::{error, run};

#[test]
fn a_declared_group_of_definitions_is_mutually_recursive() {
    let source = r#"
        use /std/{Nat, Handle};
        let f(n : Nat) -> Nat = match n | 0 => 0 | p + 1; _ => g(p) end
        and g(n : Nat) -> Nat = match n | 0 => 1 | p + 1; _ => f(p) end;
        /std/print(Nat/to_str(f(3)))
        "#;

    assert_eq!(run(source), b"1");
}

#[test]
fn a_definition_that_names_itself_recurses() {
    let source = r#"
        use /std/{Nat, Handle};
        let count(n : Nat) -> Nat = match n | 0 => 0 | p + 1; _ => count(p) + 1 end;
        /std/print(Nat/to_str(count(3)))
        "#;

    assert_eq!(run(source), b"3");
}

#[test]
fn a_local_binding_that_names_itself_recurses() {
    let source = r#"
        use /std/{Nat, Handle};
        let twice(n : Nat) -> Nat =
            let go(k : Nat) -> Nat = match k | 0 => 0 | p + 1; _ => go(p) + 2 end;
            go(n);
        /std/print(Nat/to_str(twice(3)))
        "#;

    assert_eq!(run(source), b"6");
}

#[test]
fn a_local_group_is_mutually_recursive() {
    let source = r#"
        use /std/{Nat, Handle};
        let parity(n : Nat) -> Nat =
            let even(k : Nat) -> Nat = match k | 0 => 1 | p + 1; _ => odd(p) end
            and odd(k : Nat) -> Nat = match k | 0 => 0 | p + 1; _ => even(p) end;
            even(n);
        /std/print(Nat/to_str(parity(4)))
        "#;

    assert_eq!(run(source), b"1");
}

// A self-reference under a lambda is a knot forced by need, which the erased program ties through a cell: legal, and the shape a lazy structure takes.
#[test]
fn a_value_that_names_itself_under_a_lambda_is_admitted() {
    let source = r#"
        use /std/{Nat, Handle};
        struct Stream : pub Type { head: Nat, tail: () -> Stream }
        let ones : Stream = Stream { head = 1, tail = () => ones };
        /std/print(Nat/to_str(ones.tail().head + ones.head))
        "#;

    assert_eq!(run(source), b"2");
}

#[test]
fn an_undeclared_cycle_is_refused_with_the_way_out() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat = g(n);
        let g(n : Nat) -> Nat = f(n);
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("`/f` and `/g` reference each other")
            && report.contains("join them with `and`"),
        "expected the cycle named with its way out:\n{report}"
    );
}

// Read outside every lambda, the self-reference is an initializer evaluating itself, which the erased verifier refuses. The refusal stays at the erase boundary on purpose: forcing on first use is what a recursive value means, and no syntactic net can tell this from the knots the language admits — a member read through a closure, a later member forced first, a self-knot nothing ever forces.
#[test]
fn a_value_that_reads_itself_is_refused_at_the_erase_boundary() {
    let source = r#"
        use /std/{Nat};
        let xs : Nat = xs + 1;
        /std/print(Nat/to_str(xs))
        "#;

    let report = error(source);
    assert!(
        report.contains("evaluates itself"),
        "expected the verifier's refusal:\n{report}"
    );
}

#[test]
fn a_local_value_that_reads_itself_is_refused_at_the_erase_boundary() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat =
            let m : Nat = m + 1;
            m;
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("evaluates itself"),
        "expected the verifier's refusal:\n{report}"
    );
}

// The shadowing idiom: the new `n` is in scope of its own value, so the value mentions it, and an untyped recursive binding is refused before elaboration — with the rename it needs.
#[test]
fn a_shadowing_rebinding_is_refused_as_a_self_reference() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat =
            let n = n + 1;
            n;
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("`n` mentions itself and states no type")
            && report.contains("rename one of them"),
        "expected the self-reference named with the rename:\n{report}"
    );
}
