//! Recursion is implicit and a group is declared: a definition may mention itself, at the top level and locally, and definitions that mention one another are declared as one `let … and …;` group.
//!
//! The refusals carry the rule's weight. A cycle the source did not declare is refused by name with the way out; a value that reads itself while it is being computed is refused where the erased program is verified, since only there can a read be told from a knot forced by need; and a local binding that names itself must be a plain, typed name — which is what turns the shadowing idiom `let n = n + 1;` into an error that says so, since the new binding is now in scope of its own value.

use crate::tests::{error, run};

#[test]
fn a_declared_group_of_definitions_is_mutually_recursive() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat = match n | 0 => 0 | p + 1; _ => g(p) end
        and g(n : Nat) -> Nat = match n | 0 => 1 | p + 1; _ => f(p) end;
        /std/print(Nat/to_str(f(3)))
        "#;

    assert_eq!(run(source), b"1");
}

#[test]
fn a_definition_that_names_itself_recurses() {
    let source = r#"
        use /std/{Nat};
        let count(n : Nat) -> Nat = match n | 0 => 0 | p + 1; _ => count(p) + 1 end;
        /std/print(Nat/to_str(count(3)))
        "#;

    assert_eq!(run(source), b"3");
}

#[test]
fn a_local_binding_that_names_itself_recurses() {
    let source = r#"
        use /std/{Nat};
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
        use /std/{Nat};
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
        use /std/{Nat};
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

// A `rec` group whose member is reached through a *dependent* index family, at both the top level and inside a function.
//
// A member's name is defined to its slot while the group is being checked, so reduction turns a recursive reference into that slot and a committed solution can carry one. `RecItem::try_new` captures member *names* into the group's binder, so a slot reaching that point is not something the capture can bind: substitution expanded it to the member's body instead, the body mentioned that same solution, and the walk never ended — `recurse` answers a deepening walk by asking the allocator for stack, so the compilation died by exhausting memory rather than refusing anything. Both substitution walks now spell a filled slot as its member's name, which is the capturable thing the group's binder is waiting for, and `elaborate_rec` materializes before it closes so the name lands where the capture still binds it.
#[test]
fn a_rec_group_over_a_dependent_family_closes_without_expanding_its_own_members() {
    let source = r#"
        use /std/{Nat, Str, List};

        induct Shape: pub Type
        | leaf() | node(a: List(Shape))
        end

        induct Sizes: (Shape, Nat) -> pub Type
        | one(h: Nat): (Shape/leaf(), h)
        | many(@ps: List(Shape), @h: Nat, col: Column(ps, h)): (Shape/node(ps), h)
        | fitted(@s: Shape, @h0: Nat, h: Nat, inner: Sizes(s, h0)): (s, h)
        and Column: (List(Shape), Nat) -> pub Type
        | stop(): ([], 0)
        | part(
            @s: Shape,
            @rest: List(Shape),
            @h1: Nat,
            @h2: Nat,
            head: Sizes(s, h1),
            tail: Column(rest, h2),
          ): ([s, ..rest], h1 + h2)
        end

        let split(s: Shape, h: Nat) -> Sizes(s, h) =
            match s
            | leaf() => Sizes/one(h)
            | node(ps) =>
                let made = split_column(ps, h);
                Sizes/fitted(h, Sizes/many(made.col))
            end
        and split_column(ps: List(Shape), h: Nat) -> {th: Nat, col: Column(ps, th)} =
            match ps
            | [] => (th = 0, col = Column/stop())
            | [child, ..rest] =>
                let below = split_column(rest, h);
                (th = h + below.th, col = Column/part(split(child, h), below.col))
            end;

        let local(root: Shape, avail: Nat) -> Nat =
            let inner(s: Shape, h: Nat) -> Sizes(s, h) =
                match s
                | leaf() => Sizes/one(h)
                | node(ps) =>
                    let made = inner_column(ps, h);
                    Sizes/fitted(h, Sizes/many(made.col))
                end
            and inner_column(ps: List(Shape), h: Nat) -> {th: Nat, col: Column(ps, th)} =
                match ps
                | [] => (th = 0, col = Column/stop())
                | [child, ..rest] =>
                    let below = inner_column(rest, h);
                    (th = h + below.th, col = Column/part(inner(child, h), below.col))
                end;
            avail;

        /std/print(Nat/to_str(local(Shape/leaf(), 7)))
        "#;

    assert_eq!(run(source), b"7");
}
