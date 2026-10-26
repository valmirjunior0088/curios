//! Recursion is implicit and a group is declared: a definition may mention itself, at the top level and locally, and definitions that mention one another are declared as one `let … and …;` group.
//!
//! The refusals carry the rule's weight. A cycle the source did not declare is refused by name with the way out, by the elaborator, which asks for each declaration when it is read and so is the one place that sees a cycle through a name, a registry entry or a witness alike; a value that reads itself while it is being computed is refused where the erased program is verified, since only there can a read be told from a knot forced by need; and a local binding that names itself must be a plain, typed name — which is what turns the shadowing idiom `let n = n + 1;` into an error that says so, since the new binding is in scope of its own value.

use {
    crate::tests::{error, run},
    curios_utilities::fill,
};

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

/// A local recursive definition computes in a type stated in the function that declares it, as a top-level one does anywhere and a `let`-bound value does in its tail: both checkers read a member as its group there. `_held` leaves `Eq/refl`'s implicit to be solved, to a term that spells `halve` by value, and `_one_body` puts two definitions of one body to each other. The kernel typed a group's tail over members that stood for nothing, and refused each of the five where the elaborator accepted it.
#[test]
fn a_local_recursive_definition_computes_in_a_type_where_it_is_declared() {
    let source = r#"
        use /std/{Nat, Eq};
        let through(n : Nat) -> Nat =
            let go(k : Nat) -> Nat = match k | 0 => 0 | p + 1 => go(p) + 2 end;
            let halve(k : Nat) -> Nat = match 0 < k | true => halve(k / 2) + 1 | false => 0 end;
            let again(k : Nat) -> Nat = match 0 < k | true => again(k / 2) + 1 | false => 0 end;
            let _closed : Eq()(go(3), 6) = Eq/refl();
            let _one_step : Eq()(go(n + 1), go(n) + 2) = Eq/refl();
            let _guarded : Eq()(halve(4), 3) = Eq/refl();
            let _held : Eq()(halve(n) + 1, 1 + halve(n)) = Eq/refl();
            let _one_body : Eq()(halve(n), again(n)) = Eq/refl();
            go(n);
        /std/print(Nat/to_str(through(3)))
        "#;

    assert_eq!(run(source), b"6");
}

/// A recursive definition whose result is a function computes when applied past its own parameters, at the top level, locally, and as the loop `Nat/descend` runs when its motive is a function: `f(3)(1)` is the call `f(3)` applied to what is left, and a demand for its value unfolds that call rather than stopping at the outer application. A symbolic argument past the parameters rides along, so the last claim is a law rather than a sample.
#[test]
fn a_recursive_call_applied_past_its_parameters_computes_in_a_type() {
    let source = r#"
        use /std/{Nat, Eq};
        use /std/Bool/{True};
        let top(n : Nat) -> (Nat) -> Nat =
            match n | 0 => (x) => x | p + 1 => (x) => top(p)(x) + 1 end;
        let nested(n : Nat) -> (Nat) -> Nat =
            let go(m : Nat) -> (Nat) -> Nat = match m | 0 => (x) => x | p + 1 => (x) => go(p)(x) + 1 end;
            go(n);
        let measured(n : Nat, k : Nat) -> Nat =
            Nat/descend(
                (x : Nat) => x,
                (_ : Nat) => (k : Nat) -> Nat,
                (x, ih) => (k) => match x | 0 => k | p + 1 => ih(p, True/qed())(k) + 1 end,
                n)(k);
        let _top : Eq()(top(3)(1), 4) = Eq/refl();
        let _nested : Eq()(nested(3)(1), 4) = Eq/refl();
        let _measured : Eq()(measured(3, 10), 13) = Eq/refl();
        let _symbolic(k : Nat) -> Eq()(top(2)(k), k + 2) = Eq/refl();
        /std/print(Nat/to_str(measured(3, 10)))
        "#;

    assert_eq!(run(source), b"13");
}

/// `Eq(@Nat)` is the relation `(Nat, Nat) -> Prop` with no lambda around it: an indexed family at its parameters is a family of its own, so it stands where a relation is expected and reflexivity inhabits its diagonal. Both checkers see the partial application, since the kernel re-checks the program.
#[test]
fn an_indexed_family_at_its_parameters_is_a_relation() {
    let source = r#"
        use /std/{Nat, Eq};
        let diagonal(R: (Nat, Nat) -> Prop) -> Prop = (n: Nat) -> R(n, n);
        let _reflexive: diagonal(Eq(@Nat)) = (n) => Eq/refl();
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
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
        report.contains("'f' and 'g' need one another") && report.contains("'let ... and ...'"),
        "expected the cycle named with its way out:\n{report}"
    );
    // Placed at the first member's name: a cycle is about its declarations as wholes, and holds no position of its own.
    assert!(
        report.contains("let f(n : Nat) -> Nat = g(n);"),
        "expected the snippet at `f`'s declaration:\n{report}"
    );
    assert_eq!(
        report.matches("need one another").count(),
        1,
        "expected the cycle reported once:\n{report}"
    );
}

/// A cycle through a family's constructor and one through a record's field are cycles as one through two bodies is: a registry entry reaches what its payload and field types name, so each is refused by name, with the way out, by the one place that refuses any.
#[test]
fn a_cycle_through_a_registry_entry_is_refused_by_name() {
    let through_a_family = r#"
        induct Tree : pub Type | leaf() | node(Forest) end
        let Forest : Type = Tree;
        /std/print("unreachable")
        "#;
    let through_a_record = r#"
        struct Tree : pub Type { held: Forest }
        let Forest : Type = Tree;
        /std/print("unreachable")
        "#;

    for source in [through_a_family, through_a_record] {
        let report = error(source);
        assert!(
            report.contains("'Tree' and 'Forest' need one another")
                && report.contains("'let ... and ...'"),
            "expected the cycle named with its way out:\n{report}"
        );
        assert_eq!(
            report.matches("need one another").count(),
            1,
            "expected the cycle reported once:\n{report}"
        );
    }
}

/// A top-level group ties declarations that need one another and nothing else, a declaration reading any other wherever it is written: one whose members fall into parts is refused once, at its first member, naming each part, whether it groups definitions, families or witnesses, and each is accepted declared part by part. A witness's resolutions count as what its member reaches.
#[test]
fn a_top_level_group_that_is_no_cycle_is_refused_naming_its_parts() {
    let cases = [
        (
            r#"
            use /std/{Nat};
            let alone(n: Nat) -> Nat = n
            and generic(A: Type, n: Nat) -> Nat = alone(n);
            /std/print(Nat/to_str(generic(Nat, 1)))
            "#,
            r#"
            use /std/{Nat};
            let alone(n: Nat) -> Nat = n;
            let generic(A: Type, n: Nat) -> Nat = alone(n);
            /std/print(Nat/to_str(generic(Nat, 1)))
            "#,
            "'alone' alone, and 'generic' alone",
        ),
        (
            r#"
            induct Pause: pub Type
            | yield_now()
            and Step(A: Type): pub Type
            | done(A)
            | step(pause: Pause)
            end
            /std/print("1")
            "#,
            r#"
            induct Pause: pub Type
            | yield_now()
            end
            induct Step(A: Type): pub Type
            | done(A)
            | step(pause: Pause)
            end
            /std/print("1")
            "#,
            "'Pause' alone, and 'Step' alone",
        ),
        (
            r#"
            use /std/{Show};
            induct A: pub Type | a() end
            induct B: pub Type | b() end
            satisfy Show(A) {
                show(_) = "1",
            }
            and Show(B) {
                show(_) = Show/show(A/a()),
            }
            /std/print(Show/show(B/b()))
            "#,
            r#"
            use /std/{Show};
            induct A: pub Type | a() end
            induct B: pub Type | b() end
            satisfy Show(A) {
                show(_) = "1",
            }
            satisfy Show(B) {
                show(_) = Show/show(A/a()),
            }
            /std/print(Show/show(B/b()))
            "#,
            "the witness of Show(A) alone, and the witness of Show(B) alone",
        ),
    ];

    for (grouped, apart, parts) in cases {
        let report = error(grouped);
        assert!(
            report.contains("are grouped, and do not all need one another")
                && report.contains(&format!("declare each part on its own: {parts}")),
            "expected the group refused, naming its parts:\n{report}"
        );
        assert_eq!(
            report.matches("do not all need one another").count(),
            1,
            "expected the group refused once:\n{report}"
        );
        assert_eq!(
            run(apart),
            b"1",
            "declared part by part, the group is accepted"
        );
    }
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
    assert!(
        report.contains("/std/print(Nat/to_str(xs))"),
        "expected the refusal framed at the read:\n{report}"
    );
    assert!(
        report.contains("let xs : Nat = xs + 1;"),
        "expected a note at the value's own line:\n{report}"
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
    assert!(
        report.contains("let m : Nat = m + 1;"),
        "expected the refusal framed at the local's own line:\n{report}"
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
// A member's name is defined to its slot while the group is being checked, so reduction turns a recursive reference into that slot and a committed solution can carry one. `RecItem::try_new` captures member *names* into the group's binder, so a slot reaching that point is not something the capture can bind. Both substitution walks spell a filled slot as its member's name, which is the capturable thing the group's binder is waiting for, and `elaborate_rec` materializes before it closes so the name lands where the capture still binds it; expanded to the member's body instead, the slot would meet the same solution in that body and the walk would never end — `recurse` answers a deepening walk by asking the allocator for stack, so the compilation would die by exhausting memory rather than refusing anything.
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

#[test]
fn a_descent_through_a_local_definition_is_graded_wherever_the_call_stands() {
    // The call's argument names a `let` bound before it. In the tail the walk reads it as its value; in a later binding's value it once read the binding as stored, and refused a proof the kernel accepts.
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let halved(n: Nat) -> Holds(0 <= n) =
            match n < 2
            | true => True/qed()
            | false => let half = n / 2; let _inner = halved(half); True/qed()
            end;
        let depth(n: Nat) -> Nat =
            match n < 2
            | true => 0
            | false => let half = n / 2; let below = depth(half); below + 1
            end;
        let Chosen: Type = match depth(5) | 2 => Nat | _ => Bool end;
        let chosen: Chosen = 7;
        /std/print(Nat/to_str(chosen + depth(9)))
        "#;

    assert_eq!(run(source), b"10");
}

#[test]
fn a_call_through_a_local_definition_that_does_not_descend_is_refused() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let grown(n: Nat) -> Holds(0 <= n) =
            match n < 2
            | true => True/qed()
            | false => let more = n + 1; let _inner = grown(more); True/qed()
            end;
        /std/print("unreachable")
        "#;

    assert!(error(source).contains("not known to terminate"));
}

/// A type reaching two partial definitions is refused by the one written first, whichever spells first: renaming either moves nothing.
// A group's bodies are checked over members that stand for nothing, as the kernel checks them: no judgment inside a body reads a member's body, its own or a sibling's, written before it or after it, at the top level or locally. The elaborator once opened a member's slot into the body it was filled with, an earlier sibling's at once and its own or a later one's on retry, and so filled each bound here, took `True/qed()` at `Holds(g(0) == 0)` and `taint` at `T(2)` — each refused by the kernel.
#[test]
fn no_judgment_inside_a_group_body_reads_a_members_body() {
    let bounded = |member: &str, groups: &str| {
        let error = error(&fill!(
            r#"
            use /std/{Nat, List};
            ${groups}
            /std/print("")
            "#,
            groups,
        ));
        assert!(
            error.contains(&fill!(
                "nothing discharged /std/Bool/Holds(${member}(0) < List/len(@Nat, [1, 2, 3]))",
                member,
            )) && !error.contains("the kernel refused"),
            "{error}"
        );
    };

    bounded(
        "f",
        "let f(n: Nat) -> Nat = match n | 0 => 0 | _ + 1 => List/get([1, 2, 3], f(0)) end;",
    );
    bounded(
        "h",
        "let g(n: Nat) -> Nat = match n | 0 => 0 | _ + 1 => List/get([1, 2, 3], h(0)) end
        and h(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => g(k) end;",
    );
    bounded(
        "g",
        "let g(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => h(k) end
        and h(n: Nat) -> Nat = match n | 0 => 0 | _ + 1 => List/get([1, 2, 3], g(0)) end;",
    );
    bounded(
        "r",
        "let outer(m: Nat) -> Nat =
            let r(n: Nat) -> Nat = match n | 0 => 0 | _ + 1 => List/get([1, 2, 3], r(0)) end;
            r(m);",
    );

    let proved = error(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};
        let need(n: Nat, p: Holds(n == 0)) -> Nat = n;
        let g(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => h(k) end
        and h(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => need(g(0), True/qed()) end;
        /std/print("")
        "#,
    );
    assert!(
        proved.contains("expected: Holds(g(0) == 0)") && !proved.contains("the kernel refused"),
        "{proved}"
    );

    let annotated = error(
        r#"
        use /std/{Nat};
        let outer(taint: Nat) -> Nat =
            let T(n: Nat) -> Type = match n | 0 => Nat | k + 1 => T(k) end
            and val: Nat =
                let x: T(2) = taint;
                x;
            val;
        /std/print("")
        "#,
    );
    assert!(
        annotated.contains("expected: T(2)") && !annotated.contains("the kernel refused"),
        "{annotated}"
    );
}

// A signature reads its group as the kernel reads one, folded: `val`'s type `T(2)` unfolds `T` through its own recursion to `Nat`, both where `val`'s body is checked at it and where a sibling's body reads `val` at it. Read by the members' names alone, as a body reads them, `T(2)` stopped at `T(1)` and both were refused.
#[test]
fn a_group_signature_reads_its_members_bodies() {
    let source = r#"
        use /std/{Nat};
        let outer(taint: Nat) -> Nat =
            let T(n: Nat) -> Type = match n | 0 => Nat | k + 1 => T(k) end
            and val: T(2) = taint
            and after(n: Nat) -> Nat = match n | 0 => val + 1 | k + 1 => after(k) end;
            after(1);
        /std/print(Nat/to_str(outer(2)))
        "#;

    assert_eq!(run(source), b"3");
}

// A group's signatures are sorted with its members opaque, so `val: T(2)` is read at a stuck `T(2)` whose sort is `Type`; its bodies are checked with them folded, where `T(2)` reduces through `T`. The two readings cannot disagree on which sort `T(2)` is: `T`'s result sort holds every arm to `Type`, so an arm that is a proposition is refused where `T`'s body is checked, and no member's type folds to a `Prop` the opaque reading took for a `Type`. Refused by the elaborator, not the kernel.
#[test]
fn a_group_member_type_cannot_fold_to_a_proposition() {
    let error = error(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};
        let T(n: Nat) -> Type = match n | 0 => Holds(true) | k + 1 => T(k) end
        and val: T(2) = True/qed();
        /std/print("")
        "#,
    );
    assert!(
        error.contains("inferred: Prop")
            && error.contains("expected: Type")
            && !error.contains("the kernel refused"),
        "{error}"
    );
}

// A recursive slot is the one solution the occurs check does not hold, and a signature reads a member through its slot, so a body reading a sibling at its signature, or an implicit solved from one, names a slot inside the solution that fills it. A walk through solutions stops at a filled slot, which stands for its member; walked into, each program here overflowed the elaborator's stack. An implicit met at a type headed by a filled slot is solved with the type's reduct, which names no member, so `id`'s `@A` is `(Nat) -> Nat` and the first program runs.
#[test]
fn a_slot_reached_from_its_own_solution_is_judged() {
    let program = |groups: &str, tail: &str| {
        fill!(
            r#"
            use /std/{Nat};
            let id(@A: Type, a: A) -> A = a;
            ${groups}
            /std/print(Nat/to_str(${tail}))
            "#,
            groups,
            tail,
        )
    };
    let refused = |groups: &str, tail: &str, diagnostic: &str| {
        let error = error(&program(groups, tail));
        assert!(
            error.contains(diagnostic) && !error.contains("the kernel refused"),
            "{error}"
        );
    };

    assert_eq!(
        run(&program(
            "let outer(m: Nat) -> Nat =
            let T(n: Nat) -> Type = match n | 0 => (Nat) -> Nat | k + 1 => T(k) end
            and val: T(2) = id((x) => x + 1);
            val(m);",
            "outer(3)",
        )),
        b"4"
    );
    refused(
        "let outer(m: Nat) -> Nat =
            let T(n: Nat) -> Type = match n | 0 => (Nat) -> Nat | k + 1 => T(k) end
            and val: T(2) = id(@T(2), m);
            val(m);",
        "outer(3)",
        "expected: T(2)",
    );
    refused(
        "let T(n: Nat) -> Type = match n | 0 => (Nat) -> Nat | k + 1 => let _w = val; T(k) end
        and val: T(2) = id((x) => x + 1);",
        "val(3)",
        "is a type position but does not terminate on every input",
    );
}

#[test]
fn a_type_reaching_two_partial_definitions_names_the_first_written() {
    let refusal = |arguments: &str| {
        error(&fill!(
            r#"
            use /std/{Nat, Io};
            pub let zeta(n: Nat) -> Nat = zeta(n);
            pub let alpha(n: Nat) -> Nat = alpha(n);
            pub let F(_a: Nat, _b: Nat) -> Type = Nat;
            pub let x: F(${arguments}) = 0;
            Io/pure(())
            "#,
            arguments,
        ))
    };

    assert!(refusal("zeta(0), alpha(0)").contains("reaches 'zeta'"));
    assert!(refusal("alpha(0), zeta(0)").contains("reaches 'alpha'"));
}

// Two definitions that write one recursion two ways are one function to both checkers, called and bare: their groups are compared as the binders they are, and neither is unfolded against the other.
#[test]
fn two_spellings_of_one_recursion_are_one_function() {
    let source = r#"
        use /std/{Nat, Eq};
        let f(n : Nat) -> Nat = match n | 0 => 0 | k + 1 => f(k) + 2 end;
        let h(n : Nat) -> Nat = match n | 0 => 0 | k + 1 => 2 + h(k) end;
        let called(n : Nat) -> Eq()(f(n), h(n)) = Eq/refl();
        let bare : Eq()(f, h) = Eq/refl();
        /std/print(Nat/to_str(f(3)))
        "#;

    assert_eq!(run(source), b"6");
}
