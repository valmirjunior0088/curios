//! A call through a record's field, graded as a call of that field: a member whose leading lambdas answer a record — a witness, a structure of handlers — is called where a field of it is applied, against the member's parameters and the field lambda's after them.
//!
//! The rule is the totality decision's, *A call is read off a path* (`documentation/design/soundness/totality-of-the-erased-program.md`).

use super::test_support::*;

// A witness whose method recurses on its argument descends where the method does: `count`'s calls of `Count/count` on the children are calls of the witness's field, below `t`, so a type reaching it is formed. The refusal hands the method the tree rebuilt around the children, which is `t` again.
#[test]
fn a_witness_whose_method_recurses_on_its_argument_descends() {
    control(
        r#"
        use /std/{Nat, Bool};

        concept Count(A: Type): Type {
            count(A) -> Nat,
        }

        induct Tree: Type
        | leaf(Nat)
        | node(Tree, Tree)
        end

        satisfy Count(Tree) {
            count(t) = match t | leaf(n) => n | node(l, r) => Count/count(l) + Count/count(r) end,
        }

        let Small(t: Tree) -> Prop = Bool/Holds(Count/count(t) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "Count/count(l) + Count/count(r)",
        "Count/count(Tree/node(l, r)) + Count/count(r)",
    )])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A derived witness on a recursive type is a written one the compiler wrote: its method compares the children through its own field, below each argument, so a type reaching `==` on the type is formed.
#[test]
fn a_derived_witness_on_a_recursive_type_is_reached_by_a_type() {
    control(
        r#"
        use /std/{Nat, Bool};
        use /std/ops/{Eql};

        induct Tree: Type
        | leaf(Nat)
        | node(Tree, Tree)
        end

        satisfy Eql(Tree) {
            ..
        }

        let Same(a: Tree, b: Tree) -> Prop = Bool/Holds(a == b);

        /std/print("ok")
        "#,
        b"ok",
    );
}

// A witness under a telescope is a member whose leading lambdas bind the telescope and answer the record: its method's parameters follow them, and a call of the method on a child, the telescope handed on in place, is below `t` as a written witness's is. The refusal hands the method the tree rebuilt around the children.
#[test]
fn a_witness_under_a_telescope_recurses_through_its_field_below_its_argument() {
    control(
        r#"
        use /std/{Nat, Bool};

        concept Count(A: Type): Type {
            count(A) -> Nat,
        }

        induct Tree(A: Type): Type
        | leaf(A)
        | node(Tree(A), Tree(A))
        end

        satisfy Count(Nat) {
            count(n) = n,
        }

        satisfy (@A: Type, use Count(A)) => Count(Tree(A)) {
            count(t) = match t | leaf(a) => Count/count(a) | node(l, r) => Count/count(l) + Count/count(r) end,
        }

        let Small(t: Tree(Nat)) -> Prop = Bool/Holds(Count/count(t) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "Count/count(l) + Count/count(r)",
        "Count/count(Tree/node(l, r)) + Count/count(r)",
    )])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A record is any literal of functions, a tuple of handlers as much as a witness: `handlers`, a value whose fields call each other on a predecessor, is graded field by field, each call below its caller's argument, so a type reaching one is formed, and a cycle through the pair descends where either call does. The refusal has each hand the other its own argument, so `handlers.0(1)` reaches `handlers.1(1)` and itself again.
#[test]
fn a_tuple_of_handlers_calling_each_other_descends_field_by_field() {
    control(
        r#"
        use /std/{Nat, Bool};

        let handlers: {(Nat) -> Nat, (Nat) -> Nat} = (
            (n) => match n | 0 => 0 | k + 1 => handlers.1(k) end,
            (n) => match n | 0 => 1 | k + 1 => handlers.0(k) end
        );

        let Low(n: Nat) -> Prop = Bool/Holds(handlers.0(n) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("| k + 1 => handlers.1(k)", "| k + 1 => handlers.1(n)"),
        ("| k + 1 => handlers.0(k)", "| k + 1 => handlers.0(n)"),
    ])
    .by_elaborator_in("/Low", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Low", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A member named anywhere but at a path's root lets the record it answers escape, so it is a call of each of its parts, claiming nothing: `W` handed bare to `apply0`, which reads its field and applies it, reaches `W.0` again with `n`, which is no descent, and both checkers see the call. The control reaches the field by its path, below its argument.
#[test]
fn a_member_handed_bare_is_a_call_of_each_of_its_parts() {
    control(
        r#"
        use /std/{Nat, Bool};

        let apply0(w: {(Nat) -> Nat}, n: Nat) -> Nat = w.0(n);

        let W: {(Nat) -> Nat} = ((n) => match n | 0 => 0 | k + 1 => W.0(k) end,);

        let Low(n: Nat) -> Prop = Bool/Holds(apply0(W, n) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| k + 1 => W.0(k)", "| k + 1 => apply0(W, n)")])
    .by_elaborator_in("/Low", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Low", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A member handed where its callee applies it lets the record each application answers escape to the callee, so each application is a call of each of its parts, graded on the member's own arguments and claiming nothing past them: `go` applies `f` and calls a field of what it answers, which is `W`'s field lambda, and the refusal hands `go` the field's own argument back, so `W(0).0(0)` reaches itself through `go(W, 0)`.
#[test]
fn a_member_handed_where_its_callee_applies_it_is_a_call_of_each_of_its_parts() {
    control(
        r#"
        use /std/{Nat, Bool};

        let go(f: (Nat) -> {(Nat) -> Nat}, n: Nat) -> Nat =
            match n | 0 => f(0).0(0) | k + 1 => go(f, k) end;

        let W(m: Nat) -> {(Nat) -> Nat} = ((x) => match x | 0 => m | k + 1 => W(m).0(k) end,);

        let Low(n: Nat) -> Prop = Bool/Holds(go(W, n) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| k + 1 => W(m).0(k)", "| k + 1 => go(W, x)")])
    .by_elaborator_in("/Low", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Low", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A witness handed to a definition that applies its method is graded where the definition applies it: `/std/List/eql` applies `e.0` to elements below its lists, so the method's call of `List/eql` on the children is a call of the method below its arguments, and a type reaching `==` on the type is formed. The refusal hands `List/eql` lists built around the arguments themselves, whose elements are no smaller.
#[test]
fn a_witness_handed_to_a_container_operation_descends_where_the_operation_applies_its_method() {
    control(
        r#"
        use /std/{Nat, Bool, List};
        use /std/ops/{Eql};

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        satisfy Eql(Rose) {
            eql(a, b) = match a | rose(n, cs) => match b | rose(m, ds) => n == m && List/eql(cs, ds) end end,
            neq(a, b) = Bool/not(Eql/eql(a, b)),
        }

        let Same(a: Rose, b: Rose) -> Prop = Bool/Holds(a == b);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("List/eql(cs, ds)", "List/eql([a], [b])")])
    .by_elaborator_in("/Same", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Same", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A witness under a telescope is handed to a container operation as the member applied to its own parameters: its own arguments are graded as a call's are, and the method's after them as the operation applies it, so the call on the children descends as the witness of no telescope does. The refusal hands `List/eql` lists built around the arguments themselves.
#[test]
fn a_witness_applied_to_its_own_parameters_and_handed_to_a_container_operation_descends() {
    control(
        r#"
        use /std/{Nat, Bool, List};
        use /std/ops/{Eql};

        induct Rose(A: Type): Type
        | rose(A, List(Rose(A)))
        end

        satisfy (@A: Type, use Eql(A)) => Eql(Rose(A)) {
            eql(a, b) = match a | rose(n, cs) => match b | rose(m, ds) => n == m && List/eql(cs, ds) end end,
            neq(a, b) = Bool/not(Eql/eql(a, b)),
        }

        let Same(a: Rose(Nat), b: Rose(Nat)) -> Prop = Bool/Holds(a == b);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("List/eql(cs, ds)", "List/eql([a], [b])")])
    .by_elaborator_in("/Same", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Same", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A structure of handlers handed to a walker is graded as a witness is: `walk` applies `h.0` to an element below its list, so `sizes`'s call of `walk` on the children is a call of its own field below `t`. The refusal is the walker applying the handler to a tree it built around its list.
#[test]
fn a_structure_of_handlers_handed_to_a_walker_descends_where_the_walker_applies_its_field() {
    control(
        r#"
        use /std/{Nat, Bool, List};

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        let walk(h: {(Rose) -> Nat}, l: List(Rose)) -> Nat =
            match l | [] => 0 | [x, ..xs] => h.0(x) + walk(h, xs) end;

        let sizes: {(Rose) -> Nat} = ((t) => match t | rose(n, cs) => n + walk(sizes, cs) end,);

        let Small(t: Rose) -> Prop = Bool/Holds(sizes.0(t) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("h.0(x)", "h.0(Rose/rose(0, l))")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A record literal handed to a walker hands each field what the walker applies it with: the lambda at the field `walk` applies binds an element below the list it is handed, so its call of `sizes` on that element descends. The refusal has the lambda call `sizes` on the tree it was walking.
#[test]
fn a_record_literal_handed_to_a_walker_binds_each_field_as_the_walker_applies_it() {
    control(
        r#"
        use /std/{Nat, Bool, List};

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        let walk(h: {(Rose) -> Nat}, l: List(Rose)) -> Nat =
            match l | [] => 0 | [x, ..xs] => h.0(x) + walk(h, xs) end;

        let sizes: {(Rose) -> Nat} = ((t) => match t | rose(n, cs) => n + walk(((u) => sizes.0(u),), cs) end,);

        let Small(t: Rose) -> Prop = Bool/Holds(sizes.0(t) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(u) => sizes.0(u)", "(u) => sizes.0(t)")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A walker that hands on a record it built from its handler uses the handler otherwise than applying a field of it, so its summary states nothing of it, and a structure of handlers handed there is a call of each of its parts claiming nothing. The control hands the handler on unchanged.
#[test]
fn a_structure_of_handlers_handed_where_nothing_is_summarized_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat, Bool, List};

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        let walk(h: {(Rose) -> Nat}, l: List(Rose)) -> Nat =
            match l | [] => 0 | [x, ..xs] => h.0(x) + walk(h, xs) end;

        let sizes: {(Rose) -> Nat} = ((t) => match t | rose(n, cs) => n + walk(sizes, cs) end,);

        let Small(t: Rose) -> Prop = Bool/Holds(sizes.0(t) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("walk(h, xs)", "walk((h.0,), xs)")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A derived witness on a type whose children sit in a container compares them through the container's witness: `List`'s, applied to `Rose` and the witness being derived, its method projected and applied. The method is a part of `List`'s witness, whose summary states that it applies the witness it is handed as `List/eql` does, to elements below its lists, so the derived method's call on the children is a call of its own field below its arguments, and a type reaching `==` on `Rose` is formed.
#[test]
fn a_derived_witness_compares_children_held_in_a_container_through_the_containers_witness() {
    control(
        r#"
        use /std/{Nat, Bool, List};
        use /std/ops/{Eql};

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        satisfy Eql(Rose) {
            ..
        }

        let Same(a: Rose, b: Rose) -> Prop = Bool/Holds(a == b);

        /std/print("ok")
        "#,
        b"ok",
    );
}

// A derived witness under a telescope hands the container's witness itself applied to its own parameters, as the written one does: the call on the children descends as the witness of no telescope does.
#[test]
fn a_derived_witness_under_a_telescope_compares_children_held_in_a_container() {
    control(
        r#"
        use /std/{Nat, Bool, List};
        use /std/ops/{Eql};

        induct Rose(A: Type): Type
        | rose(A, List(Rose(A)))
        end

        satisfy (@A: Type, use Eql(A)) => Eql(Rose(A)) {
            ..
        }

        let Same(a: Rose(Nat), b: Rose(Nat)) -> Prop = Bool/Holds(a == b);

        /std/print("ok")
        "#,
        b"ok",
    );
}

// A container's witness of the program's own is read as `List`'s is: `Wrap`'s method applies the witness it is handed to the fields of its arguments, below them, so `Tree`'s method comparing two wrapped children descends. The refusal compares the wraps built around the trees being compared, whose fields are those trees again.
#[test]
fn a_containers_witness_is_graded_by_the_summary_of_its_method() {
    control(
        r#"
        use /std/{Nat, Bool};
        use /std/ops/{Eql};

        struct Wrap(A: Type): Type {
            item: A,
        }

        satisfy (@A: Type, use Eql(A)) => Eql(Wrap(A)) {
            eql(a, b) = Eql/eql(a.item, b.item),
            neq(a, b) = Bool/not(Eql/eql(a.item, b.item)),
        }

        induct Tree: Type
        | leaf(Nat)
        | node(Wrap(Tree))
        end

        satisfy Eql(Tree) {
            eql(a, b) = match a
                | leaf(n) => match b | leaf(m) => n == m | _ => false end
                | node(x) => match b | node(y) => x == y | _ => false end
                end,
            neq(a, b) = Bool/not(Eql/eql(a, b)),
        }

        let Same(a: Tree, b: Tree) -> Prop = Bool/Holds(a == b);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| node(y) => x == y",
        "| node(y) => Wrap { item = a } == Wrap { item = b }",
    )])
    .by_elaborator_in("/Same", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Same", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}
