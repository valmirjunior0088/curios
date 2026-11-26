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

        let Small(t: Tree) -> /std/Bool/True =
            match Count/count(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Same(a: Tree, b: Tree) -> /std/Bool/True =
            match a == b | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Small(t: Tree(Nat)) -> /std/Bool/True =
            match Count/count(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Low(n: Nat) -> /std/Bool/True =
            match handlers.0(n) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Low(n: Nat) -> /std/Bool/True =
            match apply0(W, n) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Low(n: Nat) -> /std/Bool/True =
            match go(W, n) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

// A witness handed to a definition that applies its method is graded where the definition applies it: `same_list` applies `e.0` to elements below its lists, as `/std/List/eql` does, so the method's call of it on the children is a call of the method below its arguments, and a proof reaching the method is formed. The refusal hands `same_list` lists built around the arguments themselves, whose elements are no smaller. The concept is the program's own and carries no law, so a refusal is derived by editing its one method.
#[test]
fn a_witness_handed_to_a_container_operation_descends_where_the_operation_applies_its_method() {
    control(
        r#"
        use /std/{Nat, Bool, List};

        concept Same(A: Type): Type {
            same(A, A) -> Bool,
        }

        satisfy Same(Nat) {
            same(a, b) = a == b,
        }

        let same_list(@A: Type, use Same(A), a: List(A), b: List(A)) -> Bool =
            match (a, b)
            | ([], []) => true
            | ([x, ..xs], [y, ..ys]) => Same/same(x, y) && same_list(xs, ys)
            | _ => false
            end;

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        satisfy Same(Rose) {
            same(a, b) = match a | rose(n, cs) => match b | rose(m, ds) => n == m && same_list(cs, ds) end end,
        }

        let Alike(a: Rose, b: Rose) -> /std/Bool/True =
            match Same/same(a, b) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("same_list(cs, ds)", "same_list([a], [b])")])
    .by_elaborator_in("/Alike", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Alike", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A witness under a telescope is handed to a container operation as the member applied to its own parameters: its own arguments are graded as a call's are, and the method's after them as the operation applies it, so the call on the children descends as the witness of no telescope does. The refusal hands `same_list` lists built around the arguments themselves.
#[test]
fn a_witness_applied_to_its_own_parameters_and_handed_to_a_container_operation_descends() {
    control(
        r#"
        use /std/{Nat, Bool, List};

        concept Same(A: Type): Type {
            same(A, A) -> Bool,
        }

        satisfy Same(Nat) {
            same(a, b) = a == b,
        }

        let same_list(@A: Type, use Same(A), a: List(A), b: List(A)) -> Bool =
            match (a, b)
            | ([], []) => true
            | ([x, ..xs], [y, ..ys]) => Same/same(x, y) && same_list(xs, ys)
            | _ => false
            end;

        induct Rose(A: Type): Type
        | rose(A, List(Rose(A)))
        end

        satisfy (@A: Type, use Same(A)) => Same(Rose(A)) {
            same(a, b) = match a | rose(n, cs) => match b | rose(m, ds) => Same/same(n, m) && same_list(cs, ds) end end,
        }

        let Alike(a: Rose(Nat), b: Rose(Nat)) -> /std/Bool/True =
            match Same/same(a, b) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("same_list(cs, ds)", "same_list([a], [b])")])
    .by_elaborator_in("/Alike", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Alike", |error| {
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

        let Small(t: Rose) -> /std/Bool/True =
            match sizes.0(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Small(t: Rose) -> /std/Bool/True =
            match sizes.0(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Small(t: Rose) -> /std/Bool/True =
            match sizes.0(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Same(a: Rose, b: Rose) -> /std/Bool/True =
            match a == b | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        let Same(a: Rose(Nat), b: Rose(Nat)) -> /std/Bool/True =
            match a == b | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

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

        concept Same(A: Type): Type {
            same(A, A) -> Bool,
        }

        struct Wrap(A: Type): Type {
            item: A,
        }

        satisfy (@A: Type, use Same(A)) => Same(Wrap(A)) {
            same(a, b) = Same/same(a.item, b.item),
        }

        induct Tree: Type
        | leaf(Nat)
        | node(Wrap(Tree))
        end

        satisfy Same(Tree) {
            same(a, b) = match a
                | leaf(n) => match b | leaf(m) => n == m | _ => false end
                | node(x) => match b | node(y) => Same/same(x, y) | _ => false end
                end,
        }

        let Alike(a: Tree, b: Tree) -> /std/Bool/True =
            match Same/same(a, b) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| node(y) => Same/same(x, y)",
        "| node(y) => Same/same(Wrap { item = a }, Wrap { item = b })",
    )])
    .by_elaborator_in("/Alike", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Alike", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A field lambda is the part at its slot among every field of the literal, a field that is no lambda counted: `loop` is slot `1` of a structure whose slot `0` is a number, so its call of `H.loop` is a call of itself, as `down`'s of `H.down` is of itself. The refusal hands `H.loop` its own argument back; counted among the lambdas alone, `H.loop` would be the part `down` is, which descends, and the cycle through `loop` would go unseen.
#[test]
fn a_field_lambda_is_the_part_at_its_slot_among_every_field() {
    control(
        r#"
        use /std/{Nat, Bool};

        struct Handlers: Type {
            base: Nat,
            loop: (Nat) -> Nat,
            down: (Nat) -> Nat,
        }

        let H: Handlers = Handlers {
            base = 0,
            loop = (n) => match n | 0 => 0 | k + 1 => H.loop(k) end,
            down = (n) => match n | 0 => 0 | k + 1 => H.down(k) end,
        };

        let Low(n: Nat) -> /std/Bool/True =
            match H.loop(n) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| k + 1 => H.loop(k)", "| k + 1 => H.loop(n)")])
    .by_elaborator_in("/Low", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Low", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A path stops at the first field that is no part, and the call is of the part reached so far: `W.0(m).1(z)` projects slot `1` out of the record `W`'s field lambda at slot `0` answers, which is no part of `W`, so it is a call of that field lambda at `m` again, which the lambda in its record makes for ever. Read as `W`'s own slot `1`, it would be a call of the part that descends. The control calls that part itself.
#[test]
fn a_field_of_what_a_field_lambda_answers_is_no_part_of_the_member() {
    control(
        r#"
        use /std/{Nat, Bool};

        let W: {(Nat) -> {Nat, (Nat) -> Nat}, (Nat) -> Nat} = (
            (m) => (0, (z) => W.1(z)),
            (n) => match n | 0 => 0 | k + 1 => W.1(k) end
        );

        let Low(n: Nat) -> /std/Bool/True =
            match W.0(n).1(n) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(z) => W.1(z)", "(z) => W.0(m).1(z)")])
    .by_elaborator_in("/Low", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Low", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A path that follows a field calls the member's leading lambdas too, since strict evaluation answers the record, its other fields with it, before the field is read: with `W(n).0(1)` at slot `1`, every `W(n)` evaluates it, so each call of the field lambda through `W(n)` calls it again at `n`, and evaluating `W(n)` does so without end. Were a field read alone, only the field lambda, which descends on `m`, would be seen. The control's slot `1` calls nothing.
#[test]
fn a_call_through_a_field_calls_the_fields_answered_beside_it() {
    control(
        r#"
        use /std/{Nat, Bool};

        let W(n: Nat) -> {(Nat) -> Nat, Nat} = (
            (m) => match m | 0 => 0 | k + 1 => W(n).0(k) end,
            n * 2
        );

        let Low(n: Nat) -> /std/Bool/True =
            match W(n).0(n) == W(n).1 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("n * 2", "W(n).0(1)")])
    .by_elaborator_in("/Low", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Low", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A path that reaches no field lambda lets the record escape, so it is a call of every part under where it stands, graded on the arguments it handed and claiming nothing past them: `W(n)` bound by a `let`, `W(n)` handed to `pass`, which states nothing of what it is handed, and `V.0`, a record literal held at a field, bound by a `let`, are each a call of the field lambda the record holds, which each refusal reaches through one of them at its own argument again. The control reaches each field by its path, below its argument.
#[test]
fn a_record_that_escapes_is_a_call_of_every_part_under_where_it_stands() {
    let program = control(
        r#"
        use /std/{Nat, Bool};

        let pass(r: {(Nat) -> Nat}) -> {(Nat) -> Nat} = r;

        let W(n: Nat) -> {(Nat) -> Nat} = ((m) => match m | 0 => n | k + 1 => W(n).0(k) end,);

        let V: {{(Nat) -> Nat}} = (((m) => match m | 0 => 0 | k + 1 => V.0.0(k) end,),);

        let Low(n: Nat) -> /std/Bool/True =
            match W(n).0(n) == V.0.0(n) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    );
    for (from, to) in [
        ("W(n).0(k)", "let r = W(n); r.0(m)"),
        ("W(n).0(k)", "pass(W(n)).0(m)"),
        ("V.0.0(k)", "let r = V.0; r.0(m)"),
    ] {
        program
            .refused(&[(from, to)])
            .by_elaborator_in("/Low", |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_certifier_in("/Low", |error| {
                matches!(error, curios_cert::Error::NotTotal { .. })
            });
    }
}

// A member of no parameter handed bare to a definition that applies a field of it is a call of its leading lambdas, whose record holds that field, where no part lies at the field applied: `W`'s slot `1` is a lambda under a `let`, which is no part of `W`, so `apply1(W, y)` in it is a call of `W` its leading lambdas make again, and handing `y` back it recurs for ever. The control's slot `1` is a field lambda, which `apply1` applies below `y`.
#[test]
fn a_member_handed_bare_where_no_part_lies_at_the_field_applied_calls_its_leading_lambdas() {
    control(
        r#"
        use /std/{Nat, Bool};

        let apply1(w: {Nat, (Nat) -> Nat}, n: Nat) -> Nat = w.1(n);

        let W: {Nat, (Nat) -> Nat} = (0, (y) => match y | 0 => 1 | k + 1 => apply1(W, k) end);

        let Low(n: Nat) -> /std/Bool/True =
            match W.1(n) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "(y) => match y | 0 => 1 | k + 1 => apply1(W, k) end",
            "(() => let q = 1; (y: Nat) => match y | 0 => q | k + 1 => apply1(W, k) end)()",
        ),
        ("apply1(W, k)", "apply1(W, y)"),
    ])
    .by_elaborator_in("/Low", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Low", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A call through a definition's field is graded by that field's summary and by no other part's: `w` answers two field lambdas over its `f`, the first applying it below `n` and the second at `n`, so `spin`'s recursion through the lambda it hands `w`, projected to the first field, descends. The refusal projects the second field: graded by the first field's summary, or by the leading lambdas', `spin(1)`, which is `w(1, …).1(0)` applying the lambda at `1`, would be a total `spin(1)` again.
#[test]
fn a_definitions_field_call_is_graded_by_the_field_it_projects() {
    control(
        r#"
        use /std/{Nat, Bool};

        let w(n: Nat, f: (Nat) -> Bool) -> {(Nat) -> Bool, (Nat) -> Bool} =
            ((x) => match n | 0 => false | k + 1 => f(k) end, (y) => f(n));

        let spin(n: Nat) -> Bool = w(n, (c) => spin(c)).0(0);

        let Spun(n: Nat) -> /std/Bool/True =
            match spin(n) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("w(n, (c) => spin(c)).0(0)", "w(n, (c) => spin(c)).1(0)")])
    .by_elaborator_in("/Spun", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spun", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A call through nested records is graded by the field lambda its whole path reaches: `w` answers a record holding a pair of field lambdas, the first applying `f` below `n` and the second at `n`, so `.0.0` reaches the first and `spin` descends. The refusal projects `.0.1`: graded by the pair's first field, or by the path's first slot alone, `spin(1)` would be a total `spin(1)` again.
#[test]
fn a_definitions_nested_field_call_is_graded_by_the_path_it_projects() {
    control(
        r#"
        use /std/{Nat, Bool};

        let w(n: Nat, f: (Nat) -> Bool) -> {{(Nat) -> Bool, (Nat) -> Bool}} =
            (((x) => match n | 0 => false | k + 1 => f(k) end, (y) => f(n)),);

        let spin(n: Nat) -> Bool = w(n, (c) => spin(c)).0.0(0);

        let Spun(n: Nat) -> /std/Bool/True =
            match spin(n) | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("w(n, (c) => spin(c)).0.0(0)", "w(n, (c) => spin(c)).0.1(0)")])
    .by_elaborator_in("/Spun", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spun", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A record literal handed to a definition hands each field what that field's path is applied with, and nothing another field's is: `walk` applies `h.0` to an element below `t` and `h.1` to `t` itself, so a lambda at the first field may recur on its argument and one at the second may not. The control recurs at the first field and descends; the refusal recurs at the second, which is `sizes(t)` again: bounded by the first field's applications, it would descend.
#[test]
fn a_record_literal_hands_each_field_its_own_applications_alone() {
    control(
        r#"
        use /std/{Nat, Bool, List};

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        let walk(h: {(Rose) -> Nat, (Rose) -> Nat}, t: Rose) -> Nat =
            match t | rose(n, cs) => List/fold(cs, 0, (c, acc) => acc + h.0(c)) + h.1(t) end;

        let sizes(t: Rose) -> Nat = walk(((u) => sizes(u), (v) => 0), t);

        let Small(t: Rose) -> /std/Bool/True =
            match sizes(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("((u) => sizes(u), (v) => 0)", "((u) => 0, (v) => sizes(v))")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// An arm of another case than its scrutinee already is holds no call: no reduction takes it, so the call of the witness on `x` and `y` in the `node` arm of a match on `leaf()` is none `same` makes, and the witness descends where its recursion on `a` does, which a type reaching `same` unfolds. Both checkers read it so, the certifier typing the arm and recording no call. The refusal hands the match a node, whose arm, now taken, calls the witness on binders nothing places below a parameter, which keeps it folded.
#[test]
fn an_arm_its_scrutinees_constructor_rules_out_holds_no_call() {
    control(
        r#"
        use /std/{Eq, Bool};

        induct Tree: Type | leaf() | node(Tree, Tree) end

        concept Same(A: Type): Type { same(A, A) -> Bool }

        satisfy Same(Tree) {
            same(a, b) =
                match Tree/leaf()
                | leaf() =>
                    match a
                    | leaf() => match b | leaf() => true | node(_, _) => false end
                    | node(l, r) => match b | leaf() => false | node(p, q) => Same/same(l, p) && Same/same(r, q) end
                    end
                | node(x, y) => Same/same(x, y)
                end,
        }

        let both: Eq()(Same/same(Tree/leaf(), Tree/leaf()), true) = Eq/refl();

        /std/print(Bool/to_str(Same/same(Tree/leaf(), Tree/leaf())))
        "#,
        b"true",
    )
    .refused(&[("match Tree/leaf()\n", "match Tree/node(Tree/leaf(), Tree/leaf())\n")])
    .saying("is kept folded");
}

// A law closed by reflexivity has its implicit argument inferred as the literal's own `same` applied to `(leaf(), leaf())`, a redex whose binders stand for their arguments, so its `node` arms are of another case than their scrutinees and hold no call: the witness descends, and the law checks against the method it unfolds. The refusal picks a node, whose arm the redex takes and whose call of the witness descends from no parameter.
#[test]
fn a_law_closed_by_reflexivity_holds_no_call_of_an_arm_its_arguments_rule_out() {
    control(
        r#"
        use /std/{Eq, Bool};

        induct Tree: Type | leaf() | node(Tree, Tree) end

        concept Pick(A: Type): Type {
            same(A, A) -> Bool,
            pick: A,
            picked: Eq()(same(pick, pick), true),
        }

        satisfy Pick(Tree) {
            same(a, b) =
                match a
                | leaf() => match b | leaf() => true | node(_, _) => false end
                | node(l, r) => match b | leaf() => false | node(p, q) => Pick/same(l, p) && Pick/same(r, q) end
                end,
            pick = Tree/leaf(),
            picked = Eq/refl(),
        }

        /std/print(Bool/to_str(Pick/same(Pick/pick(@Tree), Tree/leaf())))
        "#,
        b"true",
    )
    .refused(&[("pick = Tree/leaf()", "pick = Tree/node(Tree/leaf(), Tree/leaf())")])
    .by_elaborator_in("/witness~/Pick(/Tree)", |error| {
        matches!(error, curios_elab::Error::MemberNotKnownToTerminate { .. })
    });
}
