//! The size-change obligation through a callback, part by part: a definition's summary states each parameter's applications as the part that makes them, a field lambda's — itself or through a member it hands the parameter on to — as the field's and never as its leading lambdas'.

use super::test_support::*;

// A callback a field lambda hands on to a member at its own place is applied wherever the field is called, so the summary of the leading lambdas states nothing of it: `def` applies `f` below `n` in its leading lambdas, and in the control its field hands `app` a lambda of its own, so `spin`, which hands `def` its lambda and lets the record escape to `h`, descends. The refusal has the field hand `f` itself on to `app`, which applies it at `n`; read by the leading lambdas' `f(k)` alone, `spin(0)`, which is `h(def(0, …), 0)`, which is `app(0, …)`, would be a total `spin(0)` again.
#[test]
fn a_callback_a_field_lambda_hands_on_to_a_member_is_applied_where_the_field_is_called() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let def(n: Nat, f: (Nat) -> True) -> {Nat, (Nat) -> True} =
            (
                match n
                | 0 => 0
                | k + 1 =>
                    let _ = f(k);
                    0
                end,
                (m) => app(n, (_) => True/qed()))
        and app(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => f(n)
            | k + 1 =>
                let _ = def(k, f);
                f(n)
            end;

        let h(r: {Nat, (Nat) -> True}, y: Nat) -> True =
            r.1(y);

        let spin(n: Nat) -> True =
            h(def(n, (c) => spin(c)), n);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(m) => app(n, (_) => True/qed())", "(m) => app(n, f)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A member whose leading lambdas answer another member's record states, of a parameter it hands that member at its own place, what that record's fields apply too: `outer` answers `inner(t, f)`, a call that lets `inner`'s record escape, so the summary of `outer`'s leading lambdas closes over a call of each of `inner`'s parts, its field lambda among them, which `call` applies. The control's field applies nothing, so `outer` applies `f` below `t` alone and `size`'s recursion through the lambda it hands `outer` descends. The refusal has `inner`'s field apply `f` at `t`: stated as `inner`'s leading lambdas' rows alone, `outer`'s summary would hold `f(l)` and `f(r)`, and `size(c)` would be graded below `t` while `call` reaches `f(t)`, `size(t)` again.
#[test]
fn a_member_answering_another_members_record_states_what_its_fields_apply() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Tree: Type
        | leaf(Nat)
        | node(Tree, Tree)
        end

        let outer(t: Tree, f: (Tree) -> Nat) -> {Nat, (Nat) -> Nat} = inner(t, f)
        and inner(t: Tree, f: (Tree) -> Nat) -> {Nat, (Nat) -> Nat} =
            (match t | leaf(n) => n | node(l, r) => let _ = outer(l, f); f(l) + f(r) end, (m) => m);

        let call(r: {Nat, (Nat) -> Nat}, y: Nat) -> Nat = r.1(y);

        let size(t: Tree) -> Nat = call(outer(t, (c) => size(c)), 0);

        let Small(t: Tree) -> /std/Bool/True =
            match size(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(m) => m)", "(m) => m + f(t))")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A field lambda that applies a field of a definition's parameter withholds the leading lambdas' summary of the parameter, its field rows with it: `sizes` applies `e.0` below `t` in the record's first field, and its field lambda is called wherever `call` applies it, at arguments no row of the leading lambdas states. The control's field lambda applies nothing, so the record literal `size` hands `sizes` binds its field below `t`. The refusal has the field lambda apply `e.0` at `t`: stated by the leading lambdas' rows, `size(c)` would be graded below `t` while `call` reaches `e.0(t)`, `size(t)` again.
#[test]
fn a_field_lambda_applying_a_parameters_field_withholds_the_leading_lambdas_summary_of_it() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Tree: Type
        | leaf(Nat)
        | node(Tree, Tree)
        end

        let sizes(t: Tree, e: {(Tree) -> Nat}) -> {Nat, (Nat) -> Nat} =
            (match t | leaf(n) => n | node(l, r) => e.0(l) + e.0(r) end, (m) => m);

        let call(r: {Nat, (Nat) -> Nat}, y: Nat) -> Nat = r.1(y);

        let size(t: Tree) -> Nat = call(sizes(t, ((c) => size(c),)), 0);

        let Small(t: Tree) -> /std/Bool/True =
            match size(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(m) => m)", "(m) => m + e.0(t))")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// What counts as a field lambda for a summary is what counts as a part for the call graph, a record literal held at a field followed: `sizes`'s lambda at `.1.0` is a part of its own, called wherever `call` applies it, so a parameter it applies is one the leading lambdas' summary states nothing of. The control's lambda applies nothing, so `sizes` applies `f` below `t` alone and `size` descends. The refusal has the lambda apply `f` at `t`: read as no part, it would leave `f(l)` and `f(r)` the leading lambdas' rows, and `size(c)` graded below `t` while `call` reaches `f(t)`, `size(t)` again.
#[test]
fn a_field_lambda_in_a_nested_record_withholds_the_leading_lambdas_summary_of_what_it_applies() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Tree: Type
        | leaf(Nat)
        | node(Tree, Tree)
        end

        let sizes(t: Tree, f: (Tree) -> Nat) -> {Nat, {(Nat) -> Nat}} =
            (match t | leaf(n) => n | node(l, r) => f(l) + f(r) end, ((m) => m,));

        let call(r: {Nat, {(Nat) -> Nat}}, y: Nat) -> Nat = r.1.0(y);

        let size(t: Tree) -> Nat = call(sizes(t, (c) => size(c)), 0);

        let Small(t: Tree) -> /std/Bool/True =
            match size(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("((m) => m,)", "((m) => m + f(t),)")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A field lambda's summary closes over the calls of its group it hands the parameter on through, however many: `sizes`'s field hands `f` at its own place to `one`, which hands it to `two`, which applies it to the children of `t`, so the path `sizes(t, …).1(0)` binds `size`'s lambda below `t` and `size` descends. The refusal has `two` apply `f` at `t`: closed over one call, or none, the field's summary would hold nothing of it, and `size(c)` would be graded as the lambda's other applications leave it while `sizes(t, …).1(0)` reaches `f(t)`, `size(t)` again.
#[test]
fn a_field_lambdas_summary_closes_over_the_calls_its_group_hands_the_parameter_on_through() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Tree: Type
        | leaf(Nat)
        | node(Tree, Tree)
        end

        let sizes(t: Tree, f: (Tree) -> Nat) -> {Nat, (Nat) -> Nat} = (0, (_) => one(t, f))
        and one(t: Tree, f: (Tree) -> Nat) -> Nat = two(t, f)
        and two(t: Tree, f: (Tree) -> Nat) -> Nat =
            match t | leaf(n) => n | node(l, r) => let _ = sizes(l, f); f(l) + f(r) end;

        let size(t: Tree) -> Nat = sizes(t, (c) => size(c)).1(0);

        let Small(t: Tree) -> /std/Bool/True =
            match size(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("f(l) + f(r)", "f(l) + f(t)")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}
