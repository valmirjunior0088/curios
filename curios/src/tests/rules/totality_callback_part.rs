//! The size-change obligation through a callback, part by part: a definition's summary states each parameter's applications as the part that makes them, a field lambda's — itself or through a member it hands the parameter on to — as the field's and never as its leading lambdas'.

use super::test_support::*;

// A callback a field lambda hands on to a member at its own place is applied wherever the field is called, as the field's own: `def` applies `f` below `n` in its leading lambdas, and in the control its field hands `app` a lambda of its own, so `spin`, which hands `def` its lambda and lets the record escape to `h`, descends. The refusal has the field hand `f` itself on to `app`, which applies it at `n`; read by the leading lambdas' `f(k)` alone, `spin(0)`, which is `h(def(0, …), 0)`, which is `app(0, …)`, would be a total `spin(0)` again.
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

// A field lambda that applies a field of a definition's parameter states it as its own, which a call of the field reads beside what the leading lambdas apply as they answer: `sizes` applies `e.0` below `t` in the record's first field, and its field lambda is called wherever `call` applies it, at arguments no row of the leading lambdas states. The control's field lambda applies nothing, so the record literal `size` hands `sizes` binds its field below `t`. The refusal has the field lambda apply `e.0` at `t`: read by the leading lambdas' rows alone, `size(c)` would be graded below `t` while `call` reaches `e.0(t)`, `size(t)` again.
#[test]
fn a_field_lambda_applying_a_parameters_field_states_it_as_its_own() {
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

// What counts as a field lambda for a summary is what counts as a part for the call graph, a record literal held at a field followed: `sizes`'s lambda at `.1.0` is a part of its own, called wherever `call` applies it, so what it applies of a parameter is its own row, which a call of it reads beside the leading lambdas'. The control's lambda applies nothing, so `sizes` applies `f` below `t` alone and `size` descends. The refusal has the lambda apply `f` at `t`: read as no part, it would leave `f(l)` and `f(r)` the leading lambdas' rows, and `size(c)` graded below `t` while `call` reaches `f(t)`, `size(t)` again.
#[test]
fn a_field_lambda_in_a_nested_record_states_what_it_applies_as_its_own() {
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

// A field lambda is called on a record its definition's leading lambdas answered, so what they apply of a parameter where they answer it is applied wherever the field is called: `d`'s plain field holds a number and its field lambda applies `f` below its own argument, so `g`, which calls the field by its path, descends. The refusal has the plain field hold `f(100)`: read by the field lambda's own applications alone, `g` would be known to terminate while answering the record calls `g(100)`.
#[test]
fn a_field_called_by_its_path_states_what_its_record_applied_where_it_was_answered() {
    control(
        r#"
        use /std/{Nat, Bool};

        struct Handlers: Type {
            y: Nat,
            m: (Nat) -> Nat,
        }

        let d(f: (Nat) -> Nat) -> Handlers =
            Handlers { y = 0, m = (x) => match x | 0 => 0 | n + 1 => f(n) end };

        let g(t: Nat) -> Nat = d(g).m(t);

        let Small(t: Nat) -> /std/Bool/True =
            match g(t) == 0 | true => /std/Bool/True/qed() | false => /std/Bool/True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("y = 0,", "y = f(100),")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A member's field handed to a definition that applies what it is handed is the member at that path: `all` applies its `e`'s `step` to each element of `s`, so `w.inner` handed it is `w`'s part at `inner.step`, applied below `d` where `s` is `d`'s payload, and `w` descends as it does handed whole through a definition that projects the field itself. The refusal hands `all` a list built around `d`, at which the part is applied at `d` itself.
#[test]
fn a_members_field_handed_where_its_callee_applies_it_is_the_member_at_that_path() {
    control(
        r#"
        use /std/{List};
        use /std/Bool/{True};

        induct Doc: Type | leaf() | arr(List(Doc)) end

        struct Inner(A: Type): Type { step: (A) -> True }

        struct Outer(A: Type): Type { inner: Inner(A) }

        let all(@T: Type, e: Inner(T), s: List(T)) -> True =
            match s | [] => True/qed() | [x, ..rest] => match e.step(x) | qed() => all(e, rest) end end;

        let w: Outer(Doc) = Outer {
            inner = Inner { step(d) = match d | leaf() => True/qed() | arr(s) => all(w.inner, s) end },
        };

        let _ = w.inner.step(Doc/arr([Doc/leaf()]));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("all(w.inner, s)", "all(w.inner, [d])")])
    .by_elaborator_in("/w", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// A witness whose superclass is built from a premise's field read as a value publishes what it applies of the premise: `mark: Nat` holds nothing that can be applied, so `List`'s `Mark` witness reading `Mark/mark(@T)` uses its premise as no call does, and `List`'s `Big` witness, whose superclass slot holds it, states what `all_big` applies of its own premise, through which `Doc`'s witness descends. The refusal makes `mark` a record holding a function, read as a value and handed on, where it may be applied wherever it goes.
#[test]
fn a_premises_field_read_as_a_value_that_holds_no_function_is_no_use() {
    control(
        r#"
        use /std/{Nat, List};
        use /std/Bool/{True};

        induct Doc: Type | leaf() | arr(List(Doc)) end

        concept Mark(A: Type): pub Type { mark: Nat }

        concept Big(A: Type): pub Type { use Mark(A), big(A) -> True }

        satisfy Mark(Doc) { mark = 0 }

        satisfy (@T: Type, use Mark(T)) => Mark(List(T)) { mark = Mark/mark(@T) + 1 }

        let all_big(@T: Type, use Big(T), xs: List(T)) -> True =
            match xs | [] => True/qed() | [x, ..rest] => match Big/big(x) | qed() => all_big(rest) end end;

        satisfy (@T: Type, use Big(T)) => Big(List(T)) { big = all_big }

        satisfy Big(Doc) { big(d) = match d | leaf() => True/qed() | arr(xs) => Big/big(xs) end }

        let _ = Big/big(Doc/arr([Doc/leaf()]));
        /std/print(Nat/to_str(Mark/mark(@List(Doc))))
        "#,
        b"1",
    )
    .refused(&[
        ("concept Mark(A: Type): pub Type { mark: Nat }", "struct Box: Type { f: (Nat) -> Nat }\n\n        concept Mark(A: Type): pub Type { mark: Box }"),
        ("satisfy Mark(Doc) { mark = 0 }", "satisfy Mark(Doc) { mark = Box { f(n) = n } }"),
        ("{ mark = Mark/mark(@T) + 1 }", "{ mark = Mark/mark(@T) }"),
        ("/std/print(Nat/to_str(Mark/mark(@List(Doc))))", "/std/print(Nat/to_str(Mark/mark(@List(Doc)).f(1)))"),
    ])
    .by_elaborator_in("/witness~/Big(/Doc)", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// A definition that applies nothing of a parameter every use of which is seen states so, and a caller handing it the parameter makes no use of it there: `List`'s `Mark` witness never reads its premise, so `List`'s `Big` witness, whose superclass slot hands it the premise's own superclass, states what `all_big` applies of its premise, through which `Doc`'s witness descends. The refusal reads the premise's `mark`, a record holding a function, as a value and hands it on, where it may be applied wherever it goes.
#[test]
fn a_definition_that_applies_nothing_of_its_parameter_states_so_to_its_callers() {
    control(
        r#"
        use /std/{Nat, List};
        use /std/Bool/{True};

        induct Doc: Type | leaf() | arr(List(Doc)) end

        concept Mark(A: Type): pub Type { mark: Nat }

        concept Big(A: Type): pub Type { use Mark(A), big(A) -> True }

        satisfy Mark(Doc) { mark = 0 }

        satisfy (@T: Type, use Mark(T)) => Mark(List(T)) { mark = 1 }

        let all_big(@T: Type, use Big(T), xs: List(T)) -> True =
            match xs | [] => True/qed() | [x, ..rest] => match Big/big(x) | qed() => all_big(rest) end end;

        satisfy (@T: Type, use Big(T)) => Big(List(T)) { big = all_big }

        satisfy Big(Doc) { big(d) = match d | leaf() => True/qed() | arr(xs) => Big/big(xs) end }

        let _ = Big/big(Doc/arr([Doc/leaf()]));
        /std/print(Nat/to_str(Mark/mark(@List(Doc))))
        "#,
        b"1",
    )
    .refused(&[
        ("concept Mark(A: Type): pub Type { mark: Nat }", "struct Box: Type { f: (Nat) -> Nat }\n\n        concept Mark(A: Type): pub Type { mark: Box }"),
        ("satisfy Mark(Doc) { mark = 0 }", "satisfy Mark(Doc) { mark = Box { f(n) = n } }"),
        ("{ mark = 1 }", "{ mark = Mark/mark(@T) }"),
        ("/std/print(Nat/to_str(Mark/mark(@List(Doc))))", "/std/print(Nat/to_str(Mark/mark(@List(Doc)).f(1)))"),
    ])
    .by_elaborator_in("/witness~/Big(/Doc)", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// A member handed where its callee applies it nowhere, every use of it seen, makes no call there: `ignore` never applies `f`, so `go` handing itself to it calls nothing of itself, whole or wrapped in a lambda, and both checkers accept. The refusal applies `f` at the number it is handed, so `go` reaches itself at its own argument.
#[test]
fn a_member_handed_where_its_callee_applies_it_nowhere_makes_no_call() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let ignore(f: (Nat) -> True, n: Nat) -> True = True/qed();

        let go(n: Nat) -> True = match n | 0 => True/qed() | k + 1 => ignore(go, n) end;

        let wrapped(n: Nat) -> True = match n | 0 => True/qed() | k + 1 => ignore((m) => wrapped(m), n) end;

        let _ = go(3);
        let _ = wrapped(3);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("= True/qed();\n", "= f(n);\n")])
    .by_elaborator_in("/go", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}
