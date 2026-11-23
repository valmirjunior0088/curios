//! A descent through what stands between a call and its argument: a function a parameter binds, a callback and where its callee applies it, a lambda's binder, a let, a convoy, the library's recursion on a bound, and a member named only in its goal.

use {
    crate::tests::{rules::test_support::*, test_support::*},
    curios_analysis::Erased,
};

// The control: the same call through a function that is *not* a constructor payload. `again` is a parameter, so nothing relates `again(n)` to anything smaller, the group does not descend, and (V) refuses the proof position. A rule that read every application of a binder as a decrease would accept this, a proof that never reaches its base. Its own control descends on the predecessor where the refusal calls `again(n)`.
#[test]
fn a_call_through_a_parameter_bound_function_does_not_descend() {
    control(
        r#"
        use /std/{Nat};

        let loop(P : Prop, n : Nat, again : (m : Nat) -> Nat, base : P) -> P =
            match n
            | 0 => base
            | k + 1 => loop(P, k, again, base)
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("loop(P, k, again, base)", "loop(P, again(n), again, base)")])
    .by_elaborator_in("/loop", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Proof,
                ..
            }
        )
    })
    .by_certifier_in("/loop", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// The library's recursion along `<` needs no accessibility: `/std/Nat/descend` recurses on a bound its measure fits under, so a proposition recursing along `<` is written once against the library and is total by the successor's descent. The proof is consumed in `List/get`'s erased bound, so nothing about it runs.
#[test]
fn the_library_recursion_on_a_bound_serves_a_proof() {
    let source = r#"
        use /std/{Nat, Str, Char, List};
        use /std/Bool/{True, Holds};
        let two_more(n: Nat) -> Holds(n < n + 2) =
            Nat/descend((k: Nat) => k, (k) => Holds(k < k + 2), (k, ih) => True/qed(), n);
        /std/print(Str/of_char(List/get(@Char, ['a', 'b', 'c'], 1, @two_more(1))))
        "#;
    assert_eq!(run(source), b"b");
}

// The same recursion at a family that is a type depending on its argument: `pick(n)` has type `Shape(n)`, the unit type at zero and `Nat` above it. Written in a type, `pick(5)` puts `recurse` and its local loop under obligation (T), and the checkers reduce it on the bound to `4` with no proof read; the running program computes the same value with the bound's proofs erased.
#[test]
fn the_library_recursion_on_a_bound_computes_at_a_dependent_family() {
    let source = r#"
        use /std/{Nat, Eq};
        let Shape(n: Nat) -> Type = match n | 0 => {} | _ + 1 => Nat end;
        let pick(n: Nat) -> Shape(n) =
            Nat/descend((k: Nat) => k, Shape, (k, _) => match k | 0 => () | kp + 1 => kp end, n);
        let _computed: Eq()(pick(5), 4) = Eq/refl();
        /std/print(Nat/to_str(pick(5)))
        "#;
    assert_eq!(run(source), b"4");
}

// A convoy written by hand around the recursive call: the arm generalizes the witness it descends on and applies the lambda back to it. The analysis grades the redex as its contractum, so `rest` is still read as the payload of `d` and the proof descends.
#[test]
fn a_hand_written_convoy_keeps_its_descent_visible() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Eq, Str};
        use /std/Str/{Scan, step};

        induct Utf8: (Scan, Bytes) -> Prop
        | stop(): (Scan/lead(), 0x[])
        | more(c: Byte, st: Scan, t: Bytes, rest: Utf8(step(c, st), t)): (st, 0x[c, ..t])
        end

        let dv(s: Scan, @b: Bytes, d: Utf8(s, b)) -> Eq()(0, 0) =
            match d
            | stop() => Eq/refl()
            | more(c, st, t, rest) => ((r: Utf8(step(c, st), t)) => dv(step(c, st), r))(rest)
            end;

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// The same descent through a `let` alias of the payload, which is a redex by another spelling.
#[test]
fn a_let_alias_keeps_its_descent_visible() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Eq, Str};
        use /std/Str/{Scan, step};

        induct Utf8: (Scan, Bytes) -> Prop
        | stop(): (Scan/lead(), 0x[])
        | more(c: Byte, st: Scan, t: Bytes, rest: Utf8(step(c, st), t)): (st, 0x[c, ..t])
        end

        let dv(s: Scan, @b: Bytes, d: Utf8(s, b)) -> Eq()(0, 0) =
            match d
            | stop() => Eq/refl()
            | more(c, st, t, rest) => let r = rest; dv(step(c, st), r)
            end;

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// A program a synthesized convoy would refuse: the goal's implicit is still a metavariable when the inner match is elaborated, its spine names every local, and a convoy generalizing the hypothesis `d` would hand the walk a lambda-bound copy it cannot read as below the parameter. At the ambient result there is no convoy: `rest` is the payload of `d`, and the proof descends.
#[test]
fn a_proof_over_a_dependent_hypothesis_descends() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Eq, Str};
        use /std/Str/{Scan, step};

        induct Utf8: (Scan, Bytes) -> Prop
        | stop(): (Scan/lead(), 0x[])
        | more(c: Byte, st: Scan, t: Bytes, rest: Utf8(step(c, st), t)): (st, 0x[c, ..t])
        end

        let dv(s : Scan, @b : Bytes, d : Utf8(s, b)) -> Eq()(0, 0) =
            match s
            | lead() => match d | stop() => Eq/refl() | more(c, _, t, rest) => dv(step(c, Scan/lead()), rest) end
            | cont(_, _, _) => match d | more(c, _, _, rest) => dv(step(c, s), rest) end
            | bad() => match d | more(c, _, _, rest) => dv(step(c, Scan/bad()), rest) end
            end;

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// A member named only in its own goal is a call, at an argument nothing is below. `curios_cert`'s `recheck::recursion_tests` puts the module to the certifier under this name. The control's goal names no member.
#[test]
fn a_member_named_only_in_its_own_goal_is_a_call_its_group_must_descend_on() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        pub let Const(_: True) -> Prop = True;

        pub let g(n: Nat) -> True =
            let held: Const(True/qed()) =
                match n
                | 0 => True/qed()
                | _ + 1 => True/qed()
                end;
            held;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("let held: Const(True/qed())", "let held: Const(g(n))")])
    .by_elaborator_in("/g", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// A lambda applied on the spot to the caller's own argument, whose binder an arm splits and whose call hands the binder back: `again(x)` is `again(n)`. `curios_cert`'s `recheck::recursion_tests` puts the module to the certifier under this name. The control's arm hands on the predecessor.
#[test]
fn a_call_handing_back_a_lambdas_binder_does_not_descend() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        pub let again(n: Nat) -> True =
            ((x: Nat) =>
                match x
                | 0 => True/qed()
                | k + 1 => again(k)
                end)(n);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| k + 1 => again(k)", "| _ + 1 => again(x)")])
    .by_elaborator_in("/again", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// A recursion through a callback a list function applies descends where the function's record files how it applies it: `List/any` applies `pred` to an element below `l`, and `children` is below `t`, so `has_zero(c)` is below `t`, and a type may reach it. The refusal hands the callback the tree rebuilt around the same children, which loops wherever there is a child, and stays as refused as a recursion through a callback was before any record said how one is applied.
#[test]
fn a_call_in_a_callback_descends_where_its_callee_applies_it_below() {
    control(
        r#"
        use /std/{Nat, List, Bool};
        induct Rose : Type
        | node(label : Nat, children : List(Rose))
        end
        let has_zero(t : Rose) -> Bool =
            match t
            | node(label, children) => label == 0 || List/any(children, (c) => has_zero(c))
            end;
        let Zeroed(t : Rose) -> Prop = Bool/Holds(has_zero(t));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "(c) => has_zero(c)",
        "(c) => has_zero(Rose/node(label, children))",
    )])
    .by_elaborator_in("/Zeroed", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Zeroed", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A parameter handed to a definition that applies it as its summary states is applied where that definition applies it, through the call: `any_kept` hands its `pred` to `List/any` over the tail of its list, so its own summary applies `pred` below `l`, and a recursion through a callback handed to `any_kept` descends. The refusal has `any_kept` hand `List/any` a list holding a tree built around `l`, where `pred` is applied at something not below.
#[test]
fn a_callback_its_callee_hands_on_is_graded_where_the_callee_applies_it() {
    control(
        r#"
        use /std/{Nat, List, Bool};
        induct Rose : Type
        | node(label : Nat, children : List(Rose))
        end
        let any_kept(l : List(Rose), pred : (Rose) -> Bool) -> Bool =
            match l
            | [] => false
            | [h, ..t] => pred(h) || List/any(t, pred)
            end;
        let has_zero(t : Rose) -> Bool =
            match t
            | node(label, children) => label == 0 || any_kept(children, (c) => has_zero(c))
            end;
        let Zeroed(t : Rose) -> Prop = Bool/Holds(has_zero(t));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("List/any(t, pred)", "List/any([Rose/node(0, l)], pred)")])
    .by_elaborator_in("/Zeroed", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Zeroed", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A parameter handed on at its own place to a member that takes no parameter there goes to what the member answers, which applies it where no summary of the member's parameters looks: `wrap(n)(f)` hands `f` to the function `wrap` answers, which applies it to `0`, so `hide` publishes nothing for `f`, and a recursion through a callback handed to `hide` is graded as fresh. Read as `wrap`'s parameter, `f` would be summarized by `hide`'s `f(k)` alone, below `n`, and `spin(0)`, which is `spin(0)` again, would be total. The control hands `wrap` a lambda of its own, so `hide` applies `f` below `n` and nowhere else, and `spin` descends.
#[test]
fn a_callback_handed_past_a_members_parameters_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let hide(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => wrap(n)((_) => True/qed())
            | k + 1 => f(k)
            end
        and wrap(m: Nat) -> ((Nat) -> True) -> True =
            match m
            | 0 => (g) => g(0)
            | k + 1 => (g) => hide(k, g)
            end;

        let spin(n: Nat) -> True = hide(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("wrap(n)((_) => True/qed())", "wrap(n)(f)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// Each checker grades a recursion through a `/std` function's callback by its own record of how the function applies it, filed by its own walk over `/std`: `List/find` applies `pred` below `l` in a plain arm, `List/take_while` in a guard of a guarded match, and `List/traverse` under a monad's bind, and a type reaches a recursion through each. The refusal hands each callback the tree rebuilt around the same children, which both checkers refuse at every `Prop` that reaches one; a summary one checker's record files and the other's does not shows here as a refusal by one checker alone, or as the control refused.
#[test]
fn a_recursion_through_a_std_callback_is_graded_alike_by_both_checkers() {
    let refusals = control(
        r#"
        use /std/{Nat, List, Bool, Option};
        use /std/Bool/{Holds};

        induct Rose : Type
        | node(label : Nat, children : List(Rose))
        end

        let via_find(t : Rose) -> Bool =
            match t
            | node(label, children) =>
                label == 0 || match List/find(children, (c) => via_find(c)) | some(_) => true | none() => false end
            end;
        let Found(t : Rose) -> Prop = Holds(via_find(t));

        let via_take_while(t : Rose) -> Nat =
            match t
            | node(label, children) => label + List/len(List/take_while(children, (c) => via_take_while(c) == 0))
            end;
        let Taken(t : Rose) -> Prop = Holds(via_take_while(t) == 0);

        let via_traverse(t : Rose) -> Option(Nat) =
            match t
            | node(label, children) =>
                match List/traverse(children, (c) => via_traverse(c))
                | some(ns) => Option/some(label + List/len(ns))
                | none() => Option/none()
                end
            end;
        let Traversed(t : Rose) -> Prop = Holds(Option/is_some(via_traverse(t)));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("(c) => via_find(c)", "(c) => via_find(Rose/node(label, children))"),
        ("(c) => via_take_while(c)", "(c) => via_take_while(Rose/node(label, children))"),
        ("(c) => via_traverse(c)", "(c) => via_traverse(Rose/node(label, children))"),
    ]);

    for at in ["/Found", "/Taken", "/Traversed"] {
        refusals
            .by_elaborator_in(at, |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_certifier_in(at, |error| {
                matches!(error, curios_cert::Error::NotTotal { .. })
            });
    }
}

// Zero ruled out of a lambda's binder applied on the spot to the parameter is ruled out of the parameter, so a subtraction past it descends, as it does written over the parameter itself: in a dispatch's default, which enumerating `0` rules zero out of, and in a guard's `false` arm alike. The refusals subtract where nothing rules zero out, `chained(0)` and `guarded(0)` being themselves again.
#[test]
fn zero_ruled_out_of_an_applied_lambdas_binder_is_ruled_out_of_what_it_stands_for() {
    let program = control(
        r#"
        use /std/{Nat, Bool};

        let chained(n : Nat) -> Bool =
            ((x : Nat) => match x | 0 => false | 5 => true | _ => chained(x - 1) end)(n);

        let Chained(n : Nat) -> Prop = Bool/Holds(chained(n));

        let guarded(n : Nat) -> Bool =
            ((x : Nat) => match x == 0 | true => false | false => guarded(x - 1) end)(n);

        let Guarded(n : Nat) -> Prop = Bool/Holds(guarded(n));

        /std/print("ok")
        "#,
        b"ok",
    );
    for (edit, at) in [
        (
            ("| 0 => false | 5", "| 0 => chained(x - 1) | 5"),
            "/Chained",
        ),
        (
            ("| true => false |", "| true => guarded(x - 1) |"),
            "/Guarded",
        ),
    ] {
        program
            .refused(&[edit])
            .by_elaborator_in(at, |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_certifier_in(at, |error| {
                matches!(error, curios_cert::Error::NotTotal { .. })
            });
    }
}

// A `let` bound before an arm over a variable the arm's index inversion solves is graded as that variable, as written, since the size order reads nothing an inversion concluded: `hop(u)` is `hop(t)`, as the inlined group's `step(t)` is, and the cycle through it descends on `j`. The refusal hands `hop` the successor, which `j` is no longer below.
#[test]
fn a_let_over_a_variable_an_inversion_solves_is_graded_as_the_variable() {
    control(
        r#"
        use /std/{Nat, Bool};

        induct Q : (Nat) -> Type
        | c(k : Nat) : (k)
        end

        let inlined(t : Nat, q : Q(t)) -> Bool =
            match q
            | c(k) => step(t)
            end
        and step(m : Nat) -> Bool =
            match m
            | 0 => false
            | j + 1 => inlined(j, Q/c(j))
            end;

        let Inlined(t : Nat, q : Q(t)) -> Prop = Bool/Holds(inlined(t, q));

        let bound(t : Nat, q : Q(t)) -> Bool =
            let u = t;
            match q
            | c(k) => hop(u)
            end
        and hop(m : Nat) -> Bool =
            match m
            | 0 => false
            | j + 1 => bound(j, Q/c(j))
            end;

        let Bound(t : Nat, q : Q(t)) -> Prop = Bool/Holds(bound(t, q));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| c(k) => hop(u)", "| c(k) => hop(u + 1)")])
    .by_elaborator_in("/Bound", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Bound", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}
