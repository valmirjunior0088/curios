//! A call in a callback, graded where its callee applies it: an application of a callback is graded against the member's parameters at the place it stands, whatever binder it stands under, and the bound a call hands a lambda's binders is each binder's own, in the lambda the callee applies and nowhere past it.

use super::test_support::*;

// Appended to curios/src/tests/rules/totality_descent_tests.rs; the file's imports already hold every name below.

// A callback applied through a lambda applied on the spot, whose binder shadows the arm's payload, is graded at the argument the binder stands for: `((h : Rose) => p(h))(h)` is `p` applied to the payload, below `l`, and `has_zero` descends through `some`. The refusal hands the lambda the tree rebuilt around `l`, so `p` is applied to something above `l`; read as the arm's `h` it would grade below `l`, and `has_zero(node(1, …))` would recurse on itself through `some` while a type reaches it.
#[test]
fn a_callback_applied_through_a_lambda_whose_binder_shadows_a_payload_is_graded_at_the_lambdas_argument()
 {
    control(
        r#"
        use /std/{Nat, List, Bool};
        induct Rose : Type
        | node(label : Nat, children : List(Rose))
        end
        let some(l : List(Rose), p : (Rose) -> Bool) -> Bool =
            match l
            | [] => false
            | [h, ..t] => ((h : Rose) => p(h))(h) || some(t, p)
            end;
        let has_zero(t : Rose) -> Bool =
            match t
            | node(label, children) => label == 0 || some(children, (c) => has_zero(c))
            end;
        let Zeroed(t : Rose) -> Prop = Bool/Holds(has_zero(t));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "((h : Rose) => p(h))(h)",
        "((h : Rose) => p(h))(Rose/node(1, l))",
    )])
    .by_elaborator_in("/Zeroed", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Zeroed", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A callback applied to a local that shadows the arm's payload is graded at the local's value: `let t = h; p(t)` applies `p` to the head, below `l`. The refusal binds the shadowing `t` to the tree rebuilt around `l`; read as the arm's tail it would grade below `l`.
#[test]
fn a_callback_applied_to_a_local_that_shadows_a_payload_is_graded_at_the_locals_value() {
    control(
        r#"
        use /std/{Nat, List, Bool};
        induct Rose : Type
        | node(label : Nat, children : List(Rose))
        end
        let some(l : List(Rose), p : (Rose) -> Bool) -> Bool =
            match l
            | [] => false
            | [h, ..t] => (let t = h; p(t)) || some(t, p)
            end;
        let has_zero(t : Rose) -> Bool =
            match t
            | node(label, children) => label == 0 || some(children, (c) => has_zero(c))
            end;
        let Zeroed(t : Rose) -> Prop = Bool/Holds(has_zero(t));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(let t = h; p(t))", "(let t = Rose/node(1, l); p(t))")])
    .by_elaborator_in("/Zeroed", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Zeroed", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A callback applied inside a local recursive function is graded where the function is written, against the enclosing member's parameters: `go` applies `p` to the arm's payload `h`, which it captures, so `p` is applied below `l` however often `go` recurses. The refusal gives `go` a parameter that shadows the payload and calls it with the tree rebuilt around `l`; a nested member's parameter stands below nothing the enclosing member was handed, and read as the arm's `h` it would grade below `l`.
#[test]
fn a_callback_applied_to_a_nested_members_parameter_that_shadows_a_payload_is_graded_as_fresh() {
    control(
        r#"
        use /std/{Nat, List, Bool};
        induct Rose : Type
        | node(label : Nat, children : List(Rose))
        end
        let nested(l : List(Rose), p : (Rose) -> Bool) -> Bool =
            match l
            | [] => false
            | [h, ..t] =>
                (let go(n : Nat) -> Bool = match n | 0 => p(h) | k + 1 => go(k) end; go(3))
                || nested(t, p)
            end;
        let has_one(t : Rose) -> Bool =
            match t
            | node(label, children) => label == 1 || nested(children, (c) => has_one(c))
            end;
        let Oned(t : Rose) -> Prop = Bool/Holds(has_one(t));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("go(n : Nat)", "go(h : Rose, n : Nat)"),
        ("go(k)", "go(h, k)"),
        ("go(3)", "go(Rose/node(1, l), 3)"),
    ])
    .by_elaborator_in("/Oned", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Oned", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// What an arm establishes of the scrutinee grades a callback applied inside that arm alone. `upto` applies `p(4)` in the arm where `n` is `5`, below `n`, and `p(n - 1)` in the catch-all, which enumerating `0` rules zero out of; the refusal applies `p(4)` in the catch-all too, where `n` is no literal and `4` is below nothing. The literal arm is walked first, so its case left standing would grade the catch-all's `p(4)` below `n`. The second pair is the boolean form, whose `false` arm is walked before its `true` arm: `n - 1` is below `n` where `n == 0` is false and at most `n` where it is true, so a nonzero fact left standing past the `false` arm would grade the `true` arm's `p(n - 1)` as a descent.
#[test]
fn what_an_arm_establishes_grades_no_callback_applied_outside_it() {
    control(
        r#"
        use /std/{Nat, Bool};
        let upto(n : Nat, p : (Nat) -> Bool) -> Bool =
            match n
            | 0 => false
            | 5 => p(4) || upto(4, p)
            | _ => p(n - 1) || upto(n - 1, p)
            end;
        let deep(n : Nat) -> Bool = upto(n, (m) => deep(m));
        let Deep(n : Nat) -> Prop = Bool/Holds(deep(n));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| _ => p(n - 1)", "| _ => p(4)")])
    .by_elaborator_in("/Deep", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Deep", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
    control(
        r#"
        use /std/{Nat, Bool};
        let down(n : Nat, p : (Nat) -> Bool) -> Bool =
            match n == 0
            | false => p(n - 1) || down(n - 1, p)
            | true => false
            end;
        let deep(n : Nat) -> Bool = down(n, (m) => deep(m));
        let Deep(n : Nat) -> Prop = Bool/Holds(deep(n));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| true => false", "| true => p(n - 1)")])
    .by_elaborator_in("/Deep", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Deep", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// An arm splitting a lambda's binder that stands for the parameter splits the parameter for as long as the arm stands, and no longer: `chained`'s lambda is applied on the spot to `n`, so its arm `k + 1` makes `n` the successor of `k` and `p(k)` descends. The refusal applies `p(n - 1)` past the match, where `n` is split by nothing and `n - 1` is at most `n`; the split left standing would read `n` as `k + 1` there and `p(n - 1)` as below it.
#[test]
fn an_arm_splitting_an_applied_lambdas_binder_grades_no_callback_applied_past_it() {
    control(
        r#"
        use /std/{Nat, Bool};
        let chained(n : Nat, p : (Nat) -> Bool) -> Bool =
            ((x : Nat) => match x | 0 => false | k + 1 => p(k) || chained(k, p) end)(n);
        let deeper(n : Nat) -> Bool = chained(n, (m) => deeper(m));
        let Deeper(n : Nat) -> Prop = Bool/Holds(deeper(n));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "match x | 0 => false | k + 1 => p(k) || chained(k, p) end",
        "(match x | 0 => false | k + 1 => chained(k, p) end) || p(n - 1)",
    )])
    .by_elaborator_in("/Deeper", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Deeper", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A callback a sibling applies is graded through the call that hands it on: `walk` hands `p` to `visit` with its own `l`, and `visit` applies `p` to an element below that, so `walk`'s summary grades the element below `walk`'s `l`. The refusal hands `visit` the list rebuilt around `l` — the group still descends on `fuel` — so `visit`'s application is below what `walk` hands it and nothing `walk` was handed; composed apart from the call, `visit`'s own grading would read it below `l`, and `has_zero(node(1, …))` would recurse on itself.
#[test]
fn a_callback_a_sibling_applies_is_graded_through_what_the_call_hands_the_sibling() {
    control(
        r#"
        use /std/{Nat, List, Bool};
        induct Rose : Type
        | node(label : Nat, children : List(Rose))
        end
        let walk(fuel : Nat, l : List(Rose), p : (Rose) -> Bool) -> Bool =
            match fuel
            | 0 => false
            | k + 1 => visit(k, l, p)
            end
        and visit(fuel : Nat, l : List(Rose), p : (Rose) -> Bool) -> Bool =
            match l
            | [] => false
            | [h, ..t] => p(h) || walk(fuel, t, p)
            end;
        let has_zero(t : Rose) -> Bool =
            match t
            | node(label, children) => label == 0 || walk(9, children, (c) => has_zero(c))
            end;
        let Zeroed(t : Rose) -> Prop = Bool/Holds(has_zero(t));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("visit(k, l, p)", "visit(k, [Rose/node(1, l), ..l], p)")])
    .by_elaborator_in("/Zeroed", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Zeroed", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// Each member of a group publishes how it applies its own parameter, and a call is graded by the summary of the member it calls: `below` applies `p` to `l`'s tail and reaches `above`'s application of `p` to its own `l` only through a tail, so it grades every application below `l`, while `above` applies `p` to `l` itself. A recursion through `below` descends, written with a lambda, handing a member of its own group bare, or applying a sibling under the lambda's binder; the refusals recurse through `above`, which a summary filed under the other member would grade as `below`'s.
#[test]
fn each_member_of_a_group_publishes_how_it_applies_its_own_callback() {
    let group = control(
        r#"
        use /std/{Nat, List, Bool};
        let below(l : List(Nat), p : (List(Nat)) -> Bool) -> Bool =
            match l
            | [] => false
            | [_, ..t] => p(t) || above(t, p)
            end
        and above(l : List(Nat), p : (List(Nat)) -> Bool) -> Bool =
            match l
            | [] => false
            | [_, ..t] => p(l) || below(t, p)
            end;
        let zeros(l : List(Nat)) -> Bool = below(l, (c) => zeros(c));
        let Zeros(l : List(Nat)) -> Prop = Bool/Holds(zeros(l));
        let handed(l : List(Nat)) -> Bool = below(l, twice)
        and twice(l : List(Nat)) -> Bool = handed(l);
        let Handed(l : List(Nat)) -> Prop = Bool/Holds(handed(l));
        let sibling(l : List(Nat)) -> Bool = below(l, (c) => other(c))
        and other(l : List(Nat)) -> Bool = sibling(l);
        let Sibling(l : List(Nat)) -> Prop = Bool/Holds(sibling(l));
        /std/print("ok")
        "#,
        b"ok",
    );
    group
        .refused(&[("below(l, (c) => zeros(c))", "above(l, (c) => zeros(c))")])
        .by_elaborator_in("/Zeros", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_certifier_in("/Zeros", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
    group
        .refused(&[("below(l, twice)", "above(l, twice)")])
        .by_elaborator_in("/Handed", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_certifier_in("/Handed", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
    group
        .refused(&[("below(l, (c) => other(c))", "above(l, (c) => other(c))")])
        .by_elaborator_in("/Sibling", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_certifier_in("/Sibling", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
}

// A parameter whose applications close to more matrices than a summary holds is summarized as used otherwise, so a recursion through it is graded as fresh: `app` applies `f` with its first argument below `a` in every one of sixty-four applications, each a distinct matrix, and `deep` descends on that argument. The refusal adds a sixty-fifth, as far below `a` as the rest, and the summary holds nothing; a summary kept to its first sixty-four would hold whatever the order of the set left in it.
#[test]
fn a_callback_applied_past_the_applications_limit_is_graded_as_fresh() {
    control(
        r#"
        use /std/{Nat, Bool};
        let app(a : Nat, b : Nat, c : Nat, f : (Nat, Nat, Nat, Nat, Nat) -> Bool) -> Bool =
            match a
            | 0 => false
            | x + 1 =>
                match b
                | 0 => false
                | y + 1 =>
                    match c
                    | 0 => false
                    | z + 1 =>
                        f(x, x, x, x, x) || f(x, x, x, x, y) || f(x, x, x, x, z) || f(x, x, x, y, x)
                        || f(x, x, x, y, y) || f(x, x, x, y, z) || f(x, x, x, z, x) || f(x, x, x, z, y)
                        || f(x, x, x, z, z) || f(x, x, y, x, x) || f(x, x, y, x, y) || f(x, x, y, x, z)
                        || f(x, x, y, y, x) || f(x, x, y, y, y) || f(x, x, y, y, z) || f(x, x, y, z, x)
                        || f(x, x, y, z, y) || f(x, x, y, z, z) || f(x, x, z, x, x) || f(x, x, z, x, y)
                        || f(x, x, z, x, z) || f(x, x, z, y, x) || f(x, x, z, y, y) || f(x, x, z, y, z)
                        || f(x, x, z, z, x) || f(x, x, z, z, y) || f(x, x, z, z, z) || f(x, y, x, x, x)
                        || f(x, y, x, x, y) || f(x, y, x, x, z) || f(x, y, x, y, x) || f(x, y, x, y, y)
                        || f(x, y, x, y, z) || f(x, y, x, z, x) || f(x, y, x, z, y) || f(x, y, x, z, z)
                        || f(x, y, y, x, x) || f(x, y, y, x, y) || f(x, y, y, x, z) || f(x, y, y, y, x)
                        || f(x, y, y, y, y) || f(x, y, y, y, z) || f(x, y, y, z, x) || f(x, y, y, z, y)
                        || f(x, y, y, z, z) || f(x, y, z, x, x) || f(x, y, z, x, y) || f(x, y, z, x, z)
                        || f(x, y, z, y, x) || f(x, y, z, y, y) || f(x, y, z, y, z) || f(x, y, z, z, x)
                        || f(x, y, z, z, y) || f(x, y, z, z, z) || f(x, z, x, x, x) || f(x, z, x, x, y)
                        || f(x, z, x, x, z) || f(x, z, x, y, x) || f(x, z, x, y, y) || f(x, z, x, y, z)
                        || f(x, z, x, z, x) || f(x, z, x, z, y) || f(x, z, x, z, z) || f(x, z, y, x, x)
                        || app(x, y, z, f)
                    end
                end
            end;
        let deep(a : Nat, b : Nat, c : Nat) -> Bool = app(a, b, c, (p, q, r, s, t) => deep(p, p, p));
        let Deep(a : Nat) -> Prop = Bool/Holds(deep(a, a, a));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("|| app(x, y, z, f)", "|| f(x, z, y, x, y) || app(x, y, z, f)")])
    .by_elaborator_in("/Deep", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Deep", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A binder that takes a parameter's name stands for what it binds, in the callee and in the caller alike: `app`'s arm `n + 1` binds the predecessor as `n`, so `f(n)` is applied below the parameter `n`, and `shadowed`'s lambda takes the name `n` and is applied to the parameter, so the callback `app` hands it is below `shadowed`'s `n`. The refusals apply `f` to the arm's `n + 1`, the parameter itself, and apply the lambda to `n + 1`, so `app` applies the callback to `n`, the parameter again; each binder read as the name it shadows would grade below.
#[test]
fn a_binder_that_takes_a_parameters_name_stands_for_what_it_binds() {
    let shadows = control(
        r#"
        use /std/{Nat, Bool};
        let app(n : Nat, f : (Nat) -> Bool) -> Bool =
            match n
            | 0 => false
            | n + 1 => f(n) || app(n, f)
            end;
        let deep(n : Nat) -> Bool = app(n, (m) => deep(m));
        let Deep(n : Nat) -> Prop = Bool/Holds(deep(n));
        let shadowed(n : Nat) -> Bool = ((n : Nat) => app(n, (m) => shadowed(m)))(n);
        let Shadowed(n : Nat) -> Prop = Bool/Holds(shadowed(n));
        /std/print("ok")
        "#,
        b"ok",
    );
    shadows
        .refused(&[("| n + 1 => f(n)", "| n + 1 => f(n + 1)")])
        .by_elaborator_in("/Deep", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_certifier_in("/Deep", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
    shadows
        .refused(&[("shadowed(m)))(n)", "shadowed(m)))(n + 1)")])
        .by_elaborator_in("/Shadowed", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_certifier_in("/Shadowed", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
}

// A bound a callback is handed stays with its binders: `apply_inner` applies `p` to a child below `t` and the function that answers to `r`, so `c` stands below `t` and the lambda the callback answers binds `e` to `r`, which `size` hands `t` itself. `curios_cert`'s `recheck::callback_tests` puts the same claim to the certifier alone under this name. The refusal recurses on `e`, which is `size(t)` again wherever `t` has a child.
#[test]
fn a_lambda_a_callback_answers_takes_none_of_the_callbacks_bounds() {
    control(
        r#"
        use /std/{Nat, Bool};
        induct Tr : Type
        | leaf(n : Nat)
        | node(a : Tr, b : Tr)
        end
        let apply_inner(t : Tr, r : Tr, p : (Tr) -> (Tr) -> Nat) -> Nat =
            match t
            | leaf(n) => n
            | node(a, b) => p(a)(r) + apply_inner(b, r, p)
            end;
        let size(t : Tr) -> Nat = apply_inner(t, t, (c) => (e) => 1 + size(c));
        let Sized(t : Tr) -> Prop = Bool/Holds(size(t) == 2);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(e) => 1 + size(c)", "(e) => 1 + size(e)")])
    .by_elaborator_in("/Sized", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Sized", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// Each argument of a call is bounded by the applications of its own parameter: `split` applies `p` to a child below `t` and `q` to `r` unchanged, so the callback at `q` stands at `t` itself, beside one below it. `curios_cert`'s `recheck::callback_tests` puts the same claim to the certifier alone under this name. The refusal recurses on `q`'s binder, which is `size(t)` again wherever `t` has a child.
#[test]
fn a_callbacks_sibling_is_bounded_by_its_own_applications() {
    control(
        r#"
        use /std/{Nat, Bool};
        induct Tr : Type
        | leaf(n : Nat)
        | node(a : Tr, b : Tr)
        end
        let split(t : Tr, r : Tr, p : (Tr) -> Nat, q : (Tr) -> Nat) -> Nat =
            match t
            | leaf(n) => n
            | node(a, b) => p(a) + q(r) + split(b, r, p, q)
            end;
        let size(t : Tr) -> Nat = split(t, t, (c) => 1 + size(c), (e) => 0);
        let Sized(t : Tr) -> Prop = Bool/Holds(size(t) == 2);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(e) => 0", "(e) => size(e)")])
    .by_elaborator_in("/Sized", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Sized", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A lambda a callback hands on is bounded through the call that hands it, and by nothing the callback was handed: inside `each`'s callback, `at(0, c, f)` applies `f` to `c`, which is below `t`, so `d` is too. The refusal hands `at` the whole of `t`, which `f` is applied to unchanged, beside a `c` still below it.
#[test]
fn a_lambda_a_callback_hands_on_is_bounded_through_the_call_that_hands_it() {
    control(
        r#"
        use /std/{Nat, Bool};
        induct Tr : Type
        | leaf(n : Nat)
        | node(a : Tr, b : Tr)
        end
        let each(t : Tr, p : (Tr) -> Nat) -> Nat =
            match t
            | leaf(n) => n
            | node(a, b) => p(a) + each(b, p)
            end;
        let at(n : Nat, x : Tr, f : (Tr) -> Nat) -> Nat =
            match n
            | 0 => f(x)
            | k + 1 => at(k, x, f)
            end;
        let size(t : Tr) -> Nat = each(t, (c) => 1 + at(0, c, (d) => size(d)));
        let Sized(t : Tr) -> Prop = Bool/Holds(size(t) == 2);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("at(0, c, ", "at(0, t, ")])
    .by_elaborator_in("/Sized", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Sized", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// Only a lambda written where the callee applies it is bounded by the callee's applications: one bound by a `let`, stored in a tuple, chosen by a match or answered by another call reaches the callee as a value, and is applied besides wherever the program applies it, here to `t` itself. Each refusal hands `each` the lambda the control writes in place through one indirection and applies it to `t` beside the call.
#[test]
fn a_callback_reached_through_an_indirection_takes_no_bounds() {
    let program = control(
        r#"
        use /std/{Nat, Bool};
        induct Tr : Type
        | leaf(n : Nat)
        | node(a : Tr, b : Tr)
        end
        let each(t : Tr, p : (Tr) -> Nat) -> Nat =
            match t
            | leaf(n) => n
            | node(a, b) => p(a) + each(b, p)
            end;
        let twice(f : (Tr) -> Nat, x : Tr) -> (Tr) -> Nat = (y : Tr) => f(y) + f(x);
        let size(t : Tr) -> Nat = each(t, (c) => 1 + size(c));
        let Sized(t : Tr) -> Prop = Bool/Holds(size(t) == 2);
        /std/print("ok")
        "#,
        b"ok",
    );
    for indirection in [
        "let g = (c : Tr) => 1 + size(c); each(t, g) + g(t)",
        "let held = ((c : Tr) => 1 + size(c), t); each(t, held.0) + held.0(held.1)",
        "let g = match t | leaf(n) => (c : Tr) => 1 + size(c) | node(a, b) => (c : Tr) => 1 + size(c) end; each(t, g) + g(t)",
        "each(t, twice((c) => 1 + size(c), t))",
    ] {
        program
            .refused(&[("each(t, (c) => 1 + size(c))", indirection)])
            .by_elaborator_in("/Sized", |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_certifier_in("/Sized", |error| {
                matches!(error, curios_cert::Error::NotTotal { .. })
            });
    }
}

// A callback is bounded by what every application its callee reaches establishes, the ones a path of the callee's group reaches included: `kids` applies `p` below `t` and hands it on to `whole`, which applies it below `r`, and `size` hands both `t`. The refusal has `whole` apply `p` to `r` itself, which only the path through `whole` reaches, and which is `size(t)` again.
#[test]
fn a_callback_is_bounded_by_every_application_its_callee_reaches() {
    control(
        r#"
        use /std/{Nat, Bool};
        induct Tr : Type
        | leaf(n : Nat)
        | node(a : Tr, b : Tr)
        end
        let kids(p : (Tr) -> Nat, r : Tr, t : Tr) -> Nat =
            match t
            | leaf(n) => whole(p, r)
            | node(a, b) => p(a) + kids(p, r, b)
            end
        and whole(p : (Tr) -> Nat, r : Tr) -> Nat =
            match r
            | leaf(n) => n
            | node(a, b) => p(a) + kids(p, a, a)
            end;
        let size(t : Tr) -> Nat = kids((c) => 1 + size(c), t, t);
        let Sized(t : Tr) -> Prop = Bool/Holds(size(t) == 2);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| node(a, b) => p(a) + kids(p, a, a)",
        "| node(a, b) => p(r) + kids(p, a, a)",
    )])
    .by_elaborator_in("/Sized", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Sized", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A summary reads a lambda a definition hands on as a call reads one: `relay` hands `apply_inner` a lambda whose `x` stands below `t` and whose answer binds `y` to `t` itself, so `relay` applies `p` below `t` only where it applies it to `x`. The refusal applies `p` to `y`, so `relay` hands `size`'s callback `t` itself.
#[test]
fn a_lambda_a_summarized_callback_answers_takes_none_of_its_bounds() {
    control(
        r#"
        use /std/{Nat, Bool};
        induct Tr : Type
        | leaf(n : Nat)
        | node(a : Tr, b : Tr)
        end
        let apply_inner(t : Tr, r : Tr, q : (Tr) -> (Tr) -> Nat) -> Nat =
            match t
            | leaf(n) => n
            | node(a, b) => q(a)(r) + apply_inner(b, r, q)
            end;
        let relay(t : Tr, p : (Tr) -> Nat) -> Nat =
            match t
            | leaf(n) => n
            | node(a, b) => apply_inner(t, t, (x) => (y) => p(x)) + relay(b, p)
            end;
        let size(t : Tr) -> Nat = relay(t, (c) => 1 + size(c));
        let Sized(t : Tr) -> Prop = Bool/Holds(size(t) == 2);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(y) => p(x)", "(y) => p(y)")])
    .by_elaborator_in("/Sized", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Sized", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A callback handed through two callees is bounded by each call on the way: `relay` hands `each` a lambda applying `p`, which `each` applies below `t`, so `relay` applies `p` below `t`, whether `size` hands it a callback or `count` hands it `count` itself. The refusals hand `each` a tree holding `t` as a child, so `p` is applied to `t` itself, and have `relay`'s lambda apply `p` to `t` beside its element.
#[test]
fn a_callback_handed_through_two_callees_is_bounded_by_each_call() {
    let program = control(
        r#"
        use /std/{Nat, Bool};
        induct Tr : Type
        | leaf(n : Nat)
        | node(a : Tr, b : Tr)
        end
        let each(t : Tr, p : (Tr) -> Nat) -> Nat =
            match t
            | leaf(n) => n
            | node(a, b) => p(a) + each(b, p)
            end;
        let relay(t : Tr, p : (Tr) -> Nat) -> Nat =
            match t
            | leaf(n) => n
            | node(a, b) => each(t, (x) => p(x) + p(a)) + relay(b, p)
            end;
        let size(t : Tr) -> Nat = relay(t, (c) => 1 + size(c));
        let count(t : Tr) -> Nat = 1 + relay(t, count);
        let Sized(t : Tr) -> Prop = Bool/Holds(size(t) == count(t));
        /std/print("ok")
        "#,
        b"ok",
    );
    for edit in [
        ("each(t, (x)", "each(Tr/node(t, t), (x)"),
        ("p(x) + p(a)", "p(x) + p(t)"),
    ] {
        program
            .refused(&[edit])
            .by_elaborator_in("/Sized", |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_certifier_in("/Sized", |error| {
                matches!(error, curios_cert::Error::NotTotal { .. })
            });
    }
}

// A callback handed inside an inner group's body runs within the enclosing member's invocation, so its binders stand to that member's parameters too, as a call of the enclosing member from the inner body is graded against them: `scan`, local to `has_zero`, hands `List/any` the lambda over `children`, below `t`, so the lambda's `c` is below `t` wherever `any` applies it, and so is what the member `bare`, handed itself, is applied to. The refusals hand the callback the tree rebuilt around the same children, which is `t` again.
#[test]
fn a_callback_handed_inside_an_inner_group_stands_to_the_enclosing_members_parameters() {
    let program = control(
        r#"
        use /std/{Nat, Bool, List};

        induct Rose : Type
        | node(label : Nat, children : List(Rose))
        end

        let has_zero(t : Rose) -> Bool =
            match t
            | node(label, children) =>
                let scan(k : Nat) -> Bool =
                    match k
                    | 0 => List/any(children, (c) => has_zero(c))
                    | j + 1 => scan(j)
                    end;
                label == 0 || scan(1)
            end;

        let Zeroed(t : Rose) -> Prop = Bool/Holds(has_zero(t));

        let bare(t : Rose) -> Bool =
            match t
            | node(label, children) =>
                let scan(k : Nat) -> Bool =
                    match k
                    | 0 => List/any(children, bare)
                    | j + 1 => scan(j)
                    end;
                label == 0 || scan(1)
            end;

        let Bare(t : Rose) -> Prop = Bool/Holds(bare(t));

        /std/print("ok")
        "#,
        b"ok",
    );
    for (edit, at) in [
        (
            (
                "(c) => has_zero(c)",
                "(c) => has_zero(Rose/node(label, children))",
            ),
            "/Zeroed",
        ),
        (
            (
                "List/any(children, bare)",
                "List/any([Rose/node(label, children)], bare)",
            ),
            "/Bare",
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

// A curried spine is read whole, as an uncurried call is: `any_c(pred)(l)` hands `pred` at the place `any_c`'s summary counts it, across its leading lambdas, so `c` is below `children` and `has_zero` descends; and `((x) => (y) => …)(0)(n)` binds `y` to `n` as the uncurried `((x, y) => …)(0, n)` does, so the split of `y` descends on `n`. The refusals hand the callback the tree rebuilt around the same children, and recur on `y` itself, each of which is the call again.
#[test]
fn a_curried_spine_is_read_whole_as_an_uncurried_call_is() {
    let program = control(
        r#"
        use /std/{Nat, List, Bool};

        induct Rose : Type
        | node(label : Nat, children : List(Rose))
        end

        let any_c(pred: (Rose) -> Bool) -> (List(Rose)) -> Bool =
            (l) => match l | [] => false | [h, ..t] => pred(h) || any_c(pred)(t) end;

        let has_zero(t : Rose) -> Bool =
            match t
            | node(label, children) => label == 0 || any_c((c) => has_zero(c))(children)
            end;

        let Zeroed(t : Rose) -> Prop = Bool/Holds(has_zero(t));

        let curried(n : Nat) -> Bool =
            ((x : Nat) => (y : Nat) => match y | 0 => false | k + 1 => curried(k) end)(0)(n);

        let Curried(n : Nat) -> Prop = Bool/Holds(curried(n));

        /std/print("ok")
        "#,
        b"ok",
    );
    for (edit, at) in [
        (
            (
                "(c) => has_zero(c)",
                "(c) => has_zero(Rose/node(label, children))",
            ),
            "/Zeroed",
        ),
        (
            ("| k + 1 => curried(k)", "| k + 1 => curried(y)"),
            "/Curried",
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
