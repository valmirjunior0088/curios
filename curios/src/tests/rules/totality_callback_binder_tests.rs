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
