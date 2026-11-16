//! The size-change obligation through a callback: a call in a lambda handed to a definition is graded where the definition applies the parameter the lambda stands for, as the summary `curios_analysis::group_applications` files states, and a parameter the definition uses any other way publishes nothing.

use super::test_support::*;

// A summary holds every application of its parameter however the body reaches it, or holds nothing: `h` applies `f` below `n` and nowhere else, so `spin`'s recursion through the lambda it hands `h` descends, and each refusal has `h`'s `0` arm apply `f` once more, at `n + 1`, by one route the walk summarizing `h` must follow or count as a use it does not see — directly, through a `let` alias, a closure a `let` binds, a local function, a local recursive definition, a guard, a tuple's projection, a constructor's payload, and a witness's method. Read as unseen, any one of them leaves the summary holding `f(j)` alone, below `n`, and `spin(1)`, which is `h(1, …)`, which is `h(0, …)` applying the lambda at `1`, is `spin(1)` again.
#[test]
fn a_callback_applied_by_any_route_at_a_value_not_below_is_not_graded_below() {
    let control = control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        induct Box : Type
        | hold(g : (Nat) -> True)
        end

        concept Runs(A : Type) : Type {
            run(A, (Nat) -> True, Nat) -> True,
        }

        satisfy Runs(Nat) {
            run(_, g, m) = g(m),
        }

        let h(n : Nat, f : (Nat) -> True) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => match k | 0 => h(k, f) | j + 1 => f(j) end
            end;

        let spin(n : Nat) -> True = h(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    );
    for route in [
        "| 0 => f(n + 1)",
        "| 0 => let g = f; g(n + 1)",
        "| 0 => let g = (x : Nat) => f(x); g(n + 1)",
        "| 0 => let g(x : Nat) -> True = f(x); g(n + 1)",
        "| 0 => let go(x : Nat, d : Nat) -> True = match d | 0 => f(x) | e + 1 => go(x, e) end; go(n + 1, 1)",
        "| 0, (match f(n + 1) | qed() => true end) => True/qed() | 0 => True/qed()",
        "| 0 => (f, n + 1).0((f, n + 1).1)",
        "| 0 => match Box/hold(f) | hold(g) => g(n + 1) end",
        "| 0 => Runs/run(n, f, n + 1)",
    ] {
        control
            .refused(&[("| 0 => True/qed()", route)])
            .by_elaborator_in("/spin", |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_certifier_in("/spin", |error| {
                matches!(error, curios_cert::Error::NotDescending { .. })
            });
    }
}

// The bind a `!` lowers to is a route the walk follows: `h` sequences `f(j)` with a `!`, which `/std/Monad/bind` receives as its action, so `h` applies `f` below `n`, and `Spun`, a type reaching `spin`'s recursion through the lambda it hands `h`, is formed. The refusal sequences `f(n + 1)` in `h`'s `0` arm: read as unseen, it would leave `spin(1)`, which sequences `spin(1)` again, total.
#[test]
fn a_callback_sequenced_with_a_bang_is_graded_where_the_bind_applies_it() {
    control(
        r#"
        use /std/{Nat, Option, Bool};
        use /std/Bool/{Holds};

        let h(n: Nat, f: (Nat) -> Option(Bool)) -> Option(Bool) =
            match n
            | 0 => Option/some(true)
            | k + 1 => match k | 0 => h(k, f) | j + 1 => let b = f(j)!; Option/some(b) end
            end;

        let spin(n: Nat) -> Option(Bool) = h(n, (c) => spin(c));

        let Spun(n: Nat) -> Prop = Holds(match spin(n) | some(_) => true | none() => false end);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| 0 => Option/some(true)",
        "| 0 => let b = f(n + 1)!; Option/some(b)",
    )])
    .by_elaborator_in("/Spun", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Spun", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A callback eta-expanded into another definition's callback is graded through both summaries: `at` applies `g` at `m` or below it, and `h` hands `at` the lambda `(x) => f(x)` with `j`, below `n`, so `f` is applied below `n` and `spin` descends. The refusal hands `at` `n` itself, where `f` is applied at `n`, and `spin(2)` is `spin(2)` again.
#[test]
fn a_callback_eta_expanded_into_another_definitions_callback_is_graded_through_both_summaries() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let at(m: Nat, g: (Nat) -> True) -> True =
            match m | 0 => g(m) | i + 1 => match i | 0 => at(i, g) | _ + 1 => g(m) end end;

        let h(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => match k | 0 => h(k, f) | j + 1 => at(j, (x) => f(x)) end
            end;

        let spin(n: Nat) -> True = h(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("at(j, (x) => f(x))", "at(n, (x) => f(x))")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// An implicit callback is a parameter as an explicit one is: `h` takes `f` under `@`, applies it below `n` and hands it on at its own place, so `spin` descends. The refusal hands `f` to `at` in `h`'s `0` arm, which applies it at `n + 1`: handed to another definition, `f` publishes nothing, where read as unseen `spin(1)` would be a total `spin(1)` again.
#[test]
fn an_implicit_callback_handed_to_another_definition_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let at(@g: (Nat) -> True, m: Nat) -> True = g(m);

        let h(n: Nat, @f: (Nat) -> True) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => match k | 0 => h(k, @f) | j + 1 => f(j) end
            end;

        let spin(n: Nat) -> True = h(n, @(c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| 0 => True/qed()", "| 0 => at(@f, n + 1)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A callback handed to a member at another place than its own publishes nothing at either: `h` applies `f` below `n` and `g` at `n + 1`, and hands both on in place, so `spin`'s recursion through the lambda it hands `h` as `f` descends. The refusal hands them on exchanged, so the lambda `spin` hands as `f` is applied as `g` one call down, at what is `n` again: read as handed on at their own places, `f` would still be summarized below `n`, and `spin(1)` would be a total `spin(1)` again.
#[test]
fn a_callback_handed_to_a_member_at_another_place_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let h(n: Nat, f: (Nat) -> True, g: (Nat) -> True) -> True =
            match n
            | 0 => g(n + 1)
            | k + 1 => let _ = f(k); h(k, f, g)
            end;

        let spin(n: Nat) -> True = h(n, (c) => spin(c), (c) => True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("h(k, f, g)", "h(k, g, f)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A callback a member answers publishes nothing: `give` hands its `g` on to `h` at the same place, inside the function it answers, so `h`'s `f` is applied below `n` and `spin` descends. The refusal has `give` answer `g` itself at `0`, which `h` applies at `n + 1`, and `spin(1)` is `spin(1)` again.
#[test]
fn a_callback_a_member_answers_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let h(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => give(n, f)(n + 1)
            | k + 1 => match k | 0 => h(k, f) | j + 1 => f(j) end
            end
        and give(m: Nat, g: (Nat) -> True) -> (Nat) -> True =
            match m
            | 0 => (_) => True/qed()
            | i + 1 => (_) => h(i, g)
            end;

        let spin(n: Nat) -> True = h(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| 0 => (_) => True/qed()", "| 0 => g")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A lambda handed to a member in its callback's own place is another function, whose applications of the callback are counted where the lambda is written: `h` applies `f` below `n` and hands it on, so `spin` descends. The refusal hands `h` the closure `(x) => f(x + 2)` in `f`'s place, which one call down applies `f` at what is `n` again: counted as `f` handed on, it would leave `f` summarized by `f(k)` alone, and `spin(2)` would be a total `spin(2)` again.
#[test]
fn a_closure_handed_in_its_callbacks_own_place_is_counted_where_it_is_written() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let h(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => let _ = f(k); h(k, f)
            end;

        let spin(n: Nat) -> True = h(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("h(k, f)", "h(k, (x) => f(x + 2))")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A member's parameters are the ones its leading lambdas bind, every list they bind and no more: `wrap`'s body opens both of its type's lists, so `wrap(n)(f)` hands `f` at its own place to `wrap`'s `g`, and what `wrap` does with `g` is in `hide`'s summary. The control's `wrap` hands `g` back to `hide` alone, so `hide` applies `f` below `n` and `spin` descends; counted at its first lambda alone, `wrap` would take no parameter at `f`'s place and the control would be refused. The refusal's `wrap` applies `g` to `0` where `m` is `0`, which the summary holds through the call at `n` and nothing below it, so `spin(c)` is graded at `n` and refused, as `spin(0)` is `spin(0)` again.
#[test]
fn a_callback_handed_to_a_member_at_a_place_its_leading_lambdas_bind_is_graded_where_it_applies_it()
{
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let hide(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => wrap(n)(f)
            | k + 1 => f(k)
            end
        and wrap(m: Nat) -> ((Nat) -> True) -> True =
            (g) =>
                match m
                | 0 => True/qed()
                | k + 1 => hide(k, g)
                end;

        let spin(n: Nat) -> True = hide(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| 0 => True/qed()", "| 0 => g(0)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A member's hidden parameters are its parameters as its plain ones are, in the places Core gives them: `wrap` binds `@B`, `m` and an erased bound by its leading lambda and answers a function, so `wrap(@A, n)(f)` hands `f` at the fourth place, past the three it takes, to the function `wrap` answers, which applies it to `0`. Counted by its type's lists, `wrap` would take a fourth parameter at `f`'s place, which no body applies, and `hide`'s summary would hold `f(k)` alone, below `n`, admitting `spin`, which at `0` is `spin(0)` again. The control hands `wrap` a callback of its own, so `hide` applies `f` below `n` alone. Mutation-checked: with `*position < arity` removed from `handed_on` in `curios-analysis/src/totality.rs`, the refusal is admitted.
#[test]
fn a_callback_handed_past_a_members_hidden_and_plain_parameters_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};

        let hide(@A: Type, n: Nat, @_: Holds(0 == 0), f: (Nat) -> True) -> True =
            match n
            | 0 => wrap(@A, n)((_) => True/qed())
            | k + 1 => f(k)
            end
        and wrap(@B: Type, m: Nat, @_: Holds(0 == 0)) -> ((Nat) -> True) -> True =
            match m
            | 0 => (g) => g(0)
            | k + 1 => (g) => hide(@B, k, g)
            end;

        let spin(n: Nat) -> True = hide(@Nat, n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("wrap(@A, n)((_) => True/qed())", "wrap(@A, n)(f)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A call split across a `let` is the call it spells: the tail is read with `w` standing for `wrap(n)`, so `w(f)` is `wrap(n)(f)`, which hands `f` past `wrap`'s one parameter to the function `wrap` answers, and `hide` publishes nothing for `f`. Read as an application of the binder `w`, or as `wrap`'s call with `f` at a place it takes, `hide`'s summary would hold `f(k)` alone and admit `spin`, which at `0` is `spin(0)` again. The control hands `w` a callback of its own. Mutation-checked: with `*position < arity` removed from `handed_on` in `curios-analysis/src/totality.rs`, the refusal is admitted.
#[test]
fn a_callback_handed_past_a_members_parameters_through_a_let_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let hide(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 =>
                let w = wrap(n);
                w((_) => True/qed())
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
    .refused(&[("w((_) => True/qed())", "w(f)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A member applied in part relates the places its call leaves unfilled to nothing, whoever fills them later: `wrap` binds three lists and applies `g` to `z`, its third, where `m` is `0`. The control fills all three at the call, `wrap(k)(f)(k)`, so `hide`'s summary holds `g(z)` at `k`, below `n`, and `spin` descends. The refusal leaves the third to `at`, outside the group, which fills it with `n` itself: `at(wrap(k)(f), n)` is `f(n)` at `k` `0`, and `spin(1)` is `spin(1)` again. The call `hide` makes hands `wrap` two places, so `g(z)` reaches the summary at nothing and `spin` is refused; read as filled where the call stands, the third place would be graded at `k`, below `n`.
#[test]
fn a_member_applied_in_part_relates_its_unfilled_places_to_nothing() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let at(h: (Nat) -> True, x: Nat) -> True = h(x);

        let hide(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => True/qed()
            | k + 1 => wrap(k)(f)(k)
            end
        and wrap(m: Nat) -> ((Nat) -> True) -> (Nat) -> True =
            (g) => (z) =>
                match m
                | 0 => g(z)
                | j + 1 => hide(j, g)
                end;

        let spin(n: Nat) -> True = hide(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("wrap(k)(f)(k)", "at(wrap(k)(f), n)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A method's call is a projection of its witness applied, not a call of a definition whose record says how it applies a parameter: `wrap` hands `g` to `Relay/relay`, whose witness applies it to the `m` it is handed, so `g` is used in a way the walk does not follow, `hide`'s `f`, handed to `wrap` at that place, is summarized as nothing, and `spin`, which at `0` is `relay(0, …)`, `spin(0)` again, is refused. The control hands the method a callback of its own, so `wrap` hands `g` back to `hide` alone and `spin` descends.
#[test]
fn a_callback_handed_to_a_witness_method_is_graded_as_unrelated() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        concept Relay(A: Type): Type {
            relay(A, (Nat) -> True) -> True,
        }

        satisfy Relay(Nat) {
            relay(m, g) = g(m),
        }

        let hide(n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => wrap(n, f)
            | k + 1 => f(k)
            end
        and wrap(m: Nat, g: (Nat) -> True) -> True =
            match m
            | 0 => Relay/relay(m, (_) => True/qed())
            | k + 1 => hide(k, g)
            end;

        let spin(n: Nat) -> True = hide(n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Relay/relay(m, (_) => True/qed())", "Relay/relay(m, g)")])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/spin", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A parameter's own place is the one Core counts, hidden binders included. In the control both members open with a hidden `@_`, so `wrap(@Nat, n, f, 0)` hands `f` at the third place, its own, to `wrap`'s `g`, which `wrap` hands back to `hide` alone, and `spin` descends. The first refusal has `wrap` apply `g` to `z` where `m` is `0`, which the summary holds through the call at `n`, so `spin` is refused. The second also takes `wrap`'s hidden binder away: `f` is still `hide`'s first plain parameter and the call's, but the call hands it at the second place Core counts, `wrap`'s `g`, where `hide`'s second is `n`, so it is no hand-on at its own place and `hide` publishes nothing for it. Read by plain places it would be handed on, and read in `wrap` at its own Core place, the third, it would be `z`, which no body applies: `hide`'s summary would hold `f(k)` alone and admit `spin`, which at `0` is `wrap(0, …, 0)`, `spin(0)` again.
#[test]
fn a_callback_is_handed_on_at_the_place_core_counts_hidden_binders_included() {
    let aligned = control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let hide(@_: Type, n: Nat, f: (Nat) -> True) -> True =
            match n
            | 0 => wrap(@Nat, n, f, 0)
            | k + 1 => f(k)
            end
        and wrap(@_: Type, m: Nat, g: (Nat) -> True, z: Nat) -> True =
            match m
            | 0 => True/qed()
            | k + 1 => hide(@Nat, k, g)
            end;

        let spin(n: Nat) -> True = hide(@Nat, n, (c) => spin(c));

        /std/print("ok")
        "#,
        b"ok",
    );
    aligned
        .refused(&[("| 0 => True/qed()", "| 0 => g(z)")])
        .by_elaborator_in("/spin", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_certifier_in("/spin", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
    aligned
        .refused(&[
            ("and wrap(@_: Type, m: Nat,", "and wrap(m: Nat,"),
            ("wrap(@Nat, n, f, 0)", "wrap(n, f, 0)"),
            ("| 0 => True/qed()", "| 0 => g(z)"),
        ])
        .by_elaborator_in("/spin", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_certifier_in("/spin", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
}

// A definition that is no recursive group publishes how it applies its parameters as a group's member does: `apply_some` applies `f` to the payload of `m`, so `depth`'s recursion through the lambda it hands `apply_some` descends. The refusal has `apply_some` apply `f` to a tree it built around `m`, which nothing is below.
#[test]
fn a_callback_handed_to_a_definition_of_no_group_is_graded_where_it_applies_it() {
    control(
        r#"
        use /std/{Nat, Bool, Option};

        induct Tree: Type
        | leaf(Nat)
        | node(Option(Tree))
        end

        let apply_some(m: Option(Tree), f: (Tree) -> Nat) -> Nat =
            match m | some(a) => f(a) | none() => 0 end;

        let depth(t: Tree) -> Nat =
            match t | leaf(n) => n | node(o) => apply_some(o, (c) => depth(c)) end;

        let Shallow(t: Tree) -> Prop = Bool/Holds(depth(t) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| some(a) => f(a)", "| some(a) => f(Tree/node(m))")])
    .by_elaborator_in("/Shallow", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Shallow", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// `/sys/List/map` applies its function to each element of its list, so a member handed to it bare is called on something below the list: `total` mapped over a tree's children descends. The refusal maps `total` over a list holding the tree itself.
#[test]
fn a_member_handed_to_a_list_map_is_called_below_the_list() {
    control(
        r#"
        use /std/{Nat, Bool, List};

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        let total(t: Rose) -> Nat =
            match t | rose(n, cs) => n + List/len(List/map(cs, total)) end;

        let Small(t: Rose) -> Prop = Bool/Holds(total(t) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("List/map(cs, total)", "List/map([t], total)")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// `/sys/List/fold` applies its function to each element of its list and the accumulator, so a lambda handed to it binds an element below the list and an accumulator below nothing: `total`'s call on the element descends. The refusal calls `total` on the tree being folded over.
#[test]
fn a_lambda_handed_to_a_list_fold_binds_its_element_below_the_list() {
    control(
        r#"
        use /std/{Nat, Bool, List};

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        let total(t: Rose) -> Nat =
            match t | rose(n, cs) => List/fold(cs, n, (c, acc) => acc + total(c)) end;

        let Small(t: Rose) -> Prop = Bool/Holds(total(t) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("acc + total(c)", "acc + total(t)")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}
