//! A type a recursive member computes, met again on its own path and read by the least fixpoint of its unfolding: a proposition over a tree, a mutual pair over a list of trees, a group closed over the declaration's parameter, a call wrapped left of an arrow, and a predicate over a list of the family itself.

use {
    super::test_support::*, crate::test_support::*, curios_common::fill, curios_core::Polarity,
    curios_elab::Error,
};

// `Balanced` meets itself on its own path; read by its summary, `V` stands in its body only as the argument its recursive calls hand back, so it is no occurrence, and `U`, holding a `B` of itself, is admitted. The control's leaf arm puts `V` left of an arrow, which every unfolding holds.
#[test]
fn a_recursive_proposition_over_a_tree_is_read_by_its_unfolding() {
    control(
        r#"
        use /std/{Bool};
        use /std/Bool/{False};

        induct Tree(V: Type): pub Type
        | tip(v: V)
        | fork(l: Tree(V), r: Tree(V))
        end

        let Balanced(@V: Type, t: Tree(V)) -> Prop =>
            match t | tip(_) => Bool/True | fork(l, r) => {Balanced(l), Balanced(r)} end;

        struct B(V: Type): pub Type { t: Tree(V), @ok: Balanced(t) }

        induct U: pub Type
        | leaf()
        | hold(b: B(U))
        end

        /std/print("balanced")
        "#,
        b"balanced",
    )
    .refused(&[("tip(_) => Bool/True", "tip(_) => (V) -> False")])
    .by_elaborator_in(
        "/U",
        |error| matches!(error, Error::NotStrictlyPositive { name, .. } if *name == global("/U")),
    );
}

// A mutual pair, `Ok` over a tree and `All` over a list of trees, each meeting the other on its path: the list's element type names `Rose(V)`, an annotation read for its declarations alone, so `Held` is strict in `V` and `T`, holding a `Held` of itself, is admitted.
#[test]
fn a_mutual_pair_over_a_list_of_trees_is_read_by_its_unfolding() {
    let source = r#"
        use /std/{List, Bool};

        induct Rose(V: Type): pub Type
        | leaf(v: V)
        | node(kids: List(Rose(V)))
        end

        let Ok(@V: Type, t: Rose(V)) -> Prop =>
            match t | leaf(_) => Bool/True | node(kids) => All(kids) end
        and All(@V: Type, l: List(Rose(V))) -> Prop =>
            match l | [] => Bool/True | [x, ..rest] => {Ok(x), All(rest)} end;

        struct Held(V: Type): pub Type { t: Rose(V), @ok: Ok(t) }

        induct T: pub Type
        | leaf()
        | hold(h: Held(T))
        end

        /std/print("held")
        "#;
    assert_eq!(run(source), b"held");
    assert_eq!(family_polarities(source)["/Held"], vec![Polarity::Strict]);
}

// A group closed over the declaration's parameter, met first as `Option`'s argument, so it is unfolded once before its cycle closes: `X` stands strictly in the first arm and left of an arrow one level down, where only the summary of what the group's body names reaches it. `S(G)` at `n = 1` holds `Option((G) -> Void)`, so `G` is refused. The control's second arm holds the call strictly.
#[test]
fn a_group_closed_over_the_declarations_parameter_is_read_by_its_unfolding() {
    control(
        r#"
        use /std/{Nat, Option};

        induct Void: pub Type
        end

        struct S(X: Type): pub Type {
            n: Nat,
            f: (() => let R(k: Nat) -> Type => match k | 0 => X | _ => {Nat, R(k - 1)} end; Option(R(n)))(),
        }

        induct G: pub Type
        | g(s: S(G))
        end

        /std/print("inline")
        "#,
        b"inline",
    )
    .refused(&[("{Nat, R(k - 1)}", "(R(k - 1)) -> Void")])
    .by_elaborator_in(
        "/G",
        |error| matches!(error, Error::NotStrictlyPositive { name, .. } if *name == global("/G")),
    );
}

// A definition that wraps its own call left of an arrow: its parameter stands strictly in one arm and, through the call, negatively one level down, so the summary reads it both ways and `Bad`, holding an `R` of itself, is refused. The control's second arm holds the call strictly.
#[test]
fn a_definition_wrapping_its_own_call_left_of_an_arrow_reads_its_parameter_both_ways() {
    control(
        r#"
        use /std/{Nat};

        let R(X: Type, n: Nat) -> Type =>
            match n | 0 => X | k + 1 => {Nat, R(X, k)} end;

        induct Bad: pub Type
        | c(n: Nat, f: R(Bad, n))
        end

        /std/print("wrapped")
        "#,
        b"wrapped",
    )
    .refused(&[("{Nat, R(X, k)}", "(R(X, k)) -> Nat")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Mixed,
                ..
            }
        )
    });
}

// A generic predicate over a list, instantiated at the family itself: unfolded, its list's element type names the family, an annotation read at `Mixed` for the family it names, so `T` is refused. Held through a container instead, the element type is the container's parameter, which no annotation reads, and the family is admitted.
#[test]
fn a_predicate_over_a_list_of_the_family_itself_is_held_through_a_container() {
    let predicate = r#"
        use /std/{List, Bool};

        let AllSmall(@X: Type, l: List(X)) -> Prop =>
            match l | [] => Bool/True | [_, ..rest] => AllSmall(rest) end;
        "#;

    let held = fill!(
        "${predicate}\nstruct Bag(X: Type): pub Type { kids: List(X), @ok: AllSmall(kids) }\n\ninduct T: pub Type\n| leaf()\n| node(b: Bag(T))\nend\n\n/std/print(\"bag\")\n",
        predicate
    );
    control(&held, b"bag")
        .refused(&[("| node(b: Bag(T))", "| node(kids: List(T), @ok: AllSmall(kids))")])
        .by_elaborator_in(
            "/T",
            |error| matches!(error, Error::NotStrictlyPositive { name, .. } if *name == global("/T")),
        );
}
