//! Index inversion as one problem: what an arm learns from its index equations, solved in order over one set of flexible variables, and what it may not learn.

use {super::test_support::*, curios_elab::Error};

// A variable forced twice is one equation between its two forcings. `refl()` forces its binder at `some(x)` and at `some(y)`, and the second, read through the first, is `some(y) ~ some(x)`, which injectivity takes apart, refining `y` to `x`. The refusal claims `y + 1`, which that solution does not give.
#[test]
fn a_variable_forced_twice_is_one_equation_between_its_forcings() {
    control(
        r#"
        use /std/{Nat, Eq, Option};

        let injective(@x: Nat, @y: Nat, e: Eq()(Option/some(x), Option/some(y))) -> Eq()(x, y) =
            match e | refl() => Eq/refl() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("-> Eq()(x, y) =", "-> Eq()(x, y + 1) =")])
    .by_elaborator_in("/injective", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// An outer variable forced twice is one equation too, and the binders its two forcings name are as flexible as it is. `n` is refined to `z + 1`, so the second position is `z + 1 ~ w + 1`, which refines `z` to `w`: neither pass of a unifier that solved binders and outer variables apart could, since the equation needs the binders flexible where `n` is solved. The refusal claims `w + 1`.
#[test]
fn an_outer_variable_forced_twice_refines_the_binders_its_forcings_name() {
    control(
        r#"
        use /std/{Nat, Eq};

        induct Two: (Nat, Nat) -> Type
        | mk(z: Nat, w: Nat): (z + 1, w + 1)
        end

        let same(@n: Nat, t: Two(n, n)) -> Nat =
            match t
            | mk(z, w) =>
                let _: Eq()(z, w) = Eq/refl();
                z
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("let _: Eq()(z, w) =", "let _: Eq()(z, w + 1) =")])
    .by_elaborator_in("/same", |error| matches!(error, Error::TypeMismatch { .. }));
}

// A free monoid cancels a generator: `[h, ..l] ~ [x, ..l']` holds exactly where `h = x` and `l = l'`, so the `here` arm learns its head. The `there` arm learns nothing of the kind, its element being another, and the refusal claims it does.
#[test]
fn a_cons_index_is_taken_apart_at_its_known_element() {
    control(
        r#"
        use /std/{Eq, List};
        use /std/List/{Member};

        let head_is(
            @T: Type,
            @x: T,
            @h: T,
            @l: List(T),
            m: Member()(x, [h, ..l]),
            otherwise: Eq()(x, h),
        ) -> Eq()(x, h) =
            match m
            | here() => Eq/refl()
            | there(_) => otherwise
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| there(_) => otherwise", "| there(_) => Eq/refl()")])
    .by_elaborator_in("/head_is", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// Two words solve nothing where neither starts with a known element: `[..c, ..a] ~ [..d, ..b]` does not say `a = b`, the lengths of `c` and `d` being unknown. The control cancels a known head and learns the tails are one.
#[test]
fn a_word_with_no_known_element_at_its_head_is_not_taken_apart() {
    control(
        r#"
        use /std/{Nat, Eq, List};

        let tails(@x: Nat, @y: Nat, @a: List(Nat), @b: List(Nat), e: Eq()([x, ..a], [y, ..b])) -> Eq()(a, b) =
            match e | refl() => Eq/refl() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "@x: Nat, @y: Nat, @a: List(Nat), @b: List(Nat), e: Eq()([x, ..a], [y, ..b])",
        "@c: List(Nat), @d: List(Nat), @a: List(Nat), @b: List(Nat), e: Eq()([..c, ..a], [..d, ..b])",
    )])
    .by_elaborator_in("/tails", |error| matches!(error, Error::TypeMismatch { .. }));
}

// An equation that stands undecided does not take its siblings with it. `f(a) ~ f(z)` holds of an opaque `f` whatever `a` and `z` are, so it decides nothing, while `n ~ z` beside it in the same tuple refines `n`. The refusal claims what only the undecided component could say.
#[test]
fn an_undecided_component_leaves_its_siblings_solved() {
    control(
        r#"
        use /std/{Nat, Eq};

        induct Paired(f: (Nat) -> Nat): ({Nat, Nat}) -> Type
        | mk(z: Nat): ((z, f(z)))
        end

        let second(@f: (Nat) -> Nat, @a: Nat, @n: Nat, p: Paired(f)((n, f(a)))) -> Nat =
            match p
            | mk(z) =>
                let _: Eq()(z, n) = Eq/refl();
                z
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("let _: Eq()(z, n) =", "let _: Eq()(z, a) =")])
    .by_elaborator_in("/second", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// A binder forced to a projection is solved to it: no arm refines an index to a binder before the arm's own equations are solved, so a projection at the top of a position is a value like any other. The refusal claims the other field.
#[test]
fn a_binder_forced_to_a_projection_is_solved_to_it() {
    control(
        r#"
        use /std/{Nat, Eq};

        induct Single: (Nat) -> Type
        | mk(z: Nat): (z)
        end

        let first(p: {Nat, Nat}, s: Single(p.0)) -> Nat =
            match s
            | mk(z) =>
                let _: Eq()(z, p.0) = Eq/refl();
                z
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("let _: Eq()(z, p.0) =", "let _: Eq()(z, p.1) =")])
    .by_elaborator_in("/first", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// A solution may not reach its own variable through what an arm in force refined. The arm `node(t)` refines the key `p.0` to `node(t)`, and the inner case forces `t ~ node(p.0)`: solved, `t` would stand for a value whose key reads back to `t`, a cycle the certifier meets by name. The occurs check closes the value's locals under the refinements in force, so `t` stays unsolved, the arm is checked as written, and the claim that would need the solution is refused.
#[test]
fn a_solution_does_not_reach_its_variable_through_a_refined_projection() {
    control(
        r#"
        use /std/{Eq};

        pub induct Tree: pub Type
        | leaf()
        | node(Tree)
        end

        pub induct Box(m: Tree): (Tree) -> pub Type
        | mk(): (Tree/node(m))
        end

        pub let g(p: {Tree, Tree}, b: (t: Tree) -> Box(p.0)(t)) -> Tree =
            match p.0
            | leaf() => Tree/leaf()
            | node(t) => match b(t) | mk() => let _: Eq()(t, t) = Eq/refl(); t end
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("let _: Eq()(t, t) =", "let _: Eq()(t, Tree/node(p.0)) =")])
    .by_elaborator_in("/g", |error| matches!(error, Error::TypeMismatch { .. }));
}

// The same cycle through a stuck application an arm refined, `f(p)`: its key names `f` and `p`, which the value `node(f(p))` names, so the refinement's value joins what the value reaches.
#[test]
fn a_solution_does_not_reach_its_variable_through_a_refined_application() {
    control(
        r#"
        use /std/{Eq};

        pub induct Tree: pub Type
        | leaf()
        | node(Tree)
        end

        pub induct Box(m: Tree): (Tree) -> pub Type
        | mk(): (Tree/node(m))
        end

        pub let g(f: (Tree) -> Tree, p: Tree, b: (t: Tree) -> Box(f(p))(t)) -> Tree =
            match f(p)
            | leaf() => Tree/leaf()
            | node(t) => match b(t) | mk() => let _: Eq()(t, t) = Eq/refl(); t end
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("let _: Eq()(t, t) =", "let _: Eq()(t, Tree/node(f(p))) =")])
    .by_elaborator_in("/g", |error| matches!(error, Error::TypeMismatch { .. }));
}

// The same cycle through a variable an arm refined: the outer arm solves `z := [1, ..m]`, and the inner one meets `[2, ..z]`, which the certifier reads as `[2, 1, ..m]`. Posed as the certifier reads it, the inner equations refine `m` to the binder `w` and then leave `w ~ [2, 1, ..w]` standing, since `w` reaches itself through `m`.
#[test]
fn a_solution_does_not_reach_its_variable_through_a_refined_variable() {
    control(
        r#"
        use /std/{Eq, List, Nat};

        induct Box(m: List(Nat)): (List(Nat)) -> Type
        | mk(z: List(Nat), e: Eq()(m, [2, ..z])): (z)
        end

        let g(m: List(Nat), b: Box(m)([1, ..m])) -> Nat =
            match b
            | mk(_, e) => match e | refl() => 0 end
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| refl() => 0",
        "| refl() => let _: Eq()(m, [2, 1, ..m]) = Eq/refl(); 0",
    )])
    .by_elaborator_in("/g", |error| matches!(error, Error::TypeMismatch { .. }));
}
