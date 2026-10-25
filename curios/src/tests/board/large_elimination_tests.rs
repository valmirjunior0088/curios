//! A proposition taken apart into data: the sort the guard reads off a motive, what the singleton rung counts as pinned, the instance a match on a proof fires on, and the sizing that keeps a cast from unfolding without end.

use {super::test_support::*, crate::tests::run};

// The guard reads the result's sort off the motive typed under its own binders, and a motive is held to land in a sort there. `F(i)` is the proposition `True` at `true` and `Nat` at `false`, so read at each arm's case the motive would pass the guard at `qa` and type `qb`'s arm as data out of a proposition of two constructors. Typed at the binder `i` it lands in no sort, and the match is refused at its motive before either arm is read. `curios-cert`'s `kernel::infer::eliminate::proof_match_tests` puts the same motive to the kernel under this name.
#[test]
fn a_motive_whose_sort_its_index_decides_is_no_motive() {
    rejected_by(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True};

        induct Q: (Bool) -> Prop
        | qa(): (true)
        | qb(): (false)
        end

        let S(b: Bool) -> Type = match b | true => Prop | false => Type end;

        let F(b: Bool) -> S(b) = match b : (c) => S(c) | true => True | false => Nat end;

        let read(t: Q(false)) -> Nat = match t : (i, _) => F(i) | qa() => True/qed() | qb() => 7 end;

        /std/print("unreachable")
        "#,
        "inferred: S(i)",
    );
}

// A clash is concluded between two forcings of one binder only where they do not convert. `refl(@A)` forces `A` to `Nat` and to `Id(Nat)`, which are one type through `Id`'s definition, so the deletion rule keeps one and the arm stays mandatory: excused, `absurd(TEq/refl(@Nat))` is a closed inhabitant of `False`.
#[test]
fn two_spellings_of_one_type_forced_on_one_binder_do_not_clash() {
    rejected_by(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        induct TEq: (Type, Type) -> Prop
        | refl(@A: Type): (A, A)
        end

        let Id(A: Type) -> Type = A;

        let absurd(p: TEq(Nat, Id(Nat))) -> False = match p end;

        let _forged: False = absurd(TEq/refl(@Nat));

        /std/print("unreachable")
        "#,
        "is not provably impossible at the scrutinee's indices",
    );
}

// The shared walk pins a binder only where a target is the binder itself, and does not descend into a constructor: at a proposition `Tag/t(0)` and `Tag/t(7)` are one value, so `P(Tag/t(0))` and `P(Tag/t(7))` are one type and `mk(@a)` holds a payload no index recovers. Read through the constructor, `read` would answer `0` and `7` at two calls that convert, `apart` would equate them, and the transport would prove `Holds(7 == 0)`. `curios-analysis`'s `invert::tests::a_binder_under_a_constructor_is_not_pinned` holds the walk at the same target.
#[test]
fn a_payload_under_a_propositions_constructor_is_not_pinned() {
    rejected_by(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{True, False, Holds};

        induct Tag: Prop
        | t(n: Nat)
        end

        induct P: (Tag) -> Prop
        | mk(@a: Nat): (Tag/t(a))
        end

        let read(@x: Tag, p: P(x)) -> Nat = match p | mk(@a) => a end;

        let apart: Eq()(read(@Tag/t(0), P/mk(@0)), read(@Tag/t(7), P/mk(@7))) = Eq/refl();

        let _forged: Holds(read(@Tag/t(7), P/mk(@7)) == 0) = Eq/subst((n: Nat) => Holds(n == 0), apart, True/qed());

        /std/print("unreachable")
        "#,
        "cannot eliminate the proposition 'P'",
    );
}

// A guard's equation is settled with every equation inside it withheld, so a spelling settled under one inner case answers in no other. The guard is the match on `p`, which stands while `n` is open. Under the `0` arm of the split on `g(n)` the case equation makes `g(n)` and `g(g(n))` one, and the match on `p` fires to `k(0)`: a settlement that read that equation would spell the guard `k(0)` and answer it `true` under `j + 1` too, where the guard says `k(1)`, and at `k(m) = (m == 1)` that is `Holds(0 == 1)`. Withheld, the guard keeps its own spelling, `k(0)` is refused in the `0` arm, and nothing is settled for the next. The soundness board's elimination part holds the same guard settled under a split on a variable.
#[test]
fn a_guard_over_a_proof_match_is_settled_with_the_equations_inside_it_withheld() {
    rejected_by(
        r#"
        use /std/{Nat, Eq, Bool};
        use /std/Bool/{True, False, Holds};

        let g(n: Nat) -> Nat = match n | 0 => 0 | j + 1 => 1 end;

        let f(
            k: (Nat) -> Bool,
            n: Nat,
            p: Eq()(g(n), g(g(n))),
            guard: Holds(match p | refl(@z) => (match z | 0 => k(0) | j + 1 => k(1) end) end)) -> Holds(k(0)) =
            match (match p | refl(@z) => (match z | 0 => k(0) | j + 1 => k(1) end) end)
            | false => match guard end
            | true =>
                match g(n)
                | 0 => let settled: Holds(k(0)) = True/qed(); settled
                | j + 1 => True/qed()
                end
            end;

        let _forged: Holds(0 == 1) = f((m: Nat) => m == 1, 1, Eq/refl(), True/qed());

        /std/print("unreachable")
        "#,
        "expected: Holds(k(0))",
    );
}

// A proposition sits at the level of what it quantifies over, so none ranges over the propositions of its own level. `A` quantifies over every proposition and would be one of them, the instantiation Abel and Coquand's cast that unfolds without end needs and the self-application `delta` writes; sized, `A` sits a level above the `P` it takes, and `x(A)` is refused.
#[test]
fn a_proposition_over_every_proposition_is_not_applied_at_itself() {
    rejected_by(
        r#"
        use /std/{Nat};

        let A: Prop = (P: Prop) -> (P) -> P;

        let delta(x: A) -> A = x(A)(x);

        /std/print("unreachable")
        "#,
        "this Prop would need to be strictly below itself",
    );
}

// A match that takes a proposition into data is one term however its proof is spelled. Along a hypothesis the algebra equates with its reverse, a cast through `Eq/subst`, a direct match, a local lemma, and along the hypothesis passed through `Eq/sym` twice and through `Eq/trans` with `Eq/refl()`, is its argument in each, in both checkers. Along a hypothesis whose ends nothing holds one, each spelling stands with its proof unreduced: one that fired on the shape of the match would accept its statement by reflexivity.
#[test]
fn a_cast_fires_or_stands_alike_in_every_spelling() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Eq};

        let cast(P: (Nat) -> Type, @x: Nat, @y: Nat, q: Eq()(x, y), w: P(x)) -> P(y) =
            match q | refl() => w end;

        let by_subst(P: (Nat) -> Type, a: Nat, b: Nat, p: Eq()(a + b, b + a), v: P(a + b))
            -> Eq()(Eq/subst(P, p, v), v) = Eq/refl();
        let by_match(P: (Nat) -> Type, a: Nat, b: Nat, p: Eq()(a + b, b + a), v: P(a + b))
            -> Eq()(match p : (x, y, _) => P(y) | refl(@z) => v end, v) = Eq/refl();
        let by_lemma(P: (Nat) -> Type, a: Nat, b: Nat, p: Eq()(a + b, b + a), v: P(a + b))
            -> Eq()(cast(P, p, v), v) = Eq/refl();
        let by_sym(P: (Nat) -> Type, a: Nat, b: Nat, p: Eq()(a + b, b + a), v: P(a + b))
            -> Eq()(Eq/subst(P, Eq/sym(Eq/sym(p)), v), v) = Eq/refl();
        let by_trans(P: (Nat) -> Type, a: Nat, b: Nat, p: Eq()(a + b, b + a), v: P(a + b))
            -> Eq()(cast(P, Eq/trans(p, Eq/refl()), v), v) = Eq/refl();
        let by_match_sym(P: (Nat) -> Type, a: Nat, b: Nat, p: Eq()(a + b, b + a), v: P(a + b))
            -> Eq()(match Eq/sym(Eq/sym(p)) : (x, y, _) => P(y) | refl(@z) => v end, v) = Eq/refl();

        /std/print("ok")
        "#),
        b"ok"
    );

    let stands = r#"
        use /std/{Nat, Eq};

        let cast(P: (Nat) -> Type, @x: Nat, @y: Nat, q: Eq()(x, y), w: P(x)) -> P(y) =
            match q | refl() => w end;

        let u_subst(a: Nat, b: Nat, p: Eq()(a, b), v: Nat)
            -> Eq()(Eq/subst((_) => Nat, p, v), v) = Eq/refl();
        let u_match(a: Nat, b: Nat, p: Eq()(a, b), v: Nat)
            -> Eq()(match p : (x, y, _) => Nat | refl(@z) => v end, v) = Eq/refl();
        let u_lemma(a: Nat, b: Nat, p: Eq()(a, b), v: Nat)
            -> Eq()(cast((_) => Nat, p, v), v) = Eq/refl();
        let u_sym(a: Nat, b: Nat, p: Eq()(a, b), v: Nat)
            -> Eq()(Eq/subst((_) => Nat, Eq/sym(Eq/sym(p)), v), v) = Eq/refl();
        let u_trans(a: Nat, b: Nat, p: Eq()(a, b), v: Nat)
            -> Eq()(cast((_) => Nat, Eq/trans(p, Eq/refl()), v), v) = Eq/refl();

        /std/print("unreachable")
        "#;
    for refused in [
        "expected: Eq(@Nat)(Eq/subst(@Nat, @a, @b, (_) => Nat, p, v), v)",
        "expected: Eq(@Nat)(match p | refl(@z) => v end, v)",
        "expected: Eq(@Nat)(cast((_) => Nat, @a, @b, p, v), v)",
        "expected: Eq(@Nat)(Eq/subst(@Nat, @a, @b, (_) => Nat, Eq/sym(@Nat, @b, @a, Eq/sym(@Nat, @a, @b, p)), v), v)",
        "expected: Eq(@Nat)(cast((_) => Nat, @a, @b, Eq/trans(@Nat, @a, @b, @b, p, Eq/refl(@Nat, @b)), v), v)",
    ] {
        rejected_by(stands, refused);
    }
}
