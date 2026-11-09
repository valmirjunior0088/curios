//! A proposition taken apart into data: the sort the guard reads off a motive however the motive is spelled, a universe among the data, what the singleton rung counts as pinned, the instance a match on a proof fires on, and the sizing that keeps a cast from unfolding without end and a tree from holding itself.

use {super::test_support::*, crate::tests::test_support::*, curios_elab::Error};

// The guard reads the result's sort off the motive typed under its own binders, and a motive is held to land in a sort there. `F(i)` is the proposition `True` at `true` and `Nat` at `false`, so read at each arm's case the motive would pass the guard at `qa` and type `qb`'s arm as data out of a proposition of two constructors. Typed at the binder `i` it lands in no sort, and the match is refused at its motive before either arm is read. `curios-cert`'s `kernel::infer::eliminate::proof_match_tests` puts the same motive to the kernel under this name. The control's motive is a proposition at every index.
#[test]
fn a_motive_whose_sort_its_index_decides_is_no_motive() {
    control(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True};

        induct Q: (Bool) -> Prop
        | qa(): (true)
        | qb(): (false)
        end

        let S(b: Bool) -> Type = match b | true => Prop | false => Type end;

        let F(b: Bool) -> S(b) = match b : (c) => S(c) | true => True | false => Nat end;

        let read(t: Q(false)) -> True = match t : (i, _) => True | qa() => True/qed() | qb() => True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> True = match t : (i, _) => True | qa() => True/qed() | qb() => True/qed() end", "-> Nat = match t : (i, _) => F(i) | qa() => True/qed() | qb() => 7 end"),
    ])
    .by_elaborator_in("/read", |error| matches!(error, Error::TypeMismatch { inferred, .. } if pinned(inferred) == "/S(i)"));
}

// A clash is concluded between two forcings of one binder only where they do not convert. `refl(@A)` forces `A` to `Nat` and to `Id(Nat)`, which are one type through `Id`'s definition, so the deletion rule keeps one and the arm stays mandatory: excused, `absurd(TEq/refl(@Nat))` is a closed inhabitant of `False`. The control writes the arm.
#[test]
fn two_spellings_of_one_type_forced_on_one_binder_do_not_clash() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        induct TEq: (Type, Type) -> Prop
        | refl(@A: Type): (A, A)
        end

        let Id(A: Type) -> Type = A;

        let absurd(p: TEq(Nat, Id(Nat))) -> Nat = match p | refl() => 0 end;

        let _forged: Nat = absurd(TEq/refl(@Nat));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "-> Nat = match p | refl() => 0 end",
            "-> False = match p end",
        ),
        ("let _forged: Nat", "let _forged: False"),
    ])
    .by_elaborator_in("/absurd", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// The shared walk pins a binder only where a target is the binder itself, and does not descend into a constructor: at a proposition `Tag/t(0)` and `Tag/t(7)` are one value, so `P(Tag/t(0))` and `P(Tag/t(7))` are one type and `mk(@a)` holds a payload no index recovers. Read through the constructor, `read` would answer `0` and `7` at two calls that convert, `apart` would equate them, and the transport would prove `Holds(7 == 0)`. `curios-analysis`'s `invert::tests::a_binder_under_a_constructor_is_not_pinned` holds the walk at the same target. The control's target is the binder itself, which pins it.
#[test]
fn a_payload_under_a_propositions_constructor_is_not_pinned() {
    control(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{True, False, Holds};

        induct Tag: Prop
        | t(n: Nat)
        end

        induct P: (Nat) -> Prop
        | mk(@a: Nat): (a)
        end

        let read(@x: Nat, p: P(x)) -> Nat = match p | mk(@a) => a end;

        let apart = read;

        let _forged = read;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("induct P: (Nat) -> Prop", "induct P: (Tag) -> Prop"),
        ("mk(@a: Nat): (a)", "mk(@a: Nat): (Tag/t(a))"),
        ("read(@x: Nat, p: P(x))", "read(@x: Tag, p: P(x))"),
        ("let apart = read;", "let apart: Eq()(read(@Tag/t(0), P/mk(@0)), read(@Tag/t(7), P/mk(@7))) = Eq/refl();"),
        ("let _forged = read;", "let _forged: Holds(read(@Tag/t(7), P/mk(@7)) == 0) = Eq/subst((n: Nat) => Holds(n == 0), apart, True/qed());"),
    ])
    .by_elaborator_in(
        "/read",
        |error| matches!(error, Error::LargeElimOfProp { name } if *name == global("/P")),
    );
}

// A guard's equation is settled with every equation inside it withheld, so a spelling settled under one inner case answers in no other. The guard is the match on `p`, which stands while `n` is open. Under the `0` arm of the split on `g(n)` the case equation makes `g(n)` and `g(g(n))` one, and the match on `p` fires to `k(0)`: a settlement that read that equation would spell the guard `k(0)` and answer it `true` under `j + 1` too, where the guard says `k(1)`, and at `k(m) = (m == 1)` that is `Holds(0 == 1)`. Withheld, the guard keeps its own spelling, `k(0)` is refused in the `0` arm, and nothing is settled for the next. The soundness board's elimination part holds the same guard settled under a split on a variable. The control claims nothing of `k`.
#[test]
fn a_guard_over_a_proof_match_is_settled_with_the_equations_inside_it_withheld() {
    control(
        r#"
        use /std/{Nat, Eq, Bool};
        use /std/Bool/{True, False, Holds};

        let g(n: Nat) -> Nat = match n | 0 => 0 | j + 1 => 1 end;

        let f(
            k: (Nat) -> Bool,
            n: Nat,
            p: Eq()(g(n), g(g(n))),
            guard: Holds(match p | refl(@z) => (match z | 0 => k(0) | j + 1 => k(1) end) end)) -> True =
            match (match p | refl(@z) => (match z | 0 => k(0) | j + 1 => k(1) end) end)
            | false => match guard end
            | true =>
                match g(n)
                | 0 => let settled: True = True/qed(); settled
                | j + 1 => True/qed()
                end
            end;

        let _forged: True = f((m: Nat) => m == 1, 1, Eq/refl(), True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (")) -> True =", ")) -> Holds(k(0)) ="),
        ("let settled: True =", "let settled: Holds(k(0)) ="),
        ("let _forged: True", "let _forged: Holds(0 == 1)"),
    ])
    .by_elaborator_in("/f", |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/sys/Bool/Holds(k(0))"));
}

// A proposition sits at the level of what it quantifies over, so none ranges over the propositions of its own level. `A` quantifies over every proposition and would be one of them, the instantiation Abel and Coquand's cast that unfolds without end needs and the self-application `delta` writes; sized, `A` sits a level above the `P` it takes, and `x(A)` is refused. The control applies `x` at a proposition of a lower level.
#[test]
fn a_proposition_over_every_proposition_is_not_applied_at_itself() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        let A: Prop = (P: Prop) -> (P) -> P;

        let delta(x: A) -> True = x(True)(True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("-> True = x(True)(True/qed())", "-> A = x(A)(x)")])
    .by_elaborator_in("/delta", |error| {
        matches!(error, Error::UniverseInconsistency { .. })
    });
}

// Berardi's route, where proof irrelevance meets a proposition that decides something: `F` answers `True` at `Two/a()` and `False` at `Two/b()`, irrelevance makes the two one value, so `F(Two/a())` and `F(Two/b())` would be one type and `True/qed()` a proof of `False`. A universe is relevant, so a match into `Prop` is the large elimination the guard refuses, as one into `Nat` is. `curios-cert`'s `kernel::infer::eliminate::proof_match_tests` puts the same elimination to the kernel under this name. The control's `Two` has the one bare constructor, which tells nothing, so it is taken into a universe as into any type.
#[test]
fn a_proposition_with_two_constructors_does_not_eliminate_into_a_universe() {
    control(
        r#"
        use /std/Bool/{False, True};

        induct Two: Prop
        | a()
        end

        let F(t: Two) -> Prop = match t | a() => True end;

        let cast(h: F(Two/a())) -> F(Two/a()) = h;

        let kept: F(Two/a()) = cast(True/qed());

        /std/print("one")
        "#,
        b"one",
    )
    .refused(&[
        (
            "        | a()\n        end",
            "        | a()\n        | b()\n        end",
        ),
        (
            "match t | a() => True end",
            "match t | a() => True | b() => False end",
        ),
        ("-> F(Two/a()) = h", "-> F(Two/b()) = h"),
        (
            "let kept: F(Two/a()) = cast(True/qed());\n\n        /std/print(\"one\")",
            "cast(True/qed())",
        ),
    ])
    .by_elaborator_in(
        "/F",
        |error| matches!(error, Error::LargeElimOfProp { name } if *name == global("/Two")),
    );
}

// The control: a proposition whose one constructor holds nothing tells nothing, so it is taken into a universe as into any type, and the refusal above is the second constructor's.
#[test]
fn a_proposition_with_one_bare_constructor_eliminates_into_a_universe() {
    assert_eq!(
        run(r#"
        use /std/Bool/{True};

        induct One: Prop
        | it()
        end

        let F(o: One) -> Prop = match o | it() => True end;

        let held: F(One/it()) = True/qed();

        /std/print("one")
        "#),
        b"one"
    );
}

// A motive is a term, so the result an elimination states may be computed by one: a definition named in its place, a function value a local binds, or a family the caller hands in. Each is typed under the motive's binders before the guard reads its sort, so each is the large elimination the written `(_) => Prop` above is, and the caller's family is held at the `Type` its parameter states. Each control's `Two` has one bare constructor.
#[test]
fn a_motive_computed_by_a_term_meets_the_guard_however_it_is_spelled() {
    control(
        r#"
        use /std/Bool/{False, True};

        induct Two: Prop
        | a()
        end

        let Universe(s: Two) -> Type = Prop;

        let F(t: Two) -> Prop = match t : Universe | a() => True end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "        | a()\n        end",
            "        | a()\n        | b()\n        end",
        ),
        ("| a() => True end", "| a() => True | b() => False end"),
    ])
    .by_elaborator_in(
        "/F",
        |error| matches!(error, Error::LargeElimOfProp { name } if *name == global("/Two")),
    );
    control(
        r#"
        use /std/Bool/{False, True};

        induct Two: Prop
        | a()
        end

        let F(t: Two) -> Prop =
            let m = (s: Two) => Prop;
            match t : m | a() => True end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "        | a()\n        end",
            "        | a()\n        | b()\n        end",
        ),
        ("| a() => True end", "| a() => True | b() => False end"),
    ])
    .by_elaborator_in(
        "/F",
        |error| matches!(error, Error::LargeElimOfProp { name } if *name == global("/Two")),
    );
    control(
        r#"
        induct Two: Prop
        | a()
        end

        let elim(P: (Two) -> Type, x: P(Two/a()), t: Two) -> P(t) =
            match t : P | a() => x end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "        | a()\n        end",
            "        | a()\n        | b()\n        end",
        ),
        (
            "x: P(Two/a()), t: Two",
            "x: P(Two/a()), y: P(Two/b()), t: Two",
        ),
        ("| a() => x end", "| a() => x | b() => y end"),
    ])
    .by_elaborator_in(
        "/elim",
        |error| matches!(error, Error::LargeElimOfProp { name } if *name == global("/Two")),
    );
}

// Coquand's paradox of trees, which needs a tree over every proposition to be one of them and its label read back out. Sized, `T` sits a level above the propositions `node` ranges over, so `T/node(T, …)` asks that `Prop` to be strictly below itself; and `node` holds a payload no index pins, so `label` is the large elimination the guard refuses. Each refusal stands where its own declaration is, so one program shows both. The control's tree ranges over `False`, a lower proposition, and its label is a proof.
#[test]
fn a_tree_over_every_proposition_is_none_of_them_and_hands_back_no_label() {
    control(
        r#"
        use /std/Bool/{True, False};

        induct T: Prop
        | node(A: Prop, f: (A) -> T)
        end

        let t: T = T/node(False, (x) => match x end);

        let label(x: T) -> True = match x | node(A, f) => True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{True, False}", "{False}"),
        ("T/node(False, (x) => match x end)", "T/node(T, (x) => x)"),
        (
            "-> True = match x | node(A, f) => True/qed() end",
            "-> Prop = match x | node(A, f) => A end",
        ),
    ])
    .by_elaborator_in("/t", |error| {
        matches!(error, Error::UniverseInconsistency { .. })
    })
    .by_elaborator_in(
        "/label",
        |error| matches!(error, Error::LargeElimOfProp { name } if *name == global("/T")),
    );
}

// A match that takes a proposition into data is one term however its proof is spelled. Along a hypothesis the algebra equates with its reverse, a cast through `Eq/subst`, a direct match, a local lemma, and along the hypothesis passed through `Eq/sym` twice and through `Eq/trans` with `Eq/refl()`, is its argument in each, in both checkers. Along a hypothesis whose ends nothing holds one, each spelling stands with its proof unreduced: one that fired on the shape of the match would accept its statement by reflexivity. The refused spellings' control is each of them along a hypothesis whose ends are one, where every cast fires.
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

    let refusals = control(
        r#"
        use /std/{Nat, Eq};

        let cast(P: (Nat) -> Type, @x: Nat, @y: Nat, q: Eq()(x, y), w: P(x)) -> P(y) =
            match q | refl() => w end;

        let u_subst(a: Nat, b: Nat, p: Eq()(a, a), v: Nat)
            -> Eq()(Eq/subst((_) => Nat, p, v), v) = Eq/refl();
        let u_match(a: Nat, b: Nat, p: Eq()(a, a), v: Nat)
            -> Eq()(match p : (x, y, _) => Nat | refl(@z) => v end, v) = Eq/refl();
        let u_lemma(a: Nat, b: Nat, p: Eq()(a, a), v: Nat)
            -> Eq()(cast((_) => Nat, p, v), v) = Eq/refl();
        let u_sym(a: Nat, b: Nat, p: Eq()(a, a), v: Nat)
            -> Eq()(Eq/subst((_) => Nat, Eq/sym(Eq/sym(p)), v), v) = Eq/refl();
        let u_trans(a: Nat, b: Nat, p: Eq()(a, a), v: Nat)
            -> Eq()(cast((_) => Nat, Eq/trans(p, Eq/refl()), v), v) = Eq/refl();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "u_subst(a: Nat, b: Nat, p: Eq()(a, a)",
            "u_subst(a: Nat, b: Nat, p: Eq()(a, b)",
        ),
        (
            "u_match(a: Nat, b: Nat, p: Eq()(a, a)",
            "u_match(a: Nat, b: Nat, p: Eq()(a, b)",
        ),
        (
            "u_lemma(a: Nat, b: Nat, p: Eq()(a, a)",
            "u_lemma(a: Nat, b: Nat, p: Eq()(a, b)",
        ),
        (
            "u_sym(a: Nat, b: Nat, p: Eq()(a, a)",
            "u_sym(a: Nat, b: Nat, p: Eq()(a, b)",
        ),
        (
            "u_trans(a: Nat, b: Nat, p: Eq()(a, a)",
            "u_trans(a: Nat, b: Nat, p: Eq()(a, b)",
        ),
    ]);
    for (at, statement) in [
        (
            "/u_subst",
            "/std/Eq/Eq(Nat)(/std/Eq/subst(@Nat, @a, @b, (_) => Nat, p, v), v)",
        ),
        (
            "/u_match",
            "/std/Eq/Eq(Nat)(match p: (x, y, x2) => Nat | refl(@z) => v end, v)",
        ),
        (
            "/u_lemma",
            "/std/Eq/Eq(Nat)(/cast((_) => Nat, @a, @b, p, v), v)",
        ),
        (
            "/u_sym",
            "/std/Eq/Eq(Nat)(/std/Eq/subst(@Nat, @a, @b, (_) => Nat, /std/Eq/sym(@Nat, @b, @a, /std/Eq/sym(@Nat, @a, @b, p)), v), v)",
        ),
        (
            "/u_trans",
            "/std/Eq/Eq(Nat)(/cast((_) => Nat, @a, @b, /std/Eq/trans(@Nat, @a, @b, @b, p, /std/Eq/Eq/refl(b)), v), v)",
        ),
    ] {
        refusals.by_elaborator_in(at, |error| {
            matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == statement)
        });
    }
}

// The singleton rung asks every payload to be pinned, a hidden one as a plain one. `Q/mk`'s plain `n` is its index and its hidden `k` is pinned by none, so `Q/mk(@0, 5)` and `Q/mk(@1, 5)` are one proof of `Q(5)`, and `read` telling them apart would carry `True` to `False` through `cast`. `curios-cert`'s `recheck::shape_tests` puts the route to the kernel under this name. The control is the same family with `k` an index too, read into data.
#[test]
fn a_hidden_payload_no_index_pins_is_not_read_out_of_a_proof() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{Holds, True, False};

        induct Q: (Nat, Nat) -> pub Prop
        | mk(@k: Nat, n: Nat): (k, n)
        end

        let read(j: Nat, m: Nat, q: Q(j, m)) -> Nat = match q | mk(@k, _) => k end;

        let cast = read;

        let _forged: Holds(read(1, 5, Q/mk(@1, 5)) == 1) = True/qed();

        /std/print("1")
        "#,
        b"1",
    )
    .refused(&[
        ("induct Q: (Nat, Nat) -> pub Prop", "induct Q: (Nat) -> pub Prop"),
        ("mk(@k: Nat, n: Nat): (k, n)", "mk(@k: Nat, n: Nat): (n)"),
        ("read(j: Nat, m: Nat, q: Q(j, m))", "read(m: Nat, q: Q(m))"),
        ("let cast = read;", "let cast(G: (Q(5)) -> Prop, y: G(Q/mk(@0, 5))) -> G(Q/mk(@1, 5)) = y;"),
        ("let _forged: Holds(read(1, 5, Q/mk(@1, 5)) == 1) = True/qed();", "let _forged: False = cast((q: Q(5)) => Holds(match read(5, q) | 0 => true | _ => false end), True/qed());"),
    ])
    .by_elaborator_in(
        "/read",
        |error| matches!(error, Error::LargeElimOfProp { name } if *name == global("/Q")),
    );
}

// Reduction reads a tuple's slot off a constructor's value through the flat runtime view `(tag, payload...)`, so `P/mk(1).1` would be `1`; a slot is typed off what it is a slot of, and a family's value is no tuple, so `get` is refused, where `P/mk(0)` and `P/mk(1)` being one proof would carry `True` to `False` through `cast`. `curios-cert`'s `recheck::shape_tests` puts the route to the kernel under this name. The control is the same slot read off a tuple.
#[test]
fn a_proofs_payload_is_read_by_no_tuple_slot() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{Holds, True, False};

        let get(p: {Nat, Nat}) -> Nat = p.1;

        let cast = get;

        let _held: Holds(match get((0, 1)) | 1 => true | _ => false end) = True/qed();

        /std/print("1")
        "#,
        b"1",
    )
    .refused(&[
        ("let get(p: {Nat, Nat})", "induct P: pub Prop\n        | mk(n: Nat)\n        end\n\n        let get(p: P)"),
        ("let cast = get;", "let cast(G: (P) -> Prop, y: G(P/mk(0))) -> G(P/mk(1)) = y;"),
        ("let _held: Holds(match get((0, 1)) | 1 => true | _ => false end) = True/qed();", "let _forged: False = cast((p: P) => Holds(match get(p) | 0 => true | _ => false end), True/qed());"),
    ])
    .by_elaborator_in("/get", |error| matches!(error, Error::NotATuple { .. }));
}
