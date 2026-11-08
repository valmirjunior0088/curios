//! A type the walk computes or unfolds before it reads an occurrence: an alias, a type-level match decided or stuck, a sort a caller would decide, and a field or payload a type-level `rec` computes.

use {
    super::test_support::*, crate::test_support::*, curios_common::fill, curios_core::Polarity,
    curios_elab::Error,
};

// A `let`-defined alias is unfolded rather than read as an opaque former, so the arrow it hides counts where it stands — once for the negative, twice for the merely positive. The control's alias holds its argument right of the arrow.
#[test]
fn a_negative_occurrence_behind_an_alias_is_rejected() {
    control(
        r#"
        use /std/{Bool};

        let Arrow(X : Type) -> Type = (Bool) -> X;

        induct Bad : pub Type
        | c(Arrow(Bad))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(Bool) -> X", "(X) -> Bool")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// The same twice over. The control's alias holds its argument right of the arrow.
#[test]
fn a_non_strict_occurrence_behind_an_alias_is_rejected() {
    control(
        r#"
        use /std/{Bool};

        let Arrow(X : Type) -> Type = (Bool) -> X;

        induct Bad : pub Type
        | c(Arrow(Arrow(Bad)))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(Bool) -> X", "(X) -> Bool")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Pos,
                ..
            }
        )
    });
}

// The other green row, and the reason the walk forces a head at all: a field type bound by a plain type-level `let` must still be unfolded to the type former it names. A guard that declined to force would take this with it.
#[test]
fn a_field_type_bound_by_a_plain_alias_is_admitted() {
    let source = r#"
        use /std/{Nat, Bool};

        let Pairing : Type = {Nat, Bool};

        struct Row : pub Type { value : Pairing }

        let one : Row = Row { value = (1, true) };

        /std/print("aliased")
        "#;
    assert_eq!(run(source), b"aliased");
}

// A type-level `match` whose scrutinee is decided is unfolded to the arm it takes, so the occurrence is read where that arm puts it. The control decides it at the arm `X`.
#[test]
fn a_negative_occurrence_behind_a_decided_type_level_match_is_rejected() {
    control(
        r#"
        use /std/{Bool};

        let Pick(b : Bool, X : Type) -> Type =
            match b : (_) => Type
            | true => (X) -> Bool
            | false => X
            end;

        induct Bad : pub Type
        | c(Pick(false, Bad))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Pick(false, Bad)", "Pick(true, Bad)")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// The same `match` stuck on a parameter is no arm at all: the analysis answers that it cannot see through it, whatever the arms hold — here both are strict — which is the safe reading of a position it has not read. The control decides the same `match` at `true`.
#[test]
fn an_occurrence_behind_a_stuck_type_level_match_is_rejected() {
    control(
        r#"
        use /std/{Bool, Nat};

        let Pick(b : Bool, X : Type) -> Type =
            match b : (_) => Type
            | true => {Nat, X}
            | false => X
            end;

        induct Stuck(b : Bool) : pub Type
        | leaf()
        | c(Pick(true, Stuck(b)))
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Pick(true, Stuck(b))", "Pick(b, Stuck(b))")])
    .by_elaborator_in("/Stuck", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Mixed,
                ..
            }
        )
    });
}

// A recursive occurrence that only a sort left to a caller would uncover. At `Prop` the match on `h` is its arm, an equation between two proofs being one, and the arm holds `Bad` left of an arrow. The entry is decided as it is stated, where the match stands: one the walk cannot see through is read at `Mixed`, so the occurrence is refused whatever a use states. `curios-cert`'s `recheck::sort_tests` puts the same declaration to the kernel under this name. The control's arm holds no occurrence.
#[test]
fn an_occurrence_a_sort_left_to_a_caller_would_uncover_is_refused() {
    control(
        r#"
        use /std/{Eq};
        use /std/Bool/{False};

        induct Bad(@A: Sort, x: A, y: A, h: Eq()(x, y)): pub Type
        | c(f: match h | refl() => (False) -> False end)
        end

        /std/print("")
        "#,
        b"",
    )
    .refused(&[("(False) -> False", "(Bad(x, y, h)) -> False")])
    .by_elaborator_in(
        "/Bad",
        |error| matches!(error, Error::NotStrictlyPositive { name, .. } if *name == global("/Bad")),
    );
}

// A parameter that only a sort left to a caller would uncover. `F`'s `B` stands in the arm of a match on `h`, which at `Prop` is its arm, an equation between two proofs being one, and holds `B` left of an arrow. `F` is decided as it is stated, where the match stands, so `G`, holding an `F` at `Prop` with itself at `B`, reaches itself through what the checker cannot see through. `curios-cert`'s `recheck::assignment_tests` puts the same declarations to the kernel under this name. Rename both or neither. The control's `G` holds an `F` at `{}` in `B`.
#[test]
fn a_parameter_a_sort_left_to_a_caller_would_uncover_is_read_at_mixed() {
    control(
        r#"
        use /std/{Eq};
        use /std/Bool/{True, False};

        induct F(@A: Sort, x: A, y: A, h: Eq()(x, y), B: Type): pub Type
        | mk(f: match h: (_, _, _) => Prop | refl() => (B) -> False end)
        end

        induct G: pub Type
        | g(w: F(True/qed(), True/qed(), Eq/refl(), {}))
        end

        /std/print("")
        "#,
        b"",
    )
    .refused(&[("Eq/refl(), {}))", "Eq/refl(), G))")])
    .by_elaborator_in(
        "/G",
        |error| matches!(error, Error::NotStrictlyPositive { name, .. } if *name == global("/G")),
    );
}

// A field whose type a self-calling type-level `rec` computes. The walk forces the `rec` head, cannot see through the stuck `match` it exposes, descends into the arm holding the recursive call, opens that arm against a fresh binder — and forces the same node again, one level deeper, forever. `RecGroup::member_body` substitutes `Term::rec_proj` for every recursive occurrence, so each unfolding hands the walk a structurally identical group; nothing but a guard keyed on that group stops the descent, and `unfolded` cannot be it because an inline group has no name to key on.
#[test]
fn a_field_type_a_self_calling_rec_computes_is_admitted() {
    let source = r#"
        use /std/{Nat, Str};

        induct Labels : pub Type
        | nil()
        | cons(Str, Labels)
        end

        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;

        struct Row(L : Labels) : pub Type { value : Count(L) }

        let one : Row(Labels/cons("a", Labels/nil())) =
            Row(Labels/cons("a", Labels/nil())) { value = (1, ()) };

        /std/print("computed")
        "#;
    assert_eq!(run(source), b"computed");
}

// The recursive call under another type former rather than in a bare tuple field. Polarity composition walks into `Option`'s parameter, so the forced node is reached by a different route and must be guarded on the same key.
#[test]
fn a_computed_field_type_under_a_parameterized_family_is_admitted() {
    let source = r#"
        use /std/{Str, Option};

        induct Labels : pub Type
        | nil()
        | cons(Str, Labels)
        end

        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => Option(Count(rest))
            end;

        struct Row(L : Labels) : pub Type { value : Count(L) }

        let one : Row(Labels/nil()) = Row(Labels/nil()) { value = () };

        /std/print("optioned")
        "#;
    assert_eq!(run(source), b"optioned");
}

// The arm is the recursive call and nothing else — no former to descend through, no product to open. The shortest program that closes the cycle, and the one that shows the guard is about the `rec` head rather than about what encloses it.
#[test]
fn a_computed_field_type_that_is_a_bare_recursive_call_is_admitted() {
    let source = r#"
        use /std/{Str};

        induct Labels : pub Type
        | nil()
        | cons(Str, Labels)
        end

        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => Count(rest)
            end;

        struct Row(L : Labels) : pub Type { value : Count(L) }

        let one : Row(Labels/nil()) = Row(Labels/nil()) { value = () };

        /std/print("bare")
        "#;
    assert_eq!(run(source), b"bare");
}

// The same shape over an intrinsic carrier: the description the type is computed from is a numeral rather than a declared family, so nothing about the loop depends on the index being inductive.
#[test]
fn a_field_type_computed_by_recursion_over_a_numeral_is_admitted() {
    let source = r#"
        use /std/{Nat, Bool};

        let Rep(n : Nat) -> Type =
            match n : (_) => Type
            | 0 => {}
            | p + 1 => {Bool, Rep(p)}
            end;

        struct Vector(n : Nat) : pub Type { value : Rep(n) }

        let one : Vector(1) = Vector(1) { value = (true, ()) };

        /std/print("counted")
        "#;
    assert_eq!(run(source), b"counted");
}

// A constructor payload rather than a struct field. Both declaration forms are split into parts and walked by the same traversal, so a guard that fixed only one of them would be fixing the caller instead of the walk.
#[test]
fn a_constructor_payload_a_self_calling_rec_computes_is_admitted() {
    let source = r#"
        use /std/{Nat, Str};

        induct Labels : pub Type
        | nil()
        | cons(Str, Labels)
        end

        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;

        induct Boxed(L : Labels) : pub Type
        | mk(Count(L))
        end

        let boxed : Boxed(Labels/nil()) = Boxed/mk(@Labels/nil(), ());

        match boxed
        | mk(_) => /std/print("boxed")
        end
        "#;
    assert_eq!(run(source), b"boxed");
}

// A user's own refinement over a packed carrier — the shape `/std/Cli` declares, a struct field whose proposition is computed from an earlier field: a self-calling fold applied to it.
//
// The cost is the positivity walk's, not the declaration checker's, and it is spent where the declaration is published, on a budget restored for that walk. Nothing here turns on how much native stack a thread has, and the tree sets none anywhere. What keeps a fold clear of this shape is the contrast in the test below.
#[test]
fn a_refinement_field_over_a_self_calling_fold_is_admitted() {
    let source = r#"
        use /std/{Bytes, Bool};
        use /std/Bool/{True, False};

        let always(b : Bytes) -> Bool =
            match b
            | x[] => true
            | x[h, ..t] => always(t)
            end;

        let Certified(b : Bytes) -> Prop =
            match always(b) | true => True | false => False end;

        struct Wrapped : Type {
            bytes : Bytes,
            ok : Certified(bytes),
        }

        let w : Wrapped = Wrapped { bytes = x[0x61], ok = True/qed() };

        /std/print("certified")
        "#;
    assert_eq!(run(source), b"certified");
}

// The green row: a fold like `/std/Bytes/to_nat` trips none of the above, and the `; ih` binding is why: a fold hypothesis is a *binder*, not a self-application, so forcing the member exposes no recursive call to force again. Swap this one arm for the explicit `always(t)` of the test above and the identical program diverges.
#[test]
fn a_refinement_field_over_a_fold_hypothesis_is_admitted() {
    let source = r#"
        use /std/{Bytes, Bool};
        use /std/Bool/{True, False};

        let always(b : Bytes) -> Bool =
            match b
            | x[] => true
            | x[h, ..t]; ih => ih
            end;

        let Certified(b : Bytes) -> Prop =
            match always(b) | true => True | false => False end;

        struct Wrapped : Type {
            bytes : Bytes,
            ok : Certified(bytes),
        }

        let w : Wrapped = Wrapped { bytes = x[0x61], ok = True/qed() };

        /std/print("folded")
        "#;
    assert_eq!(run(source), b"folded");
}

// A family the analysis cannot finish reading is refused for the budget, and so is nothing declared after it. `Wrap` stores its parameter under twelve doubling aliases, strictly, and `Outer` stores itself in that parameter. A checker short of the budget to unfold the aliases read the parameter at `Mixed`, and with that vector filed `Outer` was refused as not strictly positive, by an analysis that ran out of nothing. At no budget of the ladder is that the report: each accepts the program or names the budget.
#[test]
fn a_family_read_under_a_short_budget_is_never_refused_as_not_positive() {
    let aliases = (1..=12)
        .map(|k| format!("let W{k}(A: Type) -> Type = W{}(W{}(A));\n", k - 1, k - 1))
        .collect::<String>();
    let source = fill!(
        "struct Box(A: Type): pub Type { held: A }\nlet W0(A: Type) -> Type = A;\n${aliases}induct Wrap(A: Type): Type | wrap(Box(W12(A))) end\ninduct Outer: Type | leaf() | node(Wrap(Outer)) end\n/std/print(\"ok\")\n",
        aliases
    );

    let mut short = 0;
    for budget in (4..=20).map(|step| step * 2_000) {
        if let Err(report) = typecheck_within(budget, &source) {
            assert!(
                !report.contains("not strictly positive") && report.contains("ran out"),
                "at a budget of {budget}:\n{report}"
            );
            short += 1;
        }
    }
    assert!(short > 0, "no budget of the ladder is short of the program");
}
