//! What the gate refuses, and by which rule.
//!
//! Each row names the rule that must do the rejecting rather than asserting a bare failure, which would pass on a typo in the fixture — worth the words wherever a future relaxation of `occurrences` could plausibly start admitting the shape.

use {super::test_support::*, crate::tests::typecheck_within};

// A declaration reaching itself through a *type-former parameter* is refused, and this pins that it is refused rather than assumed.
//
// `curios-analysis/src/positivity.rs` names this obligation deliberately out of scope: `F` is a binder with no known polarity, so `Mu` cannot be checked from its own body, and discharging it properly needs an inferred per-binder obligation in a side store. Out of scope leaves open which way the analysis falls, and only one way is safe — an unknown former must read as `Mixed`, never as `Strict`. Nothing in the corpus takes a type-former parameter on an `induct`, so nothing else exercises the choice, and a later improvement to `occurrences` that taught it to see through `F(Mu(F))` would flip it silently.
//
// What the wrong direction costs is Curry's paradox with no recursion in sight. Admitting `Mu` lets it be instantiated at a negative former — `let Neg(X : Type) -> Type = (X) -> False` — and `Mu(Neg)`'s constructor is then `fix : ((Mu(Neg)) -> False) -> Mu(Neg)`, the negative occurrence this gate exists to forbid, spelled through a parameter instead of directly. `out(m) = match m | fix(f) => f end` and `delta(m) = out(m)(m)` give `delta(Mu/fix(delta)) : False`.
//
// Refused as it stands rather than a closed hole, and the diagnostic is asserted so the fixture cannot pass on an unrelated failure. Its control is `a_type_former_parameter_not_recursed_through_is_admitted`, which shows the refusal is about self-reference through an unknown former and not a blanket ban on higher-kinded parameters — a ban would take `/std`'s `Monad`-shaped abstractions with it.
#[test]
fn a_declaration_recursing_through_a_type_former_parameter_is_refused() {
    rejected_by(
        r#"
        induct Mu(F : (Type) -> Type) : pub Type
        | fix(F(Mu(F)))
        end

        /std/print("unreachable")
        "#,
        "is not strictly positive",
    );
}

// The whole reason the gate exists. `Bad` is not the initial algebra of any functor — the payload is a function *out of* `Bad` — and admitting it hands back an eliminator that inhabits `False` in four lines with no recursion.
#[test]
fn a_negative_occurrence_is_rejected() {
    rejected_by(
        r#"
        use /std/Bool/{False};

        induct Bad : pub Type
        | c(f : (Bad) -> False)
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself negatively",
    );
}

// Positive but not strictly positive: two arrows, so the sign flips back. This records the impredicative-`Prop` decision as a test rather than as prose — with an impredicative `Prop` and a universe hierarchy both present, the Coquand–Paulin construction applies, so the merely-positive relaxation other systems allow is not available here.
#[test]
fn a_positive_but_not_strictly_positive_occurrence_is_rejected() {
    rejected_by(
        r#"
        use /std/Bool/{False};

        induct Bad2 : pub Type
        | c(f : ((Bad2) -> False) -> False)
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself positively, but not strictly",
    );
}

// The composition case, and the one a check without polarity vectors would miss entirely: `Sink` is contravariant in its parameter, so `Trap`'s payload puts `Trap` left of an arrow one indirection away. Nothing about `Trap`'s own constructor looks negative — the rejection comes from `Sink`'s vector.
#[test]
fn a_negative_occurrence_borrowed_through_another_declaration_is_rejected() {
    rejected_by(
        r#"
        use /std/{Nat};

        induct Sink(A : Type) : pub Type
        | drain(f : (A) -> Nat)
        end

        induct Trap : pub Type
        | caught(Sink(Trap))
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself negatively",
    );
}

// A cycle whose negative step is in the *other* member. Neither declaration is negative on its own inspection, and the group boundary is not on the registry entry, so this is caught only by closing the occurrence relation transitively.
#[test]
fn a_negative_cycle_through_a_mutual_group_is_rejected() {
    rejected_by(
        r#"
        use /std/Bool/{False};

        induct Left : Type
        | wrap(Right)
        and Right : Type
        | back(f : (Left) -> False)
        end

        /std/print("unreachable")
        "#,
        "is not strictly positive",
    );
}

// A struct is checked on the same footing as an inductive: it is a nominal record, so a field that consumes the record it belongs to is the same unsoundness wearing different syntax.
#[test]
fn a_negative_struct_field_is_rejected() {
    rejected_by(
        r#"
        use /std/Bool/{False};

        struct Consume : pub Type {
            run : (Consume) -> False,
        }

        /std/print("unreachable")
        "#,
        "it occurs in itself negatively",
    );
}

// `Cell` is invariant — it is read *and* written — so nothing recursive may travel through one, even though the occurrence looks like a plain payload.
#[test]
fn recursion_through_an_invariant_intrinsic_is_rejected() {
    rejected_by(
        r#"
        use /std/{Cell};

        induct Knot : pub Type
        | tie(Cell(Knot))
        end

        /std/print("unreachable")
        "#,
        "is not strictly positive",
    );
}

// The double negative borrowed the same way: `Twice` is positive in its parameter and not strictly so, and the diagonal it gives `Trap` is refused under the merely-positive phrase rather than read as an occurrence the composition cleaned.
#[test]
fn a_non_strict_occurrence_borrowed_through_another_declaration_is_rejected() {
    rejected_by(
        r#"
        use /std/Bool/{False};

        induct Twice(A : Type) : pub Type
        | mk(f : ((A) -> False) -> False)
        end

        induct Trap : pub Type
        | caught(Twice(Trap))
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself positively, but not strictly",
    );
}

// A `struct` parameter carries a polarity vector as an `induct`'s does, and a record is the declaration a consumer is likeliest to wrap a recursive type in.
#[test]
fn a_negative_occurrence_borrowed_through_a_struct_parameter_is_rejected() {
    rejected_by(
        r#"
        use /std/{Nat};

        struct Sink(A : Type) : pub Type { f : (A) -> Nat }

        induct Trap : pub Type
        | caught(Sink(Trap))
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself negatively",
    );
}

#[test]
fn a_non_strict_occurrence_borrowed_through_a_struct_parameter_is_rejected() {
    rejected_by(
        r#"
        use /std/Bool/{False};

        struct Twice(A : Type) : pub Type { f : ((A) -> False) -> False }

        induct Trap : pub Type
        | caught(Twice(Trap))
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself positively, but not strictly",
    );
}

// A `let`-defined alias is unfolded rather than read as an opaque former, so the arrow it hides counts where it stands — once for the negative, twice for the merely positive.
#[test]
fn a_negative_occurrence_behind_an_alias_is_rejected() {
    rejected_by(
        r#"
        use /std/{Bool};

        let Neg(X : Type) -> Type = (X) -> Bool;

        induct Bad : pub Type
        | c(Neg(Bad))
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself negatively",
    );
}

#[test]
fn a_non_strict_occurrence_behind_an_alias_is_rejected() {
    rejected_by(
        r#"
        use /std/{Bool};

        let Neg(X : Type) -> Type = (X) -> Bool;

        induct Bad : pub Type
        | c(Neg(Neg(Bad)))
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself positively, but not strictly",
    );
}

// `List` is covariant, so an occurrence under it is whatever it was inside: a strict one stays admitted, and the arrow's left is still the arrow's left.
#[test]
fn a_negative_occurrence_under_a_covariant_intrinsic_is_rejected() {
    rejected_by(
        r#"
        use /std/{List, Bool};

        induct Bad : pub Type
        | c(List((Bad) -> Bool))
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself negatively",
    );
}

// A type-level `match` whose scrutinee is decided is unfolded to the arm it takes, so the occurrence is read where that arm puts it.
#[test]
fn a_negative_occurrence_behind_a_decided_type_level_match_is_rejected() {
    rejected_by(
        r#"
        use /std/{Bool};

        let Pick(b : Bool, X : Type) -> Type =
            match b : (_) => Type
            | true => (X) -> Bool
            | false => X
            end;

        induct Bad : pub Type
        | c(Pick(true, Bad))
        end

        /std/print("unreachable")
        "#,
        "it occurs in itself negatively",
    );
}

// The same `match` stuck on a parameter is no arm at all: the analysis answers that it cannot see through it, whatever the arms hold — here both are strict — which is the safe reading of a position it has not read.
#[test]
fn an_occurrence_behind_a_stuck_type_level_match_is_rejected() {
    rejected_by(
        r#"
        use /std/{Bool, Nat};

        let Pick(b : Bool, X : Type) -> Type =
            match b : (_) => Type
            | true => {Nat, X}
            | false => X
            end;

        induct Stuck(b : Bool) : pub Type
        | leaf()
        | c(Pick(b, Stuck(b)))
        end

        /std/print("unreachable")
        "#,
        "a position the checker cannot see through",
    );
}

// A family the analysis cannot finish reading is refused for the budget, and so is nothing declared after it. `Wrap` stores its parameter under twelve doubling aliases, strictly, and `Outer` stores itself in that parameter. A checker short of the budget to unfold the aliases read the parameter at `Mixed`, and with that vector filed `Outer` was refused as not strictly positive, by an analysis that ran out of nothing. At no budget of the ladder is that the report: each accepts the program or names the budget.
#[test]
fn a_family_read_under_a_short_budget_is_never_refused_as_not_positive() {
    let aliases = (1..=12)
        .map(|k| format!("let W{k}(A: Type) -> Type = W{}(W{}(A));\n", k - 1, k - 1))
        .collect::<String>();
    let source = format!(
        "struct Box(A: Type): pub Type {{ held: A }}\nlet W0(A: Type) -> Type = A;\n{aliases}induct Wrap(A: Type): Type | wrap(Box(W12(A))) end\ninduct Outer: Type | leaf() | node(Wrap(Outer)) end\n/std/print(\"ok\")\n"
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
