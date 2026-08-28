//! What the gate refuses, and by which rule.
//!
//! Each row names the rule that must do the rejecting rather than asserting a bare failure, which would pass on a typo in the fixture — worth the words wherever a future relaxation of `occurrences` could plausibly start admitting the shape.

use super::test_support::*;

// A declaration reaching itself through a *type-former parameter* is refused, and this pins that it is refused rather than assumed.
//
// `curios-analysis/src/positivity.rs` names this obligation deliberately out of scope: `F` is a binder with no known polarity, so `Mu` cannot be checked from its own body, and discharging it properly needs an inferred per-binder obligation in a side store. Out of scope leaves open which way the analysis falls, and only one way is safe — an unknown former must read as `Mixed`, never as `Strict`. Nothing in the corpus takes a type-former parameter on an `induct`, so nothing exercised the choice, and a later improvement to `occurrences` that taught it to see through `F(Mu(F))` would flip it silently.
//
// What the wrong direction costs is Curry's paradox with no recursion in sight. Admitting `Mu` lets it be instantiated at a negative former — `let Neg(X : Type) -> Type = (X) -> False` — and `Mu(Neg)`'s constructor is then `fix : ((Mu(Neg)) -> False) -> Mu(Neg)`, the negative occurrence this gate exists to forbid, spelled through a parameter instead of directly. `out(m) = match m | fix(f) => f end` and `delta(m) = out(m)(m)` give `delta(Mu/fix(delta)) : False`.
//
// Verified as refused rather than as a closed hole: this run found the rule already holding, and the diagnostic is asserted so the fixture cannot pass on an unrelated failure. Its control is `a_type_former_parameter_not_recursed_through_is_admitted`, which shows the refusal is about self-reference through an unknown former and not a blanket ban on higher-kinded parameters — a ban would take `/std`'s `Monad`-shaped abstractions with it.
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
    rejected(
        r#"
        use /syn/{False};

        induct Bad : pub Type
        | c(f : (Bad) -> False)
        end

        /std/print("unreachable")
        "#,
    );
}

// Positive but not strictly positive: two arrows, so the sign flips back. This records the impredicative-`Prop` decision as a test rather than as prose — with an impredicative `Prop` and a universe hierarchy both present, the Coquand–Paulin construction applies, so the merely-positive relaxation other systems allow is not available here.
#[test]
fn a_positive_but_not_strictly_positive_occurrence_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        induct Bad2 : pub Type
        | c(f : ((Bad2) -> False) -> False)
        end

        /std/print("unreachable")
        "#,
    );
}

// The composition case, and the one a check without polarity vectors would miss entirely: `Sink` is contravariant in its parameter, so `Trap`'s payload puts `Trap` left of an arrow one indirection away. Nothing about `Trap`'s own constructor looks negative — the rejection comes from `Sink`'s vector.
#[test]
fn a_negative_occurrence_borrowed_through_another_declaration_is_rejected() {
    rejected(
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
    );
}

// A cycle whose negative step is in the *other* member. Neither declaration is negative on its own inspection, and the group boundary is not on the registry entry, so this is caught only by closing the occurrence relation transitively.
#[test]
fn a_negative_cycle_through_a_mutual_group_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        induct Left : Type
        | wrap(Right)
        and Right : Type
        | back(f : (Left) -> False)
        end

        /std/print("unreachable")
        "#,
    );
}

// A struct is checked on the same footing as an inductive: it is a nominal record, so a field that consumes the record it belongs to is the same unsoundness wearing different syntax.
#[test]
fn a_negative_struct_field_is_rejected() {
    rejected(
        r#"
        use /syn/{False};

        struct Consume : pub Type {
            run : (Consume) -> False,
        }

        /std/print("unreachable")
        "#,
    );
}

// `Cell` is invariant — it is read *and* written — so nothing recursive may travel through one, even though the occurrence looks like a plain payload.
#[test]
fn recursion_through_an_invariant_intrinsic_is_rejected() {
    rejected(
        r#"
        use /std/{Cell};

        induct Knot : pub Type
        | tie(Cell(Knot))
        end

        /std/print("unreachable")
        "#,
    );
}
