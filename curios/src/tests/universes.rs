//! End-to-end coverage for the implicit cumulative universe hierarchy.
//!
//! The hierarchy exists to remove one source of unsoundness: under `Type :
//! Type` a type could classify itself, which admits Girard's paradox. The
//! solver's own rules are unit-tested in `curios-elab/src/universe/tests.rs`;
//! these check what a *user* can observe.
//!
//! There is deliberately no "the paradox is rejected" test here. Every
//! declaration generalizes over its levels, so a self-reference like
//! `Box/wrap(Box)` instantiates `Box` at two different levels and is admitted —
//! correctly, since that is stratification working. And `SYNTAX.md` gives no
//! syntax for universe variables or explicit arguments, so a program cannot
//! force two occurrences to share a level. Whether *any* surface program can
//! reach `UniverseInconsistency` is an open question; until it is answered,
//! asserting a rejection here would only pin behavior that holds for the wrong
//! reason. Stratification itself is covered at unit level by
//! `a_polymorphic_definition_instantiates_at_prop_and_type`, which pins
//! `id(Prop)` to level 1 and `id(Type)` to level 2.

use super::run;

// `Unit` is an ordinary
// level-0 type, and storing it in `Box` requires it where a higher level is
// expected. Cumulativity is what admits this, and it is the reason a
// declaration whose level is determined can be minimized rather than
// generalized — the lower level is usable wherever a higher one is required.
#[test]
fn cumulativity_admits_a_lower_universe_where_a_higher_is_required() {
    let source = r#"
        use /std/{Str, Handle};
        induct Unit : pub Type
        | only()
        end
        induct Box : pub Type
        | wrap(Type)
        end
        let boxed : Box = Box/wrap(Unit);
        match boxed | wrap(_) => /std/print("stored") end
        "#;

    assert_eq!(run(source), b"stored");
}

// One polymorphic declaration instantiated at two distinct levels in a single
// program: `pick(42)` chooses `A := Nat` (level 0), while `pick(Nat)` chooses
// `A := Type` (level 1). A declaration monomorphized to either level cannot
// serve both, so this is the direct guard on minimizing result-only levels —
// that change must not reach a level a use site genuinely chooses.
#[test]
fn one_declaration_serves_two_universe_levels() {
    let source = r#"
        use /std/{Nat, Str, Handle};
        let pick(@A : Type, x : A) -> A = x;
        let small : Nat = pick(7);
        let large : Type = pick(Nat);
        /std/print("both")
        "#;

    assert_eq!(run(source), b"both");
}
