//! End-to-end coverage for the implicit cumulative universe hierarchy.
//!
//! The hierarchy exists to remove one source of unsoundness: under `Type : Type` a type could classify itself, which admits Girard's paradox. The solver's own rules are unit-tested in `curios-analysis/src/satisfy/tests.rs`; these check what a *user* can observe.
//!
//! There is deliberately no "the paradox is rejected" test here. A declaration generalizes over the levels its *interface* carries, so a self-reference like `Box/wrap(Box)` instantiates `Box` at two different levels and is admitted — correctly, since that is stratification working. And `syntax.md` gives no syntax for universe variables or explicit arguments, so a program cannot force two occurrences to share a level. Whether *any* surface program can reach `UniverseInconsistency` is an open question; until it is answered, asserting a rejection here would only pin behavior that holds for the wrong reason. Stratification itself is covered at unit level by `a_polymorphic_definition_instantiates_at_prop_and_type`, which pins `id(Prop)` to level 1 and `id(Type)` to level 2.
//!
//! "Interface" is load-bearing in that sentence and is what `a_body_carried_level_is_minimized_rather_than_generalized` pins: the levels reachable only through a body are *minimized* instead, so the set of declarations a use site can instantiate at two levels is narrower than "every declaration".

use {
    super::run,
    curios_core::Module,
    curios_pipeline::{DEFAULT_STEP_BUDGET, typecheck_with_prelude},
    curios_text::{Entrypoint, RootSource},
    std::collections::BTreeMap,
};

/// Every definition's finalized universe parameter count, keyed by the name its item describes itself with.
fn universe_parameters(source: &str) -> BTreeMap<String, usize> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let (module, _): (Module, _) =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fixture type-checks");

    module
        .items
        .iter()
        .flat_map(|item| {
            let described = item.describe();
            item.definitions().into_iter().map(move |definition| {
                (
                    described.clone(),
                    definition.universe_context.parameter_count,
                )
            })
        })
        .collect()
}

// `Unit` is an ordinary level-0 type, and storing it in `Box` requires it where a higher level is expected. Cumulativity is what admits this, and it is the reason a declaration whose level is determined can be minimized rather than generalized — the lower level is usable wherever a higher one is required.
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

// One polymorphic declaration instantiated at two distinct levels in a single program: `pick(42)` chooses `A := Nat` (level 0), while `pick(Nat)` chooses `A := Type` (level 1). A declaration monomorphized to either level cannot serve both, so this is the direct guard on minimizing result-only levels — that change must not reach a level a use site genuinely chooses.
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

// A universe parameter is minted only from a declaration's *interface* — its type and the registry signatures a use site instantiates. A level reachable only through the body is minimized to a constant instead (`UniverseSolver::finalize`'s `internal` set, which no occurrence could choose a value for), so `carrier`, whose sole sort occurrence is the `Type` it stores, is monomorphic, while `holder`, whose level is tied to a `Type`-sorted parameter, generalizes.
//
// That asymmetry is what shuts [The refinement key](../../../documentation/soundness/what-the-kernel-consults/the-refinement-key.md)'s still-open elaborator-side copy against *source*. The defect needs two occurrences of one definition at differing instances whose values differ by level, and only a level carried into a payload can make a value differ — `Type u` embedded in a term is the entry's own counterexample. Such a level is body-only, so it never becomes a parameter, so no occurrence can choose one and the pair has no surface spelling. The entry records that nothing in the corpus spells it; what this pins is the stronger claim that nothing *can*, which is the half that does not decay when the corpus changes.
//
// The counts are asserted rather than a printout matched, because the parameter count is the thing the argument turns on and a printer is free to render it differently.
#[test]
fn a_body_carried_level_is_minimized_rather_than_generalized() {
    let source = r#"
        use /std/{Nat, Str, Handle};
        induct Box : pub Type
        | wrap(Type)
        end
        let carrier : Box = Box/wrap(Type);
        let holder(@A : Type) -> Box = Box/wrap(A);
        /std/print("both")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/carrier"),
        Some(&0),
        "a body-carried level became a parameter a use site can choose: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/holder"),
        Some(&1),
        "an interface level stopped generalizing, so the control no longer separates the two: {parameters:?}",
    );
}

// A `match` arm is checked at the motive opened on the constructor value the scrutinee is refined to, and that value is what a metavariable in the arm's expected type gets solved to — here `Eq/refl()`'s `@z`, against `Eq(len(xs), len(xs))` with `xs := L/cons(x, rest)`. The family is universe-polymorphic through its `A: Type`, so the occurrence needs its level instance; built without one, it zonked into the definition, where the elaborator's own arity check refused a program that is plainly well-typed. Both arms are `Eq/refl()` on purpose: no `rec`, no `Eq/cong`, nothing but the refinement itself.
#[test]
fn a_refined_scrutinee_carries_the_family_universe_levels() {
    let source = r#"
        use /std/{Nat, Eq};
        induct L(A: Type): pub Type
        | nil()
        | cons(A, L(A))
        end
        rec len(xs: L(Nat)) -> Nat =
            match xs
            | nil() => 0
            | cons(_, rest) => len(rest) + 1
            end;
        let len_self(xs: L(Nat)) -> Eq(len(xs), len(xs)) =
            match xs
            | nil() => Eq/refl()
            | cons(x, rest) => Eq/refl()
            end;
        /std/print(Nat/to_str(len(L/cons(1, L/cons(2, L/nil())))))
        "#;

    assert_eq!(run(source), b"2");
}

// The same defect over a prelude family, where the detector differs: the elaborator's arity check knows only the arities of the module's own inductives, so the level-less `Vec/cons` passed it and the kernel refused the definition instead. Keeping both fixtures pins that the fix reaches the prelude's families and not only the module's.
#[test]
fn a_refined_prelude_scrutinee_carries_the_family_universe_levels() {
    let source = r#"
        use /std/{Nat, Eq, Vec};
        rec count(@A: Type, @n: Nat, xs: Vec(A, n)) -> Nat =
            match xs
            | nil() => 0
            | cons(@m, _, rest) => count(rest) + 1
            end;
        rec count_is_n(@A: Type, @n: Nat, xs: Vec(A, n)) -> Eq(count(xs), n) =
            match xs
            | nil() => Eq/refl()
            | cons(@m, x, rest) => Eq/cong((k) => k + 1, count_is_n(rest))
            end;
        /std/print(Nat/to_str(count(Vec/cons(1, Vec/cons(2, Vec/nil())))))
        "#;

    assert_eq!(run(source), b"2");
}

// A level that occurs only in a declaration's result sort is one no use site can choose, and `finalize_definition` minimizes it rather than minting a parameter for it. The group path generalizes the same signatures, so the same rule must hold there: a `rec` returning a type, and a `rec` proof whose family level sits only in its result, take no more parameters than the `let` beside them. Without this a definition's scheme depended on which path elaborated it — a fact no reader can see once a group is decided by whether a body names itself.
#[test]
fn a_rec_result_sort_level_is_minimized_like_a_let_s() {
    let source = r#"
        use /std/{Nat, Eq};
        induct N : pub Type | z() | s(N) end
        let f(n: N) -> Type = N;
        rec g(n: N) -> Type =
            match n
            | z() => N
            | s(m) => g(m)
            end;
        rec count(n: N) -> Nat =
            match n
            | z() => 0
            | s(m) => count(m) + 1
            end;
        rec count_self(n: N) -> Eq(count(n), count(n)) =
            match n
            | z() => Eq/refl()
            | s(m) => Eq/refl()
            end;
        /std/print("same")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/f"),
        Some(&0),
        "the let's result-sort level became a parameter: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/g"),
        Some(&0),
        "the rec's result-sort level was generalized where the let's was minimized: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/count_self"),
        Some(&0),
        "the rec proof's family level was generalized: {parameters:?}",
    );
}
