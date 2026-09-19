//! End-to-end coverage for the implicit cumulative universe hierarchy.
//!
//! The hierarchy exists to remove one source of unsoundness: under `Type : Type` a type could classify itself, which admits Girard's paradox. The solver's own rules are unit-tested in `curios-analysis/src/satisfy/tests.rs`; these check what a *user* can observe.
//!
//! A surface program reaches `UniverseInconsistency`, and three fixtures hold where. What a program cannot do is force two *occurrences of a name* to share a level: a declaration generalizes over the levels its *interface* carries, so a self-reference like `Box/wrap(Box)` instantiates `Box` at two different levels and is admitted — correctly, since that is stratification working — and `syntax.md` gives no syntax for universe variables or explicit arguments. A bound variable and the type that binds it are another matter: they share a level by construction, and a level a declaration carries only in its body is minimized rather than generalized, so `let U: Type = (X: Type, …) -> …` fixes `X` one level below `U`, and instantiating `X` at `U` itself asks for a level strictly below itself. `a_type_quantifying_over_types_cannot_be_instantiated_at_itself` is that refusal alone, `the_self_application_girards_paradox_needs_is_refused` is the paradox's own shape, refused at the same instantiation, and `the_same_quantifier_instantiates_at_a_type_below_it` is the control. Stratification itself is covered at unit level by `a_polymorphic_definition_instantiates_at_prop_and_type`, which pins `id(Prop)` to level 1 and `id(Type)` to level 2.
//!
//! "Interface" is load-bearing in that sentence and is what `a_body_carried_level_is_minimized_rather_than_generalized` pins: the levels reachable only through a body are *minimized* instead, so the set of declarations a use site can instantiate at two levels is narrower than "every declaration".

use {
    super::{error, run},
    curios_core::Module,
    curios_pipeline::{DEFAULT_STEP_BUDGET, typecheck_with_prelude},
    curios_text::{Entrypoint, RootSource},
    std::collections::BTreeMap,
};

/// A signature that instantiates `/std/List/zip` twice: the declared result type spells the global at the levels of `A` and `B`, while `zip_len`'s instantiated result — captured into `Eq/trans`'s solved middle term — carries an inlined copy of the same group at the levels of `a`'s and `b`'s `List` types, related to the former only by cumulativity. `same` is an identity whose universe instance is what pulls the two spellings apart; with `a` written in its place the program takes half a second.
const TWO_INSTANCES_OF_ZIP: &str = r#"
    use /std/{Nat, List, Eq, Io, Bool};
    let same(@T: Type, l: List(T)) -> List(T) = l;
    let zero_not_succ(@n: Nat, e: Eq(0, n + 1)) -> Bool/False =
        Eq/subst((m: Nat) => Bool/Holds(Nat/eql(0, m)), e, Bool/True/qed());
    let succ_cancel(@a: Nat, @b: Nat, e: Eq(a + 1, b + 1)) -> Eq(a, b) =
        Eq/cong((w: Nat) => w - 1, e);
    pub let zip_len(@A: Type, @B: Type, a: List(A), b: List(B), p: Eq(List/len(b), List/len(a)))
        -> Eq(List/len(List/zip(a, b)), List/len(a)) =
        (match a: (l) =>
                (c: List(B), q: Eq(List/len(c), List/len(l))) -> Eq(List/len(List/zip(l, c)), List/len(l))
        | [] => (_, _) => Eq/refl()
        | [x, ..xs]; ih =>
            (c, q) =>
                (match c: (d) =>
                        (Eq(List/len(d), List/len(xs) + 1))
                            -> Eq(List/len(List/zip([x, ..xs], d)), List/len(xs) + 1)
                | [] => (r) => match zero_not_succ(r) end
                | [_, ..ys] => (r) => Eq/cong((y: Nat) => y + 1, ih(ys, succ_cancel(r)))
                end)(q)
        end)(b, p);
    pub let use_call(@A: Type, @B: Type, n: Nat, a: List(A), b: List(B),
                     ca: Eq(List/len(same(a)), n), cb: Eq(List/len(b), n))
        -> Eq(List/len(List/zip(same(a), b)), n) =
        Eq/trans(zip_len(same(a), b, Eq/trans(cb, Eq/sym(ca))), ca);
    Io/pure(())
    "#;

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
        use /std/{Str};
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
        use /std/{Nat, Str};
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
        use /std/{Nat, Str};
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

/// A type that quantifies over a type and answers a double powerset of it — the carrier Hurkens' form of Girard's paradox is stated over — with `tau`, the half of the paradox that stratifies. Both of `U`'s levels are carried by its body alone, so they are minimized as `a_body_carried_level_is_minimized_rather_than_generalized` pins: `X` ranges over level 0 and `U` sits at level 1.
const A_TYPE_QUANTIFYING_OVER_TYPES: &str = r#"
    let Pow(A: Type) -> Type = (A) -> Type;
    let U: Type = (X: Type, f: (Pow(Pow(X))) -> X) -> Pow(Pow(X));
    let tau(t: Pow(Pow(U))) -> U = (X, f) => (p) => t((x) => p(f(x(X, f))));
    "#;

// The refusal with nothing beside it: `s(U, f)` instantiates `U`'s bound `X` at `U`, which asks `U`, at level 1, to be a type of level 0. The diagnostic names the rule and the declaration, since a fixture that accepted any error would pass on a program broken some other way — and `tau`, which mentions `U` as often, is not where it is refused.
#[test]
fn a_type_quantifying_over_types_cannot_be_instantiated_at_itself() {
    let source = format!(
        r#"{A_TYPE_QUANTIFYING_OVER_TYPES}
        let at_itself(s: U, f: (Pow(Pow(U))) -> U) -> Pow(Pow(U)) = s(U, f);
        /std/print("no")
        "#
    );

    let message = error(&source);
    assert!(message.contains("strictly below itself"), "got: {message}");
    assert!(message.contains("/at_itself"), "got: {message}");
}

// The paradox's own shape: `sigma` is the self-application Girard's paradox needs, and it is refused at the same instantiation for the same reason. The hierarchy does not make this unspellable; it makes it ill-typed.
#[test]
fn the_self_application_girards_paradox_needs_is_refused() {
    let source = format!(
        r#"{A_TYPE_QUANTIFYING_OVER_TYPES}
        let sigma(s: U) -> Pow(Pow(U)) = s(U, tau);
        /std/print("no")
        "#
    );

    let message = error(&source);
    assert!(message.contains("strictly below itself"), "got: {message}");
    assert!(message.contains("/sigma"), "got: {message}");
}

// The control, differing in the last declaration alone: the same `s` instantiated at a type its caller supplies. `A`'s level is held to the one `X` ranges over, which is satisfiable — what the refusals above rest on is the self-instantiation, not the quantifier.
#[test]
fn the_same_quantifier_instantiates_at_a_type_below_it() {
    let source = format!(
        r#"{A_TYPE_QUANTIFYING_OVER_TYPES}
        let sigma_at(s: U, A: Type, f: (Pow(Pow(A))) -> A) -> Pow(Pow(A)) = s(A, f);
        /std/print("stratified")
        "#
    );

    assert_eq!(run(&source), b"stratified");
}

// A `match` arm is checked at the motive opened on the constructor value the scrutinee is refined to, and that value is what a metavariable in the arm's expected type gets solved to — here `Eq/refl()`'s `@z`, against `Eq(len(xs), len(xs))` with `xs := L/cons(x, rest)`. The family is universe-polymorphic through its `A: Type`, so the occurrence needs its level instance; built without one, it zonked into the definition, where the elaborator's own arity check refused a program that is plainly well-typed. Both arms are `Eq/refl()` on purpose: no `rec`, no `Eq/cong`, nothing but the refinement itself. A twin fixture over a *prelude* family stood beside this one, where the detector differs — the elaborator's arity check knows only the module's own inductives, so a level-less prelude constructor passed it and the kernel refused the definition instead. It was written over `/std/Vec` when `Vec` was an indexed inductive; `/std` now has no `Type`-valued universe-polymorphic indexed family to state it over, and the prelude rung is held by the build rather than by a fixture: `/std/Eq`'s own `sym`, `trans`, `cong` and `subst` each match on that universe-polymorphic family and answer with `Eq/refl()` in the arm, and `cargo x clippy` certifies every `/std` module with the kernel on each build.
#[test]
fn a_refined_scrutinee_carries_the_family_universe_levels() {
    let source = r#"
        use /std/{Nat, Eq};
        induct L(A: Type): pub Type
        | nil()
        | cons(A, L(A))
        end
        let len(xs: L(Nat)) -> Nat =
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

// `use_call`'s two spellings of `zip` were two instances of one recursive group related only by `u ≤ x1`, `v ≤ z1` while the elaborator assumed their recurrence: the kernel refused the pair, and before it decided such a pair by its levels it unfolded them against each other until the host died. The elaborator now identifies the two instances where they meet, so the declared type's `zip` is spelled at the list levels the body already carries and the kernel accepts the program by identity. Nothing merges: the signature keeps every universe parameter it had, which is what the count pins — the identification chose one spelling for one occurrence rather than making two parameters one.
#[test]
fn a_signature_instantiating_one_recursive_definition_twice_certifies_with_its_levels_identified() {
    super::typecheck_within(DEFAULT_STEP_BUDGET, TWO_INSTANCES_OF_ZIP)
        .expect("both checkers accept the program");

    let parameters = universe_parameters(TWO_INSTANCES_OF_ZIP);
    assert_eq!(
        parameters.get("/use_call"),
        Some(&22),
        "identifying the two spellings changed how polymorphic the signature is: {parameters:?}",
    );
}

// A level that occurs only in a declaration's result sort is one no use site can choose, and `finalize_definition` minimizes it rather than minting a parameter for it. The group path generalizes the same signatures, so the same rule must hold there: a `rec` returning a type, and a `rec` proof whose family level sits only in its result, take no more parameters than the `let` beside them. Without this a definition's scheme depended on which path elaborated it — a fact no reader can see once a group is decided by whether a body names itself.
#[test]
fn a_rec_result_sort_level_is_minimized_like_a_let_s() {
    let source = r#"
        use /std/{Nat, Eq};
        induct N : pub Type | z() | s(N) end
        let f(n: N) -> Type = N;
        let g(n: N) -> Type =
            match n
            | z() => N
            | s(m) => g(m)
            end;
        let count(n: N) -> Nat =
            match n
            | z() => 0
            | s(m) => count(m) + 1
            end;
        let count_self(n: N) -> Eq(count(n), count(n)) =
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
        "the recursive proof's family level was generalized: {parameters:?}",
    );
}
