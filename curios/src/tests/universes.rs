//! End-to-end coverage for the implicit cumulative universe hierarchy.
//!
//! The hierarchy exists to remove one source of unsoundness: under `Type : Type` a type could classify itself, which admits Girard's paradox. The solver's own rules are unit-tested in `curios-elab/src/universe_solver/tests.rs`, and the kernel's satisfiability in `curios-cert/src/satisfy/tests.rs`; these check what a *user* can observe.
//!
//! A surface program reaches `UniverseInconsistency`, and three fixtures hold where. What a program cannot do is force two *occurrences of a name* to share a level: a declaration generalizes over the levels its *interface* carries, so a self-reference like `Box/wrap(Box)` instantiates `Box` at two different levels and is admitted — correctly, since that is stratification working — and `syntax.md` gives no syntax for universe variables or explicit arguments. A bound variable and the type that binds it are another matter: they share a level by construction, and a level a declaration carries only in its body is minimized rather than generalized, so `let U: Type = (X: Type, …) -> …` fixes `X` one level below `U`, and instantiating `X` at `U` itself asks for a level strictly below itself. `a_type_quantifying_over_types_cannot_be_instantiated_at_itself` is that refusal alone, `the_self_application_girards_paradox_needs_is_refused` is the paradox's own shape, refused at the same instantiation, and `the_same_quantifier_instantiates_at_a_type_below_it` is the control. Stratification itself is covered at unit level by `a_polymorphic_definition_instantiates_at_prop_and_type`, which pins `id(Prop)` to level 1 and `id(Type)` to level 2.
//!
//! **Nor need the program be a paradox.** Two more shapes reach the same refusal from ordinary code, because generalization is a privilege of the top level and a group is monomorphic in its own levels: a *local* definition applied to itself, and a member of a recursive group called at a level above the group's. The last two fixtures here are those, each with the control admitted once the definition is hoisted or the group split, and what they assert is the *advice* — levels have no syntax, so a user who meets this has nothing to annotate and needs the message to say what to write instead.
//!
//! "Interface" is load-bearing in that sentence and is what `a_body_carried_level_is_minimized_rather_than_generalized` pins: the levels reachable only through a body are *minimized* instead, so the set of declarations a use site can instantiate at two levels is narrower than "every declaration".

use {
    super::{error, run, typecheck},
    curios_core::Program,
    curios_pipeline::{DEFAULT_STEP_BUDGET, typecheck_with_prelude},
    curios_text::{Entrypoint, RootSource},
    std::collections::BTreeMap,
};

/// A signature that instantiates `/std/List/zip` twice: the declared result type spells the global at the levels of `A` and `B`, while `zip_len`'s instantiated result — captured into `Eq/trans`'s solved middle term — carries an inlined copy of the same group at the levels of `a`'s and `b`'s `List` types, related to the former only by cumulativity. `same` is an identity whose universe instance is what pulls the two spellings apart.
const TWO_INSTANCES_OF_ZIP: &str = r#"
    use /std/{Nat, List, Eq, Io};
    use /std/Bool/{True, False, Holds};
    let same(@T: Type, l: List(T)) -> List(T) = l;
    let zero_not_succ(@n: Nat, e: Eq()(0, n + 1)) -> False =
        Eq/subst((m: Nat) => Holds(Nat/eql(0, m)), e, True/qed());
    let succ_cancel(@a: Nat, @b: Nat, e: Eq()(a + 1, b + 1)) -> Eq()(a, b) =
        Eq/cong((w: Nat) => w - 1, e);
    pub let zip_len(@A: Type, @B: Type, a: List(A), b: List(B), p: Eq()(List/len(b), List/len(a)))
        -> Eq()(List/len(List/zip(a, b)), List/len(a)) =
        (match a: (l) =>
                (c: List(B), q: Eq()(List/len(c), List/len(l))) -> Eq()(List/len(List/zip(l, c)), List/len(l))
        | [] => (_, _) => Eq/refl()
        | [x, ..xs]; ih =>
            (c, q) =>
                (match c: (d) =>
                        (Eq()(List/len(d), List/len(xs) + 1))
                            -> Eq()(List/len(List/zip([x, ..xs], d)), List/len(xs) + 1)
                | [] => (r) => match zero_not_succ(r) end
                | [_, ..ys] => (r) => Eq/cong((y: Nat) => y + 1, ih(ys, succ_cancel(r)))
                end)(q)
        end)(b, p);
    pub let use_call(@A: Type, @B: Type, n: Nat, a: List(A), b: List(B),
                     ca: Eq()(List/len(same(a)), n), cb: Eq()(List/len(b), n))
        -> Eq()(List/len(List/zip(same(a), b)), n) =
        Eq/trans(zip_len(same(a), b, Eq/trans(cb, Eq/sym(ca))), ca);
    Io/pure(())
    "#;

/// Every definition's finalized universe parameter count, keyed by the name its item describes itself with.
fn universe_parameters(source: &str) -> BTreeMap<String, usize> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fixture type-checks")
            .program;

    program
        .module
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
// That asymmetry is what shuts [Case equations and their key](../../../documentation/design/soundness/elimination/case-equations-and-their-key.md)'s still-open elaborator-side copy against *source*. The defect needs two occurrences of one definition at differing instances whose values differ by level, and only a level carried into a payload can make a value differ — `Type u` embedded in a term is the entry's own counterexample. Such a level is body-only, so it never becomes a parameter, so no occurrence can choose one and the pair has no surface spelling. The entry records that nothing in the corpus spells it; what this pins is the stronger claim that nothing *can*, which is the half that does not decay when the corpus changes.
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

// **A concept's method level is its family's domain.** `ret`'s `@A: Type` is bounded only by the domain of `M`, and cumulativity already lets a caller pass anything smaller, so it is that domain: `Mon` takes its family's domain and codomain and no parameter per method. Generalized apart, a method level is one an implementation may answer at below its family's domain, which would leave `Result`'s `Monad` witness taking `Type 0` payloads only. `Two` is the control: its `pick` is bounded by two families' domains, which name no one level to be, so it keeps a level of its own.
#[test]
fn a_method_level_bounded_by_one_domain_is_that_domain() {
    let source = r#"
        concept Mon(M: (Type) -> Type): pub Type {
            ret(@A: Type, A) -> M(A),
        }
        concept Two(M: (Type) -> Type, N: (Type) -> Type): pub Type {
            pick(@A: Type, M(A)) -> N(A),
        }
        /std/print("tied")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/Mon"),
        Some(&2),
        "a method level bounded by its family's domain alone was generalized apart from it: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/Two"),
        Some(&5),
        "a method level bounded by two domains was identified with one of them: {parameters:?}",
    );
}

// **A witness leaves its method levels to its callers.** `Mon`'s `ret` quantifies `@A: Type` bounded above by the domain of the family `M`, and that level is the family's domain (`a_method_level_bounded_by_one_domain_is_that_domain`), which the witness's `(A: Type) => Box(A)` leaves to its caller. At a level of its own, the witness's goal would name it only as an argument of its occurrence of `Mon`, where it counts as a result sort and goes to zero, so `ret` through the witness would take `Type 0` payloads only and refuse `Mon/ret(Nat)` at `Box(Type)` as a `Type` strictly below itself. `small` is the control: the same witness at a small payload.
#[test]
fn a_witness_leaves_its_method_levels_to_its_callers() {
    let source = r#"
        use /std/{Nat};
        concept Mon(M: (Type) -> Type): pub Type {
            ret(@A: Type, A) -> M(A),
        }
        induct Box(A: Type): pub Type
        | box(A)
        end
        satisfy Mon((A: Type) => Box(A)) {
            ret(@_, a) = Box/box(a),
        }
        let small: Box(Nat) = Mon/ret(3);
        let large: Box(Type) = Mon/ret(Nat);
        match large | box(_) => /std/print("large") end
        "#;

    assert_eq!(run(source), b"large");
}

// **An occurrence sits where its argument does.** `List(Nat)` checks `Nat : Type 0` against the level its occurrence of `List` mints, which records nothing a store keeps; generalized as interface where it stands in a signature, that level would give `pair_of` a parameter per occurrence, each above the `Type 0` both checkers size the enclosing tuple and `Io` by — a tuple type is sized from its parts' reducts, where the occurrence is gone. `UniverseSolver::finalize` settles an occurrence's level at its recorded floor instead, which is its argument's level, so the written type sits where its reduct does. `Tree` holds a list of itself and settles the same way rather than generalizing the list's level above the family's own. `keep` is the control: its `A` is a binder a caller chooses, and the occurrence of `List` over it settles at `A`'s level, which stays the one parameter.
#[test]
fn an_occurrence_level_settles_at_its_argument_rather_than_generalizing() {
    let source = r#"
        use /std/{List, Nat, Io};
        induct Tree : pub Type
        | leaf(Nat)
        | node(List(Tree))
        end
        let pair_of(x: List(Nat)) -> Io({List(Nat), List(Nat)}) = Io/pure((x, x));
        let keep(@A: Type, x: List(A)) -> List(A) = x;
        /std/print("settled")
        "#;

    let parameters = universe_parameters(source);

    assert_eq!(
        parameters.get("/pair_of"),
        Some(&0),
        "an occurrence's level was generalized above its argument's: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/Tree"),
        Some(&0),
        "a family's occurrence of a former over itself was generalized: {parameters:?}",
    );
    assert_eq!(
        parameters.get("/keep"),
        Some(&1),
        "a level a caller chooses stopped generalizing, so the control no longer separates the two: {parameters:?}",
    );
}

// **A goal deferred past its declaration settles at its least levels.** `rewrap`'s `!` asks for `Monad(Box)` before the unit has registered one, so the goal defers and is retried only after `rewrap`'s scheme has closed, where the witness it finds can be pinned to a level the goal already fixes and a constraint the witness brings reaches no scheme at all. `UniverseSolver::finalize` therefore makes a deferred goal ground — the levels it names settle, and so does every level a settlement lands on, `A`'s included — so a witness declared after its use leaves `rewrap` at `Type 0`. Left at `A`'s level instead, `/std/tcp/Socket/close_raising` would offer its `A` at every level while the `Lift(Io, Io)` its `!` resolves later exists at one, and the kernel would refuse it. The control declares the witness first: the goal resolves while `rewrap`'s levels are open, and `A` stays the caller's. Ordering an item after the witnesses its `!` dispatches through would make the two agree.
#[test]
fn a_goal_deferred_past_its_declaration_settles_at_its_least_levels() {
    let rewrap = "let rewrap(@A: Type, b: Box(A)) -> Box(A) = let a = b!; Box { value = a };";
    let witness =
        "satisfy Monad(Box) { pure(@_, a) = Box { value = a }, bind(@_, @_, m, f) = f(m.value) }";
    let program = |first: &str, second: &str| {
        format!(
            r#"
            use /std/{{Monad}};
            pub struct Box(A: Type): pub Type {{ value: A }}
            {first}
            {second}
            /std/print("settled")
            "#
        )
    };

    let deferred = universe_parameters(&program(rewrap, witness));
    let resolved = universe_parameters(&program(witness, rewrap));

    assert_eq!(
        deferred.get("/rewrap"),
        Some(&0),
        "a goal deferred past its declaration kept a level its later witness can constrain: {deferred:?}",
    );
    assert_eq!(
        resolved.get("/rewrap"),
        Some(&1),
        "a goal resolved in time no longer leaves the caller's level free, so the control no longer separates the two: {resolved:?}",
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

// A `match` arm is checked at the motive opened on the constructor value the scrutinee is refined to, and that value is what a metavariable in the arm's expected type gets solved to — here `Eq/refl()`'s `@z`, against `Eq()(len(xs), len(xs))` with `xs := L/cons(x, rest)`. The family is universe-polymorphic through its `A: Type`, so the occurrence needs its level instance; built without one, it would zonk into the definition, where the elaborator's own arity check would refuse a program that is plainly well-typed. Both arms are `Eq/refl()` on purpose: no `rec`, no `Eq/cong`, nothing but the refinement itself. Over a *prelude* family the detector differs — the elaborator's arity check knows only the module's own inductives, so a level-less prelude constructor would pass it and the kernel would refuse the definition instead — and `/std` has no `Type`-valued universe-polymorphic indexed family to state a fixture over, so that rung is held by the build: `/std/Eq`'s own `sym`, `trans`, `cong` and `subst` each match on that universe-polymorphic family and answer with `Eq/refl()` in the arm, and `cargo xtask clippy` certifies every `/std` module with the kernel on each build.
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
        let len_self(xs: L(Nat)) -> Eq()(len(xs), len(xs)) =
            match xs
            | nil() => Eq/refl()
            | cons(x, rest) => Eq/refl()
            end;
        /std/print(Nat/to_str(len(L/cons(1, L/cons(2, L/nil())))))
        "#;

    assert_eq!(run(source), b"2");
}

// A solution committed while a recursive group elaborates is stamped with the group's instance when the group generalizes, as its signatures and registry entries are. `Eq()(a, a)` and `same2()(a, a)` in a constructor's payload solve the hidden `@A` to the family being declared, spelled from before it had an instance; unstamped, the recorded type of the call filling the hidden list would keep that spelling past generalization, where the proof-totality check refuses an instance with no levels. The written `@Wit` is the control, stamped with the payload it sits in.
#[test]
fn a_solution_naming_its_own_group_is_stamped_with_the_groups_instance() {
    for payload in ["Eq()(a, a)", "same2()(a, a)", "Eq(@Wit)(a, a)"] {
        let source = format!(
            r#"
            use /std/{{Eq}};
            pub let same2(@A : Type) -> (x : A, y : A) -> Prop = (x, y) => Eq(@A)(x, y);
            induct Wit : pub Type
            | base()
            | tied(a : Wit, p : {payload})
            end
            /std/print("declared")
            "#
        );
        assert_eq!(run(&source), b"declared", "{payload}");
    }
}

// `use_call`'s two spellings of `zip` are two instances of one recursive group, related only by `u ≤ x1`, `v ≤ z1` if left apart, which the kernel would refuse. The elaborator identifies the two instances where they meet, so the declared type's `zip` is spelled at the list levels the body already carries and the kernel accepts the program by identity. Nothing merges: the signature keeps the two levels its writer chose, `A`'s and `B`'s, which is what the count pins — the identification chose one spelling for one occurrence rather than making two parameters one. Every other level is an occurrence's, and finalization settles each at the level its argument or its floor gives it (`UniverseSolver::finalize`): `List(A)` at `A`'s, `Eq()(List/len(b), n)` at zero, which is why the count is two.
#[test]
fn a_signature_instantiating_one_recursive_definition_twice_certifies_with_its_levels_identified() {
    super::typecheck_within(DEFAULT_STEP_BUDGET, TWO_INSTANCES_OF_ZIP)
        .expect("both checkers accept the program");

    let parameters = universe_parameters(TWO_INSTANCES_OF_ZIP);
    assert_eq!(
        parameters.get("/use_call"),
        Some(&2),
        "identifying the two spellings changed how polymorphic the signature is: {parameters:?}",
    );
}

// A level that occurs only in a declaration's result sort is one no use site can choose, and `finalize_definition` minimizes it rather than minting a parameter for it. The group path generalizes the same signatures, so the same rule must hold there: a `rec` returning a type, and a `rec` proof whose family level sits only in its result, take no more parameters than the `let` beside them. Without this a definition's scheme would depend on which path elaborated it — a fact no reader can see once a group is decided by whether a body names itself.
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
        let count_self(n: N) -> Eq()(count(n), count(n)) =
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

// **An honest program that reaches the refusal, and what the message owes it.** Two surface programs do: a *local* polymorphic definition applied to itself, and a recursive call at a level above its group's own. Neither is a paradox — the top-level twin of the first is admitted below, and stratification is what refuses them — so each is a program a user can write by accident and has no way to annotate out of, levels having no syntax.
//
// What these assert is therefore the *advice* rather than the refusal alone. A raw constraint such as `?u784+1 ≤ ?u784` gives a reader nothing to do, so the message carries the span the constraint came from, a level numbering local to the message, and the two facts that decide what to write instead. The raw metavariable id counts every level the unit has invented and moves with an edit anywhere, which is why the numbering exists and why a fixture may assert on it at all.
#[test]
fn a_local_polymorphic_definition_applied_to_itself_is_refused_with_its_remedy() {
    let source = r#"
        use /std/{Nat};

        let self_applied: (@A: Type, x: A) -> A =
            let id(@A: Type, x: A) -> A = x;
            id(id);

        /std/print(Nat/to_str(self_applied(1)))
        "#;

    let message = error(source);
    assert!(
        message.contains("this Type would need to be strictly below itself"),
        "{message}"
    );
    assert!(
        message.contains("required constraint: ?u1+1 ≤ ?u1"),
        "the message numbers its levels within itself:\n{message}"
    );
    assert!(
        message.contains("has to be hoisted"),
        "the message says what to write instead:\n{message}"
    );
}

// The control, and the reason the refusal above is about *locality* rather than about self-application: hoisted to the top level, `id` generalizes over the level its interface carries, the two occurrences instantiate it at their own, and the program runs.
#[test]
fn the_same_definition_at_the_top_level_is_admitted() {
    let source = r#"
        use /std/{Nat};

        let id(@A: Type, x: A) -> A = x;
        let self_applied: (@A: Type, x: A) -> A = id(id);

        /std/print(Nat/to_str(self_applied(1)))
        "#;

    assert_eq!(run(source), b"1");
}

// The second shape: a recursive call at a level above its group's own. `depth` calls itself at `A := Type`, and a group is monomorphic in its universes, so the call needs the group's own level strictly above itself. The remedy clause names that case too — there is no annotation, and what the sibling needs has to be a declaration apart from the one needing it.
#[test]
fn a_recursive_call_a_level_above_its_group_is_refused_with_its_remedy() {
    let source = r#"
        use /std/{Nat};

        let depth(@A: Type, n: Nat, x: A) -> Nat =
            match n | 0 => 0 | k + 1 => depth(@Type, k, A) + 1 end;

        /std/print(Nat/to_str(depth(0, 1)))
        "#;

    let message = error(source);
    assert!(
        message.contains("this Type would need to be strictly below itself"),
        "{message}"
    );
    assert!(
        message.contains("monomorphic in its own levels"),
        "the message names the group rule:\n{message}"
    );
}

// **A group's own levels are constrained by its own recursion, and the kernel has to read that.** A member used at a type one level up — `pick(@Type, …)` for `pick(@A: Type, …)` — needs `1 ≤ u` of the group's instance, a group being monomorphic in its universes. The elaborator records exactly that in the scheme, and the kernel's entailment reaches the hypotheses before it decides a level's constant part structurally, since a parameter bounds no constant until something assumes it does.
//
// Both spellings are here because the two-member group is incidental: the same demand raised inside one self-recursive member refuses identically, so what the rule turns on is the group instance rather than the sibling.
#[test]
fn a_group_member_used_a_level_above_its_own_certifies() {
    let source = r#"
        use /std/{Nat};

        let pick(@A: Type, x: A) -> Nat = 0
        and other(n: Nat) -> Nat = pick(Nat) + n;

        /std/print(Nat/to_str(other(1)))
        "#;

    assert_eq!(run(source), b"1");
}

#[test]
fn a_self_recursive_call_a_level_above_its_own_certifies() {
    let source = r#"
        use /std/{Nat};

        let depth(@A: Type, n: Nat, x: A) -> Nat =
            match n | 0 => 0 | k + 1 => depth(@Type, k, Nat) end;

        /std/print(Nat/to_str(depth(0, 1)))
        "#;

    assert_eq!(run(source), b"0");
}

// **A type's spelling carries a universe its value need not.** `Pick(true)` is `Nat`, in `Type 0`, but a call to `Pick` is typed by `Pick`'s codomain, `Type 1`, which the other arm's quantifier over `Type` needs. `three`'s type is that call, so `same`'s `@A` meets it as written; solved by that spelling, `A` would sit in `Type 1`, where `Nat`'s `Eql` witness, at `Type 0`, then asks it to be strictly below itself. The solver commits the reduct instead, which names the type at its own universe.
#[test]
fn a_type_named_through_a_higher_universe_solves_at_its_own() {
    let source = r#"
        use /std/{Bool, Nat};
        use /std/ops/{Eql};

        let Pick(b: Bool) -> Type =
            match b
            | true => Nat
            | false => (A: Type) -> A
            end;
        let same(@A: Type, use Eql(A), x: A, y: A) -> Bool = x == y;
        let three: Pick(true) = 3;

        /std/print(Bool/to_str(same(three, 3)))
        "#;

    assert_eq!(run(source), b"true");
}

// **A bare former passed as a family is held at one instance.** `through(Io, …)` checks both arguments against `Io` at the instance it was passed at, and the refusal reads as that instance being committed before `Io` unfolds to a former that carries no level. Eta-expanded, `F(Nat)` is a redex and both sides are reduced, so the same program is accepted. `/sys/List` and an alias of `Io` are refused alike.
#[test]
fn a_bare_former_passed_as_a_family_is_held_at_one_instance() {
    let through = |declarations: &str, family: &str, small: &str, large: &str| {
        format!(
            r#"
            use /std/{{Nat, Io, List}};
            {declarations}
            let through(F: (Type) -> Type, x: F(Nat), y: F(Type)) -> Nat = 0;
            let probe(n: Nat) -> Nat = through({family}, {small}, {large});
            /std/print("held")
            "#
        )
    };
    let io = "let small(n: Nat) -> Io(Nat) = Io/pure(n);";
    let list = "let small(n: Nat) -> List(Nat) = [n];";
    let alias = "let Act(A: Type) -> Type = Io(A);
        let small(n: Nat) -> Act(Nat) = Io/pure(n);
        let large(n: Nat) -> Act(Type) = Io/pure(Nat);";

    for bare in [
        through(io, "Io", "small(n)", "Io/pure(Nat)"),
        through(list, "List", "small(n)", "[Nat]"),
        through(alias, "Act", "small(n)", "large(n)"),
    ] {
        let message = typecheck(&bare).expect_err("a bare former is held at one instance");
        assert!(message.contains("strictly below itself"), "got: {message}");
    }

    for expanded in [
        through(io, "(A) => Io(A)", "small(n)", "Io/pure(Nat)"),
        through(list, "(A) => List(A)", "small(n)", "[Nat]"),
    ] {
        assert_eq!(typecheck(&expanded), Ok(()));
    }
}
