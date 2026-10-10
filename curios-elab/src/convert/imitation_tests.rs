//! Imitation against an inductive, a struct and an intrinsic former, the arities that block it, and abstraction over the bound variables a metavariable is applied to.

use {
    super::test_support::*,
    crate::*,
    curios_core::{
        InductDecl, Intrinsic, Level, MetavarId, MetavarOrigin, StructDecl, Telescope, Term,
        UniverseContext, UniverseParam, Variance,
    },
    curios_utilities::Qualifier,
};

// === Flex-apply imitation (higher-kinded metavariables) =====================

/// Register a `List`-shaped inductive: one parameter, no indices.
fn register_list(context: &mut Context) {
    let elem = context.fresh(Some("A"));
    context
        .register_induct(
            &nominal("List"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::build([(elem, Term::type_ground())], Telescope::done(())),
                constructors: Vec::new(),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();
}

/// Register a `Vec`-shaped inductive: one parameter, one `Nat` index.
fn register_vec(context: &mut Context) {
    let elem = context.fresh(Some("T"));
    let length = context.fresh(Some("n"));
    context
        .register_induct(
            &nominal("Vec"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::build(
                    [(elem, Term::type_ground())],
                    Telescope::build([(length, Term::intrinsic(Intrinsic::NatType))], ()),
                ),
                constructors: Vec::new(),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();
}

/// Register `induct Wrap.{u}(A: Type u): Type u`, carrying `variance` for its one level.
fn register_wrap(context: &mut Context, variance: Variance) {
    let elem = context.fresh(Some("A"));
    let sort = Term::type_at(Level::param(UniverseParam(0)));
    context
        .register_induct(
            &nominal("Wrap"),
            InductDecl {
                universe_context: UniverseContext {
                    parameter_count: 1,
                    quality_count: 0,
                    constraints: Vec::new(),
                },
                arity: Telescope::build([(elem, sort.clone())], Telescope::done(())),
                constructors: Vec::new(),
                result_sort: sort,
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: vec![variance],
            },
        )
        .unwrap();
}

/// The kind `(Type) -> Type`.
fn type_to_type(context: &mut Context) -> Term {
    Term::func_type(
        [(context.fresh(Some("A")), Term::type_ground())],
        Term::type_ground(),
    )
}

#[test]
fn solves_flex_apply_against_inductive() {
    let mut context = context();
    register_list(&mut context);
    let kind = type_to_type(&mut context);
    context.birth_metavar(MetavarId(0), Vec::new(), kind);

    // ?0(Nat) ≟ List(Nat)  — commits ?0 := λA. List(A).
    let flex = Term::apply(Term::hole(0), [nat_type()]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());

    // The committed solution is the imitation, not the constant: applied to a different argument it yields List of *that* argument.
    let at_bool = Term::apply(Term::hole(0), [Term::intrinsic(Intrinsic::BoolType)]);
    let list_bool = Term::induct_type(
        nominal("List"),
        [Term::intrinsic(Intrinsic::BoolType)],
        Vec::<Term>::new(),
    );
    assert_eq!(conv(&mut context, &at_bool, &list_bool), Ok(true));
}

#[test]
fn imitation_is_symmetric() {
    let mut context = context();
    register_list(&mut context);
    let kind = type_to_type(&mut context);
    context.birth_metavar(MetavarId(0), Vec::new(), kind);

    // Rigid on the left, stuck application on the right.
    let flex = Term::apply(Term::hole(0), [nat_type()]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &rigid, &flex), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());
}

#[test]
fn equates_arguments_pairwise() {
    let mut context = context();
    register_list(&mut context);
    let kind = type_to_type(&mut context);
    context.birth_metavar(MetavarId(0), Vec::new(), kind);
    context.birth_metavar(MetavarId(1), Vec::new(), Term::type_ground());

    // ?0(?1) ≟ List(Nat) — the imitation solves ?0, the pairwise equation ?1.
    let flex = Term::apply(Term::hole(0), [Term::hole(1)]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());
    assert_eq!(context.metavar_solution(MetavarId(1)), Some(&nat_type()));
}

#[test]
fn splits_params_and_indices() {
    let mut context = context();
    let elem = context.fresh(Some("T"));
    let n = context.fresh(Some("n"));
    register_vec(&mut context);
    context.birth_metavar(
        MetavarId(0),
        Vec::new(),
        Term::func_type(
            [
                (elem, Term::type_ground()),
                (n, Term::intrinsic(Intrinsic::NatType)),
            ],
            Term::type_ground(),
        ),
    );

    // ?0(Nat, 3) ≟ Vec(Nat, 3) — arity 2 = 1 param + 1 index; the candidate's body must mirror the rigid node's split or re-validation rejects it.
    let flex = Term::apply(Term::hole(0), [nat_type(), nat(3)]);
    let rigid = Term::induct_type(nominal("Vec"), [nat_type()], [nat(3)]);
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());

    let at_two = Term::apply(
        Term::hole(0),
        [Term::intrinsic(Intrinsic::BoolType), nat(2)],
    );
    let vec_two = Term::induct_type(
        nominal("Vec"),
        [Term::intrinsic(Intrinsic::BoolType)],
        [nat(2)],
    );
    assert_eq!(conv(&mut context, &at_two, &vec_two), Ok(true));
}

#[test]
fn solves_against_struct_type() {
    let mut context = context();
    let first = context.fresh(Some("A"));
    let second = context.fresh(Some("B"));
    context
        .register_struct(
            &nominal("Pair"),
            StructDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::build(
                    [(first, Term::type_ground()), (second, Term::type_ground())],
                    Telescope::done(()),
                ),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();
    context.birth_metavar(
        MetavarId(0),
        Vec::new(),
        Term::func_type(
            [(first, Term::type_ground()), (second, Term::type_ground())],
            Term::type_ground(),
        ),
    );

    let flex = Term::apply(Term::hole(0), [nat_type(), nat_type()]);
    let rigid = Term::struct_type(nominal("Pair"), [nat_type(), nat_type()]);
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());
}

#[test]
fn arity_mismatch_blocks() {
    let mut context = context();
    register_vec(&mut context);
    let kind = type_to_type(&mut context);
    context.birth_metavar(MetavarId(0), Vec::new(), kind);

    // ?0(Nat) ≟ Vec(Nat, 3) — apply arity 1 against constructor arity 2: the right-biased imitation abstracts the index, a split `?0`'s birth kind refuses, so the goal blocks (it is not provably unequal — a constant solution could exist).
    let flex = Term::apply(Term::hole(0), [nat_type()]);
    let rigid = Term::induct_type(nominal("Vec"), [nat_type()], [nat(3)]);
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &flex, &rigid);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn non_function_birth_type_blocks() {
    let mut context = context();
    register_list(&mut context);
    // ?0's frozen type is not a function type: no candidate can be built.
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    let flex = Term::apply(Term::hole(0), [nat_type()]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &flex, &rigid);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn leaves_rigid_apply_pairs_alone() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    register_list(&mut context);
    let kind = type_to_type(&mut context);
    context.assume(&f, &kind);

    // A *rigid* stuck application against a nominal type is not the imitation case: the guard falls back to the neutral path, which cannot equate them — a definite mismatch.
    let stuck = Term::apply(Term::free_var(&f), [nat_type()]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &stuck, &rigid), Ok(false));
}

#[test]
fn solves_flex_apply_against_intrinsic_former() {
    let mut context = context();
    let kind = type_to_type(&mut context);
    context.birth_metavar(MetavarId(0), Vec::new(), kind);

    // ?0(?1) ≟ List(Nat) — the imitation solves ?0 := λT. List(T), the pairwise equation ?1 := Nat. This is what pins `M := List` for `Monad(List)`.
    context.birth_metavar(MetavarId(1), Vec::new(), Term::type_ground());
    let flex = Term::apply(Term::hole(0), [Term::hole(1)]);
    let rigid = Term::intrinsic(Intrinsic::ListType(nat_type()));
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetavarId(0)).is_some());
    assert_eq!(context.metavar_solution(MetavarId(1)), Some(&nat_type()));
}

// An imitated family is a fresh occurrence of it, paired with the rigid side's as two instances are. `?0 : (Type 1) -> Type 1` against `Wrap.{0}(Nat)`: the candidate `(A) => Wrap.{l}(A)` needs `l` at 1 for its binder. Where `Wrap` is irrelevant in the level, the pair with the rigid side's zero is only recorded, so the candidate fits and the declaration closes; where it is invariant the pair is an equation and the declaration is refused. Built at the rigid side's own instance, the candidate is refused in both.
#[test]
fn an_imitated_family_takes_an_instance_of_its_own() {
    for (variance, refused) in [(Variance::Irrelevant, false), (Variance::Invariant, true)] {
        let mut context = context();
        register_wrap(&mut context, variance);
        let large = Term::type_at(Level::constant(1));
        let kind = Term::func_type([(context.fresh(Some("A")), large.clone())], large.clone());
        context.birth_metavar(MetavarId(0), Vec::new(), kind);
        context.birth_metavar(MetavarId(1), Vec::new(), large);

        let flex = Term::apply(Term::hole(0), [Term::hole(1)]);
        let rigid = Term::induct_type_at(
            nominal("Wrap"),
            [Level::zero()],
            [nat_type()],
            Vec::<Term>::new(),
        );
        assert_eq!(conv(&mut context, &flex, &rigid), Ok(true), "{variance:?}");
        assert!(context.metavar_solution(MetavarId(0)).is_some());
        assert_eq!(
            context.universes_mut().finalize([], [], []).is_err(),
            refused,
            "{variance:?}"
        );
    }
}

// A family read off a rigid side whole, with no spine to imitate, is a fresh occurrence of it as an imitated one is. `?0 : (Type 1) -> Type 1` against `(A) => Wrap.{0}(A)`: committed as it was read, the candidate holds its binder to the one instance it was solved from, and is refused whatever `Wrap` holds.
#[test]
fn a_family_read_off_a_rigid_side_takes_an_instance_of_its_own() {
    for (variance, refused) in [(Variance::Irrelevant, false), (Variance::Invariant, true)] {
        let mut context = context();
        register_wrap(&mut context, variance);
        let large = Term::type_at(Level::constant(1));
        let kind = Term::func_type([(context.fresh(Some("A")), large.clone())], large.clone());
        context.birth_metavar(MetavarId(0), Vec::new(), kind);

        let carrier = context.fresh(Some("A"));
        let rigid = Term::func(
            [(carrier, large)],
            Term::induct_type_at(
                nominal("Wrap"),
                [Level::zero()],
                [Term::free_var(&carrier)],
                Vec::<Term>::new(),
            ),
        );
        assert_eq!(
            conv(&mut context, &Term::hole(0), &rigid),
            Ok(true),
            "{variance:?}"
        );
        assert!(context.metavar_solution(MetavarId(0)).is_some());
        assert_eq!(
            context.universes_mut().finalize([], [], []).is_err(),
            refused,
            "{variance:?}"
        );
    }
}

// A goal imitation blocks is compared again once the run makes progress, not assumed. `(x: ?1) -> ?0(Nat)` against `(x: Nat) -> V`, with `V` defined as `Vec(Nat, 3)`: the domain solves `?1`, the codomain's imitation is refused by `?0`'s kind, and the retry the solution prompts meets the same goal. Its key is recorded as reduced and the goal blocked as written, so a key removed under the written spelling stays, and the retry reads the goal as a recurrence.
#[test]
fn a_goal_imitation_blocks_is_compared_again_on_its_retry() {
    let mut context = context();
    register_vec(&mut context);
    let kind = type_to_type(&mut context);
    context.birth_metavar(MetavarId(0), Vec::new(), kind);
    context.birth_metavar(MetavarId(1), Vec::new(), Term::type_ground());
    let alias = context.fresh(Some("V"));
    context.define(
        &alias,
        &Term::induct_type(nominal("Vec"), [nat_type()], [nat(3)]),
        None,
    );

    let flex = Term::func_type(
        [(context.fresh(Some("x")), Term::hole(1))],
        Term::apply(Term::hole(0), [nat_type()]),
    );
    let rigid = Term::func_type(
        [(context.fresh(Some("x")), nat_type())],
        Term::free_var(&alias),
    );
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &flex, &rigid);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))), "{outcome:?}");
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

// === Flex-apply abstraction (a metavariable applied to bound variables) =====

/// The kind `(Nat) -> Type`.
fn nat_to_type(context: &mut Context) -> Term {
    Term::func_type(
        [(context.fresh(Some("n")), nat_type())],
        Term::type_ground(),
    )
}

/// The kind `(Nat) -> Nat`.
fn nat_to_nat(context: &mut Context) -> Term {
    Term::func_type([(context.fresh(Some("n")), nat_type())], nat_type())
}

// `?0(n) ≟ n` for a bound `n`: the abstraction `(n) => n` is the one solution, a projection no imitation produces, so at `3` it answers `3`.
#[test]
fn a_metavariable_applied_to_a_bound_variable_is_solved_by_abstraction() {
    let mut context = context();
    let kind = nat_to_nat(&mut context);
    context.birth_metavar(MetavarId(0), Vec::new(), kind);
    let n = context.fresh(Some("n"));

    context.with_frame(|context| {
        context.assume(&n, &nat_type());
        let flex = Term::apply(Term::hole(0), [Term::free_var(&n)]);
        assert_eq!(
            convert(context, &nat_type(), &flex, &Term::free_var(&n)),
            Ok(true)
        );
    });
    assert!(context.metavar_solution(MetavarId(0)).is_some());

    let at_three = Term::apply(Term::hole(0), [nat(3)]);
    assert_eq!(
        convert(&mut context, &nat_type(), &at_three, &nat(3)),
        Ok(true)
    );
}

#[test]
fn abstraction_is_symmetric() {
    let mut context = context();
    let kind = nat_to_nat(&mut context);
    context.birth_metavar(MetavarId(0), Vec::new(), kind);
    let n = context.fresh(Some("n"));

    context.with_frame(|context| {
        context.assume(&n, &nat_type());
        let flex = Term::apply(Term::hole(0), [Term::free_var(&n)]);
        assert_eq!(
            convert(context, &nat_type(), &Term::free_var(&n), &flex),
            Ok(true)
        );
    });
    assert!(context.metavar_solution(MetavarId(0)).is_some());
}

// `?0(n) ≟ List(Nat)`: the family ignores its binder. Imitation answers `(n) => List(n)` and then holds `n` to `Nat`; the abstraction is the constant family, which is what the equation states.
#[test]
fn a_family_that_ignores_its_binder_is_the_constant_one() {
    let mut context = context();
    register_list(&mut context);
    let kind = nat_to_type(&mut context);
    context.birth_metavar(MetavarId(0), Vec::new(), kind);
    let n = context.fresh(Some("n"));
    let list_nat = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());

    context.with_frame(|context| {
        context.assume(&n, &nat_type());
        let flex = Term::apply(Term::hole(0), [Term::free_var(&n)]);
        assert_eq!(conv(context, &flex, &list_nat), Ok(true));
    });

    let at_three = Term::apply(Term::hole(0), [nat(3)]);
    assert_eq!(conv(&mut context, &at_three, &list_nat), Ok(true));
}

// `?0(n, n)`: two abstractions answer it, over either argument, so it is no pattern and the goal waits.
#[test]
fn a_variable_applied_twice_is_no_pattern() {
    let mut context = context();
    let first = context.fresh(Some("a"));
    let second = context.fresh(Some("b"));
    context.birth_metavar(
        MetavarId(0),
        Vec::new(),
        Term::func_type([(first, nat_type()), (second, nat_type())], nat_type()),
    );
    let n = context.fresh(Some("n"));

    context.with_frame(|context| {
        context.assume(&n, &nat_type());
        let flex = Term::apply(Term::hole(0), [Term::free_var(&n), Term::free_var(&n)]);
        let outcome = convert_outcome(context, &nat_type(), &flex, &Term::free_var(&n));
        assert!(matches!(outcome, Ok(Outcome::Blocked(_))), "{outcome:?}");
    });
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

// `?0[n](n)`: the spine carries the variable the metavariable is applied to, so a solution may read it through either, and the goal waits.
#[test]
fn a_variable_the_spine_carries_is_no_pattern() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let kind = nat_to_type(&mut context);
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], kind);
    let n = context.fresh(Some("n"));

    context.with_frame(|context| {
        context.assume(&n, &nat_type());
        let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&n)]);
        let flex = Term::apply(occurrence, [Term::free_var(&n)]);
        let outcome = convert_outcome(context, &Term::type_ground(), &flex, &nat_type());
        assert!(matches!(outcome, Ok(Outcome::Blocked(_))), "{outcome:?}");
    });
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}
