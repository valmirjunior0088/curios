//! Imitation against an inductive, a struct and an intrinsic former, and the arities that block it.

use super::test_support::*;
use curios_core::*;
use {crate::*, curios_utilities::Qualifier};

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
    context.birth_metavar(MetaId(0), Vec::new(), kind);

    // ?0(Nat) ≟ List(Nat)  — commits ?0 := λA. List(A).
    let flex = Term::apply(Term::hole(0), [nat_type()]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());

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
    context.birth_metavar(MetaId(0), Vec::new(), kind);

    // Rigid on the left, stuck application on the right.
    let flex = Term::apply(Term::hole(0), [nat_type()]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &rigid, &flex), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());
}

#[test]
fn equates_arguments_pairwise() {
    let mut context = context();
    register_list(&mut context);
    let kind = type_to_type(&mut context);
    context.birth_metavar(MetaId(0), Vec::new(), kind);
    context.birth_metavar(MetaId(1), Vec::new(), Term::type_ground());

    // ?0(?1) ≟ List(Nat) — the imitation solves ?0, the pairwise equation ?1.
    let flex = Term::apply(Term::hole(0), [Term::hole(1)]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());
    assert_eq!(context.metavar_solution(MetaId(1)), Some(&nat_type()));
}

#[test]
fn splits_params_and_indices() {
    let mut context = context();
    let elem = context.fresh(Some("T"));
    let n = context.fresh(Some("n"));
    register_vec(&mut context);
    context.birth_metavar(
        MetaId(0),
        Vec::new(),
        Term::func_type(
            [
                (elem.clone(), Term::type_ground()),
                (n.clone(), Term::intrinsic(Intrinsic::NatType)),
            ],
            Term::type_ground(),
        ),
    );

    // ?0(Nat, 3) ≟ Vec(Nat, 3) — arity 2 = 1 param + 1 index; the candidate's body must mirror the rigid node's split or re-validation rejects it.
    let flex = Term::apply(Term::hole(0), [nat_type(), nat(3)]);
    let rigid = Term::induct_type(nominal("Vec"), [nat_type()], [nat(3)]);
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());

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
                    [
                        (first.clone(), Term::type_ground()),
                        (second.clone(), Term::type_ground()),
                    ],
                    Telescope::done(()),
                ),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
            },
        )
        .unwrap();
    context.birth_metavar(
        MetaId(0),
        Vec::new(),
        Term::func_type(
            [
                (first.clone(), Term::type_ground()),
                (second.clone(), Term::type_ground()),
            ],
            Term::type_ground(),
        ),
    );

    let flex = Term::apply(Term::hole(0), [nat_type(), nat_type()]);
    let rigid = Term::struct_type(nominal("Pair"), [nat_type(), nat_type()]);
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());
}

#[test]
fn arity_mismatch_blocks() {
    let mut context = context();
    register_vec(&mut context);
    let kind = type_to_type(&mut context);
    context.birth_metavar(MetaId(0), Vec::new(), kind);

    // ?0(Nat) ≟ Vec(Nat, 3) — apply arity 1 against constructor arity 2: v1 has no partial-application solutions, so the goal blocks (it is not provably unequal — a constant solution could exist).
    let flex = Term::apply(Term::hole(0), [nat_type()]);
    let rigid = Term::induct_type(nominal("Vec"), [nat_type()], [nat(3)]);
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &flex, &rigid);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn non_function_birth_type_blocks() {
    let mut context = context();
    register_list(&mut context);
    // ?0's frozen type is not a function type: no candidate can be built.
    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());

    let flex = Term::apply(Term::hole(0), [nat_type()]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &flex, &rigid);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetaId(0)), None);
}

#[test]
fn leaves_rigid_apply_pairs_alone() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    register_list(&mut context);
    let kind = type_to_type(&mut context);
    context.assume(&f, &kind);

    // A *rigid* stuck application against a nominal type is not the imitation case: the guard falls back to the neutral path, which cannot equate them — a definite mismatch, exactly as before the rule existed.
    let stuck = Term::apply(Term::free_var(&f), [nat_type()]);
    let rigid = Term::induct_type(nominal("List"), [nat_type()], Vec::<Term>::new());
    assert_eq!(conv(&mut context, &stuck, &rigid), Ok(false));
}

#[test]
fn solves_flex_apply_against_intrinsic_former() {
    let mut context = context();
    let kind = type_to_type(&mut context);
    context.birth_metavar(MetaId(0), Vec::new(), kind);

    // ?0(?1) ≟ List(Nat) — the imitation solves ?0 := λT. List(T), the pairwise equation ?1 := Nat. This is what pins `M := List` for `Monad(List)`.
    context.birth_metavar(MetaId(1), Vec::new(), Term::type_ground());
    let flex = Term::apply(Term::hole(0), [Term::hole(1)]);
    let rigid = Term::intrinsic(Intrinsic::ListType(nat_type()));
    assert_eq!(conv(&mut context, &flex, &rigid), Ok(true));
    assert!(context.metavar_solution(MetaId(0)).is_some());
    assert_eq!(context.metavar_solution(MetaId(1)), Some(&nat_type()));
}
