//! Tuples, projections, eta at a known type, and the irrelevance that fires at a computed proposition.

use super::test_support::*;
use curios_core::*;
use {
    crate::*,
    curios_utilities::{Plicity, Qualifier},
};

#[test]
fn convert_tuple_equal() {
    let mut context = context();

    let this = Term::tuple([nat(1), nat(2)]);
    let that = Term::tuple([nat(1), nat(2)]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn tuple_unequal_field() {
    let mut context = context();

    let this = Term::tuple([nat(1), nat(2)]);
    let that = Term::tuple([nat(1), nat(3)]);

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn proj_same_index_and_head() {
    let mut context = context();
    let r = context.fresh(Some("r"));

    let this = Term::proj(Term::free_var(&r), 0);
    let that = Term::proj(Term::free_var(&r), 0);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn proj_different_index_is_false() {
    let mut context = context();
    let r = context.fresh(Some("r"));

    let this = Term::proj(Term::free_var(&r), 0);
    let that = Term::proj(Term::free_var(&r), 1);

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn eta_tuple_neutral_with_known_type() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    let r_binder = context.fresh(Some("r"));
    let s_binder = context.fresh(Some("s"));

    let tuple_type: Term = Term::tuple_type([
        (x.clone(), Term::intrinsic(Intrinsic::NatType)),
        (y.clone(), Term::intrinsic(Intrinsic::BoolType)),
    ]);

    let r: Term = Term::free_var(&r_binder);
    let s: Term = Term::free_var(&s_binder);

    assert_eq!(convert(&mut context, &tuple_type, &r, &r), Ok(true));

    assert_eq!(convert(&mut context, &tuple_type, &r, &s), Ok(false));
}

#[test]
fn partial_projection_tuple_at_narrow_type() {
    let mut context = context();
    let p = context.fresh(Some("p"));
    let q = context.fresh(Some("q"));
    let x = context.fresh(Some("x"));

    // p = (1, 2), q = (1, 3) — both 2-tuples agreeing on field 0, differing on field 1.
    context.define(&p, &Term::tuple([nat(1), nat(2)]), None);
    context.define(&q, &Term::tuple([nat(1), nat(3)]), None);

    let type_: Term = Term::tuple_type([(x.clone(), Term::intrinsic(Intrinsic::NatType))]);

    // this = (p.0), that = (q.0). At the 1-field type both denote (a), so conversion should return true.
    let this: Term = Term::tuple([Term::proj(Term::free_var(&p), 0)]);
    let that: Term = Term::tuple([Term::proj(Term::free_var(&q), 0)]);

    // Even though eta_reduce_tuple widens each 1-tuple to its bare base (`Var p`, `Var q`), the convert loop then routes the neutral pair through `eta_expand_neutral`, which re-projects according to the TRUE type telescope (1 field). Each `proj(_, 0)` then reduces to `1`, so the comparison succeeds — the bug is masked here.
    assert_eq!(convert(&mut context, &type_, &this, &that), Ok(true));
}

#[test]
fn times_out_on_pathological_inputs() {
    let mut context = context();
    let loop_ = context.fresh(Some("loop"));
    let x = context.fresh(Some("x"));
    let z = context.fresh(Some("z"));
    let y = context.fresh(Some("y"));

    context.define(&loop_, &Term::free_var(&loop_), None);

    let this = Term::tuple_type([
        (
            x.clone(),
            Term::apply(func([&z], Term::free_var(&z)), [Term::free_var(&loop_)]),
        ),
        (y.clone(), Term::free_var(&x)),
    ]);

    let that = Term::tuple_type([
        (x.clone(), Term::free_var(&loop_)),
        (y.clone(), Term::free_var(&x)),
    ]);

    assert!(conv(&mut context, &this, &that).is_err_and(|spent| spent.is_exhausted()));
}

#[test]
fn unit_typed_neutrals_in_type_argument() {
    let mut context = context();
    let func = context.fresh(Some("F"));
    let wildcard = context.fresh(Some("_"));
    let r_binder = context.fresh(Some("r"));
    let s_binder = context.fresh(Some("s"));

    // F : (()) -> Type ; r, s : ()   (all neutral assumptions). r ≡ s by η for the empty tuple (unit / proof irrelevance), so F r ≡ F s. `conv` compares at `Type`, exactly as the pipeline does via `expect`.
    context.assume(
        &func,
        &Term::func_type(
            [(wildcard.clone(), Term::tuple_type_unit())],
            Term::type_ground(),
        ),
    );
    context.assume(&r_binder, &Term::tuple_type_unit());
    context.assume(&s_binder, &Term::tuple_type_unit());

    let f = Term::free_var(&func);
    let r = Term::free_var(&r_binder);
    let s = Term::free_var(&s_binder);

    let this = Term::apply(f.clone(), [r]); // F r
    let that = Term::apply(f, [s]); // F s

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// A struct's fields compare at their declared types, recovered from the registry — so a proof-irrelevant (unit-typed) field equates distinct neutrals, and two structs differing only there are convertible.
#[test]
fn struct_unit_field_is_irrelevant() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let u = context.fresh(Some("u"));
    let r_binder = context.fresh(Some("r"));
    let s_binder = context.fresh(Some("s"));

    // struct Wrap { x : Nat, u : () }
    context
        .register_struct(
            &nominal("Wrap"),
            StructDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::build(
                    [
                        (x.clone(), Term::intrinsic(Intrinsic::NatType)),
                        (u.clone(), Term::tuple_type_unit()),
                    ],
                    (),
                )),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
            },
        )
        .unwrap();

    context.assume(&r_binder, &Term::tuple_type_unit());
    context.assume(&s_binder, &Term::tuple_type_unit());

    let r = Term::free_var(&r_binder);
    let s = Term::free_var(&s_binder);

    // Wrap { 1, r } and Wrap { 1, s } differ only in the unit field's neutral.
    let this = Term::struct_(nominal("Wrap"), Vec::<Term>::new(), [nat(1), r]);
    let that = Term::struct_(nominal("Wrap"), Vec::<Term>::new(), [nat(1), s]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// Likewise a variant's payload compares at its constructor's declared types, so a unit-typed payload field is proof-irrelevant.
#[test]
fn variant_unit_payload_is_irrelevant() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let u = context.fresh(Some("u"));
    let r_binder = context.fresh(Some("r"));
    let s_binder = context.fresh(Some("s"));

    // induct Wrap | wrap(x : Nat, u : ()) end
    context
        .register_induct(
            &nominal("Wrap"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::from([(
                    Atom::from("wrap"),
                    InductParam {
                        telescope: Telescope::build(
                            [
                                (x.clone(), Term::intrinsic(Intrinsic::NatType)),
                                (u.clone(), Term::tuple_type_unit()),
                            ],
                            Vec::new(),
                        ),
                        plicities: vec![Plicity::Explicit, Plicity::Explicit],
                    },
                )]),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
            },
        )
        .unwrap();

    context.assume(&r_binder, &Term::tuple_type_unit());
    context.assume(&s_binder, &Term::tuple_type_unit());

    let r = Term::free_var(&r_binder);
    let s = Term::free_var(&s_binder);

    // wrap(1, r) and wrap(1, s) differ only in the unit payload's neutral.
    let this = Term::variant(nominal("Wrap"), Vec::<Term>::new(), "wrap", [nat(1), r]);
    let that = Term::variant(nominal("Wrap"), Vec::<Term>::new(), "wrap", [nat(1), s]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

/// Proof irrelevance at a *computed* proposition — a stuck `match` whose motive is `Prop`.
///
/// The rule is measured, and the measurement is why this fixture exists. Across the prelude's elaboration it fires 37 times in 84,826 conversion goals, and **every one of the 37 is at a computed proposition rather than a nominal `Prop` family**: validity predicates over `Bits` and `Nat`, the shape a decision procedure takes. Not one is at a bare `Prop`-sorted declaration. So the tests that name irrelevance — `curios-cert`'s `any_two_inhabitants_of_a_proposition_convert` and its control — cover a shape the corpus never actually presents, and they cover the *kernel's* copy, which the same measurement found inert at 0 firings in 86,547 goals. This crate's copy is the one that does the work and had no direct test; its only exercise was the prelude happening to be written with those predicates, which is coverage by accident rather than by assertion.
///
/// What the fixture pins is the mechanism those 37 firings rest on. `Sort::of` classifies a stuck `Match` by reading its **motive**, not its arms — "a type-valued match: its sort is the motive" — so a match that cannot reduce is nonetheless a proposition when its motive says `Prop`, and irrelevance may then discharge a goal at it without examining either side. Two distinct neutrals convert there.
///
/// The control is the identical term with the motive at `Type`. It must not convert, and it is what makes this a test of the *motive* rather than of matches in general: read the arms instead, or default to `Prop` for anything unclassifiable, and the two fixtures stop disagreeing.
#[test]
fn fires_at_a_computed_proposition() {
    let mut context = context();
    let (left, right) = (context.fresh(Some("p")), context.fresh(Some("q")));
    let computed = computed_type(&mut context, Term::prop());

    assert_eq!(
        convert(
            &mut context,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(true),
    );
}

/// The control for the fixture above: the same stuck `match`, with its motive at `Type`. Irrelevance is a property of the type, and a computed type is no exception.
#[test]
fn does_not_fire_at_a_computed_relevant_type() {
    let mut context = context();
    let (left, right) = (context.fresh(Some("p")), context.fresh(Some("q")));
    let computed = computed_type(&mut context, Term::type_ground());

    assert_eq!(
        convert(
            &mut context,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(false),
    );
}

/// `match n | 0 => Nat | _ => Nat end` at the given motive, stuck because `n` is a neutral assumption. The arms are deliberately a *relevant* type in both fixtures: what decides the sort is the motive, and picking arms that agree with it would let a rule reading the arms pass too.
fn computed_type(context: &mut Context, motive: Term) -> Term {
    let subject = context.fresh(Some("n"));
    context.assume(&subject, &Term::intrinsic(Intrinsic::NatType));

    let scrutinee = context.fresh(Some("k"));
    Term::switch_scoped(
        Term::free_var(&subject),
        Scope::close(Many(1), &[&scrutinee], motive),
        [(0u32, Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    )
}

/// An intrinsic with no hand-written congruence arm is still compared operand by operand.
///
/// `ListMap` had no arm, and the wildcard beneath the table answered a *hard* mismatch rather than a postponement — so a metavariable standing in one of its operands was refused instead of solved. `convert`'s syntactic-identity short circuit hid that for every spelling that happened to be identical, which is why the omission survived. The rule now reads the operands off `Intrinsic::traverse`, so the table cannot be short an operation.
#[test]
fn an_intrinsic_without_a_hand_written_arm_solves_a_metavariable_in_its_operand() {
    let mut context = context();
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    let xs = context.fresh(Some("xs"));
    context.assume(
        &xs,
        &Term::intrinsic(Intrinsic::list_type(nat_type.clone())),
    );

    let n = context.fresh(Some("n"));
    let mapper_type = Term::func_type([(n, nat_type.clone())], nat_type.clone());
    let f = context.fresh(Some("f"));
    context.assume(&f, &mapper_type);

    context.birth_metavar(MetaId(0), Vec::new(), mapper_type);

    let flexible = Term::intrinsic(Intrinsic::list_map(
        nat_type.clone(),
        nat_type.clone(),
        Term::free_var(&xs),
        Term::hole(0),
    ));
    let rigid = Term::intrinsic(Intrinsic::list_map(
        nat_type.clone(),
        nat_type,
        Term::free_var(&xs),
        Term::free_var(&f),
    ));

    assert_eq!(conv(&mut context, &flexible, &rigid), Ok(true));
}
