//! Checking and inference: a term checked against its type or inferring one, a hole born where it is met, and a literal realized at its type.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Exhaustion, Intrinsic, MetavarId, MetavarOrigin, Minted, Mints, StructType, Subterm, Term,
        Universes,
    },
    curios_num::{Floating, Natural},
    curios_utilities::Sign,
    std::rc::Rc,
};

#[test]
fn infer_synthesizes_an_intrinsic_type() {
    let mut context = context();

    let (term, type_) = elaborate(&mut context, &nat_lit(0), Mode::Infer).unwrap();

    assert_eq!(term, nat_lit(0));
    assert_eq!(type_, nat());
}

#[test]
fn check_accepts_a_well_typed_term() {
    let mut context = context();

    let (term, type_) = elaborate(&mut context, &nat_lit(3), Mode::Check(nat())).unwrap();

    assert_eq!(term, nat_lit(3));
    assert_eq!(type_, nat());
}

#[test]
fn check_rejects_a_type_mismatch() {
    let mut context = context();

    let bool_ = Subterm::Intrinsic(Intrinsic::BoolType).into();
    let result = elaborate(&mut context, &nat_lit(3), Mode::Check(bool_));

    assert!(result.is_err());
}

#[test]
fn naturally_checked_func_elaborates_against_a_function_type() {
    let mut context = context();

    // `\ _ -> 0` checked against `(_ : Nat) -> Nat`.
    let x = context.fresh(Some("x"));
    let func_type = Term::func_type([(x, nat())], nat());
    let func = Term::func([(x, Term::hole(0))], nat_lit(0));

    let (term, type_) = elaborate(&mut context, &func, Mode::Check(func_type.clone())).unwrap();

    // Elaboration is authoritative: the rebuilt lambda carries its domain solved from the expected function type, so the hole is gone and the term is meta-free.
    assert!(term.metavars().is_empty());
    assert_eq!(type_, func_type);
}

#[test]
fn naturally_checked_func_cannot_infer() {
    let mut context = context();

    // A lambda whose domain is an unconstrained hole (the bare `(x) => …` sugar) has nothing to synthesize a domain from, so inference still fails.
    let x = context.fresh(Some("x"));
    let func = Term::func([(x, Term::hole(0))], nat_lit(0));
    let result = elaborate(&mut context, &func, Mode::Infer);

    assert!(result.is_err());
}

#[test]
fn annotated_func_infers_a_function_type() {
    let mut context = context();

    // `(x : Nat) => x` synthesizes `(Nat) -> Nat` on its own — no expected type.
    let x = context.fresh(Some("x"));
    let func = Term::func([(x, nat())], Term::free_var(&x));
    let (term, type_) = elaborate(&mut context, &func, Mode::Infer).unwrap();

    // Meta-free, and convertible (alpha-insensitive) to the expected function type; a structural `assert_eq!` would trip only on the cosmetic fresh binder label the Infer arm generates.
    assert!(term.metavars().is_empty());
    assert!(
        convert_at(
            &mut context,
            &Term::type_ground(),
            &type_,
            &Term::func_type([(x, nat())], nat())
        )
        .unwrap()
    );
}

#[test]
fn check_on_a_hole_births_it_freezing_the_local_context() {
    let mut context = context();

    // `x : Nat` is a genuine *local* binder — assumed inside a frame, the way a lambda or match body brings one into scope. Only locals are frozen into a metavariable's Γ; top-level definitions are excluded (a solution may mention them as globals instead — see `Context::identity_snapshot`), so the binder must be inside a frame to appear here. Checking the hole `?0` against `Nat` births it, recording `Nat` as its type and the in-scope locals as its frozen Γ.
    let x = context.fresh(Some("x"));
    let (term, type_) = context.with_frame(|context| {
        context.assume(&x, &nat());
        let hole = Term::hole(0);
        elaborate(context, &hole, Mode::Check(nat())).unwrap()
    });

    // Birth rebuilds the hole with the identity spine over its frozen Γ — the delayed substitution that keeps its eventual solution aligned through every later `close`/`open`.
    assert_eq!(
        term,
        Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&x)])
    );
    assert_eq!(type_, nat());

    let entry = context.metavar_entry(MetavarId(0)).expect("hole was born");
    assert_eq!(*entry.result(), nat());
    assert_eq!(
        entry.telescope().iter().cloned().collect::<Vec<_>>(),
        vec![(x, nat())]
    );
}

#[test]
fn infer_on_an_unborn_hole_cannot_infer() {
    let mut context = context();

    let result = elaborate(&mut context, &Term::hole(0), Mode::Infer);

    assert!(result.is_err());
}

#[test]
fn infer_on_an_unborn_goal_births_it_with_a_meta_type() {
    let mut context = context();
    // Mirror `elaborate_module_suffix`: written ids live below the lowering's count, so the stand-in type metavariable minted here cannot collide with the goal's.
    let mut seeded = Minted::default();
    seeded.entry = Mints {
        metavariables: 1,
        ..Mints::default()
    };
    context.seed(&Rc::new(seeded));
    context.attempt(&[]);

    // A written goal in synthesis position does not die with `CannotInfer`: a fresh unmarked metavariable stands in as its type, and the goal is birthed under it so zonk can report it.
    let (term, type_) = elaborate(&mut context, &Term::goal(0), Mode::Infer).unwrap();

    assert!(matches!(&*term, Subterm::Metavar(m) if m.id == MetavarId(0)));
    assert!(matches!(&*type_, Subterm::Metavar(m) if m.id == MetavarId(1)));

    let entry = context.metavar_entry(MetavarId(0)).expect("goal was born");
    assert_eq!(*entry.result(), type_);
}

#[test]
fn a_num_lit_that_overflows_flt_is_refused() {
    let mut context = context();
    let flt = || Term::intrinsic(Intrinsic::FltType);

    // 2^2048 rounds to infinity in the model, a value no literal spells — refused like an out-of-range Byte.
    let huge = Term::num_lit(Natural::from(2u32).pow(2048), Sign::Unmarked);
    assert!(matches!(
        elaborate(&mut context, &huge, Mode::Check(flt())),
        Err(Error::FltLiteralOutOfRange { .. })
    ));

    // A magnitude inside the finite range still resolves at `Flt`.
    let small = Term::num_lit(Natural::from(42u32), Sign::Unmarked);
    let (term, _) = elaborate(&mut context, &small, Mode::Check(flt())).unwrap();
    assert_eq!(term, Term::intrinsic(Intrinsic::Flt(Floating::from(42.0))));
}

#[test]
fn a_num_lit_realizes_at_bool_only_for_zero_and_one() {
    let mut context = context();
    let bool_ = || Term::intrinsic(Intrinsic::BoolType);

    // `0` and `1` are the two bits, selected only by an expected `Bool` — the rule that lets a packed literal's constant atoms be numerals.
    let zero = Term::num_lit(Natural::from(0u32), Sign::Unmarked);
    let (term, _) = elaborate(&mut context, &zero, Mode::Check(bool_())).unwrap();
    assert_eq!(term, Term::intrinsic(Intrinsic::Bool(false)));

    let one = Term::num_lit(Natural::from(1u32), Sign::Unmarked);
    let (term, _) = elaborate(&mut context, &one, Mode::Check(bool_())).unwrap();
    assert_eq!(term, Term::intrinsic(Intrinsic::Bool(true)));

    // Anything past a bit is refused, like an out-of-range Byte.
    let two = Term::num_lit(Natural::from(2u32), Sign::Unmarked);
    assert!(matches!(
        elaborate(&mut context, &two, Mode::Check(bool_())),
        Err(Error::BoolLiteralOutOfRange { .. })
    ));

    // A negative literal has no `Bool` realization: it reports a mismatch rather than realizing.
    let negative = Term::num_lit(Natural::from(1u32), Sign::Negative);
    assert!(elaborate(&mut context, &negative, Mode::Check(bool_())).is_err());
}

/// A monad's shape reads its context arguments as probes: one the budget cannot afford propagates the refusal, rather than keying on nothing — compatible with any region — and letting the `!` oracle go ahead on that reading.
#[test]
fn a_context_argument_the_budget_cannot_read_propagates_the_refusal() {
    let mut context = Context::new(100_000, SYNTAX);
    let loop_ = context.fresh(Some("loop"));
    context.define(&loop_, &Term::free_var(&loop_), None);
    let region = Term::from(Subterm::StructType(StructType {
        name: nominal("Region"),
        universes: Universes::default(),
        params: vec![Term::free_var(&loop_), Term::type_ground()],
    }));

    let shape = monad_shape(&mut context, &region);

    assert!(shape.is_err_and(|spent| spent.is_exhausted()));
}
