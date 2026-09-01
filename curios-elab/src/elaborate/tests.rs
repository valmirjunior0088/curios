use curios_core::*;
use {
    crate::*,
    curios_num::Floating,
    curios_num::Natural,
    curios_utilities::{Plicity, Qualifier},
};

/// A declaration's name, from the path a test writes. Fixture-only.
fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

fn context() -> Context {
    Context::with_default_budget(crate::SYNTAX)
}

// Only the witness lowering mints a `Derive`, always in checked position against a concept application; met in inference, or checked against anything else, the transient has nothing to derive from and refuses rather than passing through.
#[test]
fn a_derive_transient_is_refused_outside_a_checked_witness_body() {
    let mut context = context();
    let term = Term::derive();

    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::DeriveOutsideWitness)
    ));
    assert!(matches!(
        elaborate(&mut context, &term, Mode::Check(nat())),
        Err(Error::DeriveOutsideWitness)
    ));
}

fn nat() -> Term {
    Subterm::Intrinsic(Intrinsic::NatType).into()
}

fn nat_lit(n: usize) -> Term {
    Subterm::Intrinsic(Intrinsic::Nat(Nat::new(n))).into()
}

fn opt_type() -> Term {
    Term::induct_type(nominal("Opt"), Vec::<Term>::new(), Vec::<Term>::new())
}

// induct Opt : Type | none() | some(x : Nat) end — an unindexed, two-constructor data type, the minimal shape a `| _ =>` catch-all is interesting over.
fn register_opt(context: &mut Context) {
    let payload = context.fresh(Some("x"));
    context
        .register_induct(
            &nominal("Opt"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::from([
                    (
                        Atom::from("none"),
                        InductParam::new(Telescope::done(Vec::new()), vec![]),
                    ),
                    (
                        Atom::from("some"),
                        InductParam::new(
                            Telescope::build(
                                [(payload, Term::intrinsic(Intrinsic::NatType))],
                                Vec::new(),
                            ),
                            vec![Plicity::Explicit],
                        ),
                    ),
                ]),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
            },
        )
        .unwrap();
}

fn make_opt_opaque(context: &mut Context) {
    let mut induct_decl = context.induct_decl(&nominal("Opt")).unwrap().clone();
    induct_decl.module = Qualifier::from(["Owner"]);
    induct_decl.rep_public = false;
    context.update_induct(&nominal("Opt"), induct_decl);
}

#[test]
fn opaque_inductive_construction_is_rejected_outside_declaring_module() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);

    let term = Term::variant(
        nominal("Opt"),
        Vec::<Term>::new(),
        "none",
        Vec::<Term>::new(),
    );
    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::PrivateRepresentation { name }) if name == "/Opt"
    ));

    context.set_island(Qualifier::from(["Owner"]));
    assert!(elaborate(&mut context, &term, Mode::Infer).is_ok());
}

#[test]
fn opaque_inductive_eliminators_are_rejected_before_shape_analysis() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);
    let scrutinee = context.fresh(Some("x"));
    let payload = context.fresh(Some("n"));
    context.assume(&scrutinee, &opt_type());

    let empty = Term::induct_match(
        Term::free_var(&scrutinee),
        Some(&scrutinee),
        nat(),
        Vec::<(Atom, Vec<Free>, Term)>::new(),
    );
    let named = Term::induct_match(
        Term::free_var(&scrutinee),
        Some(&scrutinee),
        nat(),
        [
            ("none", Vec::<Free>::new(), nat_lit(0)),
            ("some", vec![payload], nat_lit(1)),
        ],
    );
    let defaulted = Term::induct_match_default(
        Term::free_var(&scrutinee),
        Some(&scrutinee),
        nat(),
        [("none", Vec::<Free>::new(), nat_lit(0))],
        nat_lit(1),
    );

    for term in [empty, named, defaulted] {
        assert!(matches!(
            elaborate(&mut context, &term, Mode::Infer),
            Err(Error::PrivateRepresentation { name }) if name == "/Opt"
        ));
    }
}

// Privacy is a property of surface elaboration: machinery re-deriving types from already-elaborated terms runs under `with_suppressed_privacy` (no island — no use site to judge from), which admits what enforcement rejects and restores the island on exit.
#[test]
fn privacy_suppression_admits_machinery_rederivation() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);

    let term = Term::variant(
        nominal("Opt"),
        Vec::<Term>::new(),
        "none",
        Vec::<Term>::new(),
    );
    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::PrivateRepresentation { name }) if name == "/Opt"
    ));

    let admitted =
        context.with_suppressed_privacy(|context| elaborate(context, &term, Mode::Infer).is_ok());
    assert!(admitted);

    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::PrivateRepresentation { name }) if name == "/Opt"
    ));
}

// The oracle's suppressions are a package: a re-validated candidate can embed machinery-built projections of private representations, and a swallowed privacy error would silently flip the verdict.
#[test]
fn oracle_suppresses_privacy_as_part_of_its_package() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);

    let term = Term::variant(
        nominal("Opt"),
        Vec::<Term>::new(),
        "none",
        Vec::<Term>::new(),
    );
    let admitted = context.with_oracle(|context| elaborate(context, &term, Mode::Infer).is_ok());
    assert!(admitted);
}

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
    let func_type = Term::func_type([(x.clone(), nat())], nat());
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
    let func = Term::func([(x.clone(), nat())], Term::free_var(&x));
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
    assert_eq!(entry.result, nat());
    assert_eq!(*entry.telescope, vec![(x, nat())]);
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
    // Mirror `elaborate_module_suffix`: written ids live below the floor, so the stand-in type metavariable minted here cannot collide with the goal's.
    context.seed_metavars(1);

    // A written goal in synthesis position does not die with `CannotInfer`: a fresh unmarked metavariable stands in as its type, and the goal is birthed under it so zonk can report it.
    let (term, type_) = elaborate(&mut context, &Term::goal(0), Mode::Infer).unwrap();

    assert!(matches!(&*term, Subterm::Metavar(m) if m.id == MetavarId(0)));
    assert!(matches!(&*type_, Subterm::Metavar(m) if m.id == MetavarId(1)));

    let entry = context.metavar_entry(MetavarId(0)).expect("goal was born");
    assert_eq!(entry.result, type_);
}

#[test]
fn inductive_match_default_relaxes_coverage() {
    let mut context = context();
    register_opt(&mut context);
    let motive = context.fresh(Some("m"));

    // `match some(5) : Nat | none() => 0 | _ => 99 end` — only `none` is enumerated; the un-written `some` constructor is covered by the catch-all, so this otherwise-incomplete match elaborates, at the motive's type.
    let term = Term::induct_match_default(
        Term::variant(nominal("Opt"), Vec::<Term>::new(), "some", [nat_lit(5)]),
        Some(&motive),
        nat(),
        [("none", Vec::<Free>::new(), nat_lit(0))],
        nat_lit(99),
    );

    let (_, type_) = elaborate(&mut context, &term, Mode::Infer).unwrap();
    assert_eq!(type_, nat());
}

#[test]
fn inductive_match_missing_arm_without_default_is_rejected() {
    let mut context = context();
    register_opt(&mut context);
    let motive = context.fresh(Some("m"));

    // The same match without the catch-all: `some` is genuinely missing from an unindexed inductive, so coverage fails.
    let term = Term::induct_match(
        Term::variant(nominal("Opt"), Vec::<Term>::new(), "some", [nat_lit(5)]),
        Some(&motive),
        nat(),
        [("none", Vec::<Free>::new(), nat_lit(0))],
    );

    assert!(elaborate(&mut context, &term, Mode::Infer).is_err());
}

// induct Flag : (b : Nat) -> Type | off() : (0) | on() : (1) end — the minimal indexed family, for the motive binders a catch-all has to ride along with.
fn flag_type(index: Term) -> Term {
    Term::induct_type(nominal("Flag"), Vec::<Term>::new(), vec![index])
}

fn register_flag(context: &mut Context) {
    let index = context.fresh(Some("b"));
    context
        .register_induct(
            &nominal("Flag"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::build([(index, nat())], ())),
                constructors: Vec::from([
                    (
                        Atom::from("off"),
                        InductParam::new(Telescope::done(vec![nat_lit(0)]), vec![]),
                    ),
                    (
                        Atom::from("on"),
                        InductParam::new(Telescope::done(vec![nat_lit(1)]), vec![]),
                    ),
                ]),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
            },
        )
        .unwrap();
}

#[test]
fn inductive_match_default_is_allowed_on_an_indexed_family() {
    let mut context = context();
    register_flag(&mut context);
    let index = context.fresh(Some("b"));
    let motive = context.fresh(Some("m"));

    // A `| _ =>` catch-all over an indexed family. Every motive binds its indices whether or not the body uses them, so there is no "pattern motive" for a default to conflict with: the enumerated arm is checked at its own case target index and the default at the scrutinee's actual one.
    let term: Term = Subterm::Match(Match {
        head: Term::variant(
            nominal("Flag"),
            Vec::<Term>::new(),
            "on",
            Vec::<Term>::new(),
        ),
        motive: Scope::close(Many(2), &[&index, &motive], nat()),
        cases: Cases::Induct {
            cases: Vec::from([(
                Atom::from("on"),
                InductArm::new(Scope::close(Many(0), &[], nat_lit(0)), vec![]),
            )]),
            default: Some(nat_lit(1)),
        },
    })
    .into();

    let (_, type_) = elaborate(&mut context, &term, Mode::Infer).unwrap();
    assert_eq!(type_, nat());
}

#[test]
fn motive_binder_count_is_checked_against_the_index_telescope() {
    let mut context = context();
    register_flag(&mut context);
    let motive = context.fresh(Some("m"));

    // The same match with an arity-1 motive: `Flag` has one index, so its eliminator's motive binds two names. A written motive that binds one is reported as itself, not as a downstream domain mismatch.
    let term: Term = Subterm::Match(Match {
        head: Term::variant(
            nominal("Flag"),
            Vec::<Term>::new(),
            "on",
            Vec::<Term>::new(),
        ),
        motive: Term::match_motive_written(Term::func([(motive, flag_type(nat_lit(1)))], nat())),
        cases: Cases::Induct {
            cases: Vec::from([(
                Atom::from("on"),
                InductArm::new(Scope::close(Many(0), &[], nat_lit(0)), vec![]),
            )]),
            default: Some(nat_lit(1)),
        },
    })
    .into();

    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::MotiveBinderCount {
            expected: 2,
            written: 1,
            ..
        })
    ));
}

#[test]
fn a_num_lit_that_overflows_flt_is_refused() {
    let mut context = context();
    let flt = || Term::intrinsic(Intrinsic::FltType);

    // 2^512 rounds to infinity in the model, a value no literal spells — refused like an out-of-range Byte.
    let huge = Term::num_lit(
        Natural::from(2u32).pow(512),
        curios_utilities::Sign::Unmarked,
    );
    assert!(matches!(
        elaborate(&mut context, &huge, Mode::Check(flt())),
        Err(Error::FltLiteralOutOfRange { .. })
    ));

    // A magnitude inside the finite range still resolves at `Flt`.
    let small = Term::num_lit(Natural::from(42u32), curios_utilities::Sign::Unmarked);
    let (term, _) = elaborate(&mut context, &small, Mode::Check(flt())).unwrap();
    assert_eq!(
        term,
        Term::intrinsic(Intrinsic::Flt(Floating::from_f32(42.0)))
    );
}

#[test]
fn a_num_lit_realizes_at_bool_only_for_zero_and_one() {
    let mut context = context();
    let bool_ = || Term::intrinsic(Intrinsic::BoolType);

    // `0` and `1` are the two bits, selected only by an expected `Bool` — the rule that lets a packed literal's constant atoms be numerals.
    let zero = Term::num_lit(Natural::from(0u32), curios_utilities::Sign::Unmarked);
    let (term, _) = elaborate(&mut context, &zero, Mode::Check(bool_())).unwrap();
    assert_eq!(term, Term::intrinsic(Intrinsic::Bool(false)));

    let one = Term::num_lit(Natural::from(1u32), curios_utilities::Sign::Unmarked);
    let (term, _) = elaborate(&mut context, &one, Mode::Check(bool_())).unwrap();
    assert_eq!(term, Term::intrinsic(Intrinsic::Bool(true)));

    // Anything past a bit is refused, like an out-of-range Byte.
    let two = Term::num_lit(Natural::from(2u32), curios_utilities::Sign::Unmarked);
    assert!(matches!(
        elaborate(&mut context, &two, Mode::Check(bool_())),
        Err(Error::BoolLiteralOutOfRange { .. })
    ));

    // A negative literal has no `Bool` realization: it reports a mismatch rather than realizing.
    let negative = Term::num_lit(Natural::from(1u32), curios_utilities::Sign::Negative);
    assert!(elaborate(&mut context, &negative, Mode::Check(bool_())).is_err());
}
