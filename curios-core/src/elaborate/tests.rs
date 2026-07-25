use {
    crate::*,
    curios_base::{Plicity, Qualifier, RootId},
    std::{collections::BTreeMap, time::Duration},
};

fn context() -> Context {
    Context::new(Duration::from_secs(1))
}

fn nat() -> Term {
    Subterm::Prim(Prim::NatType).into()
}

fn nat_lit(n: usize) -> Term {
    Subterm::Prim(Prim::Nat(Nat::new(n))).into()
}

fn opt_type() -> Term {
    Term::induct_type("Opt", Vec::<Term>::new(), Vec::<Term>::new())
}

// induct Opt : Type | none() | some(x : Nat) end — an unindexed, two-constructor
// data type, the minimal shape a `| _ =>` catch-all is interesting over.
fn register_opt(context: &mut Context) {
    context
        .register_induct(
            "Opt",
            InductDecl {
                universe_context: UniverseContext::empty(),
                params: Telescope::done(()),
                indices: Telescope::done(()),
                constructors: BTreeMap::from([
                    (
                        Atom::from("none"),
                        InductParam {
                            telescope: Telescope::done(opt_type()),
                            plicities: vec![],
                        },
                    ),
                    (
                        Atom::from("some"),
                        InductParam {
                            telescope: Telescope::build(
                                [("x", Term::prim(Prim::NatType))],
                                opt_type(),
                            ),
                            plicities: vec![Plicity::Explicit],
                        },
                    ),
                ]),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                root: RootId::Entry,
                rep_public: true,
            },
        )
        .unwrap();
}

fn make_opt_opaque(context: &mut Context) {
    let mut induct_decl = context.induct_decl("Opt").unwrap().clone();
    induct_decl.module = Qualifier::from(["Owner"]);
    induct_decl.rep_public = false;
    context.update_induct("Opt", induct_decl);
}

#[test]
fn opaque_inductive_construction_is_rejected_outside_declaring_module() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);

    let term = Term::variant("Opt", Vec::<Term>::new(), "none", Vec::<Term>::new());
    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::PrivateRepresentation { name }) if name == "Opt"
    ));

    context.set_island(Qualifier::from(["Owner"]));
    assert!(elaborate(&mut context, &term, Mode::Infer).is_ok());
}

#[test]
fn opaque_inductive_eliminators_are_rejected_before_shape_analysis() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);
    context.assume("x", &opt_type());

    let empty = Term::induct_match(
        Term::free_var("x"),
        Some("x"),
        nat(),
        Vec::<(Atom, Vec<&str>, Term)>::new(),
    );
    let named = Term::induct_match(
        Term::free_var("x"),
        Some("x"),
        nat(),
        [
            ("none", Vec::<&str>::new(), nat_lit(0)),
            ("some", vec!["n"], nat_lit(1)),
        ],
    );
    let defaulted = Term::induct_match_default(
        Term::free_var("x"),
        Some("x"),
        nat(),
        [("none", Vec::<&str>::new(), nat_lit(0))],
        nat_lit(1),
    );

    for term in [empty, named, defaulted] {
        assert!(matches!(
            elaborate(&mut context, &term, Mode::Infer),
            Err(Error::PrivateRepresentation { name }) if name == "Opt"
        ));
    }
}

// Privacy is a property of surface elaboration: machinery re-deriving types
// from already-elaborated terms runs under `with_suppressed_privacy` (no
// island — no use site to judge from), which admits what enforcement rejects
// and restores the island on exit.
#[test]
fn privacy_suppression_admits_machinery_rederivation() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);

    let term = Term::variant("Opt", Vec::<Term>::new(), "none", Vec::<Term>::new());
    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::PrivateRepresentation { name }) if name == "Opt"
    ));

    let admitted =
        context.with_suppressed_privacy(|context| elaborate(context, &term, Mode::Infer).is_ok());
    assert!(admitted);

    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::PrivateRepresentation { name }) if name == "Opt"
    ));
}

// The oracle's suppressions are a package: a re-validated candidate can embed
// machinery-built projections of private representations, and a swallowed
// privacy error would silently flip the verdict.
#[test]
fn oracle_suppresses_privacy_as_part_of_its_package() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);

    let term = Term::variant("Opt", Vec::<Term>::new(), "none", Vec::<Term>::new());
    let admitted = context.with_oracle(|context| elaborate(context, &term, Mode::Infer).is_ok());
    assert!(admitted);
}

#[test]
fn infer_synthesizes_a_primitive_type() {
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

    let bool_ = Subterm::Prim(Prim::BoolType).into();
    let result = elaborate(&mut context, &nat_lit(3), Mode::Check(bool_));

    assert!(result.is_err());
}

#[test]
fn naturally_checked_func_elaborates_against_a_function_type() {
    let mut context = context();

    // `\ _ -> 0` checked against `(_ : Nat) -> Nat`.
    let func_type = Term::func_type([("x", nat())], nat());
    let func = Term::func([("x", Term::metavar(0))], nat_lit(0));

    let (term, type_) = elaborate(&mut context, &func, Mode::Check(func_type.clone())).unwrap();

    // Elaboration is authoritative: the rebuilt lambda carries its domain solved
    // from the expected function type, so the hole is gone and the term is
    // meta-free.
    assert!(term.metavars().is_empty());
    assert_eq!(type_, func_type);
}

#[test]
fn naturally_checked_func_cannot_infer() {
    let mut context = context();

    // A lambda whose domain is an unconstrained hole (the bare `(x) => …` sugar)
    // has nothing to synthesize a domain from, so inference still fails.
    let func = Term::func([("x", Term::metavar(0))], nat_lit(0));
    let result = elaborate(&mut context, &func, Mode::Infer);

    assert!(result.is_err());
}

#[test]
fn annotated_func_infers_a_function_type() {
    let mut context = context();

    // `(x : Nat) => x` synthesizes `(Nat) -> Nat` on its own — no expected type.
    let func = Term::func([("x", nat())], Term::free_var("x"));
    let (term, type_) = elaborate(&mut context, &func, Mode::Infer).unwrap();

    // Meta-free, and convertible (alpha-insensitive) to the expected function
    // type; a structural `assert_eq!` would trip only on the cosmetic fresh
    // binder label the Infer arm generates.
    assert!(term.metavars().is_empty());
    assert!(
        convert_at(
            &mut context,
            &Term::type_ground(),
            &type_,
            &Term::func_type([("x", nat())], nat())
        )
        .unwrap()
    );
}

#[test]
fn check_on_a_hole_births_it_freezing_the_local_context() {
    let mut context = context();

    // `x : Nat` is a genuine *local* binder — assumed inside a frame, the way a
    // lambda or match body brings one into scope. Only locals are frozen into a
    // metavariable's Γ; top-level definitions are excluded (a solution may
    // mention them as globals instead — see `Context::identity_snapshot`), so
    // the binder must be inside a frame to appear here. Checking the hole `?0`
    // against `Nat` births it, recording `Nat` as its type and the in-scope
    // locals as its frozen Γ.
    let (term, type_) = context.with_frame(|context| {
        context.assume("x", &nat());
        let hole = Term::metavar(0);
        elaborate(context, &hole, Mode::Check(nat())).unwrap()
    });

    // Birth rebuilds the hole with the identity spine over its frozen Γ — the
    // delayed substitution that keeps its eventual solution aligned through
    // every later `close`/`open`.
    assert_eq!(
        term,
        Term::metavar_birthed(0, None, vec![Term::free_var("x")])
    );
    assert_eq!(type_, nat());

    let entry = context.metavar_entry(MetaId(0)).expect("hole was born");
    assert_eq!(entry.result, nat());
    assert_eq!(*entry.telescope, vec![("x".to_string(), nat())]);
}

#[test]
fn infer_on_an_unborn_hole_cannot_infer() {
    let mut context = context();

    let result = elaborate(&mut context, &Term::metavar(0), Mode::Infer);

    assert!(result.is_err());
}

#[test]
fn infer_on_an_unborn_goal_births_it_with_a_meta_type() {
    let mut context = context();
    // Mirror `elaborate_module`: written ids live below the floor, so the
    // stand-in type metavariable minted here cannot collide with the goal's.
    context.seed_metavars(1);

    // A written goal in synthesis position does not die with `CannotInfer`:
    // a fresh unmarked metavariable stands in as its type, and the goal is
    // birthed under it so zonk can report it.
    let (term, type_) = elaborate(&mut context, &Term::goal(0), Mode::Infer).unwrap();

    assert!(matches!(&*term, Subterm::Metavar(m) if m.id == MetaId(0)));
    assert!(matches!(&*type_, Subterm::Metavar(m) if m.id == MetaId(1)));

    let entry = context.metavar_entry(MetaId(0)).expect("goal was born");
    assert_eq!(entry.result, type_);
}

#[test]
fn inductive_match_default_relaxes_coverage() {
    let mut context = context();
    register_opt(&mut context);

    // `match some(5) : Nat | none() => 0 | _ => 99 end` — only `none` is
    // enumerated; the un-written `some` constructor is covered by the catch-all,
    // so this otherwise-incomplete match elaborates, at the motive's type.
    let term = Term::induct_match_default(
        Term::variant("Opt", Vec::<Term>::new(), "some", [nat_lit(5)]),
        Some("m"),
        nat(),
        [("none", Vec::<&str>::new(), nat_lit(0))],
        nat_lit(99),
    );

    let (_, type_) = elaborate(&mut context, &term, Mode::Infer).unwrap();
    assert_eq!(type_, nat());
}

#[test]
fn inductive_match_missing_arm_without_default_is_rejected() {
    let mut context = context();
    register_opt(&mut context);

    // The same match without the catch-all: `some` is genuinely missing from an
    // unindexed inductive, so coverage fails.
    let term = Term::induct_match(
        Term::variant("Opt", Vec::<Term>::new(), "some", [nat_lit(5)]),
        Some("m"),
        nat(),
        [("none", Vec::<&str>::new(), nat_lit(0))],
    );

    assert!(elaborate(&mut context, &term, Mode::Infer).is_err());
}

// induct Flag : (b : Nat) -> Type | off() : (0) | on() : (1) end — the minimal
// indexed family, for the motive binders a catch-all has to ride along with.
fn flag_type(index: Term) -> Term {
    Term::induct_type("Flag", Vec::<Term>::new(), vec![index])
}

fn register_flag(context: &mut Context) {
    context
        .register_induct(
            "Flag",
            InductDecl {
                universe_context: UniverseContext::empty(),
                params: Telescope::done(()),
                indices: Telescope::build([("b", nat())], ()),
                constructors: BTreeMap::from([
                    (
                        Atom::from("off"),
                        InductParam {
                            telescope: Telescope::done(flag_type(nat_lit(0))),
                            plicities: vec![],
                        },
                    ),
                    (
                        Atom::from("on"),
                        InductParam {
                            telescope: Telescope::done(flag_type(nat_lit(1))),
                            plicities: vec![],
                        },
                    ),
                ]),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                root: RootId::Entry,
                rep_public: true,
            },
        )
        .unwrap();
}

#[test]
fn inductive_match_default_is_allowed_on_an_indexed_family() {
    let mut context = context();
    register_flag(&mut context);

    // A `| _ =>` catch-all over an indexed family. Every motive binds its
    // indices whether or not the body uses them, so there is no "pattern
    // motive" for a default to conflict with: the enumerated arm is checked at
    // its own case target index and the default at the scrutinee's actual one.
    let term: Term = Subterm::Match(Match {
        head: Term::variant("Flag", Vec::<Term>::new(), "on", Vec::<Term>::new()),
        motive: Scope::close(Many(2), &["b", "m"], nat()),
        cases: Cases::Induct {
            cases: BTreeMap::from([(
                Atom::from("on"),
                InductArm {
                    body: Scope::close(Many(0), &[], nat_lit(0)),
                    plicities: vec![],
                },
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

    // The same match with an arity-1 motive: `Flag` has one index, so its
    // eliminator's motive binds two names. A written motive that binds one is
    // reported as itself, not as a downstream domain mismatch.
    let term: Term = Subterm::Match(Match {
        head: Term::variant("Flag", Vec::<Term>::new(), "on", Vec::<Term>::new()),
        motive: Term::match_motive_written(Term::func([("m", flag_type(nat_lit(1)))], nat())),
        cases: Cases::Induct {
            cases: BTreeMap::from([(
                Atom::from("on"),
                InductArm {
                    body: Scope::close(Many(0), &[], nat_lit(0)),
                    plicities: vec![],
                },
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
