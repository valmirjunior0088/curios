use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Cases, Exhaustion, Free, Global, InductArm, InductDecl, InductParam, Intrinsic, Many,
        Match, MatchResult, MetavarId, MetavarOrigin, Nat, Scope, Struct, StructDecl, StructEntry,
        StructType, Subterm, Telescope, Term, UniverseContext,
    },
    curios_num::{Floating, Natural},
    curios_utilities::{Plicity, Qualifier, Sign},
};

/// A declaration's name, from the path a test writes. Fixture-only.
fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

fn context() -> Context {
    Context::with_default_budget(SYNTAX)
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
                variances: Vec::new(),
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
    let admitted = context.with_oracle(&Refinements::default(), |context| {
        elaborate(context, &term, Mode::Infer).is_ok()
    });
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
    // Mirror `elaborate_module_suffix`: written ids live below the lowering's count, so the stand-in type metavariable minted here cannot collide with the goal's.
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

// induct Tagged : Type | tag(@n : Nat, x : Nat) end — a hidden payload ahead of a plain one, the least telescope an arm's binders are aligned against.
fn register_tagged(context: &mut Context) {
    let hidden = context.fresh(Some("n"));
    let plain = context.fresh(Some("x"));
    context
        .register_induct(
            &nominal("Tagged"),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::from([(
                    Atom::from("tag"),
                    InductParam::new(
                        Telescope::build([(hidden, nat()), (plain, nat())], Vec::new()),
                        vec![Plicity::Implicit, Plicity::Explicit],
                    ),
                )]),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();
}

// `match tag(3, 5) : Nat | tag(<binders>) => <body> end`, the arm's binders under the marks written.
fn match_tagged(context: &mut Context, binders: &[(Plicity, &Free)], body: Term) -> Term {
    let motive = context.fresh(Some("m"));
    let names = binders.iter().map(|(_, name)| *name).collect::<Vec<_>>();
    Subterm::Match(Match {
        head: Term::variant(
            nominal("Tagged"),
            Vec::<Term>::new(),
            "tag",
            [nat_lit(3), nat_lit(5)],
        ),
        result: MatchResult::Family(Scope::close(Many(1), &[&motive], nat())),
        cases: Cases::Induct {
            cases: Vec::from([(
                Atom::from("tag"),
                InductArm::new(
                    Scope::close(Many(names.len()), &names, body),
                    binders.iter().map(|(mark, _)| *mark).collect(),
                ),
            )]),
            default: None,
        },
    })
    .into()
}

// The marks of the one arm an elaborated match over `Tagged` holds.
fn arm_marks(term: &Term) -> Vec<Plicity> {
    let Subterm::Match(Match {
        cases: Cases::Induct { cases, .. },
        ..
    }) = &**term
    else {
        panic!("expected an inductive match, got {term:?}");
    };
    cases[0].1.plicities().to_vec()
}

// An arm writes the plain payloads and leaves the hidden one out, as the call that builds the value does. The elaborated arm binds every payload under the constructor's marks, so it is elaborated again unchanged.
#[test]
fn an_arm_leaves_a_hidden_payload_out() {
    let mut context = context();
    register_tagged(&mut context);
    let x = context.fresh(Some("x"));
    let term = match_tagged(&mut context, &[(Plicity::Explicit, &x)], Term::free_var(&x));

    let (elaborated, type_) = elaborate(&mut context, &term, Mode::Infer).unwrap();
    assert_eq!(type_, nat());
    assert_eq!(
        arm_marks(&elaborated),
        [Plicity::Implicit, Plicity::Explicit]
    );

    let (again, _) = elaborate(&mut context, &elaborated, Mode::Infer).unwrap();
    assert_eq!(again, elaborated);
}

// A hidden payload is written under its mark, ahead of the plain payload it precedes, where the arm names it.
#[test]
fn an_arm_names_a_hidden_payload_under_its_mark() {
    let mut context = context();
    register_tagged(&mut context);
    let n = context.fresh(Some("n"));
    let x = context.fresh(Some("x"));
    let term = match_tagged(
        &mut context,
        &[(Plicity::Implicit, &n), (Plicity::Explicit, &x)],
        Term::free_var(&n),
    );

    let (elaborated, type_) = elaborate(&mut context, &term, Mode::Infer).unwrap();
    assert_eq!(type_, nat());
    assert_eq!(
        arm_marks(&elaborated),
        [Plicity::Implicit, Plicity::Explicit]
    );
}

// An arm's refusals are a lambda's: plain binders are counted against plain payloads, a mark of the other kind is out of order, and a marked binder with no hidden payload left stands at a plain payload or at nothing.
#[test]
fn an_arm_that_misaligns_is_refused_as_a_lambda_is() {
    let mut context = context();
    register_tagged(&mut context);
    let n = context.fresh(Some("n"));
    let x = context.fresh(Some("x"));
    let mut refusal = |binders: &[(Plicity, &Free)]| {
        let term = match_tagged(&mut context, binders, nat_lit(0));
        elaborate(&mut context, &term, Mode::Infer).unwrap_err()
    };

    assert!(matches!(
        refusal(&[(Plicity::Explicit, &n), (Plicity::Explicit, &x)]),
        Error::CtorArityMismatch {
            expected: 1,
            got: 2,
            ..
        }
    ));
    assert!(matches!(
        refusal(&[]),
        Error::CtorArityMismatch {
            expected: 1,
            got: 0,
            ..
        }
    ));
    assert!(matches!(
        refusal(&[(Plicity::Witness, &n), (Plicity::Explicit, &x)]),
        Error::HiddenMemberOutOfOrder {
            written: Plicity::Witness,
            slot: Plicity::Implicit,
            ..
        }
    ));
    assert!(matches!(
        refusal(&[(Plicity::Implicit, &n), (Plicity::Implicit, &x)]),
        Error::BinderPlicityMismatch {
            site: BinderSite::Payload { .. },
            position: 2,
            expected: Plicity::Explicit,
            written: Plicity::Implicit,
            ..
        }
    ));
    assert!(matches!(
        refusal(&[(Plicity::Explicit, &x), (Plicity::Implicit, &n)]),
        Error::HiddenMemberWithoutSlot {
            written: Plicity::Implicit
        }
    ));
}

// A structure of the fields `{ @n : Nat, x : Nat }` — a hidden field ahead of a plain one, the least telescope a written position is counted over. A concept's edge ahead of its first method is the same shape.
fn register_marked(context: &mut Context) -> Term {
    let hidden = context.fresh(Some("n"));
    let plain = context.fresh(Some("x"));
    context
        .register_struct(
            &nominal("Marked"),
            StructDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::build_marked(
                    [
                        (Plicity::Implicit, hidden, nat()),
                        (Plicity::Explicit, plain, nat()),
                    ],
                    (),
                )),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();

    Term::from(Subterm::StructType(StructType {
        name: nominal("Marked"),
        universes: Vec::new(),
        params: Vec::new(),
    }))
}

// A written position counts the plain fields, so `v.0` is the first of them and the hidden field ahead of it is reached by its label. The projection is rebuilt as the slot, which is what it reads as when elaborated again.
#[test]
fn a_written_position_counts_the_plain_fields() {
    let mut context = context();
    let marked = register_marked(&mut context);
    let value = context.fresh(Some("v"));
    context.assume(&value, &marked);
    let written = |position| Term::proj_position(Term::free_var(&value), position);
    let slot = |index| Term::proj(Term::free_var(&value), index);

    let (elaborated, type_) = elaborate(&mut context, &written(0), Mode::Infer).unwrap();
    assert_eq!((&elaborated, &type_), (&slot(1), &nat()));
    let (again, _) = elaborate(&mut context, &elaborated, Mode::Infer).unwrap();
    assert_eq!(again, slot(1));

    let labelled = Term::proj_label(Term::free_var(&value), "n");
    let (elaborated, _) = elaborate(&mut context, &labelled, Mode::Infer).unwrap();
    assert_eq!(elaborated, slot(0));

    // Past the plain fields, counted as a reader counts them.
    assert!(matches!(
        elaborate(&mut context, &written(1), Mode::Infer),
        Err(Error::TupleIndexOutOfBounds { index: 1, arity: 1 })
    ));
}

// A structure value with a field per slot and no entries is the normal form: every slot in order, the hidden one included, read as the literal written in full and so elaborated again unchanged. A literal as written carries its entries, and a plain one never fills a hidden slot.
#[test]
fn a_structure_value_in_normal_form_is_elaborated_again_unchanged() {
    let mut context = context();
    let marked = register_marked(&mut context);
    let whole = Term::struct_(
        nominal("Marked"),
        Vec::<Term>::new(),
        [nat_lit(3), nat_lit(5)],
    );

    let (elaborated, type_) = elaborate(&mut context, &whole, Mode::Infer).unwrap();
    assert_eq!((&elaborated, &type_), (&whole, &marked));
    let (again, _) = elaborate(&mut context, &elaborated, Mode::Infer).unwrap();
    assert_eq!(again, whole);

    let positional = Term::struct_entries(
        nominal("Marked"),
        Vec::<Term>::new(),
        [
            (StructEntry::Field(None), nat_lit(3)),
            (StructEntry::Field(None), nat_lit(5)),
        ],
    );
    assert!(matches!(
        elaborate(&mut context, &positional, Mode::Infer),
        Err(Error::WrongNumberOfFields {
            expected: 1,
            got: 2,
            ..
        })
    ));
}

// `Marked { <entries> }`, as lowering hands a written literal over.
fn marked_literal<const N: usize>(entries: [(StructEntry, Term); N]) -> Term {
    Term::struct_entries(nominal("Marked"), Vec::<Term>::new(), entries)
}

// A literal writes a hidden field under `@`, named or alone in its run, and the value it builds is the normal form, every slot in order.
#[test]
fn a_literal_writes_a_hidden_field_under_its_mark() {
    let mut context = context();
    register_marked(&mut context);
    let whole = Term::struct_(
        nominal("Marked"),
        Vec::<Term>::new(),
        [nat_lit(3), nat_lit(5)],
    );

    for hidden in [
        StructEntry::Implicit(None),
        StructEntry::Implicit(Some("n".to_string())),
    ] {
        let literal =
            marked_literal([(hidden, nat_lit(3)), (StructEntry::Field(None), nat_lit(5))]);
        let (elaborated, _) = elaborate(&mut context, &literal, Mode::Infer).unwrap();
        assert_eq!(elaborated, whole);
    }
}

// A hidden field left out is no entry the literal lacks: its slot is filled as a call's omitted `@` argument is — here by a metavariable, nothing having determined it — and the plain field is counted against the plain fields alone.
#[test]
fn a_literal_leaves_a_hidden_field_out() {
    let mut context = context();
    register_marked(&mut context);
    let literal = marked_literal([(StructEntry::Field(None), nat_lit(5))]);

    let (elaborated, _) = elaborate(&mut context, &literal, Mode::Infer).unwrap();
    let Subterm::Struct(Struct {
        fields, entries, ..
    }) = &*elaborated
    else {
        panic!("expected a structure value, got {elaborated:?}");
    };
    assert!(entries.is_empty());
    assert!(matches!(&*fields[0], Subterm::Metavar(_)));
    assert_eq!(fields[1], nat_lit(5));
}

// A hidden entry is held to its run and to the name of the field it fills, and `use` is a concept's mark alone.
#[test]
fn a_hidden_entry_that_misaligns_is_refused() {
    let mut context = context();
    register_marked(&mut context);
    let mut refusal = |entries: [(StructEntry, Term); 2]| {
        elaborate(&mut context, &marked_literal(entries), Mode::Infer).unwrap_err()
    };

    assert!(matches!(
        refusal([
            (StructEntry::Field(None), nat_lit(5)),
            (StructEntry::Implicit(None), nat_lit(3)),
        ]),
        Error::HiddenMemberWithoutSlot {
            written: Plicity::Implicit
        }
    ));
    assert!(matches!(
        refusal([
            (StructEntry::Implicit(Some("m".to_string())), nat_lit(3)),
            (StructEntry::Field(None), nat_lit(5)),
        ]),
        Error::UnknownStructField { label, .. } if label == "@m"
    ));
    assert!(matches!(
        refusal([
            (StructEntry::Use, nat_lit(3)),
            (StructEntry::Field(None), nat_lit(5)),
        ]),
        Error::UseEntryOutsideConcept { .. }
    ));
}

// A spread copies the plain fields and never a hidden one: left out it is inferred anew, and written it is held to its field as in any literal.
#[test]
fn a_spread_never_copies_a_hidden_field() {
    let mut context = context();
    let marked = register_marked(&mut context);
    let value = context.fresh(Some("v"));
    context.assume(&value, &marked);
    let base = || (StructEntry::Spread, Term::free_var(&value));

    for literal in [
        marked_literal([base()]),
        marked_literal([base(), (StructEntry::Implicit(None), nat_lit(7))]),
        marked_literal([
            base(),
            (StructEntry::Implicit(Some("n".to_string())), nat_lit(7)),
        ]),
    ] {
        let (_, type_) = elaborate(&mut context, &literal, Mode::Infer).unwrap();
        assert_eq!(type_, marked);
    }

    let misnamed = marked_literal([
        base(),
        (StructEntry::Implicit(Some("m".to_string())), nat_lit(7)),
    ]);
    assert!(matches!(
        elaborate(&mut context, &misnamed, Mode::Infer),
        Err(Error::UnknownStructField { label, .. }) if label == "@m"
    ));
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
                variances: Vec::new(),
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
        result: MatchResult::Family(Scope::close(Many(2), &[&index, &motive], nat())),
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
        result: MatchResult::Family(Term::match_motive_written(Term::func(
            [(motive, flag_type(nat_lit(1)))],
            nat(),
        ))),
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
        universes: Vec::new(),
        params: vec![Term::free_var(&loop_), Term::type_ground()],
    }));

    let shape = monad_shape(&mut context, &region);

    assert!(shape.is_err_and(|spent| spent.is_exhausted()));
}
