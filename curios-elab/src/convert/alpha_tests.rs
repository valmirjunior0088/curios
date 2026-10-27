//! Structural conversion: alpha equivalence, plicity and marks, and matches and intrinsics compared part by part.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Free, InductDecl, InductParam, Intrinsic, Level, ReduceError, Subterm, Telescope,
        Term, UniverseContext, UniverseMetaId, UniverseRole,
    },
    curios_num::{Binary, Integer},
    curios_utilities::{Plicity, Qualifier},
};

/// The value fast path's sound population, pinned together with the license that makes it sound.
///
/// One partial computation at two fresh instances — `bytes_get` past the end of an empty `Bytes`, whose evaluation refuses — must convert *without being evaluated*: deciding that two spellings of one computation agree may not cost the computation. That is the verdict-and-method half, and any rewrite of `identify_universe_levels` that starts reducing here turns the `Ok(true)` below into an error.
///
/// The second assertion is the license, and it is what separates the sound mechanism from a projection comparison, which `value_conversion_does_not_identify_distinct_type_payloads` records the cost of: acceptance must come *with* the level identification that justifies it — after the call the two metas are one level, so the accepted goal is literally reflexive. A projection comparison accepts while leaving the metas untouched, acceptance with no residue, and fails this assertion (mutation-checked). A deliberately more complete future rule — say, accepting rigid-distinct instances on a head whose levels provably never reach a value — would also fail this assertion, and revisiting it then is a deliberate expectation change, not a regression.
#[test]
fn value_conversion_does_not_unfold_terms_differing_only_by_universes() {
    let mut context = context();
    let partial = context.fresh(Some("partial"));
    let ignored = context.fresh(Some("ignored"));
    context.define(
        &partial,
        &Term::func(
            [(ignored, Term::intrinsic(Intrinsic::NatType))],
            Term::intrinsic(Intrinsic::bytes_get(
                Term::intrinsic(Intrinsic::Bytes(Binary::from_bytes(Vec::<u8>::new()))),
                nat(0),
                qed(),
            )),
        ),
        None,
    );
    let u0 = context.universes_mut().fresh(UniverseRole::Flexible, None);
    let u1 = context.universes_mut().fresh(UniverseRole::Flexible, None);
    let applied = |meta: UniverseMetaId| {
        Term::apply(
            Term::instance_of(&partial, vec![Level::meta(meta)]),
            [nat(0)],
        )
    };

    assert_eq!(
        convert(
            &mut context,
            &Term::intrinsic(Intrinsic::ByteType),
            &applied(u0),
            &applied(u1),
        ),
        Ok(true)
    );

    let identified = context
        .universes()
        .zonk(&Level::meta(u0))
        .expect("levels zonk")
        == context
            .universes()
            .zonk(&Level::meta(u1))
            .expect("levels zonk");
    assert!(
        identified,
        "acceptance must carry the level identification that licenses it",
    );
}

/// Reading `project_erased_universes` as a quotient by definitional equality rests on the premise `documentation/design/soundness/case-equations-and-their-key.md` refutes for the kernel's copy of the same reading: a `Type` payload embeds a level *in a term*, so two spellings the projection identifies can be two genuinely different values. `wrap(Type 0)` and `wrap(Type 1)` at a relevant `E` are the direct witness — two constructor values differing in a relevant payload, which the projection would collapse to one spelling and accept with no residue for the declaration boundary to refuse.
///
/// The fixture puts the identical goal, seeded with the identical declaration, to both checkers, each asked by itself (`documentation/design/soundness/conversion-is-one-relation-in-both-checkers.md` says why). A false acceptance here would be fenced twice — no surface program reaches it, a body-carried level being minimized rather than generalized (`curios`'s `tests::universes` holds that construction), and the kernel would still refuse a module elaborated through it on the compile path — but each fence is a fact about the neighbours, not about the rule.
///
/// Under `identify_universe_levels` the pair of unequal ground levels declines the fast path with nothing inserted, and the structural payload comparison then refuses the goal as the universe inconsistency `1 ≤ 0` — which is what the assertion pins: an `Err` naming the universe arithmetic, where the kernel's boolean spells the same refusal as `Ok(false)`. The control is this file's first fixture, `value_conversion_does_not_unfold_terms_differing_only_by_universes`: the fast path's sound population — one computation at two identifiable instances — must keep converting without being evaluated.
#[test]
fn value_conversion_does_not_identify_distinct_type_payloads() {
    let e = nominal("E");
    let two = Level::zero()
        .succ()
        .and_then(|one| one.succ())
        .expect("small levels have successors");
    let three = two.clone().succ().expect("small levels have successors");
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("wrap"),
            InductParam::new(
                Telescope::build(
                    [(Free::local(0, Some("T")), Term::type_at(two))],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_at(three),
        module: Qualifier::from(["E"]),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let e_type = Term::induct_type(e, Vec::<Term>::new(), Vec::<Term>::new());
    let wrap =
        |level: Level| Term::variant(e, Vec::<Term>::new(), "wrap", vec![Term::type_at(level)]);
    let one = Level::zero().succ().expect("level zero has a successor");

    // The differential's fixed side: the kernel, handed the identical declaration and the identical goal, refuses it at the payloads.
    let mut kernel = curios_cert::Kernel::new(100_000, SYNTAX);
    kernel
        .declare_induct(&e, &declaration)
        .expect("a name is declared once");
    assert!(
        matches!(
            curios_cert::convert(
                &mut kernel,
                &e_type,
                &wrap(Level::zero()),
                &wrap(one.clone())
            ),
            Ok(false)
        ),
        "the kernel refuses two constructor values differing in a relevant Type payload",
    );

    let mut context = context();
    context
        .register_induct(&e, declaration)
        .expect("the family registers");
    assert!(
        matches!(
            convert(&mut context, &e_type, &wrap(Level::zero()), &wrap(one)),
            Err(ReduceError::Universe(_))
        ),
        "two constructor values differing in a relevant Type payload are refused by the universe arithmetic",
    );
}

/// The elaborator's half of the kernel's fixture: like terms convert, and a sum against a literal clashes, both in the fold.
#[test]
fn like_terms_convert_and_a_stuck_sum_clashes_with_a_literal() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let sum = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), Term::free_var(&x)));
    let scaled = Term::intrinsic(Intrinsic::nat_mul(nat(2), Term::free_var(&x)));
    assert_eq!(conv(&mut context, &sum, &scaled), Ok(true));
    let stuck = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(1)));
    assert_eq!(conv(&mut context, &stuck, &nat(0)), Ok(false));
}

#[test]
fn func_type_is_alpha_equivalent() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    let this = Term::func_type([(x, Term::type_ground())], Term::free_var(&x));

    let that = Term::func_type([(y, Term::type_ground())], Term::free_var(&y));

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn func_is_alpha_equivalent() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    let this = func([&x], Term::free_var(&x));

    let that = func([&y], Term::free_var(&y));

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

/// Plicity is part of a function's identity, a literal's as a type's: two literals under one mark whose bodies differ by a β-step are one function, and two apart in a mark are two. The kernel's twin keeps the name.
#[test]
fn plicity_distinguishes_two_function_literals() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    let identity = Term::func_marked(
        [(Plicity::Implicit, n, nat_type.clone())],
        Term::free_var(&n),
    );
    let forwarded = Term::func_marked(
        [(Plicity::Implicit, n, nat_type.clone())],
        Term::apply(
            Term::func([(x, nat_type.clone())], Term::free_var(&x)),
            [Term::free_var(&n)],
        ),
    );
    assert_eq!(conv(&mut context, &identity, &forwarded), Ok(true));

    let shown = Term::func([(n, nat_type)], Term::free_var(&n));
    assert_eq!(conv(&mut context, &identity, &shown), Ok(false));
}

#[test]
fn plicity_distinguishes_two_function_types() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    // Three telescopes with identical domains and results, differing only in the one binder's plicity.
    let explicit = Term::func_type([(x, Term::type_ground())], Term::type_ground());
    let implicit = Term::func_type_marked(
        [(Plicity::Implicit, x, Term::type_ground())],
        Term::type_ground(),
    );
    let witness = Term::func_type_marked(
        [(Plicity::Witness, x, Term::type_ground())],
        Term::type_ground(),
    );

    // Plicity is part of function-type identity: every pairwise mix is non-convertible even though the dependent telescopes agree.
    assert_eq!(conv(&mut context, &explicit, &implicit), Ok(false));
    assert_eq!(conv(&mut context, &explicit, &witness), Ok(false));
    assert_eq!(conv(&mut context, &implicit, &witness), Ok(false));

    // Same plicity, alpha-renamed binder: still convertible.
    let implicit_y = Term::func_type_marked(
        [(Plicity::Implicit, y, Term::type_ground())],
        Term::type_ground(),
    );
    assert_eq!(conv(&mut context, &implicit, &implicit_y), Ok(true));
}

#[test]
fn inductive_match_compares_cases_and_motive() {
    let mut context = context();
    let r = context.fresh(Some("r"));
    let m = context.fresh(Some("m"));
    let n = context.fresh(Some("n"));
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    let make = |motive_label: &Free, binder: &Free| {
        let binder = *binder;
        Term::induct_match(
            Term::free_var(&r),
            Some(motive_label),
            Term::intrinsic(Intrinsic::NatType),
            [
                ("none", Vec::<Free>::new(), nat(0)),
                ("some", vec![binder], Term::free_var(&binder)),
            ],
        )
    };

    // Alpha-equivalent binders and motive labels are convertible.
    assert_eq!(conv(&mut context, &make(&m, &x), &make(&n, &y)), Ok(true));

    let different = Term::induct_match(
        Term::free_var(&r),
        Some(&m),
        Term::intrinsic(Intrinsic::NatType),
        [
            ("none", Vec::<Free>::new(), nat(1)),
            ("some", vec![x], Term::free_var(&x)),
        ],
    );

    assert_eq!(conv(&mut context, &make(&m, &x), &different), Ok(false));
}

#[test]
fn inductive_match_compares_default() {
    let mut context = context();
    let r = context.fresh(Some("r"));
    let m = context.fresh(Some("m"));

    let with_default = |d: usize| {
        Term::induct_match_default(
            Term::free_var(&r),
            Some(&m),
            Term::intrinsic(Intrinsic::NatType),
            [("none", Vec::<Free>::new(), nat(0))],
            nat(d),
        )
    };

    // Same enumerated arm and same default: convertible.
    assert_eq!(
        conv(&mut context, &with_default(9), &with_default(9)),
        Ok(true)
    );

    // Same arm, different default body: not convertible — the default is a real arm, not erased provenance.
    assert_eq!(
        conv(&mut context, &with_default(9), &with_default(8)),
        Ok(false)
    );

    // A defaulted match never converts with an otherwise-identical bare one: presence of the catch-all is itself a difference.
    let bare = Term::induct_match(
        Term::free_var(&r),
        Some(&m),
        Term::intrinsic(Intrinsic::NatType),
        [("none", Vec::<Free>::new(), nat(0))],
    );
    assert_eq!(conv(&mut context, &with_default(9), &bare), Ok(false));
}

#[test]
fn intrinsic_recurses_into_operands() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    let this = func(
        [&x],
        Subterm::Intrinsic(Intrinsic::int_add(
            Term::free_var(&x),
            Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
        )),
    );

    let that = func(
        [&y],
        Subterm::Intrinsic(Intrinsic::int_add(
            Term::free_var(&y),
            Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
        )),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn intrinsic_distinguishes_operator_kind() {
    let mut context = context();
    let x = context.fresh(Some("x"));

    let this = func(
        [&x],
        Subterm::Intrinsic(Intrinsic::int_add(
            Term::free_var(&x),
            Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
        )),
    );

    let that = func(
        [&x],
        Subterm::Intrinsic(Intrinsic::int_sub(
            Term::free_var(&x),
            Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
        )),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

/// Two spines of one head converge by their arguments alone: an application is rebuilt under its binders' marks where it is elaborated, so conversion reads none. Put to both checkers under this name.
#[test]
fn applications_differing_in_a_mark_alone_convert() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    context.assume(&f, &Term::func_type([(x, nat_type.clone())], nat_type));

    let plain = Term::apply(Term::free_var(&f), [nat(1)]);
    let marked = Term::apply_marked(Term::free_var(&f), [(Plicity::Implicit, nat(1))]);

    assert_eq!(conv(&mut context, &plain, &marked), Ok(true));
}
