//! Structural conversion: alpha equivalence, plicity, matches, and the recursive heads that converge coinductively or spend the budget.

use super::test_support::*;
use curios_core::*;
use {
    crate::*,
    curios_num::Integer,
    curios_utilities::{Grain, PackedBin, Plicity, Qualifier},
};

/// The value fast path's sound population, pinned together with the license that makes it sound.
///
/// One partial computation at two fresh instances — `bin_get` past the end of an empty `Bin`, whose evaluation refuses — must convert *without being evaluated*: deciding that two spellings of one computation agree may not cost the computation. That is the verdict-and-method half, and any rewrite of `identify_universe_levels` that starts reducing here turns the `Ok(true)` below into an error.
///
/// The second assertion is the license, and it is what separates the sound mechanism from the one `value_conversion_does_not_identify_distinct_type_payloads` records falling: acceptance must come *with* the level identification that justifies it — after the call the two metas are one level, so the accepted goal is literally reflexive. The old projection comparison accepted while leaving the metas untouched, acceptance with no residue, and this assertion was red under it (mutation-checked by restoring that comparison). A deliberately more complete future rule — say, accepting rigid-distinct instances on a head whose levels provably never reach a value — would also fail this assertion, and revisiting it then is a deliberate expectation change, not a regression.
#[test]
fn value_conversion_does_not_unfold_terms_differing_only_by_universes() {
    let mut context = context();
    let partial = context.fresh(Some("partial"));
    let ignored = context.fresh(Some("ignored"));
    context.define(
        &partial,
        &Term::func(
            [(ignored.clone(), Term::intrinsic(Intrinsic::NatType))],
            Term::intrinsic(Intrinsic::bin_get(
                Grain::X,
                Term::intrinsic(Intrinsic::Bin(
                    Grain::X,
                    PackedBin::from_bytes(Vec::<u8>::new()),
                )),
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
            Term::universe_inst(Term::free_var(&partial), vec![Level::meta(meta)]),
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

/// The value-conversion fast path used to read `project_erased_universes` as a quotient by definitional equality, and its premise was the one `documentation/soundness/what-the-kernel-consults/the-refinement-key.md` refuted for the kernel's copy of the same reading: a `Type` payload embeds a level *in a term*, so two spellings the projection identifies can be two genuinely different values. `wrap(Type 0)` and `wrap(Type 1)` at a relevant `E` are the direct witness — two constructor values differing in a relevant payload, which the projection collapsed to one spelling and accepted with no residue for the declaration boundary to refuse.
///
/// Verified while the hole was open: on the identical goal, seeded with the identical declaration, this side's `convert` answered true and `curios_cert::convert` answered false — the goal-level conversion differential `documentation/soundness/across-the-perimeter.md` named as missing, put to the one rule the two sides spelled most differently. The false acceptance was fenced twice — no surface program reaches it, a body-carried level being minimized rather than generalized (`curios`'s `tests::universes` holds that construction), and a module elaborated through it would still have been refused by the kernel on the compile path — but each fence is a fact about the neighbours, not about the rule.
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
            InductParam {
                telescope: Telescope::build(
                    [(Free::local(0, Some("T")), Term::type_at(two))],
                    Vec::new(),
                ),
                plicities: vec![Plicity::Explicit],
            },
        )],
        result_sort: Term::type_at(three),
        module: Qualifier::from(["E"]),
        rep_public: true,
        polarities: Vec::new(),
    };

    let e_type = Term::induct_type(e.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let wrap = |level: Level| {
        Term::variant(
            e.clone(),
            Vec::<Term>::new(),
            "wrap",
            vec![Term::type_at(level)],
        )
    };
    let one = Level::zero().succ().expect("level zero has a successor");

    // The differential's fixed side: the kernel, handed the identical declaration and the identical goal, refuses it at the payloads.
    let mut kernel = curios_cert::Kernel::new(100_000, crate::SYNTAX);
    kernel.set_local_floor(1_000);
    kernel.declare_induct(&e, &declaration);
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

    let this = Term::func_type([(x.clone(), Term::type_ground())], Term::free_var(&x));

    let that = Term::func_type([(y.clone(), Term::type_ground())], Term::free_var(&y));

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

#[test]
fn func_type_distinguishes_plicity() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    // Three telescopes with identical domains and results, differing only in the one binder's plicity.
    let explicit = Term::func_type([(x.clone(), Term::type_ground())], Term::type_ground());
    let implicit = Term::func_type_marked(
        [(Plicity::Implicit, x.clone(), Term::type_ground())],
        Term::type_ground(),
    );
    let witness = Term::func_type_marked(
        [(Plicity::Witness, x.clone(), Term::type_ground())],
        Term::type_ground(),
    );

    // Plicity is part of function-type identity: every pairwise mix is non-convertible even though the dependent telescopes agree.
    assert_eq!(conv(&mut context, &explicit, &implicit), Ok(false));
    assert_eq!(conv(&mut context, &explicit, &witness), Ok(false));
    assert_eq!(conv(&mut context, &implicit, &witness), Ok(false));

    // Same plicity, alpha-renamed binder: still convertible.
    let implicit_y = Term::func_type_marked(
        [(Plicity::Implicit, y.clone(), Term::type_ground())],
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
        let binder = binder.clone();
        Term::induct_match(
            Term::free_var(&r),
            Some(motive_label),
            Term::intrinsic(Intrinsic::NatType),
            [
                ("none", Vec::<Free>::new(), nat(0)),
                ("some", vec![binder.clone()], Term::free_var(&binder)),
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
            ("some", vec![x.clone()], Term::free_var(&x)),
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

// === Folded recursive calls (match-guarded delta) ===========================

/// `λx. match x | none() => <none_value> | some(p) => head(p) end` — stuck at a neutral scrutinee, with a `head`-call in an arm to make unfolding self-feeding.
fn recursive_matcher(context: &mut Context, head: &Free, none_value: usize) -> Term {
    let scrutinee = context.fresh(Some("x"));
    let motive = context.fresh(Some("m"));
    let payload = context.fresh(Some("p"));
    Term::func(
        [(scrutinee.clone(), Term::intrinsic(Intrinsic::NatType))],
        Term::induct_match(
            Term::free_var(&scrutinee),
            Some(&motive),
            Term::intrinsic(Intrinsic::NatType),
            [
                ("none", Vec::<Free>::new(), nat(none_value)),
                (
                    "some",
                    vec![payload.clone()],
                    Term::apply(Term::free_var(head), [Term::free_var(&payload)]),
                ),
            ],
        ),
    )
}

#[test]
fn folded_recursive_call_against_its_unfolding() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let a = context.fresh(Some("a"));
    let m = context.fresh(Some("m"));
    let p = context.fresh(Some("p"));
    let z = context.fresh(Some("z"));
    let body = recursive_matcher(&mut context, &f, 0);
    context.define(&f, &body, None);

    let folded = Term::apply(Term::free_var(&f), [Term::free_var(&a)]);

    // The literal stuck body, spelled reducibly (the η-redex around `p`) so the sides are not syntactically equal: the folded call meets a non-apply shape, lazy delta unfolds it once, and the two stuck matches compare structurally.
    let unfolded = Term::induct_match(
        Term::free_var(&a),
        Some(&m),
        Term::intrinsic(Intrinsic::NatType),
        [
            ("none", Vec::<Free>::new(), nat(0)),
            (
                "some",
                vec![p.clone()],
                Term::apply(
                    Term::free_var(&f),
                    [Term::apply(
                        func([&z], Term::free_var(&z)),
                        [Term::free_var(&p)],
                    )],
                ),
            ),
        ],
    );

    assert_eq!(conv(&mut context, &folded, &unfolded), Ok(true));
}

#[test]
fn same_recursive_head_compares_spines() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let a = context.fresh(Some("a"));
    let z = context.fresh(Some("z"));
    let b = context.fresh(Some("b"));
    let body = recursive_matcher(&mut context, &f, 0);
    context.define(&f, &body, None);

    // Convertible (but not syntactically equal) spines: true.
    let this = Term::apply(Term::free_var(&f), [Term::free_var(&a)]);
    let that = Term::apply(
        Term::free_var(&f),
        [Term::apply(
            func([&z], Term::free_var(&z)),
            [Term::free_var(&a)],
        )],
    );
    assert_eq!(conv(&mut context, &this, &that), Ok(true));

    // Mismatching spines: committal false, no unfolding retry.
    let other = Term::apply(Term::free_var(&f), [Term::free_var(&b)]);
    assert_eq!(conv(&mut context, &this, &other), Ok(false));
}

/// `body` is built over the member's own parameter, which this helper mints — a caller that wants the identity function has to be handed that binder, not mint a like-named one of its own.
fn structural_rec_proj(context: &mut Context, body: impl FnOnce(&Free) -> Term) -> Term {
    let member = context.fresh(Some("f"));
    let parameter = context.fresh(Some("x"));
    let body = body(&parameter);
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let rec = Term::rec(
        [(
            member.clone(),
            Term::func_type([(parameter.clone(), nat_type.clone())], nat_type),
            Term::func([(parameter, Term::intrinsic(Intrinsic::NatType))], body),
        )],
        Term::free_var(&member),
    );
    let Subterm::Rec(rec) = Term::unwrap_or_clone(rec) else {
        unreachable!()
    };
    Term::rec_proj(rec.group, 0)
}

#[test]
fn same_structural_recursive_head_does_not_assume_injective_spines() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let constant = structural_rec_proj(&mut context, |_| nat(0));
    let this = Term::apply(constant.clone(), [Term::free_var(&a)]);
    let that = Term::apply(constant, [Term::free_var(&b)]);
    assert_eq!(conv(&mut context, &this, &that), Ok(true));

    let identity = structural_rec_proj(&mut context, Term::free_var);
    let this = Term::apply(identity.clone(), [Term::free_var(&a)]);
    let that = Term::apply(identity, [Term::free_var(&b)]);
    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn distinct_recursive_heads_with_identical_bodies_converge_coinductively() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let g = context.fresh(Some("g"));
    let a = context.fresh(Some("a"));
    // Identical bodies, distinct names: each round unfolds both sides to the same stuck match and re-opens its `some` arm at a fresh binder — the recurrence differs from the previous round only in that opening entropy, so the canonicalized history recognizes the cycle and assumes it. No finite disagreement exists: the functions are bisimilar. Before goal canonicalization this pair spun to `Err(Exhausted)`.
    let body = recursive_matcher(&mut context, &f, 0);
    context.define(&f, &body, None);
    let body = recursive_matcher(&mut context, &g, 0);
    context.define(&g, &body, None);

    let this = Term::apply(Term::free_var(&f), [Term::free_var(&a)]);
    let that = Term::apply(Term::free_var(&g), [Term::free_var(&a)]);
    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

#[test]
fn distinct_recursive_heads_with_differing_bodies_is_false() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let g = context.fresh(Some("g"));
    let a = context.fresh(Some("a"));
    // The `none` arms disagree: the first unfolding round surfaces the finite disagreement on a sibling goal, well before the budget runs out.
    let body = recursive_matcher(&mut context, &f, 0);
    context.define(&f, &body, None);
    let body = recursive_matcher(&mut context, &g, 1);
    context.define(&g, &body, None);

    let this = Term::apply(Term::free_var(&f), [Term::free_var(&a)]);
    let that = Term::apply(Term::free_var(&g), [Term::free_var(&a)]);
    assert_eq!(conv(&mut context, &this, &that), Ok(false));
}

#[test]
fn growing_recursive_unfolding_spends_the_budget() {
    // Its own small budget: the subject here is that the budget stops an unfolding that grows without bound, and this shape drives native recursion deep enough to overflow the stack somewhere above 20,000 steps — well under the shipped default. See the note in `Context::new`.
    let mut context = Context::new(20_000, crate::SYNTAX);
    let x = context.fresh(Some("x"));
    let m = context.fresh(Some("m"));
    let s = context.fresh(Some("s"));
    let p = context.fresh(Some("p"));
    let f = context.fresh(Some("f"));
    let g = context.fresh(Some("g"));
    let a = context.fresh(Some("a"));
    // `λx. match x | none() => head(s(x)) | some(p) => 0 end` never recurs — every unfolding round's arm goal is structurally new, one more `s` on the folded argument — so no cycle exists to detect and the comparison rightly spends the budget: the accepted cost of fully general recursion. (The growth rides the match arm so each round refolds and returns to the drain queue; bare `f = λx. f(s(x))` growth would nest inside one `reduce` call instead.)
    let growing = |head: &Free| {
        Term::func(
            [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
            Term::induct_match(
                Term::free_var(&x),
                Some(&m),
                Term::intrinsic(Intrinsic::NatType),
                [
                    (
                        "none",
                        Vec::<Free>::new(),
                        Term::apply(
                            Term::free_var(head),
                            [Term::apply(Term::free_var(&s), [Term::free_var(&x)])],
                        ),
                    ),
                    ("some", vec![p.clone()], nat(0)),
                ],
            ),
        )
    };
    let f_body = growing(&f);
    let g_body = growing(&g);
    context.define(&f, &f_body, None);
    context.define(&g, &g_body, None);

    let this = Term::apply(Term::free_var(&f), [Term::free_var(&a)]);
    let that = Term::apply(Term::free_var(&g), [Term::free_var(&a)]);
    assert!(conv(&mut context, &this, &that).is_err_and(|spent| spent.is_exhausted()));
}

#[test]
fn recursive_values_are_bisimilar() {
    let mut context = context();
    let xs = context.fresh(Some("xs"));
    let ys = context.fresh(Some("ys"));
    // Two distinct recursive value definitions unfolding to the same constructor shape: the payload goal recurs exactly (no openings involved), history cuts it, and the streams are equal coinductively.
    let stream = |name: &Free| {
        Term::variant(
            nominal("E"),
            Vec::<Term>::new(),
            "cons",
            [nat(1), Term::free_var(name)],
        )
    };
    context.define(&xs, &stream(&xs), None);
    context.define(&ys, &stream(&ys), None);

    assert_eq!(
        conv(&mut context, &Term::free_var(&xs), &Term::free_var(&ys)),
        Ok(true)
    );
}

#[test]
fn folded_recursive_call_against_neutral_head_is_false() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let a = context.fresh(Some("a"));
    let h = context.fresh(Some("h"));
    let body = recursive_matcher(&mut context, &f, 0);
    context.define(&f, &body, None);

    // The recursive side unfolds to its stuck match, the neutral side cannot unfold at all: a structural mismatch, decided well within the budget.
    let this = Term::apply(Term::free_var(&f), [Term::free_var(&a)]);
    let neutral = Term::apply(Term::free_var(&h), [Term::free_var(&a)]);
    assert_eq!(conv(&mut context, &this, &neutral), Ok(false));
}

// === `Rec` (local groups, still a term-level construct — no lambda-lifting
// in this design) ============================================================

#[test]
fn rec_is_alpha_equivalent() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    let this = Term::rec(
        vec![(x.clone(), Term::type_ground(), Term::free_var(&x))],
        Term::free_var(&x),
    );

    let that = Term::rec(
        vec![(y.clone(), Term::type_ground(), Term::free_var(&y))],
        Term::free_var(&y),
    );

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}
