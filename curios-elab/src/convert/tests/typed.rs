//! What conversion reads at a term's type: an elimination and a constructor typed by their declarations, the irrelevance that fires at a computed proposition, and a comparison at below.

use {
    crate::convert::test_support::*,
    crate::*,
    curios_common::{Plicity, Qualifier},
    curios_core::{
        Atom, Cases, Constructors, Free, Global, InductDecl, Intrinsic, Level, Match, MetavarId,
        StructDecl, Subterm, Telescope, Term, UniverseContext, Universes,
    },
};

/// A stuck elimination is typed by its goal as written. So where a child is compared with no type, an elimination at a proposition converges with a variable at it and with an elimination of another scrutinee, and one at a record of units with a variable at it. The certifier's twin of this proposition shares the name.
///
/// The refusals are what the type does not decide: an elimination at a relevant type against a variable at it, and one at a proposition against a variable at another.
///
/// Mutation-checked: with no type read off an elimination the three held goals are refused.
#[test]
fn a_stuck_elimination_is_typed_by_its_goal() {
    let mut context = context();
    let (proposition, another) = (
        declare_proposition(&mut context, "P"),
        declare_proposition(&mut context, "Q"),
    );
    let empty = declare_family(&mut context, "E", Telescope::done(()), Term::type_ground());
    let empty = Term::induct_type(empty, Vec::<Term>::new(), Vec::<Term>::new());
    let unit = Term::tuple_type_unit();
    let (a, b) = (context.fresh(Some("a")), context.fresh(Some("b")));
    let units = Term::tuple_type([(a, unit.clone()), (b, unit.clone())]);

    let assumed = |context: &mut Context, hint: &str, type_: &Term| {
        let name = context.fresh(Some(hint));
        context.assume(&name, type_);

        Term::free_var(&name)
    };
    let (c, d) = (
        assumed(&mut context, "c", &empty),
        assumed(&mut context, "d", &empty),
    );
    let (p, q) = (
        assumed(&mut context, "p", &proposition),
        assumed(&mut context, "q", &another),
    );
    let r = assumed(&mut context, "r", &units);
    let n = assumed(&mut context, "n", &nat_type());

    let no_arm = || Cases::Induct {
        cases: Vec::new(),
        default: None,
        stated: None,
    };
    let eliminated = |scrutinee: &Term, goal: &Term| {
        Term::from(Subterm::Match(Match {
            head: scrutinee.clone(),
            goal: goal.clone(),
            cases: no_arm(),
        }))
    };
    let ground = Term::type_ground();
    let mut at = |this: &Term, that: &Term| convert(&mut context, &ground, this, that);

    assert_eq!(
        [
            at(&eliminated(&c, &proposition), &p),
            at(&eliminated(&c, &proposition), &eliminated(&d, &proposition)),
            at(&eliminated(&c, &units), &r),
        ],
        [Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            at(&eliminated(&c, &nat_type()), &n),
            at(&eliminated(&c, &proposition), &q),
        ],
        [Ok(false), Ok(false)]
    );
}

/// Two eliminations that state the instance they eliminate are compared by it: the two instances, and then the scrutinees at the instance, a typed position where two proofs are one unread and no lookup types either. Two variables nothing was assumed of converge there, and stay apart where the eliminations state no instance, where the two instances differ, and where one states its instance and the other none. The certifier's twin of this proposition shares the name.
#[test]
fn two_eliminations_are_compared_at_the_instance_they_state() {
    let mut context = context();
    let (proposition, another) = (
        declare_proposition(&mut context, "P"),
        declare_proposition(&mut context, "Q"),
    );
    let (p, q) = (
        Term::free_var(&context.fresh(Some("p"))),
        Term::free_var(&context.fresh(Some("q"))),
    );
    let eliminated = |proof: &Term| {
        Term::from(Subterm::Match(Match {
            head: proof.clone(),
            goal: nat_type(),
            cases: Cases::Induct {
                cases: Vec::new(),
                default: None,
                stated: None,
            },
        }))
    };
    let stating = |proof: &Term, instance: &Term| eliminated(proof).stating(instance.clone());
    let number = nat_type();
    let mut at = |this: &Term, that: &Term| convert(&mut context, &number, this, that);

    assert_eq!(
        at(&stating(&p, &proposition), &stating(&q, &proposition)),
        Ok(true)
    );
    assert_eq!(
        [
            at(&eliminated(&p), &eliminated(&q)),
            at(&stating(&p, &proposition), &stating(&q, &another)),
            at(&stating(&p, &proposition), &eliminated(&p)),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// A constructor's value is typed by its declaration: the family at the value's parameters and at the index targets its constructor states. So where a child is compared with no type, a proof a constructor builds converges with a proof a variable names and with one another constructor builds. The certifier's twin of this proposition shares the name.
///
/// The refusals: two constructors of a relevant family, a constructor's value against a variable at it, and a proof against a variable at the family's other index.
///
/// Mutation-checked: with no type read off a constructor's value the three held goals are refused, and with the index targets left out the indexed one is.
#[test]
fn a_constructors_value_is_typed_by_its_declaration() {
    let mut context = context();
    let nullary = |tags: [&str; 2]| {
        Vec::from(tags.map(|tag| {
            (
                Atom::from(tag),
                curios_core::test_support::case(0, Telescope::done(Vec::new()), Vec::new()),
            )
        }))
    };
    // `induct Or : Prop | left() | right()`, the same two at `Type`, and `induct Zero : (n : Nat) -> Prop | zero() : (0)`.
    let either = register(
        &mut context,
        "Or",
        Telescope::done(()),
        nullary(["left", "right"]),
        Term::prop(),
    );
    let two = register(
        &mut context,
        "Two",
        Telescope::done(()),
        nullary(["left", "right"]),
        Term::type_ground(),
    );
    let index = context.fresh(Some("n"));
    let zero = register(
        &mut context,
        "Zero",
        Telescope::build([(index, nat_type())], ()),
        Vec::from([(
            Atom::from("zero"),
            curios_core::test_support::case(0, Telescope::done(vec![nat(0)]), Vec::new()),
        )]),
        Term::prop(),
    );

    let built =
        |name: Global, tag: &str| Term::variant(name, Vec::<Term>::new(), tag, Vec::<Term>::new());
    let assumed = |context: &mut Context, hint: &str, type_: &Term| {
        let name = context.fresh(Some(hint));
        context.assume(&name, type_);

        Term::free_var(&name)
    };
    let unindexed = |name: Global| Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let p = assumed(&mut context, "p", &unindexed(either));
    let t = assumed(&mut context, "t", &unindexed(two));
    let at_zero = assumed(
        &mut context,
        "z",
        &Term::induct_type(zero, Vec::<Term>::new(), [nat(0)]),
    );
    let at_one = assumed(
        &mut context,
        "o",
        &Term::induct_type(zero, Vec::<Term>::new(), [nat(1)]),
    );

    let ground = Term::type_ground();
    let mut at = |this: &Term, that: &Term| convert(&mut context, &ground, this, that);

    assert_eq!(
        [
            at(&built(either, "left"), &p),
            at(&built(either, "left"), &built(either, "right")),
            at(&built(zero, "zero"), &at_zero),
        ],
        [Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            at(&built(two, "left"), &built(two, "right")),
            at(&built(two, "left"), &t),
            at(&built(zero, "zero"), &at_one),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
}

/// An arm's binders are opened at the types its position gives them: a constructor's arm at its telescope over the scrutinee's parameters, and a cons arm at its carrier's domains. Two eliminations whose arms differ in which of two bound proofs they hand on are one term, as are two folds over a list of proofs, one handing on the head it peeled and the other a proof in scope. The certifier's twin of this proposition shares the name.
///
/// The control for each is the same pair over a relevant family, where the two binders stay apart; and where no lookup types the scrutinee the arm's binders carry no type, under which the first pair stays apart too.
///
/// Mutation-checked: with no type recorded for an arm's binders the two held goals are refused.
#[test]
fn an_arms_binders_are_opened_at_the_types_their_constructor_gives_them() {
    let judged = |sort: Term| {
        let mut context = context();
        let held = declare_family(&mut context, "F", Telescope::done(()), sort);
        let held = Term::induct_type(held, Vec::<Term>::new(), Vec::<Term>::new());
        // `induct Pair : Type | two(a : F, b : F)`.
        let (a, b) = (context.fresh(Some("a")), context.fresh(Some("b")));
        let pair = register(
            &mut context,
            "Pair",
            Telescope::done(()),
            Vec::from([(
                Atom::from("two"),
                curios_core::test_support::case(
                    0,
                    Telescope::build([(a, held.clone()), (b, held.clone())], Vec::new()),
                    vec![Plicity::Explicit, Plicity::Explicit],
                ),
            )]),
            Term::type_ground(),
        );
        let (typed, untyped) = (context.fresh(Some("t")), context.fresh(Some("u")));
        context.assume(
            &typed,
            &Term::induct_type(pair, Vec::<Term>::new(), Vec::<Term>::new()),
        );
        let in_scope = context.fresh(Some("p"));
        context.assume(&in_scope, &held);
        let list = context.fresh(Some("l"));
        context.assume(&list, &Term::intrinsic(Intrinsic::ListType(held.clone())));

        let (x, y) = (context.fresh(Some("x")), context.fresh(Some("y")));
        let result = Term::tuple_type([(x, held.clone()), (y, nat_type())]);
        // `match t | two(a, b) => (<chosen>, 1) end`.
        let handing_on = |context: &mut Context, scrutinee: &Free, first: bool| {
            let (a, b) = (context.fresh(Some("a")), context.fresh(Some("b")));
            let chosen = match first {
                true => a,
                false => b,
            };

            Term::induct_match(
                Term::free_var(scrutinee),
                result.clone(),
                [(
                    "two",
                    vec![a, b],
                    Term::tuple([Term::free_var(&chosen), nat(1)]),
                )],
            )
        };
        let pairs = [
            (
                handing_on(&mut context, &typed, true),
                handing_on(&mut context, &typed, false),
            ),
            (
                handing_on(&mut context, &untyped, true),
                handing_on(&mut context, &untyped, false),
            ),
        ];
        // `match l | [] => (p, 0) | [h, ..t] => (<chosen>, 1) end`.
        let folding = |context: &mut Context, peeled: bool| {
            let (h, t) = (context.fresh(Some("h")), context.fresh(Some("t")));
            let chosen = match peeled {
                true => h,
                false => in_scope,
            };

            Term::list_match(
                Term::free_var(&list),
                held.clone(),
                result.clone(),
                Term::tuple([Term::free_var(&in_scope), nat(0)]),
                &[&h, &t],
                Term::tuple([Term::free_var(&chosen), nat(1)]),
            )
        };
        let folds = (folding(&mut context, true), folding(&mut context, false));
        let ground = Term::type_ground();

        [
            convert(&mut context, &ground, &pairs[0].0, &pairs[0].1),
            convert(&mut context, &ground, &folds.0, &folds.1),
            convert(&mut context, &ground, &pairs[1].0, &pairs[1].1),
        ]
    };

    assert_eq!(judged(Term::prop()), [Ok(true), Ok(true), Ok(false)]);
    assert_eq!(
        judged(Term::type_ground()),
        [Ok(false), Ok(false), Ok(false)]
    );
}

// A struct's fields compare at their declared types, recovered from the registry, so two neutrals at a unit-typed field are one, by the unit's eta, and two structs differing only there convert.
#[test]
fn a_struct_field_at_the_unit_type_converges_unread() {
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
                        (x, Term::intrinsic(Intrinsic::NatType)),
                        (u, Term::tuple_type_unit()),
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

    context.assume(&r_binder, &Term::tuple_type_unit());
    context.assume(&s_binder, &Term::tuple_type_unit());

    let r = Term::free_var(&r_binder);
    let s = Term::free_var(&s_binder);

    // Wrap { 1, r } and Wrap { 1, s } differ only in the unit field's neutral.
    let this = Term::struct_(nominal("Wrap"), Vec::<Term>::new(), [nat(1), r]);
    let that = Term::struct_(nominal("Wrap"), Vec::<Term>::new(), [nat(1), s]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

// The same discipline at a nominal proposition rather than the unit — the shape the proof-carrying idiom writes, and the proposition the certifier's twin of this test puts to its own copy.
#[test]
fn a_struct_field_at_a_proposition_is_not_read() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let p_field = context.fresh(Some("p"));
    let (p, q) = (context.fresh(Some("p")), context.fresh(Some("q")));

    context
        .register_induct(
            &nominal("P"),
            InductDecl::new(
                UniverseContext::empty(),
                Telescope::done(Telescope::done(())),
                Constructors::default(),
                Term::prop(),
                Qualifier::empty(),
                true,
            ),
        )
        .unwrap();
    let proposition = Term::induct_type(nominal("P"), Vec::<Term>::new(), Vec::<Term>::new());
    context
        .register_struct(
            &nominal("Wrap"),
            StructDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::build(
                    [
                        (n, Term::intrinsic(Intrinsic::NatType)),
                        (p_field, proposition.clone()),
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
    context.assume(&p, &proposition);
    context.assume(&q, &proposition);

    let this = Term::struct_(
        nominal("Wrap"),
        Vec::<Term>::new(),
        [nat(1), Term::free_var(&p)],
    );
    let that = Term::struct_(
        nominal("Wrap"),
        Vec::<Term>::new(),
        [nat(1), Term::free_var(&q)],
    );
    assert_eq!(conv(&mut context, &this, &that), Ok(true));

    let other = Term::struct_(
        nominal("Wrap"),
        Vec::<Term>::new(),
        [nat(2), Term::free_var(&p)],
    );
    assert_eq!(conv(&mut context, &this, &other), Ok(false));
}

#[test]
fn a_constructor_payload_at_a_proposition_is_not_read() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let p_field = context.fresh(Some("p"));
    let (p, q) = (context.fresh(Some("p")), context.fresh(Some("q")));

    context
        .register_induct(
            &nominal("P"),
            InductDecl::new(
                UniverseContext::empty(),
                Telescope::done(Telescope::done(())),
                Constructors::default(),
                Term::prop(),
                Qualifier::empty(),
                true,
            ),
        )
        .unwrap();
    let proposition = Term::induct_type(nominal("P"), Vec::<Term>::new(), Vec::<Term>::new());
    context
        .register_induct(
            &nominal("Wrap"),
            InductDecl::new(
                UniverseContext::empty(),
                Telescope::done(Telescope::done(())),
                Vec::from([(
                    Atom::from("wrap"),
                    curios_core::test_support::case(
                        0,
                        Telescope::build(
                            [
                                (n, Term::intrinsic(Intrinsic::NatType)),
                                (p_field, proposition.clone()),
                            ],
                            Vec::new(),
                        ),
                        vec![Plicity::Explicit, Plicity::Explicit],
                    ),
                )])
                .try_into()
                .unwrap(),
                Term::type_ground(),
                Qualifier::empty(),
                true,
            ),
        )
        .unwrap();
    context.assume(&p, &proposition);
    context.assume(&q, &proposition);

    let this = Term::variant(
        nominal("Wrap"),
        Vec::<Term>::new(),
        "wrap",
        [nat(1), Term::free_var(&p)],
    );
    let that = Term::variant(
        nominal("Wrap"),
        Vec::<Term>::new(),
        "wrap",
        [nat(1), Term::free_var(&q)],
    );
    assert_eq!(conv(&mut context, &this, &that), Ok(true));

    let other = Term::variant(
        nominal("Wrap"),
        Vec::<Term>::new(),
        "wrap",
        [nat(2), Term::free_var(&p)],
    );
    assert_eq!(conv(&mut context, &this, &other), Ok(false));
}

// Likewise a variant's payload compares at its constructor's declared types, so two neutrals at a unit-typed payload field are one.
#[test]
fn a_constructor_payload_at_the_unit_type_converges_unread() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let u = context.fresh(Some("u"));
    let r_binder = context.fresh(Some("r"));
    let s_binder = context.fresh(Some("s"));

    // induct Wrap | wrap(x : Nat, u : ()) end
    context
        .register_induct(
            &nominal("Wrap"),
            InductDecl::new(
                UniverseContext::empty(),
                Telescope::done(Telescope::done(())),
                Vec::from([(
                    Atom::from("wrap"),
                    curios_core::test_support::case(
                        0,
                        Telescope::build(
                            [
                                (x, Term::intrinsic(Intrinsic::NatType)),
                                (u, Term::tuple_type_unit()),
                            ],
                            Vec::new(),
                        ),
                        vec![Plicity::Explicit, Plicity::Explicit],
                    ),
                )])
                .try_into()
                .unwrap(),
                Term::type_ground(),
                Qualifier::empty(),
                true,
            ),
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

/// Proof irrelevance at a *computed* proposition — a stuck `match` whose goal is `Prop`.
///
/// Across the prelude's elaboration **every firing is at a computed proposition rather than a nominal `Prop` family**: validity predicates, the shape a decision procedure takes. So the tests that name irrelevance at a nominal family — `curios-cert`'s `any_two_inhabitants_of_a_proposition_convert` and its control — cover a shape the corpus never presents, and cover the *certifier's* copy, which no program reaches. This crate's copy is the one that does the work, and this fixture asserts it rather than leaving it to the prelude happening to be written with those predicates.
///
/// What the fixture pins is the mechanism those firings rest on. `sort_of` classifies a stuck `Match` by reading its **goal**, not its arms — "a type-valued match: its sort is its goal" — so a match that cannot reduce is nonetheless a proposition when its goal says `Prop`, and irrelevance may then discharge a goal at it without examining either side. Two distinct neutrals convert there.
///
/// The control is the identical term with the goal at `Type`. It must not convert, and it is what makes this a test of the *goal* rather than of matches in general: read the arms instead, or default to `Prop` for anything unclassifiable, and the two fixtures stop disagreeing.
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

/// The control for the fixture above: the same stuck `match`, with its goal at `Type`. Irrelevance is a property of the type, and a computed type is no exception.
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

/// `match n | 0 => Nat | _ => Nat end` at the given goal, stuck because `n` is a neutral assumption. The arms are deliberately a *relevant* type in both fixtures: what decides the sort is the goal, and picking arms that agree with it would let a rule reading the arms pass too.
fn computed_type(context: &mut Context, goal: Term) -> Term {
    let subject = context.fresh(Some("n"));
    context.assume(&subject, &Term::intrinsic(Intrinsic::NatType));

    Term::switch(
        Term::free_var(&subject),
        goal,
        [(0u32, Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    )
}

/// An intrinsic with no hand-written congruence arm is still compared operand by operand.
///
/// A hand-written table short an operation would answer a *hard* mismatch rather than a postponement, refusing a metavariable standing in one of that operation's operands instead of solving it — and `convert`'s syntactic-identity short circuit would hide that for every spelling that happens to be identical. The rule reads the operands off `Intrinsic::traverse`, so the table cannot be short an operation; `ListMap` is the one put to it here.
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
    let f = Global::Authored(Qualifier::from(["f"]));
    context.declare(&f, &mapper_type);

    context.birth_metavar(MetavarId(0), Vec::new(), mapper_type);

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
        Term::instance(f, Universes::default()),
    ));

    assert_eq!(conv(&mut context, &flexible, &rigid), Ok(true));
}

// A function type's codomain is decided at below past a domain that waits. `(x: ?0(0)) -> Prop ≤ (x: Bool) -> Prop` at a larger level: the domain is an equation that waits on `?0` and is all the run surrenders, the codomain being a level bound. With `Type` for the upper codomain the pair is refused outright, at below as at equal: no proposition stands where a type is wanted.
#[test]
fn a_codomain_is_decided_at_below_past_a_domain_that_waits() {
    let mut context = context();
    let kind = Term::func_type(
        [(context.fresh(Some("n")), nat_type())],
        Term::type_ground(),
    );
    context.birth_metavar(MetavarId(0), Vec::new(), kind);
    let lower = Term::func_type(
        [(
            context.fresh(Some("x")),
            Term::apply(Term::hole(0), [nat(0)]),
        )],
        Term::prop(),
    );
    let over = |context: &mut Context, codomain: Term| {
        Term::func_type(
            [(
                context.fresh(Some("x")),
                Term::intrinsic(Intrinsic::BoolType),
            )],
            codomain,
        )
    };
    let upper = over(&mut context, Term::prop_at(Level::constant(1)));
    let into_a_type = over(&mut context, Term::type_ground());

    let below = subsume_outcome(&mut context, &lower, &upper, None, None);
    let Ok(Outcome::Blocked(goals)) = below else {
        panic!("{below:?}");
    };
    assert_eq!(goals.len(), 1);
    assert_eq!(goals[0].relation, Relation::Equal);

    let below = subsume_outcome(&mut context, &lower, &into_a_type, None, None);
    assert!(matches!(below, Ok(Outcome::Mismatch(_))), "{below:?}");
    let equal = convert_outcome(&mut context, &Term::type_ground(), &lower, &into_a_type);
    assert!(matches!(equal, Ok(Outcome::Mismatch(_))), "{equal:?}");
}

// Two sorts of one kind at below are the level bound and at equal the level equation: `Type 0 ≤ Type 1` and `Prop 0 ≤ Prop 1` hold, and `Type 1 ≤ Type 0` is refused where the levels close. Two sorts of two kinds are refused either way round: no proposition stands where a type is wanted.
#[test]
fn two_sorts_at_below_are_a_level_bound() {
    let mut context = context();
    let small = Term::type_ground();
    let large = Term::type_at(Level::constant(1));

    assert!(matches!(
        subsume_outcome(&mut context, &small, &large, None, None),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        subsume_outcome(
            &mut context,
            &Term::prop(),
            &Term::prop_at(Level::constant(1)),
            None,
            None
        ),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &Term::prop(), &small, None, None),
        Ok(Outcome::Mismatch(_))
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &small, &Term::prop(), None, None),
        Ok(Outcome::Mismatch(_))
    ));
}
