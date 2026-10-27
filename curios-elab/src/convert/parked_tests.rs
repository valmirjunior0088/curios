//! Parked goals: retried under the refinements and binders they parked under, woken by their type's solution, and resumed in the direction they parked in.

use {
    super::{convert_under, test_support::*},
    crate::*,
    curios_core::{
        Atom, InductDecl, InductParam, Level, MetavarId, Telescope, Term, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

// === Parked-constraint retries ==============================================

#[test]
fn parked_goals_retry_under_their_frozen_refinements() {
    let mut context = context();
    let b = context.fresh(Some("b"));

    // Park (inside an arm-like frame) a goal that converts only through the frame's counterfactual refinement: `b` reduces to `Nat` via `refine`, not via any definition.
    context.with_frame(|context| {
        context.assume(&b, &Term::type_ground());
        context.refine(&b, &nat_type());
        context.park(
            ParkedWork::Conversion(Problem {
                type_: Term::type_ground(),
                this: Term::free_var(&b),
                that: nat_type(),
                binders: Binders::default(),
                pairs: Pairs::default(),
                relation: Relation::Equal,
            }),
            Term::free_var(&b),
        );
    });

    // The frame is gone; the drain retries under the frozen one, where the refinement still holds and the goal converts.
    assert!(context.drain_parked().is_ok());
}

// A parked conversion watches the type it is compared at as it watches its two sides: postponed because that type is unsolved, it is woken where the type's metavariable is solved and does not wait for the drain's last sweep.
#[test]
fn a_parked_goal_is_woken_by_its_types_solution() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.park(
        ParkedWork::Conversion(Problem {
            type_: Term::hole(0),
            this: nat(1),
            that: nat(1),
            binders: Binders::default(),
            pairs: Pairs::default(),
            relation: Relation::Equal,
        }),
        nat(1),
    );
    assert!(context.wake_parked().is_empty());

    context.solve_metavar(MetavarId(0), nat_type());

    assert_eq!(context.wake_parked().len(), 1);
}

#[test]
fn parked_goals_without_their_refinement_mismatch() {
    let mut context = context();
    let b = context.fresh(Some("b"));

    // Control: the same goal parked without the refinement cannot convert, and the drain reports it at its origin.
    context.with_frame(|context| {
        context.assume(&b, &Term::type_ground());
        context.park(
            ParkedWork::Conversion(Problem {
                type_: Term::type_ground(),
                this: Term::free_var(&b),
                that: nat_type(),
                binders: Binders::default(),
                pairs: Pairs::default(),
                relation: Relation::Equal,
            }),
            Term::free_var(&b),
        );
    });

    assert!(context.drain_parked().is_err());
}

/// A goal conversion surrenders is retried under the binders it was posed under. Two eliminations of one scrutinee hand on, in their arm, a proof the arm binds: one through an elimination stuck on an unsolved metavariable, the other the arm's second proof. The pair cannot be decided while the metavariable is unsolved, so it is surrendered as blocked; once it is solved the first side reduces to the arm's first proof, and the two are one by the proposition the arm's binders were opened at. Retried under no binder they are two labels nothing types, and stay apart — which is what the verdict would be, following whether the goal parked, were the binders kept anywhere but with the goal.
///
/// Mutation-checked: with a goal parked under no binder the retry is a mismatch.
#[test]
fn a_goal_surrendered_under_a_binder_is_retried_under_it() {
    let mut context = context();
    let family = |context: &mut Context, path: &str, constructors, sort| {
        context
            .register_induct(
                &nominal(path),
                InductDecl {
                    universe_context: UniverseContext::empty(),
                    arity: Telescope::done(Telescope::done(())),
                    constructors,
                    result_sort: sort,
                    module: Qualifier::empty(),
                    rep_public: true,
                    polarities: Vec::new(),
                    variances: Vec::new(),
                },
            )
            .unwrap();

        Term::induct_type(nominal(path), Vec::<Term>::new(), Vec::<Term>::new())
    };
    // `induct P : Prop` and `induct Pair : Type | two(a : P, b : P)`.
    let proposition = family(&mut context, "P", Vec::new().into(), Term::prop());
    let (a, b) = (context.fresh(Some("a")), context.fresh(Some("b")));
    let pair = family(
        &mut context,
        "Pair",
        Vec::from([(
            Atom::from("two"),
            InductParam::new(
                Telescope::build(
                    [(a, proposition.clone()), (b, proposition.clone())],
                    Vec::new(),
                ),
                vec![Plicity::Explicit, Plicity::Explicit],
            ),
        )])
        .into(),
        Term::type_ground(),
    );
    let scrutinee = context.fresh(Some("t"));
    context.assume(&scrutinee, &pair);
    context.birth_metavar(MetavarId(0), Vec::new(), nat_type());

    // `match t | two(a, b) => (<handed on>, 1) end`, at the ambient `{P, Nat}`.
    let (x, y) = (context.fresh(Some("x")), context.fresh(Some("y")));
    let result = Term::tuple_type([(x, proposition.clone()), (y, nat_type())]);
    let handing_on = |context: &mut Context, through_the_hole: bool| {
        let (a, b, m) = (
            context.fresh(Some("a")),
            context.fresh(Some("b")),
            context.fresh(Some("m")),
        );
        // `match ?0 | 0 => a | pred + 1; ih => a end`, stuck until the metavariable is solved.
        let (inner, pred, ih) = (
            context.fresh(Some("m")),
            context.fresh(Some("pred")),
            context.fresh(Some("ih")),
        );
        let handed = match through_the_hole {
            true => Term::nat_match(
                Term::hole(0),
                Some(&inner),
                proposition.clone(),
                Term::free_var(&a),
                &pred,
                &ih,
                Term::free_var(&a),
            ),
            false => Term::free_var(&b),
        };

        Term::induct_match(
            Term::free_var(&scrutinee),
            Some(&m),
            result.clone(),
            [("two", vec![a, b], Term::tuple([handed, nat(1)]))],
        )
    };
    let (this, that) = (
        handing_on(&mut context, true),
        handing_on(&mut context, false),
    );

    let Ok(Outcome::Blocked(goals)) =
        convert_outcome(&mut context, &Term::type_ground(), &this, &that)
    else {
        panic!("a pair stuck on an unsolved metavariable was decided");
    };
    let [goal] = &goals[..] else {
        panic!(
            "one goal is stuck on the metavariable, and {} were surrendered",
            goals.len()
        );
    };
    context.solve_metavar(MetavarId(0), nat(0));

    assert!(matches!(
        convert_under(
            &mut context,
            &goal.binders,
            &goal.pairs,
            &goal.type_,
            &goal.this,
            &goal.that
        ),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        convert_outcome(&mut context, &goal.type_, &goal.this, &goal.that),
        Ok(Outcome::Mismatch(_))
    ));
}

// A goal parked at below resumes at below. `Type 0 ≤ ?0` waits on `?0`; solved to `Type 1`, the retry finds `Type 0` below it, where an equation would compare the two levels and refuse.
#[test]
fn a_goal_parked_at_below_resumes_at_below() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));
    context.park(
        ParkedWork::Conversion(Problem {
            type_: Term::type_ground(),
            this: Term::type_ground(),
            that: Term::hole(0),
            binders: Binders::default(),
            pairs: Pairs::default(),
            relation: Relation::Below,
        }),
        Term::hole(0),
    );
    context.solve_metavar(MetavarId(0), Term::type_at(Level::constant(1)));

    assert!(context.drain_parked().is_ok());
}

// Control: the same goal parked as an equation is refused once `?0` is `Type 1`.
#[test]
fn a_goal_parked_at_equal_resumes_at_equal() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));
    context.park(
        ParkedWork::Conversion(Problem {
            type_: Term::type_ground(),
            this: Term::type_ground(),
            that: Term::hole(0),
            binders: Binders::default(),
            pairs: Pairs::default(),
            relation: Relation::Equal,
        }),
        Term::hole(0),
    );
    context.solve_metavar(MetavarId(0), Term::type_at(Level::constant(1)));

    assert!(context.drain_parked().is_err());
}
