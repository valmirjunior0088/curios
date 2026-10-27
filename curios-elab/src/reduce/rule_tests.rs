//! Beta, zeta, iota, projection and eta, and the metavariable arms.

use {
    super::test_support::{context, nat, nominal},
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Apply, Atom, Bound, Exhaustion, Free, InductDecl, InductParam, Intrinsic, Level, MetavarId,
        MetavarOrigin, Nat, Subterm, Telescope, Term, UniverseContext, UniverseParam,
    },
    curios_utilities::Qualifier,
};

#[test]
fn apply_beta_reduces() {
    let mut context = context();
    let x = context.fresh(Some("x"));

    let term: Term = Term::apply(
        Term::func([(x, Term::type_ground())], Term::free_var(&x)),
        [nat(1)],
    );

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(1)));
}

#[test]
fn recursive_application_stays_folded_until_its_result_is_demanded() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let m = context.fresh(Some("m"));
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));
    let countdown = context.fresh(Some("countdown"));
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let body = Term::func(
        [(n, nat_type.clone())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&m),
            nat_type.clone(),
            nat(0),
            &pred,
            &ih,
            Term::apply(Term::free_var(&countdown), [Term::free_var(&pred)]),
        ),
    );

    let neutral = Term::rec(
        [(
            countdown,
            Term::func_type([(n, nat_type.clone())], nat_type.clone()),
            body.clone(),
        )],
        Term::apply(Term::free_var(&countdown), [Term::free_var(&x)]),
    );
    let Subterm::Rec(rec) = Term::unwrap_or_clone(neutral) else {
        unreachable!()
    };
    let opened = unfold_rec(&mut context, rec).expect("opening a group's tail is affordable");
    let reduced = reduce(&mut context, opened).expect("ordinary reduction should terminate");
    assert!(matches!(
        &*reduced,
        Subterm::Apply(Apply { head, .. }) if head.as_rec_proj().is_some()
    ));

    let concrete = Term::rec(
        [(
            countdown,
            Term::func_type([(n, nat_type.clone())], nat_type),
            body,
        )],
        Term::apply(Term::free_var(&countdown), [nat(2)]),
    );
    assert_eq!(reduce_forced(&mut context, concrete), Ok(nat(0)));
}

/// A member whose result is a function, forced where it is applied past its own parameters: `f(2)(y)` is the call `f(2)` applied to `y`, so the force reaches through the outer application to the call, and `y`, symbolic, rides along to the answer. Read one level deep, the outer application would be a neutral the force hands back folded.
#[test]
fn a_recursive_call_applied_past_its_parameters_unfolds_when_forced() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let m = context.fresh(Some("m"));
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));
    let f = context.fresh(Some("f"));
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let arrow = Term::func_type([(x, nat_type.clone())], nat_type.clone());
    let called = |on: &Free, argument: Term| {
        Term::apply(
            Term::apply(Term::free_var(&f), [Term::free_var(on)]),
            [argument],
        )
    };
    let body = Term::func(
        [(n, nat_type.clone())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&m),
            arrow.clone(),
            Term::func([(x, nat_type.clone())], Term::free_var(&x)),
            &pred,
            &ih,
            Term::func([(x, nat_type.clone())], called(&pred, Term::free_var(&x))),
        ),
    );

    let term = Term::rec(
        [(f, Term::func_type([(n, nat_type)], arrow), body)],
        Term::apply(
            Term::apply(Term::free_var(&f), [nat(2)]),
            [Term::free_var(&y)],
        ),
    );

    assert_eq!(reduce_forced(&mut context, term), Ok(Term::free_var(&y)));
}

#[test]
fn an_application_whose_group_dissolved_to_its_member_still_unfolds() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let unused = context.fresh(Some("unused"));
    let value = context.fresh(Some("value"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let identity = Term::func([(n, nat_type.clone())], Term::free_var(&n));

    // A group whose member never mentions itself has no fixed point to keep, so opening its tail reduces past the projection to the member's own value and `expose_rec_tail` leaves a `Func`. Taking the step only on a projection would decline here with that `Func` in hand, and the caller would keep the folded spelling — which the positivity walk reads at `Mixed`, so an `induct`'s type constructor reached through this spelling would stop composing.
    let term: Term = Term::apply(
        Term::rec(
            [(
                unused,
                Term::func_type([(n, nat_type)], Term::intrinsic(Intrinsic::NatType)),
                identity.clone(),
            )],
            identity,
        ),
        [Term::free_var(&value)],
    );
    let Subterm::Apply(apply) = Term::unwrap_or_clone(term) else {
        unreachable!()
    };

    assert_eq!(
        unfold_rec_apply(&mut context, apply),
        Ok(Some(Term::free_var(&value)))
    );
}

#[test]
fn inductive_match_selects_case_and_projects_payload() {
    let mut context = context();
    let m = context.fresh(Some("m"));
    let x = context.fresh(Some("x"));

    // Dispatch inspects the reduced head's `Variant`; the arm's binder is bound call-by-name to the flat projection `head.1`, which then reduces to the payload component.
    let term: Term = Term::induct_match(
        Term::variant(nominal("E"), Vec::<Term>::new(), "some", [nat(42)]),
        Some(&m),
        Term::intrinsic(Intrinsic::NatType),
        [
            ("none", Vec::<Free>::new(), nat(0)),
            ("some", vec![x], Term::free_var(&x)),
        ],
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(42)));
}

#[test]
fn inductive_match_absent_tag_takes_default() {
    let mut context = context();
    let m = context.fresh(Some("m"));

    // The scrutinee is `some(42)`, but only `none` has an explicit arm; the `some` tag is absent from the cases, so dispatch falls through to the binding-free `| _ =>` default (no payload projected).
    let term: Term = Term::induct_match_default(
        Term::variant(nominal("E"), Vec::<Term>::new(), "some", [nat(42)]),
        Some(&m),
        Term::intrinsic(Intrinsic::NatType),
        [("none", Vec::<Free>::new(), nat(0))],
        nat(99),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(99)));
}

#[test]
fn inductive_match_present_tag_ignores_default() {
    let mut context = context();
    let m = context.fresh(Some("m"));
    let x = context.fresh(Some("x"));

    // With the `some` arm present, dispatch selects it (binding the payload) rather than the default — the default is only for absent tags.
    let term: Term = Term::induct_match_default(
        Term::variant(nominal("E"), Vec::<Term>::new(), "some", [nat(42)]),
        Some(&m),
        Term::intrinsic(Intrinsic::NatType),
        [
            ("none", Vec::<Free>::new(), nat(0)),
            ("some", vec![x], Term::free_var(&x)),
        ],
        nat(99),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(42)));
}

/// A match taking a proof into data stands while the instance it states holds a metavariable no solution has landed for, and fires on that instance once one has. Over `induct Zero : (n : Nat) -> Prop | zero() : (0)`, `match p | zero() => 7 end` stating `Zero(?m)` is left as it is, whether `p` is a variable or the constructor's own value, whose arm the ordinary rule would take; with `?m` solved to `0` each is `7`, the variable unread.
///
/// Mutation-checked: with an open instance answered as one that says nothing, the match on the constructor's value fires before the solution lands.
#[test]
fn a_proofs_match_stands_while_the_instance_it_states_is_open() {
    let mut context = context();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let index = context.fresh(Some("n"));
    let zero = nominal("Zero");
    context
        .register_induct(
            &zero,
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::build([(index, nat_type.clone())], ())),
                constructors: vec![(
                    Atom::from("zero"),
                    InductParam::new(Telescope::done(vec![nat(0)]), Vec::new()),
                )]
                .into(),
                result_sort: Term::prop(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();
    context.birth_metavar(MetavarId(0), Vec::new(), nat_type.clone());

    let stated = Term::induct_type(zero, Vec::<Term>::new(), [Term::hole(0)]);
    let eliminated = |proof: Term| {
        Term::induct_match(
            proof,
            None,
            nat_type.clone(),
            [("zero", Vec::<Free>::new(), nat(7))],
        )
        .stating(stated.clone())
    };
    let variable = context.fresh(Some("p"));
    let of_a_variable = eliminated(Term::free_var(&variable));
    let of_a_value = eliminated(Term::variant(
        zero,
        Vec::<Term>::new(),
        "zero",
        Vec::<Term>::new(),
    ));

    assert_eq!(
        reduce(&mut context, of_a_variable.clone()),
        Ok(of_a_variable.clone())
    );
    assert_eq!(
        reduce(&mut context, of_a_value.clone()),
        Ok(of_a_value.clone())
    );

    context.solve_metavar(MetavarId(0), nat(0));

    assert_eq!(reduce(&mut context, of_a_variable), Ok(nat(7)));
    assert_eq!(reduce(&mut context, of_a_value), Ok(nat(7)));
}

#[test]
fn nat_fold_zero_takes_the_zero_case() {
    let mut context = context();
    let m = context.fresh(Some("m"));
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));

    let term: Term = Term::nat_match(
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        Some(&m),
        Term::intrinsic(Intrinsic::BoolType),
        Term::intrinsic(Intrinsic::Bool(false)),
        &pred,
        &ih,
        Term::intrinsic(Intrinsic::Bool(true)),
    );

    assert_eq!(
        reduce(&mut context, term),
        Ok(Term::intrinsic(Intrinsic::Bool(false)))
    );
}

#[test]
fn let_then_var_unfolds_definition() {
    let mut context = context();
    let y = context.fresh(Some("y"));
    let x = context.fresh(Some("x"));

    context.define(&y, &nat(7), None);

    let term: Term = Term::let_(
        &x,
        Term::type_ground(),
        Term::free_var(&y),
        Term::free_var(&x),
    );

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(7)));
}

#[test]
fn polymorphic_definition_unfolds_only_through_an_explicit_universe_instance() {
    let mut context = context();
    let poly = context.fresh(Some("poly"));
    let parameter = Level::param(UniverseParam(0));
    let body = Term::type_at(parameter.clone());
    context.assume(&poly, &Term::type_at(parameter.succ().unwrap()));
    context.define(&poly, &body, None);
    context.set_assumption_universe_context(
        &poly,
        UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
    );

    let raw = Term::free_var(&poly);
    assert_eq!(reduce(&mut context, raw.clone()), Ok(raw.clone()));
    assert_eq!(
        reduce(
            &mut context,
            Term::instance_of(&poly, vec![Level::constant(3)])
        ),
        Ok(Term::type_at(Level::constant(3)))
    );
}

#[test]
fn let_binds_each_value_to_its_own_slot() {
    // Two distinct bindings referenced together in the tail: pins the positional correctness of `reduce_let`'s substitution. The tail is `(λ p q. q) a b`, so the result is `b`'s value — and only if `a`/`b` land in the right slots. A transposed open would beta-reduce to `a`'s value instead.
    let mut context = context();
    let p = context.fresh(Some("p"));
    let q = context.fresh(Some("q"));
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));

    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let pick_second = Term::apply(
        Term::func(
            [(p, nat_type.clone()), (q, nat_type.clone())],
            Term::free_var(&q),
        ),
        [Term::free_var(&a), Term::free_var(&b)],
    );

    let term = Term::let_(
        &a,
        nat_type.clone(),
        nat(3),
        Term::let_(&b, nat_type, nat(7), pick_second),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(7)));
}

#[test]
fn let_shadowing_tail_picks_innermost() {
    // `let x = 3; let x = 7; x` — two bindings share the name `x`. The flat block is built by name-based `capture`, so the tail's `x` must bind to the *innermost* binding (7), not the shadowed outer one (3).
    let mut context = context();
    let x_binder = context.fresh(Some("x"));

    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let term = Term::let_(
        &x_binder,
        nat_type.clone(),
        nat(3),
        Term::let_(&x_binder, nat_type, nat(7), Term::free_var(&x_binder)),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(7)));
}

#[test]
fn let_shadowing_value_sees_the_outer_binding() {
    // `let x = 5; let x = x; x` — the middle binding's value is the *outer* `x`, since a `let` is non-recursive. Merging must leave that reference free so the enclosing binder captures it to the first binding, not to itself: a self-capture would define `x := x` and diverge instead of yielding 5.
    let mut context = context();
    let x_binder = context.fresh(Some("x"));

    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let term = Term::let_(
        &x_binder,
        nat_type.clone(),
        nat(5),
        Term::let_(
            &x_binder,
            nat_type,
            Term::free_var(&x_binder),
            Term::free_var(&x_binder),
        ),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(5)));
}

#[test]
fn deep_let_chain_is_one_flat_block_reducing_without_native_recursion() {
    // A long straight-line `let` sequence must lower to a single flat `Let` block, not a nest: `Term::let_` merges each binding into the block already built for its tail, so folding the chain bottom-up (as `into_core` and the elaborator's rebuild both do) yields one node. That flatness is what bounds every walk over it — `reduce` here, and `traverse` via `reach` — to a loop instead of one native stack frame per binding.
    let depth = 1000;
    let mut context = Context::with_default_budget(SYNTAX);
    let binders = (0..depth)
        .map(|i| context.fresh(Some(&format!("x{i}"))))
        .collect::<Vec<_>>();
    let base = Term::free_var(&binders[depth - 1]);

    // `let x0 = 0; let x1 = x0; …; let x{n-1} = x{n-2}; x{n-1}`.
    let term = (0..depth).rev().fold(base, |tail, i| {
        let value = match i {
            0 => nat(0),
            _ => Term::free_var(&binders[i - 1]),
        };

        Term::let_(
            &binders[i],
            Term::intrinsic(Intrinsic::NatType),
            value,
            tail,
        )
    });

    match &*term {
        Subterm::Let(let_) => {
            assert_eq!(
                let_.bindings.len(),
                depth,
                "the chain must collapse to one flat block"
            )
        }
        other => panic!("expected a single flat `Let` block, got {other:?}"),
    }

    // Every reference is internal (no free variables escape), and both `reach` and `reduce` compute over the whole depth without recursing per binding.
    assert_eq!(term.reach(), 0);
    assert_eq!(reduce(&mut context, term), Ok(nat(0)));
}

#[test]
fn a_match_tower_reduces_without_overflowing() {
    // Each level's scrutinee is the level below it, so reducing the top costs one nested `reduce` per link. That depth is what `recurse` carries, and it is data-shaped: a tower this tall is generated rather than written.
    //
    // Deep enough that a regression is a stack overflow rather than a slow test, and under a budget large enough that the budget is not what decides it — which is the property `reduce`'s own documentation claims.
    //
    // **The stated budget is not incidental.** A guarded level charges `Cost::FRAME` when it is a new peak, so depth is a priced resource and a budget could decide this test rather than the stack: ten thousand levels cost about 10.2 million units of frames alone. A test whose subject is the stack has to take the budget out of the answer, and stating one is how.
    const DEEP: usize = 10_000;

    let mut context = Context::new(100_000_000, SYNTAX);
    let bool_type = Term::intrinsic(Intrinsic::BoolType);
    let true_ = || Term::intrinsic(Intrinsic::Bool(true));

    let mut term = true_();
    for _ in 0..DEEP {
        term = Term::bool_match(
            term,
            None,
            bool_type.clone(),
            Term::intrinsic(Intrinsic::Bool(false)),
            true_(),
        );
    }

    assert_eq!(reduce(&mut context, term), Ok(true_()));
}

#[test]
fn var_cycle_times_out() {
    let mut context = context();
    let loop_ = context.fresh(Some("loop"));

    context.define(&loop_, &Term::free_var(&loop_), None);

    assert!(reduce(&mut context, Term::free_var(&loop_)).is_err_and(|spent| spent.is_exhausted()));
}

#[test]
fn proj_beta_reduces() {
    let mut context = context();

    let term: Term = Term::proj(Term::tuple([nat(1), nat(2)]), 1);

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(2)));
}

#[test]
fn does_not_eta_reduce_tuple() {
    let mut context = context();
    let r = context.fresh(Some("r"));

    // Tuple η is type-directed and lives in `convert`, not `reduce`: `reduce` cannot verify `r`'s arity without type info, so collapsing `(r.0, r.1)` to `r` would widen the tuple whenever `r` has more fields than the tuple does.
    let term: Term = Term::tuple([
        Term::proj(Term::free_var(&r), 0),
        Term::proj(Term::free_var(&r), 1),
    ]);

    assert_eq!(reduce(&mut context, term.clone()), Ok(term));
}

#[test]
fn eta_reduce_func_fires() {
    let mut context = context();
    let y = context.fresh(Some("y"));
    let f = context.fresh(Some("f"));

    let term: Term = Term::func(
        [(y, Term::type_ground())],
        Term::apply(Term::free_var(&f), [Term::free_var(&y)]),
    );

    assert_eq!(reduce(&mut context, term.clone()), Ok(Term::free_var(&f)));
}

#[test]
fn unsolved_metavar_is_neutral() {
    let mut context = context();
    let m = Term::hole(0);

    // No store entry, or an unsolved one, both reduce to the metavariable itself.
    assert_eq!(reduce(&mut context, m.clone()), Ok(m.clone()));

    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    assert_eq!(reduce(&mut context, m.clone()), Ok(m));
}

#[test]
fn solved_metavar_yields_solution() {
    let mut context = context();
    let m = Term::hole(0);

    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // An unsolved metavariable reduces to itself, but that reduct names an unsolved metavariable, so it is deliberately not memoized.
    assert_eq!(reduce(&mut context, m.clone()), Ok(m.clone()));

    let solution = nat(1);
    context.solve_metavar(MetavarId(0), solution.clone());

    // Nothing stale was cached, so the reduct now follows the solution — `solve_metavar` needs no cache clear.
    assert_eq!(reduce(&mut context, m), Ok(solution));
}

// A solution that reaches its own metavariable sends the display walk round forever: reducing `?0` unfolds it to `f(?0)`, whose argument is `?0` again. The walk is display-only, and on the native stack with no bound a diagnostic about such a term would abort the process instead of rendering. Charged per level, the declaration's budget refuses it, and the caller falls back to the un-normalized spelling as its contract says.
#[test]
fn normalizing_a_solution_that_reaches_itself_is_refused_rather_than_overflowing() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    let hole = Term::metavar_birthed(0, MetavarOrigin::Hole, Vec::new());
    context.solve_metavar(
        MetavarId(0),
        Term::apply(Term::free_var(&f), [hole.clone()]),
    );

    assert!(normalize(&mut context, hole).is_err());
}
