//! Beta, zeta, iota, projection and eta, the metavariable arms, and what invalidates a cached reduct.

use {
    super::test_support::{context, nat, nominal, qed},
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Apply, Bound, Exhaustion, Free, InductDecl, Intrinsic, Level, MetavarId, MetavarOrigin,
        Nat, ReduceError, Subterm, Telescope, Term, UniverseContext, UniverseMetaId, UniverseParam,
        Variance,
    },
    curios_num::{Binary, Floating, Grain, Integer, Rounding},
    curios_utilities::Qualifier,
};

#[test]
fn nat_to_byte_reflects_byte_to_nat() {
    let mut context = context();
    let byte_binder = context.fresh(Some("byte"));
    let byte = Term::free_var(&byte_binder);
    let term = Term::intrinsic(Intrinsic::nat_to_byte(
        Term::intrinsic(Intrinsic::ByteToNat(byte.clone())),
        qed(),
    ));

    assert_eq!(reduce(&mut context, term), Ok(byte.clone()));

    // The other direction, which the narrowing's domain is what buys: a `Byte` read back out of a `Nat` it was built from is that `Nat` again, so a bound established before the trip survives it.
    let number = Term::intrinsic(Intrinsic::Nat(Nat::new(7usize)));
    let round_trip = Term::intrinsic(Intrinsic::ByteToNat(Term::intrinsic(
        Intrinsic::nat_to_byte(number.clone(), qed()),
    )));

    assert_eq!(reduce(&mut context, round_trip), Ok(number));
}

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
fn int_add_computes() {
    let mut context = context();

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::int_add(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(2)))
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Int(Integer::from(3))).into())
    );
}

#[test]
fn int_eql_returns_true_or_false_bool() {
    let mut context = context();

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::int_eql(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(4))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(4)))
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bool(true)).into())
    );
    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::int_eql(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(4))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(5)))
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bool(false)).into())
    );
}

#[test]
fn flt_folds_through_the_model() {
    let mut context = context();

    let flt = |value: f64| Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from(value))));

    // Two literals fold by calling the model, so the answer is a value rather than a normal form standing in for one.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_mul(Rounding::TiesToEven, flt(1.5), flt(2.0)))
        ),
        Ok(flt(3.0)),
    );

    // The cases the host would leave to itself, and the model does not: division by zero is a value, and `0.0 / 0.0` is the default NaN, positive, so `copysign` reads a `+`.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_div(Rounding::TiesToEven, flt(1.0), flt(0.0)))
        ),
        Ok(flt(f64::INFINITY)),
    );
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::FltCopysign(
                flt(1.0),
                Term::intrinsic(Intrinsic::flt_div(Rounding::TiesToEven, flt(0.0), flt(0.0))),
            ))
        ),
        Ok(flt(1.0)),
    );

    // A direction folds as the model rounds in it: a tenth is inexact, so each direction lands on its own side of it, and ties away from zero parts from ties to even at a halfway integer.
    let mut tenth = |rounding| {
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_div(rounding, flt(1.0), flt(10.0))),
        )
    };
    assert_eq!(tenth(Rounding::TiesToEven), Ok(flt(0.1)));
    assert_eq!(
        tenth(Rounding::TowardNegative),
        Ok(flt(f64::from_bits(0.1f64.to_bits() - 1)))
    );
    assert_eq!(tenth(Rounding::TowardZero), tenth(Rounding::TowardNegative));
    assert_eq!(tenth(Rounding::TowardPositive), Ok(flt(0.1)));
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_round_integral(
                Rounding::TiesToAway,
                flt(2.5)
            ))
        ),
        Ok(flt(3.0)),
    );
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_fma(
                Rounding::TiesToEven,
                flt(0.1),
                flt(10.0),
                flt(-1.0)
            ))
        ),
        Ok(flt(2f64.powi(-54))),
    );

    // A symbolic operand still rebuilds the neutral term.
    let symbolic = Term::intrinsic(Intrinsic::flt_mul(
        Rounding::TiesToEven,
        Term::free_var(&Free::local(1, Some("x"))),
        flt(2.0),
    ));
    assert_eq!(reduce(&mut context, symbolic.clone()), Ok(symbolic));
}

#[test]
fn list_get_returns_element_at_index() {
    let mut context = context();

    let list = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(10usize))).into(),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(20usize))).into(),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(30usize))).into(),
        ],
    });

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::list_get(
                Subterm::Intrinsic(Intrinsic::NatType),
                list.clone(),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                qed(),
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(10usize))).into())
    );
    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::list_get(
                Subterm::Intrinsic(Intrinsic::NatType),
                list,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(2usize))),
                qed(),
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(30usize))).into())
    );
}

#[test]
fn list_get_errors_on_out_of_bounds() {
    let mut context = context();

    let list = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))).into()],
    });

    assert!(matches!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::list_get(
                Subterm::Intrinsic(Intrinsic::NatType),
                list,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                qed(),
            ))
            .into(),
        ),
        Err(ReduceError::ListGetOutOfBounds {
            len: 1,
            index: 1,
            ..
        })
    ));
}

#[test]
fn bin_append_adds_byte() {
    let mut context = context();

    let bin = Subterm::Intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(vec![1, 2])));
    let byte: Subterm = Subterm::Intrinsic(Intrinsic::Byte(3));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::bin_append(Grain::X, bin, byte)).into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(vec![1, 2, 3]))).into())
    );
}

#[test]
fn bin_append_adds_the_full_byte_range() {
    let mut context = context();

    let bin = Subterm::Intrinsic(Intrinsic::Bin(Grain::X, Binary::from_bytes(vec![1, 2])));
    let byte: Subterm = Subterm::Intrinsic(Intrinsic::Byte(255));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::bin_append(Grain::X, bin, byte)).into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bin(
            Grain::X,
            Binary::from_bytes(vec![1, 2, 255])
        ))
        .into())
    );
}

#[test]
fn list_append_adds_element() {
    let mut context = context();

    let list = Subterm::Intrinsic(Intrinsic::List {
        element: Term::intrinsic(Intrinsic::NatType),
        items: vec![
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(10usize))).into(),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(20usize))).into(),
        ],
    });

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::list_append(
                Subterm::Intrinsic(Intrinsic::NatType),
                list,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(30usize)))
            ))
            .into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::List {
            element: Term::intrinsic(Intrinsic::NatType),
            items: vec![
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(10usize))).into(),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(20usize))).into(),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(30usize))).into(),
            ]
        })
        .into())
    );
}

#[test]
fn proj_beta_reduces() {
    let mut context = context();

    let term: Term = Term::proj(Term::tuple([nat(1), nat(2)]), 1);

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(2)));
}

#[test]
fn proj_refinement_lookup() {
    let mut context = context();
    let r = context.fresh(Some("r"));

    context.refine_projection(Term::free_var(&r), 0, nat(1));

    let term: Term = Term::proj(Term::free_var(&r), 0);

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(1)));
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
fn define_invalidates_cached_reduction() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    let x: Term = Term::free_var(&x_binder);

    // No definition yet: x reduces to itself and the result is cached.
    assert_eq!(reduce(&mut context, x.clone()), Ok(x.clone()));

    // Defining x must clear the cache so the next reduce unfolds.
    context.define(&x_binder, &nat(3), None);
    assert_eq!(reduce(&mut context, x), Ok(nat(3)));
}

#[test]
fn scrutinee_refinement_ignores_fresh_universe_instances() {
    let mut context = context();
    let classify = context.fresh(Some("classify"));
    let registered = Term::apply(
        Term::instance_of(&classify, vec![Level::meta(UniverseMetaId(0))]),
        [nat(0)],
    );
    let probe = Term::apply(
        Term::instance_of(&classify, vec![Level::meta(UniverseMetaId(1))]),
        [nat(0)],
    );
    let canonical = shallow_scrutinee(&context, &registered);
    context.refine_scrutinee_spellings(vec![(canonical, registered, false)], &nat(1));

    assert_eq!(reduce(&mut context, probe), Ok(nat(1)));
}

/// The pair the key must keep apart, and the one it does not: two occurrences of one definition at *ground* instances.
///
/// A fresh instance is undecided and collapses — the test above is that rule, and it is what the prelude needs — but `Type u` embeds a level in a *term*, so a definition carrying its parameter into a constructor payload reduces to genuinely different values at two ground instances. Erasing the instance identifies them, and the arm's equation then refines a stuck term it was never shown to be about. The kernel refuses the coercion this forges (`curios-cert`'s `recheck::universes_tests::a_case_equation_does_not_refine_an_occurrence_at_another_universe_instance`, over the whole module); this is the elaborator's own store, asked directly.
///
/// The probe is checker-level rather than a source program on purpose: the pair has no surface spelling, because `UniverseSolver::finalize` minimizes a body-only level to a constant instead of generalizing it.
///
/// Mutation-checked: removing the guard fails these two and leaves both `ignores_fresh_universe_instances` tests green, so the forgery pair and the collapse pair are not testing one thing twice — which is the whole of what the guard is claimed to do.
#[test]
fn scrutinee_refinement_does_not_fire_at_another_ground_universe_instance() {
    let mut context = context();
    let classify = context.fresh(Some("classify"));
    let at = |level: Level| Term::apply(Term::instance_of(&classify, vec![level]), [nat(0)]);
    let registered = at(Level::zero());
    let probe = at(Level::zero().succ().expect("level zero has a successor"));
    let canonical = shallow_scrutinee(&context, &registered);
    context.refine_scrutinee_spellings(vec![(canonical, registered, false)], &nat(1));

    assert_eq!(reduce(&mut context, probe.clone()), Ok(probe));
}

/// [`scrutinee_refinement_does_not_fire_at_another_ground_universe_instance`] with the level on a nominal node whose family is irrelevant in it. Conversion calls `Wrap.{0}(Nat)` and `Wrap.{1}(Nat)` one type, and the guard still keeps the two spellings apart: it reads no family's variance, because the kernel's key is the scrutinee compared with every level, and an equation fired here would be one the kernel does not fire.
///
/// Mutation-checked against a guard handed the context's registries, under which the two spellings stop clashing and the arm's value answers the probe.
#[test]
fn scrutinee_refinement_does_not_fire_at_another_instance_of_an_irrelevant_level() {
    let mut context = context();
    let classify = context.fresh(Some("classify"));
    let carrier = context.fresh(Some("A"));
    let sort = Term::type_at(Level::param(UniverseParam(0)));
    context
        .register_induct(
            &nominal("Wrap"),
            InductDecl {
                universe_context: UniverseContext {
                    parameter_count: 1,
                    constraints: Vec::new(),
                },
                arity: Telescope::build([(carrier, sort.clone())], Telescope::done(())),
                constructors: Vec::new(),
                result_sort: sort,
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: vec![Variance::Irrelevant],
                plicities: Vec::new(),
            },
        )
        .unwrap();
    let at = |level: Level| {
        Term::apply(
            Term::free_var(&classify),
            [Term::induct_type_at(
                nominal("Wrap"),
                [level],
                [Term::intrinsic(Intrinsic::NatType)],
                Vec::<Term>::new(),
            )],
        )
    };
    let registered = at(Level::zero());
    let probe = at(Level::constant(1));
    let canonical = shallow_scrutinee(&context, &registered);
    context.refine_scrutinee_spellings(vec![(canonical, registered.clone(), false)], &nat(1));

    assert_eq!(reduce(&mut context, registered), Ok(nat(1)));
    assert_eq!(reduce(&mut context, probe.clone()), Ok(probe));
}

/// [`scrutinee_refinement_does_not_fire_at_another_ground_universe_instance`] over the projection store, which keys the same way and keeps no unerased spelling of its own.
#[test]
fn projection_refinement_does_not_fire_at_another_ground_universe_instance() {
    let mut context = context();
    let record_binder = context.fresh(Some("record"));
    let at = |level: Level| Term::apply(Term::instance_of(&record_binder, vec![level]), [nat(0)]);
    let registered = at(Level::zero());
    let probe = Term::proj(
        at(Level::zero().succ().expect("level zero has a successor")),
        0,
    );
    context.refine_projection(registered, 0, nat(1));

    assert_eq!(reduce(&mut context, probe.clone()), Ok(probe));
}

#[test]
fn projection_refinement_ignores_fresh_universe_instances() {
    let mut context = context();
    let record_binder = context.fresh(Some("record"));
    let registered = Term::apply(
        Term::instance_of(&record_binder, vec![Level::meta(UniverseMetaId(0))]),
        [nat(0)],
    );
    let probe = Term::apply(
        Term::instance_of(&record_binder, vec![Level::meta(UniverseMetaId(1))]),
        [nat(0)],
    );
    context.refine_projection(registered, 0, nat(1));

    assert_eq!(reduce(&mut context, Term::proj(probe, 0)), Ok(nat(1)));
}

#[test]
fn refine_projection_invalidates_cached_reduction() {
    let mut context = context();
    let r = context.fresh(Some("r"));
    let proj: Term = Term::proj(Term::free_var(&r), 0);

    // No projection refinement yet: proj reduces to itself and is cached.
    assert_eq!(reduce(&mut context, proj.clone()), Ok(proj.clone()));

    // Refining the projection must clear the cache.
    context.refine_projection(Term::free_var(&r), 0, nat(1));
    assert_eq!(reduce(&mut context, proj), Ok(nat(1)));
}

#[test]
fn redefine_invalidates_reduction_cached_under_the_old_value() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    let x: Term = Term::free_var(&x_binder);

    // First definition: x reduces to 4 and the reduct — which no longer mentions `x` — is cached.
    context.define(&x_binder, &nat(4), None);
    assert_eq!(reduce(&mut context, x.clone()), Ok(nat(4)));

    // Rebinding the same label must evict that entry even though a selective retain keyed on mentions of `x` cannot see it.
    context.define(&x_binder, &nat(5), None);
    assert_eq!(reduce(&mut context, x), Ok(nat(5)));
}

#[test]
fn leave_frame_with_definitions_invalidates_cached_reduction() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    let x: Term = Term::free_var(&x_binder);

    // Inside a frame, define x and reduce — the cache will hold x → "inner".
    context.with_frame(|context| {
        context.define(&x_binder, &nat(4), None);
        assert_eq!(reduce(context, x.clone()), Ok(nat(4)));
    });

    // After the frame pops, x has no definition again. A stale cache entry would still return "inner"; the cache clear on leave_frame prevents that.
    assert_eq!(reduce(&mut context, x.clone()), Ok(x));
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

#[test]
fn refinement_is_suppressible() {
    let mut context = context();
    let b_binder = context.fresh(Some("b"));
    let b = Term::free_var(&b_binder);
    let truth = Term::intrinsic(Intrinsic::Bool(true));

    context.refine(&b_binder, &truth);

    // With the refinement active, `b` reduces to its counterfactual value.
    assert_eq!(reduce(&mut context, b.clone()), Ok(truth));

    // Suppressed (as in re-validation), `b` is abstract again.
    let reduced = context.with_suppressed_refinements(|context| reduce(context, b.clone()));
    assert_eq!(reduced, Ok(b));
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

/// Writing the universe scheme a name already has rewrites nothing, so the reduction cache keeps its reducts: the second reduction of a closed term across such a write costs no steps. A scheme that does change still clears, since a reduct through the old scheme is unsound.
#[test]
fn an_unchanged_universe_scheme_keeps_the_reduction_cache_warm() {
    let mut context = context();
    let poly = context.fresh(Some("poly"));
    let scheme = UniverseContext {
        parameter_count: 1,
        constraints: Vec::new(),
    };
    context.assume(
        &poly,
        &Term::type_at(Level::param(UniverseParam(0)).succ().unwrap()),
    );
    context.define(&poly, &Term::type_at(Level::param(UniverseParam(0))), None);
    context.set_assumption_universe_context(&poly, scheme.clone());

    let term = Term::intrinsic(Intrinsic::nat_add(
        nat(3),
        Term::intrinsic(Intrinsic::nat_add(nat(2), nat(2))),
    ));
    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(7)));
    let spent = context.consumed().units();

    context.set_assumption_universe_context(&poly, scheme.clone());
    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(7)));
    assert_eq!(
        context.consumed().units(),
        spent,
        "an unchanged scheme leaves the reduct cached, so the second reduction is a hit"
    );

    context.set_assumption_universe_context(
        &poly,
        UniverseContext {
            parameter_count: 2,
            constraints: Vec::new(),
        },
    );
    assert_eq!(reduce(&mut context, term), Ok(nat(7)));
    assert!(
        context.consumed().units() > spent,
        "a changed scheme clears the cache, so the third reduction is paid again"
    );
}

/// A closed reduct is served across universe spellings: a definition unfolded at a universe metavariable and then at a constant is one computation, the second answered from the first through the cache's erased second key with the reduct's level rewritten to the asking spelling. Checking writes a term with metavariables and totality reads it with them solved, so without the second key every fold over a literal would run once per phase and per mention.
#[test]
fn a_closed_reduct_is_served_across_universe_spellings() {
    let mut context = context();
    // A global rather than a minted local: the second door serves closed terms alone, since a local-bearing reduct dies with its declaration.
    let poly = Free::Global(nominal("poly"));
    let parameter = Level::param(UniverseParam(0));
    context.assume(&poly, &Term::type_at(parameter.succ().unwrap()));
    context.define(&poly, &Term::type_at(parameter), None);
    context.set_assumption_universe_context(
        &poly,
        UniverseContext {
            parameter_count: 1,
            constraints: Vec::new(),
        },
    );

    let meta = Level::meta(UniverseMetaId(7));
    assert_eq!(
        reduce(&mut context, Term::instance_of(&poly, vec![meta.clone()])),
        Ok(Term::type_at(meta.clone()))
    );
    let spent = context.consumed().units();
    assert_eq!(
        reduce(
            &mut context,
            Term::instance_of(&poly, vec![Level::constant(3)])
        ),
        Ok(Term::type_at(Level::constant(3))),
        "the reduct is rewritten to the asking spelling's level"
    );
    assert_eq!(
        context.consumed().units(),
        spent,
        "and served without a step, since the two spellings share one erased key"
    );
}

/// A spelling that is its own weak-head form keeps its binder names when the erased door answers for it: the stored entry belongs to another declaration's Π-type, α-equivalent and equal once universes are erased, and the answer is the asking spelling itself rather than that one. Served the stored spelling, `satisfy Show(Tree)`'s refusal would read `(T: Type) -> Type` for a `Tree` declared over `A`.
#[test]
fn a_self_entry_served_across_spellings_keeps_the_asking_binder_names() {
    let mut context = context();
    let stored = Term::func_type(
        [(Free::local(1, Some("T")), Term::type_at(Level::constant(0)))],
        Term::type_at(Level::constant(0)),
    );
    assert_eq!(reduce(&mut context, stored.clone()), Ok(stored));

    let asked = Term::func_type(
        [(
            Free::local(2, Some("A")),
            Term::type_at(Level::meta(UniverseMetaId(3))),
        )],
        Term::type_at(Level::meta(UniverseMetaId(4))),
    );
    let answer = reduce(&mut context, asked.clone()).expect("a Π-type is its own form");
    assert_eq!(answer, asked);
    assert!(
        answer.to_string().starts_with("(A: "),
        "the asking spelling's binder, not the stored one's: {answer}"
    );
}

/// A guard answers a term the carriers' readers hold equal to it: its sum commuted, and its dual with the sum commuted, neither a spelling the key is held under. The kernel's `whnf::equations_tests` holds this proposition under this name, and `curios_analysis::answers` is the one rule both ask.
#[test]
fn a_guard_answers_a_term_the_readers_hold_equal_to_it() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let sum = |left: &Free, right: &Free| {
        Term::intrinsic(Intrinsic::nat_add(
            Term::free_var(left),
            Term::free_var(right),
        ))
    };
    let guard = Term::intrinsic(Intrinsic::nat_lt(sum(&a, &b), nat(10)));
    let commuted = Term::intrinsic(Intrinsic::nat_lt(sum(&b, &a), nat(10)));
    let dual = Term::intrinsic(Intrinsic::NatLe(nat(10), sum(&b, &a)));

    assert_eq!(
        reduce(&mut context, commuted.clone()).map(|reduct| reduct.as_bool()),
        Ok(None),
        "with no guard the respelling is the stuck comparison it is"
    );

    let canonical = shallow_scrutinee(&context, &guard);
    context.refine_scrutinee_spellings(
        vec![(canonical, guard, false)],
        &Term::intrinsic(Intrinsic::Bool(true)),
    );

    assert_eq!(
        reduce(&mut context, commuted).map(|reduct| reduct.as_bool()),
        Ok(Some(true))
    );
    assert_eq!(
        reduce(&mut context, dual).map(|reduct| reduct.as_bool()),
        Ok(Some(false))
    );
}

/// The binders and terms the tests of what reduction asks conversion share, as the kernel's `whnf::equations_tests` has them: `a`, `b`, `c` at `Nat`, `f: (Nat) -> Nat` and `h: (Bool) -> Nat`.
struct Asked {
    a: Free,
    b: Free,
    c: Free,
    f: Free,
    h: Free,
}

impl Asked {
    fn over(context: &mut Context) -> Self {
        let asked = Asked {
            a: context.fresh(Some("a")),
            b: context.fresh(Some("b")),
            c: context.fresh(Some("c")),
            f: context.fresh(Some("f")),
            h: context.fresh(Some("h")),
        };
        let x = context.fresh(Some("x"));
        let flag = context.fresh(Some("flag"));
        let nat_type = Term::intrinsic(Intrinsic::NatType);
        for binder in [&asked.a, &asked.b, &asked.c] {
            context.assume(binder, &nat_type);
        }
        context.assume(
            &asked.f,
            &Term::func_type([(x, nat_type.clone())], nat_type.clone()),
        );
        context.assume(
            &asked.h,
            &Term::func_type([(flag, Term::intrinsic(Intrinsic::BoolType))], nat_type),
        );
        asked
    }

    /// `h(flag) < 5`.
    fn over_flag(&self, flag: Term) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&self.h), [flag]),
            nat(5),
        ))
    }

    fn call(&self, left: &Free, right: &Free) -> Term {
        Term::apply(
            Term::free_var(&self.f),
            [Term::intrinsic(Intrinsic::nat_add(
                Term::free_var(left),
                Term::free_var(right),
            ))],
        )
    }

    fn guard(&self) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(self.call(&self.a, &self.b), nat(10)))
    }

    fn respelled(&self) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(self.call(&self.b, &self.a), nat(10)))
    }

    /// Register the guard as holding, for the frame `inside` runs in.
    fn under<T>(&self, context: &mut Context, inside: impl FnOnce(&mut Context) -> T) -> T {
        context.with_frame(|context| {
            let guard = self.guard();
            let canonical = shallow_scrutinee(context, &guard);
            context.refine_scrutinee_spellings(
                vec![(canonical, guard, false)],
                &Term::intrinsic(Intrinsic::Bool(true)),
            );
            inside(context)
        })
    }
}

/// A guard answers a term the elaborator's own conversion holds equal to its scrutinee: a call respelled in its argument, the guard's dual over that call, and — where the scrutinee is no operation of the algebra — the call itself. The kernel's `whnf::equations_tests` holds this proposition under this name.
#[test]
fn a_guard_answers_a_term_conversion_holds_equal_to_it() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let decided =
        |context: &mut Context, term: Term| reduce(context, term).map(|reduct| reduct.as_bool());

    asked.under(&mut context, |context| {
        assert_eq!(decided(context, asked.respelled()), Ok(Some(true)));
        let dual = Term::intrinsic(Intrinsic::NatLe(nat(10), asked.call(&asked.b, &asked.a)));
        assert_eq!(decided(context, dual), Ok(Some(false)));
        let other = Term::intrinsic(Intrinsic::nat_lt(asked.call(&asked.a, &asked.c), nat(10)));
        assert_eq!(decided(context, other), Ok(None));
    });

    context.with_frame(|context| {
        let scrutinee = asked.call(&asked.a, &asked.b);
        let canonical = shallow_scrutinee(context, &scrutinee);
        context.refine_scrutinee_spellings(vec![(canonical, scrutinee, false)], &nat(0));

        assert_eq!(reduce(context, asked.call(&asked.b, &asked.a)), Ok(nat(0)));
        let other = asked.call(&asked.a, &asked.c);
        assert_eq!(reduce(context, other.clone()), Ok(other));
    });
}

/// A shared analysis reads a term by plain reduction, which asks the elaborator's conversion nothing, in either order the two reductions are taken in: under a guard, the respelling a judgment's reduction answers by asking stays stuck through `Env::force`, so totality rests on no verdict of conversion's, and a reduct one of the two filed does not answer the other. The kernel's `whnf::equations_tests` holds the first half under this name.
///
/// Mutation-checked: with `Env::force` reducing as a judgment does the respelling is `true` to the analysis, and with the judgment's reduction table read whichever reduction asks, the judgment's `true` answers the analysis in the first order.
#[test]
fn a_shared_analysis_reads_a_term_by_reduction_that_asks_nothing() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let judged =
        |context: &mut Context| reduce(context, asked.respelled()).map(|reduct| reduct.as_bool());
    let read = |context: &mut Context| {
        curios_analysis::Env::force(context, &asked.respelled())
            .ok()
            .map(|reduct| reduct.as_bool())
    };

    asked.under(&mut context, |context| {
        assert_eq!(judged(context), Ok(Some(true)));
        assert_eq!(read(context), Some(None));
    });
    asked.under(&mut context, |context| {
        assert_eq!(read(context), Some(None));
        assert_eq!(judged(context), Ok(Some(true)));
    });
}

/// A question reduction puts to conversion is answered by plain reduction, so an answer that needs a question answered inside a question is refused: under the guards `f(a + b) < 10` and `h(f(a + b) < 10) < 5`, the term `h(f(b + a) < 10) < 5` stays stuck, the second guard naming every binder it names. The kernel's `whnf::equations_tests` holds this proposition under this name.
///
/// Mutation-checked: with the lookup asking under plain reduction too, the question decides the argument and the term is `true`.
#[test]
fn a_question_is_answered_by_reduction_that_asks_nothing() {
    let mut context = context();
    let asked = Asked::over(&mut context);

    asked.under(&mut context, |context| {
        let guard = asked.over_flag(asked.guard());
        let canonical = shallow_scrutinee(context, &guard);
        context.refine_scrutinee_spellings(
            vec![(canonical, guard, false)],
            &Term::intrinsic(Intrinsic::Bool(true)),
        );

        assert_eq!(
            reduce(context, asked.respelled()).map(|reduct| reduct.as_bool()),
            Ok(Some(true)),
            "one question deep the argument is decided, or the refusal below proves nothing"
        );
        let nested = asked.over_flag(asked.respelled());
        assert_eq!(
            reduce(context, nested).map(|reduct| reduct.as_bool()),
            Ok(None)
        );
    });
}

/// The binders the tests of a call under two proofs share, as the kernel's `whnf::equations_tests` has them: `a` at `Nat`, a proposition `bound`, two proofs of it, and `w: (n: Nat, at: bound) -> Nat`.
struct Proved {
    a: Free,
    w: Free,
    p1: Free,
    p2: Free,
}

impl Proved {
    fn over(context: &mut Context) -> Self {
        let proved = Proved {
            a: context.fresh(Some("a")),
            w: context.fresh(Some("w")),
            p1: context.fresh(Some("p1")),
            p2: context.fresh(Some("p2")),
        };
        let bound = context.fresh(Some("bound"));
        let n = context.fresh(Some("n"));
        let at = context.fresh(Some("at"));
        let nat_type = Term::intrinsic(Intrinsic::NatType);
        let proposition = Term::free_var(&bound);
        context.assume(&proved.a, &nat_type);
        context.assume(&bound, &Term::prop());
        context.assume(
            &proved.w,
            &Term::func_type([(n, nat_type.clone()), (at, proposition.clone())], nat_type),
        );
        context.assume(&proved.p1, &proposition);
        context.assume(&proved.p2, &proposition);
        proved
    }

    fn call(&self, proof: &Free) -> Term {
        Term::apply(
            Term::free_var(&self.w),
            [Term::free_var(&self.a), Term::free_var(proof)],
        )
    }

    fn below(&self, proof: &Free) -> Term {
        Term::intrinsic(Intrinsic::nat_lt(self.call(proof), nat(10)))
    }
}

/// Register `scrutinee` as assumed to be `value`, for the frame `inside` runs in.
fn assumed<T>(
    context: &mut Context,
    scrutinee: Term,
    value: Term,
    inside: impl FnOnce(&mut Context) -> T,
) -> T {
    context.with_frame(|context| {
        let canonical = shallow_scrutinee(context, &scrutinee);
        context.refine_scrutinee_spellings(vec![(canonical, scrutinee, false)], &value);
        inside(context)
    })
}

/// A guard answers a call under another proof of its bound, and so does a match on the call itself: the form names a proof the scrutinee does not, so it is no reduct of it, and the filter in front of every entry passes over a binder that is itself a proof. The kernel's `whnf::equations_tests` holds this proposition under this name.
///
/// Mutation-checked: with a proof counted as any other binder, both forms stay stuck.
#[test]
fn a_guard_answers_a_call_under_another_proof_of_its_bound() {
    let mut context = context();
    let proved = Proved::over(&mut context);
    let truth = Term::intrinsic(Intrinsic::Bool(true));

    assumed(&mut context, proved.below(&proved.p1), truth, |context| {
        assert_eq!(
            reduce(context, proved.below(&proved.p2)).map(|reduct| reduct.as_bool()),
            Ok(Some(true))
        );
    });
    assumed(&mut context, proved.call(&proved.p1), nat(0), |context| {
        assert_eq!(reduce(context, proved.call(&proved.p2)), Ok(nat(0)));
    });
}

/// A form naming a binder the scrutinee does not, outside a proof, is put to no entry: `f(b + 0 * c) < 10` converts with the guard `f(b) < 10` and names `c`, which is no proof, so the form stays stuck. The kernel's `whnf::equations_tests` holds this proposition under this name.
///
/// Mutation-checked: with the filter passing every binder, the form is asked about and is `true`.
#[test]
fn a_form_naming_another_binder_outside_a_proof_is_put_to_no_equation() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let below = |argument: Term| {
        let call = Term::apply(Term::free_var(&asked.f), [argument]);
        Term::intrinsic(Intrinsic::nat_lt(call, nat(10)))
    };
    let erased = Intrinsic::NatMul(nat(0), Term::free_var(&asked.c));
    let guard = below(Term::free_var(&asked.b));
    let respelled = below(Term::intrinsic(Intrinsic::nat_add(
        Term::free_var(&asked.b),
        Term::intrinsic(erased),
    )));
    let bool_type = Term::intrinsic(Intrinsic::BoolType);
    let truth = Term::intrinsic(Intrinsic::Bool(true));

    assert_eq!(
        convert(&mut context, &bool_type, &guard, &respelled),
        Ok(true),
        "the two convert, or the miss below is no limit of the filter"
    );
    assumed(&mut context, guard, truth, |context| {
        assert_eq!(
            reduce(context, respelled).map(|reduct| reduct.as_bool()),
            Ok(None)
        );
    });
}

/// An entry's reduced spelling is settled by the reduction that asks for it, once for each. Under the guard `f(a + b) < 10`, the inner guard `f(b + a) < 10 && c < 3`, registered in a frame of its own so the outer guard stands while it is settled, reduces to `c < 3` for a judgment, which asks its conversion about the left operand, and stands as written for plain reduction; so `c < 3` is `true` to a judgment and stuck to an analysis, whichever asked first. The kernel's `whnf::equations_tests` holds this proposition under this name.
///
/// Mutation-checked both ways: with the spelling read as a judgment's whichever reduction asks, plain reduction is handed `true` in the first order, and with it filed there whichever reduction settled it, the judgment meets plain reduction's spelling in the second and answers nothing.
#[test]
fn a_reduced_spelling_is_settled_by_the_reduction_that_asks_for_it() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let probe = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&asked.c), nat(3)));
    let inner = Term::intrinsic(Intrinsic::BoolAnd(asked.respelled(), probe.clone()));
    let judged =
        |context: &mut Context| reduce(context, probe.clone()).map(|reduct| reduct.as_bool());
    let read = |context: &mut Context| {
        curios_analysis::Env::force(context, &probe)
            .ok()
            .map(|reduct| reduct.as_bool())
    };
    let under = |context: &mut Context, inside: &dyn Fn(&mut Context)| {
        asked.under(context, |context| {
            context.with_frame(|context| {
                let canonical = shallow_scrutinee(context, &inner);
                context.refine_scrutinee_spellings(
                    vec![(canonical, inner.clone(), false)],
                    &Term::intrinsic(Intrinsic::Bool(true)),
                );
                inside(context)
            })
        })
    };

    under(&mut context, &|context| {
        assert_eq!(judged(context), Ok(Some(true)));
        assert_eq!(read(context), Some(None));
    });
    under(&mut context, &|context| {
        assert_eq!(read(context), Some(None));
        assert_eq!(judged(context), Ok(Some(true)));
    });
}

/// A stuck fold is taken again over the atoms the elaborator's own conversion holds one, a fold that decides no more is left as plain reduction spells it, and plain reduction takes no fold again. The kernel's `whnf::equations_tests` holds this proposition under this name.
#[test]
fn a_stuck_fold_is_taken_again_over_atoms_conversion_holds_one() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let left = asked.call(&asked.a, &asked.b);
    let right = asked.call(&asked.b, &asked.a);
    let decided =
        |context: &mut Context, term: Term| reduce(context, term).map(|reduct| reduct.as_bool());

    let equal = Term::intrinsic(Intrinsic::nat_eql(left.clone(), right.clone()));
    assert_eq!(decided(&mut context, equal.clone()), Ok(Some(true)));
    let at_most = Term::intrinsic(Intrinsic::nat_lte(left.clone(), right.clone()));
    assert_eq!(decided(&mut context, at_most), Ok(Some(true)));
    let difference = Term::intrinsic(Intrinsic::nat_sub(left.clone(), right.clone()));
    assert_eq!(reduce(&mut context, difference), Ok(nat(0)));

    let other = Term::intrinsic(Intrinsic::nat_eql(
        left.clone(),
        asked.call(&asked.a, &asked.c),
    ));
    assert_eq!(decided(&mut context, other), Ok(None));

    let read = curios_analysis::Env::force(&mut context, &equal).ok();
    assert_eq!(
        read.map(|reduct| reduct.as_bool()),
        Some(None),
        "plain reduction takes no fold again"
    );

    let undecided = Term::intrinsic(Intrinsic::nat_lt(
        left,
        Term::intrinsic(Intrinsic::NatMul(right, Term::free_var(&asked.c))),
    ));
    let plain = curios_analysis::Env::force(&mut context, &undecided).ok();
    assert_eq!(reduce(&mut context, undecided).ok(), plain);
    assert!(plain.is_some());
}

/// A fold inside a question is not taken again, a question being answered by plain reduction: `h(f(a + b) == f(b + a)) == h(true)` stays stuck. The kernel's `whnf::equations_tests` holds this proposition under this name.
///
/// Mutation-checked: with the fold taken again under plain reduction too, the question decides the argument and the comparison is `true`.
#[test]
fn a_fold_inside_a_question_is_not_taken_again() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let h = &asked.h;
    let equal = Term::intrinsic(Intrinsic::nat_eql(
        asked.call(&asked.a, &asked.b),
        asked.call(&asked.b, &asked.a),
    ));
    let nested = Term::intrinsic(Intrinsic::nat_eql(
        Term::apply(Term::free_var(h), [equal.clone()]),
        Term::apply(Term::free_var(h), [Term::intrinsic(Intrinsic::Bool(true))]),
    ));

    assert_eq!(
        reduce(&mut context, equal).map(|reduct| reduct.as_bool()),
        Ok(Some(true)),
        "one question deep the fold is decided, or the refusal below proves nothing"
    );
    assert_eq!(
        reduce(&mut context, nested).map(|reduct| reduct.as_bool()),
        Ok(None)
    );
}

/// An equation in force follows the solution an arm is checked under: under the guard `f(n) < 5`, an arm that refines `n` to `0` holds `f(0) < 5`, the recorded spelling stepping aside for that instance, and gives both back when it is left. The kernel's `whnf::equations_tests` holds this proposition under this name, by substituting the solution through the arm; here the variable stays spelled, so a term of the arm that still names it — the guard as written, or `f(n + 0) < 5`, which is no key — is read as the arm spells it.
///
/// Mutation-checked four ways. With `Context::refine` restating nothing the instance stays stuck inside the arm. With the recorded equation left answering beside its instance, the recorded key still answers there. With a key spelled as it is written, the guard and its instance are two keys in the arm. With a stuck form put to the reduced spellings as it is written, `f(n + 0) < 5` is put to no equation, the instance naming no `n`. `context::tests` holds an instance that names no local withheld.
#[test]
fn a_case_equation_follows_the_solution_an_arm_is_checked_under() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let f = context.fresh(Some("f"));
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    context.assume(&n, &nat_type);
    context.assume(&f, &Term::func_type([(x, nat_type.clone())], nat_type));
    let guard = |argument: Term| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&f), [argument]),
            nat(5),
        ))
    };
    let written = guard(Term::free_var(&n));
    let at_zero = guard(nat(0));
    let respelled = guard(Term::intrinsic(Intrinsic::nat_add(
        Term::free_var(&n),
        nat(0),
    )));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let decided = |context: &mut Context, term: &Term| {
        reduce(context, term.clone()).map(|reduct| reduct.as_bool())
    };

    context.with_frame(|context| {
        let recorded = shallow_scrutinee(context, &written);
        context
            .refine_scrutinee_spellings(vec![(recorded.clone(), written.clone(), false)], &truth);
        assert_eq!(
            decided(context, &at_zero),
            Ok(None),
            "the instance is no term the guard was written as, or the arm below proves nothing"
        );

        context.with_frame(|context| {
            context.refine(&n, &nat(0));

            assert_eq!(
                decided(context, &at_zero),
                Ok(Some(true)),
                "the equation answers at the arm's solution"
            );
            assert_eq!(
                context.scrutinee_reduct(&recorded, &written),
                None,
                "and the recorded spelling steps aside for it"
            );
            assert_eq!(
                shallow_scrutinee(context, &written),
                shallow_scrutinee(context, &at_zero),
                "a key is spelled as the arm spells it, so the guard as written meets the instance for one lookup"
            );
            assert_eq!(
                decided(context, &written),
                Ok(Some(true)),
                "a term that still names the variable is read as the arm spells it"
            );
            assert_eq!(
                decided(context, &respelled),
                Ok(Some(true)),
                "and so is one that is no key, where it reaches the reduced spellings"
            );
        });

        assert_eq!(
            decided(context, &at_zero),
            Ok(None),
            "the restatement does not outlive the arm"
        );
        assert_eq!(
            decided(context, &written),
            Ok(Some(true)),
            "and the recorded equation answers again"
        );
    });
}

/// A solution may name a variable an arm inside solves in turn: under the guard `f(n) < 5`, the arm that solves `n` as `k + 1` holds the equation at `f(k + 1) < 5`, and the arm inside it that solves `k` as `0` holds it at that instance again. The kernel has substituted both solutions by then; here both variables stay spelled, so a term is spelled as refined until no refined variable is left in it.
///
/// Mutation-checked: with a term spelled as refined for one round only, the guard as written still names `k` in the inner arm and meets no equation.
#[test]
fn a_case_equation_follows_a_solution_naming_a_variable_solved_in_turn() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let k = context.fresh(Some("k"));
    let f = context.fresh(Some("f"));
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    context.assume(&n, &nat_type);
    context.assume(&k, &nat_type);
    context.assume(&f, &Term::func_type([(x, nat_type.clone())], nat_type));
    let guard = |argument: Term| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&f), [argument]),
            nat(5),
        ))
    };
    let written = guard(Term::free_var(&n));
    let successor = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&k), nat(1)));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let decided = |context: &mut Context, term: &Term| {
        reduce(context, term.clone()).map(|reduct| reduct.as_bool())
    };

    context.with_frame(|context| {
        let recorded = shallow_scrutinee(context, &written);
        context.refine_scrutinee_spellings(vec![(recorded, written.clone(), false)], &truth);

        context.with_frame(|context| {
            context.refine(&n, &successor);
            assert_eq!(decided(context, &guard(successor.clone())), Ok(Some(true)));

            context.with_frame(|context| {
                context.refine(&k, &nat(0));
                assert_eq!(
                    decided(context, &guard(nat(1))),
                    Ok(Some(true)),
                    "the equation followed both solutions"
                );
                assert_eq!(
                    decided(context, &written),
                    Ok(Some(true)),
                    "and the guard as written is read under both"
                );
            });

            assert_eq!(decided(context, &written), Ok(Some(true)));
        });
    });
}

/// A guard over a local definition answers a term spelled over the definition's value: under `let n = a + 0` and the guard `f(n) < 5`, `f(a) < 5` is `true`. The kernel holds the guard by value, so the filter in front of its equation reads `a`; the elaborator's key names `n`, and its filter reads the scrutinee as the kernel spells it, or the form is put to no equation. `curios`'s `tests::matching` holds a program of this shape under this name, through both checkers.
///
/// Mutation-checked: with the filter reading the scrutinee as written, `f(a) < 5` stays stuck.
#[test]
fn a_guard_over_a_local_definition_answers_a_term_over_its_value() {
    let mut context = context();
    let asked = Asked::over(&mut context);
    let n = context.fresh(Some("n"));
    let over = |argument: Term| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&asked.f), [argument]),
            nat(5),
        ))
    };
    let truth = Term::intrinsic(Intrinsic::Bool(true));

    context.with_frame(|context| {
        context.assume(&n, &Term::intrinsic(Intrinsic::NatType));
        context.define(
            &n,
            &Term::intrinsic(Intrinsic::nat_add(Term::free_var(&asked.a), nat(0))),
            None,
        );
        let guard = over(Term::free_var(&n));
        let key = shallow_scrutinee(context, &guard);
        context.refine_scrutinee_spellings(vec![(key, guard, false)], &truth);

        let by_value = over(Term::free_var(&asked.a));
        assert_eq!(
            reduce(context, by_value).map(|reduct| reduct.as_bool()),
            Ok(Some(true))
        );
        let other = over(Term::free_var(&asked.b));
        assert_eq!(
            reduce(context, other).map(|reduct| reduct.as_bool()),
            Ok(None)
        );
    });
}
