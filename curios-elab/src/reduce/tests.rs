use curios_core::*;
use {
    crate::*,
    curios_num::{Floating, Integer},
    curios_utilities::{Grain, PackedBin, Qualifier},
};

/// A declaration's name, from the path a test writes. Fixture-only.
fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A stand-in for a discharged bound. Reduction never inspects one — proof irrelevance makes its value unobservable, and these tests are about the fold laws rather than the obligation.
fn qed() -> Term {
    Term::free_var(&Free::local(9_999, Some("qed")))
}

fn context() -> Context {
    Context::new(100_000, crate::SYNTAX)
}

fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

#[test]
fn nat_to_byte_reflects_byte_to_nat() {
    let mut context = context();
    let byte_binder = context.fresh(Some("byte"));
    let byte = Term::free_var(&byte_binder);
    let term = Term::intrinsic(Intrinsic::NatToByte(Term::intrinsic(Intrinsic::ByteToNat(
        byte.clone(),
    ))));

    assert_eq!(reduce(&mut context, term), Ok(byte));
}

#[test]
fn reduce_apply_beta_reduces() {
    let mut context = context();
    let x = context.fresh(Some("x"));

    let term: Term = Term::apply(
        Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x)),
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
        [(n.clone(), nat_type.clone())],
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
            countdown.clone(),
            Term::func_type([(n.clone(), nat_type.clone())], nat_type.clone()),
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
            countdown.clone(),
            Term::func_type([(n.clone(), nat_type.clone())], nat_type),
            body,
        )],
        Term::apply(Term::free_var(&countdown), [nat(2)]),
    );
    assert_eq!(reduce_forced(&mut context, concrete), Ok(nat(0)));
}

#[test]
fn reduce_inductive_match_selects_case_and_projects_payload() {
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
            ("some", vec![x.clone()], Term::free_var(&x)),
        ],
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(42)));
}

#[test]
fn reduce_inductive_match_absent_tag_takes_default() {
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
fn reduce_inductive_match_present_tag_ignores_default() {
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
            ("some", vec![x.clone()], Term::free_var(&x)),
        ],
        nat(99),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(42)));
}

#[test]
fn reduce_nat_fold_zero_takes_the_zero_case() {
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
fn reduce_let_then_var_unfolds_definition() {
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
            Term::universe_inst(raw, vec![Level::constant(3)])
        ),
        Ok(Term::type_at(Level::constant(3)))
    );
}

#[test]
fn reduce_let_binds_each_value_to_its_own_slot() {
    // Two distinct bindings referenced together in the tail: pins the positional correctness of `reduce_let`'s environment open. The tail is `(λ p q. q) a b`, so the result is `b`'s value — and only if `a`/`b` land in the right slots. A transposed open would beta-reduce to `a`'s value instead.
    let mut context = context();
    let p = context.fresh(Some("p"));
    let q = context.fresh(Some("q"));
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));

    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let pick_second = Term::apply(
        Term::func(
            [(p.clone(), nat_type.clone()), (q.clone(), nat_type.clone())],
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
fn reduce_let_shadowing_tail_picks_innermost() {
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
fn reduce_let_shadowing_value_sees_the_outer_binding() {
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
    let mut context = Context::with_default_budget(crate::SYNTAX);
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
    // Each level's scrutinee is the level below it, so reducing the top costs one nested `reduce` per link. That is the depth `PendingMatch` used to absorb and `recurse` now carries, and it is data-shaped: a tower this tall is generated rather than written.
    //
    // Deep enough that a regression is a stack overflow rather than a slow test, and under a budget large enough that the budget is not what decides it — which is the property `reduce`'s own documentation claims.
    //
    // **The stated budget is the part that changed with pricing, and it is not incidental.** A guarded level now charges `Cost::FRAME` when it is a new peak, so depth is a priced resource and the default would decide this test rather than the stack: ten thousand levels cost about 10.2 million units of frames alone, which is past what the compiler ships. A test whose subject is the stack has to take the budget out of the answer, and stating one is how.
    const DEEP: usize = 10_000;

    let mut context = Context::new(100_000_000, crate::SYNTAX);
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
fn reduce_var_cycle_times_out() {
    let mut context = context();
    let loop_ = context.fresh(Some("loop"));

    context.define(&loop_, &Term::free_var(&loop_), None);

    assert!(reduce(&mut context, Term::free_var(&loop_)).is_err_and(|spent| spent.is_exhausted()));
}

#[test]
fn reduce_int_add_computes() {
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
fn reduce_int_eql_returns_true_or_false_bool() {
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
fn reduce_flt_folds_through_the_model() {
    let mut context = context();

    let flt = |value: f32| {
        Term::from(Subterm::Intrinsic(Intrinsic::Flt(Floating::from_f32(
            value,
        ))))
    };

    // Two literals fold by calling the model, so the answer is a value rather than a normal form standing in for one.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_mul(flt(1.5), flt(2.0)))
        ),
        Ok(flt(3.0)),
    );

    // The cases the host would leave to itself, and the model does not: division by zero is a value, and `0.0 / 0.0` is the one NaN, whose sign `copysign` therefore cannot read.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::flt_div(flt(1.0), flt(0.0)))
        ),
        Ok(flt(f32::INFINITY)),
    );
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::FltCopysign(
                flt(1.0),
                Term::intrinsic(Intrinsic::flt_div(flt(0.0), flt(0.0))),
            ))
        ),
        Ok(flt(1.0)),
    );

    // A symbolic operand still rebuilds the neutral term.
    let symbolic = Term::intrinsic(Intrinsic::flt_mul(
        Term::free_var(&Free::local(1, Some("x"))),
        flt(2.0),
    ));
    assert_eq!(reduce(&mut context, symbolic.clone()), Ok(symbolic));
}

#[test]
fn reduce_list_get_returns_element_at_index() {
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
fn reduce_list_get_errors_on_out_of_bounds() {
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
fn reduce_bin_append_adds_byte() {
    let mut context = context();

    let bin = Subterm::Intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2])));
    let byte: Subterm = Subterm::Intrinsic(Intrinsic::Byte(3));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::bin_append(Grain::X, bin, byte)).into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bin(
            Grain::X,
            PackedBin::from_bytes(vec![1, 2, 3])
        ))
        .into())
    );
}

#[test]
fn reduce_bin_append_adds_the_full_byte_range() {
    let mut context = context();

    let bin = Subterm::Intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2])));
    let byte: Subterm = Subterm::Intrinsic(Intrinsic::Byte(255));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Intrinsic(Intrinsic::bin_append(Grain::X, bin, byte)).into()
        ),
        Ok(Subterm::Intrinsic(Intrinsic::Bin(
            Grain::X,
            PackedBin::from_bytes(vec![1, 2, 255])
        ))
        .into())
    );
}

#[test]
fn reduce_list_append_adds_element() {
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
fn reduce_proj_beta_reduces() {
    let mut context = context();

    let term: Term = Term::proj(Term::tuple([nat(1), nat(2)]), 1);

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(2)));
}

#[test]
fn reduce_proj_refinement_lookup() {
    let mut context = context();
    let r = context.fresh(Some("r"));

    context.refine_projection(Term::free_var(&r), 0, nat(1));

    let term: Term = Term::proj(Term::free_var(&r), 0);

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(1)));
}

#[test]
fn reduce_does_not_eta_reduce_tuple() {
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
        [(y.clone(), Term::type_ground())],
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
    let function = Term::free_var(&classify);
    let registered = Term::apply(
        Term::universe_inst(function.clone(), vec![Level::meta(UniverseMetaId(0))]),
        [nat(0)],
    );
    let probe = Term::apply(
        Term::universe_inst(function, vec![Level::meta(UniverseMetaId(1))]),
        [nat(0)],
    );
    let canonical = canonical_scrutinee(&mut context, &registered).unwrap();
    context.refine_scrutinee(canonical, registered, nat(1));

    assert_eq!(reduce(&mut context, probe), Ok(nat(1)));
}

#[test]
fn projection_refinement_ignores_fresh_universe_instances() {
    let mut context = context();
    let record_binder = context.fresh(Some("record"));
    let record = Term::free_var(&record_binder);
    let registered = Term::apply(
        Term::universe_inst(record.clone(), vec![Level::meta(UniverseMetaId(0))]),
        [nat(0)],
    );
    let probe = Term::apply(
        Term::universe_inst(record, vec![Level::meta(UniverseMetaId(1))]),
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
fn reduce_unsolved_metavar_is_neutral() {
    let mut context = context();
    let m = Term::metavar(0);

    // No store entry, or an unsolved one, both reduce to the metavariable itself.
    assert_eq!(reduce(&mut context, m.clone()), Ok(m.clone()));

    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());
    assert_eq!(reduce(&mut context, m.clone()), Ok(m));
}

#[test]
fn reduce_solved_metavar_yields_solution() {
    let mut context = context();
    let m = Term::metavar(0);

    context.birth_metavar(MetaId(0), Vec::new(), Term::type_ground());

    // An unsolved metavariable reduces to itself, but that reduct names an unsolved metavariable, so it is deliberately not memoized.
    assert_eq!(reduce(&mut context, m.clone()), Ok(m.clone()));

    let solution = nat(1);
    context.solve_metavar(MetaId(0), solution.clone());

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

// === Type-level partial arithmetic ===========================================
//
// A literal zero divisor is mathematically undefined and reports through a `ReduceError` (the `BinGet` pattern, span and all) — never a Rust panic. Runtime *range* limits, by contrast, never error here: `Nat`/`Int` are unbounded at the type level, folds compute exactly, and the 31-bit narrowing is enforced downstream (`ersd`'s carriers at the erase boundary, the i31 traps in `cont` → wasm).

#[test]
fn reduce_nat_div_by_zero_reports() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::nat_div(
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Nat/div",
            span: None,
        })
    );

    // The divisor alone forces the trap: a neutral dividend still reports.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::nat_div(
                Term::free_var(&x),
                Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Nat/div",
            span: None,
        })
    );

    // A symbolic divisor is not a zero divisor: the term just stays stuck.
    let stuck = Term::intrinsic(Intrinsic::nat_div(
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
        Subterm::Var(Var::free(y.clone())),
        Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    ));
    assert_eq!(reduce(&mut context, stuck.clone()), Ok(stuck));
}

#[test]
fn reduce_nat_rem_by_zero_reports() {
    let mut context = context();
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::nat_rem(
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Nat/rem",
            span: None,
        })
    );
}

#[test]
fn reduce_int_div_by_zero_reports() {
    let mut context = context();
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::int_div(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(0))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Int/div",
            span: None,
        })
    );

    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::int_rem(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(0))),
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Int/rem",
            span: None,
        })
    );
}

#[test]
fn reduce_int_arithmetic_is_unbounded() {
    let mut context = context();

    // Past the runtime's i31 range the type level keeps computing exactly — the limit is the runtime's, enforced downstream, not the checker's.
    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::int_add(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from((1i64 << 30) - 1))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1))),
            )),
        ),
        Ok(Term::intrinsic(Intrinsic::Int(Integer::from(1i64 << 30))))
    );

    assert_eq!(
        reduce(
            &mut context,
            Term::intrinsic(Intrinsic::int_mul(
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1i64 << 30))),
                Subterm::Intrinsic(Intrinsic::Int(Integer::from(1i64 << 30))),
            )),
        ),
        Ok(Term::intrinsic(Intrinsic::Int(Integer::from(1i64 << 60))))
    );
}

#[test]
fn reduce_flt_to_int_answers_the_exact_integer() {
    let mut context = context();

    // The narrowing answers the *exact* unbounded integer, past what any runtime carrier holds: `2^31` is out of `i32` range, and that refusal belongs to the erasure boundary rather than here, where `Int` pretends ℤ.
    let exact = Term::intrinsic(Intrinsic::FltToInt {
        flt: Term::intrinsic(Intrinsic::Flt(Floating::from_f32(2147483648.0))),
        finite: qed(),
    });
    assert_eq!(
        reduce(&mut context, exact),
        Ok(Term::intrinsic(Intrinsic::Int(Integer::from(1i64 << 31)))),
    );

    // Outside the domain `Finite` states there is no integer to answer, so the operation stays stuck rather than inventing one. A well-typed call cannot reach this — the bound excludes a NaN — and reduction does not rely on being handed only well-typed terms.
    let nan = Term::intrinsic(Intrinsic::FltToInt {
        flt: Term::intrinsic(Intrinsic::Flt(Floating::from_f32(f32::NAN))),
        finite: qed(),
    });
    assert_eq!(reduce(&mut context, nan.clone()), Ok(nan));
}

mod intrinsic {
    use super::qed;

    use curios_core::*;
    use {
        crate::{Context, reduce},
        curios_num::Natural,
        curios_utilities::{Grain, PackedBin},
    };

    fn context() -> Context {
        Context::with_default_budget(crate::SYNTAX)
    }

    fn lit(n: u32) -> Term {
        Term::intrinsic(Intrinsic::Nat(Nat::new(n as usize)))
    }

    fn succ(inner: Term) -> Term {
        Term::intrinsic(Intrinsic::Nat(Nat::Succ(Natural::from(1u32), inner)))
    }

    fn reduced(context: &mut Context, term: Term) -> Subterm {
        Term::unwrap_or_clone(reduce(context, term).expect("reduces"))
    }

    // Symbolic successor bounds the family must decide — exactly the cases the old bespoke `lt` rule handled, now shared by the whole family (a regression guard).
    #[test]
    fn comparisons_decide_symbolic_successor_bounds() {
        let mut context = context();
        let symbolic = context.fresh(Some("x"));
        let x = || Term::free_var(&symbolic);

        // `succ x ≥ 1`: lt is false, ge is true; and `0 < succ x` is true.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_lt(succ(x()), lit(1)))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(false)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_lte(lit(1), succ(x())))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(true)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_lt(lit(0), succ(x())))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(true)),
        );

        // Shared inner: `lt(x, succ x) = true`, `le(succ x, x) = false`.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_lt(x(), succ(x())))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(true)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_lte(succ(x()), x()))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(false)),
        );

        // The Str decoder blocker: `eql(succ(succ x), 1) = false` (shapes differ once the shared floor is peeled).
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_eql(succ(succ(x())), lit(1)))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(false)),
        );

        // A non-strict bound decides `le` but leaves `lt` genuinely undecidable (neutral), since `2 ≤ succ(succ x)` says nothing about strictness.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_lte(lit(2), succ(succ(x()))))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(true)),
        );
        assert!(matches!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_lt(lit(2), succ(succ(x()))))
            ),
            Subterm::Intrinsic(Intrinsic::NatLt(..)),
        ));
    }

    // Soundness gate for the distributing `Nat/mul`: on closed inputs it must still agree with the host product — the literal fold the floor distribution subsumes (`il = ir = 0`, so only the floors `sl · sr` remain).
    #[test]
    fn mul_agrees_with_literal_product() {
        let mut context = context();
        let samples = [0u32, 1, 2, 7, 13, 100];
        for &a in &samples {
            for &b in &samples {
                assert_eq!(
                    reduced(
                        &mut context,
                        Term::intrinsic(Intrinsic::nat_mul(lit(a), lit(b)))
                    ),
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::new((a * b) as usize))),
                    "mul disagreed with the literal product on ({a}, {b})",
                );
            }
        }
    }

    // `Nat/mul` distributes a literal factor over a symbolic successor floor, the multiplicative twin of `NatAdd`'s floor law: `(x + 1) · c` and `x · c + c` reduce to the same normal form (either side may be the literal). Two symbolic operands have no literal factor, so the product stays neutral.
    #[test]
    fn mul_distributes_literal_over_symbolic_floor() {
        let mut context = context();
        let symbolic = context.fresh(Some("x"));
        let x = || Term::free_var(&symbolic);

        // `(x + 1) · 2 = x · 2 + 2`.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_mul(succ(x()), lit(2)))
            ),
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_add(
                    Term::intrinsic(Intrinsic::nat_mul(x(), lit(2))),
                    lit(2)
                )),
            ),
        );

        // Commutative: `2 · (x + 1) = 2 · x + 2`.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_mul(lit(2), succ(x())))
            ),
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_add(
                    Term::intrinsic(Intrinsic::nat_mul(lit(2), x())),
                    lit(2)
                )),
            ),
        );

        // No literal factor ⇒ neutral.
        assert!(matches!(
            reduced(&mut context, Term::intrinsic(Intrinsic::nat_mul(x(), x()))),
            Subterm::Intrinsic(Intrinsic::NatMul(..)),
        ));
    }

    // `cons(7, xs) = [7] ++ xs` over a symbolic tail `xs` — the symbolic cons `List/get` and `List/slice` previously could not peel (they folded only literal arrays), now decoded one element at a time like their `Bin` twins.
    fn list_cons_seven(xs: &Term) -> Term {
        Term::intrinsic(Intrinsic::list_concat(
            Term::intrinsic(Intrinsic::NatType),
            [
                Term::intrinsic(Intrinsic::List {
                    element: Term::intrinsic(Intrinsic::NatType),
                    items: vec![lit(7)],
                }),
                xs.clone(),
            ],
        ))
    }

    #[test]
    fn list_get_peels_symbolic_cons() {
        let mut context = context();
        let xs_binder = context.fresh(Some("xs"));
        let cons = list_cons_seven(&Term::free_var(&xs_binder));

        // `get(cons(7, xs), 0) = 7`.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::list_get(
                    Term::intrinsic(Intrinsic::NatType),
                    cons.clone(),
                    lit(0),
                    qed(),
                ))
            ),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(7usize))),
        );

        // `get(cons(7, xs), 1)` peels to `get(xs, 0)` — neutral over a symbolic tail.
        assert!(matches!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::list_get(
                    Term::intrinsic(Intrinsic::NatType),
                    cons,
                    lit(1),
                    qed(),
                ))
            ),
            Subterm::Intrinsic(Intrinsic::ListGet { .. }),
        ));
    }

    #[test]
    fn list_slice_peels_symbolic_cons() {
        let mut context = context();
        let xs_binder = context.fresh(Some("xs"));
        let cons = list_cons_seven(&Term::free_var(&xs_binder));

        // `slice(cons(7, xs), 0, 1) = [7] ++ slice(xs, 0, 0) = [7]` — one element from the front.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::list_slice(
                    Term::intrinsic(Intrinsic::NatType),
                    cons.clone(),
                    lit(0),
                    lit(1),
                    qed(),
                ))
            ),
            Subterm::Intrinsic(Intrinsic::List {
                element: Term::intrinsic(Intrinsic::NatType),
                items: vec![lit(7)]
            }),
        );

        // `slice(cons(7, xs), 1, 0) = []` — the empty-window identity, which a count decides on the length alone.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::list_slice(
                    Term::intrinsic(Intrinsic::NatType),
                    cons,
                    lit(1),
                    lit(0),
                    qed(),
                ))
            ),
            Subterm::Intrinsic(Intrinsic::List {
                element: Term::intrinsic(Intrinsic::NatType),
                items: Vec::new()
            }),
        );
    }

    // `List/len` distributes over the monoid like `Bin/len`: a symbolic cons or append reduces its length to a `succ` spine instead of stalling.
    #[test]
    fn list_len_distributes_over_cons_and_append() {
        let mut context = context();
        let xs_binder = context.fresh(Some("xs"));
        let xs = Term::free_var(&xs_binder);
        // `1 + len(xs)`, the shape both symbolic cases reduce to.
        let succ_len = |context: &mut Context| {
            reduced(
                context,
                Term::intrinsic(Intrinsic::nat_add(
                    lit(1),
                    Term::intrinsic(Intrinsic::list_len(
                        Term::intrinsic(Intrinsic::NatType),
                        xs.clone(),
                    )),
                )),
            )
        };

        // Literal: `len([1, 2, 3]) = 3`.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::list_len(
                    Term::intrinsic(Intrinsic::NatType),
                    Term::intrinsic(Intrinsic::List {
                        element: Term::intrinsic(Intrinsic::NatType),
                        items: vec![lit(1), lit(2), lit(3)]
                    })
                )),
            ),
            Subterm::Intrinsic(Intrinsic::Nat(Nat::new(3usize))),
        );

        // `len(cons(7, xs)) = 1 + len(xs)`.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::list_len(
                    Term::intrinsic(Intrinsic::NatType),
                    list_cons_seven(&xs)
                ))
            ),
            succ_len(&mut context),
        );

        // `len(append(xs, 9)) = 1 + len(xs)`.
        let appended = Term::intrinsic(Intrinsic::list_append(
            Term::intrinsic(Intrinsic::NatType),
            xs.clone(),
            lit(9),
        ));
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::list_len(
                    Term::intrinsic(Intrinsic::NatType),
                    appended
                ))
            ),
            succ_len(&mut context),
        );
    }

    // The full slice is the identity even over a symbolic array: `slice(xs, 0, len xs) = xs` (the `List` twin of `BinSlice`'s full-window identity).
    #[test]
    fn list_slice_full_window_is_identity() {
        let mut context = context();
        let xs_binder = context.fresh(Some("xs"));
        let xs = Term::free_var(&xs_binder);
        let len = Term::intrinsic(Intrinsic::list_len(
            Term::intrinsic(Intrinsic::NatType),
            xs.clone(),
        ));
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::list_slice(
                    Term::intrinsic(Intrinsic::NatType),
                    xs.clone(),
                    lit(0),
                    len,
                    qed(),
                )),
            ),
            reduced(&mut context, xs.clone()),
        );
    }

    // `Bin/eql` decides definitional equality through the spine peel: reflexivity and a peeled-equal pair fold to `true`, a definite byte/length clash to `false`, and a genuinely undecided pair stays neutral.
    #[test]
    fn bin_eql_decides_structurally() {
        let mut context = context();
        let x_binder = context.fresh(Some("x"));
        let y_binder = context.fresh(Some("y"));
        let bin = |bytes: Vec<u8>| {
            Term::intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(bytes)))
        };
        let x = Term::free_var(&x_binder);

        // Reflexivity over a symbolic value: `eql(x, x) = true`.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_eql(Grain::X, x.clone(), x.clone()))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(true)),
        );

        // Literal decisions: equal folds true, unequal folds false.
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_eql(
                    Grain::X,
                    bin(vec![1, 2]),
                    bin(vec![1, 2])
                ))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(true)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_eql(
                    Grain::X,
                    bin(vec![1, 2]),
                    bin(vec![1, 3])
                ))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(false)),
        );

        // A first-byte clash decides `false` even past a shared symbolic tail: `eql([1] ++ x, [2] ++ x) = false`.
        let lhs = Term::intrinsic(Intrinsic::bin_concat(Grain::X, [bin(vec![1]), x.clone()]));
        let rhs = Term::intrinsic(Intrinsic::bin_concat(Grain::X, [bin(vec![2]), x.clone()]));
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_eql(Grain::X, lhs, rhs))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(false)),
        );

        // Distinct variables are undecidable: `eql(x, y)` stays neutral.
        let y = Term::free_var(&y_binder);
        assert!(matches!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_eql(Grain::X, x, y))
            ),
            Subterm::Intrinsic(Intrinsic::BinEql(Grain::X, ..)),
        ));
    }

    #[test]
    fn bits_reduce_through_symbolic_free_monoid_spines() {
        let mut context = context();
        let tail_binder = context.fresh(Some("tail"));
        let bits = |values: &[bool]| {
            Term::intrinsic(Intrinsic::Bin(
                Grain::B,
                PackedBin::from_bits(values.iter().copied()),
            ))
        };
        let tail = Term::free_var(&tail_binder);
        let cons = Term::intrinsic(Intrinsic::bin_concat(
            Grain::B,
            [bits(&[true]), tail.clone()],
        ));

        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_get(Grain::B, cons.clone(), lit(0), qed()))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(true)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_slice(
                    Grain::B,
                    cons.clone(),
                    lit(0),
                    lit(1),
                    qed(),
                ))
            ),
            Term::unwrap_or_clone(bits(&[true])),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_len(Grain::B, cons.clone()))
            ),
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::nat_add(
                    lit(1),
                    Term::intrinsic(Intrinsic::bin_len(Grain::B, tail.clone())),
                ))
            ),
        );

        let false_cons = Term::intrinsic(Intrinsic::bin_concat(
            Grain::B,
            [bits(&[false]), tail.clone()],
        ));
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_eql(Grain::B, cons, false_cons))
            ),
            Subterm::Intrinsic(Intrinsic::Bool(false)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::intrinsic(Intrinsic::bin_concat(
                    Grain::B,
                    [bits(&[]), tail.clone(), bits(&[])],
                ))
            ),
            Term::unwrap_or_clone(tail),
        );
    }
}

/// The kernel in `curios-core` re-decides reduction from the term alone, with none of this crate's machinery — no cache, no refinements, no metavariables. These tests are the check that the two agree where they must.
///
/// Agreement is worth asserting precisely because the implementations are separate. If the kernel simply called this reducer the tests would be tautologies; because it does not, a divergence here is a real disagreement about what a term computes to, and one of the two is wrong.
///
/// The known *deliberate* divergences are internal to reduction and invisible in the result: a `let` is an environment step here and a substitution there, and a match arm binds a projection of the scrutinee here and the payload itself there. Both routes land on the same weak-head normal form, which is exactly what these assertions pin.
mod kernel_agreement {
    use curios_core::*;
    use {
        super::{context, nat, nominal},
        curios_cert::Kernel,
    };

    /// A kernel minting above every binder these fixtures use.
    fn kernel() -> Kernel {
        let mut kernel = Kernel::new(100_000, crate::fixture::SYNTAX);
        kernel.set_local_floor(10_000);
        kernel
    }

    /// Reduce `term` both ways and require the same answer.
    fn agree(term: Term) {
        let mut context = context();
        let mut kernel = kernel();

        let elaborated = super::reduce_forced(&mut context, term.clone());
        let checked = kernel.reduce_forced(term.clone());

        assert_eq!(
            elaborated, checked,
            "the elaborator and the kernel disagree on {term}",
        );
    }

    #[test]
    fn beta_agrees() {
        let mut context = context();
        let x = context.fresh(Some("x"));

        agree(Term::apply(
            Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x)),
            [nat(9)],
        ));
    }

    #[test]
    fn intrinsic_folds_agree() {
        agree(Term::intrinsic(Intrinsic::nat_add(nat(20), nat(22))));
        agree(Term::intrinsic(Intrinsic::nat_mul(nat(6), nat(7))));
        agree(Term::intrinsic(Intrinsic::nat_lt(nat(2), nat(3))));
    }

    #[test]
    fn a_stuck_intrinsic_agrees() {
        let mut context = context();
        let n = context.fresh(Some("n"));

        agree(Term::intrinsic(Intrinsic::nat_add(
            Term::free_var(&n),
            nat(0),
        )));
        agree(Term::intrinsic(Intrinsic::nat_add(
            nat(1),
            Term::free_var(&n),
        )));
    }

    #[test]
    fn zeta_agrees_despite_different_mechanisms() {
        let mut context = context();
        let x = context.fresh(Some("x"));
        let y = context.fresh(Some("y"));

        agree(Term::let_(
            &x,
            Term::intrinsic(Intrinsic::NatType),
            nat(4),
            Term::let_(
                &y,
                Term::intrinsic(Intrinsic::NatType),
                Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(5))),
                Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&x), Term::free_var(&y))),
            ),
        ));
    }

    #[test]
    fn iota_agrees_despite_different_arm_binding() {
        let mut context = context();
        let m = context.fresh(Some("m"));
        let payload = context.fresh(Some("a"));

        agree(Term::induct_match(
            Term::variant(nominal("E"), Vec::<Term>::new(), "some", [nat(42)]),
            Some(&m),
            Term::intrinsic(Intrinsic::NatType),
            [
                ("none", Vec::<Free>::new(), nat(0)),
                (
                    "some",
                    vec![payload.clone()],
                    Term::intrinsic(Intrinsic::nat_add(Term::free_var(&payload), nat(1))),
                ),
            ],
        ));
    }

    #[test]
    fn structural_nat_induction_agrees() {
        let mut context = context();
        let m = context.fresh(Some("m"));
        let pred = context.fresh(Some("pred"));
        let ih = context.fresh(Some("ih"));

        // The cons arm sums the hypothesis, so the whole spine is walked rather than one layer peeled.
        agree(Term::nat_match(
            nat(5),
            Some(&m),
            Term::intrinsic(Intrinsic::NatType),
            nat(0),
            &pred,
            &ih,
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&ih), nat(2))),
        ));
    }

    #[test]
    fn a_stuck_match_agrees() {
        let mut context = context();
        let m = context.fresh(Some("m"));
        let n = context.fresh(Some("n"));
        let pred = context.fresh(Some("pred"));
        let ih = context.fresh(Some("ih"));

        agree(Term::nat_match(
            Term::free_var(&n),
            Some(&m),
            Term::intrinsic(Intrinsic::NatType),
            nat(0),
            &pred,
            &ih,
            Term::free_var(&pred),
        ));
    }

    #[test]
    fn projection_agrees() {
        agree(Term::proj(Term::tuple([nat(10), nat(20), nat(30)]), 2));
        agree(Term::proj(
            Term::variant(nominal("E"), Vec::<Term>::new(), "some", [nat(42)]),
            1,
        ));
    }

    #[test]
    fn recursion_agrees_to_a_literal_and_stays_folded_otherwise() {
        let mut context = context();
        let n = context.fresh(Some("n"));
        let m = context.fresh(Some("m"));
        let pred = context.fresh(Some("pred"));
        let ih = context.fresh(Some("ih"));
        let countdown = context.fresh(Some("countdown"));
        let x = context.fresh(Some("x"));
        let nat_type = Term::intrinsic(Intrinsic::NatType);

        let body = Term::func(
            [(n.clone(), nat_type.clone())],
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

        let group = [(
            countdown.clone(),
            Term::func_type([(n.clone(), nat_type.clone())], nat_type),
            body,
        )];

        agree(Term::rec(
            group.clone(),
            Term::apply(Term::free_var(&countdown), [nat(4)]),
        ));
        agree(Term::rec(
            group,
            Term::apply(Term::free_var(&countdown), [Term::free_var(&x)]),
        ));
    }
}
