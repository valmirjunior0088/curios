use {
    crate::*,
    curios_base::{Flt, Grain, Int, PackedBin},
    std::time::{Duration, Instant},
};

fn context() -> Context {
    Context::new(Duration::from_millis(10))
}

fn nat(n: usize) -> Term {
    Term::prim(Prim::Nat(Nat::new(n)))
}

#[test]
fn nat_to_byte_reflects_byte_to_nat() {
    let mut context = context();
    let byte = Term::free_var("byte");
    let term = Term::prim(Prim::NatToByte(Term::prim(Prim::ByteToNat(byte.clone()))));

    assert_eq!(reduce(&mut context, term), Ok(byte));
}

#[test]
fn reduce_apply_beta_reduces() {
    let mut context = context();

    let term: Term = Term::apply(
        Term::func([("x", Term::type_())], Term::free_var("x")),
        [nat(1)],
    );

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(1)));
}

#[test]
fn recursive_application_stays_folded_until_its_result_is_demanded() {
    let mut context = context();
    let nat_type = Term::prim(Prim::NatType);
    let body = Term::func(
        [("n", nat_type.clone())],
        Term::nat_match(
            Term::free_var("n"),
            Some("m"),
            nat_type.clone(),
            nat(0),
            "pred",
            "ih",
            Term::apply(Term::free_var("countdown"), [Term::free_var("pred")]),
        ),
    );

    let neutral = Term::rec(
        [(
            "countdown",
            Term::func_type([("n", nat_type.clone())], nat_type.clone()),
            body.clone(),
        )],
        Term::apply(Term::free_var("countdown"), [Term::free_var("x")]),
    );
    let Subterm::Rec(rec) = Term::unwrap_or_clone(neutral) else {
        unreachable!()
    };
    let opened = unfold_rec(&mut context, rec);
    let reduced = reduce(&mut context, opened).expect("ordinary reduction should terminate");
    assert!(matches!(
        &*reduced,
        Subterm::Apply(Apply { head, .. }) if matches!(&**head, Subterm::RecMember(_))
    ));

    let concrete = Term::rec(
        [(
            "countdown",
            Term::func_type([("n", nat_type.clone())], nat_type),
            body,
        )],
        Term::apply(Term::free_var("countdown"), [nat(2)]),
    );
    assert_eq!(reduce_forced(&mut context, concrete), Ok(nat(0)));
}

#[test]
fn reduce_inductive_match_selects_case_and_projects_payload() {
    let mut context = context();

    // Dispatch inspects the reduced head's `Variant`; the arm's binder is
    // bound call-by-name to the flat projection `head.1`, which then reduces
    // to the payload component.
    let term: Term = Term::inductive_match(
        Term::variant("E", Vec::<Term>::new(), "some", [nat(42)]),
        Some("m"),
        Term::prim(Prim::NatType),
        [
            ("none", Vec::<&str>::new(), nat(0)),
            ("some", vec!["x"], Term::free_var("x")),
        ],
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(42)));
}

#[test]
fn reduce_inductive_match_absent_tag_takes_default() {
    let mut context = context();

    // The scrutinee is `some(42)`, but only `none` has an explicit arm; the
    // `some` tag is absent from the cases, so dispatch falls through to the
    // binding-free `| _ =>` default (no payload projected).
    let term: Term = Term::inductive_match_default(
        Term::variant("E", Vec::<Term>::new(), "some", [nat(42)]),
        Some("m"),
        Term::prim(Prim::NatType),
        [("none", Vec::<&str>::new(), nat(0))],
        nat(99),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(99)));
}

#[test]
fn reduce_inductive_match_present_tag_ignores_default() {
    let mut context = context();

    // With the `some` arm present, dispatch selects it (binding the payload)
    // rather than the default — the default is only for absent tags.
    let term: Term = Term::inductive_match_default(
        Term::variant("E", Vec::<Term>::new(), "some", [nat(42)]),
        Some("m"),
        Term::prim(Prim::NatType),
        [
            ("none", Vec::<&str>::new(), nat(0)),
            ("some", vec!["x"], Term::free_var("x")),
        ],
        nat(99),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(42)));
}

#[test]
fn reduce_nat_fold_zero_is_not_true() {
    let mut context = context();

    let term: Term = Term::nat_match(
        Subterm::Prim(Prim::Nat(Nat::new(0usize))),
        Some("m"),
        Term::prim(Prim::BlnType),
        Term::prim(Prim::Bln(false)),
        "pred",
        "ih",
        Term::prim(Prim::Bln(true)),
    );

    assert_ne!(
        reduce(&mut context, term.clone()),
        Ok(Term::prim(Prim::Bln(true)))
    );
}

#[test]
fn reduce_let_then_var_unfolds_definition() {
    let mut context = context();

    context.define("y", &nat(7));

    let term: Term = Term::let_("x", Term::type_(), Term::free_var("y"), Term::free_var("x"));

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(7)));
}

#[test]
fn reduce_let_binds_each_value_to_its_own_slot() {
    // Two distinct bindings referenced together in the tail: pins the positional
    // correctness of `reduce_let`'s environment open. The tail is `(λ p q. q) a b`,
    // so the result is `b`'s value — and only if `a`/`b` land in the right slots.
    // A transposed open would beta-reduce to `a`'s value instead.
    let mut context = context();

    let nat_type = Term::prim(Prim::NatType);
    let pick_second = Term::apply(
        Term::func(
            [("p", nat_type.clone()), ("q", nat_type.clone())],
            Term::free_var("q"),
        ),
        [Term::free_var("a"), Term::free_var("b")],
    );

    let term = Term::let_(
        "a",
        nat_type.clone(),
        nat(3),
        Term::let_("b", nat_type, nat(7), pick_second),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(7)));
}

#[test]
fn reduce_let_shadowing_tail_picks_innermost() {
    // `let x = 3; let x = 7; x` — two bindings share the name `x`. The flat
    // block is built by name-based `capture`, so the tail's `x` must bind to the
    // *innermost* binding (7), not the shadowed outer one (3).
    let mut context = context();

    let nat_type = Term::prim(Prim::NatType);
    let term = Term::let_(
        "x",
        nat_type.clone(),
        nat(3),
        Term::let_("x", nat_type, nat(7), Term::free_var("x")),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(7)));
}

#[test]
fn reduce_let_shadowing_value_sees_the_outer_binding() {
    // `let x = 5; let x = x; x` — the middle binding's value is the *outer* `x`,
    // since a `let` is non-recursive. Merging must leave that reference free so
    // the enclosing binder captures it to the first binding, not to itself: a
    // self-capture would define `x := x` and diverge instead of yielding 5.
    let mut context = context();

    let nat_type = Term::prim(Prim::NatType);
    let term = Term::let_(
        "x",
        nat_type.clone(),
        nat(5),
        Term::let_("x", nat_type, Term::free_var("x"), Term::free_var("x")),
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(5)));
}

#[test]
fn deep_let_chain_is_one_flat_block_reducing_without_native_recursion() {
    // A long straight-line `let` sequence must lower to a single flat `Let`
    // block, not a nest: `Term::let_` merges each binding into the block already
    // built for its tail, so folding the chain bottom-up (as `into_core` and the
    // elaborator's rebuild both do) yields one node. That flatness is what bounds
    // every walk over it — `reduce` here, and `traverse` via `reach` — to a loop
    // instead of one native stack frame per binding.
    let depth = 1000;
    let base = Term::free_var(format!("x{}", depth - 1));

    // `let x0 = 0; let x1 = x0; …; let x{n-1} = x{n-2}; x{n-1}`.
    let t0 = Instant::now();
    let term = (0..depth).rev().fold(base, |tail, i| {
        let value = if i == 0 {
            nat(0)
        } else {
            Term::free_var(format!("x{}", i - 1))
        };

        Term::let_(format!("x{i}"), Term::prim(Prim::NatType), value, tail)
    });
    eprintln!("build: {:?}", t0.elapsed());

    let mut context = Context::new(Duration::from_secs(30));

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

    // Every reference is internal (no free variables escape), and both `reach`
    // and `reduce` compute over the whole depth without recursing per binding.
    assert_eq!(term.reach(), 0);
    let t1 = Instant::now();
    assert_eq!(reduce(&mut context, term), Ok(nat(0)));
    eprintln!("reduce: {:?}", t1.elapsed());
}

#[test]
fn reduce_var_cycle_times_out() {
    let mut context = context();

    context.define("loop", &Term::free_var("loop"));

    assert_eq!(
        reduce(&mut context, Term::free_var("loop")),
        Err(ReduceError::Preempted)
    );
}

#[test]
fn reduce_int_add_computes() {
    let mut context = context();

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::int_add(
                Subterm::Prim(Prim::Int(Int::new(1))),
                Subterm::Prim(Prim::Int(Int::new(2)))
            ))
            .into()
        ),
        Ok(Subterm::Prim(Prim::Int(Int::new(3))).into())
    );
}

#[test]
fn reduce_int_eql_returns_true_or_false_bln() {
    let mut context = context();

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::int_eql(
                Subterm::Prim(Prim::Int(Int::new(4))),
                Subterm::Prim(Prim::Int(Int::new(4)))
            ))
            .into()
        ),
        Ok(Subterm::Prim(Prim::Bln(true)).into())
    );
    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::int_eql(
                Subterm::Prim(Prim::Int(Int::new(4))),
                Subterm::Prim(Prim::Int(Int::new(5)))
            ))
            .into()
        ),
        Ok(Subterm::Prim(Prim::Bln(false)).into())
    );
}

#[test]
fn reduce_flt_mul_computes() {
    let mut context = context();

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::flt_mul(
                Subterm::Prim(Prim::Flt(Flt::from_f32(1.5))),
                Subterm::Prim(Prim::Flt(Flt::from_f32(2.0)))
            ))
            .into()
        ),
        Ok(Subterm::Prim(Prim::Flt(Flt::from_f32(3.0))).into())
    );
}

#[test]
fn reduce_lst_get_returns_element_at_index() {
    let mut context = context();

    let list = Subterm::Prim(Prim::lst(vec![
        Subterm::Prim(Prim::Nat(Nat::new(10usize))),
        Subterm::Prim(Prim::Nat(Nat::new(20usize))),
        Subterm::Prim(Prim::Nat(Nat::new(30usize))),
    ]));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::lst_get(
                Subterm::Prim(Prim::NatType),
                list.clone(),
                Subterm::Prim(Prim::Nat(Nat::new(0usize)))
            ))
            .into()
        ),
        Ok(Subterm::Prim(Prim::Nat(Nat::new(10usize))).into())
    );
    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::lst_get(
                Subterm::Prim(Prim::NatType),
                list,
                Subterm::Prim(Prim::Nat(Nat::new(2usize)))
            ))
            .into()
        ),
        Ok(Subterm::Prim(Prim::Nat(Nat::new(30usize))).into())
    );
}

#[test]
fn reduce_lst_get_errors_on_out_of_bounds() {
    let mut context = context();

    let list = Subterm::Prim(Prim::lst(vec![Subterm::Prim(Prim::Nat(Nat::new(1usize)))]));

    assert!(matches!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::lst_get(
                Subterm::Prim(Prim::NatType),
                list,
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
            ))
            .into(),
        ),
        Err(ReduceError::LstGetOutOfBounds {
            len: 1,
            index: 1,
            ..
        })
    ));
}

#[test]
fn reduce_bin_append_adds_byte() {
    let mut context = context();

    let bin = Subterm::Prim(Prim::Bin(
        curios_base::Grain::X,
        PackedBin::from_bytes(vec![1, 2]),
    ));
    let byte: Subterm = Subterm::Prim(Prim::Byte(3));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::bin_append(Grain::X, bin, byte)).into()
        ),
        Ok(Subterm::Prim(Prim::Bin(
            curios_base::Grain::X,
            PackedBin::from_bytes(vec![1, 2, 3])
        ))
        .into())
    );
}

#[test]
fn reduce_bin_append_adds_the_full_byte_range() {
    let mut context = context();

    let bin = Subterm::Prim(Prim::Bin(
        curios_base::Grain::X,
        PackedBin::from_bytes(vec![1, 2]),
    ));
    let byte: Subterm = Subterm::Prim(Prim::Byte(255));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::bin_append(Grain::X, bin, byte)).into()
        ),
        Ok(Subterm::Prim(Prim::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2, 255]))).into())
    );
}

#[test]
fn reduce_lst_append_adds_element() {
    let mut context = context();

    let list = Subterm::Prim(Prim::lst(vec![
        Subterm::Prim(Prim::Nat(Nat::new(10usize))),
        Subterm::Prim(Prim::Nat(Nat::new(20usize))),
    ]));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::lst_append(
                Subterm::Prim(Prim::NatType),
                list,
                Subterm::Prim(Prim::Nat(Nat::new(30usize)))
            ))
            .into()
        ),
        Ok(Subterm::Prim(Prim::lst(vec![
            Subterm::Prim(Prim::Nat(Nat::new(10usize))),
            Subterm::Prim(Prim::Nat(Nat::new(20usize))),
            Subterm::Prim(Prim::Nat(Nat::new(30usize))),
        ]))
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

    context.refine_projection(Term::free_var("r"), 0, nat(1));

    let term: Term = Term::proj(Term::free_var("r"), 0);

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(1)));
}

#[test]
fn reduce_does_not_eta_reduce_tuple() {
    let mut context = context();

    // Tuple η is type-directed and lives in `convert`, not `reduce`:
    // `reduce` cannot verify `r`'s arity without type info, so collapsing
    // `(r.0, r.1)` to `r` would widen the tuple whenever `r` has more
    // fields than the tuple does.
    let term: Term = Term::tuple([
        Term::proj(Term::free_var("r"), 0),
        Term::proj(Term::free_var("r"), 1),
    ]);

    assert_eq!(reduce(&mut context, term.clone()), Ok(term));
}

#[test]
fn eta_reduce_func_fires() {
    let mut context = context();

    let term: Term = Term::func(
        [("y", Term::type_())],
        Term::apply(Term::free_var("f"), [Term::free_var("y")]),
    );

    assert_eq!(reduce(&mut context, term.clone()), Ok(Term::free_var("f")));
}

#[test]
fn define_invalidates_cached_reduction() {
    let mut context = context();
    let x: Term = Term::free_var("x");

    // No definition yet: x reduces to itself and the result is cached.
    assert_eq!(reduce(&mut context, x.clone()), Ok(x.clone()));

    // Defining x must clear the cache so the next reduce unfolds.
    context.define("x", &nat(3));
    assert_eq!(reduce(&mut context, x), Ok(nat(3)));
}

#[test]
fn refine_projection_invalidates_cached_reduction() {
    let mut context = context();
    let proj: Term = Term::proj(Term::free_var("r"), 0);

    // No projection refinement yet: proj reduces to itself and is cached.
    assert_eq!(reduce(&mut context, proj.clone()), Ok(proj.clone()));

    // Refining the projection must clear the cache.
    context.refine_projection(Term::free_var("r"), 0, nat(1));
    assert_eq!(reduce(&mut context, proj), Ok(nat(1)));
}

#[test]
fn leave_frame_with_definitions_invalidates_cached_reduction() {
    let mut context = context();
    let x: Term = Term::free_var("x");

    // Inside a frame, define x and reduce — the cache will hold x → "inner".
    context.with_frame(|context| {
        context.define("x", &nat(4));
        assert_eq!(reduce(context, x.clone()), Ok(nat(4)));
    });

    // After the frame pops, x has no definition again. A stale cache entry
    // would still return "inner"; the cache clear on leave_frame prevents that.
    assert_eq!(reduce(&mut context, x.clone()), Ok(x));
}

#[test]
fn reduce_unsolved_metavar_is_neutral() {
    let mut context = context();
    let m = Term::metavar(0);

    // No store entry, or an unsolved one, both reduce to the metavariable itself.
    assert_eq!(reduce(&mut context, m.clone()), Ok(m.clone()));

    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_());
    assert_eq!(reduce(&mut context, m.clone()), Ok(m));
}

#[test]
fn reduce_solved_metavar_yields_solution() {
    let mut context = context();
    let m = Term::metavar(0);

    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_());

    // An unsolved metavariable reduces to itself, but that reduct names an
    // unsolved metavariable, so it is deliberately not memoized.
    assert_eq!(reduce(&mut context, m.clone()), Ok(m.clone()));

    let solution = nat(1);
    context.solve_metavar(MetavarId(0), solution.clone());

    // Nothing stale was cached, so the reduct now follows the solution —
    // `solve_metavar` needs no cache clear.
    assert_eq!(reduce(&mut context, m), Ok(solution));
}

#[test]
fn refinement_is_suppressible() {
    let mut context = context();
    let b = Term::free_var("b");
    let truth = Term::prim(Prim::Bln(true));

    context.refine("b", &truth);

    // With the refinement active, `b` reduces to its counterfactual value.
    assert_eq!(reduce(&mut context, b.clone()), Ok(truth));

    // Suppressed (as in re-validation), `b` is abstract again.
    let reduced = context.with_suppressed_refinements(|context| reduce(context, b.clone()));
    assert_eq!(reduced, Ok(b));
}

// === Type-level partial arithmetic ===========================================
//
// A literal zero divisor is mathematically undefined and reports through a
// `ReduceError` (the `BinGet` pattern, span and all) — never a Rust panic.
// Runtime *range* limits, by contrast, never error here: `Nat`/`Int` are
// unbounded at the type level, folds compute exactly, and the 31-bit
// narrowing is enforced downstream (`ersd`'s carriers at the erase boundary,
// the i31 traps in `cont` → wasm).

#[test]
fn reduce_nat_div_by_zero_reports() {
    let mut context = context();
    assert_eq!(
        reduce(
            &mut context,
            Term::prim(Prim::nat_div(
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
                Subterm::Prim(Prim::Nat(Nat::new(0usize))),
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
            Term::prim(Prim::nat_div(
                Term::free_var("x"),
                Term::prim(Prim::Nat(Nat::new(0usize))),
            )),
        ),
        Err(ReduceError::DivisionByZero {
            kind: "Nat/div",
            span: None,
        })
    );

    // A symbolic divisor is not a zero divisor: the term just stays stuck.
    let stuck = Term::prim(Prim::nat_div(
        Subterm::Prim(Prim::Nat(Nat::new(1usize))),
        Subterm::Var(Var::free("y")),
    ));
    assert_eq!(reduce(&mut context, stuck.clone()), Ok(stuck));
}

#[test]
fn reduce_nat_rem_by_zero_reports() {
    let mut context = context();
    assert_eq!(
        reduce(
            &mut context,
            Term::prim(Prim::nat_rem(
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
                Subterm::Prim(Prim::Nat(Nat::new(0usize))),
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
            Term::prim(Prim::int_div(
                Subterm::Prim(Prim::Int(Int::new(1))),
                Subterm::Prim(Prim::Int(Int::new(0))),
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
            Term::prim(Prim::int_rem(
                Subterm::Prim(Prim::Int(Int::new(1))),
                Subterm::Prim(Prim::Int(Int::new(0))),
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

    // Past the runtime's i31 range the type level keeps computing exactly —
    // the limit is the runtime's, enforced downstream, not the checker's.
    assert_eq!(
        reduce(
            &mut context,
            Term::prim(Prim::int_add(
                Subterm::Prim(Prim::Int(Int::new((1i64 << 30) - 1))),
                Subterm::Prim(Prim::Int(Int::new(1))),
            )),
        ),
        Ok(Term::prim(Prim::Int(Int::new(1i64 << 30))))
    );

    assert_eq!(
        reduce(
            &mut context,
            Term::prim(Prim::int_mul(
                Subterm::Prim(Prim::Int(Int::new(1i64 << 30))),
                Subterm::Prim(Prim::Int(Int::new(1i64 << 30))),
            )),
        ),
        Ok(Term::prim(Prim::Int(Int::new(1i64 << 60))))
    );
}

#[test]
fn reduce_flt_to_int_is_exact_or_stuck() {
    let mut context = context();

    // 2^31 is exactly representable in f32 and folds exactly, well past i31.
    assert_eq!(
        reduce(
            &mut context,
            Term::prim(Prim::FltToInt(Term::prim(Prim::Flt(Flt::from_f32(
                2147483648.0
            ))))),
        ),
        Ok(Term::prim(Prim::Int(Int::new(1i64 << 31))))
    );

    // NaN has no integer part — no value to pretend, so the fold stays stuck
    // (the runtime's trunc would trap).
    let nan = Term::prim(Prim::FltToInt(Term::prim(Prim::Flt(Flt::from_f32(
        f32::NAN,
    )))));
    assert_eq!(reduce(&mut context, nan.clone()), Ok(nan));
}

mod prim {
    use {
        crate::{Context, Nat, Prim, Subterm, Term, reduce},
        curios_base::{Grain, PackedBin},
        num_bigint::BigUint,
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_millis(50))
    }

    fn lit(n: u32) -> Term {
        Term::prim(Prim::Nat(Nat::new(n as usize)))
    }

    fn succ(inner: Term) -> Term {
        Term::prim(Prim::Nat(Nat::Succ(BigUint::from(1u32), inner)))
    }

    fn x() -> Term {
        Term::free_var("x")
    }

    fn reduced(context: &mut Context, term: Term) -> Subterm {
        Term::unwrap_or_clone(reduce(context, term).expect("reduces"))
    }

    // Symbolic successor bounds the family must decide — exactly the cases the old
    // bespoke `lt` rule handled, now shared by the whole family (a regression guard).
    #[test]
    fn comparisons_decide_symbolic_successor_bounds() {
        let mut context = context();

        // `succ x ≥ 1`: lt is false, gte is true; and `0 < succ x` is true.
        assert_eq!(
            reduced(&mut context, Term::prim(Prim::nat_lt(succ(x()), lit(1)))),
            Subterm::Prim(Prim::Bln(false)),
        );
        assert_eq!(
            reduced(&mut context, Term::prim(Prim::nat_gte(succ(x()), lit(1)))),
            Subterm::Prim(Prim::Bln(true)),
        );
        assert_eq!(
            reduced(&mut context, Term::prim(Prim::nat_lt(lit(0), succ(x())))),
            Subterm::Prim(Prim::Bln(true)),
        );

        // Shared inner: `lt(x, succ x) = true`, `gte(x, succ x) = false`.
        assert_eq!(
            reduced(&mut context, Term::prim(Prim::nat_lt(x(), succ(x())))),
            Subterm::Prim(Prim::Bln(true)),
        );
        assert_eq!(
            reduced(&mut context, Term::prim(Prim::nat_gte(x(), succ(x())))),
            Subterm::Prim(Prim::Bln(false)),
        );

        // The Str decoder blocker: `eql(succ(succ x), 1) = false` (shapes differ
        // once the shared floor is peeled).
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::nat_eql(succ(succ(x())), lit(1)))
            ),
            Subterm::Prim(Prim::Bln(false)),
        );

        // A non-strict bound decides `lte` but leaves `lt` genuinely undecidable
        // (neutral), since `2 ≤ succ(succ x)` says nothing about strictness.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::nat_lte(lit(2), succ(succ(x()))))
            ),
            Subterm::Prim(Prim::Bln(true)),
        );
        assert!(matches!(
            reduced(
                &mut context,
                Term::prim(Prim::nat_lt(lit(2), succ(succ(x()))))
            ),
            Subterm::Prim(Prim::NatLt(..)),
        ));
    }

    // Soundness gate for the distributing `Nat/mul`: on closed inputs it must still
    // agree with the host product — the literal fold the floor distribution
    // subsumes (`il = ir = 0`, so only the floors `sl · sr` remain).
    #[test]
    fn mul_agrees_with_literal_product() {
        let mut context = context();
        let samples = [0u32, 1, 2, 7, 13, 100];
        for &a in &samples {
            for &b in &samples {
                assert_eq!(
                    reduced(&mut context, Term::prim(Prim::nat_mul(lit(a), lit(b)))),
                    Subterm::Prim(Prim::Nat(Nat::new((a * b) as usize))),
                    "mul disagreed with the literal product on ({a}, {b})",
                );
            }
        }
    }

    // `Nat/mul` distributes a literal factor over a symbolic successor floor, the
    // multiplicative twin of `NatAdd`'s floor law: `(x + 1) · c` and `x · c + c`
    // reduce to the same normal form (either side may be the literal). Two symbolic
    // operands have no literal factor, so the product stays neutral.
    #[test]
    fn mul_distributes_literal_over_symbolic_floor() {
        let mut context = context();

        // `(x + 1) · 2 = x · 2 + 2`.
        assert_eq!(
            reduced(&mut context, Term::prim(Prim::nat_mul(succ(x()), lit(2)))),
            reduced(
                &mut context,
                Term::prim(Prim::nat_add(
                    Term::prim(Prim::nat_mul(x(), lit(2))),
                    lit(2)
                )),
            ),
        );

        // Commutative: `2 · (x + 1) = 2 · x + 2`.
        assert_eq!(
            reduced(&mut context, Term::prim(Prim::nat_mul(lit(2), succ(x())))),
            reduced(
                &mut context,
                Term::prim(Prim::nat_add(
                    Term::prim(Prim::nat_mul(lit(2), x())),
                    lit(2)
                )),
            ),
        );

        // No literal factor ⇒ neutral.
        assert!(matches!(
            reduced(&mut context, Term::prim(Prim::nat_mul(x(), x()))),
            Subterm::Prim(Prim::NatMul(..)),
        ));
    }

    // `cons(7, xs) = [7] ++ xs` over a symbolic tail `xs` — the symbolic cons
    // `Lst/get` and `Lst/slice` previously could not peel (they folded only literal
    // arrays), now decoded one element at a time like their `Bin` twins.
    fn lst_cons_seven(xs: &Term) -> Term {
        Term::prim(Prim::lst_concat(
            Term::prim(Prim::NatType),
            [Term::prim(Prim::Lst(vec![lit(7)])), xs.clone()],
        ))
    }

    #[test]
    fn lst_get_peels_symbolic_cons() {
        let mut context = context();
        let cons = lst_cons_seven(&Term::free_var("xs"));

        // `get(cons(7, xs), 0) = 7`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::lst_get(
                    Term::prim(Prim::NatType),
                    cons.clone(),
                    lit(0)
                ))
            ),
            Subterm::Prim(Prim::Nat(Nat::new(7usize))),
        );

        // `get(cons(7, xs), 1)` peels to `get(xs, 0)` — neutral over a symbolic tail.
        assert!(matches!(
            reduced(
                &mut context,
                Term::prim(Prim::lst_get(Term::prim(Prim::NatType), cons, lit(1)))
            ),
            Subterm::Prim(Prim::LstGet(..)),
        ));
    }

    #[test]
    fn lst_slice_peels_symbolic_cons() {
        let mut context = context();
        let cons = lst_cons_seven(&Term::free_var("xs"));

        // `slice(cons(7, xs), 0, 1) = [7] ++ slice(xs, 0, 0) = [7]`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::lst_slice(
                    Term::prim(Prim::NatType),
                    cons.clone(),
                    lit(0),
                    lit(1)
                ))
            ),
            Subterm::Prim(Prim::Lst(vec![lit(7)])),
        );

        // `slice(cons(7, xs), 1, 1) = []` — the empty-slice identity.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::lst_slice(
                    Term::prim(Prim::NatType),
                    cons,
                    lit(1),
                    lit(1)
                ))
            ),
            Subterm::Prim(Prim::Lst(Vec::new())),
        );
    }

    // `Lst/len` distributes over the monoid like `Bin/len`: a symbolic cons or
    // append reduces its length to a `succ` spine instead of stalling.
    #[test]
    fn lst_len_distributes_over_cons_and_append() {
        let mut context = context();
        let xs = Term::free_var("xs");
        // `1 + len(xs)`, the shape both symbolic cases reduce to.
        let succ_len = |context: &mut Context| {
            reduced(
                context,
                Term::prim(Prim::nat_add(
                    lit(1),
                    Term::prim(Prim::lst_len(Term::prim(Prim::NatType), xs.clone())),
                )),
            )
        };

        // Literal: `len([1, 2, 3]) = 3`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::lst_len(
                    Term::prim(Prim::NatType),
                    Term::prim(Prim::Lst(vec![lit(1), lit(2), lit(3)]))
                )),
            ),
            Subterm::Prim(Prim::Nat(Nat::new(3usize))),
        );

        // `len(cons(7, xs)) = 1 + len(xs)`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::lst_len(
                    Term::prim(Prim::NatType),
                    lst_cons_seven(&xs)
                ))
            ),
            succ_len(&mut context),
        );

        // `len(append(xs, 9)) = 1 + len(xs)`.
        let appended = Term::prim(Prim::lst_append(
            Term::prim(Prim::NatType),
            xs.clone(),
            lit(9),
        ));
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::lst_len(Term::prim(Prim::NatType), appended))
            ),
            succ_len(&mut context),
        );
    }

    // The full slice is the identity even over a symbolic array: `slice(xs, 0, len
    // xs) = xs` (the `Lst` twin of `BinSlice`'s full-window identity).
    #[test]
    fn lst_slice_full_window_is_identity() {
        let mut context = context();
        let xs = Term::free_var("xs");
        let len = Term::prim(Prim::lst_len(Term::prim(Prim::NatType), xs.clone()));
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::lst_slice(
                    Term::prim(Prim::NatType),
                    xs.clone(),
                    lit(0),
                    len
                )),
            ),
            reduced(&mut context, xs.clone()),
        );
    }

    // `Bin/eql` decides definitional equality through the spine peel: reflexivity and
    // a peeled-equal pair fold to `true`, a definite byte/length clash to `false`,
    // and a genuinely undecided pair stays neutral.
    #[test]
    fn bin_eql_decides_structurally() {
        let mut context = context();
        let bin = |bytes: Vec<u8>| Term::prim(Prim::Bin(Grain::X, PackedBin::from_bytes(bytes)));
        let x = Term::free_var("x");

        // Reflexivity over a symbolic value: `eql(x, x) = true`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_eql(Grain::X, x.clone(), x.clone()))
            ),
            Subterm::Prim(Prim::Bln(true)),
        );

        // Literal decisions: equal folds true, unequal folds false.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_eql(Grain::X, bin(vec![1, 2]), bin(vec![1, 2])))
            ),
            Subterm::Prim(Prim::Bln(true)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_eql(Grain::X, bin(vec![1, 2]), bin(vec![1, 3])))
            ),
            Subterm::Prim(Prim::Bln(false)),
        );

        // A first-byte clash decides `false` even past a shared symbolic tail:
        // `eql([1] ++ x, [2] ++ x) = false`.
        let lhs = Term::prim(Prim::bin_concat(Grain::X, [bin(vec![1]), x.clone()]));
        let rhs = Term::prim(Prim::bin_concat(Grain::X, [bin(vec![2]), x.clone()]));
        assert_eq!(
            reduced(&mut context, Term::prim(Prim::bin_eql(Grain::X, lhs, rhs))),
            Subterm::Prim(Prim::Bln(false)),
        );

        // Distinct variables are undecidable: `eql(x, y)` stays neutral.
        let y = Term::free_var("y");
        assert!(matches!(
            reduced(&mut context, Term::prim(Prim::bin_eql(Grain::X, x, y))),
            Subterm::Prim(Prim::BinEql(Grain::X, ..)),
        ));
    }

    #[test]
    fn bits_reduce_through_symbolic_free_monoid_spines() {
        let mut context = context();
        let bits = |values: &[bool]| {
            Term::prim(Prim::Bin(
                Grain::B,
                PackedBin::from_bits(values.iter().copied()),
            ))
        };
        let tail = Term::free_var("tail");
        let cons = Term::prim(Prim::bin_concat(Grain::B, [bits(&[true]), tail.clone()]));

        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_get(Grain::B, cons.clone(), lit(0)))
            ),
            Subterm::Prim(Prim::Bln(true)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_slice(Grain::B, cons.clone(), lit(0), lit(1)))
            ),
            Term::unwrap_or_clone(bits(&[true])),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_len(Grain::B, cons.clone()))
            ),
            reduced(
                &mut context,
                Term::prim(Prim::nat_add(
                    lit(1),
                    Term::prim(Prim::bin_len(Grain::B, tail.clone())),
                ))
            ),
        );

        let false_cons = Term::prim(Prim::bin_concat(Grain::B, [bits(&[false]), tail.clone()]));
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_eql(Grain::B, cons, false_cons))
            ),
            Subterm::Prim(Prim::Bln(false)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_concat(
                    Grain::B,
                    [bits(&[]), tail.clone(), bits(&[])],
                ))
            ),
            Term::unwrap_or_clone(tail),
        );
    }

    // A nested `Lst/slice` reassociates to one slice over the base, even when the
    // base is symbolic: `slice(slice(xs, 1, 5), 0, 2) = slice(xs, 1, 3)` (the `Lst`
    // twin of `BinSlice`'s window reassociation).
    #[test]
    fn lst_slice_reassociates_nested() {
        let mut context = context();
        let xs = Term::free_var("xs");
        let inner = Term::prim(Prim::lst_slice(
            Term::prim(Prim::NatType),
            xs.clone(),
            lit(1),
            lit(5),
        ));
        let outer = Term::prim(Prim::lst_slice(
            Term::prim(Prim::NatType),
            inner,
            lit(0),
            lit(2),
        ));
        assert_eq!(
            reduced(&mut context, outer),
            reduced(
                &mut context,
                Term::prim(Prim::lst_slice(
                    Term::prim(Prim::NatType),
                    xs.clone(),
                    lit(1),
                    lit(3)
                )),
            ),
        );
    }
}
