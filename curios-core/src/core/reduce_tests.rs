use {
    super::*,
    crate::core::{Flt, Int, MetavarId, Nat, Prim, Var},
    std::time::Duration,
};

fn context() -> Context {
    Context::new(Duration::from_millis(10))
}

fn nat(n: usize) -> Term {
    Term::prim(Prim::Nat(Nat::new(n)))
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

    let list = Subterm::Prim(Prim::arr(vec![
        Subterm::Prim(Prim::Nat(Nat::new(10usize))),
        Subterm::Prim(Prim::Nat(Nat::new(20usize))),
        Subterm::Prim(Prim::Nat(Nat::new(30usize))),
    ]));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::arr_get(
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
            Subterm::Prim(Prim::arr_get(
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

    let list = Subterm::Prim(Prim::arr(vec![Subterm::Prim(Prim::Nat(Nat::new(1usize)))]));

    assert!(matches!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::arr_get(
                Subterm::Prim(Prim::NatType),
                list,
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
            ))
            .into(),
        ),
        Err(ReduceError::ArrGetOutOfBounds {
            len: 1,
            index: 1,
            ..
        })
    ));
}

#[test]
fn reduce_bin_append_adds_byte() {
    let mut context = context();

    let bin = Subterm::Prim(Prim::Bin(vec![1, 2]));
    let byte: Subterm = Subterm::Prim(Prim::Nat(Nat::new(3usize)));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::bin_append(bin, byte)).into()
        ),
        Ok(Subterm::Prim(Prim::Bin(vec![1, 2, 3])).into())
    );
}

#[test]
fn reduce_bin_append_truncates_byte_to_low_eight_bits() {
    let mut context = context();

    let bin = Subterm::Prim(Prim::Bin(vec![1, 2]));
    let byte: Subterm = Subterm::Prim(Prim::Nat(Nat::new(259usize)));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::bin_append(bin, byte)).into()
        ),
        Ok(Subterm::Prim(Prim::Bin(vec![1, 2, 3])).into())
    );
}

#[test]
fn reduce_lst_append_adds_element() {
    let mut context = context();

    let list = Subterm::Prim(Prim::arr(vec![
        Subterm::Prim(Prim::Nat(Nat::new(10usize))),
        Subterm::Prim(Prim::Nat(Nat::new(20usize))),
    ]));

    assert_eq!(
        reduce(
            &mut context,
            Subterm::Prim(Prim::arr_append(
                Subterm::Prim(Prim::NatType),
                list,
                Subterm::Prim(Prim::Nat(Nat::new(30usize)))
            ))
            .into()
        ),
        Ok(Subterm::Prim(Prim::arr(vec![
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
        crate::core::{
            Context, Nat, Prim, Subterm, Term, reduce,
            reduce_prim::{compare_nat, from_ordering},
        },
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

    // Soundness gate: the structural body agrees with the host ordering on every
    // pair of literals — the decidable closed case where the two routes into a
    // `Comparison` (the shared-inner shortcut vs. the host `cmp`) must coincide.
    #[test]
    fn compare_nat_agrees_with_literal_ordering() {
        let mut context = context();
        let samples = [0u32, 1, 2, 5, 42, 128, 255, 256, 1000];
        for &m in &samples {
            for &n in &samples {
                assert_eq!(
                    compare_nat(&mut context, lit(m), lit(n))
                        .expect("reduces")
                        .0,
                    from_ordering(m.cmp(&n)),
                    "compare_nat disagreed with the literal ordering on ({m}, {n})",
                );
            }
        }
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
    // `Arr/get` and `Arr/slice` previously could not peel (they folded only literal
    // arrays), now decoded one element at a time like their `Bin` twins.
    fn arr_cons_seven(xs: &Term) -> Term {
        Term::prim(Prim::arr_concat(
            Term::prim(Prim::NatType),
            [Term::prim(Prim::Arr(vec![lit(7)])), xs.clone()],
        ))
    }

    #[test]
    fn arr_get_peels_symbolic_cons() {
        let mut context = context();
        let cons = arr_cons_seven(&Term::free_var("xs"));

        // `get(cons(7, xs), 0) = 7`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_get(
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
                Term::prim(Prim::arr_get(Term::prim(Prim::NatType), cons, lit(1)))
            ),
            Subterm::Prim(Prim::ArrGet(..)),
        ));
    }

    #[test]
    fn arr_slice_peels_symbolic_cons() {
        let mut context = context();
        let cons = arr_cons_seven(&Term::free_var("xs"));

        // `slice(cons(7, xs), 0, 1) = [7] ++ slice(xs, 0, 0) = [7]`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_slice(
                    Term::prim(Prim::NatType),
                    cons.clone(),
                    lit(0),
                    lit(1)
                ))
            ),
            Subterm::Prim(Prim::Arr(vec![lit(7)])),
        );

        // `slice(cons(7, xs), 1, 1) = []` — the empty-slice identity.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_slice(
                    Term::prim(Prim::NatType),
                    cons,
                    lit(1),
                    lit(1)
                ))
            ),
            Subterm::Prim(Prim::Arr(Vec::new())),
        );
    }

    // `Arr/len` distributes over the monoid like `Bin/len`: a symbolic cons or
    // append reduces its length to a `succ` spine instead of stalling.
    #[test]
    fn arr_len_distributes_over_cons_and_append() {
        let mut context = context();
        let xs = Term::free_var("xs");
        // `1 + len(xs)`, the shape both symbolic cases reduce to.
        let succ_len = |context: &mut Context| {
            reduced(
                context,
                Term::prim(Prim::nat_add(
                    lit(1),
                    Term::prim(Prim::arr_len(Term::prim(Prim::NatType), xs.clone())),
                )),
            )
        };

        // Literal: `len([1, 2, 3]) = 3`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_len(
                    Term::prim(Prim::NatType),
                    Term::prim(Prim::Arr(vec![lit(1), lit(2), lit(3)]))
                )),
            ),
            Subterm::Prim(Prim::Nat(Nat::new(3usize))),
        );

        // `len(cons(7, xs)) = 1 + len(xs)`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_len(
                    Term::prim(Prim::NatType),
                    arr_cons_seven(&xs)
                ))
            ),
            succ_len(&mut context),
        );

        // `len(append(xs, 9)) = 1 + len(xs)`.
        let appended = Term::prim(Prim::arr_append(
            Term::prim(Prim::NatType),
            xs.clone(),
            lit(9),
        ));
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_len(Term::prim(Prim::NatType), appended))
            ),
            succ_len(&mut context),
        );
    }

    // The full slice is the identity even over a symbolic array: `slice(xs, 0, len
    // xs) = xs` (the `Arr` twin of `BinSlice`'s full-window identity).
    #[test]
    fn arr_slice_full_window_is_identity() {
        let mut context = context();
        let xs = Term::free_var("xs");
        let len = Term::prim(Prim::arr_len(Term::prim(Prim::NatType), xs.clone()));
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_slice(
                    Term::prim(Prim::NatType),
                    xs.clone(),
                    lit(0),
                    len
                )),
            ),
            reduced(&mut context, xs.clone()),
        );
    }

    // `Arr/flatten` distributes over the monoid like `Bin/flatten`: literal parts
    // merge, and a part that is symbolic survives as an `ArrConcat` instead of
    // stalling the whole flatten (the old rule bailed to neutral on any non-literal).
    #[test]
    fn arr_flatten_distributes() {
        let mut context = context();
        let arr = |elems: Vec<Term>| Term::prim(Prim::Arr(elems));

        // Literal-of-literals: `flatten([[1, 2], [3]]) = [1, 2, 3]`.
        let outer = arr(vec![arr(vec![lit(1), lit(2)]), arr(vec![lit(3)])]);
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_flatten(Term::prim(Prim::NatType), outer))
            ),
            Subterm::Prim(Prim::Arr(vec![lit(1), lit(2), lit(3)])),
        );

        // A symbolic inner array `xs : Arr(Nat)` no longer stalls the flatten:
        // `flatten([[1, 2], xs])` reduces to `[1, 2] ++ xs` (an `ArrConcat`).
        let xs = Term::free_var("xs");
        let mixed = arr(vec![arr(vec![lit(1), lit(2)]), xs]);
        assert!(matches!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_flatten(Term::prim(Prim::NatType), mixed))
            ),
            Subterm::Prim(Prim::ArrConcat(..)),
        ));

        // `flatten` distributes over an outer append like `len`/`map`:
        // `flatten(append(xss, [9]))` over a symbolic `xss : Arr(Arr(Nat))` reduces
        // to `flatten(xss) ++ [9]` (an `ArrConcat`) instead of stalling.
        let xss = Term::free_var("xss");
        let appended = Term::prim(Prim::arr_append(
            Term::prim(Prim::ArrType(Term::prim(Prim::NatType))),
            xss,
            arr(vec![lit(9)]),
        ));
        assert!(matches!(
            reduced(
                &mut context,
                Term::prim(Prim::arr_flatten(Term::prim(Prim::NatType), appended))
            ),
            Subterm::Prim(Prim::ArrConcat(..)),
        ));
    }

    // `Bin/flatten` distributes over an outer append too: `flatten(append(xs, \09))`
    // over a symbolic `xs : Arr(Bin)` reduces to `flatten(xs) ++ \09` (a `BinConcat`)
    // rather than stalling — the `Bin` twin of the `Arr/flatten` append rule.
    #[test]
    fn bin_flatten_distributes_over_append() {
        let mut context = context();
        let xs = Term::free_var("xs");
        let appended = Term::prim(Prim::arr_append(
            Term::prim(Prim::BinType),
            xs,
            Term::prim(Prim::Bin(vec![9])),
        ));
        assert!(matches!(
            reduced(&mut context, Term::prim(Prim::bin_flatten(appended))),
            Subterm::Prim(Prim::BinConcat(..)),
        ));
    }

    // `Bin/eql` decides definitional equality through the spine peel: reflexivity and
    // a peeled-equal pair fold to `true`, a definite byte/length clash to `false`,
    // and a genuinely undecided pair stays neutral.
    #[test]
    fn bin_eql_decides_structurally() {
        let mut context = context();
        let bin = |bytes: Vec<u8>| Term::prim(Prim::Bin(bytes));
        let x = Term::free_var("x");

        // Reflexivity over a symbolic value: `eql(x, x) = true`.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_eql(x.clone(), x.clone()))
            ),
            Subterm::Prim(Prim::Bln(true)),
        );

        // Literal decisions: equal folds true, unequal folds false.
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_eql(bin(vec![1, 2]), bin(vec![1, 2])))
            ),
            Subterm::Prim(Prim::Bln(true)),
        );
        assert_eq!(
            reduced(
                &mut context,
                Term::prim(Prim::bin_eql(bin(vec![1, 2]), bin(vec![1, 3])))
            ),
            Subterm::Prim(Prim::Bln(false)),
        );

        // A first-byte clash decides `false` even past a shared symbolic tail:
        // `eql([1] ++ x, [2] ++ x) = false`.
        let lhs = Term::prim(Prim::bin_concat([bin(vec![1]), x.clone()]));
        let rhs = Term::prim(Prim::bin_concat([bin(vec![2]), x.clone()]));
        assert_eq!(
            reduced(&mut context, Term::prim(Prim::bin_eql(lhs, rhs))),
            Subterm::Prim(Prim::Bln(false)),
        );

        // Distinct variables are undecidable: `eql(x, y)` stays neutral.
        let y = Term::free_var("y");
        assert!(matches!(
            reduced(&mut context, Term::prim(Prim::bin_eql(x, y))),
            Subterm::Prim(Prim::BinEql(..)),
        ));
    }

    // A nested `Arr/slice` reassociates to one slice over the base, even when the
    // base is symbolic: `slice(slice(xs, 1, 5), 0, 2) = slice(xs, 1, 3)` (the `Arr`
    // twin of `BinSlice`'s window reassociation).
    #[test]
    fn arr_slice_reassociates_nested() {
        let mut context = context();
        let xs = Term::free_var("xs");
        let inner = Term::prim(Prim::arr_slice(
            Term::prim(Prim::NatType),
            xs.clone(),
            lit(1),
            lit(5),
        ));
        let outer = Term::prim(Prim::arr_slice(
            Term::prim(Prim::NatType),
            inner,
            lit(0),
            lit(2),
        ));
        assert_eq!(
            reduced(&mut context, outer),
            reduced(
                &mut context,
                Term::prim(Prim::arr_slice(
                    Term::prim(Prim::NatType),
                    xs.clone(),
                    lit(1),
                    lit(3)
                )),
            ),
        );
    }
}
