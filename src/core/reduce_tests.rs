use {
    super::*,
    crate::core::{Flt, Int, Nat, Prim, Var},
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
        Term::func([("x", Term::type_())], Term::var(Var::free("x"))),
        [nat(1)],
    );

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(1)));
}

#[test]
fn reduce_union_match_selects_case_and_projects_payload() {
    let mut context = context();

    // Dispatch inspects the reduced head's `Variant`; the arm's binder is
    // bound call-by-name to the flat projection `head.1`, which then reduces
    // to the payload component.
    let term: Term = Term::union_match(
        Term::variant("E", Vec::<Term>::new(), "some", [nat(42)]),
        Some("m"),
        Term::prim(Prim::NatType),
        [
            ("none", Vec::<&str>::new(), nat(0)),
            ("some", vec!["x"], Term::var(Var::free("x"))),
        ],
    );

    assert_eq!(reduce(&mut context, term), Ok(nat(42)));
}

#[test]
fn reduce_nat_fold_zero_is_not_true() {
    let mut context = context();

    let term: Term = Term::nat_induction(
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

    let term: Term = Term::let_(
        "x",
        Term::type_(),
        Term::var(Var::free("y")),
        Term::var(Var::free("x")),
    );

    assert_eq!(reduce(&mut context, term.clone()), Ok(nat(7)));
}

#[test]
fn reduce_var_cycle_times_out() {
    let mut context = context();

    context.define("loop", &Term::var(Var::free("loop")));

    assert_eq!(
        reduce(&mut context, Term::var(Var::free("loop"))),
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

    context.refine_projection(Term::var(Var::free("r")), 0, nat(1));

    let term: Term = Term::proj(Term::var(Var::free("r")), 0);

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
        Term::proj(Term::var(Var::free("r")), 0),
        Term::proj(Term::var(Var::free("r")), 1),
    ]);

    assert_eq!(reduce(&mut context, term.clone()), Ok(term));
}

#[test]
fn eta_reduce_func_fires() {
    let mut context = context();

    let term: Term = Term::func(
        [("y", Term::type_())],
        Term::apply(Term::var(Var::free("f")), [Term::var(Var::free("y"))]),
    );

    assert_eq!(
        reduce(&mut context, term.clone()),
        Ok(Term::var(Var::free("f")))
    );
}

#[test]
fn define_invalidates_cached_reduction() {
    let mut context = context();
    let x: Term = Term::var(Var::free("x"));

    // No definition yet: x reduces to itself and the result is cached.
    assert_eq!(reduce(&mut context, x.clone()), Ok(x.clone()));

    // Defining x must clear the cache so the next reduce unfolds.
    context.define("x", &nat(3));
    assert_eq!(reduce(&mut context, x), Ok(nat(3)));
}

#[test]
fn refine_projection_invalidates_cached_reduction() {
    let mut context = context();
    let proj: Term = Term::proj(Term::var(Var::free("r")), 0);

    // No projection refinement yet: proj reduces to itself and is cached.
    assert_eq!(reduce(&mut context, proj.clone()), Ok(proj.clone()));

    // Refining the projection must clear the cache.
    context.refine_projection(Term::var(Var::free("r")), 0, nat(1));
    assert_eq!(reduce(&mut context, proj), Ok(nat(1)));
}

#[test]
fn leave_frame_with_definitions_invalidates_cached_reduction() {
    let mut context = context();
    let x: Term = Term::var(Var::free("x"));

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

    context.birth_metavar(0, Vec::new(), Term::type_(), None);
    assert_eq!(reduce(&mut context, m.clone()), Ok(m));
}

#[test]
fn reduce_solved_metavar_yields_solution_and_clears_cache() {
    let mut context = context();
    let m = Term::metavar(0);

    context.birth_metavar(0, Vec::new(), Term::type_(), None);

    // First reduce caches the metavariable as itself (it is `reach == 0`).
    assert_eq!(reduce(&mut context, m.clone()), Ok(m.clone()));

    let solution = nat(1);
    context.solve_metavar(0, solution.clone());

    // `solve` cleared the cache, so the stale "itself" reduct is gone.
    assert_eq!(reduce(&mut context, m), Ok(solution));
}

#[test]
fn refinement_is_suppressible() {
    let mut context = context();
    let b = Term::var(Var::free("b"));
    let truth = Term::prim(Prim::Bln(true));

    context.refine("b", &truth);

    // With the refinement active, `b` reduces to its counterfactual value.
    assert_eq!(reduce(&mut context, b.clone()), Ok(truth));

    // Suppressed (as in re-validation), `b` is abstract again.
    let reduced = context.with_suppressed_refinements(|context| reduce(context, b.clone()));
    assert_eq!(reduced, Ok(b));
}
