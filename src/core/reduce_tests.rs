use {
    super::*,
    crate::core::{Atom, Flt, Int, Nat, Prim, Var},
    std::time::Duration,
};

fn context() -> Context {
    Context::new(Duration::from_millis(10))
}

#[test]
fn reduce_apply_beta_reduces() {
    let mut context = context();

    let term: Term = Term::apply(
        Term::func(["x"], Term::var(Var::free("x"))),
        [Term::atom(Atom::from("ok"))],
    );

    assert_eq!(
        reduce(&mut context, term.clone()),
        Ok(Term::atom(Atom::from("ok")))
    );
}

#[test]
fn reduce_match_selects_match() {
    let mut context = context();

    let term: Term = Term::match_(
        Term::atom(Atom::from("a")),
        Some("m"),
        Term::type_(),
        vec![
            ("a", Term::atom(Atom::from("yes"))),
            ("b", Term::atom(Atom::from("no"))),
        ],
    );

    assert_eq!(
        reduce(&mut context, term.clone()),
        Ok(Term::atom(Atom::from("yes")))
    );
}

#[test]
fn reduce_nat_fold_zero_is_not_true() {
    let mut context = context();

    let term: Term = Term::nat_induction(
        Subterm::Prim(Prim::Nat(Nat::new(0usize))),
        Some("m"),
        Term::atom_type(["false", "true"]),
        Term::atom(Atom::from("false")),
        "pred",
        "ih",
        Term::atom(Atom::from("true")),
    );

    assert_ne!(
        reduce(&mut context, term.clone()),
        Ok(Term::atom(Atom::from("true")))
    );
}

#[test]
fn reduce_let_then_var_unfolds_definition() {
    let mut context = context();

    context.define("y", &Term::atom(Atom::from("done")));

    let term: Term = Term::let_(
        "x",
        Term::type_(),
        Term::var(Var::free("y")),
        Term::var(Var::free("x")),
    );

    assert_eq!(
        reduce(&mut context, term.clone()),
        Ok(Term::atom(Atom::from("done")))
    );
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
fn reduce_int_eql_returns_true_or_false_atom() {
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

    let term: Term = Term::proj(
        Term::tuple([Term::atom(Atom::from("a")), Term::atom(Atom::from("b"))]),
        1,
    );

    assert_eq!(
        reduce(&mut context, term.clone()),
        Ok(Term::atom(Atom::from("b")))
    );
}

#[test]
fn reduce_proj_table_lookup() {
    let mut context = context();

    context.define_projection(Term::var(Var::free("r")), 0, Term::atom(Atom::from("ok")));

    let term: Term = Term::proj(Term::var(Var::free("r")), 0);

    assert_eq!(
        reduce(&mut context, term.clone()),
        Ok(Term::atom(Atom::from("ok")))
    );
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
        ["y"],
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
    context.define("x", &Term::atom(Atom::from("hi")));
    assert_eq!(reduce(&mut context, x), Ok(Term::atom(Atom::from("hi"))));
}

#[test]
fn define_projection_invalidates_cached_reduction() {
    let mut context = context();
    let proj: Term = Term::proj(Term::var(Var::free("r")), 0);

    // No projection entry yet: proj reduces to itself and is cached.
    assert_eq!(reduce(&mut context, proj.clone()), Ok(proj.clone()));

    // Refining the projection must clear the cache.
    context.define_projection(Term::var(Var::free("r")), 0, Term::atom(Atom::from("ok")));
    assert_eq!(reduce(&mut context, proj), Ok(Term::atom(Atom::from("ok"))));
}

#[test]
fn leave_frame_with_definitions_invalidates_cached_reduction() {
    let mut context = context();
    let x: Term = Term::var(Var::free("x"));

    // Inside a frame, define x and reduce — the cache will hold x → "inner".
    context.with_frame(|context| {
        context.define("x", &Term::atom(Atom::from("inner")));
        assert_eq!(
            reduce(context, x.clone()),
            Ok(Term::atom(Atom::from("inner")))
        );
    });

    // After the frame pops, x has no definition again. A stale cache entry
    // would still return "inner"; the cache clear on leave_frame prevents that.
    assert_eq!(reduce(&mut context, x.clone()), Ok(x));
}
