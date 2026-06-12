use {
    super::*,
    crate::core::{Nat, Prim, Term},
    std::time::Duration,
};

fn context() -> Context {
    Context::new(Duration::from_secs(1))
}

fn nat() -> Term {
    Subterm::Prim(Prim::NatType).into()
}

fn nat_lit(n: usize) -> Term {
    Subterm::Prim(Prim::Nat(Nat::new(n))).into()
}

#[test]
fn zonk_leaves_a_meta_free_term_unchanged() {
    let context = context();

    let term = Term::func([("x", Term::type_())], nat_lit(0));
    let zonked = zonk(&context, &term).unwrap();

    assert_eq!(zonked, term);
}

#[test]
fn zonk_replaces_a_solved_metavariable_with_its_solution() {
    let mut context = context();

    context.birth_metavar(0, Vec::new(), Term::type_());
    context.solve_metavar(0, nat());

    let zonked = zonk(&context, &Term::metavar(0)).unwrap();

    assert_eq!(zonked, nat());
}

#[test]
fn zonk_resolves_a_metavariable_nested_in_a_structure() {
    let mut context = context();

    context.birth_metavar(0, Vec::new(), Term::type_());
    context.solve_metavar(0, nat());

    // A tuple `{ ?0 }` zonks to `{ Nat }`.
    let term = Subterm::Tuple(Tuple {
        fields: vec![Term::metavar(0)],
        names: vec![],
    })
    .into();

    let zonked = zonk(&context, &term).unwrap();

    let expected = Subterm::Tuple(Tuple {
        fields: vec![nat()],
        names: vec![],
    })
    .into();

    assert_eq!(zonked, expected);
    assert!(zonked.metavars().is_empty());
}

#[test]
fn zonk_chases_a_solution_that_mentions_another_metavariable() {
    let mut context = context();

    // ?0 := ?1, ?1 := Nat. Zonking ?0 must resolve through to `Nat`.
    context.birth_metavar(0, Vec::new(), Term::type_());
    context.birth_metavar(1, Vec::new(), Term::type_());
    context.solve_metavar(1, nat());
    context.solve_metavar(0, Term::metavar(1));

    let zonked = zonk(&context, &Term::metavar(0)).unwrap();

    assert_eq!(zonked, nat());
}

#[test]
fn zonk_rejects_an_unsolved_metavariable() {
    let mut context = context();

    context.birth_metavar(0, Vec::new(), Term::type_());

    let result = zonk(&context, &Term::metavar(0));

    assert!(result.is_err());
}
