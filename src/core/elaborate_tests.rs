use {
    super::*,
    crate::core::{Atom, Nat, Prim, Term},
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
fn infer_synthesizes_a_primitive_type() {
    let mut context = context();

    let (term, type_) = elaborate(&mut context, &nat_lit(0), Mode::Infer).unwrap();

    assert_eq!(term, nat_lit(0));
    assert_eq!(type_, nat());
}

#[test]
fn check_accepts_a_well_typed_term() {
    let mut context = context();

    let (term, type_) = elaborate(&mut context, &nat_lit(3), Mode::Check(nat())).unwrap();

    assert_eq!(term, nat_lit(3));
    assert_eq!(type_, nat());
}

#[test]
fn check_rejects_a_type_mismatch() {
    let mut context = context();

    let bln = Subterm::Prim(Prim::BlnType).into();
    let result = elaborate(&mut context, &nat_lit(3), Mode::Check(bln));

    assert!(result.is_err());
}

#[test]
fn naturally_checked_func_elaborates_against_a_function_type() {
    let mut context = context();

    // `\ _ -> 0` checked against `(_ : Nat) -> Nat`.
    let func_type = Term::func_type([("x", nat())], nat());
    let func = Term::func(["x"], nat_lit(0));

    let (term, type_) = elaborate(&mut context, &func, Mode::Check(func_type.clone())).unwrap();

    assert_eq!(term, func);
    assert_eq!(type_, func_type);
}

#[test]
fn naturally_checked_func_cannot_infer() {
    let mut context = context();

    let func = Term::func(["x"], nat_lit(0));
    let result = elaborate(&mut context, &func, Mode::Infer);

    assert!(result.is_err());
}

#[test]
fn naturally_checked_atom_elaborates_against_its_atom_type() {
    let mut context = context();

    let atom_type = Term::atom_type(["red", "green"]);
    let atom = Term::atom(Atom::from("green"));

    let (term, type_) = elaborate(&mut context, &atom, Mode::Check(atom_type.clone())).unwrap();

    assert_eq!(term, atom);
    assert_eq!(type_, atom_type);
}

#[test]
fn check_on_a_hole_births_it_freezing_the_local_context() {
    let mut context = context();

    // With `x : Nat` in scope, checking the hole `?0` against `Nat` births it,
    // recording `Nat` as its type and the in-scope assumptions as its frozen Γ.
    context.assume("x", &nat());

    let hole = Term::metavar(0);
    let (term, type_) = elaborate(&mut context, &hole, Mode::Check(nat())).unwrap();

    assert_eq!(term, hole);
    assert_eq!(type_, nat());

    let entry = context.metavar_entry(0).expect("hole was born");
    assert_eq!(entry.result, nat());
    assert_eq!(entry.telescope, vec![("x".to_string(), nat())]);
}

#[test]
fn infer_on_an_unborn_hole_cannot_infer() {
    let mut context = context();

    let result = elaborate(&mut context, &Term::metavar(0), Mode::Infer);

    assert!(result.is_err());
}
