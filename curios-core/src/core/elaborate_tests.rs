use {
    super::*,
    crate::core::{MetavarId, Nat, Prim, Term},
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
    let func = Term::func([("x", Term::metavar(0))], nat_lit(0));

    let (term, type_) = elaborate(&mut context, &func, Mode::Check(func_type.clone())).unwrap();

    // Elaboration is authoritative: the rebuilt lambda carries its domain solved
    // from the expected function type, so the hole is gone and the term is
    // meta-free.
    assert!(term.metavars().is_empty());
    assert_eq!(type_, func_type);
}

#[test]
fn naturally_checked_func_cannot_infer() {
    let mut context = context();

    // A lambda whose domain is an unconstrained hole (the bare `(x) => …` sugar)
    // has nothing to synthesize a domain from, so inference still fails.
    let func = Term::func([("x", Term::metavar(0))], nat_lit(0));
    let result = elaborate(&mut context, &func, Mode::Infer);

    assert!(result.is_err());
}

#[test]
fn annotated_func_infers_a_function_type() {
    let mut context = context();

    // `(x : Nat) => x` synthesizes `(Nat) -> Nat` on its own — no expected type.
    let func = Term::func([("x", nat())], Term::free_var("x"));
    let (term, type_) = elaborate(&mut context, &func, Mode::Infer).unwrap();

    // Meta-free, and convertible (alpha-insensitive) to the expected function
    // type; a structural `assert_eq!` would trip only on the cosmetic fresh
    // binder label the Infer arm generates.
    assert!(term.metavars().is_empty());
    assert!(
        convert_with(
            &mut context,
            &type_,
            &Term::func_type([("x", nat())], nat())
        )
        .unwrap()
    );
}

#[test]
fn check_on_a_hole_births_it_freezing_the_local_context() {
    let mut context = context();

    // `x : Nat` is a genuine *local* binder — assumed inside a frame, the way a
    // lambda or match body brings one into scope. Only locals are frozen into a
    // metavariable's Γ; top-level definitions are excluded (a solution may
    // mention them as globals instead — see `Context::identity_snapshot`), so
    // the binder must be inside a frame to appear here. Checking the hole `?0`
    // against `Nat` births it, recording `Nat` as its type and the in-scope
    // locals as its frozen Γ.
    let (term, type_) = context.with_frame(|context| {
        context.assume("x", &nat());
        let hole = Term::metavar(0);
        elaborate(context, &hole, Mode::Check(nat())).unwrap()
    });

    // Birth rebuilds the hole with the identity spine over its frozen Γ — the
    // delayed substitution that keeps its eventual solution aligned through
    // every later `close`/`open`.
    assert_eq!(
        term,
        Term::metavar_birthed(0, None, vec![Term::free_var("x")])
    );
    assert_eq!(type_, nat());

    let entry = context.metavar_entry(MetavarId(0)).expect("hole was born");
    assert_eq!(entry.result, nat());
    assert_eq!(*entry.telescope, vec![("x".to_string(), nat())]);
}

#[test]
fn infer_on_an_unborn_hole_cannot_infer() {
    let mut context = context();

    let result = elaborate(&mut context, &Term::metavar(0), Mode::Infer);

    assert!(result.is_err());
}
