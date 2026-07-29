use {
    crate::{
        Atom, Free, Global, InductDecl, InductParam, Kernel, KernelError, Level, Prim, Telescope,
        Term, UniverseContext,
        kernel::infer::{check, infer},
    },
    curios_base::{Plicity, Qualifier, RootId},
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(1_000);
    kernel
}

fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

fn nat(n: usize) -> Term {
    Term::prim(Prim::Nat(crate::Nat::new(n)))
}

fn nat_type() -> Term {
    Term::prim(Prim::NatType)
}

fn bool_type() -> Term {
    Term::prim(Prim::BoolType)
}

fn one() -> Level {
    Level::zero().succ().expect("level zero has a successor")
}

#[test]
fn a_universe_is_one_level_above_itself() {
    let mut kernel = kernel();

    assert_eq!(
        infer(&mut kernel, &Term::type_ground()),
        Ok(Term::type_at(one())),
    );
    assert_eq!(infer(&mut kernel, &Term::prop()), Ok(Term::type_ground()));
}

#[test]
fn a_literal_has_its_carriers_type() {
    let mut kernel = kernel();

    assert_eq!(infer(&mut kernel, &nat(7)), Ok(nat_type()));
    assert_eq!(
        infer(&mut kernel, &Term::prim(Prim::Bool(true))),
        Ok(bool_type()),
    );
}

#[test]
fn a_variable_has_the_type_it_was_bound_at() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    kernel.assume(&x, &nat_type());

    assert_eq!(infer(&mut kernel, &Term::free_var(&x)), Ok(nat_type()));
}

/// A finished term has no free names. Refusing rather than treating one as a
/// neutral is what makes that a checked statement.
#[test]
fn an_unbound_variable_is_refused() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    assert_eq!(
        infer(&mut kernel, &Term::free_var(&x)),
        Err(KernelError::Unbound(x)),
    );
}

#[test]
fn a_lambda_has_the_function_type_over_its_telescope() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let identity = Term::func([(x.clone(), nat_type())], Term::free_var(&x));
    let arrow = Term::func_type([(x, nat_type())], nat_type());

    assert_eq!(infer(&mut kernel, &identity), Ok(arrow));
}

#[test]
fn an_application_substitutes_its_arguments_into_the_result() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let identity = Term::func([(x.clone(), nat_type())], Term::free_var(&x));

    assert_eq!(
        infer(&mut kernel, &Term::apply(identity, [nat(4)])),
        Ok(nat_type()),
    );
}

/// Dependency: applying a family to an argument puts *that argument* into the
/// result type, which is the whole point of a dependent function.
#[test]
fn a_dependent_result_mentions_the_argument_supplied() {
    let mut kernel = kernel();
    let a = binder(0, "A");
    let x = binder(1, "x");

    // `(A : Type, x : A) -> A` applied at `(Nat, 3)` results in `Nat`.
    let f = Term::func(
        [
            (a.clone(), Term::type_ground()),
            (x.clone(), Term::free_var(&a)),
        ],
        Term::free_var(&x),
    );

    assert_eq!(
        infer(&mut kernel, &Term::apply(f, [nat_type(), nat(3)])),
        Ok(nat_type()),
    );
}

#[test]
fn an_argument_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let f = Term::func([(x.clone(), nat_type())], Term::free_var(&x));
    let applied = Term::apply(f, [Term::prim(Prim::Bool(true))]);

    assert!(matches!(
        infer(&mut kernel, &applied),
        Err(KernelError::Mismatch { .. }),
    ));
}

#[test]
fn an_application_of_the_wrong_arity_is_refused() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let f = Term::func([(x.clone(), nat_type())], Term::free_var(&x));
    let applied = Term::apply(f, [nat(1), nat(2)]);

    assert_eq!(
        infer(&mut kernel, &applied),
        Err(KernelError::Arity {
            expected: 1,
            actual: 2
        }),
    );
}

#[test]
fn applying_a_non_function_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        infer(&mut kernel, &Term::apply(nat(1), [nat(2)])),
        Err(KernelError::NotAFunction(_)),
    ));
}

#[test]
fn a_tuple_has_the_product_of_its_components_types() {
    let mut kernel = kernel();

    let pair = Term::tuple([nat(1), Term::prim(Prim::Bool(false))]);
    let type_ = infer(&mut kernel, &pair).expect("a closed pair has a type");

    assert_eq!(
        infer(&mut kernel, &Term::proj(pair.clone(), 0)),
        Ok(nat_type()),
    );
    assert_eq!(infer(&mut kernel, &Term::proj(pair, 1)), Ok(bool_type()));
    assert!(matches!(&*type_, crate::Subterm::TupleType(_)));
}

#[test]
fn projecting_from_a_non_tuple_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        infer(&mut kernel, &Term::proj(nat(1), 0)),
        Err(KernelError::NotATuple(_)),
    ));
}

/// `let` is checked binding by binding and then substituted, so the tail's type
/// is computed with the values in place rather than with opaque names.
#[test]
fn a_let_checks_its_binding_and_substitutes_it() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let term = Term::let_(&x, nat_type(), nat(2), Term::free_var(&x));
    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));

    let wrong = Term::let_(&x, bool_type(), nat(2), Term::free_var(&x));
    assert!(matches!(
        infer(&mut kernel, &wrong),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A recursive group's members are assumed at their declared types while their
/// bodies are checked, which is what lets a member call itself.
#[test]
fn a_recursive_group_checks_its_bodies_against_its_declared_types() {
    let mut kernel = kernel();
    let countdown = binder(0, "countdown");
    let n = binder(1, "n");
    let motive = binder(2, "m");
    let pred = binder(3, "pred");
    let hypothesis = binder(4, "ih");

    let signature = Term::func_type([(n.clone(), nat_type())], nat_type());
    let body = Term::func(
        [(n.clone(), nat_type())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&motive),
            nat_type(),
            nat(0),
            &pred,
            &hypothesis,
            Term::apply(Term::free_var(&countdown), [Term::free_var(&pred)]),
        ),
    );

    let term = Term::rec(
        [(countdown.clone(), signature.clone(), body)],
        Term::apply(Term::free_var(&countdown), [nat(3)]),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// A group whose body does not have the type it declares is refused — the
/// assumption is what the body must live up to, not a licence.
#[test]
fn a_recursive_body_that_misses_its_declared_type_is_refused() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let n = binder(1, "n");

    let signature = Term::func_type([(n.clone(), nat_type())], nat_type());
    let body = Term::func([(n, nat_type())], Term::prim(Prim::Bool(true)));

    let term = Term::rec(
        [(f.clone(), signature, body)],
        Term::apply(Term::free_var(&f), [nat(1)]),
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A constructor's type is its signature's terminal — the family at the
/// parameters and index targets this case aims at.
#[test]
fn a_constructor_has_the_type_its_signature_ends_in() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["Wrapped"]));
    let payload = binder(0, "value");

    // `induct Wrapped | mk(value : Nat) end`
    let constructed = Term::induct_type(name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            params: Telescope::done(()),
            indices: Telescope::done(()),
            constructors: vec![(
                Atom::from("mk"),
                InductParam {
                    telescope: Telescope::build([(payload, nat_type())], constructed.clone()),
                    plicities: vec![Plicity::Explicit],
                },
            )],
            result_sort: Term::type_ground(),
            module: Qualifier::from(["Wrapped"]),
            root: RootId::Entry,
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    let value = Term::variant(name, Vec::<Term>::new(), "mk", [nat(5)]);
    assert_eq!(infer(&mut kernel, &value), Ok(constructed));
}

/// A constructor's payload is checked against its declared field types.
#[test]
fn a_constructor_payload_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["Wrapped"]));
    let payload = binder(0, "value");
    let constructed = Term::induct_type(name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            params: Telescope::done(()),
            indices: Telescope::done(()),
            constructors: vec![(
                Atom::from("mk"),
                InductParam {
                    telescope: Telescope::build([(payload, nat_type())], constructed),
                    plicities: vec![Plicity::Explicit],
                },
            )],
            result_sort: Term::type_ground(),
            module: Qualifier::from(["Wrapped"]),
            root: RootId::Entry,
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    let value = Term::variant(
        name,
        Vec::<Term>::new(),
        "mk",
        [Term::prim(Prim::Bool(true))],
    );
    assert!(matches!(
        infer(&mut kernel, &value),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// An elimination's type is its motive at the scrutinee.
#[test]
fn an_elimination_has_the_type_its_motive_states() {
    let mut kernel = kernel();
    let motive = binder(0, "m");
    let pred = binder(1, "pred");
    let hypothesis = binder(2, "ih");

    let term = Term::nat_match(
        nat(3),
        Some(&motive),
        nat_type(),
        nat(0),
        &pred,
        &hypothesis,
        Term::free_var(&pred),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// Cumulativity: a small type stands where a larger universe is wanted, and
/// `Prop` stands wherever a `Type` is. The reverse does not hold.
#[test]
fn a_small_universe_is_admitted_where_a_larger_one_is_wanted() {
    let mut kernel = kernel();

    assert_eq!(
        check(&mut kernel, &Term::type_ground(), &Term::type_at(one())),
        Ok(()),
    );
    assert_eq!(
        check(&mut kernel, &nat_type(), &Term::type_ground()),
        Ok(())
    );
    assert!(matches!(
        check(&mut kernel, &Term::type_at(one()), &Term::type_ground()),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A primitive's operands are checked against the types its rule demands.
#[test]
fn a_primitive_operand_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();

    let mixed = Term::prim(Prim::nat_add(Term::prim(Prim::Bool(true)), nat(1)));

    assert!(matches!(
        infer(&mut kernel, &mixed),
        Err(KernelError::Mismatch { .. }),
    ));
}

#[test]
fn a_primitive_operation_has_the_result_type_its_rule_states() {
    let mut kernel = kernel();

    assert_eq!(
        infer(&mut kernel, &Term::prim(Prim::nat_add(nat(1), nat(2)))),
        Ok(nat_type()),
    );
    assert_eq!(
        infer(&mut kernel, &Term::prim(Prim::nat_lt(nat(1), nat(2)))),
        Ok(bool_type()),
    );
}

/// A bare list literal carries no element type, so the kernel reads one off the
/// first element and requires the rest to agree. An empty literal names no type
/// at all and is refused rather than guessed.
#[test]
fn a_list_literal_takes_its_element_type_from_its_elements() {
    let mut kernel = kernel();

    assert_eq!(
        infer(&mut kernel, &Term::prim(Prim::Lst(vec![nat(1), nat(2)]))),
        Ok(Term::prim(Prim::LstType(nat_type()))),
    );

    assert!(matches!(
        infer(
            &mut kernel,
            &Term::prim(Prim::Lst(vec![nat(1), Term::prim(Prim::Bool(true))])),
        ),
        Err(KernelError::Mismatch { .. }),
    ));

    assert!(matches!(
        infer(&mut kernel, &Term::prim(Prim::Lst(Vec::new()))),
        Err(KernelError::Unclassified(_)),
    ));
}

/// Elaboration-only syntax reaching the kernel means a term was handed over
/// before elaboration finished with it.
#[test]
fn elaboration_only_syntax_is_refused() {
    let mut kernel = kernel();

    let metavar = Term::metavar(crate::MetaId::from(0usize));
    assert!(matches!(
        infer(&mut kernel, &metavar),
        Err(KernelError::NotCore(_)),
    ));
}
