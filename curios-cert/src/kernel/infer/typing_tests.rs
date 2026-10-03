//! One typing rule each: universes, literals, variables, lambdas, applications, tuples, lets and recursive groups.

use {
    super::test_support::*,
    crate::{Counted, Error, Kernel, infer},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Intrinsic, Nat, Subterm, Term},
};

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
        infer(&mut kernel, &Term::intrinsic(Intrinsic::Bool(true))),
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

/// A finished term has no free names. Refusing rather than treating one as a neutral is what makes that a checked statement.
#[test]
fn an_unbound_variable_is_refused() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    assert_eq!(
        infer(&mut kernel, &Term::free_var(&x)),
        Err(Error::Unbound(x)),
    );
}

#[test]
fn a_lambda_has_the_function_type_over_its_telescope() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let identity = Term::func([(x, nat_type())], Term::free_var(&x));
    let arrow = Term::func_type([(x, nat_type())], nat_type());

    assert_eq!(infer(&mut kernel, &identity), Ok(arrow));
}

#[test]
fn an_application_substitutes_its_arguments_into_the_result() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let identity = Term::func([(x, nat_type())], Term::free_var(&x));

    assert_eq!(
        infer(&mut kernel, &Term::apply(identity, [nat(4)])),
        Ok(nat_type()),
    );
}

/// Dependency: applying a family to an argument puts *that argument* into the result type, which is the whole point of a dependent function.
#[test]
fn a_dependent_result_mentions_the_argument_supplied() {
    let mut kernel = kernel();
    let a = binder(0, "A");
    let x = binder(1, "x");

    // `(A : Type, x : A) -> A` applied at `(3)` results in `Nat`.
    let f = Term::func(
        [(a, Term::type_ground()), (x, Term::free_var(&a))],
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

    let f = Term::func([(x, nat_type())], Term::free_var(&x));
    let applied = Term::apply(f, [Term::intrinsic(Intrinsic::Bool(true))]);

    assert!(matches!(
        infer(&mut kernel, &applied),
        Err(Error::Mismatch { .. }),
    ));
}

#[test]
fn an_application_of_the_wrong_arity_is_refused() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let f = Term::func([(x, nat_type())], Term::free_var(&x));
    let applied = Term::apply(f, [nat(1), nat(2)]);

    assert_eq!(
        infer(&mut kernel, &applied),
        Err(Error::Arity {
            counted: Counted::Arguments,
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
        Err(Error::NotAFunction(_)),
    ));
}

#[test]
fn a_tuple_has_the_product_of_its_components_types() {
    let mut kernel = kernel();

    let pair = Term::tuple([nat(1), Term::intrinsic(Intrinsic::Bool(false))]);
    let type_ = infer(&mut kernel, &pair).expect("a closed pair has a type");

    assert_eq!(
        infer(&mut kernel, &Term::proj(pair.clone(), 0)),
        Ok(nat_type()),
    );
    assert_eq!(infer(&mut kernel, &Term::proj(pair, 1)), Ok(bool_type()));
    assert!(matches!(&*type_, Subterm::TupleType(_)));
}

#[test]
fn projecting_from_a_non_tuple_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        infer(&mut kernel, &Term::proj(nat(1), 0)),
        Err(Error::NotATuple(_)),
    ));
}

/// `let` is checked binding by binding and then substituted, so the tail's type is computed with the values in place rather than with opaque names.
#[test]
fn a_let_checks_its_binding_and_substitutes_it() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let term = Term::let_(&x, nat_type(), nat(2), Term::free_var(&x));
    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));

    let wrong = Term::let_(&x, bool_type(), nat(2), Term::free_var(&x));
    assert!(matches!(
        infer(&mut kernel, &wrong),
        Err(Error::Mismatch { .. }),
    ));
}

/// A recursive group's members are assumed at their declared types while their bodies are checked, which is what lets a member call itself.
#[test]
fn a_recursive_group_checks_its_bodies_against_its_declared_types() {
    let mut kernel = kernel();
    let countdown = binder(0, "countdown");
    let n = binder(1, "n");
    let motive = binder(2, "m");
    let pred = binder(3, "pred");
    let hypothesis = binder(4, "ih");

    let signature = Term::func_type([(n, nat_type())], nat_type());
    let body = Term::func(
        [(n, nat_type())],
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
        [(countdown, signature.clone(), body)],
        Term::apply(Term::free_var(&countdown), [nat(3)]),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// A group whose body does not have the type it declares is refused — the assumption is what the body must live up to, not a licence.
#[test]
fn a_recursive_body_that_misses_its_declared_type_is_refused() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let n = binder(1, "n");

    let signature = Term::func_type([(n, nat_type())], nat_type());
    let body = Term::func([(n, nat_type())], Term::intrinsic(Intrinsic::Bool(true)));

    let term = Term::rec(
        [(f, signature, body)],
        Term::apply(Term::free_var(&f), [nat(1)]),
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::Mismatch { .. }),
    ));
}

/// A local-free term's type is remembered for the declaration, so a term whose tree is exponential in its depth — a sum adding a shared subterm to itself sixty times over, the shape a reduct takes when each step mentions the one before it more than once — is typed in the size of its graph. At a depth the uncached kernel can afford, both give the same type.
#[test]
fn a_shared_closed_term_is_typed_once_per_node() {
    let doubled = |depth: usize| {
        let mut term = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
        for _ in 0..depth {
            term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
        }
        term
    };

    assert!(infer(&mut kernel(), &doubled(60)).is_ok());

    let mut uncached = Kernel::uncached(100_000, SYNTAX);
    assert_eq!(
        infer(&mut kernel(), &doubled(8)),
        infer(&mut uncached, &doubled(8))
    );
}

/// Nor does a hit replay the identities the inference it remembers minted. Each level here applies a lambda, whose binder typing opens, to a sum sharing the level below twice; replayed per hit, those identities double with every level as a memo-free kernel's would, and forty levels are past the identity space. A hit opens nothing, so the graph mints what typing each of its nodes once does.
#[test]
fn a_shared_closed_term_mints_once_per_node() {
    let x = binder(0, "x");
    let identity = Term::func([(x, nat_type())], Term::free_var(&x));
    let mut term = nat(1);
    for _ in 0..40 {
        term = Term::apply(
            identity.clone(),
            [Term::intrinsic(Intrinsic::nat_add(term.clone(), term))],
        );
    }

    let mut kernel = kernel();
    let (_, before) = kernel.consumption();
    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
    let (_, after) = kernel.consumption();
    assert!(after - before <= 40, "minted {}", after - before);
}
