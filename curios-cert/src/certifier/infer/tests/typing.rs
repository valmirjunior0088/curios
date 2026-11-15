//! One typing rule each: universes, literals, variables, lambdas, applications, tuples, lets and recursive groups.

use {
    crate::certifier::infer::test_support::*,
    crate::{test_support::*, *},
    curios_core::{Free, Subterm},
};

#[test]
fn a_universe_is_one_level_above_itself() {
    let mut certifier = certifier();
    let one = term("#{level = 1} Type");

    assert_eq!(infer(&mut certifier, &term("Type")), Ok(one.clone()));
    assert_eq!(infer(&mut certifier, &term("Prop")), Ok(one));
}

#[test]
fn a_literal_has_its_carriers_type() {
    let mut certifier = certifier();

    assert_eq!(infer(&mut certifier, &term("7")), Ok(term("#NatType")));
    assert_eq!(infer(&mut certifier, &term("true")), Ok(term("#BoolType")));
}

#[test]
fn a_variable_has_the_type_it_was_bound_at() {
    let mut certifier = certifier();
    let [x] = under(&mut certifier, "x: #NatType", "x,");

    assert_eq!(infer(&mut certifier, &x), Ok(term("#NatType")));
}

/// A finished term has no free names. Refusing rather than treating one as a neutral is what makes that a checked statement.
#[test]
fn an_unbound_variable_is_refused() {
    let mut certifier = certifier();

    assert_eq!(
        infer(&mut certifier, &term("#{binding = Free(0)} x")),
        Err(Error::Unbound(Free::local(0, Some("x")))),
    );
}

#[test]
fn a_lambda_has_the_function_type_over_its_telescope() {
    let mut certifier = certifier();

    assert_eq!(
        infer(&mut certifier, &term("(x: #NatType) => x")),
        Ok(term("(x: #NatType) -> #NatType"))
    );
}

#[test]
fn an_application_substitutes_its_arguments_into_the_result() {
    let mut certifier = certifier();

    assert_eq!(
        infer(&mut certifier, &term("((x: #NatType) => x)(4)")),
        Ok(term("#NatType")),
    );
}

/// Dependency: applying a family to an argument puts *that argument* into the result type, which is the whole point of a dependent function.
#[test]
fn a_dependent_result_mentions_the_argument_supplied() {
    let mut certifier = certifier();

    // `(A : Type, x : A) -> A` applied at `(3)` results in `Nat`.
    assert_eq!(
        infer(&mut certifier, &term("((A: Type, x: A) => x)(#NatType, 3)")),
        Ok(term("#NatType")),
    );
}

#[test]
fn an_argument_of_the_wrong_type_is_refused() {
    let mut certifier = certifier();

    assert!(matches!(
        infer(&mut certifier, &term("((x: #NatType) => x)(true)")),
        Err(Error::Mismatch { .. }),
    ));
}

#[test]
fn an_application_of_the_wrong_arity_is_refused() {
    let mut certifier = certifier();

    assert_eq!(
        infer(&mut certifier, &term("((x: #NatType) => x)(1, 2)")),
        Err(Error::Arity {
            counted: Counted::Arguments,
            expected: 1,
            actual: 2
        }),
    );
}

/// A domain is opened at the argument its own binder was given and no other: under `(T: Type, U: Type, x: T)`, `3` at `(Bool, Nat)` is refused, where opening `x`'s domain one binder off would read it at `Nat`.
#[test]
fn a_later_domain_is_opened_at_the_argument_its_own_binder_was_given() {
    let mut certifier = certifier();

    assert!(matches!(
        infer(
            &mut certifier,
            &term("((T: Type, U: Type, x: T) => x)(#BoolType, #NatType, 3)")
        ),
        Err(Error::Mismatch { .. }),
    ));
}

/// A tuple checked against a Σ checks each component at its entry opened at the components before it: `(Bool, 3)` is no value of `(t: Type, x: t)`.
#[test]
fn a_tuple_component_is_checked_at_its_entry_opened_at_the_components_before_it() {
    let mut certifier = certifier();

    assert!(matches!(
        check(
            &mut certifier,
            &term("(#BoolType, 3)"),
            &term(DEPENDENT_PAIR)
        ),
        Err(Error::Mismatch { .. }),
    ));
}

#[test]
fn applying_a_non_function_is_refused() {
    let mut certifier = certifier();

    assert!(matches!(
        infer(&mut certifier, &term("1(2)")),
        Err(Error::NotAFunction(_)),
    ));
}

#[test]
fn a_tuple_has_the_product_of_its_components_types() {
    let mut certifier = certifier();
    let [pair, first, second] = terms("((1, false), (1, false).0, (1, false).1)");

    let type_ = infer(&mut certifier, &pair).expect("a closed pair has a type");

    assert_eq!(infer(&mut certifier, &first), Ok(term("#NatType")));
    assert_eq!(infer(&mut certifier, &second), Ok(term("#BoolType")));
    assert!(matches!(&*type_, Subterm::TupleType(_)));
}

#[test]
fn projecting_from_a_non_tuple_is_refused() {
    let mut certifier = certifier();

    assert!(matches!(
        infer(&mut certifier, &term("(1).0")),
        Err(Error::NotATuple(_)),
    ));
}

/// `let` is checked binding by binding, and its tail typed over the binders — `binding_tests` holds what binding one means.
#[test]
fn a_let_checks_its_binding_before_its_tail() {
    let mut certifier = certifier();

    assert_eq!(
        infer(&mut certifier, &term("(let x: #NatType = 2; x)")),
        Ok(term("#NatType"))
    );
    assert!(matches!(
        infer(&mut certifier, &term("(let x: #BoolType = 2; x)")),
        Err(Error::Mismatch { .. }),
    ));
}

/// A recursive group's members are assumed at their declared types while their bodies are checked, which is what lets a member call itself.
#[test]
fn a_recursive_group_checks_its_bodies_against_its_declared_types() {
    let mut certifier = certifier();

    assert_eq!(
        infer(
            &mut certifier,
            &term(
                "#{node = Rec} (let countdown: (n: #NatType) -> #NatType = (n: #NatType) => match n: (m) => #NatType | 0 => 0 | pred + 1 => countdown(pred) end; countdown(3))"
            )
        ),
        Ok(term("#NatType"))
    );
}

/// A group whose body does not have the type it declares is refused — the assumption is what the body must live up to, not a licence.
#[test]
fn a_recursive_body_that_misses_its_declared_type_is_refused() {
    let mut certifier = certifier();

    assert!(matches!(
        infer(
            &mut certifier,
            &term("#{node = Rec} (let f: (n: #NatType) -> #NatType = (n: #NatType) => true; f(1))")
        ),
        Err(Error::Mismatch { .. }),
    ));
}

/// A lambda is checked against a function type under that type's marks: one whose binder is `@` where the slot is plain takes no Π-introduction there, and inferred instead it inhabits a function type of other marks, which the expectation refuses. The control is the same lambda under the slot's own mark.
#[test]
fn a_lambda_of_other_marks_does_not_check_against_a_function_type() {
    let mut certifier = certifier();
    let expected = term("(m: #NatType) -> #NatType");

    assert_eq!(
        check(&mut certifier, &term("(n: #NatType) => n"), &expected),
        Ok(())
    );
    assert!(matches!(
        check(&mut certifier, &term("(@n: #NatType) => n"), &expected),
        Err(Error::Mismatch { .. })
    ));
}
