//! Variance and cumulativity of a function type, the bare-sort head rules, and the sigma and pi rules at an argument position.

use {super::subsumes, crate::infer, curios_core::Term};

use super::test_support::*;

/// The subsumption fork, which the perimeter records as having no fixture in either direction.
///
/// `subsumes` compares a Π's **domains by conversion** and its **codomains cumulatively**. Only one of those two choices can be got wrong in the admitting direction, and it is the domain: reading it *covariantly* would accept `(x : Type 0) -> B` where `(x : Type 1) -> B'` is wanted, so a function that only handles small arguments would be applied to a large one, which is the shape the hierarchy exists to forbid. Contravariance would be sound and strictly more permissive; invariance is what ships, and it is the freely-revisable side precisely because widening later breaks nothing already accepted.
///
/// Both directions are asserted, because invariance is the conjunction of two refusals and a rule that had drifted to covariance would still refuse the other one. The head cases are the control: without them a `subsumes` that refused every function type would satisfy the two assertions above, and cumulativity would be dead while the test stayed green.
#[test]
fn a_function_types_domain_is_invariant() {
    let mut kernel = kernel();
    let x = binder(900, "x");
    let pi = |domain: Term| Term::func_type([(x.clone(), domain)], Term::tuple_type_unit());

    // Neither direction: the domains are compared by conversion, and `Type 0` is not convertible with `Type 1`.
    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Term::type_ground()),
            &pi(Term::type_at(one()))
        ),
        Ok(false),
    );
    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Term::type_at(one())),
            &pi(Term::type_ground())
        ),
        Ok(false),
    );
}

/// The other half of the same fork, and the direction the language needs: under a binder the codomains are still compared cumulatively, `Prop` included.
#[test]
fn a_function_types_codomain_is_cumulative() {
    let mut kernel = kernel();
    let x = binder(901, "x");
    let pi = |codomain: Term| Term::func_type([(x.clone(), nat_type())], codomain);

    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Term::type_ground()),
            &pi(Term::type_at(one()))
        ),
        Ok(true),
    );
    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Term::type_at(one())),
            &pi(Term::type_ground())
        ),
        Ok(false),
    );
    // A proposition stands wherever a type is wanted, under a binder as at the head.
    assert_eq!(
        subsumes(&mut kernel, &pi(Term::prop()), &pi(Term::type_ground())),
        Ok(true),
    );
}

/// The control for both fixtures above: the same three verdicts at the head, where no telescope is walked at all. A `subsumes` that had stopped descending into function types would pass those two and fail these.
#[test]
fn the_head_rules_still_decide_a_bare_sort() {
    let mut kernel = kernel();

    assert_eq!(
        subsumes(&mut kernel, &Term::type_ground(), &Term::type_at(one())),
        Ok(true),
    );
    assert_eq!(
        subsumes(&mut kernel, &Term::type_at(one()), &Term::type_ground()),
        Ok(false),
    );
    assert_eq!(
        subsumes(&mut kernel, &Term::prop(), &Term::type_ground()),
        Ok(true),
    );
}

/// An argument reaches the *checked* rules, not merely inference and subsumption.
///
/// `check` dispatches let-descent, Π-introduction and Σ-introduction before falling through to infer-then-subsume, and an argument position must reach them. For a period it did not: typing drove its child obligations through an explicit worklist that called the node rule and `subsumes` directly, and every argument, constructor payload and record field lost all three rules. Nothing in the prelude or the corpus reached a shape that shows it, so the whole gate passed for two weeks — which is why this fixture is written against the *rule* rather than against a program.
///
/// Mutation-checked: replacing the `check` call at the application arm of `infer_within` with an inference and a `subsumes` refuses this term with a mismatch between `(Type, Nat)` and the dependent pair.
#[test]
fn a_dependent_tuple_in_argument_position_reaches_the_sigma_rule() {
    let mut kernel = kernel();

    let f = binder(42, "f");
    kernel.assume(
        &f,
        &Term::func_type(
            [(binder(43, "p"), dependent_pair_type())],
            Term::tuple_type_unit(),
        ),
    );

    // `f((7))`: well-typed only by opening the telescope at the first component, which is what the Σ rule does and inference cannot.
    let call = Term::apply(Term::free_var(&f), [Term::tuple([nat_type(), nat(7)])]);

    assert_eq!(infer(&mut kernel, &call), Ok(Term::tuple_type_unit()));
}

/// The Π half of the rule above: a lambda in argument position is checked against its expected function type, so its *body* keeps the expectation.
///
/// Routing the body through `check` is what lets it reach the Σ rule with that expectation intact; inferring the lambda instead manufactures the non-dependent codomain `(Type, Nat)` first, and the mismatch surfaces at the codomain rather than at the body. The two rules compose here exactly as they do in `check`.
///
/// Mutation-checked with the fixture above, and by the same edit.
#[test]
fn a_lambda_in_argument_position_reaches_the_pi_rule() {
    let mut kernel = kernel();

    let g = binder(44, "g");
    kernel.assume(
        &g,
        &Term::func_type(
            [(
                binder(45, "k"),
                Term::func_type([(binder(46, "n"), nat_type())], dependent_pair_type()),
            )],
            Term::tuple_type_unit(),
        ),
    );

    // `g((n) => (n))`: the body is a dependent pair, and only the checked route hands it the expectation that makes it one.
    let n = binder(47, "n");
    let call = Term::apply(
        Term::free_var(&g),
        [Term::func(
            [(n.clone(), nat_type())],
            Term::tuple([nat_type(), Term::free_var(&n)]),
        )],
    );

    assert_eq!(infer(&mut kernel, &call), Ok(Term::tuple_type_unit()));
}
