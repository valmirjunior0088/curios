//! One typing rule each: universes, literals, variables, lambdas, applications, tuples, lets and recursive groups.

use {
    super::test_support::*,
    crate::{Counted, Error, Kernel, check, infer},
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

/// `let` is checked binding by binding, and its tail typed over the binders — `binding_tests` holds what binding one means.
#[test]
fn a_let_checks_its_binding_before_its_tail() {
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

/// A pair of the level below with itself, `depth` deep, over `base`, beside the record of its type over `base_type`.
fn doubled_pair(base: Term, base_type: Term, depth: u32) -> (Term, Term) {
    (0..depth).fold((base, base_type), |(value, type_), level| {
        (
            Term::tuple([value.clone(), value]),
            Term::tuple_type([
                (binder(2 * level + 100, "first"), type_.clone()),
                (binder(2 * level + 101, "second"), type_),
            ]),
        )
    })
}

/// A term naming a local is typed once per node too, for as long as what its typing read of the scope stands: sixty levels of a pair over a binder are sixty-one nodes, and inferring it answers where a walk per path does not. At a depth the uncached kernel affords, both give the same type.
///
/// Mutation-checked: with no type read off the scope answered, the sixty-level inference runs the budget out.
#[test]
fn a_shared_term_over_a_local_is_typed_once_per_node() {
    let n = binder(0, "n");
    let over = |mut kernel: Kernel| {
        kernel.assume(&n, &nat_type());
        kernel
    };
    let pair = |depth| doubled_pair(Term::free_var(&n), nat_type(), depth).0;

    assert!(infer(&mut over(kernel()), &pair(60)).is_ok());
    assert_eq!(
        infer(&mut over(kernel()), &pair(8)),
        infer(&mut over(Kernel::uncached(100_000, SYNTAX)), &pair(8)),
    );
}

/// And under an arm, where no type was remembered at all: the same pair is typed once per node while the arm's equation stands.
#[test]
fn a_shared_term_under_an_arm_is_typed_once_per_node() {
    let n = binder(0, "n");
    let m = binder(1, "m");
    let mut kernel = kernel();
    kernel.assume(&n, &nat_type());
    kernel.assume(&m, &nat_type());

    let inferred = kernel.scoped(|kernel| {
        kernel
            .refine(Term::free_var(&m), nat(0))
            .expect("the equation records");
        infer(kernel, &doubled_pair(Term::free_var(&n), nat_type(), 60).0)
    });

    assert!(inferred.is_ok());
}

/// A tuple checked against a record reaches no inference — the Σ rule checks each field at its entry's type — so the check itself is remembered: a pair of the level below with itself, sixty deep, checks against the record of its type in the size of the two graphs.
///
/// Mutation-checked: with no check read off the scope answered, it runs the budget out.
#[test]
fn a_shared_tuple_is_checked_once_per_node() {
    let n = binder(0, "n");
    let mut kernel = kernel();
    kernel.assume(&n, &nat_type());
    let (value, type_) = doubled_pair(Term::free_var(&n), nat_type(), 60);

    assert_eq!(check(&mut kernel, &value, &type_), Ok(()));
}

/// A remembered type, and a remembered check, live as long as the equations they were taken under. `f(a)` is typed only where `a`'s type converts with `f`'s domain, which holds under `n = 0` and nowhere else: refused before the arm, accepted inside it, refused after it — and the uncached kernel agrees on all three, inferring and checking.
///
/// Mutation-checked: with either scoped table left standing where an equation moves, its judgment accepts the term after the arm.
#[test]
fn a_remembered_type_does_not_outlive_the_equations_it_was_taken_under() {
    type Judge = fn(&mut Kernel, &Term) -> bool;
    let inferring: Judge = |kernel, term| infer(kernel, term).is_ok();
    let checking: Judge = |kernel, term| check(kernel, term, &nat_type()).is_ok();

    let sequence = |mut kernel: Kernel, judge: Judge| {
        let n = binder(0, "n");
        let family = binder(1, "P");
        let f = binder(2, "f");
        let a = binder(3, "a");
        let x = binder(4, "x");
        let at = |index: Term| Term::apply(Term::free_var(&family), [index]);

        kernel.assume(&n, &nat_type());
        kernel.assume(
            &family,
            &Term::func_type([(x, nat_type())], Term::type_ground()),
        );
        kernel.assume(
            &f,
            &Term::func_type([(x, at(Term::free_var(&n)))], nat_type()),
        );
        kernel.assume(&a, &at(nat(0)));
        let term = Term::apply(Term::free_var(&f), [Term::free_var(&a)]);

        let before = judge(&mut kernel, &term);
        let inside = kernel.scoped(|kernel| {
            kernel
                .refine(Term::free_var(&n), nat(0))
                .expect("the equation records");
            judge(kernel, &term)
        });
        let after = judge(&mut kernel, &term);

        [before, inside, after]
    };

    for judge in [inferring, checking] {
        let cached = sequence(kernel(), judge);

        assert_eq!(cached, [false, true, false]);
        assert_eq!(cached, sequence(Kernel::uncached(100_000, SYNTAX), judge));
    }
}

/// Nor its binder: a name assumed again at another type, once its first binder is closed, is typed at the second.
///
/// Mutation-checked: answering a scoped type without asking whether its binders stand types the second `h` at `Nat`.
#[test]
fn a_remembered_type_does_not_outlive_its_binder() {
    let mut kernel = kernel();
    let h = binder(0, "h");
    let term = Term::free_var(&h);

    let first = kernel.scoped(|kernel| {
        kernel.assume(&h, &nat_type());
        infer(kernel, &term)
    });
    let second = kernel.scoped(|kernel| {
        kernel.assume(&h, &bool_type());
        infer(kernel, &term)
    });

    assert_eq!(first, Ok(nat_type()));
    assert_eq!(second, Ok(bool_type()));
}
