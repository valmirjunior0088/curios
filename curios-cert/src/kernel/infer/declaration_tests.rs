//! Constructors, eliminations, definitions and instances, and the descent a recursive declaration owes.

use {
    crate::{KernelError, check, check_definition, infer},
    curios_core::{
        Atom, Global, InductDecl, InductParam, Intrinsic, Level, Telescope, Term,
        UniverseConstraint, UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext,
        UniverseParam,
    },
    curios_utilities::{Plicity, Qualifier},
};

use super::test_support::*;

/// A constructor's type is its signature's terminal — the family at the parameters and index targets this case aims at.
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
            arity: Telescope::done(Telescope::done(())),
            constructors: vec![(
                Atom::from("mk"),
                InductParam::new(
                    Telescope::build([(payload, nat_type())], Vec::new()),
                    vec![Plicity::Explicit],
                ),
            )],
            result_sort: Term::type_ground(),
            module: Qualifier::from(["Wrapped"]),
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

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: vec![(
                Atom::from("mk"),
                InductParam::new(
                    Telescope::build([(payload, nat_type())], Vec::new()),
                    vec![Plicity::Explicit],
                ),
            )],
            result_sort: Term::type_ground(),
            module: Qualifier::from(["Wrapped"]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    let value = Term::variant(
        name,
        Vec::<Term>::new(),
        "mk",
        [Term::intrinsic(Intrinsic::Bool(true))],
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

/// Cumulativity: a small type stands where a larger universe is wanted, and `Prop` stands wherever a `Type` is. The reverse does not hold.
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

/// A generic definition is checked *under* its own constraint set. `(x : Type.{u}) => x` inhabits `(x : Type.{u}) -> Type.{w}` exactly when `u ≤ w` is among the hypotheses — discarding them was the route by which a correct polymorphic definition was refused.
#[test]
fn a_definition_checks_under_its_own_constraints() {
    let (u, w) = (
        Level::param(UniverseParam(0)),
        Level::param(UniverseParam(1)),
    );
    let x = binder(0, "x");
    let name = binder(1, "poly");

    let type_ = Term::func_type(
        [(x.clone(), Term::type_at(u.clone()))],
        Term::type_at(w.clone()),
    );
    let body = Term::func([(x.clone(), Term::type_at(u.clone()))], Term::free_var(&x));

    let constrained = UniverseContext {
        parameter_count: 2,
        constraints: vec![UniverseConstraint {
            lower: u,
            upper: w,
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }],
    };
    let mut kernel = kernel();
    assert_eq!(
        check_definition(&mut kernel, &name, &type_, &body, &constrained),
        Ok(()),
    );

    let unconstrained = UniverseContext {
        parameter_count: 2,
        constraints: Vec::new(),
    };
    let mut kernel = self::kernel();
    assert!(matches!(
        check_definition(&mut kernel, &name, &type_, &body, &unconstrained),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// The other direction: an occurrence must *satisfy* the scheme it instantiates. A scheme declaring `u + 1 ≤ w` refuses the instance `{0, 0}` and admits `{0, 1}`.
#[test]
fn an_instance_must_satisfy_its_schemes_constraints() {
    let u = Level::param(UniverseParam(0));
    let w = Level::param(UniverseParam(1));
    let name = binder(1, "bounded");

    let mut kernel = kernel();
    kernel.declare(
        &name,
        &Term::type_at(u.clone()),
        &UniverseContext {
            parameter_count: 2,
            constraints: vec![UniverseConstraint {
                lower: u.succ().expect("level has a successor"),
                upper: w,
                origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
            }],
        },
    );

    let at = |levels: Vec<Level>| Term::instance_of(&name, levels);

    assert_eq!(
        infer(&mut kernel, &at(vec![Level::zero(), one()])),
        Ok(Term::type_at(Level::zero())),
    );
    assert!(matches!(
        infer(&mut kernel, &at(vec![Level::zero(), Level::zero()])),
        Err(KernelError::UniverseInstance { .. }),
    ));
}

/// The two-line route to `False`: a recursive proof assumed at its own type. Erasure deletes proofs, so a proof-typed `rec` member must descend — and `f = f` has a self-call that decreases on nothing.
#[test]
fn a_recursive_proof_that_does_not_descend_is_refused() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["False"]));
    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: Vec::new(),
            result_sort: Term::prop(),
            module: Qualifier::from(["False"]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );
    let false_ = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());

    let f = binder(0, "f");
    let term = Term::rec(
        [(f.clone(), false_, Term::free_var(&f))],
        Term::free_var(&f),
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::NotDescending { .. }),
    ));
}

/// The type-level twin — `rec Bad : Type = Bad` — is the equi-recursive route: a type-yielding member must descend for the same reason.
#[test]
fn a_recursive_type_that_does_not_descend_is_refused() {
    let mut kernel = kernel();
    let bad = binder(0, "Bad");
    let term = Term::rec(
        [(bad.clone(), Term::type_ground(), Term::free_var(&bad))],
        Term::free_var(&bad),
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::NotDescending { .. }),
    ));
}

/// A *value* recursion that does not descend is not an error: `rec` is general recursion by design, and a program that loops is only a program that loops.
#[test]
fn a_recursive_value_needs_no_descent() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let n = binder(1, "n");

    let signature = Term::func_type([(n.clone(), nat_type())], nat_type());
    let body = Term::func(
        [(n.clone(), nat_type())],
        Term::apply(Term::free_var(&f), [Term::free_var(&n)]),
    );
    let term = Term::rec(
        [(f.clone(), signature, body)],
        Term::apply(Term::free_var(&f), [nat(1)]),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}
