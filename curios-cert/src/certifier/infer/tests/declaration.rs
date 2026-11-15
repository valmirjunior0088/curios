//! Constructors, eliminations, definitions and instances, and the descent a recursive declaration owes.

use {
    crate::certifier::infer::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::{Definition, DefinitionKind},
};

/// `induct Wrapped | mk(value : Nat) end`.
const WRAPPED: &str = "induct /Wrapped: pub Type\n| mk(value: #NatType)\nend";

/// A constructor's type is its signature's terminal — the family at the parameters and index targets this case aims at.
#[test]
fn a_constructor_has_the_type_its_signature_ends_in() {
    let mut certifier = certifier();
    declared(&mut certifier, WRAPPED);

    assert_eq!(
        infer(&mut certifier, &term("#{node = Variant} /Wrapped/mk(5)")),
        Ok(term("#{node = InductType} /Wrapped()"))
    );
}

/// A constructor's payload is checked against its declared field types.
#[test]
fn a_constructor_payload_of_the_wrong_type_is_refused() {
    let mut certifier = certifier();
    declared(&mut certifier, WRAPPED);

    assert!(matches!(
        infer(&mut certifier, &term("#{node = Variant} /Wrapped/mk(true)")),
        Err(Error::Mismatch { .. }),
    ));
}

/// An elimination's type is its goal.
#[test]
fn an_elimination_has_the_type_its_goal_states() {
    let mut certifier = certifier();

    assert_eq!(
        infer(
            &mut certifier,
            &term("#{goal = #NatType} match 3 | 0 => 0 | pred + 1 => pred end")
        ),
        Ok(term("#NatType"))
    );
}

/// Cumulativity: a small type stands where a larger universe is wanted, and `Prop` stands wherever a `Type` is. The reverse does not hold.
#[test]
fn a_small_universe_is_admitted_where_a_larger_one_is_wanted() {
    let mut certifier = certifier();
    let (ground, one) = (term("Type"), term("#{level = 1} Type"));

    assert_eq!(check(&mut certifier, &ground, &one), Ok(()));
    assert_eq!(check(&mut certifier, &term("#NatType"), &ground), Ok(()));
    assert!(matches!(
        check(&mut certifier, &one, &ground),
        Err(Error::Mismatch { .. }),
    ));
}

/// The one definition `text` writes.
fn definition(text: &str) -> Definition {
    let mut definitions = read_module(text)
        .items
        .iter()
        .flat_map(|item| item.definitions())
        .collect::<Vec<_>>();
    assert_eq!(definitions.len(), 1, "one definition");

    definitions.remove(0)
}

/// `poly.{u, v} : (x : Type.{u}) -> Type.{v} = (x) => x` under `${constraints}`.
const POLY: &str = "#{universe_context = {parameter_count = 2, constraints = [${constraints}]}}\nlet /poly: (x: #{level = u} Type) -> #{level = v} Type = (x: #{level = u} Type) => x;";

/// A generic definition is checked *under* its own constraint set. `(x : Type.{u}) => x` inhabits `(x : Type.{u}) -> Type.{v}` exactly when `u ≤ v` is among the hypotheses — discarding them would refuse a correct polymorphic definition.
#[test]
fn a_definition_checks_under_its_own_constraints() {
    let checked = |constraints: &str| {
        let poly = definition(&fill!(POLY, constraints));

        check_definition(
            &mut certifier(),
            &poly.name,
            &DefinitionKind::Authored,
            &poly.type_,
            &poly.body,
            &poly.universe_context,
        )
    };

    assert!(checked("u <= v").is_ok());
    assert!(matches!(checked(""), Err(Error::Mismatch { .. })));
}

/// The other direction: an occurrence must *satisfy* the scheme it instantiates. A scheme declaring `u + 1 ≤ v` refuses the instance `{0, 0}` and admits `{0, 1}`.
///
/// The name is declared with no body, as a judgment declares one before its body, so the type and the scheme are all an occurrence reads.
#[test]
fn an_instance_must_satisfy_its_schemes_constraints() {
    let mut certifier = certifier();
    let bounded = definition(
        "#{universe_context = {parameter_count = 2, constraints = [u + 1 <= v]}}\nlet /bounded: #{level = u} Type = #NatType;",
    );
    certifier
        .declare(&bounded.name, &bounded.type_, &bounded.universe_context)
        .expect("a name is declared once");

    assert_eq!(
        infer(
            &mut certifier,
            &term("#{universes = {levels = [0, 1]}} /bounded")
        ),
        Ok(term("Type")),
    );
    assert!(matches!(
        infer(
            &mut certifier,
            &term("#{universes = {levels = [0, 0]}} /bounded")
        ),
        Err(Error::UniverseInstance { .. }),
    ));
}

/// The two-line route to `False`: a recursive proof assumed at its own type. Erasure deletes proofs, so a proof-typed `rec` member must descend — and `f = f` has a self-call that decreases on nothing.
#[test]
fn a_recursive_proof_that_does_not_descend_is_refused() {
    let mut certifier = certifier();
    declared(&mut certifier, "induct /False: pub Prop\nend");

    assert!(matches!(
        infer(
            &mut certifier,
            &term("#{node = Rec} (let f: #{node = InductType} /False() = f; f)")
        ),
        Err(Error::NotDescending { .. }),
    ));
}

/// The type-level twin — `rec Bad : Type = Bad` — is the equi-recursive route: a type-yielding member must descend for the same reason.
#[test]
fn a_recursive_type_that_does_not_descend_is_refused() {
    let mut certifier = certifier();

    assert!(matches!(
        infer(
            &mut certifier,
            &term("#{node = Rec} (let Bad: Type = Bad; Bad)")
        ),
        Err(Error::NotDescending { .. }),
    ));
}

/// A *value* recursion that does not descend is not an error: `rec` is general recursion by design, and a program that loops is only a program that loops.
#[test]
fn a_recursive_value_needs_no_descent() {
    let mut certifier = certifier();

    assert_eq!(
        infer(
            &mut certifier,
            &term("#{node = Rec} (let f: (n: #NatType) -> #NatType = (n: #NatType) => f(n); f(1))")
        ),
        Ok(term("#NatType"))
    );
}
