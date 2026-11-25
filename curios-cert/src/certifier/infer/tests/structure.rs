//! Structure occurrences at their declared parameter count, and the syntax elaboration may leave behind.

use {
    crate::certifier::infer::test_support::*,
    crate::{test_support::*, *},
    curios_common::Qualifier,
    curios_core::Global,
};

/// A list or a box *of* proofs is not a proposition, and the typing rule has to say so — `Sort::of` already does.
///
/// `Sort::of` routes a parameterized former through `sort_of_intrinsic`, which lands a `Prop`-sorted element at `Type 0` on the reasoning `sort/classification_tests.rs` states: a list has a length and a box has an identity, so their inhabitants are distinguishable however indistinguishable the elements are. The typing rule must agree: reporting the *element's* sort as the former's would infer `List(P)` at `Prop` while `Sort::of(List(P))` says `Type 0`.
///
/// Only one of those can be right, and the disagreement is a closed inhabitant of `False`. `Prop` is the type of propositions, so a former admitted there stands wherever one is wanted: at `(X : Prop, x : X, y : X) -> Same(@X)(x, y)`, an equation between two proofs of `X` — its one constructor discharges it, since irrelevance identifies any two inhabitants of `X` — instantiating `X` at `List(P)` yields `Same(@List(P))([p], [])` for a one-element list against the empty one. Congruence through `List/length` carries that to `1` and `0`, and transport turns `()` into a proof of `False`.
///
/// `curios/src/tests/rules`'s `a_list_of_proofs_is_not_a_proposition` is the surface program.
///
/// The controls are the other half. A list at a relevant element still reports that element's level, `List(Type 0)` included, so the rule does not pin every former at zero; and a genuine proposition still stands where a `Prop` is wanted, so it does not refuse the position outright.
#[test]
fn a_list_or_box_of_proofs_is_not_a_proposition() {
    let mut certifier = certifier();
    // `induct P : Prop end` — an empty proposition, so `Sort::of` reads `Prop` off its declaration.
    declared(&mut certifier, "induct /P: pub Prop\nend");
    let [list, held, proposition] = terms(
        "(#ListType(#{node = InductType} /P()), #BoxType(#{node = InductType} /P()), #{node = InductType} /P())",
    );
    let (ground, prop) = (term("Type"), term("Prop"));

    assert_eq!(infer(&mut certifier, &list), Ok(ground.clone()));
    assert_eq!(infer(&mut certifier, &held), Ok(ground.clone()));

    assert!(matches!(
        check(&mut certifier, &list, &prop),
        Err(Error::Mismatch { .. }),
    ));
    assert!(matches!(
        check(&mut certifier, &held, &prop),
        Err(Error::Mismatch { .. }),
    ));

    // A former still carries its element's level rather than being pinned at zero, and a proposition still stands where one is wanted.
    assert_eq!(
        infer(&mut certifier, &term("#ListType(#NatType)")),
        Ok(ground)
    );
    assert_eq!(
        infer(&mut certifier, &term("#ListType(Type)")),
        Ok(term("#{level = 1} Type")),
    );
    assert_eq!(check(&mut certifier, &proposition, &prop), Ok(()));
}

/// Elaboration-only syntax reaching the certifier means a term was handed over before elaboration finished with it.
#[test]
fn elaboration_only_syntax_is_refused() {
    let mut certifier = certifier();

    // A derived witness body that elaboration never wrote is the same class of residue as a metavariable: the transient is refused wholesale, so no derivation can smuggle an unchecked body past the certifier.
    assert!(matches!(
        infer(&mut certifier, &term("#{node = Derive} ?")),
        Err(Error::NotCore(_)),
    ));
    assert!(matches!(
        infer(&mut certifier, &term("#{id = 0, origin = Hole} ?")),
        Err(Error::NotCore(_)),
    ));
}

/// A nominal occurrence states as many parameters as its declaration declares, and the two rules that open a `struct` arity at an occurrence's parameters must check that before opening it.
///
/// `Telescope::open` asserts on a count mismatch, so an unguarded rule does not refuse the item — it **aborts the walk**, losing every other verdict, which is what makes `certify_definitions`' count a count. `check_instance` decides the *universe* width and says nothing about the parameters, so `infer`'s projection rule and `check`'s record rule reach `open` only through the checked handle, as `Sort::of` and `synth_neutral` do.
///
/// Neither is reachable from a surface program — the elaborator does not emit an occurrence at the wrong count — so this is the shape that needs a term built by hand, and it is a fail-open-to-abort rather than an unsoundness. Both rules are reached here at a one-parameter family occurring with none: the projection through a variable assumed at that type, and the record through its literal, which states the parameters it is built at.
#[test]
fn a_structure_occurrence_at_the_wrong_parameter_count_is_refused() {
    let mut certifier = certifier();
    declared(&mut certifier, BOXED);
    let [projected, literal] = under(
        &mut certifier,
        "v: #{node = StructType} /Boxed()",
        "#{field = Struct(/Boxed, 0)} v.0, /Boxed { 0 }",
    );

    assert!(
        matches!(infer(&mut certifier, &projected), Err(Error::Arity { .. }),),
        "a projection opened the arity at a parameter count the declaration does not have",
    );
    assert!(
        matches!(infer(&mut certifier, &literal), Err(Error::Arity { .. }),),
        "a record literal opened the arity at a parameter count the declaration does not have",
    );
}

/// The control: at the parameter count it declares, the same family still projects and still accepts a record.
///
/// Without it the guard above would pass just as well if the rules refused every `struct` occurrence.
#[test]
fn a_structure_occurrence_at_its_declared_parameter_count_still_works() {
    let mut certifier = certifier();
    declared(&mut certifier, BOXED);
    let [projected, literal, exact] = under(
        &mut certifier,
        "v: #{node = StructType} /Boxed(#NatType)",
        "#{field = Struct(/Boxed, 0)} v.0, /Boxed(#NatType) { 7 }, #{node = StructType} /Boxed(#NatType)",
    );

    assert_eq!(infer(&mut certifier, &projected), Ok(term("#NatType")));
    assert_eq!(check(&mut certifier, &literal, &exact), Ok(()));
}

/// A structure's type has one literal, the structure's own: a tuple literal of the same fields is no value of it.
///
/// Elaboration refuses the literal there, so only a term built by hand holds one, and a rule that took it field by field as a Σ's is taken admitted a value nothing reads: a projection out of a structure names it and reads a literal of that structure alone, so `.0` of a binder bound to `(7)` at `Boxed(Nat)` was typed at `Nat` and reduced to no literal. The control above is the literal that is one.
#[test]
fn a_tuple_literal_is_no_value_of_a_structure() {
    let mut certifier = certifier();
    declared(&mut certifier, BOXED);

    assert!(matches!(
        check(
            &mut certifier,
            &term("(7,)"),
            &term("#{node = StructType} /Boxed(#NatType)")
        ),
        Err(Error::Mismatch { .. }),
    ));
}

/// A record's field is checked at the field's type opened at the parameters its literal states: `Boxed(Nat) { true }` is refused, inferred as it is written.
#[test]
fn a_records_field_is_checked_at_the_parameters_its_literal_states() {
    let mut certifier = certifier();
    declared(&mut certifier, BOXED);

    assert!(matches!(
        infer(&mut certifier, &term("/Boxed(#NatType) { true }")),
        Err(Error::Mismatch { .. }),
    ));
}

/// A projection says what its slot is a slot of, and the certifier holds that to its head's type.
///
/// The name is what a reader of the term reads the field by with no type in hand: a report writes a structure's field by its label or its place among the plain ones, which it looks up under the name the projection carries. A name nothing held to the head's type would be read as a field of a structure the value is none of, so each way the two can part is refused: a tuple's slot off a structure's value, a structure's off a tuple, and a slot of one structure off a value of another. The control above is the pair that meets.
#[test]
fn a_slot_of_one_thing_read_off_a_value_of_another_is_refused() {
    let mut certifier = certifier();
    declared(&mut certifier, BOXED);
    let [tuple_slot, struct_slot, other_slot, own_slot] = under(
        &mut certifier,
        "v: #{node = StructType} /Boxed(#NatType), p: {a: #NatType}",
        "v.0, #{field = Struct(/Boxed, 0)} p.0, #{field = Struct(/Other, 0)} v.0, p.0",
    );
    let named = |path: &str| Global::Authored(Qualifier::from([path]));

    assert!(matches!(
        infer(&mut certifier, &tuple_slot),
        Err(Error::NotASlotOf { named: None, .. }),
    ));
    assert!(matches!(
        infer(&mut certifier, &struct_slot),
        Err(Error::NotASlotOf { named: Some(found), .. }) if found == named("Boxed"),
    ));
    assert!(matches!(
        infer(&mut certifier, &other_slot),
        Err(Error::NotASlotOf { named: Some(found), .. }) if found == named("Other"),
    ));

    // The tuple's own slot still reads.
    assert_eq!(infer(&mut certifier, &own_slot), Ok(term("#NatType")));
}
