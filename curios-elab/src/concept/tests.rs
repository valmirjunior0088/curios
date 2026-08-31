use {
    super::{HeadKey, WitnessKey},
    curios_core::{Free, Global, Term},
    curios_utilities::{Grain, Qualifier},
};

/// A tuple type whose fields are all `Type` — the field types are not in the key, so any term does.
fn tuple_type(labels: [Option<&str>; 2]) -> Term {
    Term::tuple_type(
        labels
            .into_iter()
            .enumerate()
            .map(|(index, label)| (Free::local(index as u32, label), Term::type_ground())),
    )
}

// Arity one displays bare, so single-parameter diagnostics keep today's spelling ("for head 'Nat'", never "for head '(Nat)'").
#[test]
fn witness_key_displays_bare_for_arity_one() {
    let key = WitnessKey(vec![HeadKey::Nat]);
    assert_eq!(key.to_string(), "Nat");
}

#[test]
fn witness_key_displays_as_a_tuple_for_higher_arities() {
    let key = WitnessKey(vec![
        HeadKey::Nat,
        HeadKey::Nominal(Global::Authored(Qualifier::from(["std", "Str", "Str"]))),
    ]);
    assert_eq!(key.to_string(), "(Nat, /std/Str/Str)");
}

// Tuple keys are compared componentwise: same first head, different second head is a different table entry.
#[test]
fn witness_keys_differ_beyond_the_first_head() {
    let a = WitnessKey(vec![HeadKey::Nat, HeadKey::Bool]);
    let b = WitnessKey(vec![HeadKey::Nat, HeadKey::Bin(Grain::X)]);
    assert_ne!(a, b);
}

// The shape is the label at each position, so a positional tuple type keys on one empty label per field and on nothing about the field types.
#[test]
fn a_positional_tuple_type_keys_on_its_arity() {
    assert_eq!(
        HeadKey::of_whnf(&tuple_type([None, None])),
        Some(HeadKey::TupleType(vec![String::new(), String::new()]))
    );
}

#[test]
fn a_labeled_tuple_type_keys_on_its_labels() {
    assert_eq!(
        HeadKey::of_whnf(&tuple_type([Some("x"), Some("y")])),
        Some(HeadKey::TupleType(vec!["x".to_owned(), "y".to_owned()]))
    );
}

// Labels are part of a tuple type's identity, so two shapes of one arity are two table entries rather than a duplicate.
#[test]
fn labels_separate_two_shapes_of_one_arity() {
    assert_ne!(
        HeadKey::of_whnf(&tuple_type([None, None])),
        HeadKey::of_whnf(&tuple_type([Some("x"), Some("y")]))
    );
}

#[test]
fn the_unit_type_keys_on_the_empty_shape() {
    assert_eq!(
        HeadKey::of_whnf(&Term::tuple_type_unit()),
        Some(HeadKey::TupleType(Vec::new()))
    );
}

// A shape stands for a type whose field types it does not carry, so it displays them elided rather than inventing a spelling for them.
#[test]
fn a_shape_displays_its_field_types_elided() {
    assert_eq!(HeadKey::TupleType(Vec::new()).to_string(), "{}");
    assert_eq!(
        HeadKey::of_whnf(&tuple_type([None, None]))
            .unwrap()
            .to_string(),
        "{_, _}"
    );
    assert_eq!(
        HeadKey::of_whnf(&tuple_type([Some("x"), Some("y")]))
            .unwrap()
            .to_string(),
        "{x: _, y: _}"
    );
}
