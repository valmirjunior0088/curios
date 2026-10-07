use {
    super::{HeadKey, WitnessId, WitnessKey},
    crate::{Free, Global, Term},
    curios_num::Grain,
    curios_utilities::{Plicity, Qualifier},
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

/// A function type whose domains and result are all `Type`, every binder named `name` — the domains and result are not in the key, and the names must not be either.
fn func_type(marks: &[Plicity], name: &str) -> Term {
    Term::func_type_marked(
        marks.iter().enumerate().map(|(index, mark)| {
            (
                *mark,
                Free::local(index as u32, Some(name)),
                Term::type_ground(),
            )
        }),
        Term::type_ground(),
    )
}

// Arity one displays bare, so a single-parameter diagnostic spells a bare head ("for head 'Nat'", never "for head '(Nat)'").
#[test]
fn witness_key_displays_bare_for_arity_one() {
    let key = WitnessKey::from([HeadKey::Nat]);
    assert_eq!(key.to_string(), "Nat");
}

#[test]
fn witness_key_displays_as_a_tuple_for_higher_arities() {
    let key = WitnessKey::from([
        HeadKey::Nat,
        HeadKey::Nominal(Global::Authored(Qualifier::from(["std", "Str", "Str"]))),
    ]);
    assert_eq!(key.to_string(), "(Nat, /std/Str/Str)");
}

// Tuple keys are compared componentwise: same first head, different second head is a different table entry.
#[test]
fn witness_keys_differ_beyond_the_first_head() {
    let a = WitnessKey::from([HeadKey::Nat, HeadKey::Bool]);
    let b = WitnessKey::from([HeadKey::Nat, HeadKey::Bin(Grain::X)]);
    assert_ne!(a, b);
}

// A key is an identity: built twice from equal heads it is one key, and two keys order as their heads do, whichever was named first.
#[test]
fn a_key_is_one_identity_for_equal_heads_and_orders_by_them() {
    let later = WitnessKey::from([HeadKey::Bool]);
    let earlier = WitnessKey::from([HeadKey::Nat]);

    assert_eq!(later, WitnessKey::from(vec![HeadKey::Bool]));
    assert_eq!(later.heads(), [HeadKey::Bool]);
    assert!(earlier < later);
}

fn witness(module: &str, concept: &str, key: WitnessKey, place: u32) -> WitnessId {
    WitnessId::Spelled {
        concept: Qualifier::from(["std", concept]),
        key,
        module: Qualifier::from(["std", module]),
        place,
    }
}

// A witness's name says what it witnesses and where: its module, then its concept applied to its key, and nothing of where it sits in its module.
#[test]
fn a_witness_is_displayed_by_its_module_concept_and_key() {
    let lift = WitnessKey::from([
        HeadKey::Nominal(Global::Authored(Qualifier::from(["std", "Option"]))),
        HeadKey::Nat,
    ]);

    assert_eq!(
        witness("Nat", "Show", WitnessKey::from([HeadKey::Nat]), 0).to_string(),
        "/std/Nat/witness@/std/Show(Nat)"
    );
    assert_eq!(
        witness("Option", "Lift", lift, 0).to_string(),
        "/std/Option/witness@/std/Lift(/std/Option, Nat)"
    );
    // A second witness of one module at one concept and key is the duplicate the elaborator refuses, and needs a name to be refused under.
    assert_eq!(
        witness("Nat", "Show", WitnessKey::from([HeadKey::Nat]), 1).to_string(),
        "/std/Nat/witness@/std/Show(Nat)#1"
    );
    assert_eq!(
        WitnessId::Unspelled {
            module: Qualifier::from(["std", "Nat"]),
            place: 3,
        }
        .to_string(),
        "/std/Nat/witness@3"
    );
}

// The concept and the key lead the order, so the witnesses a unit declares at one concept and key are neighbours whichever modules declare them.
#[test]
fn witnesses_order_by_concept_and_key_before_their_module() {
    let nat = WitnessKey::from([HeadKey::Nat]);
    let bool_ = WitnessKey::from([HeadKey::Bool]);
    let mut names = [
        witness("Zeta", "Show", bool_, 0),
        witness("Alpha", "Show", nat, 0),
        witness("Alpha", "Show", bool_, 0),
        witness("Alpha", "Eq", nat, 0),
    ];
    names.sort();

    assert_eq!(
        names,
        [
            witness("Alpha", "Eq", nat, 0),
            witness("Alpha", "Show", nat, 0),
            witness("Alpha", "Show", bool_, 0),
            witness("Zeta", "Show", bool_, 0),
        ]
    );
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

// A function type is not keyable at all, whatever its marks: its key space is nearly one point, so a concept's owner claiming a shape would claim it program-wide.
#[test]
fn a_function_type_is_not_a_key() {
    for marks in [
        [].as_slice(),
        &[Plicity::Explicit],
        &[Plicity::Implicit],
        &[Plicity::Explicit, Plicity::Witness],
    ] {
        assert_eq!(HeadKey::of_whnf(&func_type(marks, "p")), None);
    }
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
