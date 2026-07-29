use {
    super::{HeadKey, WitnessKey},
    curios_base::{Grain, Qualifier},
};

// Arity one displays bare, so single-parameter diagnostics keep today's
// spelling ("for head 'Nat'", never "for head '(Nat)'").
#[test]
fn witness_key_displays_bare_for_arity_one() {
    let key = WitnessKey(vec![HeadKey::Nat]);
    assert_eq!(key.to_string(), "Nat");
}

#[test]
fn witness_key_displays_as_a_tuple_for_higher_arities() {
    let key = WitnessKey(vec![
        HeadKey::Nat,
        HeadKey::Nominal(crate::Global::Authored(Qualifier::from([
            "std", "Str", "Str",
        ]))),
    ]);
    assert_eq!(key.to_string(), "(Nat, /std/Str/Str)");
}

// Tuple keys are compared componentwise: same first head, different second
// head is a different table entry.
#[test]
fn witness_keys_differ_beyond_the_first_head() {
    let a = WitnessKey(vec![HeadKey::Nat, HeadKey::Bool]);
    let b = WitnessKey(vec![HeadKey::Nat, HeadKey::Bin(Grain::X)]);
    assert_ne!(a, b);
}
