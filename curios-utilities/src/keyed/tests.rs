use {
    super::Keyed,
    std::{
        cell::Cell,
        collections::HashMap,
        hash::{Hash, Hasher},
        rc::Rc,
    },
};

/// A field that counts how often it is compared, as a graph's comparison would be a walk.
#[derive(Clone)]
struct Counted {
    value: u32,
    compared: Rc<Cell<u32>>,
}

impl PartialEq for Counted {
    fn eq(&self, other: &Self) -> bool {
        self.compared.set(self.compared.get() + 1);

        self.value == other.value
    }
}

impl Hash for Counted {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

/// Two keys that differ in a later field are told apart by their hashes, the field they share never compared; a key is compared field by field only with the key it is.
///
/// Mutation-checked: with the fields compared whatever the hashes say, the shared field is read where the keys differ.
#[test]
fn a_key_that_differs_is_told_apart_before_a_field_is_compared() {
    let compared = Rc::new(Cell::new(0));
    let shared = || Counted {
        value: 7,
        compared: compared.clone(),
    };
    let (this, that) = (Keyed::new((shared(), 1u8)), Keyed::new((shared(), 2u8)));

    assert!(this != that);
    assert_eq!(compared.get(), 0);

    assert!(this == Keyed::new((shared(), 1u8)));
    assert_eq!(compared.get(), 1);
}

/// A table keyed this way finds an entry under a key built apart from the one it was filed under, and under no other.
#[test]
fn an_entry_is_found_under_a_key_built_apart() {
    let mut table = HashMap::new();
    table.insert(Keyed::new((String::from("type"), true)), 1);

    assert_eq!(
        table.get(&Keyed::new((String::from("type"), true))),
        Some(&1)
    );
    assert_eq!(table.get(&Keyed::new((String::from("type"), false))), None);
}
