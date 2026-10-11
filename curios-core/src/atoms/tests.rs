use {
    super::{Atoms, same_atom},
    crate::{Free, Level, Term},
    curios_algebra::Carrier,
    curios_num::Grain,
};

/// One call of a universe-polymorphic name at two instances, and the call at another argument.
fn calls() -> (Term, Term, Term) {
    let name = Free::local(0, Some("f"));
    let at = |level: u32, argument: u32| {
        Term::apply(
            Term::instance_of(&name, vec![Level::constant(level)]),
            [Term::free_var(&Free::local(argument, Some("x")))],
        )
    };
    (at(0, 1), at(1, 1), at(0, 2))
}

/// An atom of a carrier whose values hold no type is one up to universe instances, to the direct comparison and to the handle a table hands out, and two calls apart in an argument stay two.
#[test]
fn an_atom_of_a_carrier_that_holds_no_type_is_one_up_to_universe_instances() {
    let (this, that, other) = calls();
    for carrier in [
        Carrier::Natural,
        Carrier::Integer,
        Carrier::Boolean,
        Carrier::Byte,
        Carrier::Float,
        Carrier::Packed(Grain::X),
        Carrier::Packed(Grain::B),
    ] {
        assert!(same_atom(carrier, &this, &that), "{carrier:?}");
        assert!(!same_atom(carrier, &this, &other), "{carrier:?}");

        let mut atoms = Atoms::default();
        let handle = atoms.handle(carrier, &this);
        assert!(handle == atoms.handle(carrier, &that), "{carrier:?}");
        assert!(handle != atoms.handle(carrier, &other), "{carrier:?}");
    }
}

/// A list holds values of any type, a type among them, so an atom of one is known as written: two instances are two atoms.
#[test]
fn an_atom_of_a_list_is_known_as_written() {
    let (this, that, _) = calls();
    assert!(same_atom(Carrier::List, &this, &this));
    assert!(!same_atom(Carrier::List, &this, &that));

    let mut atoms = Atoms::default();
    let handle = atoms.handle(Carrier::List, &this);
    assert!(handle != atoms.handle(Carrier::List, &that));
}

/// A product's factor is known as written at every carrier: its rank is its own structural hash, which is the order distribution spells a spine in.
#[test]
fn a_factor_as_written_is_two_at_two_instances() {
    let (this, that, _) = calls();
    let mut atoms = Atoms::default();
    let handle = atoms.exact(&this);
    assert!(handle == atoms.exact(&this));
    assert!(handle != atoms.exact(&that));
}
