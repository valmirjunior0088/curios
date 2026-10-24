use {
    super::{Atoms, same_atom},
    crate::{Free, Intrinsic, Level, Nat, Term},
    curios_algebra::Carrier,
};

/// One call of a universe-polymorphic name at two instances, and the call at another argument.
fn calls() -> (Term, Term, Term) {
    let name = Free::local(0, Some("f"));
    let at = |level: u32, argument: u64| {
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
        Carrier::Bytes,
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

fn sym(index: u64, hint: &'static str) -> Term {
    Term::free_var(&Free::local(index, Some(hint)))
}

fn nat(value: u32) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

/// A truth value that is a comparison is the atom of what it states: `x < y` and `x + 1 <= y` are one, to the direct comparison, to a table and to a signed table, and `y <= x` and `y < x + 1` are its negation — another handle, and the signed handle read negated.
#[test]
fn a_comparison_is_the_atom_of_what_it_states() {
    let (x, y) = (sym(0, "x"), sym(1, "y"));
    let less = Term::intrinsic(Intrinsic::nat_lt(x.clone(), y.clone()));
    let successor = Term::intrinsic(Intrinsic::nat_lte(
        Nat::rebuild(1u32.into(), x.clone()),
        y.clone(),
    ));
    let dual = Term::intrinsic(Intrinsic::nat_lte(y.clone(), x.clone()));
    let dual_successor = Term::intrinsic(Intrinsic::nat_lt(y, Nat::rebuild(1u32.into(), x)));

    assert!(same_atom(Carrier::Boolean, &less, &successor));
    assert!(same_atom(Carrier::Boolean, &dual, &dual_successor));
    assert!(!same_atom(Carrier::Boolean, &less, &dual));

    let mut atoms = Atoms::default();
    let handle = atoms.handle(Carrier::Boolean, &less);
    assert!(handle == atoms.handle(Carrier::Boolean, &successor));
    let negation = atoms.handle(Carrier::Boolean, &dual);
    assert!(negation != handle);
    assert!(negation == atoms.handle(Carrier::Boolean, &dual_successor));

    let mut atoms = Atoms::default();
    let (atom, negated) = atoms.signed(&less);
    assert!(!negated);
    assert_eq!(atoms.signed(&successor), (atom.clone(), false));
    assert_eq!(atoms.signed(&dual), (atom.clone(), true));
    assert_eq!(atoms.signed(&dual_successor), (atom, true));
}

/// An equality of a sum of naturals against zero is the order comparison it states, and an equality is one whichever way round it is written: `n == 0` is `n <= 0` and the negation of `0 < n` and of `n != 0`, and `j != i` is the negation of `i == j`.
#[test]
fn an_equality_is_read_as_what_it_states() {
    let n = sym(0, "n");
    let mut atoms = Atoms::default();
    let (zero, negated) = atoms.signed(&Term::intrinsic(Intrinsic::nat_eql(n.clone(), nat(0))));
    assert!(!negated);
    let at_most = Term::intrinsic(Intrinsic::nat_lte(n.clone(), nat(0)));
    assert_eq!(atoms.signed(&at_most), (zero.clone(), false));
    let positive = Term::intrinsic(Intrinsic::nat_lt(nat(0), n.clone()));
    assert_eq!(atoms.signed(&positive), (zero.clone(), true));
    let unequal = Term::intrinsic(Intrinsic::NatNeq(n, nat(0)));
    assert_eq!(atoms.signed(&unequal), (zero, true));

    let (i, j) = (sym(1, "i"), sym(2, "j"));
    let mut atoms = Atoms::default();
    let (equal, negated) = atoms.signed(&Term::intrinsic(Intrinsic::IntEql(i.clone(), j.clone())));
    assert!(!negated);
    let swapped = Term::intrinsic(Intrinsic::IntEql(j.clone(), i.clone()));
    assert_eq!(atoms.signed(&swapped), (equal.clone(), false));
    let unequal = Term::intrinsic(Intrinsic::IntNeq(j, i));
    assert_eq!(atoms.signed(&unequal), (equal, true));
}

/// A comparison with no linear view is known as written: on `Flt` both `a < b` and `b <= a` are false against a NaN, so neither is the other's negation and they are two atoms read positive.
#[test]
fn a_comparison_with_no_view_is_known_as_written() {
    let (a, b) = (sym(0, "a"), sym(1, "b"));
    let less = Term::intrinsic(Intrinsic::FltLt(a.clone(), b.clone()));
    let mirrored = Term::intrinsic(Intrinsic::FltLe(b, a));
    assert!(!same_atom(Carrier::Boolean, &less, &mirrored));

    let mut atoms = Atoms::default();
    let (this, this_negated) = atoms.signed(&less);
    let (that, that_negated) = atoms.signed(&mirrored);
    assert!(this != that);
    assert!(!this_negated && !that_negated);
}
