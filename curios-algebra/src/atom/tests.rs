//! A rank is compared by its key, then by the tie its caller handed it, and hashed by its key alone.

use {
    super::{Atom, Monomial, Rank, Tie},
    std::{
        any::Any,
        cmp::Ordering,
        collections::hash_map::DefaultHasher,
        hash::{Hash, Hasher},
        rc::Rc,
    },
};

/// A caller's tie: a number compared where two keys are one.
#[derive(Debug)]
struct Age(u64);

impl Tie for Age {
    fn tie(&self, other: &dyn Tie) -> Ordering {
        match other.as_any().downcast_ref::<Age>() {
            Some(other) => self.0.cmp(&other.0),
            None => Ordering::Equal,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

fn tied(key: u64, age: u64) -> Rank {
    Rank::Tied(key, Rc::new(Age(age)))
}

fn hashed(rank: &Rank) -> u64 {
    let mut hasher = DefaultHasher::new();
    rank.hash(&mut hasher);
    hasher.finish()
}

#[test]
fn a_tie_is_asked_only_of_two_ranks_of_one_key() {
    assert!(tied(1, 9) < tied(2, 0));
    assert!(tied(1, 0) < tied(1, 1));
    assert_eq!(tied(1, 5), tied(1, 5));
    assert!(tied(1, 5) < Rank::One(2));
    assert_eq!(Rank::One(1).cmp(&tied(1, 5)), Ordering::Equal);
}

#[test]
fn two_ranks_one_order_holds_equal_hash_alike() {
    assert_eq!(hashed(&tied(7, 1)), hashed(&tied(7, 1)));
    assert_eq!(hashed(&Rank::One(7)), hashed(&tied(7, 3)));
}

#[test]
fn a_product_keeps_the_order_it_was_given_on_a_tie_and_sorts_by_the_tie_otherwise() {
    let first = Atom::new(0, tied(1, 5));
    let second = Atom::new(1, tied(1, 5));
    let product = Monomial::product(vec![second.clone(), first.clone()]);
    assert_eq!(product.atoms(), &[second, first]);

    let ordered = Monomial::product(vec![Atom::new(2, tied(1, 6)), Atom::new(3, tied(1, 4))]);
    let indices = ordered.atoms().iter().map(Atom::index).collect::<Vec<_>>();
    assert_eq!(indices, vec![3, 2]);
}
