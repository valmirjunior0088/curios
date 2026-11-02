//! Every walk this crate's classification runs over whole terms, over one doubling term: sixty levels that each hold the one below twice under one operation, around a local's application, a tree no walk per path finishes and a graph of sixty-one nodes. A walk the crate adds joins the table here, so one a change makes per-path again stalls its row rather than waiting for a profile; one that reads through a checker joins the table in `tests/driven/walk_tests.rs`, which has one.

use {
    crate::{Classes, atoms_in, atoms_of, atoms_within, classed, classed_in, refold},
    curios_core::{Cost, Free, Intrinsic, ReduceError, Reducer, Subterm, Term},
};

/// A reducer that reduces nothing and spends nothing, so a walk meets the term as it was built.
struct Inert;

impl Reducer for Inert {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    fn spend(&mut self, _cost: Cost) -> Result<(), ReduceError> {
        Ok(())
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        Free::local(1_000_000, hint)
    }
}

/// `base` under sixty levels of `Nat/and`, each holding the level below twice.
fn doubled(base: Term) -> Term {
    (0..60).fold(base, |below, _| {
        Term::intrinsic(Intrinsic::NatAnd(below.clone(), below))
    })
}

/// `f(x)` for a local `f` and the local `x`, an atom no reading takes apart.
fn applied(x: u64, hint: &'static str) -> Term {
    Term::apply(
        Term::free_var(&Free::local(0, Some("f"))),
        [Term::free_var(&Free::local(x, Some(hint)))],
    )
}

#[test]
fn every_walk_answers_a_doubling_term_in_its_own_size() {
    let this = doubled(applied(1, "x"));
    let that = doubled(applied(2, "y"));
    let Subterm::Intrinsic(operation) = &*this else {
        panic!("a doubling term is an operation");
    };

    let rows: Vec<(&str, bool)> = vec![
        ("a pair's atoms", atoms_of(&mut Inert, &this, &that).is_ok()),
        ("a term's atoms", atoms_in(&mut Inert, &this).is_ok()),
        (
            "a stuck operation's atoms",
            atoms_within(&mut Inert, operation, &[]).is_ok(),
        ),
        (
            "a stuck operation's fold taken again",
            atoms_within(&mut Inert, operation, &[])
                .and_then(|within| refold(&mut Inert, operation, within, &[]))
                .is_ok(),
        ),
        (
            "a pair as its classes spell it",
            classed(&mut Inert, &this, &that, &mut Classes::default()).is_ok(),
        ),
        (
            "a term as its classes spell it",
            classed_in(&mut Inert, &this, &mut Classes::default()).is_ok(),
        ),
    ];

    for (walk, answered) in rows {
        assert!(answered, "{walk} over a doubling term did not answer");
    }
}
