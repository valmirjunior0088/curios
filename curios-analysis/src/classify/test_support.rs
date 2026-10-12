//! Fixtures the classification's suite reads its terms through: the two reducers a reading is stated under, and the terms it is stated over.
//!
//! `pub(super)` rather than private: consumed by the suite beside it, and nothing outside this module.

use curios_core::{
    Cost, Free, Intrinsic, Nat, ReduceError, Reducer, Subterm, Term, reduce_intrinsic,
};

/// A binder no fixture spells: the suite names its symbols by small indices, so one minted far above them aliases none, and a process-wide counter keeps two openings in one test apart.
fn fresh(hint: Option<&str>) -> Free {
    static MINTED: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(1_000_000);

    Free::local(
        MINTED.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        hint,
    )
}

/// A reducer that reduces nothing, so an atom is the operand as it was written.
pub(super) struct Inert;

impl Reducer for Inert {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    /// Unbudgeted: these fixtures are about what a reading decides, and a limit would only decide it a second time.
    fn spend(&mut self, _cost: Cost) -> Result<(), ReduceError> {
        Ok(())
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        fresh(hint)
    }
}

/// A reducer that folds intrinsics all the way down, for a reading that must take a rebuilt operation's fold rather than inspect its shape. `reduce_intrinsic` reduces its own operands through this seam, so one pass suffices for the terms the suite builds.
pub(super) struct Folding;

impl Reducer for Folding {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        match &*term {
            Subterm::Intrinsic(intrinsic) => Ok(reduce_intrinsic(self, intrinsic)?.into()),
            _ => Ok(term),
        }
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        self.reduce(term)
    }

    fn spend(&mut self, _cost: Cost) -> Result<(), ReduceError> {
        Ok(())
    }

    fn fresh_binder(&mut self, hint: Option<&str>) -> Free {
        fresh(hint)
    }
}

pub(super) fn lit(n: u32) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n as usize)))
}

pub(super) fn sym(index: u32, hint: &'static str) -> Term {
    Term::free_var(&Free::local(index, Some(hint)))
}

pub(super) fn add(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_add(left, right))
}
