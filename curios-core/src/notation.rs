//! The hook a crate below `curios-surface` writes Core through: a kernel's refusal names terms and levels, and what writes them is the surface crate's printer, handed in by whoever reports the refusal.

use crate::{Level, Term};

/// What writes Core's terms and levels for a crate that cannot reach the printer. `curios-surface`'s spelling implements it, and a reporter hands one in under the policy it reports by: how globals are shortened, whether universes show.
pub trait Notation {
    /// `term`, written.
    fn term(&self, term: &Term) -> String;

    /// `level`, written.
    fn level(&self, level: &Level) -> String;
}
