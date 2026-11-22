//! What the suites of `calls` share: the groups they type and the calls read off them.

use {
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::Term,
};

pub fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

/// The group `rec f(n : Nat) -> Type = ${body}`, as the term that states it and names `f` in its tail: the member every fixture defines.
pub const GROUP: &str =
    "#{node = Rec} (let f: (n: #NatType) -> Type = (n: #NatType) => ${body}; f)";

/// [`GROUP`] at `body`.
pub fn group(body: &str) -> Term {
    term(&fill!(GROUP, body))
}

/// The two-arm split of `${scrutinee}` at `Type`: `Nat` when it is zero, `${cons}` when it is `pred + 1`.
pub const SPLIT: &str =
    "#{goal = Type} match ${scrutinee} | 0 => #NatType | pred + 1 => ${cons} end";

/// [`SPLIT`] of `scrutinee` with `cons` in its successor arm.
pub fn split(scrutinee: &str, cons: &str) -> String {
    fill!(SPLIT, scrutinee, cons)
}
