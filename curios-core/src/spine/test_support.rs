//! Fixtures more than one of the spine's peel suites needs: a symbol, a `Nat` spelling over a symbolic inner, a sum, and a literal term read back as its intrinsic; and the `Nat` peel over terms, which the reduction suite puts to a term it has not lifted.

use super::*;

pub fn sym(index: u32, hint: &'static str) -> Term {
    Term::free_var(&crate::Free::local(u64::from(index), Some(hint)))
}

pub fn nat_of(floor: u32, inner: Term) -> Nat {
    Nat::Succ(floor.into(), inner)
}

pub fn add(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_add(left, right))
}

pub fn as_intrinsic(term: &Term) -> &Intrinsic {
    match &**term {
        Subterm::Intrinsic(intrinsic) => intrinsic,
        _ => unreachable!("a literal term"),
    }
}

/// The `Nat` peel over two reduced terms: [`peel_nat_pair`]'s cancellation, read where either side is a successor floor or a sum spine. `None` means neither is, so the caller keeps its own handling.
pub fn peel_nat_terms(left: &Term, right: &Term) -> Option<Verdict> {
    (nat_shaped(left) || nat_shaped(right)).then(|| Nat::cancellation_deduced(left, right))
}

/// Whether a reduced term is one of the two shapes [`peel_nat_terms`] can act on: a successor floor, or a sum spine.
fn nat_shaped(term: &Term) -> bool {
    match &**term {
        Subterm::Intrinsic(intrinsic) => nat_shaped_intrinsic(intrinsic),
        _ => false,
    }
}
