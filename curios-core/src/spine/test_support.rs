//! Fixtures more than one of the spine's peel suites needs: a symbol, a `Nat` spelling over a symbolic inner, a sum, and a literal term read back as its intrinsic.

use super::*;

pub(super) fn sym(index: u32, hint: &'static str) -> Term {
    Term::free_var(&crate::Free::local(index, Some(hint)))
}

pub(super) fn nat_of(floor: u32, inner: Term) -> Nat {
    Nat::Succ(floor.into(), inner)
}

pub(super) fn add(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_add(left, right))
}

pub(super) fn as_intrinsic(term: &Term) -> &Intrinsic {
    match &**term {
        Subterm::Intrinsic(intrinsic) => intrinsic,
        _ => unreachable!("a literal term"),
    }
}
