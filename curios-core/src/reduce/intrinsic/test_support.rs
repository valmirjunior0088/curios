//! Fixtures the intrinsic-fold suites share: the terms they fold and the reducer they fold through.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    super::{Reducer, reduce_intrinsic},
    crate::{Cost, Free, Intrinsic, Nat, One, ReduceError, Scope, Subterm, Term},
    curios_utilities::{Grain, PackedBin},
};

/// A reducer that reduces nothing. Every operand below is already a literal — a weak-head normal form — so no strategy is involved, and running the comparison body against an inert reducer says exactly that: the outcome is decided by the structural compare, not by anything unfolded.
pub(super) struct Inert;

impl Reducer for Inert {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        Ok(term)
    }

    /// Unbudgeted: these fixtures are about what a fold *decides*, and a limit would only decide it a second time.
    fn spend(&mut self, _cost: Cost) -> Result<(), ReduceError> {
        Ok(())
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

pub(super) fn occurrences(term: &Term, wanted: &Term) -> usize {
    Nat::summands(term)
        .iter()
        .filter(|summand| *summand == wanted)
        .count()
}

/// A reducer that folds intrinsics all the way down, for the gates that must *evaluate* a rebuilt term rather than inspect its shape. `reduce_intrinsic` already reduces its own operands through this seam, so one pass suffices for the terms below.
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

    /// Unbudgeted, for [`Inert`]'s reason.
    fn spend(&mut self, _cost: Cost) -> Result<(), ReduceError> {
        Ok(())
    }
}

/// [`Folding`] under a budget, for the gates whose subject is a *charge* rather than a value.
pub(super) struct Budgeted {
    pub(super) remaining: u64,
}

impl Reducer for Budgeted {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        match &*term {
            Subterm::Intrinsic(intrinsic) => Ok(reduce_intrinsic(self, intrinsic)?.into()),
            _ => Ok(term),
        }
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        self.reduce(term)
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        if cost.is_refused() {
            return Err(ReduceError::exhausted(self.remaining, cost));
        }

        match self.remaining.checked_sub(cost.get()) {
            Some(remaining) => {
                self.remaining = remaining;
                Ok(())
            }
            None => Err(ReduceError::exhausted(self.remaining, cost)),
        }
    }
}

/// What reducing `term` costs a fresh [`Budgeted`], for the gates whose subject is a charge.
pub(super) fn charged(term: Term) -> u64 {
    const AMPLE: u64 = 1_000_000_000;
    let mut reducer = Budgeted { remaining: AMPLE };
    reducer.reduce(term).expect("the subject reduces");

    AMPLE - reducer.remaining
}

/// A literal run of `n` naturals.
pub(super) fn run_of(n: usize) -> Term {
    Term::intrinsic(Intrinsic::List {
        element: nat_type(),
        items: (0..n).map(|i| lit(i as u32)).collect(),
    })
}

pub(super) fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// A stand-in for a discharged bound. Reduction never inspects one — proof irrelevance makes its value unobservable, and these tests are about the fold laws rather than the obligation — so every bounded operation below states it with the same name.
pub(super) fn qed() -> Term {
    symbol(9_999, "qed")
}

pub(super) fn symbol(index: u32, hint: &'static str) -> Term {
    Term::free_var(&Free::local(index, Some(hint)))
}

pub(super) fn to_nat_of(term: Term) -> Term {
    Term::intrinsic(Intrinsic::ByteToNat(term))
}

pub(super) fn scaled(coefficient: u32, factor: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_mul(lit(coefficient), factor))
}

pub(super) fn plus(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_add(left, right))
}

pub(super) fn fold(term: Term) -> Term {
    Folding.reduce(term).expect("reduces")
}

/// `term` with the free variable `binder` replaced by `value`: close over the binder, then open at the value. Comparing *values* rather than shapes is what this gate needs — `4 · (3 · x)` and `12 · x` are the same number, and reduction does not re-associate nested literal factors.
pub(super) fn at(term: Term, binder: &Free, value: Term) -> Term {
    Scope::close(One, &[binder], term).open(&[&value])
}

pub(super) fn as_nat(term: &Term) -> Nat {
    match &**term {
        Subterm::Intrinsic(Intrinsic::Nat(nat)) => nat.clone(),
        _ => unreachable!("a folded `Nat` carrying a successor floor"),
    }
}

pub(super) fn run_bytes(run: &[u8]) -> Term {
    Term::intrinsic(Intrinsic::Bin(
        Grain::X,
        PackedBin::from_bytes(run.to_vec()),
    ))
}

/// The same five bytes, spelled four ways: whole, split once, split twice, and left-nested the way an accumulation builds one.
pub(super) fn groupings(whole: &[u8]) -> Vec<Term> {
    let nest = |left: Term, right: Term| {
        Term::intrinsic(Intrinsic::BinConcat {
            grain: Grain::X,
            operands: vec![left, right],
        })
    };

    vec![
        run_bytes(whole),
        nest(run_bytes(&whole[..2]), run_bytes(&whole[2..])),
        Term::intrinsic(Intrinsic::BinConcat {
            grain: Grain::X,
            operands: vec![
                run_bytes(&whole[..1]),
                run_bytes(&whole[1..3]),
                run_bytes(&whole[3..]),
            ],
        }),
        nest(
            nest(run_bytes(&whole[..1]), run_bytes(&whole[1..2])),
            run_bytes(&whole[2..]),
        ),
    ]
}

/// The `List` twin of the three tests above, in one: its carrier flattens element vectors where `Bin` copies packed bytes, and the walks are written separately, so agreement on one is not agreement on the other. Elements are symbols, compared syntactically — which is all the property needs, since regrouping never changes an element, only which run it sits in.
pub(super) fn list_of(elements: &[u32]) -> Term {
    Term::intrinsic(Intrinsic::List {
        element: symbol(1000, "T"),
        items: elements.iter().map(|n| symbol(*n, "e")).collect(),
    })
}

/// A literal `List` run of `Nat` literals — [`list_of`]'s twin for the grids below, whose ground truth needs closed elements rather than symbols.
pub(super) fn nat_list(elements: &[u32]) -> Term {
    Term::intrinsic(Intrinsic::List {
        element: symbol(1000, "T"),
        items: elements.iter().map(|n| lit(*n)).collect(),
    })
}

/// Fold a closed `Bin` term to its literal run — the ground truth the peel grids compare, so it must land on a literal or the instantiation was not closed.
pub(super) fn bin_value(term: Term) -> Term {
    let folded = fold(term);
    assert!(
        matches!(
            &*folded,
            Subterm::Intrinsic(Intrinsic::Bin(Grain::X, _))
                | Subterm::Intrinsic(Intrinsic::Byte(_))
        ),
        "a closed instantiation folds to a literal, got {folded:?}",
    );
    folded
}

/// Fold a closed `List` term to its element run, folding each element too — a fused literal keeps its elements as written, so `[2 + 3]` and `[5]` are one value and must compare as one.
pub(super) fn list_value(term: Term) -> Vec<Term> {
    let folded = fold(term);
    match &*folded {
        Subterm::Intrinsic(Intrinsic::List {
            element: _,
            items: elems,
        }) => elems.iter().map(|elem| fold(elem.clone())).collect(),
        other => unreachable!("a closed List folds to a literal run, got {other:?}"),
    }
}

/// Fold a closed term to the value the commutation grid compares: a `List` literal folds its elements too, since a fused run keeps them as written, and every other carrier's closed fold is already the value.
pub(super) fn closed_value(term: Term) -> Term {
    let folded = fold(term);
    match &*folded {
        Subterm::Intrinsic(Intrinsic::List {
            element: elem,
            items: elems,
        }) => Term::intrinsic(Intrinsic::List {
            element: elem.clone(),
            items: elems.iter().map(|element| fold(element.clone())).collect(),
        }),
        _ => folded,
    }
}
