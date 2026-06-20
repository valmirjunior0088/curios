//! The free-monoid peel shared by inversion (`invert`) and conversion
//! (`convert`). A primitive whose values are a literal run of generators over a
//! symbolic tail — a `Nat` count today, a `Bin` byte run or `Arr` element run
//! later — reduces two values by stripping their longest common literal head;
//! the residual tails go back to the caller's own recursion. `Bln`/`Int` are
//! the degenerate, zero-generator spines. The point of the seam: a new instance
//! is one `peel_prim` arm and nothing else — the drivers, the `Peel`
//! vocabulary, and the termination argument are shared.

use {
    super::{Nat, Prim, Subterm, Term},
    num_traits::Zero,
    std::{cmp::Ordering, collections::VecDeque},
};

/// One step of peeling two free-monoid values. Each caller maps it into its own
/// vocabulary: `invert` to `Step::{Ok, Clash, Refuse}`, `convert` to a `bool`
/// with the residual enqueued.
pub enum Peel {
    /// Both sides consumed to the identity — definitionally equal.
    Equal,
    /// A common head peeled off; compare these residual tails next.
    Continue(Term, Term),
    /// Literal heads differ, or a positive head meets the identity — unequal.
    Clash,
    /// Undecidable by peeling (a symbolic-length head); the caller falls back.
    /// Unreachable for `Nat` — it is the seam the harder primitives plug into.
    Stuck,
}

/// Classify a reduced primitive pair. `None` means the pair is not a matched
/// spine-primitive, so the caller keeps its own handling; `Some` is the peel
/// outcome.
pub fn peel_prim(left: &Prim, right: &Prim) -> Option<Peel> {
    match (left, right) {
        (Prim::Nat(actual), Prim::Nat(target)) => Some(peel_nat(actual, target)),
        // Finite scalars are the degenerate (zero-generator) spines: no tail.
        (Prim::Bln(actual), Prim::Bln(target)) => Some(decide(actual == target)),
        (Prim::Int(actual), Prim::Int(target)) => Some(decide(actual == target)),
        // `Bin` is the free monoid on its bytes: peel the longest common prefix.
        _ => peel_bin(left, right),
    }
}

fn decide(equal: bool) -> Peel {
    match equal {
        true => Peel::Equal,
        false => Peel::Clash,
    }
}

/// `Nat` is the free monoid on one generator: `k + a ~ k' + t` peels the shared
/// successor spine and the leftover rides on the longer side — `2 ~ ?n + 1`
/// becomes `1 ~ ?n`. A leftover positive spine against zero is a definite clash.
/// The `is_zero` guards mirror the inverter's defence against a non-canonical
/// `Succ(0, _)` (which `Nat::new` normalisation never actually produces).
pub fn peel_nat(actual: &Nat, target: &Nat) -> Peel {
    let zero = || Term::prim(Prim::Nat(Nat::Zero));
    let succ = |spine, rest: &Term| Term::prim(Prim::Nat(Nat::Succ(spine, rest.clone())));

    match (actual, target) {
        (Nat::Zero, Nat::Zero) => Peel::Equal,
        (Nat::Zero, Nat::Succ(spine, rest)) => match spine.is_zero() {
            true => Peel::Continue(zero(), rest.clone()),
            false => Peel::Clash,
        },
        (Nat::Succ(spine, rest), Nat::Zero) => match spine.is_zero() {
            true => Peel::Continue(rest.clone(), zero()),
            false => Peel::Clash,
        },
        (Nat::Succ(ka, ra), Nat::Succ(kt, rt)) => match ka.cmp(kt) {
            Ordering::Equal => Peel::Continue(ra.clone(), rt.clone()),
            Ordering::Greater => Peel::Continue(succ(ka - kt, ra), rt.clone()),
            Ordering::Less => Peel::Continue(ra.clone(), succ(kt - ka, rt)),
        },
    }
}

/// One segment of a flattened `Bin` value: either a run of concrete bytes or an
/// opaque symbolic chunk (a variable, a `BinAppend`, a slice — anything whose
/// bytes and length are not statically known). A `Bin` value is a sequence of
/// these, and `BinConcat` is their juxtaposition; flattening normalises away the
/// monoid laws (associativity, the empty-bytestring identity, and re-segmented
/// literal runs) so two definitionally equal values decompose to the same list.
enum Atom {
    Lit(Vec<u8>),
    Sym(Term),
}

/// `Bin` is the free monoid on its bytes. Two values reduce by stripping their
/// longest common prefix — concrete bytes byte-for-byte, and identical symbolic
/// chunks whole — and the residual tails ride back on `Continue` (so the inverter
/// can solve a flex binder forced to equal a leftover suffix, and conversion can
/// enqueue the rest). A definite byte disagreement, or a positive run meeting the
/// empty bytestring, is a `Clash`; a symbolic chunk facing an unlike chunk or the
/// identity is `Stuck` (its length is unknown, so peeling cannot decide). `None`
/// means the pair is not two `Bin` values, so the caller keeps its own handling.
///
/// Prefix-only, mirroring `peel_nat`: a common *suffix* (`x ++ \01 ~ y ++ \01`)
/// is sound to cancel but not yet attempted. Symbolic chunks are matched by
/// syntactic equality, so two convertible-but-unequal chunks (`append(\\, h1)`
/// vs `append(\\, h2)`) are left to the caller's structural comparison rather
/// than decided here.
pub fn peel_bin(left: &Prim, right: &Prim) -> Option<Peel> {
    if !bin_valued(left) || !bin_valued(right) {
        return None;
    }

    let mut left = atoms(left);
    let mut right = atoms(right);
    let mut peeled = false;

    loop {
        match (left.front(), right.front()) {
            (Some(Atom::Lit(a)), Some(Atom::Lit(b))) => {
                let common = a.iter().zip(b).take_while(|(x, y)| x == y).count();
                if common == 0 {
                    break;
                }
                peeled = true;
                consume_bytes(&mut left, common);
                consume_bytes(&mut right, common);
            }
            (Some(Atom::Sym(x)), Some(Atom::Sym(y))) if x == y => {
                peeled = true;
                left.pop_front();
                right.pop_front();
            }
            _ => break,
        }
    }

    Some(match (left.front(), right.front()) {
        (None, None) => Peel::Equal,
        // One side is the identity. A leftover concrete run is a definite length
        // mismatch; a leftover symbolic chunk might itself be empty — undecidable.
        (None, Some(atom)) | (Some(atom), None) => match atom {
            Atom::Lit(_) => Peel::Clash,
            Atom::Sym(_) => Peel::Stuck,
        },
        // Both still lead with a concrete run: the loop only stops here once their
        // first bytes disagree, so the values are unequal.
        (Some(Atom::Lit(_)), Some(Atom::Lit(_))) => Peel::Clash,
        // A literal facing a symbolic chunk, or two unlike symbolic chunks. If a
        // common prefix was peeled the residual tails go back to the caller;
        // otherwise nothing here is decidable by peeling.
        _ => match peeled {
            true => Peel::Continue(reassemble(left), reassemble(right)),
            false => Peel::Stuck,
        },
    })
}

/// The `Bin`-valued primitives `peel_bin` decomposes. `Bin` and `BinConcat` carry
/// the monoid structure; the rest (`BinAppend`, `BinSlice`, `BinFlatten`) only
/// ever appear as opaque symbolic chunks, so they are left to the caller's own
/// (structural) comparison rather than routed through the peel.
fn bin_valued(prim: &Prim) -> bool {
    matches!(prim, Prim::Bin(_) | Prim::BinConcat(_))
}

/// Flatten a `Bin` value to its segment list, normalising the monoid laws:
/// nested `BinConcat`s splice in, empty runs drop out, and adjacent concrete runs
/// merge (so no two `Lit`s are ever adjacent).
fn atoms(prim: &Prim) -> VecDeque<Atom> {
    let mut out = Vec::new();
    collect_prim(prim, &mut out);
    out.into()
}

fn collect_prim(prim: &Prim, out: &mut Vec<Atom>) {
    match prim {
        Prim::Bin(bytes) => push(out, Atom::Lit(bytes.clone())),
        Prim::BinConcat(operands) => operands.iter().for_each(|op| collect_term(op, out)),
        other => push(out, Atom::Sym(Term::prim(other.clone()))),
    }
}

fn collect_term(term: &Term, out: &mut Vec<Atom>) {
    match &**term {
        Subterm::Prim(prim) => collect_prim(prim, out),
        _ => push(out, Atom::Sym(term.clone())),
    }
}

/// Append an atom, keeping the list normalised: empty runs vanish (identity) and a
/// run abutting another run merges into it (associativity of the byte sequence).
fn push(out: &mut Vec<Atom>, atom: Atom) {
    match atom {
        Atom::Lit(bytes) if bytes.is_empty() => {}
        Atom::Lit(bytes) => match out.last_mut() {
            Some(Atom::Lit(run)) => run.extend(bytes),
            _ => out.push(Atom::Lit(bytes)),
        },
        Atom::Sym(term) => out.push(Atom::Sym(term)),
    }
}

/// Drop `count` bytes off the leading concrete run, removing the run outright when
/// it is exactly consumed. `count` never exceeds the run's length.
fn consume_bytes(atoms: &mut VecDeque<Atom>, count: usize) {
    match atoms.front_mut() {
        Some(Atom::Lit(run)) if run.len() == count => {
            atoms.pop_front();
        }
        Some(Atom::Lit(run)) => {
            run.drain(0..count);
        }
        _ => unreachable!("consume_bytes called on a non-literal head"),
    }
}

/// Rebuild a `Bin` term from a residual segment list: a lone run is a `Bin`
/// literal, a lone symbolic chunk is itself (so the inverter sees the bare binder
/// it must solve), and a mixture is their `BinConcat`.
fn reassemble(atoms: VecDeque<Atom>) -> Term {
    let into_term = |atom| match atom {
        Atom::Lit(bytes) => Term::prim(Prim::Bin(bytes)),
        Atom::Sym(term) => term,
    };

    match atoms.len() {
        1 => into_term(atoms.into_iter().next().unwrap()),
        _ => Term::prim(Prim::BinConcat(atoms.into_iter().map(into_term).collect())),
    }
}
