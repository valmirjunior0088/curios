//! The free-monoid peel shared by inversion (`invert`) and conversion
//! (`convert`). A primitive whose values are a literal run of generators over a
//! symbolic tail — a `Nat` count, a `Bin` byte run, an `Arr` element run —
//! reduces two values by stripping their longest common literal head; the
//! residual tails go back to the caller's own recursion. `Bln`/`Int` are the
//! degenerate, zero-generator spines. The point of the seam: a new instance is
//! one `peel_prim` arm and nothing else — the drivers, the `Peel` vocabulary,
//! and the termination argument are shared, and `Bin`/`Arr` further share the
//! `peel_prefix` step itself (they differ only in element type and whether a
//! stalled literal head is a clash).

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
        // `Bin`/`Arr` are the free monoids on their bytes/elements: peel the
        // longest common prefix (each returns `None` for the other's shapes).
        _ => peel_bin(left, right).or_else(|| peel_arr(left, right)),
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

/// One segment of a flattened free-monoid value: either a run of consecutive
/// literal elements — concrete bytes (`Bin`) or terms (`Arr`) — or an opaque
/// symbolic chunk (a variable, an append, a slice: anything whose contents and
/// length are not statically known). A value is a sequence of these, and the
/// concatenation primitive is their juxtaposition; flattening normalises the
/// monoid laws (associativity, the empty identity, re-segmented literal runs) so
/// two definitionally equal values decompose to the same list.
enum Atom<E> {
    Lit(Vec<E>),
    Sym(Term),
}

/// The one free-monoid step `peel_bin` and `peel_arr` share: strip the longest
/// common prefix the two segment lists *certainly* agree on — literal elements
/// matched one-for-one and whole symbolic chunks that are syntactically identical
/// — leaving each list at its residual tail. Reports whether anything was peeled,
/// so the caller knows it made progress (and a `Continue` cannot loop). Literal
/// elements compare by `==`: exact for `Bin`'s bytes, *syntactic* for `Arr`'s
/// terms — hence `peel_arr` must not read a stalled literal head as a clash.
fn peel_prefix<E: PartialEq>(left: &mut VecDeque<Atom<E>>, right: &mut VecDeque<Atom<E>>) -> bool {
    let mut peeled = false;

    loop {
        match (left.front(), right.front()) {
            (Some(Atom::Lit(a)), Some(Atom::Lit(b))) => {
                let common = a.iter().zip(b).take_while(|(x, y)| x == y).count();
                if common == 0 {
                    break;
                }
                peeled = true;
                consume(left, common);
                consume(right, common);
            }
            (Some(Atom::Sym(x)), Some(Atom::Sym(y))) if x == y => {
                peeled = true;
                left.pop_front();
                right.pop_front();
            }
            _ => break,
        }
    }

    peeled
}

/// Drop `count` leading elements off the head run, removing the run outright when
/// it is exactly consumed. `count` never exceeds the run's length.
fn consume<E>(atoms: &mut VecDeque<Atom<E>>, count: usize) {
    match atoms.front_mut() {
        Some(Atom::Lit(run)) if run.len() == count => {
            atoms.pop_front();
        }
        Some(Atom::Lit(run)) => {
            run.drain(0..count);
        }
        _ => unreachable!("consume called on a non-literal head"),
    }
}

/// Append an atom, keeping the list normalised: empty runs vanish (the identity)
/// and a run abutting another run merges into it (so no two `Lit`s are adjacent).
fn push<E>(out: &mut Vec<Atom<E>>, atom: Atom<E>) {
    match atom {
        Atom::Lit(run) if run.is_empty() => {}
        Atom::Lit(run) => match out.last_mut() {
            Some(Atom::Lit(head)) => head.extend(run),
            _ => out.push(Atom::Lit(run)),
        },
        Atom::Sym(term) => out.push(Atom::Sym(term)),
    }
}

/// One side peeled down to the empty identity while the other did not: a leftover
/// literal run is a definite length mismatch (`Clash`); a leftover symbolic chunk
/// might itself be empty, so its emptiness is undecidable (`Stuck`).
fn against_identity<E>(atom: &Atom<E>) -> Peel {
    match atom {
        Atom::Lit(_) => Peel::Clash,
        Atom::Sym(_) => Peel::Stuck,
    }
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

    let mut left = bin_atoms(left);
    let mut right = bin_atoms(right);
    let peeled = peel_prefix(&mut left, &mut right);

    Some(match (left.front(), right.front()) {
        (None, None) => Peel::Equal,
        (None, Some(atom)) | (Some(atom), None) => against_identity(atom),
        // Both still lead with a concrete run: the loop only stops here once their
        // first bytes disagree, and bytes are decided — so the values are unequal.
        (Some(Atom::Lit(_)), Some(Atom::Lit(_))) => Peel::Clash,
        // A literal facing a symbolic chunk, or two unlike symbolic chunks. If a
        // common prefix was peeled the residual tails go back to the caller;
        // otherwise nothing here is decidable by peeling.
        _ => match peeled {
            true => Peel::Continue(reassemble_bin(left), reassemble_bin(right)),
            false => Peel::Stuck,
        },
    })
}

/// `Arr` is the free monoid on its elements — the same peel as `peel_bin`, with
/// two differences. Its literal runs hold *terms*, not decided bytes, so two
/// leading runs whose heads disagree are NOT a clash (the elements may still be
/// convertible): the peel defers, and the caller's structural element-wise
/// comparison settles it. And its concatenation carries an element type, recovered
/// here to rebuild a residual `ArrConcat`. A leftover literal run against the empty
/// identity (`[x] ~ []`) is still a definite length clash, as in `peel_bin`.
pub fn peel_arr(left: &Prim, right: &Prim) -> Option<Peel> {
    if !arr_valued(left) || !arr_valued(right) {
        return None;
    }

    // The element type for a rebuilt `ArrConcat` residual — present whenever a side
    // is itself an `ArrConcat`, which is exactly when a multi-segment residual (the
    // only thing that rebuilds an `ArrConcat`) can arise.
    let elem = arr_elem(left).or_else(|| arr_elem(right));

    let mut left = arr_atoms(left);
    let mut right = arr_atoms(right);
    let peeled = peel_prefix(&mut left, &mut right);

    Some(match (left.front(), right.front()) {
        (None, None) => Peel::Equal,
        (None, Some(atom)) | (Some(atom), None) => against_identity(atom),
        // Two leading literal runs whose heads differ, a literal facing a symbolic
        // chunk, or two unlike chunks — none decidable by peeling (an element
        // disagreement is syntactic, not semantic). Hand back any peeled residual.
        _ => match peeled {
            true => Peel::Continue(
                reassemble_arr(left, elem.clone()),
                reassemble_arr(right, elem),
            ),
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

/// The `Arr` analogue of [`bin_valued`]: only `Arr` and `ArrConcat` carry the
/// monoid structure (`ArrSlice`/`ArrAppend`/`ArrFlatten` stay opaque chunks).
fn arr_valued(prim: &Prim) -> bool {
    matches!(prim, Prim::Arr(_) | Prim::ArrConcat(_, _))
}

/// The element type of an `ArrConcat`, for rebuilding residuals (`None` for a bare
/// `Arr` literal, which never needs it — it rebuilds as a single run).
fn arr_elem(prim: &Prim) -> Option<Term> {
    match prim {
        Prim::ArrConcat(elem, _) => Some(elem.clone()),
        _ => None,
    }
}

/// Flatten a `Bin` value to its segment list, normalising the monoid laws: nested
/// `BinConcat`s splice in, empty runs drop out, adjacent runs merge.
fn bin_atoms(prim: &Prim) -> VecDeque<Atom<u8>> {
    let mut out = Vec::new();
    bin_collect_prim(prim, &mut out);
    out.into()
}

fn bin_collect_prim(prim: &Prim, out: &mut Vec<Atom<u8>>) {
    match prim {
        Prim::Bin(bytes) => push(out, Atom::Lit(bytes.clone())),
        Prim::BinConcat(operands) => operands.iter().for_each(|op| bin_collect_term(op, out)),
        other => push(out, Atom::Sym(Term::prim(other.clone()))),
    }
}

fn bin_collect_term(term: &Term, out: &mut Vec<Atom<u8>>) {
    match &**term {
        Subterm::Prim(prim) => bin_collect_prim(prim, out),
        _ => push(out, Atom::Sym(term.clone())),
    }
}

/// Flatten an `Arr` value to its segment list — the [`bin_atoms`] decomposition
/// over element terms rather than bytes.
fn arr_atoms(prim: &Prim) -> VecDeque<Atom<Term>> {
    let mut out = Vec::new();
    arr_collect_prim(prim, &mut out);
    out.into()
}

fn arr_collect_prim(prim: &Prim, out: &mut Vec<Atom<Term>>) {
    match prim {
        Prim::Arr(elems) => push(out, Atom::Lit(elems.clone())),
        Prim::ArrConcat(_, operands) => operands.iter().for_each(|op| arr_collect_term(op, out)),
        other => push(out, Atom::Sym(Term::prim(other.clone()))),
    }
}

fn arr_collect_term(term: &Term, out: &mut Vec<Atom<Term>>) {
    match &**term {
        Subterm::Prim(prim) => arr_collect_prim(prim, out),
        _ => push(out, Atom::Sym(term.clone())),
    }
}

/// Rebuild a `Bin` term from a residual segment list: a lone run is a `Bin`
/// literal, a lone symbolic chunk is itself (so the inverter sees the bare binder
/// it must solve), and a mixture is their `BinConcat`.
fn reassemble_bin(atoms: VecDeque<Atom<u8>>) -> Term {
    let into_term = |atom| match atom {
        Atom::Lit(bytes) => Term::prim(Prim::Bin(bytes)),
        Atom::Sym(term) => term,
    };

    match atoms.len() {
        1 => into_term(atoms.into_iter().next().unwrap()),
        _ => Term::prim(Prim::BinConcat(atoms.into_iter().map(into_term).collect())),
    }
}

/// Rebuild an `Arr` term from a residual segment list — [`reassemble_bin`] over
/// element runs, restoring the element type a multi-segment `ArrConcat` carries.
/// `elem` is `Some` whenever the residual has more than one segment (such a
/// residual can only come from an input `ArrConcat`).
fn reassemble_arr(atoms: VecDeque<Atom<Term>>, elem: Option<Term>) -> Term {
    let into_term = |atom| match atom {
        Atom::Lit(elems) => Term::prim(Prim::Arr(elems)),
        Atom::Sym(term) => term,
    };

    match atoms.len() {
        1 => into_term(atoms.into_iter().next().unwrap()),
        _ => {
            let elem = elem.expect("a multi-segment Arr residual implies an ArrConcat operand");
            Term::prim(Prim::ArrConcat(elem, atoms.into_iter().map(into_term).collect()))
        }
    }
}
