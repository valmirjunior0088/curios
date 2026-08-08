//! The free-monoid peel shared by inversion (`invert`) and conversion (`convert`). An intrinsic whose values are a literal run of generators over a symbolic tail — a `Nat` count, a `Bin` byte run, an `Lst` element run — reduces two values by stripping their longest common literal head; the residual tails go back to the caller's own recursion. `Bool`/`Int` are the degenerate, zero-generator spines. The point of the seam: a new instance is one `peel_intrinsic` arm and nothing else — the drivers, the `Peel` vocabulary, and the termination argument are shared, and `Bin`/`Lst` further share the `peel_prefix` step itself (they differ only in element type and whether a stalled literal head is a clash).

use {
    super::{Intrinsic, Nat, Subterm, Term},
    curios_base::{Grain, PackedBin},
    num_traits::Zero,
    std::{cmp::Ordering, collections::VecDeque},
};

/// One step of peeling two free-monoid values. Each caller maps it into its own vocabulary: `invert` to `Step::{Ok, Clash, Refuse}`, `convert` to a `bool` with the residual enqueued.
pub enum Peel {
    /// Both sides consumed to the identity — definitionally equal.
    Equal,
    /// A common head peeled off; compare these residual tails next.
    Continue(Term, Term),
    /// Literal heads differ, or a positive head meets the identity — unequal.
    Clash,
    /// Undecidable by peeling (a symbolic-length head); the caller falls back. Unreachable for `Nat` — it is the seam the harder intrinsics plug into.
    Stuck,
}

/// Classify a reduced intrinsic pair. `None` means the pair is not a matched spine-intrinsic, so the caller keeps its own handling; `Some` is the peel outcome.
pub fn peel_intrinsic(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    match (left, right) {
        (Intrinsic::Nat(actual), Intrinsic::Nat(target)) => Some(peel_nat(actual, target)),
        // Finite scalars are the degenerate (zero-generator) spines: no tail.
        (Intrinsic::Bool(actual), Intrinsic::Bool(target)) => Some(decide(actual == target)),
        (Intrinsic::Int(actual), Intrinsic::Int(target)) => Some(decide(actual == target)),
        // `Bin`/`Lst` are the free monoids on their bytes/elements: peel the longest common prefix (each returns `None` for the other's shapes).
        _ => peel_bin(left, right).or_else(|| peel_lst(left, right)),
    }
}

fn decide(equal: bool) -> Peel {
    match equal {
        true => Peel::Equal,
        false => Peel::Clash,
    }
}

/// `Nat` is the free monoid on one generator: `k + a ~ k' + t` peels the shared successor spine and the leftover rides on the longer side — `2 ~ ?n + 1` becomes `1 ~ ?n`. A leftover positive spine against zero is a definite clash. The `is_zero` guards mirror the inverter's defence against a non-canonical `Succ(0, _)` (which `Nat::new` normalisation never actually produces).
pub fn peel_nat(actual: &Nat, target: &Nat) -> Peel {
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    let succ = |spine, rest: &Term| Term::intrinsic(Intrinsic::Nat(Nat::Succ(spine, rest.clone())));

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

/// One segment of a flattened free-monoid value: a run of consecutive literal elements — concrete bytes (`Bin`) or terms (`Lst`) — a `Window` into a base value (a `Bin/slice(base, lo, hi)`: contents symbolic, but length `hi - lo` statically known as a `Nat` term), or an opaque symbolic chunk (a variable, an append: anything whose contents *and* length are unknown). A value is a sequence of these, and the concatenation intrinsic is their juxtaposition; flattening normalises the monoid laws — associativity, the empty identity, re-segmented literal runs, and fused adjacent windows of one base (`slice(b, s, m) ++ slice(b, m, e) = slice(b, s, e)`) — so two definitionally equal values decompose to the same list.
enum Atom<E> {
    Literal(Vec<E>),
    Window { base: Term, lo: Term, hi: Term },
    Symbolic(Term),
}

/// The one free-monoid step `peel_bin` and `peel_lst` share: strip the longest common prefix the two segment lists *certainly* agree on — literal elements matched one-for-one and whole symbolic chunks that are syntactically identical — leaving each list at its residual tail. Reports whether anything was peeled, so the caller knows it made progress (and a `Continue` cannot loop). Literal elements compare by `==`: exact for `Bin`'s bytes, *syntactic* for `Lst`'s terms — hence `peel_lst` must not read a stalled literal head as a clash.
fn peel_prefix<E: PartialEq>(left: &mut VecDeque<Atom<E>>, right: &mut VecDeque<Atom<E>>) -> bool {
    let mut peeled = false;

    loop {
        match (left.front(), right.front()) {
            (Some(Atom::Literal(a)), Some(Atom::Literal(b))) => {
                let common = a.iter().zip(b).take_while(|(x, y)| x == y).count();
                if common == 0 {
                    break;
                }
                peeled = true;
                consume(left, common);
                consume(right, common);
            }
            (Some(Atom::Symbolic(x)), Some(Atom::Symbolic(y))) if x == y => {
                peeled = true;
                left.pop_front();
                right.pop_front();
            }
            // Two windows into the same base over the same span are equal whole. A shared base/`lo` with differing `hi` (one window extends past the other) could peel too, but that needs ordering the symbolic bounds, so it is left to defer rather than decided here.
            (
                Some(Atom::Window {
                    base: b1,
                    lo: l1,
                    hi: h1,
                }),
                Some(Atom::Window {
                    base: b2,
                    lo: l2,
                    hi: h2,
                }),
            ) if b1 == b2 && l1 == l2 && h1 == h2 => {
                peeled = true;
                left.pop_front();
                right.pop_front();
            }
            _ => break,
        }
    }

    peeled
}

/// Drop `count` leading elements off the head run, removing the run outright when it is exactly consumed. `count` never exceeds the run's length.
fn consume<E>(atoms: &mut VecDeque<Atom<E>>, count: usize) {
    match atoms.front_mut() {
        Some(Atom::Literal(run)) if run.len() == count => {
            atoms.pop_front();
        }
        Some(Atom::Literal(run)) => {
            run.drain(0..count);
        }
        _ => unreachable!("consume called on a non-literal head"),
    }
}

/// Append an atom, keeping the list normalised: empty runs vanish (the identity), a run abutting another run merges into it (so no two literal runs are adjacent), an empty window (`slice(b, i, i)`) vanishes like an empty run, and a window abutting another window of the same base across a shared seam fuses into one (`slice(b, s, m) ++ slice(b, m, e) = slice(b, s, e)`) — the monoid law that makes `Bin/slice` a first-class spine citizen rather than an opaque chunk.
fn push<E>(out: &mut Vec<Atom<E>>, atom: Atom<E>) {
    match atom {
        Atom::Literal(run) if run.is_empty() => {}
        Atom::Literal(run) => match out.last_mut() {
            Some(Atom::Literal(head)) => head.extend(run),
            _ => out.push(Atom::Literal(run)),
        },
        // An empty window is the identity: equal bounds slice nothing.
        Atom::Window { lo, hi, .. } if lo == hi => {}
        Atom::Window { base, lo, hi } => {
            // Fuse with a preceding window of the same base whose `hi` meets this window's `lo` at a shared seam; the fused span carries through, so a run of touching windows collapses left-to-right to one.
            let fuses = matches!(
                out.last(),
                Some(Atom::Window { base: prev, hi: seam, .. }) if *prev == base && *seam == lo,
            );
            match fuses {
                true => {
                    if let Some(Atom::Window { hi: seam, .. }) = out.last_mut() {
                        *seam = hi;
                    }
                }
                false => out.push(Atom::Window { base, lo, hi }),
            }
        }
        Atom::Symbolic(term) => out.push(Atom::Symbolic(term)),
    }
}

/// One side peeled down to the empty identity while the other did not: a leftover literal run is a definite length mismatch (`Clash`); a leftover symbolic chunk or window might itself be empty (its length is a symbolic `hi - lo`), so its emptiness is undecidable (`Stuck`).
fn against_identity<E>(atom: &Atom<E>) -> Peel {
    match atom {
        Atom::Literal(_) => Peel::Clash,
        Atom::Window { .. } | Atom::Symbolic(_) => Peel::Stuck,
    }
}

/// `Bin` is the free monoid on its bytes. Two values reduce by stripping their longest common prefix — concrete bytes byte-for-byte, identical symbolic chunks whole, and equal slice windows whole (after `bin_atoms` has fused adjacent windows of one base) — and the residual tails ride back on `Continue` (so the inverter can solve a flex binder forced to equal a leftover suffix, and conversion can enqueue the rest). A definite byte disagreement, or a positive run meeting the empty bytestring, is a `Clash`; a symbolic chunk or window facing an unlike one or the identity is `Stuck` (its length is unknown, so peeling cannot decide). `None` means the pair is not two `Bin` values, so the caller keeps its own handling.
///
/// Prefix-only, mirroring `peel_nat`: a common *suffix* (`x ++ x[\01] ~ y ++ x[\01]`) is sound to cancel but not yet attempted. Symbolic chunks and windows are matched by syntactic equality, so two convertible-but-unequal chunks (`append(x[], h1)` vs `append(x[], h2)`) — or two windows whose bounds differ only up to arithmetic — are left to the caller's structural comparison rather than decided here.
pub fn peel_bin(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    let grain = bin_grain(left)?;
    if bin_grain(right) != Some(grain) {
        return None;
    }

    let mut left = bin_atoms(grain, left);
    let mut right = bin_atoms(grain, right);
    let peeled = peel_prefix(&mut left, &mut right);

    Some(match (left.front(), right.front()) {
        (None, None) => Peel::Equal,
        (None, Some(atom)) | (Some(atom), None) => against_identity(atom),
        // Both still lead with a concrete run: the loop only stops here once their first bytes disagree, and bytes are decided — so the values are unequal.
        (Some(Atom::Literal(_)), Some(Atom::Literal(_))) => Peel::Clash,
        // A literal facing a symbolic chunk, or two unlike symbolic chunks. If a common prefix was peeled the residual tails go back to the caller; otherwise nothing here is decidable by peeling.
        _ => match peeled {
            true => Peel::Continue(reassemble_bin(grain, left), reassemble_bin(grain, right)),
            false => Peel::Stuck,
        },
    })
}

/// `Lst` is the free monoid on its elements — the same peel as `peel_bin`, with two differences. Its literal runs hold *terms*, not decided bytes, so two leading runs whose heads disagree are NOT a clash (the elements may still be convertible): the peel defers, and the caller's structural element-wise comparison settles it. And its concatenation carries an element type, recovered here to rebuild a residual `LstConcat`. A leftover literal run against the empty identity (`[x] ~ []`) is still a definite length clash, as in `peel_bin`.
pub fn peel_lst(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    if !lst_valued(left) || !lst_valued(right) {
        return None;
    }

    // The element type for a rebuilt `LstConcat` residual — present whenever a side is itself an `LstConcat`, which is exactly when a multi-segment residual (the only thing that rebuilds an `LstConcat`) can arise.
    let elem = lst_elem(left).or_else(|| lst_elem(right));

    let mut left = lst_atoms(left);
    let mut right = lst_atoms(right);
    let peeled = peel_prefix(&mut left, &mut right);

    Some(match (left.front(), right.front()) {
        (None, None) => Peel::Equal,
        (None, Some(atom)) | (Some(atom), None) => against_identity(atom),
        // Two leading literal runs whose heads differ, a literal facing a symbolic chunk, or two unlike chunks — none decidable by peeling (an element disagreement is syntactic, not semantic). Hand back any peeled residual.
        _ => match peeled {
            true => Peel::Continue(
                reassemble_lst(left, elem.clone()),
                reassemble_lst(right, elem),
            ),
            false => Peel::Stuck,
        },
    })
}

/// The `Bin`-valued intrinsics `peel_bin` decomposes. `Bin` and `BinConcat` carry the monoid's literals and juxtaposition; `BinSlice` rides in as a measured `Window` (a length-`hi - lo` chunk whose contents are symbolic), so adjacent slices of one base fuse and equal slices cancel; `BinAppend` rides in as its base followed by the appended byte. Any other producer stays an opaque symbolic chunk left to the caller's own (structural) comparison.
fn bin_grain(intrinsic: &Intrinsic) -> Option<Grain> {
    match intrinsic {
        Intrinsic::Bin(grain, _)
        | Intrinsic::BinConcat(grain, _)
        | Intrinsic::BinSlice(grain, ..)
        | Intrinsic::BinAppend(grain, ..) => Some(*grain),
        _ => None,
    }
}

/// The concrete byte an already-reduced `Bin/append` operand carries, taken mod 256 (matching the runtime's packed store), or `None` for a symbolic byte.
fn bin_atom(grain: Grain, term: &Term) -> Option<u8> {
    match (grain, &**term) {
        (Grain::B, Subterm::Intrinsic(Intrinsic::Bool(bit))) => Some(u8::from(*bit)),
        (Grain::X, Subterm::Intrinsic(Intrinsic::Byte(byte))) => Some(*byte),
        _ => None,
    }
}

/// The `Lst` analogue of [`bin_grain`]: `Lst` and `LstConcat` carry the monoid's literals and juxtaposition, `LstSlice` rides in as a measured `Window` (like `BinSlice`), and `LstAppend` rides in as its base followed by a length-1 literal run — so `append(xs, e) ≡ concat(xs, single(e))`. Any other producer stays an opaque chunk left to the caller's comparison.
fn lst_valued(intrinsic: &Intrinsic) -> bool {
    matches!(
        intrinsic,
        Intrinsic::Lst(..)
            | Intrinsic::LstConcat(_, _)
            | Intrinsic::LstSlice(..)
            | Intrinsic::LstAppend(..)
    )
}

/// The element type carried by an `LstConcat`, `LstSlice`, or `LstAppend`, for rebuilding residuals (every atom of an `Lst(T)` value shares `T`, so one suffices for the whole list). `None` for a bare `Lst` literal, which rebuilds as a single run that never needs it.
fn lst_elem(intrinsic: &Intrinsic) -> Option<Term> {
    match intrinsic {
        Intrinsic::Lst(elem, _)
        | Intrinsic::LstConcat(elem, _)
        | Intrinsic::LstSlice(elem, ..)
        | Intrinsic::LstAppend(elem, ..) => Some(elem.clone()),
        _ => None,
    }
}

/// Flatten a `Bin` value to its segment list, normalising the monoid laws: nested `BinConcat`s splice in, empty runs drop out, adjacent runs merge.
fn bin_atoms(grain: Grain, intrinsic: &Intrinsic) -> VecDeque<Atom<u8>> {
    let mut out = Vec::new();
    bin_collect_intrinsic(grain, intrinsic, &mut out);
    out.into()
}

fn bin_collect_intrinsic(grain: Grain, intrinsic: &Intrinsic, out: &mut Vec<Atom<u8>>) {
    match intrinsic {
        Intrinsic::Bin(found, value) if *found == grain => push(
            out,
            Atom::Literal(match grain {
                Grain::B => (0..value.bit_length())
                    .map(|index| u8::from(value.bit(index).unwrap()))
                    .collect(),
                Grain::X => value.to_bytes().unwrap(),
            }),
        ),
        Intrinsic::BinConcat(found, operands) if *found == grain => operands
            .iter()
            .for_each(|op| bin_collect_term(grain, op, out)),
        Intrinsic::BinSlice(found, base, lo, hi) if *found == grain => push(
            out,
            Atom::Window {
                base: base.clone(),
                lo: lo.clone(),
                hi: hi.clone(),
            },
        ),
        // `append(base, b) = base ++ [b]`: decode the base, then the appended byte. A concrete byte is a length-1 literal run (so it merges with an abutting run and unifies with `concat(base, \b)`); a symbolic byte is the canonical one-byte chunk `append(x[], b)` — opaque, so its emptiness stays undecidable.
        Intrinsic::BinAppend(found, base, atom) if *found == grain => {
            bin_collect_term(grain, base, out);

            match bin_atom(grain, atom) {
                Some(b) => push(out, Atom::Literal(vec![b])),
                None => {
                    let empty = Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::empty()));
                    let chunk = Term::intrinsic(Intrinsic::bin_append(grain, empty, atom.clone()));
                    push(out, Atom::Symbolic(chunk));
                }
            }
        }
        other => push(out, Atom::Symbolic(Term::intrinsic(other.clone()))),
    }
}

fn bin_collect_term(grain: Grain, term: &Term, out: &mut Vec<Atom<u8>>) {
    match &**term {
        Subterm::Intrinsic(intrinsic) => bin_collect_intrinsic(grain, intrinsic, out),
        _ => push(out, Atom::Symbolic(term.clone())),
    }
}

/// Flatten an `Lst` value to its segment list — the [`bin_atoms`] decomposition over element terms rather than bytes.
fn lst_atoms(intrinsic: &Intrinsic) -> VecDeque<Atom<Term>> {
    let mut out = Vec::new();
    lst_collect_intrinsic(intrinsic, &mut out);
    out.into()
}

fn lst_collect_intrinsic(intrinsic: &Intrinsic, out: &mut Vec<Atom<Term>>) {
    match intrinsic {
        Intrinsic::Lst(_, elems) => push(out, Atom::Literal(elems.clone())),
        Intrinsic::LstConcat(_, operands) => {
            operands.iter().for_each(|op| lst_collect_term(op, out))
        }
        Intrinsic::LstSlice(_, base, lo, hi) => push(
            out,
            Atom::Window {
                base: base.clone(),
                lo: lo.clone(),
                hi: hi.clone(),
            },
        ),
        // `append(base, e) = base ++ [e]`: decode the base, then the appended element as a length-1 literal run, so it merges with an abutting run and unifies with `concat(base, single(e))`.
        Intrinsic::LstAppend(_, base, elem) => {
            lst_collect_term(base, out);
            push(out, Atom::Literal(vec![elem.clone()]));
        }
        other => push(out, Atom::Symbolic(Term::intrinsic(other.clone()))),
    }
}

fn lst_collect_term(term: &Term, out: &mut Vec<Atom<Term>>) {
    match &**term {
        Subterm::Intrinsic(intrinsic) => lst_collect_intrinsic(intrinsic, out),
        _ => push(out, Atom::Symbolic(term.clone())),
    }
}

/// Rebuild a `Bin` term from a residual segment list: a lone run is a `Bin` literal, a window is its `BinSlice`, a lone symbolic chunk is itself (so the inverter sees the bare binder it must solve), and a mixture is their `BinConcat`.
fn reassemble_bin(grain: Grain, atoms: VecDeque<Atom<u8>>) -> Term {
    let into_term = |atom| match atom {
        Atom::Literal(atoms) => Term::intrinsic(Intrinsic::Bin(
            grain,
            match grain {
                Grain::B => PackedBin::from_bits(atoms.into_iter().map(|bit| bit != 0)),
                Grain::X => PackedBin::from_bytes(atoms),
            },
        )),
        Atom::Window { base, lo, hi } => Term::intrinsic(Intrinsic::bin_slice(grain, base, lo, hi)),
        Atom::Symbolic(term) => term,
    };

    match atoms.len() {
        1 => into_term(atoms.into_iter().next().unwrap()),
        _ => Term::intrinsic(Intrinsic::BinConcat(
            grain,
            atoms.into_iter().map(into_term).collect(),
        )),
    }
}

/// Rebuild an `Lst` term from a residual segment list — [`reassemble_bin`] over element runs, restoring the element type `Lst`'s `LstConcat`/`LstSlice` carry. `elem` is `Some` whenever the residual has more than one segment or holds a slice window — both can only come from an input carrying the element type.
fn reassemble_lst(atoms: VecDeque<Atom<Term>>, elem: Option<Term>) -> Term {
    fn into_term(atom: Atom<Term>, elem: &Option<Term>) -> Term {
        match atom {
            Atom::Literal(elems) => {
                let elem = elem
                    .clone()
                    .expect("every Lst-valued producer carries its element type");

                Term::intrinsic(Intrinsic::Lst(elem, elems))
            }
            // A slice window rebuilds with the value's element type, threaded through `elem` (every atom of an `Lst(T)` shares `T`).
            Atom::Window { base, lo, hi } => {
                let elem = elem
                    .clone()
                    .expect("an Lst slice window carries its element type via `elem`");

                Term::intrinsic(Intrinsic::lst_slice(elem, base, lo, hi))
            }
            Atom::Symbolic(term) => term,
        }
    }

    match atoms.len() {
        1 => into_term(atoms.into_iter().next().unwrap(), &elem),
        _ => {
            let parts = atoms
                .into_iter()
                .map(|atom| into_term(atom, &elem))
                .collect::<Vec<Term>>();
            let elem = elem.expect("a multi-segment Lst residual implies an LstConcat operand");

            Term::intrinsic(Intrinsic::LstConcat(elem, parts))
        }
    }
}
