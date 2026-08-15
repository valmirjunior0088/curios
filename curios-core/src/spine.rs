//! The free-monoid peel shared by inversion (`invert`) and conversion (`convert`). An intrinsic whose values are a literal run of generators over a symbolic tail — a `Nat` count, a `Bin` byte run, a `List` element run — reduces two values by stripping their longest common literal head; the residual tails go back to the caller's own recursion. `Bool`/`Int` are the degenerate, zero-generator spines. The point of the seam: a new instance is one `peel_intrinsic` arm and nothing else — the drivers, the `Peel` vocabulary, and the termination argument are shared, and `Bin`/`List` further share the `peel_prefix` step itself (they differ only in element type and whether a stalled literal head is a clash).

use {
    super::{Intrinsic, Nat, Subterm, Term},
    curios_utilities::{Grain, PackedBin},
    std::collections::VecDeque,
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
        // `Bin`/`List` are the free monoids on their bytes/elements: peel the longest common prefix (each returns `None` for the other's shapes).
        _ => peel_bin(left, right).or_else(|| peel_list(left, right)),
    }
}

fn decide(equal: bool) -> Peel {
    match equal {
        true => Peel::Equal,
        false => Peel::Clash,
    }
}

/// `Nat` is the free commutative monoid on its symbolic summands: `k + a ~ k' + t` cancels everything the two sides carry in common and the leftover rides on whichever side kept it — `2 ~ ?n + 1` becomes `1 ~ ?n`, and `x + a ~ x + b` becomes `a ~ b`.
///
/// The cancellation itself is `Nat::cancel_common`, which the reduction-side comparison and subtraction folds read too — one law, three readers. This function is only its translation into [`Peel`]: both residuals gone is equality, a surviving positive floor against nothing is a definite clash, and anything else is a smaller pair for the caller to keep comparing. The non-canonical `Succ(0, _)` the inverter used to need its own guard against falls out of `Nat::rebuild` collapsing a zero floor, so no arm states it.
///
/// Cancelling *summands* rather than only the successor spine is what lets a commuted sum decide equal here instead of being handed to a structural comparison that would refuse it. `Peel::Stuck` stays unreachable for `Nat`: a residual pair is always something the caller can go on comparing, so peeling never has to decline.
pub fn peel_nat(actual: &Nat, target: &Nat) -> Peel {
    let lift = |value: &Nat| Term::intrinsic(Intrinsic::Nat(value.clone()));
    let (left, right) = Nat::cancel_common(&lift(actual), &lift(target));

    let floored = |term: &Term| !Nat::decompose(term).0.is_zero();

    match (Nat::is_zero(&left), Nat::is_zero(&right)) {
        (true, true) => Peel::Equal,
        (true, false) if floored(&right) => Peel::Clash,
        (false, true) if floored(&left) => Peel::Clash,
        _ => Peel::Continue(left, right),
    }
}

/// One segment of a flattened free-monoid value: a run of consecutive literal elements — concrete bytes (`Bin`) or terms (`List`) — a `Window` into a base value (a `Bin/slice(base, lo, hi)`: contents symbolic, but length `hi - lo` statically known as a `Nat` term), or an opaque symbolic chunk (a variable, an append: anything whose contents *and* length are unknown). A value is a sequence of these, and the concatenation intrinsic is their juxtaposition; flattening normalises the monoid laws — associativity, the empty identity, re-segmented literal runs, and fused adjacent windows of one base (`slice(b, s, m) ++ slice(b, m, e) = slice(b, s, e)`) — so two definitionally equal values decompose to the same list.
enum Atom<E> {
    Literal(Vec<E>),
    Window { base: Term, lo: Term, hi: Term },
    Symbolic(Term),
}

/// The one free-monoid step `peel_bin` and `peel_list` share: strip the longest common prefix the two segment lists *certainly* agree on — literal elements matched one-for-one and whole symbolic chunks that are syntactically identical — leaving each list at its residual tail. Reports whether anything was peeled, so the caller knows it made progress (and a `Continue` cannot loop). Literal elements compare by `==`: exact for `Bin`'s bytes, *syntactic* for `List`'s terms — hence `peel_list` must not read a stalled literal head as a clash.
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

/// `List` is the free monoid on its elements — the same peel as `peel_bin`, with two differences. Its literal runs hold *terms*, not decided bytes, so two leading runs whose heads disagree are NOT a clash (the elements may still be convertible): the peel defers, and the caller's structural element-wise comparison settles it. And every `List`-valued producer carries its element type, recovered here to rebuild residuals. A leftover literal run against the empty identity (`[x] ~ []`) is still a definite length clash, as in `peel_bin`.
pub fn peel_list(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    let elem = list_elem(left)?;
    list_elem(right)?;

    let mut left = list_atoms(left);
    let mut right = list_atoms(right);
    let peeled = peel_prefix(&mut left, &mut right);

    Some(match (left.front(), right.front()) {
        (None, None) => Peel::Equal,
        (None, Some(atom)) | (Some(atom), None) => against_identity(atom),
        // Two leading literal runs whose heads differ, a literal facing a symbolic chunk, or two unlike chunks — none decidable by peeling (an element disagreement is syntactic, not semantic). Hand back any peeled residual.
        _ => match peeled {
            true => Peel::Continue(
                reassemble_list(left, elem.clone()),
                reassemble_list(right, elem),
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

/// The `List` analogue of [`bin_grain`] — [`peel_list`]'s gate, doubling as the element type residuals rebuild with (every atom of a `List(T)` value shares `T`, so one suffices for the whole list). `List` and `ListConcat` carry the monoid's literals and juxtaposition, `ListSlice` rides in as a measured `Window` (like `BinSlice`), and `ListAppend` rides in as its base followed by a length-1 literal run — so `append(xs, e) ≡ concat(xs, single(e))`. Any other producer is `None` and stays an opaque chunk left to the caller's comparison.
fn list_elem(intrinsic: &Intrinsic) -> Option<Term> {
    match intrinsic {
        Intrinsic::List(elem, _)
        | Intrinsic::ListConcat(elem, _)
        | Intrinsic::ListSlice(elem, ..)
        | Intrinsic::ListAppend(elem, ..) => Some(elem.clone()),
        _ => None,
    }
}

/// Flatten a `Bin` value to its segment list, normalising the monoid laws: nested `BinConcat`s splice in, empty runs drop out, adjacent runs merge.
fn bin_atoms(grain: Grain, intrinsic: &Intrinsic) -> VecDeque<Atom<u8>> {
    let mut out = Vec::new();
    bin_collect_intrinsic(grain, intrinsic, &mut out);
    out.into()
}

/// One item of a flattening walk's worklist. `Appended` is a `BinAppend`'s trailing atom, held back so it lands *after* everything its base contributes — the one place order is not simply left-to-right, and the reason a plain stack of operands would not do.
///
/// Explicit rather than recursive because a concatenation's depth is data-shaped once [`crate::FUSION_CAP`] stops an accumulation fusing; see [`crate::free_monoid`]'s `BinLevel`, which states the argument in full for the destructor side.
enum BinPending<'a> {
    Term(&'a Term),
    Intrinsic(&'a Intrinsic),
    Appended(&'a Term),
}

fn bin_collect_intrinsic(grain: Grain, intrinsic: &Intrinsic, out: &mut Vec<Atom<u8>>) {
    // A stack, so operands are pushed in reverse to come back off in order — `push`'s run merging depends on segments arriving left to right.
    let mut pending = vec![BinPending::Intrinsic(intrinsic)];

    while let Some(item) = pending.pop() {
        match item {
            BinPending::Term(term) => match &**term {
                Subterm::Intrinsic(intrinsic) => pending.push(BinPending::Intrinsic(intrinsic)),
                _ => push(out, Atom::Symbolic(term.clone())),
            },
            // The appended atom of a `BinAppend`, reached once its base has been flattened. A concrete byte is a length-1 literal run (so it merges with an abutting run and unifies with `concat(base, \b)`); a symbolic byte is the canonical one-byte chunk `append(x[], b)` — opaque, so its emptiness stays undecidable.
            BinPending::Appended(atom) => match bin_atom(grain, atom) {
                Some(b) => push(out, Atom::Literal(vec![b])),
                None => {
                    let empty = Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::empty()));
                    let chunk = Term::intrinsic(Intrinsic::bin_append(grain, empty, atom.clone()));
                    push(out, Atom::Symbolic(chunk));
                }
            },
            BinPending::Intrinsic(intrinsic) => match intrinsic {
                Intrinsic::Bin(found, value) if *found == grain => push(
                    out,
                    Atom::Literal(match grain {
                        Grain::B => (0..value.bit_length())
                            .map(|index| u8::from(value.bit(index).unwrap()))
                            .collect(),
                        Grain::X => value.to_bytes().unwrap(),
                    }),
                ),
                Intrinsic::BinConcat(found, operands) if *found == grain => {
                    pending.extend(operands.iter().rev().map(BinPending::Term));
                }
                Intrinsic::BinSlice(found, base, lo, hi) if *found == grain => push(
                    out,
                    Atom::Window {
                        base: base.clone(),
                        lo: lo.clone(),
                        hi: hi.clone(),
                    },
                ),
                // `append(base, b) = base ++ [b]`: decode the base, then the appended byte.
                Intrinsic::BinAppend(found, base, atom) if *found == grain => {
                    pending.push(BinPending::Appended(atom));
                    pending.push(BinPending::Term(base));
                }
                other => push(out, Atom::Symbolic(Term::intrinsic(other.clone()))),
            },
        }
    }
}

/// Flatten a `List` value to its segment list — the [`bin_atoms`] decomposition over element terms rather than bytes.
fn list_atoms(intrinsic: &Intrinsic) -> VecDeque<Atom<Term>> {
    let mut out = Vec::new();
    list_collect_intrinsic(intrinsic, &mut out);
    out.into()
}

/// [`BinPending`] over the element carrier.
enum ListPending<'a> {
    Term(&'a Term),
    Intrinsic(&'a Intrinsic),
    Appended(&'a Term),
}

fn list_collect_intrinsic(intrinsic: &Intrinsic, out: &mut Vec<Atom<Term>>) {
    let mut pending = vec![ListPending::Intrinsic(intrinsic)];

    while let Some(item) = pending.pop() {
        match item {
            ListPending::Term(term) => match &**term {
                Subterm::Intrinsic(intrinsic) => pending.push(ListPending::Intrinsic(intrinsic)),
                _ => push(out, Atom::Symbolic(term.clone())),
            },
            // The appended element of a `ListAppend`, as a length-1 literal run, so it merges with an abutting run and unifies with `concat(base, single(e))`.
            ListPending::Appended(elem) => push(out, Atom::Literal(vec![elem.clone()])),
            ListPending::Intrinsic(intrinsic) => match intrinsic {
                Intrinsic::List(_, elems) => push(out, Atom::Literal(elems.clone())),
                Intrinsic::ListConcat(_, operands) => {
                    pending.extend(operands.iter().rev().map(ListPending::Term));
                }
                Intrinsic::ListSlice(_, base, lo, hi) => push(
                    out,
                    Atom::Window {
                        base: base.clone(),
                        lo: lo.clone(),
                        hi: hi.clone(),
                    },
                ),
                // `append(base, e) = base ++ [e]`: decode the base, then the appended element.
                Intrinsic::ListAppend(_, base, elem) => {
                    pending.push(ListPending::Appended(elem));
                    pending.push(ListPending::Term(base));
                }
                other => push(out, Atom::Symbolic(Term::intrinsic(other.clone()))),
            },
        }
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

/// Rebuild a `List` term from a residual segment list — [`reassemble_bin`] over element runs, restoring the element type every `List`-valued producer carries.
fn reassemble_list(atoms: VecDeque<Atom<Term>>, elem: Term) -> Term {
    fn into_term(atom: Atom<Term>, elem: &Term) -> Term {
        match atom {
            Atom::Literal(elems) => Term::intrinsic(Intrinsic::List(elem.clone(), elems)),
            Atom::Window { base, lo, hi } => {
                Term::intrinsic(Intrinsic::list_slice(elem.clone(), base, lo, hi))
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

            Term::intrinsic(Intrinsic::ListConcat(elem, parts))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sym(index: u32, hint: &'static str) -> Term {
        Term::free_var(&crate::Free::local(index, Some(hint)))
    }

    fn nat_of(floor: u32, inner: Term) -> Nat {
        Nat::Succ(floor.into(), inner)
    }

    fn add(left: Term, right: Term) -> Term {
        Term::intrinsic(Intrinsic::nat_add(left, right))
    }

    // The conclusion cancelling summands adds to the peel: two sums that differ only in the order of their addends are the same number, so peeling decides them equal instead of handing a pair to a structural comparison that compares spellings and refuses. Sound because `+` commutes; new, because nothing else in the peel normalises summand order.
    #[test]
    fn peel_nat_decides_a_commuted_sum_equal() {
        let (x, y) = (sym(0, "x"), sym(1, "y"));

        let peel = peel_nat(
            &nat_of(1, add(x.clone(), y.clone())),
            &nat_of(1, add(y.clone(), x.clone())),
        );

        assert!(
            matches!(peel, Peel::Equal),
            "`x + y + 1` and `y + x + 1` are one number"
        );
    }

    // The clash the inverter reads as *impossible*, which is what excuses an omitted arm — so it must fire only where the two sides genuinely cannot be equal. A surviving positive floor against nothing is that case: whatever the symbolic residual takes, one side stays strictly larger.
    #[test]
    fn peel_nat_clashes_a_surviving_floor_against_the_identity() {
        let x = sym(0, "x");

        let peel = peel_nat(&nat_of(2, x.clone()), &nat_of(1, x.clone()));

        assert!(matches!(peel, Peel::Clash), "`x + 2` never equals `x + 1`");
    }

    // The control against closing the clash above by clashing everything: a shared floor over *distinct* symbols cancels to a pair that may still be equal, so peeling must hand it on rather than decide it.
    #[test]
    fn peel_nat_continues_where_the_residuals_may_still_agree() {
        let (x, y) = (sym(0, "x"), sym(1, "y"));

        let peel = peel_nat(&nat_of(1, x.clone()), &nat_of(1, y.clone()));

        assert!(
            matches!(peel, Peel::Continue(..)),
            "`x` and `y` are undecided, not unequal"
        );
    }

    fn bytes(run: impl Into<Vec<u8>>) -> Term {
        Term::intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(run.into())))
    }

    fn bits(run: impl IntoIterator<Item = bool>) -> Term {
        Term::intrinsic(Intrinsic::Bin(Grain::B, PackedBin::from_bits(run)))
    }

    fn nats(run: impl IntoIterator<Item = u32>) -> Term {
        let elems = run.into_iter().map(|n| sym(n, "e")).collect();
        Term::intrinsic(Intrinsic::List(sym(1000, "T"), elems))
    }

    fn as_intrinsic(term: &Term) -> &Intrinsic {
        match &**term {
            Subterm::Intrinsic(intrinsic) => intrinsic,
            _ => unreachable!("a literal term"),
        }
    }

    // **How a literal run is grouped is invisible to the peel, and that is the premise the fusion cap rests on.** Reduction fuses an all-literal concatenation into one value today; capping that leaves the `Concat` node standing instead, so a capped spelling and the literal it would have fused to must still decide equal. They do because [`bin_atoms`] flattens a concatenation into segments and [`push`] merges every pair of adjacent literal runs, so both groupings reach the same segment list before anything is compared.
    //
    // A trailing symbolic operand is what makes this a test rather than a tautology: without it both sides are all-literal, reduction fuses each into one value on the way in, and the assertion holds without the peel having decided anything. With it, neither side fuses and the peel is the only thing that can equate them.
    #[test]
    fn peel_bin_decides_a_split_literal_run_against_a_whole_one() {
        let tail = sym(0, "b");

        for (grain, split, whole) in [
            (
                Grain::X,
                vec![bytes([0x30, 0x31]), bytes([0x32, 0x33]), tail.clone()],
                vec![bytes([0x30, 0x31, 0x32, 0x33]), tail.clone()],
            ),
            (
                Grain::B,
                vec![
                    bits([true, false]),
                    bits([true, true]),
                    bits([false]),
                    tail.clone(),
                ],
                vec![bits([true, false, true, true, false]), tail.clone()],
            ),
        ] {
            let split = Intrinsic::BinConcat(grain, split);
            let whole = Intrinsic::BinConcat(grain, whole);

            assert!(
                matches!(peel_bin(&split, &whole), Some(Peel::Equal)),
                "{grain:?}: a run split across operands is the run"
            );
        }
    }

    // The same premise one level deeper, over the shape the cap actually produces. An accumulation appends to its own result, so what it builds is *left-nested* — `concat(concat(concat(a, b), c), d)` — never a flat operand list. Flattening has to see through that nesting, or the cap would hold for a spelling nobody writes.
    #[test]
    fn peel_bin_decides_a_left_nested_concat_against_a_flat_one() {
        let tail = sym(0, "b");
        let nest = |left: Term, right: Term| {
            Term::intrinsic(Intrinsic::BinConcat(Grain::X, vec![left, right]))
        };

        let nested = Intrinsic::BinConcat(
            Grain::X,
            vec![
                nest(
                    nest(bytes([0x30]), bytes([0x31, 0x32])),
                    bytes([0x33, 0x34, 0x35]),
                ),
                tail.clone(),
            ],
        );
        let flat = Intrinsic::BinConcat(
            Grain::X,
            vec![bytes([0x30, 0x31, 0x32, 0x33, 0x34, 0x35]), tail],
        );

        assert!(
            matches!(peel_bin(&nested, &flat), Some(Peel::Equal)),
            "nesting is associativity, and the peel is flat"
        );
    }

    // The `List` twin. Its literal runs hold *terms* rather than decided bytes, so the elements here are symbols compared syntactically — which is all the premise needs, since regrouping never changes an element, only which run it sits in.
    #[test]
    fn peel_list_decides_a_split_literal_run_against_a_whole_one() {
        let tail = sym(0, "xs");

        let split = Intrinsic::ListConcat(
            sym(1000, "T"),
            vec![nats([1, 2]), nats([3]), nats([4, 5]), tail.clone()],
        );
        let whole = Intrinsic::ListConcat(sym(1000, "T"), vec![nats([1, 2, 3, 4, 5]), tail]);

        assert!(
            matches!(peel_list(&split, &whole), Some(Peel::Equal)),
            "a run split across segments is the run"
        );
    }

    // The control the three above would be worthless without: the peel must not decide *everything* equal. Regrouping preserves the element order, so a genuine reordering has to clash rather than merge.
    #[test]
    fn peel_bin_still_clashes_a_reordered_run() {
        let split = Intrinsic::BinConcat(Grain::X, vec![bytes([0x30]), bytes([0x31])]);
        let swapped = bytes([0x31, 0x30]);

        assert!(
            matches!(peel_bin(&split, as_intrinsic(&swapped)), Some(Peel::Clash)),
            "`0x30 ++ 0x31` is not `0x31 0x30`"
        );
    }
}
