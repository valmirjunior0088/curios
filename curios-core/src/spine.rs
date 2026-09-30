//! The free-monoid peel shared by inversion (`invert`) and conversion (`convert`). An intrinsic whose values are a literal run of generators over a symbolic tail — a `Nat` count or sum, a `Bin` byte run, a `List` element run — reduces two values by stripping what they carry in common; the residual tails go back to the caller's own recursion. `Bool`/`Int` are the degenerate, zero-generator spines. The point of the seam: a new instance is one `peel_intrinsic` arm and nothing else — the drivers, the verdict vocabulary (`curios-algebra`'s `Deduction`), and the termination argument are shared, and `Bin`/`List` further share `curios-algebra`'s word strip itself (they differ only in their runs, and whether two runs whose heads differ are a clash).
//!
//! `Nat` is the one whose gate is a *shape* rather than a carrier, because it is the one commutative member: its values are also spelled as `NatAdd` spines, which no `Intrinsic::Nat` arm can match. See [`peel_nat_terms`].

use {
    super::{
        Atoms, Declaration, Element, Intrinsic, Nat, Operands, Sequence, Subterm, Term, Words,
        bin_grain, int_cancellation, int_monomial, int_rebuild_cancelled, int_shaped, list_element,
    },
    curios_algebra::{
        Carrier, Conclusion, Deduction, Operation, Stripped, pair_factors, same_leaves,
        same_position,
    },
};

/// What a peel concludes, over the pair of residuals it hands back. Every peel here is at [`Deduction`]'s strength — a residual it hands back holds exactly when the pair does, a common prefix or summand peeled off or a side regrouped — except [`peel_monomial`]'s and [`peel_comparison`]'s, which are merely sufficient and are therefore [`Conclusion`]s only conversion reads. `Undecided` is a pair the peel reads and makes no progress on, which every reader treats as the refusing direction, so declining can only cost reductions; `None` beside it is a pair the peel does not read at all.
pub type Verdict = Deduction<(Term, Term)>;

/// Classify a reduced intrinsic pair: the entry inversion reads, and so only ever a [`Verdict`]. `None` means the pair is not a matched spine-intrinsic, so the caller keeps its own handling.
pub fn peel_intrinsic(left: &Intrinsic, right: &Intrinsic) -> Option<Verdict> {
    match (left, right) {
        // Finite scalars are the degenerate (zero-generator) spines: no tail.
        (Intrinsic::Bool(actual), Intrinsic::Bool(target)) => Some(decide(actual == target)),
        (Intrinsic::Int(actual), Intrinsic::Int(target)) => Some(decide(actual == target)),
        // `Nat` is the free commutative monoid on its summands, `Bin`/`List` the free monoids on their bytes/elements (each returns `None` for the other's shapes), and `&&`/`||` the semilattices on their leaves.
        _ => peel_nat_pair(left, right)
            .or_else(|| peel_int_pair(left, right))
            .or_else(|| peel_bin(left, right))
            .or_else(|| peel_list(left, right))
            .or_else(|| peel_bool(left, right))
            .or_else(|| peel_symmetric(left, right))
            .or_else(|| peel_position(left, right)),
    }
}

/// The `Int` peel: ℤ under `+` is a group, so two reduced sums are one value exactly when their difference is zero, and `curios-algebra`'s cancellation moves that difference to the two sides by sign. Two constant residuals decide `Equal` or `Impossible`; a pair the cancellation changed carries on as `Equivalent` over its residuals, so `i + a ~ i + b` becomes `a ~ b` for the caller; and a pair it left untouched is `Undecided`, the stability [`peel_nat_terms`] rests on for the same reason. `None` when neither side is a literal, a sum spine or a product, so the caller keeps its own handling.
pub fn peel_int_pair(left: &Intrinsic, right: &Intrinsic) -> Option<Verdict> {
    let this = Term::intrinsic(left.clone());
    let that = Term::intrinsic(right.clone());
    (int_shaped(&this) || int_shaped(&that)).then(|| {
        int_cancellation(&this, &that)
            .deduction()
            .map(|cancelled| int_rebuild_cancelled(cancelled, &this, &that))
    })
}

/// A symmetric operation — an equality or an inequality at any carrier the algebra declares one of, the `xor` that `!=` on `Bool` lowers through, and the bitwise `and`, `or` and `xor` on ℕ — denotes one value with its operands in either order, so two of one operation are `Equal` when their operand pairs are one pair swapped, and `Undecided` otherwise, never `Impossible`. `None` for any other pair.
///
/// Decided here rather than by spelling the operands in one order at the fold, because a comparison is what a `choose` guard refines on, and a refinement is recorded under the guard's *written* spelling: both checkers canonicalize a probe's operands and never its node, so a fold that swapped them would take `rem == 1` past its own refinement inside `Str/step`. The peel changes no spelling, so every key stays where it was written.
pub fn peel_symmetric(left: &Intrinsic, right: &Intrinsic) -> Option<Verdict> {
    let commuting = |intrinsic: &Intrinsic| match intrinsic.algebra() {
        Declaration::Operation {
            carrier,
            operation,
            operands: Operands::Two([a, b]),
        } if operation.commutes() => Some(((carrier, operation), a.clone(), b.clone())),
        _ => None,
    };
    match (commuting(left), commuting(right)) {
        (Some((this, a, b)), Some((that, c, d))) if this == that => Some(match a == d && b == c {
            true => Deduction::Equal,
            false => Deduction::Undecided,
        }),
        _ => None,
    }
}

/// Two monomials of one carrier — two `Nat` products or two `Int` products — with their factors paired by identity before anything reads them in order: one coefficient and one multiset of factors is `Equal`, and one factor left on each side is `Sufficient` over that pair. `None` for anything else, so the caller's shape congruence decides.
///
/// **A monomial's factor order is a hash, and a hash is not a value.** The product fold sorts factors by their structural hash, which is canonical only while every factor is what it will stay: an unsolved metavariable hashes as itself and not as the term it is solved to, and so does a factor convertible to another without being identical. The shape congruence compared factors in that order, so `c · ?d · k` against `d · c · k` paired `c` with `d` and refused, where cancellation leaves `?d` against `d` and solves it — and whether the positions happened to line up could turn on a comment line elsewhere in the file. Summands have had exactly this pairing, by cancellation, all along; this is the product's.
///
/// **Conversion's alone, never inversion's.** The residual is a sufficient condition — equal residuals make equal monomials — and `x · f = x · g` does not give `f = g` at `x = 0`, so this is a [`Conclusion`], which inversion's entry cannot hand on: it is not in [`peel_intrinsic`], and both converters ask for it by name.
pub fn peel_monomial(left: &Intrinsic, right: &Intrinsic) -> Option<Conclusion<(Term, Term)>> {
    match (left, right) {
        (Intrinsic::NatMul(..), Intrinsic::NatMul(..)) => {
            let (left_coefficient, left_factors) = Nat::monomial(&Term::intrinsic(left.clone()));
            let (right_coefficient, right_factors) = Nat::monomial(&Term::intrinsic(right.clone()));
            paired(
                (&left_coefficient, &left_factors),
                (&right_coefficient, &right_factors),
            )
        }
        (Intrinsic::IntMul(..), Intrinsic::IntMul(..)) => {
            let (left_coefficient, left_factors) = int_monomial(&Term::intrinsic(left.clone()));
            let (right_coefficient, right_factors) = int_monomial(&Term::intrinsic(right.clone()));
            paired(
                (&left_coefficient, &left_factors),
                (&right_coefficient, &right_factors),
            )
        }
        _ => None,
    }
}

/// Two `Nat` or `Int` equalities, or two disequalities, with their sides paired by identity up to universe instances rather than by position: one pair of sides is `Equal`, and one side left on each is `Conclusion::Sufficient` over those two. `None` for anything else, so the caller's shape congruence decides.
///
/// **A comparison's side order is a hash**, as a monomial's factor order is ([`peel_monomial`]). The linear views respell two comparisons alike before any peel reads them, each side of the difference where its atoms' ranks put it, and a rank is a structural hash, which an unsolved metavariable takes from its own number. The shape congruence then compared sides in that order, so `?w != y` against `x != y` paired `?w` with `x` or with `y` according to where `?w` sorted, and a rule that solves the metavariable solved it or not as metavariables minted earlier in the item moved its number. Pairing by identity leaves `?w` against `x` wherever the two were put.
///
/// **Conversion's alone**, as the monomial pairing is: `a == b` against `c == b` does not give `a = c`, since the two are one value wherever both `a` and `c` differ from `b`. It is not in [`peel_intrinsic`], whose [`Deduction`] has no sufficient variant to carry it.
pub fn peel_comparison(left: &Intrinsic, right: &Intrinsic) -> Option<Conclusion<(Term, Term)>> {
    let sides = |intrinsic: &Intrinsic| match intrinsic.algebra() {
        Declaration::Operation {
            carrier: carrier @ (Carrier::Natural | Carrier::Integer),
            operation: operation @ (Operation::Equal | Operation::Unequal),
            operands: Operands::Two([a, b]),
        } => Some(((carrier, operation), [a.clone(), b.clone()])),
        _ => None,
    };
    let (this, left_sides) = sides(left)?;
    let (that, right_sides) = sides(right)?;
    paired((&this, &left_sides), (&that, &right_sides))
}

/// Two monomials' factors paired by `curios-algebra`'s `pair_factors` over numeric atoms, each leftover read back as the factor it was on its own side.
fn paired<C: PartialEq>(
    left: (&C, &[Term]),
    right: (&C, &[Term]),
) -> Option<Conclusion<(Term, Term)>> {
    let mut atoms = Atoms::default();
    let mut read = |factors: &[Term]| {
        factors
            .iter()
            .map(|factor| atoms.numeric(factor))
            .collect::<Vec<_>>()
    };
    let (left_atoms, right_atoms) = (read(left.1), read(right.1));
    let conclusion = pair_factors((left.0, &left_atoms), (right.0, &right_atoms))?;
    Some(conclusion.map(|(at, other)| (left.1[at].clone(), right.1[other].clone())))
}

fn decide(equal: bool) -> Verdict {
    match equal {
        true => Deduction::Equal,
        false => Deduction::Impossible,
    }
}

/// `&&` and `||` are each idempotent, commutative and associative, so two conjunctions — or two disjunctions — are one value exactly when they hold the same *set* of leaves under that connective. Each side is flattened to its leaves and the two sets compared by `curios-algebra`'s `same_leaves`, over leaves identified as written: the same set is `Equal`, anything else is `Undecided`, never `Impossible`, since two different leaf sets may still agree as values (`x && y` against `x` when `y` is `true`). `None` for a pair that is not two conjunctions or two disjunctions, so the caller keeps its own handling.
///
/// Decided here rather than by a canonical spelling in the fold: a fold that normalizes a tree whole on every step pays for the whole tree at every leaf, where a comparison flattens each side once. A leaf that is convertible but not identical is the caller's shape congruence's, so declining costs reductions and never correctness.
pub fn peel_bool(left: &Intrinsic, right: &Intrinsic) -> Option<Verdict> {
    let (conjunction, left_leaves) = bool_leaves(left)?;
    let (that, right_leaves) = bool_leaves(right)?;
    if conjunction != that {
        return None;
    }

    let mut atoms = Atoms::default();
    let mut read = |leaves: &[Term]| {
        leaves
            .iter()
            .map(|leaf| atoms.exact(leaf))
            .collect::<Vec<_>>()
    };
    let (left_atoms, right_atoms) = (read(&left_leaves), read(&right_leaves));
    let verdict = match same_leaves(&left_atoms, &right_atoms) {
        true => Deduction::Equal,
        false => Deduction::Undecided,
    };

    Some(verdict)
}

/// The leaves of a `&&` tree (`true`) or a `||` tree (`false`), left to right, with an explicit worklist because the tree's depth is data-shaped. `None` for any other intrinsic.
fn bool_leaves(intrinsic: &Intrinsic) -> Option<(bool, Vec<Term>)> {
    let conjunction = match intrinsic {
        Intrinsic::BoolAnd(..) => true,
        Intrinsic::BoolOr(..) => false,
        _ => return None,
    };

    let mut leaves = Vec::new();
    let mut pending = vec![Term::intrinsic(intrinsic.clone())];
    while let Some(term) = pending.pop() {
        match (&*term, conjunction) {
            (Subterm::Intrinsic(Intrinsic::BoolAnd(left, right)), true)
            | (Subterm::Intrinsic(Intrinsic::BoolOr(left, right)), false) => {
                pending.push(right.clone());
                pending.push(left.clone());
            }
            _ => leaves.push(term),
        }
    }

    Some((conjunction, leaves))
}

/// The `Nat` peel over two reduced intrinsics — [`peel_nat_terms`] at the shape [`peel_intrinsic`] and the two congruences hold their operands in.
pub fn peel_nat_pair(left: &Intrinsic, right: &Intrinsic) -> Option<Verdict> {
    // Gated before lifting, so a pair that is not a `Nat` at all costs a shape test rather than two allocations.
    (nat_shaped_intrinsic(left) || nat_shaped_intrinsic(right)).then(|| {
        Nat::cancellation_deduced(
            &Term::intrinsic(left.clone()),
            &Term::intrinsic(right.clone()),
        )
    })
}

/// The `Nat` peel over two reduced terms. `None` means neither side is a shape the cancellation can read, so the caller keeps its own handling.
///
/// **The gate is a shape, not a carrier, and that is the whole of what floorless sums needed.** `Nat::decompose` reads a successor floor and `Nat::summands` reads a `NatAdd` spine; those two shapes are what the cancellation acts on, and a `Nat`-valued operation that is neither rides in as an opaque summand either way. Admitting a pair where *one* side is one of them is what reaches the mixed case — `(s + 1) + l` reduces to a floored `Succ(1, s + l)` while `s + l` stays a bare `NatAdd`, so a gate demanding both sides be `Intrinsic::Nat` sees neither the reassociation nor the shared floor and hands the pair to a shape congruence that refuses it.
///
/// Sound at that width for the reason [`peel_bin`] and [`peel_list`] already rest on: conversion and inversion ask about pairs that inhabit one type, so a side carrying a floor or a sum spine makes both sides `Nat`s.
///
/// `Nat` is the free commutative monoid on its symbolic summands: `k + a ~ k' + t` cancels everything the two sides carry in common and the leftover rides on whichever side kept it — `2 ~ ?n + 1` becomes `1 ~ ?n`, and `x + a ~ x + b` becomes `a ~ b`.
///
/// The cancellation itself is `curios-algebra`'s, the one `Nat::cancel_common` runs for the reduction-side comparison and subtraction folds — one law, three readers — and so is what it concludes: both residuals gone is equality, a surviving positive floor against nothing is impossible, and anything else is a smaller pair for the caller to keep comparing. `Nat::cancellation_deduced` rebuilds that pair as terms, and this function only gates it. A non-canonical `Succ(0, _)` needs no guard of its own: `Nat::rebuild` collapses a zero floor, so no arm states it.
///
/// Cancelling *summands* rather than only the successor spine is what lets a commuted sum decide equal here instead of being handed to a structural comparison that would refuse it.
///
/// **A pass that changed nothing must decline, not carry.** Every `Equivalent` off a floored pair strips a shared floor, and that structural decrease is the termination argument; a floorless pair sharing no summand comes back from the cancellation *identically* — its no-progress arm returns the operands untouched on purpose — and handing that back as `Equivalent` re-enters the same congruence on the same terms and never settles. So a cancellation that took nothing off is `Undecided`, and the pair falls through to the caller's shape congruence, exactly as `Bin`'s and `List`'s peels do.
///
/// `Undecided` therefore stays unreachable for a pair of `Nat` *carriers*: two `Nat`s that are not both zero and not zero-against-floored are both `Succ`-headed, so they share a positive floor and always progress.
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

fn nat_shaped_intrinsic(intrinsic: &Intrinsic) -> bool {
    matches!(intrinsic, Intrinsic::Nat(_) | Intrinsic::NatAdd(..))
}

/// Two stuck `get`s are one value when they read one position of one root: `get(slice(xs, s, l), i)` is `xs`'s element at `s + i`, which is `get(xs, s + i)`, and `get(xs ++ ys, i)` is `get(xs, i)` where the second read's own bound places `i` inside `xs`. `Equal` when `curios-algebra`'s `same_position` finds the two positions one, `Undecided` otherwise and never `Impossible` — two unlike positions may still hold one element. `None` for a pair that is not two `get`s of one carrier and grain.
///
/// Decided here, as a comparison, because reduction cannot take it: rewriting the node would owe `s + i < len(xs)`, which follows from the window's bound and the index's by transitivity and is convertible with neither, and a reducer that derives a proof is the defect window fusion was reparameterised to avoid. Comparing builds no term, so it owes no proof — the two bounds are never read, which is proof irrelevance, the line a window's proof already draws.
pub fn peel_position(left: &Intrinsic, right: &Intrinsic) -> Option<Verdict> {
    let same = match (left, right) {
        (
            Intrinsic::ListGet {
                element,
                list: this,
                index: here,
                ..
            },
            Intrinsic::ListGet {
                list: that,
                index: there,
                ..
            },
        ) => same_position(
            &Words::new(Element(element.clone())),
            (this, here),
            (that, there),
        ),
        (
            Intrinsic::BinGet {
                grain,
                bin: this,
                index: here,
                ..
            },
            Intrinsic::BinGet {
                grain: other,
                bin: that,
                index: there,
                ..
            },
        ) if grain == other => same_position(&Words::new(*grain), (this, here), (that, there)),
        _ => return None,
    };

    Some(match same {
        true => Deduction::Equal,
        false => Deduction::Undecided,
    })
}

/// `Bin` is the free monoid on its bits or bytes: two values reduce by stripping their longest common prefix — `curios-algebra`'s `Word::strip_common_prefix` over the words `crate::words` reads — and the residual tails ride back on `Equivalent`, so the inverter can solve a flex binder forced to equal a leftover suffix and conversion can enqueue the rest. A definite element disagreement, or a residual with a positive segment facing the empty value, is `Impossible`; a chunk or window facing an unlike one is `Undecided`, and so is a residual of nothing but those facing the empty value, whose lengths are unknown. `None` means the pair is not two `Bin` values at one grain, so the caller keeps its own handling.
///
/// Prefix-only: a common *suffix* (`x ++ x[0x01] ~ y ++ x[0x01]`) is sound to cancel but not yet attempted. Chunks and single elements are matched as written, so two convertible-but-unequal elements (`append(x[], h1)` against `append(x[], h2)`) are left to the caller's structural comparison, and they reach it *flat*: see `regroup`.
pub fn peel_bin(left: &Intrinsic, right: &Intrinsic) -> Option<Verdict> {
    let grain = bin_grain(left)?;
    if bin_grain(right) != Some(grain) {
        return None;
    }

    Some(peel_words(&Words::new(grain), left, right))
}

/// `List` is the free monoid on its elements — [`peel_bin`]'s strip, with two differences. Its runs hold *terms*, not decided values, so two leading runs whose heads differ are not a clash (the elements may still be convertible): the peel defers, and the caller's element-wise comparison settles it. And every `List`-valued producer carries its element type, which residuals are rebuilt with. A leftover run against the empty list (`[x] ~ []`) is still a definite length clash.
pub fn peel_list(left: &Intrinsic, right: &Intrinsic) -> Option<Verdict> {
    let element = list_element(left)?;
    list_element(right)?;

    Some(peel_words(&Words::new(Element(element)), left, right))
}

/// The verdict [`peel_bin`] and [`peel_list`] share: what the strip decided, or its residuals rebuilt — handed back where a prefix came off, regrouped where none did.
fn peel_words<S: Sequence>(words: &Words<S>, left: &Intrinsic, right: &Intrinsic) -> Verdict {
    let (mut this, mut that) = (words.read(left), words.read(right));
    match this.strip_common_prefix(words, &mut that) {
        Stripped::Equal => Deduction::Equal,
        Stripped::Impossible => Deduction::Impossible,
        Stripped::Undecided => Deduction::Undecided,
        Stripped::Residual { peeled } => {
            let (flat_left, flat_right) = (words.rebuild(this), words.rebuild(that));
            match peeled {
                true => Deduction::Equivalent((flat_left, flat_right)),
                false => regroup(left, flat_left, right, flat_right),
            }
        }
    }
}

/// The verdict for a pair whose leading segments the strip could not match. A side spelled as anything but its own segment list — a nesting, an append, a run split across operands, an empty operand — is handed back as that list, and the pair carries on as `Equivalent`: regrouping is the identity on values, so the residuals hold the same obligation as any other `Equivalent`'s, and what the caller then sees is one operand list against another rather than a nesting against its flattening, which its shape congruence refused on operand count before comparing a single chunk. A pair already spelled flat declines as `Undecided`, exactly as [`peel_nat_terms`] declines an unchanged pair — an `Equivalent` that changed nothing would re-enter the caller on the same terms and never settle. The round after a regroup is that flat pair, so the two arms are the whole termination argument.
fn regroup(left: &Intrinsic, flat_left: Term, right: &Intrinsic, flat_right: Term) -> Verdict {
    let flat = |written: &Intrinsic, spelled: &Term| match &**spelled {
        Subterm::Intrinsic(intrinsic) => intrinsic == written,
        _ => false,
    };

    match flat(left, &flat_left) && flat(right, &flat_right) {
        true => Deduction::Undecided,
        false => Deduction::Equivalent((flat_left, flat_right)),
    }
}

#[cfg(test)]
mod commutative_tests;
#[cfg(test)]
mod monoid_tests;
#[cfg(test)]
mod nat_tests;
#[cfg(test)]
mod position_tests;
#[cfg(test)]
mod test_support;
