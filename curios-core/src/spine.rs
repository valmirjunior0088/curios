//! The free-monoid peel shared by inversion (`invert`) and conversion (`convert`). An intrinsic whose values are a literal run of generators over a symbolic tail — a `Nat` count or sum, a `Bin` byte run, a `List` element run — reduces two values by stripping what they carry in common; the residual tails go back to the caller's own recursion. `Bool`/`Int` are the degenerate, zero-generator spines. The point of the seam: a new instance is one `peel_intrinsic` arm and nothing else — the drivers, the `Peel` vocabulary, and the termination argument are shared, and `Bin`/`List` further share the `peel_prefix` step itself (they differ only in element type and whether a stalled literal head is a clash).
//!
//! `Nat` is the one whose gate is a *shape* rather than a carrier, because it is the one commutative member: its values are also spelled as `NatAdd` spines, which no `Intrinsic::Nat` arm can match. See [`peel_nat_terms`].

use {
    super::{
        Intrinsic, Nat, Subterm, Term, int_cancel_common, int_monomial, int_shaped,
        project_erased_universes,
    },
    curios_num::{Binary, Grain},
    std::collections::VecDeque,
};

/// One step of peeling two free-monoid values. Each caller maps it into its own vocabulary: `invert` to `Step::{Ok, Clash, Refuse}`, `convert` to a `bool` with the residual enqueued.
pub enum Peel {
    /// Both sides consumed to the identity — definitionally equal.
    Equal,
    /// A common head peeled off, or a side regrouped to its segment list; compare these residuals next.
    Continue(Term, Term),
    /// Literal heads differ, or a positive head meets the identity — unequal.
    Clash,
    /// Undecidable by peeling — a symbolic-length head, or a pair already spelled flat that the peel made no progress on; the caller falls back. Every reader treats it as the refusing direction, so declining can only cost reductions.
    Stuck,
}

/// Classify a reduced intrinsic pair. `None` means the pair is not a matched spine-intrinsic, so the caller keeps its own handling; `Some` is the peel outcome.
pub fn peel_intrinsic(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
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

/// The `Int` peel: ℤ under `+` is a group, so two reduced sums are one value exactly when their difference is zero, and [`int_cancel_common`] moves that difference to the two sides by sign. Two constant residuals decide `Equal` or `Clash`; a pair the cancellation changed carries on as `Continue` over its residuals, so `i + a ~ i + b` becomes `a ~ b` for the caller; and a pair it left untouched is `Stuck`, the stability `classify_nat` rests on for the same reason. `None` when neither side is a literal, a sum spine or a product, so the caller keeps its own handling.
pub fn peel_int_pair(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    let this = Term::intrinsic(left.clone());
    let that = Term::intrinsic(right.clone());
    (int_shaped(&this) || int_shaped(&that)).then(|| {
        let (residual_left, residual_right) = int_cancel_common(&this, &that);
        match (residual_left.as_int(), residual_right.as_int()) {
            (Some(a), Some(b)) => decide(a == b),
            _ => match residual_left != this || residual_right != that {
                true => Peel::Continue(residual_left, residual_right),
                false => Peel::Stuck,
            },
        }
    })
}

/// A symmetric operation — `==`, `!=`, the `xor` that `!=` on `Bool` lowers through, and the bitwise `and`, `or` and `xor` on ℕ — denotes one value with its operands in either order, so two of one operation are `Equal` when their operand pairs are one pair swapped, and `Stuck` otherwise, never `Clash`. `None` for any other pair.
///
/// Decided here rather than by spelling the operands in one order at the fold, because a comparison is what a `choose` guard refines on, and a refinement is recorded under the guard's *written* spelling: both checkers canonicalize a probe's operands and never its node, so a fold that swapped them would take `rem == 1` past its own refinement inside `Str/step`. The peel changes no spelling, so every key stays where it was written.
pub fn peel_symmetric(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    let swapped = match (left, right) {
        (Intrinsic::NatEql(a, b), Intrinsic::NatEql(c, d))
        | (Intrinsic::NatNeq(a, b), Intrinsic::NatNeq(c, d))
        | (Intrinsic::IntEql(a, b), Intrinsic::IntEql(c, d))
        | (Intrinsic::IntNeq(a, b), Intrinsic::IntNeq(c, d))
        | (Intrinsic::BoolEql(a, b), Intrinsic::BoolEql(c, d))
        | (Intrinsic::BoolNeq(a, b), Intrinsic::BoolNeq(c, d))
        | (Intrinsic::BoolXor(a, b), Intrinsic::BoolXor(c, d))
        | (Intrinsic::FltEql(a, b), Intrinsic::FltEql(c, d))
        | (Intrinsic::FltNeq(a, b), Intrinsic::FltNeq(c, d))
        | (Intrinsic::NatAnd(a, b), Intrinsic::NatAnd(c, d))
        | (Intrinsic::NatOr(a, b), Intrinsic::NatOr(c, d))
        | (Intrinsic::NatXor(a, b), Intrinsic::NatXor(c, d)) => a == d && b == c,
        (Intrinsic::BinEql(this, a, b), Intrinsic::BinEql(that, c, d)) if this == that => {
            a == d && b == c
        }
        _ => return None,
    };

    Some(match swapped {
        true => Peel::Equal,
        false => Peel::Stuck,
    })
}

/// Two monomials of one carrier — two `Nat` products or two `Int` products — with their factors paired by identity before anything reads them in order: one coefficient and one multiset of factors is `Equal`, and one factor left on each side is `Continue` over that pair. `None` for anything else, so the caller's shape congruence decides as it did.
///
/// **A monomial's factor order is a hash, and a hash is not a value.** The product fold sorts factors by their structural hash, which is canonical only while every factor is what it will stay: an unsolved metavariable hashes as itself and not as the term it is solved to, and so does a factor convertible to another without being identical. The shape congruence compared factors in that order, so `c · ?d · k` against `d · c · k` paired `c` with `d` and refused, where cancellation leaves `?d` against `d` and solves it — and whether the positions happened to line up could turn on a comment line elsewhere in the file. Summands have had exactly this pairing, by cancellation, all along; this is the product's.
///
/// **Conversion's alone, never inversion's.** Here `Continue` is a sufficient condition — equal residuals make equal monomials — and that is all a conversion reads it as. Inversion would read it as an equation to *deduce*, and `x · f = x · g` does not give `f = g` at `x = 0`, so this peel is not in [`peel_intrinsic`] and both converters ask for it by name.
pub fn peel_monomial(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    let (left_factors, right_factors) = match (left, right) {
        (Intrinsic::NatMul(..), Intrinsic::NatMul(..)) => {
            let (left_coefficient, left_factors) = Nat::monomial(&Term::intrinsic(left.clone()));
            let (right_coefficient, right_factors) = Nat::monomial(&Term::intrinsic(right.clone()));
            if left_coefficient != right_coefficient {
                return None;
            }
            (left_factors, right_factors)
        }
        (Intrinsic::IntMul(..), Intrinsic::IntMul(..)) => {
            let (left_coefficient, left_factors) = int_monomial(&Term::intrinsic(left.clone()));
            let (right_coefficient, right_factors) = int_monomial(&Term::intrinsic(right.clone()));
            if left_coefficient != right_coefficient {
                return None;
            }
            (left_factors, right_factors)
        }
        _ => return None,
    };

    let key = project_erased_universes::<Term>;
    let mut unmatched = right_factors;
    let mut residual = Vec::new();
    for factor in left_factors {
        let wanted = key(&factor);
        match unmatched
            .iter()
            .position(|candidate| key(candidate) == wanted)
        {
            Some(position) => {
                unmatched.swap_remove(position);
            }
            None => residual.push(factor),
        }
    }

    match (residual.as_slice(), unmatched.as_slice()) {
        ([], []) => Some(Peel::Equal),
        ([left], [right]) => Some(Peel::Continue(left.clone(), right.clone())),
        _ => None,
    }
}

fn decide(equal: bool) -> Peel {
    match equal {
        true => Peel::Equal,
        false => Peel::Clash,
    }
}

/// `&&` and `||` are each idempotent, commutative and associative, so two conjunctions — or two disjunctions — are one value exactly when they hold the same *set* of leaves under that connective. Each side is flattened to its leaves and the two sets compared by syntactic identity: the same set is `Equal`, anything else is `Stuck`, never `Clash`, since two different leaf sets may still agree as values (`x && y` against `x` when `y` is `true`). `None` for a pair that is not two conjunctions or two disjunctions, so the caller keeps its own handling.
///
/// Decided here rather than by a canonical spelling in the fold, on the record `documentation/roadmap.md` keeps of the `&&`/`||` cliff: a fold that normalized a tree whole on every step paid for the whole tree at every leaf, where a comparison flattens each side once. A leaf that is convertible but not identical is the caller's shape congruence's, as before, so declining costs reductions and never correctness.
pub fn peel_bool(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    let (conjunction, left_leaves) = bool_leaves(left)?;
    let (that, right_leaves) = bool_leaves(right)?;
    if conjunction != that {
        return None;
    }

    let covers = |these: &[Term], those: &[Term]| these.iter().all(|leaf| those.contains(leaf));

    Some(
        match covers(&left_leaves, &right_leaves) && covers(&right_leaves, &left_leaves) {
            true => Peel::Equal,
            false => Peel::Stuck,
        },
    )
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
pub fn peel_nat_pair(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    // Gated before lifting, so a pair that is not a `Nat` at all costs a shape test rather than two allocations.
    (nat_shaped_intrinsic(left) || nat_shaped_intrinsic(right)).then(|| {
        classify_nat(
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
pub fn peel_nat_terms(left: &Term, right: &Term) -> Option<Peel> {
    (nat_shaped(left) || nat_shaped(right)).then(|| classify_nat(left, right))
}

/// `Nat` is the free commutative monoid on its symbolic summands: `k + a ~ k' + t` cancels everything the two sides carry in common and the leftover rides on whichever side kept it — `2 ~ ?n + 1` becomes `1 ~ ?n`, and `x + a ~ x + b` becomes `a ~ b`.
///
/// The cancellation itself is `Nat::cancel_common`, which the reduction-side comparison and subtraction folds read too — one law, three readers. This function is only its translation into [`Peel`]: both residuals gone is equality, a surviving positive floor against nothing is a definite clash, and anything else is a smaller pair for the caller to keep comparing. The non-canonical `Succ(0, _)` the inverter used to need its own guard against falls out of `Nat::rebuild` collapsing a zero floor, so no arm states it.
///
/// Cancelling *summands* rather than only the successor spine is what lets a commuted sum decide equal here instead of being handed to a structural comparison that would refuse it.
///
/// **A pass that changed nothing must decline, not carry.** Every `Continue` off a floored pair strips a shared floor, and that structural decrease is the termination argument; a floorless pair sharing no summand comes back from `cancel_common` *identically* — its no-progress arm returns the operands untouched on purpose — and handing that back as `Continue` re-enters the same congruence on the same terms and never settles. So the residuals are compared against what went in, and an unchanged pair falls through as `Stuck` to the caller's shape congruence, exactly as `Bin`'s and `List`'s peels already do. Difference means *decrease* rather than merely change because `cancel_common` only ever rebuilds after removing a summand or a floor, which is the contract `cancellation_is_stable_when_nothing_is_shared` pins.
///
/// `Stuck` therefore stays unreachable for a pair of `Nat` *carriers*, which is the narrower claim this used to make of every pair: two `Nat`s that are not both zero and not zero-against-floored are both `Succ`-headed, so they share a positive floor and always progress.
fn classify_nat(left: &Term, right: &Term) -> Peel {
    let (residual_left, residual_right) = Nat::cancel_common(left, right);

    let floored = |term: &Term| !Nat::decompose(term).0.is_zero();

    match (Nat::is_zero(&residual_left), Nat::is_zero(&residual_right)) {
        (true, true) => Peel::Equal,
        (true, false) if floored(&residual_right) => Peel::Clash,
        (false, true) if floored(&residual_left) => Peel::Clash,
        _ => match residual_left != *left || residual_right != *right {
            true => Peel::Continue(residual_left, residual_right),
            false => Peel::Stuck,
        },
    }
}

/// Whether a reduced term is one of the two shapes [`classify_nat`] can act on: a successor floor, or a sum spine.
fn nat_shaped(term: &Term) -> bool {
    match &**term {
        Subterm::Intrinsic(intrinsic) => nat_shaped_intrinsic(intrinsic),
        _ => false,
    }
}

fn nat_shaped_intrinsic(intrinsic: &Intrinsic) -> bool {
    matches!(intrinsic, Intrinsic::Nat(_) | Intrinsic::NatAdd(..))
}

/// The `Nat` peel over two carriers — the entry the reduction-side folds and the fixtures reach it at, where a `Nat` is already in hand rather than a term.
pub fn peel_nat(actual: &Nat, target: &Nat) -> Peel {
    let lift = |value: &Nat| Term::intrinsic(Intrinsic::Nat(value.clone()));

    classify_nat(&lift(actual), &lift(target))
}

/// Whether two reduced `Nat` terms are one number: syntactic identity first, then the cancellation for the pairs it decides. A `None` or an undecided verdict answers `false`, which is the declining direction at the one caller — a window that does not fuse is compared whole instead.
fn nat_equal(left: &Term, right: &Term) -> bool {
    left == right || matches!(peel_nat_terms(left, right), Some(Peel::Equal))
}

/// A position in a value read through every window that value is itself cut from: the root the windows were taken of, and the position counted from the root's own start. `slice(b, s, l)` begins at `s`, so its position `i` is `b`'s position `s + i`, and a window of a window nests the same way; the sum is [`Nat::sum`]'s, so it is the term the fold would have built. The windows' own counts and proofs are not read: what makes every window on the way well-placed is the typing of the term in hand, and this states where a position *is*, never that it is in range.
fn rooted(base: &Term, position: &Term) -> (Term, Term) {
    let (mut base, mut position) = (base.clone(), position.clone());
    loop {
        let (inner, start) = match &*base {
            Subterm::Intrinsic(Intrinsic::BinSlice { bin, start, .. }) => {
                (bin.clone(), start.clone())
            }
            Subterm::Intrinsic(Intrinsic::ListSlice { list, start, .. }) => {
                (list.clone(), start.clone())
            }
            _ => return (base, position),
        };
        position = Nat::sum(&start, &position);
        base = inner;
    }
}

/// Two stuck `get`s are one value when they read one position of one root: `get(slice(xs, s, l), i)` is `xs`'s element at `s + i`, which is `get(xs, s + i)`. `Equal` when the roots are identical and the cancellation decides the two absolute positions one number, `Stuck` otherwise and never `Clash` — two unlike positions may still hold one element. `None` for a pair that is not two `get`s of one carrier and grain.
///
/// Decided here, as a comparison, because reduction cannot take it: rewriting the node would owe `s + i < len(xs)`, which follows from the window's bound and the index's by transitivity and is convertible with neither, and a reducer that derives a proof is the defect window fusion was reparameterised to avoid. Comparing builds no term, so it owes no proof — the two bounds are never read, which is proof irrelevance, the line `Atom::Window`'s `within` already draws.
pub fn peel_position(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    let ((this, here), (that, there)) = match (left, right) {
        (
            Intrinsic::ListGet {
                list: this,
                index: here,
                ..
            },
            Intrinsic::ListGet {
                list: that,
                index: there,
                ..
            },
        ) => ((this, here), (that, there)),
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
        ) if grain == other => ((this, here), (that, there)),
        _ => return None,
    };

    let (this_root, this_position) = rooted(this, here);
    let (that_root, that_position) = rooted(that, there);

    Some(
        match this_root == that_root && nat_equal(&this_position, &that_position) {
            true => Peel::Equal,
            false => Peel::Stuck,
        },
    )
}

/// One segment of a flattened free-monoid value: a run of consecutive literal elements — concrete bytes (`Bin`) or terms (`List`) — a `Single` symbolic element (a `Bin/append`'s byte when it is not a literal: contents unknown, length exactly one), a `Window` into a base value (a `Bin/slice(base, offset, length)`: contents symbolic, but length carried outright as a `Nat` term), or an opaque symbolic chunk (a variable, an unknown producer: anything whose contents *and* length are unknown). A value is a sequence of these, and the concatenation intrinsic is their juxtaposition; flattening normalises the monoid laws — associativity, the empty identity, re-segmented literal runs, and fused adjacent windows of one base (`slice(b, s, l₁) ++ slice(b, s + l₁, l₂) = slice(b, s, l₁ + l₂)`) — so two definitionally equal values decompose to the same list.
enum Atom<E> {
    Literal(Vec<E>),
    /// A `List` never produces one: its literal runs hold terms, so an appended element is a length-1 run. `Bin`'s runs hold decided bytes, and a symbolic byte is what this variant is for — the kind says its length, which is what lets the identity check clash it without reading a spelling.
    Single(Term),
    Window {
        base: Term,
        offset: Term,
        length: Term,
        /// The window's own `within` proof, carried so a residual can be rebuilt as the bounded node it came from — and so fusion can *hand one on* rather than derive one. Never compared: two windows over the same span are the same window whatever proved them, which is proof irrelevance and is why the equality test below reads the span alone.
        within: Term,
    },
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
            (Some(Atom::Symbolic(x)), Some(Atom::Symbolic(y)))
            | (Some(Atom::Single(x)), Some(Atom::Single(y)))
                if x == y =>
            {
                peeled = true;
                left.pop_front();
                right.pop_front();
            }
            // Two windows over the same span of the same root are equal whole, each read through the windows its own base was cut from, so `slice(slice(b, s, l), t, n)` is `slice(b, s + t, n)`; the starts and the counts are compared as numbers, by the cancellation. A shared root and start with differing lengths (one window extends past the other) could peel too, but that needs ordering the symbolic bounds, so it is left to defer rather than decided here.
            (
                Some(Atom::Window {
                    base: b1,
                    offset: o1,
                    length: n1,
                    within: _,
                }),
                Some(Atom::Window {
                    base: b2,
                    offset: o2,
                    length: n2,
                    within: _,
                }),
            ) if nat_equal(n1, n2) && {
                let (this_root, this_start) = rooted(b1, o1);
                let (that_root, that_start) = rooted(b2, o2);
                this_root == that_root && nat_equal(&this_start, &that_start)
            } =>
            {
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
        // An empty window is the identity: a zero-length window slices nothing.
        Atom::Window { length, .. } if Nat::is_zero(&length) => {}
        Atom::Window {
            base,
            offset,
            length,
            within,
        } => {
            // Fuse with a preceding window of the same base that this one begins at the end of. Under `(start, length)` that seam is an *arithmetic* fact — `offset = prev.offset + prev.length` — where it used to be a shared term, so it is decided by the `Nat` peel rather than read off syntactic equality. Strictly wider than the test it replaces, which is kept as the cheap first answer; a run of touching windows still collapses left-to-right to one.
            let abuts = match out.last() {
                Some(Atom::Window {
                    base: prev,
                    offset: at,
                    length: run,
                    within: _,
                }) => *prev == base && nat_equal(&offset, &Nat::sum(at, run)),
                _ => false,
            };

            match abuts {
                true => {
                    // **The fused window takes the *second* window's proof, unchanged.** Window₂ proved `(s + l₁) + l₂ <= len b` and the fused window needs `s + (l₁ + l₂) <= len b`, which is the same proposition up to a reassociation `peel_nat_terms` decides — so the term that proved one proves the other, and nothing here derives anything. That is the whole of what this file was blocked on: composing the old `(start, end)` window's `ordered` needed transitivity of `<=`, an implication no equality procedure supplies, and a reducer able to prove is the defect the reparameterisation removed rather than accommodated.
                    if let Some(Atom::Window {
                        length: run,
                        within: proof,
                        ..
                    }) = out.last_mut()
                    {
                        *run = Nat::sum(run, &length);
                        *proof = within;
                    }
                }
                false => out.push(Atom::Window {
                    base,
                    offset,
                    length,
                    within,
                }),
            }
        }
        other @ (Atom::Single(_) | Atom::Symbolic(_)) => out.push(other),
    }
}

/// One side peeled down to the empty identity while the other did not. The whole residual is read, not its head: a literal run (never empty, `push` drops those) or a single element anywhere in it gives the value a positive length, so the pair is a definite length mismatch (`Clash`) whatever the chunks around it take — `x ++ x[05] ~ x[]` clashes as `x[05] ++ x ~ x[]` does. A residual of windows and symbolic chunks alone might itself be empty (a window whose length is symbolic), so its emptiness is undecidable (`Stuck`).
fn against_identity<E>(residual: &VecDeque<Atom<E>>) -> Peel {
    let positive = residual
        .iter()
        .any(|atom| matches!(atom, Atom::Literal(_) | Atom::Single(_)));
    match positive {
        true => Peel::Clash,
        false => Peel::Stuck,
    }
}

/// `Bin` is the free monoid on its bytes. Two values reduce by stripping their longest common prefix — concrete bytes byte-for-byte, identical symbolic chunks whole, and equal slice windows whole (after `bin_atoms` has fused adjacent windows of one base) — and the residual tails ride back on `Continue` (so the inverter can solve a flex binder forced to equal a leftover suffix, and conversion can enqueue the rest). A definite byte disagreement, or a residual with a positive segment in it meeting the empty bytestring, is a `Clash`; a symbolic chunk or window facing an unlike one is `Stuck`, and so is a residual of nothing but those facing the identity (their lengths are unknown, so peeling cannot decide). `None` means the pair is not two `Bin` values, so the caller keeps its own handling.
///
/// Prefix-only, mirroring `peel_nat`: a common *suffix* (`x ++ x[0x01] ~ y ++ x[0x01]`) is sound to cancel but not yet attempted. Symbolic chunks, single elements and windows are matched by syntactic equality, so two convertible-but-unequal elements (`append(x[], h1)` vs `append(x[], h2)`) — or two windows whose bounds differ only up to arithmetic — are left to the caller's structural comparison rather than decided here, and they reach it *flat*: see `regroup`.
pub fn peel_bin(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    let grain = bin_grain(left)?;
    if bin_grain(right) != Some(grain) {
        return None;
    }

    let mut left_atoms = bin_atoms(grain, left);
    let mut right_atoms = bin_atoms(grain, right);
    let peeled = peel_prefix(&mut left_atoms, &mut right_atoms);

    Some(match (left_atoms.front(), right_atoms.front()) {
        (None, None) => Peel::Equal,
        (None, Some(_)) => against_identity(&right_atoms),
        (Some(_), None) => against_identity(&left_atoms),
        // Both still lead with a concrete run: the loop only stops here once their first bytes disagree, and bytes are decided — so the values are unequal.
        (Some(Atom::Literal(_)), Some(Atom::Literal(_))) => Peel::Clash,
        // A literal facing a symbolic chunk, or two unlike symbolic chunks. If a common prefix was peeled the residual tails go back to the caller; otherwise nothing here is decidable by peeling, and the pair regroups or declines.
        _ => {
            let flat_left = reassemble_bin(grain, left_atoms);
            let flat_right = reassemble_bin(grain, right_atoms);
            match peeled {
                true => Peel::Continue(flat_left, flat_right),
                false => regroup(left, flat_left, right, flat_right),
            }
        }
    })
}

/// The verdict for a pair whose leading segments the prefix step could not match. A side spelled as anything but its own segment list — a nesting, an append, a run split across operands, an empty operand — is handed back as that list, and the pair carries on as `Continue`: regrouping is the identity on values, so the residuals hold the same obligation as any other `Continue`'s, and what the caller then sees is one operand list against another rather than a nesting against its flattening, which its shape congruence refused on operand count before comparing a single chunk. A pair already spelled flat declines as `Stuck`, exactly as `classify_nat` declines an unchanged pair — a `Continue` that changed nothing would re-enter the caller on the same terms and never settle. The round after a regroup is that flat pair, so the two arms are the whole termination argument.
fn regroup(left: &Intrinsic, flat_left: Term, right: &Intrinsic, flat_right: Term) -> Peel {
    let flat = |written: &Intrinsic, spelled: &Term| match &**spelled {
        Subterm::Intrinsic(intrinsic) => intrinsic == written,
        _ => false,
    };

    match flat(left, &flat_left) && flat(right, &flat_right) {
        true => Peel::Stuck,
        false => Peel::Continue(flat_left, flat_right),
    }
}

/// `List` is the free monoid on its elements — the same peel as `peel_bin`, with two differences. Its literal runs hold *terms*, not decided bytes, so two leading runs whose heads disagree are NOT a clash (the elements may still be convertible): the peel defers, and the caller's structural element-wise comparison settles it. And every `List`-valued producer carries its element type, recovered here to rebuild residuals. A leftover literal run against the empty identity (`[x] ~ []`) is still a definite length clash, as in `peel_bin`.
pub fn peel_list(left: &Intrinsic, right: &Intrinsic) -> Option<Peel> {
    let elem = list_elem(left)?;
    list_elem(right)?;

    let mut left_atoms = list_atoms(left);
    let mut right_atoms = list_atoms(right);
    let peeled = peel_prefix(&mut left_atoms, &mut right_atoms);

    Some(match (left_atoms.front(), right_atoms.front()) {
        (None, None) => Peel::Equal,
        (None, Some(_)) => against_identity(&right_atoms),
        (Some(_), None) => against_identity(&left_atoms),
        // Two leading literal runs whose heads differ, a literal facing a symbolic chunk, or two unlike chunks — none decidable by peeling (an element disagreement is syntactic, not semantic). Hand back any peeled residual; otherwise the pair regroups or declines.
        _ => {
            let flat_left = reassemble_list(left_atoms, elem.clone());
            let flat_right = reassemble_list(right_atoms, elem);
            match peeled {
                true => Peel::Continue(flat_left, flat_right),
                false => regroup(left, flat_left, right, flat_right),
            }
        }
    })
}

/// The `Bin`-valued intrinsics `peel_bin` decomposes. `Bin` and `BinConcat` carry the monoid's literals and juxtaposition; `BinSlice` rides in as a measured `Window` (a chunk carrying its own length, whose contents are symbolic), so adjacent slices of one base fuse and equal slices cancel; `BinAppend` rides in as its base followed by the appended byte; `BinReplicate` rides in as its leading generator followed by a fill one shorter, which is what lets a fill compare against the cons it equals. Any other producer stays an opaque symbolic chunk left to the caller's own (structural) comparison.
fn bin_grain(intrinsic: &Intrinsic) -> Option<Grain> {
    match intrinsic {
        Intrinsic::Bin(grain, _)
        | Intrinsic::BinConcat { grain, operands: _ }
        | Intrinsic::BinSlice { grain, .. }
        | Intrinsic::BinAppend { grain, .. }
        | Intrinsic::BinReplicate { grain, .. } => Some(*grain),
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
        Intrinsic::List {
            element: elem,
            items: _,
        }
        | Intrinsic::ListConcat {
            element: elem,
            operands: _,
        }
        | Intrinsic::ListSlice { element: elem, .. }
        | Intrinsic::ListAppend { element: elem, .. } => Some(elem.clone()),
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
            // The appended atom of a `BinAppend`, reached once its base has been flattened. A concrete byte is a length-1 literal run (so it merges with an abutting run and unifies with `concat(base, \b)`); a symbolic byte is a `Single`, whose contents are unknown and whose length is one.
            BinPending::Appended(atom) => match bin_atom(grain, atom) {
                Some(b) => push(out, Atom::Literal(vec![b])),
                None => push(out, Atom::Single(atom.clone())),
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
                Intrinsic::BinConcat {
                    grain: found,
                    operands,
                } if *found == grain => {
                    pending.extend(operands.iter().rev().map(BinPending::Term));
                }
                Intrinsic::BinSlice {
                    grain: found,
                    bin: base,
                    start,
                    length,
                    within,
                } if *found == grain => push(
                    out,
                    Atom::Window {
                        base: base.clone(),
                        offset: start.clone(),
                        length: length.clone(),
                        within: within.clone(),
                    },
                ),
                // `replicate(n + 1, a) = [a] ++ replicate(n, a)`: emit the leading generator and leave the shorter fill as one symbolic chunk, which is all conversion needs — the other side's own residual fill is that same chunk, so the two match without either being unrolled. A concrete atom is a length-1 literal run, so it merges with an abutting run exactly as an appended byte does; a count of zero contributes nothing, and a count of unknown size stays opaque, since a fill that might be empty exposes no generator.
                Intrinsic::BinReplicate {
                    grain: found,
                    count,
                    atom,
                } if *found == grain => match Nat::peel_succ(count) {
                    Some(rest) => {
                        match bin_atom(grain, atom) {
                            Some(byte) => push(out, Atom::Literal(vec![byte])),
                            None => push(out, Atom::Single(atom.clone())),
                        }
                        push(
                            out,
                            Atom::Symbolic(Term::intrinsic(Intrinsic::BinReplicate {
                                grain,
                                count: rest,
                                atom: atom.clone(),
                            })),
                        );
                    }
                    None => {
                        if !Nat::is_zero(count) {
                            push(out, Atom::Symbolic(Term::intrinsic(intrinsic.clone())));
                        }
                    }
                },
                // `append(base, b) = base ++ [b]`: decode the base, then the appended byte.
                Intrinsic::BinAppend {
                    grain: found,
                    bin: base,
                    element: atom,
                } if *found == grain => {
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
                Intrinsic::List {
                    element: _,
                    items: elems,
                } => push(out, Atom::Literal(elems.clone())),
                Intrinsic::ListConcat {
                    element: _,
                    operands,
                } => {
                    pending.extend(operands.iter().rev().map(ListPending::Term));
                }
                Intrinsic::ListSlice {
                    element: _,
                    list: base,
                    start,
                    length,
                    within,
                } => push(
                    out,
                    Atom::Window {
                        base: base.clone(),
                        offset: start.clone(),
                        length: length.clone(),
                        within: within.clone(),
                    },
                ),
                // `append(base, e) = base ++ [e]`: decode the base, then the appended element.
                Intrinsic::ListAppend {
                    element: _,
                    list: base,
                    item: elem,
                } => {
                    pending.push(ListPending::Appended(elem));
                    pending.push(ListPending::Term(base));
                }
                other => push(out, Atom::Symbolic(Term::intrinsic(other.clone()))),
            },
        }
    }
}

/// Rebuild a `Bin` term from a residual segment list: a lone run is a `Bin` literal, a single element is the one-byte `append(x[], b)`, a window is its `BinSlice`, a lone symbolic chunk is itself (so the inverter sees the bare binder it must solve), and a mixture is their `BinConcat`.
fn reassemble_bin(grain: Grain, atoms: VecDeque<Atom<u8>>) -> Term {
    let into_term = |atom| match atom {
        Atom::Literal(atoms) => Term::intrinsic(Intrinsic::Bin(
            grain,
            match grain {
                Grain::B => Binary::from_bits(atoms.into_iter().map(|bit| bit != 0)),
                Grain::X => Binary::from_bytes(atoms),
            },
        )),
        Atom::Single(byte) => Term::intrinsic(Intrinsic::bin_append(
            grain,
            Term::intrinsic(Intrinsic::Bin(grain, Binary::empty())),
            byte,
        )),
        Atom::Window {
            base,
            offset,
            length,
            within,
        } => Term::intrinsic(Intrinsic::bin_slice(grain, base, offset, length, within)),
        Atom::Symbolic(term) => term,
    };

    match atoms.len() {
        1 => into_term(atoms.into_iter().next().unwrap()),
        _ => Term::intrinsic(Intrinsic::BinConcat {
            grain,
            operands: atoms.into_iter().map(into_term).collect(),
        }),
    }
}

/// Rebuild a `List` term from a residual segment list — [`reassemble_bin`] over element runs, restoring the element type every `List`-valued producer carries.
fn reassemble_list(atoms: VecDeque<Atom<Term>>, elem: Term) -> Term {
    fn into_term(atom: Atom<Term>, elem: &Term) -> Term {
        match atom {
            Atom::Literal(elems) => Term::intrinsic(Intrinsic::List {
                element: elem.clone(),
                items: elems,
            }),
            Atom::Single(item) => Term::intrinsic(Intrinsic::List {
                element: elem.clone(),
                items: vec![item],
            }),
            Atom::Window {
                base,
                offset,
                length,
                within,
            } => Term::intrinsic(Intrinsic::list_slice(
                elem.clone(),
                base,
                offset,
                length,
                within,
            )),
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

            Term::intrinsic(Intrinsic::ListConcat {
                element: elem,
                operands: parts,
            })
        }
    }
}

#[cfg(test)]
mod tests;
