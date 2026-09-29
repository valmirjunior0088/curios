//! Measures: where a window lies among a concatenation's operands, what a concatenation normalizes to, and where a literal splits against one.
//!
//! A length is a homomorphism from a word into `(ℕ, +, 0)`, and everything here reads a concatenation through it. Over lengths known as numbers, [`locate`] places a window exactly and [`split`] cuts a literal against a spine. Over lengths known only symbolically, [`seam_window`] places a window where it meets the operands' seams, consuming a distance by the alphabet's [`Alphabet::difference`]. What the operands are, and what a located piece is rebuilt as, is the caller's.

use {crate::Alphabet, std::ops::Range};

/// The total of `lengths`, or `None` where it does not fit a `usize`.
pub fn total(lengths: &[usize]) -> Option<usize> {
    lengths
        .iter()
        .try_fold(0usize, |total, length| total.checked_add(*length))
}

/// One operand a located window spans: whole, or the half-open range of it the window covers.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Span {
    Whole(usize),
    Part { operand: usize, range: Range<usize> },
}

/// The operands a `count`-long window at `start` spans, over operands of the given `lengths`, each narrowed to its overlap — the pieces whose concatenation *is* the window. `None` where the total does not fit a `usize`; `Err` carries the total where the window runs past the end.
///
/// An operand the window covers whole is a [`Span::Whole`], which a caller hands back untouched so it shares its payload; only the two at the edges are narrowed. A window is a start and a count, so a reversed range is not a shape this can be handed; only running past the end remains, and an end that overflows `usize` has certainly done so.
pub fn locate(lengths: &[usize], start: usize, count: usize) -> Option<Result<Vec<Span>, usize>> {
    let total = total(lengths)?;
    let Some(end) = start.checked_add(count).filter(|end| *end <= total) else {
        return Some(Err(total));
    };

    let mut spans = Vec::new();
    let mut offset = 0usize;
    for (operand, &length) in lengths.iter().enumerate() {
        let (from, to) = (offset, offset + length);
        offset = to;

        // Half-open overlap: an operand wholly before or after the window contributes nothing, and an empty overlap is no piece.
        let (lo, hi) = (start.max(from), end.min(to));
        if lo >= hi {
            continue;
        }
        spans.push(match (lo - from, hi - from) {
            (0, upper) if upper == length => Span::Whole(operand),
            (lower, upper) => Span::Part {
                operand,
                range: lower..upper,
            },
        });
    }

    Some(Ok(spans))
}

/// Where a window aligned to a concatenation's seams lies: the operands between two seams, or inside one operand at a distance into it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Seam<N> {
    Parts(Range<usize>),
    Inside { operand: usize, start: N },
}

/// The window of `length` at `start` over `operands` when it meets their seams: `slice(xs ++ ys, 0, len(xs))` is `xs` and `slice(xs ++ ys, len(xs), len(ys))` is `ys`, whatever those lengths are. `measure` hands over each operand's length as the walk reaches it, so a walk that stops early measures nothing past where it stopped. `None` where no seam matches.
///
/// **The walk consumes a distance rather than growing a prefix.** Each operand's measure is taken off the distance still to cover by [`Alphabet::difference`], which reads that operand's own measure rather than every measure before it; the window's end, a sum of the start and the length, is never built.
///
/// **A measure the distance cannot absorb is an overshoot**: the seam is already behind, and a prefix only grows, so the walk declines there, exactly rather than conservatively.
///
/// **An overshoot on the last operand is a narrowing, not a decline.** A window that has consumed every operand before the last and still has distance to cover lies inside the last one, at whatever distance remains: `get(xs ++ ys, len(xs) + i)` is `get(ys, i)`. The caller's bound, `start + length <= len(whole)`, with the consumed prefix taken off both sides, is exactly the bound on that operand, so the caller's proof proves the narrowed operation and nothing is derived. Inside an earlier operand, the later operands' measures would be left standing in that bound, which is why that case still declines.
pub fn seam_window<A: Alphabet, E>(
    alphabet: &A,
    operands: &[A::Symbol],
    start: &A::Number,
    length: &A::Number,
    mut measure: impl FnMut(&A::Symbol) -> Result<A::Number, E>,
) -> Result<Option<Seam<A::Number>>, E> {
    let mut remaining = start.clone();
    let mut begin = None;

    for (index, operand) in operands.iter().enumerate() {
        if begin.is_none() && alphabet.is_zero(&remaining) {
            begin = Some(index);
            remaining = length.clone();
        }
        if let Some(begin) = begin
            && alphabet.is_zero(&remaining)
        {
            return Ok(Some(Seam::Parts(begin..index)));
        }
        let measured = measure(operand)?;
        match alphabet.difference(&remaining, &measured) {
            Some(rest) => remaining = rest,
            None if index + 1 == operands.len() => {
                return Ok(match begin {
                    // The window begins at this operand's seam and ends inside it.
                    Some(begin) if begin == index => Some(Seam::Inside {
                        operand: index,
                        start: alphabet.count(0),
                    }),
                    // The window begins and ends inside this operand, `remaining` into it.
                    None => Some(Seam::Inside {
                        operand: index,
                        start: remaining,
                    }),
                    // The window began at an earlier seam and ends inside this operand.
                    Some(_) => None,
                });
            }
            None => return Ok(None),
        }
    }

    Ok(match begin {
        Some(begin) if alphabet.is_zero(&remaining) => Some(Seam::Parts(begin..operands.len())),
        _ => None,
    })
}

/// What one operand of a concatenation is to its normal form.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Joining {
    /// The empty value: the identity, dropped.
    Empty,
    /// A literal run the caller may fuse.
    Fusible,
    /// Anything else, which stays standing.
    Standing,
}

/// A concatenation's normal form under the unit and associativity laws.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Concatenated {
    /// Every surviving operand fuses into one literal, in order; none survives when all were empty.
    Fused(Vec<usize>),
    /// One operand survives, and a concatenation of one is that one.
    Lone(usize),
    /// The surviving operands stand, in order.
    Kept(Vec<usize>),
}

/// What a concatenation of operands like `operands` normalizes to: the empty identity dropped, every survivor fused where all are literal, and a lone survivor collapsed to itself. Fusion is all or nothing — the first operand that stands stops it — which is what keeps a literal from being copied into a concatenation that stays standing anyway.
pub fn join(operands: &[Joining]) -> Concatenated {
    let kept = operands
        .iter()
        .enumerate()
        .filter(|(_, joining)| **joining != Joining::Empty)
        .map(|(index, _)| index)
        .collect::<Vec<_>>();

    match kept
        .iter()
        .all(|&index| operands[index] == Joining::Fusible)
    {
        true => Concatenated::Fused(kept),
        false if kept.len() == 1 => Concatenated::Lone(kept[0]),
        false => Concatenated::Kept(kept),
    }
}

/// Where a literal splits against a concatenation: the range of the literal each operand takes, in order, as far as the split was placed, and how far that was.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Split {
    pub ranges: Vec<Range<usize>>,
    pub cut: Cut,
}

/// How far a [`Split`] was placed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cut {
    /// Every operand took a range, and together they cover the literal.
    Whole,
    /// The lengths fix a total the literal does not have, which no solution for the operands can repair.
    Clash,
    /// An operand of unknown length stands where the split is not fixed.
    Undetermined,
}

/// A literal of `total` elements cut against operands of the given `lengths`, consumed left to right: an operand of unknown length is admitted last, where it takes the remainder, and anywhere else leaves the split undetermined. Lengths that run past the literal, or fall short of it with nothing left to take the rest, clash.
///
/// The ranges placed before the split stopped are handed back whatever it concluded, so a caller relating each operand to its range as the split reaches it relates exactly those.
pub fn split(lengths: &[Option<usize>], total: usize) -> Split {
    let mut ranges = Vec::with_capacity(lengths.len());
    let mut offset = 0usize;
    let cut = 'cut: {
        for (index, length) in lengths.iter().enumerate() {
            match length {
                Some(length) => match offset.checked_add(*length).filter(|end| *end <= total) {
                    Some(end) => {
                        ranges.push(offset..end);
                        offset = end;
                    }
                    None => break 'cut Cut::Clash,
                },
                None if index + 1 == lengths.len() => {
                    ranges.push(offset..total);
                    break 'cut Cut::Whole;
                }
                None => break 'cut Cut::Undetermined,
            }
        }
        match offset == total {
            true => Cut::Whole,
            false => Cut::Clash,
        }
    };
    Split { ranges, cut }
}

#[cfg(test)]
mod tests;
