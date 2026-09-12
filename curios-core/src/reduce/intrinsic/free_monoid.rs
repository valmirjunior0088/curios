//! Folding an operation that distributes over a free-monoid spine.
//!
//! A `Bin`, a `List` and a `Nat` are all a concatenation of pieces, and an operation that is a homomorphism over that concatenation reduces piecewise — so a symbolic tail does not block the literal pieces around it. [`Shape`] is the peeled spine, and [`reduce_homomorphism`] is the fold over it that every such operation shares.

use {
    super::*,
    crate::{Intrinsic, Nat, Piece, ReduceError, Reducer, Subterm, Term},
    curios_utilities::{Grain, PackedBin},
};

/// The free-monoid product structure of a reduced carrier value, the view a monoid homomorphism (`len`/`map`) distributes over: a literal run of generators `L` (bytes for `Bin`, elements for `List`), an n-ary `Concat` of operands to recurse on, an `Append` of a base and one appended generator, or an `Opaque` node (a variable / slice) the homomorphism leaves neutral. `Empty` is just `Literal(∅)`.
pub(super) enum Shape<L> {
    Literal(Vec<L>),
    Concat(Vec<Term>),
    Append(Term, Term),
    Opaque(Term),
}

/// Classify a reduced `Bin` value into its product shape (generators are bytes).
///
/// **The literal arm materializes the whole run**, one `u8` per generator — which at the bit grain is a byte per *bit*, eight times the value's own width. An operation whose result is a single `Nat` therefore allocates its entire subject to compute it, and that is why this takes a reducer: the buffer is charged before it is filled. `Bin/len` no longer reaches here for a wholly-literal value, which answers from the free monoid's measure instead, but every symbolic shape still falls through to the homomorphism and still pays this.
pub(super) fn bin_shape(
    reducer: &mut impl Reducer,
    grain: Grain,
    value: Term,
) -> Result<Shape<u8>, ReduceError> {
    Ok(match Term::unwrap_or_clone(value) {
        Subterm::Intrinsic(Intrinsic::Bin(found, value)) if found == grain => {
            reducer.spend(Cost::buffer(value.len(grain) as u64))?;

            Shape::Literal(match grain {
                Grain::B => (0..value.bit_length())
                    .map(|index| u8::from(value.bit(index).unwrap()))
                    .collect(),
                Grain::X => value.to_bytes().unwrap(),
            })
        }
        Subterm::Intrinsic(Intrinsic::BinConcat {
            grain: found,
            operands,
        }) if found == grain => Shape::Concat(operands),
        Subterm::Intrinsic(Intrinsic::BinAppend {
            grain: found,
            bin: base,
            element: atom,
        }) if found == grain => Shape::Append(base, atom),
        other => Shape::Opaque(other.into()),
    })
}

/// Classify a reduced `List` value into its product shape (generators are elements).
///
/// No charge: every arm hands back storage the value already held. The literal arm moves the element vector out of a uniquely-held node, or clones its reference slots out of a shared one — which is the sharing case, since the elements themselves are reference-count bumps rather than rebuilt terms.
pub(super) fn list_shape(value: Term) -> Shape<Term> {
    match Term::unwrap_or_clone(value) {
        Subterm::Intrinsic(Intrinsic::List {
            element: _,
            items: elems,
        }) => Shape::Literal(elems),
        Subterm::Intrinsic(Intrinsic::ListConcat {
            element: _,
            operands,
        }) => Shape::Concat(operands),
        Subterm::Intrinsic(Intrinsic::ListAppend {
            element: _,
            list: base,
            item: elem,
        }) => Shape::Append(base, elem),
        other => Shape::Opaque(other.into()),
    }
}

/// The shared driver for a free-monoid homomorphism `h` — the one place its distribution law lives, so a carrier physically cannot forget a case. A literal run maps via `literal`; a concatenation recurses `h` over its operands and folds the images with `combine`; an append combines `h(base)` with the appended generator via `append`; an opaque value stays neutral, rebuilt by `node` (which also builds `h(sub)` to recurse). `len` and `map` differ only in those four slots. The built image is reduced, so the homomorphism is eager.
pub(super) fn reduce_homomorphism<L>(
    reducer: &mut impl Reducer,
    shape: Shape<L>,
    literal: impl Fn(Vec<L>) -> Term,
    combine: impl Fn(Vec<Term>) -> Term,
    append: impl Fn(Term, Term) -> Term,
    node: impl Fn(Term) -> Term,
) -> Result<Subterm, ReduceError> {
    let built = match shape {
        Shape::Literal(run) => literal(run),
        Shape::Concat(operands) => {
            // One rebuilt image node per operand, collected into one vector — the homomorphism's whole allocation, and the only arm of the four that scales with anything.
            reducer.spend(
                Cost::collection(operands.len() as u64)
                    .saturating_add(Cost::term(1).saturating_mul(operands.len() as u64)),
            )?;

            combine(operands.into_iter().map(node).collect())
        }
        Shape::Append(base, generator) => append(node(base), generator),
        Shape::Opaque(value) => return Ok(Term::unwrap_or_clone(node(value))),
    };

    reducer.reduce(built).map(Term::unwrap_or_clone)
}

/// `Σ` over a run of `Nat` images — the `combine` of the `len` homomorphism into `(ℕ, +, 0)`. `NatAdd`'s successor peeling carries the count out of a symbolic spine.
pub(super) fn nat_sum(images: Vec<Term>) -> Term {
    images
        .into_iter()
        .rev()
        .fold(Term::intrinsic(Intrinsic::Nat(Nat::Zero)), |acc, image| {
            Term::intrinsic(Intrinsic::nat_add(image, acc))
        })
}

/// One piece of a located `Bin` window, as a value.
///
/// Every segment [`bin_segments`](crate::free_monoid) admits is a literal run, so a narrowed edge is narrowed *here* — `PackedBin::slice` is an O(1) window into the same payload — rather than rebuilt as a `BinSlice` node for the next pass to fold into exactly this. Same value, same operation, one round trip earlier, and the window arm then constructs no bounded node at all.
pub(super) fn bin_piece(grain: Grain, piece: Piece<'_>) -> Term {
    match piece {
        Piece::Whole(operand) => operand.clone(),
        Piece::Part(operand, lo, hi) => match &**operand {
            Subterm::Intrinsic(Intrinsic::Bin(found, run)) if *found == grain => {
                let narrowed = run
                    .slice(grain, lo, hi)
                    .expect("a window's piece lies inside the run it was located in");

                Term::intrinsic(Intrinsic::Bin(grain, narrowed))
            }
            _ => unreachable!("a located window's segments are literal runs"),
        },
    }
}

/// The generator a located index names, read straight out of the literal run holding it.
///
/// Every segment [`bin_segments`](crate::free_monoid) admits is a literal run, so the read is performed here rather than rebuilt as a `BinGet` over that operand for the next pass to fold into exactly this — which is also what keeps the located path from having to *state* a bound it would then have to prove.
pub(super) fn bin_element(grain: Grain, operand: &Term, local: usize) -> Option<Subterm> {
    let Subterm::Intrinsic(Intrinsic::Bin(found, run)) = &**operand else {
        unreachable!("a located index lies in a literal run");
    };
    assert_eq!(*found, grain, "a located segment shares the value's grain");

    element_of_run(grain, run, local)
}

/// The generator at an index of a literal run, as the value its grain reads it as.
///
/// **The one place the grain decides what a generator *is*.** `PackedBin` has a reader per grain and the two produce different carriers — a `Byte` at X, a `Bool` at B — so every path that reads one generator branches here and nowhere else, which is what lets the fold arms above take the grain as a parameter rather than as a case.
pub(super) fn element_of_run(grain: Grain, run: &PackedBin, local: usize) -> Option<Subterm> {
    match grain {
        Grain::X => run
            .byte(local)
            .map(|byte| Subterm::Intrinsic(Intrinsic::Byte(byte))),
        Grain::B => run
            .bit(local)
            .map(|bit| Subterm::Intrinsic(Intrinsic::Bool(bit))),
    }
}

/// One generator, read out of the term an element reduced to.
///
/// The other direction of [`element_of_run`]'s seam, and the rest of what the grain decides: reading a generator *into* a run takes both a carrier — a `Byte` at X, a `Bool` at B — and an append that can refuse, since a byte has to land on a byte boundary where a bit lands anywhere. Naming the generator separates the two steps the append arm needs kept apart, because the budget is charged between them: the read decides whether there is work to do, and the charge has to precede the copy that does it.
pub(super) enum Generator {
    Byte(u8),
    Bit(bool),
}

impl Generator {
    /// The generator a reduced element names at this grain, or `None` where it is not a literal of it.
    pub(super) fn read(grain: Grain, element: &Subterm) -> Option<Self> {
        match grain {
            Grain::X => match element {
                Subterm::Intrinsic(Intrinsic::Byte(byte)) => Some(Self::Byte(*byte)),
                _ => None,
            },
            Grain::B => element.as_bool().map(Self::Bit),
        }
    }

    /// The run with this generator appended — `None` at the byte grain alone, over a run that is not byte-aligned.
    pub(super) fn appended_to(self, run: &PackedBin) -> Option<PackedBin> {
        match self {
            Self::Byte(byte) => run.append_byte(byte),
            Self::Bit(bit) => Some(run.append_bit(bit)),
        }
    }
}

/// A value read as a concatenation's operands: a concatenation's own, or an append's base beside the one-generator run it adds. `None` for anything that is neither.
///
/// **An append *is* a concatenation, and only the locator disagreed.** `append(b, k) = b ++ append(x[], k)` is the peel's own law, conversion's spine flattens both spellings to one atom list, and `x[..p, k]` and `x[..p, ..x[k]]` are definitionally equal terms. A locator gated on the concatenation node alone therefore declined a window it had already decided for the other spelling of the same value — incompleteness with nothing on the refusing side to justify it, and the shape `Bytes/of_nat` builds with, so every base-256 encoding was outside what a window could locate.
pub(super) fn concatenated(grain: Grain, value: &Term) -> Option<Vec<Term>> {
    fn flatten(grain: Grain, value: &Term, into: &mut Vec<Term>) {
        match &**value {
            Subterm::Intrinsic(Intrinsic::BinConcat {
                grain: found,
                operands,
            }) if *found == grain => {
                for operand in operands {
                    flatten(grain, operand, into);
                }
            }
            Subterm::Intrinsic(Intrinsic::BinAppend {
                grain: found,
                bin,
                element,
            }) if *found == grain => {
                flatten(grain, bin, into);
                into.push(Term::intrinsic(Intrinsic::bin_append(
                    grain,
                    Term::intrinsic(Intrinsic::Bin(grain, PackedBin::empty())),
                    element.clone(),
                )));
            }
            _ => into.push(value.clone()),
        }
    }

    let nested = matches!(
        &**value,
        Subterm::Intrinsic(Intrinsic::BinConcat { grain: found, .. } | Intrinsic::BinAppend { grain: found, .. })
            if *found == grain
    );
    nested.then(|| {
        let mut operands = Vec::new();
        flatten(grain, value, &mut operands);
        operands
    })
}

/// [`concatenated`] over the `List` carrier, whose append names its parts differently and carries an element type.
pub(super) fn list_concatenated(value: &Term) -> Option<Vec<Term>> {
    match &**value {
        Subterm::Intrinsic(Intrinsic::ListConcat { operands, .. }) => Some(operands.clone()),
        Subterm::Intrinsic(Intrinsic::ListAppend {
            element,
            list,
            item,
        }) => Some(vec![
            list.clone(),
            Term::intrinsic(Intrinsic::list_append(
                element.clone(),
                Term::intrinsic(Intrinsic::List {
                    element: element.clone(),
                    items: Vec::new(),
                }),
                item.clone(),
            )),
        ]),
        _ => None,
    }
}

/// The single generator of a value whose measure is exactly one, or `None` for anything else.
///
/// **Why this rather than an inner `get`.** A located one-operand window gives `get(v, i) = get(w, 0)`, and rebuilding that inner read would need a proof that `0 < len(w)` — a proposition the caller's own bound does not state, and one a reducer may not construct: [`crate::Intrinsic::signature`]'s module documentation records that a reducer emitting proofs is the defect the bound fields exist to remove. Reading the generator out of the two shapes a one-element value takes needs no proof at all, and declining every other shape costs reductions and never an answer.
pub(super) fn single_generator(grain: Grain, operand: &Term) -> Option<Term> {
    match &**operand {
        Subterm::Intrinsic(Intrinsic::Bin(found, run))
            if *found == grain && run.len(grain) == 1 =>
        {
            bin_element(grain, operand, 0).map(Term::from)
        }
        Subterm::Intrinsic(Intrinsic::BinAppend {
            grain: found,
            bin: base,
            element,
        }) if *found == grain => match &**base {
            Subterm::Intrinsic(Intrinsic::Bin(empty, run))
                if *empty == grain && run.len(grain) == 0 =>
            {
                Some(element.clone())
            }
            _ => None,
        },
        _ => None,
    }
}

/// A window aligned to the seams of a concatenation is the run of operands between those seams: `slice([..xs, ..ys], 0, len(xs)) = xs` and `slice([..xs, ..ys], len(xs), len(ys)) = ys`, over *symbolic* operands — the case the literal-run locators above decline. Sound for every value of the symbolic operands: a window whose start is exactly a prefix's length and whose end is exactly a longer prefix's length covers exactly the operands between, whatever those lengths are. `None` where no seam matches; the operands of the matched run otherwise, for the caller to concatenate.
///
/// **The walk consumes a distance rather than growing a prefix.** Each operand's measure is *cancelled off* the distance still to cover by [`Nat::cancel_common`], which reads that operand's own summands rather than every summand before it. What the walk spends is one measure per operand and nothing else: the accumulation no longer re-enters the reducer, and the window's end — a sum of the start and the count — is never built at all. This is a charge against the budget rather than an asymptotic win, and the distinction is worth keeping because it was once claimed the other way: a window written over a long prefix sum spends the bulk of its time normalizing that sum where it is *written*, in `NatAdd`'s own fold, and only a small remainder here.
///
/// **What a surviving measure means.** [`consume`] hands back whatever neither side absorbed, and a measure carrying a summand the distance lacks is an overshoot: the seam is already behind, and since a prefix only ever grows no later operand can bring it back. Declining there is exact rather than conservative, which is why the walk can stop at the first one. What cancellation sees through that whole-term equality did not is a universe instance, [`Nat::cancel_common`] keying its summands through `project_erased_universes` — the admitting direction, on the licence the carrier already gives its other readers, recorded in `documentation/soundness/what-the-kernel-consults/the-refinement-key.md`. Two spellings of one length that cancellation does not pair still decline, which is the refusing direction and the incompleteness this rule keeps.
pub(super) fn seam_window(
    reducer: &mut impl Reducer,
    operands: &[Term],
    start: &Term,
    length: &Term,
    measure: impl Fn(&Term) -> Intrinsic,
) -> Result<Option<Vec<Term>>, ReduceError> {
    let mut remaining = start.clone();
    let mut begin = None;

    for (index, operand) in operands.iter().enumerate() {
        if begin.is_none() && Nat::is_zero(&remaining) {
            begin = Some(index);
            remaining = length.clone();
        }
        if let Some(begin) = begin
            && Nat::is_zero(&remaining)
        {
            return Ok(Some(operands[begin..index].to_vec()));
        }
        let measured = reducer.reduce_forced(Term::intrinsic(measure(operand)))?;
        match consume(&remaining, &measured) {
            Some(rest) => remaining = rest,
            None => return Ok(None),
        }
    }

    Ok(match begin {
        Some(begin) if Nat::is_zero(&remaining) => Some(operands[begin..].to_vec()),
        _ => None,
    })
}

/// `remaining` with `measured` taken off it, or `None` where `measured` carries a summand `remaining` does not — the overshoot [`seam_window`] declines on.
///
/// [`Nat::cancel_common`] is the whole of it because a distance and a measure are two `Nat`s in one cancellative monoid: what it leaves on the left is the distance still to cover, and what it leaves on the right is what the measure had and the distance did not. Clamping each shared coefficient with a minimum is what keeps the subtraction total — `Natural`'s own panics on underflow — so the overshoot arrives as a residual to read rather than as a difference to guard.
fn consume(remaining: &Term, measured: &Term) -> Option<Term> {
    let (rest, unmatched) = Nat::cancel_common(remaining, measured);
    Nat::is_zero(&unmatched).then_some(rest)
}

/// [`bin_piece`] over the element carrier, restoring the element type every `List` value carries.
pub(super) fn list_piece(element: &Term, piece: Piece<'_>) -> Term {
    match piece {
        Piece::Whole(operand) => operand.clone(),
        Piece::Part(operand, lo, hi) => match &**operand {
            Subterm::Intrinsic(Intrinsic::List { element: _, items }) => {
                Term::intrinsic(Intrinsic::List {
                    element: element.clone(),
                    items: items[lo..hi].to_vec(),
                })
            }
            _ => unreachable!("a located window's segments are literal runs"),
        },
    }
}
