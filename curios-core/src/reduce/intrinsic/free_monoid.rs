//! Folding an operation that distributes over a free-monoid spine.
//!
//! A `Bin`, a `List` and a `Nat` are all a concatenation of pieces, and an operation that is a homomorphism over that concatenation reduces piecewise — so a symbolic tail does not block the literal pieces around it. [`Shape`] is the peeled spine, and [`reduce_homomorphism`] is the fold over it that every such operation shares.
//!
//! **Two readers here stay one per carrier, for reasons that are not drift.** [`bin_shape`] and [`list_shape`] answer different types — `Shape<u8>` against `Shape<Term>` — and only the first is fallible, because only it materializes a run to charge for. [`bin_piece`] and [`list_piece`] differ in what they need from outside the value: a grain is a `Copy` tag [`FreeMonoid`] already carries, while a `List`'s element type is a *term* it does not, and every narrowed piece has to restate it. The readers that did fold into the carrier — the spine walk, the joined walk, the one-generator read — needed nothing from outside that the carrier did not already hold, which is the line between the two groups.

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
/// The single generator of a value whose measure is exactly one, or `None` for anything else.
///
/// [`single_generator`] over the element carrier: the one element of a value whose measure is exactly one.
///
impl FreeMonoid {
    /// The single generator of a value whose measure is exactly one, or `None` for anything else.
    ///
    /// **Why this rather than an inner `get`.** A located one-operand window gives `get(v, i) = get(w, 0)`, and rebuilding that inner read would need a proof that `0 < len(w)` — a proposition the caller's own bound does not state, and one a reducer may not construct: [`crate::Intrinsic::signature`]'s module documentation records that a reducer emitting proofs is the defect the bound fields exist to remove. Reading the generator out of the two shapes a one-element value takes needs no proof at all, and declining every other shape costs reductions and never an answer.
    ///
    /// Lives here rather than beside the carrier's other readers because reading a generator *out* is the grain's own seam ([`element_of_run`]), which is reduction's rather than the term representation's.
    pub(super) fn single_generator(self, operand: &Term) -> Option<Term> {
        match (self, &**operand) {
            (FreeMonoid::Bin(grain), Subterm::Intrinsic(Intrinsic::Bin(found, run)))
                if *found == grain && run.len(grain) == 1 =>
            {
                element_of_run(grain, run, 0).map(Term::from)
            }
            (
                FreeMonoid::Bin(grain),
                Subterm::Intrinsic(Intrinsic::BinAppend {
                    grain: found,
                    bin: base,
                    element,
                }),
            ) if *found == grain => match &**base {
                Subterm::Intrinsic(Intrinsic::Bin(empty, run))
                    if *empty == grain && run.len(grain) == 0 =>
                {
                    Some(element.clone())
                }
                _ => None,
            },
            (
                FreeMonoid::List,
                Subterm::Intrinsic(Intrinsic::List {
                    element: _,
                    items: elems,
                }),
            ) => match elems.as_slice() {
                [only] => Some(only.clone()),
                _ => None,
            },
            (
                FreeMonoid::List,
                Subterm::Intrinsic(Intrinsic::ListAppend {
                    element: _,
                    list: base,
                    item,
                }),
            ) => match &**base {
                Subterm::Intrinsic(Intrinsic::List {
                    element: _,
                    items: elems,
                }) if elems.is_empty() => Some(item.clone()),
                _ => None,
            },
            _ => None,
        }
    }
}

/// What a window came to: the pieces whose concatenation *is* it, or the measured total of a value it ran past.
///
/// `Past` is the concrete strategy's alone — the symbolic one never learns a total, so a window it cannot place is simply not found. It carries the bounds it read as well as the total, because only that strategy has them as indices and the refusal names all three.
pub(super) enum Windowed {
    Parts(Vec<Term>),
    Past {
        total: usize,
        start: usize,
        count: usize,
    },
}

impl FreeMonoid {
    /// The window of `count` generators at `start`, by whichever strategy reaches it — or `None` where neither does, which is where the caller falls through to its cons peel.
    ///
    /// **Two strategies, one entry, and that is the point.** They decide different things: the concrete one measures every operand, so it can validate the whole range and *split* the two at the edges, while the symbolic one never learns a total and can only take operands whole at exact seams. Written out at each call site they drifted — `ListGet` had the first and not the second where `ListSlice` had both, so a window at a symbolic seam was located and an index at the same seam was not. An arm that asks here cannot have one without the other.
    ///
    /// **The concrete strategy runs first and spends nothing**, which is what keeps a measurable value off the budget: `FreeMonoid::segments` reads each operand's length rather than reducing a `len` term for it. Only the fallback charges, one measure per operand, and only once the first has declined.
    ///
    /// `piece` and `measure` are the two things the carrier does not itself hold — a narrowed piece must restate a `List`'s element type, and a measure is spelled in the carrier's own `len`.
    pub(super) fn window(
        self,
        reducer: &mut impl Reducer,
        value: &Term,
        start: &Term,
        count: &Term,
        piece: impl Fn(Piece<'_>) -> Term,
        measure: impl Fn(&Term) -> Intrinsic,
    ) -> Result<Option<Windowed>, ReduceError> {
        // Every operand it covers whole is handed back untouched and shares its payload; only the two at the edges are narrowed, and everything outside the window is dropped without being read. A narrowed edge is narrowed *here* rather than rebuilt as a bounded node for the next pass to fold into exactly this, which is also what leaves this arm constructing no bounded node at all.
        if let (Some(start), Some(count)) = (as_index(start), as_index(count)) {
            match self.measured_window(value, start, count) {
                Some(Ok(pieces)) => {
                    let parts = pieces.into_iter().map(piece).collect::<Vec<Term>>();
                    reducer.spend(Cost::collection(parts.len() as u64))?;

                    return Ok(Some(Windowed::Parts(parts)));
                }
                Some(Err(total)) => {
                    return Ok(Some(Windowed::Past {
                        total,
                        start,
                        count,
                    }));
                }
                None => {}
            }
        }

        // A window on the seams of a symbolic concatenation. An append is one of those, which `FreeMonoid::concatenated` is what says.
        if let Some(operands) = self.concatenated(value)
            && let Some(run) = seam_window(reducer, &operands, start, count, measure)?
        {
            return Ok(Some(Windowed::Parts(run)));
        }

        Ok(None)
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
