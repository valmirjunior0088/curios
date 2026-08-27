//! Folding an operation that distributes over a free-monoid spine.
//!
//! A `Bin`, a `List` and a `Nat` are all a concatenation of pieces, and an operation that is a homomorphism over that concatenation reduces piecewise — so a symbolic tail does not block the literal pieces around it. [`Shape`] is the peeled spine, and [`reduce_homomorphism`] is the fold over it that every such operation shares.

use {
    super::*,
    crate::{Intrinsic, Nat, Piece, ReduceError, Reducer, Subterm, Term},
    curios_utilities::Grain,
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
    debug_assert_eq!(*found, grain, "a located segment shares the value's grain");

    match grain {
        Grain::X => run
            .byte(local)
            .map(|byte| Subterm::Intrinsic(Intrinsic::Byte(byte))),
        Grain::B => run
            .bit(local)
            .map(|bit| Subterm::Intrinsic(Intrinsic::Bool(bit))),
    }
}

/// A window aligned to the seams of a concatenation is the run of operands between those seams: `slice([..xs, ..ys], 0, len(xs)) = xs` and `slice([..xs, ..ys], len(xs), len(ys)) = ys`, over *symbolic* operands — the case the literal-run locators above decline. The seams are found by measuring each operand the way `len` measures it and comparing the running sum with the window's start and end as reduced terms; a symbolic operand contributes its own `len`, and the comparison is structural, so two sums that are definitionally but not syntactically equal decline, which is the refusing direction. Sound for every value of the symbolic operands: a window whose start is exactly a prefix's length and whose end is exactly a longer prefix's length covers exactly the operands between, whatever those lengths are. `None` where no seam matches; the operands of the matched run otherwise, for the caller to concatenate.
pub(super) fn seam_window(
    reducer: &mut impl Reducer,
    operands: &[Term],
    start: &Term,
    length: &Term,
    measure: impl Fn(&Term) -> Intrinsic,
) -> Result<Option<Vec<Term>>, ReduceError> {
    let end = reducer.reduce_forced(Term::intrinsic(Intrinsic::nat_add(
        start.clone(),
        length.clone(),
    )))?;
    let mut prefix = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
    let mut begin = None;
    for (index, operand) in operands.iter().enumerate() {
        if begin.is_none() && prefix == *start {
            begin = Some(index);
        }
        if let Some(begin) = begin
            && prefix == end
        {
            return Ok(Some(operands[begin..index].to_vec()));
        }
        let measured = reducer.reduce_forced(Term::intrinsic(measure(operand)))?;
        prefix = reducer.reduce_forced(Term::intrinsic(Intrinsic::nat_add(prefix, measured)))?;
    }
    Ok(match begin {
        Some(begin) if prefix == end => Some(operands[begin..].to_vec()),
        _ => None,
    })
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
