use {
    super::{reduce, reduce_forced},
    crate::{
        Context, Nat, Peel, Prim, ReduceError, Subterm, Term, normalize_concat, peel_bin,
        peel_first_atom, peel_first_elem,
    },
    curios_base::{Flt, Grain, Int, PackedBin},
    num_traits::{ToPrimitive, Zero},
    std::cmp::Ordering,
};

/// Read an already-reduced `Nat` term as a concrete `usize` index — `None` when
/// it is still symbolic or too large to fit. The shared decode behind the
/// `Bin`/`Lst` `get`/`slice` bounds.
fn as_index(term: &Term) -> Option<usize> {
    term.as_nat().and_then(|n| n.to_big_uint()?.to_usize())
}

/// Reduce both operands of a `Bln` binary primitive, then either `fold` the two
/// literals or `rebuild` the neutral term. `Bln` has no numeric carrier at the
/// type level, so the fold reads the `true`/`false` constructors directly.
fn reduce_bln_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(bool, bool) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let left = reduce_forced(context, left.clone())?;
    let right = reduce_forced(context, right.clone())?;

    Ok(Subterm::Prim(match (left.as_bln(), right.as_bln()) {
        (Some(l), Some(r)) => Prim::Bln(fold(l, r)),
        _ => rebuild(left, right),
    }))
}

fn reduce_byte_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(u8, u8) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let left = reduce_forced(context, left.clone())?;
    let right = reduce_forced(context, right.clone())?;

    Ok(Subterm::Prim(match (&*left, &*right) {
        (Subterm::Prim(Prim::Byte(left)), Subterm::Prim(Prim::Byte(right))) => {
            Prim::Bln(fold(*left, *right))
        }
        _ => rebuild(left, right),
    }))
}

/// Reduce both operands of a `Nat` binary primitive, then either `fold` the two literals or
/// `rebuild` the neutral term from the reduced operands.
fn reduce_nat_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Nat, Nat) -> Option<Prim>,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let left = reduce_forced(context, left.clone())?;
    let right = reduce_forced(context, right.clone())?;

    let folded = match (left.as_nat(), right.as_nat()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => None,
    };

    Ok(Subterm::Prim(match folded {
        Some(prim) => prim,
        None => rebuild(left, right),
    }))
}

/// `Nat/div`/`Nat/rem`: like [`reduce_nat_binary`], but partial — a divisor
/// that reduces to literal zero is a reported error (the type-level mirror of
/// the runtime trap, following `BinGet`'s pattern), never a Rust panic. A
/// symbolic operand still rebuilds the neutral term.
fn reduce_nat_division(
    context: &mut Context,
    left: &Term,
    right: &Term,
    kind: &'static str,
    fold: impl FnOnce(Nat, Nat) -> Option<Nat>,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reduce_forced(context, left.clone())?;
    let right = reduce_forced(context, right.clone())?;

    if right
        .as_nat()
        .and_then(|divisor| divisor.to_big_uint())
        .is_some_and(|divisor| divisor.is_zero())
    {
        return Err(ReduceError::DivisionByZero { kind, span });
    }

    let folded = match (left.as_nat(), right.as_nat()) {
        (Some(l), Some(r)) => fold(l, r).map(Prim::Nat),
        _ => None,
    };

    Ok(Subterm::Prim(match folded {
        Some(prim) => prim,
        None => rebuild(left, right),
    }))
}

/// `Int` counterpart of [`reduce_nat_binary`]: fold both literal operands or
/// rebuild the neutral term. The fold is partial for the same reason — the
/// shifts decline a negative or oversized literal shift count (`None`); the
/// total ops just wrap their result in `Some`.
fn reduce_int_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Int, Int) -> Option<Prim>,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let left = reduce_forced(context, left.clone())?;
    let right = reduce_forced(context, right.clone())?;

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => None,
    };

    Ok(Subterm::Prim(match folded {
        Some(prim) => prim,
        None => rebuild(left, right),
    }))
}

/// `Int/div`/`Int/rem`: like [`reduce_int_binary`], but a divisor that
/// reduces to literal zero is a reported error — mathematically undefined,
/// following `BinGet`'s pattern. The fold itself is exact and total past
/// that: the type level pretends ℤ (see [`Int`]).
fn reduce_int_division(
    context: &mut Context,
    left: &Term,
    right: &Term,
    kind: &'static str,
    fold: impl FnOnce(Int, Int) -> Option<Int>,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reduce_forced(context, left.clone())?;
    let right = reduce_forced(context, right.clone())?;

    if right.as_int().is_some_and(|divisor| divisor.is_zero()) {
        return Err(ReduceError::DivisionByZero { kind, span });
    }

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r).map(Prim::Int),
        _ => None,
    };

    Ok(Subterm::Prim(match folded {
        Some(prim) => prim,
        None => rebuild(left, right),
    }))
}

/// `Flt` counterpart of [`reduce_nat_binary`].
fn reduce_flt_binary(
    context: &mut Context,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Flt, Flt) -> Prim,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let left = reduce_forced(context, left.clone())?;
    let right = reduce_forced(context, right.clone())?;

    Ok(Subterm::Prim(match (left.as_flt(), right.as_flt()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => rebuild(left, right),
    }))
}

/// Reduce the operand of a `Nat` unary primitive, then either `fold` the literal or `rebuild`
/// the neutral term from the reduced operand.
fn reduce_nat_unary(
    context: &mut Context,
    inner: &Term,
    fold: impl FnOnce(Nat) -> Option<Prim>,
    rebuild: impl FnOnce(Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let inner = reduce_forced(context, inner.clone())?;

    Ok(Subterm::Prim(match inner.as_nat().and_then(fold) {
        Some(prim) => prim,
        None => rebuild(inner),
    }))
}

/// `Int` counterpart of [`reduce_nat_unary`]. The fold's `None` rebuilds the
/// neutral term: with `Int` unbounded at the type level, a conversion of a
/// value the target cannot represent simply stays stuck.
fn reduce_int_unary(
    context: &mut Context,
    inner: &Term,
    fold: impl FnOnce(Int) -> Option<Prim>,
    rebuild: impl FnOnce(Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let inner = reduce_forced(context, inner.clone())?;

    Ok(Subterm::Prim(match inner.as_int().and_then(fold) {
        Some(prim) => prim,
        None => rebuild(inner),
    }))
}

/// `Flt` counterpart of [`reduce_nat_unary`].
fn reduce_flt_unary(
    context: &mut Context,
    inner: &Term,
    fold: impl FnOnce(Flt) -> Prim,
    rebuild: impl FnOnce(Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let inner = reduce_forced(context, inner.clone())?;

    Ok(Subterm::Prim(match inner.as_flt() {
        Some(value) => fold(value),
        None => rebuild(inner),
    }))
}

/// The structural outcome of comparing two `Nat`s. The whole comparison family
/// (`eql`/`neq`/`lt`/`lte`/`gt`/`gte`) reads this one result; each op differs only
/// in how it maps the outcome to a `bool`. `Le`/`Ge` record a *non-strict* bound
/// the operands force without pinning equality (e.g. `succ x ≥ 1`), letting
/// `lt`/`gte` decide where `eql` still cannot; `Stuck` is undecidable, and the
/// op's neutral term is rebuilt.
#[derive(Debug, PartialEq)]
enum Comparison {
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Stuck,
}

fn from_ordering(ordering: Ordering) -> Comparison {
    match ordering {
        Ordering::Less => Comparison::Lt,
        Ordering::Equal => Comparison::Eq,
        Ordering::Greater => Comparison::Gt,
    }
}

/// The `Nat` eliminator's structural comparison, specialized to the flat `BigUint`
/// successor spine: the floors stand in for peeling successors, so no recursion is
/// needed and two literals decide in one `BigUint` compare (the literal fold folds
/// into the shared-inner shortcut). It decides ONLY where the answer is forced and
/// is `Stuck` otherwise — a sound partial decision procedure, the shared body of
/// the whole comparison family. (The `lt` partner of the `Unary` eliminator's
/// successor peel; for `Bin`/`Lst` the same `Comparison` shape would recurse via
/// `uncons`.)
///
/// Returns the operands with their shared successor floor peeled off, so an
/// *undecided* comparison still rebuilds a normalized neutral: `cmp(x + m, y + m)`
/// and `cmp(x, y)` reduce to the same term, which conversion needs (e.g.
/// `Lt(a, succ b) ≡ Lt(succ a, succ(succ b))`).
fn compare_nat(
    context: &mut Context,
    left: Term,
    right: Term,
) -> Result<(Comparison, Term, Term), ReduceError> {
    let (sl, il) = Nat::decompose(&reduce_forced(context, left)?);
    let (sr, ir) = Nat::decompose(&reduce_forced(context, right)?);

    // Same inner ⇒ the floors alone decide: `cmp(x + sl, x + sr) = cmp(sl, sr)`
    // (so `lt(pred, succ pred) = true`). Two literals — inner `0` on both sides —
    // also land here: this is the O(1) literal fold. Otherwise, whichever side
    // keeps successors past the shared floor is larger *iff* the other bottomed
    // out at literal zero (`inner ≥ 0`); equal floors with one zero inner give a
    // non-strict bound (`a ≤ b`/`a ≥ b`) the strict/`gte`/`lte` reads still use;
    // anything else is undecidable.
    let outcome = if il == ir {
        from_ordering(sl.cmp(&sr))
    } else {
        match sl.cmp(&sr) {
            Ordering::Greater if Nat::is_zero(&ir) => Comparison::Gt,
            Ordering::Less if Nat::is_zero(&il) => Comparison::Lt,
            Ordering::Equal if Nat::is_zero(&il) => Comparison::Le,
            Ordering::Equal if Nat::is_zero(&ir) => Comparison::Ge,
            _ => Comparison::Stuck,
        }
    };

    let m = sl.clone().min(sr.clone());
    Ok((
        outcome,
        Nat::rebuild(sl - &m, il),
        Nat::rebuild(sr - &m, ir),
    ))
}

/// Reduce a `Nat` comparison through the shared structural body [`compare_nat`].
/// `read` projects the outcome to this op's boolean (or `None` when the operands
/// do not decide it), in which case the neutral term is rebuilt from the peeled
/// operands so undecided comparisons land in a normal form.
fn reduce_nat_compare(
    context: &mut Context,
    left: &Term,
    right: &Term,
    read: impl FnOnce(Comparison) -> Option<bool>,
    rebuild: impl FnOnce(Term, Term) -> Prim,
) -> Result<Subterm, ReduceError> {
    let (outcome, left, right) = compare_nat(context, left.clone(), right.clone())?;

    Ok(match read(outcome) {
        Some(value) => Subterm::Prim(Prim::Bln(value)),
        None => Subterm::Prim(rebuild(left, right)),
    })
}

/// The free-monoid product structure of a reduced carrier value, the view a monoid
/// homomorphism (`len`/`map`) distributes over: a literal run of generators `L`
/// (bytes for `Bin`, elements for `Lst`), an n-ary `Concat` of operands to recurse
/// on, an `Append` of a base and one appended generator, or an `Opaque` node (a
/// variable / slice) the homomorphism leaves neutral. `Empty` is just `Literal(∅)`.
enum Shape<L> {
    Literal(Vec<L>),
    Concat(Vec<Term>),
    Append(Term, Term),
    Opaque(Term),
}

/// Classify a reduced `Bin` value into its product shape (generators are bytes).
fn bin_shape(grain: Grain, value: Term) -> Shape<u8> {
    match Term::unwrap_or_clone(value) {
        Subterm::Prim(Prim::Bin(found, value)) if found == grain => Shape::Literal(match grain {
            Grain::B => (0..value.bit_length())
                .map(|index| u8::from(value.bit(index).unwrap()))
                .collect(),
            Grain::X => value.to_bytes().unwrap(),
        }),
        Subterm::Prim(Prim::BinConcat(found, operands)) if found == grain => {
            Shape::Concat(operands)
        }
        Subterm::Prim(Prim::BinAppend(found, base, atom)) if found == grain => {
            Shape::Append(base, atom)
        }
        other => Shape::Opaque(other.into()),
    }
}

/// Classify a reduced `Lst` value into its product shape (generators are elements).
fn lst_shape(value: Term) -> Shape<Term> {
    match Term::unwrap_or_clone(value) {
        Subterm::Prim(Prim::Lst(elems)) => Shape::Literal(elems),
        Subterm::Prim(Prim::LstConcat(_, operands)) => Shape::Concat(operands),
        Subterm::Prim(Prim::LstAppend(_, base, elem)) => Shape::Append(base, elem),
        other => Shape::Opaque(other.into()),
    }
}

/// The shared driver for a free-monoid homomorphism `h` — the one place its
/// distribution law lives, so a carrier physically cannot forget a case. A literal
/// run maps via `literal`; a
/// concatenation recurses `h` over its operands and folds the images with `combine`;
/// an append combines `h(base)` with the appended generator via `append`; an opaque
/// value stays neutral, rebuilt by `node` (which also builds `h(sub)` to recurse).
/// `len` and `map` differ only in those four slots. The built image is
/// reduced, so the homomorphism is eager.
fn reduce_homomorphism<L>(
    context: &mut Context,
    shape: Shape<L>,
    literal: impl Fn(Vec<L>) -> Term,
    combine: impl Fn(Vec<Term>) -> Term,
    append: impl Fn(Term, Term) -> Term,
    node: impl Fn(Term) -> Term,
) -> Result<Subterm, ReduceError> {
    let built = match shape {
        Shape::Literal(run) => literal(run),
        Shape::Concat(operands) => combine(operands.into_iter().map(node).collect()),
        Shape::Append(base, generator) => append(node(base), generator),
        Shape::Opaque(value) => return Ok(Term::unwrap_or_clone(node(value))),
    };

    reduce(context, built).map(Term::unwrap_or_clone)
}

/// `Σ` over a run of `Nat` images — the `combine` of the `len` homomorphism into
/// `(ℕ, +, 0)`. `NatAdd`'s successor peeling carries the count out of a symbolic
/// spine.
fn nat_sum(images: Vec<Term>) -> Term {
    images
        .into_iter()
        .rev()
        .fold(Term::prim(Prim::Nat(Nat::Zero)), |acc, image| {
            Term::prim(Prim::nat_add(image, acc))
        })
}

pub(crate) fn reduce_prim(context: &mut Context, prim: &Prim) -> Result<Subterm, ReduceError> {
    match prim {
        Prim::BlnType => Ok(Subterm::Prim(Prim::BlnType)),
        Prim::Bln(value) => Ok(Subterm::Prim(Prim::Bln(*value))),
        Prim::BlnAnd(left, right) => {
            reduce_bln_binary(context, left, right, |l, r| l && r, Prim::BlnAnd)
        }
        Prim::BlnOr(left, right) => {
            reduce_bln_binary(context, left, right, |l, r| l || r, Prim::BlnOr)
        }
        Prim::BlnXor(left, right) => {
            reduce_bln_binary(context, left, right, |l, r| l != r, Prim::BlnXor)
        }
        Prim::BlnEql(left, right) => {
            reduce_bln_binary(context, left, right, |l, r| l == r, Prim::BlnEql)
        }
        Prim::BlnNeq(left, right) => {
            reduce_bln_binary(context, left, right, |l, r| l != r, Prim::BlnNeq)
        }
        Prim::NatType => Ok(Subterm::Prim(Prim::NatType)),
        Prim::Nat(Nat::Zero) => Ok(Subterm::Prim(Prim::Nat(Nat::Zero))),
        Prim::Nat(Nat::Succ(spine, inner)) => {
            let inner = reduce_forced(context, inner.clone())?;

            Ok(match Term::unwrap_or_clone(inner) {
                Subterm::Prim(Prim::Nat(Nat::Succ(j, tail))) => {
                    Subterm::Prim(Prim::Nat(Nat::Succ(spine.clone() + j, tail)))
                }
                inner => Subterm::Prim(Prim::Nat(Nat::Succ(spine.clone(), Term::from(inner)))),
            })
        }
        Prim::ByteType => Ok(Subterm::Prim(Prim::ByteType)),
        Prim::Byte(value) => Ok(Subterm::Prim(Prim::Byte(*value))),
        Prim::ByteToNat(inner) => {
            let inner = reduce_forced(context, inner.clone())?;
            Ok(Subterm::Prim(match &*inner {
                Subterm::Prim(Prim::Byte(value)) => Prim::Nat(Nat::new(usize::from(*value))),
                _ => Prim::ByteToNat(inner),
            }))
        }
        Prim::NatToByte(inner) => {
            let inner = reduce_forced(context, inner.clone())?;
            if let Subterm::Prim(Prim::ByteToNat(byte)) = &*inner {
                return reduce(context, byte.clone()).map(Term::unwrap_or_clone);
            }

            Ok(Subterm::Prim(
                match inner.as_nat().and_then(|value| {
                    let value = value.to_big_uint()?;
                    Some((value.to_u32().unwrap_or(0) & 0xff) as u8)
                }) {
                    Some(value) => Prim::Byte(value),
                    None => Prim::NatToByte(inner),
                },
            ))
        }
        Prim::ByteEql(l, r) => reduce_byte_binary(context, l, r, |l, r| l == r, Prim::ByteEql),
        Prim::ByteLt(l, r) => reduce_byte_binary(context, l, r, |l, r| l < r, Prim::ByteLt),
        Prim::ByteLte(l, r) => reduce_byte_binary(context, l, r, |l, r| l <= r, Prim::ByteLte),
        Prim::ByteGt(l, r) => reduce_byte_binary(context, l, r, |l, r| l > r, Prim::ByteGt),
        Prim::ByteGte(l, r) => reduce_byte_binary(context, l, r, |l, r| l >= r, Prim::ByteGte),
        Prim::NatEql(left, right) => reduce_nat_compare(
            context,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(true),
                Comparison::Lt | Comparison::Gt => Some(false),
                Comparison::Le | Comparison::Ge | Comparison::Stuck => None,
            },
            Prim::nat_eql,
        ),
        // Handles are opaque runtime tokens with no compile-time literal form,
        // so this only ever reduces its operands and rebuilds — it never folds.
        Prim::IoEql(left, right) => {
            reduce_nat_binary(context, left, right, |_, _| None, Prim::IoEql)
        }
        Prim::NatNeq(left, right) => reduce_nat_compare(
            context,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(false),
                Comparison::Lt | Comparison::Gt => Some(true),
                Comparison::Le | Comparison::Ge | Comparison::Stuck => None,
            },
            Prim::nat_neq,
        ),
        // Addition combines the literal successor floors and recurses on the
        // symbolic tails: `(il + sl) + (ir + sr) = (il + ir) + (sl + sr)`. A zero
        // tail drops by the unit law; two non-zero tails stay as the neutral `add`.
        // Lifting the combined floor back out with `rebuild` is what makes the unit
        // laws and successor peeling *definitional* — `Nat/add(j + 1, m)` normalises
        // to `(Nat/add(j, m)) + 1` — so an indexed constructor's target meets the
        // motive's expected index without unification. The floor only ever moves
        // outward, so the rewrite terminates.
        Prim::NatAdd(left, right) => {
            let left = reduce_forced(context, left.clone())?;
            let right = reduce_forced(context, right.clone())?;
            let (sl, il) = Nat::decompose(&left);
            let (sr, ir) = Nat::decompose(&right);

            let inner = match (Nat::is_zero(&il), Nat::is_zero(&ir)) {
                (false, false) => Term::prim(Prim::nat_add(il, ir)),
                (true, _) => ir,
                (_, true) => il,
            };
            Ok(Term::unwrap_or_clone(Nat::rebuild(sl + sr, inner)))
        }
        // `(il + sl) - k` for a literal subtrahend `k`: when the floor covers it
        // (`sl ≥ k`) the borrow stays within the floor and the tail `il ≥ 0` is
        // untouched, so the result is `il + (sl - k)`. The subtraction twin of the
        // addition floor law (and it gives `x - 0 = x` for any `x`, the unit law
        // `NatAdd` already has): it turns the `succ e - 1` bounds the cons-slice rule
        // produces back into `e`, so a slice over a symbolic cons keeps reducing
        // instead of stalling on a stuck `Nat/sub`. Both-literal subtraction with `k`
        // overshooting the floor truncates to zero; anything else stays neutral.
        Prim::NatSub(left, right) => {
            let left = reduce_forced(context, left.clone())?;
            let right = reduce_forced(context, right.clone())?;
            let (sl, il) = Nat::decompose(&left);
            let (k, ir) = Nat::decompose(&right);

            if Nat::is_zero(&ir) {
                if sl >= k {
                    return Ok(Term::unwrap_or_clone(Nat::rebuild(sl - k, il)));
                }
                if Nat::is_zero(&il) {
                    return Ok(Subterm::Prim(Prim::Nat(Nat::Zero)));
                }
            }
            Ok(Subterm::Prim(Prim::nat_sub(left, right)))
        }
        // Multiplication distributes a literal factor over the other operand's
        // successor floor: `(it + st) · c = (it · c) + (st · c)`. The literal floors
        // multiply out; the symbolic tail rides as a neutral `mul` (or drops when it
        // is zero, which folds two literals). The multiplicative twin of `NatAdd`'s
        // floor law — it lets `n · k` extract `k` past a symbolic `n` (`(x + 1) · 2 =
        // x · 2 + 2`) the same way `n + k` does. Whichever side is the literal drives;
        // two symbolic operands have no literal factor, so the product stays neutral.
        Prim::NatMul(left, right) => {
            let left = reduce_forced(context, left.clone())?;
            let right = reduce_forced(context, right.clone())?;
            let (sl, il) = Nat::decompose(&left);
            let (sr, ir) = Nat::decompose(&right);

            if Nat::is_zero(&ir) {
                // right is the literal `sr`: distribute over the left floor.
                let inner = match Nat::is_zero(&il) {
                    true => il,
                    false => Term::prim(Prim::nat_mul(il, right.clone())),
                };
                return Ok(Term::unwrap_or_clone(Nat::rebuild(sl * sr, inner)));
            }
            if Nat::is_zero(&il) {
                // left is the literal `sl`, right symbolic: distribute over the right
                // floor.
                let inner = Term::prim(Prim::nat_mul(left.clone(), ir));
                return Ok(Term::unwrap_or_clone(Nat::rebuild(sl * sr, inner)));
            }
            Ok(Subterm::Prim(Prim::nat_mul(left, right)))
        }
        Prim::NatLt(left, right) => reduce_nat_compare(
            context,
            left,
            right,
            |c| match c {
                Comparison::Lt => Some(true),
                Comparison::Eq | Comparison::Gt | Comparison::Ge => Some(false),
                Comparison::Le | Comparison::Stuck => None,
            },
            Prim::nat_lt,
        ),
        Prim::NatDiv(left, right) => reduce_nat_division(
            context,
            left,
            right,
            "Nat/div",
            Nat::checked_div,
            Prim::NatDiv,
        ),
        Prim::NatRem(left, right) => reduce_nat_division(
            context,
            left,
            right,
            "Nat/rem",
            Nat::checked_rem,
            Prim::NatRem,
        ),
        Prim::NatGt(left, right) => reduce_nat_compare(
            context,
            left,
            right,
            |c| match c {
                Comparison::Gt => Some(true),
                Comparison::Eq | Comparison::Lt | Comparison::Le => Some(false),
                Comparison::Ge | Comparison::Stuck => None,
            },
            Prim::nat_gt,
        ),
        Prim::NatLte(left, right) => reduce_nat_compare(
            context,
            left,
            right,
            |c| match c {
                Comparison::Lt | Comparison::Eq | Comparison::Le => Some(true),
                Comparison::Gt => Some(false),
                Comparison::Ge | Comparison::Stuck => None,
            },
            Prim::nat_lte,
        ),
        Prim::NatGte(left, right) => reduce_nat_compare(
            context,
            left,
            right,
            |c| match c {
                Comparison::Gt | Comparison::Eq | Comparison::Ge => Some(true),
                Comparison::Lt => Some(false),
                Comparison::Le | Comparison::Stuck => None,
            },
            Prim::nat_gte,
        ),
        // Bitwise ops fold on the unbounded ℕ the type level pretends: `and`,
        // `or`, `xor` on the infinite binary expansion, `shl` as `· 2^n` and
        // `shr` as `⌊·/2^n⌋`. The runtime's 31-bit carrier (truncating `shl`,
        // logical `shr`) is imposed only in the backend, never here.
        Prim::NatAnd(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_bitand(r).map(Prim::Nat),
            Prim::NatAnd,
        ),
        Prim::NatOr(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_bitor(r).map(Prim::Nat),
            Prim::NatOr,
        ),
        Prim::NatXor(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_bitxor(r).map(Prim::Nat),
            Prim::NatXor,
        ),
        Prim::NatShl(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_shl(r).map(Prim::Nat),
            Prim::NatShl,
        ),
        Prim::NatShr(left, right) => reduce_nat_binary(
            context,
            left,
            right,
            |l, r| l.checked_shr(r).map(Prim::Nat),
            Prim::NatShr,
        ),
        Prim::IntType => Ok(Subterm::Prim(Prim::IntType)),
        Prim::Int(value) => Ok(Subterm::Prim(Prim::Int(value.clone()))),
        Prim::IntEql(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left == right)),
            Prim::IntEql,
        ),
        Prim::IntNeq(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left != right)),
            Prim::IntNeq,
        ),
        Prim::IntAdd(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left + right)),
            Prim::IntAdd,
        ),
        Prim::IntSub(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left - right)),
            Prim::IntSub,
        ),
        Prim::IntMul(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left * right)),
            Prim::IntMul,
        ),
        Prim::IntDiv(left, right) => reduce_int_division(
            context,
            left,
            right,
            "Int/div",
            Int::checked_div,
            Prim::IntDiv,
        ),
        Prim::IntRem(left, right) => reduce_int_division(
            context,
            left,
            right,
            "Int/rem",
            Int::checked_rem,
            Prim::IntRem,
        ),
        Prim::IntLt(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left < right)),
            Prim::IntLt,
        ),
        Prim::IntGt(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left > right)),
            Prim::IntGt,
        ),
        Prim::IntLte(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left <= right)),
            Prim::IntLte,
        ),
        Prim::IntGte(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Bln(left >= right)),
            Prim::IntGte,
        ),
        // Bitwise ops fold on the unbounded ℤ the type level pretends: `and`,
        // `or`, `xor` on the infinite two's-complement expansion, `shl` as
        // `· 2^n` and `shr` as the arithmetic `⌊·/2^n⌋`. The runtime's signed
        // 31-bit carrier (truncating `shl`, `shr_s`) is imposed only in the
        // backend, never here.
        Prim::IntAnd(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left & right)),
            Prim::IntAnd,
        ),
        Prim::IntOr(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left | right)),
            Prim::IntOr,
        ),
        Prim::IntXor(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| Some(Prim::Int(left ^ right)),
            Prim::IntXor,
        ),
        Prim::IntShl(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| left.checked_shl(right).map(Prim::Int),
            Prim::IntShl,
        ),
        Prim::IntShr(left, right) => reduce_int_binary(
            context,
            left,
            right,
            |left, right| left.checked_shr(right).map(Prim::Int),
            Prim::IntShr,
        ),
        Prim::FltType => Ok(Subterm::Prim(Prim::FltType)),
        Prim::Flt(flt) => Ok(Subterm::Prim(Prim::Flt(*flt))),
        Prim::FltAdd(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left + right),
            Prim::FltAdd,
        ),
        Prim::FltSub(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left - right),
            Prim::FltSub,
        ),
        Prim::FltMul(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left * right),
            Prim::FltMul,
        ),
        Prim::FltDiv(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left / right),
            Prim::FltDiv,
        ),
        // `%` on `f32` is C `fmod`: `x - trunc(x / y) * y`, sign of the dividend —
        // the same value the `cont -> wasm` expansion computes.
        Prim::FltRem(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left % right),
            Prim::FltRem,
        ),
        Prim::FltMin(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left.min(right)),
            Prim::FltMin,
        ),
        Prim::FltMax(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Flt(left.max(right)),
            Prim::FltMax,
        ),
        Prim::FltEql(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.eql(right)),
            Prim::FltEql,
        ),
        Prim::FltNeq(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.neq(right)),
            Prim::FltNeq,
        ),
        Prim::FltLt(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.lt(right)),
            Prim::FltLt,
        ),
        Prim::FltGt(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.gt(right)),
            Prim::FltGt,
        ),
        Prim::FltLte(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.lte(right)),
            Prim::FltLte,
        ),
        Prim::FltGte(left, right) => reduce_flt_binary(
            context,
            left,
            right,
            |left, right| Prim::Bln(left.gte(right)),
            Prim::FltGte,
        ),
        Prim::FltNeg(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(-flt), Prim::FltNeg)
        }
        Prim::FltAbs(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.abs()), Prim::FltAbs)
        }
        Prim::FltSqrt(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.sqrt()), Prim::FltSqrt)
        }
        Prim::FltFloor(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.floor()), Prim::FltFloor)
        }
        Prim::FltCeil(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.ceil()), Prim::FltCeil)
        }
        Prim::FltTrunc(inner) => {
            reduce_flt_unary(context, inner, |flt| Prim::Flt(flt.trunc()), Prim::FltTrunc)
        }
        Prim::FltNearest(inner) => reduce_flt_unary(
            context,
            inner,
            |flt| Prim::Flt(flt.nearest()),
            Prim::FltNearest,
        ),
        Prim::FltToLeBytes(inner) => reduce_flt_unary(
            context,
            inner,
            |v| {
                Prim::Bin(
                    Grain::X,
                    PackedBin::from_bytes(v.to_f32().to_le_bytes().to_vec()),
                )
            },
            Prim::FltToLeBytes,
        ),
        // Assemble a float from an exact 4-byte literal; anything else —
        // symbolic, or a wrong-length literal (the runtime trap) — stays stuck.
        Prim::FltOfLeBytes(inner) => {
            let inner = reduce_forced(context, inner.clone())?;
            Ok(Subterm::Prim(match &*inner {
                Subterm::Prim(Prim::Bin(Grain::X, bytes)) if bytes.len(Grain::X) == 4 => {
                    Prim::Flt(Flt::from_f32(f32::from_le_bytes(
                        bytes.as_bytes().unwrap().try_into().unwrap(),
                    )))
                }
                _ => Prim::FltOfLeBytes(inner),
            }))
        }
        Prim::NatToInt(inner) => reduce_nat_unary(
            context,
            inner,
            |v| {
                let bits = v.to_big_uint()?.to_u32().unwrap_or(0) & 0x7FFF_FFFF;
                let signed = if bits >= 0x4000_0000 {
                    bits as i64 - (1i64 << 31)
                } else {
                    bits as i64
                };
                Some(Prim::Int(Int::new(signed)))
            },
            Prim::NatToInt,
        ),
        Prim::NatToFlt(inner) => reduce_nat_unary(
            context,
            inner,
            |v| {
                Some(Prim::Flt(Flt::from_f32(
                    v.to_big_uint()?.to_f64().unwrap_or(0.0) as f32,
                )))
            },
            Prim::NatToFlt,
        ),
        Prim::IntToNat(inner) => reduce_int_unary(
            context,
            inner,
            |v| Some(Prim::Nat(Nat::new(v.to_i32()? as u32))),
            Prim::IntToNat,
        ),
        Prim::IntToFlt(inner) => reduce_int_unary(
            context,
            inner,
            |v| Some(Prim::Flt(Flt::from_f32(v.to_i32()? as f32))),
            Prim::IntToFlt,
        ),
        Prim::FltToNat(inner) => reduce_flt_unary(
            context,
            inner,
            |flt| Prim::Nat(Nat::new(flt.to_f32() as u32)),
            Prim::FltToNat,
        ),
        // Exact — the type level pretends ℤ, so no finite float is out of
        // range. A float with no integer part at all (NaN, ±inf) folds to
        // nothing and the term stays stuck.
        Prim::FltToInt(inner) => {
            let inner = reduce_forced(context, inner.clone())?;
            Ok(Subterm::Prim(
                match inner.as_flt().and_then(|v| Int::from_f32_trunc(v.to_f32())) {
                    Some(int) => Prim::Int(int),
                    None => Prim::FltToInt(inner),
                },
            ))
        }
        Prim::BinType(Grain::X) => Ok(Subterm::Prim(Prim::BinType(Grain::X))),
        Prim::Bin(Grain::X, bytes) => Ok(Subterm::Prim(Prim::Bin(Grain::X, bytes.clone()))),
        Prim::BinLen(Grain::X, bin) => {
            let bin = reduce_forced(context, bin.clone())?;
            reduce_homomorphism(
                context,
                bin_shape(Grain::X, bin),
                |run| Term::prim(Prim::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::prim(Prim::nat_add(
                        Term::prim(Prim::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::prim(Prim::bin_len(Grain::X, sub)),
            )
        }
        Prim::BinEql(Grain::X, left, right) => {
            let left = reduce_forced(context, left.clone())?;
            let right = reduce_forced(context, right.clone())?;

            // Reflexivity: any value equals itself. Catches a shared variable, which
            // the peel below cannot — a bare variable is not a `Bin`-valued prim.
            if left == right {
                return Ok(Subterm::Prim(Prim::Bln(true)));
            }

            // Structural decision via the free-monoid peel (`core::spine`): a
            // peeled-equal pair is `true`, a definite byte or length clash is `false`
            // (so `eql([1] ++ x, [2] ++ x) = false` regardless of `x`). Anything the
            // peel leaves undecided stays neutral — the same conservative seam
            // conversion reads, so the fold only ever strengthens, never weakens.
            if let (Subterm::Prim(l), Subterm::Prim(r)) = (&*left, &*right) {
                match peel_bin(l, r) {
                    Some(Peel::Equal) => return Ok(Subterm::Prim(Prim::Bln(true))),
                    Some(Peel::Clash) => return Ok(Subterm::Prim(Prim::Bln(false))),
                    Some(Peel::Continue(..)) | Some(Peel::Stuck) | None => {}
                }
            }

            Ok(Subterm::Prim(Prim::bin_eql(
                Grain::X,
                Term::unwrap_or_clone(left),
                Term::unwrap_or_clone(right),
            )))
        }
        Prim::BinGet(Grain::X, bin, index) => {
            let bin = reduce_forced(context, bin.clone())?;
            let index_reduced = reduce_forced(context, index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (Subterm::Prim(Prim::Bin(Grain::X, bytes)), Some(i)) = (&*bin, i) {
                return match bytes.byte(i) {
                    Some(byte) => Ok(Subterm::Prim(Prim::Byte(byte))),
                    None => Err(ReduceError::BinGetOutOfBounds {
                        len: bytes.len(Grain::X),
                        index: i,
                        span: index.span(),
                    }),
                };
            }
            // The cons head's byte: `get(append(\\, byte), 0) = byte` — the base
            // case of the cons-peel below, and the partner of `BinSlice`'s rules.
            if let Subterm::Prim(Prim::BinAppend(Grain::X, base, byte)) = &*bin
                && let Subterm::Prim(Prim::Bin(Grain::X, b)) = &**base
                && b.is_empty()
                && let Subterm::Prim(Prim::Nat(Nat::Zero)) = &*index_reduced
            {
                return reduce(context, byte.clone()).map(Term::unwrap_or_clone);
            }
            // A get over a cons spine peels one byte per `0`/`succ` index step:
            //   `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            if let Some((head, tail)) = peel_first_atom(Grain::X, &bin) {
                match &*index_reduced {
                    Subterm::Prim(Prim::Nat(Nat::Zero)) => {
                        let zero = Term::prim(Prim::Nat(Nat::Zero));
                        return reduce(context, Term::prim(Prim::bin_get(Grain::X, head, zero)))
                            .map(Term::unwrap_or_clone);
                    }
                    Subterm::Prim(Prim::Nat(Nat::Succ(..))) => {
                        let one = Term::prim(Prim::Nat(Nat::new(1usize)));
                        let prev = Term::prim(Prim::nat_sub(index_reduced.clone(), one));
                        return reduce(context, Term::prim(Prim::bin_get(Grain::X, tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Prim(Prim::bin_get(Grain::X, bin, index_reduced)))
        }
        Prim::BinSlice(Grain::X, bin, start, end) => {
            let bin = reduce_forced(context, bin.clone())?;
            let start_reduced = reduce_forced(context, start.clone())?;
            let end_reduced = reduce_forced(context, end.clone())?;
            // The full slice is the identity: `slice(b, 0, len b) = b`. Sound even
            // for a symbolic `b` — `0..len` is always in range, never trapping —
            // and the runtime partner of `core::spine`'s window-collapse: it lets a
            // bare full-window `BinSlice` reduce to its base, so a `Bin/slice` over
            // the whole value costs no copy and converts against the base directly.
            if matches!(&*start_reduced, Subterm::Prim(Prim::Nat(Nat::Zero)))
                && matches!(&*end_reduced, Subterm::Prim(Prim::BinLen(Grain::X, whole)) if *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            // The empty slice is empty: `slice(b, i, i) = \\`. The dual of the
            // full-window identity and equally sound — an empty range yields no
            // bytes regardless of `b`, and never equates two distinct literals.
            // It lets a codepoint take collapse its zero-width base (`take 0`) to
            // the empty string even over a symbolic cons.
            if start_reduced == end_reduced {
                return Ok(Subterm::Prim(Prim::Bin(Grain::X, PackedBin::empty())));
            }
            // A nested slice reassociates: `slice(slice(b, p, q), i, j) =
            // slice(b, p + i, p + j)`. Sound for the in-range bounds real call
            // sites produce; reassociating the window lets a codepoint walk
            // collapse a `slice(drop1(b), ..)` back onto `b`.
            if let Subterm::Prim(Prim::BinSlice(Grain::X, inner, p, _q)) = &*bin {
                let lo = Term::prim(Prim::nat_add(p.clone(), start_reduced.clone()));
                let hi = Term::prim(Prim::nat_add(p.clone(), end_reduced.clone()));
                let flattened = Term::prim(Prim::bin_slice(Grain::X, inner.clone(), lo, hi));
                return reduce(context, flattened).map(Term::unwrap_or_clone);
            }
            let s = as_index(&start_reduced);
            let e = as_index(&end_reduced);
            // A concrete slice of a literal run.
            if let (Subterm::Prim(Prim::Bin(Grain::X, bytes)), Some(s), Some(e)) = (&*bin, s, e) {
                return match bytes.slice(Grain::X, s, e) {
                    Some(slice) => Ok(Subterm::Prim(Prim::Bin(Grain::X, slice))),
                    None => Err(ReduceError::BinSliceOutOfRange {
                        len: bytes.len(Grain::X),
                        start: s,
                        end: e,
                        span: start.span().or_else(|| end.span()),
                    }),
                };
            }
            // A slice over a cons spine peels one byte per `0`/`succ` boundary
            // step — the reduction partner of the `Utf8` cons the validity proofs
            // walk:  `slice(cons(h, t), 0, succ e) = h ++ slice(t, 0, e)`  and
            // `slice(cons(h, t), succ s, e) = slice(t, s, e - 1)`.
            if let Some((head, tail)) = peel_first_atom(Grain::X, &bin) {
                let dec = |n: &Term| {
                    let one = Term::prim(Prim::Nat(Nat::new(1usize)));
                    Term::prim(Prim::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Prim(Prim::Nat(Nat::Zero)),
                        Subterm::Prim(Prim::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::prim(Prim::Nat(Nat::Zero));
                        let rest =
                            Term::prim(Prim::bin_slice(Grain::X, tail, zero, dec(&end_reduced)));
                        let consed = Term::prim(Prim::bin_concat(Grain::X, [head, rest]));
                        return reduce(context, consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Prim(Prim::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::prim(Prim::bin_slice(
                            Grain::X,
                            tail,
                            dec(&start_reduced),
                            dec(&end_reduced),
                        ));
                        return reduce(context, sliced).map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Prim(Prim::bin_slice(
                Grain::X,
                bin,
                start_reduced,
                end_reduced,
            )))
        }
        Prim::BinAppend(Grain::X, bin, byte) => {
            let bin = reduce_forced(context, bin.clone())?;
            let byte = reduce_forced(context, byte.clone())?;
            // A concrete byte is taken mod 256 — its low 8 bits — matching the
            // runtime's packed-`i8` store and the optimizer's `as u8`. A symbolic
            // operand has no `as_nat`, so it stays stuck rather than truncating.
            let n = match &*byte {
                Subterm::Prim(Prim::Byte(byte)) => Some(*byte),
                _ => None,
            };
            Ok(match (Term::unwrap_or_clone(bin), n) {
                (Subterm::Prim(Prim::Bin(Grain::X, bytes)), Some(n)) => {
                    Subterm::Prim(Prim::Bin(Grain::X, bytes.append_byte(n).unwrap()))
                }
                (bin, _) => Subterm::Prim(Prim::bin_append(Grain::X, bin, byte)),
            })
        }
        Prim::BinConcat(Grain::X, operands) => {
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reduce_forced(context, e.clone()))
                .collect::<Result<_, _>>()?;
            // Normalise by the monoid unit/associativity laws — drop the empty
            // bytestring (so `concat(\\, a)`/`concat(a, \\)` collapse to `a`), merge
            // adjacent literal runs, collapse a lone operand. The definitional
            // partner of `peel_bin`'s `\\`-handling (`core::spine`); see
            // `normalize_concat`.
            fn literal(operand: &Term) -> Option<&[u8]> {
                match &**operand {
                    Subterm::Prim(Prim::Bin(Grain::X, bytes)) => bytes.as_bytes(),
                    _ => None,
                }
            }
            Ok(normalize_concat(
                reduced,
                literal,
                |bytes| Subterm::Prim(Prim::Bin(Grain::X, PackedBin::from_bytes(bytes))),
                |kept| Subterm::Prim(Prim::BinConcat(Grain::X, kept)),
            ))
        }
        Prim::BinType(Grain::B) => Ok(Subterm::Prim(Prim::BinType(Grain::B))),
        Prim::Bin(Grain::B, bits) => Ok(Subterm::Prim(Prim::Bin(Grain::B, bits.clone()))),
        Prim::BinLen(Grain::B, bin) => {
            let bin = reduce_forced(context, bin.clone())?;
            reduce_homomorphism(
                context,
                bin_shape(Grain::B, bin),
                |run| Term::prim(Prim::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::prim(Prim::nat_add(
                        Term::prim(Prim::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::prim(Prim::bin_len(Grain::B, sub)),
            )
        }
        Prim::BinEql(Grain::B, left, right) => {
            let left = reduce_forced(context, left.clone())?;
            let right = reduce_forced(context, right.clone())?;
            if left == right {
                return Ok(Subterm::Prim(Prim::Bln(true)));
            }
            if let (Subterm::Prim(l), Subterm::Prim(r)) = (&*left, &*right) {
                match peel_bin(l, r) {
                    Some(Peel::Equal) => return Ok(Subterm::Prim(Prim::Bln(true))),
                    Some(Peel::Clash) => return Ok(Subterm::Prim(Prim::Bln(false))),
                    Some(Peel::Continue(..)) | Some(Peel::Stuck) | None => {}
                }
            }
            Ok(Subterm::Prim(Prim::BinEql(Grain::B, left, right)))
        }
        Prim::BinGet(Grain::B, bin, index) => {
            let span = index.span();
            let bin = reduce_forced(context, bin.clone())?;
            let index_reduced = reduce_forced(context, index.clone())?;
            if let (Subterm::Prim(Prim::Bin(Grain::B, bits)), Some(index)) =
                (&*bin, as_index(&index_reduced))
            {
                return bits
                    .bit(index)
                    .map(|bit| Subterm::Prim(Prim::Bln(bit)))
                    .ok_or_else(|| ReduceError::BinGetOutOfBounds {
                        len: bits.bit_length(),
                        index,
                        span,
                    });
            }
            if let Some((head, tail)) = peel_first_atom(Grain::B, &bin) {
                match &*index_reduced {
                    Subterm::Prim(Prim::Nat(Nat::Zero)) => {
                        return reduce(
                            context,
                            Term::prim(Prim::bin_get(
                                Grain::B,
                                head,
                                Term::prim(Prim::Nat(Nat::Zero)),
                            )),
                        )
                        .map(Term::unwrap_or_clone);
                    }
                    Subterm::Prim(Prim::Nat(Nat::Succ(..))) => {
                        let prev = Term::prim(Prim::nat_sub(
                            index_reduced.clone(),
                            Term::prim(Prim::Nat(Nat::new(1usize))),
                        ));
                        return reduce(context, Term::prim(Prim::bin_get(Grain::B, tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Prim(Prim::BinGet(Grain::B, bin, index_reduced)))
        }
        Prim::BinSlice(Grain::B, bin, start, end) => {
            let span = start.span().or_else(|| end.span());
            let bin = reduce_forced(context, bin.clone())?;
            let start_reduced = reduce_forced(context, start.clone())?;
            let end_reduced = reduce_forced(context, end.clone())?;
            if matches!(&*start_reduced, Subterm::Prim(Prim::Nat(Nat::Zero)))
                && matches!(&*end_reduced, Subterm::Prim(Prim::BinLen(Grain::B, whole)) if *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            if start_reduced == end_reduced {
                return Ok(Subterm::Prim(Prim::Bin(Grain::B, PackedBin::empty())));
            }
            if let Subterm::Prim(Prim::BinSlice(Grain::B, inner, offset, _)) = &*bin {
                let lo = Term::prim(Prim::nat_add(offset.clone(), start_reduced.clone()));
                let hi = Term::prim(Prim::nat_add(offset.clone(), end_reduced.clone()));
                return reduce(
                    context,
                    Term::prim(Prim::bin_slice(Grain::B, inner.clone(), lo, hi)),
                )
                .map(Term::unwrap_or_clone);
            }
            if let (Subterm::Prim(Prim::Bin(Grain::B, bits)), Some(start), Some(end)) =
                (&*bin, as_index(&start_reduced), as_index(&end_reduced))
            {
                return bits
                    .slice(Grain::B, start, end)
                    .map(|bits| Subterm::Prim(Prim::Bin(Grain::B, bits)))
                    .ok_or_else(|| ReduceError::BinSliceOutOfRange {
                        len: bits.bit_length(),
                        start,
                        end,
                        span,
                    });
            }
            if let Some((head, tail)) = peel_first_atom(Grain::B, &bin) {
                let dec = |n: &Term| {
                    Term::prim(Prim::nat_sub(
                        n.clone(),
                        Term::prim(Prim::Nat(Nat::new(1usize))),
                    ))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Prim(Prim::Nat(Nat::Zero)),
                        Subterm::Prim(Prim::Nat(Nat::Succ(..))),
                    ) => {
                        let rest = Term::prim(Prim::bin_slice(
                            Grain::B,
                            tail,
                            Term::prim(Prim::Nat(Nat::Zero)),
                            dec(&end_reduced),
                        ));
                        return reduce(
                            context,
                            Term::prim(Prim::bin_concat(Grain::B, [head, rest])),
                        )
                        .map(Term::unwrap_or_clone);
                    }
                    (Subterm::Prim(Prim::Nat(Nat::Succ(..))), _) => {
                        return reduce(
                            context,
                            Term::prim(Prim::bin_slice(
                                Grain::B,
                                tail,
                                dec(&start_reduced),
                                dec(&end_reduced),
                            )),
                        )
                        .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Prim(Prim::BinSlice(
                Grain::B,
                bin,
                start_reduced,
                end_reduced,
            )))
        }
        Prim::BinAppend(Grain::B, bin, bit) => {
            let bin = reduce_forced(context, bin.clone())?;
            let bit = reduce_forced(context, bit.clone())?;
            Ok(Subterm::Prim(match (&*bin, bit.as_bln()) {
                (Subterm::Prim(Prim::Bin(Grain::B, bits)), Some(bit)) => {
                    Prim::Bin(Grain::B, bits.append_bit(bit))
                }
                _ => Prim::BinAppend(Grain::B, bin, bit),
            }))
        }
        Prim::BinConcat(Grain::B, operands) => {
            let mut operands = operands
                .iter()
                .map(|operand| reduce_forced(context, operand.clone()))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .filter(|operand| {
                    !matches!(&**operand, Subterm::Prim(Prim::Bin(Grain::B, bits)) if bits.is_empty())
                })
                .collect::<Vec<_>>();
            let literals = operands
                .iter()
                .map(|operand| match &**operand {
                    Subterm::Prim(Prim::Bin(Grain::B, bits)) => Some(bits),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>();
            Ok(match literals {
                Some(literals) => Subterm::Prim(Prim::Bin(Grain::B, PackedBin::concat(literals))),
                None if operands.len() == 1 => Term::unwrap_or_clone(operands.pop().unwrap()),
                None => Subterm::Prim(Prim::BinConcat(Grain::B, operands)),
            })
        }
        Prim::LstType(elem) => {
            let elem = reduce(context, elem.clone())?;
            Ok(Subterm::Prim(Prim::lst_type(elem)))
        }
        Prim::Lst(elems) => {
            let elems = elems
                .iter()
                .map(|e| reduce(context, e.clone()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Subterm::Prim(Prim::Lst(elems)))
        }
        Prim::LstLen(type_, list) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce_forced(context, list.clone())?;
            reduce_homomorphism(
                context,
                lst_shape(list),
                |run| Term::prim(Prim::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::prim(Prim::nat_add(
                        Term::prim(Prim::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::prim(Prim::lst_len(type_.clone(), sub)),
            )
        }
        Prim::LstGet(type_, list, index) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce_forced(context, list.clone())?;
            let index_reduced = reduce_forced(context, index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (Subterm::Prim(Prim::Lst(elems)), Some(i)) = (&*list, i) {
                let len = elems.len();
                return match elems.get(i).cloned().map(Term::unwrap_or_clone) {
                    Some(elem) => Ok(elem),
                    None => Err(ReduceError::LstGetOutOfBounds {
                        len,
                        index: i,
                        span: index.span(),
                    }),
                };
            }
            // A get over a cons spine peels one element per `0`/`succ` index step,
            // the `Lst` twin of `BinGet`'s byte peel:
            //   `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            if let Some((head, tail)) = peel_first_elem(&list) {
                match &*index_reduced {
                    Subterm::Prim(Prim::Nat(Nat::Zero)) => return Ok(Term::unwrap_or_clone(head)),
                    Subterm::Prim(Prim::Nat(Nat::Succ(..))) => {
                        let one = Term::prim(Prim::Nat(Nat::new(1usize)));
                        let prev = Term::prim(Prim::nat_sub(index_reduced.clone(), one));
                        return reduce(context, Term::prim(Prim::lst_get(type_, tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Prim(Prim::lst_get(type_, list, index_reduced)))
        }
        Prim::LstSlice(type_, list, start, end) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce_forced(context, list.clone())?;
            let start_reduced = reduce_forced(context, start.clone())?;
            let end_reduced = reduce_forced(context, end.clone())?;
            // The full slice is the identity: `slice(a, 0, len a) = a`. Sound even for
            // a symbolic `a` — `0..len` is always in range — the `Lst` twin of
            // `BinSlice`'s full-window identity, letting a full-length `Lst/slice`
            // reduce to its base instead of copying.
            if matches!(&*start_reduced, Subterm::Prim(Prim::Nat(Nat::Zero)))
                && matches!(&*end_reduced, Subterm::Prim(Prim::LstLen(_, whole)) if *whole == list)
            {
                return Ok(Term::unwrap_or_clone(list));
            }
            // The empty slice is empty: `slice(a, i, i) = []`. Sound for a symbolic
            // `a` — an empty range yields no elements regardless — and the base case
            // the cons peel below bottoms out on (the `Lst` twin of `BinSlice`'s
            // empty-slice identity).
            if start_reduced == end_reduced {
                return Ok(Subterm::Prim(Prim::Lst(Vec::new())));
            }
            // A nested slice reassociates: `slice(slice(a, p, q), i, j) =
            // slice(a, p + i, p + j)`. Sound for the in-range bounds real call sites
            // produce — the `Lst` twin of `BinSlice`'s window reassociation — so a
            // slice of a slice over a symbolic base collapses to one slice.
            if let Subterm::Prim(Prim::LstSlice(_inner_ty, inner, p, _q)) = &*list {
                let lo = Term::prim(Prim::nat_add(p.clone(), start_reduced.clone()));
                let hi = Term::prim(Prim::nat_add(p.clone(), end_reduced.clone()));
                let flattened = Term::prim(Prim::lst_slice(type_.clone(), inner.clone(), lo, hi));
                return reduce(context, flattened).map(Term::unwrap_or_clone);
            }
            let s = as_index(&start_reduced);
            let e = as_index(&end_reduced);
            // A concrete slice of a literal run.
            if let (Subterm::Prim(Prim::Lst(elems)), Some(s), Some(e)) = (&*list, s, e) {
                return match elems.get(s..e) {
                    Some(slice) => Ok(Subterm::Prim(Prim::Lst(slice.to_vec()))),
                    None => Err(ReduceError::LstSliceOutOfRange {
                        len: elems.len(),
                        start: s,
                        end: e,
                        span: start.span().or_else(|| end.span()),
                    }),
                };
            }
            // A slice over a cons spine peels one element per `0`/`succ` boundary
            // step, the `Lst` twin of `BinSlice`'s byte peel:
            //   `slice(cons(h, t), 0, succ e) = [h] ++ slice(t, 0, e)`  and
            //   `slice(cons(h, t), succ s, e) = slice(t, s - 1, e - 1)`.
            if let Some((head, tail)) = peel_first_elem(&list) {
                let dec = |n: &Term| {
                    let one = Term::prim(Prim::Nat(Nat::new(1usize)));
                    Term::prim(Prim::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Prim(Prim::Nat(Nat::Zero)),
                        Subterm::Prim(Prim::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::prim(Prim::Nat(Nat::Zero));
                        let rest = Term::prim(Prim::lst_slice(
                            type_.clone(),
                            tail,
                            zero,
                            dec(&end_reduced),
                        ));
                        let head_singleton: Term = Subterm::Prim(Prim::Lst(vec![head])).into();
                        let consed = Term::prim(Prim::lst_concat(type_, [head_singleton, rest]));
                        return reduce(context, consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Prim(Prim::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::prim(Prim::lst_slice(
                            type_,
                            tail,
                            dec(&start_reduced),
                            dec(&end_reduced),
                        ));
                        return reduce(context, sliced).map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Prim(Prim::lst_slice(
                type_,
                list,
                start_reduced,
                end_reduced,
            )))
        }
        Prim::LstAppend(type_, list, elem) => {
            let type_ = reduce(context, type_.clone())?;
            let list = reduce_forced(context, list.clone())?;
            let elem = reduce(context, elem.clone())?;
            Ok(match Term::unwrap_or_clone(list) {
                Subterm::Prim(Prim::Lst(mut elems)) => {
                    elems.push(elem);
                    Subterm::Prim(Prim::Lst(elems))
                }
                list => Subterm::Prim(Prim::lst_append(type_, list, elem)),
            })
        }
        Prim::LstConcat(type_, operands) => {
            let type_ = reduce(context, type_.clone())?;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reduce_forced(context, e.clone()))
                .collect::<Result<_, _>>()?;
            // The `Lst` twin of `BinConcat` normalisation: drop the empty list (so
            // `concat([], a)`/`concat(a, [])` collapse to `a`), merge adjacent literal
            // runs, collapse a lone operand — the definitional partner of `peel_arr`'s
            // `[]`-handling (`core::spine`); see `normalize_concat`.
            fn literal(operand: &Term) -> Option<&[Term]> {
                match &**operand {
                    Subterm::Prim(Prim::Lst(elems)) => Some(elems.as_slice()),
                    _ => None,
                }
            }
            Ok(normalize_concat(
                reduced,
                literal,
                |elems| Subterm::Prim(Prim::Lst(elems)),
                |kept| Subterm::Prim(Prim::lst_concat(type_, kept)),
            ))
        }
        // `map`: the eliminator homomorphism. The literal case applies `f`
        // elementwise; the spine cases distribute (`map f (concat segs) =
        // concat (map f segs)`, `map f (append b x) = append (map f b) (f x)`) — the
        // same normal form a structural `foldr (\x ih. f x :: ih) []` produces, so
        // map-based proofs still reduce. A symbolic list stays neutral (the
        // `Opaque` case), so there is no unfold of a variable.
        Prim::LstMap(a, b, lst, f) => {
            let a = reduce(context, a.clone())?;
            let b = reduce(context, b.clone())?;
            let lst = reduce_forced(context, lst.clone())?;
            let f = reduce(context, f.clone())?;
            reduce_homomorphism(
                context,
                lst_shape(lst),
                |elems| {
                    Term::prim(Prim::Lst(
                        elems
                            .into_iter()
                            .map(|x| Term::apply(f.clone(), [x]))
                            .collect(),
                    ))
                },
                |images| Term::prim(Prim::lst_concat(b.clone(), images)),
                |base_map, generator| {
                    Term::prim(Prim::lst_append(
                        b.clone(),
                        base_map,
                        Term::apply(f.clone(), [generator]),
                    ))
                },
                |sub| Term::prim(Prim::lst_map(a.clone(), b.clone(), sub, f.clone())),
            )
        }
        // The handle type and handle tokens are inert values, like `Nat`/`Nat(_)`.
        Prim::IoType => Ok(Subterm::Prim(Prim::IoType)),
        Prim::Io(token) => Ok(Subterm::Prim(Prim::Io(*token))),
        Prim::IoExit(_, code) => Err(ReduceError::IoAtTypeLevel {
            kind: "IoExit".to_string(),
            span: code.span(),
        }),
        // A store-described host call never reduces at the type level — the
        // effect cannot happen at compile time; it becomes a host call only
        // at erasure.
        Prim::Foreign(function, args) => Err(ReduceError::IoAtTypeLevel {
            kind: function.name.clone(),
            span: args.first().and_then(|arg| arg.span()),
        }),
        Prim::CellType(elem) => {
            let elem = reduce(context, elem.clone())?;
            Ok(Subterm::Prim(Prim::cell_type(elem)))
        }
        Prim::Cell(_, init) => Err(ReduceError::IoAtTypeLevel {
            kind: "Cell".to_string(),
            span: init.span(),
        }),
        Prim::CellSet(_, cell, _) => Err(ReduceError::IoAtTypeLevel {
            kind: "CellSet".to_string(),
            span: cell.span(),
        }),
        Prim::CellGet(_, cell) => Err(ReduceError::IoAtTypeLevel {
            kind: "CellGet".to_string(),
            span: cell.span(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use {
        super::{compare_nat, from_ordering},
        crate::{Context, Nat, Prim, Term},
        std::time::Duration,
    };

    fn lit(n: u32) -> Term {
        Term::prim(Prim::Nat(Nat::new(n as usize)))
    }

    // Soundness gate: the structural body agrees with the host ordering on every
    // pair of literals — the decidable closed case where the two routes into a
    // `Comparison` (the shared-inner shortcut vs. the host `cmp`) must coincide.
    #[test]
    fn compare_nat_agrees_with_literal_ordering() {
        let mut context = Context::new(Duration::from_millis(50));
        let samples = [0u32, 1, 2, 5, 42, 128, 255, 256, 1000];
        for &m in &samples {
            for &n in &samples {
                assert_eq!(
                    compare_nat(&mut context, lit(m), lit(n))
                        .expect("reduces")
                        .0,
                    from_ordering(m.cmp(&n)),
                    "compare_nat disagreed with the literal ordering on ({m}, {n})",
                );
            }
        }
    }
}
