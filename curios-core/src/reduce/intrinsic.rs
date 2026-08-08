use {
    super::{ReduceError, Reducer},
    crate::{
        Intrinsic, Nat, Peel, Subterm, Term, normalize_concat, peel_bin, peel_first_atom,
        peel_first_elem,
    },
    curios_base::{Grain, Int, PackedBin, int_rotl, int_rotr, nat_rotl, nat_rotr},
    num_traits::{ToPrimitive, Zero},
    std::cmp::Ordering,
};

/// Read an already-reduced `Nat` term as a concrete `usize` index — `None` when it is still symbolic or too large to fit. The shared decode behind the `Bin`/`Lst` `get`/`slice` bounds.
fn as_index(term: &Term) -> Option<usize> {
    term.as_nat().and_then(|n| n.to_big_uint()?.to_usize())
}

/// Reduce both operands of a `Bool` binary intrinsic, then either `fold` the two literals or `rebuild` the neutral term. `Bool` has no numeric carrier at the type level, so the fold reads the `true`/`false` constructors directly.
fn reduce_bool_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(bool, bool) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    Ok(Subterm::Intrinsic(
        match (left.as_bool(), right.as_bool()) {
            (Some(l), Some(r)) => Intrinsic::Bool(fold(l, r)),
            _ => rebuild(left, right),
        },
    ))
}

fn reduce_byte_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(u8, u8) -> bool,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    Ok(Subterm::Intrinsic(match (&*left, &*right) {
        (Subterm::Intrinsic(Intrinsic::Byte(left)), Subterm::Intrinsic(Intrinsic::Byte(right))) => {
            Intrinsic::Bool(fold(*left, *right))
        }
        _ => rebuild(left, right),
    }))
}

/// Reduce both operands of a `Nat` binary intrinsic, then either `fold` the two literals or `rebuild` the neutral term from the reduced operands.
fn reduce_nat_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Nat, Nat) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_nat(), right.as_nat()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Nat/div`/`Nat/rem`: like [`reduce_nat_binary`], but partial — a divisor that reduces to literal zero is a reported error (the type-level mirror of the runtime trap, following `BinGet`'s pattern), never a Rust panic. A symbolic operand still rebuilds the neutral term.
fn reduce_nat_division(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    kind: &'static str,
    fold: impl FnOnce(Nat, Nat) -> Option<Nat>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    if right
        .as_nat()
        .and_then(|divisor| divisor.to_big_uint())
        .is_some_and(|divisor| divisor.is_zero())
    {
        return Err(ReduceError::DivisionByZero { kind, span });
    }

    let folded = match (left.as_nat(), right.as_nat()) {
        (Some(l), Some(r)) => fold(l, r).map(Intrinsic::Nat),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Int` counterpart of [`reduce_nat_binary`]: fold both literal operands or rebuild the neutral term. The fold is partial for the same reason — the shifts decline a negative or oversized literal shift count (`None`); the total ops just wrap their result in `Some`.
fn reduce_int_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    fold: impl FnOnce(Int, Int) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Int/div`/`Int/rem`: like [`reduce_int_binary`], but a divisor that reduces to literal zero is a reported error — mathematically undefined, following `BinGet`'s pattern. The fold itself is exact and total past that: the type level pretends ℤ (see [`Int`]).
fn reduce_int_division(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    kind: &'static str,
    fold: impl FnOnce(Int, Int) -> Option<Int>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let span = right.span().or_else(|| left.span());
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    if right.as_int().is_some_and(|divisor| divisor.is_zero()) {
        return Err(ReduceError::DivisionByZero { kind, span });
    }

    let folded = match (left.as_int(), right.as_int()) {
        (Some(l), Some(r)) => fold(l, r).map(Intrinsic::Int),
        _ => None,
    };

    Ok(Subterm::Intrinsic(match folded {
        Some(intrinsic) => intrinsic,
        None => rebuild(left, right),
    }))
}

/// `Flt` operations are opaque at the type level: operands reduce, the operation never folds — `FltAdd(1.0, 1.0)` is its own normal form, so `Eq(@Flt, 1.0 + 1.0, 2.0)` is deliberately unprovable. IEEE semantics inside definitional equality is a soundness hazard with no consumer: the corpus proves nothing about floats, and IEEE equality identifies values (`0.0`, `-0.0`) that `FltToLeBytes` observes apart — the exact shape the singleton guard exists to forbid. Runtime-faithful constant folding belongs downstream in `curios-ersd`'s partial evaluator, which is untrusted. The rule this instance establishes: an intrinsic needs a fold here only if a type or a proof can depend on its value.
fn reduce_flt_binary(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let left = reducer.reduce_forced(left.clone())?;
    let right = reducer.reduce_forced(right.clone())?;

    Ok(Subterm::Intrinsic(rebuild(left, right)))
}

/// Reduce the operand of a `Nat` unary intrinsic, then either `fold` the literal or `rebuild` the neutral term from the reduced operand.
fn reduce_nat_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    fold: impl FnOnce(Nat) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(match inner.as_nat().and_then(fold) {
        Some(intrinsic) => intrinsic,
        None => rebuild(inner),
    }))
}

/// `Int` counterpart of [`reduce_nat_unary`]. The fold's `None` rebuilds the neutral term: with `Int` unbounded at the type level, a conversion of a value the target cannot represent simply stays stuck.
fn reduce_int_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    fold: impl FnOnce(Int) -> Option<Intrinsic>,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(match inner.as_int().and_then(fold) {
        Some(intrinsic) => intrinsic,
        None => rebuild(inner),
    }))
}

/// [`reduce_flt_binary`]'s unary counterpart: opaque at the type level, the operand reduces and the operation always rebuilds.
fn reduce_flt_unary(
    reducer: &mut impl Reducer,
    inner: &Term,
    rebuild: impl FnOnce(Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let inner = reducer.reduce_forced(inner.clone())?;

    Ok(Subterm::Intrinsic(rebuild(inner)))
}

/// The structural outcome of comparing two `Nat`s. The whole comparison family (`eql`/`neq`/`lt`/`lte`/`gt`/`gte`) reads this one result; each op differs only in how it maps the outcome to a `bool`. `Le`/`Ge` record a *non-strict* bound the operands force without pinning equality (e.g. `succ x ≥ 1`), letting `lt`/`gte` decide where `eql` still cannot; `Stuck` is undecidable, and the op's neutral term is rebuilt.
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

/// The `Nat` eliminator's structural comparison, specialized to the flat `BigUint` successor spine: the floors stand in for peeling successors, so no recursion is needed and two literals decide in one `BigUint` compare (the literal fold folds into the shared-inner shortcut). It decides ONLY where the answer is forced and is `Stuck` otherwise — a sound partial decision procedure, the shared body of the whole comparison family. (The `lt` partner of the `Unary` eliminator's successor peel; for `Bin`/`Lst` the same `Comparison` shape would recurse via `uncons`.)
///
/// Returns the operands with their shared successor floor peeled off, so an *undecided* comparison still rebuilds a normalized neutral: `cmp(x + m, y + m)` and `cmp(x, y)` reduce to the same term, which conversion needs (e.g. `Lt(a, succ b) ≡ Lt(succ a, succ(succ b))`).
fn compare_nat(
    reducer: &mut impl Reducer,
    left: Term,
    right: Term,
) -> Result<(Comparison, Term, Term), ReduceError> {
    let (sl, il) = Nat::decompose(&reducer.reduce_forced(left)?);
    let (sr, ir) = Nat::decompose(&reducer.reduce_forced(right)?);

    // Same inner ⇒ the floors alone decide: `cmp(x + sl, x + sr) = cmp(sl, sr)` (so `lt(pred, succ pred) = true`). Two literals — inner `0` on both sides — also land here: this is the O(1) literal fold. Otherwise, whichever side keeps successors past the shared floor is larger *iff* the other bottomed out at literal zero (`inner ≥ 0`); equal floors with one zero inner give a non-strict bound (`a ≤ b`/`a ≥ b`) the strict/`gte`/`lte` reads still use; anything else is undecidable.
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

/// Reduce a `Nat` comparison through the shared structural body [`compare_nat`]. `read` projects the outcome to this op's boolean (or `None` when the operands do not decide it), in which case the neutral term is rebuilt from the peeled operands so undecided comparisons land in a normal form.
fn reduce_nat_compare(
    reducer: &mut impl Reducer,
    left: &Term,
    right: &Term,
    read: impl FnOnce(Comparison) -> Option<bool>,
    rebuild: impl FnOnce(Term, Term) -> Intrinsic,
) -> Result<Subterm, ReduceError> {
    let (outcome, left, right) = compare_nat(reducer, left.clone(), right.clone())?;

    Ok(match read(outcome) {
        Some(value) => Subterm::Intrinsic(Intrinsic::Bool(value)),
        None => Subterm::Intrinsic(rebuild(left, right)),
    })
}

/// The free-monoid product structure of a reduced carrier value, the view a monoid homomorphism (`len`/`map`) distributes over: a literal run of generators `L` (bytes for `Bin`, elements for `Lst`), an n-ary `Concat` of operands to recurse on, an `Append` of a base and one appended generator, or an `Opaque` node (a variable / slice) the homomorphism leaves neutral. `Empty` is just `Literal(∅)`.
enum Shape<L> {
    Literal(Vec<L>),
    Concat(Vec<Term>),
    Append(Term, Term),
    Opaque(Term),
}

/// Classify a reduced `Bin` value into its product shape (generators are bytes).
fn bin_shape(grain: Grain, value: Term) -> Shape<u8> {
    match Term::unwrap_or_clone(value) {
        Subterm::Intrinsic(Intrinsic::Bin(found, value)) if found == grain => {
            Shape::Literal(match grain {
                Grain::B => (0..value.bit_length())
                    .map(|index| u8::from(value.bit(index).unwrap()))
                    .collect(),
                Grain::X => value.to_bytes().unwrap(),
            })
        }
        Subterm::Intrinsic(Intrinsic::BinConcat(found, operands)) if found == grain => {
            Shape::Concat(operands)
        }
        Subterm::Intrinsic(Intrinsic::BinAppend(found, base, atom)) if found == grain => {
            Shape::Append(base, atom)
        }
        other => Shape::Opaque(other.into()),
    }
}

/// Classify a reduced `Lst` value into its product shape (generators are elements).
fn lst_shape(value: Term) -> Shape<Term> {
    match Term::unwrap_or_clone(value) {
        Subterm::Intrinsic(Intrinsic::Lst(_, elems)) => Shape::Literal(elems),
        Subterm::Intrinsic(Intrinsic::LstConcat(_, operands)) => Shape::Concat(operands),
        Subterm::Intrinsic(Intrinsic::LstAppend(_, base, elem)) => Shape::Append(base, elem),
        other => Shape::Opaque(other.into()),
    }
}

/// The shared driver for a free-monoid homomorphism `h` — the one place its distribution law lives, so a carrier physically cannot forget a case. A literal run maps via `literal`; a concatenation recurses `h` over its operands and folds the images with `combine`; an append combines `h(base)` with the appended generator via `append`; an opaque value stays neutral, rebuilt by `node` (which also builds `h(sub)` to recurse). `len` and `map` differ only in those four slots. The built image is reduced, so the homomorphism is eager.
fn reduce_homomorphism<L>(
    reducer: &mut impl Reducer,
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

    reducer.reduce(built).map(Term::unwrap_or_clone)
}

/// `Σ` over a run of `Nat` images — the `combine` of the `len` homomorphism into `(ℕ, +, 0)`. `NatAdd`'s successor peeling carries the count out of a symbolic spine.
fn nat_sum(images: Vec<Term>) -> Term {
    images
        .into_iter()
        .rev()
        .fold(Term::intrinsic(Intrinsic::Nat(Nat::Zero)), |acc, image| {
            Term::intrinsic(Intrinsic::nat_add(image, acc))
        })
}

pub fn reduce_intrinsic(
    reducer: &mut impl Reducer,
    intrinsic: &Intrinsic,
) -> Result<Subterm, ReduceError> {
    match intrinsic {
        Intrinsic::BoolType => Ok(Subterm::Intrinsic(Intrinsic::BoolType)),
        Intrinsic::Bool(value) => Ok(Subterm::Intrinsic(Intrinsic::Bool(*value))),
        Intrinsic::BoolAnd(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l && r, Intrinsic::BoolAnd)
        }
        Intrinsic::BoolOr(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l || r, Intrinsic::BoolOr)
        }
        Intrinsic::BoolXor(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l != r, Intrinsic::BoolXor)
        }
        Intrinsic::BoolEql(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l == r, Intrinsic::BoolEql)
        }
        Intrinsic::BoolNeq(left, right) => {
            reduce_bool_binary(reducer, left, right, |l, r| l != r, Intrinsic::BoolNeq)
        }
        Intrinsic::NatType => Ok(Subterm::Intrinsic(Intrinsic::NatType)),
        Intrinsic::Nat(Nat::Zero) => Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))),
        Intrinsic::Nat(Nat::Succ(spine, inner)) => {
            let inner = reducer.reduce_forced(inner.clone())?;

            Ok(match Term::unwrap_or_clone(inner) {
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(j, tail))) => {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(spine.clone() + j, tail)))
                }
                inner => {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(spine.clone(), Term::from(inner))))
                }
            })
        }
        Intrinsic::ByteType => Ok(Subterm::Intrinsic(Intrinsic::ByteType)),
        Intrinsic::Byte(value) => Ok(Subterm::Intrinsic(Intrinsic::Byte(*value))),
        Intrinsic::ByteToNat(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            Ok(Subterm::Intrinsic(match &*inner {
                Subterm::Intrinsic(Intrinsic::Byte(value)) => {
                    Intrinsic::Nat(Nat::new(usize::from(*value)))
                }
                _ => Intrinsic::ByteToNat(inner),
            }))
        }
        Intrinsic::NatToByte(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            if let Subterm::Intrinsic(Intrinsic::ByteToNat(byte)) = &*inner {
                return reducer.reduce(byte.clone()).map(Term::unwrap_or_clone);
            }

            Ok(Subterm::Intrinsic(
                match inner.as_nat().and_then(|value| {
                    let value = value.to_big_uint()?;
                    Some((value.to_u32()? & 0xff) as u8)
                }) {
                    Some(value) => Intrinsic::Byte(value),
                    None => Intrinsic::NatToByte(inner),
                },
            ))
        }
        Intrinsic::ByteEql(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l == r, Intrinsic::ByteEql)
        }
        Intrinsic::ByteLt(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l < r, Intrinsic::ByteLt)
        }
        Intrinsic::ByteLte(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l <= r, Intrinsic::ByteLte)
        }
        Intrinsic::ByteGt(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l > r, Intrinsic::ByteGt)
        }
        Intrinsic::ByteGte(l, r) => {
            reduce_byte_binary(reducer, l, r, |l, r| l >= r, Intrinsic::ByteGte)
        }
        Intrinsic::NatEql(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(true),
                Comparison::Lt | Comparison::Gt => Some(false),
                Comparison::Le | Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::nat_eql,
        ),
        // Handles are opaque runtime tokens with no compile-time literal form, so this only ever reduces its operands and rebuilds — it never folds.
        Intrinsic::HandleEql(left, right) => {
            reduce_nat_binary(reducer, left, right, |_, _| None, Intrinsic::HandleEql)
        }
        Intrinsic::NatNeq(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Eq => Some(false),
                Comparison::Lt | Comparison::Gt => Some(true),
                Comparison::Le | Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::nat_neq,
        ),
        // Addition combines the literal successor floors and recurses on the symbolic tails: `(il + sl) + (ir + sr) = (il + ir) + (sl + sr)`. A zero tail drops by the unit law; two non-zero tails stay as the neutral `add`. Lifting the combined floor back out with `rebuild` is what makes the unit laws and successor peeling *definitional* — `Nat/add(j + 1, m)` normalises to `(Nat/add(j, m)) + 1` — so an indexed constructor's target meets the motive's expected index without unification. The floor only ever moves outward, so the rewrite terminates.
        Intrinsic::NatAdd(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            let (sl, il) = Nat::decompose(&left);
            let (sr, ir) = Nat::decompose(&right);

            let inner = match (Nat::is_zero(&il), Nat::is_zero(&ir)) {
                (false, false) => Term::intrinsic(Intrinsic::nat_add(il, ir)),
                (true, _) => ir,
                (_, true) => il,
            };
            Ok(Term::unwrap_or_clone(Nat::rebuild(sl + sr, inner)))
        }
        // `(il + sl) - k` for a literal subtrahend `k`: when the floor covers it (`sl ≥ k`) the borrow stays within the floor and the tail `il ≥ 0` is untouched, so the result is `il + (sl - k)`. The subtraction twin of the addition floor law (and it gives `x - 0 = x` for any `x`, the unit law `NatAdd` already has): it turns the `succ e - 1` bounds the cons-slice rule produces back into `e`, so a slice over a symbolic cons keeps reducing instead of stalling on a stuck `Nat/sub`. Both-literal subtraction with `k` overshooting the floor truncates to zero; anything else stays neutral.
        Intrinsic::NatSub(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            let (sl, il) = Nat::decompose(&left);
            let (k, ir) = Nat::decompose(&right);

            if Nat::is_zero(&ir) {
                if sl >= k {
                    return Ok(Term::unwrap_or_clone(Nat::rebuild(sl - k, il)));
                }
                if Nat::is_zero(&il) {
                    return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)));
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::nat_sub(left, right)))
        }
        // Multiplication distributes a literal factor over the other operand's successor floor: `(it + st) · c = (it · c) + (st · c)`. The literal floors multiply out; the symbolic tail rides as a neutral `mul` (or drops when it is zero, which folds two literals). The multiplicative twin of `NatAdd`'s floor law — it lets `n · k` extract `k` past a symbolic `n` (`(x + 1) · 2 = x · 2 + 2`) the same way `n + k` does. Whichever side is the literal drives; two symbolic operands have no literal factor, so the product stays neutral.
        Intrinsic::NatMul(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            let (sl, il) = Nat::decompose(&left);
            let (sr, ir) = Nat::decompose(&right);

            if Nat::is_zero(&ir) {
                // right is the literal `sr`: distribute over the left floor.
                let inner = match Nat::is_zero(&il) {
                    true => il,
                    false => Term::intrinsic(Intrinsic::nat_mul(il, right.clone())),
                };
                return Ok(Term::unwrap_or_clone(Nat::rebuild(sl * sr, inner)));
            }
            if Nat::is_zero(&il) {
                // left is the literal `sl`, right symbolic: distribute over the right floor.
                let inner = Term::intrinsic(Intrinsic::nat_mul(left.clone(), ir));
                return Ok(Term::unwrap_or_clone(Nat::rebuild(sl * sr, inner)));
            }
            Ok(Subterm::Intrinsic(Intrinsic::nat_mul(left, right)))
        }
        Intrinsic::NatLt(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Lt => Some(true),
                Comparison::Eq | Comparison::Gt | Comparison::Ge => Some(false),
                Comparison::Le | Comparison::Stuck => None,
            },
            Intrinsic::nat_lt,
        ),
        Intrinsic::NatDiv(left, right) => reduce_nat_division(
            reducer,
            left,
            right,
            "Nat/div",
            Nat::checked_div,
            Intrinsic::NatDiv,
        ),
        Intrinsic::NatRem(left, right) => reduce_nat_division(
            reducer,
            left,
            right,
            "Nat/rem",
            Nat::checked_rem,
            Intrinsic::NatRem,
        ),
        Intrinsic::NatGt(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Gt => Some(true),
                Comparison::Eq | Comparison::Lt | Comparison::Le => Some(false),
                Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::nat_gt,
        ),
        Intrinsic::NatLte(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Lt | Comparison::Eq | Comparison::Le => Some(true),
                Comparison::Gt => Some(false),
                Comparison::Ge | Comparison::Stuck => None,
            },
            Intrinsic::nat_lte,
        ),
        Intrinsic::NatGte(left, right) => reduce_nat_compare(
            reducer,
            left,
            right,
            |c| match c {
                Comparison::Gt | Comparison::Eq | Comparison::Ge => Some(true),
                Comparison::Lt => Some(false),
                Comparison::Le | Comparison::Stuck => None,
            },
            Intrinsic::nat_gte,
        ),
        // Bitwise ops fold on the unbounded ℕ the type level pretends: `and`, `or`, `xor` on the infinite binary expansion, `shl` as `· 2^n` and `shr` as `⌊·/2^n⌋`. The runtime's 31-bit carrier (truncating `shl`, logical `shr`) is imposed only in the backend, never here.
        Intrinsic::NatAnd(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_bitand(r).map(Intrinsic::Nat),
            Intrinsic::NatAnd,
        ),
        Intrinsic::NatOr(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_bitor(r).map(Intrinsic::Nat),
            Intrinsic::NatOr,
        ),
        Intrinsic::NatXor(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_bitxor(r).map(Intrinsic::Nat),
            Intrinsic::NatXor,
        ),
        Intrinsic::NatShl(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_shl(r).map(Intrinsic::Nat),
            Intrinsic::NatShl,
        ),
        Intrinsic::NatShr(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| l.checked_shr(r).map(Intrinsic::Nat),
            Intrinsic::NatShr,
        ),
        // The rotation and bit-count operations are 32-bit-carrier notions: they fold only a literal that fits the u32 view (the erased carrier) and stay neutral otherwise, like every other declined fold.
        Intrinsic::NatRotl(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| {
                let l = l.to_big_uint()?.to_u32()?;
                let r = r.to_big_uint()?.to_u32()?;
                Some(Intrinsic::Nat(Nat::new(nat_rotl(l, r) as usize)))
            },
            Intrinsic::NatRotl,
        ),
        Intrinsic::NatRotr(left, right) => reduce_nat_binary(
            reducer,
            left,
            right,
            |l, r| {
                let l = l.to_big_uint()?.to_u32()?;
                let r = r.to_big_uint()?.to_u32()?;
                Some(Intrinsic::Nat(Nat::new(nat_rotr(l, r) as usize)))
            },
            Intrinsic::NatRotr,
        ),
        Intrinsic::NatClz(inner) => reduce_nat_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Nat(Nat::new(
                    n.to_big_uint()?.to_u32()?.leading_zeros() as usize,
                )))
            },
            Intrinsic::NatClz,
        ),
        Intrinsic::NatCtz(inner) => reduce_nat_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Nat(Nat::new(
                    n.to_big_uint()?.to_u32()?.trailing_zeros() as usize,
                )))
            },
            Intrinsic::NatCtz,
        ),
        Intrinsic::NatPopcnt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Nat(Nat::new(
                    n.to_big_uint()?.to_u32()?.count_ones() as usize,
                )))
            },
            Intrinsic::NatPopcnt,
        ),
        Intrinsic::IntType => Ok(Subterm::Intrinsic(Intrinsic::IntType)),
        Intrinsic::Int(value) => Ok(Subterm::Intrinsic(Intrinsic::Int(value.clone()))),
        Intrinsic::IntEql(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left == right)),
            Intrinsic::IntEql,
        ),
        Intrinsic::IntNeq(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left != right)),
            Intrinsic::IntNeq,
        ),
        Intrinsic::IntAdd(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left + right)),
            Intrinsic::IntAdd,
        ),
        Intrinsic::IntSub(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left - right)),
            Intrinsic::IntSub,
        ),
        Intrinsic::IntMul(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left * right)),
            Intrinsic::IntMul,
        ),
        Intrinsic::IntDiv(left, right) => reduce_int_division(
            reducer,
            left,
            right,
            "Int/div",
            Int::checked_div,
            Intrinsic::IntDiv,
        ),
        Intrinsic::IntRem(left, right) => reduce_int_division(
            reducer,
            left,
            right,
            "Int/rem",
            Int::checked_rem,
            Intrinsic::IntRem,
        ),
        Intrinsic::IntLt(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left < right)),
            Intrinsic::IntLt,
        ),
        Intrinsic::IntGt(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left > right)),
            Intrinsic::IntGt,
        ),
        Intrinsic::IntLte(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left <= right)),
            Intrinsic::IntLte,
        ),
        Intrinsic::IntGte(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left >= right)),
            Intrinsic::IntGte,
        ),
        // Bitwise ops fold on the unbounded ℤ the type level pretends: `and`, `or`, `xor` on the infinite two's-complement expansion, `shl` as `· 2^n` and `shr` as the arithmetic `⌊·/2^n⌋`. The runtime's signed 31-bit carrier (truncating `shl`, `shr_s`) is imposed only in the backend, never here.
        Intrinsic::IntAnd(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left & right)),
            Intrinsic::IntAnd,
        ),
        Intrinsic::IntOr(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left | right)),
            Intrinsic::IntOr,
        ),
        Intrinsic::IntXor(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Int(left ^ right)),
            Intrinsic::IntXor,
        ),
        Intrinsic::IntShl(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| left.checked_shl(right).map(Intrinsic::Int),
            Intrinsic::IntShl,
        ),
        Intrinsic::IntShr(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| left.checked_shr(right).map(Intrinsic::Int),
            Intrinsic::IntShr,
        ),
        // 32-bit-carrier rotations and bit counts over the i32 view; a literal outside it stays neutral.
        Intrinsic::IntRotl(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |l, r| Some(Intrinsic::Int(Int::new(int_rotl(l.to_i32()?, r.to_i32()?)))),
            Intrinsic::IntRotl,
        ),
        Intrinsic::IntRotr(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |l, r| Some(Intrinsic::Int(Int::new(int_rotr(l.to_i32()?, r.to_i32()?)))),
            Intrinsic::IntRotr,
        ),
        Intrinsic::IntClz(inner) => reduce_int_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Int(Int::new(
                    (n.to_i32()? as u32).leading_zeros() as i32,
                )))
            },
            Intrinsic::IntClz,
        ),
        Intrinsic::IntCtz(inner) => reduce_int_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Int(Int::new(
                    (n.to_i32()? as u32).trailing_zeros() as i32,
                )))
            },
            Intrinsic::IntCtz,
        ),
        Intrinsic::IntPopcnt(inner) => reduce_int_unary(
            reducer,
            inner,
            |n| {
                Some(Intrinsic::Int(Int::new(
                    (n.to_i32()? as u32).count_ones() as i32
                )))
            },
            Intrinsic::IntPopcnt,
        ),
        Intrinsic::FltType => Ok(Subterm::Intrinsic(Intrinsic::FltType)),
        Intrinsic::Flt(flt) => Ok(Subterm::Intrinsic(Intrinsic::Flt(*flt))),
        Intrinsic::FltAdd(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltAdd)
        }
        Intrinsic::FltSub(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltSub)
        }
        Intrinsic::FltMul(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltMul)
        }
        Intrinsic::FltDiv(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltDiv)
        }
        // `%` on `f32` is C `fmod`: `x - trunc(x / y) * y`, sign of the dividend — the same value the `cont -> wasm` expansion computes.
        Intrinsic::FltRem(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltRem)
        }
        Intrinsic::FltMin(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltMin)
        }
        Intrinsic::FltMax(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltMax)
        }
        Intrinsic::FltCopysign(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltCopysign)
        }
        Intrinsic::FltEql(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltEql)
        }
        Intrinsic::FltNeq(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltNeq)
        }
        Intrinsic::FltLt(left, right) => reduce_flt_binary(reducer, left, right, Intrinsic::FltLt),
        Intrinsic::FltGt(left, right) => reduce_flt_binary(reducer, left, right, Intrinsic::FltGt),
        Intrinsic::FltLte(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltLte)
        }
        Intrinsic::FltGte(left, right) => {
            reduce_flt_binary(reducer, left, right, Intrinsic::FltGte)
        }
        Intrinsic::FltNeg(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltNeg),
        Intrinsic::FltAbs(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltAbs),
        Intrinsic::FltSqrt(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltSqrt),
        Intrinsic::FltFloor(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltFloor),
        Intrinsic::FltCeil(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltCeil),
        Intrinsic::FltTrunc(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltTrunc),
        Intrinsic::FltNearest(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltNearest),
        Intrinsic::FltToLeBytes(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltToLeBytes),
        Intrinsic::FltOfLeBytes(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::FltOfLeBytes(inner)))
        }
        // The conversions preserve the number, never the bits — a bit view belongs to explicit `Bin` casts. `Nat/to_int` is total: ℕ embeds in ℤ, and both are unbounded here. The runtime's carrier-range traps stay where they always were, at the `into_wasm` boundary.
        Intrinsic::NatToInt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Int(Int::new(v.to_big_uint()?))),
            Intrinsic::NatToInt,
        ),
        // Opaque at the type level, like every `Flt` operation: constructing a float *is* float semantics.
        Intrinsic::NatToFlt(inner) => {
            reduce_nat_unary(reducer, inner, |_| None, Intrinsic::NatToFlt)
        }
        // `Int/to_nat` of a negative literal is a value no natural holds — reported like a zero divisor, never wrapped. A symbolic operand rebuilds the neutral term.
        Intrinsic::IntToNat(inner) => {
            let span = inner.span();
            let inner = reducer.reduce_forced(inner.clone())?;
            match inner.as_int() {
                Some(value) => match value.to_big_uint() {
                    Some(number) => Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(number)))),
                    None => Err(ReduceError::IntToNatNegative { value, span }),
                },
                None => Ok(Subterm::Intrinsic(Intrinsic::IntToNat(inner))),
            }
        }
        Intrinsic::IntToFlt(inner) => {
            reduce_int_unary(reducer, inner, |_| None, Intrinsic::IntToFlt)
        }
        Intrinsic::FltToNat(inner) => reduce_flt_unary(reducer, inner, Intrinsic::FltToNat),
        Intrinsic::FltToInt(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::FltToInt(inner)))
        }
        Intrinsic::BinType(Grain::X) => Ok(Subterm::Intrinsic(Intrinsic::BinType(Grain::X))),
        Intrinsic::Bin(Grain::X, bytes) => {
            Ok(Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes.clone())))
        }
        Intrinsic::BinLen(Grain::X, bin) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            reduce_homomorphism(
                reducer,
                bin_shape(Grain::X, bin),
                |run| Term::intrinsic(Intrinsic::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::intrinsic(Intrinsic::nat_add(
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::bin_len(Grain::X, sub)),
            )
        }
        Intrinsic::BinEql(Grain::X, left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;

            // Reflexivity: any value equals itself. Catches a shared variable, which the peel below cannot — a bare variable is not a `Bin`-valued intrinsic.
            if left == right {
                return Ok(Subterm::Intrinsic(Intrinsic::Bool(true)));
            }

            // Structural decision via the free-monoid peel (`core::spine`): a peeled-equal pair is `true`, a definite byte or length clash is `false` (so `eql([1] ++ x, [2] ++ x) = false` regardless of `x`). Anything the peel leaves undecided stays neutral — the same conservative seam conversion reads, so the fold only ever strengthens, never weakens.
            if let (Subterm::Intrinsic(l), Subterm::Intrinsic(r)) = (&*left, &*right) {
                match peel_bin(l, r) {
                    Some(Peel::Equal) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(true))),
                    Some(Peel::Clash) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(false))),
                    Some(Peel::Continue(..)) | Some(Peel::Stuck) | None => {}
                }
            }

            Ok(Subterm::Intrinsic(Intrinsic::bin_eql(
                Grain::X,
                Term::unwrap_or_clone(left),
                Term::unwrap_or_clone(right),
            )))
        }
        Intrinsic::BinGet(Grain::X, bin, index) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes)), Some(i)) = (&*bin, i) {
                return match bytes.byte(i) {
                    Some(byte) => Ok(Subterm::Intrinsic(Intrinsic::Byte(byte))),
                    None => Err(ReduceError::BinGetOutOfBounds {
                        len: bytes.len(Grain::X),
                        index: i,
                        span: index.span(),
                    }),
                };
            }
            // The cons head's byte: `get(append(x[], byte), 0) = byte` — the base case of the cons-peel below, and the partner of `BinSlice`'s rules.
            if let Subterm::Intrinsic(Intrinsic::BinAppend(Grain::X, base, byte)) = &*bin
                && let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, b)) = &**base
                && b.is_empty()
                && let Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) = &*index_reduced
            {
                return reducer.reduce(byte.clone()).map(Term::unwrap_or_clone);
            }
            // A get over a cons spine peels one byte per `0`/`succ` index step: `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            if let Some((head, tail)) = peel_first_atom(Grain::X, &bin) {
                match &*index_reduced {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(Grain::X, head, zero)))
                            .map(Term::unwrap_or_clone);
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                        let prev = Term::intrinsic(Intrinsic::nat_sub(index_reduced.clone(), one));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(Grain::X, tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::bin_get(
                Grain::X,
                bin,
                index_reduced,
            )))
        }
        Intrinsic::BinSlice(Grain::X, bin, start, end) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let end_reduced = reducer.reduce_forced(end.clone())?;
            // The full slice is the identity: `slice(b, 0, len b) = b`. Sound even for a symbolic `b` — `0..len` is always in range, never trapping — and the runtime partner of `core::spine`'s window-collapse: it lets a bare full-window `BinSlice` reduce to its base, so a `Bin/slice` over the whole value costs no copy and converts against the base directly.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*end_reduced, Subterm::Intrinsic(Intrinsic::BinLen(Grain::X, whole)) if *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            // The empty slice is empty: `slice(b, i, i) = x[]`. The dual of the full-window identity and equally sound — an empty range yields no bytes regardless of `b`, and never equates two distinct literals. It lets a codepoint take collapse its zero-width base (`take 0`) to the empty string even over a symbolic cons.
            if start_reduced == end_reduced {
                return Ok(Subterm::Intrinsic(Intrinsic::Bin(
                    Grain::X,
                    PackedBin::empty(),
                )));
            }
            let s = as_index(&start_reduced);
            let e = as_index(&end_reduced);
            // A concrete slice of a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes)), Some(s), Some(e)) =
                (&*bin, s, e)
            {
                return match bytes.slice(Grain::X, s, e) {
                    Some(slice) => Ok(Subterm::Intrinsic(Intrinsic::Bin(Grain::X, slice))),
                    None => Err(ReduceError::BinSliceOutOfRange {
                        len: bytes.len(Grain::X),
                        start: s,
                        end: e,
                        span: start.span().or_else(|| end.span()),
                    }),
                };
            }
            // A slice over a cons spine peels one byte per `0`/`succ` boundary step — the reduction partner of the `Utf8` cons the validity proofs walk:  `slice(cons(h, t), 0, succ e) = h ++ slice(t, 0, e)`  and `slice(cons(h, t), succ s, e) = slice(t, s, e - 1)`.
            if let Some((head, tail)) = peel_first_atom(Grain::X, &bin) {
                let dec = |n: &Term| {
                    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                    Term::intrinsic(Intrinsic::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        let rest = Term::intrinsic(Intrinsic::bin_slice(
                            Grain::X,
                            tail,
                            zero,
                            dec(&end_reduced),
                        ));
                        let consed = Term::intrinsic(Intrinsic::bin_concat(Grain::X, [head, rest]));
                        return reducer.reduce(consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::intrinsic(Intrinsic::bin_slice(
                            Grain::X,
                            tail,
                            dec(&start_reduced),
                            dec(&end_reduced),
                        ));
                        return reducer.reduce(sliced).map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::bin_slice(
                Grain::X,
                bin,
                start_reduced,
                end_reduced,
            )))
        }
        Intrinsic::BinAppend(Grain::X, bin, byte) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let byte = reducer.reduce_forced(byte.clone())?;
            // A concrete byte is taken mod 256 — its low 8 bits — matching the runtime's packed-`i8` store and the optimizer's `as u8`. A symbolic operand has no `as_nat`, so it stays stuck rather than truncating.
            let n = match &*byte {
                Subterm::Intrinsic(Intrinsic::Byte(byte)) => Some(*byte),
                _ => None,
            };
            Ok(match (Term::unwrap_or_clone(bin), n) {
                (Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes)), Some(n)) => {
                    Subterm::Intrinsic(Intrinsic::Bin(Grain::X, bytes.append_byte(n).unwrap()))
                }
                (bin, _) => Subterm::Intrinsic(Intrinsic::bin_append(Grain::X, bin, byte)),
            })
        }
        Intrinsic::BinConcat(grain, operands) => {
            let grain = *grain;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reducer.reduce_forced(e.clone()))
                .collect::<Result<_, _>>()?;
            // Normalise by the monoid unit/associativity laws — drop the empty identity (so `concat(x[], a)`/`concat(a, x[])` collapse to `a`), fuse an all-literal survivor set with `PackedBin::concat`, collapse a lone operand. Grain-generic: both carriers fuse in the packed representation. The definitional partner of `peel_bin`'s `x[]`-handling (`core::spine`); see `normalize_concat`.
            Ok(normalize_concat(
                reduced,
                |operand: &Term| match &**operand {
                    Subterm::Intrinsic(Intrinsic::Bin(found, bytes)) if *found == grain => {
                        Some(bytes)
                    }
                    _ => None,
                },
                |runs| Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::concat(runs))),
                |kept| Subterm::Intrinsic(Intrinsic::BinConcat(grain, kept)),
            ))
        }
        Intrinsic::BinType(Grain::B) => Ok(Subterm::Intrinsic(Intrinsic::BinType(Grain::B))),
        Intrinsic::Bin(Grain::B, bits) => {
            Ok(Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits.clone())))
        }
        Intrinsic::BinLen(Grain::B, bin) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            reduce_homomorphism(
                reducer,
                bin_shape(Grain::B, bin),
                |run| Term::intrinsic(Intrinsic::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::intrinsic(Intrinsic::nat_add(
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::bin_len(Grain::B, sub)),
            )
        }
        Intrinsic::BinEql(Grain::B, left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            if left == right {
                return Ok(Subterm::Intrinsic(Intrinsic::Bool(true)));
            }
            if let (Subterm::Intrinsic(l), Subterm::Intrinsic(r)) = (&*left, &*right) {
                match peel_bin(l, r) {
                    Some(Peel::Equal) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(true))),
                    Some(Peel::Clash) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(false))),
                    Some(Peel::Continue(..)) | Some(Peel::Stuck) | None => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::BinEql(Grain::B, left, right)))
        }
        Intrinsic::BinGet(Grain::B, bin, index) => {
            let span = index.span();
            let bin = reducer.reduce_forced(bin.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)), Some(index)) =
                (&*bin, as_index(&index_reduced))
            {
                return bits
                    .bit(index)
                    .map(|bit| Subterm::Intrinsic(Intrinsic::Bool(bit)))
                    .ok_or_else(|| ReduceError::BinGetOutOfBounds {
                        len: bits.bit_length(),
                        index,
                        span,
                    });
            }
            // The cons head's bit: `get(append(b[], bit), 0) = bit` — the base case of the cons-peel below, and the partner of `BinSlice`'s rules. Without it the peel's symbolic head chunk is this same `append(b[], bit)`, so the `0`-index step would rebuild the redex it came from until the budget exhausted.
            if let Subterm::Intrinsic(Intrinsic::BinAppend(Grain::B, base, bit)) = &*bin
                && let Subterm::Intrinsic(Intrinsic::Bin(Grain::B, b)) = &**base
                && b.is_empty()
                && let Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) = &*index_reduced
            {
                return reducer.reduce(bit.clone()).map(Term::unwrap_or_clone);
            }
            if let Some((head, tail)) = peel_first_atom(Grain::B, &bin) {
                match &*index_reduced {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                Grain::B,
                                head,
                                Term::intrinsic(Intrinsic::Nat(Nat::Zero)),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let prev = Term::intrinsic(Intrinsic::nat_sub(
                            index_reduced.clone(),
                            Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        ));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(Grain::B, tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::BinGet(
                Grain::B,
                bin,
                index_reduced,
            )))
        }
        Intrinsic::BinSlice(Grain::B, bin, start, end) => {
            let span = start.span().or_else(|| end.span());
            let bin = reducer.reduce_forced(bin.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let end_reduced = reducer.reduce_forced(end.clone())?;
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*end_reduced, Subterm::Intrinsic(Intrinsic::BinLen(Grain::B, whole)) if *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            if start_reduced == end_reduced {
                return Ok(Subterm::Intrinsic(Intrinsic::Bin(
                    Grain::B,
                    PackedBin::empty(),
                )));
            }
            if let (Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)), Some(start), Some(end)) =
                (&*bin, as_index(&start_reduced), as_index(&end_reduced))
            {
                return bits
                    .slice(Grain::B, start, end)
                    .map(|bits| Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)))
                    .ok_or_else(|| ReduceError::BinSliceOutOfRange {
                        len: bits.bit_length(),
                        start,
                        end,
                        span,
                    });
            }
            if let Some((head, tail)) = peel_first_atom(Grain::B, &bin) {
                let dec = |n: &Term| {
                    Term::intrinsic(Intrinsic::nat_sub(
                        n.clone(),
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                    ))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let rest = Term::intrinsic(Intrinsic::bin_slice(
                            Grain::B,
                            tail,
                            Term::intrinsic(Intrinsic::Nat(Nat::Zero)),
                            dec(&end_reduced),
                        ));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_concat(
                                Grain::B,
                                [head, rest],
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_slice(
                                Grain::B,
                                tail,
                                dec(&start_reduced),
                                dec(&end_reduced),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::BinSlice(
                Grain::B,
                bin,
                start_reduced,
                end_reduced,
            )))
        }
        Intrinsic::BinAppend(Grain::B, bin, bit) => {
            let bin = reducer.reduce_forced(bin.clone())?;
            let bit = reducer.reduce_forced(bit.clone())?;
            Ok(Subterm::Intrinsic(match (&*bin, bit.as_bool()) {
                (Subterm::Intrinsic(Intrinsic::Bin(Grain::B, bits)), Some(bit)) => {
                    Intrinsic::Bin(Grain::B, bits.append_bit(bit))
                }
                _ => Intrinsic::BinAppend(Grain::B, bin, bit),
            }))
        }
        Intrinsic::LstType(elem) => {
            let elem = reducer.reduce(elem.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::lst_type(elem)))
        }
        Intrinsic::Lst(elem, elems) => {
            let elem = reducer.reduce(elem.clone())?;
            let elems = elems
                .iter()
                .map(|e| reducer.reduce(e.clone()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Subterm::Intrinsic(Intrinsic::Lst(elem, elems)))
        }
        Intrinsic::LstLen(type_, list) => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            reduce_homomorphism(
                reducer,
                lst_shape(list),
                |run| Term::intrinsic(Intrinsic::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::intrinsic(Intrinsic::nat_add(
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::lst_len(type_.clone(), sub)),
            )
        }
        Intrinsic::LstGet(type_, list, index) => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Lst(_, elems)), Some(i)) = (&*list, i) {
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
            // A get over a cons spine peels one element per `0`/`succ` index step, the `Lst` twin of `BinGet`'s byte peel: `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            if let Some((head, tail)) = peel_first_elem(&list) {
                match &*index_reduced {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        return Ok(Term::unwrap_or_clone(head));
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                        let prev = Term::intrinsic(Intrinsic::nat_sub(index_reduced.clone(), one));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::lst_get(type_, tail, prev)))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::lst_get(
                type_,
                list,
                index_reduced,
            )))
        }
        Intrinsic::LstSlice(type_, list, start, end) => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let end_reduced = reducer.reduce_forced(end.clone())?;
            // The full slice is the identity: `slice(a, 0, len a) = a`. Sound even for a symbolic `a` — `0..len` is always in range — the `Lst` twin of `BinSlice`'s full-window identity, letting a full-length `Lst/slice` reduce to its base instead of copying.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*end_reduced, Subterm::Intrinsic(Intrinsic::LstLen(_, whole)) if *whole == list)
            {
                return Ok(Term::unwrap_or_clone(list));
            }
            // The empty slice is empty: `slice(a, i, i) = []`. Sound for a symbolic `a` — an empty range yields no elements regardless — and the base case the cons peel below bottoms out on (the `Lst` twin of `BinSlice`'s empty-slice identity).
            if start_reduced == end_reduced {
                return Ok(Subterm::Intrinsic(Intrinsic::Lst(
                    type_.clone(),
                    Vec::new(),
                )));
            }
            let s = as_index(&start_reduced);
            let e = as_index(&end_reduced);
            // A concrete slice of a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Lst(_, elems)), Some(s), Some(e)) = (&*list, s, e)
            {
                return match elems.get(s..e) {
                    Some(slice) => Ok(Subterm::Intrinsic(Intrinsic::Lst(
                        type_.clone(),
                        slice.to_vec(),
                    ))),
                    None => Err(ReduceError::LstSliceOutOfRange {
                        len: elems.len(),
                        start: s,
                        end: e,
                        span: start.span().or_else(|| end.span()),
                    }),
                };
            }
            // A slice over a cons spine peels one element per `0`/`succ` boundary step, the `Lst` twin of `BinSlice`'s byte peel: `slice(cons(h, t), 0, succ e) = [h] ++ slice(t, 0, e)`  and `slice(cons(h, t), succ s, e) = slice(t, s, e - 1)`.
            if let Some((head, tail)) = peel_first_elem(&list) {
                let dec = |n: &Term| {
                    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                    Term::intrinsic(Intrinsic::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*end_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        let rest = Term::intrinsic(Intrinsic::lst_slice(
                            type_.clone(),
                            tail,
                            zero,
                            dec(&end_reduced),
                        ));
                        let head_singleton: Term =
                            Subterm::Intrinsic(Intrinsic::Lst(type_.clone(), vec![head])).into();
                        let consed =
                            Term::intrinsic(Intrinsic::lst_concat(type_, [head_singleton, rest]));
                        return reducer.reduce(consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::intrinsic(Intrinsic::lst_slice(
                            type_,
                            tail,
                            dec(&start_reduced),
                            dec(&end_reduced),
                        ));
                        return reducer.reduce(sliced).map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::lst_slice(
                type_,
                list,
                start_reduced,
                end_reduced,
            )))
        }
        Intrinsic::LstAppend(type_, list, elem) => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let elem = reducer.reduce(elem.clone())?;
            Ok(match Term::unwrap_or_clone(list) {
                Subterm::Intrinsic(Intrinsic::Lst(lst_elem, mut elems)) => {
                    elems.push(elem);
                    Subterm::Intrinsic(Intrinsic::Lst(lst_elem, elems))
                }
                list => Subterm::Intrinsic(Intrinsic::lst_append(type_, list, elem)),
            })
        }
        Intrinsic::LstConcat(type_, operands) => {
            let type_ = reducer.reduce(type_.clone())?;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reducer.reduce_forced(e.clone()))
                .collect::<Result<_, _>>()?;
            // The `Lst` twin of `BinConcat` normalisation: drop the empty list (so `concat([], a)`/`concat(a, [])` collapse to `a`), fuse an all-literal survivor set into one flattened literal, collapse a lone operand — the definitional partner of `peel_arr`'s `[]`-handling (`core::spine`); see `normalize_concat`.
            fn literal(operand: &Term) -> Option<&Vec<Term>> {
                match &**operand {
                    Subterm::Intrinsic(Intrinsic::Lst(_, elems)) => Some(elems),
                    _ => None,
                }
            }
            Ok(normalize_concat(
                reduced,
                literal,
                |runs| {
                    Subterm::Intrinsic(Intrinsic::Lst(
                        type_.clone(),
                        runs.into_iter().flatten().cloned().collect(),
                    ))
                },
                |kept| Subterm::Intrinsic(Intrinsic::lst_concat(type_.clone(), kept)),
            ))
        }
        // `map`: the eliminator homomorphism. The literal case applies `f` elementwise; the spine cases distribute (`map f (concat segs) = concat (map f segs)`, `map f (append b x) = append (map f b) (f x)`) — the same normal form a structural `foldr (\x ih. f x :: ih) []` produces, so map-based proofs still reduce. A symbolic list stays neutral (the `Opaque` case), so there is no unfold of a variable.
        Intrinsic::LstMap(a, b, lst, f) => {
            let a = reducer.reduce(a.clone())?;
            let b = reducer.reduce(b.clone())?;
            let lst = reducer.reduce_forced(lst.clone())?;
            let f = reducer.reduce(f.clone())?;
            reduce_homomorphism(
                reducer,
                lst_shape(lst),
                |elems| {
                    Term::intrinsic(Intrinsic::Lst(
                        b.clone(),
                        elems
                            .into_iter()
                            .map(|x| Term::apply(f.clone(), [x]))
                            .collect(),
                    ))
                },
                |images| Term::intrinsic(Intrinsic::lst_concat(b.clone(), images)),
                |base_map, generator| {
                    Term::intrinsic(Intrinsic::lst_append(
                        b.clone(),
                        base_map,
                        Term::apply(f.clone(), [generator]),
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::lst_map(a.clone(), b.clone(), sub, f.clone())),
            )
        }
        // The handle type and handle tokens are inert values, like `Nat`/`Nat(_)`.
        Intrinsic::HandleType => Ok(Subterm::Intrinsic(Intrinsic::HandleType)),
        Intrinsic::Handle(token) => Ok(Subterm::Intrinsic(Intrinsic::Handle(*token))),
        // Every operation the host performs is an `Io`, which is to say a *description*: it denotes one inert value here and becomes a host call only at erasure, where the entrypoint boundary forces the program's description exactly once.
        //
        // These arms used to refuse instead, and the refusal was the type-level half of the effect discipline: a spelling that does not fix a value must not reach a type. It is now the typing that keeps them out — a term of non-`Io` type cannot perform an effect, and an `Io` supports no elimination through which one could reach a type position. So the operands reduce, the node rebuilds, and nothing else follows.
        Intrinsic::ProcExit(code) => {
            let code = reducer.reduce(code.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::ProcExit(code)))
        }
        Intrinsic::CellType(elem) => {
            let elem = reducer.reduce(elem.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::cell_type(elem)))
        }
        Intrinsic::Cell(type_, init) => {
            let type_ = reducer.reduce(type_.clone())?;
            let init = reducer.reduce(init.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::Cell(type_, init)))
        }
        Intrinsic::CellSet(type_, cell, value) => {
            let type_ = reducer.reduce(type_.clone())?;
            let cell = reducer.reduce(cell.clone())?;
            let value = reducer.reduce(value.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::CellSet(type_, cell, value)))
        }
        Intrinsic::CellGet(type_, cell) => {
            let type_ = reducer.reduce(type_.clone())?;
            let cell = reducer.reduce(cell.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::CellGet(type_, cell)))
        }
        Intrinsic::IoType(result) => {
            let result = reducer.reduce(result.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_type(result)))
        }
        // A description is an inert value: its operands reduce and the node rebuilds, and no monad law fires. `bind(pure(x), f)` is deliberately *not* definitionally `f(x)` — an `Io` supports no proof for a law to be useful about, and admitting one would make conversion decide when an effect happens.
        Intrinsic::IoPure(type_, value) => {
            let type_ = reducer.reduce(type_.clone())?;
            let value = reducer.reduce(value.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_pure(type_, value)))
        }
        Intrinsic::IoBind(from, to, action, f) => {
            let from = reducer.reduce(from.clone())?;
            let to = reducer.reduce(to.clone())?;
            let action = reducer.reduce(action.clone())?;
            let f = reducer.reduce(f.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_bind(from, to, action, f)))
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::{Reducer, compare_nat, from_ordering, reduce_intrinsic},
        crate::{Free, Intrinsic, Nat, ReduceError, Subterm, Term},
        curios_base::{Grain, Int, PackedBin},
    };

    /// A reducer that reduces nothing. Every operand below is already a literal — a weak-head normal form — so no strategy is involved, and running the comparison body against an inert reducer says exactly that: the outcome is decided by the structural compare, not by anything unfolded.
    struct Inert;

    impl Reducer for Inert {
        fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
            Ok(term)
        }

        fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
            Ok(term)
        }
    }

    fn lit(n: u32) -> Term {
        Term::intrinsic(Intrinsic::Nat(Nat::new(n as usize)))
    }

    // Regression: `get(append(b[], x), 0)` must reduce to `x` through its own base-case arm — the cons peel's symbolic head chunk IS `append(b[], x)`, so without that arm the rewrite rebuilt the redex it came from until the step budget exhausted.
    #[test]
    fn bit_get_of_a_symbolic_cons_head_is_the_bit() {
        let bit = Term::free_var(&Free::local(0, Some("bit")));
        let empty: Term = Subterm::Intrinsic(Intrinsic::Bin(Grain::B, PackedBin::empty())).into();
        let cons = Term::intrinsic(Intrinsic::bin_append(Grain::B, empty, bit.clone()));
        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
        let get = Intrinsic::bin_get(Grain::B, cons, zero);

        let reduced = reduce_intrinsic(&mut Inert, &get).expect("reduces");

        assert_eq!(Term::from(reduced), bit);
    }

    // Soundness gate: the conversions preserve the number. `Nat/to_int` folds every literal — ℕ embeds in ℤ, both unbounded here — and `Int/to_nat` folds a non-negative to the same value and reports a negative like a zero divisor, never wrapping bits.
    #[test]
    fn conversion_folds_preserve_the_number() {
        for n in [
            0u64,
            1,
            0x3FFF_FFFF,
            0x4000_0000,
            0x7FFF_FFFF,
            0x8000_0000,
            0xFFFF_FFFF,
            0x1_0000_0000,
        ] {
            let nat = Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
            let reduced = reduce_intrinsic(&mut Inert, &Intrinsic::NatToInt(nat)).expect("reduces");
            assert_eq!(
                reduced,
                Subterm::Intrinsic(Intrinsic::Int(Int::new(n))),
                "Nat/to_int changed the number on {n}",
            );
        }
        for i in [0i64, 1, 0x3FFF_FFFF, 0x7FFF_FFFF, 0x1_0000_0000] {
            let int = Term::intrinsic(Intrinsic::Int(Int::new(i)));
            let reduced = reduce_intrinsic(&mut Inert, &Intrinsic::IntToNat(int)).expect("reduces");
            assert_eq!(
                reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::new(i as u64))),
                "Int/to_nat changed the number on {i}",
            );
        }
        for i in [-1i64, -0x4000_0000, i32::MIN as i64, i64::MIN] {
            let int = Term::intrinsic(Intrinsic::Int(Int::new(i)));
            let reduced = reduce_intrinsic(&mut Inert, &Intrinsic::IntToNat(int));
            assert!(
                matches!(reduced, Err(ReduceError::IntToNatNegative { .. })),
                "Int/to_nat failed to report the negative {i}",
            );
        }
    }

    // Soundness gate: the structural body agrees with the host ordering on every pair of literals — the decidable closed case where the two routes into a `Comparison` (the shared-inner shortcut vs. the host `cmp`) must coincide.
    #[test]
    fn compare_nat_agrees_with_literal_ordering() {
        let mut reducer = Inert;
        let samples = [0u32, 1, 2, 5, 42, 128, 255, 256, 1000];
        for &m in &samples {
            for &n in &samples {
                assert_eq!(
                    compare_nat(&mut reducer, lit(m), lit(n))
                        .expect("reduces")
                        .0,
                    from_ordering(m.cmp(&n)),
                    "compare_nat disagreed with the literal ordering on ({m}, {n})",
                );
            }
        }
    }
}
