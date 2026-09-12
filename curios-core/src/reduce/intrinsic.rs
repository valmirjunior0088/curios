mod compare;
use compare::*;

mod cost;
use cost::*;

mod free_monoid;
use free_monoid::*;

mod laws;
use laws::*;

mod nat;
use nat::*;

mod scalar;
use scalar::*;

use {
    super::{ReduceError, Reducer},
    crate::{
        Cost, FUSION_CAP, FreeMonoid, Intrinsic, Located, Nat, Peel, Subterm, Term,
        normalize_concat, peel_bin, peel_first_atom, peel_first_elem, project_erased_universes,
    },
    curios_num::{Floating, Integer, Natural},
    curios_utilities::{Grain, PackedBin},
};

pub fn reduce_intrinsic(
    reducer: &mut impl Reducer,
    intrinsic: &Intrinsic,
) -> Result<Subterm, ReduceError> {
    match intrinsic {
        Intrinsic::BoolType => Ok(Subterm::Intrinsic(Intrinsic::BoolType)),
        Intrinsic::Bool(value) => Ok(Subterm::Intrinsic(Intrinsic::Bool(*value))),
        Intrinsic::BoolAnd(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l && r, Intrinsic::BoolAnd)?,
            |l, r| bool_lattice_laws(l, r, true),
        )),
        Intrinsic::BoolOr(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l || r, Intrinsic::BoolOr)?,
            |l, r| bool_lattice_laws(l, r, false),
        )),
        Intrinsic::BoolXor(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l != r, Intrinsic::BoolXor)?,
            bool_xor_laws,
        )),
        Intrinsic::BoolEql(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l == r, Intrinsic::BoolEql)?,
            |l, r| bool_eql_laws(l, r, true),
        )),
        Intrinsic::BoolNeq(left, right) => Ok(then_laws(
            reduce_bool_binary(reducer, left, right, |l, r| l != r, Intrinsic::BoolNeq)?,
            |l, r| bool_eql_laws(l, r, false),
        )),
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
        // Inversion of the constructor, not an equation about arithmetic: `Nat/to_byte` states `nat < 256`, so the byte it builds *is* that number and reading it back is the number again. This is the half that makes `Byte` transparent to the bounds oracle — a bound established in `Nat` survives the round trip, where before the trip erased it.
        Intrinsic::ByteToNat(inner) => {
            let inner = reducer.reduce_forced(inner.clone())?;
            Ok(Subterm::Intrinsic(match &*inner {
                Subterm::Intrinsic(Intrinsic::Byte(value)) => {
                    Intrinsic::Nat(Nat::new(usize::from(*value)))
                }
                Subterm::Intrinsic(Intrinsic::NatToByte { nat, .. }) => {
                    return reducer.reduce(nat.clone()).map(Term::unwrap_or_clone);
                }
                _ => Intrinsic::ByteToNat(inner),
            }))
        }
        // A closed operand past the carrier is *refused* rather than masked. Masking made this total by changing a value, which is the one thing a narrowing may not do, and it was the only such row on a numeric carrier; the `below` field is what replaces it, so a program that cannot prove its operand small no longer compiles rather than silently computing a different byte.
        Intrinsic::NatToByte { nat, below } => {
            let nat = reducer.reduce_forced(nat.clone())?;
            if let Subterm::Intrinsic(Intrinsic::ByteToNat(byte)) = &*nat {
                return reducer.reduce(byte.clone()).map(Term::unwrap_or_clone);
            }

            let span = nat.span();
            match nat.as_nat().map(|value| value.to_natural()) {
                Some(Some(value)) => {
                    match value.to_u32().and_then(|value| u8::try_from(value).ok()) {
                        Some(value) => Ok(Subterm::Intrinsic(Intrinsic::Byte(value))),
                        None => Err(ReduceError::NatToByteAbove { value, span }),
                    }
                }
                _ => Ok(Subterm::Intrinsic(Intrinsic::NatToByte {
                    nat,
                    below: below.clone(),
                })),
            }
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
            // Through the sum normal form, which is what merges like terms: `x + x` is `2 · x`, and `2 · x + 3 · x` is `5 · x`. Idempotent by construction — `Nat::summands` reads in the order `Nat::from_linear` writes — which is what lets the reducer rebuild a sum it was handed already reduced without changing it.
            Ok(Term::unwrap_or_clone(Nat::sum(&left, &right)))
        }
        // `(il + sl) - k` for a literal subtrahend `k`: when the floor covers it (`sl ≥ k`) the borrow stays within the floor and the tail `il ≥ 0` is untouched, so the result is `il + (sl - k)`. The subtraction twin of the addition floor law (and it gives `x - 0 = x` for any `x`, the unit law `NatAdd` already has): it turns the `succ e - 1` bounds the cons-slice rule produces back into `e`, so a slice over a symbolic cons keeps reducing instead of stalling on a stuck `Nat/sub`. Both-literal subtraction with `k` overshooting the floor truncates to zero; anything else stays neutral.
        Intrinsic::NatSub(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            // The same cancellation the comparisons take, and for the same law: a borrow never reaches what both sides carry, so `(x + a) - (x + b)` is `a - b` and the floor law below gets to see a literal subtrahend where it would otherwise have seen a sum.
            let (left, right) = Nat::cancel_common(&left, &right);
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
            // A zero minuend: `0 - x = 0` for every `x`, truncation being what makes it so.
            if Nat::is_zero(&left) {
                return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)));
            }
            // A neutral left that is itself a subtraction reassociates: `(a - b) - c = a - (b + c)` holds for truncated subtraction as it does for the integers, and the right-nested form is the one where a later literal subtrahend meets `a`'s floor. The sum is reduced so the cancellation above sees its summands, and the result re-enters this arm for the laws it may now satisfy.
            if let Subterm::Intrinsic(Intrinsic::NatSub(minuend, subtrahend)) = &*left {
                let subtrahend = reducer.reduce_forced(Term::intrinsic(Intrinsic::nat_add(
                    subtrahend.clone(),
                    right.clone(),
                )))?;
                return reduce_intrinsic(reducer, &Intrinsic::nat_sub(minuend.clone(), subtrahend));
            }
            Ok(Subterm::Intrinsic(Intrinsic::nat_sub(left, right)))
        }
        // Multiplication distributes in full, through `Nat::multiply`: every summand of one operand times every summand of the other, each product a monomial in canonical factor order, the results merged as a linear combination. The literal-factor floor law `(x + 1) · 2 = x · 2 + 2`, the unit and annihilation laws, the nested-factor fold `2 · (3 · x) = 6 · x`, a literal over a symbolic sum, a symbolic factor over a symbolic sum, and `x · y = y · x` are all the one rule; the floor only ever moves outward and a monomial is never nested, so the rewrite terminates.
        Intrinsic::NatMul(left, right) => {
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;
            // **A product of two symbolic sums is its own weak-head form.** Distribution is the one quadratic step in the `Nat` normal form — every summand of one operand against every summand of the other — and a web of definitions each naming the one before it twice made it build 1 222 222 monomials to keep 25 412, to answer a comparison a head clash settles. A product with a literal or a single symbolic summand on either side distributes here as it always did, in O(summands); only sum × sum stays stuck, and `Nat::normalize` distributes it where a value is asked for by name — see `documentation/design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md`.
            let symbolic_summands = |term: &Term| Nat::summands(&Nat::decompose(term).1).len();
            if symbolic_summands(&left) > 1 && symbolic_summands(&right) > 1 {
                return Ok(Subterm::Intrinsic(Intrinsic::nat_mul(left, right)));
            }
            reducer.spend(operand_bound(
                left.as_nat().map_or(0, |value| value.bits()),
                right.as_nat().map_or(0, |value| value.bits()),
            ))?;
            Ok(Term::unwrap_or_clone(Nat::multiply(&left, &right)))
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
        Intrinsic::NatDiv {
            dividend,
            divisor,
            non_zero,
        } => reduce_nat_division(reducer, dividend, divisor, non_zero, Euclid::Quotient),
        Intrinsic::NatRem {
            dividend,
            divisor,
            non_zero,
        } => reduce_nat_division(reducer, dividend, divisor, non_zero, Euclid::Remainder),
        Intrinsic::NatLe(left, right) => reduce_nat_compare(
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
        // Bitwise ops fold on the unbounded ℕ the type level pretends: `and`, `or`, `xor` on the infinite binary expansion, `shl` as `· 2^n` and `shr` as `⌊·/2^n⌋`. The runtime's 31-bit carrier (truncating `shl`, logical `shr`) is imposed only in the backend, never here.
        Intrinsic::NatAnd(left, right) => Ok(then_laws(
            reduce_nat_binary(
                reducer,
                left,
                right,
                |l, r| l.checked_bitand(r).map(Intrinsic::Nat),
                Intrinsic::NatAnd,
            )?,
            |l, r| nat_bitwise_laws(l, r, intrinsic),
        )),
        Intrinsic::NatOr(left, right) => Ok(then_laws(
            reduce_nat_binary(
                reducer,
                left,
                right,
                |l, r| l.checked_bitor(r).map(Intrinsic::Nat),
                Intrinsic::NatOr,
            )?,
            |l, r| nat_bitwise_laws(l, r, intrinsic),
        )),
        Intrinsic::NatXor(left, right) => Ok(then_laws(
            reduce_nat_binary(
                reducer,
                left,
                right,
                |l, r| l.checked_bitxor(r).map(Intrinsic::Nat),
                Intrinsic::NatXor,
            )?,
            |l, r| nat_bitwise_laws(l, r, intrinsic),
        )),
        Intrinsic::NatShl(left, right) => Ok(then_laws(
            reduce_nat_shl(reducer, left, right)?,
            nat_shift_laws,
        )),
        Intrinsic::NatShr(left, right) => Ok(then_laws(
            reduce_nat_binary(
                reducer,
                left,
                right,
                |l, r| l.checked_shr(r).map(Intrinsic::Nat),
                Intrinsic::NatShr,
            )?,
            nat_shift_laws,
        )),
        Intrinsic::IntType => Ok(Subterm::Intrinsic(Intrinsic::IntType)),
        Intrinsic::Int(value) => Ok(Subterm::Intrinsic(Intrinsic::Int(value.clone()))),
        Intrinsic::IntEql(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Bool(left == right)),
                Intrinsic::IntEql,
            )?,
            |l, r| identity_laws(l, r, true),
        )),
        Intrinsic::IntNeq(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Bool(left != right)),
                Intrinsic::IntNeq,
            )?,
            |l, r| identity_laws(l, r, false),
        )),
        Intrinsic::IntAdd(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Int(left + right)),
                Intrinsic::IntAdd,
            )?,
            |l, r| int_ring_laws(l, r, intrinsic),
        )),
        Intrinsic::IntSub(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Int(left - right)),
                Intrinsic::IntSub,
            )?,
            |l, r| int_ring_laws(l, r, intrinsic),
        )),
        Intrinsic::IntMul(left, right) => Ok(then_laws(
            reduce_int_binary(
                reducer,
                left,
                right,
                |left, right| Some(Intrinsic::Int(left * right)),
                Intrinsic::IntMul,
            )?,
            |l, r| int_ring_laws(l, r, intrinsic),
        )),
        Intrinsic::IntDiv {
            dividend,
            divisor,
            non_zero,
        } => reduce_int_division(
            reducer,
            dividend,
            divisor,
            "Int/div",
            Integer::checked_div,
            |dividend, divisor| Intrinsic::IntDiv {
                dividend,
                divisor,
                non_zero: non_zero.clone(),
            },
        ),
        Intrinsic::IntRem {
            dividend,
            divisor,
            non_zero,
        } => reduce_int_division(
            reducer,
            dividend,
            divisor,
            "Int/rem",
            Integer::checked_rem,
            |dividend, divisor| Intrinsic::IntRem {
                dividend,
                divisor,
                non_zero: non_zero.clone(),
            },
        ),
        Intrinsic::IntLt(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left < right)),
            Intrinsic::IntLt,
        ),
        Intrinsic::IntLe(left, right) => reduce_int_binary(
            reducer,
            left,
            right,
            |left, right| Some(Intrinsic::Bool(left <= right)),
            Intrinsic::IntLe,
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
        Intrinsic::IntShl(left, right) => reduce_int_shift(
            reducer,
            left,
            right,
            shift_bound,
            |value, amount| value.checked_shl(amount),
            Intrinsic::IntShl,
        ),
        Intrinsic::IntShr(left, right) => reduce_int_shift(
            reducer,
            left,
            right,
            |value, amount| {
                operand_bound(
                    value,
                    amount.map_or(0, |amount| u64::from(u64::BITS - amount.leading_zeros())),
                )
            },
            |value, amount| value.checked_shr(amount),
            Intrinsic::IntShr,
        ),
        Intrinsic::FltType => Ok(Subterm::Intrinsic(Intrinsic::FltType)),
        Intrinsic::Flt(flt) => Ok(Subterm::Intrinsic(Intrinsic::Flt(*flt))),
        Intrinsic::FltAdd(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l + r),
            Intrinsic::FltAdd,
        ),
        Intrinsic::FltSub(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l - r),
            Intrinsic::FltSub,
        ),
        Intrinsic::FltMul(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l * r),
            Intrinsic::FltMul,
        ),
        Intrinsic::FltDiv(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l / r),
            Intrinsic::FltDiv,
        ),
        // `%` on `f32` is C `fmod`: `x - trunc(x / y) * y`, sign of the dividend — the same value the `cont -> wasm` expansion computes.
        Intrinsic::FltRem(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l % r),
            Intrinsic::FltRem,
        ),
        Intrinsic::FltMin(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.min(r)),
            Intrinsic::FltMin,
        ),
        Intrinsic::FltMax(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.max(r)),
            Intrinsic::FltMax,
        ),
        Intrinsic::FltCopysign(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Flt(l.copysign(r)),
            Intrinsic::FltCopysign,
        ),
        Intrinsic::FltEql(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Bool(l.eql(r)),
            Intrinsic::FltEql,
        ),
        Intrinsic::FltNeq(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Bool(l.neq(r)),
            Intrinsic::FltNeq,
        ),
        Intrinsic::FltLt(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Bool(l.lt(r)),
            Intrinsic::FltLt,
        ),
        Intrinsic::FltLe(left, right) => reduce_flt_binary(
            reducer,
            left,
            right,
            |l, r| Intrinsic::Bool(l.le(r)),
            Intrinsic::FltLe,
        ),
        Intrinsic::FltNeg(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(-v)),
            Intrinsic::FltNeg,
        ),
        Intrinsic::FltAbs(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.abs())),
            Intrinsic::FltAbs,
        ),
        Intrinsic::FltSqrt(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.sqrt())),
            Intrinsic::FltSqrt,
        ),
        Intrinsic::FltFloor(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.floor())),
            Intrinsic::FltFloor,
        ),
        Intrinsic::FltCeil(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.ceil())),
            Intrinsic::FltCeil,
        ),
        Intrinsic::FltTrunc(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.trunc())),
            Intrinsic::FltTrunc,
        ),
        Intrinsic::FltNearest(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(v.nearest())),
            Intrinsic::FltNearest,
        ),
        // The two reinterpretations, whose round-trip laws are now theorems of the model rather than a postulate: `of_le_bytes(to_le_bytes(x))` is `x` for every `x`, and `to_le_bytes(of_le_bytes(b))` is `b` for every `b` that is not a non-canonical NaN pattern — which every NaN pattern reaching `Floating` is turned into.
        Intrinsic::FltToLeBytes(inner) => reduce_flt_unary(
            reducer,
            inner,
            |v| {
                Some(Intrinsic::Bin(
                    Grain::X,
                    PackedBin::from_bytes(v.to_bits().to_le_bytes().to_vec()),
                ))
            },
            Intrinsic::FltToLeBytes,
        ),
        Intrinsic::FltOfLeBytes { bin, eight_bytes } => {
            let bin = reducer.reduce_forced(bin.clone())?;

            let folded = match &*bin {
                Subterm::Intrinsic(Intrinsic::Bin(Grain::X, packed)) => packed
                    .to_bytes()
                    .and_then(|bytes| <[u8; 8]>::try_from(bytes).ok())
                    .map(|bytes| Intrinsic::Flt(Floating::from_bits(u64::from_le_bytes(bytes)))),
                _ => None,
            };

            Ok(Subterm::Intrinsic(match folded {
                Some(intrinsic) => intrinsic,
                None => Intrinsic::FltOfLeBytes {
                    bin,
                    eight_bytes: eight_bytes.clone(),
                },
            }))
        }
        // The conversions preserve the number, never the bits — a bit view belongs to explicit `Bin` casts. `Nat/to_int` is total: ℕ embeds in ℤ, and both are unbounded here. The runtime's carrier-range traps stay where they always were, at the `into_wasm` boundary.
        Intrinsic::NatToInt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Int(Integer::from(v.to_natural()?))),
            Intrinsic::NatToInt,
        ),
        // Into `Flt` the conversions are total and take no proof: rounding to nearest is the canonical extension of the embedding, forced by the structure the way monus is for `Nat/sub`, and a magnitude past the largest finite value answers the infinity of its sign.
        Intrinsic::NatToFlt(inner) => reduce_nat_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(Floating::of_natural(&v.to_natural()?))),
            Intrinsic::NatToFlt,
        ),
        // `Int/to_nat` of a negative literal is a value no natural holds — reported like a zero divisor, never wrapped. The bound the operation now states does not retire that report: a bound is discharged in the context the call was written in, and an open term reduces under hypotheses that context may not have. A symbolic operand rebuilds the neutral term, carrying the proof it was handed.
        Intrinsic::IntToNat { int, non_neg } => {
            let span = int.span();
            let int = reducer.reduce_forced(int.clone())?;
            match int.as_int() {
                Some(value) => match value.to_natural() {
                    Some(number) => Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(number)))),
                    None => Err(ReduceError::IntToNatNegative { value, span }),
                },
                None => Ok(Subterm::Intrinsic(Intrinsic::IntToNat {
                    int,
                    non_neg: non_neg.clone(),
                })),
            }
        }
        Intrinsic::IntToFlt(inner) => reduce_int_unary(
            reducer,
            inner,
            |v| Some(Intrinsic::Flt(Floating::of_integer(&v))),
            Intrinsic::IntToFlt,
        ),
        // The two narrowings truncate toward zero and answer the *exact* unbounded natural or integer: `to_nat(3.0e9)` is `3000000000`, a value no runtime carrier holds, refused downstream exactly as an overflowing `Nat` is rather than bent to fit here. Outside the domain each bound states, the model declines and the neutral is rebuilt, carrying the proof it was handed.
        Intrinsic::FltToNat { flt, non_neg } => reduce_flt_unary(
            reducer,
            flt,
            |v| Some(Intrinsic::Nat(Nat::new(v.to_natural()?))),
            |flt| Intrinsic::FltToNat {
                flt,
                non_neg: non_neg.clone(),
            },
        ),
        Intrinsic::FltToInt { flt, finite } => reduce_flt_unary(
            reducer,
            flt,
            |v| Some(Intrinsic::Int(v.to_integer()?)),
            |flt| Intrinsic::FltToInt {
                flt,
                finite: finite.clone(),
            },
        ),
        Intrinsic::BinType(grain) => Ok(Subterm::Intrinsic(Intrinsic::BinType(*grain))),
        Intrinsic::Bin(grain, run) => Ok(Subterm::Intrinsic(Intrinsic::Bin(*grain, run.clone()))),
        Intrinsic::BinLen(grain, bin) => {
            let grain = *grain;
            let bin = reducer.reduce_forced(bin.clone())?;
            // The measure answers a wholly-literal spine by folding it, without rebuilding a `Bin/len` per operand and handing each back to the reducer — which is what made a length over a deep concatenation cost a re-walk of every sub-spine. It agrees with the homomorphism below by construction on the shapes it accepts (a literal run's length, summed over a concatenation's operands) and declines everything else, so every other value reduces exactly as it did.
            if let Some(total) = FreeMonoid::Bin(grain).measure(&bin) {
                return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(total))));
            }
            let shape = bin_shape(reducer, grain, bin)?;

            reduce_homomorphism(
                reducer,
                shape,
                |run| Term::intrinsic(Intrinsic::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::intrinsic(Intrinsic::nat_add(
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::bin_len(grain, sub)),
            )
        }
        Intrinsic::BinEql(grain, left, right) => {
            let grain = *grain;
            let left = reducer.reduce_forced(left.clone())?;
            let right = reducer.reduce_forced(right.clone())?;

            // Reflexivity: any value equals itself. Catches a shared variable, which the peel below cannot — a bare variable is not a `Bin`-valued intrinsic.
            if left == right {
                return Ok(Subterm::Intrinsic(Intrinsic::Bool(true)));
            }

            // Structural decision via the free-monoid peel (`core::spine`): a peeled-equal pair is `true`, a definite generator or length clash is `false` (so `eql([1] ++ x, [2] ++ x) = false` regardless of `x`). Anything the peel leaves undecided stays neutral — the same conservative seam conversion reads, so the fold only ever strengthens, never weakens.
            if let (Subterm::Intrinsic(l), Subterm::Intrinsic(r)) = (&*left, &*right) {
                match peel_bin(l, r) {
                    Some(Peel::Equal) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(true))),
                    Some(Peel::Clash) => return Ok(Subterm::Intrinsic(Intrinsic::Bool(false))),
                    Some(Peel::Continue(..)) | Some(Peel::Stuck) | None => {}
                }
            }

            Ok(Subterm::Intrinsic(Intrinsic::BinEql(grain, left, right)))
        }
        Intrinsic::BinGet {
            grain,
            bin,
            index,
            in_range,
        } => {
            let grain = *grain;
            let span = index.span();
            let bin = reducer.reduce_forced(bin.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Bin(found, run)), Some(i)) = (&*bin, i)
                && *found == grain
            {
                return element_of_run(grain, run, i).ok_or(ReduceError::BinGetOutOfBounds {
                    len: run.len(grain),
                    index: i,
                    span,
                });
            }
            // The cons head's generator: `get(append(x[], k), 0) = k` — the base case of the cons-peel below, and the partner of `BinSlice`'s rules. Without it the peel's symbolic head chunk is this same `append(x[], k)`, so the `0`-index step would rebuild the redex it came from until the budget exhausted.
            if let Subterm::Intrinsic(Intrinsic::BinAppend {
                grain: found,
                bin: base,
                element,
            }) = &*bin
                && *found == grain
                && let Subterm::Intrinsic(Intrinsic::Bin(empty, b)) = &**base
                && *empty == grain
                && b.is_empty()
                && let Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) = &*index_reduced
            {
                return reducer.reduce(element.clone()).map(Term::unwrap_or_clone);
            }
            // A get over a cons spine peels one generator per `0`/`succ` index step: `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            // Locate the index by the operands' own lengths rather than peeling one generator at a time. A peel walks the whole spine to expose one generator and rebuilds the rest, so reading an index costs a pass per generator ahead of it; the measure reaches the operand holding it in one pass and indexes within that operand alone. `None` means some operand's length is not statically known, which is what the peel below is for.
            if let Some(i) = i {
                match FreeMonoid::Bin(grain).locate(&bin, i) {
                    Some(Located::At(operand, local)) => {
                        return bin_element(grain, operand, local).ok_or(
                            ReduceError::BinGetOutOfBounds {
                                len: local,
                                index: i,
                                span,
                            },
                        );
                    }
                    Some(Located::Past(len)) => {
                        return Err(ReduceError::BinGetOutOfBounds {
                            len,
                            index: i,
                            span,
                        });
                    }
                    None => {}
                }
            }
            // An index at a seam of a concatenation, where the operand starting there holds exactly one generator: `get([..p, k], len(p)) = k`. It is the walk `seam_window` already performs, asked for a window of one — so what located a symbolic *position* now also locates a symbolic *index*, which the locator above cannot because it reads a `usize`. This closes the asymmetry `curios`'s `tests::laws` recorded: a window at a seam was found and an index at the same seam was not.
            if let Some(operands) = FreeMonoid::Bin(grain).concatenated(&bin)
                && let Some(run) = seam_window(
                    reducer,
                    &operands,
                    &index_reduced,
                    &Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                    |operand| Intrinsic::bin_len(grain, operand.clone()),
                )?
                && let [only] = run.as_slice()
                && let Some(generator) = FreeMonoid::Bin(grain).single_generator(only)
            {
                return reducer.reduce(generator).map(Term::unwrap_or_clone);
            }
            if let Some((head, tail)) = peel_first_atom(grain, &bin) {
                match &*index_reduced {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                grain,
                                head,
                                zero,
                                in_range.clone(),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                        let prev = Term::intrinsic(Intrinsic::nat_sub(index_reduced.clone(), one));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_get(
                                grain,
                                tail,
                                prev,
                                in_range.clone(),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::bin_get(
                grain,
                bin,
                index_reduced,
                in_range.clone(),
            )))
        }
        Intrinsic::BinSlice {
            grain,
            bin,
            start,
            length,
            within,
        } => {
            let grain = *grain;
            let span = start.span().or_else(|| length.span());
            let bin = reducer.reduce_forced(bin.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let length_reduced = reducer.reduce_forced(length.clone())?;
            // The full slice is the identity: `slice(b, 0, len b) = b`. Sound even for a symbolic `b` — a window covering everything is always in range, never trapping — and the runtime partner of `core::spine`'s window-collapse: it lets a bare full-window `BinSlice` reduce to its base, so a `Bin/slice` over the whole value costs no copy and converts against the base directly.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*length_reduced, Subterm::Intrinsic(Intrinsic::BinLen(found, whole)) if *found == grain && *whole == bin)
            {
                return Ok(Term::unwrap_or_clone(bin));
            }
            // The empty slice is empty: `slice(b, i, 0)` is the empty run. The dual of the full-window identity and equally sound — a zero-length window yields no generators regardless of `b` or `i`, and never equates two distinct literals. It lets a codepoint take collapse its zero-width base (`take 0`) to the empty string even over a symbolic cons. Reading a *count* is what makes this one test rather than a comparison of two subjects.
            if Nat::is_zero(&length_reduced) {
                return Ok(Subterm::Intrinsic(Intrinsic::Bin(
                    grain,
                    PackedBin::empty(),
                )));
            }
            let s = as_index(&start_reduced);
            let n = as_index(&length_reduced);
            // A concrete slice of a literal run.
            if let (Subterm::Intrinsic(Intrinsic::Bin(found, run)), Some(s), Some(n)) =
                (&*bin, s, n)
                && *found == grain
            {
                return match s.checked_add(n).and_then(|e| run.slice(grain, s, e)) {
                    Some(slice) => Ok(Subterm::Intrinsic(Intrinsic::Bin(grain, slice))),
                    None => Err(ReduceError::BinSliceOutOfRange {
                        len: run.len(grain),
                        start: s,
                        length: n,
                        span,
                    }),
                };
            }
            // Locate the window by the operands' own lengths. Every operand it covers whole is handed back untouched and shares its payload; only the two at the edges are narrowed, and everything outside the window is dropped without being read.
            //
            // Every segment `bin_segments` admits is a literal run, so a narrowed edge is narrowed *here* rather than rebuilt as a `BinSlice` for the next pass to fold — `PackedBin::slice` is an O(1) window into the same payload, so this is the same value by the same operation, one round trip earlier. It also leaves this arm constructing no bounded node at all, which is what keeps a bound off the reducer once these accessors carry one.
            if let (Some(s), Some(n)) = (s, n) {
                match FreeMonoid::Bin(grain).window(&bin, s, n) {
                    Some(Ok(pieces)) => {
                        let parts = pieces
                            .into_iter()
                            .map(|piece| bin_piece(grain, piece))
                            .collect::<Vec<Term>>();
                        reducer.spend(Cost::collection(parts.len() as u64))?;

                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::bin_concat(grain, parts)))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Err(len)) => {
                        return Err(ReduceError::BinSliceOutOfRange {
                            len,
                            start: s,
                            length: n,
                            span,
                        });
                    }
                    None => {}
                }
            }
            // A window on the seams of a symbolic concatenation — see `seam_window`. An append is one of those, which `concatenated` is what says.
            if let Some(operands) = FreeMonoid::Bin(grain).concatenated(&bin)
                && let Some(run) = seam_window(
                    reducer,
                    &operands,
                    &start_reduced,
                    &length_reduced,
                    |operand| Intrinsic::bin_len(grain, operand.clone()),
                )?
            {
                return reducer
                    .reduce(Term::intrinsic(Intrinsic::bin_concat(grain, run)))
                    .map(Term::unwrap_or_clone);
            }
            // A slice over a cons spine peels one generator per `0`/`succ` boundary step — the reduction partner of the `Utf8` cons the validity proofs walk:  `slice(cons(h, t), 0, succ n) = h ++ slice(t, 0, n)`  and  `slice(cons(h, t), succ s, n) = slice(t, s, n)`.
            //
            // Advancing the start no longer touches the length, which is the reparameterisation paying for itself: the count is invariant under peeling the base, so nothing about the window has to be recomputed to move it.
            if let Some((head, tail)) = peel_first_atom(grain, &bin) {
                let dec = |n: &Term| {
                    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                    Term::intrinsic(Intrinsic::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*length_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        let rest = Term::intrinsic(Intrinsic::bin_slice(
                            grain,
                            tail,
                            zero,
                            dec(&length_reduced),
                            within.clone(),
                        ));
                        let consed = Term::intrinsic(Intrinsic::bin_concat(grain, [head, rest]));
                        return reducer.reduce(consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::intrinsic(Intrinsic::bin_slice(
                            grain,
                            tail,
                            dec(&start_reduced),
                            length_reduced.clone(),
                            within.clone(),
                        ));
                        return reducer.reduce(sliced).map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::bin_slice(
                grain,
                bin,
                start_reduced,
                length_reduced,
                within.clone(),
            )))
        }
        Intrinsic::BinAppend {
            grain,
            bin,
            element,
        } => {
            let grain = *grain;
            let bin = reducer.reduce_forced(bin.clone())?;
            // A concrete generator is a literal of the grain: a `Byte` at X, taken as the runtime's packed-`i8` store and the optimizer's `as u8` take it, a `Bool` at B. A symbolic operand reads as no generator, so it stays stuck rather than truncating.
            let element = reducer.reduce_forced(element.clone())?;
            if let Subterm::Intrinsic(Intrinsic::Bin(found, run)) = &*bin
                && *found == grain
                && let Some(generator) = Generator::read(grain, &element)
            {
                // Twice the whole rebuilt value: an append copies the packed payload out and then copies the extended run into a fresh buffer. Appending one generator therefore costs the length of everything appended so far, twice — which is the shape that makes a naive accumulation quadratic, and the reason it is charged rather than treated as an increment. Charged before the copy, which is why the generator is read first.
                reducer.spend(
                    packed_bound(grain, run.bit_length() as u64 + grain.bits() as u64)
                        .saturating_mul(2),
                )?;

                let appended = generator
                    .appended_to(run)
                    .expect("a literal run of a grain takes that grain's generator");

                return Ok(Subterm::Intrinsic(Intrinsic::Bin(grain, appended)));
            }

            Ok(Subterm::Intrinsic(Intrinsic::bin_append(
                grain, bin, element,
            )))
        }
        Intrinsic::BinConcat { grain, operands } => {
            let grain = *grain;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reducer.reduce_forced(e.clone()))
                .collect::<Result<_, _>>()?;
            // Normalise by the monoid unit/associativity laws — drop the empty identity (so `concat(x[], a)`/`concat(a, x[])` collapse to `a`), fuse an all-literal survivor set with `PackedBin::concat`, collapse a lone operand. Grain-generic: both carriers fuse in the packed representation. The definitional partner of `peel_bin`'s `x[]`-handling (`core::spine`); see `normalize_concat`.
            //
            // A run past `FUSION_CAP` declines to lend itself, so the concatenation keeps its node instead of copying both operands into a third. Measured in the grain's own generators, which is what makes one constant serve both: a bit-grain operand is capped at 64 bits and a byte-grain one at 64 bytes, and the corpus reaches neither.
            // The reduced operand vector, and the survivor vector the normalizer filters out of it — two collections whose length is the operand count, charged together before either exists.
            reducer.spend(Cost::collection(reduced.len() as u64).saturating_mul(2))?;

            normalize_concat(
                reduced,
                |operand: &Term| match &**operand {
                    Subterm::Intrinsic(Intrinsic::Bin(found, bytes))
                        if *found == grain && bytes.len(grain) <= FUSION_CAP =>
                    {
                        Some(bytes)
                    }
                    _ => None,
                },
                |runs| {
                    // Twice the fused payload, per the price list's last paragraph: `PackedBin::concat` fills a `Vec<u8>` and then converts it into an `Arc<[u8]>`, which allocates a second buffer of the same length. The operation costs two payloads even though one survives.
                    let bits = runs
                        .iter()
                        .map(|run| run.bit_length() as u64)
                        .fold(0u64, u64::saturating_add);
                    reducer.spend(packed_bound(grain, bits).saturating_mul(2))?;

                    Ok(Subterm::Intrinsic(Intrinsic::Bin(
                        grain,
                        PackedBin::concat(runs),
                    )))
                },
                |kept| {
                    Subterm::Intrinsic(Intrinsic::BinConcat {
                        grain,
                        operands: kept,
                    })
                },
            )
        }
        Intrinsic::ListType(elem) => {
            let elem = reducer.reduce(elem.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::list_type(elem)))
        }
        Intrinsic::List {
            element: elem,
            items: elems,
        } => {
            let elem = reducer.reduce(elem.clone())?;
            reducer.spend(Cost::collection(elems.len() as u64))?;
            let elems = elems
                .iter()
                .map(|e| reducer.reduce(e.clone()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Subterm::Intrinsic(Intrinsic::List {
                element: elem,
                items: elems,
            }))
        }
        Intrinsic::ListLen {
            element: type_,
            list,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            if let Some(total) = FreeMonoid::List.measure(&list) {
                return Ok(Subterm::Intrinsic(Intrinsic::Nat(Nat::new(total))));
            }
            // `len(map(xs, f)) = len(xs)`: a map is elementwise, so the measure passes through it whatever `f` does.
            if let Subterm::Intrinsic(Intrinsic::ListMap {
                from, list: inner, ..
            }) = &*list
            {
                return reduce_intrinsic(
                    reducer,
                    &Intrinsic::list_len(from.clone(), inner.clone()),
                );
            }
            reduce_homomorphism(
                reducer,
                list_shape(list),
                |run| Term::intrinsic(Intrinsic::Nat(Nat::new(run.len()))),
                nat_sum,
                |base_len, _| {
                    Term::intrinsic(Intrinsic::nat_add(
                        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                        base_len,
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::list_len(type_.clone(), sub)),
            )
        }
        Intrinsic::ListGet {
            element: type_,
            list,
            index,
            in_range,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let index_reduced = reducer.reduce_forced(index.clone())?;
            let i = as_index(&index_reduced);
            // A concrete index into a literal run.
            if let (
                Subterm::Intrinsic(Intrinsic::List {
                    element: _,
                    items: elems,
                }),
                Some(i),
            ) = (&*list, i)
            {
                let len = elems.len();
                return match elems.get(i).cloned().map(Term::unwrap_or_clone) {
                    Some(elem) => Ok(elem),
                    None => Err(ReduceError::ListGetOutOfBounds {
                        len,
                        index: i,
                        span: index.span(),
                    }),
                };
            }
            // The `List` twin of `BinGet`'s locator: reach the segment holding the index by the segments' own lengths, then index within it, rather than peeling one element at a time.
            if let Some(i) = i {
                match FreeMonoid::List.locate(&list, i) {
                    Some(Located::At(operand, local)) => {
                        let local = Term::intrinsic(Intrinsic::Nat(Nat::new(local)));
                        let operand = operand.clone();
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::list_get(
                                type_,
                                operand,
                                local,
                                in_range.clone(),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Located::Past(len)) => {
                        return Err(ReduceError::ListGetOutOfBounds {
                            len,
                            index: i,
                            span: index.span(),
                        });
                    }
                    None => {}
                }
            }
            // An index at a seam of a concatenation, where the operand starting there holds exactly one element: `get([..p, k], len(p)) = k`. The `List` twin of `BinGet`'s seam rule, and it closes the asymmetry *within* this carrier: `ListSlice` already walks the seams, so a window at one was located where an index at the same seam was not, though the walk reaching each is the same one.
            if let Some(operands) = FreeMonoid::List.concatenated(&list)
                && let Some(run) = seam_window(
                    reducer,
                    &operands,
                    &index_reduced,
                    &Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
                    |operand| Intrinsic::list_len(type_.clone(), operand.clone()),
                )?
                && let [only] = run.as_slice()
                && let Some(generator) = FreeMonoid::List.single_generator(only)
            {
                return reducer.reduce(generator).map(Term::unwrap_or_clone);
            }
            // A get over a cons spine peels one element per `0`/`succ` index step, the `List` twin of `BinGet`'s byte peel: `get(cons(h, t), 0) = h`   and   `get(cons(h, t), succ k) = get(t, k)`.
            if let Some((head, tail)) = peel_first_elem(&list) {
                match &*index_reduced {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        return Ok(Term::unwrap_or_clone(head));
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))) => {
                        let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                        let prev = Term::intrinsic(Intrinsic::nat_sub(index_reduced.clone(), one));
                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::list_get(
                                type_,
                                tail,
                                prev,
                                in_range.clone(),
                            )))
                            .map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::list_get(
                type_,
                list,
                index_reduced,
                in_range.clone(),
            )))
        }
        Intrinsic::ListSlice {
            element: type_,
            list,
            start,
            length,
            within,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let start_reduced = reducer.reduce_forced(start.clone())?;
            let length_reduced = reducer.reduce_forced(length.clone())?;
            // The full slice is the identity: `slice(a, 0, len a) = a`. Sound even for a symbolic `a` — a window covering everything is always in range — the `List` twin of `BinSlice`'s full-window identity, letting a full-length `List/slice` reduce to its base instead of copying.
            if matches!(
                &*start_reduced,
                Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero))
            ) && matches!(&*length_reduced, Subterm::Intrinsic(Intrinsic::ListLen { element: _, list: whole }) if *whole == list)
            {
                return Ok(Term::unwrap_or_clone(list));
            }
            // The empty slice is empty: `slice(a, i, 0) = []`. Sound for a symbolic `a` — a zero-length window yields no elements regardless — and the base case the cons peel below bottoms out on (the `List` twin of `BinSlice`'s empty-slice identity).
            if Nat::is_zero(&length_reduced) {
                return Ok(Subterm::Intrinsic(Intrinsic::List {
                    element: type_.clone(),
                    items: Vec::new(),
                }));
            }
            let s = as_index(&start_reduced);
            let n = as_index(&length_reduced);
            // A concrete slice of a literal run.
            if let (
                Subterm::Intrinsic(Intrinsic::List {
                    element: _,
                    items: elems,
                }),
                Some(s),
                Some(n),
            ) = (&*list, s, n)
            {
                return match s.checked_add(n).and_then(|e| elems.get(s..e)) {
                    Some(slice) => {
                        reducer.spend(Cost::collection(slice.len() as u64))?;

                        Ok(Subterm::Intrinsic(Intrinsic::List {
                            element: type_.clone(),
                            items: slice.to_vec(),
                        }))
                    }
                    None => Err(ReduceError::ListSliceOutOfRange {
                        len: elems.len(),
                        start: s,
                        length: n,
                        span: start.span().or_else(|| length.span()),
                    }),
                };
            }
            // The `List` twin of `BinSlice`'s locator: the window's segments, each already narrowed to its overlap, and — since every segment is a literal run — narrowed here rather than rebuilt as a `ListSlice` node for the next pass to fold.
            if let (Some(s), Some(n)) = (s, n) {
                match FreeMonoid::List.window(&list, s, n) {
                    Some(Ok(pieces)) => {
                        let parts = pieces
                            .into_iter()
                            .map(|piece| list_piece(&type_, piece))
                            .collect::<Vec<Term>>();
                        reducer.spend(Cost::collection(parts.len() as u64))?;

                        return reducer
                            .reduce(Term::intrinsic(Intrinsic::list_concat(type_, parts)))
                            .map(Term::unwrap_or_clone);
                    }
                    Some(Err(len)) => {
                        return Err(ReduceError::ListSliceOutOfRange {
                            len,
                            start: s,
                            length: n,
                            span: start.span().or_else(|| length.span()),
                        });
                    }
                    None => {}
                }
            }
            // A window on the seams of a symbolic concatenation — see `seam_window`. An append is one of those, which `list_concatenated` is what says.
            if let Some(operands) = FreeMonoid::List.concatenated(&list)
                && let Some(run) = seam_window(
                    reducer,
                    &operands,
                    &start_reduced,
                    &length_reduced,
                    |operand| Intrinsic::list_len(type_.clone(), operand.clone()),
                )?
            {
                return reducer
                    .reduce(Term::intrinsic(Intrinsic::list_concat(type_, run)))
                    .map(Term::unwrap_or_clone);
            }
            // A slice over a cons spine peels one element per `0`/`succ` boundary step, the `List` twin of `BinSlice`'s element peel: `slice(cons(h, t), 0, succ n) = [h] ++ slice(t, 0, n)`  and  `slice(cons(h, t), succ s, n) = slice(t, s, n)` — the count riding through the second untouched.
            if let Some((head, tail)) = peel_first_elem(&list) {
                let dec = |n: &Term| {
                    let one = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
                    Term::intrinsic(Intrinsic::nat_sub(n.clone(), one))
                };
                match (&*start_reduced, &*length_reduced) {
                    (
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)),
                        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))),
                    ) => {
                        let zero = Term::intrinsic(Intrinsic::Nat(Nat::Zero));
                        let rest = Term::intrinsic(Intrinsic::list_slice(
                            type_.clone(),
                            tail,
                            zero,
                            dec(&length_reduced),
                            within.clone(),
                        ));
                        let head_singleton: Term = Subterm::Intrinsic(Intrinsic::List {
                            element: type_.clone(),
                            items: vec![head],
                        })
                        .into();
                        let consed =
                            Term::intrinsic(Intrinsic::list_concat(type_, [head_singleton, rest]));
                        return reducer.reduce(consed).map(Term::unwrap_or_clone);
                    }
                    (Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(..))), _) => {
                        let sliced = Term::intrinsic(Intrinsic::list_slice(
                            type_,
                            tail,
                            dec(&start_reduced),
                            length_reduced.clone(),
                            within.clone(),
                        ));
                        return reducer.reduce(sliced).map(Term::unwrap_or_clone);
                    }
                    _ => {}
                }
            }
            Ok(Subterm::Intrinsic(Intrinsic::list_slice(
                type_,
                list,
                start_reduced,
                length_reduced,
                within.clone(),
            )))
        }
        Intrinsic::ListAppend {
            element: type_,
            list,
            item: elem,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let elem = reducer.reduce(elem.clone())?;
            let appended = match Term::unwrap_or_clone(list) {
                Subterm::Intrinsic(Intrinsic::List {
                    element: list_elem,
                    items: mut elems,
                }) => {
                    // Growing a vector reallocates it, so the whole extended run is charged rather than the one slot appended — the same reason `BinAppend` charges its whole rebuilt value.
                    reducer.spend(Cost::collection(elems.len() as u64 + 1))?;
                    elems.push(elem);

                    Subterm::Intrinsic(Intrinsic::List {
                        element: list_elem,
                        items: elems,
                    })
                }
                list => Subterm::Intrinsic(Intrinsic::list_append(type_, list, elem)),
            };

            Ok(appended)
        }
        Intrinsic::ListConcat {
            element: type_,
            operands,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let reduced: Vec<Term> = operands
                .iter()
                .map(|e| reducer.reduce_forced(e.clone()))
                .collect::<Result<_, _>>()?;
            // The `List` twin of `BinConcat` normalisation: drop the empty list (so `concat([], a)`/`concat(a, [])` collapse to `a`), fuse an all-literal survivor set into one flattened literal, collapse a lone operand — the definitional partner of `peel_arr`'s `[]`-handling (`core::spine`); see `normalize_concat`.
            // A run past `FUSION_CAP` declines to lend itself, exactly as on the `Bin` side, so a growing accumulation stops flattening its element vector into a longer one every step.
            fn literal(operand: &Term) -> Option<&Vec<Term>> {
                match &**operand {
                    Subterm::Intrinsic(Intrinsic::List {
                        element: _,
                        items: elems,
                    }) if elems.len() <= FUSION_CAP => Some(elems),
                    _ => None,
                }
            }
            reducer.spend(Cost::collection(reduced.len() as u64).saturating_mul(2))?;

            normalize_concat(
                reduced,
                literal,
                |runs| {
                    // One flattened vector of every operand's elements, each a retained reference rather than a rebuilt term — so this is the collection row and not the term row, and the elements it clones are reference-count bumps.
                    let slots = runs
                        .iter()
                        .map(|run| run.len() as u64)
                        .fold(0u64, u64::saturating_add);
                    reducer.spend(Cost::collection(slots))?;

                    Ok(Subterm::Intrinsic(Intrinsic::List {
                        element: type_.clone(),
                        items: runs.into_iter().flatten().cloned().collect(),
                    }))
                },
                |kept| Subterm::Intrinsic(Intrinsic::list_concat(type_.clone(), kept)),
            )
        }
        // `map`: the eliminator homomorphism. The literal case applies `f` elementwise; the spine cases distribute (`map f (concat segs) = concat (map f segs)`, `map f (append b x) = append (map f b) (f x)`) — the same normal form a structural `foldr (\x ih. f x :: ih) []` produces, so map-based proofs still reduce. A symbolic list stays neutral (the `Opaque` case), so there is no unfold of a variable.
        Intrinsic::ListMap {
            from: a,
            to: b,
            list,
            function: f,
        } => {
            let a = reducer.reduce(a.clone())?;
            let b = reducer.reduce(b.clone())?;
            let list = reducer.reduce_forced(list.clone())?;
            let f = reducer.reduce(f.clone())?;
            reduce_homomorphism(
                reducer,
                list_shape(list),
                |elems| {
                    Term::intrinsic(Intrinsic::List {
                        element: b.clone(),
                        items: elems
                            .into_iter()
                            .map(|x| Term::apply(f.clone(), [x]))
                            .collect(),
                    })
                },
                |images| Term::intrinsic(Intrinsic::list_concat(b.clone(), images)),
                |base_map, generator| {
                    Term::intrinsic(Intrinsic::list_append(
                        b.clone(),
                        base_map,
                        Term::apply(f.clone(), [generator]),
                    ))
                },
                |sub| Term::intrinsic(Intrinsic::list_map(a.clone(), b.clone(), sub, f.clone())),
            )
        }
        // The handle type and handle tokens are inert values, like `Nat`/`Nat(_)`.
        Intrinsic::HandleType => Ok(Subterm::Intrinsic(Intrinsic::HandleType)),
        Intrinsic::Handle(token) => Ok(Subterm::Intrinsic(Intrinsic::Handle(*token))),
        // Every operation the host performs is an `Io`, which is to say a *description*: it denotes one inert value here and becomes a host call only at erasure, where the entrypoint boundary forces the program's description exactly once.
        //
        // These arms used to refuse instead, and the refusal was the type-level half of the effect discipline: a spelling that does not fix a value must not reach a type. It is now the typing that keeps them out — a term of non-`Io` type cannot perform an effect, and an `Io` supports no elimination through which one could reach a type position. So the operands reduce, the node rebuilds, and nothing else follows.
        Intrinsic::ProcExit { result, code } => {
            let result = reducer.reduce(result.clone())?;
            let code = reducer.reduce(code.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::proc_exit(result, code)))
        }
        Intrinsic::CellType(elem) => {
            let elem = reducer.reduce(elem.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::cell_type(elem)))
        }
        Intrinsic::Cell {
            element: type_,
            initial: init,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let init = reducer.reduce(init.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::Cell {
                element: type_,
                initial: init,
            }))
        }
        Intrinsic::CellSet {
            element: type_,
            cell,
            value,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let cell = reducer.reduce(cell.clone())?;
            let value = reducer.reduce(value.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::CellSet {
                element: type_,
                cell,
                value,
            }))
        }
        Intrinsic::CellGet {
            element: type_,
            cell,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let cell = reducer.reduce(cell.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::CellGet {
                element: type_,
                cell,
            }))
        }
        Intrinsic::IoType(result) => {
            let result = reducer.reduce(result.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_type(result)))
        }
        // A description is an inert value: its operands reduce and the node rebuilds, and no monad law fires. `bind(pure(x), f)` is deliberately *not* definitionally `f(x)` — an `Io` supports no proof for a law to be useful about, and admitting one would make conversion decide when an effect happens.
        Intrinsic::IoPure {
            result: type_,
            value,
        } => {
            let type_ = reducer.reduce(type_.clone())?;
            let value = reducer.reduce(value.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_pure(type_, value)))
        }
        Intrinsic::IoBind {
            from,
            to,
            action,
            continuation: f,
        } => {
            let from = reducer.reduce(from.clone())?;
            let to = reducer.reduce(to.clone())?;
            let action = reducer.reduce(action.clone())?;
            let f = reducer.reduce(f.clone())?;
            Ok(Subterm::Intrinsic(Intrinsic::io_bind(from, to, action, f)))
        }
    }
}

#[cfg(test)]
mod compare_tests;
#[cfg(test)]
mod cost_tests;
#[cfg(test)]
mod free_monoid_tests;
#[cfg(test)]
mod laws_tests;
#[cfg(test)]
mod nat_tests;
#[cfg(test)]
mod test_support;
