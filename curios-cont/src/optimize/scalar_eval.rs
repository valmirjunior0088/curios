//! Wasm-faithful evaluator for `Code` operations against an abstract environment.
//!
//! Owns the arithmetic, bitwise, conversion, and aggregate-builder semantics that
//! `cont/into_wasm/code_emitter` lowers — and the value-dependent trap conditions
//! that go with them. Two passes consume it through the [`EvalEnv`] trait:
//!
//! - `constant_folding` evaluates against a region's literal bindings ([`Lits`],
//!   where an aggregate element is a `ValueName`), replacing every `Value::Eval`
//!   whose operands are literal with its result.
//! - `evaluate_pure_calls` evaluates against its interpreter frame (where an
//!   aggregate element is a runtime snapshot), so the trap and host-boundary set
//!   is identical between compile-time folding and compile-time interpretation.
//!
//! The two consumers represent aggregate *elements* differently, so the
//! environment carries an associated [`EvalEnv::Elem`] type and results come back as
//! an [`Evaluated`] over it: an owned scalar, a fresh array of elements, or a
//! forwarded element. The leaf operations are kept private; the entry points the
//! two passes need — `literals`, `simplify`, `eval`, `project`, `decide_match` —
//! are `pub`.

use {super::*, std::rc::Rc};

/// What `eval_scalar` produces: a scalar or bytestring, owned outright. A
/// dedicated carrier rather than [`Data`], whose aggregate variants hold
/// `ValueName`s an interpreter frame could never resolve — results that carry
/// elements travel as [`Evaluated::Lst`]/[`Evaluated::Elem`] instead, so the
/// aggregate case is unrepresentable here by construction.
pub(crate) enum Scalar {
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Vec<u8>),
}

impl Scalar {
    /// The i31ref representation of a `Bln`.
    fn bln(value: bool) -> Self {
        Self::Nat(value as u32)
    }

    /// Fold `Flt` min/max only when neither operand is NaN — the sole case where
    /// the wasm op and Rust's `f32::min`/`f32::max` disagree on the *value*
    /// returned.
    fn flt_minmax(left: f32, right: f32, op: fn(f32, f32) -> f32) -> Option<Self> {
        (!left.is_nan() && !right.is_nan()).then(|| Self::Flt(op(left, right)))
    }

    /// `i32.trunc_f32_u` followed by the backend's 31-bit overflow check: fold
    /// only when the truncation toward zero lands in `[0, 2^31)`; anything else
    /// traps.
    fn flt_to_nat(value: f32) -> Option<Self> {
        let truncated = value.trunc();
        (value.is_finite() && truncated > -1.0 && truncated < 2_147_483_648.0)
            .then_some(Self::Nat(truncated as u32))
    }

    /// `i32.trunc_f32_s` followed by the backend's 31-bit overflow check: fold
    /// only when the truncation toward zero lands in `[-2^30, 2^30)`; anything
    /// else traps.
    fn flt_to_int(value: f32) -> Option<Self> {
        let truncated = value.trunc();
        (value.is_finite() && (-1_073_741_824.0..1_073_741_824.0).contains(&truncated))
            .then_some(Self::Int(truncated as i32))
    }

    /// `bytes[start..end]` as a fresh `Bin`, when the bounds are in range; an
    /// out-of-range slice traps, so it is left unfolded.
    fn bin_slice(bytes: &[u8], start: u32, end: u32) -> Option<Self> {
        let (start, end) = (start as usize, end as usize);
        (start <= end && end <= bytes.len()).then(|| Self::Bin(bytes[start..end].to_vec()))
    }

    /// The snapshot of an owned scalar result — total, since a [`Scalar`]
    /// cannot carry an aggregate.
    pub(crate) fn snapshot(self) -> Snapshot {
        match self {
            Scalar::Nat(value) => Snapshot::Nat(value),
            Scalar::Int(value) => Snapshot::Int(value),
            Scalar::Flt(value) => Snapshot::Flt(value),
            Scalar::Bin(bytes) => Snapshot::Bin(Rc::new(bytes)),
        }
    }
}

/// The result of evaluating or projecting a `Code` operation.
pub(crate) enum Evaluated<E> {
    /// A scalar or bytestring, owned outright.
    Scalar(Scalar),
    /// A fresh array of environment elements (`Lst` concat/slice/append).
    Lst(Vec<E>),
    /// A forwarded element — a projection out of a known aggregate whose target
    /// is not an inlinable scalar.
    Elem(E),
}

/// The replacement for an `Eval`, if any: a folded result, or a forwarded
/// aggregate projection.
pub(crate) fn simplify<E: EvalEnv>(code: &Code, env: &E) -> Option<Evaluated<E::Elem>> {
    eval(code, env).or_else(|| project(code, env))
}

/// The arm a `Match` takes when its operand is a known `Nat` tag: the matching
/// case, else the default. A tag with neither is left unfolded.
pub(crate) fn decide_match(tail: &Tail, env: &impl EvalEnv) -> Option<JumpTarget> {
    let Tail::Match(target) = tail else {
        return None;
    };
    let tag = env.nat(&target.operand)?;

    target.cases.get(&tag).or(target.default.as_ref()).cloned()
}

/// Resolve a projection out of a known aggregate to the element (or length/byte)
/// it reads. Aggregates are immutable, so this is always sound; out-of-bounds
/// access would trap, so it is left unfolded.
fn project<E: EvalEnv>(code: &Code, env: &E) -> Option<Evaluated<E::Elem>> {
    use Code::*;

    match code {
        TplGet(t, index) => env.tpl(t)?.get(*index).map(|elem| forward(elem, env)),
        LstGet(a, i) => env
            .lst(a)?
            .get(env.nat(i)? as usize)
            .map(|elem| forward(elem, env)),
        BinGet(b, i) => env
            .bin(b)?
            .get(env.nat(i)? as usize)
            .map(|byte| Evaluated::Scalar(Scalar::Nat(*byte as u32))),
        LstLen(a) => fits31u(env.lst(a)?.len() as u64).map(|n| Evaluated::Scalar(Scalar::Nat(n))),
        BinLen(b) => fits31u(env.bin(b)?.len() as u64).map(|n| Evaluated::Scalar(Scalar::Nat(n))),
        _ => None,
    }
}

/// Forward an aggregate element. A scalar is inlined so it cascades through
/// further folds; anything else is forwarded as the element itself, leaving the
/// consumer to alias it (folding) or use it directly (interpretation).
fn forward<E: EvalEnv>(elem: &E::Elem, env: &E) -> Evaluated<E::Elem> {
    match env.scalar(elem) {
        Some(data) => Evaluated::Scalar(data),
        None => Evaluated::Elem(elem.clone()),
    }
}

/// Evaluate a `Code` operation against the environment. Returns `None` when an
/// operand is non-literal, the operation is unsupported (projection — handled by
/// [`project`] — or `Io`), or the operation would trap at runtime —
/// `evaluate_pure_calls` promotes that `None` into an interpreter abort so the
/// trap remains observable.
pub(crate) fn eval<E: EvalEnv>(code: &Code, env: &E) -> Option<Evaluated<E::Elem>> {
    use Code::*;

    match code {
        // `Lst` builders — total given in-bounds literal indices; they yield a
        // fresh array of elements, so they cascade through further projection
        // and concatenation.
        LstConcat(operands) => {
            let mut elems = Vec::new();
            for name in operands {
                elems.extend_from_slice(env.lst(name)?);
            }
            Some(Evaluated::Lst(elems))
        }
        LstSlice(a, start, end) => {
            lst_slice(env.lst(a)?, env.nat(start)?, env.nat(end)?).map(Evaluated::Lst)
        }
        LstAppend(a, elem) => {
            let mut elems = env.lst(a)?.to_vec();
            elems.push(env.elem(elem)?);
            Some(Evaluated::Lst(elems))
        }

        // Everything else yields an owned scalar or bytestring.
        _ => eval_scalar(code, env).map(Evaluated::Scalar),
    }
}

/// The scalar- and `Bin`-resulting operations, mirroring `code_emitter`'s
/// lowering — including 31-bit wrapping and the value-dependent traps.
fn eval_scalar<E: EvalEnv>(code: &Code, env: &E) -> Option<Scalar> {
    use Code::*;

    match code {
        // Nat — 31-bit unsigned.
        NatAdd(a, b) => fits31u(env.nat(a)? as u64 + env.nat(b)? as u64).map(Scalar::Nat),
        NatSub(a, b) => Some(Scalar::Nat(env.nat(a)?.saturating_sub(env.nat(b)?))),
        NatMul(a, b) => fits31u(env.nat(a)? as u64 * env.nat(b)? as u64).map(Scalar::Nat),
        NatDiv(a, b) => Some(Scalar::Nat(env.nat(a)? / (nonzero_u(env.nat(b)?)?))),
        NatRem(a, b) => Some(Scalar::Nat(env.nat(a)? % (nonzero_u(env.nat(b)?)?))),
        NatAnd(a, b) => Some(Scalar::Nat(env.nat(a)? & env.nat(b)?)),
        NatOr(a, b) => Some(Scalar::Nat(env.nat(a)? | env.nat(b)?)),
        NatXor(a, b) => Some(Scalar::Nat(env.nat(a)? ^ env.nat(b)?)),
        NatEql(a, b) => Some(Scalar::bln(env.nat(a)? == env.nat(b)?)),
        NatNeq(a, b) => Some(Scalar::bln(env.nat(a)? != env.nat(b)?)),
        NatLt(a, b) => Some(Scalar::bln(env.nat(a)? < env.nat(b)?)),
        NatGt(a, b) => Some(Scalar::bln(env.nat(a)? > env.nat(b)?)),
        NatLte(a, b) => Some(Scalar::bln(env.nat(a)? <= env.nat(b)?)),
        NatGte(a, b) => Some(Scalar::bln(env.nat(a)? >= env.nat(b)?)),

        // Int — 31-bit signed.
        IntAdd(a, b) => fits31s(env.int(a)? as i64 + env.int(b)? as i64).map(Scalar::Int),
        IntSub(a, b) => fits31s(env.int(a)? as i64 - env.int(b)? as i64).map(Scalar::Int),
        IntMul(a, b) => fits31s(env.int(a)? as i64 * env.int(b)? as i64).map(Scalar::Int),
        IntDiv(a, b) => {
            fits31s(env.int(a)? as i64 / (nonzero_s(env.int(b)?)?) as i64).map(Scalar::Int)
        }
        IntRem(a, b) => Some(Scalar::Int(env.int(a)? % (nonzero_s(env.int(b)?)?))),
        IntEql(a, b) => Some(Scalar::bln(env.int(a)? == env.int(b)?)),
        IntNeq(a, b) => Some(Scalar::bln(env.int(a)? != env.int(b)?)),
        IntLt(a, b) => Some(Scalar::bln(env.int(a)? < env.int(b)?)),
        IntGt(a, b) => Some(Scalar::bln(env.int(a)? > env.int(b)?)),
        IntLte(a, b) => Some(Scalar::bln(env.int(a)? <= env.int(b)?)),
        IntGte(a, b) => Some(Scalar::bln(env.int(a)? >= env.int(b)?)),

        // Flt — f32, total.
        FltAdd(a, b) => Some(Scalar::Flt(env.flt(a)? + env.flt(b)?)),
        FltSub(a, b) => Some(Scalar::Flt(env.flt(a)? - env.flt(b)?)),
        FltMul(a, b) => Some(Scalar::Flt(env.flt(a)? * env.flt(b)?)),
        FltDiv(a, b) => Some(Scalar::Flt(env.flt(a)? / env.flt(b)?)),
        FltRem(a, b) => Some(Scalar::Flt(env.flt(a)? % env.flt(b)?)),
        FltNeg(a) => Some(Scalar::Flt(-env.flt(a)?)),
        FltAbs(a) => Some(Scalar::Flt(env.flt(a)?.abs())),
        FltSqrt(a) => Some(Scalar::Flt(env.flt(a)?.sqrt())),
        FltFloor(a) => Some(Scalar::Flt(env.flt(a)?.floor())),
        FltCeil(a) => Some(Scalar::Flt(env.flt(a)?.ceil())),
        FltTrunc(a) => Some(Scalar::Flt(env.flt(a)?.trunc())),
        FltCopysign(a, b) => Some(Scalar::Flt(env.flt(a)?.copysign(env.flt(b)?))),
        FltEql(a, b) => Some(Scalar::bln(env.flt(a)? == env.flt(b)?)),
        FltNeq(a, b) => Some(Scalar::bln(env.flt(a)? != env.flt(b)?)),
        FltLt(a, b) => Some(Scalar::bln(env.flt(a)? < env.flt(b)?)),
        FltGt(a, b) => Some(Scalar::bln(env.flt(a)? > env.flt(b)?)),
        FltLte(a, b) => Some(Scalar::bln(env.flt(a)? <= env.flt(b)?)),
        FltGte(a, b) => Some(Scalar::bln(env.flt(a)? >= env.flt(b)?)),

        // Nat — shifts, rotates, and bit scans. A left shift truncates back into
        // the 31-bit carrier (the backend's `ref.i31` drops bit 31), so the fold
        // masks to 31 bits to match; the logical right shift and the scans always
        // land back inside 31 bits, while a rotate can land on bit 31 and trap.
        NatShl(a, b) => Some(Scalar::Nat(
            env.nat(a)?.wrapping_shl(env.nat(b)?) & 0x7FFF_FFFF,
        )),
        NatShr(a, b) => Some(Scalar::Nat(env.nat(a)?.wrapping_shr(env.nat(b)?))),
        NatRotl(a, b) => fits31u(env.nat(a)?.rotate_left(env.nat(b)?) as u64).map(Scalar::Nat),
        NatRotr(a, b) => fits31u(env.nat(a)?.rotate_right(env.nat(b)?) as u64).map(Scalar::Nat),
        NatClz(a) => Some(Scalar::Nat(env.nat(a)?.leading_zeros())),
        NatCtz(a) => Some(Scalar::Nat(env.nat(a)?.trailing_zeros())),
        NatPopcnt(a) => Some(Scalar::Nat(env.nat(a)?.count_ones())),
        NatEqz(a) => Some(Scalar::bln(env.nat(a)? == 0)),

        // Int — bitwise, shifts, rotates, and bit scans. Bitwise, both shifts, and
        // the scans are total once the result is reduced to its 31-bit payload
        // (`wrap31s`); the left shift truncates into the carrier like `Nat/shl`
        // rather than trapping. Only the rotates trap-check, like `Int` arithmetic.
        IntAnd(a, b) => Some(Scalar::Int(wrap31s(env.int(a)? & env.int(b)?))),
        IntOr(a, b) => Some(Scalar::Int(wrap31s(env.int(a)? | env.int(b)?))),
        IntXor(a, b) => Some(Scalar::Int(wrap31s(env.int(a)? ^ env.int(b)?))),
        IntShl(a, b) => Some(Scalar::Int(wrap31s(
            env.int(a)?.wrapping_shl(env.int(b)? as u32),
        ))),
        IntShr(a, b) => Some(Scalar::Int(wrap31s(
            env.int(a)?.wrapping_shr(env.int(b)? as u32),
        ))),
        IntRotl(a, b) => {
            let rotated = (env.int(a)? as u32).rotate_left(env.int(b)? as u32) as i32;
            fits31s(rotated as i64).map(Scalar::Int)
        }
        IntRotr(a, b) => {
            let rotated = (env.int(a)? as u32).rotate_right(env.int(b)? as u32) as i32;
            fits31s(rotated as i64).map(Scalar::Int)
        }
        IntClz(a) => Some(Scalar::Int((env.int(a)? as u32).leading_zeros() as i32)),
        IntCtz(a) => Some(Scalar::Int((env.int(a)? as u32).trailing_zeros() as i32)),
        IntPopcnt(a) => Some(Scalar::Int((env.int(a)? as u32).count_ones() as i32)),
        IntEqz(a) => Some(Scalar::bln(env.int(a)? == 0)),

        // Flt — min/max are folded only when neither operand is NaN, the one case
        // where they *value*-diverge (wasm yields NaN, Rust yields the operand);
        // nearest rounds half-to-even, matching `f32.nearest` on every input.
        FltMin(a, b) => Scalar::flt_minmax(env.flt(a)?, env.flt(b)?, f32::min),
        FltMax(a, b) => Scalar::flt_minmax(env.flt(a)?, env.flt(b)?, f32::max),
        FltNearest(a) => Some(Scalar::Flt(env.flt(a)?.round_ties_even())),

        // Conversions. The int↔int casts reinterpret the 31-bit payload exactly as
        // the backend does (`ref.i31` then `i31.get_{u,s}`), so a high `Nat` reads
        // back as a negative `Int` and a negative `Int` as a large `Nat`. The
        // float→int casts truncate toward zero and trap-check the 31-bit range.
        NatToInt(a) => Some(Scalar::Int(wrap31s(env.nat(a)? as i32))),
        NatToFlt(a) => Some(Scalar::Flt(env.nat(a)? as f32)),
        IntToNat(a) => Some(Scalar::Nat(env.int(a)? as u32 & 0x7FFF_FFFF)),
        IntToFlt(a) => Some(Scalar::Flt(env.int(a)? as f32)),
        FltToNat(a) => Scalar::flt_to_nat(env.flt(a)?),
        FltToInt(a) => Scalar::flt_to_int(env.flt(a)?),

        FltToLeBin(a) => Some(Scalar::Bin(env.flt(a)?.to_le_bytes().to_vec())),

        // Bytewise equality — total whenever both operands are known.
        BinEql(a, b) => Some(Scalar::bln(env.bin(a)? == env.bin(b)?)),

        // Variadic concatenation — total, so always foldable when every operand is
        // a literal of the matching kind.
        BinConcat(operands) => {
            let mut bytes = Vec::new();
            for name in operands {
                bytes.extend_from_slice(env.bin(name)?);
            }
            Some(Scalar::Bin(bytes))
        }

        // `Bin` builders. A slice needs in-bounds literal indices; an append needs
        // a literal byte. Each yields a fresh literal bytestring, so it cascades
        // through further projection and concatenation.
        BinSlice(b, start, end) => Scalar::bin_slice(env.bin(b)?, env.nat(start)?, env.nat(end)?),
        BinAppend(b, byte) => {
            let mut bytes = env.bin(b)?.to_vec();
            bytes.push(env.nat(byte)? as u8);
            Some(Scalar::Bin(bytes))
        }

        // Aggregate *projection* is handled in `project`; the `Lst` builders are
        // handled in `eval` (their results carry elements, not owned data); `Io`
        // has no `Code` form. Anything unsupported is left untouched.
        _ => None,
    }
}

/// Reduce an `i32` to the 31-bit signed value an `i31ref` round-trip yields: keep
/// the low 31 bits and sign-extend bit 30. Mirrors `ref.i31` then `i31.get_s`, the
/// only way a non-trapping `Int` result is normalized by the backend.
fn wrap31s(value: i32) -> i32 {
    value.wrapping_shl(1) >> 1
}

/// `elems[start..end]` as a fresh element list, preserving the elements, when the
/// bounds are in range; an out-of-range slice traps, so it is left unfolded.
fn lst_slice<E: Clone>(elems: &[E], start: u32, end: u32) -> Option<Vec<E>> {
    let (start, end) = (start as usize, end as usize);
    (start <= end && end <= elems.len()).then(|| elems[start..end].to_vec())
}

/// Accept a `Nat` result only if it fits the 31-bit unsigned range; otherwise the
/// runtime traps on overflow.
fn fits31u(value: u64) -> Option<u32> {
    (value < (1 << 31)).then_some(value as u32)
}

/// Accept an `Int` result only if it fits the 31-bit signed range; otherwise the
/// runtime traps on overflow.
fn fits31s(value: i64) -> Option<i32> {
    ((-(1 << 30))..(1 << 30))
        .contains(&value)
        .then_some(value as i32)
}

fn nonzero_u(divisor: u32) -> Option<u32> {
    (divisor != 0).then_some(divisor)
}

fn nonzero_s(divisor: i32) -> Option<i32> {
    (divisor != 0).then_some(divisor)
}
