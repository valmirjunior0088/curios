//! Wasm-faithful evaluator for `Code` operations against a literal environment.
//!
//! Owns the arithmetic, bitwise, conversion, and aggregate-builder semantics that
//! `cont/to_wasm/code_emitter` lowers — and the value-dependent trap conditions
//! that go with them. Two passes consume it:
//!
//! - `constant_folding` runs it across each region, replacing every `Value::Eval`
//!   whose operands are literal with its result (or with the projected element of
//!   a known aggregate).
//! - `evaluate_pure_calls` runs the same primitives inside its interpreter, so the
//!   trap and host-boundary set is identical between compile-time folding and
//!   compile-time interpretation.
//!
//! The leaf operations are kept private; the entry points the two passes need —
//! `literals`, `simplify`, `eval`, `project`, `decide_match` — are `pub`.

use {super::*, std::collections::HashMap};

/// A region-tree-wide map from value name to its bound literal. Names are unique
/// per body and scoping is lexical, so a single flat map is sound.
pub type Lits = HashMap<ValueName, Data>;

/// Collect every literal-bound scalar or aggregate in the region tree.
pub fn literals(region: &Region) -> Lits {
    let mut lits = Lits::new();
    collect_literals(region, &mut lits);
    lits
}

fn collect_literals(region: &Region, lits: &mut Lits) {
    for (name, value) in &region.values {
        if let Value::Pure(data) = value {
            lits.insert(name.clone(), data.clone());
        }
    }

    for (_, block) in &region.blocks {
        collect_literals(&block.region, lits);
    }
}

/// The replacement for an `Eval`, if any: a folded scalar literal, or a forwarded
/// aggregate projection.
pub fn simplify(code: &Code, lits: &Lits) -> Option<Value> {
    if let Some(data) = eval(code, lits) {
        return Some(Value::Pure(data));
    }

    project(code, lits)
}

/// The arm a `Match` takes when its operand is a known `Nat` tag: the matching
/// case, else the default. A tag with neither is left unfolded.
pub fn decide_match(tail: &Tail, lits: &Lits) -> Option<JumpTarget> {
    let Tail::Match(target) = tail else {
        return None;
    };
    let Data::Nat(tag) = lits.get(&target.operand)? else {
        return None;
    };

    target.cases.get(tag).or(target.default.as_ref()).cloned()
}

/// Resolve a projection out of a known aggregate to the element (or length/byte)
/// it reads. Aggregates are immutable, so this is always sound; out-of-bounds
/// access would trap, so it is left unfolded.
pub fn project(code: &Code, lits: &Lits) -> Option<Value> {
    use Code::*;

    match code {
        TplGet(t, index) => tpl(lits, t)?.get(*index).map(|elem| forward(elem, lits)),
        ArrGet(a, i) => arr(lits, a)?
            .get(nat(lits, i)? as usize)
            .map(|elem| forward(elem, lits)),
        BinGet(b, i) => bin(lits, b)?
            .get(nat(lits, i)? as usize)
            .map(|byte| Value::Pure(Data::Nat(*byte as u32))),
        ArrLen(a) => fits31u(arr(lits, a)?.len() as u64).map(|n| Value::Pure(Data::Nat(n))),
        BinLen(b) => fits31u(bin(lits, b)?.len() as u64).map(|n| Value::Pure(Data::Nat(n))),
        _ => None,
    }
}

/// Forward an aggregate element. A scalar literal is inlined as a `Pure` so it
/// cascades through further folds; anything else is forwarded by name, leaving
/// copy propagation and dead-code elimination to collapse the alias and reclaim
/// the now-dead aggregate.
fn forward(elem: &ValueName, lits: &Lits) -> Value {
    match lits.get(elem) {
        Some(data @ (Data::Nat(_) | Data::Int(_) | Data::Flt(_))) => Value::Pure(data.clone()),
        _ => Value::Alias(elem.clone()),
    }
}

fn tpl<'a>(lits: &'a Lits, name: &ValueName) -> Option<&'a [ValueName]> {
    match lits.get(name)? {
        Data::Tpl(elems) => Some(elems),
        _ => None,
    }
}

fn arr<'a>(lits: &'a Lits, name: &ValueName) -> Option<&'a [ValueName]> {
    match lits.get(name)? {
        Data::Arr(elems) => Some(elems),
        _ => None,
    }
}

fn bin<'a>(lits: &'a Lits, name: &ValueName) -> Option<&'a [u8]> {
    match lits.get(name)? {
        Data::Bin(bytes) => Some(bytes),
        _ => None,
    }
}

/// Evaluate a `Code` operation against the literal environment. Returns `None`
/// when an operand is non-literal, the operation is unsupported (projection,
/// `*ToStr`, `Io`), or the operation would trap at runtime — `evaluate_pure_calls`
/// promotes that `None` into an interpreter abort so the trap remains observable.
pub fn eval(code: &Code, lits: &Lits) -> Option<Data> {
    use Code::*;

    match code {
        // Nat — 31-bit unsigned.
        NatAdd(a, b) => fits31u(nat(lits, a)? as u64 + nat(lits, b)? as u64).map(Data::Nat),
        NatSub(a, b) => Some(Data::Nat(nat(lits, a)?.saturating_sub(nat(lits, b)?))),
        NatMul(a, b) => fits31u(nat(lits, a)? as u64 * nat(lits, b)? as u64).map(Data::Nat),
        NatDiv(a, b) => Some(Data::Nat(nat(lits, a)? / (nonzero_u(nat(lits, b)?)?))),
        NatRem(a, b) => Some(Data::Nat(nat(lits, a)? % (nonzero_u(nat(lits, b)?)?))),
        NatAnd(a, b) => Some(Data::Nat(nat(lits, a)? & nat(lits, b)?)),
        NatOr(a, b) => Some(Data::Nat(nat(lits, a)? | nat(lits, b)?)),
        NatXor(a, b) => Some(Data::Nat(nat(lits, a)? ^ nat(lits, b)?)),
        NatEql(a, b) => Some(bln(nat(lits, a)? == nat(lits, b)?)),
        NatNeq(a, b) => Some(bln(nat(lits, a)? != nat(lits, b)?)),
        NatLt(a, b) => Some(bln(nat(lits, a)? < nat(lits, b)?)),
        NatGt(a, b) => Some(bln(nat(lits, a)? > nat(lits, b)?)),
        NatLte(a, b) => Some(bln(nat(lits, a)? <= nat(lits, b)?)),
        NatGte(a, b) => Some(bln(nat(lits, a)? >= nat(lits, b)?)),

        // Int — 31-bit signed.
        IntAdd(a, b) => fits31s(int(lits, a)? as i64 + int(lits, b)? as i64).map(Data::Int),
        IntSub(a, b) => fits31s(int(lits, a)? as i64 - int(lits, b)? as i64).map(Data::Int),
        IntMul(a, b) => fits31s(int(lits, a)? as i64 * int(lits, b)? as i64).map(Data::Int),
        IntDiv(a, b) => {
            fits31s(int(lits, a)? as i64 / (nonzero_s(int(lits, b)?)?) as i64).map(Data::Int)
        }
        IntRem(a, b) => Some(Data::Int(int(lits, a)? % (nonzero_s(int(lits, b)?)?))),
        IntEql(a, b) => Some(bln(int(lits, a)? == int(lits, b)?)),
        IntNeq(a, b) => Some(bln(int(lits, a)? != int(lits, b)?)),
        IntLt(a, b) => Some(bln(int(lits, a)? < int(lits, b)?)),
        IntGt(a, b) => Some(bln(int(lits, a)? > int(lits, b)?)),
        IntLte(a, b) => Some(bln(int(lits, a)? <= int(lits, b)?)),
        IntGte(a, b) => Some(bln(int(lits, a)? >= int(lits, b)?)),

        // Flt — f32, total.
        FltAdd(a, b) => Some(Data::Flt(flt(lits, a)? + flt(lits, b)?)),
        FltSub(a, b) => Some(Data::Flt(flt(lits, a)? - flt(lits, b)?)),
        FltMul(a, b) => Some(Data::Flt(flt(lits, a)? * flt(lits, b)?)),
        FltDiv(a, b) => Some(Data::Flt(flt(lits, a)? / flt(lits, b)?)),
        FltNeg(a) => Some(Data::Flt(-flt(lits, a)?)),
        FltAbs(a) => Some(Data::Flt(flt(lits, a)?.abs())),
        FltSqrt(a) => Some(Data::Flt(flt(lits, a)?.sqrt())),
        FltFloor(a) => Some(Data::Flt(flt(lits, a)?.floor())),
        FltCeil(a) => Some(Data::Flt(flt(lits, a)?.ceil())),
        FltTrunc(a) => Some(Data::Flt(flt(lits, a)?.trunc())),
        FltCopysign(a, b) => Some(Data::Flt(flt(lits, a)?.copysign(flt(lits, b)?))),
        FltEql(a, b) => Some(bln(flt(lits, a)? == flt(lits, b)?)),
        FltNeq(a, b) => Some(bln(flt(lits, a)? != flt(lits, b)?)),
        FltLt(a, b) => Some(bln(flt(lits, a)? < flt(lits, b)?)),
        FltGt(a, b) => Some(bln(flt(lits, a)? > flt(lits, b)?)),
        FltLte(a, b) => Some(bln(flt(lits, a)? <= flt(lits, b)?)),
        FltGte(a, b) => Some(bln(flt(lits, a)? >= flt(lits, b)?)),

        // Nat — shifts, rotates, and bit scans. A left shift or rotate can push a
        // set bit into position 31, which the backend trap-checks; the logical
        // right shift and the scans always land back inside 31 bits.
        NatShl(a, b) => fits31u(nat(lits, a)?.wrapping_shl(nat(lits, b)?) as u64).map(Data::Nat),
        NatShr(a, b) => Some(Data::Nat(nat(lits, a)?.wrapping_shr(nat(lits, b)?))),
        NatRotl(a, b) => fits31u(nat(lits, a)?.rotate_left(nat(lits, b)?) as u64).map(Data::Nat),
        NatRotr(a, b) => fits31u(nat(lits, a)?.rotate_right(nat(lits, b)?) as u64).map(Data::Nat),
        NatClz(a) => Some(Data::Nat(nat(lits, a)?.leading_zeros())),
        NatCtz(a) => Some(Data::Nat(nat(lits, a)?.trailing_zeros())),
        NatPopcnt(a) => Some(Data::Nat(nat(lits, a)?.count_ones())),
        NatEqz(a) => Some(bln(nat(lits, a)? == 0)),

        // Int — bitwise, shifts, rotates, and bit scans. Bitwise, right shift, and
        // the scans are total once the result is reduced to its 31-bit payload
        // (`wrap31s`); the left shift and rotates trap-check like `Int` arithmetic.
        IntAnd(a, b) => Some(Data::Int(wrap31s(int(lits, a)? & int(lits, b)?))),
        IntOr(a, b) => Some(Data::Int(wrap31s(int(lits, a)? | int(lits, b)?))),
        IntXor(a, b) => Some(Data::Int(wrap31s(int(lits, a)? ^ int(lits, b)?))),
        IntShl(a, b) => {
            fits31s(int(lits, a)?.wrapping_shl(int(lits, b)? as u32) as i64).map(Data::Int)
        }
        IntShr(a, b) => Some(Data::Int(wrap31s(
            int(lits, a)?.wrapping_shr(int(lits, b)? as u32),
        ))),
        IntRotl(a, b) => {
            let rotated = (int(lits, a)? as u32).rotate_left(int(lits, b)? as u32) as i32;
            fits31s(rotated as i64).map(Data::Int)
        }
        IntRotr(a, b) => {
            let rotated = (int(lits, a)? as u32).rotate_right(int(lits, b)? as u32) as i32;
            fits31s(rotated as i64).map(Data::Int)
        }
        IntClz(a) => Some(Data::Int((int(lits, a)? as u32).leading_zeros() as i32)),
        IntCtz(a) => Some(Data::Int((int(lits, a)? as u32).trailing_zeros() as i32)),
        IntPopcnt(a) => Some(Data::Int((int(lits, a)? as u32).count_ones() as i32)),
        IntEqz(a) => Some(bln(int(lits, a)? == 0)),

        // Flt — min/max are folded only when neither operand is NaN, the one case
        // where they *value*-diverge (wasm yields NaN, Rust yields the operand);
        // nearest rounds half-to-even, matching `f32.nearest` on every input.
        FltMin(a, b) => flt_minmax(flt(lits, a)?, flt(lits, b)?, f32::min),
        FltMax(a, b) => flt_minmax(flt(lits, a)?, flt(lits, b)?, f32::max),
        FltNearest(a) => Some(Data::Flt(flt(lits, a)?.round_ties_even())),

        // Conversions. The int↔int casts reinterpret the 31-bit payload exactly as
        // the backend does (`ref.i31` then `i31.get_{u,s}`), so a high `Nat` reads
        // back as a negative `Int` and a negative `Int` as a large `Nat`. The
        // float→int casts truncate toward zero and trap-check the 31-bit range.
        NatToInt(a) => Some(Data::Int(wrap31s(nat(lits, a)? as i32))),
        NatToFlt(a) => Some(Data::Flt(nat(lits, a)? as f32)),
        IntToNat(a) => Some(Data::Nat(int(lits, a)? as u32 & 0x7FFF_FFFF)),
        IntToFlt(a) => Some(Data::Flt(int(lits, a)? as f32)),
        FltToNat(a) => flt_to_nat(flt(lits, a)?),
        FltToInt(a) => flt_to_int(flt(lits, a)?),

        // Number → string. Deterministic — every value formats. The output must
        // match `src/run/host.rs`'s free functions byte-for-byte so compile-time
        // folding and runtime conversion agree.
        NatToStr(a) => Some(Data::Bin(format!("{}", nat(lits, a)?).into_bytes())),
        IntToStr(a) => Some(Data::Bin(format!("{:+}", int(lits, a)?).into_bytes())),
        FltToStr(a) => Some(Data::Bin(format!("{:+}", flt(lits, a)?).into_bytes())),

        // Bytewise equality — total whenever both operands are known.
        BinEql(a, b) => Some(bln(bin(lits, a)? == bin(lits, b)?)),

        // Variadic concatenation — total, so always foldable when every operand is
        // a literal of the matching kind.
        BinConcat(operands) => {
            let mut bytes = Vec::new();
            for name in operands {
                match lits.get(name)? {
                    Data::Bin(part) => bytes.extend_from_slice(part),
                    _ => return None,
                }
            }
            Some(Data::Bin(bytes))
        }
        ArrConcat(operands) => {
            let mut elems = Vec::new();
            for name in operands {
                match lits.get(name)? {
                    Data::Arr(part) => elems.extend_from_slice(part),
                    _ => return None,
                }
            }
            Some(Data::Arr(elems))
        }

        // Aggregate builders. A slice needs in-bounds literal indices; an append
        // needs a literal byte for `Bin`, while `Arr` appends any element by
        // reference. Each yields a fresh literal aggregate, so it cascades through
        // further projection and concatenation just like the constructors above.
        BinSlice(b, start, end) => bin_slice(bin(lits, b)?, nat(lits, start)?, nat(lits, end)?),
        ArrSlice(a, start, end) => arr_slice(arr(lits, a)?, nat(lits, start)?, nat(lits, end)?),
        BinAppend(b, byte) => {
            let mut bytes = bin(lits, b)?.to_vec();
            bytes.push(nat(lits, byte)? as u8);
            Some(Data::Bin(bytes))
        }
        ArrAppend(a, elem) => {
            let mut elems = arr(lits, a)?.to_vec();
            elems.push(elem.clone());
            Some(Data::Arr(elems))
        }

        // `*ToStr` (runtime formatter), `Io`, and aggregate *projection* (handled
        // in `project`) are intentionally left untouched.
        _ => None,
    }
}

/// Reduce an `i32` to the 31-bit signed value an `i31ref` round-trip yields: keep
/// the low 31 bits and sign-extend bit 30. Mirrors `ref.i31` then `i31.get_s`, the
/// only way a non-trapping `Int` result is normalized by the backend.
fn wrap31s(value: i32) -> i32 {
    value.wrapping_shl(1) >> 1
}

/// Fold `Flt` min/max only when neither operand is NaN — the sole case where the
/// wasm op and Rust's `f32::min`/`f32::max` disagree on the *value* returned.
fn flt_minmax(left: f32, right: f32, op: fn(f32, f32) -> f32) -> Option<Data> {
    (!left.is_nan() && !right.is_nan()).then(|| Data::Flt(op(left, right)))
}

/// `i32.trunc_f32_u` followed by the backend's 31-bit overflow check: fold only
/// when the truncation toward zero lands in `[0, 2^31)`; anything else traps.
fn flt_to_nat(value: f32) -> Option<Data> {
    let truncated = value.trunc();
    (value.is_finite() && truncated > -1.0 && truncated < 2_147_483_648.0)
        .then_some(Data::Nat(truncated as u32))
}

/// `i32.trunc_f32_s` followed by the backend's 31-bit overflow check: fold only
/// when the truncation toward zero lands in `[-2^30, 2^30)`; anything else traps.
fn flt_to_int(value: f32) -> Option<Data> {
    let truncated = value.trunc();
    (value.is_finite() && (-1_073_741_824.0..1_073_741_824.0).contains(&truncated))
        .then_some(Data::Int(truncated as i32))
}

/// `bytes[start..end]` as a fresh `Bin`, when the bounds are in range; an
/// out-of-range slice traps, so it is left unfolded.
fn bin_slice(bytes: &[u8], start: u32, end: u32) -> Option<Data> {
    let (start, end) = (start as usize, end as usize);
    (start <= end && end <= bytes.len()).then(|| Data::Bin(bytes[start..end].to_vec()))
}

/// `elems[start..end]` as a fresh `Arr`, preserving element references, when the
/// bounds are in range; an out-of-range slice traps, so it is left unfolded.
fn arr_slice(elems: &[ValueName], start: u32, end: u32) -> Option<Data> {
    let (start, end) = (start as usize, end as usize);
    (start <= end && end <= elems.len()).then(|| Data::Arr(elems[start..end].to_vec()))
}

fn nat(lits: &Lits, name: &ValueName) -> Option<u32> {
    match lits.get(name)? {
        Data::Nat(value) => Some(*value),
        _ => None,
    }
}

fn int(lits: &Lits, name: &ValueName) -> Option<i32> {
    match lits.get(name)? {
        Data::Int(value) => Some(*value),
        _ => None,
    }
}

fn flt(lits: &Lits, name: &ValueName) -> Option<f32> {
    match lits.get(name)? {
        Data::Flt(value) => Some(*value),
        _ => None,
    }
}

/// The i31ref representation of a `Bln`.
fn bln(value: bool) -> Data {
    Data::Nat(value as u32)
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
