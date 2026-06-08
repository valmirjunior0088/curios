use {super::*, std::collections::HashMap};

/// Constant folding: evaluate `Value::Eval(code)` bindings whose operands are all
/// known literals, replacing each with its simplified `Value` — a `Pure(result)`
/// for scalar arithmetic, or, for a projection out of a known aggregate, the
/// projected element itself.
///
/// ## Aggregate projection forwarding
///
/// `Tpl`/`Arr`/`Bin` are immutable once built (the IR has no field-store op), so a
/// projection out of a *known* aggregate can be resolved at compile time:
///
/// - `TplGet(t, i)` / `ArrGet(a, i)` where the aggregate is a literal (and, for
///   `ArrGet`, the index is too) forward the `i`-th element. A scalar element
///   becomes a `Pure` literal (so it cascades through further folds); any other
///   element is forwarded by name as a `Value::Alias`, avoiding a copy.
/// - `ArrLen`/`BinLen` fold to the literal length; `BinGet` to the literal byte.
/// - Out-of-bounds access would trap at runtime, so it is left unfolded.
///
/// Once every projection of an aggregate is forwarded, the aggregate itself goes
/// unreferenced and dead-code elimination drops its allocation entirely — the
/// point of running this after inlining brings a constructor next to its consumer.
///
/// ## Match on a known tag
///
/// A `Match` whose operand is a known `Nat` tag is decided at compile time: the
/// whole `Match` tail is replaced by the selected arm's `Jump` (its matching case,
/// or the default). The arms not taken become unreferenced and dead-code
/// elimination drops them. This is the control-flow dual of folding a scalar op,
/// and it relies on jump threading having brought the constructor's tag into the
/// same region as its eliminator.
///
/// ## Semantics follow the wasm backend, not the core reducer
///
/// At this stage `Nat`/`Int` are 31-bit (i31ref) and `Flt` is `f32`; the core
/// reducer works on bignums, so it is *not* the source of truth here. Each fold
/// mirrors `code_emitter`'s lowering exactly:
///
/// - `Nat` is 31-bit unsigned. `add`/`mul` trap on overflow, `sub` is monus
///   (saturating at zero), `div`/`rem` trap on a zero divisor.
/// - `Int` is 31-bit signed (`[-2^30, 2^30 - 1]`). `add`/`sub`/`mul`/`div` trap
///   on overflow, `div`/`rem` trap on a zero divisor.
/// - Comparisons yield `Data::Nat(0 | 1)` — the i31ref representation of `Bln`.
/// - `Flt` is `f32` and total.
///
/// Beyond arithmetic, the same evaluator covers shifts, rotates, and bit scans
/// (`Nat` and `Int`), the bitwise `Int` ops, `Flt` min/max/nearest, every numeric
/// conversion, bytewise equality, and the `Bin`/`Arr` slice and append builders —
/// each mirroring its lowering, including 31-bit wrapping and the value-dependent
/// traps. Only `*ToStr` (a runtime formatter) and `Io` are deliberately left out.
///
/// ## Totality is value-dependent, so the evaluator owns it
///
/// Because the traps above depend on operand *values* (not just the operation),
/// folding cannot be gated by a static purity predicate: it folds only when it
/// can both reproduce the result and prove the runtime would not trap, returning
/// `None` otherwise. Unsupported operations also return `None` and are left
/// untouched — the supported set can grow without changing the structure.
pub fn fold_constants(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        fold_tree(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        fold_tree(&mut clsr.region);
    }
}

type Lits = HashMap<ValueName, Data>;

/// Fold a region tree to a fixed point, so a chain like `v = add(2, 3); w =
/// add(v, 4)` collapses fully (folding `v` to a literal exposes `w`).
fn fold_tree(region: &mut Region) {
    loop {
        let lits = literals(region);

        if !fold_region(region, &lits) {
            break;
        }
    }
}

/// Collect the scalar literals bound in the tree. Names are unique per body and
/// scoping is lexical, so a single tree-wide map is sound.
fn literals(region: &Region) -> Lits {
    let mut lits = Lits::new();
    collect_literals(region, &mut lits);
    lits
}

fn collect_literals(region: &Region, lits: &mut Lits) {
    for (name, value) in &region.values {
        if let Value::Pure(
            data @ (Data::Nat(_)
            | Data::Int(_)
            | Data::Flt(_)
            | Data::Bin(_)
            | Data::Arr(_)
            | Data::Tpl(_)),
        ) = value
        {
            lits.insert(name.clone(), data.clone());
        }
    }

    for (_, block) in &region.blocks {
        collect_literals(&block.region, lits);
    }
}

/// Replace every foldable `Eval` in the tree with its `Pure` result. Returns
/// whether anything changed.
fn fold_region(region: &mut Region, lits: &Lits) -> bool {
    let mut changed = false;

    for (_, value) in &mut region.values {
        if let Value::Eval(code) = value
            && let Some(replacement) = simplify(code, lits)
        {
            *value = replacement;
            changed = true;
        }
    }

    if let Some(jump) = decide_match(&region.tail, lits) {
        region.tail = Tail::Jump(jump);
        changed = true;
    }

    for (_, block) in &mut region.blocks {
        changed |= fold_region(&mut block.region, lits);
    }

    changed
}

/// The arm a `Match` takes when its operand is a known `Nat` tag: the matching
/// case, else the default. A tag with neither is left unfolded.
fn decide_match(tail: &Tail, lits: &Lits) -> Option<JumpTarget> {
    let Tail::Match(target) = tail else {
        return None;
    };
    let Data::Nat(tag) = lits.get(&target.operand)? else {
        return None;
    };

    target.cases.get(tag).or(target.default.as_ref()).cloned()
}

// --- Simplification ---------------------------------------------------------

/// The replacement for an `Eval`, if any: a folded scalar literal, or a forwarded
/// aggregate projection.
fn simplify(code: &Code, lits: &Lits) -> Option<Value> {
    if let Some(data) = eval(code, lits) {
        return Some(Value::Pure(data));
    }

    project(code, lits)
}

/// Resolve a projection out of a known aggregate to the element (or length/byte)
/// it reads. Aggregates are immutable, so this is always sound; out-of-bounds
/// access would trap, so it is left unfolded.
fn project(code: &Code, lits: &Lits) -> Option<Value> {
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

// --- Evaluation -------------------------------------------------------------

fn eval(code: &Code, lits: &Lits) -> Option<Data> {
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
        // `*ToStr` calls a runtime formatter and is left to it.
        NatToInt(a) => Some(Data::Int(wrap31s(nat(lits, a)? as i32))),
        NatToFlt(a) => Some(Data::Flt(nat(lits, a)? as f32)),
        IntToNat(a) => Some(Data::Nat(int(lits, a)? as u32 & 0x7FFF_FFFF)),
        IntToFlt(a) => Some(Data::Flt(int(lits, a)? as f32)),
        FltToNat(a) => flt_to_nat(flt(lits, a)?),
        FltToInt(a) => flt_to_int(flt(lits, a)?),

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

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn b(name: &str) -> BlockName {
        BlockName::from(name)
    }

    fn run(values: Vec<(ValueName, Value)>) -> Vec<(ValueName, Value)> {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("b0"),
                region: Region {
                    preallocs: vec![],
                    values,
                    blocks: vec![],
                    tail: Tail::Jump(JumpTarget {
                        target: BlockName::from("b0"),
                        params: vec![],
                    }),
                },
            },
        );

        fold_constants(&mut module);

        let (_, func) = module.funcs().iter().next().unwrap();
        func.region.values.clone()
    }

    fn folded(values: Vec<(ValueName, Value)>, name: &str) -> Data {
        let values = run(values);
        match &values.iter().find(|(n, _)| *n == v(name)).unwrap().1 {
            Value::Pure(data) => data.clone(),
            other => panic!("expected `{name}` folded to a literal, got {other:?}"),
        }
    }

    fn stays_eval(values: Vec<(ValueName, Value)>, name: &str) {
        let values = run(values);
        assert!(
            matches!(
                &values.iter().find(|(n, _)| *n == v(name)).unwrap().1,
                Value::Eval(_)
            ),
            "expected `{name}` to stay unevaluated",
        );
    }

    fn forwarded(values: Vec<(ValueName, Value)>, name: &str) -> ValueName {
        let values = run(values);
        match &values.iter().find(|(n, _)| *n == v(name)).unwrap().1 {
            Value::Alias(source) => source.clone(),
            other => panic!("expected `{name}` forwarded to an alias, got {other:?}"),
        }
    }

    fn jt(target: &str) -> JumpTarget {
        JumpTarget {
            target: b(target),
            params: vec![],
        }
    }

    fn match_tail(operand: &str, cases: Vec<(u32, &str)>, default: Option<&str>) -> Tail {
        Tail::Match(MatchTarget {
            operand: v(operand),
            cases: cases.into_iter().map(|(tag, t)| (tag, jt(t))).collect(),
            default: default.map(jt),
        })
    }

    /// Fold a region with a custom tail and return the resulting tail.
    fn folded_tail(values: Vec<(ValueName, Value)>, tail: Tail) -> Tail {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: Region {
                    preallocs: vec![],
                    values,
                    blocks: vec![],
                    tail,
                },
            },
        );

        fold_constants(&mut module);

        module.funcs().iter().next().unwrap().1.region.tail.clone()
    }

    #[test]
    fn folds_nat_add() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Nat(2))),
                (v("b"), Value::Pure(Data::Nat(3))),
                (v("c"), Value::Eval(Code::NatAdd(v("a"), v("b")))),
            ],
            "c",
        );
        assert!(matches!(data, Data::Nat(5)));
    }

    #[test]
    fn folds_chain_to_a_fixed_point() {
        // c = a + b = 5; d = c + e = 9
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Nat(2))),
                (v("b"), Value::Pure(Data::Nat(3))),
                (v("e"), Value::Pure(Data::Nat(4))),
                (v("c"), Value::Eval(Code::NatAdd(v("a"), v("b")))),
                (v("d"), Value::Eval(Code::NatAdd(v("c"), v("e")))),
            ],
            "d",
        );
        assert!(matches!(data, Data::Nat(9)));
    }

    #[test]
    fn comparison_folds_to_bln_nat() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Nat(2))),
                (v("b"), Value::Pure(Data::Nat(3))),
                (v("c"), Value::Eval(Code::NatLt(v("a"), v("b")))),
            ],
            "c",
        );
        assert!(matches!(data, Data::Nat(1)));
    }

    #[test]
    fn nat_monus_saturates() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Nat(3))),
                (v("b"), Value::Pure(Data::Nat(5))),
                (v("c"), Value::Eval(Code::NatSub(v("a"), v("b")))),
            ],
            "c",
        );
        assert!(matches!(data, Data::Nat(0)));
    }

    #[test]
    fn does_not_fold_nat_overflow() {
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Nat(1 << 30))),
                (v("b"), Value::Pure(Data::Nat(1 << 30))),
                (v("c"), Value::Eval(Code::NatAdd(v("a"), v("b")))),
            ],
            "c",
        );
    }

    #[test]
    fn does_not_fold_division_by_zero() {
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Nat(7))),
                (v("b"), Value::Pure(Data::Nat(0))),
                (v("c"), Value::Eval(Code::NatDiv(v("a"), v("b")))),
            ],
            "c",
        );
    }

    #[test]
    fn does_not_fold_or_panic_on_runtime_dividend() {
        // A literal divisor with a non-literal dividend (`p` is never bound): the
        // fold must bail, not read the absent dividend and panic. `runtime % const`
        // and `runtime / const` are ordinary user code (digit extraction, halving).
        for make in [
            Code::NatDiv as fn(ValueName, ValueName) -> Code,
            Code::NatRem,
        ] {
            stays_eval(
                vec![
                    (v("b"), Value::Pure(Data::Nat(3))),
                    (v("c"), Value::Eval(make(v("p"), v("b")))),
                ],
                "c",
            );
        }

        stays_eval(
            vec![
                (v("b"), Value::Pure(Data::Int(3))),
                (v("c"), Value::Eval(Code::IntRem(v("p"), v("b")))),
            ],
            "c",
        );
    }

    #[test]
    fn does_not_fold_non_literal_operand() {
        // `p` is never bound to a literal here, so the op is left alone.
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Nat(2))),
                (v("c"), Value::Eval(Code::NatAdd(v("a"), v("p")))),
            ],
            "c",
        );
    }

    #[test]
    fn folds_flt_arithmetic() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Flt(1.5))),
                (v("b"), Value::Pure(Data::Flt(2.0))),
                (v("c"), Value::Eval(Code::FltMul(v("a"), v("b")))),
            ],
            "c",
        );
        match data {
            Data::Flt(value) => assert_eq!(value, 3.0),
            other => panic!("expected flt, got {other:?}"),
        }
    }

    #[test]
    fn folds_bin_concat() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Bin(vec![1, 2]))),
                (v("b"), Value::Pure(Data::Bin(vec![3]))),
                (v("c"), Value::Eval(Code::BinConcat(vec![v("a"), v("b")]))),
            ],
            "c",
        );
        match data {
            Data::Bin(bytes) => assert_eq!(bytes, vec![1, 2, 3]),
            other => panic!("expected bin, got {other:?}"),
        }
    }

    #[test]
    fn folds_arr_concat_preserving_element_references() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Arr(vec![v("x"), v("y")]))),
                (v("b"), Value::Pure(Data::Arr(vec![v("z")]))),
                (v("c"), Value::Eval(Code::ArrConcat(vec![v("a"), v("b")]))),
            ],
            "c",
        );
        match data {
            Data::Arr(elems) => assert_eq!(elems, vec![v("x"), v("y"), v("z")]),
            other => panic!("expected arr, got {other:?}"),
        }
    }

    #[test]
    fn does_not_fold_concat_with_non_literal_operand() {
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Bin(vec![1]))),
                (v("c"), Value::Eval(Code::BinConcat(vec![v("a"), v("p")]))),
            ],
            "c",
        );
    }

    #[test]
    fn forwards_tpl_projection_by_reference() {
        // A non-literal element is forwarded by name, not copied.
        let source = forwarded(
            vec![
                (v("t"), Value::Pure(Data::Tpl(vec![v("x"), v("y")]))),
                (v("w"), Value::Eval(Code::TplGet(v("t"), 1))),
            ],
            "w",
        );
        assert_eq!(source, v("y"));
    }

    #[test]
    fn folds_tpl_projection_of_scalar_literal_and_cascades() {
        // The projected element is a literal, so it inlines and the dependent add
        // folds in the same fixed point: w = t.0 = 7; z = w + 1 = 8.
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Nat(7))),
                (v("one"), Value::Pure(Data::Nat(1))),
                (v("t"), Value::Pure(Data::Tpl(vec![v("a")]))),
                (v("w"), Value::Eval(Code::TplGet(v("t"), 0))),
                (v("z"), Value::Eval(Code::NatAdd(v("w"), v("one")))),
            ],
            "z",
        );
        assert!(matches!(data, Data::Nat(8)));
    }

    #[test]
    fn forwards_arr_get_with_literal_index() {
        let source = forwarded(
            vec![
                (
                    v("arr"),
                    Value::Pure(Data::Arr(vec![v("x"), v("y"), v("z")])),
                ),
                (v("i"), Value::Pure(Data::Nat(2))),
                (v("w"), Value::Eval(Code::ArrGet(v("arr"), v("i")))),
            ],
            "w",
        );
        assert_eq!(source, v("z"));
    }

    #[test]
    fn folds_arr_and_bin_len() {
        let arr_len = folded(
            vec![
                (
                    v("arr"),
                    Value::Pure(Data::Arr(vec![v("x"), v("y"), v("z")])),
                ),
                (v("n"), Value::Eval(Code::ArrLen(v("arr")))),
            ],
            "n",
        );
        assert!(matches!(arr_len, Data::Nat(3)));

        let bin_len = folded(
            vec![
                (v("b"), Value::Pure(Data::Bin(vec![1, 2, 3, 4]))),
                (v("n"), Value::Eval(Code::BinLen(v("b")))),
            ],
            "n",
        );
        assert!(matches!(bin_len, Data::Nat(4)));
    }

    #[test]
    fn folds_bin_get_to_byte() {
        let data = folded(
            vec![
                (v("b"), Value::Pure(Data::Bin(vec![10, 20, 30]))),
                (v("i"), Value::Pure(Data::Nat(1))),
                (v("w"), Value::Eval(Code::BinGet(v("b"), v("i")))),
            ],
            "w",
        );
        assert!(matches!(data, Data::Nat(20)));
    }

    #[test]
    fn does_not_fold_out_of_bounds_access() {
        stays_eval(
            vec![
                (v("arr"), Value::Pure(Data::Arr(vec![v("x")]))),
                (v("i"), Value::Pure(Data::Nat(5))),
                (v("w"), Value::Eval(Code::ArrGet(v("arr"), v("i")))),
            ],
            "w",
        );
    }

    #[test]
    fn does_not_fold_projection_with_non_literal_index() {
        stays_eval(
            vec![
                (v("arr"), Value::Pure(Data::Arr(vec![v("x"), v("y")]))),
                (v("w"), Value::Eval(Code::ArrGet(v("arr"), v("i")))),
            ],
            "w",
        );
    }

    #[test]
    fn decides_match_on_known_tag() {
        let tail = folded_tail(
            vec![(v("t"), Value::Pure(Data::Nat(1)))],
            match_tail("t", vec![(0, "a0"), (1, "a1")], None),
        );
        match tail {
            Tail::Jump(jump) => assert_eq!(jump.target, b("a1")),
            other => panic!("expected jump to the selected arm, got {other:?}"),
        }
    }

    #[test]
    fn decides_match_to_default_when_no_case_matches() {
        let tail = folded_tail(
            vec![(v("t"), Value::Pure(Data::Nat(5)))],
            match_tail("t", vec![(0, "a0"), (1, "a1")], Some("fallback")),
        );
        match tail {
            Tail::Jump(jump) => assert_eq!(jump.target, b("fallback")),
            other => panic!("expected jump to the default arm, got {other:?}"),
        }
    }

    #[test]
    fn does_not_decide_match_on_unknown_operand() {
        let tail = folded_tail(vec![], match_tail("t", vec![(0, "a0"), (1, "a1")], None));
        assert!(matches!(tail, Tail::Match(_)));
    }

    #[test]
    fn does_not_decide_match_without_a_matching_case_or_default() {
        let tail = folded_tail(
            vec![(v("t"), Value::Pure(Data::Nat(5)))],
            match_tail("t", vec![(0, "a0"), (1, "a1")], None),
        );
        assert!(matches!(tail, Tail::Match(_)));
    }

    fn unary(operand: Data, code: impl Fn(ValueName) -> Code) -> Data {
        folded(
            vec![
                (v("a"), Value::Pure(operand)),
                (v("r"), Value::Eval(code(v("a")))),
            ],
            "r",
        )
    }

    fn binary(left: Data, right: Data, code: impl Fn(ValueName, ValueName) -> Code) -> Data {
        folded(
            vec![
                (v("a"), Value::Pure(left)),
                (v("b"), Value::Pure(right)),
                (v("r"), Value::Eval(code(v("a"), v("b")))),
            ],
            "r",
        )
    }

    #[test]
    fn folds_nat_shifts() {
        assert!(matches!(
            binary(Data::Nat(3), Data::Nat(2), Code::NatShl),
            Data::Nat(12)
        ));
        assert!(matches!(
            binary(Data::Nat(12), Data::Nat(2), Code::NatShr),
            Data::Nat(3)
        ));
    }

    #[test]
    fn does_not_fold_nat_shift_overflow() {
        // 2^30 << 1 sets bit 31, which the backend traps on.
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Nat(1 << 30))),
                (v("b"), Value::Pure(Data::Nat(1))),
                (v("r"), Value::Eval(Code::NatShl(v("a"), v("b")))),
            ],
            "r",
        );
    }

    #[test]
    fn folds_nat_rotate_or_traps_on_overflow() {
        assert!(matches!(
            binary(Data::Nat(1), Data::Nat(2), Code::NatRotl),
            Data::Nat(4)
        ));
        // 2^30 rotated left by 1 lands on bit 31 — a trap.
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Nat(1 << 30))),
                (v("b"), Value::Pure(Data::Nat(1))),
                (v("r"), Value::Eval(Code::NatRotl(v("a"), v("b")))),
            ],
            "r",
        );
    }

    #[test]
    fn folds_nat_bit_scans() {
        assert!(matches!(unary(Data::Nat(1), Code::NatClz), Data::Nat(31)));
        assert!(matches!(unary(Data::Nat(8), Code::NatCtz), Data::Nat(3)));
        assert!(matches!(unary(Data::Nat(7), Code::NatPopcnt), Data::Nat(3)));
        assert!(matches!(unary(Data::Nat(0), Code::NatEqz), Data::Nat(1)));
        assert!(matches!(unary(Data::Nat(5), Code::NatEqz), Data::Nat(0)));
    }

    #[test]
    fn folds_int_bitwise() {
        assert!(matches!(
            binary(Data::Int(5), Data::Int(2), Code::IntAnd),
            Data::Int(0)
        ));
        assert!(matches!(
            binary(Data::Int(5), Data::Int(2), Code::IntOr),
            Data::Int(7)
        ));
        // -1 is all 31 bits set; xor with 1 clears the low bit, staying negative.
        assert!(matches!(
            binary(Data::Int(-1), Data::Int(1), Code::IntXor),
            Data::Int(-2)
        ));
    }

    #[test]
    fn folds_int_shifts() {
        assert!(matches!(
            binary(Data::Int(3), Data::Int(2), Code::IntShl),
            Data::Int(12)
        ));
        // Arithmetic right shift preserves the sign.
        assert!(matches!(
            binary(Data::Int(-8), Data::Int(1), Code::IntShr),
            Data::Int(-4)
        ));
    }

    #[test]
    fn does_not_fold_int_shift_overflow() {
        // 2^29 << 1 = 2^30, just past the signed 31-bit range — a trap.
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Int(1 << 29))),
                (v("b"), Value::Pure(Data::Int(1))),
                (v("r"), Value::Eval(Code::IntShl(v("a"), v("b")))),
            ],
            "r",
        );
    }

    #[test]
    fn folds_int_bit_scans_over_the_sign_extended_word() {
        assert!(matches!(unary(Data::Int(-1), Code::IntClz), Data::Int(0)));
        assert!(matches!(
            unary(Data::Int(-1), Code::IntPopcnt),
            Data::Int(32)
        ));
        // `Eqz` yields a `Bln`, represented as `Nat(0 | 1)` like every comparison.
        assert!(matches!(unary(Data::Int(0), Code::IntEqz), Data::Nat(1)));
    }

    #[test]
    fn folds_flt_min_max_for_non_nan() {
        match binary(Data::Flt(1.0), Data::Flt(2.0), Code::FltMin) {
            Data::Flt(value) => assert_eq!(value, 1.0),
            other => panic!("expected flt, got {other:?}"),
        }
        match binary(Data::Flt(1.0), Data::Flt(2.0), Code::FltMax) {
            Data::Flt(value) => assert_eq!(value, 2.0),
            other => panic!("expected flt, got {other:?}"),
        }
    }

    #[test]
    fn does_not_fold_flt_min_with_nan() {
        // wasm returns NaN here; Rust would return the operand, so we leave it.
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Flt(f32::NAN))),
                (v("b"), Value::Pure(Data::Flt(2.0))),
                (v("r"), Value::Eval(Code::FltMin(v("a"), v("b")))),
            ],
            "r",
        );
    }

    #[test]
    fn folds_flt_nearest_ties_to_even() {
        // 2.5 rounds to 2.0 (not 3.0), distinguishing ties-to-even from round-away.
        match unary(Data::Flt(2.5), Code::FltNearest) {
            Data::Flt(value) => assert_eq!(value, 2.0),
            other => panic!("expected flt, got {other:?}"),
        }
        match unary(Data::Flt(3.5), Code::FltNearest) {
            Data::Flt(value) => assert_eq!(value, 4.0),
            other => panic!("expected flt, got {other:?}"),
        }
    }

    #[test]
    fn folds_int_int_conversions_by_reinterpreting_the_payload() {
        assert!(matches!(unary(Data::Nat(5), Code::NatToInt), Data::Int(5)));
        // A `Nat` with bit 30 set reads back as a negative `Int`.
        assert!(matches!(
            unary(Data::Nat(1 << 30), Code::NatToInt),
            Data::Int(-1_073_741_824)
        ));
        // A negative `Int` reads back as a large `Nat` (its low 31 bits).
        assert!(matches!(
            unary(Data::Int(-1), Code::IntToNat),
            Data::Nat(0x7FFF_FFFF)
        ));
    }

    #[test]
    fn folds_int_to_float_conversions() {
        match unary(Data::Nat(5), Code::NatToFlt) {
            Data::Flt(value) => assert_eq!(value, 5.0),
            other => panic!("expected flt, got {other:?}"),
        }
        match unary(Data::Int(-3), Code::IntToFlt) {
            Data::Flt(value) => assert_eq!(value, -3.0),
            other => panic!("expected flt, got {other:?}"),
        }
    }

    #[test]
    fn folds_float_to_int_conversions_in_range() {
        // Truncation toward zero.
        assert!(matches!(
            unary(Data::Flt(3.7), Code::FltToNat),
            Data::Nat(3)
        ));
        assert!(matches!(
            unary(Data::Flt(-0.5), Code::FltToNat),
            Data::Nat(0)
        ));
        assert!(matches!(
            unary(Data::Flt(-3.7), Code::FltToInt),
            Data::Int(-3)
        ));
        // The inclusive lower bound of the signed range folds.
        assert!(matches!(
            unary(Data::Flt(-1_073_741_824.0), Code::FltToInt),
            Data::Int(-1_073_741_824)
        ));
    }

    #[test]
    fn does_not_fold_float_to_int_out_of_range() {
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Flt(-1.0))),
                (v("r"), Value::Eval(Code::FltToNat(v("a")))),
            ],
            "r",
        );
        stays_eval(
            vec![
                (v("a"), Value::Pure(Data::Flt(1_073_741_824.0))),
                (v("r"), Value::Eval(Code::FltToInt(v("a")))),
            ],
            "r",
        );
    }

    #[test]
    fn folds_bin_eql() {
        assert!(matches!(
            binary(Data::Bin(vec![1, 2]), Data::Bin(vec![1, 2]), Code::BinEql),
            Data::Nat(1)
        ));
        assert!(matches!(
            binary(Data::Bin(vec![1, 2]), Data::Bin(vec![1, 3]), Code::BinEql),
            Data::Nat(0)
        ));
    }

    #[test]
    fn folds_bin_slice_in_bounds() {
        let data = folded(
            vec![
                (v("b"), Value::Pure(Data::Bin(vec![1, 2, 3, 4]))),
                (v("s"), Value::Pure(Data::Nat(1))),
                (v("e"), Value::Pure(Data::Nat(3))),
                (v("r"), Value::Eval(Code::BinSlice(v("b"), v("s"), v("e")))),
            ],
            "r",
        );
        match data {
            Data::Bin(bytes) => assert_eq!(bytes, vec![2, 3]),
            other => panic!("expected bin, got {other:?}"),
        }
    }

    #[test]
    fn folds_arr_slice_preserving_references() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Arr(vec![v("x"), v("y"), v("z")]))),
                (v("s"), Value::Pure(Data::Nat(1))),
                (v("e"), Value::Pure(Data::Nat(3))),
                (v("r"), Value::Eval(Code::ArrSlice(v("a"), v("s"), v("e")))),
            ],
            "r",
        );
        match data {
            Data::Arr(elems) => assert_eq!(elems, vec![v("y"), v("z")]),
            other => panic!("expected arr, got {other:?}"),
        }
    }

    #[test]
    fn does_not_fold_out_of_bounds_slice() {
        stays_eval(
            vec![
                (v("b"), Value::Pure(Data::Bin(vec![1, 2]))),
                (v("s"), Value::Pure(Data::Nat(0))),
                (v("e"), Value::Pure(Data::Nat(9))),
                (v("r"), Value::Eval(Code::BinSlice(v("b"), v("s"), v("e")))),
            ],
            "r",
        );
    }

    #[test]
    fn folds_bin_append_truncating_the_byte() {
        // 258 & 0xFF == 2: the byte is stored modulo 256, as the array set does.
        let data = folded(
            vec![
                (v("b"), Value::Pure(Data::Bin(vec![1, 2]))),
                (v("x"), Value::Pure(Data::Nat(258))),
                (v("r"), Value::Eval(Code::BinAppend(v("b"), v("x")))),
            ],
            "r",
        );
        match data {
            Data::Bin(bytes) => assert_eq!(bytes, vec![1, 2, 2]),
            other => panic!("expected bin, got {other:?}"),
        }
    }

    #[test]
    fn folds_arr_append_by_reference() {
        // The appended element need not be a literal; it is kept by name.
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Arr(vec![v("x"), v("y")]))),
                (v("r"), Value::Eval(Code::ArrAppend(v("a"), v("z")))),
            ],
            "r",
        );
        match data {
            Data::Arr(elems) => assert_eq!(elems, vec![v("x"), v("y"), v("z")]),
            other => panic!("expected arr, got {other:?}"),
        }
    }
}
