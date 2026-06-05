use {
    super::*,
    std::collections::HashMap,
};

/// Constant folding: evaluate `Value::Eval(code)` bindings whose operands are all
/// known literals, replacing them with `Value::Pure(result)`.
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
/// ## Totality is value-dependent, so the evaluator owns it
///
/// Because the traps above depend on operand *values* (not just the operation),
/// folding cannot be gated by a static purity predicate: it folds only when it
/// can both reproduce the result and prove the runtime would not trap, returning
/// `None` otherwise. Unsupported operations (conversions, `Bin`/`Arr`/`Tpl`
/// access, `Io`, shifts, rotates, bit scans) also return `None` and are left
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
            data @ (Data::Nat(_) | Data::Int(_) | Data::Flt(_) | Data::Bin(_) | Data::Arr(_)),
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
            && let Some(data) = eval(code, lits)
        {
            *value = Value::Pure(data);
            changed = true;
        }
    }

    for (_, block) in &mut region.blocks {
        changed |= fold_region(&mut block.region, lits);
    }

    changed
}

// --- Evaluation -------------------------------------------------------------

fn eval(code: &Code, lits: &Lits) -> Option<Data> {
    use Code::*;

    match code {
        // Nat — 31-bit unsigned.
        NatAdd(a, b) => fits31u(nat(lits, a)? as u64 + nat(lits, b)? as u64).map(Data::Nat),
        NatSub(a, b) => Some(Data::Nat(nat(lits, a)?.saturating_sub(nat(lits, b)?))),
        NatMul(a, b) => fits31u(nat(lits, a)? as u64 * nat(lits, b)? as u64).map(Data::Nat),
        NatDiv(a, b) => nonzero_u(nat(lits, b)?).map(|d| Data::Nat(nat(lits, a).unwrap() / d)),
        NatRem(a, b) => nonzero_u(nat(lits, b)?).map(|d| Data::Nat(nat(lits, a).unwrap() % d)),
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
            let divisor = nonzero_s(int(lits, b)?)?;
            fits31s(int(lits, a)? as i64 / divisor as i64).map(Data::Int)
        }
        IntRem(a, b) => nonzero_s(int(lits, b)?).map(|d| Data::Int(int(lits, a).unwrap() % d)),
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

        // `Flt` min/max and nearest differ from Rust's NaN/rounding semantics, and
        // conversions, the remaining `Bin`/`Arr`/`Tpl` access ops, `Io`, shifts,
        // rotates, and bit scans are not yet handled — leave them as is.
        _ => None,
    }
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
    ((-(1 << 30))..(1 << 30)).contains(&value).then_some(value as i32)
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
}
