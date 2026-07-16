use super::*;

/// Constant folding: evaluate `Value::Eval(code)` bindings whose operands are all
/// known literals, replacing each with its simplified `Value` — a `Pure(result)`
/// for scalar arithmetic, or, for a projection out of a known aggregate, the
/// projected element itself.
///
/// ## Aggregate projection forwarding
///
/// `Tpl`/`Lst`/`Bin` are immutable once built (the IR has no field-store op), so a
/// projection out of a *known* aggregate can be resolved at compile time:
///
/// - `TplGet(t, i)` / `LstGet(a, i)` where the aggregate is a literal (and, for
///   `LstGet`, the index is too) forward the `i`-th element. A scalar element
///   becomes a `Pure` literal (so it cascades through further folds); any other
///   element is forwarded by name as a `Value::Alias`, avoiding a copy.
/// - `LstLen`/`BinLen` fold to the literal length; `BinGet` to the literal byte.
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
/// conversion, bytewise equality, and the `Bin`/`Lst` slice and append builders —
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
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub(crate) fn fold_constants(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        fold_tree(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        fold_tree(&mut clsr.region);
    }
}

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

/// Replace every foldable `Eval` in the tree with its `Pure` result. Returns
/// whether anything changed.
fn fold_region(region: &mut Region, lits: &Lits) -> bool {
    let mut changed = false;

    for (_, value) in &mut region.values {
        if let Value::Eval(code) = value
            && let Some(replacement) = simplify(code, lits)
        {
            *value = match replacement {
                Evaluated::Scalar(scalar) => Value::Pure(scalar.into()),
                Evaluated::Lst(elems) => Value::Pure(Data::Lst(elems)),
                Evaluated::Elem(source) => Value::Alias(source),
            };
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

#[cfg(test)]
mod tests {
    use {
        super::*,
        curios_base::{Grain, PackedBin},
    };

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
    fn folds_flt_to_le_bytes() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Flt(1.5))),
                (v("b"), Value::Eval(Code::FltToLeBytes(v("a")))),
            ],
            "b",
        );
        match data {
            Data::Bin(Grain::X, bytes) => {
                assert_eq!(bytes.as_bytes(), Some(1.5f32.to_le_bytes().as_slice()))
            }
            other => panic!("expected bin, got {other:?}"),
        }
    }

    #[test]
    fn folds_bin_concat() {
        let data = folded(
            vec![
                (
                    v("a"),
                    Value::Pure(Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2]))),
                ),
                (
                    v("b"),
                    Value::Pure(Data::Bin(Grain::X, PackedBin::from_bytes(vec![3]))),
                ),
                (
                    v("c"),
                    Value::Eval(Code::BinConcat(Grain::X, vec![v("a"), v("b")])),
                ),
            ],
            "c",
        );
        match data {
            Data::Bin(Grain::X, bytes) => {
                assert_eq!(bytes.as_bytes(), Some(&[1, 2, 3][..]))
            }
            other => panic!("expected bin, got {other:?}"),
        }
    }

    #[test]
    fn folds_lst_concat_preserving_element_references() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Lst(vec![v("x"), v("y")]))),
                (v("b"), Value::Pure(Data::Lst(vec![v("z")]))),
                (v("c"), Value::Eval(Code::LstConcat(vec![v("a"), v("b")]))),
            ],
            "c",
        );
        match data {
            Data::Lst(elems) => assert_eq!(elems, vec![v("x"), v("y"), v("z")]),
            other => panic!("expected lst, got {other:?}"),
        }
    }

    #[test]
    fn does_not_fold_concat_with_non_literal_operand() {
        stays_eval(
            vec![
                (
                    v("a"),
                    Value::Pure(Data::Bin(Grain::X, PackedBin::from_bytes(vec![1]))),
                ),
                (
                    v("c"),
                    Value::Eval(Code::BinConcat(Grain::X, vec![v("a"), v("p")])),
                ),
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
    fn forwards_lst_get_with_literal_index() {
        let source = forwarded(
            vec![
                (
                    v("lst"),
                    Value::Pure(Data::Lst(vec![v("x"), v("y"), v("z")])),
                ),
                (v("i"), Value::Pure(Data::Nat(2))),
                (v("w"), Value::Eval(Code::LstGet(v("lst"), v("i")))),
            ],
            "w",
        );
        assert_eq!(source, v("z"));
    }

    #[test]
    fn folds_lst_and_bin_len() {
        let lst_len = folded(
            vec![
                (
                    v("lst"),
                    Value::Pure(Data::Lst(vec![v("x"), v("y"), v("z")])),
                ),
                (v("n"), Value::Eval(Code::LstLen(v("lst")))),
            ],
            "n",
        );
        assert!(matches!(lst_len, Data::Nat(3)));

        let bin_len = folded(
            vec![
                (
                    v("b"),
                    Value::Pure(Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2, 3, 4]))),
                ),
                (v("n"), Value::Eval(Code::BinLen(Grain::X, v("b")))),
            ],
            "n",
        );
        assert!(matches!(bin_len, Data::Nat(4)));
    }

    #[test]
    fn folds_bin_get_to_byte() {
        let data = folded(
            vec![
                (
                    v("b"),
                    Value::Pure(Data::Bin(Grain::X, PackedBin::from_bytes(vec![10, 20, 30]))),
                ),
                (v("i"), Value::Pure(Data::Nat(1))),
                (v("w"), Value::Eval(Code::BinGet(Grain::X, v("b"), v("i")))),
            ],
            "w",
        );
        assert!(matches!(data, Data::Nat(20)));
    }

    #[test]
    fn does_not_fold_out_of_bounds_access() {
        stays_eval(
            vec![
                (v("lst"), Value::Pure(Data::Lst(vec![v("x")]))),
                (v("i"), Value::Pure(Data::Nat(5))),
                (v("w"), Value::Eval(Code::LstGet(v("lst"), v("i")))),
            ],
            "w",
        );
    }

    #[test]
    fn does_not_fold_projection_with_non_literal_index() {
        stays_eval(
            vec![
                (v("lst"), Value::Pure(Data::Lst(vec![v("x"), v("y")]))),
                (v("w"), Value::Eval(Code::LstGet(v("lst"), v("i")))),
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
    fn folds_nat_shl_truncating() {
        // 2^30 << 1 would set bit 31, which the i31 carrier drops — a truncating
        // left shift, folded to match the backend rather than trapping.
        assert!(matches!(
            binary(Data::Nat(1 << 30), Data::Nat(1), Code::NatShl),
            Data::Nat(0)
        ));
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
    fn folds_int_shl_truncating() {
        // 2^29 << 1 sets bit 30 — the sign bit of the 31-bit carrier — so the
        // truncating left shift folds to -2^30 rather than trapping, matching
        // `Nat/shl` and the backend's `ref.i31`.
        assert!(matches!(
            binary(Data::Int(1 << 29), Data::Int(1), Code::IntShl),
            Data::Int(v) if v == -(1 << 30)
        ));
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
            binary(
                Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2])),
                Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2])),
                |left, right| Code::BinEql(Grain::X, left, right)
            ),
            Data::Nat(1)
        ));
        assert!(matches!(
            binary(
                Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2])),
                Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 3])),
                |left, right| Code::BinEql(Grain::X, left, right)
            ),
            Data::Nat(0)
        ));
    }

    #[test]
    fn folds_bin_slice_in_bounds() {
        let data = folded(
            vec![
                (
                    v("b"),
                    Value::Pure(Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2, 3, 4]))),
                ),
                (v("s"), Value::Pure(Data::Nat(1))),
                (v("e"), Value::Pure(Data::Nat(3))),
                (
                    v("r"),
                    Value::Eval(Code::BinSlice(Grain::X, v("b"), v("s"), v("e"))),
                ),
            ],
            "r",
        );
        match data {
            Data::Bin(Grain::X, bytes) => {
                assert_eq!(bytes.as_bytes(), Some(&[2, 3][..]))
            }
            other => panic!("expected bin, got {other:?}"),
        }
    }

    #[test]
    fn folds_lst_slice_preserving_references() {
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Lst(vec![v("x"), v("y"), v("z")]))),
                (v("s"), Value::Pure(Data::Nat(1))),
                (v("e"), Value::Pure(Data::Nat(3))),
                (v("r"), Value::Eval(Code::LstSlice(v("a"), v("s"), v("e")))),
            ],
            "r",
        );
        match data {
            Data::Lst(elems) => assert_eq!(elems, vec![v("y"), v("z")]),
            other => panic!("expected lst, got {other:?}"),
        }
    }

    #[test]
    fn does_not_fold_out_of_bounds_slice() {
        stays_eval(
            vec![
                (
                    v("b"),
                    Value::Pure(Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2]))),
                ),
                (v("s"), Value::Pure(Data::Nat(0))),
                (v("e"), Value::Pure(Data::Nat(9))),
                (
                    v("r"),
                    Value::Eval(Code::BinSlice(Grain::X, v("b"), v("s"), v("e"))),
                ),
            ],
            "r",
        );
    }

    #[test]
    fn folds_bin_append_with_a_byte() {
        let data = folded(
            vec![
                (
                    v("b"),
                    Value::Pure(Data::Bin(Grain::X, PackedBin::from_bytes(vec![1, 2]))),
                ),
                (v("x"), Value::Pure(Data::Nat(255))),
                (
                    v("r"),
                    Value::Eval(Code::BinAppend(Grain::X, v("b"), v("x"))),
                ),
            ],
            "r",
        );
        match data {
            Data::Bin(Grain::X, bytes) => {
                assert_eq!(bytes.as_bytes(), Some(&[1, 2, 255][..]))
            }
            other => panic!("expected bin, got {other:?}"),
        }
    }

    #[test]
    fn folds_lst_append_by_reference() {
        // The appended element need not be a literal; it is kept by name.
        let data = folded(
            vec![
                (v("a"), Value::Pure(Data::Lst(vec![v("x"), v("y")]))),
                (v("r"), Value::Eval(Code::LstAppend(v("a"), v("z")))),
            ],
            "r",
        );
        match data {
            Data::Lst(elems) => assert_eq!(elems, vec![v("x"), v("y"), v("z")]),
            other => panic!("expected lst, got {other:?}"),
        }
    }
}
