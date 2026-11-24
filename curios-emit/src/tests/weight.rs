//! Every intrinsic's weight, read off its lowering: what one application emits over operands the representation analysis holds nothing of, weighed as [`Intrinsic::weight_of`] weighs instructions.

use {
    crate::test_support::*,
    curios_cps::Intrinsic,
    curios_num::{Natural, Rounding},
};

/// One of every intrinsic, each beside the pattern naming its variant, so a variant added to the vocabulary refuses to compile here until it is listed.
macro_rules! every {
    ($($op:expr => $pattern:pat),* $(,)?) => {{
        fn _listed(op: Intrinsic) {
            match op {
                $($pattern => {})*
            }
        }
        vec![$($op),*]
    }};
}

/// Every intrinsic, a variadic form at arities two to four, eight and sixteen.
fn every_intrinsic(row: curios_cps::RowId) -> Vec<Intrinsic> {
    use Intrinsic::*;
    let mut every = every! {
        BoolAnd => BoolAnd,
        BoolOr => BoolOr,
        NatEql => NatEql,
        NatNeq => NatNeq,
        NatAdd => NatAdd,
        NatSub => NatSub,
        NatMul => NatMul,
        NatLt => NatLt,
        NatDiv => NatDiv,
        NatRem => NatRem,
        NatLe => NatLe,
        NatAnd => NatAnd,
        NatOr => NatOr,
        NatXor => NatXor,
        NatMin => NatMin,
        NatMax => NatMax,
        NatShl => NatShl,
        NatPow => NatPow,
        NatShr => NatShr,
        NatEqz => NatEqz,
        NatToInt => NatToInt,
        NatToFlt(Rounding::TiesToEven) => NatToFlt(_),
        IntEql => IntEql,
        IntNeq => IntNeq,
        IntAdd => IntAdd,
        IntSub => IntSub,
        IntMul => IntMul,
        IntDiv => IntDiv,
        IntRem => IntRem,
        IntLt => IntLt,
        IntLe => IntLe,
        IntAnd => IntAnd,
        IntOr => IntOr,
        IntXor => IntXor,
        IntMin => IntMin,
        IntMax => IntMax,
        IntAbs => IntAbs,
        IntSign => IntSign,
        IntShl => IntShl,
        IntShr => IntShr,
        IntEqz => IntEqz,
        IntToNat => IntToNat,
        IntToFlt(Rounding::TiesToEven) => IntToFlt(_),
        FltAdd(Rounding::TiesToEven) => FltAdd(_),
        FltSub(Rounding::TiesToEven) => FltSub(_),
        FltMul(Rounding::TiesToEven) => FltMul(_),
        FltDiv(Rounding::TiesToEven) => FltDiv(_),
        FltFma(Rounding::TiesToEven) => FltFma(_),
        FltRem => FltRem,
        FltEql => FltEql,
        FltNeq => FltNeq,
        FltLt => FltLt,
        FltLe => FltLe,
        FltMin => FltMin,
        FltMax => FltMax,
        FltNeg => FltNeg,
        FltAbs => FltAbs,
        FltSqrt(Rounding::TiesToEven) => FltSqrt(_),
        FltRoundIntegral(Rounding::TiesToEven) => FltRoundIntegral(_),
        FltCopysign => FltCopysign,
        FltToNat => FltToNat,
        FltToLeBytes => FltToLeBytes,
        FltOfLeBytes => FltOfLeBytes,
        FltToInt => FltToInt,
        FltMantissa => FltMantissa,
        FltExponent => FltExponent,
        BytesLen => BytesLen,
        BytesEql => BytesEql,
        BytesGet => BytesGet,
        BytesSlice => BytesSlice,
        BytesRest => BytesRest,
        BytesAppend => BytesAppend,
        BytesConcat(2) => BytesConcat(_),
        BytesReplicate => BytesReplicate,
        BytesAnd => BytesAnd,
        BytesOr => BytesOr,
        BytesXor => BytesXor,
        BytesChunk(2) => BytesChunk(_),
        ListLen => ListLen,
        ListGet => ListGet,
        ListSlice => ListSlice,
        ListSet => ListSet,
        ListOverwrite => ListOverwrite,
        ListRest => ListRest,
        ListAppend => ListAppend,
        ListConcat(2) => ListConcat(_),
        ListSettle => ListSettle,
        ListFlat(2) => ListFlat(_),
        TupleGet(0) => TupleGet(_),
        RowGet(row, 1) => RowGet(..),
        WindowExtent => WindowExtent,
        IsImmediate => IsImmediate,
        ImmediateGet => ImmediateGet,
        IsRow(row) => IsRow(_),
    };
    every.extend([3, 4, 8, 16].into_iter().flat_map(|arity| {
        [
            BytesConcat(arity),
            BytesChunk(arity),
            ListConcat(arity),
            ListFlat(arity),
        ]
    }));
    every
}

/// A `Nat` past the i31: a reference, which an operation reading a number tests before taking its fast path.
fn wide() -> curios_cps::Atom {
    curios_cps::Atom::Literal(curios_cps::Literal::Nat(Natural::from(1u64 << 40)))
}

/// A module whose entry hands `f` an argument of each representation `op` reads and lets `f` escape, so its parameters hold whatever a caller could hand them, references nothing is known of; `f` binds `op` over its parameters where `applied`, and answers its first parameter either way.
fn calling(op: Intrinsic, applied: bool) -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let row = module.add_row(curios_cps::Row {
        debug_name: Some("pair".into()),
        slots: vec![curios_cps::Slot::Tag, curios_cps::Slot::Opaque],
    });
    let op = match op {
        Intrinsic::RowGet(_, slot) => Intrinsic::RowGet(row, slot),
        Intrinsic::IsRow(_) => Intrinsic::IsRow(row),
        op => op,
    };
    let f = module.reserve_function();
    let f_return = module.reserve_continuation();
    let params = (0..op.arity())
        .map(|_| module.add_value(None))
        .collect::<Vec<_>>();
    // Both return the first parameter, so what the answer's return would add, a word boxed or a float allocated, is no part of the difference.
    let mut body = module.add_node(curios_cps::Node::ApplyCont(curios_cps::Edge {
        target: f_return,
        args: vec![curios_cps::Atom::Value(params[0])],
    }));
    if applied {
        let answer = module.add_value(Some("answer".into()));
        body = module.add_node(curios_cps::Node::LetIntrinsic {
            result: answer,
            op,
            args: params
                .iter()
                .map(|param| curios_cps::Atom::Value(*param))
                .collect(),
            next: body,
        });
    }
    module.define_function(
        f,
        curios_cps::Function {
            lineage: None,
            debug_name: Some("f".into()),
            params,
            return_cont: f_return,
            body,
            droppable: curios_cps::Droppable::Never,
            terminates: false,
        },
    );

    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let resume = module.reserve_continuation();
    let ignored = module.add_value(None);
    let exit = module.add_node(halt_zero());
    module.define_continuation(
        resume,
        curios_cps::Continuation {
            lineage: None,
            debug_name: None,
            params: vec![ignored],
            body: exit,
            terminates: false,
        },
    );
    // Each argument a value a binding below makes, or a literal; the bindings are made innermost first, so the call is built before them.
    let mut bindings = Vec::new();
    let args = (0..op.arity())
        .map(|index| {
            let bound = |bindings: &mut Vec<_>, module: &mut curios_cps::Module, value| {
                let id = module.add_value(None);
                bindings.push((id, value));
                curios_cps::Atom::Value(id)
            };
            match (op, op.operand_repr(index)) {
                (_, curios_cps::Repr::Number | curios_cps::Repr::Length) => wide(),
                (_, curios_cps::Repr::Nat) => nat(1),
                (_, curios_cps::Repr::Flt) => flt(1.5),
                (_, curios_cps::Repr::Bytes) => bytes_lit(vec![1, 2, 3, 4]),
                (_, curios_cps::Repr::List) => bound(
                    &mut bindings,
                    &mut module,
                    curios_cps::ValueExpr::List(vec![wide(), wide()]),
                ),
                (Intrinsic::TupleGet(_), curios_cps::Repr::Ref) => bound(
                    &mut bindings,
                    &mut module,
                    curios_cps::ValueExpr::Tuple(vec![wide()]),
                ),
                (Intrinsic::RowGet(..) | Intrinsic::IsRow(_), curios_cps::Repr::Ref) => bound(
                    &mut bindings,
                    &mut module,
                    curios_cps::ValueExpr::Row(row, vec![nat(0), wide()]),
                ),
                (_, curios_cps::Repr::Ref) => wide(),
            }
        })
        .collect::<Vec<_>>();
    let mut body = module.add_node(curios_cps::Node::ApplyFun {
        callee: curios_cps::Callee::Known(f),
        args,
        return_to: resume,
    });
    let escaped = module.add_value(Some("escaped".into()));
    bindings.push((
        escaped,
        curios_cps::ValueExpr::Tuple(vec![curios_cps::Atom::Fun(f)]),
    ));
    for (result, value) in bindings.into_iter().rev() {
        body = module.add_node(curios_cps::Node::LetValue {
            result,
            value,
            next: body,
        });
    }
    let body = module.add_node(curios_cps::Node::LetCont {
        continuations: vec![resume],
        body,
    });
    let body = module.add_node(curios_cps::Node::LetFun {
        functions: vec![f],
        body,
    });
    module.define_function(
        main,
        curios_cps::Function {
            lineage: None,
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: main_return,
            body,
            droppable: curios_cps::Droppable::Never,
            terminates: false,
        },
    );
    module.set_entry(main);
    module
}

/// The instructions an expression holds, those nested in a block, a loop or either arm of an `if` counted with it.
fn instructions(instrs: &[curios_wasm::Instr]) -> usize {
    instrs
        .iter()
        .map(|instr| {
            1 + match instr {
                curios_wasm::Instr::Block {
                    instructions: nested,
                    ..
                }
                | curios_wasm::Instr::Loop {
                    instructions: nested,
                    ..
                } => instructions(nested),
                curios_wasm::Instr::If {
                    then_instructions,
                    else_instructions,
                    ..
                } => instructions(then_instructions) + instructions(else_instructions),
                _ => 0,
            }
        })
        .sum()
}

/// The instructions the body of the function `f` of `module` emits, which the emitter names after its hint, apart from the closure it is also made into.
fn emitted(module: &curios_cps::Module) -> usize {
    let (mut wasm, _) = crate::into_wasm(module, None);
    let counts = wasm
        .funcs_mut()
        .filter(|(name, _)| {
            let name = name.to_string();
            name.starts_with("func/") && name.ends_with("$f")
        })
        .map(|(_, func)| instructions(&func.expr.instrs))
        .collect::<Vec<_>>();
    assert_eq!(counts.len(), 1, "one function is emitted for `f`");
    counts[0]
}

#[test]
fn every_intrinsic_weighs_what_its_lowering_emits() {
    // Any row: `calling` reads a row operation over the row its own module declares.
    let wrong = every_intrinsic(curios_cps::RowId(0))
        .into_iter()
        .filter_map(|op| {
            let added = emitted(&calling(op, true)).saturating_sub(emitted(&calling(op, false)));
            let weight = Intrinsic::weight_of(added);
            (op.weight() != weight).then(|| {
                format!(
                    "{op:?}: states {}, emits {added} instructions, {weight}",
                    op.weight()
                )
            })
        })
        .collect::<Vec<_>>();
    assert!(wrong.is_empty(), "{}", wrong.join("\n"));
}
