use {
    crate::*,
    curios_num::{Binary, Grain, Integer, Natural},
};

#[test]
fn traps_and_effects_classify_by_operation() {
    // The float narrowings keep their trap for non-finite and out-of-range input.
    assert!(
        Semantics::operation(Operation::FltToNat)
            .observable
            .may_trap
    );
    // No division carries a trap: `/sys` states a nonzero divisor as a precondition, so a term reaching here was refused if it could not supply one, and `IntDiv`'s signed overflow is none either, since a quotient past the i31 grows into a boxed magnitude.
    assert!(!Semantics::operation(Operation::NatDiv).is_observable());
    assert!(!Semantics::operation(Operation::IntDiv).is_observable());
    assert!(!Semantics::operation(Operation::NatRem).is_observable());
    assert!(!Semantics::operation(Operation::IntRem).is_observable());
    assert!(!Semantics::operation(Operation::NatAdd).is_observable());
    assert!(!Semantics::operation(Operation::BoolNeq).is_observable());
    assert!(Semantics::sequence(SequenceOp::ListGet).observable.may_trap);
    assert!(!Semantics::sequence(SequenceOp::ListLen).is_observable());
    assert!(
        Semantics::sequence(SequenceOp::ListAppend)
            .operational
            .allocation
            == Allocation::Immutable
    );
    assert!(Semantics::cell(CellOperation::New).is_observable());
    assert!(
        Semantics::cell(CellOperation::Poll {
            some: ConstructorId(0),
            none: ConstructorId(1)
        })
        .observable
        .state_read
    );
    assert!(Semantics::cell(CellOperation::Fill).observable.state_write);
}

#[test]
fn a_decode_that_folds_to_a_trap_is_classified_as_one() {
    // The classifier and the folder have to name the same set, and `FltOfLeBytes` is where they can disagree: `TrapKind::MalformedInput` exists for this operation alone, and a classifier reporting it pure would let `prune` drop a top-level item whose only observable effect is the malformed decode.
    let eight = Constant::Bin(
        Grain::X,
        Binary::from_bytes(vec![0, 0, 0, 0, 0, 0, 0xf0, 0x3f]),
    );
    let four = Constant::Bin(Grain::X, Binary::from_bytes(vec![0, 0, 0x80, 0x3f]));

    assert!(matches!(
        Semantics::fold_operation(Operation::FltOfLeBytes, &[four], u64::MAX),
        FoldOutcome::WouldTrap(TrapKind::MalformedInput)
    ));
    assert!(matches!(
        Semantics::fold_operation(Operation::FltOfLeBytes, &[eight], u64::MAX),
        FoldOutcome::Value(Constant::Flt(_))
    ));
    assert!(
        Semantics::operation(Operation::FltOfLeBytes)
            .observable
            .may_trap
    );
}

#[test]
fn constructing_functions_is_dormant_and_aggregates_are_discardable() {
    // Dormancy: binding a function performs nothing. Immutable allocation is operational but not observable — an unused product is deletable.
    let product = Rhs::Product {
        schema: ProductId(0),
        fields: vec![],
    };
    let behavior = Semantics::local_behavior(&product);
    assert_eq!(behavior.operational.allocation, Allocation::Immutable);
    assert!(!behavior.is_observable());
}

#[test]
fn terminators_report_exit_and_trap() {
    let unit = Atom::Constant(ConstantId(0));
    assert!(!Semantics::terminator(&Terminator::Return(unit)).is_effectful());
    let halt = Terminator::Halt {
        foreign: ForeignId(0),
        operands: vec![unit],
    };
    assert!(Semantics::terminator(&halt).may_exit);
    assert!(Semantics::terminator(&Terminator::Unreachable).may_trap);
}

#[test]
fn the_join_is_a_union() {
    let trap = Semantics::operation(Operation::FltToNat);
    let host = Semantics::local_behavior(&Rhs::Foreign {
        foreign: ForeignId(0),
        operands: vec![],
    });
    let both = trap.join(host);
    assert!(both.observable.may_trap && both.observable.host_effect);
    assert!(LocalBehavior::unknown().is_observable());
}

/// Each listing is written once and read twice: as the list returned, and as the patterns of an exhaustive match every listed operation is put through, so an operation the alphabet gains fails to compile here until it is listed.
macro_rules! listed {
    ($($pattern:pat => $operation:expr),+ $(,)?) => {{
        let covered = |operation: Operation| match operation {
            $($pattern => ()),+
        };
        let every = vec![$($operation),+];
        every.iter().copied().for_each(covered);
        every
    }};
}

/// Every scalar operation, one rounding direction standing for all of an operation that rounds: the oracle names none of them a monoid in any direction.
fn every_operation() -> Vec<Operation> {
    use {Operation::*, curios_num::Rounding::TiesToEven};
    listed![
        BoolAnd => BoolAnd,
        BoolOr => BoolOr,
        BoolXor => BoolXor,
        BoolEql => BoolEql,
        BoolNeq => BoolNeq,
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
        ByteToNat => ByteToNat,
        NatToByte => NatToByte,
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
        FltAdd(_) => FltAdd(TiesToEven),
        FltSub(_) => FltSub(TiesToEven),
        FltMul(_) => FltMul(TiesToEven),
        FltDiv(_) => FltDiv(TiesToEven),
        FltFma(_) => FltFma(TiesToEven),
        FltRem => FltRem,
        FltEql => FltEql,
        FltNeq => FltNeq,
        FltLt => FltLt,
        FltLe => FltLe,
        FltMin => FltMin,
        FltMax => FltMax,
        FltCopysign => FltCopysign,
        FltNeg => FltNeg,
        FltAbs => FltAbs,
        FltSqrt(_) => FltSqrt(TiesToEven),
        FltRoundIntegral(_) => FltRoundIntegral(TiesToEven),
        NatToInt => NatToInt,
        NatToFlt(_) => NatToFlt(TiesToEven),
        IntToNat => IntToNat,
        IntToFlt(_) => IntToFlt(TiesToEven),
        FltToNat => FltToNat,
        FltToInt => FltToInt,
        FltMantissa => FltMantissa,
        FltExponent => FltExponent,
        FltToLeBytes => FltToLeBytes,
        FltOfLeBytes => FltOfLeBytes,
    ]
}

/// Values of the carrier `identity` is of, around the points where the carrier's representation changes: zero, the i31's edge, and past a machine word, on both signs where it has them.
fn samples_beside(identity: &Constant) -> Vec<Constant> {
    match identity {
        Constant::Bool(_) => vec![Constant::Bool(false), Constant::Bool(true)],
        Constant::Nat(_) => [0u128, 1, 2, 7, (1 << 30) - 1, 1 << 30, (1 << 64) + 3]
            .into_iter()
            .map(|value| Constant::Nat(Natural::from(value)))
            .collect(),
        Constant::Int(_) => [
            -(1i128 << 64) - 3,
            -(1 << 30) - 1,
            -(1 << 30),
            -7,
            -1,
            0,
            1,
            2,
            (1 << 30) - 1,
            (1 << 64) + 3,
        ]
        .into_iter()
        .map(|value| Constant::Int(Integer::from(value)))
        .collect(),
        other => panic!("no samples of {other:?}'s carrier"),
    }
}

/// Every operation the oracle names a monoid is one, by its own fold: its identity leaves every sample unchanged on both sides, every two samples commute, which is what lets the rebase fold the addends on both sides of a recursion into one accumulator, and every three associate.
#[test]
fn every_monoid_the_oracle_names_keeps_its_laws_under_its_fold() {
    let mut carriers = Vec::new();
    for operation in every_operation() {
        let Some(identity) = Semantics::monoid_identity(operation) else {
            continue;
        };
        carriers.push(std::mem::discriminant(&identity));
        let fold = |left: &Constant, right: &Constant| match Semantics::fold_operation(
            operation,
            &[left.clone(), right.clone()],
            1 << 12,
        ) {
            FoldOutcome::Value(value) => value,
            other => panic!("{operation:?} does not fold {left:?} and {right:?}: {other:?}"),
        };
        let samples = samples_beside(&identity);
        for a in &samples {
            assert_eq!(
                fold(&identity, a),
                *a,
                "{operation:?}'s identity on the left of {a:?}"
            );
            assert_eq!(
                fold(a, &identity),
                *a,
                "{operation:?}'s identity on the right of {a:?}"
            );
            for b in &samples {
                assert_eq!(fold(a, b), fold(b, a), "{operation:?} over {a:?} and {b:?}");
                for c in &samples {
                    assert_eq!(
                        fold(&fold(a, b), c),
                        fold(a, &fold(b, c)),
                        "{operation:?} over {a:?}, {b:?} and {c:?}"
                    );
                }
            }
        }
    }
    // A carrier the oracle names no monoid of would let the laws hold of nothing.
    for carrier in [
        Constant::Bool(true),
        Constant::Nat(Natural::zero()),
        Constant::Int(Integer::from(0u32)),
    ] {
        assert!(carriers.contains(&std::mem::discriminant(&carrier)));
    }
}
