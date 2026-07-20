use super::*;

#[test]
fn traps_and_effects_classify_by_operation() {
    assert!(Semantics::operation(Operation::NatDiv).observable.may_trap);
    assert!(
        Semantics::operation(Operation::FltToNat)
            .observable
            .may_trap
    );
    assert!(!Semantics::operation(Operation::NatAdd).is_observable());
    assert!(!Semantics::operation(Operation::BlnNeq).is_observable());
    assert!(Semantics::sequence(SequenceOp::LstGet).observable.may_trap);
    assert!(!Semantics::sequence(SequenceOp::LstLen).is_observable());
    assert!(
        Semantics::sequence(SequenceOp::LstAppend)
            .operational
            .allocation
            == Allocation::Immutable
    );
    assert!(Semantics::cell(CellOperation::New).is_observable());
    assert!(Semantics::cell(CellOperation::Get).observable.state_read);
    assert!(Semantics::cell(CellOperation::Set).observable.state_write);
}

#[test]
fn constructing_functions_is_dormant_and_aggregates_are_discardable() {
    // Dormancy: binding a function performs nothing. Immutable allocation is
    // operational but not observable — an unused product is deletable.
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
    let unit = ErasedAtom::Constant(ConstantId(0));
    assert!(!Semantics::terminator(&Terminator::Return(unit)).is_effectful());
    assert!(Semantics::terminator(&Terminator::Exit(unit)).may_exit);
    assert!(Semantics::terminator(&Terminator::Unreachable).may_trap);
}

#[test]
fn the_join_is_a_union() {
    let trap = Semantics::operation(Operation::NatDiv);
    let host = Semantics::local_behavior(&Rhs::Foreign {
        foreign: ForeignId(0),
        operands: vec![],
    });
    let both = trap.join(host);
    assert!(both.observable.may_trap && both.observable.host_effect);
    assert!(LocalBehavior::unknown().is_observable());
}
