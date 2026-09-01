use {
    super::{
        CpsAtom, CpsContId, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsIntrinsic,
        CpsLiteral, CpsModule, CpsNode, CpsNodeId, CpsRow, CpsSlot, CpsUseTarget, CpsValueExpr,
        CpsValueId, FieldGroup,
    },
    std::collections::BTreeMap,
};

/// Splitting a lower parameter after a higher one moves the higher group along: recording a start without shifting what follows it leaves a record the verifier reads as overlapping, which is how this was found.
#[test]
fn a_later_split_moves_every_group_past_it() {
    let mut module = CpsModule::new();
    let continuation = CpsContId(0);
    module.record_split(continuation, 3, 3);
    module.record_split(continuation, 1, 3);
    assert_eq!(
        module.field_groups().get(&continuation),
        Some(&vec![
            FieldGroup { start: 1, width: 3 },
            FieldGroup { start: 5, width: 3 },
        ]),
    );
}

fn minimal_module() -> CpsModule {
    let mut module = CpsModule::new();
    let fun = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let result = module.add_value(Some("result".into()));
    let return_node = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(result)],
    }));
    let body = module.add_node(CpsNode::LetValue {
        result,
        value: CpsValueExpr::Literal(CpsLiteral::Nat(0)),
        next: return_node,
    });
    module.define_function(
        fun,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont,
            body,
        },
    );
    module.set_entry(fun);
    module
}

#[test]
fn registers_and_rewires_uses() {
    let mut module = minimal_module();
    let old = module
        .values()
        .iter()
        .enumerate()
        .find_map(|(index, value)| {
            (value.as_ref()?.debug_name.as_deref() == Some("result"))
                .then_some(CpsValueId(index as u32))
        })
        .unwrap();
    let replacement = module.add_value(Some("replacement".into()));
    let entry = module.entry().unwrap();
    module
        .functions
        .get_mut(entry)
        .unwrap()
        .params
        .push(replacement);
    let count = |module: &CpsModule, value| module.value_use_counts().get(&value).copied();
    assert_eq!(count(&module, old), Some(1));
    module.replace_atom(CpsUseTarget::Value(old), CpsAtom::Value(replacement));
    assert_eq!(count(&module, old), None);
    assert_eq!(count(&module, replacement), Some(1));
    module.verify().unwrap();
}

#[test]
fn verifier_rejects_an_existing_but_out_of_scope_value() {
    let mut module = minimal_module();
    let result = module
        .values()
        .iter()
        .enumerate()
        .find_map(|(index, value)| {
            (value.as_ref()?.debug_name.as_deref() == Some("result"))
                .then_some(CpsValueId(index as u32))
        })
        .unwrap();
    let orphan = module.add_value(Some("orphan".into()));
    module.replace_atom(CpsUseTarget::Value(result), CpsAtom::Value(orphan));

    let error = module.verify().unwrap_err();
    assert!(error.to_string().contains("out-of-scope"));
}

#[test]
fn node_ids_are_not_reused_after_tombstoning() {
    let mut module = minimal_module();
    let removed = CpsNodeId(0);
    module.remove_node(removed).unwrap();
    let fresh = module.add_node(CpsNode::Unreachable);
    assert!(fresh.0 > removed.0);
}

#[test]
fn verifier_rejects_intrinsic_arity_mismatch() {
    let mut module = minimal_module();
    let result = module.add_value(None);
    let next = module.add_node(CpsNode::Unreachable);
    module.add_node(CpsNode::LetIntrinsic {
        result,
        op: CpsIntrinsic::NatAdd,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
        next,
    });
    let bad = CpsNodeId((module.nodes.len() - 1) as u32);
    module.functions.get_mut(CpsFunId(0)).unwrap().body = bad;
    assert!(
        module
            .verify()
            .unwrap_err()
            .0
            .contains("expects 2 operands")
    );
}

/// The vocabulary clause of `verify_rows`: a value minted as a structural tuple and read as a row — the rebuild `split_returns` used to emit for a class whose only own return edges were tail calls — is refused here, not at the `ref.cast` the emitter would otherwise produce for it. The mirror mismatch, a row read structurally, is refused by the same clause.
#[test]
fn verifier_rejects_a_read_in_the_other_vocabulary() {
    for (minted_as_row, read_as_row) in [(false, true), (true, false)] {
        let mut module = minimal_module();
        let row = module.add_row(CpsRow {
            debug_name: Some("Option".into()),
            slots: vec![CpsSlot::Tag, CpsSlot::Opaque],
        });
        let built = module.add_value(Some("built".into()));
        let field = module.add_value(Some("field".into()));
        // Ahead of the minimal body rather than in place of it, so every other clause of the verifier is satisfied and the vocabulary one is the only thing left to refuse.
        let next = module.function(CpsFunId(0)).unwrap().body;
        let atoms = vec![
            CpsAtom::Literal(CpsLiteral::Nat(0)),
            CpsAtom::Literal(CpsLiteral::Nat(0)),
        ];
        let read = module.add_node(CpsNode::LetIntrinsic {
            result: field,
            op: match read_as_row {
                true => CpsIntrinsic::RowGet(row, 1),
                false => CpsIntrinsic::TupleGet(1),
            },
            args: vec![CpsAtom::Value(built)],
            next,
        });
        let construction = module.add_node(CpsNode::LetValue {
            result: built,
            value: match minted_as_row {
                true => CpsValueExpr::Row(row, atoms),
                false => CpsValueExpr::Tuple(atoms),
            },
            next: read,
        });
        module.functions.get_mut(CpsFunId(0)).unwrap().body = construction;

        let error = module.verify().unwrap_err().0;
        assert!(
            error.contains("was built as") && error.contains("but is read as"),
            "{error}"
        );
    }
}

/// The vocabulary clause's one legal violation, and why the round boundary must not check it: constant folding pushes a decided reply's payload into both arms of its dispatch, so the dead arm reads the payload in the other vocabulary with only the decided tag keeping it honest, until a later round threads the switch and prunes the arm. The full verify keeps refusing the state — the entry and exit gates check it where its premise holds — while `verify_structure`, the round boundary's set, leaves the vocabulary clause to convergence.
#[test]
fn the_round_boundary_accepts_the_dead_arm_only_convergence_removes() {
    let mut module = minimal_module();
    let row = module.add_row(CpsRow {
        debug_name: Some("Refusal".into()),
        slots: vec![CpsSlot::Tag, CpsSlot::Opaque],
    });
    let built = module.add_value(Some("built".into()));
    let field = module.add_value(Some("field".into()));
    let live_body = module.function(CpsFunId(0)).unwrap().body;
    let return_cont = module.function(CpsFunId(0)).unwrap().return_cont;

    // The dead arm reads the value in the row vocabulary its live construction below does not carry.
    let dead = module.reserve_continuation();
    let live = module.reserve_continuation();
    let dead_return = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(field)],
    }));
    let dead_read = module.add_node(CpsNode::LetIntrinsic {
        result: field,
        op: CpsIntrinsic::RowGet(row, 1),
        args: vec![CpsAtom::Value(built)],
        next: dead_return,
    });
    module.define_continuation(
        dead,
        CpsContinuation {
            debug_name: None,
            params: vec![],
            body: dead_read,
        },
    );
    module.define_continuation(
        live,
        CpsContinuation {
            debug_name: None,
            params: vec![],
            body: live_body,
        },
    );

    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Literal(CpsLiteral::Nat(0)),
        cases: BTreeMap::from([
            (
                0,
                CpsEdge {
                    target: live,
                    args: vec![],
                },
            ),
            (
                1,
                CpsEdge {
                    target: dead,
                    args: vec![],
                },
            ),
        ]),
        default: None,
    });
    let let_cont = module.add_node(CpsNode::LetCont {
        continuations: vec![live, dead],
        body: switch,
    });
    let construction = module.add_node(CpsNode::LetValue {
        result: built,
        value: CpsValueExpr::Tuple(vec![
            CpsAtom::Literal(CpsLiteral::Nat(0)),
            CpsAtom::Literal(CpsLiteral::Nat(0)),
        ]),
        next: let_cont,
    });
    module.functions.get_mut(CpsFunId(0)).unwrap().body = construction;

    // The full set refuses the mismatch; the boundary set accepts everything else about the module and leaves the mismatch to the exit gate.
    assert!(module.verify().unwrap_err().0.contains("was built as"));
    module
        .verify_structure()
        .expect("the boundary leaves the vocabulary clause to convergence");
}

#[test]
fn list_map_is_not_an_intrinsic_opcode() {
    assert!(CpsIntrinsic::ListAppend.allocates());
    assert!(!CpsIntrinsic::NatAdd.is_total());
}

#[test]
fn every_guarded_operation_is_classified_as_trapping() {
    // Found by reading `into_wasm`'s emission against this table rather than by a failure: each of these emits a guard — the first through the same checked helper as siblings already listed, the last three through an inline `Unreachable` — while the wildcard this match replaced answered `Total` for all of them, which is `eliminate_dead_bindings` deleting a refusal.
    for op in [
        CpsIntrinsic::IntShl,
        CpsIntrinsic::NatToInt,
        CpsIntrinsic::IntToNat,
        CpsIntrinsic::FltOfLeBytes,
    ] {
        assert!(op.may_trap(), "{op:?} emits a guard but is not `MayTrap`");
        assert!(!op.is_total(), "{op:?} must not be deletable when dead");
    }

    // The controls that keep the rule from being "guard everything": monus saturates and a right shift only clears bits, so neither can leave the envelope.
    assert!(CpsIntrinsic::NatSub.is_total());
    assert!(CpsIntrinsic::NatShr.is_total());
    assert!(CpsIntrinsic::IntShr.is_total());
}

#[test]
fn return_continuation_is_a_bodyless_non_tombstone_slot() {
    let module = minimal_module();
    let function = module.function(module.entry().unwrap()).unwrap();
    assert!(module.continuation(function.return_cont).is_none());
    assert_eq!(module.tombstones().3, 0);
    module.verify().unwrap();
}

#[test]
fn verifier_rejects_shared_return_continuations() {
    let mut module = minimal_module();
    let shared_return = module
        .function(module.entry().unwrap())
        .unwrap()
        .return_cont;
    let second = module.reserve_function();
    let body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: shared_return,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
    }));
    module.define_function(
        second,
        CpsFunction {
            debug_name: Some("second".into()),
            params: vec![],
            return_cont: shared_return,
            body,
        },
    );
    assert!(
        module
            .verify()
            .unwrap_err()
            .0
            .contains("return continuation of both")
    );
}

#[test]
fn verifier_rejects_another_functions_return_target() {
    let mut module = minimal_module();
    let second = module.reserve_function();
    let second_return = module.reserve_continuation();
    let second_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: second_return,
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
    }));
    module.define_function(
        second,
        CpsFunction {
            debug_name: Some("second".into()),
            params: vec![],
            return_cont: second_return,
            body: second_body,
        },
    );
    let entry = module.entry().unwrap();
    let entry_body = module.function(entry).unwrap().body;
    module.nodes.set(
        entry_body,
        CpsNode::ApplyCont(CpsEdge {
            target: second_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }),
    );
    assert!(
        module
            .verify()
            .unwrap_err()
            .0
            .contains("references ~f1's return continuation")
    );
}

#[test]
fn verifier_rejects_undefined_non_return_continuation() {
    let mut module = minimal_module();
    let undefined = module.reserve_continuation();
    let entry = module.entry().unwrap();
    let entry_body = module.function(entry).unwrap().body;
    module.nodes.set(
        entry_body,
        CpsNode::ApplyCont(CpsEdge {
            target: undefined,
            args: vec![],
        }),
    );
    assert!(
        module
            .verify()
            .unwrap_err()
            .0
            .contains("undefined or out-of-scope continuation")
    );
}

#[test]
fn verifier_rejects_local_body_at_return_id() {
    let mut module = minimal_module();
    let entry = module.entry().unwrap();
    let return_cont = module.function(entry).unwrap().return_cont;
    let local_body = module.add_node(CpsNode::Unreachable);
    module.define_continuation(
        return_cont,
        CpsContinuation {
            debug_name: Some("invalid-return-body".into()),
            params: vec![],
            body: local_body,
        },
    );
    assert!(
        module
            .verify()
            .unwrap_err()
            .0
            .contains("also identifies a local continuation")
    );
}
