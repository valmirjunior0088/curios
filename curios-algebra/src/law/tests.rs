//! The law table: what each family states, and that every declaration it makes is well formed.

use crate::{Carrier, Constant, Expr, Family, Operation, Position, TABLE, laws, round_trip};

#[test]
fn a_unit_at_both_sides_states_both_laws() {
    let stated =
        Family::Unit(Position::Both, Constant::Zero).laws(Carrier::Natural, Operation::Sum);
    assert_eq!(stated.len(), 2);
    let zero = Expr::Constant {
        value: Constant::Zero,
        carrier: Carrier::Natural,
    };
    let x = Expr::Var {
        index: 0,
        carrier: Carrier::Natural,
    };
    assert_eq!(
        stated[0].left,
        Expr::Apply {
            carrier: Carrier::Natural,
            operation: Operation::Sum,
            operands: vec![x.clone(), zero.clone()],
        }
    );
    assert_eq!(stated[1].right, x);
}

#[test]
fn every_inverse_pair_the_table_declares_is_a_round_trip_the_algebra_takes() {
    for declared in TABLE {
        if declared.families.contains(&Family::InversePair) {
            let Operation::Conversion { from } = declared.operation else {
                panic!("an inverse pair is declared at a conversion");
            };
            assert!(
                round_trip(declared.carrier, from),
                "{:?} from {from:?}",
                declared.carrier
            );
            assert!(
                declared.operation.undoes(
                    declared.carrier,
                    Operation::Conversion {
                        from: declared.carrier
                    },
                    from
                ),
                "{:?} from {from:?}",
                declared.carrier
            );
        }
    }
}

#[test]
fn no_operation_is_declared_twice_at_one_carrier() {
    for (at, declared) in TABLE.iter().enumerate() {
        for other in &TABLE[at + 1..] {
            assert!(
                (declared.carrier, declared.operation) != (other.carrier, other.operation),
                "{:?} {:?}",
                declared.carrier,
                declared.operation
            );
        }
    }
}

#[test]
fn every_family_states_at_least_one_law() {
    for (declared, law) in laws() {
        assert_ne!(
            law.left, law.right,
            "{:?} {:?}",
            declared.carrier, law.family
        );
    }
    assert!(!laws().is_empty());
}
