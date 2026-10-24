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

/// The connectives of `Bool`, every one of which the table declares.
const CONNECTIVES: [Operation; 5] = [
    Operation::And,
    Operation::Or,
    Operation::Xor,
    Operation::Equal,
    Operation::Unequal,
];

/// Every family a `Bool` connective could be declared with, one side at a time, at `Bool`'s constants and over its connectives.
fn boolean_candidates() -> Vec<Family> {
    let mut candidates = vec![
        Family::Idempotence,
        Family::Commutativity,
        Family::Associativity,
        Family::NestedCancellation,
    ];
    for constant in [Constant::True, Constant::False] {
        for side in [Position::Left, Position::Right] {
            candidates.push(Family::Unit(side, constant));
            candidates.push(Family::Absorber(side, constant));
        }
        candidates.push(Family::SelfCancellation(constant));
        candidates.push(Family::Complement(constant));
    }
    for over in CONNECTIVES {
        candidates.push(Family::Distribution(over));
        candidates.push(Family::Dual(over, false));
        candidates.push(Family::Dual(over, true));
    }
    candidates
}

/// `family` one side at a time: a family stated at both positions is the two stated at each.
fn sides(family: Family) -> Vec<Family> {
    match family {
        Family::Unit(Position::Both, constant) => vec![
            Family::Unit(Position::Left, constant),
            Family::Unit(Position::Right, constant),
        ],
        Family::Absorber(Position::Both, constant) => vec![
            Family::Absorber(Position::Left, constant),
            Family::Absorber(Position::Right, constant),
        ],
        family => vec![family],
    }
}

/// `expr`'s value where its variables take the bits of `assignment`, each variable the bit at its index.
fn evaluated(expr: &Expr, assignment: u32) -> bool {
    match expr {
        Expr::Var { index, .. } => assignment >> index & 1 == 1,
        Expr::Constant {
            value: Constant::True,
            ..
        } => true,
        Expr::Constant {
            value: Constant::False,
            ..
        } => false,
        Expr::Apply {
            operation,
            operands,
            ..
        } => {
            let (left, right) = (
                evaluated(&operands[0], assignment),
                evaluated(&operands[1], assignment),
            );
            match operation {
                Operation::And => left && right,
                Operation::Or => left || right,
                Operation::Xor | Operation::Unequal => left != right,
                Operation::Equal => left == right,
                _ => unreachable!("a connective of `Bool`"),
            }
        }
        Expr::Constant { .. } => unreachable!("a constant of `Bool`"),
    }
}

/// Whether every law `family` states of `operation` at `Bool` holds at every value of its variables.
fn holds(family: Family, operation: Operation) -> bool {
    family.laws(Carrier::Boolean, operation).iter().all(|law| {
        (0..8)
            .all(|assignment| evaluated(&law.left, assignment) == evaluated(&law.right, assignment))
    })
}

/// Whether `family` follows from the families declared beside it, which leaves it undeclared: a commutative operation's dual with its operands swapped, given the dual as written, and an idempotent operation's distribution over itself.
fn given(family: Family, operation: Operation) -> bool {
    match family {
        Family::Dual(over, true) => {
            holds(Family::Commutativity, over) && holds(Family::Dual(over, false), operation)
        }
        Family::Distribution(over) => over == operation && holds(Family::Idempotence, operation),
        _ => false,
    }
}

/// The table declares of each `Bool` connective exactly the families that hold of it at every value, but for what declared families give: the rung decides every identity of the connectives, so a family left out is a law the audit never holds, and a family declared that fails is a law conversion would be asked to decide wrongly.
#[test]
fn every_boolean_family_that_holds_is_declared() {
    for operation in CONNECTIVES {
        let declared = TABLE
            .iter()
            .filter(|declared| {
                declared.carrier == Carrier::Boolean && declared.operation == operation
            })
            .flat_map(|declared| declared.families.iter().copied().flat_map(sides))
            .collect::<Vec<_>>();
        for candidate in boolean_candidates() {
            let wanted = holds(candidate, operation) && !given(candidate, operation);
            assert_eq!(
                declared.contains(&candidate),
                wanted,
                "{candidate:?} of {operation:?} at `Bool`: holds {}, given {}",
                holds(candidate, operation),
                given(candidate, operation)
            );
        }
    }
}
