//! Law families: what a kind of operation satisfies, stated once over variables and the constants a carrier supplies, and the table of which operation at which carrier implements which family.
//!
//! A [`Family`] states its laws over abstract operands, as [`Law`]s between [`Expr`]s. [`TABLE`] names, for each operation at each carrier, exactly the families conversion decides for it — `And` at `Bool` has the unit `true` and the absorber `false`, `And` at ℕ the absorber `0` and no unit — so a family is stated wherever it holds and nowhere it does not. A caller instantiates the laws, spells them at its own syntax, and holds each against the procedure that decides it and against the carrier's semantics at closed values.
//!
//! **The table is a claim about the implementation, not about mathematics.** A law true of a carrier and not decided — the associativity of `and` at ℕ, a floating-point sum's commutativity — has no row here: declaring it would state a law conversion refuses. The families a checker decides by rules reading `Intrinsic::algebra`'s declarations and those it decides by rules of its own sit in one table, since what the table records is what holds, whichever rule makes it hold.

use {
    crate::{Carrier, Operation},
    curios_num::Grain,
};

/// A constant a family is stated with, read at the carrier its operand has.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Constant {
    Zero,
    One,
    True,
    False,
    /// The empty word.
    Empty,
}

/// The operand position a one-sided law is stated at.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Position {
    Left,
    Right,
    Both,
}

/// One kind of law, with the constants and companion operations its instances need.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Family {
    /// `x ⋆ u = x` at the right, `u ⋆ x = x` at the left.
    Unit(Position, Constant),
    /// `x ⋆ a = a` at the right, `a ⋆ x = a` at the left.
    Absorber(Position, Constant),
    /// `x ⋆ x = x`.
    Idempotence,
    /// `x ⋆ x = c`.
    SelfCancellation(Constant),
    /// `x ⋆ y = y ⋆ x`.
    Commutativity,
    /// `(x ⋆ y) ⋆ z = x ⋆ (y ⋆ z)`.
    Associativity,
    /// `x ⋆ ¬x = c` and `¬x ⋆ x = c`, over the Boolean negation.
    Complement(Constant),
    /// `(x ⋆ y) ⋆ y = x`.
    NestedCancellation,
    /// `x ⋆ (y ⊕ z) = (x ⋆ y) ⊕ (x ⋆ z)` and its mirror, over the operation named.
    Distribution(Operation),
    /// `¬(x ⋈ y)` is the comparison named, over the same operands or swapped: `¬(x < y) = y <= x`.
    Dual(Operation, bool),
    /// `x + 1 ⋈ y` is the comparison named: `x + 1 <= y` is `x < y`.
    SuccessorSeam(Operation),
    /// `(x ⋆ y) ⊖ y = x` and `(x ⋆ y) ⊖ x = y`, the difference named taking off what the operation added.
    Cancellation(Operation),
    /// A word's length onto `(ℕ, +, 0)`: the length of a concatenation is the sum of the lengths, the empty word's is zero, and an appended element adds one.
    Homomorphism,
    /// A conversion undoes the conversion back: `this(back(x)) = x`.
    InversePair,
}

/// A term a law states: a variable of one carrier, a constant, or an operation at a carrier applied to operands.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    Var {
        index: usize,
        carrier: Carrier,
    },
    Constant {
        value: Constant,
        carrier: Carrier,
    },
    Apply {
        carrier: Carrier,
        operation: Operation,
        operands: Vec<Expr>,
    },
}

/// One equation a family states, from the family it instantiates.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Law {
    pub family: Family,
    pub left: Expr,
    pub right: Expr,
}

/// The families one operation at one carrier implements.
#[derive(Clone, Copy, Debug)]
pub struct Declared {
    pub carrier: Carrier,
    pub operation: Operation,
    pub families: &'static [Family],
}

/// Which families each operation at each carrier implements — the whole of what the law grid states, and nothing it does not.
pub const TABLE: &[Declared] = &[
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Sum,
        families: &[
            Family::Unit(Position::Both, Constant::Zero),
            Family::Commutativity,
            Family::Associativity,
            Family::Cancellation(Operation::Difference),
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Product,
        families: &[
            Family::Unit(Position::Both, Constant::One),
            Family::Absorber(Position::Both, Constant::Zero),
            Family::Commutativity,
            Family::Associativity,
            Family::Distribution(Operation::Sum),
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Difference,
        families: &[
            Family::Unit(Position::Right, Constant::Zero),
            Family::Absorber(Position::Left, Constant::Zero),
            Family::SelfCancellation(Constant::Zero),
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Quotient,
        families: &[Family::Unit(Position::Right, Constant::One)],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::And,
        families: &[
            Family::Absorber(Position::Both, Constant::Zero),
            Family::Idempotence,
            Family::Commutativity,
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Or,
        families: &[
            Family::Unit(Position::Both, Constant::Zero),
            Family::Idempotence,
            Family::Commutativity,
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Xor,
        families: &[
            Family::Unit(Position::Both, Constant::Zero),
            Family::SelfCancellation(Constant::Zero),
            Family::Commutativity,
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::ShiftLeft,
        families: &[
            Family::Unit(Position::Right, Constant::Zero),
            Family::Absorber(Position::Left, Constant::Zero),
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::ShiftRight,
        families: &[
            Family::Unit(Position::Right, Constant::Zero),
            Family::Absorber(Position::Left, Constant::Zero),
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Equal,
        families: &[
            Family::SelfCancellation(Constant::True),
            Family::Commutativity,
            Family::Dual(Operation::Unequal, false),
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Unequal,
        families: &[
            Family::SelfCancellation(Constant::False),
            Family::Commutativity,
            Family::Dual(Operation::Equal, false),
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Less,
        families: &[
            Family::SelfCancellation(Constant::False),
            Family::Dual(Operation::AtMost, true),
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::AtMost,
        families: &[
            Family::SelfCancellation(Constant::True),
            Family::Dual(Operation::Less, true),
            Family::SuccessorSeam(Operation::Less),
        ],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Conversion {
            from: Carrier::Integer,
        },
        families: &[Family::InversePair],
    },
    Declared {
        carrier: Carrier::Integer,
        operation: Operation::Sum,
        families: &[
            Family::Unit(Position::Both, Constant::Zero),
            Family::Commutativity,
            Family::Associativity,
            Family::Cancellation(Operation::Difference),
        ],
    },
    Declared {
        carrier: Carrier::Integer,
        operation: Operation::Product,
        families: &[
            Family::Unit(Position::Both, Constant::One),
            Family::Absorber(Position::Both, Constant::Zero),
            Family::Commutativity,
            Family::Distribution(Operation::Sum),
        ],
    },
    Declared {
        carrier: Carrier::Integer,
        operation: Operation::Difference,
        families: &[
            Family::Unit(Position::Right, Constant::Zero),
            Family::SelfCancellation(Constant::Zero),
        ],
    },
    Declared {
        carrier: Carrier::Integer,
        operation: Operation::Equal,
        families: &[
            Family::SelfCancellation(Constant::True),
            Family::Commutativity,
            Family::Dual(Operation::Unequal, false),
        ],
    },
    Declared {
        carrier: Carrier::Integer,
        operation: Operation::Unequal,
        families: &[
            Family::SelfCancellation(Constant::False),
            Family::Commutativity,
            Family::Dual(Operation::Equal, false),
        ],
    },
    Declared {
        carrier: Carrier::Integer,
        operation: Operation::Less,
        families: &[
            Family::SelfCancellation(Constant::False),
            Family::Dual(Operation::AtMost, true),
        ],
    },
    Declared {
        carrier: Carrier::Integer,
        operation: Operation::AtMost,
        families: &[
            Family::SelfCancellation(Constant::True),
            Family::Dual(Operation::Less, true),
            Family::SuccessorSeam(Operation::Less),
        ],
    },
    Declared {
        carrier: Carrier::Integer,
        operation: Operation::Conversion {
            from: Carrier::Natural,
        },
        families: &[Family::InversePair],
    },
    Declared {
        carrier: Carrier::Boolean,
        operation: Operation::And,
        families: &[
            Family::Unit(Position::Both, Constant::True),
            Family::Absorber(Position::Both, Constant::False),
            Family::Idempotence,
            Family::Commutativity,
            Family::Associativity,
            Family::Complement(Constant::False),
            Family::Distribution(Operation::Or),
        ],
    },
    Declared {
        carrier: Carrier::Boolean,
        operation: Operation::Or,
        families: &[
            Family::Unit(Position::Both, Constant::False),
            Family::Absorber(Position::Both, Constant::True),
            Family::Idempotence,
            Family::Commutativity,
            Family::Associativity,
            Family::Complement(Constant::True),
            Family::Distribution(Operation::And),
        ],
    },
    Declared {
        carrier: Carrier::Boolean,
        operation: Operation::Xor,
        families: &[
            Family::Unit(Position::Both, Constant::False),
            Family::SelfCancellation(Constant::False),
            Family::Commutativity,
            Family::NestedCancellation,
        ],
    },
    Declared {
        carrier: Carrier::Boolean,
        operation: Operation::Equal,
        families: &[
            Family::Unit(Position::Both, Constant::True),
            Family::SelfCancellation(Constant::True),
            Family::Commutativity,
            Family::Complement(Constant::False),
            Family::Dual(Operation::Unequal, false),
        ],
    },
    Declared {
        carrier: Carrier::Boolean,
        operation: Operation::Unequal,
        families: &[
            Family::Unit(Position::Right, Constant::False),
            Family::SelfCancellation(Constant::False),
            Family::Commutativity,
            Family::Complement(Constant::True),
            Family::Dual(Operation::Equal, false),
        ],
    },
    Declared {
        carrier: Carrier::Byte,
        operation: Operation::Conversion {
            from: Carrier::Natural,
        },
        families: &[Family::InversePair],
    },
    Declared {
        carrier: Carrier::Natural,
        operation: Operation::Conversion {
            from: Carrier::Byte,
        },
        families: &[Family::InversePair],
    },
    Declared {
        carrier: Carrier::Float,
        operation: Operation::Equal,
        families: &[Family::Commutativity],
    },
    Declared {
        carrier: Carrier::Float,
        operation: Operation::Unequal,
        families: &[Family::Commutativity],
    },
    Declared {
        carrier: Carrier::Float,
        operation: Operation::Conversion {
            from: Carrier::Packed(Grain::X),
        },
        families: &[Family::InversePair],
    },
    Declared {
        carrier: Carrier::Packed(Grain::X),
        operation: Operation::Concat,
        families: &[
            Family::Unit(Position::Both, Constant::Empty),
            Family::Associativity,
        ],
    },
    Declared {
        carrier: Carrier::Packed(Grain::X),
        operation: Operation::Length,
        families: &[Family::Homomorphism],
    },
    Declared {
        carrier: Carrier::Packed(Grain::B),
        operation: Operation::Concat,
        families: &[
            Family::Unit(Position::Both, Constant::Empty),
            Family::Associativity,
        ],
    },
    Declared {
        carrier: Carrier::Packed(Grain::B),
        operation: Operation::Length,
        families: &[Family::Homomorphism],
    },
    Declared {
        carrier: Carrier::List,
        operation: Operation::Concat,
        families: &[
            Family::Unit(Position::Both, Constant::Empty),
            Family::Associativity,
        ],
    },
    Declared {
        carrier: Carrier::List,
        operation: Operation::Length,
        families: &[Family::Homomorphism],
    },
    Declared {
        carrier: Carrier::Packed(Grain::X),
        operation: Operation::Equal,
        families: &[
            Family::SelfCancellation(Constant::True),
            Family::Commutativity,
        ],
    },
    Declared {
        carrier: Carrier::Packed(Grain::B),
        operation: Operation::Equal,
        families: &[
            Family::SelfCancellation(Constant::True),
            Family::Commutativity,
        ],
    },
    Declared {
        carrier: Carrier::Packed(Grain::X),
        operation: Operation::Conversion {
            from: Carrier::Packed(Grain::B),
        },
        families: &[Family::InversePair],
    },
    Declared {
        carrier: Carrier::Packed(Grain::B),
        operation: Operation::Conversion {
            from: Carrier::Packed(Grain::X),
        },
        families: &[Family::InversePair],
    },
];

impl Family {
    /// The laws this family states of `operation` at `carrier`, each over variables numbered from zero.
    pub fn laws(self, carrier: Carrier, operation: Operation) -> Vec<Law> {
        let operand = operand_carrier(carrier, operation);
        let result = result_carrier(carrier, operation);
        let var = |index| Expr::Var {
            index,
            carrier: operand,
        };
        let constant = |value, carrier| Expr::Constant { value, carrier };
        let apply = |operation, operands| Expr::Apply {
            carrier,
            operation,
            operands,
        };
        let law = |left, right| Law {
            family: self,
            left,
            right,
        };
        let (x, y, z) = (var(0), var(1), var(2));

        match self {
            Family::Unit(side, unit) => sided(side, |at_right| {
                let unit = constant(unit, operand);
                let operands = match at_right {
                    true => vec![x.clone(), unit],
                    false => vec![unit, x.clone()],
                };
                law(apply(operation, operands), x.clone())
            }),
            Family::Absorber(side, absorber) => sided(side, |at_right| {
                let operands = match at_right {
                    true => vec![x.clone(), constant(absorber, operand)],
                    false => vec![constant(absorber, operand), x.clone()],
                };
                law(apply(operation, operands), constant(absorber, result))
            }),
            Family::Idempotence => vec![law(apply(operation, vec![x.clone(), x.clone()]), x)],
            Family::SelfCancellation(value) => vec![law(
                apply(operation, vec![x.clone(), x]),
                constant(value, result),
            )],
            Family::Commutativity => vec![law(
                apply(operation, vec![x.clone(), y.clone()]),
                apply(operation, vec![y, x]),
            )],
            Family::Associativity => vec![law(
                apply(
                    operation,
                    vec![apply(operation, vec![x.clone(), y.clone()]), z.clone()],
                ),
                apply(operation, vec![x, apply(operation, vec![y, z])]),
            )],
            Family::Complement(value) => vec![
                law(
                    apply(operation, vec![x.clone(), negated(x.clone())]),
                    constant(value, result),
                ),
                law(
                    apply(operation, vec![negated(x.clone()), x]),
                    constant(value, result),
                ),
            ],
            Family::NestedCancellation => vec![law(
                apply(
                    operation,
                    vec![apply(operation, vec![x.clone(), y.clone()]), y],
                ),
                x,
            )],
            Family::Distribution(over) => vec![
                law(
                    apply(
                        operation,
                        vec![x.clone(), apply(over, vec![y.clone(), z.clone()])],
                    ),
                    apply(
                        over,
                        vec![
                            apply(operation, vec![x.clone(), y.clone()]),
                            apply(operation, vec![x.clone(), z.clone()]),
                        ],
                    ),
                ),
                law(
                    apply(
                        operation,
                        vec![apply(over, vec![y.clone(), z.clone()]), x.clone()],
                    ),
                    apply(
                        over,
                        vec![
                            apply(operation, vec![y, x.clone()]),
                            apply(operation, vec![z, x]),
                        ],
                    ),
                ),
            ],
            Family::Dual(dual, swapped) => {
                let operands = match swapped {
                    true => vec![y.clone(), x.clone()],
                    false => vec![x.clone(), y.clone()],
                };
                vec![law(
                    negated(apply(operation, vec![x, y])),
                    apply(dual, operands),
                )]
            }
            Family::SuccessorSeam(strict) => {
                let successor = Expr::Apply {
                    carrier: operand,
                    operation: Operation::Sum,
                    operands: vec![x.clone(), constant(Constant::One, operand)],
                };
                vec![law(
                    apply(operation, vec![successor, y.clone()]),
                    apply(strict, vec![x, y]),
                )]
            }
            Family::Cancellation(difference) => {
                let combined = apply(operation, vec![x.clone(), y.clone()]);
                vec![
                    law(
                        apply(difference, vec![combined.clone(), y.clone()]),
                        x.clone(),
                    ),
                    law(apply(difference, vec![combined, x]), y),
                ]
            }
            Family::Homomorphism => {
                let element = element_carrier(carrier)
                    .expect("a length homomorphism is declared at a word carrier");
                let length = |word| Expr::Apply {
                    carrier,
                    operation: Operation::Length,
                    operands: vec![word],
                };
                let sum = |left, right| Expr::Apply {
                    carrier: Carrier::Natural,
                    operation: Operation::Sum,
                    operands: vec![left, right],
                };
                let word = |operation, operands| Expr::Apply {
                    carrier,
                    operation,
                    operands,
                };
                let element = Expr::Var {
                    index: 3,
                    carrier: element,
                };
                vec![
                    law(
                        length(word(Operation::Concat, vec![x.clone(), y.clone()])),
                        sum(length(x.clone()), length(y)),
                    ),
                    law(
                        length(constant(Constant::Empty, carrier)),
                        constant(Constant::Zero, Carrier::Natural),
                    ),
                    law(
                        length(word(Operation::Append, vec![x.clone(), element])),
                        sum(length(x), constant(Constant::One, Carrier::Natural)),
                    ),
                ]
            }
            Family::InversePair => {
                let x = Expr::Var { index: 0, carrier };
                let back = Expr::Apply {
                    carrier: operand,
                    operation: Operation::Conversion { from: carrier },
                    operands: vec![x.clone()],
                };
                vec![law(apply(operation, vec![back]), x)]
            }
        }
    }
}

/// Whether [`TABLE`] declares `family` of `operation` at `carrier`.
pub fn declares(carrier: Carrier, operation: Operation, family: Family) -> bool {
    TABLE.iter().any(|declared| {
        declared.carrier == carrier
            && declared.operation == operation
            && declared.families.contains(&family)
    })
}

/// Every law [`TABLE`] states, in its order.
pub fn laws() -> Vec<(Declared, Law)> {
    TABLE
        .iter()
        .flat_map(|declared| {
            declared.families.iter().flat_map(move |family| {
                family
                    .laws(declared.carrier, declared.operation)
                    .into_iter()
                    .map(move |law| (*declared, law))
            })
        })
        .collect()
}

/// The carrier an operation's operands are of: a conversion's source, and otherwise the carrier it is declared at.
fn operand_carrier(carrier: Carrier, operation: Operation) -> Carrier {
    match operation {
        Operation::Conversion { from } => from,
        _ => carrier,
    }
}

/// The carrier an operation's result is of: a comparison's is the Boolean algebra and a length's ℕ, and every other operation's is the carrier it is declared at.
fn result_carrier(carrier: Carrier, operation: Operation) -> Carrier {
    match operation {
        Operation::Equal | Operation::Unequal | Operation::Less | Operation::AtMost => {
            Carrier::Boolean
        }
        Operation::Length => Carrier::Natural,
        _ => carrier,
    }
}

/// The carrier a word's elements are of, and `None` for a carrier that is no word.
fn element_carrier(carrier: Carrier) -> Option<Carrier> {
    match carrier {
        Carrier::Packed(Grain::B) => Some(Carrier::Boolean),
        Carrier::Packed(Grain::X) => Some(Carrier::Byte),
        Carrier::List => Some(Carrier::Natural),
        _ => None,
    }
}

/// The Boolean negation, `x xor true`.
fn negated(operand: Expr) -> Expr {
    Expr::Apply {
        carrier: Carrier::Boolean,
        operation: Operation::Xor,
        operands: vec![
            operand,
            Expr::Constant {
                value: Constant::True,
                carrier: Carrier::Boolean,
            },
        ],
    }
}

/// The instances of a one-sided law at the sides named, the right first.
fn sided(side: Position, instance: impl Fn(bool) -> Law) -> Vec<Law> {
    match side {
        Position::Right => vec![instance(true)],
        Position::Left => vec![instance(false)],
        Position::Both => vec![instance(true), instance(false)],
    }
}

#[cfg(test)]
mod tests;
