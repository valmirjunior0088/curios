//! What each intrinsic is algebraically: the operation `curios-algebra` gives it a meaning as, over the operands that meaning reads — or opaque.
//!
//! **The match is exhaustive and states opacity outright**, as [`Intrinsic::signature`]'s does: a new intrinsic has to say what it is to the algebra, possibly nothing, before it compiles, so an operation can neither gain a law by being forgotten nor lose one by being added. What each [`Operation`] means — which operands bound it, which it never exceeds, which conversion undoes which — is `curios-algebra`'s; this is only which intrinsic is which, and where its operands are. Only the implemented reasoning is declared: an intrinsic is declared where some rule reads it at its carrier, and declaring one never enables an identity nothing implements — which is why a float's quotient, its remainder and its square root are opaque, no identity of theirs being implemented. An operation a rule reads through is declared though no law is its own — a `Nat`'s successor floor, `Int/abs`, `Int/sign`, a float's negation and `copysign` — since the layers of a term the classification walks are the operations this table declares, and a second list of what is read through would be a second statement of it.

use {
    super::Intrinsic,
    crate::{Nat, Subterm, Term},
    curios_algebra::{Carrier, Family, Operation, declares},
    curios_num::{Grain, Rounding},
};

/// An intrinsic as the algebra reads it.
pub enum Declaration<'a> {
    /// Nothing the algebra reasons about.
    Opaque,
    /// An operation over a carrier, with the operands its meaning reads, in the operation's order.
    Operation {
        carrier: Carrier,
        operation: Operation,
        operands: Operands<'a>,
    },
}

/// The operands a declaration's meaning reads, in its operation's order: a quotient's and a remainder's are the dividend and the divisor, their proof being no operand the algebra reads.
pub enum Operands<'a> {
    One([&'a Term; 1]),
    Two([&'a Term; 2]),
    Three([&'a Term; 3]),
}

impl<'a> Operands<'a> {
    /// The operands, in order.
    pub fn as_slice(&self) -> &[&'a Term] {
        match self {
            Operands::One(operands) => operands,
            Operands::Two(operands) => operands,
            Operands::Three(operands) => operands,
        }
    }
}

impl Intrinsic {
    /// What this intrinsic is algebraically.
    pub fn algebra(&self) -> Declaration<'_> {
        let declared = |carrier, operation, operands| Declaration::Operation {
            carrier,
            operation,
            operands,
        };
        match self {
            Intrinsic::BoolAnd(left, right) => declared(
                Carrier::Boolean,
                Operation::And,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolOr(left, right) => declared(
                Carrier::Boolean,
                Operation::Or,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolXor(left, right) => declared(
                Carrier::Boolean,
                Operation::Xor,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolEql(left, right) => declared(
                Carrier::Boolean,
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::BoolNeq(left, right) => declared(
                Carrier::Boolean,
                Operation::Unequal,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatAdd(left, right) => declared(
                Carrier::Natural,
                Operation::Sum,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatSub(left, right) => declared(
                Carrier::Natural,
                Operation::Difference,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatMul(left, right) => declared(
                Carrier::Natural,
                Operation::Product,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatDiv {
                dividend, divisor, ..
            } => declared(
                Carrier::Natural,
                Operation::Quotient,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::NatRem {
                dividend, divisor, ..
            } => declared(
                Carrier::Natural,
                Operation::Remainder,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::NatAnd(left, right) => declared(
                Carrier::Natural,
                Operation::And,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatOr(left, right) => declared(
                Carrier::Natural,
                Operation::Or,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatXor(left, right) => declared(
                Carrier::Natural,
                Operation::Xor,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatMin(left, right) => declared(
                Carrier::Natural,
                Operation::Min,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatMax(left, right) => declared(
                Carrier::Natural,
                Operation::Max,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntMin(left, right) => declared(
                Carrier::Integer,
                Operation::Min,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntMax(left, right) => declared(
                Carrier::Integer,
                Operation::Max,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatPow(left, right) => declared(
                Carrier::Natural,
                Operation::Power,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatShl(left, right) => declared(
                Carrier::Natural,
                Operation::ShiftLeft,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatShr(left, right) => declared(
                Carrier::Natural,
                Operation::ShiftRight,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatEql(left, right) => declared(
                Carrier::Natural,
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatNeq(left, right) => declared(
                Carrier::Natural,
                Operation::Unequal,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatLt(left, right) => declared(
                Carrier::Natural,
                Operation::Less,
                Operands::Two([left, right]),
            ),
            Intrinsic::NatLe(left, right) => declared(
                Carrier::Natural,
                Operation::AtMost,
                Operands::Two([left, right]),
            ),
            Intrinsic::ByteToNat(operand) => declared(
                Carrier::Natural,
                Operation::Conversion {
                    from: Carrier::Byte,
                },
                Operands::One([operand]),
            ),
            Intrinsic::NatToByte { nat, below: _ } => declared(
                Carrier::Byte,
                Operation::Conversion {
                    from: Carrier::Natural,
                },
                Operands::One([nat]),
            ),
            Intrinsic::NatToInt(operand) => declared(
                Carrier::Integer,
                Operation::Conversion {
                    from: Carrier::Natural,
                },
                Operands::One([operand]),
            ),
            Intrinsic::IntToNat { int, non_neg: _ } => declared(
                Carrier::Natural,
                Operation::Conversion {
                    from: Carrier::Integer,
                },
                Operands::One([int]),
            ),
            Intrinsic::FltToLeBytes(operand) => declared(
                Carrier::Packed(Grain::X),
                Operation::Conversion {
                    from: Carrier::Float,
                },
                Operands::One([operand]),
            ),
            Intrinsic::FltOfLeBytes {
                bin,
                eight_bytes: _,
            } => declared(
                Carrier::Float,
                Operation::Conversion {
                    from: Carrier::Packed(Grain::X),
                },
                Operands::One([bin]),
            ),
            Intrinsic::BinReinterp {
                grain,
                bin,
                aligned: _,
            } => declared(
                Carrier::Packed(grain.other()),
                Operation::Conversion {
                    from: Carrier::Packed(*grain),
                },
                Operands::One([bin]),
            ),
            Intrinsic::FltAdd(rounding, left, right) => declared(
                Carrier::Float,
                Operation::RoundedSum(*rounding),
                Operands::Two([left, right]),
            ),
            Intrinsic::FltMul(rounding, left, right) => declared(
                Carrier::Float,
                Operation::RoundedProduct(*rounding),
                Operands::Two([left, right]),
            ),
            Intrinsic::FltFma(rounding, left, right, addend) => declared(
                Carrier::Float,
                Operation::Fused(*rounding),
                Operands::Three([left, right, addend]),
            ),
            Intrinsic::FltMin(left, right) => {
                declared(Carrier::Float, Operation::Min, Operands::Two([left, right]))
            }
            Intrinsic::FltMax(left, right) => {
                declared(Carrier::Float, Operation::Max, Operands::Two([left, right]))
            }
            Intrinsic::FltEql(left, right) => declared(
                Carrier::Float,
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::FltNeq(left, right) => declared(
                Carrier::Float,
                Operation::Unequal,
                Operands::Two([left, right]),
            ),
            Intrinsic::BinEql(grain, left, right) => declared(
                Carrier::Packed(*grain),
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntAdd(left, right) => declared(
                Carrier::Integer,
                Operation::Sum,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntSub(left, right) => declared(
                Carrier::Integer,
                Operation::Difference,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntMul(left, right) => declared(
                Carrier::Integer,
                Operation::Product,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntDiv {
                dividend, divisor, ..
            } => declared(
                Carrier::Integer,
                Operation::Quotient,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::IntRem {
                dividend, divisor, ..
            } => declared(
                Carrier::Integer,
                Operation::Remainder,
                Operands::Two([dividend, divisor]),
            ),
            Intrinsic::IntAnd(left, right) => declared(
                Carrier::Integer,
                Operation::And,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntOr(left, right) => declared(
                Carrier::Integer,
                Operation::Or,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntXor(left, right) => declared(
                Carrier::Integer,
                Operation::Xor,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntShl(left, right) => declared(
                Carrier::Integer,
                Operation::ShiftLeft,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntShr(left, right) => declared(
                Carrier::Integer,
                Operation::ShiftRight,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntEql(left, right) => declared(
                Carrier::Integer,
                Operation::Equal,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntNeq(left, right) => declared(
                Carrier::Integer,
                Operation::Unequal,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntLt(left, right) => declared(
                Carrier::Integer,
                Operation::Less,
                Operands::Two([left, right]),
            ),
            Intrinsic::IntLe(left, right) => declared(
                Carrier::Integer,
                Operation::AtMost,
                Operands::Two([left, right]),
            ),
            Intrinsic::Nat(Nat::Succ(_, inner)) => declared(
                Carrier::Natural,
                Operation::Successor,
                Operands::One([inner]),
            ),
            Intrinsic::IntAbs(operand) => declared(
                Carrier::Natural,
                Operation::Absolute,
                Operands::One([operand]),
            ),
            Intrinsic::IntSign(operand) => declared(
                Carrier::Integer,
                Operation::Signum,
                Operands::One([operand]),
            ),
            Intrinsic::FltNeg(operand) => declared(
                Carrier::Float,
                Operation::Negation,
                Operands::One([operand]),
            ),
            Intrinsic::FltCopysign(magnitude, sign) => declared(
                Carrier::Float,
                Operation::Copysign,
                Operands::Two([magnitude, sign]),
            ),
            Intrinsic::BoolType
            | Intrinsic::Bool { .. }
            | Intrinsic::NatType
            | Intrinsic::Nat(Nat::Zero)
            | Intrinsic::ByteType
            | Intrinsic::Byte { .. }
            | Intrinsic::IntType
            | Intrinsic::Int { .. }
            | Intrinsic::FltType
            | Intrinsic::Flt { .. }
            | Intrinsic::FltSub { .. }
            | Intrinsic::FltDiv { .. }
            | Intrinsic::FltRem { .. }
            | Intrinsic::FltLt { .. }
            | Intrinsic::FltLe { .. }
            | Intrinsic::FltAbs { .. }
            | Intrinsic::FltSqrt { .. }
            | Intrinsic::FltRoundIntegral { .. }
            | Intrinsic::NatToFlt { .. }
            | Intrinsic::IntToFlt { .. }
            | Intrinsic::FltToNat { .. }
            | Intrinsic::FltToInt { .. }
            | Intrinsic::FltMantissa { .. }
            | Intrinsic::FltExponent { .. }
            | Intrinsic::BinType { .. }
            | Intrinsic::Bin { .. }
            | Intrinsic::BinLen { .. }
            | Intrinsic::BinGet { .. }
            | Intrinsic::BinSlice { .. }
            | Intrinsic::BinAppend { .. }
            | Intrinsic::BinConcat { .. }
            | Intrinsic::BinReplicate { .. }
            | Intrinsic::BinAnd { .. }
            | Intrinsic::BinOr { .. }
            | Intrinsic::BinXor { .. }
            | Intrinsic::ListType { .. }
            | Intrinsic::List { .. }
            | Intrinsic::ListLen { .. }
            | Intrinsic::ListGet { .. }
            | Intrinsic::ListSlice { .. }
            | Intrinsic::ListAppend { .. }
            | Intrinsic::ListConcat { .. }
            | Intrinsic::ListMap { .. }
            | Intrinsic::ListFold { .. }
            | Intrinsic::HandleType
            | Intrinsic::Handle { .. }
            | Intrinsic::CellType { .. }
            | Intrinsic::Cell { .. }
            | Intrinsic::CellFill { .. }
            | Intrinsic::CellPoll { .. }
            | Intrinsic::ChannelType { .. }
            | Intrinsic::Channel { .. }
            | Intrinsic::ChannelPush { .. }
            | Intrinsic::ChannelTake { .. }
            | Intrinsic::ChannelClose { .. }
            | Intrinsic::ChannelClosed { .. }
            | Intrinsic::ChannelCount { .. }
            | Intrinsic::ChannelCapacity { .. }
            | Intrinsic::IoType { .. }
            | Intrinsic::IoPure { .. }
            | Intrinsic::IoBind { .. }
            | Intrinsic::IoSuspend { .. }
            | Intrinsic::IoAdvance { .. } => Declaration::Opaque,
        }
    }

    /// Where `operand` — this conversion's operand, reduced — is the result of a conversion this one undoes, that conversion's own operand: `Nat/to_byte(Byte/to_nat(b))` is `b`, and `Flt/of_le_bytes(Flt/to_le_bytes(x))` is `x`. Which conversions undo which is `curios-algebra`'s `Operation::undoes`; this reads the two declarations. `None` for anything else.
    ///
    /// **A round trip is the inversion of a constructor, not an equation about arithmetic.** A narrowing states the bound that makes its operand a value of the narrower carrier, so the value it builds *is* that operand, and reading it back is the operand again. That is what keeps a bound established on one side of a round trip standing after it.
    pub(crate) fn undone<'a>(&self, operand: &'a Term) -> Option<&'a Term> {
        let Declaration::Operation {
            carrier, operation, ..
        } = self.algebra()
        else {
            return None;
        };
        let Subterm::Intrinsic(inner) = &**operand else {
            return None;
        };
        match inner.algebra() {
            Declaration::Operation {
                carrier: inner_carrier,
                operation: inner_operation,
                operands: Operands::One([inner_operand]),
            } if operation.undoes(carrier, inner_operation, inner_carrier) => Some(inner_operand),
            _ => None,
        }
    }

    /// The carrier, the operation and the two operands of this intrinsic, where it is an operation the law table declares commutative — the one statement of which operations denote one value with their operands in either order.
    pub fn commutative(&self) -> Option<((Carrier, Operation), &Term, &Term)> {
        match self.algebra() {
            Declaration::Operation {
                carrier,
                operation,
                operands: Operands::Two([left, right]),
            } if declares(carrier, operation, Family::Commutativity) => {
                Some(((carrier, operation), left, right))
            }
            _ => None,
        }
    }

    /// The rounding direction, the two factors and the addend of this intrinsic, where it is a fused multiply-add: the factors pair in either order, as a commutative operation's operands do, and the addend stands in place.
    pub fn fused(&self) -> Option<(Rounding, [&Term; 3])> {
        match self.algebra() {
            Declaration::Operation {
                carrier: Carrier::Float,
                operation: Operation::Fused(rounding),
                operands: Operands::Three(operands),
            } => Some((rounding, operands)),
            _ => None,
        }
    }

    /// The comparison of `carrier` that `operation` names, over `left` and `right` — the way back from a declaration to the intrinsic it declares, for the comparisons the algebra hands back by operation: `<`, `<=`, `==` and `!=` over `Nat` and `Int`, and `==`, `!=` and `xor` over `Bool`. `None` for any other pair.
    pub fn comparison(
        carrier: Carrier,
        operation: Operation,
        left: Term,
        right: Term,
    ) -> Option<Intrinsic> {
        Some(match (carrier, operation) {
            (Carrier::Natural, Operation::Less) => Intrinsic::NatLt(left, right),
            (Carrier::Natural, Operation::AtMost) => Intrinsic::NatLe(left, right),
            (Carrier::Natural, Operation::Equal) => Intrinsic::NatEql(left, right),
            (Carrier::Natural, Operation::Unequal) => Intrinsic::NatNeq(left, right),
            (Carrier::Integer, Operation::Less) => Intrinsic::IntLt(left, right),
            (Carrier::Integer, Operation::AtMost) => Intrinsic::IntLe(left, right),
            (Carrier::Integer, Operation::Equal) => Intrinsic::IntEql(left, right),
            (Carrier::Integer, Operation::Unequal) => Intrinsic::IntNeq(left, right),
            (Carrier::Boolean, Operation::Equal) => Intrinsic::BoolEql(left, right),
            (Carrier::Boolean, Operation::Unequal) => Intrinsic::BoolNeq(left, right),
            (Carrier::Boolean, Operation::Xor) => Intrinsic::BoolXor(left, right),
            _ => return None,
        })
    }
}
