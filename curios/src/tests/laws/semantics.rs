//! What a generated law means at closed values: each operation the law table names, computed through `curios-num` apart from the compiler, and each law held at every assignment of its variables over a small grid of values per carrier — at `Flt`, the binary64 pattern grid.

use {
    curios_algebra::{Carrier, Constant, Expr, Law, Operation},
    curios_num::{Binary, Floating, Grain, Integer, Natural},
};

/// A closed value of one carrier.
#[derive(Clone, Debug, PartialEq)]
pub(super) enum Value {
    Natural(Natural),
    Integer(Integer),
    Boolean(bool),
    Byte(u8),
    /// Compared by bit pattern, so a law about a NaN states the NaN it means.
    Float(Floating),
    Packed(Grain, Binary),
    List(Vec<Natural>),
}

/// The binary64 patterns every `Flt` law is held at: both zeros, both infinities, the least and the greatest subnormal, the least normal, two ordinary values, the greatest finite value, and a quiet and a signaling NaN with a payload of each sign.
const FLOAT_PATTERNS: &[u64] = &[
    0x0000_0000_0000_0000,
    0x8000_0000_0000_0000,
    0x7FF0_0000_0000_0000,
    0xFFF0_0000_0000_0000,
    0x0000_0000_0000_0001,
    0x800F_FFFF_FFFF_FFFF,
    0x0010_0000_0000_0000,
    0x3FF0_0000_0000_0000,
    0xBFF8_0000_0000_0000,
    0x7FEF_FFFF_FFFF_FFFF,
    0x7FF8_0000_0000_0001,
    0xFFF8_0000_0000_0123,
    0x7FF0_0000_0000_0001,
    0xFFF4_0000_0000_0000,
];

/// The values a variable of `carrier` ranges over.
fn samples(carrier: Carrier) -> Vec<Value> {
    match carrier {
        Carrier::Natural => [0u32, 1, 2, 3, 7, 8, 255, 256, 1000]
            .into_iter()
            .map(|value| Value::Natural(Natural::from(value)))
            .collect(),
        Carrier::Integer => [-7i32, -1, 0, 1, 2, 9]
            .into_iter()
            .map(|value| Value::Integer(Integer::from(value)))
            .collect(),
        Carrier::Boolean => vec![Value::Boolean(false), Value::Boolean(true)],
        Carrier::Byte => [0u8, 1, 127, 255].into_iter().map(Value::Byte).collect(),
        Carrier::Float => FLOAT_PATTERNS
            .iter()
            .map(|bits| Value::Float(Floating::from_bits(*bits)))
            .collect(),
        Carrier::Packed(Grain::X) => [&[][..], &[0], &[1, 2], &[255, 0, 7]]
            .into_iter()
            .map(|bytes| Value::Packed(Grain::X, Binary::from_bytes(bytes.to_vec())))
            .collect(),
        Carrier::Packed(Grain::B) => [&[][..], &[true], &[true, false, true], &[false; 8]]
            .into_iter()
            .map(|bits| Value::Packed(Grain::B, Binary::from_bits(bits.iter().copied())))
            .collect(),
        Carrier::List => [&[][..], &[0u32], &[1, 2, 3]]
            .into_iter()
            .map(|items| Value::List(items.iter().map(|item| Natural::from(*item)).collect()))
            .collect(),
    }
}

/// Whether `law` holds at every assignment of its variables over their samples; an assignment where either side is undefined — a narrowing outside its domain — is outside the law and skipped. `Err` names the first assignment where the two sides differ.
pub(super) fn holds(law: &Law) -> Result<(), String> {
    let mut variables = Vec::new();
    collect(&law.left, &mut variables);
    collect(&law.right, &mut variables);

    let grids = variables
        .iter()
        .map(|(_, carrier)| samples(*carrier))
        .collect::<Vec<_>>();
    let mut at = vec![0usize; variables.len()];
    loop {
        let value = |index: usize, carrier: Carrier| {
            let position = variables
                .iter()
                .position(|variable| *variable == (index, carrier))
                .expect("every variable was collected");
            grids[position][at[position]].clone()
        };
        if let (Some(left), Some(right)) =
            (evaluate(&law.left, &value), evaluate(&law.right, &value))
            && left != right
        {
            let assignment = variables
                .iter()
                .zip(&at)
                .zip(&grids)
                .map(|(((index, carrier), at), grid)| {
                    format!("{carrier:?}#{index} = {:?}", grid[*at])
                })
                .collect::<Vec<_>>()
                .join(", ");
            return Err(format!("{left:?} against {right:?} at {assignment}"));
        }

        // The next assignment, odometer-wise; done once every position has wrapped.
        let mut position = 0;
        loop {
            if position == at.len() {
                return Ok(());
            }
            at[position] += 1;
            if at[position] < grids[position].len() {
                break;
            }
            at[position] = 0;
            position += 1;
        }
    }
}

fn collect(expr: &Expr, into: &mut Vec<(usize, Carrier)>) {
    match expr {
        Expr::Var { index, carrier } => {
            if !into.contains(&(*index, *carrier)) {
                into.push((*index, *carrier));
            }
        }
        Expr::Constant { .. } => {}
        Expr::Apply { operands, .. } => {
            for operand in operands {
                collect(operand, into);
            }
        }
    }
}

fn evaluate(expr: &Expr, value: &impl Fn(usize, Carrier) -> Value) -> Option<Value> {
    match expr {
        Expr::Var { index, carrier } => Some(value(*index, *carrier)),
        Expr::Constant { value, carrier } => Some(constant(*value, *carrier)),
        Expr::Apply {
            carrier,
            operation,
            operands,
        } => {
            let operands = operands
                .iter()
                .map(|operand| evaluate(operand, value))
                .collect::<Option<Vec<_>>>()?;
            apply(*carrier, *operation, &operands)
        }
    }
}

fn constant(value: Constant, carrier: Carrier) -> Value {
    match (value, carrier) {
        (Constant::Zero, Carrier::Natural) => Value::Natural(Natural::zero()),
        (Constant::One, Carrier::Natural) => Value::Natural(Natural::one()),
        (Constant::Zero, Carrier::Integer) => Value::Integer(Integer::from(0)),
        (Constant::One, Carrier::Integer) => Value::Integer(Integer::from(1)),
        (Constant::True, Carrier::Boolean) => Value::Boolean(true),
        (Constant::False, Carrier::Boolean) => Value::Boolean(false),
        (Constant::Empty, Carrier::Packed(grain)) => Value::Packed(grain, Binary::empty()),
        (Constant::Empty, Carrier::List) => Value::List(Vec::new()),
        _ => panic!("no {value:?} at {carrier:?}"),
    }
}

/// `operation` at `carrier`, applied to `operands`; `None` outside the operation's domain.
fn apply(carrier: Carrier, operation: Operation, operands: &[Value]) -> Option<Value> {
    use Value::{Boolean, Byte, Float, List, Packed};

    Some(match (carrier, operation, operands) {
        (Carrier::Natural, _, [Value::Natural(a), Value::Natural(b)]) => match operation {
            Operation::Sum => Value::Natural(a.clone() + b),
            Operation::Difference => Value::Natural(a.monus(b)),
            Operation::Product => Value::Natural(a.clone() * b),
            Operation::Quotient => Value::Natural(a.div(b).ok()?),
            Operation::Remainder => Value::Natural(a.rem(b).ok()?),
            Operation::And => Value::Natural(a.clone() & b),
            Operation::Or => Value::Natural(a.clone() | b),
            Operation::Xor => Value::Natural(a.clone() ^ b),
            Operation::ShiftLeft => Value::Natural(a.shl_within(b, 1 << 20)?),
            Operation::ShiftRight => Value::Natural(a >> b),
            Operation::Equal => Boolean(a == b),
            Operation::Unequal => Boolean(a != b),
            Operation::Less => Boolean(a < b),
            Operation::AtMost => Boolean(a <= b),
            _ => return None,
        },
        (Carrier::Integer, _, [Value::Integer(a), Value::Integer(b)]) => match operation {
            Operation::Sum => Value::Integer(a.clone() + b.clone()),
            Operation::Difference => Value::Integer(a.clone() - b.clone()),
            Operation::Product => Value::Integer(a.clone() * b.clone()),
            Operation::Equal => Boolean(a == b),
            Operation::Unequal => Boolean(a != b),
            Operation::Less => Boolean(a < b),
            Operation::AtMost => Boolean(a <= b),
            _ => return None,
        },
        (Carrier::Boolean, _, [Boolean(a), Boolean(b)]) => Boolean(match operation {
            Operation::And => *a && *b,
            Operation::Or => *a || *b,
            Operation::Xor | Operation::Unequal => a != b,
            Operation::Equal => a == b,
            _ => return None,
        }),
        (Carrier::Float, Operation::Equal, [Float(a), Float(b)]) => Boolean(a.eql(*b)),
        (Carrier::Float, Operation::Unequal, [Float(a), Float(b)]) => Boolean(a.neq(*b)),
        (Carrier::Packed(_), Operation::Equal, [Packed(_, a), Packed(_, b)]) => Boolean(a == b),
        (
            Carrier::Natural,
            Operation::Conversion {
                from: Carrier::Integer,
            },
            [Value::Integer(value)],
        ) => Value::Natural(Natural::try_from(value).ok()?),
        (
            Carrier::Natural,
            Operation::Conversion {
                from: Carrier::Byte,
            },
            [Byte(value)],
        ) => Value::Natural(Natural::from(*value)),
        (
            Carrier::Integer,
            Operation::Conversion {
                from: Carrier::Natural,
            },
            [Value::Natural(value)],
        ) => Value::Integer(Integer::from(value.clone())),
        (
            Carrier::Byte,
            Operation::Conversion {
                from: Carrier::Natural,
            },
            [Value::Natural(value)],
        ) => Byte(u8::try_from(value).ok()?),
        (
            Carrier::Float,
            Operation::Conversion {
                from: Carrier::Packed(Grain::X),
            },
            [Packed(Grain::X, bytes)],
        ) => Float(Floating::of_le_bytes(bytes).ok()?),
        (
            Carrier::Packed(Grain::X),
            Operation::Conversion {
                from: Carrier::Float,
            },
            [Float(value)],
        ) => Packed(Grain::X, value.to_le_bytes()),
        (
            Carrier::Packed(Grain::X),
            Operation::Conversion {
                from: Carrier::Packed(Grain::B),
            },
            [Packed(Grain::B, bits)],
        ) => {
            if !bits.bit_length().is_multiple_of(8) {
                return None;
            }
            Packed(Grain::X, Binary::from_bytes(bits.to_packed_bytes()))
        }
        (
            Carrier::Packed(Grain::B),
            Operation::Conversion {
                from: Carrier::Packed(Grain::X),
            },
            [Packed(Grain::X, bytes)],
        ) => Packed(Grain::B, bytes.clone()),
        (Carrier::Packed(grain), Operation::Concat, [Packed(_, a), Packed(_, b)]) => {
            Packed(grain, Binary::concat([a, b]))
        }
        (Carrier::Packed(grain), Operation::Length, [Packed(_, word)]) => {
            Value::Natural(Natural::from(word.len(grain)))
        }
        (Carrier::Packed(Grain::B), Operation::Append, [Packed(_, word), Boolean(bit)]) => {
            Packed(Grain::B, word.append_bit(*bit))
        }
        (Carrier::Packed(Grain::X), Operation::Append, [Packed(_, word), Byte(byte)]) => {
            Packed(Grain::X, word.append_byte(*byte)?)
        }
        (Carrier::List, Operation::Concat, [List(a), List(b)]) => {
            List(a.iter().chain(b).cloned().collect())
        }
        (Carrier::List, Operation::Length, [List(items)]) => {
            Value::Natural(Natural::from(items.len()))
        }
        (Carrier::List, Operation::Append, [List(items), Value::Natural(item)]) => {
            List(items.iter().chain([item]).cloned().collect())
        }
        _ => panic!("no semantics for {operation:?} at {carrier:?} over {operands:?}"),
    })
}
