//! The interpreter's runtime value domain and the reasons an evaluation bails.
//!
//! A [`Value`] is the compile-time result of running a closed computation —
//! internal to the evaluator, never stored in the module — so it carries the
//! shapes the constant alphabet deliberately omits: `Rc`-shared list,
//! product, and constructor aggregates, and a closure over a module function
//! with its resolved local captures. Reification ([`super::reify`]) turns a
//! value back into arena statements.

use {
    crate::{Constant, ConstructorId, FunctionId, ProductId, ValueId},
    curios_base::{Flt, Grain, PackedBin},
    std::{cell::RefCell, rc::Rc},
};

/// A compile-time runtime value. Aggregates are `Rc`-shared so an environment
/// clones freely; a closure names its module function and carries its
/// resolved local captures.
#[derive(Clone)]
pub(super) enum Value {
    /// The unit value — the carrier of an erased proof or type slot.
    Unit,
    Bool(bool),
    Nat(u32),
    Byte(u8),
    Int(i32),
    Flt(Flt),
    Io(u32),
    Bin(Grain, Rc<PackedBin>),
    Lst(Rc<Vec<Value>>),
    /// A product value, in the schema's field order.
    Product(ProductId, Rc<Vec<Value>>),
    /// A variant value: its constructor and payload, in payload order.
    Construct(ConstructorId, Rc<Vec<Value>>),
    /// A closure over a module function with its resolved local captures. The
    /// environment is a `RefCell` so a local recursive group can bind its
    /// members first and backpatch each to see its siblings.
    Closure(Rc<Closure>),
}

pub(super) struct Closure {
    pub(super) function: FunctionId,
    pub(super) env: RefCell<Vec<(ValueId, Value)>>,
}

impl Value {
    /// The value as an interned constant, if it is a leaf.
    pub(super) fn as_constant(&self) -> Option<Constant> {
        Some(match self {
            Value::Unit => Constant::Unit,
            Value::Bool(value) => Constant::Bool(*value),
            Value::Nat(value) => Constant::Nat(*value),
            Value::Byte(value) => Constant::Byte(*value),
            Value::Int(value) => Constant::Int(*value),
            Value::Flt(value) => Constant::Flt(*value),
            Value::Io(value) => Constant::Io(*value),
            Value::Bin(grain, value) => Constant::Bin(*grain, value.as_ref().clone()),
            Value::Lst(_) | Value::Product(..) | Value::Construct(..) | Value::Closure(_) => {
                return None;
            }
        })
    }

    /// The leaf value of an interned constant.
    pub(super) fn from_constant(constant: &Constant) -> Value {
        match constant {
            Constant::Unit => Value::Unit,
            Constant::Bool(value) => Value::Bool(*value),
            Constant::Nat(value) => Value::Nat(*value),
            Constant::Byte(value) => Value::Byte(*value),
            Constant::Int(value) => Value::Int(*value),
            Constant::Flt(value) => Value::Flt(*value),
            Constant::Io(value) => Value::Io(*value),
            Constant::Bin(grain, value) => Value::Bin(*grain, Rc::new(value.clone())),
        }
    }

    pub(super) fn nat(&self) -> Result<u32, Bail> {
        match self {
            Value::Nat(value) => Ok(*value),
            _ => Err(Bail::Unsupported),
        }
    }

    pub(super) fn bool_(&self) -> Result<bool, Bail> {
        match self {
            Value::Bool(value) => Ok(*value),
            _ => Err(Bail::Unsupported),
        }
    }
}

/// Why one evaluation stopped without producing a value. Only [`Bail::Effect`]
/// can convert into a residual (a tail-position effect); every other reason
/// leaves the candidate untouched.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Bail {
    /// An effectful computation reached outside the candidate's tail.
    Effect,
    /// Per-candidate or shared step fuel exhausted.
    Fuel,
    /// The call-nesting cap was reached.
    Depth,
    /// The computation would trap at runtime on these operands; folding it
    /// would erase the trap.
    Trap,
    /// A name or identity could not be resolved to a value.
    Unknown,
    /// A call arity did not match the closure.
    Arity,
    /// A reified replacement exceeded the node or payload cap.
    TooBig,
    /// A value-recursive cycle was detected while forcing or reifying.
    Cycle,
    /// The computation reached a form the interpreter does not evaluate.
    Unsupported,
}
