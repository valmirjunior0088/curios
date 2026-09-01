//! The interpreter's runtime value domain and the reasons an evaluation bails.
//!
//! A [`Value`] is the compile-time result of running a closed computation — internal to the evaluator, never stored in the module — so it carries the shapes the constant alphabet deliberately omits: `Rc`-shared product and constructor aggregates, a list as a [`ListWindow`] over shared elements, and a closure over a module function with its resolved local captures. Reification ([`super::reify`]) turns a value back into arena statements.

use {
    crate::{Constant, ConstructorId, FunctionId, Module, ProductId, ValueId},
    curios_num::Floating,
    curios_utilities::{Grain, PackedBin},
    std::{cell::RefCell, ops::Deref, rc::Rc},
};

/// A compile-time runtime value. Aggregates are `Rc`-shared so an environment clones freely; a closure names its module function and carries its resolved local captures.
#[derive(Clone)]
pub(super) enum Value {
    /// The unit value — the carrier of an erased proof or type slot.
    Unit,
    Bool(bool),
    Nat(u32),
    Byte(u8),
    Int(i32),
    Flt(Floating),
    Handle(u32),
    Bin(Grain, Rc<PackedBin>),
    List(ListWindow),
    /// A product value, in the schema's field order.
    Product(ProductId, Rc<Vec<Value>>),
    /// A variant value: its constructor and payload, in payload order.
    Construct(ConstructorId, Rc<Vec<Value>>),
    /// A closure over a module function with its resolved local captures. The environment is a `RefCell` so a local recursive group can bind its members first and backpatch each to see its siblings.
    Closure(Rc<Closure>),
}

pub(super) struct Closure {
    pub(super) function: FunctionId,
    pub(super) env: RefCell<Vec<(ValueId, Value)>>,
}

/// An immutable window over shared list elements — the list mirror of [`PackedBin`], so a suffix or a slice is a start and a length over the same allocation rather than a copy of the elements.
///
/// This is the runtime's own shape, not an evaluator convenience: `ListRest` and `ListSlice` are windows over one rope, and the peel the interpreter performs has to cost what the door's `emit_peel` costs, or a walk the program takes in linear time is taken here in quadratic. Before this, a suffix was rebuilt element by element at every step.
#[derive(Clone)]
pub(super) struct ListWindow {
    items: Rc<[Value]>,
    start: usize,
    len: usize,
}

impl ListWindow {
    /// A window over all of `items`.
    pub(super) fn new(items: Vec<Value>) -> Self {
        let len = items.len();
        Self {
            items: Rc::from(items),
            start: 0,
            len,
        }
    }

    /// The `len`-long window at `start` within this one, or `None` when it runs past the end — the bounds a slice traps on.
    pub(super) fn window(&self, start: usize, len: usize) -> Option<Self> {
        (start <= self.len && len <= self.len - start).then(|| Self {
            items: Rc::clone(&self.items),
            start: self.start + start,
            len,
        })
    }
}

impl Deref for ListWindow {
    type Target = [Value];

    fn deref(&self) -> &[Value] {
        &self.items[self.start..self.start + self.len]
    }
}

impl Value {
    /// Whether this value holds an erased description anywhere — a closure stamped at its birth by erasure's `thunk`, directly or through captures and containers. A candidate holding one gets the tight description budget: the host runs a description once per force, so a big replacement is a sequencing chain's suffix riding along, while the small staged residuals the collapse pins protect pass untouched.
    pub(super) fn contains_description(&self, module: &Module) -> bool {
        match self {
            Value::Unit
            | Value::Bool(_)
            | Value::Nat(_)
            | Value::Byte(_)
            | Value::Int(_)
            | Value::Flt(_)
            | Value::Handle(_)
            | Value::Bin(..) => false,
            Value::List(items) => items.iter().any(|item| item.contains_description(module)),
            Value::Product(_, fields) | Value::Construct(_, fields) => fields
                .iter()
                .any(|field| field.contains_description(module)),
            Value::Closure(closure) => {
                module
                    .function(closure.function)
                    .is_some_and(|function| function.description)
                    || closure
                        .env
                        .borrow()
                        .iter()
                        .any(|(_, held)| held.contains_description(module))
            }
        }
    }

    /// The value as an interned constant, if it is a leaf.
    pub(super) fn as_constant(&self) -> Option<Constant> {
        Some(match self {
            Value::Unit => Constant::Unit,
            Value::Bool(value) => Constant::Bool(*value),
            Value::Nat(value) => Constant::Nat(*value),
            Value::Byte(value) => Constant::Byte(*value),
            Value::Int(value) => Constant::Int(*value),
            Value::Flt(value) => Constant::Flt(*value),
            Value::Handle(value) => Constant::Handle(*value),
            Value::Bin(grain, value) => Constant::Bin(*grain, value.as_ref().clone()),
            Value::List(_) | Value::Product(..) | Value::Construct(..) | Value::Closure(_) => {
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
            Constant::Handle(value) => Value::Handle(*value),
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

/// Why one evaluation stopped without producing a value. Only [`Bail::Effect`] can convert into a residual (a tail-position effect); every other reason leaves the candidate untouched.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Bail {
    /// An effectful computation reached outside the candidate's tail.
    Effect,
    /// Per-candidate or shared step fuel exhausted.
    Fuel,
    /// The call-nesting cap was reached.
    Depth,
    /// The computation would trap at runtime on these operands; folding it would erase the trap.
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
