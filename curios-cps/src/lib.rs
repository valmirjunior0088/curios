//! The Curios continuation IR: the arena-backed, pre-closure CPS graph the erased stage lowers into, and the optimizer that rewrites it.
//!
//! `curios_anf::lower_to_cps` constructs the public [`Module`], and [`optimize`](optimize()) rewrites that high-CPS graph. Lowering it to WebAssembly is `curios-emit`'s, which depends on this crate rather than the reverse, so the erased stage that lowers into this IR never builds the emitter or `curios-wasm`. [`representation`] derives which values a machine register can hold and at which carriers a function the analysis sees whole takes its parameters and hands back its results, and [`Module::entry_suspension`] derives whether the residual entry needs the turn ABI, both from the dataflow solver the passes share.
//!
//! Every CPS function owns a globally unique bodyless return continuation. Ordinary return is `ApplyCont(function.return_cont, [value])`; `Halt` is reserved for a host call that terminates the process.

mod analysis;
use analysis::*;

mod bound;
use bound::*;

mod dataflow;
use dataflow::*;

mod demand;
use demand::*;

mod intrinsic;
pub use intrinsic::{Intrinsic, IntrinsicEffect, Repr};

mod module;
pub use module::{FieldGroup, Module, Row, Slot};

mod node;
use node::*;
pub use node::{
    Atom, Callee, CellOp, ChannelOp, Continuation, ContinuationId, ENVELOPE_BITS, Edge, Function,
    FunctionId, IntrinsicCall, Literal, Node, NodeId, Panic, Register, RowId, UseTarget, ValueDef,
    ValueExpr, ValueId, atoms, int_is_small, nat_is_small,
};

mod optimize;
pub use optimize::optimize;
use optimize::*;

mod origin;
use origin::*;

mod row_flow;
use row_flow::*;

mod sharing;
use sharing::*;

mod print;

mod rebuild;
pub use rebuild::rebuild_in_place;

mod represent;
pub use represent::{Representation, Storage, representation};

mod survey;
pub use survey::{Fate, Outcome, descendants, fates};

mod verify;
pub use verify::VerifyError;
use verify::*;

#[cfg(test)]
mod test_support;
#[cfg(test)]
mod tests;
