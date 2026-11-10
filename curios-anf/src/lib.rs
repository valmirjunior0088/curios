//! The erased, first-order IR — the pipeline stage between `curios-erase`'s type-directed erasure and the continuation IR of `curios-cps`. Types, proofs, and erasable binders are gone by construction.
//!
//! The program is flat, explicit, first-order data: a [`Module`] of arena-allocated blocks of single-operation statements over atomic operands, addressed by typed `u32` identities, with ordered top-level items plus an entry block at its top level. The alphabet is erased Core's vocabulary with its semantic identities intact, as `README.md`'s "Shapes stay distinct" decides. Functions store no capture lists; free values, uses, the call graph, and recursive components are derived on demand ([`Analysis`]).
//!
//! `curios-erase` is the sole producer, constructing through the checked [`Builder`]. It erases each declaration into a module of its own, a *fragment*, which declares what another declaration binds, and what it reads of another's rows, as an import ([`Imports`]) and verifies with no entry ([`Module::verify_fragment`]); [`Module::link`] appends fragments into one program under fresh identities, and only a program, which has no import left, is optimized or lowered. [`optimize`](optimize()) is the ANF-level optimizer (behavior-summary pruning, closed-term evaluation, literal-spine specialization, reconstruction recovery, monoid worker/wrapper), and [`lower_to_cps`] is the one-way door where every encoding decision is made, lowering the module to a `curios_cps::Module`.

mod id;
use id::*;
pub use id::{BlockId, ConstructorId, FamilyId, ForeignId, FunctionId, ProductId, ValueId};

mod atom;
pub use atom::{ArchivedAtom, ArchivedConstant, Atom, Constant};

mod operation;
pub use operation::{ArchivedOperation, Operation};

mod sequence;
pub use sequence::{
    ArchivedSequenceGrain, ArchivedSequenceOp, SequenceArity, SequenceGrain, SequenceOp,
};

mod schema;
pub use schema::{
    ArchivedConstructor, ArchivedField, ArchivedFieldShape, ArchivedProductFields,
    ArchivedProductSchema, ArchivedRegisterShape, ArchivedVariantFamily, Constructor, Field,
    FieldShape, ProductFields, ProductSchema, RegisterShape, VariantFamily,
};

mod node;
pub use node::{
    ArchivedBlock, ArchivedCellOperation, ArchivedChannelOperation, ArchivedFoldNatStep,
    ArchivedFoldSequenceStep, ArchivedFunction, ArchivedIntrinsic, ArchivedNatCase,
    ArchivedRecGroup, ArchivedRecValue, ArchivedRhs, ArchivedStatement,
    ArchivedSuspensionOperation, ArchivedTerminator, ArchivedUnconsSequenceStep,
    ArchivedVariantArm, Block, CellOperation, ChannelOperation, FoldNatStep, FoldSequenceStep,
    Function, Intrinsic, NatCase, RecGroup, RecValue, Rhs, Statement, SuspensionOperation,
    Terminator, UnconsSequenceStep, VariantArm,
};

mod module;
pub use module::{
    ArchivedImportedFunction, ArchivedImports, ArchivedInterface, ArchivedModule, ArchivedValueDef,
    Compaction, ImportedFunction, Imports, Interface, Module, ValueDef,
};

mod verify;
use verify::*;
pub use verify::{CycleCall, CycleStep, VerifyError};

mod build;
pub use build::Builder;

mod print;
use print::*;

mod walk;
use walk::*;

mod analysis;
pub use analysis::Analysis;
use analysis::*;

mod into_cps;
pub use into_cps::lower_to_cps;

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: the path says at its use site that the caller reached for scaffolding rather than product API. This crate's own tests read it too, which the feature does not reach.
#[cfg(any(test, feature = "test-support"))]
pub mod test_support;

mod semantics;
use semantics::*;

mod rebindings;
use rebindings::*;

mod summary;
use summary::*;

mod remap;
use remap::*;

mod link;
pub use link::{Linked, Mark, Resolution, Taken};

mod optimize;
pub use optimize::{evaluate_since, optimize};
