//! The lowering into the landed Cont interface — the one-way door from meaning to mechanism.
//!
//! Every encoding decision the erasure deliberately deferred is made here, exactly once, and this is the table: `Unit`, `Bool`, and `Byte` ride the `Nat` carrier (`Bool` operations become `Nat` bit operations, `Byte` comparisons `Nat` comparisons, `NatToByte` a mask, `ByteToNat` the identity); a `Handle` token is its little-endian bytes as a byte-grain binary and `HandleEql` that grain's binary equality; products and variants are generic tuples (a variant is `(tag, payload…)`, the tag the constructor's position in its family); a single-constructor family collapses instead — nothing ever needs discriminating, so the tag is never minted and it encodes as the struct with the same relevant row would (one payload the bare value, several an untagged tuple, none the `Nat` zero, matches dispatch-free); a family whose one immediate-unary constructor stands beside boxed siblings rides that constructor bare too, matches discriminating with an `IsImmediate` test instead of a tag read; matches and switches are otherwise one `Nat`-keyed `Switch` behind the tag projection; the fold forms are synthesized accumulator loops; a function-only recursive group is a `LetFun`, and a group with computed members is a knot tied through compiler-internal cells. The per-family choice is [`FamilyEncoding`], a pure function of the registered schema; see `documentation/design/toolchain/a-variant-collapses-when-nothing-needs-to-distinguish-it.md` for the decision.
//!
//! The lowering is target-continuation shaped: each arena block is lowered against the continuation that receives its result — its terminator delivers there, and its statements build the node chain in front. Because the arena is already ANF, every operand is an atom and maps directly to a [`curios_cont::CpsAtom`]; no administrative continuation is introduced merely to evaluate an operand. Only a genuine control split — an application return, a switch or match, a fold loop, a host call — opens a join continuation whose parameter receives the split's result and whose body is the rest of the block.
//!
//! Arena identities are globally unique and never shadowed, so flat maps to their Cont counterparts suffice; source hints are carried onto the Cont values and functions they lower to.

mod census;
pub(crate) use census::{SequenceFacts, sequence_census};

mod emitter;
use emitter::*;

mod intrinsic;
use intrinsic::*;

mod layout;
use layout::*;

mod lowerer;
use lowerer::*;

#[cfg(test)]
mod tests;

use {
    super::{
        Analysis, Atom, Block, BlockId, CellOperation, Constant, ConstantId, ConstructorId,
        FamilyId, FieldShape, FoldNatStep, FoldSequenceStep, Function, FunctionId, Module,
        Operation, ProductId, RecGroup, RecGroupId, Rhs, SequenceGrain, SequenceOp, Sign,
        Statement, StatementId, Terminator, UnconsSequenceStep, ValueId, VariantArm,
    },
    curios_utilities::grown,
};

/// Lower a verified arena [`Module`] into the landed Cont [`curios_cont::CpsModule`]. The module's top level — its item chain followed by its entry block — becomes the parameterless Cps entry `main`, delivering its result to a bodyless `return_cont`. The produced module is verified; a failure is a lowering bug, not a user error, so it panics.
///
/// The walk recurses once per statement and once per block nesting, inside [`recurse`](curios_utilities::recurse), and the stage takes its first segment with [`grown`]: a folded parser is a chain of thousands of statements, which overflowed the default test-thread stack at about 1 900 levels.
pub fn lower_to_cont(source: &Module) -> curios_cont::CpsModule {
    curios_profile::profile!("lower_to_cont");
    grown(|| lower_to_cont_within(source))
}

fn lower_to_cont_within(source: &Module) -> curios_cont::CpsModule {
    Lowerer::new(source).finish()
}

/// An argumentless edge into a switch arm.
fn edge(target: curios_cont::CpsContId) -> curios_cont::CpsEdge {
    curios_cont::CpsEdge {
        target,
        args: Vec::new(),
    }
}
