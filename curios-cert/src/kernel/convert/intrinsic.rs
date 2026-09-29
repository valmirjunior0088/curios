//! Congruence for intrinsic operations.
//!
//! Two intrinsics are equal when they are the same operation applied to convertible operands. That is a congruence rule rather than a computation rule — the computation already happened, since both sides arrived in weak-head normal form and a foldable operation would have folded.
//!
//! The rule is stated *generically* rather than as one arm per operation, and it is stated once for both checkers: `curios-analysis`'s `convert_intrinsics` runs the carriers' algebra and reads the congruence off the traversal that defines an intrinsic's operands, so a new operation is covered the moment it is representable. What this module keeps is the kernel's own discharge.

use {
    super::{History, compare, ground},
    crate::{Kernel, KernelError},
    curios_analysis::{Congruence, Driver, Obligation, Outcome, convert_intrinsics},
    curios_core::Intrinsic,
    curios_utilities::SyntaxRegistry,
};

/// Whether `this` and `that` are the same intrinsic operation on convertible operands.
///
/// The chain that decides the pair or leaves what is left to compare is `curios-analysis`'s `convert_intrinsics`, the elaborator's too. What is the kernel's is the discharge: a residual is compared at `Type`, the levels by entailment, and each operand in order at its declared type, all under the active `History`, stopping at the first that fails.
pub(super) fn convert_intrinsic(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Intrinsic,
    that: &Intrinsic,
) -> Result<bool, KernelError> {
    match convert_intrinsics(kernel, this.clone(), that.clone())? {
        Outcome::Equal => Ok(true),
        Outcome::Unequal => Ok(false),
        Outcome::Residual(this, that) => ground(kernel, history, &this, &that),
        Outcome::Congruence(Congruence {
            this_levels,
            that_levels,
            operands,
        }) => {
            if !kernel.levels_eq(&this_levels, &that_levels) {
                return Ok(false);
            }
            let Some(operands) = operands else {
                return Ok(false);
            };
            for Obligation { type_, this, that } in operands {
                let converted = match type_ {
                    Some(type_) => compare(kernel, history, &type_, &this, &that)?,
                    None => ground(kernel, history, &this, &that)?,
                };
                if !converted {
                    return Ok(false);
                }
            }
            Ok(true)
        }
    }
}

/// The kernel is handed finished terms, so it prepares nothing, and it has no packed-literal view: the elaborator's view only proposes solutions, and once they are committed the two spellings agree by reduction.
impl Driver for Kernel {
    fn prepare(&mut self, intrinsic: Intrinsic) -> Intrinsic {
        intrinsic
    }

    fn packed_view(&mut self, _: &Intrinsic, _: &Intrinsic) -> Option<bool> {
        None
    }

    fn syntax(&self) -> SyntaxRegistry {
        Kernel::syntax(self)
    }
}
