//! Congruence for intrinsic operations.
//!
//! Two intrinsics are equal when they are the same operation applied to convertible operands. That is a congruence rule rather than a computation rule — the computation already happened, since both sides arrived in weak-head normal form and a foldable operation would have folded.
//!
//! The rule is stated *generically* rather than as one arm per operation, and it is stated once for both checkers: `curios-analysis`'s `convert_intrinsics` runs the carriers' algebra and reads the congruence off the traversal that defines an intrinsic's operands, so a new operation is covered the moment it is representable. What this module keeps is the kernel's own discharge.

use {
    super::{Pairs, classing, compare, ground},
    crate::{
        Error, Kernel, connectives_agree, connectives_agree_classed, convert_classed,
        convert_intrinsics,
    },
    curios_analysis::{Agreement, Classes, Congruence, Driver, Obligation, Outcome, Pass},
    curios_core::{Intrinsic, Term},
    curios_utilities::SyntaxRegistry,
};

/// Whether `this` and `that` are the same intrinsic operation on convertible operands.
///
/// The chain that decides the pair or leaves what is left to compare is `curios-analysis`'s `convert_intrinsics`, the elaborator's too. What is the kernel's is the discharge: the atoms the chain hands back are classed by the kernel's own comparison, a residual is compared at `Type`, the levels by entailment, and each operand in order at its declared type, all inside the pairs the goal stands in, stopping at the first that fails.
pub(super) fn convert_intrinsic(
    kernel: &mut Kernel,
    pairs: &mut Pairs,
    this: &Intrinsic,
    that: &Intrinsic,
) -> Result<bool, Error> {
    let outcome = match convert_intrinsics(kernel, this.clone(), that.clone())? {
        Pass::Settled(outcome) => outcome,
        Pass::Atoms(atoms) => {
            let classes = classes(kernel, pairs, &atoms)?;
            convert_classed(kernel, this.clone(), that.clone(), &classes)?
        }
    };
    match outcome {
        Outcome::Equal => Ok(true),
        // The kernel is handed finished terms: operands nothing paired are not going to be.
        Outcome::Unequal | Outcome::Unpaired => Ok(false),
        Outcome::Residual(this, that) => ground(kernel, pairs, &this, &that),
        Outcome::Congruence(Congruence {
            this_universes,
            that_universes,
            operands,
        }) => {
            if !kernel.universes_eq(&this_universes, &that_universes) {
                return Ok(false);
            }
            let Some(operands) = operands else {
                return Ok(false);
            };
            for Obligation { type_, this, that } in operands {
                let converted = match type_ {
                    Some(type_) => compare(kernel, pairs, &type_, &this, &that)?,
                    None => ground(kernel, pairs, &this, &that)?,
                };
                if !converted {
                    return Ok(false);
                }
            }
            Ok(true)
        }
    }
}

/// Whether a `Bool` connective on either side of a pair that is not two intrinsics agrees with the other side: `curios-analysis`'s `connectives_agree`, its atoms classed here where it hands them back.
pub(super) fn connectives_convert(
    kernel: &mut Kernel,
    pairs: &mut Pairs,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    match connectives_agree(kernel, this, that)? {
        Agreement::Agree => Ok(true),
        Agreement::Silent => Ok(false),
        Agreement::Atoms(atoms) => {
            let classes = classes(kernel, pairs, &atoms)?;
            Ok(connectives_agree_classed(kernel, this, that, &classes)?)
        }
    }
}

/// Which of `atoms` are one: each compared at `Type` with the representative of every class opened before it, inside the pairs of the goal the pair belongs to, as an operand of the congruence is. A comparison that fails leaves nothing behind, a pair being released where the comparison of its unfoldings returns.
fn classes(kernel: &mut Kernel, pairs: &mut Pairs, atoms: &[Term]) -> Result<Classes, Error> {
    curios_profile::profile!("convert::classes");
    classing(kernel, atoms, |kernel, this, that| {
        ground(kernel, pairs, this, that)
    })
}

/// The kernel is handed finished terms, so it prepares nothing, and it has no solving view: the elaborator's views only propose solutions, and once they are committed the two spellings agree by reduction.
impl Driver for Kernel<'_> {
    fn prepare(&mut self, intrinsic: Intrinsic) -> Intrinsic {
        intrinsic
    }

    fn solving_view(&mut self, _: &Intrinsic, _: &Intrinsic) -> Option<bool> {
        None
    }

    fn syntax(&self) -> SyntaxRegistry {
        Kernel::syntax(self)
    }
}
