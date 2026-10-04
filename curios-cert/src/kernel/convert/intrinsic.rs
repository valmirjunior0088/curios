//! Congruence for intrinsic operations.
//!
//! Two intrinsics are equal when they are the same operation applied to convertible operands. That is a congruence rule rather than a computation rule — the computation already happened, since both sides arrived in weak-head normal form and a foldable operation would have folded.
//!
//! The rule is stated *generically* rather than as one arm per operation, and it is stated once for both checkers: `curios-analysis`'s `convert_intrinsics` runs the carriers' algebra and reads the congruence off the traversal that defines an intrinsic's operands, so a new operation is covered the moment it is representable. What this module keeps is the kernel's own discharge.

use {
    super::{History, compare, ground},
    crate::{Error, Kernel},
    curios_analysis::{
        Agreement, Congruence, Driver, Obligation, Outcome, Pass, connectives_agree,
        connectives_agree_classed, convert_classed, convert_intrinsics,
    },
    curios_core::{Classes, Intrinsic, Term},
    curios_utilities::SyntaxRegistry,
};

/// Whether `this` and `that` are the same intrinsic operation on convertible operands.
///
/// The chain that decides the pair or leaves what is left to compare is `curios-analysis`'s `convert_intrinsics`, the elaborator's too. What is the kernel's is the discharge: the atoms the chain hands back are classed by the kernel's own comparison, a residual is compared at `Type`, the levels by entailment, and each operand in order at its declared type, all under the active `History`, stopping at the first that fails.
pub(super) fn convert_intrinsic(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Intrinsic,
    that: &Intrinsic,
) -> Result<bool, Error> {
    let outcome = match convert_intrinsics(kernel, this.clone(), that.clone())? {
        Pass::Settled(outcome) => outcome,
        Pass::Atoms(atoms) => {
            let classes = classes(kernel, history, &atoms)?;
            convert_classed(kernel, this.clone(), that.clone(), &classes)?
        }
    };
    match outcome {
        Outcome::Equal => Ok(true),
        // The kernel is handed finished terms: operands nothing paired are not going to be.
        Outcome::Unequal | Outcome::Unpaired => Ok(false),
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

/// Whether a `Bool` connective on either side of a pair that is not two intrinsics agrees with the other side: `curios-analysis`'s `connectives_agree`, its atoms classed here where it hands them back.
pub(super) fn connectives_convert(
    kernel: &mut Kernel,
    history: &mut History,
    this: &Term,
    that: &Term,
) -> Result<bool, Error> {
    match connectives_agree(kernel, this, that)? {
        Agreement::Agree => Ok(true),
        Agreement::Silent => Ok(false),
        Agreement::Atoms(atoms) => {
            let classes = classes(kernel, history, &atoms)?;
            Ok(connectives_agree_classed(kernel, this, that, &classes)?)
        }
    }
}

/// Which of `atoms` are one: each compared at `Type` with the representative of every class opened before it, under the history of the goal the pair belongs to. A comparison that fails leaves nothing behind, since a goal leaves the history whatever its outcome.
///
/// **Two atoms whose comparison is already in progress are left apart.** The recurrence rule assumes a goal met again, which is sound for a goal whose children are all still compared; a classing that took the assumption would spell one atom as the other and decide the pair it belongs to without comparing anything. So an atom pair the history holds is two atoms here, which is the refusing direction.
fn classes(kernel: &mut Kernel, history: &mut History, atoms: &[Term]) -> Result<Classes, Error> {
    curios_profile::profile!("convert::classes");
    Classes::of(atoms, |this, that| {
        if in_progress(kernel, history, this, that) || in_progress(kernel, history, that, this) {
            return Ok(false);
        }
        ground(kernel, history, this, that)
    })
}

/// Whether comparing `this` with `that` at `Type` is a goal the history already holds.
fn in_progress(kernel: &Kernel, history: &mut History, this: &Term, that: &Term) -> bool {
    match history.enter(kernel, &Term::type_ground(), this, that) {
        None => true,
        Some(goal) => {
            history.leave(&goal);
            false
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
