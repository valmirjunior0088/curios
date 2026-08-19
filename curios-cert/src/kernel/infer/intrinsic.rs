//! Typing rules for intrinsic operations.
//!
//! One rule per operation: what its operands must be, and what it produces. Nothing here is inferred or negotiated — an intrinsic's signature is fixed by the language, so this module is a table, and the table is the specification.
//!
//! **The table is no longer written here.** It is `Intrinsic::signature` in `curios-core`, and this module is the walk that *applies* it. That is the whole of the change: a table written as a checking procedure can be executed by one caller and read by none, so the same signatures were written three times — as `/sys`'s declarations, as these rules, and again as elaboration's — in three crates with nothing checking the three agreed. Now the kernel and the elaborator check against one statement, and disagreeing about an operand's type is not something either can express.
//!
//! What the walk still owns is the two judgments a table cannot state: a type operand is established by *typing* it (`check_is_type`, never a structural read), and a parameterized former's sort is `sort_of_intrinsic`'s to compute, because the element's own sort is not the answer — a list or a cell of proofs has a length or an identity, and a description of proofs has an effect, so none of them is itself a proposition.

use {
    super::{check, infer},
    crate::{Kernel, KernelError, sort_of_intrinsic},
    curios_core::{Intrinsic, Operand, Produced, Reducer, Subterm, Term},
};

/// The type of `intrinsic`, having checked every operand against the type this operation demands of it.
pub(super) fn infer_intrinsic(
    kernel: &mut Kernel,
    intrinsic: &Intrinsic,
) -> Result<Term, KernelError> {
    let signature = intrinsic.signature(&kernel.syntax());
    let operands = intrinsic.operands();

    // The table and the traversal are two statements of the same operand list, and zipping them is only safe while they agree. A disagreement is this crate's own bug rather than a fault in the term, so it asserts rather than refusing — see `documentation/design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md` on what the kernel reports and what it must not.
    debug_assert_eq!(
        operands.len(),
        signature.operands.len(),
        "`signature` and `operands` disagree about {intrinsic:?}",
    );

    for (operand, demand) in operands.iter().zip(&signature.operands) {
        match demand {
            Operand::At(type_) => {
                check(kernel, operand, type_)?;
            }
            Operand::IsType => {
                check_is_type(kernel, operand)?;
            }
            Operand::Function { domain, codomain } => {
                let binder = kernel.fresh(Some("x"));
                check(
                    kernel,
                    operand,
                    &Term::func_type([(binder, domain.clone())], codomain.clone()),
                )?;
            }
        }
    }

    match signature.produced {
        Produced::Fixed(type_) => Ok(type_),
        Produced::Sort => Ok(sort_of_intrinsic(kernel, intrinsic)?.term()),
    }
}

/// Check that `term` is a type, and hand it back. An intrinsic that carries its element type carries a *type*, and taking that on trust is how a container of nonsense would be admitted.
fn check_is_type(kernel: &mut Kernel, term: &Term) -> Result<Term, KernelError> {
    let inferred = infer(kernel, term)?;

    match &*kernel.reduce_forced(inferred.clone())? {
        Subterm::Type(_) | Subterm::Prop => Ok(term.clone()),
        _ => Err(KernelError::NotASort(inferred)),
    }
}
