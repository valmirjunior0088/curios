//! The independent kernel, run over modules this compiler has already accepted.
//!
//! These are the only tests that ask the second opinion about *real* elaborated
//! output rather than a hand-built fixture, so they are where a disagreement
//! between the two checkers actually shows up. A failure here is a question,
//! not a verdict: the kernel is incomplete in known places (coverage is
//! unverified, free-monoid elimination arms are unchecked, several conversion
//! positions compare syntactically), and each of those refuses valid programs.
//! See `curios-elab/src/recheck.rs`.
//!
//! # What these found, and what the previous note got wrong
//!
//! This note used to record that the kernel was blocked on unsolved universe
//! metavariables surviving into zonked Core, and that grounding them was the
//! next thing standing between this and a working second opinion. Both were
//! wrong, and how they were wrong is the part worth keeping: these tests read
//! `Stage::Core`, which the pipeline emits *before* elaboration. The
//! metavariables were the lowering's own universe seeds, and the module the
//! kernel kept refusing had never been type-checked at all. A zonked module's
//! levels are ground — `validate_universes` rejects a term-level metavariable,
//! and always did.
//!
//! The lesson is the one this effort keeps relearning: the diagnosis was
//! derived by reading the refusal and reasoning about which pass must have let
//! it through, and it named a mechanism that was working correctly. Printing
//! the module settled it in one command.
//!
//! Reading `Stage::CoreElab`, the kernel now walks the prelude and stops at
//! index inversion. `/std/Nat/Lte/trans` scrutinizes two `Lte` proofs: one arm
//! refines `b` to `b2 + 1`, the other to `b3 + 1`, and the recursive call needs
//! `b2 ≡ b3`, which follows only by inverting successor. That is what
//! `curios-elab/src/invert.rs` does and the kernel has no equivalent of, so the
//! refusal is a gap in the kernel rather than a defect in what it was handed.
//!
//! These stay ignored as the record of where the walk stops. Running them is
//! how the next gap gets found.

use curios_elab::{KernelError, recheck_module};

/// Compile `source` and hand the elaborated module to the kernel.
fn recheck(source: &str) -> Result<(), KernelError> {
    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("the fixture parses");

    let mut core = None;
    curios_pipeline::compile_entrypoint(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| {
            if let curios_pipeline::Stage::CoreElab(module) = stage {
                core = Some(module.clone());
            }
        },
    )
    .expect("the fixture compiles");

    let core = core.expect("Stage::CoreElab observed");

    recheck_module(&core, crate::DEFAULT_STEP_BUDGET)
}

/// The smallest thing that is still a whole program: the kernel walks every
/// prelude item ahead of the entrypoint, so even this exercises the module
/// driver over the real standard library.
#[test]
#[ignore = "blocked: the kernel has no index inversion (see the module note)"]
fn a_trivial_program_rechecks() {
    let outcome = recheck("()");

    assert_eq!(outcome, Ok(()), "the kernel refused a trivial program");
}

#[test]
#[ignore = "blocked: the kernel has no index inversion (see the module note)"]
fn arithmetic_rechecks() {
    let outcome = recheck(
        r#"
        use /std/{Nat};
        let double(n : Nat) -> Nat = Nat/add(n, n);
        /std/print(Nat/to_str(double(21)))
        "#,
    );

    assert_eq!(outcome, Ok(()), "the kernel refused an arithmetic program");
}
