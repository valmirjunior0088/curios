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
//! # Why these are ignored, and what they found
//!
//! Both currently fail, and on something more basic than any gap listed above:
//! **the module the kernel is handed still carries unsolved universe
//! metavariables in its levels.** Two occurrences of the same scheme arrive as
//! `(T : Type ?10) -> Type ?10` and `(T : Type ?8) -> Type ?9`, and the kernel
//! compares levels structurally, so the meta identities have to match exactly
//! and never do.
//!
//! That is not a bug in either checker. Deciding whether two levels *can* be
//! equal is what the elaborator's universe solver does, by recording a
//! constraint and discharging it later; the kernel has no solver, deliberately
//! — a `UniverseContext` is data, and satisfiability is inference over it. So
//! either the module reaches the kernel with its levels ground, or the kernel
//! cannot check universes at all. Guessing is not available: reading two
//! distinct metas as equal would admit `Type 0` where `Type 5` was meant, which
//! is the unsound direction.
//!
//! Grounding the levels is a change to declaration finalization, not to the
//! kernel, and it is the next thing standing between this and a working second
//! opinion. These tests stay here, ignored, as the record of exactly what is
//! blocking — running them is how the block gets confirmed gone.

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
            if let curios_pipeline::Stage::Core(module) = stage {
                core = Some(module.clone());
            }
        },
    )
    .expect("the fixture compiles");

    let core = core.expect("Stage::Core observed");

    recheck_module(&core, crate::DEFAULT_STEP_BUDGET)
}

/// The smallest thing that is still a whole program: the kernel walks every
/// prelude item ahead of the entrypoint, so even this exercises the module
/// driver over the real standard library.
#[test]
#[ignore = "blocked: zonked Core carries unsolved universe metavariables (see the module note)"]
fn a_trivial_program_rechecks() {
    let outcome = recheck("()");

    assert_eq!(outcome, Ok(()), "the kernel refused a trivial program");
}

#[test]
#[ignore = "blocked: zonked Core carries unsolved universe metavariables (see the module note)"]
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
