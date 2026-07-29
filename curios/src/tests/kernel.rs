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

use {
    curios_elab::{KernelError, Module, Term, recheck_module, recheck_module_verdicts},
    std::collections::BTreeMap,
};

/// Compile `source` and return the elaborated module the kernel checks.
fn elaborated(source: &str) -> Module {
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

    core.expect("Stage::CoreElab observed")
}

/// Compile `source` and hand the elaborated module to the kernel.
fn recheck(source: &str) -> Result<(), KernelError> {
    recheck_module(&elaborated(source), crate::DEFAULT_STEP_BUDGET)
}

/// A term's printed head, clipped — enough to tell one refusal's shape from
/// another's without pasting a standard-library type into a tally.
fn head(term: &Term) -> String {
    let rendered = format!("{term}");
    let rendered = rendered.split_whitespace().collect::<Vec<_>>().join(" ");

    match rendered.char_indices().nth(44) {
        Some((cut, _)) => format!("{}…", &rendered[..cut]),
        None => rendered,
    }
}

/// The class a refusal is tallied under.
///
/// Deliberately mechanical: the variant, plus for a mismatch the two sides'
/// printed heads. Naming classes like "index inversion" here would be inventing
/// categories from a heuristic, which is how this project's wrong answers get
/// made — the point of the tally is to let the categories fall out of it.
fn class(error: &KernelError) -> String {
    match error {
        KernelError::Mismatch { inferred, expected } => {
            format!("Mismatch  {}  vs  {}", head(inferred), head(expected))
        }
        other => {
            let rendered = format!("{other:?}");

            rendered
                .split(['(', ' ', '{'])
                .next()
                .unwrap_or("?")
                .to_string()
        }
    }
}

/// Every item the kernel refuses across a few whole programs, tallied by class.
///
/// Not an assertion — a measurement, run on demand. `recheck_module` stops at
/// the first refusal and so says nothing about what lies past it; this walks to
/// the end with each verdict independent of the others (see
/// `recheck_module_verdicts`), which is what makes the classes countable rather
/// than discovered one build at a time.
///
/// # This aborts in a debug build, and that is a defect
///
/// The kernel's judgment depth scales with a `Str` literal's *length* — 103
/// nested judgments at 40 bytes, 324 at 160, 494 at 640 — because a literal is
/// one certified-UTF-8 link per scalar and the `compare`/`sort_of`/`whnf`/
/// `reduce_prim` cycle walks the chain natively. Bounded by data length rather
/// than by written nesting is exactly the shape `AGENTS.md` forbids, and a
/// checker that aborts cannot be made load-bearing whatever else it decides.
///
/// Release completes the standard library only because its frames are smaller;
/// the depth is identical in both profiles, measured at the same watermark. So
/// running this in release is a way to take the measurement *until the defect
/// is fixed*, not a property of the test and not a reason the defect is
/// tolerable. Stack size is not something to hide behind — see the same rule
/// about `RUST_MIN_STACK` in `AGENTS.md`.
///
/// An abort rather than a tally is likewise a finding, not noise: nothing here
/// is wrapped in a catch, because a kernel that aborts is a kernel to fix.
#[test]
#[ignore = "inventory: measures where the kernel disagrees rather than asserting"]
fn kernel_disagreements() {
    let fixtures = [
        ("trivial", "()"),
        (
            "arithmetic",
            r#"
            use /std/{Nat};
            let double(n : Nat) -> Nat = Nat/add(n, n);
            /std/print(Nat/to_str(double(21)))
            "#,
        ),
        // A string literal is one `Utf8` derivation link per byte, which is the
        // shape that forced the totality walk to go iterative.
        ("literal", r#"/std/print("the quick brown fox\n")"#),
    ];

    for (label, source) in fixtures {
        let module = elaborated(source);
        let verdicts = recheck_module_verdicts(&module, crate::DEFAULT_STEP_BUDGET);

        let mut tally: BTreeMap<String, usize> = BTreeMap::new();
        for verdict in &verdicts {
            *tally.entry(class(&verdict.error)).or_default() += 1;
        }

        println!(
            "\n=== {label}: {} of {} items refused ===",
            verdicts.len(),
            module.items.len()
        );
        for (class, count) in &tally {
            println!("  {count:>4}  {class}");
        }
        for verdict in &verdicts {
            let name = match &verdict.name {
                Some(name) => format!("{name}"),
                None => "<entrypoint>".to_string(),
            };
            println!("        {name}  —  {}", class(&verdict.error));
        }
    }
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
