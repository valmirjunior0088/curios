//! The independent kernel, run over modules this compiler has already accepted.
//!
//! These are the only tests that ask the second opinion about *real* elaborated output rather than a hand-built fixture, so they are where a disagreement between the two checkers actually shows up. A failure here is a question, not a verdict: the kernel is incomplete in known places (coverage is unverified, free-monoid elimination arms are unchecked, several conversion positions compare syntactically), and each of those refuses valid programs. See `curios-cert/src/recheck.rs`.
//!
//! What these assert is stronger than what a compilation already does. `compile_entrypoint` runs `recheck_module_suffix`, judging the user's items while the archived prelude prefix is defined on the archive's word; `recheck` here runs `recheck_module` over the whole elaborated module, so every prelude item is re-derived rather than read from a verdict recorded when the archive was built.
//!
//! # What these found, and what the previous notes got wrong
//!
//! This note used to record that the kernel was blocked on unsolved universe metavariables surviving into zonked Core, and that grounding them was the next thing standing between this and a working second opinion. Both were wrong, and how they were wrong is the part worth keeping: these tests read `Stage::Core`, which the pipeline emits *before* elaboration. The metavariables were the lowering's own universe seeds, and the module the kernel kept refusing had never been type-checked at all. A zonked module's levels are ground — `validate_universes` rejects a term-level metavariable, and always did.
//!
//! The lesson is the one this effort keeps relearning: the diagnosis was derived by reading the refusal and reasoning about which pass must have let it through, and it named a mechanism that was working correctly. Printing the module settled it in one command.
//!
//! Reading `Stage::CoreElab`, the walk then stopped at index inversion. `/std/Nat/Lte/trans` scrutinizes two `Lte` proofs: one arm refines `b` to `b2 + 1`, the other to `b3 + 1`, and the recursive call needs `b2 ≡ b3`, which follows only by inverting successor — which the elaborator could do and the kernel had no equivalent of, making the refusal a gap in the kernel rather than a defect in what it was handed. It closed by making inversion an analysis both checkers reach through the `Env`/`Judge` seam (`curios-cert/src/invert.rs`) instead of an elaborator-only one, which is also why it bought no second opinion about inversion itself: a shared analysis is a place the two checkers *cannot* disagree (see DESIGN.md, "An independent kernel re-checks what the elaborator accepts").
//!
//! So these assert rather than record. The next gap in the kernel arrives as a failure here.

use {
    curios_cert::KernelError,
    curios_cert::{recheck_module, recheck_module_verdicts, recheck_module_verdicts_uncached},
    curios_core::Module,
    curios_core::Term,
    curios_pipeline::{Stage, compile_entrypoint},
    curios_text::{Entrypoint, RootSource},
    std::collections::BTreeMap,
};

/// Compile `source` and return the elaborated module the kernel checks.
fn elaborated(source: &str) -> Module {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");

    let mut core = None;
    compile_entrypoint(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        |stage| {
            if let Stage::CoreElab(module) = stage {
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

/// A term's printed head, clipped — enough to tell one refusal's shape from another's without pasting a standard-library type into a tally.
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
/// Deliberately mechanical: the variant, plus for a mismatch the two sides' printed heads. Naming classes like "index inversion" here would be inventing categories from a heuristic, which is how this project's wrong answers get made — the point of the tally is to let the categories fall out of it.
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
/// Not an assertion — a measurement, run on demand. `recheck_module` stops at the first refusal and so says nothing about what lies past it; this walks to the end with each verdict independent of the others (see `recheck_module_verdicts`), which is what makes the classes countable rather than discovered one build at a time.
///
/// # It used to abort in a debug build, and that was the defect
///
/// Judgment depth once scaled with a `Str` literal's *length* — 103 nested judgments at 40 bytes, 324 at 160, 494 at 640 — because a literal is one certified-UTF-8 link per scalar and `infer`/`check` descended two frames per link. At roughly 21.5KiB of stack per level in a debug build that exhausted a 2MiB thread partway through `/std/Toml`, and no reduction budget could prevent it: a budget bounds steps, and depth is not steps.
///
/// `infer` now defers the child obligations of an application, a constructor, and a record onto a stack rather than descending into them, so depth is bounded by written nesting. Both profiles complete, and both report the same verdicts — which is the check that this was a restructuring rather than a change of rule.
///
/// The measurement that found it is worth keeping: a backtrace at depth 300 showed the stack was 300 `infer` and 298 `check` frames and *nothing else*, which retired an earlier diagnosis naming four functions that were never on it. Stack size is not something to hide behind either: raising `RUST_MIN_STACK` would have concealed this rather than fixed it.
///
/// An abort rather than a tally is a finding, not noise: nothing here is wrapped in a catch, because a kernel that aborts is a kernel to fix.
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
        // A string literal is one `Utf8` derivation link per byte, which is the shape that forced the totality walk to go iterative.
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

/// Memoization is an evaluation strategy exactly as long as switching it off changes nothing. This runs the whole-prelude walk both ways and requires the verdict lists identical — the same instrument that validated the defunctionalized judgment (identical counts across profiles).
#[test]
#[ignore = "parity: runs the whole-prelude walk twice, the second time uncached"]
fn kernel_memo_parity() {
    let module = elaborated("()");

    assert_eq!(
        recheck_module_verdicts(&module, crate::DEFAULT_STEP_BUDGET),
        recheck_module_verdicts_uncached(&module, crate::DEFAULT_STEP_BUDGET),
    );
}

/// The smallest thing that is still a whole program: the kernel walks every prelude item ahead of the entrypoint, so even this exercises the module driver over the real standard library.
#[test]
fn a_trivial_program_rechecks() {
    let outcome = recheck("()");

    assert_eq!(outcome, Ok(()), "the kernel refused a trivial program");
}

#[test]
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
