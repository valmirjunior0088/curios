//! What `wonder stage` answers: a rung the driver reached is an answer whatever stopped the program after it, and a rung it never reached is a refusal carrying what stopped it.

use {
    crate::{Origin, Subject},
    curios_pipeline::DEFAULT_STEP_BUDGET,
    curios_text::Overlay,
};

/// One rung of `program`, asked for the way the one-shot transport asks: what the asker was handed, beside what the query answered.
fn rung(name: &str, text: &str) -> Result<(String, crate::Rendering), crate::Refusal> {
    let mut handed = String::new();
    let reached = crate::stage(
        DEFAULT_STEP_BUDGET,
        Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                label: "<stdin>".to_string(),
                text: text.to_string(),
            },
            declares: None,
            unlinked: None,
        },
        &Overlay::default(),
        None,
        name,
        |rung| handed.push_str(rung),
    );

    match reached {
        Ok(crate::Reached::Rendered(rendering)) => Ok((handed, rendering)),
        Ok(crate::Reached::Wasm(_)) => panic!("asked for a driver rung, reached the module"),
        Err(refusal) => Err(refusal),
    }
}

const REFUSED_LATER: &str = "use /std/{Nat};\nlet x : Nat = true;\n/std/print(\"ok\\n\")";

/// A rung the driver emitted is an answer, and a failure *after* it does not unmake one. Asking for `core` on a program that fails to elaborate is asking what the lowering produced, which is most of the reason to ask at all.
#[test]
fn a_rung_reached_before_the_failure_is_still_answered() {
    for name in ["text", "core"] {
        let (handed, rendering) = rung(name, REFUSED_LATER)
            .unwrap_or_else(|_| panic!("{name} was observed before the refusal"));
        assert_eq!(rendering.name, name);
        assert!(!handed.is_empty(), "{name} rendered nothing");
        assert!(
            !rendering.diagnostics.is_empty(),
            "{name} lost the diagnostics that stopped the compilation"
        );
    }
}

/// The other side of the same boundary: a rung the program never reached has not been answered, and stays a refusal. `ersd` sits past elaboration, which is where this program stops.
#[test]
fn a_rung_the_program_never_reached_is_refused() {
    let refusal = rung("ersd", REFUSED_LATER).expect_err("ersd is never reached");
    assert!(
        matches!(refusal, crate::Refusal::Diagnostics(diagnostics) if !diagnostics.is_empty()),
        "expected the refusal to carry what stopped the program"
    );
}

/// A program that compiles answers with nothing beside the rendering, so the added channel stays empty on the ordinary path.
#[test]
fn a_rung_of_a_program_that_compiles_carries_no_diagnostics() {
    let (_, rendering) = rung("core", "/std/print(\"hi\\n\")").expect("core is reached");
    assert!(rendering.diagnostics.is_empty());
}
