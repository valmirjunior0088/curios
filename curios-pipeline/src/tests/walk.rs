//! How a unit's check tells how far its walk over its declarations has come: as each is asked for and as each finishes.

use {
    super::test_support::*,
    crate::*,
    curios_core::Global,
    curios_elab::{DEFAULT_BUDGET, Walked},
    curios_module::{RootSource, SYNTAX, UnitSource},
    curios_prelude::with_prelude,
    curios_unit::Predecessors,
};

/// A value read through a witness written after it: the walk's first attempt at the value meets the witness unfinished and is voided, the witness is finished, and the value is attempted again.
const READ_AHEAD: &str = r#"use /std/{Str};

concept Spoken(A: Type): Type {
    spoken(A) -> Str,
}

induct Tone: Type
| hush()
end

let said: Str = Spoken/spoken(Tone/hush());

satisfy Spoken(Tone) {
    spoken(_) => "hush",
}
"#;

/// How far a walk had come, as a test reads it: how many declarations were finished, of how many, and the one it was asking for.
type Told = (usize, usize, Option<String>);

/// `walked` as a test reads it, an authored name by its path and a witness as `witness`.
fn told(walked: Walked<'_>) -> Told {
    let on = walked.on.map(|name| match name {
        Global::Authored(path) => path.join(),
        Global::Witness(_) => "witness".to_string(),
    });

    (walked.done, walked.of, on)
}

/// What the fold told of `source`'s walk, mounted at `/lib`.
fn walked(source: &str) -> Vec<Told> {
    let modules = mounted("lib", source);
    let sources = [(UnitSource::mounted(&modules), None)];
    let mut walked = Vec::new();

    with_prelude(|prelude| {
        compile_units(
            DEFAULT_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &sources,
            None,
            |progress| {
                if let Progress::Walked { done, of, on, .. } = progress {
                    walked.push(told(Walked { done, of, on }));
                }
            },
            Listeners::default(),
        )
    })
    .expect("the unit compiles");

    walked
}

/// Each declaration is told as it is asked for and as it finishes, the count rising by one at each finish: the attempt at a value voided for a witness written after it asks for the witness inside it, and the value is counted once, when the attempt that stands ends.
#[test]
fn a_voided_attempt_is_counted_once_when_its_declaration_finishes() {
    let on = |name: &str| Some(name.to_string());

    assert_eq!(
        walked(READ_AHEAD),
        [
            (0, 4, on("/lib/Spoken")),
            (1, 4, None),
            (1, 4, on("/lib/Tone")),
            (2, 4, None),
            (2, 4, on("/lib/said")),
            (2, 4, on("witness")),
            (3, 4, on("/lib/said")),
            (4, 4, None),
        ]
    );
}

/// A program's own declarations are a unit of its own, told of by the root prefix the entry owns, and walked as a mounted unit's are.
#[test]
fn a_programs_own_declarations_are_told_of_by_the_root() {
    let entrypoint = entrypoint_of(&[READ_AHEAD, "\n/std/print(said)\n"].concat());
    let mut walked = Vec::new();
    Fold::new(DEFAULT_BUDGET, &[], None)
        .compile(
            &entrypoint,
            &RootSource::none(),
            |_| {},
            |progress| {
                if let Progress::Walked { unit, done, of, on } = progress {
                    assert!(unit.is_root(), "told of by `{}`", unit.join());
                    walked.push(told(Walked { done, of, on }));
                }
            },
        )
        .map_err(String::from)
        .expect("the program compiles");
    let on = |name: &str| Some(name.to_string());

    assert_eq!(
        walked,
        [
            (0, 4, on("/Spoken")),
            (1, 4, None),
            (1, 4, on("/Tone")),
            (2, 4, None),
            (2, 4, on("/said")),
            (2, 4, on("witness")),
            (3, 4, on("/said")),
            (4, 4, None),
        ]
    );
}

/// Over a baseline the walk holds what the edit reached and no more, so its count is of those and ends on every one of them finished.
#[test]
fn a_walk_over_a_baseline_counts_what_the_edit_reached() {
    let baseline = unit_after("lib", READ_AHEAD, &[]);
    let modules = mounted("lib", &READ_AHEAD.replace("\"hush\"", "\"hushed\""));
    let mut walked = Vec::new();
    with_prelude(|prelude| {
        compile_unit_over(
            DEFAULT_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &UnitSource::mounted(&modules),
            Over {
                baseline: &baseline,
                moved: &vec![false; prelude.len()],
                candidates: None,
            },
            &|_| {},
            Telling {
                walked: Some(&mut |walk: Walked<'_>| walked.push(told(walk))),
                refused: None,
            },
        )
    })
    .map_err(String::from)
    .expect("the unit compiles over its baseline");
    let of = walked.last().map_or(0, |(_, of, _)| *of);

    assert!(0 < of && of < 4, "the edit reached {of} of 4: {walked:?}");
    assert!(walked.iter().all(|(_, held, _)| *held == of), "{walked:?}");
    assert_eq!(walked.last(), Some(&(of, of, None)));
}
