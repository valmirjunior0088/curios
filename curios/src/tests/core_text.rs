//! What a dump of Core writes a program's declarations as: a test and a foreign row in their forms, which imply their definitions, where the dump is handed what those are made against.

use {
    super::test_support::*,
    curios_module::{RootSource, SYNTAX},
    curios_pipeline::{DEFAULT_BUDGET, test_support::*},
    curios_prelude::with_prelude,
    curios_surface::{Entrypoint, FormScope, Spellable, Spelling},
    std::rc::Rc,
};

/// A foreign row is written as its declaration states it, and the definition that calls it, which the row makes, is written nowhere else.
#[test]
fn a_foreign_row_is_written_in_its_form() {
    let printed = core_elab(
        "foreign host_echo(Bytes) -> Bytes;
        /std/print(\"\")",
    );

    assert!(
        printed.contains("foreign /host_echo(Bytes) -> Bytes;"),
        "{printed}"
    );
    assert!(!printed.contains("let /host_echo"), "{printed}");
}

/// A test is written as its declaration states it, its value under its name, the thunk and the type its form makes left to it.
#[test]
fn a_test_is_written_in_its_form() {
    let source = "test held = /std/Test/assert(true);
        /std/print(\"\")";
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");
    let program = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none().tested())
        .expect("the fixture type-checks")
        .program;
    let printed = with_prelude(|prelude| {
        let scope = FormScope::over(SYNTAX, prelude.iter().map(|root| root.core()));
        let spelling = Spelling::core_text().with_form_scope(Rc::new(scope));

        program.spelled(&Rc::new(spelling)).to_string()
    });

    assert!(printed.contains("test /held =\n    "), "{printed}");
    assert!(!printed.contains("kind = Test"), "{printed}");
}
