use {super::Entrypoint, crate::Error, curios_utilities::Qualifier};

/// Text has no file, but it has line numbers — so a diagnostic about it still says where, with the label standing exactly where a path would.
#[test]
fn supplied_text_names_itself_in_diagnostics() {
    let Err(error) = Entrypoint::supplied("<stdin>", "/std/print(\"unclosed\n") else {
        panic!("the string literal never closes");
    };

    let error = error.format();
    assert!(error.contains("<stdin>:2:1"), "{error}");
}

/// A file-backed `mod` has nowhere to resolve from, and answers as a missing module rather than by reading a directory invented from a stem nothing has.
#[test]
fn supplied_text_resolves_no_file_backed_modules() {
    let (_, loader, _) = Entrypoint::supplied("<stdin>", "mod util;\n()").expect("it parses");

    assert!(
        loader.directories().is_empty(),
        "supplied text reads from nowhere on disk"
    );
    assert!(matches!(
        loader.load(&Qualifier::from(["util"])),
        Err(Error::ModuleNotFound { .. })
    ));
}

/// What a supplied program cannot do is spread itself over files — not declare modules. An inline one is untouched.
#[test]
fn supplied_text_keeps_inline_modules() {
    let (entrypoint, _, _) = Entrypoint::supplied(
        "<stdin>",
        "mod util\n    pub let greeting: Str = \"hi\";\nend\n\nutil/greeting",
    )
    .expect("it parses");

    assert_eq!(entrypoint.module.items.len(), 1);
}
