use {
    super::{Entrypoint, Form},
    crate::Error,
    curios_utilities::Qualifier,
    std::path::Path,
};

/// A text's form is read off the text: items alone are a module, a final term after them makes a program, and a text that does not parse is a program, whose own reading reports what is wrong with it.
#[test]
fn a_text_is_a_module_when_no_final_term_follows_its_items() {
    let path = Path::new("probe.crs");

    assert_eq!(
        Form::of(path, "pub let w : /std/Str = \"a\";\n"),
        Form::Module
    );
    assert_eq!(Form::of(path, ""), Form::Module);
    assert_eq!(
        Form::of(path, "pub let w : /std/Str = \"a\";\n/std/print(w)\n"),
        Form::Program
    );
    assert_eq!(Form::of(path, "let w : = ;\n"), Form::Program);
}

/// Text has no file, but it has line numbers — so a diagnostic about it still says where, with the label standing exactly where a path would.
#[test]
fn supplied_text_names_itself_in_diagnostics() {
    let Err(error) = Entrypoint::supplied("<stdin>", "/std/print(\n") else {
        panic!("the call never closes");
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
