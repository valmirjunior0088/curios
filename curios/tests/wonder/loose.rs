//! `curios wonder` over a file and where it is checked: placed in the unit whose `mod` lines reach it, or alone — a file no `mod` declares, a module on standard input, an entry of declarations alone, a loose module's rungs and a stem that is no name.

use {super::test_support::*, std::fs};

/// A file is placed in the unit that declares it: the executable compiles against its library, so `/app/util/word` resolves — which it would not standalone — and a module of the library is checked as the library, reporting at the module's own path.
#[test]
fn a_file_is_placed_in_its_unit() {
    let root = project("placed");

    let executable = curios(&root, &["check", "app.crs"], "");
    assert!(executable.status.success());
    assert_eq!(stdout(&executable), "", "the executable sees its library");

    write(&root, "util.crs", "pub let word : /std/Str = 1;\n");
    let module = curios(&root, &["check", "util.crs"], "");
    assert_eq!(
        module.status.code(),
        Some(1),
        "an error is reported, and exits as a refusal"
    );
    let text = stdout(&module);
    assert!(text.contains("type mismatch"), "{text}");
    assert!(text.contains("util.crs:1:27"), "{text}");
}

/// A file under a package that no `mod` reaches is in no unit, so it is checked on its own, as a unit of its own where it is written as a module, with nothing said of that, on a cold cabinet and on a warm one alike, since placement walks the headers and not the record of a compile. Declaring it puts it in the library.
#[test]
fn a_file_no_mod_declares_is_checked_on_its_own() {
    let root = project("undeclared");
    write(&root, "stray.crs", "pub let w : /std/Str = 1;\n");

    let cold = curios(&root, &["check", "stray.crs"], "");
    assert_eq!(cold.status.code(), Some(1));
    let text = stdout(&cold);
    assert!(
        text.starts_with("type mismatch"),
        "checked on its own, and nothing said of it: {text}"
    );
    assert!(text.contains("stray.crs:1:24"), "{text}");

    // Warm: the library is served from the cabinet, and placement still walks the headers.
    let built = curios(&root, &["run", "app"], "");
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    let warm = curios(&root, &["check", "stray.crs"], "");
    assert_eq!(stdout(&warm), text);

    write(&root, "lib.crs", "pub mod util;\npub mod stray;\n");
    let declared = curios(&root, &["check", "stray.crs"], "");
    assert!(
        stdout(&declared).contains("stray.crs:1:24"),
        "its own error, read as the library's: {}",
        stdout(&declared)
    );
}

/// Text on standard input written as a module is checked as one, as a loose file is — a unit of its own, mounted at `/stdin` since it has no stem — so its items are answered one by one; a rung past a unit's needs a program, and refuses it as written as a module.
#[test]
fn a_module_on_standard_input_is_checked_as_a_unit_of_its_own() {
    let root = temporary("stdin-module");
    fs::create_dir_all(&root).unwrap();
    let module = "pub let w : /std/Str = 1;\n";

    let answered = curios(&root, &["check", "-"], module);
    assert_eq!(answered.status.code(), Some(1));
    let text = stdout(&answered);
    assert!(text.contains("└─ [error] <stdin>:1:24"), "{text}");

    let staged = curios(&root, &["wonder", "--stage", "anf", "-"], module);
    let refusal = String::from_utf8_lossy(&staged.stderr);
    assert_eq!(staged.status.code(), Some(1), "{refusal}");
    assert!(staged.stdout.is_empty());
    assert!(
        refusal.contains(
            "not a program\n│  this text is written as a module\n│  it has no final term to compile a program from\n└─ [error] <stdin>"
        ),
        "{refusal}"
    );
}

/// A loose module is reprinted at the rungs a unit has, as a library's file is — `text` its own text parsed, `core` its lowering and `core-elab` its elaborated declarations, under its stem from a file and under `/stdin` from standard input — and a rung past them, rooted at a program's entry, refuses it as written as a module.
#[test]
fn a_loose_module_is_reprinted_at_a_units_rungs_and_no_other() {
    let root = temporary("loose-rungs");
    fs::create_dir_all(&root).unwrap();
    let module = "pub let double(n: /std/Nat) -> /std/Nat =>\n    n + n;\n";
    write(&root, "double.crs", module);

    for (subject, stdin, mounted) in [
        ("double.crs", "", "/double/double"),
        ("-", module, "/stdin/double"),
    ] {
        let rung = |name: &str| curios(&root, &["wonder", "--stage", name, subject], stdin);

        let text = rung("text");
        assert!(text.status.success(), "{subject}: {}", stdout(&text));
        assert_eq!(stdout(&text), module, "{subject}");
        for name in ["core", "core-elab"] {
            let reprinted = rung(name);
            let printed = stdout(&reprinted);
            assert!(reprinted.status.success(), "{subject} at {name}: {printed}");
            assert!(
                printed.starts_with(&format!("let {mounted}: ")),
                "{subject} at {name}: {printed}"
            );
        }

        let past = rung("anf");
        let refusal = String::from_utf8_lossy(&past.stderr);
        assert_eq!(past.status.code(), Some(1), "{subject}: {refusal}");
        assert!(past.stdout.is_empty(), "{subject}");
        assert!(
            refusal.starts_with("not a program\n"),
            "{subject}: {refusal}"
        );
    }
}

/// A declared executable is a program by its declaration, so an entry of declarations alone is refused by the program grammar, in its words, whichever command reads it: the check answers the refusal and `run` stops on it, one sentence in both.
#[test]
fn an_entry_of_declarations_alone_is_refused_for_its_term_by_every_reader() {
    let root = project("termless-entry");
    write(&root, "app.crs", "pub let w : /std/Str = \"a\";\n");
    let refusal = "a program closes with one term, the description it performs, and this text ends before one";

    let checked = curios(&root, &["check", "app.crs"], "");
    assert_eq!(checked.status.code(), Some(1));
    let answered = stdout(&checked);
    assert!(answered.contains(refusal), "{answered}");
    assert!(answered.contains("app.crs:2:1"), "{answered}");

    let ran = curios(&root, &["run", "app"], "");
    let stopped = String::from_utf8_lossy(&ran.stderr);
    assert_eq!(ran.status.code(), Some(1), "{stopped}");
    assert!(stopped.contains(refusal), "{stopped}");
}

/// A file written as a module whose stem is no name a module can take can be mounted nowhere, so it is refused where it is admitted, for its stem, by every command alike: the check, a test run and a question in its scope, none of which answers it as anything else. The same text under a stem that is a name is checked as a module and reports nothing.
#[test]
fn a_module_file_whose_stem_is_no_name_is_refused_for_it() {
    let root = temporary("stemless");
    fs::create_dir_all(&root).unwrap();
    let module = "use /std/{Nat};\n\npub let double(n: Nat) -> Nat =>\n    n * 2;\n";
    write(&root, "my-file.crs", module);
    write(&root, "myfile.crs", module);

    for command in [
        &["check", "my-file.crs"][..],
        &["test", "my-file.crs"],
        &["wonder", "--name", "/std/Nat/succ", "my-file.crs"],
    ] {
        let refused = curios(&root, command, "");
        let said = String::from_utf8_lossy(&refused.stderr);
        assert_eq!(refused.status.code(), Some(1), "{command:?}: {said}");
        assert!(refused.stdout.is_empty(), "{command:?}: {said}");
        assert!(
            said.starts_with("unnameable module\n│  my-file.crs is written as a module, with no final term\n│  its stem `my-file` is no name a module can take"),
            "{command:?}: {said}"
        );
    }

    let named = stdout(&curios(&root, &["check", "myfile.crs"], ""));
    assert_eq!(named, "");
}
