//! `curios wonder` at the command line: a question answered on standard output, a file placed in its unit or checked alone, and the exit code each outcome takes.

use {
    super::test_support::{curios, project, stdout, temporary, write},
    std::{fs, process::Output},
};

/// A goal on standard input is an answer: located, rendered as `run` reports it, on stdout, exit 0.
#[test]
fn a_goal_is_answered_on_stdout_with_exit_zero() {
    let root = temporary("stdin");
    fs::create_dir_all(&root).unwrap();

    let answered = curios(
        &root,
        &["wonder", "diagnostics", "-"],
        "let m : /std/Nat = ?;\n/std/print(\"\")\n",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let text = stdout(&answered);
    assert!(text.starts_with("goal `?`"), "{text}");
    assert!(text.contains("--> <stdin>:1:20"), "{text}");
    assert!(answered.stderr.is_empty(), "nothing is narrated");
}

/// `--manifest` takes the file as it is spelled, and a bare `curios.toml` names the one in the working directory: its parent is the empty path, which is no directory to resolve on its own.
#[test]
fn a_bare_manifest_override_names_the_working_directory() {
    let root = project("manifest-override");

    let answered = curios(
        &root,
        &["wonder", "diagnostics", "--manifest", "curios.toml"],
        "",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    assert!(stdout(&answered).is_empty(), "{}", stdout(&answered));
}

/// A file is placed in the unit that declares it: the executable compiles against its library, so `/app/util/word` resolves — which it would not standalone — and a module of the library is checked as the library, reporting at the module's own path.
#[test]
fn a_file_is_placed_in_its_unit() {
    let root = project("placed");

    let executable = curios(&root, &["wonder", "diagnostics", "app.crs"], "");
    assert!(executable.status.success());
    assert_eq!(stdout(&executable), "", "the executable sees its library");

    write(&root, "util.crs", "pub let word : /std/Str = 1;\n");
    let module = curios(&root, &["wonder", "diagnostics", "util.crs"], "");
    assert!(module.status.success(), "an error is still an answer");
    let text = stdout(&module);
    assert!(text.contains("type mismatch"), "{text}");
    assert!(text.contains("util.crs:1:27"), "{text}");
}

/// Every question about a file its package holds and no unit declares says so first, and names what the file's form is declared by: the `mod` line for one written as a module, and the `[[executables]]` row for one written as a program, which a `mod` line would be refused over. Where the answer is a list of diagnostics the note opens it; where it is the tests, the rung or the fates on standard output, the note stands beside it on standard error.
#[test]
fn every_question_about_a_file_no_unit_declares_names_what_its_form_is_declared_by() {
    let root = project("undeclared-forms");
    write(&root, "items.crs", "pub let w : /std/Str = \"a\";\n");
    write(&root, "scratch.crs", "/std/print(\"x\")\n");
    let stderr = |output: &Output| String::from_utf8_lossy(&output.stderr).into_owned();

    for (file, remedy) in [
        ("items.crs", "declare it with `mod items;` in lib.crs"),
        (
            "scratch.crs",
            "an `[[executables]]` row in its manifest declares it",
        ),
    ] {
        let said = |taken: &str| {
            format!(
                "note: {file} is in no unit of `/app`, so it {taken} on its own against `/std`: {remedy}"
            )
        };

        for question in [&["wonder", "diagnostics", file][..], &["lint", file]] {
            let answered = stdout(&curios(&root, question, ""));
            assert!(
                answered.starts_with(&said("was checked")),
                "{question:?}: {answered}"
            );
        }

        let listed = stderr(&curios(&root, &["wonder", "tests", file], ""));
        assert!(listed.starts_with(&said("was checked")), "{listed}");

        for question in [
            &["wonder", "stage", "text", file][..],
            &["wonder", "cost", file],
        ] {
            let answered = stderr(&curios(&root, question, ""));
            assert!(
                answered.starts_with(&said("is compiled")),
                "{question:?}: {answered}"
            );
        }
    }
}

/// A file under a package that no `mod` reaches is in no unit, so it is checked on its own — written as a module, as a unit of its own — behind a note saying so and what would declare it, on a cold store and on a warm one alike, since placement walks the headers and not the record of a compile. Declaring it puts it in the library, and the note goes.
#[test]
fn a_file_no_mod_declares_is_checked_on_its_own_behind_a_note() {
    let root = project("undeclared");
    write(&root, "stray.crs", "pub let w : /std/Str = 1;\n");

    let cold = curios(&root, &["wonder", "diagnostics", "stray.crs"], "");
    assert!(cold.status.success());
    let text = stdout(&cold);
    assert!(
        text.starts_with("note: stray.crs is in no unit of `/app`"),
        "{text}"
    );
    assert!(
        text.contains("declare it with `mod stray;` in lib.crs"),
        "{text}"
    );
    assert!(
        !text.contains("stray.crs:1:1"),
        "a note is its message alone: {text}"
    );
    assert!(text.contains("type mismatch"), "checked on its own: {text}");

    // Warm: the library is served from the store, and placement still walks the headers.
    let built = curios(&root, &["run", "app"], "");
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    let warm = curios(&root, &["wonder", "diagnostics", "stray.crs"], "");
    assert!(
        stdout(&warm).contains("is in no unit of `/app`"),
        "{}",
        stdout(&warm)
    );

    write(&root, "lib.crs", "pub mod util;\npub mod stray;\n");
    let declared = curios(&root, &["wonder", "diagnostics", "stray.crs"], "");
    let text = stdout(&declared);
    assert!(!text.contains("is in no unit"), "{text}");
    assert!(
        text.contains("stray.crs:1:24"),
        "its own error, read as the library's: {text}"
    );
}

/// Text on standard input written as a module is checked as one, as a loose file is — a unit of its own, mounted at `/stdin` since it has no stem — so its items are answered one by one; a stage needs a program, and refuses it as written as a module.
#[test]
fn a_module_on_standard_input_is_checked_as_a_unit_of_its_own() {
    let root = temporary("stdin-module");
    fs::create_dir_all(&root).unwrap();
    let module = "pub let w : /std/Str = 1;\n";

    let answered = curios(&root, &["wonder", "diagnostics", "-"], module);
    assert!(answered.status.success());
    let text = stdout(&answered);
    assert!(text.contains("while elaborating w"), "{text}");
    assert!(text.contains("--> <stdin>:1:24"), "{text}");

    let staged = curios(&root, &["wonder", "stage", "core", "-"], module);
    let refusal = String::from_utf8_lossy(&staged.stderr);
    assert_eq!(staged.status.code(), Some(1), "{refusal}");
    assert!(staged.stdout.is_empty());
    assert!(
        refusal.contains(
            "<stdin> is written as a module, with no final term to compile a program from"
        ),
        "{refusal}"
    );
}

/// A file written as a module whose stem is no name a module can take is mounted nowhere, so it is checked item by item by nothing: the answer says so, where the program's grammar refused its last line for a term it was never meant to hold. The same text under a stem that is a name is checked as a module and reports nothing.
#[test]
fn a_module_file_whose_stem_is_no_name_is_refused_for_it() {
    let root = temporary("stemless");
    fs::create_dir_all(&root).unwrap();
    let module = "use /std/{Nat};\n\npub let double(n: Nat) -> Nat = n * 2;\n";
    write(&root, "my-file.crs", module);
    write(&root, "myfile.crs", module);

    let refused = stdout(&curios(
        &root,
        &["wonder", "diagnostics", "my-file.crs"],
        "",
    ));
    assert!(
        refused.contains(
            "my-file.crs is written as a module, with no final term, and its stem `my-file` is no name a module can take"
        ),
        "{refused}"
    );
    assert!(!refused.contains("expected a term"), "{refused}");

    let named = stdout(&curios(&root, &["wonder", "diagnostics", "myfile.crs"], ""));
    assert_eq!(named, "");
}

/// A place answers the name written there as one row on stdout: the declaration's path, what it is declared as, and where its name is written, a tab between each.
#[test]
fn a_place_answers_the_name_written_there() {
    let root = project("at");
    write(
        &root,
        "util.crs",
        "--- The word a run prints.\npub let word : /std/Str = \"placed\";\n",
    );

    let answered = curios(&root, &["wonder", "at", "1:20", "app.crs"], "");

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let answer = stdout(&answered);
    let (row, stated) = answer
        .split_once('\n')
        .expect("a row, then what follows it");
    let fields = row.split('\t').collect::<Vec<_>>();
    assert_eq!(fields[..2], ["/app/util/word", "definition"], "{answer}");
    assert!(fields[2].ends_with("util.crs:2:9"), "{answer}");
    // Beneath the row, the declaration's head as its author wrote it, and the prose above it.
    assert_eq!(
        stated,
        "\npub let word: /std/Str\n\nThe word a run prints.\n"
    );

    // Asked again, of the library the first question filed: a unit restored from the store states its declarations as one compiled now does.
    let again = curios(&root, &["wonder", "at", "1:20", "app.crs"], "");
    assert_eq!(stdout(&again), answer);
}

/// What a file declares is one row a definition, and a place where nothing is named is an answer of nothing: both exit 0.
#[test]
fn a_files_declarations_are_listed_and_a_place_naming_nothing_answers_nothing() {
    let root = project("declarations");

    let listed = curios(&root, &["wonder", "declarations", "util.crs"], "");
    assert!(
        listed.status.success(),
        "{}",
        String::from_utf8_lossy(&listed.stderr)
    );
    let rows = stdout(&listed);
    assert!(
        rows.starts_with("/app/util/word\tdefinition\t") && rows.ends_with("util.crs:1:9\n"),
        "{rows}"
    );
    assert_eq!(rows.lines().count(), 1, "{rows}");

    // The `pub` a declaration opens with names nothing.
    let nothing = curios(&root, &["wonder", "at", "1:1", "util.crs"], "");
    assert!(nothing.status.success());
    assert_eq!(stdout(&nothing), "");
}

/// A written name is resolved where it is lowered, so a program whose first declaration is a type mismatch still answers where a name is declared.
#[test]
fn a_program_that_does_not_check_answers_where_a_name_is_declared() {
    let root = temporary("at-refused");
    fs::create_dir_all(&root).unwrap();

    let answered = curios(
        &root,
        &["wonder", "at", "2:21", "-"],
        "let one: /std/Nat = true;\nlet two: /std/Nat = one;\n/std/print(\"\")\n",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    assert_eq!(
        stdout(&answered),
        "/one\tdefinition\t<stdin>:1:5\n\nlet one: /std/Nat\n"
    );
}

/// A stage that the program does not reach is not an answer: diagnostics on stderr, exit 1, stdout empty.
#[test]
fn an_unreached_stage_leaves_stdout_empty() {
    let root = temporary("unreached");
    fs::create_dir_all(&root).unwrap();

    let refused = curios(
        &root,
        &["wonder", "stage", "wasm", "-"],
        "let m : /std/Nat = true;\n/std/print(\"\")\n",
    );

    assert_eq!(refused.status.code(), Some(1));
    assert!(refused.stdout.is_empty());
    assert!(String::from_utf8_lossy(&refused.stderr).contains("type mismatch"));
}

/// A question stopped before it could answer exits as a build would, so written goals alone are the incomplete state that exits 2 — for `stage`, `cost` and `tests` alike — while `diagnostics`, whose answer the goals are, exits 0.
#[test]
fn a_question_stopped_by_goals_alone_exits_two() {
    let root = temporary("goals");
    fs::create_dir_all(&root).unwrap();
    let program = "let n : /std/Nat = ?;\n/std/print(\"\")\n";

    for query in [
        &["wonder", "stage", "wasm", "-"][..],
        &["wonder", "cost", "-"],
        &["wonder", "tests", "-"],
    ] {
        let stopped = curios(&root, query, program);
        assert_eq!(
            stopped.status.code(),
            Some(2),
            "{}: {}",
            query.join(" "),
            String::from_utf8_lossy(&stopped.stderr)
        );
        assert!(stopped.stdout.is_empty(), "{}", query.join(" "));
    }

    let answered = curios(&root, &["wonder", "diagnostics", "-"], program);
    assert_eq!(answered.status.code(), Some(0));
}
