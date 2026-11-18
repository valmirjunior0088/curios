//! `curios wonder` at the command line: a question answered on standard output, a file placed in its unit or checked alone, and the exit code each outcome takes.

use {
    super::test_support::*,
    std::{
        fs,
        io::Write,
        path::Path,
        process::{Command, Stdio},
    },
};

/// An answer is written for whoever reads it, so a reader that leaves early — `curios wonder --stage wasm | head` — ends the command as a filter ends, with the code a broken pipe gives and nothing said: a panic over the pipe says what no answer does.
#[test]
fn a_reader_that_leaves_early_ends_the_question_with_nothing_said() {
    let root = temporary("reader-gone");
    fs::create_dir_all(&root).unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .current_dir(&root)
        .args(["wonder", "--stage", "wasm", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run the compiler");
    // Gone before the program is so much as read, so the rung's first line already has no reader.
    drop(child.stdout.take());
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"/std/print(\"\")\n")
        .unwrap();

    let ended = child.wait_with_output().expect("the compiler exits");
    let said = String::from_utf8_lossy(&ended.stderr);
    assert_eq!(ended.status.code(), Some(141), "{said}");
    assert!(!said.contains("panicked"), "{said}");
}

/// A goal on standard input is an answer: located, rendered as `run` reports it, on stdout, exit 0.
#[test]
fn a_goal_is_answered_on_stdout_with_exit_zero() {
    let root = temporary("stdin");
    fs::create_dir_all(&root).unwrap();

    let answered = curios(
        &root,
        &["wonder", "-"],
        "let m : /std/Nat = ?;\n/std/print(\"\")\n",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let text = stdout(&answered);
    assert!(text.contains("└─ [goal] <stdin>:1:20"), "{text}");
    // The check is narrated beside the answer, closed by what stopped it.
    let narrated = String::from_utf8_lossy(&answered.stderr);
    assert!(
        narrated.starts_with("<stdin>\n└─ check; 1 goal in "),
        "{narrated}"
    );
}

/// A text the formatter would write otherwise is said once, after what the compiler says of it, with the command that rewrites it, and stops nothing: standard input by its own name, and a canonical text not at all.
#[test]
fn a_text_the_formatter_would_rewrite_is_said_once_with_the_way_out() {
    let root = temporary("not-canonical-stdin");
    fs::create_dir_all(&root).unwrap();

    let answered = curios(
        &root,
        &["wonder", "-"],
        "let   unused = 1;\n/std/print(  \"\")\n",
    );

    assert!(answered.status.success());
    let text = stdout(&answered);
    assert!(
        text.ends_with(concat!(
            "└─ [lint] <stdin>:1:7\n",
            "\n",
            "not canonical\n",
            "│  the formatter would rewrite this text\n",
            "├─ try; curios format -\n",
            "└─ [format] <stdin>\n",
        )),
        "{text}"
    );

    let canonical = curios(&root, &["wonder", "-"], "/std/print(\"\")\n");
    assert_eq!(stdout(&canonical), "");
}

/// Every file a subject is written in is asked about, as `format` would rewrite it, and each is named as its reader spells it: a package's two files that are not canonical are two reports, and a file asked about alone is one.
#[test]
fn each_file_the_formatter_would_rewrite_is_a_report_of_its_own() {
    let root = project("not-canonical-files");
    write(&root, "lib.crs", "pub   mod util;\n");
    write(&root, "app.crs", "/std/print(  /app/util/word)\n");

    let entire = curios(&root, &["wonder"], "");
    assert!(entire.status.success());
    assert_eq!(
        stdout(&entire),
        concat!(
            "not canonical\n",
            "│  the formatter would rewrite this file\n",
            "├─ try; curios format lib.crs\n",
            "└─ [format] lib.crs\n",
            "\n",
            "not canonical\n",
            "│  the formatter would rewrite this file\n",
            "├─ try; curios format app.crs\n",
            "└─ [format] app.crs\n",
        )
    );

    let alone = curios(&root, &["wonder", "app.crs"], "");
    assert_eq!(
        stdout(&alone),
        concat!(
            "not canonical\n",
            "│  the formatter would rewrite this file\n",
            "├─ try; curios format app.crs\n",
            "└─ [format] app.crs\n",
        )
    );

    // `lint` reads what the compilation found, which a text's layout is no part of.
    let linted = curios(&root, &["lint"], "");
    assert_eq!(stdout(&linted), "");
}

/// `--manifest` takes the file as it is spelled, and a bare `curios.toml` names the one in the working directory: its parent is the empty path, which is no directory to resolve on its own.
#[test]
fn a_bare_manifest_override_names_the_working_directory() {
    let root = project("manifest-override");

    let answered = curios(&root, &["wonder", "--manifest", "curios.toml"], "");

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

    let executable = curios(&root, &["wonder", "app.crs"], "");
    assert!(executable.status.success());
    assert_eq!(stdout(&executable), "", "the executable sees its library");

    write(&root, "util.crs", "pub let word : /std/Str = 1;\n");
    let module = curios(&root, &["wonder", "util.crs"], "");
    assert!(module.status.success(), "an error is still an answer");
    let text = stdout(&module);
    assert!(text.contains("type mismatch"), "{text}");
    assert!(text.contains("util.crs:1:27"), "{text}");
}

/// A file under a package that no `mod` reaches is in no unit, so it is checked on its own, as a unit of its own where it is written as a module, with nothing said of that, on a cold store and on a warm one alike, since placement walks the headers and not the record of a compile. Declaring it puts it in the library.
#[test]
fn a_file_no_mod_declares_is_checked_on_its_own() {
    let root = project("undeclared");
    write(&root, "stray.crs", "pub let w : /std/Str = 1;\n");

    let cold = curios(&root, &["wonder", "stray.crs"], "");
    assert!(cold.status.success());
    let text = stdout(&cold);
    assert!(
        text.starts_with("type mismatch"),
        "checked on its own, and nothing said of it: {text}"
    );
    assert!(text.contains("stray.crs:1:24"), "{text}");

    // Warm: the library is served from the store, and placement still walks the headers.
    let built = curios(&root, &["run", "app"], "");
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    let warm = curios(&root, &["wonder", "stray.crs"], "");
    assert_eq!(stdout(&warm), text);

    write(&root, "lib.crs", "pub mod util;\npub mod stray;\n");
    let declared = curios(&root, &["wonder", "stray.crs"], "");
    assert!(
        stdout(&declared).contains("stray.crs:1:24"),
        "its own error, read as the library's: {}",
        stdout(&declared)
    );
}

/// Text on standard input written as a module is checked as one, as a loose file is — a unit of its own, mounted at `/stdin` since it has no stem — so its items are answered one by one; a stage needs a program, and refuses it as written as a module.
#[test]
fn a_module_on_standard_input_is_checked_as_a_unit_of_its_own() {
    let root = temporary("stdin-module");
    fs::create_dir_all(&root).unwrap();
    let module = "pub let w : /std/Str = 1;\n";

    let answered = curios(&root, &["wonder", "-"], module);
    assert!(answered.status.success());
    let text = stdout(&answered);
    assert!(text.contains("└─ [error] <stdin>:1:24"), "{text}");

    let staged = curios(&root, &["wonder", "--stage", "core", "-"], module);
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

/// A file written as a module whose stem is no name a module can take is mounted nowhere, so it is checked item by item by nothing: the answer says so, where the program's grammar refused its last line for a term it was never meant to hold. The same text under a stem that is a name is checked as a module and reports nothing.
#[test]
fn a_module_file_whose_stem_is_no_name_is_refused_for_it() {
    let root = temporary("stemless");
    fs::create_dir_all(&root).unwrap();
    let module = "use /std/{Nat};\n\npub let double(n: Nat) -> Nat =\n    n * 2;\n";
    write(&root, "my-file.crs", module);
    write(&root, "myfile.crs", module);

    let refused = stdout(&curios(&root, &["wonder", "my-file.crs"], ""));
    assert!(
        refused.contains(
            "my-file.crs is written as a module, with no final term\n│  its stem `my-file` is no name a module can take"
        ),
        "{refused}"
    );
    assert!(!refused.contains("expected a term"), "{refused}");

    let named = stdout(&curios(&root, &["wonder", "myfile.crs"], ""));
    assert_eq!(named, "");
}

/// A place answers the name written there on stdout as `--name` answers a declaration, a block — its path, what it is declared as, and where its name is written — then the prose and head its author wrote.
#[test]
fn a_place_answers_the_name_written_there() {
    let root = project("at");
    write(
        &root,
        "util.crs",
        "--- The word a run prints.\npub let word : /std/Str = \"placed\";\n",
    );

    let answered = curios(&root, &["wonder", "--at", "1:20", "app.crs"], "");

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let answer = stdout(&answered);
    assert_eq!(
        answer,
        concat!(
            "/app/util/word\n",
            "│  public definition\n",
            "└─ [declaration] util.crs:2:9\n",
            "\n",
            "--- The word a run prints.\n",
            "pub let word: /std/Str\n",
        )
    );

    // Asked again, of the library the first question filed: a unit restored from the store states its declarations as one compiled now does.
    let again = curios(&root, &["wonder", "--at", "1:20", "app.crs"], "");
    assert_eq!(stdout(&again), answer);
}

/// A place in a library's file answers what is written there whatever the store holds of the library: here the check of the package entire files the library, beside an executable, before the place is asked.
#[test]
fn a_place_answers_over_a_library_a_check_filed() {
    let root = project("at-filed");
    let checked = curios(&root, &["wonder"], "");
    assert!(
        checked.status.success(),
        "{}",
        String::from_utf8_lossy(&checked.stderr)
    );

    let answered = curios(&root, &["wonder", "--at", "1:9", "util.crs"], "");

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let answer = stdout(&answered);
    assert!(answer.starts_with("/app/util/word\n"), "{answer:?}");
}

/// A place in a file of the standard library's package answers what is written there: the package is lowered over the library the compiler carries, and what it keeps of that library's files lies in the files the package holds. Asked of a copy, so the tree's own package keeps no store.
#[test]
fn a_place_in_the_standard_librarys_package_answers() {
    fn copied(from: &Path, to: &Path) {
        fs::create_dir_all(to).unwrap();
        for entry in fs::read_dir(from).unwrap() {
            let entry = entry.unwrap();
            let name = entry.file_name();
            if name == ".curios" {
                continue;
            }
            match entry.file_type().unwrap().is_dir() {
                true => copied(&entry.path(), &to.join(&name)),
                false => {
                    fs::copy(entry.path(), to.join(&name)).unwrap();
                }
            }
        }
    }

    let root = temporary("at-std");
    copied(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("../curios-prelude/std"),
        &root,
    );
    let text = fs::read_to_string(root.join("Bool.crs")).unwrap();
    let line = 1 + text
        .lines()
        .position(|line| line.starts_with("pub let not("))
        .expect("the library declares `not`");

    let answered = curios(
        &root,
        &["wonder", "--at", &format!("{line}:9"), "Bool.crs"],
        "",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    let answer = stdout(&answered);
    assert!(answer.starts_with("/std/Bool/not\n"), "{answer:?}");
}

/// A place where nothing is named is an answer of nothing, and exits 0.
#[test]
fn a_place_naming_nothing_answers_nothing() {
    let root = project("nothing-named");

    // The `pub` a declaration opens with names nothing.
    let nothing = curios(&root, &["wonder", "--at", "1:1", "util.crs"], "");
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
        &["wonder", "--at", "2:21", "-"],
        "let one: /std/Nat = true;\nlet two: /std/Nat = one;\n/std/print(\"\")\n",
    );

    assert!(
        answered.status.success(),
        "{}",
        String::from_utf8_lossy(&answered.stderr)
    );
    assert_eq!(
        stdout(&answered),
        concat!(
            "/one\n",
            "│  private definition\n",
            "└─ [declaration] <stdin>:1:5\n",
            "\n",
            "let one: /std/Nat\n",
        )
    );
}

/// A stage that the program does not reach is not an answer: diagnostics on stderr, exit 1, stdout empty.
#[test]
fn an_unreached_stage_leaves_stdout_empty() {
    let root = temporary("unreached");
    fs::create_dir_all(&root).unwrap();

    let refused = curios(
        &root,
        &["wonder", "--stage", "wasm", "-"],
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
        &["wonder", "--stage", "wasm", "-"][..],
        &["wonder", "--cost", "-"],
        &["wonder", "--tests", "-"],
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

    let answered = curios(&root, &["wonder", "-"], program);
    assert_eq!(answered.status.code(), Some(0));
}
