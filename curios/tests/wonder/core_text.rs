//! `curios wonder` over Core text: a `.crsc` checked as a program is, standard input read as Core text under `--core`, and every refusal placed in the text.

use {
    super::test_support::*,
    std::{fs, path::Path},
};

/// What a run said on standard output and standard error, and its exit code.
fn said(output: &std::process::Output) -> (String, String, Option<i32>) {
    (
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
        output.status.code(),
    )
}

/// The module `wonder --stage core-elab` prints for a program reads back as Core text and checks clean: its declarations under the certifier, and the term it closes with at the type the dump states for it, `Io({})`, judged as it is written.
#[test]
fn the_module_core_elab_prints_for_a_program_reads_back_and_checks_clean() {
    let root = temporary("dump-read-back");
    fs::create_dir_all(&root).unwrap();
    let programs = Path::new(env!("CARGO_MANIFEST_DIR")).join("../programs");

    for program in [
        "dependent_vectors",
        "monad_result",
        "parse_digits",
        "state_monad",
    ] {
        let path = programs.join(format!("{program}.crs"));
        let dumped = curios(
            &root,
            &["wonder", "--stage", "core-elab", path.to_str().unwrap()],
            "",
        );
        let (dump, stderr, code) = said(&dumped);
        assert_eq!(code, Some(0), "{program}: {stderr}");

        let file = format!("{program}.crsc");
        write(&root, &file, &dump);
        let checked = curios(&root, &["wonder", &file], "");
        let (stdout, stderr, code) = said(&checked);
        assert_eq!(code, Some(0), "{program}: {stderr}");
        assert!(!stdout.contains("[error]"), "{program}:\n{stdout}");
    }
}

/// A declaration the certifier refuses is reported where the text writes its name, in the certifier's own words.
#[test]
fn a_declaration_the_certifier_refuses_is_reported_where_its_text_writes_it() {
    let root = temporary("refused-in-place");
    write(
        &root,
        "proof.crsc",
        "let /proof/one : #NatType = 1;\nlet /proof/bad : #NatType = #BoolType;\n",
    );

    let checked = curios(&root, &["wonder", "proof.crsc"], "");
    let (stdout, stderr, code) = said(&checked);
    assert_eq!(code, Some(0), "{stderr}");
    assert!(stdout.contains("certifier refusal"), "{stdout}");
    assert!(stdout.contains("proof.crsc:2:5"), "{stdout}");
}

/// Standard input is Core text under `--core`, read as a `.crsc` is, its refusals placed in `<stdin>`; without the flag the same text is the program it is not, and beside a file the flag is refused, the file's name saying what it is written in.
#[test]
fn core_reads_standard_input_as_a_crsc_is_read() {
    let root = temporary("stdin-core");
    write(&root, "proof.crsc", "0\n");

    let checked = curios(&root, &["wonder", "--core", "-"], "0\n");
    let (stdout, stderr, code) = said(&checked);
    assert_eq!(code, Some(0), "{stderr}");
    assert!(stdout.contains("certifier refusal"), "{stdout}");
    assert!(stdout.contains("<stdin>:1:1"), "{stdout}");

    let as_a_program = curios(&root, &["wonder", "-"], "let /x : #NatType = 0;\n0\n");
    let (stdout, _, _) = said(&as_a_program);
    assert!(stdout.contains("syntax error"), "{stdout}");

    let beside_a_file = curios(&root, &["wonder", "--core", "proof.crsc"], "");
    let (_, stderr, code) = said(&beside_a_file);
    assert_eq!(code, Some(1), "{stderr}");
    assert!(stderr.contains("surplus flag"), "{stderr}");
}

/// A closing term stating a type other than the one a check puts it at is refused by the reader where the text states it, before any checker reads it.
#[test]
fn a_closing_term_stating_another_type_is_refused_where_it_states_it() {
    let root = temporary("stated-type");
    write(&root, "proof.crsc", "0\n: /sys/Bool/False\n");

    let checked = curios(&root, &["wonder", "proof.crsc"], "");
    let (stdout, stderr, code) = said(&checked);
    assert_eq!(code, Some(0), "{stderr}");
    assert!(stdout.contains("put at"), "{stdout}");
    assert!(stdout.contains("proof.crsc:2:3"), "{stdout}");
}
