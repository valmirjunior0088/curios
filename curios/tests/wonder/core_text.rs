//! `curios wonder` over Curios/Core: a `.crsc` checked as a program is, standard input read as Curios/Core under `--core`, and every refusal placed in the text.

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

/// The module `wonder --stage core-elab` prints for a program reads back as Curios/Core and checks clean: its declarations under the certifier, and the term it closes with at the type the dump states for it, `Io({})`, judged as it is written.
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
        let checked = curios(&root, &["check", &file], "");
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

    let checked = curios(&root, &["check", "proof.crsc"], "");
    let (stdout, stderr, code) = said(&checked);
    assert_eq!(code, Some(1), "{stderr}");
    assert!(stdout.contains("certifier refusal"), "{stdout}");
    assert!(stdout.contains("proof.crsc:2:5"), "{stdout}");
}

/// Standard input is Curios/Core under `--core`, read as a `.crsc` is, its refusals placed in `<stdin>`; without the flag the same text is the program it is not, and beside a file the flag is refused, the file's name saying what it is written in.
#[test]
fn core_reads_standard_input_as_a_crsc_is_read() {
    let root = temporary("stdin-core");
    write(&root, "proof.crsc", "0\n");

    let checked = curios(&root, &["check", "--core", "-"], "0\n");
    let (stdout, stderr, code) = said(&checked);
    assert_eq!(code, Some(1), "{stderr}");
    assert!(stdout.contains("certifier refusal"), "{stdout}");
    assert!(stdout.contains("<stdin>:1:1"), "{stdout}");

    let as_a_program = curios(&root, &["check", "-"], "let /x : #NatType = 0;\n0\n");
    let (stdout, _, _) = said(&as_a_program);
    assert!(stdout.contains("syntax error"), "{stdout}");

    let beside_a_file = curios(&root, &["check", "--core", "proof.crsc"], "");
    let (_, stderr, code) = said(&beside_a_file);
    assert_eq!(code, Some(1), "{stderr}");
    assert!(stderr.contains("surplus flag"), "{stderr}");
}

/// A program in `root` as `p.crs`, and the module `--stage core-elab` prints for it as `p.crsc`: a declaration and a closing term reaching it.
fn dumped(root: &Path) {
    write(
        root,
        "p.crs",
        "use /std/{Nat, print};\n\nlet double(n: Nat) -> Nat = n + n;\n\nprint(Nat/to_str(double(21)))\n",
    );
    let (dump, stderr, code) = said(&curios(
        root,
        &["wonder", "--stage", "core-elab", "p.crs"],
        "",
    ));
    assert_eq!(code, Some(0), "{stderr}");
    write(root, "p.crsc", &dump);
}

/// A program read from Curios/Core has every rung from `core-elab` on, each printed as the program the text was dumped from prints it, and the two rungs a lowering makes are refused, Curios/Core being lowered by nothing.
#[test]
fn a_dumped_programs_rungs_are_printed_from_its_core_text_as_from_the_program() {
    let root = temporary("rungs");
    fs::create_dir_all(&root).unwrap();
    dumped(&root);

    for rung in ["anf", "cps-optm", "wasm"] {
        let (program, stderr, code) =
            said(&curios(&root, &["wonder", "--stage", rung, "p.crs"], ""));
        assert_eq!(code, Some(0), "{rung}: {stderr}");
        let (text, stderr, code) = said(&curios(&root, &["wonder", "--stage", rung, "p.crsc"], ""));
        assert_eq!(code, Some(0), "{rung}: {stderr}");
        assert_eq!(text, program, "{rung}");
    }

    for rung in ["text", "core"] {
        let (_, stderr, code) = said(&curios(&root, &["wonder", "--stage", rung, "p.crsc"], ""));
        assert_eq!(code, Some(1), "{rung}: {stderr}");
        assert!(stderr.contains("lowered by nothing"), "{rung}: {stderr}");
    }
}

/// A module of Curios/Core is a unit, reprinted at `core-elab` — the one rung a unit of Curios/Core has — as the loose module it was dumped from prints there; a lowering's rungs are refused, and a rung past a unit's refuses it as written as a module.
#[test]
fn a_module_of_core_text_is_reprinted_at_core_elab_as_the_module_it_was_dumped_from() {
    let root = temporary("module-rungs");
    fs::create_dir_all(&root).unwrap();
    write(
        &root,
        "double.crs",
        "pub let double(n: /std/Nat) -> /std/Nat =\n    n + n;\n",
    );
    let rung =
        |name: &str, file: &str| said(&curios(&root, &["wonder", "--stage", name, file], ""));

    let (dump, stderr, code) = rung("core-elab", "double.crs");
    assert_eq!(code, Some(0), "{stderr}");
    write(&root, "double.crsc", &dump);

    let (reprinted, stderr, code) = rung("core-elab", "double.crsc");
    assert_eq!(code, Some(0), "{stderr}");
    assert_eq!(reprinted, dump);

    let (_, stderr, code) = rung("text", "double.crsc");
    assert_eq!(code, Some(1), "{stderr}");
    assert!(stderr.contains("lowered by nothing"), "{stderr}");

    let (_, stderr, code) = rung("anf", "double.crsc");
    assert_eq!(code, Some(1), "{stderr}");
    assert!(stderr.starts_with("not a program\n"), "{stderr}");
}

/// A declaration a text writes is answered off the unit it reads to, placed where the text writes it, as is its part at `core-elab` and the footprint of its functions; a name of the prelude is answered as the prelude's scope answers it; and a question answered off a lowering is refused, naming the cause.
#[test]
fn a_declaration_a_text_writes_is_answered_off_the_unit_it_reads_to() {
    let root = temporary("named");
    fs::create_dir_all(&root).unwrap();
    dumped(&root);

    let (named, stderr, code) = said(&curios(
        &root,
        &["wonder", "--name", "/double", "p.crsc"],
        "",
    ));
    assert_eq!(code, Some(0), "{stderr}");
    assert!(named.contains("/double"), "{named}");
    assert!(named.contains("definition"), "{named}");
    assert!(named.contains("total"), "{named}");
    assert!(named.contains("p.crsc:1:5"), "{named}");

    let (part, stderr, code) = said(&curios(
        &root,
        &[
            "wonder",
            "--name",
            "/double",
            "--stage",
            "core-elab",
            "p.crsc",
        ],
        "",
    ));
    assert_eq!(code, Some(0), "{stderr}");
    assert!(part.contains("let /double"), "{part}");

    let (footprint, stderr, code) = said(&curios(
        &root,
        &["wonder", "--name", "/double", "--footprint", "p.crsc"],
        "",
    ));
    assert_eq!(code, Some(0), "{stderr}");
    assert!(
        footprint.lines().any(|line| line.contains('\t')),
        "{footprint}"
    );

    let (prelude, stderr, code) = said(&curios(
        &root,
        &["wonder", "--name", "/std/List/map", "p.crsc"],
        "",
    ));
    assert_eq!(code, Some(0), "{stderr}");
    assert!(prelude.contains("/std/List/map"), "{prelude}");

    let (_, stderr, code) = said(&curios(&root, &["wonder", "--at", "p.crsc:1:5"], ""));
    assert_eq!(code, Some(1), "{stderr}");
    assert!(stderr.contains("reads no Curios/Core"), "{stderr}");
}

/// The tests a module of Curios/Core writes are listed as the unit it reads to holds them.
#[test]
fn the_tests_a_text_writes_are_listed() {
    let root = temporary("tests");
    write(
        &root,
        "t.crsc",
        "test /t/held = /std/Test/assert(/std/Str/Str { 0x[0x69], /sys/Bool/True/qed() }, true);\n",
    );

    let (listed, stderr, code) = said(&curios(&root, &["wonder", "--tests", "t.crsc"], ""));
    assert_eq!(code, Some(0), "{stderr}");
    assert_eq!(listed, "/t/held\n");
}

/// A closing term stating a type other than the one a check puts it at is refused by the reader where the text states it, before any checker reads it.
#[test]
fn a_closing_term_stating_another_type_is_refused_where_it_states_it() {
    let root = temporary("stated-type");
    write(&root, "proof.crsc", "0\n: /sys/Bool/False\n");

    let checked = curios(&root, &["check", "proof.crsc"], "");
    let (stdout, stderr, code) = said(&checked);
    assert_eq!(code, Some(1), "{stderr}");
    assert!(stdout.contains("put at"), "{stdout}");
    assert!(stdout.contains("proof.crsc:2:3"), "{stdout}");
}
