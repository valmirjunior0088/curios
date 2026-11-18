//! What the loader reads off a board's records, and every word of them it holds.

use {super::*, std::collections::BTreeMap};

/// The directory a ticket titled "A forged proof", found on 2026-10-02, is filed in.
const DIRECTORY: &str = "2026_10_02_a_forged_proof";

/// A ticket's record, with a witness of each form.
const RECORD: &str = r#"
    title = "A forged proof"
    status = "open"

    [[witness]]
    what = "a program"
    source = "a_program.crs"
    refused = [{ by = "elaborator", error = "TypeMismatch", within = "/f" }]

    [[witness]]
    what = "a module"
    source = "a_module.crsc"
    refused = [{ by = "certifier", error = "Mismatch", within = "entry" }]
"#;

/// A ticket's record whose one witness is a history: a program mounted at `/lib`, then unmounted, and Core text read over what is left.
const HISTORY: &str = r#"
    title = "A forged proof"
    status = "open"

    [[witness]]
    what = "a history"
    refused = [{ by = "elaborator" }, { by = "certifier" }]

    [[witness.step]]
    mount = "/lib"
    source = "a_program.crs"

    [[witness.step]]
    unmount = "/lib"

    [[witness.step]]
    source = "a_module.crsc"
"#;

/// The ticket `record` states, read with a text for each of the record's witnesses and any `others` beside them.
fn read(record: &str, others: &[(&str, &str)]) -> Result<Ticket, String> {
    read_in(DIRECTORY, record, others)
}

/// The ticket `record` states, filed in `directory`, read as [`read`] reads one.
fn read_in(directory: &str, record: &str, others: &[(&str, &str)]) -> Result<Ticket, String> {
    let mut files = BTreeMap::from([
        (METADATA, record),
        ("a_program.crs", "/std/Bool/True/qed()"),
        ("a_module.crsc", "0"),
    ]);
    files.extend(others.iter().copied());

    load_ticket(directory, &files)
}

/// The refusal of `record`, which must be refused.
fn refusal(record: &str) -> String {
    read(record, &[]).expect_err("the record is refused")
}

#[test]
fn a_ticket_reads_from_its_record_and_its_witnesses_texts() {
    let ticket = read(RECORD, &[]).expect("the ticket reads");

    assert_eq!(ticket.title, "A forged proof");
    assert_eq!(ticket.found_on, Date::new(2026, 10, 2).expect("a day"));
    assert_eq!(ticket.status, Status::Open);

    let [program, module] = &ticket.witnesses[..] else {
        panic!("the ticket carries its two witnesses");
    };
    assert!(matches!(&program.proof, Proof::Program(text) if text == "/std/Bool/True/qed()"));
    assert!(matches!(&module.proof, Proof::Module(text) if text == "0"));
    assert_eq!(
        program.expect[0].within,
        Some(Within::Declaration("/f".to_string()))
    );
    assert_eq!(
        (
            module.expect[0].by,
            module.expect[0].error,
            &module.expect[0].within
        ),
        (Checker::Certifier, Some("Mismatch"), &Some(Within::Entry))
    );
}

#[test]
fn a_board_reads_its_parts_in_the_order_of_their_directories() {
    let files = [
        (
            "totality/metadata.toml",
            r#"description = "Does this end?""#,
        ),
        (
            "admission/metadata.toml",
            r#"description = "What is believed?""#,
        ),
        ("admission/2026_10_02_a_forged_proof/metadata.toml", RECORD),
        (
            "admission/2026_10_02_a_forged_proof/a_program.crs",
            "/std/Bool/True/qed()",
        ),
        ("admission/2026_10_02_a_forged_proof/a_module.crsc", "0"),
    ];
    let parts = load_board(files, []).expect("the board reads");

    let read = parts
        .iter()
        .map(|part| {
            (
                part.name.as_str(),
                part.description.as_str(),
                part.tickets.len(),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        read,
        [
            ("admission", "What is believed?", 1),
            ("totality", "Does this end?", 0)
        ]
    );
}

#[test]
fn a_directory_its_title_does_not_name_is_refused() {
    let record = RECORD.replace("A forged proof", "A forged proof of `False`");

    assert!(refusal(&record).contains("files the ticket as `2026_10_02_a_forged_proof_of_false`"));
}

#[test]
fn a_directory_that_does_not_open_with_its_day_is_refused() {
    let refused = |directory: &str| read_in(directory, RECORD, &[]).expect_err("refused");

    assert!(refused("a_forged_proof").contains("named for the day it was found"));
    assert!(refused("2026_10_2_a_forged_proof").contains("no day the calendar has"));
    assert!(refused("2026-10-02_a_forged_proof").contains("no day the calendar has"));
}

#[test]
fn a_record_stating_its_day_is_refused() {
    let record = RECORD.replace(
        r#"status = "open""#,
        "found = 2026-10-02\nstatus = \"open\"",
    );

    assert!(refusal(&record).contains("unknown field `found`"));
}

#[test]
fn a_status_is_open_or_fixed() {
    assert!(refusal(&RECORD.replace(r#""open""#, r#""closed""#)).contains("closed"));
}

#[test]
fn a_day_the_calendar_does_not_have_is_refused() {
    let refused = read_in("2026_02_30_a_forged_proof", RECORD, &[]).expect_err("refused");

    assert!(refused.contains("`2026_02_30` is no day the calendar has"));
}

#[test]
fn an_error_its_checker_does_not_raise_is_refused() {
    let record = RECORD.replace(r#"error = "Mismatch""#, r#"error = "Banana""#);

    assert!(refusal(&record).contains("the certifier raises no error named `Banana`"));
}

#[test]
fn a_refusal_names_a_checker_its_witness_is_put_to() {
    let record = RECORD.replace(
        r#"{ by = "certifier", error = "Mismatch", within = "entry" }"#,
        r#"{ by = "elaborator" }"#,
    );

    assert!(refusal(&record).contains("a module is not put to the elaborator"));
}

#[test]
fn a_refusal_stands_at_the_entry_or_in_a_declaration_by_its_absolute_path() {
    assert!(refusal(&RECORD.replace(r#""/f""#, r#""f""#)).contains("`f` is neither `entry`"));
}

/// The board judges by the two checkers, so a refusal by the reader of Core text is a word the loader does not know, refused as any unknown word is.
#[test]
fn a_refusal_is_by_the_elaborator_or_the_certifier() {
    let record = RECORD.replace(
        r#"within = "entry" }"#,
        r#"within = "entry" }, { by = "reader" }"#,
    );

    assert!(refusal(&record).contains("reader"), "{}", refusal(&record));
}

#[test]
fn every_file_of_a_ticket_is_put_by_a_witness() {
    let refused = read(RECORD, &[("a_draft.crs", "0")]).expect_err("a stray file is refused");

    assert!(refused.contains("a_draft.crs: no witness puts it"));
}

#[test]
fn a_witness_is_a_program_or_core_text() {
    let refused = read(
        &RECORD.replace("a_module.crsc", "a_module.txt"),
        &[("a_module.txt", "0")],
    )
    .expect_err("a text of another form is refused");

    assert!(refused.contains("neither a program"));
}

#[test]
fn a_field_no_record_states_is_refused() {
    assert!(
        refusal(&RECORD.replace("status = ", "fixed_by = \"me\"\nstatus = ")).contains("fixed_by")
    );
}

#[test]
fn a_part_holds_its_record() {
    let files = [("admission/a_forged_proof/metadata.toml", RECORD)];

    assert!(
        load_board(files, [])
            .expect_err("a part with no record is refused")
            .contains("a part's directory holds its `metadata.toml`")
    );
}

#[test]
fn a_history_reads_its_steps_and_closes_with_its_proof() {
    let ticket = read(HISTORY, &[]).expect("the ticket reads");
    let [history] = &ticket.witnesses[..] else {
        panic!("the ticket carries its witness");
    };

    assert!(matches!(
        &history.steps[..],
        [
            Step::Unit { mount, written: Written::Program, text },
            Step::Unmount { mount: unmounted },
        ] if mount == "lib" && text == "/std/Bool/True/qed()" && unmounted == "lib"
    ));
    assert!(matches!(&history.proof, Proof::Module(text) if text == "0"));
    assert_eq!(history.asked(), [Checker::Elaborator, Checker::Certifier]);
}

#[test]
fn a_witness_states_its_source_or_its_steps() {
    assert!(
        refusal(&HISTORY.replace(
            "what = \"a history\"",
            "what = \"a history\"\n    source = \"a_module.crsc\""
        ))
        .contains("its `source` or its steps, not both")
    );
    assert!(
        refusal(&RECORD.replace("source = \"a_program.crs\"", ""))
            .contains("states its `source` or its steps")
    );
}

#[test]
fn a_history_closes_with_its_proof_and_holds_more_than_it() {
    assert!(
        refusal(&HISTORY.replace(
            "[[witness.step]]\n    source = \"a_module.crsc\"",
            "[[witness.step]]\n    mount = \"/m\"\n    source = \"a_module.crsc\""
        ))
        .contains("closes with its proof")
    );

    let alone = HISTORY
        .split("    [[witness.step]]\n    mount")
        .next()
        .expect("the record opens with its witness")
        .to_string()
        + "    [[witness.step]]\n    source = \"a_module.crsc\"\n";
    assert!(
        read(&alone, &[("a_program.crs", "0")])
            .expect_err("a history of its proof alone is refused")
            .contains("is a witness's `source`")
    );
}

#[test]
fn a_unit_is_unmounted_once_a_step_mounts_it() {
    assert!(
        refusal(&HISTORY.replace("unmount = \"/lib\"", "unmount = \"/other\""))
            .contains("`/other` is unmounted where no step mounts it")
    );

    let twice = HISTORY.replace(
        "    [[witness.step]]\n    unmount = \"/lib\"",
        "    [[witness.step]]\n    unmount = \"/lib\"\n\n    [[witness.step]]\n    unmount = \"/lib\"",
    );
    assert!(refusal(&twice).contains("`/lib` is unmounted where no step mounts it"));
}

#[test]
fn a_unit_is_mounted_at_a_path_of_one_name() {
    assert!(
        refusal(&HISTORY.replace("mount = \"/lib\"", "mount = \"/lib/deep\""))
            .contains("`/lib/deep` is no path of one name")
    );
}

/// An archived ticket's record, its witness kept as it was filed: a history through a step no board takes, and a file no directory holds.
const ARCHIVED: &str = r#"
    title = "A forged proof"
    reason = "its construct is gone"

    [[witness]]
    what = "a history"
    step = [
        { mount = "/lib", source = "lib.crs" },
        { retired = "/lib" },
        { source = "proof.crsc" },
    ]
    refused = [{ by = "certifier", error = "Mismatch", within = "entry" }]
"#;

/// The archived ticket `record` states, read from its record alone.
fn archived(record: &str) -> Result<Archived, String> {
    load_archived(DIRECTORY, &BTreeMap::from([(METADATA, record)]))
}

/// The loader reads an archived record's title and reason and nothing else of it, so a witness kept as it was filed reads whatever words it was written in.
#[test]
fn an_archived_ticket_reads_its_title_and_reason_alone() {
    let archived = archived(ARCHIVED).expect("the archived ticket reads");

    assert_eq!(archived.title, "A forged proof");
    assert_eq!(archived.found_on, Date::new(2026, 10, 2).expect("a day"));
    assert_eq!(archived.reason, "its construct is gone");
}

/// A live record holds its own words, and the reason an archived one states is none of them.
#[test]
fn a_live_record_states_no_reason() {
    assert!(
        refusal(&RECORD.replace("status = ", "reason = \"gone\"\nstatus = "))
            .contains("unknown field `reason`")
    );
}

/// An archived ticket's directory is named for its day and its title, as a live one's is.
#[test]
fn an_archived_ticket_is_named_for_its_day_and_title() {
    assert!(
        archived(&ARCHIVED.replace("A forged proof", "Another proof"))
            .expect_err("refused")
            .contains("its title files the ticket as")
    );
}

#[test]
fn an_archived_record_states_its_reason() {
    assert!(
        archived(&ARCHIVED.replace("reason = \"its construct is gone\"\n", ""))
            .expect_err("refused")
            .contains("missing field `reason`")
    );
}

/// The archive files a ticket under a part the board has, after the part's own tickets.
#[test]
fn an_archived_ticket_is_filed_under_a_part_the_board_has() {
    let part = (
        "admission/metadata.toml",
        r#"description = "What is believed?""#,
    );
    let ticket = [
        (
            "admission/2026_10_02_a_forged_proof/metadata.toml",
            ARCHIVED,
        ),
        (
            "admission/2026_10_02_a_forged_proof/lib.crs",
            "/std/Bool/True/qed()",
        ),
    ];

    let parts = load_board([part], ticket).expect("the board reads");
    assert!(matches!(
        &parts[..],
        [admission] if admission.tickets.is_empty() && matches!(&admission.archived[..], [_])
    ));
    assert!(
        load_board([], ticket)
            .expect_err("a part the board does not have is refused")
            .contains("the board has no `admission`")
    );
}
