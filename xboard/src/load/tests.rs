//! What the loader reads off a board's records, and every word of them it holds.

use {super::*, std::collections::BTreeMap};

/// The directory a ticket titled "A forged proof" is filed in.
const DIRECTORY: &str = "a_forged_proof";

/// A ticket's record, with a witness of each form.
const RECORD: &str = r#"
    title = "A forged proof"
    found = 2026-10-02
    status = "open"

    [[witness]]
    what = "a program"
    source = "a_program.crs"
    refused = [{ by = "elaborator", error = "TypeMismatch", within = "/f" }]

    [[witness]]
    what = "a module"
    source = "a_module.crsc"
    refused = [{ by = "kernel", error = "Mismatch", within = "entry" }, { by = "reader" }]
"#;

/// The ticket `record` states, read with a text for each of the record's witnesses and any `others` beside them.
fn read(record: &str, others: &[(&str, &str)]) -> Result<Ticket, String> {
    let mut files = BTreeMap::from([
        (METADATA, record),
        ("a_program.crs", "/std/Bool/True/qed()"),
        ("a_module.crsc", "0"),
    ]);
    files.extend(others.iter().copied());

    load_ticket(DIRECTORY, &files)
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
        (Checker::Kernel, Some("Mismatch"), &Some(Within::Entry))
    );
    assert_eq!(
        (module.expect[1].by, module.expect[1].error),
        (Checker::Reader, None)
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
        ("admission/a_forged_proof/metadata.toml", RECORD),
        (
            "admission/a_forged_proof/a_program.crs",
            "/std/Bool/True/qed()",
        ),
        ("admission/a_forged_proof/a_module.crsc", "0"),
    ];
    let parts = load_board(files).expect("the board reads");

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

    assert!(refusal(&record).contains("files the ticket as `a_forged_proof_of_false`"));
}

#[test]
fn a_status_is_open_or_fixed() {
    assert!(refusal(&RECORD.replace(r#""open""#, r#""closed""#)).contains("closed"));
}

#[test]
fn a_day_the_calendar_does_not_have_is_refused() {
    assert!(read(&RECORD.replace("2026-10-02", "2026-02-30"), &[]).is_err());
    assert!(refusal(&RECORD.replace("2026-10-02", "2026-10-02T12:00:00")).contains("no day"));
}

#[test]
fn an_error_its_checker_does_not_raise_is_refused() {
    let record = RECORD.replace(r#"error = "Mismatch""#, r#"error = "Banana""#);

    assert!(refusal(&record).contains("the kernel raises no error named `Banana`"));
}

#[test]
fn a_refusal_names_a_checker_its_witness_is_put_to() {
    let record = RECORD.replace(r#"{ by = "reader" }"#, r#"{ by = "elaborator" }"#);

    assert!(refusal(&record).contains("a module is not put to the elaborator"));
}

#[test]
fn a_refusal_stands_at_the_entry_or_in_a_declaration_by_its_absolute_path() {
    assert!(refusal(&RECORD.replace(r#""/f""#, r#""f""#)).contains("`f` is neither `entry`"));
    assert!(
        refusal(&RECORD.replace(
            r#"{ by = "reader" }"#,
            r#"{ by = "reader", within = "/f" }"#
        ))
        .contains("stands in no declaration")
    );
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
        load_board(files)
            .expect_err("a part with no record is refused")
            .contains("a part's directory holds its `metadata.toml`")
    );
}
