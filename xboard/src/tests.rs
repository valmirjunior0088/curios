//! The board, held by the suite as `cargo xboard report` holds it.

use {super::*, curios_common::test_support::Temporary};

/// The suite fails where `cargo xboard report` would: a record the loader refuses, or a ticket that does not stand.
#[test]
fn the_board_holds() {
    let parts = load_board(TICKETS.iter().copied()).unwrap_or_else(|why| panic!("{why}"));
    let report = Report::run(&parts);

    assert!(!report.fails(), "\n{report}");
}

/// A candidate is read as what its name says it is, and a name that says none of the three is refused.
#[test]
fn a_candidate_is_read_as_what_its_name_says_it_is() {
    let directory = Temporary::new("xboard", "candidate");
    fs::create_dir_all(&*directory).expect("the directory is made");
    for file in ["a.crs", "a.crsc", "a.txt"] {
        fs::write(directory.join(file), "0").expect("the candidate is written");
    }

    assert!(matches!(
        candidate_at(&directory.join("a.crs")),
        Ok(Candidate::Proof(Proof::Program(text))) if text == "0"
    ));
    assert!(matches!(
        candidate_at(&directory.join("a.crsc")),
        Ok(Candidate::Proof(Proof::Module(text))) if text == "0"
    ));
    assert!(
        candidate_at(&directory.join("a.txt"))
            .is_err_and(|why| why.contains("a candidate is a program"))
    );
}

/// A ticket is probed by its record, its witnesses' texts read from the directory beside it and its part from the directory above.
#[test]
fn a_ticket_is_probed_by_its_record_and_the_texts_beside_it() {
    let root = Temporary::new("xboard", "ticket");
    let directory = root.join("formation").join("a_forged_proof");
    fs::create_dir_all(&directory).expect("the directory is made");
    fs::write(
        directory.join(METADATA),
        "title = \"A forged proof\"\nfound = 2026-10-02\nstatus = \"open\"\n\n[[witness]]\nwhat = \"a program\"\nsource = \"a_program.crs\"\nrefused = [{ by = \"elaborator\" }]\n",
    )
    .expect("the record is written");
    fs::write(directory.join("a_program.crs"), "/std/Bool/True/qed()")
        .expect("the text is written");

    let Ok(Candidate::Ticket { part, ticket }) = candidate_at(&directory.join(METADATA)) else {
        panic!("the ticket reads");
    };
    assert_eq!(part, "formation");
    assert!(
        matches!(&ticket.witnesses[..], [witness] if matches!(&witness.proof, Proof::Program(text) if text == "/std/Bool/True/qed()"))
    );
}
