//! Where a ticket stands for what its witnesses answered.

use {
    super::*, crate::*, curios_cert::Verdict, curios_common::Qualifier, curios_core::Global,
    std::slice,
};

/// The refusal by `by`, by the error named `error` where one is, standing `within`.
fn refused(by: Checker, error: Option<&'static str>, within: Option<Within>) -> Expected {
    Expected { by, error, within }
}

/// A witness the kernel is expected to refuse by `UntypedEntry`.
fn forged() -> Witness {
    Witness {
        what: "the forged proof".to_string(),
        steps: Vec::new(),
        proof: Proof::Program(String::new()),
        expect: vec![refused(Checker::Kernel, Some("UntypedEntry"), None)],
    }
}

fn admitted() -> Answer {
    Answer::Judged(Vec::new())
}

/// The kernel's refusal of the entry by `error`.
fn by_kernel(error: curios_cert::Error) -> Answer {
    Answer::Judged(vec![Refusal::Kernel(Verdict { name: None, error })])
}

/// The elaborator's refusal by `error`, while it was still building the module.
fn by_elaborator(error: curios_elab::Error) -> Answer {
    Answer::Unbuilt(Refusal::Elaborator {
        error,
        reported: "a diagnostic".to_string(),
    })
}

#[test]
fn a_fixed_ticket_holds_while_its_witness_is_refused_and_regresses_when_it_is_admitted() {
    assert_eq!(
        standing(
            Status::Fixed,
            &[forged()],
            &[by_kernel(curios_cert::Error::UntypedEntry)]
        ),
        Standing::Holds
    );

    assert_eq!(
        standing(Status::Fixed, &[forged()], &[admitted()]),
        Standing::Regressed(vec!["the forged proof: admitted".to_string()])
    );
}

#[test]
fn a_refusal_holds_only_by_the_error_the_witness_names() {
    let Standing::Regressed(problems) = standing(
        Status::Fixed,
        &[forged()],
        &[by_kernel(curios_cert::Error::UnclosedUniverses)],
    ) else {
        panic!("a witness refused by another error holds nothing shut");
    };

    assert!(problems[0].contains("not by the kernel's `UntypedEntry`"));
    assert!(problems[0].contains("kernel `UnclosedUniverses`"));
}

#[test]
fn a_refusal_expected_of_the_kernel_is_not_met_by_the_elaborators() {
    let Standing::Regressed(problems) = standing(
        Status::Fixed,
        &[forged()],
        &[by_elaborator(curios_elab::Error::Poisoned)],
    ) else {
        panic!("the elaborator's refusal stood in for the kernel's");
    };

    assert!(problems[0].contains("which was not asked"));
}

#[test]
fn a_witness_naming_both_checkers_holds_only_where_both_refuse() {
    let both = Witness {
        expect: vec![
            refused(Checker::Elaborator, Some("Poisoned"), None),
            refused(Checker::Kernel, Some("UntypedEntry"), None),
        ],
        ..forged()
    };
    let kernel = by_kernel(curios_cert::Error::UntypedEntry);

    assert!(matches!(
        standing(Status::Fixed, slice::from_ref(&both), &[kernel]),
        Standing::Regressed(_)
    ));

    let refused_by_both = Answer::Judged(vec![
        Refusal::Elaborator {
            error: curios_elab::Error::Poisoned,
            reported: "a diagnostic".to_string(),
        },
        Refusal::Kernel(Verdict {
            name: None,
            error: curios_cert::Error::UntypedEntry,
        }),
    ]);

    assert_eq!(
        standing(Status::Fixed, slice::from_ref(&both), &[refused_by_both]),
        Standing::Holds
    );
}

#[test]
fn an_elaborators_error_is_named_beneath_what_located_it() {
    let named = Witness {
        expect: vec![refused(Checker::Elaborator, Some("Poisoned"), None)],
        ..forged()
    };
    let located = curios_elab::Error::InDeclaration {
        names: Vec::new(),
        error: Box::new(curios_elab::Error::Poisoned),
    };

    assert_eq!(
        standing(Status::Fixed, &[named], &[by_elaborator(located)]),
        Standing::Holds
    );
}

// A mismatch is every conversion's refusal, so a witness names where its flaw's construction stands: the error it expects raised in another declaration holds nothing shut, and one at the closing term meets only an expectation placed there.
#[test]
fn a_refusal_holds_only_where_the_witness_places_it() {
    let declared = |name: &str| Global::Authored(Qualifier::from([name]));
    let in_declaration = |name: &str| curios_elab::Error::InDeclaration {
        names: vec![declared(name)],
        error: Box::new(curios_elab::Error::Poisoned),
    };
    let placed = Witness {
        expect: vec![refused(
            Checker::Elaborator,
            Some("Poisoned"),
            Some(Within::Declaration("/f".to_string())),
        )],
        ..forged()
    };

    assert_eq!(
        standing(
            Status::Fixed,
            slice::from_ref(&placed),
            &[by_elaborator(in_declaration("f"))]
        ),
        Standing::Holds
    );
    let Standing::Regressed(problems) = standing(
        Status::Fixed,
        slice::from_ref(&placed),
        &[by_elaborator(in_declaration("g"))],
    ) else {
        panic!("a refusal in another declaration held the witness shut");
    };
    assert!(problems[0].contains("`Poisoned` in `/g`"), "{problems:?}");

    let at_entry = Witness {
        expect: vec![refused(
            Checker::Kernel,
            Some("UntypedEntry"),
            Some(Within::Entry),
        )],
        ..forged()
    };
    let in_g = Answer::Judged(vec![Refusal::Kernel(Verdict {
        name: Some(declared("g")),
        error: curios_cert::Error::UntypedEntry,
    })]);

    assert_eq!(
        standing(
            Status::Fixed,
            slice::from_ref(&at_entry),
            &[by_kernel(curios_cert::Error::UntypedEntry)]
        ),
        Standing::Holds
    );
    assert!(matches!(
        standing(Status::Fixed, slice::from_ref(&at_entry), &[in_g]),
        Standing::Regressed(_)
    ));
}

#[test]
fn a_refusal_no_error_names_yet_holds_by_any_error_of_its_checker() {
    let unnamed = Witness {
        expect: vec![refused(Checker::Kernel, None, None)],
        ..forged()
    };

    assert_eq!(
        standing(
            Status::Fixed,
            slice::from_ref(&unnamed),
            &[by_kernel(curios_cert::Error::UnclosedUniverses)]
        ),
        Standing::Holds
    );
    assert!(matches!(
        standing(
            Status::Fixed,
            slice::from_ref(&unnamed),
            &[by_elaborator(curios_elab::Error::Poisoned)]
        ),
        Standing::Regressed(_)
    ));
}

#[test]
fn an_open_ticket_whose_witnesses_are_refused_was_shut_without_saying_so() {
    assert_eq!(
        standing(Status::Open, &[forged()], &[admitted()]),
        Standing::Open
    );
    assert_eq!(
        standing(
            Status::Open,
            &[forged()],
            &[by_kernel(curios_cert::Error::UntypedEntry)]
        ),
        Standing::Shut
    );

    assert!(Standing::Shut.fails());
    assert!(!Standing::Open.fails());
}

// An open ticket's witness refused by an error it does not expect shows no flaw, whatever its siblings show: one that no longer parses, or one a fix closed by another rule than the ticket names, fails the run as a shut ticket does.
#[test]
fn an_open_ticket_whose_witness_is_refused_otherwise_fails_the_run() {
    let Standing::Unshown(problems) = standing(
        Status::Open,
        &[forged()],
        &[by_kernel(curios_cert::Error::UnclosedUniverses)],
    ) else {
        panic!("a witness refused by another error stood as a flaw it no longer shows");
    };
    assert!(
        problems[0].contains("kernel `UnclosedUniverses`"),
        "{problems:?}"
    );
    assert!(Standing::Unshown(problems).fails());

    assert!(matches!(
        standing(
            Status::Open,
            &[forged(), forged()],
            &[admitted(), by_elaborator(curios_elab::Error::Poisoned)]
        ),
        Standing::Unshown(_)
    ));
}

#[test]
fn a_ticket_with_no_witness_holds() {
    assert_eq!(standing(Status::Fixed, &[], &[]), Standing::Holds);
}

#[test]
fn only_a_witness_the_kernel_judged_carries_a_ticket_to_it() {
    let ticket = Ticket {
        title: "A forged proof".to_string(),
        found_on: Date::new(2026, 1, 1).expect("a day the calendar has"),
        status: Status::Open,
        witnesses: vec![forged()],
    };
    let row = |forged: Answer| Row {
        part: "a part",
        ticket: &ticket,
        answers: vec![forged],
        standing: Standing::Open,
    };

    assert!(!row(by_elaborator(curios_elab::Error::Poisoned)).reaches_kernel());
    assert!(row(by_kernel(curios_cert::Error::UntypedEntry)).reaches_kernel());
}

/// A whole ticket, as its directory holds it: a part's record, the ticket's, and its witness's text. Its term proves `True`, so the checkers refuse it at `False`, as they refuse the witness of a flaw that is fixed.
const EXAMPLE: &[(&str, &str)] = &[
    (
        "conversion/metadata.toml",
        r#"description = "Are these two terms equal?""#,
    ),
    (
        "conversion/a_proof_of_true_passes_for_one_of_false/metadata.toml",
        r#"
            title = "A proof of `True` passes for one of `False`"
            found = 2026-10-02
            status = "fixed"

            [[witness]]
            what = "the proof of `True`"
            source = "the_proof_of_true.crs"
            refused = [{ by = "elaborator", error = "TypeMismatch" }]
        "#,
    ),
    (
        "conversion/a_proof_of_true_passes_for_one_of_false/the_proof_of_true.crs",
        "-- The term the run puts, which proves `True` and so nothing at `False`.\n/std/Bool/True/qed()\n",
    ),
];

#[test]
fn a_run_puts_every_witness_and_says_where_its_ticket_stands() {
    let parts = load_board(EXAMPLE.iter().copied()).expect("the example reads");
    let report = Report::run(&parts);

    assert!(!report.fails());
    assert_eq!(report.rows[0].standing, Standing::Holds);

    let printed = report.to_string();
    assert!(printed.contains("xboard: 1 ticket(s) — 1 holding"));
    assert!(printed.contains("HOLDS      2026-10-02  A proof of `True` passes for one of `False`"));
    assert!(printed.contains(
        "program  the proof of `True` — refused by the elaborator's `TypeMismatch` [elaborator `TypeMismatch` at the closing term]"
    ));
    assert!(printed.contains(
        "conversion — 1 ticket(s), the last found 2026-10-02\n    Are these two terms equal?"
    ));
    assert!(printed.contains("not put to the kernel: 1"));
}
