//! Where a ticket stands for what its witnesses answered.

use {
    super::*,
    crate::{Date, Proof, Refusal, refused},
    curios_cert::Verdict,
};

const FORGED: Witness = Witness {
    what: "the forged proof",
    proof: Proof::Program(""),
    expect: refused![kernel: Error::UntypedEntry],
};

const TICKET: Ticket = Ticket {
    title: "A forged proof",
    found_on: Date::new(2026, 1, 1),
    status: Status::Open,
    witnesses: &[FORGED],
};

const PART: Part = Part {
    name: "a part",
    tickets: &[],
};

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
            &[FORGED],
            &[by_kernel(curios_cert::Error::UntypedEntry)]
        ),
        Standing::Holds
    );

    assert_eq!(
        standing(Status::Fixed, &[FORGED], &[admitted()]),
        Standing::Regressed(vec!["the forged proof: admitted".to_string()])
    );
}

#[test]
fn a_refusal_holds_only_by_the_error_the_witness_names() {
    let Standing::Regressed(problems) = standing(
        Status::Fixed,
        &[FORGED],
        &[by_kernel(curios_cert::Error::UnclosedUniverses)],
    ) else {
        panic!("a witness refused by another error holds nothing shut");
    };

    assert!(problems[0].contains("not by the kernel's `Error::UntypedEntry`"));
    assert!(problems[0].contains("kernel `UnclosedUniverses`"));
}

#[test]
fn a_refusal_expected_of_the_kernel_is_not_met_by_the_elaborators() {
    let Standing::Regressed(problems) = standing(
        Status::Fixed,
        &[FORGED],
        &[by_elaborator(curios_elab::Error::Poisoned)],
    ) else {
        panic!("the elaborator's refusal stood in for the kernel's");
    };

    assert!(problems[0].contains("which was not asked"));
}

#[test]
fn a_witness_naming_both_checkers_holds_only_where_both_refuse() {
    let both = Witness {
        expect: refused![elaborator: Error::Poisoned, kernel: Error::UntypedEntry],
        ..FORGED
    };
    let kernel = by_kernel(curios_cert::Error::UntypedEntry);

    assert!(matches!(
        standing(Status::Fixed, &[both], &[kernel]),
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
        standing(Status::Fixed, &[both], &[refused_by_both]),
        Standing::Holds
    );
}

#[test]
fn an_elaborators_error_is_named_beneath_what_located_it() {
    let named = Witness {
        expect: refused![elaborator: Error::Poisoned],
        ..FORGED
    };
    let located = curios_elab::Error::InDeclaration {
        name: "forged".to_string(),
        owner: None,
        error: Box::new(curios_elab::Error::Poisoned),
    };

    assert_eq!(
        standing(Status::Fixed, &[named], &[by_elaborator(located)]),
        Standing::Holds
    );
}

#[test]
fn a_refusal_no_error_names_yet_holds_by_any_error_of_its_checker() {
    let unnamed = Witness {
        expect: refused![kernel],
        ..FORGED
    };

    assert_eq!(
        standing(
            Status::Fixed,
            &[unnamed],
            &[by_kernel(curios_cert::Error::UnclosedUniverses)]
        ),
        Standing::Holds
    );
    assert!(matches!(
        standing(
            Status::Fixed,
            &[unnamed],
            &[by_elaborator(curios_elab::Error::Poisoned)]
        ),
        Standing::Regressed(_)
    ));
}

#[test]
fn an_open_ticket_whose_witnesses_are_refused_was_shut_without_saying_so() {
    assert_eq!(
        standing(Status::Open, &[FORGED], &[admitted()]),
        Standing::Open
    );
    assert_eq!(
        standing(
            Status::Open,
            &[FORGED],
            &[by_kernel(curios_cert::Error::UntypedEntry)]
        ),
        Standing::Shut
    );

    assert!(Standing::Shut.fails());
    assert!(!Standing::Open.fails());
}

#[test]
fn a_ticket_with_no_witness_holds() {
    assert_eq!(standing(Status::Fixed, &[], &[]), Standing::Holds);
}

#[test]
fn only_a_witness_the_kernel_judged_carries_a_ticket_to_it() {
    let row = |forged: Answer| Row {
        part: &PART,
        ticket: &TICKET,
        answers: vec![forged],
        standing: Standing::Open,
    };

    assert!(!row(by_elaborator(curios_elab::Error::Poisoned)).reaches_kernel());
    assert!(row(by_kernel(curios_cert::Error::UntypedEntry)).reaches_kernel());
}

/// A whole ticket, as a board file writes one. Its term proves `True`, so the checkers refuse it at `False`, as they refuse the witness of a flaw that is fixed.
static EXAMPLE: Part = Part {
    name: "conversion",
    tickets: &[Ticket {
        title: "A proof of `True` passes for one of `False`",
        found_on: Date::new(2026, 10, 2),
        status: Status::Fixed,
        witnesses: &[Witness {
            what: "the proof of `True`",
            proof: Proof::Program("/std/Bool/True/qed()"),
            expect: refused![elaborator: Error::TypeMismatch { .. }],
        }],
    }],
};

#[test]
fn a_run_puts_every_witness_and_says_where_its_ticket_stands() {
    static BOARD: &[&Part] = &[&EXAMPLE];

    let report = Report::run(BOARD);

    assert!(!report.fails());
    assert_eq!(report.rows[0].standing, Standing::Holds);

    let printed = report.to_string();
    assert!(printed.contains("xboard: 1 ticket(s) — 1 holding"));
    assert!(printed.contains("HOLDS      2026-10-02  A proof of `True` passes for one of `False`"));
    assert!(printed.contains(
        "program  the proof of `True` — refused by the elaborator's `Error::TypeMismatch { .. }` [elaborator `TypeMismatch`]"
    ));
    assert!(printed.contains("conversion — 1 ticket(s), the last found 2026-10-02"));
    assert!(printed.contains("not put to the kernel: 1"));
}
