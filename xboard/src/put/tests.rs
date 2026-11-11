//! What putting a term at a type the board states answers, through the real checkers.

use {
    super::*,
    crate::*,
    curios_core::{Intrinsic, Nat},
};

/// The `Nat` literal zero: a term with a type, and no proof of anything.
fn zero() -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))
}

#[test]
fn a_program_whose_tail_proves_something_else_is_refused_at_false() {
    let witness = Witness {
        what: "a proof of `True`".to_string(),
        steps: Vec::new(),
        proof: Proof::Program("/std/Bool/True/qed()".to_string()),
        expect: vec![Expected {
            by: Checker::Elaborator,
            error: Some("TypeMismatch"),
            within: None,
        }],
    };

    assert_eq!(witness.unmet(&witness.proof.answer()), None);
}

#[test]
fn a_module_is_admitted_at_a_type_its_term_has() {
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    let answer = History::new()
        .built(|scope| put_module(scope, Definitions::default(), zero(), nat_type))
        .expect("a history of no step builds");

    assert!(answer.admitted());
}

#[test]
fn a_module_whose_term_proves_nothing_is_refused_at_false() {
    let witness = Witness {
        what: "a number".to_string(),
        steps: Vec::new(),
        proof: Proof::Module("0".to_string()),
        expect: vec![Expected {
            by: Checker::Certifier,
            error: Some("Mismatch"),
            within: None,
        }],
    };

    assert_eq!(witness.unmet(&witness.proof.answer()), None);
}

/// A construction no type of the trusted base can hold still parses, and the reader refuses it by name, which a witness expects as it expects a checker's refusal.
#[test]
fn core_text_the_reader_refuses_is_refused_by_the_name_of_its_refusal() {
    let witness = Witness {
        what: "an intrinsic Core has no variant for".to_string(),
        steps: Vec::new(),
        proof: Proof::Module("#Banana".to_string()),
        expect: vec![Expected {
            by: Checker::Reader,
            error: Some("UnknownIntrinsic"),
            within: None,
        }],
    };
    let answer = witness.proof.answer();

    assert!(!answer.certifier_asked());
    assert_eq!(witness.unmet(&answer), None);
}

/// A text that does not parse has rotted, and no expectation meets it, so it fails whatever its witness expects.
#[test]
fn core_text_that_does_not_parse_is_refused_by_the_reader_with_the_certifier_not_asked() {
    let answer = Proof::Module("let".to_string()).answer();

    assert!(!answer.certifier_asked());
    assert!(matches!(answer.refusals(), [refusal] if refusal.by() == Some(Checker::Reader)));
}

#[test]
fn core_text_that_states_the_type_of_its_term_is_refused_by_the_reader() {
    let answer = Proof::Module("0\n: #NatType".to_string()).answer();

    assert!(!answer.certifier_asked());
    assert!(
        matches!(answer.refusals(), [Refusal::Unread(reported)] if reported.contains("states the type"))
    );
}

/// The module of the ticket found on 2026-11-02 cut in two, the carrier and the paradox closing with its term, the carrier's names beneath `/lib` where a history mounts it.
fn route() -> (String, String) {
    let (_, text) = TICKETS
        .iter()
        .find(|(path, _)| path.ends_with("/a_bare_group_read_under_the_readers_constraints.crsc"))
        .expect("the board holds the ticket of 2026-11-02");
    let text = text
        .replace("/Boxed", "/lib/Boxed")
        .replace("/carrier", "/lib/carrier");
    let at = text
        .find("#{universe_context = {parameter_count = 4, constraints = [u <= 0")
        .expect("the paradox follows the carrier");

    (text[..at].to_string(), text[at..].to_string())
}

/// The route of 2026-11-02 as a history: its carrier compiled as a unit of its own, then `withheld`, then the paradox read through the carrier as a later build mounts it.
fn carrier_read(withheld: &[Step], expect: Vec<Expected>) -> Witness {
    let (carrier, paradox) = route();
    let mut steps = vec![Step::Unit {
        mount: "lib".to_string(),
        written: Written::CoreText,
        text: carrier,
    }];
    steps.extend_from_slice(withheld);

    Witness {
        what: "the paradox read through a carrier compiled before it".to_string(),
        steps,
        proof: Proof::Module(paradox),
        expect,
    }
}

/// The certifier's refusal by `error`, standing `within`.
fn by_certifier(error: &'static str, within: Within) -> Expected {
    Expected {
        by: Checker::Certifier,
        error: Some(error),
        within: Some(within),
    }
}

/// The board's example of a history: the proof reads a unit an earlier step compiled and filed, and is refused where the ticket's fix refuses it, the carrier's totality granted from its record.
#[test]
fn a_history_puts_its_proof_over_the_units_its_steps_compiled() {
    let witness = carrier_read(
        &[],
        vec![by_certifier(
            "Mismatch",
            Within::Declaration("/paradox".to_string()),
        )],
    );
    let answer = witness.answer();

    assert_eq!(witness.unmet(&answer), None);
    assert!(
        !answer
            .refusals()
            .iter()
            .any(|refusal| refusal.named().contains("NotTotal"))
    );
}

/// The board's example of withholding: the carrier's record withheld whole, the build that reads it mounts the carrier with no verdict the certifier can adopt, so the paradox is refused as before and the totality it leaned on the carrier for is no longer granted.
#[test]
fn a_record_withheld_is_believed_of_nothing_by_a_later_build() {
    let witness = carrier_read(
        &[Step::Withhold {
            mount: "lib".to_string(),
            names: None,
        }],
        vec![
            by_certifier("Mismatch", Within::Declaration("/paradox".to_string())),
            by_certifier("NotTotal", Within::Entry),
        ],
    );

    assert_eq!(witness.unmet(&witness.answer()), None);
}

#[test]
fn a_step_the_reader_refuses_stops_the_history_and_says_where() {
    let answer = put(
        &[Step::Unit {
            mount: "lib".to_string(),
            written: Written::CoreText,
            text: "let".to_string(),
        }],
        &Proof::Module("0".to_string()),
    );

    assert!(!answer.certifier_asked());
    assert!(
        matches!(answer.refusals(), [Refusal::Unread(reported)] if reported.starts_with("in the step of `/lib`"))
    );
}

/// A program a build refuses is reported as text, so the board cannot tell its checker and no expectation meets it: a witness whose fix refuses an earlier unit writes it as Core text.
#[test]
fn a_program_step_refused_stops_the_history_by_no_checker() {
    let witness = Witness {
        what: "a library that does not elaborate".to_string(),
        steps: vec![Step::Unit {
            mount: "lib".to_string(),
            written: Written::Program,
            text: "pub let truth: /std/Bool/True = 0;\n".to_string(),
        }],
        proof: Proof::Module("0".to_string()),
        expect: vec![Expected {
            by: Checker::Elaborator,
            error: None,
            within: None,
        }],
    };
    let answer = witness.answer();

    assert!(
        matches!(answer.refusals(), [refusal @ Refusal::Stopped { mount, .. }] if mount == "lib" && refusal.by().is_none())
    );
    assert!(witness.unmet(&answer).is_some());
}
