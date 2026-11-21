use super::*;

#[test]
fn an_outcome_reads_back_the_failures_a_derived_spell_writes() {
    let answer = br#"[Failure { steps = ["case 2"], statement = "not equal", facts = [("expected", "\"a\""), ("obtained", "\"a\\n\"")] }]"#;

    assert_eq!(
        Outcome::read(answer),
        Ok(Outcome {
            failures: vec![FailedCheck {
                steps: vec!["case 2".to_string()],
                statement: "not equal".to_string(),
                facts: vec![
                    ("expected".to_string(), "\"a\"".to_string()),
                    ("obtained".to_string(), "\"a\\n\"".to_string()),
                ],
            }],
        })
    );
}

#[test]
fn an_empty_list_is_a_test_that_held() {
    assert!(Outcome::read(b"[]").is_ok_and(|outcome| outcome.held()));
}

#[test]
fn an_answer_that_spells_no_outcome_says_what_it_spells() {
    for (answer, why) in [
        (&b""[..], "the answer is no term"),
        (b"true", "`true` is no list"),
        (b"[1]", "`1` is no failed check"),
        (
            br#"[Failure { steps = [], statement = "x" }]"#,
            "has no `facts`",
        ),
    ] {
        let refusal = Outcome::read(answer).expect_err("no outcome");

        assert!(refusal.contains(why), "{refusal}");
    }
}
