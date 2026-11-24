use super::*;

#[test]
fn records_read_back_the_failures_a_derived_spell_writes_in_the_order_reported() {
    let first = br#"Failure { steps = ["case 2"], statement = "not equal", facts = [("expected", "\"a\""), ("obtained", "\"a\\n\"")] }"#;
    let second = br#"Failure { steps = [], statement = "no", facts = [] }"#;

    assert_eq!(
        Outcome::of(&[first.to_vec(), second.to_vec()]),
        Ok(Outcome {
            failures: vec![
                FailedCheck {
                    steps: vec!["case 2".to_string()],
                    statement: "not equal".to_string(),
                    facts: vec![
                        ("expected".to_string(), "\"a\"".to_string()),
                        ("obtained".to_string(), "\"a\\n\"".to_string()),
                    ],
                },
                FailedCheck {
                    steps: vec![],
                    statement: "no".to_string(),
                    facts: vec![],
                },
            ],
        })
    );
}

#[test]
fn no_record_is_a_test_that_held() {
    assert!(Outcome::of(&[]).is_ok_and(|outcome| outcome.held()));
}

#[test]
fn a_record_that_spells_no_failed_check_says_what_it_spells() {
    for (record, why) in [
        (&b""[..], "a record is no term"),
        (b"true", "`true` is no failed check"),
        (b"[]", "is no failed check"),
        (
            br#"Failure { steps = [], statement = "x" }"#,
            "has no `facts`",
        ),
    ] {
        let refusal = Outcome::of(&[record.to_vec()]).expect_err("no outcome");

        assert!(refusal.contains(why), "{refusal}");
    }
}
