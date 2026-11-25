use super::*;

/// `date -I` answers a zero-padded day, and a record holds it as TOML's local date, which refuses a day its month does not have.
#[test]
fn the_day_date_answers_is_the_day_a_record_holds() {
    assert_eq!(
        today("2026-10-07").map(|day| day.to_string()),
        Ok("2026-10-07".to_string())
    );
    assert!(today("2026-02-30").is_err());
    assert!(today("yesterday").is_err());
}
