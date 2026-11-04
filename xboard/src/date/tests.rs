//! Which days the calendar has, how they are ordered, and how one reads.

use super::*;

#[test]
fn a_day_reads_as_year_month_day_and_orders_as_the_calendar_does() {
    assert_eq!(Date::new(2026, 8, 2).to_string(), "2026-08-02");

    assert!(Date::new(2026, 7, 31) < Date::new(2026, 8, 1));
    assert!(Date::new(2025, 12, 31) < Date::new(2026, 1, 1));
}

#[test]
fn the_leap_day_exists_in_a_leap_year() {
    assert_eq!(Date::new(2028, 2, 29).to_string(), "2028-02-29");
}

#[test]
#[should_panic(expected = "the month has no such day")]
fn a_day_past_the_end_of_its_month_is_refused() {
    let _ = Date::new(2026, 2, 29);
}

#[test]
#[should_panic(expected = "a month is one of the twelve")]
fn a_thirteenth_month_is_refused() {
    let _ = Date::new(2026, 13, 1);
}
