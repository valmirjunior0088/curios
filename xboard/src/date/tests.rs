//! Which days the calendar has, how they are ordered, and how one reads.

use super::*;

/// The day the calendar has at `year`-`month`-`day`.
fn day(year: u16, month: u8, day: u8) -> Date {
    Date::new(year, month, day).expect("the calendar has the day")
}

#[test]
fn a_day_reads_as_year_month_day_and_orders_as_the_calendar_does() {
    assert_eq!(day(2026, 8, 2).to_string(), "2026-08-02");

    assert!(day(2026, 7, 31) < day(2026, 8, 1));
    assert!(day(2025, 12, 31) < day(2026, 1, 1));
}

#[test]
fn the_leap_day_exists_in_a_leap_year() {
    assert_eq!(day(2028, 2, 29).to_string(), "2028-02-29");
}

#[test]
fn a_day_past_the_end_of_its_month_is_none() {
    assert_eq!(Date::new(2026, 2, 29), None);
    assert_eq!(Date::new(2026, 4, 31), None);
    assert_eq!(Date::new(2026, 1, 0), None);
}

#[test]
fn a_thirteenth_month_is_none() {
    assert_eq!(Date::new(2026, 13, 1), None);
}
