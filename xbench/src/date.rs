//! A calendar day, which is all a run says of when. The board keeps the same type for its tickets, `xboard/src/date.rs`: the two are one file, copied.

#[cfg(test)]
mod tests;

use std::fmt;

/// A day of the calendar. Its fields are in the order that makes the derived ordering the calendar's.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Date {
    year: u16,
    month: u8,
    day: u8,
}

impl Date {
    /// The day `year`-`month`-`day`.
    ///
    /// A day the calendar does not have panics, and a ticket is a constant, so one written with such a day does not compile.
    pub const fn new(year: u16, month: u8, day: u8) -> Date {
        let leap =
            year.is_multiple_of(4) && (!year.is_multiple_of(100) || year.is_multiple_of(400));

        let last = match month {
            1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
            4 | 6 | 9 | 11 => 30,
            2 if leap => 29,
            2 => 28,
            _ => panic!("a month is one of the twelve"),
        };

        assert!(day >= 1 && day <= last, "the month has no such day");

        Date { year, month, day }
    }
}

impl fmt::Display for Date {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{:04}-{:02}-{:02}",
            self.year, self.month, self.day
        )
    }
}
