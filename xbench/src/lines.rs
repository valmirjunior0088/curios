//! The lines a measurement prints for its record: one reading a line, under a prefix nothing else a test prints opens with, so the runner reads them off whatever else the test writes.
//!
//! A line is `xbench: <unit> <size> <value> <name>`, the size `-` where the figure is taken at none, and the name last, since it alone may hold a space.

#[cfg(test)]
mod tests;

use crate::Unit;

/// What every line of a record opens with.
pub const PREFIX: &str = "xbench:";

/// One reading of one figure, as a line carries it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Line {
    pub name: String,
    pub unit: Unit,
    pub size: Option<u64>,
    pub value: u64,
}

impl Line {
    /// The line as a measurement prints it.
    pub fn printed(&self) -> String {
        let size = match self.size {
            Some(size) => size.to_string(),
            None => "-".to_string(),
        };

        format!(
            "{PREFIX} {} {size} {} {}",
            self.unit.name(),
            self.value,
            self.name
        )
    }

    /// The reading `text` carries, where it opens with [`PREFIX`]: `None` for every other line a test prints, and an error for one that opens with it and reads as no reading.
    pub fn read(text: &str) -> Option<Result<Line, String>> {
        let rest = text.strip_prefix(PREFIX)?;

        Some(parsed(rest).ok_or_else(|| format!("`{text}` reads as no reading of a figure")))
    }
}

/// The reading the words after the prefix state, where they state one.
fn parsed(rest: &str) -> Option<Line> {
    let mut words = rest.trim_start().splitn(4, ' ');
    let unit = Unit::named(words.next()?)?;
    let size = match words.next()? {
        "-" => None,
        size => Some(size.parse().ok()?),
    };
    let value = words.next()?.parse().ok()?;
    let name = words.next().filter(|name| !name.is_empty())?.to_string();

    Some(Line {
        name,
        unit,
        size,
        value,
    })
}
