//! What the crate's suites share: the documents they lay out.

use {
    crate::*,
    std::{cell::Cell, fmt},
};

pub fn render(printer: Printer, width: Option<usize>) -> String {
    struct Render(Cell<Option<(Printer, Option<usize>)>>);
    impl fmt::Display for Render {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let (printer, width) = self.0.take().expect("rendered once");
            match width {
                Some(width) => run_printer_within(printer, formatter, 2, width),
                None => run_printer(printer, formatter, 2),
            }
        }
    }
    Render(Cell::new(Some((printer, width)))).to_string()
}
