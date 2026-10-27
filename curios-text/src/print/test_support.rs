//! What the printing suites share: a term rendered at a width.

use {
    super::print_term,
    crate::Term,
    curios_print::{Printer, run_printer_within},
    std::{cell::Cell, fmt},
};

/// Parse `source` as a term and render it within `width` at the formatter's indent step.
pub fn render(source: &str, width: usize) -> String {
    let term: Term = source.parse().expect("fixture parses");
    struct Within(Cell<Option<(Printer, usize)>>);
    impl fmt::Display for Within {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let (printer, width) = self.0.take().expect("rendered once");
            run_printer_within(printer, formatter, 4, width)
        }
    }
    Within(Cell::new(Some((print_term(term), width)))).to_string()
}
