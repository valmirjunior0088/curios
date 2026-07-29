use std::fmt::{self, Write};

struct PrinterState<'a, 'b> {
    formatter: &'a mut fmt::Formatter<'b>,
    indent_step: usize,
    indent_by: usize,
    should_indent: bool,
}

impl<'a, 'b> PrinterState<'a, 'b> {
    fn new(formatter: &'a mut fmt::Formatter<'b>, indent_step: usize) -> Self {
        Self {
            formatter,
            indent_step,
            indent_by: 0,
            should_indent: true,
        }
    }

    fn write(&mut self, string: &str) -> Result<(), fmt::Error> {
        for char in string.chars() {
            if self.should_indent {
                for _ in 0..self.indent_by {
                    self.formatter.write_str(" ")?;
                }

                self.should_indent = false;
            }

            self.formatter.write_char(char)?;

            if char == '\n' {
                self.should_indent = true;
            }
        }

        Ok(())
    }
}

/// A pretty-printing document: what to emit, as data.
///
/// # Why this is an enum and not a closure
///
/// It used to be a `Box<dyn FnOnce(PrinterState) -> PrinterState>`, composed by
/// nesting closures. That made a document a *tree of closures*, and running one
/// called each child inside its parent's stack frame — so printing recursed as
/// deep as the document nested, and a deep enough term aborted the compiler
/// instead of printing. A diagnostic that cannot be printed is worse than no
/// diagnostic, and no reduction budget can prevent it, because depth is not
/// steps.
///
/// As data, the document is walked by [`run_printer`]'s explicit stack and
/// nests without bound. Every IR crate's `Display` gets that at once. The
/// combinators below keep the signatures they had, so the printers built on
/// them are unchanged.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Printer {
    /// Literal text. Newlines inside it arm the pending-indent logic, so a
    /// multi-line literal indents correctly under [`indent`].
    Text(String),
    /// Emitted in order.
    Concat(Vec<Printer>),
    /// Emitted one indentation level deeper.
    Indent(Box<Printer>),
}

/// One entry of [`run_printer`]'s work stack.
///
/// `Dedent` is what replaces the closure nesting that used to restore the
/// indentation level on the way out: pushed under a document, it runs after it.
enum Step {
    Print(Printer),
    Dedent,
}

/// The entry point: executes a printer against `formatter`, with `indent_step`
/// spaces added per [`indent`] level. Typically the entire body of a
/// `Display::fmt` impl — every IR crate's `print.rs` builds a [`Printer`] and
/// hands it here.
///
/// Iterative by construction: the work stack holds what is left to emit, so a
/// document's nesting costs heap rather than native frames.
pub fn run_printer<'b, 'c>(
    printer: Printer,
    formatter: &'b mut fmt::Formatter<'c>,
    indent_step: usize,
) -> Result<(), fmt::Error> {
    let mut state = PrinterState::new(formatter, indent_step);
    let mut stack = Vec::from([Step::Print(printer)]);

    while let Some(step) = stack.pop() {
        match step {
            Step::Print(Printer::Text(text)) => state.write(&text)?,
            // Reversed, because the stack pops last-in first.
            Step::Print(Printer::Concat(parts)) => {
                stack.extend(parts.into_iter().rev().map(Step::Print));
            }
            Step::Print(Printer::Indent(inner)) => {
                state.indent_by += state.indent_step;
                stack.push(Step::Dedent);
                stack.push(Step::Print(*inner));
            }
            Step::Dedent => state.indent_by -= state.indent_step,
        }
    }

    Ok(())
}

/// Emits a literal string. Not a raw write: any newline it contains arms the
/// pending-indentation logic, so multi-line literals indent correctly under
/// [`indent`].
pub fn pure<A>(a: A) -> Printer
where
    A: Into<String>,
{
    Printer::Text(a.into())
}

/// Concatenates a sequence of printers in order — the workhorse sequencing
/// combinator; pretty-printers are mostly nested `flat(...)` of [`pure`]
/// literals and recursive pieces.
pub fn flat<I>(i: I) -> Printer
where
    I: IntoIterator<Item = Printer>,
{
    Printer::Concat(i.into_iter().collect())
}

/// Like [`flat`] but interposes a separator between adjacent items; an empty
/// sequence prints nothing, and no separator trails.
///
/// The separator still comes from a closure rather than a value, which a
/// document made of data no longer strictly needs — it is kept so the call
/// sites do not change. It is now called while the document is built rather
/// than while it is printed, which is the same thing for a separator that takes
/// no arguments and can therefore depend on nothing that printing decides.
pub fn sep_flat<I, F>(i: I, mut f: F) -> Printer
where
    I: IntoIterator<Item = Printer>,
    F: FnMut() -> Printer,
{
    let mut parts = Vec::new();
    let mut iterator = i.into_iter();

    if let Some(first) = iterator.next() {
        parts.push(first);

        for printer in iterator {
            parts.push(f());
            parts.push(printer);
        }
    }

    Printer::Concat(parts)
}

/// Runs the printer one indentation level deeper: every line *begun* inside it
/// gets `indent_step` extra leading spaces, applied lazily at the first
/// character after each newline so blank lines stay blank. The level is
/// restored when the printer finishes.
pub fn indent(printer: Printer) -> Printer {
    Printer::Indent(Box::new(printer))
}
