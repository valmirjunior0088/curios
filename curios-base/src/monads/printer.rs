use std::fmt::{Error, Formatter, Write};

struct PrinterState<'a, 'b> {
    formatter: &'a mut Formatter<'b>,
    indent_step: usize,
    indent_by: usize,
    should_indent: bool,
}

impl<'a, 'b> PrinterState<'a, 'b> {
    fn new(formatter: &'a mut Formatter<'b>, indent_step: usize) -> Self {
        Self {
            formatter,
            indent_step,
            indent_by: 0,
            should_indent: true,
        }
    }

    fn write(mut self, string: &str) -> Result<Self, Error> {
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

        Ok(self)
    }

    fn indent<F>(self, f: F) -> Result<Self, Error>
    where
        F: FnOnce(Self) -> Result<Self, Error>,
    {
        let state = f(Self {
            indent_by: self.indent_by + self.indent_step,
            ..self
        })?;

        Ok(Self {
            indent_by: state.indent_by - state.indent_step,
            ..state
        })
    }
}

type PrinterResult<'a, 'b> = Result<PrinterState<'a, 'b>, Error>;

type PrinterInner<'a> =
    Box<dyn for<'b, 'c> FnOnce(PrinterState<'b, 'c>) -> PrinterResult<'b, 'c> + 'a>;

/// A deferred, single-use printing action: a boxed `FnOnce` threading indentation state over a `std::fmt::Formatter`. Built compositionally from [`pure`], [`flat`], [`sep_flat`], and [`indent`], and executed exactly once by [`run_printer`]; the inner closure is higher-ranked over the formatter lifetimes so a `Printer` can be built long before anyone knows which formatter it will write to.
pub struct Printer<'a>(PrinterInner<'a>);

impl<'a> Printer<'a> {
    fn new<F>(f: F) -> Self
    where
        F: for<'b, 'c> FnOnce(PrinterState<'b, 'c>) -> PrinterResult<'b, 'c> + 'a,
    {
        Printer(Box::new(f))
    }

    fn print<'b, 'c>(self, state: PrinterState<'b, 'c>) -> PrinterResult<'b, 'c> {
        (self.0)(state)
    }
}

/// The entry point: executes a printer against `formatter`, with `indent_step` spaces added per [`indent`] level. Typically the entire body of a `Display::fmt` impl — every IR crate's `print.rs` builds a [`Printer`] tree and hands it here.
pub fn run_printer<'a, 'b, 'c>(
    printer: Printer<'a>,
    formatter: &'b mut Formatter<'c>,
    indent_step: usize,
) -> Result<(), Error> {
    printer.print(PrinterState::new(formatter, indent_step))?;

    Ok(())
}

/// Emits a literal string. Not a raw write: any newline it contains arms the pending-indentation logic, so multi-line literals indent correctly under [`indent`].
pub fn pure<'a, A>(a: A) -> Printer<'a>
where
    A: Into<String> + 'a,
{
    Printer::new(move |state| state.write(&a.into()))
}

/// Concatenates a sequence of printers in order — the workhorse sequencing combinator; pretty-printers are mostly nested `flat(...)` of [`pure`] literals and recursive pieces.
pub fn flat<'a, I>(i: I) -> Printer<'a>
where
    I: IntoIterator<Item = Printer<'a>> + 'a,
{
    Printer::new(move |mut state| {
        for printer in i {
            state = printer.print(state)?;
        }

        Ok(state)
    })
}

/// Like [`flat`] but interposes a separator between adjacent items; an empty sequence prints nothing, and no separator trails. The separator comes from a closure rather than a value because [`Printer`] is single-use — each gap needs a fresh instance.
pub fn sep_flat<'a, I, F>(i: I, mut f: F) -> Printer<'a>
where
    I: IntoIterator<Item = Printer<'a>> + 'a,
    F: FnMut() -> Printer<'a> + 'a,
{
    Printer::new(move |mut state| {
        let mut iterator = i.into_iter();

        let printer = match iterator.next() {
            Some(printer) => printer,
            None => return Ok(state),
        };

        state = printer.print(state)?;

        for printer in iterator {
            state = printer.print(f().print(state)?)?;
        }

        Ok(state)
    })
}

/// Runs the printer one indentation level deeper: every line *begun* inside it gets `indent_step` extra leading spaces, applied lazily at the first character after each newline so blank lines stay blank. The level is restored when the printer finishes.
pub fn indent<'a>(printer: Printer<'a>) -> Printer<'a> {
    Printer::new(move |state| state.indent(|state| printer.print(state)))
}
