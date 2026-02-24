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

pub struct Printer<'a>(
    Box<dyn for<'b, 'c> FnOnce(PrinterState<'b, 'c>) -> PrinterResult<'b, 'c> + 'a>,
);

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

pub fn run_printer<'a, 'b, 'c>(
    printer: Printer<'a>,
    formatter: &'b mut Formatter<'c>,
    indent_step: usize,
) -> Result<(), Error> {
    printer.print(PrinterState::new(formatter, indent_step))?;

    Ok(())
}

pub fn pure<'a, A>(a: A) -> Printer<'a>
where
    A: Into<String> + 'a,
{
    Printer::new(move |state| state.write(&a.into()))
}

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

pub fn indent<'a>(printer: Printer<'a>) -> Printer<'a> {
    Printer::new(move |state| state.indent(|state| printer.print(state)))
}
