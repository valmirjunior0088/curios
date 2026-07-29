use std::{
    fmt::{self, Write},
    mem,
};

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
/// No derives: `Debug`, `Clone`, and `PartialEq` would each walk the tree
/// recursively and overflow on the documents this type exists to print.
pub enum Printer {
    /// Literal text. Newlines inside it arm the pending-indent logic, so a
    /// multi-line literal indents correctly under [`indent`].
    Text(String),
    /// Emitted in order.
    Concat(Vec<Printer>),
    /// Emitted one indentation level deeper.
    Indent(Box<Printer>),
    /// A document not built yet.
    ///
    /// The one closure variant, and it earns its place: a printer for a
    /// recursive IR is written as a recursive function, so *building* a
    /// document descends as deep as the term even though [`run_printer`] no
    /// longer does. Deferring a child turns that descent into work on the
    /// interpreter's stack — the builder is called when the interpreter reaches
    /// it, from a frame one deep rather than `n`.
    ///
    /// `Option` so the interpreter can take the thunk out: a type with a `Drop`
    /// impl cannot have a field moved away.
    Deferred(Option<Box<dyn FnOnce() -> Printer>>),
}

impl Printer {
    /// This document's children, taken out and left childless.
    ///
    /// Shared by [`run_printer`] and [`Drop`], which both need to descend
    /// without moving a field out of a type that has a destructor.
    fn take(&mut self) -> Printer {
        mem::replace(self, Printer::Text(String::new()))
    }

    /// Whether this node's children are already gone, so dropping it cannot
    /// reach another node.
    fn is_dismantled(&self) -> bool {
        match self {
            Printer::Text(_) => true,
            Printer::Concat(parts) => parts.is_empty(),
            Printer::Indent(inner) => matches!(**inner, Printer::Text(_)),
            // Holds a thunk, never a child document.
            Printer::Deferred(_) => true,
        }
    }
}

/// Dismantled with an explicit stack, for the reason the type exists.
///
/// A document nests as deep as the term it prints, and the *derived* drop
/// recurses one native frame per level — so a document deep enough to need an
/// iterative [`run_printer`] would abort while being freed instead. Measured:
/// a 100k-deep document overflows a default stack on drop alone.
impl Drop for Printer {
    fn drop(&mut self) {
        // The base case is "already dismantled", not "is a leaf", and the
        // difference is not cosmetic: taking a node's children leaves a husk
        // that is still a `Concat` or an `Indent`, so a check for `Text` alone
        // sends every husk back through here to make another husk, forever.
        // The regression below catches that at depth ten.
        if self.is_dismantled() {
            return;
        }

        let mut pending = Vec::from([self.take()]);

        while let Some(mut printer) = pending.pop() {
            match &mut printer {
                Printer::Text(_) => {}
                Printer::Concat(parts) => pending.extend(mem::take(parts)),
                Printer::Indent(inner) => pending.push(inner.take()),
                Printer::Deferred(_) => {}
            }
            // `printer` is dismantled now, so its own drop returns at once.
        }
    }
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
        // Children are taken out rather than moved out: `Printer` has a `Drop`
        // impl, so its fields cannot be moved away. What is left behind is
        // childless and costs nothing to drop at the end of the arm.
        match step {
            Step::Print(mut printer) => match &mut printer {
                Printer::Text(text) => state.write(text)?,
                // Reversed, because the stack pops last-in first.
                Printer::Concat(parts) => {
                    stack.extend(mem::take(parts).into_iter().rev().map(Step::Print));
                }
                Printer::Indent(inner) => {
                    state.indent_by += state.indent_step;
                    stack.push(Step::Dedent);
                    stack.push(Step::Print(inner.take()));
                }
                Printer::Deferred(thunk) => {
                    if let Some(thunk) = thunk.take() {
                        stack.push(Step::Print(thunk()));
                    }
                }
            },
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

/// Defers building a document until the interpreter reaches it.
///
/// Wrap a printer's *recursive* calls in this and building stops descending:
/// each child is built from the interpreter's frame rather than from inside its
/// parent's, so a document nests as deep as the term without the builder doing
/// the same. See [`Printer::Deferred`].
pub fn deferred<F>(f: F) -> Printer
where
    F: FnOnce() -> Printer + 'static,
{
    Printer::Deferred(Some(Box::new(f)))
}

/// Runs the printer one indentation level deeper: every line *begun* inside it
/// gets `indent_step` extra leading spaces, applied lazily at the first
/// character after each newline so blank lines stay blank. The level is
/// restored when the printer finishes.
pub fn indent(printer: Printer) -> Printer {
    Printer::Indent(Box::new(printer))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A document nests as deep as the term it prints, and both walks over it
    /// — printing and freeing — must survive that. Depth is not steps, so no
    /// reduction budget bounds either one; only an explicit stack does.
    fn nested(depth: usize) -> Printer {
        let mut document = pure("x");
        for _ in 0..depth {
            document = indent(flat([pure("("), document, pure(")")]));
        }
        document
    }

    #[test]
    fn a_deep_document_is_freed_without_recursing() {
        drop(nested(100_000));
    }

    #[test]
    fn a_deep_document_is_printed_without_recursing() {
        struct Deep;
        impl fmt::Display for Deep {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                run_printer(nested(100_000), formatter, 2)
            }
        }

        let printed = Deep.to_string();

        assert_eq!(printed.matches('(').count(), 100_000);
        assert_eq!(printed.matches(')').count(), 100_000);
    }
}
