//! The public builders: every document is made by composing these.

use super::Printer;

/// Emits a literal string. Not a raw write: any newline it contains arms the pending-indentation logic, so multi-line literals indent correctly under [`indent`](crate::indent).
pub fn pure<A>(a: A) -> Printer
where
    A: Into<String>,
{
    Printer::Text(a.into())
}

/// Concatenates a sequence of printers in order — the workhorse sequencing combinator; pretty-printers are mostly nested `flat(...)` of [`pure`] literals and recursive pieces.
pub fn flat<I>(i: I) -> Printer
where
    I: IntoIterator<Item = Printer>,
{
    Printer::Concat(i.into_iter().collect())
}

/// Like [`flat`] but interposes a separator between adjacent items; an empty sequence prints nothing, and no separator trails.
///
/// The separator still comes from a closure rather than a value, which a document made of data no longer strictly needs — it is kept so the call sites do not change. It is now called while the document is built rather than while it is printed, which is the same thing for a separator that takes no arguments and can therefore depend on nothing that printing decides.
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
/// Wrap a printer's *recursive* calls in this and building stops descending: each child is built from the interpreter's frame rather than from inside its parent's, so a document nests as deep as the term without the builder doing the same. See [`Printer::Deferred`].
pub fn deferred<F>(f: F) -> Printer
where
    F: FnOnce() -> Printer + 'static,
{
    Printer::Deferred(Some(Box::new(f)))
}

/// Runs the printer one indentation level deeper: every line *begun* inside it gets `indent_step` extra leading spaces, applied lazily at the first character after each newline so blank lines stay blank. The level is restored when the printer finishes.
pub fn indent(printer: Printer) -> Printer {
    Printer::Indent(Box::new(printer))
}

/// A soft separator: a single space when the enclosing [`group`](crate::group) renders flat, a newline (plus indentation) when it breaks — or unconditionally, when no group encloses it.
pub fn line() -> Printer {
    Printer::Line {
        flat: " ".into(),
        hard: false,
    }
}

/// A `line` that vanishes when flat: nothing on one line, a newline when broken.
pub fn soft_line() -> Printer {
    Printer::Line {
        flat: String::new(),
        hard: false,
    }
}

/// A mandatory break: always a newline, and no [`group`](crate::group) containing one renders flat — the fits scan fails on it.
pub fn hard_line() -> Printer {
    Printer::Line {
        flat: String::new(),
        hard: true,
    }
}

/// A width-adaptive *sequence* of already-punctuated items: each gap becomes a space or a newline on its own, so the run wraps like prose instead of breaking everywhere at once.
///
/// Use this where the items are short and interchangeable — the names of an import — and [`group`](crate::group) where they are structural parts that belong together or apart as a unit. Each item is measured and printed flat, so a fill never breaks *inside* an item; give it items that are already whole.
pub fn fill(items: impl IntoIterator<Item = Printer>) -> Printer {
    Printer::Fill(items.into_iter().collect())
}

/// The width-adaptive unit: renders `printer` flat — every enclosed `line` as its flat spelling — when that fits the room left on the line ([`run_printer_within`](crate::run_printer_within)), and broken otherwise. Without a width every group is flat, so grouping is behavior-neutral on the unbounded [`run_printer`](crate::run_printer) path.
pub fn group(printer: Printer) -> Printer {
    Printer::Group(Box::new(printer))
}

/// Mode-dependent text without being a break point: `flat` under a fitting group, `broken` under a broken one. The broken-only trailing comma is `if_break("", ",")`.
pub fn if_break(flat: impl Into<String>, broken: impl Into<String>) -> Printer {
    Printer::IfBreak {
        flat: flat.into(),
        broken: broken.into(),
    }
}

/// Note that something written at source offset `offset` begins here — see [`Printer::Mark`]. Zero width and no output.
pub fn begins(offset: usize) -> Printer {
    Printer::Mark {
        at: offset,
        begins: true,
    }
}

/// Note that the document now holds source up to `offset` — see [`Printer::Mark`]. Zero width and no output.
pub fn reaches(offset: usize) -> Printer {
    Printer::Mark {
        at: offset,
        begins: false,
    }
}
