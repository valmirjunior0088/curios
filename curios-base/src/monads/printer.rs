use std::{
    fmt::{self, Write},
    mem,
};

struct PrinterState<'a, 'b> {
    formatter: &'a mut fmt::Formatter<'b>,
    indent_step: usize,
    indent_by: usize,
    should_indent: bool,
    /// Characters emitted since the last newline, indentation included — what a [`Printer::Group`]'s fits scan subtracts from `width` to learn the room left on the line.
    column: usize,
    /// The line width [`Printer::Group`]s try to fit; `None` is unbounded, so every group renders flat.
    width: Option<usize>,
    /// [`Printer::LineSuffix`] text pending for the current line, flushed just before the next newline (or at the document's end).
    suffix: String,
}

impl<'a, 'b> PrinterState<'a, 'b> {
    fn new(
        formatter: &'a mut fmt::Formatter<'b>,
        indent_step: usize,
        width: Option<usize>,
    ) -> Self {
        Self {
            formatter,
            indent_step,
            indent_by: 0,
            should_indent: true,
            column: 0,
            width,
            suffix: String::new(),
        }
    }

    fn write(&mut self, string: &str) -> Result<(), fmt::Error> {
        for char in string.chars() {
            // A pending suffix rides at the end of the line it landed on: flush it before the newline. The suffix contains no newline itself, so the recursion is one level deep.
            if char == '\n' && !self.suffix.is_empty() {
                let suffix = mem::take(&mut self.suffix);
                self.write(&suffix)?;
            }
            if self.should_indent {
                for _ in 0..self.indent_by {
                    self.formatter.write_str(" ")?;
                    self.column += 1;
                }

                self.should_indent = false;
            }

            self.formatter.write_char(char)?;

            if char == '\n' {
                self.should_indent = true;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }

        Ok(())
    }

    /// The column the next character will actually land at: a fresh line still owes its indentation, which [`PrinterState::write`] emits lazily, so the pending spaces must be counted before a [`Printer::Group`] measures the room left on the line.
    fn effective_column(&self) -> usize {
        match self.should_indent {
            true => self.column + self.indent_by,
            false => self.column,
        }
    }
}

/// A pretty-printing document: what to emit, as data.
///
/// # Why this is an enum and not a closure
///
/// It used to be a `Box<dyn FnOnce(PrinterState) -> PrinterState>`, composed by nesting closures. That made a document a *tree of closures*, and running one called each child inside its parent's stack frame — so printing recursed as deep as the document nested, and a deep enough term aborted the compiler instead of printing. A diagnostic that cannot be printed is worse than no diagnostic, and no reduction budget can prevent it, because depth is not steps.
///
/// As data, the document is walked by [`run_printer`]'s explicit stack and nests without bound. Every IR crate's `Display` gets that at once. The combinators below keep the signatures they had, so the printers built on them are unchanged. No derives: `Debug`, `Clone`, and `PartialEq` would each walk the tree recursively and overflow on the documents this type exists to print.
pub enum Printer {
    /// Literal text. Newlines inside it arm the pending-indent logic, so a multi-line literal indents correctly under [`indent`].
    Text(String),
    /// Emitted in order.
    Concat(Vec<Printer>),
    /// Emitted one indentation level deeper.
    Indent(Box<Printer>),
    /// A document not built yet.
    ///
    /// The one closure variant, and it earns its place: a printer for a recursive IR is written as a recursive function, so *building* a document descends as deep as the term even though [`run_printer`] no longer does. Deferring a child turns that descent into work on the interpreter's stack — the builder is called when the interpreter reaches it, from a frame one deep rather than `n`.
    ///
    /// `Option` so the interpreter can take the thunk out: a type with a `Drop` impl cannot have a field moved away.
    Deferred(Option<Box<dyn FnOnce() -> Printer>>),
    /// A layout choice point: `flat` when the enclosing [`Printer::Group`] renders on one line, a newline (plus pending indentation) when it breaks — or unconditionally when no group encloses it, since the top of a document is broken context. `hard` marks a mandatory break: it always emits a newline, and the fits scan fails on it, so no group containing one ever renders flat.
    Line { flat: String, hard: bool },
    /// The width-adaptive *sequence*: its items are laid out left to right, and each gap independently becomes a space or a newline according to whether the item after it still fits the line.
    ///
    /// A [`Printer::Group`] is all-or-nothing — when its flat form does not fit, every [`Printer::Line`] inside it breaks — which is right for a structure whose parts belong on separate lines once any of them does. It is wrong for a run of short interchangeable items: a twenty-name import does not fit on one line, so a group puts each name on a line of its own and spends twenty lines on what two would hold. Each item carries its own separator, so this variant inserts only the gap and never invents punctuation.
    ///
    /// The gaps are decided while printing and owe nothing to the mode the fill was reached in: a fill inside a *flat* group still wraps. That is why [`fits`] reads a fill that runs out of room as the line ending rather than as the scan failing — any other reading would let a group render flat over a fill that then wrapped underneath it.
    Fill(Vec<Printer>),
    /// The width-adaptive unit: renders flat — every enclosed [`Printer::Line`] as its flat spelling — when its flat form fits the room left on the line, and broken otherwise. Nested groups measured inside a fitting parent render flat with it; under a broken parent each decides for itself. With no width configured every group fits.
    Group(Box<Printer>),
    /// Mode-dependent text that is *not* itself a break point: `flat` under a fitting group, `broken` under a broken one. The formatter's broken-only trailing comma is `IfBreak { flat: "", broken: "," }`. Measured at its flat spelling, so it never breaks a group by itself.
    IfBreak { flat: String, broken: String },
    /// Text buffered until just before the next emitted newline (or the document's end) — how a trailing comment rides at the end of whatever line it lands on, without its builder knowing where that line ends. Must not contain a newline itself. The fits scan fails on it: a line that must end cannot sit inside a flat group, which is the comment-is-a-hard-break law arriving mechanically.
    LineSuffix(String),
}

impl Printer {
    /// This document's children, taken out and left childless.
    ///
    /// Shared by [`run_printer`] and [`Drop`], which both need to descend without moving a field out of a type that has a destructor.
    fn take(&mut self) -> Printer {
        mem::replace(self, Printer::Text(String::new()))
    }

    /// Whether this node's children are already gone, so dropping it cannot reach another node.
    fn is_dismantled(&self) -> bool {
        match self {
            Printer::Text(_) => true,
            Printer::Concat(parts) | Printer::Fill(parts) => parts.is_empty(),
            Printer::Indent(inner) | Printer::Group(inner) => {
                matches!(**inner, Printer::Text(_))
            }
            // Holds a thunk, never a child document.
            Printer::Deferred(_) => true,
            Printer::Line { .. } => true,
            Printer::IfBreak { .. } => true,
            Printer::LineSuffix(_) => true,
        }
    }
}

/// Dismantled with an explicit stack, for the reason the type exists.
///
/// A document nests as deep as the term it prints, and the *derived* drop recurses one native frame per level — so a document deep enough to need an iterative [`run_printer`] would abort while being freed instead. Measured: a 100k-deep document overflows a default stack on drop alone.
impl Drop for Printer {
    fn drop(&mut self) {
        // The base case is "already dismantled", not "is a leaf", and the difference is not cosmetic: taking a node's children leaves a husk that is still a `Concat` or an `Indent`, so a check for `Text` alone sends every husk back through here to make another husk, forever. The regression below catches that at depth ten.
        if self.is_dismantled() {
            return;
        }

        let mut pending = Vec::from([self.take()]);

        while let Some(mut printer) = pending.pop() {
            match &mut printer {
                Printer::Text(_) => {}
                Printer::Concat(parts) | Printer::Fill(parts) => pending.extend(mem::take(parts)),
                Printer::Indent(inner) | Printer::Group(inner) => pending.push(inner.take()),
                Printer::Deferred(_) => {}
                Printer::Line { .. } => {}
                Printer::IfBreak { .. } => {}
                Printer::LineSuffix(_) => {}
            }
            // `printer` is dismantled now, so its own drop returns at once.
        }
    }
}

/// One entry of [`run_printer`]'s work stack.
///
/// `Dedent` is what replaces the closure nesting that used to restore the indentation level on the way out: pushed under a document, it runs after it. `Print`'s flag is the layout mode the document renders under: `true` inside a fitting [`Printer::Group`], where every soft [`Printer::Line`] emits its flat spelling.
enum Step {
    Print(Printer, bool),
    /// The remaining items of a [`Printer::Fill`], resumed after the one before them has been emitted — the gap decision needs the column the previous item actually left behind, which only exists once it is printed.
    Fill(Vec<Printer>),
    Dedent,
}

/// One node of [`fits`]'s walk, with the facts about its surroundings its verdict depends on.
struct Measured<'a> {
    node: &'a mut Printer,
    /// The layout mode this node renders under: `true` inside a fitting [`Printer::Group`], where every soft [`Printer::Line`] emits its flat spelling.
    flat: bool,
    /// Inside the measured group's own subtree, where a mandatory break refuses flatness instead of ending the line.
    inside: bool,
    /// Inside a [`Printer::Fill`], where running out of room means the fill *wraps* there rather than the scan failing.
    filling: bool,
}

/// Whether `printer`'s flat rendering fits in `available` characters — the [`Printer::Group`] decision. Within the group a hard [`Printer::Line`] or a [`Printer::LineSuffix`] fails the scan outright: a group containing a mandatory break never renders flat, which is what replaces build-time break propagation.
///
/// The scan does not stop at the group's edge: a group that fits exactly while unbreakable content trails it would otherwise overrun the line, so measurement continues into `rest` — the renderer's remaining work, in its recorded modes — until the line provably ends. Beyond the group the polarity of a mandatory break flips: a hard line, a text newline, or a soft [`Printer::Line`] in broken surroundings simply ends the line, deciding the scan in favor, and a pending suffix is skipped rather than counted — a trailing comment never reflows the code it rides.
///
/// A [`Printer::Fill`] is measured with the gap spaces its own layout will emit, and running out of room *inside* one also decides the scan in favor: the fill wraps at that gap, so the line ends within budget. A fill therefore never forces an enclosing group to break — which is the only answer consistent with printing, where a fill re-decides every gap regardless of the mode it was reached in.
///
/// Measuring forces every [`Printer::Deferred`] it reaches, replacing the node in place with the built document — the thunk is `FnOnce`, so a peek that discarded the result would lose it. The bounded lookahead is what keeps that cheap: at most one line width of document is ever materialized per decision, and what is materialized is exactly what printing consumes next. Iterative, like every other walk over this type.
fn fits(root: &mut Printer, rest: &mut [Step], available: usize, inside: bool) -> bool {
    let mut used = 0usize;
    let mut stack = Vec::from([Measured {
        node: root,
        flat: true,
        inside,
        filling: false,
    }]);
    let mut rest_iter = rest.iter_mut().rev();

    loop {
        let Some(Measured {
            node,
            flat,
            inside,
            filling,
        }) = stack.pop()
        else {
            match rest_iter.next() {
                // The whole document fits on this line.
                None => return true,
                Some(Step::Dedent) => continue,
                // A pending fill measures as its items would print. One gap *per item* rather than one fewer, unlike the node below: these are the items still owed a gap, the one before them having already been emitted.
                Some(Step::Fill(items)) => {
                    used += items.len();
                    if used > available {
                        return true;
                    }
                    stack.extend(items.iter_mut().rev().map(|item| Measured {
                        node: item,
                        flat: true,
                        inside: false,
                        filling: true,
                    }));
                    continue;
                }
                Some(Step::Print(printer, mode)) => {
                    stack.push(Measured {
                        node: printer,
                        flat: *mode,
                        inside: false,
                        filling: false,
                    });
                    continue;
                }
            }
        };

        // Materialization is handled before the descent match: the arms below push reborrows of `node`'s interior onto the shared stack, which makes the scrutinee borrow loop-wide — an in-match `*node = built` could never coexist with it.
        if matches!(*node, Printer::Deferred(_)) {
            let Printer::Deferred(slot) = &mut *node else {
                unreachable!("just matched");
            };
            let thunk = slot
                .take()
                .expect("a deferred thunk is present until forced");
            *node = thunk();
            stack.push(Measured {
                node,
                flat,
                inside,
                filling,
            });
            continue;
        }

        match &mut *node {
            Printer::Text(text) => {
                for char in text.chars() {
                    if char == '\n' {
                        return true;
                    }
                    used += 1;
                    if used > available {
                        return filling;
                    }
                }
            }
            Printer::Line {
                flat: spelling,
                hard,
            } => {
                if *hard {
                    return !inside;
                }
                if !flat {
                    // Broken surroundings render this as a newline, ending the line within budget. Unreachable inside the group, whose subtree is measured flat throughout.
                    return true;
                }
                used += spelling.chars().count();
                if used > available {
                    return filling;
                }
            }
            // Not a break point: measured at the spelling its mode selects.
            Printer::IfBreak {
                flat: on_flat,
                broken,
            } => {
                let spelling = if flat { &*on_flat } else { &*broken };
                used += spelling.chars().count();
                if used > available {
                    return filling;
                }
            }
            // A pending suffix means the line must end here — the comment-is-a-hard-break law. Beyond the group it flushes at the line's eventual end without reflowing what precedes it.
            Printer::LineSuffix(_) => {
                if inside {
                    return false;
                }
            }
            // Reversed, because the stack pops last-in first.
            Printer::Concat(parts) => {
                stack.extend(parts.iter_mut().rev().map(|part| Measured {
                    node: part,
                    flat,
                    inside,
                    filling,
                }));
            }
            // A fill measures as it prints: each item flat, one space per gap — charged up front, since every item is measured or the scan has already ended inside one. Whichever it is, `filling` is what decides the overflow, so the gaps need no running position of their own.
            Printer::Fill(items) => {
                used += items.len().saturating_sub(1);
                if used > available {
                    return true;
                }
                stack.extend(items.iter_mut().rev().map(|item| Measured {
                    node: item,
                    flat: true,
                    inside,
                    filling: true,
                }));
            }
            Printer::Indent(inner) => stack.push(Measured {
                node: inner,
                flat,
                inside,
                filling,
            }),
            // A nested group inside a fitting parent renders flat with it; a look-ahead group is measured flat too — if its flat form shares the line, the line holds either rendering of it.
            Printer::Group(inner) => stack.push(Measured {
                node: inner,
                flat,
                inside,
                filling,
            }),
            Printer::Deferred(_) => unreachable!("materialized above"),
        }
    }
}

/// The entry point: executes a printer against `formatter`, with `indent_step` spaces added per [`indent`] level. Typically the entire body of a `Display::fmt` impl — every IR crate's `print.rs` builds a [`Printer`] and hands it here.
///
/// Unbounded width: every [`group`] renders flat, so a document without `line`s renders exactly as it did before the layout variants existed. [`run_printer_within`] is the width-fitting entry.
///
/// Iterative by construction: the work stack holds what is left to emit, so a document's nesting costs heap rather than native frames.
pub fn run_printer<'b, 'c>(
    printer: Printer,
    formatter: &'b mut fmt::Formatter<'c>,
    indent_step: usize,
) -> Result<(), fmt::Error> {
    run(printer, formatter, indent_step, None)
}

/// [`run_printer`] against a line width: each [`group`] renders flat only when its flat form fits what remains of the line. The width is a target, not a guarantee — content with no break point still overruns.
pub fn run_printer_within<'b, 'c>(
    printer: Printer,
    formatter: &'b mut fmt::Formatter<'c>,
    indent_step: usize,
    width: usize,
) -> Result<(), fmt::Error> {
    run(printer, formatter, indent_step, Some(width))
}

fn run<'b, 'c>(
    printer: Printer,
    formatter: &'b mut fmt::Formatter<'c>,
    indent_step: usize,
    width: Option<usize>,
) -> Result<(), fmt::Error> {
    let mut state = PrinterState::new(formatter, indent_step, width);
    let mut stack = Vec::from([Step::Print(printer, false)]);

    while let Some(step) = stack.pop() {
        // Children are taken out rather than moved out: `Printer` has a `Drop` impl, so its fields cannot be moved away. What is left behind is childless and costs nothing to drop at the end of the arm.
        match step {
            Step::Print(mut printer, flat) => match &mut printer {
                Printer::Text(text) => state.write(text)?,
                // Reversed, because the stack pops last-in first.
                Printer::Concat(parts) => {
                    stack.extend(
                        mem::take(parts)
                            .into_iter()
                            .rev()
                            .map(|part| Step::Print(part, flat)),
                    );
                }
                Printer::Indent(inner) => {
                    state.indent_by += state.indent_step;
                    stack.push(Step::Dedent);
                    stack.push(Step::Print(inner.take(), flat));
                }
                Printer::Deferred(thunk) => {
                    if let Some(thunk) = thunk.take() {
                        stack.push(Step::Print(thunk(), flat));
                    }
                }
                // A hard line breaks even under a fitting group — the fits scan only guarantees the first line, and a mandatory break is mandatory.
                Printer::Line { hard: true, .. } => state.write("\n")?,
                Printer::Line { flat: spelling, .. } => match flat {
                    true => state.write(spelling)?,
                    false => state.write("\n")?,
                },
                Printer::Fill(items) => {
                    // The first item is printed directly, so `Step::Fill` always owns a *gap* and never has to ask whether one is due.
                    let mut items = mem::take(items);
                    if !items.is_empty() {
                        let first = items.remove(0);
                        if !items.is_empty() {
                            stack.push(Step::Fill(items));
                        }
                        stack.push(Step::Print(first, true));
                    }
                }
                Printer::Group(inner) => {
                    let mut inner = inner.take();
                    let inner_fits = flat
                        || match state.width {
                            // Unbounded: everything fits.
                            None => true,
                            Some(width) => fits(
                                &mut inner,
                                &mut stack,
                                width.saturating_sub(state.effective_column()),
                                true,
                            ),
                        };
                    stack.push(Step::Print(inner, inner_fits));
                }
                Printer::IfBreak {
                    flat: on_flat,
                    broken,
                } => {
                    let spelling = mem::take(if flat { on_flat } else { broken });
                    state.write(&spelling)?;
                }
                Printer::LineSuffix(text) => {
                    debug_assert!(!text.contains('\n'), "a line suffix stays on one line");
                    state.suffix.push_str(text);
                }
            },
            // One item per turn: measure it against what is left of the line, emit the gap it earned, print it flat, and queue the remainder. Printing the item flat is what makes it atomic — a fill decides *between* items, never inside one.
            Step::Fill(mut items) => {
                let mut item = items.remove(0);
                if !state.should_indent {
                    let room = match state.width {
                        None => usize::MAX,
                        Some(width) => width.saturating_sub(state.effective_column() + 1),
                    };
                    // The last item is measured against what follows the fill, for the reason a group is: whatever trails it — a closing bracket, a semicolon — has no break point before it and must share its line. A middle item is measured alone, since the items still to come will push that content onto a later line anyway.
                    let placed = match items.is_empty() {
                        true => fits(&mut item, &mut stack, room, true),
                        false => fits(&mut item, &mut [], room, true),
                    };
                    match placed {
                        true => state.write(" ")?,
                        false => state.write("\n")?,
                    }
                }
                if !items.is_empty() {
                    stack.push(Step::Fill(items));
                }
                stack.push(Step::Print(item, true));
            }
            Step::Dedent => state.indent_by -= state.indent_step,
        }
    }

    // A document that ends without a newline still owes its pending suffix.
    if !state.suffix.is_empty() {
        let suffix = mem::take(&mut state.suffix);
        state.write(&suffix)?;
    }

    Ok(())
}

/// Emits a literal string. Not a raw write: any newline it contains arms the pending-indentation logic, so multi-line literals indent correctly under [`indent`].
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

/// A soft separator: a single space when the enclosing [`group`] renders flat, a newline (plus indentation) when it breaks — or unconditionally, when no group encloses it.
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

/// A mandatory break: always a newline, and no [`group`] containing one renders flat — the fits scan fails on it.
pub fn hard_line() -> Printer {
    Printer::Line {
        flat: String::new(),
        hard: true,
    }
}

/// A width-adaptive *sequence* of already-punctuated items: each gap becomes a space or a newline on its own, so the run wraps like prose instead of breaking everywhere at once.
///
/// Use this where the items are short and interchangeable — the names of an import — and [`group`] where they are structural parts that belong together or apart as a unit. Each item is measured and printed flat, so a fill never breaks *inside* an item; give it items that are already whole.
pub fn fill(items: impl IntoIterator<Item = Printer>) -> Printer {
    Printer::Fill(items.into_iter().collect())
}

/// The width-adaptive unit: renders `printer` flat — every enclosed `line` as its flat spelling — when that fits the room left on the line ([`run_printer_within`]), and broken otherwise. Without a width every group is flat, so grouping is behavior-neutral on the unbounded [`run_printer`] path.
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

/// Buffers `text` until just before the next emitted newline (or the document's end) — how a trailing comment rides at the end of whatever line it lands on without its builder knowing where that line ends. `text` must not contain a newline; a pending suffix fails the fits scan, so the group it lands in always breaks.
pub fn line_suffix(text: impl Into<String>) -> Printer {
    Printer::LineSuffix(text.into())
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        std::{cell::Cell, rc::Rc},
    };

    /// A document nests as deep as the term it prints, and both walks over it — printing and freeing — must survive that. Depth is not steps, so no reduction budget bounds either one; only an explicit stack does.
    fn nested(depth: usize) -> Printer {
        let mut document = pure("x");
        for _ in 0..depth {
            document = indent(flat([pure("("), document, pure(")")]));
        }
        document
    }

    fn render(printer: Printer, width: Option<usize>) -> String {
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

    /// `a` and `b` separated by soft lines under one group — the canonical fits-or-breaks document.
    fn pair() -> Printer {
        group(flat([pure("aaa"), line(), pure("bbb")]))
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

    /// The layout variants must survive the same depth as the rest of the algebra, in all three walks: drop, unbounded print, and the measuring print. No `indent` here — a broken 100k-deep document would otherwise write quadratically many indentation spaces, and depth is what this exercises, not volume.
    fn nested_groups(depth: usize) -> Printer {
        let mut document = pure("x");
        for _ in 0..depth {
            document = group(flat([pure("("), soft_line(), document, pure(")")]));
        }
        document
    }

    #[test]
    fn a_deep_grouped_document_is_freed_without_recursing() {
        drop(nested_groups(100_000));
    }

    #[test]
    fn a_deep_grouped_document_is_printed_and_measured_without_recursing() {
        let printed = render(nested_groups(100_000), Some(30));
        assert_eq!(printed.matches('(').count(), 100_000);
    }

    #[test]
    fn without_a_width_every_group_is_flat() {
        assert_eq!(render(pair(), None), "aaa bbb");
    }

    #[test]
    fn a_group_that_exactly_fits_stays_flat() {
        // "aaa bbb" is seven characters.
        assert_eq!(render(pair(), Some(7)), "aaa bbb");
    }

    #[test]
    fn a_group_one_short_of_fitting_breaks() {
        assert_eq!(render(pair(), Some(6)), "aaa\nbbb");
    }

    #[test]
    fn a_soft_line_vanishes_flat_and_breaks_broken() {
        let document = || group(flat([pure("aaa"), soft_line(), pure("bbb")]));
        assert_eq!(render(document(), Some(6)), "aaabbb");
        assert_eq!(render(document(), Some(5)), "aaa\nbbb");
    }

    #[test]
    fn a_group_breaks_for_the_unbreakable_content_trailing_it() {
        // The group alone fits width 9 exactly, but the look-ahead sees " tail" with no break point before it — flat would overrun, so the group breaks.
        let document = |tail| flat([group(flat([pure("aaa"), line(), pure("bbb")])), pure(tail)]);
        assert_eq!(render(document(" tail"), Some(9)), "aaa\nbbb tail");
        assert_eq!(render(document(""), Some(9)), "aaa bbb");
    }

    #[test]
    fn look_ahead_stops_at_the_next_break_opportunity() {
        // The trailing content ends this line at its own soft line (broken mode outside any fitting group), so only " t" counts against the first group's budget.
        let document = flat([
            group(flat([pure("aaa"), line(), pure("bbb")])),
            pure(" t"),
            line(),
            pure("cccccccccc"),
        ]);
        assert_eq!(render(document, Some(9)), "aaa bbb t\ncccccccccc");
    }

    #[test]
    fn a_hard_line_forces_every_enclosing_group() {
        let document = group(flat([
            pure("aaa"),
            line(),
            group(flat([pure("bbb"), hard_line(), pure("ccc")])),
        ]));
        // Width 100 fits everything by count, but the mandatory break refuses flatness all the way up.
        assert_eq!(render(document, Some(100)), "aaa\nbbb\nccc");
    }

    #[test]
    fn a_line_outside_any_group_breaks() {
        let document = flat([pure("aaa"), line(), pure("bbb")]);
        assert_eq!(render(document, None), "aaa\nbbb");
    }

    #[test]
    fn a_broken_group_indents_its_continuation_lines() {
        let document = || {
            group(flat([
                pure("head("),
                indent(flat([soft_line(), pure("argument")])),
                soft_line(),
                pure(")"),
            ]))
        };
        assert_eq!(render(document(), Some(10)), "head(\n  argument\n)");
        assert_eq!(render(document(), Some(20)), "head(argument)");
    }

    #[test]
    fn measurement_forces_each_thunk_at_most_once() {
        let forced = Rc::new(Cell::new(0usize));
        let counter = Rc::clone(&forced);
        let document = group(flat([
            pure("aaa"),
            line(),
            deferred(move || {
                counter.set(counter.get() + 1);
                pure("bbb")
            }),
        ]));

        // The scan forces the thunk to measure it; printing then consumes the materialized document rather than forcing again.
        assert_eq!(render(document, Some(7)), "aaa bbb");
        assert_eq!(forced.get(), 1);
    }

    #[test]
    fn if_break_spells_per_mode_without_breaking() {
        // The broken-only trailing comma: absent flat, present broken — and never the cause of the break.
        let document = || {
            group(flat([
                pure("("),
                indent(flat([
                    soft_line(),
                    pure("a"),
                    pure(","),
                    line(),
                    pure("b"),
                    if_break("", ","),
                ])),
                soft_line(),
                pure(")"),
            ]))
        };
        assert_eq!(render(document(), Some(10)), "(a, b)");
        assert_eq!(render(document(), Some(3)), "(\n  a,\n  b,\n)");
    }

    #[test]
    fn a_line_suffix_rides_to_the_end_of_its_line() {
        let document = flat([
            pure("code"),
            line_suffix(" -- trailing"),
            pure(" more"),
            hard_line(),
            pure("next"),
        ]);
        assert_eq!(render(document, None), "code more -- trailing\nnext");
    }

    #[test]
    fn a_line_suffix_flushes_at_document_end() {
        let document = flat([pure("code"), line_suffix(" -- last")]);
        assert_eq!(render(document, None), "code -- last");
    }

    #[test]
    fn a_line_suffix_forces_its_group_to_break() {
        // A trailing comment means the line must end, so the group cannot stay flat however wide the width.
        let document = group(flat([pure("a"), line_suffix(" -- c"), line(), pure("b")]));
        assert_eq!(render(document, Some(100)), "a -- c\nb");
    }

    #[test]
    fn a_fill_is_measured_with_the_gaps_it_will_emit() {
        // The spaces between the names are part of the fill's width, so what follows one starts where the gaps leave off and not where the names do. Counted short, this fits at eight.
        let document = || group(flat([fill([pure("aa,"), pure("bb")]), line(), pure("cc")]));
        assert_eq!(render(document(), Some(8)), "aa, bb\ncc");
        assert_eq!(render(document(), Some(9)), "aa, bb cc");
    }

    #[test]
    fn a_wrapping_fill_does_not_break_its_group() {
        // A fill wraps rather than overrunning, so the line ends inside it and the scan decides in favor. The alternative is the layout this rule exists to prevent: the group breaking *and* the fill wrapping, which spends a line on the delimiter and then wraps anyway.
        let document = group(flat([
            pure("("),
            soft_line(),
            fill([pure("aaa,"), pure("bbb,"), pure("ccc")]),
            pure(")"),
        ]));
        assert_eq!(render(document, Some(9)), "(aaa,\nbbb, ccc)");
    }

    #[test]
    fn a_fills_last_item_makes_room_for_what_trails_it() {
        // The closer has no break point before it, so it shares the last item's line — and must be measured against it, or the line overruns by exactly its width.
        let document = || flat([fill([pure("aa,"), pure("bb")]), pure(");")]);
        assert_eq!(render(document(), Some(7)), "aa,\nbb);");
        assert_eq!(render(document(), Some(8)), "aa, bb);");
    }

    #[test]
    fn a_newline_in_literal_text_ends_the_scan_within_budget() {
        // The scan only guarantees the first line, so a literal newline before the budget runs out means the group fits.
        let document = group(flat([
            pure("aaa\nlong tail beyond any width"),
            line(),
            pure("bbb"),
        ]));
        assert_eq!(
            render(document, Some(4)),
            "aaa\nlong tail beyond any width bbb"
        );
    }
}
