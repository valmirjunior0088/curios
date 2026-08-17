//! The interpreter and the fits scan it consults.
//!
//! These are one engine rather than two files' worth: `fits` measures against `rest` — the interpreter's own remaining work stack — so the lookahead and the walk share [`Step`] and cannot be separated without inventing a seam the algorithm does not have.

use {
    super::Printer,
    std::{
        fmt::{self, Write},
        mem,
    },
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

/// The entry point: executes a printer against `formatter`, with `indent_step` spaces added per [`indent`](crate::indent) level. Typically the entire body of a `Display::fmt` impl — every IR crate's `print.rs` builds a [`Printer`] and hands it here.
///
/// Unbounded width: every [`group`](crate::group) renders flat, so a document without `line`s renders exactly as it did before the layout variants existed. [`run_printer_within`] is the width-fitting entry.
///
/// Iterative by construction: the work stack holds what is left to emit, so a document's nesting costs heap rather than native frames.
pub fn run_printer<'b, 'c>(
    printer: Printer,
    formatter: &'b mut fmt::Formatter<'c>,
    indent_step: usize,
) -> Result<(), fmt::Error> {
    run(printer, formatter, indent_step, None)
}

/// [`run_printer`] against a line width: each [`group`](crate::group) renders flat only when its flat form fits what remains of the line. The width is a target, not a guarantee — content with no break point still overruns.
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
