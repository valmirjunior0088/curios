//! The interpreter and the fits scan it consults.
//!
//! These are one engine rather than two files' worth: `fits` measures against `rest` — the interpreter's own remaining work stack — so the lookahead and the walk share [`Step`] and cannot be separated without inventing a seam the algorithm does not have.
//!
//! A formatter's run additionally *follows* the text its document was read from, which is how what that text held and the document does not — a comment — goes back between the characters it was written between: see [`Original`].

use {
    super::Printer,
    std::{
        cell::RefCell,
        collections::VecDeque,
        fmt::{self, Write},
        mem,
    },
};

struct PrinterState<'a, 'b, 'c> {
    formatter: &'a mut fmt::Formatter<'b>,
    /// Bytes emitted so far, which is what an [`Annotation`] measures its range in.
    written: usize,
    /// Every [`Printer::Named`] emitted, with where it landed — empty for every renderer but [`render_annotated`].
    annotations: Vec<Annotation>,
    indent_step: usize,
    indent_by: usize,
    should_indent: bool,
    /// Characters emitted since the last newline, indentation included — what a [`Printer::Group`]'s fits scan subtracts from `width` to learn the room left on the line.
    column: usize,
    /// The line width [`Printer::Group`]s try to fit; `None` is unbounded, so every group renders flat.
    width: Option<usize>,
    /// The text the document was read from and how far into it the output has got. `None` for every ordinary `Display`, which is what keeps a run with nothing to follow rendering as a pretty printer's.
    following: Option<(&'c Original<'c>, Cursor)>,
    /// How many of the original's comments have been taken off the cursor's arrivals, into `suffix` or `due`.
    taken: usize,
    /// Arrived comments still to be written, each on a line of its own, by index and in written order.
    due: VecDeque<usize>,
    /// The comment riding the current line, flushed just before the line ends. One at most: two line comments sharing an output line are *one* comment when it is read back.
    suffix: String,
    /// Spaces written and not yet followed by anything on their line, dropped where the line ends first. Only a following run holds any, so nothing else is ever buffered.
    spaces: usize,
    /// The line ends in a comment, which runs to the end of its line: whatever comes next starts another.
    after_comment: bool,
    /// The renderer broke a line where the document offered no break, so what continues it hangs one step in until the document's next newline.
    hanging: bool,
}

impl<'a, 'b, 'c> PrinterState<'a, 'b, 'c> {
    fn new(
        formatter: &'a mut fmt::Formatter<'b>,
        indent_step: usize,
        width: Option<usize>,
        original: Option<&'c Original<'c>>,
    ) -> Self {
        Self {
            formatter,
            written: 0,
            annotations: Vec::new(),
            indent_step,
            indent_by: 0,
            should_indent: true,
            column: 0,
            width,
            following: original.map(|original| (original, Cursor::start(original))),
            taken: 0,
            due: VecDeque::new(),
            suffix: String::new(),
            spaces: 0,
            after_comment: false,
            hanging: false,
        }
    }

    /// The indentation the next line begun here is owed.
    fn indentation(&self) -> usize {
        match self.hanging {
            true => self.indent_by + self.indent_step,
            false => self.indent_by,
        }
    }

    /// One character on the current line. A fresh line pays its indentation here, lazily, so a blank line stays blank; a following run holds a space back until something follows it.
    fn put(&mut self, char: char) -> Result<(), fmt::Error> {
        if char == ' ' && self.following.is_some() {
            self.spaces += 1;
            return Ok(());
        }

        let leading = mem::take(&mut self.spaces)
            + match self.should_indent {
                true => self.indentation(),
                false => 0,
            };
        for _ in 0..leading {
            self.formatter.write_char(' ')?;
        }
        self.written += leading;
        self.column += leading;
        self.should_indent = false;

        self.formatter.write_char(char)?;
        self.written += char.len_utf8();
        self.column += 1;

        Ok(())
    }

    /// End the current line, the comment riding it first.
    fn end_line(&mut self) -> Result<(), fmt::Error> {
        self.spaces = 0;
        if !self.suffix.is_empty() {
            let suffix = mem::take(&mut self.suffix);
            self.formatter.write_str(&suffix)?;
            self.written += suffix.len();
        }

        self.formatter.write_char('\n')?;
        self.written += 1;
        self.should_indent = true;
        self.column = 0;
        self.after_comment = false;

        Ok(())
    }

    /// A newline the document writes, which is where a hanging continuation ends.
    fn newline(&mut self) -> Result<(), fmt::Error> {
        self.hanging = false;
        self.end_line()
    }

    /// A newline the document did not offer, forced by a comment written where no line can begin.
    fn force_break(&mut self) -> Result<(), fmt::Error> {
        self.hanging = true;
        self.end_line()
    }

    /// Document text. A following run steps through the original with every character that is not blank, so each comment arrives between the two characters it was written between.
    fn text(&mut self, string: &str) -> Result<(), fmt::Error> {
        for char in string.chars() {
            if char == '\n' {
                self.newline()?;
            } else if char.is_whitespace() || self.following.is_none() {
                self.put(char)?;
            } else {
                self.before(char)?;
                self.put(char)?;
                self.after(char);
            }
        }

        Ok(())
    }

    /// What `char` owes before it is written: the comments the original holds ahead of it, and the line a comment has already ended.
    fn before(&mut self, char: char) -> Result<(), fmt::Error> {
        let Some((original, mut cursor)) = self.following else {
            return Ok(());
        };
        cursor.approach(char, original);
        self.following = Some((original, cursor));
        self.arrivals();

        // A character the document adds is no part of the original, so nothing written there is before or after it: a trailing comma stays with the member it closes.
        if !(original.added)(char) {
            self.pay(true)?;
        }
        if self.after_comment {
            self.end_line()?;
        } else if !self.suffix.is_empty() && !(original.closes)(char) {
            self.force_break()?;
        }

        Ok(())
    }

    /// Step the cursor past `char`, and take the comments written straight after it.
    fn after(&mut self, char: char) {
        if let Some((original, mut cursor)) = self.following {
            cursor.pass(char, original);
            self.following = Some((original, cursor));
            self.arrivals();
        }
    }

    /// The members of a block end here — see [`Printer::Ends`]. What was written past the last of them is due and takes its line now, at their indentation; the document offers this line start, so nothing hangs from it.
    fn ends(&mut self, rest: &mut [Step]) -> Result<(), fmt::Error> {
        self.meet(rest);
        self.pay(false)
    }

    /// A newline the document writes, taken with the work still ahead of it — see [`PrinterState::meet`].
    fn line_ends(&mut self, rest: &mut [Step]) -> Result<(), fmt::Error> {
        self.meet(rest);
        self.newline()
    }

    /// Bring the cursor up to the character the document writes next, where the original holds it in reach. What the original holds ahead of that character has then arrived: a comment written behind something the document leaves out — a parenthesis the tree does not record, a list's last comma — is taken while the line it was written on is still open, and not with the first character of the next.
    fn meet(&mut self, rest: &mut [Step]) {
        let Some((original, cursor)) = self.following else {
            return;
        };
        let Some(next) = ahead(rest) else {
            return;
        };

        let mut met = cursor;
        met.approach(next, original);
        // A character the original does not hold next is no reason to wait yet, since it has not been written.
        if !met.held || cursor.held {
            self.following = Some((original, met));
            self.arrivals();
        }
    }

    /// Note that the document holds the original up to `at` — see [`Printer::Mark`].
    fn reach(&mut self, at: usize) {
        if let Some((original, mut cursor)) = self.following {
            cursor.reach(at, original);
            self.following = Some((original, cursor));
            self.arrivals();
        }
    }

    /// Take the comments the cursor has passed since the last look. One written after code rides the line that code is on, if that line is free; every other waits in `due` for a line of its own.
    fn arrivals(&mut self) {
        let Some((original, cursor)) = self.following else {
            return;
        };

        while self.taken < cursor.arrived {
            let comment = &original.comments[self.taken];
            let rides = !comment.own_line
                && self.suffix.is_empty()
                && self.due.is_empty()
                && !self.should_indent
                && !self.after_comment;
            match rides {
                true => {
                    self.suffix.push(' ');
                    self.suffix.push_str(&comment.text);
                }
                false => self.due.push_back(self.taken),
            }
            self.taken += 1;
        }
    }

    /// Write every comment that is due, each on a line of its own. `forced` says the document offers no line start here, so a line that holds text is broken by the renderer and its continuation hangs.
    fn pay(&mut self, forced: bool) -> Result<(), fmt::Error> {
        let Some((original, _)) = self.following else {
            return Ok(());
        };

        while let Some(index) = self.due.pop_front() {
            if self.after_comment {
                self.end_line()?;
            } else if !self.should_indent {
                match forced {
                    true => self.force_break()?,
                    false => self.newline()?,
                }
            }
            for char in original.comments[index].text.chars() {
                self.put(char)?;
            }
            self.after_comment = true;
        }

        Ok(())
    }

    /// The column the next character will actually land at: a fresh line still owes its indentation, which [`PrinterState::put`] emits lazily, so the pending spaces must be counted before a [`Printer::Group`] measures the room left on the line — and a line that owes a comment a line of its own, or that a comment has ended, is over.
    fn effective_column(&self) -> usize {
        let fresh = self.should_indent || self.after_comment;
        if !self.due.is_empty() {
            match self.hanging || !fresh {
                true => self.indent_by + self.indent_step,
                false => self.indent_by,
            }
        } else if fresh {
            self.indentation()
        } else {
            self.column + self.spaces
        }
    }
}

/// One entry of [`run_printer`]'s work stack.
///
/// `Dedent` restores the indentation level on the way out: pushed under a document, it runs after it. `Print`'s flag is the layout mode the document renders under: `true` inside a fitting [`Printer::Group`], where every soft [`Printer::Line`] emits its flat spelling.
enum Step {
    Print(Printer, bool),
    /// The remaining items of a [`Printer::Fill`], resumed after the one before them has been emitted — the gap decision needs the column the previous item actually left behind, which only exists once it is printed.
    Fill(Vec<Printer>),
    Dedent,
}

/// One comment the original held: where it was written and how it reads.
pub struct Owed {
    /// The offset its first character was written at.
    pub at: usize,
    /// The offset one past its last character.
    pub end: usize,
    /// Whether it had its line to itself, rather than following code on one.
    pub own_line: bool,
    /// The text, as it should appear.
    pub text: String,
}

/// The text a document was read from, for a renderer that puts back what that text held and the document does not.
///
/// **What a formatter hands a renderer that a pretty printer never needs.** The document says what the program *is*; a comment is no part of it, and where one goes is a fact about the output: which line it rides, or which two characters it stands between. So the renderer reads the original as it writes: a cursor steps through this text with every character that goes out, and a comment has *arrived* when everything written before it has been written. Nothing in the document says where a comment belongs, so no printer can forget to.
///
/// The three predicates are what the document may spell differently from the text it was read from, and they are the caller's to state: this crate knows no language.
pub struct Original<'a> {
    pub text: &'a str,
    /// Ascending by where each was written.
    pub comments: Vec<Owed>,
    /// A character the document may write where the original has none.
    pub added: fn(char) -> bool,
    /// A character of the original the document may leave out.
    pub dropped: fn(char) -> bool,
    /// A character that may still follow on a line a comment already rides, where anything else starts the next.
    pub closes: fn(char) -> bool,
}

/// How far into an [`Original`] the output has got.
///
/// It rests on the next character the document has to write, past blanks and past every comment on the way — which is what makes a comment's arrival exact: the cursor cannot step over one without `arrived` counting it.
#[derive(Clone, Copy)]
struct Cursor {
    offset: usize,
    /// How many comments it has passed.
    arrived: usize,
    /// The document wrote a character the original does not hold next — a literal spelled another way — so the cursor waits where it is for the next [`Printer::Mark`].
    held: bool,
}

impl Cursor {
    fn start(original: &Original<'_>) -> Self {
        let mut cursor = Self {
            offset: 0,
            arrived: 0,
            held: false,
        };
        cursor.settle(original);

        cursor
    }

    /// Rest on the next character the document has to write.
    fn settle(&mut self, original: &Original<'_>) {
        loop {
            // A comment a mark carried the cursor over has arrived without moving it.
            while original
                .comments
                .get(self.arrived)
                .is_some_and(|comment| comment.at < self.offset)
            {
                self.arrived += 1;
            }

            let rest = &original.text[self.offset..];
            self.offset += rest.len() - rest.trim_start().len();

            match original.comments.get(self.arrived) {
                Some(comment) if comment.at == self.offset => {
                    self.offset = comment.end;
                    self.arrived += 1;
                }
                _ => return,
            }
        }
    }

    fn next(&self, original: &Original<'_>) -> Option<char> {
        original.text[self.offset..].chars().next()
    }

    /// Bring the cursor to where `written` stands in the original, over what the document left out on the way.
    fn approach(&mut self, written: char, original: &Original<'_>) {
        while !self.held {
            match self.next(original) {
                Some(next) if next == written => return,
                // The document's own, unless the original holds it just past what the document left out: `(a), b` written `a, b`.
                _ if (original.added)(written) => {
                    if let Some(offset) = self.past_dropped(written, original) {
                        self.offset = offset;
                    }
                    return;
                }
                Some(next) if (original.dropped)(next) => {
                    self.offset += next.len_utf8();
                    self.settle(original);
                }
                _ => self.held = true,
            }
        }
    }

    /// Where `written` stands, if nothing but blanks and what the document may leave out lies between the cursor and it. No comment is stepped over on the way, so taking the answer arrives nothing.
    fn past_dropped(&self, written: char, original: &Original<'_>) -> Option<usize> {
        let mut offset = self.offset;

        loop {
            let rest = &original.text[offset..];
            offset += rest.len() - rest.trim_start().len();
            if original
                .comments
                .get(self.arrived)
                .is_some_and(|comment| comment.at == offset)
            {
                return None;
            }

            match original.text[offset..].chars().next() {
                Some(next) if next == written => return Some(offset),
                Some(next) if (original.dropped)(next) => offset += next.len_utf8(),
                _ => return None,
            }
        }
    }

    /// Step past `written` where the original holds it next.
    fn pass(&mut self, written: char, original: &Original<'_>) {
        if !self.held && self.next(original) == Some(written) {
            self.offset += written.len_utf8();
            self.settle(original);
        }
    }

    /// The document holds the original up to `at`: a hold ends, and a cursor behind it moves up.
    fn reach(&mut self, at: usize, original: &Original<'_>) {
        self.held = false;
        self.offset = self.offset.max(at);
        self.settle(original);
    }
}

/// One [`Printer::Named`] as rendered: the byte range of the output it occupies, and the name it carries.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    pub start: usize,
    pub end: usize,
    pub name: String,
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

/// What a scan has learned of the comments its group would write past, were it flat.
struct Arriving<'a> {
    original: &'a Original<'a>,
    cursor: Cursor,
    /// One that needs a line of its own has arrived inside the group.
    own_line: bool,
    /// One that rides its line has.
    riding: bool,
}

impl Arriving<'_> {
    fn note(&mut self, from: usize) {
        for comment in &self.original.comments[from..self.cursor.arrived] {
            match comment.own_line {
                true => self.own_line = true,
                false => self.riding = true,
            }
        }
    }

    fn reach(&mut self, at: usize) {
        let from = self.cursor.arrived;
        self.cursor.reach(at, self.original);
        self.note(from);
    }

    /// Whether `char` may be written flat: nothing follows a comment on its line, but for what closes the line a riding one is on.
    fn admits(&mut self, char: char) -> bool {
        let from = self.cursor.arrived;
        self.cursor.approach(char, self.original);
        self.note(from);

        if (self.own_line && !(self.original.added)(char))
            || (self.riding && !(self.original.closes)(char))
        {
            return false;
        }

        let from = self.cursor.arrived;
        self.cursor.pass(char, self.original);
        self.note(from);

        true
    }
}

/// The next character of document text still to be written that is not blank, found through the renderer's remaining work without taking any of it. An [`Printer::IfBreak`] is read in the mode its step was queued under, which is the one a line that ends here leaves it in.
///
/// Forces every [`Printer::Deferred`] on the way in place, as [`fits`] does and for its reason; what it forces is what printing consumes next.
fn ahead(rest: &mut [Step]) -> Option<char> {
    let visible = |text: &str| text.chars().find(|char| !char.is_whitespace());
    let mut rest = rest.iter_mut().rev();
    let mut stack: Vec<(&mut Printer, bool)> = Vec::new();

    loop {
        let Some((node, flat)) = stack.pop() else {
            match rest.next()? {
                Step::Dedent => {}
                Step::Fill(items) => stack.extend(items.iter_mut().rev().map(|item| (item, true))),
                Step::Print(printer, mode) => stack.push((printer, *mode)),
            }
            continue;
        };

        if let Printer::Deferred(slot) = &mut *node {
            let thunk = slot
                .take()
                .expect("a deferred thunk is present until forced");
            *node = thunk();
            stack.push((node, flat));
            continue;
        }

        match &mut *node {
            Printer::Text(text) | Printer::Named(text) => {
                if let Some(found) = visible(text) {
                    return Some(found);
                }
            }
            Printer::IfBreak {
                flat: on_flat,
                broken,
            } => {
                if let Some(found) = visible(if flat { on_flat } else { broken }) {
                    return Some(found);
                }
            }
            Printer::Concat(parts) | Printer::Fill(parts) => {
                stack.extend(parts.iter_mut().rev().map(|part| (part, flat)));
            }
            Printer::Indent(inner) | Printer::Group(inner) => stack.push((inner, flat)),
            Printer::Line { .. } | Printer::Mark(_) | Printer::Ends => {}
            Printer::Deferred(_) => unreachable!("forced above"),
        }
    }
}

/// Whether `printer`'s flat rendering fits in `available` characters — the [`Printer::Group`] decision. Within the group a hard [`Printer::Line`] fails the scan outright, and so does text following a comment that arrives inside the group: a group containing a mandatory break never renders flat, and a comment is one. A comment that arrives with nothing after it in the group leaves the group flat — it rides the end of the line or takes the next either way.
///
/// The scan does not stop at the group's edge: a group that fits exactly while unbreakable content trails it would otherwise overrun the line, so measurement continues into `rest` — the renderer's remaining work, in its recorded modes — until the line provably ends. Beyond the group the polarity of a mandatory break flips: a hard line, a text newline, or a soft [`Printer::Line`] in broken surroundings simply ends the line, deciding the scan in favor, and a comment is not looked for — a trailing comment never reflows the code it rides.
///
/// A [`Printer::Fill`] is measured with the gap spaces its own layout will emit, and running out of room *inside* one also decides the scan in favor: the fill wraps at that gap, so the line ends within budget. A fill therefore never forces an enclosing group to break — which is the only answer consistent with printing, where a fill re-decides every gap regardless of the mode it was reached in.
///
/// Measuring forces every [`Printer::Deferred`] it reaches, replacing the node in place with the built document — the thunk is `FnOnce`, so a peek that discarded the result would lose it. The bounded lookahead is what keeps that cheap: at most one line width of document is ever materialized per decision, and what is materialized is exactly what printing consumes next. Iterative, like every other walk over this type.
fn fits(
    root: &mut Printer,
    rest: &mut [Step],
    available: usize,
    inside: bool,
    following: Option<(&Original<'_>, Cursor)>,
) -> bool {
    let mut used = 0usize;
    // A copy of the cursor, walked through the group as printing would walk the real one.
    let mut arriving = following.map(|(original, cursor)| Arriving {
        original,
        cursor,
        own_line: false,
        riding: false,
    });
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

        // Text of either kind, measured a character at a time: within the group each one that is not blank steps the cursor's copy, and the first to follow a comment refuses flatness.
        let mut measure = |spelling: &str, used: &mut usize| -> Option<bool> {
            for char in spelling.chars() {
                if char == '\n' {
                    return Some(true);
                }
                if inside
                    && !char.is_whitespace()
                    && arriving
                        .as_mut()
                        .is_some_and(|arriving| !arriving.admits(char))
                {
                    return Some(false);
                }
                *used += 1;
                if *used > available {
                    return Some(filling);
                }
            }

            None
        };

        match &mut *node {
            Printer::Text(text) | Printer::Named(text) => {
                if let Some(verdict) = measure(text, &mut used) {
                    return verdict;
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
                if let Some(verdict) = measure(spelling, &mut used) {
                    return verdict;
                }
            }
            // Not a break point: measured at the spelling its mode selects.
            Printer::IfBreak {
                flat: on_flat,
                broken,
            } => {
                let spelling = if flat { &*on_flat } else { &*broken };
                if let Some(verdict) = measure(spelling, &mut used) {
                    return verdict;
                }
            }
            // Zero width, and it moves the copy as it will move the cursor: what it carries the cursor over has arrived here.
            Printer::Mark(at) => {
                if inside && let Some(arriving) = arriving.as_mut() {
                    arriving.reach(*at);
                }
            }
            // Zero width, and nothing to measure: what it brings in arrives with the closer the scan reads next.
            Printer::Ends => {}
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
/// Unbounded width: every [`group`](crate::group) renders flat. [`run_printer_within`] is the width-fitting entry.
///
/// Iterative by construction: the work stack holds what is left to emit, so a document's nesting costs heap rather than native frames.
pub fn run_printer<'b, 'c>(
    printer: Printer,
    formatter: &'b mut fmt::Formatter<'c>,
    indent_step: usize,
) -> Result<(), fmt::Error> {
    run(printer, formatter, indent_step, None, None).map(|_| ())
}

/// [`run_printer`] against a line width: each [`group`](crate::group) renders flat only when its flat form fits what remains of the line. The width is a target, not a guarantee — content with no break point still overruns.
pub fn run_printer_within<'b, 'c>(
    printer: Printer,
    formatter: &'b mut fmt::Formatter<'c>,
    indent_step: usize,
    width: usize,
) -> Result<(), fmt::Error> {
    run(printer, formatter, indent_step, Some(width), None).map(|_| ())
}

/// [`run_printer_within`], additionally following `original` — the text the document was read from — and placing the comments it held.
///
/// **What a formatter has that a pretty printer does not.** A comment written after code rides the end of the line that code lands on; one written on a line of its own takes a line of its own between the two characters it stood between, broken open by the renderer where the document offers no break. Nothing else about rendering changes.
pub fn run_printer_placing<'b, 'c>(
    printer: Printer,
    formatter: &'b mut fmt::Formatter<'c>,
    indent_step: usize,
    width: usize,
    original: &Original<'_>,
) -> Result<(), fmt::Error> {
    run(printer, formatter, indent_step, Some(width), Some(original)).map(|_| ())
}

fn run<'b, 'c>(
    printer: Printer,
    formatter: &'b mut fmt::Formatter<'c>,
    indent_step: usize,
    width: Option<usize>,
    original: Option<&Original<'_>>,
) -> Result<Vec<Annotation>, fmt::Error> {
    let mut state = PrinterState::new(formatter, indent_step, width, original);
    let mut stack = Vec::from([Step::Print(printer, false)]);

    while let Some(step) = stack.pop() {
        // Children are taken out rather than moved out: `Printer` has a `Drop` impl, so its fields cannot be moved away. What is left behind is childless and costs nothing to drop at the end of the arm.
        match step {
            Step::Print(mut printer, flat) => match &mut printer {
                Printer::Text(text) => state.text(text)?,
                Printer::Named(text) => {
                    let start = state.written;
                    state.text(text)?;
                    state.annotations.push(Annotation {
                        start,
                        end: state.written,
                        name: mem::take(text),
                    });
                }
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
                Printer::Line { hard: true, .. } => state.line_ends(&mut stack)?,
                Printer::Line { flat: spelling, .. } => match flat {
                    true => state.text(spelling)?,
                    false => state.line_ends(&mut stack)?,
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
                            // A comment already taken onto this line ends it, so nothing measured now can render flat over the break it forces. Asked here rather than in the scan because it is a fact about the line already written, not about the document ahead.
                            Some(_) if !state.suffix.is_empty() => false,
                            Some(width) => fits(
                                &mut inner,
                                &mut stack,
                                width.saturating_sub(state.effective_column()),
                                true,
                                state.following,
                            ),
                        };
                    stack.push(Step::Print(inner, inner_fits));
                }
                Printer::IfBreak {
                    flat: on_flat,
                    broken,
                } => {
                    let spelling = mem::take(if flat { on_flat } else { broken });
                    state.text(&spelling)?;
                }
                Printer::Mark(at) => state.reach(*at),
                Printer::Ends => state.ends(&mut stack)?,
            },
            // One item per turn: measure it against what is left of the line, emit the gap it earned, print it flat, and queue the remainder. Printing the item flat is what makes it atomic — a fill decides *between* items, never inside one.
            Step::Fill(mut items) => {
                let mut item = items.remove(0);
                // Whether the item shares the line, where a line is open for it to share.
                let placed = (!state.should_indent).then(|| {
                    let room = match state.width {
                        None => usize::MAX,
                        Some(width) => width.saturating_sub(state.effective_column() + 1),
                    };
                    // A line that owes a comment is over, whatever room it has left.
                    let owes =
                        !state.suffix.is_empty() || !state.due.is_empty() || state.after_comment;
                    // The last item is measured against what follows the fill, for the reason a group is: whatever trails it — a closing bracket, a semicolon — has no break point before it and must share its line. A middle item is measured alone, since the items still to come will push that content onto a later line anyway.
                    !owes
                        && match items.is_empty() {
                            true => fits(&mut item, &mut stack, room, true, state.following),
                            false => fits(&mut item, &mut [], room, true, state.following),
                        }
                });
                if !items.is_empty() {
                    stack.push(Step::Fill(items));
                }
                stack.push(Step::Print(item, true));
                // Written with the item already queued, so a line that ends here sees what follows it.
                match placed {
                    Some(true) => state.text(" ")?,
                    Some(false) => state.line_ends(&mut stack)?,
                    None => {}
                }
            }
            Step::Dedent => state.indent_by -= state.indent_step,
        }
    }

    // Whatever the document never reached is owed at its end: a comment written past the final character has nothing after it to bring it out. The first may ride the last line if that line is free; each after it takes a line of its own.
    if let Some((original, mut cursor)) = state.following {
        cursor.arrived = original.comments.len();
        state.following = Some((original, cursor));
        state.arrivals();
        state.pay(false)?;
    }

    // A document that ends without a newline still owes the comment riding its last line.
    if !state.suffix.is_empty() {
        let suffix = mem::take(&mut state.suffix);
        state.formatter.write_str(&suffix)?;
        state.written += suffix.len();
    }

    Ok(state.annotations)
}

/// Render `printer` to a string at `width`, with every [`Printer::Named`] and the byte range it landed on — what a consumer that links names reads, since the layout is decided here and nowhere else, so nothing has to re-lex the text to find a name in it.
pub fn render_annotated(
    printer: Printer,
    indent_step: usize,
    width: usize,
) -> (String, Vec<Annotation>) {
    struct Rendering {
        printer: RefCell<Option<Printer>>,
        indent_step: usize,
        width: usize,
        annotations: RefCell<Vec<Annotation>>,
    }
    impl fmt::Display for Rendering {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            let printer = self.printer.borrow_mut().take().expect("rendered once");
            let annotations = run(printer, formatter, self.indent_step, Some(self.width), None)?;
            *self.annotations.borrow_mut() = annotations;
            Ok(())
        }
    }

    let rendering = Rendering {
        printer: RefCell::new(Some(printer)),
        indent_step,
        width,
        annotations: RefCell::new(Vec::new()),
    };
    let text = rendering.to_string();
    (text, rendering.annotations.into_inner())
}

#[cfg(test)]
mod tests;
