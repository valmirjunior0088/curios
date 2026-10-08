//! The document type: what to emit, as data, plus the iterative destructor its depth demands.

use std::mem;

/// A pretty-printing document: what to emit, as data.
///
/// # Why this is an enum and not a closure
///
/// A document made of nested closures would run each child inside its parent's stack frame, so printing would recurse as deep as the document nests and a deep enough term would abort the compiler instead of printing. A diagnostic that cannot be printed is worse than no diagnostic, and no reduction budget can prevent it, because depth is not steps.
///
/// As data, the document is walked by [`run_printer`](crate::run_printer)'s explicit stack and nests without bound. No derives: `Debug`, `Clone`, and `PartialEq` would each walk the tree recursively and overflow on the documents this type exists to print.
pub enum Printer {
    /// Literal text. Newlines inside it arm the pending-indent logic, so a multi-line literal indents correctly under [`indent`](crate::indent).
    Text(String),
    /// Literal text that names something: emitted exactly as [`Printer::Text`], and additionally reported — with the output range it landed on — to a renderer collecting names through [`render_annotated`](crate::render_annotated), so a consumer can attach a link to a name without re-lexing the text. Every other renderer treats it as text, and it measures as text.
    Named(String),
    /// Emitted in order.
    Concat(Vec<Printer>),
    /// Emitted one indentation level deeper.
    Indent(Box<Printer>),
    /// A document not built yet.
    ///
    /// The one closure variant, and it earns its place: a printer for a recursive IR is written as a recursive function, so *building* a document descends as deep as the term even though [`run_printer`](crate::run_printer) does not. Deferring a child turns that descent into work on the interpreter's stack — the builder is called when the interpreter reaches it, from a frame one deep rather than `n`.
    ///
    /// `Option` so the interpreter can take the thunk out: a type with a `Drop` impl cannot have a field moved away.
    Deferred(Option<Box<dyn FnOnce() -> Printer>>),
    /// A layout choice point: `flat` when the enclosing [`Printer::Group`] renders on one line, a newline (plus pending indentation) when it breaks — or unconditionally when no group encloses it, since the top of a document is broken context. `hard` marks a mandatory break: it always emits a newline, and the fits scan fails on it, so no group containing one ever renders flat.
    Line { flat: String, hard: bool },
    /// The width-adaptive *sequence*: its items are laid out left to right, and each gap independently becomes a space or a newline according to whether the item after it still fits the line.
    ///
    /// A [`Printer::Group`] is all-or-nothing — when its flat form does not fit, every [`Printer::Line`] inside it breaks — which is right for a structure whose parts belong on separate lines once any of them does. It is wrong for a run of short interchangeable items: a twenty-name import does not fit on one line, so a group puts each name on a line of its own and spends twenty lines on what two would hold. Each item carries its own separator, so this variant inserts only the gap and never invents punctuation.
    ///
    /// The gaps are decided while printing and owe nothing to the mode the fill was reached in: a fill inside a *flat* group still wraps. That is why `fits` reads a fill that runs out of room as the line ending rather than as the scan failing — any other reading would let a group render flat over a fill that then wrapped underneath it.
    Fill(Vec<Printer>),
    /// The width-adaptive unit: renders flat — every enclosed [`Printer::Line`] as its flat spelling — when its flat form fits the room left on the line, and broken otherwise. Nested groups measured inside a fitting parent render flat with it; under a broken parent each decides for itself. With no width configured every group fits.
    Group(Box<Printer>),
    /// Mode-dependent text that is *not* itself a break point: `flat` under a fitting group, `broken` under a broken one. The formatter's broken-only trailing comma is `IfBreak { flat: "", broken: "," }`. Measured at its flat spelling, so it never breaks a group by itself.
    IfBreak { flat: String, broken: String },
    /// A zero-width note that the document holds its original up to this offset, for a renderer following that text — see [`Original`](crate::Original).
    ///
    /// The renderer finds its own way through the original, a character at a time, and needs no note to do it. This is for the one place it cannot: where the document spells something another way than it was written — a literal respelled from its value — the characters stop agreeing and the renderer waits, and the note at the far end of what was respelled is where it picks the text up again. Emits nothing and measures as nothing, so a document carrying none renders as it would without them.
    Mark(usize),
}

impl Printer {
    /// This document's children, taken out and left childless.
    ///
    /// Shared by [`run_printer`](crate::run_printer) and [`Drop`], which both need to descend without moving a field out of a type that has a destructor.
    pub(crate) fn take(&mut self) -> Printer {
        mem::replace(self, Printer::Text(String::new()))
    }

    /// Whether this node's children are already gone, so dropping it cannot reach another node.
    fn is_dismantled(&self) -> bool {
        match self {
            Printer::Text(_) | Printer::Named(_) | Printer::Mark(_) => true,
            Printer::Concat(parts) | Printer::Fill(parts) => parts.is_empty(),
            Printer::Indent(inner) | Printer::Group(inner) => {
                matches!(**inner, Printer::Text(_))
            }
            // Holds a thunk, never a child document.
            Printer::Deferred(_) => true,
            Printer::Line { .. } => true,
            Printer::IfBreak { .. } => true,
        }
    }
}

/// Dismantled with an explicit stack, for the reason the type exists.
///
/// A document nests as deep as the term it prints, and the *derived* drop recurses one native frame per level — so a document deep enough to need an iterative [`run_printer`](crate::run_printer) would abort while being freed instead; `a_deep_document_is_freed_without_recursing` drops one 100 000 deep.
impl Drop for Printer {
    fn drop(&mut self) {
        // The base case is "already dismantled", not "is a leaf", and the difference is not cosmetic: taking a node's children leaves a husk that is still a `Concat` or an `Indent`, so a check for `Text` alone sends every husk back through here to make another husk, forever.
        if self.is_dismantled() {
            return;
        }

        let mut pending = Vec::from([self.take()]);

        while let Some(mut printer) = pending.pop() {
            match &mut printer {
                Printer::Text(_) | Printer::Named(_) => {}
                Printer::Concat(parts) | Printer::Fill(parts) => pending.extend(mem::take(parts)),
                Printer::Indent(inner) | Printer::Group(inner) => pending.push(inner.take()),
                Printer::Deferred(_) => {}
                Printer::Line { .. } => {}
                Printer::IfBreak { .. } => {}
                Printer::Mark(_) => {}
            }
            // `printer` is dismantled now, so its own drop returns at once.
        }
    }
}
