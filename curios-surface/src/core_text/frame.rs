//! The frame a render threads, and the pieces every node's writer is written with: a child term behind the one guarded door, a delimited list, a call's and a family's arguments under their marks, a telescope's parameters, a literal's text.

use {
    crate::{ELIDED_LITERAL_CHARACTERS, Minting, Spelling, core_written},
    curios_common::{Plicity, recurse},
    curios_core::{
        Argument, Cursor, Free, Global, Intrinsic, Label, Scope, Subterm, Term, Three, Two,
    },
    curios_print::{Printer, flat, group, hard_line, indent, line, pure, sep_flat, soft_line},
    std::rc::Rc,
};

/// What an elided subterm prints as: no token of the language, so no reader takes an elided print for the term.
pub const ELIDED: &str = "…";

/// The state a recursive print call threads: the render-constant [`Spelling`] and the render's [`Minting`].
#[derive(Clone, Copy)]
pub struct Frame<'a> {
    pub spelling: &'a Rc<Spelling>,
    pub mint: &'a Minting,
}

impl<'a> Frame<'a> {
    /// The label the next binder this render opens is printed under ([`Minting::label`]).
    pub fn label(&self, label: Option<&Label>) -> Free {
        self.mint.label(label)
    }

    /// Every binder of a scope, labelled in order.
    pub fn labels<'b>(&self, binders: impl Iterator<Item = Option<&'b Label>>) -> Vec<Free> {
        binders.map(|label| self.label(label)).collect()
    }

    /// Open a two-binder scope under minted labels.
    pub fn open_two(&self, scope: Scope<Two>) -> ((Free, Free), Term) {
        let fst = self.label(scope.label(0));
        let snd = self.label(scope.label(1));
        let body = scope.open(&[&Term::free_var(&fst), &Term::free_var(&snd)]);

        ((fst, snd), body)
    }

    /// The three-binder counterpart of [`Frame::open_two`].
    pub fn open_three(&self, scope: Scope<Three>) -> ((Free, Free, Free), Term) {
        let fst = self.label(scope.label(0));
        let snd = self.label(scope.label(1));
        let thd = self.label(scope.label(2));
        let body = scope.open(&[
            &Term::free_var(&fst),
            &Term::free_var(&snd),
            &Term::free_var(&thd),
        ]);

        ((fst, snd, thd), body)
    }
}

/// A child document.
///
/// Every recursive call in this module goes through here, which is what makes this the one place the descent needs guarding: printing a term is a recursive function over a recursive structure, so building the document descends as deep as the term — and a diagnostic that cannot be printed is worse than no diagnostic, since it aborts the compiler while it is trying to *report* something else. [`recurse`] is what makes that depth affordable. Running and freeing the finished document stay iterative in [`Printer`] itself, for the same reason at a different layer.
///
/// **And the one place a render is bounded.** A term is a graph and its print is that graph's tree: a type sixty lines of `let` built, each naming the one before it twice, has sixty-one nodes and a print no report survives writing. So a report builds a bounded number of child documents, the bound its [`Minting`] sets, and prints what is left as `…`: an elided report is still a report. Core text a reader reads back is written whole ([`Spelling::written_whole`](crate::Spelling::written_whole)), since an elided text reads back as no term.
pub fn sub(term: Term, frame: Frame) -> Printer {
    if !frame.spelling.writes_whole() && frame.mint.elides(&term) {
        return pure(ELIDED);
    }

    recurse(|| core_written(term, frame))
}

/// A delimited comma-list that fits on one line or breaks one item per line, indented — `f(a, b)` against `f(\n  a,\n  b\n)`. `spaced` spells the flat padding inside the delimiters: `false` for parenthesized lists, `true` for brace literals (`S { a, b }`). Behavior-neutral on the unbounded `Display` path, where every group renders flat.
pub fn listed(open: String, spaced: bool, items: Vec<Printer>, close: &'static str) -> Printer {
    let lead = if spaced { line } else { soft_line };
    group(flat([
        pure(open),
        indent(flat([
            lead(),
            sep_flat(items, || flat([pure(","), line()])),
        ])),
        lead(),
        pure(close),
    ]))
}

pub fn label_terms(binders: &[Free]) -> Vec<Term> {
    binders.iter().map(Term::free_var).collect()
}

/// The prefix a plicity marks its binder or argument with — `@` for implicit, `use ` for witness, nothing for explicit.
pub fn plicity_mark(plicity: Option<&Plicity>) -> &'static str {
    match plicity {
        Some(Plicity::Implicit) => "@",
        Some(Plicity::Witness) => "use ",
        _ => "",
    }
}

/// One argument of an applied nominal family, marked as its declaration wrote it. Indices are always explicit, so only a parameter ever takes a mark.
fn marked_argument(printer: Printer, plicity: Option<&Plicity>) -> Printer {
    match plicity_mark(plicity) {
        "" => printer,
        mark => flat([pure(mark), printer]),
    }
}

/// A call's arguments, each under its mark.
pub fn call_arguments(arguments: Vec<Argument>, frame: Frame) -> Vec<Printer> {
    arguments
        .into_iter()
        .map(|argument| marked_argument(sub(argument.term, frame), Some(&argument.plicity)))
        .collect()
}

/// The arguments of a call as the surface writes them: the implicit ones under their mark, then the written ones.
pub fn marked_arguments(implicits: Vec<Term>, explicits: Vec<Term>, frame: Frame) -> Vec<Printer> {
    implicits
        .into_iter()
        .map(|term| marked_argument(sub(term, frame), Some(&Plicity::Implicit)))
        .chain(explicits.into_iter().map(|term| sub(term, frame)))
        .collect()
}

/// A nominal type's arguments from position `offset` of its declaration on, each marked as the declaration marks it.
pub fn nominal_arguments(
    group: Vec<Term>,
    marks: Option<&[Plicity]>,
    offset: usize,
    frame: Frame,
) -> Vec<Printer> {
    group
        .into_iter()
        .enumerate()
        .map(|(index, argument)| {
            let mark = marks.and_then(|marks| marks.get(offset + index));

            marked_argument(sub(argument, frame), mark)
        })
        .collect()
}

/// A function type's parameters from `cursor` on, each rendered into `printers` in order, then its output — every entry type under the binders before it, and a witness binder's node in scope for everything after it (axis (h)).
pub fn parameter_types(
    mut cursor: Cursor<'_, Term>,
    plicities: &[Plicity],
    frame: Frame,
    printers: &mut Vec<Printer>,
) -> Printer {
    let Some((_, ty)) = cursor.entry() else {
        return sub(cursor.body().expect("a cursor past every entry"), frame);
    };
    let raw = cursor.label();
    let label = frame.label(raw);
    let plicity = plicities.get(cursor.args().len());
    let mark = plicity_mark(plicity);
    // A hintless binder is compiler-minted (an anonymous parameter), so its label appears only when the rest of the telescope references it — `(B) -> C` renders as written, not `(#6577: B) -> C`.
    let named = match raw {
        Some(name) => name.hint().is_some() || cursor.binder_used(),
        None => false,
    };
    let typed = sub(ty.clone(), frame);
    let slot = printers.len();
    printers.push(pure(""));
    cursor.advance(Term::free_var(&label));

    let output = parameter_types(cursor, plicities, frame, printers);

    printers[slot] = if named {
        flat([
            pure(mark),
            pure(frame.spelling.label(&label)),
            pure(": "),
            typed,
        ])
    } else {
        flat([pure(mark), typed])
    };
    output
}

/// A lambda's parameters from `cursor` on, their spellings pushed onto `marked`, then its body indented under them — a witness binder's node in scope for the body (axis (h)). A lambda's witness member is `use _`, as the surface writes it, and keeps a name only where the body still refers to it by one resolution would not restore: a term no source spells, shown as it is rather than with a reference to nothing. In Core text proper each parameter's domain is pushed onto `domains`, under the parameters before it: a reader leaves a lambda's domains to its type, and Core text leaves nothing to an elaborator.
pub fn lambda_parameters(
    mut cursor: Cursor<'_, Term>,
    plicities: &[Plicity],
    minting: Frame,
    marked: &mut Vec<String>,
    domains: &mut Vec<Printer>,
) -> Printer {
    if cursor.entry().is_none() {
        let body = cursor.body().expect("a cursor past every entry");
        // The body sits on the arrow's line when it fits and indents on its own line when it does not. A body that is a multi-line form of its own takes the line unconditionally: those forms spell their breaks as literal newlines, which end the fits scan within budget rather than failing it, so a group would render the arrow's line flat and leave the form's first line trailing the arrow.
        let separator = match &*body {
            Subterm::Match(_) | Subterm::Let(_) | Subterm::Rec(_) => hard_line(),
            _ => line(),
        };
        return indent(flat([separator, sub(body, minting)]));
    }
    let label = minting.label(cursor.label());
    let plicity = plicities.get(cursor.args().len());
    let mark = plicity_mark(plicity);
    if minting.core_text()
        && let Some((_, domain)) = cursor.entry()
    {
        domains.push(sub(domain, minting));
    }
    // Asked before the cursor moves past the binder it asks about.
    let used = cursor.binder_used();
    let named = label.hint().is_some() || used;
    let spelled = minting.spelling.label(&label);
    let slot = marked.len();
    marked.push(String::new());
    cursor.advance(Term::free_var(&label));

    let output = lambda_parameters(cursor, plicities, minting, marked, domains);
    let named = match plicity == Some(&Plicity::Witness) {
        true => used,
        false => named,
    };

    marked[slot] = match named {
        true => format!("{mark}{spelled}"),
        false => format!("{mark}_"),
    };
    output
}

/// A certified string as the literal it stands for — `"body"` in place of the struct over its bytes and the proof certifying them (axis (f)). `None` unless the spelling was told which declaration that is and the head is it, and `None` for a `Str` whose bytes are not one constant — a concatenation, a slice, a binder's — or are not text, each of which has nothing shorter to say than its own spelling.
pub fn string_literal(name: &Global, fields: &[Term], spelling: &Rc<Spelling>) -> Option<String> {
    if spelling.string_declaration() != Some(name) {
        return None;
    }

    let Subterm::Intrinsic(Intrinsic::Bytes(packed)) = &**fields.first()? else {
        return None;
    };

    let text = String::from_utf8(packed.to_bytes()).ok()?;

    // A diagnostic quoting a page of text whole says nothing a prefix does not, and a report is where this spelling is read: the rest is counted rather than shown.
    let shown = text.chars().count();
    if shown > ELIDED_LITERAL_CHARACTERS {
        let head: String = text.chars().take(ELIDED_LITERAL_CHARACTERS).collect();
        return Some(format!(
            "\"{}…\" ({} more characters)",
            escaped(&head),
            shown - ELIDED_LITERAL_CHARACTERS
        ));
    }

    Some(format!("\"{}\"", escaped(&text)))
}

/// A string's characters under the surface's escapes, so a rendered literal reads back as the one it came from.
pub fn escaped(text: &str) -> String {
    text.chars()
        .map(|character| match character {
            '\\' => "\\\\".to_string(),
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\t' => "\\t".to_string(),
            '\r' => "\\r".to_string(),
            other => other.to_string(),
        })
        .collect()
}

/// A character rendered as its literal body: the five escapes by their spellings, anything else verbatim.
pub fn escape_character(character: char) -> String {
    match character {
        '\n' => "\\n".to_string(),
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        '\\' => "\\\\".to_string(),
        '\'' => "\\'".to_string(),
        other => other.to_string(),
    }
}
