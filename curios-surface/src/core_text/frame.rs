//! The frame a render threads, and the pieces every node's writer is written with: a child term behind the one guarded door, the shown tree's own nodes, a call's and a family's arguments under their marks, a telescope's parameters, a literal's text.

use {
    crate::{
        CoreForm, CoreValue, Decorated, DecoratorField, ELIDED_LITERAL_CHARACTERS, FuncParam,
        FuncTypeParam, Minting, Name, Pattern, ProofLiteral, Radix, Shown, ShownForm, Spelling,
        StrLit, StructLit, StructLitEntry, TupleField, core_written,
    },
    curios_common::{Plicity, Qualifier, Sign, is_identifier_char, recurse},
    curios_core::{
        Argument, Cursor, Free, Global, Intrinsic, Label, Scope, Subterm, Term, Three, Two,
    },
    curios_num::Natural,
    std::rc::Rc,
};

/// A term as a print shows it: the tree a render builds and the program's printer lays out.
pub type Tree = crate::Term<Shown>;

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

    /// A declaration as this render names it ([`Spelling::symbol`]): a witness by its identity where it is spelled so, and every other name as the path it is spelled by.
    pub fn global(&self, name: &Global) -> Tree {
        let spelled = self.spelling.symbol(name);
        match name {
            Global::Witness(id) if spelled == id.to_string() => {
                shown(ShownForm::Core(CoreForm::Witness(*id)))
            }
            _ => word(spelled),
        }
    }

    /// A variable by the word its binder is printed under.
    pub fn variable(&self, label: &Free) -> Tree {
        word(self.spelling.label(label))
    }
}

/// A child tree.
///
/// Every recursive call in this module goes through here, which is what makes this the one place the descent needs guarding: building a term's tree is a recursive function over a recursive structure, so it descends as deep as the term — and a diagnostic that cannot be printed is worse than no diagnostic, since it aborts the compiler while it is trying to *report* something else. [`recurse`] is what makes that depth affordable. Laying the tree out and running the document stay iterative in the printer, for the same reason at a different layer.
///
/// **And the one place a render is bounded.** A term is a graph and its print is that graph's tree: a type sixty lines of `let` built, each naming the one before it twice, has sixty-one nodes and a print no report survives writing. So a report builds a bounded number of children, the bound its [`Minting`] sets, and shows what is left as `…`: an elided report is still a report. Core text a reader reads back is written whole ([`Spelling::written_whole`](crate::Spelling::written_whole)), since an elided text reads back as no term.
pub fn sub(term: Term, frame: Frame) -> Tree {
    if !frame.spelling.writes_whole() && frame.mint.elides(&term) {
        return shown(ShownForm::Elided);
    }

    recurse(|| core_written(term, frame))
}

/// `node` as a tree.
pub fn tree(node: crate::Subterm<Shown>) -> Tree {
    Tree::from(node)
}

/// A form only a print shows, as a tree.
pub fn shown(form: ShownForm) -> Tree {
    tree(crate::Subterm::Core(form))
}

/// A token as the shown spelling writes it, which no grammar reads.
pub fn verbatim(text: impl Into<String>) -> Tree {
    shown(ShownForm::Spelled(text.into()))
}

/// The name `text` spells: a path where every segment of it is a word, absolute where it leads with `/`, and a spelled token where it is not one.
pub fn word(text: impl Into<String>) -> Tree {
    let text = text.into();
    let absolute = text.starts_with('/');
    let segments = text
        .trim_start_matches('/')
        .split('/')
        .map(str::to_string)
        .collect::<Vec<_>>();
    let words = segments.iter().all(|segment| {
        !segment.is_empty()
            && segment != "_"
            && !segment.starts_with(|character: char| character.is_ascii_digit())
            && segment.chars().all(is_identifier_char)
    });
    match words {
        true => tree(crate::Subterm::Name(Name::new(
            absolute,
            Qualifier::from(segments),
        ))),
        false => verbatim(text),
    }
}

/// A numeral nobody wrote, at its natural width, under `sign`.
pub fn numeral(magnitude: impl Into<Natural>, sign: Sign) -> Tree {
    tree(crate::Subterm::NumLit(crate::NumLit {
        magnitude: magnitude.into(),
        radix: Radix::Dec(0),
        sign,
    }))
}

/// A numeral in hexadecimal, at `digits` digits.
pub fn hexadecimal(magnitude: impl Into<Natural>, digits: usize) -> Tree {
    tree(crate::Subterm::NumLit(crate::NumLit {
        magnitude: magnitude.into(),
        radix: Radix::Hex(digits),
        sign: Sign::Unmarked,
    }))
}

/// A string literal of `text`.
pub fn string(text: impl Into<String>) -> Tree {
    tree(crate::Subterm::ProofLiteral(ProofLiteral::Str(
        StrLit::line(text),
    )))
}

/// `head` called over `arguments`.
pub fn call(head: Tree, arguments: Vec<crate::Argument<Shown>>) -> Tree {
    tree(crate::Subterm::Apply(crate::Apply { head, arguments }))
}

/// One argument under `plicity`.
pub fn argument(plicity: Plicity, term: Tree) -> crate::Argument<Shown> {
    crate::Argument { term, plicity }
}

/// Each of `terms` as an argument under no mark.
pub fn explicit(terms: Vec<Tree>) -> Vec<crate::Argument<Shown>> {
    terms
        .into_iter()
        .map(|term| argument(Plicity::Explicit, term))
        .collect()
}

/// `head` called over `terms`, each under no mark.
pub fn called(head: Tree, terms: Vec<Tree>) -> Tree {
    call(head, explicit(terms))
}

/// A variant that holds fields, as a structure's literal is written: `Implicit { func = …, binder = None }`.
pub fn literal_of(head: &str, fields: Vec<(&str, Tree)>) -> Tree {
    tree(crate::Subterm::StructLit(StructLit {
        head: Name::new(false, Qualifier::from(vec![head.to_string()])),
        params: None,
        entries: fields
            .into_iter()
            .map(|(label, value)| {
                StructLitEntry::Field(TupleField {
                    label: Some(label.to_string()),
                    func_params: None,
                    value,
                })
            })
            .collect(),
    }))
}

/// One field a decorator sets: its Core label and the value it is set to.
pub type Decoration = (&'static str, CoreValue<Shown>);

/// A term as a decorator's field holds one.
pub fn value(term: Tree) -> CoreValue<Shown> {
    CoreValue::Term(term)
}

/// A word as a decorator's field holds one, `Hole`.
pub fn word_value(text: impl Into<String>) -> CoreValue<Shown> {
    value(word(text))
}

/// A struct-valued field's value: the fields of it that differ from the template's, between braces.
pub fn nested(fields: Vec<Decoration>) -> CoreValue<Shown> {
    CoreValue::Fields(decoration_fields(fields))
}

/// A list-valued field's value: its entries between brackets, each under no mark.
pub fn bracketed(entries: Vec<CoreValue<Shown>>) -> CoreValue<Shown> {
    CoreValue::List(
        entries
            .into_iter()
            .map(|entry| (Plicity::Explicit, entry))
            .collect(),
    )
}

/// The fields a decorator states, each by its label.
pub fn decoration_fields(fields: Vec<Decoration>) -> Vec<DecoratorField<Shown>> {
    fields
        .into_iter()
        .map(|(label, value)| DecoratorField {
            label: crate::Label::from(label),
            value,
        })
        .collect()
}

impl Frame<'_> {
    /// Whether this render writes Core text proper, every field stated, or its reader's spelling.
    pub fn core_text(&self) -> bool {
        self.spelling.writes_core_text()
    }

    /// `node` under a decorator setting each of `fields` in Core text proper, and `node` as it stands in a reader's spelling, which writes no decorator, or where it sets none.
    pub fn decorated(&self, fields: Vec<Decoration>, node: Tree) -> Tree {
        match self.core_text() && !fields.is_empty() {
            true => decorated(fields, node),
            false => node,
        }
    }
}

/// `node` under a decorator setting each of `fields`.
pub fn decorated(fields: Vec<Decoration>, node: Tree) -> Tree {
    shown(ShownForm::Core(CoreForm::Decorated(Decorated {
        fields: decoration_fields(fields),
        term: node,
    })))
}

pub fn label_terms(binders: &[Free]) -> Vec<Term> {
    binders.iter().map(Term::free_var).collect()
}

/// A call's arguments, each under its mark.
pub fn call_arguments(arguments: Vec<Argument>, frame: Frame) -> Vec<crate::Argument<Shown>> {
    arguments
        .into_iter()
        .map(|each| argument(each.plicity, sub(each.term, frame)))
        .collect()
}

/// The arguments of a call as the surface writes them: the implicit ones under their mark, then the written ones.
pub fn marked_arguments(
    implicits: Vec<Term>,
    explicits: Vec<Term>,
    frame: Frame,
) -> Vec<crate::Argument<Shown>> {
    implicits
        .into_iter()
        .map(|term| argument(Plicity::Implicit, sub(term, frame)))
        .chain(
            explicits
                .into_iter()
                .map(|term| argument(Plicity::Explicit, sub(term, frame))),
        )
        .collect()
}

/// A nominal type's arguments from position `offset` of its declaration on, each marked as the declaration marks it.
pub fn nominal_arguments(
    group: Vec<Term>,
    marks: Option<&[Plicity]>,
    offset: usize,
    frame: Frame,
) -> Vec<crate::Argument<Shown>> {
    group
        .into_iter()
        .enumerate()
        .map(|(index, each)| {
            let mark = marks
                .and_then(|marks| marks.get(offset + index))
                .copied()
                .unwrap_or(Plicity::Explicit);

            argument(mark, sub(each, frame))
        })
        .collect()
}

/// A function type's parameters from `cursor` on, each written into `params` in order, then its output — every entry type under the binders before it, and a witness binder's node in scope for everything after it (axis (h)).
pub fn parameter_types(
    mut cursor: Cursor<'_, Term>,
    plicities: &[Plicity],
    frame: Frame,
    params: &mut Vec<FuncTypeParam<Shown>>,
) -> Tree {
    let Some((_, ty)) = cursor.entry() else {
        return sub(cursor.body().expect("a cursor past every entry"), frame);
    };
    let raw = cursor.label();
    let label = frame.label(raw);
    let plicity = plicities
        .get(cursor.args().len())
        .copied()
        .unwrap_or(Plicity::Explicit);
    // A hintless binder is compiler-minted (an anonymous parameter), so its label appears only when the rest of the telescope references it — `(B) -> C` renders as written, not `(#6577: B) -> C`.
    let named = match raw {
        Some(name) => name.hint().is_some() || cursor.binder_used(),
        None => false,
    };
    let typed = sub(ty.clone(), frame);
    let slot = params.len();
    params.push(FuncTypeParam {
        plicity,
        label: None,
        type_: typed,
    });
    cursor.advance(Term::free_var(&label));

    let output = parameter_types(cursor, plicities, frame, params);

    if named {
        params[slot].label = Some(frame.spelling.label(&label));
    }
    output
}

/// A lambda's parameters from `cursor` on, each written into `params`, then its body — a witness binder's node in scope for the body (axis (h)). A lambda's witness member is `use _`, as the surface writes it, and keeps a name only where the body still refers to it by one resolution would not restore: a term no source spells, shown as it is rather than with a reference to nothing. In Core text proper each parameter states its domain, under the parameters before it: a reader leaves a lambda's domains to its type, and Core text leaves nothing to an elaborator.
pub fn lambda_parameters(
    mut cursor: Cursor<'_, Term>,
    plicities: &[Plicity],
    minting: Frame,
    params: &mut Vec<FuncParam<Shown>>,
) -> Tree {
    if cursor.entry().is_none() {
        return sub(cursor.body().expect("a cursor past every entry"), minting);
    }
    let label = minting.label(cursor.label());
    let plicity = plicities
        .get(cursor.args().len())
        .copied()
        .unwrap_or(Plicity::Explicit);
    let annotation = match minting.core_text() {
        true => cursor.entry().map(|(_, domain)| sub(domain, minting)),
        false => None,
    };
    // Asked before the cursor moves past the binder it asks about.
    let used = cursor.binder_used();
    let named = label.hint().is_some() || used;
    let spelled = minting.spelling.label(&label);
    let slot = params.len();
    params.push(FuncParam {
        plicity,
        pattern: Pattern::Binder(crate::Label::from(String::new())),
        annotation,
    });
    cursor.advance(Term::free_var(&label));

    let output = lambda_parameters(cursor, plicities, minting, params);
    let named = match plicity == Plicity::Witness {
        true => used,
        false => named,
    };

    params[slot].pattern = Pattern::Binder(crate::Label::from(match named {
        true => spelled,
        false => "_".to_string(),
    }));
    output
}

/// A certified string as the literal it stands for — `"body"` in place of the struct over its bytes and the proof certifying them (axis (f)). `None` unless the spelling was told which declaration that is and the head is it, and `None` for a `Str` whose bytes are not one constant — a concatenation, a slice, a binder's — or are not text, each of which has nothing shorter to say than its own spelling.
pub fn string_literal(name: &Global, fields: &[Term], spelling: &Rc<Spelling>) -> Option<Tree> {
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
        return Some(verbatim(format!(
            "\"{}…\" ({} more characters)",
            escaped(&head),
            shown - ELIDED_LITERAL_CHARACTERS
        )));
    }

    Some(string(text))
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
