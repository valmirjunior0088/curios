//! The frame a render threads, and the pieces every node's writer is written with: a child term behind the one guarded door, the shown tree's own nodes, a call's and a family's arguments under their marks, a telescope's parameters, a literal's text.

use {
    crate::{
        Binder, CoreForm, CoreValue, Decorated, DecoratorField, ELIDED_LITERAL_CHARACTERS,
        FuncParam, FuncTypeParam, Literal, Minting, Name, Occurrence, Pattern, ProofLiteral, Radix,
        Rendered, Shown, Spelling, StrLit, StructLit, StructLitEntry, TupleField, core_written,
    },
    curios_common::{Plicity, Qualifier, Sign, is_identifier_char, recurse},
    curios_core::{
        Argument, Cursor, Free, Global, Intrinsic, Label, One, Scope, Subterm, Term, Two,
    },
    curios_num::Natural,
    std::{marker::PhantomData, rc::Rc},
};

/// What an elided subterm prints as: no token of the language, so no reader takes an elided print for the term.
pub const ELIDED: &str = "…";

/// The state a recursive print call threads: the render-constant [`Spelling`] and the render's [`Minting`], and the dialect of the tree it builds — a shown one, or Core text's own where a reader reads the tree back.
pub struct Frame<'a, D: Rendered = Shown> {
    pub spelling: &'a Rc<Spelling>,
    pub mint: &'a Minting,
    /// The dialect of the tree the render builds; [`Frame::new`] makes a frame, holding Core text's own tree to Core text proper.
    pub dialect: PhantomData<fn() -> D>,
}

impl<D: Rendered> Clone for Frame<'_, D> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<D: Rendered> Copy for Frame<'_, D> {}

impl<'a, D: Rendered> Frame<'a, D> {
    /// A render's frame over `spelling` and `mint`. Core text's own tree is written in Core text proper, which is all it holds.
    pub fn new(spelling: &'a Rc<Spelling>, mint: &'a Minting) -> Self {
        assert!(
            D::leave().is_some() || spelling.writes_core_text(),
            "Core text's own tree is written in Core text proper"
        );
        Self {
            spelling,
            mint,
            dialect: PhantomData,
        }
    }

    /// The label the next binder this render opens is printed under ([`Minting::label`]).
    pub fn label(&self, label: Option<&Label>) -> Free {
        self.mint.label(label)
    }

    /// Every binder of a scope, labelled in order.
    pub fn labels<'b>(&self, binders: impl Iterator<Item = Option<&'b Label>>) -> Vec<Free> {
        binders.map(|label| self.label(label)).collect()
    }

    /// Open a one-binder scope under a minted label.
    pub fn open_one(&self, scope: Scope<One>) -> (Free, Term) {
        let only = self.label(scope.label(0));
        let body = scope.open(&[&Term::free_var(&only)]);

        (only, body)
    }

    /// Open a two-binder scope under minted labels.
    pub fn open_two(&self, scope: Scope<Two>) -> ((Free, Free), Term) {
        let fst = self.label(scope.label(0));
        let snd = self.label(scope.label(1));
        let body = scope.open(&[&Term::free_var(&fst), &Term::free_var(&snd)]);

        ((fst, snd), body)
    }

    /// A declaration as this render names it ([`Spelling::symbol`]): a witness by its identity where it is spelled so, and every other name as the path it is spelled by.
    pub fn global(&self, name: &Global) -> crate::Term<D> {
        let spelled = self.spelling.symbol(name);
        match name {
            Global::Witness(id) if spelled == id.to_string() => core_form(CoreForm::Witness(*id)),
            _ => word(spelled),
        }
    }

    /// A variable by the word its binder is printed under.
    pub fn variable(&self, label: &Free) -> crate::Term<D> {
        word(self.spelling.label(label))
    }

    /// The leave to write a reader's spelling, where this render writes one: Core text proper holds none, and neither does a tree a reader reads back, whatever spelling it is handed.
    pub fn reader(&self) -> Option<D::Leave> {
        match self.spelling.writes_core_text() {
            true => None,
            false => D::leave(),
        }
    }
}

/// A child tree.
///
/// Every recursive call in this module goes through here, which is what makes this the one place the descent needs guarding: building a term's tree is a recursive function over a recursive structure, so it descends as deep as the term — and a diagnostic that cannot be printed is worse than no diagnostic, since it aborts the compiler while it is trying to *report* something else. [`recurse`] is what makes that depth affordable. Laying the tree out and running the document stay iterative in the printer, for the same reason at a different layer.
///
/// **And the one place a render is bounded.** A term is a graph and its print is that graph's tree: a type sixty lines of `let` built, each naming the one before it twice, has sixty-one nodes and a print no report survives writing. So a report builds a bounded number of children, the bound its [`Minting`] sets, and shows what is left as `…`: an elided report is still a report. Core text a reader reads back is written whole, as Core text's own tree ([`Spelling::written_whole`](crate::Spelling::written_whole)), which has no `…` to write: an elided text reads back as no term. The shown tree a rename's dry run builds for it elides nothing either, so the two meet the same binders.
///
/// **And the one place a node met again is named**, under [`Spelling::with_sharing`]: its first occurrence under `#{share = k}` and each later one as `#{shared = k} _`, which is not descended into ([`Occurrence`]).
pub fn sub<D: Rendered>(term: Term, frame: Frame<'_, D>) -> crate::Term<D> {
    if let Some(leave) = D::leave()
        && !frame.spelling.writes_whole()
        && frame.mint.elides(&term)
    {
        return tree(crate::Subterm::Core(D::elided(leave)));
    }

    match frame.mint.occurrence(&term, frame.spelling) {
        Occurrence::Written => recurse(|| core_written(term, frame)),
        Occurrence::Named(key) => named(key, recurse(|| core_written(term, frame))),
        Occurrence::Again(key) => decorated(
            vec![("shared", shared_key(key))],
            tree(crate::Subterm::Held),
        ),
    }
}

/// `written` named `key`: the field joins the decorator `written` is under, where it is under one, since a node's fields are stated together.
fn named<D: Rendered>(key: u64, written: crate::Term<D>) -> crate::Term<D> {
    if let crate::Subterm::Core(form) = written.as_subterm()
        && let Some(CoreForm::Decorated(Decorated { fields, term })) = D::form_core(form)
    {
        let mut fields = fields.clone();
        fields.extend(decoration_fields(vec![("share", shared_key(key))]));
        return core_form(CoreForm::Decorated(Decorated {
            fields,
            term: term.clone(),
        }));
    }

    decorated(vec![("share", shared_key(key))], written)
}

/// A shared node's key, as a decorator writes it.
fn shared_key<D: Rendered>(key: u64) -> CoreValue<D> {
    value(numeral(key, Sign::Unmarked))
}

/// `node` as a tree.
pub fn tree<D: Rendered>(node: crate::Subterm<D>) -> crate::Term<D> {
    crate::Term::from(node)
}

/// A form of Core text, as a tree.
pub fn core_form<D: Rendered>(form: CoreForm<D>) -> crate::Term<D> {
    tree(crate::Subterm::Core(D::core(form)))
}

/// A token as the shown spelling writes it, which no grammar reads.
pub fn verbatim<D: Rendered>(leave: D::Leave, text: impl Into<String>) -> crate::Term<D> {
    tree(crate::Subterm::Core(D::spelled(leave, text.into())))
}

/// The name `text` spells: a path where every segment of it is a word, absolute where it leads with `/`, and a spelled token where it is not one. A tree a reader reads back holds no such token, so a text that is no path of words is refused there as the compiler's own fault.
pub fn word<D: Rendered>(text: impl Into<String>) -> crate::Term<D> {
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
        false => match D::leave() {
            Some(leave) => verbatim(leave, text),
            None => panic!("Core text names `{text}`, which no reader reads as a name"),
        },
    }
}

/// A numeral nobody wrote, at its natural width, under `sign`.
pub fn numeral<D: Rendered>(magnitude: impl Into<Natural>, sign: Sign) -> crate::Term<D> {
    tree(crate::Subterm::NumLit(crate::NumLit {
        magnitude: magnitude.into(),
        radix: Radix::Dec(0),
        sign,
    }))
}

/// A numeral in hexadecimal, at `digits` digits.
pub fn hexadecimal<D: Rendered>(magnitude: impl Into<Natural>, digits: usize) -> crate::Term<D> {
    tree(crate::Subterm::NumLit(crate::NumLit {
        magnitude: magnitude.into(),
        radix: Radix::Hex(digits),
        sign: Sign::Unmarked,
    }))
}

/// A string literal of `text`.
pub fn string<D: Rendered>(text: impl Into<String>) -> crate::Term<D> {
    tree(crate::Subterm::ProofLiteral(ProofLiteral::Str(
        StrLit::line(text),
    )))
}

/// `head` called over `arguments`.
pub fn call<D: Rendered>(
    head: crate::Term<D>,
    arguments: Vec<crate::Argument<D>>,
) -> crate::Term<D> {
    tree(crate::Subterm::Apply(crate::Apply { head, arguments }))
}

/// One argument under `plicity`.
pub fn argument<D: Rendered>(plicity: Plicity, term: crate::Term<D>) -> crate::Argument<D> {
    crate::Argument { term, plicity }
}

/// Each of `terms` as an argument under no mark.
pub fn explicit<D: Rendered>(terms: Vec<crate::Term<D>>) -> Vec<crate::Argument<D>> {
    terms
        .into_iter()
        .map(|term| argument(Plicity::Explicit, term))
        .collect()
}

/// `head` called over `terms`, each under no mark.
pub fn called<D: Rendered>(head: crate::Term<D>, terms: Vec<crate::Term<D>>) -> crate::Term<D> {
    call(head, explicit(terms))
}

/// A variant that holds fields, as a structure's literal is written: `Implicit { func = …, binder = None }`.
pub fn literal_of<D: Rendered>(head: &str, fields: Vec<(&str, crate::Term<D>)>) -> crate::Term<D> {
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
pub type Decoration<D = Shown> = (&'static str, CoreValue<D>);

/// A term as a decorator's field holds one.
pub fn value<D: Rendered>(term: crate::Term<D>) -> CoreValue<D> {
    CoreValue::Term(term)
}

/// A word as a decorator's field holds one, `Hole`.
pub fn word_value<D: Rendered>(text: impl Into<String>) -> CoreValue<D> {
    value(word(text))
}

/// A struct-valued field's value: the fields of it that differ from the template's, between braces.
pub fn nested<D: Rendered>(fields: Vec<Decoration<D>>) -> CoreValue<D> {
    CoreValue::Fields(decoration_fields(fields))
}

/// A list-valued field's value: its entries between brackets.
pub fn bracketed<D: Rendered>(entries: Vec<CoreValue<D>>) -> CoreValue<D> {
    CoreValue::List(entries)
}

/// The fields a decorator states, each by its label.
pub fn decoration_fields<D: Rendered>(fields: Vec<Decoration<D>>) -> Vec<DecoratorField<D>> {
    fields
        .into_iter()
        .map(|(label, value)| DecoratorField {
            label: crate::Label::from(label),
            value,
        })
        .collect()
}

impl<D: Rendered> Frame<'_, D> {
    /// Whether this render writes Core text proper, every field stated, or its reader's spelling.
    pub fn core_text(&self) -> bool {
        self.reader().is_none()
    }

    /// What a whole write states beside a binder's word of the label Core keeps for it, each field by the name the label gives it: where the binder sits among its declaration's written binders, and that it binds a proof, each where the label says so. Nothing in any other render, which names a binder by its word alone.
    pub fn label_fields(&self, label: Option<&Label>) -> Vec<DecoratorField<D>> {
        let Some(label) = label.filter(|_| self.spelling.writes_whole()) else {
            return Vec::new();
        };
        let mut fields = Vec::new();
        if let Some(written) = label.written() {
            fields.push(("written", value(numeral(written, Sign::Unmarked))));
        }
        if label.binds_proof() {
            fields.push((
                "proof",
                value(tree(crate::Subterm::Literal(Literal::Bool(true)))),
            ));
        }

        decoration_fields(fields)
    }

    /// `node` under a decorator setting each of `fields` in Core text proper, and `node` as it stands in a reader's spelling, which writes no decorator, or where it sets none.
    pub fn decorated(&self, fields: Vec<Decoration<D>>, node: crate::Term<D>) -> crate::Term<D> {
        match self.core_text() && !fields.is_empty() {
            true => decorated(fields, node),
            false => node,
        }
    }
}

/// `node` under a decorator setting each of `fields`.
pub fn decorated<D: Rendered>(fields: Vec<Decoration<D>>, node: crate::Term<D>) -> crate::Term<D> {
    core_form(CoreForm::Decorated(Decorated {
        fields: decoration_fields(fields),
        term: node,
    }))
}

pub fn label_terms(binders: &[Free]) -> Vec<Term> {
    binders.iter().map(Term::free_var).collect()
}

/// A call's arguments, each under its mark.
pub fn call_arguments<D: Rendered>(
    arguments: Vec<Argument>,
    frame: Frame<'_, D>,
) -> Vec<crate::Argument<D>> {
    arguments
        .into_iter()
        .map(|each| argument(each.plicity, sub(each.term, frame)))
        .collect()
}

/// The arguments of a call as the surface writes them: the implicit ones under their mark, then the written ones.
pub fn marked_arguments<D: Rendered>(
    implicits: Vec<Term>,
    explicits: Vec<Term>,
    frame: Frame<'_, D>,
) -> Vec<crate::Argument<D>> {
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
pub fn nominal_arguments<D: Rendered>(
    group: Vec<Term>,
    marks: Option<&[Plicity]>,
    offset: usize,
    frame: Frame<'_, D>,
) -> Vec<crate::Argument<D>> {
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
pub fn parameter_types<D: Rendered>(
    mut cursor: Cursor<'_, Term>,
    plicities: &[Plicity],
    frame: Frame<'_, D>,
    params: &mut Vec<FuncTypeParam<D>>,
) -> crate::Term<D> {
    let Some((_, ty)) = cursor.entry() else {
        return sub(cursor.body().expect("a cursor past every entry"), frame);
    };
    let raw = cursor.label();
    let label = frame.label(raw);
    let core = frame.label_fields(raw);
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
    // What a decorator states of a binder stands before a word, so an unnamed one that states something is written as `_`, which reads back as no hint.
    let word = (!named && !core.is_empty()).then(|| "_".to_string());
    params.push(FuncTypeParam {
        plicity,
        core,
        label: word,
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
pub fn lambda_parameters<D: Rendered>(
    mut cursor: Cursor<'_, Term>,
    plicities: &[Plicity],
    minting: Frame<'_, D>,
    params: &mut Vec<FuncParam<D>>,
) -> crate::Term<D> {
    if cursor.entry().is_none() {
        return sub(cursor.body().expect("a cursor past every entry"), minting);
    }
    let label = minting.label(cursor.label());
    let core = minting.label_fields(cursor.label());
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
        pattern: Pattern::Binder(crate::Label::from(String::new()).into()),
        annotation,
    });
    cursor.advance(Term::free_var(&label));

    let output = lambda_parameters(cursor, plicities, minting, params);
    let named = match plicity == Plicity::Witness {
        true => used,
        false => named,
    };

    params[slot].pattern = Pattern::Binder(Binder {
        core,
        label: crate::Label::from(match named {
            true => spelled,
            false => "_".to_string(),
        }),
    });
    output
}

/// A certified string as the literal it stands for — `"body"` in place of the struct over its bytes and the proof certifying them (axis (f)). `None` unless the spelling was told which declaration that is and the head is it, and `None` for a `Str` whose bytes are not one constant — a concatenation, a slice, a binder's — or are not text, each of which has nothing shorter to say than its own spelling.
pub fn string_literal<D: Rendered>(
    name: &Global,
    fields: &[Term],
    spelling: &Rc<Spelling>,
) -> Option<crate::Term<D>> {
    if spelling.string_declaration() != Some(name) {
        return None;
    }

    let Subterm::Intrinsic(Intrinsic::Bytes(packed)) = &**fields.first()? else {
        return None;
    };

    let text = String::from_utf8(packed.to_bytes()).ok()?;

    // A diagnostic quoting a page of text whole says nothing a prefix does not, and a report is where this spelling is read: the rest is counted rather than shown.
    let shown = text.chars().count();
    if let Some(leave) = D::leave()
        && shown > ELIDED_LITERAL_CHARACTERS
    {
        let head: String = text.chars().take(ELIDED_LITERAL_CHARACTERS).collect();
        return Some(verbatim(
            leave,
            format!(
                "\"{}…\" ({} more characters)",
                escaped(&head),
                shown - ELIDED_LITERAL_CHARACTERS
            ),
        ));
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
