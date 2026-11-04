//! A nominal declaration in its form: a family's, a record's and a concept's.
//!
//! **A family is `induct /F(params): (indices) -> pub Sort`, a case to a line, `| tag(payload): (targets)`, and `end`**; a record is `struct /R(params): pub Sort { fields }` and a concept `concept /C(params): pub Sort { fields }`, a superclass edge among a concept's fields written `use T`. A form builds its declaration's entry and its former ([`curios_core::Definition::former`]), and a family's its constructors ([`curios_core::Definition::constructors`]), so the module's items those make are written nowhere else. The inner `pub` is the entry's `rep_public`.
//!
//! **Each binder is written by its word, under its mark**, `@n: Nat`, or as its type alone where nothing reads it, and a later type reads it by that word: a family's cases read its parameters by the head's words. A case's parameters are its family's, each hidden as a value constructor takes it, and a case whose parameters are not states them as its signature holds them, under their marks: `| #{params = [@Type, use /Show(A)]} tag(…)`.

use {
    super::{Decoration, bracketed},
    crate::{Frame, listed, plicity_mark, sub},
    curios_common::Plicity,
    curios_core::{
        Atom, Bound, Cursor, Free, Global, InductDecl, InductParam, StructDecl, Telescope, Term,
    },
    curios_print::{Printer, flat, pure},
};

/// One binder of a declaration's telescope: its mark, its type, the variable a later type reads it by, and whether it has a hint or the rest of its own telescope reads it.
pub struct Binder {
    pub mark: Plicity,
    pub type_: Term,
    pub label: Free,
    pub variable: Term,
    pub named: bool,
}

impl Binder {
    /// Whether this binder is read as a value: in place of a superclass edge's word, which resolution reads it by, its word.
    fn variable_is(&self, term: &Term) -> bool {
        term.mentions_free(&self.label)
    }

    /// This binder under its mark: by its word where it has a hint or `read` says something reads it, and as its type alone otherwise.
    pub fn written(&self, read: bool, frame: Frame) -> Printer {
        let typed = sub(self.type_.clone(), frame);
        match self.named || read {
            true => flat([
                pure(plicity_mark(Some(&self.mark))),
                pure(frame.spelling.label(&self.label)),
                pure(": "),
                typed,
            ]),
            false => flat([pure(plicity_mark(Some(&self.mark))), typed]),
        }
    }
}

/// The entries of a telescope from `cursor` to its end, each opened at the variable of a word of its own.
pub fn binders<B: Bound>(cursor: &mut Cursor<'_, B>, frame: Frame) -> Vec<Binder> {
    let mut binders = Vec::new();
    while let (Some((hint, type_)), Some(mark)) = (cursor.entry(), cursor.mark()) {
        let label = frame.label(cursor.label());
        let named = hint.is_some() || cursor.binder_used();
        let variable = Term::free_var(&label);
        cursor.advance(variable.clone());
        binders.push(Binder {
            mark,
            type_,
            label,
            variable,
            named,
        });
    }

    binders
}

/// Each binder written, read by nothing beyond its own telescope.
pub fn written_entries(binders: &[Binder], frame: Frame) -> Vec<Printer> {
    binders
        .iter()
        .map(|binder| binder.written(false, frame))
        .collect()
}

/// What a value constructor takes a family's parameter under: a `use` one a witness slot, every other one hidden.
pub fn hidden(mark: Plicity) -> Plicity {
    match mark {
        Plicity::Witness => Plicity::Witness,
        Plicity::Explicit | Plicity::Implicit => Plicity::Implicit,
    }
}

/// A list of binders between parentheses, or nothing where there are none.
fn parameters(printers: Vec<Printer>) -> Printer {
    match printers.is_empty() {
        true => pure(""),
        false => listed("(".into(), false, printers, ")"),
    }
}

/// A family's form: its head, then each case, then `end`.
pub fn family_declared(name: &Global, entry: &InductDecl, frame: Frame) -> Printer {
    let mut cursor = entry.arity.cursor();
    let params = binders(&mut cursor, frame);
    let indices = cursor
        .body()
        .expect("a cursor past every entry holds its payload");
    let indices = binders(&mut indices.cursor(), frame);
    // A parameter a case reads is written by its word, which the case reads it by.
    let read = params
        .iter()
        .map(|param| {
            entry
                .constructors
                .iter()
                .any(|(_, signature)| reads(&signature.telescope, &params, param))
        })
        .collect::<Vec<_>>();
    let written = params
        .iter()
        .zip(read)
        .map(|(param, read)| param.written(read, frame))
        .collect();
    let indices = written_entries(&indices, frame);
    let indexed = !indices.is_empty();
    let arrow = match indexed {
        true => flat([parameters(indices), pure(" -> ")]),
        false => pure(""),
    };
    let head = flat([
        pure("induct "),
        pure(frame.spelling.symbol(name)),
        parameters(written),
        pure(": "),
        arrow,
        pure(if entry.rep_public { "pub " } else { "" }),
        sub(entry.result_sort.clone(), frame),
    ]);
    let cases = entry
        .constructors
        .iter()
        .map(|(tag, signature)| case_written(tag, signature, &params, indexed, frame))
        .collect::<Vec<_>>();

    flat([head, flat(cases), pure("\nend")])
}

/// Whether a case whose signature is `telescope` reads the family's parameter `param` past its own parameters, which are read at `params`.
fn reads(telescope: &Telescope<Vec<Term>>, params: &[Binder], param: &Binder) -> bool {
    let mut cursor = telescope.cursor();
    for each in params {
        if cursor.entry().is_none() {
            return false;
        }
        cursor.advance(each.variable.clone());
    }
    while let Some((_, type_)) = cursor.entry() {
        if param.variable_is(&type_) {
            return true;
        }
        cursor.advance(Term::free_var(&Free::local(u64::MAX, None)));
    }

    cursor
        .body()
        .is_some_and(|targets| targets.iter().any(|target| param.variable_is(target)))
}

/// A case: its tag over its payload, then its targets where its family has indices, under the parameters it states where they are not its family's.
fn case_written(
    tag: &Atom,
    signature: &InductParam,
    params: &[Binder],
    indexed: bool,
    frame: Frame,
) -> Printer {
    let mut cursor = signature.telescope.cursor();
    let mut stated = Vec::with_capacity(params.len());
    let mut own = true;
    for param in params {
        let (Some((_, type_)), Some(mark)) = (cursor.entry(), cursor.mark()) else {
            own = false;
            break;
        };
        own &= type_ == param.type_ && mark == hidden(param.mark);
        stated.push(flat([pure(plicity_mark(Some(&mark))), sub(type_, frame)]));
        cursor.advance(param.variable.clone());
    }
    let payload = written_entries(&binders(&mut cursor, frame), frame);
    let targets = cursor.body().unwrap_or_default();
    let targets = match indexed || !targets.is_empty() {
        true => flat([
            pure(": "),
            listed(
                "(".into(),
                false,
                targets
                    .into_iter()
                    .map(|target| sub(target, frame))
                    .collect(),
                ")",
            ),
        ]),
        false => pure(""),
    };
    let payload = listed("(".into(), false, payload, ")");
    let fields = match own {
        true => Vec::<Decoration>::new(),
        false => vec![("params", bracketed(stated))],
    };

    flat([
        pure("\n| "),
        frame.decorated(
            fields,
            flat([pure(tag.as_string()), payload, targets]),
            true,
        ),
    ])
}

/// A record's form under `keyword`, its fields between braces, with the word each field is written by in order — empty for a superclass edge and for a field nothing names.
pub fn record_declared(
    keyword: &'static str,
    name: &Global,
    record: &StructDecl,
    frame: Frame,
) -> (Printer, Vec<String>) {
    let mut cursor = record.arity.cursor();
    let params = binders(&mut cursor, frame);
    let fields = cursor
        .body()
        .expect("a cursor past every entry holds its payload");
    let fields = binders(&mut fields.cursor(), frame);
    // A parameter a field reads is written by its word, which the field reads it by.
    let params = params
        .iter()
        .map(|param| {
            let read = fields.iter().any(|field| param.variable_is(&field.type_));
            param.written(read, frame)
        })
        .collect::<Vec<_>>();
    let printers = written_entries(&fields, frame);
    let words = fields
        .iter()
        .map(|field| match (field.mark, field.named) {
            (Plicity::Witness, _) | (_, false) => String::new(),
            (_, true) => frame.spelling.label(&field.label),
        })
        .collect();
    // A superclass edge is a field nothing names, which resolution reaches: its word is never written.
    let body = listed(
        "{".into(),
        true,
        printers
            .into_iter()
            .zip(&fields)
            .map(|(printer, field)| match field.mark {
                Plicity::Witness => flat([pure("use "), sub(field.type_.clone(), frame)]),
                _ => printer,
            })
            .collect(),
        "}",
    );
    let form = flat([
        pure(keyword),
        pure(" "),
        pure(frame.spelling.symbol(name)),
        parameters(params),
        pure(": "),
        pure(if record.rep_public { "pub " } else { "" }),
        sub(record.result_sort.clone(), frame),
        pure(" "),
        body,
    ]);

    (form, words)
}
