//! A global named at an instance, a variable, and a metavariable.
//!
//! **A path builds an instance at no universe**, and `#{universes = …}` states the one it is at. **A word an enclosing binder binds is its variable**, and a bound index no scope in the text binds is `#{binding = Bound(n)} _`. **`?` builds a goal over no binder**, and a metavariable states its id, its origin where it is no goal, and its spine where it has one: `#{id = 7, origin = Hole, spine = [x, y]} ?`. A diagnostic spells every metavariable as a bare `?`, its id being elaboration state a reader cannot decode.

use {
    super::{
        Decoration, Fields, ReadError, Reader, Refusal, bracketed, core_read, literal_fields,
        literal_variant, malformed, nested, term_global, term_number, term_string, term_word,
        universe_suffix, universes_read, universes_written, value_number, value_term,
        value_unmarked, value_variant,
    },
    crate::{Apply, CoreText, CoreValue, Frame, escaped, sub},
    curios_common::{InfixOp, Span},
    curios_core::{
        CalleeId, Global, ImplicitOrigin, Instance, Mention, Metavar, MetavarId, MetavarOrigin,
        Subterm, Term, Universes, Var, WitnessOrigin,
    },
    curios_print::{Printer, flat, pure, sep_flat},
};

/// A global at an instance: its name, under a decorator stating the instance where it states one.
pub fn instance_written(Instance { name, universes }: Instance, frame: Frame) -> Printer {
    if !frame.core_text() {
        return pure(format!(
            "{}{}",
            frame.spelling.symbol(&name),
            universe_suffix(&universes, frame)
        ));
    }
    let fields = universes_written(&universes, frame)
        .map(|universes| vec![("universes", universes)])
        .unwrap_or_default();

    frame.decorated(fields, pure(frame.spelling.symbol(&name)), true)
}

/// A variable: a free one by the word its binder is printed under, a bound one by its index.
pub fn var_written(var: Var, frame: Frame) -> Printer {
    match var.as_bound() {
        Some(index) => frame.decorated(
            vec![("binding", pure(format!("Bound({index})")))],
            pure("_"),
            true,
        ),
        None => pure(frame.spelling.label(var.unwrap())),
    }
}

/// A metavariable as `?` under what states it, or bare where the spelling writes none.
pub fn metavar_written(metavar: Metavar, frame: Frame) -> Printer {
    if frame.spelling.anonymizes_metavariables() {
        return pure("?");
    }
    if !frame.core_text() {
        return reader_metavar(metavar, frame);
    }
    let mut fields = Vec::<Decoration>::from([("id", pure(metavar.id.0.to_string()))]);
    if metavar.origin != MetavarOrigin::Goal {
        fields.push(("origin", origin_written(&metavar.origin, frame)));
    }
    if !metavar.spine.is_empty() {
        fields.push((
            "spine",
            bracketed(
                metavar
                    .spine
                    .iter()
                    .map(|entry| sub(entry.clone(), frame))
                    .collect(),
            ),
        ));
    }

    frame.decorated(fields, pure("?"), true)
}

/// A metavariable as a reader is shown it: its id, and its spine after it where an entry is other than a variable, the case worth seeing.
fn reader_metavar(metavar: Metavar, frame: Frame) -> Printer {
    let renaming = metavar.spine.is_identity()
        || metavar
            .spine
            .iter()
            .all(|entry| matches!(&**entry, Subterm::Var(_)));
    if renaming {
        return pure(format!("?{}", metavar.id));
    }

    flat([
        pure(format!("?{}[", metavar.id)),
        sep_flat(
            metavar
                .spine
                .iter()
                .map(|entry| sub(entry.clone(), frame))
                .collect::<Vec<_>>(),
            || pure(", "),
        ),
        pure("]"),
    ])
}

/// Why a metavariable was made, as Core names it.
fn origin_written(origin: &MetavarOrigin, frame: Frame) -> Printer {
    match origin {
        MetavarOrigin::Hole => pure("Hole"),
        MetavarOrigin::Goal => pure("Goal"),
        MetavarOrigin::Domain(binder) => flat([pure("Domain("), optional(binder), pure(")")]),
        MetavarOrigin::Implicit(implicit) => variant_of_fields(
            "Implicit",
            vec![
                ("func", callee_written(&implicit.func, frame)),
                ("binder", optional(&implicit.binder)),
            ],
        ),
        MetavarOrigin::Witness(witness) => variant_of_fields(
            "Witness",
            vec![
                ("func", callee_written(&witness.func, frame)),
                ("role", optional(&witness.role)),
            ],
        ),
    }
}

/// The callee an implicit or a witness slot belongs to, as Core names it.
fn callee_written(callee: &CalleeId, frame: Frame) -> Printer {
    let named = |variant: &str, name: String| pure(format!("{variant}({name})"));
    match callee {
        CalleeId::Function(Mention::Declaration(name)) => named(
            "Function",
            format!("Declaration({})", frame.spelling.symbol(name)),
        ),
        CalleeId::Function(Mention::Local(local)) => named(
            "Function",
            format!("Local({})", frame.spelling.label(local)),
        ),
        CalleeId::Constructor { owner, tag } => variant_of_fields(
            "Constructor",
            vec![
                ("owner", pure(frame.spelling.symbol(owner))),
                ("tag", pure(format!("\"{}\"", escaped(tag)))),
            ],
        ),
        CalleeId::Method { concept, field } => variant_of_fields(
            "Method",
            vec![
                ("concept", pure(frame.spelling.symbol(concept))),
                ("field", pure(format!("\"{}\"", escaped(field)))),
            ],
        ),
        CalleeId::Witness(name) => named("Witness", frame.spelling.symbol(name)),
        CalleeId::Operator(op) => named("Operator", format!("{op:?}")),
        CalleeId::Anonymous => pure("Anonymous"),
        CalleeId::Structure(name) => named("Structure", frame.spelling.symbol(name)),
    }
}

/// A variant that holds fields, as a structure's literal is written: `Implicit { func = …, binder = None }`.
fn variant_of_fields(variant: &str, fields: Vec<Decoration>) -> Printer {
    flat([pure(format!("{variant} ")), nested(fields)])
}

/// An optional text as Core holds it: `None`, or the text under `Some`.
fn optional(text: &Option<String>) -> Printer {
    match text {
        Some(text) => pure(format!("Some(\"{}\")", escaped(text))),
        None => pure("None"),
    }
}

/// A global named at an instance: at none, and at the one `universes` states where the decorator states one.
pub fn instance_read(name: Global, fields: &mut Fields<'_>) -> Result<Term, ReadError> {
    let universes = match fields.take("universes") {
        Some(value) => universes_read(value)?,
        None => Universes::default(),
    };

    Ok(Term::instance(name, universes))
}

/// A variable no binder in the text binds, `#{binding = Bound(n)} _`: the loose index it is.
pub fn bound_read(fields: &mut Fields<'_>, span: Option<&Span>) -> Result<Term, ReadError> {
    let Some(value) = fields.take("binding") else {
        return Err(ReadError::at(
            Refusal::ProgramForm("a held place `_`"),
            span,
        ));
    };
    match value_variant(value)? {
        ("Bound", held) => match held.as_slice() {
            [index] => term_number(index)
                .and_then(|index| usize::try_from(index).ok())
                .map(|index| Term::var(Var::bound(index)))
                .ok_or_else(|| malformed("a bound index", value)),
            _ => Err(malformed("a bound index, `Bound(n)`", value)),
        },
        _ => Err(malformed("`Bound(n)`", value)),
    }
}

/// A metavariable: its id, its origin where it is no goal, and its spine.
pub fn metavar_read(
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
    span: Option<&Span>,
) -> Result<Term, ReadError> {
    let Some(id) = fields.take("id") else {
        return Err(ReadError::at(Refusal::ProgramForm("a goal `?`"), span));
    };
    let id = value_number(id).and_then(|value| {
        usize::try_from(value).map_err(|_| malformed("a metavariable's id", id))
    })?;
    let origin = match fields.take("origin") {
        Some(value) => origin_read(value, reader)?,
        None => MetavarOrigin::Goal,
    };
    let spine = match fields.take("spine") {
        Some(value) => value_unmarked(value)?
            .into_iter()
            .map(|entry| core_read(&value_term(entry)?, reader))
            .collect::<Result<Vec<_>, _>>()?,
        None => Vec::new(),
    };

    Ok(Term::from(Subterm::Metavar(Metavar {
        id: MetavarId(id),
        spine: spine.into(),
        origin,
    })))
}

/// Why a metavariable was made, as Core names it: a word, a call over what the variant holds, or a variant holding fields as a structure's literal is written.
fn origin_read(value: &CoreValue, reader: &mut Reader<'_>) -> Result<MetavarOrigin, ReadError> {
    let term = value_term(value)?;
    if let crate::Subterm::StructLit(literal) = term.as_subterm() {
        let mut fields = literal_fields(literal)?;
        let origin = match literal_variant(literal).unwrap_or_default() {
            "Implicit" => MetavarOrigin::Implicit(ImplicitOrigin {
                func: callee_read(fields.term("func")?, reader)?,
                binder: optional_read(fields.term("binder")?)?,
            }),
            "Witness" => MetavarOrigin::Witness(WitnessOrigin {
                func: callee_read(fields.term("func")?, reader)?,
                role: optional_read(fields.term("role")?)?,
            }),
            _ => return Err(malformed("a metavariable's origin", value)),
        };
        fields.finish()?;
        return Ok(origin);
    }
    match value_variant(value)? {
        ("Hole", held) if held.is_empty() => Ok(MetavarOrigin::Hole),
        ("Goal", held) if held.is_empty() => Ok(MetavarOrigin::Goal),
        ("Domain", held) => match held.as_slice() {
            [binder] => Ok(MetavarOrigin::Domain(optional_read(binder)?)),
            _ => Err(malformed("`Domain(…)`", value)),
        },
        _ => Err(malformed("a metavariable's origin", value)),
    }
}

/// An optional text as Core holds it: `None`, or the text under `Some`.
fn optional_read(term: &crate::Term<CoreText>) -> Result<Option<String>, ReadError> {
    let refused = || ReadError::at(Refusal::Malformed("`None` or `Some(\"…\")`"), term.span());
    if term_word(term) == Some("None") {
        return Ok(None);
    }
    match term.as_subterm() {
        crate::Subterm::Apply(Apply { head, arguments }) if term_word(head) == Some("Some") => {
            match arguments.as_slice() {
                [text] => term_string(&text.term).map(Some).ok_or_else(refused),
                _ => Err(refused()),
            }
        }
        _ => Err(refused()),
    }
}

/// The callee an implicit or a witness slot belongs to, as Core names it.
fn callee_read(
    term: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<CalleeId, ReadError> {
    let refused = || {
        ReadError::at(
            Refusal::Malformed("a callee, as Core names one"),
            term.span(),
        )
    };
    if term_word(term) == Some("Anonymous") {
        return Ok(CalleeId::Anonymous);
    }
    if let crate::Subterm::StructLit(literal) = term.as_subterm() {
        let mut fields = literal_fields(literal)?;
        let callee = match literal_variant(literal).unwrap_or_default() {
            "Constructor" => CalleeId::Constructor {
                owner: term_global(fields.term("owner")?).ok_or_else(refused)?,
                tag: term_string(fields.term("tag")?).ok_or_else(refused)?,
            },
            "Method" => CalleeId::Method {
                concept: term_global(fields.term("concept")?).ok_or_else(refused)?,
                field: term_string(fields.term("field")?).ok_or_else(refused)?,
            },
            _ => return Err(refused()),
        };
        fields.finish()?;
        return Ok(callee);
    }
    let crate::Subterm::Apply(Apply { head, arguments }) = term.as_subterm() else {
        return Err(refused());
    };
    let [argument] = arguments.as_slice() else {
        return Err(refused());
    };
    let held = &argument.term;
    match term_word(head).ok_or_else(refused)? {
        "Function" => {
            let crate::Subterm::Apply(Apply { head, arguments }) = held.as_subterm() else {
                return Err(refused());
            };
            let [named] = arguments.as_slice() else {
                return Err(refused());
            };
            match term_word(head).ok_or_else(refused)? {
                "Declaration" => Ok(CalleeId::Function(Mention::Declaration(
                    term_global(&named.term).ok_or_else(refused)?,
                ))),
                "Local" => Ok(CalleeId::Function(Mention::Local(
                    reader.word(term_word(&named.term).ok_or_else(refused)?),
                ))),
                _ => Err(refused()),
            }
        }
        "Witness" => Ok(CalleeId::Witness(term_global(held).ok_or_else(refused)?)),
        "Structure" => Ok(CalleeId::Structure(term_global(held).ok_or_else(refused)?)),
        "Operator" => {
            let word = term_word(held).ok_or_else(refused)?;
            InfixOp::ALL
                .iter()
                .find(|op| format!("{op:?}") == word)
                .map(|op| CalleeId::Operator(*op))
                .ok_or_else(refused)
        }
        _ => Err(refused()),
    }
}
