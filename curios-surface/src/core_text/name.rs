//! A global named at an instance, a variable, and a metavariable.
//!
//! **A path builds an instance at no universe**, and `#{universes = …}` states the one it is at. **A word an enclosing binder binds is its variable**, and a bound index no scope in the text binds is `#{binding = Bound(n)} _`. **`?` builds a goal over no binder**, and a metavariable states its id, its origin where it is no goal, and its spine where it has one: `#{id = 7, origin = Hole, spine = [x, y]} ?`. A diagnostic spells every metavariable as a bare `?`, its id being elaboration state a reader cannot decode.

use {
    super::{Decoration, bracketed, nested, universe_suffix, universes_written},
    crate::{Frame, escaped, sub},
    curios_core::{CalleeId, Instance, Mention, Metavar, MetavarOrigin, Subterm, Var},
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
        MetavarOrigin::Implicit(implicit) => flat([
            pure("Implicit("),
            nested(vec![
                ("func", callee_written(&implicit.func, frame)),
                ("binder", optional(&implicit.binder)),
            ]),
            pure(")"),
        ]),
        MetavarOrigin::Witness(witness) => flat([
            pure("Witness("),
            nested(vec![
                ("func", callee_written(&witness.func, frame)),
                ("role", optional(&witness.role)),
            ]),
            pure(")"),
        ]),
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
        CalleeId::Constructor { owner, tag } => flat([
            pure("Constructor("),
            nested(vec![
                ("owner", pure(frame.spelling.symbol(owner))),
                ("tag", pure(format!("\"{}\"", escaped(tag)))),
            ]),
            pure(")"),
        ]),
        CalleeId::Method { concept, field } => flat([
            pure("Method("),
            nested(vec![
                ("concept", pure(frame.spelling.symbol(concept))),
                ("field", pure(format!("\"{}\"", escaped(field)))),
            ]),
            pure(")"),
        ]),
        CalleeId::Witness(name) => named("Witness", frame.spelling.symbol(name)),
        CalleeId::Operator(op) => named("Operator", format!("{op:?}")),
        CalleeId::Anonymous => pure("Anonymous"),
        CalleeId::Structure(name) => named("Structure", frame.spelling.symbol(name)),
    }
}

/// An optional text as Core holds it: `None`, or the text under `Some`.
fn optional(text: &Option<String>) -> Printer {
    match text {
        Some(text) => pure(format!("Some(\"{}\")", escaped(text))),
        None => pure("None"),
    }
}
