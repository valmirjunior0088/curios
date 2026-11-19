//! A global named at an instance, a variable, and a metavariable.
//!
//! **A path builds an instance at no universe**, and `#{universes = …}` states the one it is at. **A word an enclosing binder binds is its variable**, a bound index no scope in the text binds is `#{binding = Bound(n)} _`, and a local no binder in the text binds is, written whole, `#{binding = Free(k)} word`, the local whose identity is `k`: a module that holds one is told from a module holding another by that identity alone, which a reader mints its own binders apart from. **`?` builds a goal over no binder**, and a metavariable states its id, its origin where it is no goal, and its spine where it has one: `#{id = 7, origin = Hole, spine = [x, y]} ?`. A diagnostic spells every metavariable as a bare `?`, its id being elaboration state a reader cannot decode.

use {
    super::{
        Decoration, Fields, READ_MINTED, ReadError, Reader, Refusal, bracketed, called, core_read,
        literal_fields, literal_of, literal_variant, malformed, string, term_global, term_number,
        term_string, term_word, universe_suffix, universes_read, universes_written, value,
        value_list, value_number, value_term, value_variant, verbatim, word, word_value,
    },
    crate::{Apply, CoreText, CoreValue, Frame, Minting, Rendered, sub},
    curios_common::{InfixOp, Plicity, Span, Symbol},
    curios_core::{
        ArgumentSite, Binding, CalleeId, Free, Global, Instance, Mention, Metavar, MetavarId,
        MetavarOrigin, Subterm, Term, Universes, Var, WantedPremise, WitnessOrigin, WitnessRole,
    },
    std::rc::Rc,
};

/// A global at an instance: its name, under a decorator stating the instance where it states one.
pub fn instance_written<D: Rendered>(instance: Instance, frame: Frame<'_, D>) -> crate::Term<D> {
    // A member of the group being written is what the group binds wherever it is written at the group's own instance, so an instance of one says it is the global.
    let genuine = frame.spelling.groups(&instance.name);

    instance_spelled(instance, genuine, frame)
}

/// `Instance` as Core text writes it, under `#{node = Instance}` where it is `genuine`: a member of the group being written that the group does not bind.
fn instance_spelled<D: Rendered>(
    Instance { name, universes }: Instance,
    genuine: bool,
    frame: Frame<'_, D>,
) -> crate::Term<D> {
    if let Some(leave) = frame.reader() {
        return match universe_suffix(&universes, frame) {
            suffix if suffix.is_empty() => frame.global(&name),
            suffix => verbatim(leave, format!("{}{suffix}", frame.spelling.symbol(&name))),
        };
    }
    let fields = genuine
        .then(|| ("node", word_value("Instance")))
        .into_iter()
        .chain(universes_written(&universes, frame).map(|universes| ("universes", universes)))
        .collect();

    frame.decorated(fields, frame.global(&name))
}

/// A variable: a free one by the word its binder is printed under, a bound one by its index.
pub fn var_written<D: Rendered>(var: Var, frame: Frame<'_, D>) -> crate::Term<D> {
    match var.binding() {
        Binding::Bound(index) => frame.decorated(
            vec![(
                "binding",
                value(called(word("Bound"), vec![super::numeral(*index)])),
            )],
            super::tree(crate::Subterm::Held),
        ),
        // What the group being written binds is its member at the group's own instance.
        Binding::Free(free) if frame.spelling.group_member(free).is_some() => {
            let member = frame
                .spelling
                .group_member(free)
                .expect("a member of the group")
                .clone();
            instance_spelled(member, false, frame)
        }
        // A local the render did not open is the term's own, and only its identity tells it from another; a whole write states it, and a report, read about the term, names it by its word.
        Binding::Free(free) if frame.spelling.writes_whole() && !Minting::opened(free) => frame
            .decorated(
                vec![(
                    "binding",
                    value(called(word("Free"), vec![super::numeral(free.index())])),
                )],
                frame.variable(free),
            ),
        Binding::Free(free) => frame.variable(free),
    }
}

/// A metavariable as `?` under what states it, or bare where the spelling writes none.
pub fn metavar_written<D: Rendered>(metavar: Metavar, frame: Frame<'_, D>) -> crate::Term<D> {
    if frame.spelling.anonymizes_metavariables() {
        return super::tree(crate::Subterm::Goal);
    }
    if let Some(leave) = frame.reader() {
        return reader_metavar(metavar, frame, leave);
    }
    let mut fields = Vec::<Decoration<D>>::from([("id", value(super::numeral(metavar.id.0)))]);
    if metavar.origin != MetavarOrigin::Goal {
        fields.push(("origin", value(origin_written(&metavar.origin, frame))));
    }
    if !metavar.spine.is_empty() {
        fields.push((
            "spine",
            bracketed(
                metavar
                    .spine
                    .iter()
                    .map(|entry| value(sub(entry.clone(), frame)))
                    .collect(),
            ),
        ));
    }

    frame.decorated(fields, super::tree(crate::Subterm::Goal))
}

/// A metavariable as a reader is shown it: its id, and its spine after it where an entry is other than a variable, the case worth seeing.
fn reader_metavar<D: Rendered>(
    metavar: Metavar,
    frame: Frame<'_, D>,
    leave: D::Leave,
) -> crate::Term<D> {
    let renaming = metavar.spine.is_identity()
        || metavar
            .spine
            .iter()
            .all(|entry| matches!(&**entry, Subterm::Var(_)));
    let spine = match renaming {
        true => Vec::new(),
        false => metavar
            .spine
            .iter()
            .map(|entry| sub(entry.clone(), frame))
            .collect(),
    };

    super::tree(crate::Subterm::Core(D::metavar(
        leave,
        metavar.id.to_string(),
        spine,
    )))
}

/// Why a metavariable was made, as Core names it.
fn origin_written<D: Rendered>(origin: &MetavarOrigin, frame: Frame<'_, D>) -> crate::Term<D> {
    match origin {
        MetavarOrigin::Hole => word("Hole"),
        MetavarOrigin::Goal => word("Goal"),
        MetavarOrigin::Domain(binder) => called(word("Domain"), vec![optional(binder)]),
        MetavarOrigin::Implicit(site) => literal_of(
            "Implicit",
            vec![
                ("callee", callee_written(&site.callee, frame)),
                ("parameter", optional_symbol(site.parameter)),
                ("plicity", word(plicity_word(site.plicity))),
                ("position", super::numeral(site.position)),
            ],
        ),
        MetavarOrigin::Witness(witness) => literal_of(
            "Witness",
            vec![
                ("func", callee_written(&witness.func, frame)),
                (
                    "role",
                    match &witness.role {
                        Some(role) => called(word("Some"), vec![role_written(role, frame)]),
                        None => word("None"),
                    },
                ),
            ],
        ),
    }
}

/// A slot's mark, as Core names it.
fn plicity_word(plicity: Plicity) -> &'static str {
    match plicity {
        Plicity::Explicit => "Explicit",
        Plicity::Implicit => "Implicit",
        Plicity::Witness => "Witness",
    }
}

/// What a witness slot is to its callee, as Core names it.
fn role_written<D: Rendered>(role: &WitnessRole, frame: Frame<'_, D>) -> crate::Term<D> {
    match role {
        WitnessRole::Premise(position) => called(word("Premise"), vec![super::numeral(*position)]),
        WitnessRole::Superclass(concept) => called(word("Superclass"), vec![frame.global(concept)]),
        WitnessRole::Implementation(field) => {
            called(word("Implementation"), vec![string(field.as_str())])
        }
        WitnessRole::Payload {
            label,
            ordinal,
            premise,
        } => literal_of(
            "Payload",
            vec![
                ("label", optional_symbol(*label)),
                ("ordinal", super::numeral(*ordinal)),
                (
                    "premise",
                    match premise {
                        Some(premise) => called(
                            word("Some"),
                            vec![literal_of(
                                "WantedPremise",
                                vec![
                                    ("concept", frame.global(&premise.concept)),
                                    ("variable", sub(premise.variable.clone(), frame)),
                                ],
                            )],
                        ),
                        None => word("None"),
                    },
                ),
            ],
        ),
    }
}

/// An optional name as Core holds it, as [`optional`] holds a text.
fn optional_symbol<D: Rendered>(name: Option<Symbol>) -> crate::Term<D> {
    optional(&name.map(|name| name.as_str().to_string()))
}

/// The callee an implicit or a witness slot belongs to, as Core names it.
fn callee_written<D: Rendered>(callee: &CalleeId, frame: Frame<'_, D>) -> crate::Term<D> {
    let named = |variant: &str, name: crate::Term<D>| called(word(variant), vec![name]);
    match callee {
        CalleeId::Function(Mention::Declaration(name)) => {
            named("Function", named("Declaration", frame.global(name)))
        }
        CalleeId::Function(Mention::Local(local)) => {
            named("Function", named("Local", frame.variable(local)))
        }
        CalleeId::Constructor { owner, tag } => literal_of(
            "Constructor",
            vec![
                ("owner", frame.global(owner)),
                ("tag", string(tag.as_str())),
            ],
        ),
        CalleeId::Method { concept, field } => literal_of(
            "Method",
            vec![
                ("concept", frame.global(concept)),
                ("field", string(field.as_str())),
            ],
        ),
        CalleeId::Witness(name) => named("Witness", frame.global(name)),
        CalleeId::Operator(op) => named("Operator", word(format!("{op:?}"))),
        CalleeId::Anonymous => word("Anonymous"),
        CalleeId::Structure(name) => named("Structure", frame.global(name)),
    }
}

/// An optional text as Core holds it: `None`, or the text under `Some`.
fn optional<D: Rendered>(text: &Option<String>) -> crate::Term<D> {
    match text {
        Some(text) => called(word("Some"), vec![string(text.as_str())]),
        None => word("None"),
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

/// An instance of a group's member written inside the group, `#{node = Instance} /f`: the global, which the group's reader would otherwise read as what the group binds. Read inside a group it is held apart under a local of its own until the group has bound its members ([`Reader::grouped`]).
pub fn group_instance_read(
    operand: &crate::Term<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Term, ReadError> {
    let Some(name) = term_global(operand) else {
        return Err(ReadError::at(
            Refusal::Malformed("a member's path under `#{node = Instance}`"),
            operand.span(),
        ));
    };
    let instance = instance_read(name, fields)?;

    Ok(reader.held_apart(instance))
}

/// A local no binder in the text binds, `#{binding = Free(k)} word`: the local whose identity is `k`, under `word`. An identity a reader mints its own binders at is refused, since the two would be one local.
pub fn free_read(value: &CoreValue, word: &str) -> Result<Term, ReadError> {
    let malformed = || malformed("a local's identity below a reader's own, `Free(k)`", value);
    match value_variant(value)? {
        ("Free", held) => match held.as_slice() {
            [index] => term_number(index)
                .filter(|index| *index < READ_MINTED)
                .map(|index| Term::free_var(&Free::local(index, Some(word))))
                .ok_or_else(malformed),
            _ => Err(malformed()),
        },
        _ => Err(malformed()),
    }
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
        Some(value) => value_list(value)?
            .iter()
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
            "Implicit" => MetavarOrigin::Implicit(ArgumentSite {
                callee: callee_read(fields.term("callee")?, reader)?,
                parameter: optional_read(fields.term("parameter")?)?.map(|name| Symbol::new(&name)),
                plicity: plicity_read(fields.term("plicity")?)?,
                position: number_read(fields.term("position")?, "a slot's position")?,
            }),
            "Witness" => MetavarOrigin::Witness(WitnessOrigin {
                func: callee_read(fields.term("func")?, reader)?,
                role: some_read(fields.term("role")?, |role| role_read(role, reader))?,
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

/// A slot's mark, as Core names it ([`plicity_word`]).
fn plicity_read(term: &crate::Term<CoreText>) -> Result<Plicity, ReadError> {
    match term_word(term) {
        Some("Explicit") => Ok(Plicity::Explicit),
        Some("Implicit") => Ok(Plicity::Implicit),
        Some("Witness") => Ok(Plicity::Witness),
        _ => Err(ReadError::at(
            Refusal::Malformed("`Explicit`, `Implicit` or `Witness`"),
            term.span(),
        )),
    }
}

/// A count Core writes as a numeral, `what` naming it in a refusal.
fn number_read(term: &crate::Term<CoreText>, what: &'static str) -> Result<usize, ReadError> {
    term_number(term)
        .and_then(|number| usize::try_from(number).ok())
        .ok_or_else(|| ReadError::at(Refusal::Malformed(what), term.span()))
}

/// `None`, or what `read` reads under `Some`.
fn some_read<T>(
    term: &crate::Term<CoreText>,
    read: impl FnOnce(&crate::Term<CoreText>) -> Result<T, ReadError>,
) -> Result<Option<T>, ReadError> {
    if term_word(term) == Some("None") {
        return Ok(None);
    }
    match term.as_subterm() {
        crate::Subterm::Apply(Apply { head, arguments }) if term_word(head) == Some("Some") => {
            match arguments.as_slice() {
                [held] => read(&held.term).map(Some),
                _ => Err(ReadError::at(Refusal::Malformed("`Some(…)`"), term.span())),
            }
        }
        _ => Err(ReadError::at(
            Refusal::Malformed("`None` or `Some(…)`"),
            term.span(),
        )),
    }
}

/// What a witness slot is to its callee, as Core names it ([`role_written`]).
fn role_read(
    term: &crate::Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<WitnessRole, ReadError> {
    let refused = || ReadError::at(Refusal::Malformed("a witness slot's role"), term.span());
    if let crate::Subterm::StructLit(literal) = term.as_subterm() {
        if literal_variant(literal) != Some("Payload") {
            return Err(refused());
        }
        let mut fields = literal_fields(literal)?;
        let role = WitnessRole::Payload {
            label: optional_read(fields.term("label")?)?.map(|label| Symbol::new(&label)),
            ordinal: number_read(fields.term("ordinal")?, "a payload's ordinal")?,
            premise: some_read(fields.term("premise")?, |premise| {
                let crate::Subterm::StructLit(literal) = premise.as_subterm() else {
                    return Err(refused());
                };
                let mut fields = literal_fields(literal)?;
                let premise = WantedPremise {
                    concept: term_global(fields.term("concept")?).ok_or_else(refused)?,
                    variable: core_read(fields.term("variable")?, reader)?,
                };
                fields.finish()?;
                Ok(Rc::new(premise))
            })?,
        };
        fields.finish()?;
        return Ok(role);
    }
    let crate::Subterm::Apply(Apply { head, arguments }) = term.as_subterm() else {
        return Err(refused());
    };
    let [argument] = arguments.as_slice() else {
        return Err(refused());
    };
    let held = &argument.term;
    match term_word(head) {
        Some("Premise") => Ok(WitnessRole::Premise(number_read(
            held,
            "a premise's position",
        )?)),
        Some("Superclass") => Ok(WitnessRole::Superclass(
            term_global(held).ok_or_else(refused)?,
        )),
        Some("Implementation") => Ok(WitnessRole::Implementation(Symbol::new(
            &term_string(held).ok_or_else(refused)?,
        ))),
        _ => Err(refused()),
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
