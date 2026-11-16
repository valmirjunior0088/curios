//! A recursive group's callable parts, and the path a call reaches one by.
//!
//! A member is called where its leading lambdas are applied, and where the body under them is a record literal — a structure's or a tuple's — each field that is a lambda is called where the member's value is projected to it and applied: `(W.0)(l)` runs the field's lambda, and nothing of the member's but its own parameters. So a group's call graph has a node for each such part, its parameters the member's followed by the field lambda's, and a call is read off a path — the member, its own arguments, a field, the field's arguments — rather than off a bare variable. A part is read off a member's body as it stands: whether the record came from a `satisfy` is never asked, so a structure of handlers is graded as a witness is.

#[cfg(test)]
mod tests;

use {
    crate::Env,
    curios_core::{Advance, CallMatrix, Free, Func, Proj, RecGroup, Struct, Subterm, Term, Tuple},
};

/// One callable part of a recursive group: member `member`'s leading lambdas where `path` is empty, and otherwise the field of the record literal under them that `path` projects to, nested literals followed; `arity` counts the member's parameters and the field lambda's after them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PartShape {
    pub member: usize,
    pub path: Vec<usize>,
    pub arity: usize,
}

/// One step of a call's path past its root.
#[derive(Debug, Clone)]
pub enum Segment {
    /// Arguments applied to what the path has reached.
    Arguments(Vec<Term>),
    /// A slot projected out of it.
    Field(usize),
}

impl Segment {
    /// The arguments this step applies, none for a projection.
    pub fn arguments(&self) -> &[Term] {
        match self {
            Segment::Arguments(arguments) => arguments,
            Segment::Field(_) => &[],
        }
    }
}

/// `term` as a path: its root under every application and every elaborated projection, and the steps from the root in order. A written projection names no slot yet, and ends the path where it stands.
pub fn call_path(term: &Term) -> (Term, Vec<Segment>) {
    let mut steps = Vec::new();
    let mut head = term.clone();
    loop {
        match &*head.clone() {
            Subterm::Apply(apply) => {
                steps.push(Segment::Arguments(apply.params().cloned().collect()));
                head = apply.head.clone();
            }
            Subterm::Proj(Proj { head: inner, field }) if field.slot().is_some() => {
                steps.push(Segment::Field(field.slot().expect("a slot")));
                head = inner.clone();
            }
            _ => {
                steps.reverse();
                return (head, steps);
            }
        }
    }
}

/// The calls of member `member`'s parts a path rooted at it makes, each with the arguments it hands that part.
///
/// A field is followed where the arguments before it fill exactly the part it projects out of and a part lies at the field, or a record literal holding one does; the path stops at the first field that is not, its later steps applied to what the part reached answers, so `f(x).0`, a member's result projected to a field that is no part, is the call `f(x)` it was. The calls are:
/// - the part the path reaches, with every argument it handed;
/// - where the path reaches no field lambda — the member named with its own arguments or none, or a record literal held at a field — every part under where it stands, with those arguments: the record it answers escapes, and any field lambda of it may run, with anything past them;
/// - where the path followed a field out of what the member's leading lambdas answer, those too, with their arguments: the record is answered, and its fields that are no part run, before the field is read.
pub fn part_calls(
    parts: &[PartShape],
    member: usize,
    steps: &[Segment],
) -> Vec<(usize, Vec<Term>)> {
    let base = base_part(parts, member);
    let mut reached = base;
    let mut path = Vec::new();
    let mut arguments = Vec::new();
    let mut answered = None;
    let mut stopped = false;
    for step in steps {
        match step {
            Segment::Arguments(applied) => arguments.extend(applied.iter().cloned()),
            Segment::Field(slot) => {
                if arguments.len() != parts[reached].arity {
                    stopped = true;
                    break;
                }
                if reached == base && answered.is_none() {
                    answered = Some(arguments.clone());
                }
                path.push(*slot);
                match parts
                    .iter()
                    .position(|part| part.member == member && part.path == path)
                {
                    Some(part) => reached = part,
                    None if parts
                        .iter()
                        .any(|part| part.member == member && part.path.starts_with(&path)) => {}
                    None => {
                        stopped = true;
                        break;
                    }
                }
            }
        }
    }
    let escapes = !stopped && (reached == base || parts[reached].path != path);
    let mut calls = match escapes {
        true => parts
            .iter()
            .enumerate()
            .filter(|(_, part)| part.member == member && part.path.starts_with(&path))
            .map(|(part, _)| (part, arguments.clone()))
            .collect(),
        false => vec![(reached, arguments)],
    };
    if let Some(answered) = answered
        && calls.iter().all(|(part, _)| *part != base)
    {
        calls.push((base, answered));
    }
    calls
}

/// The part member `member`'s leading lambdas are.
pub fn base_part(parts: &[PartShape], member: usize) -> usize {
    parts
        .iter()
        .position(|part| part.member == member && part.path.is_empty())
        .expect("every member's leading lambdas are a part")
}

/// The call of a part of `arity` parameters an application of its member makes where `handed` grades the application against `rows` parameters: where it hands the member's leading lambdas all their `base` parameters, `handed` with columns claiming nothing after it for the field lambda's, and a call claiming nothing otherwise.
pub fn handed_through(handed: &CallMatrix, base: usize, rows: usize, arity: usize) -> CallMatrix {
    match handed.columns() == base && handed.rows() == rows && arity >= base {
        true => handed
            .beside(&CallMatrix::unknown(rows, arity - base))
            .expect("the same rows"),
        false => CallMatrix::unknown(rows, arity),
    }
}

/// Each part of member `member`: what a call reaching the member without following a field into it makes a call of, the record its leading lambdas answer escaping ([`part_calls`]).
pub fn member_parts(parts: &[PartShape], member: usize) -> impl Iterator<Item = usize> + '_ {
    parts
        .iter()
        .enumerate()
        .filter(move |(_, part)| part.member == member)
        .map(|(part, _)| part)
}

/// A part opened for analysis: its shape, its parameters as binders, and the terms its call runs — a field lambda's body, or what a member's leading lambdas run besides the fields that are parts of their own, which run only where they are called.
pub(super) struct Part {
    pub shape: PartShape,
    pub params: Vec<Free>,
    pub runs: Vec<Term>,
}

/// The parts of `group`, in the order both checkers number them: each member's leading lambdas, then the fields of the record under them, depth first.
pub(super) fn open_parts<E: Env>(env: &mut E, group: &RecGroup) -> Vec<Part> {
    let mut parts = Vec::new();
    for member in 0..group.length() {
        let (params, body) = peel(env, group.member_body(member));
        let base = parts.len();
        parts.push(Part {
            shape: PartShape {
                member,
                path: Vec::new(),
                arity: params.len(),
            },
            params: params.clone(),
            runs: Vec::new(),
        });
        parts[base].runs = record(env, member, &[], &params, &body, &mut parts);
    }
    parts
}

/// Each part's shape: what a call is graded over and where a path leads.
pub fn group_parts<E: Env>(env: &mut E, group: &RecGroup) -> Vec<PartShape> {
    open_parts(env, group)
        .into_iter()
        .map(|part| part.shape)
        .collect()
}

/// Peel `body`'s leading lambdas, minting one binder per parameter.
///
/// A member with no lambda — `rec inf : F = F/more(inf)`, or any of `/std/Json/decode`'s nullary parsers — has no parameter, and its self-call therefore contributes a 0×0 matrix. That matrix is idempotent and has no diagonal to descend on, so a nullary self-call is rejected, which is exactly right: nothing about it can get smaller.
fn peel<E: Env>(env: &mut E, mut body: Term) -> (Vec<Free>, Term) {
    let mut params = Vec::new();
    while let Subterm::Func(Func { telescope, .. }) = &*body {
        let mut cursor = telescope.cursor();
        while !cursor.is_done() {
            params.push(cursor.advance_fresh(|hint| env.fresh(hint)));
        }
        let inner = cursor.body().expect("a cursor past every entry");
        body = inner;
    }
    (params, body)
}

/// What `body`, at `path` under member `member`'s parameters `params`, runs where the part holding it is called: itself, unless it is a record literal, whose parameters and fields that are no part it runs, each field that is a lambda pushed as a part and each that is a record literal followed in turn.
fn record<E: Env>(
    env: &mut E,
    member: usize,
    path: &[usize],
    params: &[Free],
    body: &Term,
    parts: &mut Vec<Part>,
) -> Vec<Term> {
    let (held, fields): (&[Term], &[Term]) = match &**body {
        Subterm::Struct(Struct { params, fields, .. }) => (params, fields),
        Subterm::Tuple(Tuple { fields, .. }) => (&[], fields),
        _ => return vec![body.clone()],
    };
    let mut runs = held.to_vec();
    for (slot, field) in fields.iter().enumerate() {
        let mut at = path.to_vec();
        at.push(slot);
        match &**field {
            Subterm::Func(_) => {
                let (own, inner) = peel(env, field.clone());
                let mut all = params.to_vec();
                all.extend(own);
                parts.push(Part {
                    shape: PartShape {
                        member,
                        path: at,
                        arity: all.len(),
                    },
                    params: all,
                    runs: vec![inner],
                });
            }
            Subterm::Struct(_) | Subterm::Tuple(_) => {
                runs.extend(record(env, member, &at, params, field, parts));
            }
            _ => runs.push(field.clone()),
        }
    }
    runs
}
