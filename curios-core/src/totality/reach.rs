//! What the erased half of a program reaches.
//!
//! Erasure deletes two things, so there are two obligations, and they share one
//! closure over exactly one relation — "this definition mentions that one" —
//! seeded twice.
//!
//! **(T)** is seeded from every type position, and takes the *aggressive*
//! reading: everything a type reaches must be total, not merely the values a
//! type-level eliminator scrutinizes. The narrow reading is unsound, because a
//! *total* type-level function applied to a *partial value* reties the negative
//! knot that strict positivity exists to forbid.
//!
//! **(V)** is seeded from every position whose declared type is `Prop`-sorted.
//!
//! Neither obligation subsumes the other. The type-level `rec` route violates
//! only (T); the partial-carrier and `exit` routes violate only (V).
//!
//! The closure follows a definition into its **body**, not only its type. That
//! is easy to overlook and load-bearing: `/std/BigNat/Canonical` is a
//! proposition whose body eliminates `is_trimmed`, and no type annotation
//! anywhere records that dependency.
//!
//! At the replay site the module in hand is the user suffix alone, so a prelude
//! definition is a sink of this relation. That is sound for the same reason it
//! is sound for positivity — prelude items cannot mention user code — and it is
//! why partiality persists on the definition rather than being recomputed.

use {
    super::super::{
        Bound, Context, Definition, Error, FuncType, Global, Item, Module, Struct, Subterm,
        Telescope, Term, Variant, is_prop,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// A term the erased half of the program must be total in, and what a
/// diagnostic should call it.
///
/// The obligations are stated over *terms*, not over the names those terms
/// mention, because a `rec` written inline in an erased position mentions no
/// name at all. Reporting the reached definitions alone would let
/// `rec Bad : Type = Sink(Bad)`, written as a local binding, satisfy every
/// closure while retying exactly the knot the closure exists to forbid.
pub(crate) struct Position {
    pub(crate) term: Term,
    pub(crate) site: String,
}

fn push(positions: &mut Vec<Position>, site: &str, term: &Term) {
    positions.push(Position {
        term: term.clone(),
        site: site.to_string(),
    });
}

/// Every term in a type position.
///
/// "Type position" is read syntactically and generously: a declared type, a
/// binder annotation, a match motive, a declaration telescope, a nominal type
/// former, a primitive type former, and the *body* of any definition whose own
/// type ends in a sort — the last being what reaches through
/// `/std/BigNat/Canonical` into `is_trimmed`.
pub(crate) fn type_positions(module: &Module) -> Vec<Position> {
    let mut positions = Vec::new();

    for definition in definitions(module) {
        let name = definition.name.to_string();
        push(
            &mut positions,
            &format!("the type of '{name}'"),
            &definition.type_,
        );
        if ends_in_sort(&definition.type_) {
            push(
                &mut positions,
                &format!("the body of '{name}'"),
                &definition.body,
            );
        }
        annotations(&definition.body, &format!("'{name}'"), &mut positions);
        annotations(
            &definition.type_,
            &format!("the type of '{name}'"),
            &mut positions,
        );
    }

    // The entrypoint expression and its annotation are not items, so nothing
    // above reaches them. An exploit needs only a local `rec` and one
    // construction, both of which fit in the trailing expression.
    if let Some(type_) = &module.type_ {
        push(&mut positions, "the entrypoint's type", type_);
        annotations(type_, "the entrypoint's type", &mut positions);
    }
    annotations(&module.body, "the entrypoint", &mut positions);

    // A declaration's telescopes are types by construction, and its parameter
    // and field types can name anything.
    for (name, declaration) in &module.induct_decls {
        let site = format!("a parameter of '{name}'");
        entries(&declaration.params, &site, &mut positions);
        for (tag, constructor) in &declaration.constructors {
            let site = format!("the payload of '{name}/{tag}'");
            entries(&constructor.telescope, &site, &mut positions);
        }
    }
    for (name, declaration) in &module.struct_decls {
        let site = format!("a parameter of '{name}'");
        entries(&declaration.params, &site, &mut positions);
        let site = format!("a field of '{name}'");
        entries(&declaration.fields, &site, &mut positions);
    }

    positions
}

/// The definitions these positions name — the seeds of the reachability walk.
pub(crate) fn seeds(positions: &[Position]) -> BTreeSet<Global> {
    let mut seeds = BTreeSet::new();
    for position in positions {
        mark(&position.term, &mut seeds);
    }
    seeds
}

/// Every definition named by a term checked against a `Prop`-sorted type.
///
/// Three positions carry proofs across a boundary the closure would otherwise
/// not cross: a definition that *is* a proof, a structure field declared at a
/// proposition — the certificate idiom, `Str { bytes, valid }` — and a
/// constructor payload declared at one. A proof written inline inside another
/// proof needs no rule of its own: seeding a proof-valued definition takes the
/// free variables of its whole body, arguments included.
pub(crate) fn proof_positions(
    context: &mut Context,
    module: &Module,
) -> Result<Vec<Position>, Error> {
    let mut positions = Vec::new();

    for definition in definitions(module) {
        let name = definition.name.to_string();
        if definition.type_.reach() == 0 && is_prop(context, &definition.type_)? {
            push(
                &mut positions,
                &format!("the proof '{name}'"),
                &definition.body,
            );
        }
        certificates(
            context,
            module,
            &definition.body,
            &format!("a certificate in '{name}'"),
            &mut positions,
        )?;
    }

    // The entrypoint, for the same reason type positions cover it.
    if let Some(type_) = &module.type_
        && type_.reach() == 0
        && is_prop(context, type_)?
    {
        push(&mut positions, "the entrypoint", &module.body);
    }
    certificates(
        context,
        module,
        &module.body,
        "a certificate in the entrypoint",
        &mut positions,
    )?;

    Ok(positions)
}

/// Close `seeds` over the definitions they name, following each into both its
/// type and its body.
pub(crate) fn reachable(module: &Module, seeds: BTreeSet<Global>) -> BTreeSet<Global> {
    let bodies = definitions(module)
        .map(|definition| {
            let mut named = definition.type_.free_vars();
            named.extend(definition.body.free_vars());
            let named = named
                .iter()
                .filter_map(|free| free.as_global().cloned())
                .collect::<BTreeSet<_>>();
            (definition.name.clone(), named)
        })
        .collect::<BTreeMap<_, _>>();

    let mut reached = seeds;
    let mut frontier = reached.iter().cloned().collect::<Vec<_>>();
    while let Some(name) = frontier.pop() {
        let Some(named) = bodies.get(&name) else {
            // Outside the module under analysis: a replayed prelude
            // definition, whose own closure was settled when the archive was
            // built.
            continue;
        };
        for next in named {
            if reached.insert(next.clone()) {
                frontier.push(next.clone());
            }
        }
    }

    reached
}

/// Every top-level definition in the module, `let` and `rec` alike.
fn definitions(module: &Module) -> impl Iterator<Item = Definition> + '_ {
    module.items.iter().flat_map(|item| match item {
        Item::Let(definition) => vec![definition.clone()],
        Item::Rec(rec) => rec.definitions(),
    })
}

/// Record every definition this term names.
fn mark(term: &Term, seeds: &mut BTreeSet<Global>) {
    seeds.extend(
        term.free_vars()
            .iter()
            .filter_map(|free| free.as_global())
            .cloned(),
    );
}

/// Take every entry of a telescope.
fn entries<B: Bound>(telescope: &Telescope<B>, site: &str, positions: &mut Vec<Position>) {
    let mut telescope = telescope;
    while let Telescope::Cons(entry, rest) = telescope {
        push(positions, site, entry);
        telescope = rest.body();
    }
}

/// Whether this type's terminal, after peeling every arrow, is a sort — the
/// test for "this definition denotes a type or a proposition".
fn ends_in_sort(type_: &Term) -> bool {
    let mut current = type_.clone();
    loop {
        let next = match &*current {
            Subterm::Type(_) | Subterm::Prop => return true,
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let mut telescope = telescope;
                loop {
                    match telescope {
                        Telescope::Done(inner) => break (**inner).clone(),
                        Telescope::Cons(_, rest) => telescope = rest.body(),
                    }
                }
            }
            _ => return false,
        };
        current = next;
    }
}

/// Walk a term and mark every type written inside it: binder annotations,
/// match motives, `let` and `rec` declared types, nominal and primitive type
/// formers.
fn annotations(term: &Term, site: &str, positions: &mut Vec<Position>) {
    match &**term {
        // A type former stands for its whole self; nothing inside it is a
        // value position the aggressive reading would treat differently.
        Subterm::FuncType(_)
        | Subterm::TupleType(_)
        | Subterm::InductType(_)
        | Subterm::StructType(_) => {
            push(positions, site, term);
            return;
        }

        Subterm::Func(super::super::Func { telescope, .. }) => {
            entries(telescope, site, positions);
        }

        Subterm::Match(super::super::Match { motive, .. }) => {
            push(positions, site, motive.body());
        }

        Subterm::Let(super::super::Let { bindings, .. }) => {
            for binding in bindings {
                push(positions, site, binding.type_());
            }
        }

        Subterm::Rec(super::super::Rec { group, .. })
        | Subterm::RecMember(super::super::RecMember { group, .. }) => {
            for member in group.iter() {
                push(positions, site, member.type_.body());
            }
        }

        // A nominal construction's parameters are its type arguments.
        Subterm::Variant(Variant { params, .. }) | Subterm::Struct(Struct { params, .. }) => {
            for param in params {
                push(positions, site, param);
            }
        }

        // `Prim::Exit`'s first operand is the type it inhabits; the type
        // formers name their element types.
        Subterm::Prim(
            super::super::Prim::Exit(type_, _)
            | super::super::Prim::LstType(type_)
            | super::super::Prim::CellType(type_),
        ) => push(positions, site, type_),

        _ => {}
    }

    let subterm: &Subterm = term;
    subterm.any_child_term(&mut |child| {
        annotations(child, site, positions);
        false
    });
}

/// Mark every proof handed to a `Prop`-declared structure field or constructor
/// payload.
fn certificates(
    context: &mut Context,
    module: &Module,
    term: &Term,
    site: &str,
    positions: &mut Vec<Position>,
) -> Result<(), Error> {
    match &**term {
        Subterm::Struct(Struct {
            name,
            params,
            fields,
            ..
        }) => {
            if let Some(declaration) = module.struct_decls.get(name) {
                let declared = declaration.fields.clone().open_params(params);
                prop_positions(context, declared, fields, site, positions)?;
            }
        }
        Subterm::Variant(Variant {
            name,
            params,
            tag,
            payload,
            ..
        }) => {
            if let Some(constructor) = module.induct_decls.get(name).and_then(|declaration| {
                declaration
                    .constructors
                    .iter()
                    .find(|(candidate, _)| candidate == tag)
                    .map(|(_, constructor)| constructor)
            }) {
                let declared = constructor.telescope.clone().open_params(params);
                prop_positions(context, declared, payload, site, positions)?;
            }
        }
        _ => {}
    }

    let subterm: &Subterm = term;
    let mut failure = None;
    subterm.any_child_term(
        &mut |child| match certificates(context, module, child, site, positions) {
            Ok(()) => false,
            Err(error) => {
                failure = Some(error);
                true
            }
        },
    );
    match failure {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

/// Mark the actual terms sitting at the `Prop`-sorted entries of a declared
/// telescope.
fn prop_positions<B: Bound>(
    context: &mut Context,
    declared: Telescope<B>,
    actuals: &[Term],
    site: &str,
    positions: &mut Vec<Position>,
) -> Result<(), Error> {
    let mut declared = declared;
    let mut index = 0;
    while let Telescope::Cons(entry, rest) = declared {
        let Some(actual) = actuals.get(index) else {
            return Ok(());
        };
        // A construction found under a binder still carries loose indices, and
        // deciding a sort means reducing, which assumes free occurrences. Such
        // a position is left to the definition-level rule above rather than
        // reduced here.
        if entry.reach() == 0 && actual.reach() == 0 && is_prop(context, &entry)? {
            push(positions, site, actual);
        }
        declared = rest.open(&[actual]);
        index += 1;
    }
    Ok(())
}

/// One reachability answer: which partial definitions the seeds reach.
pub(crate) fn offenders(
    reached: &BTreeSet<Global>,
    classified: &BTreeMap<Global, super::Totality>,
) -> Vec<Global> {
    reached
        .iter()
        .filter(|name| {
            classified
                .get(*name)
                .is_some_and(|totality| !totality.is_total())
        })
        .cloned()
        .collect()
}

/// Why one position fails its obligation.
///
/// A position can fail without naming anything: an inline `rec` that does not
/// descend, or a `Prim::Exit`, is partial on its own account. Reporting only
/// reached names would miss exactly the shapes that need no name.
pub(crate) enum Fault {
    Named(Global),
    Inline,
}
