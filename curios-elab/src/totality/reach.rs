//! What the erased half of a program reaches: obligation (T)'s seeding, and the closure both obligations share.
//!
//! **(T)** is seeded here, from every type position, and takes the *aggressive* reading: everything a type reaches must be total, not merely the values a type-level eliminator scrutinizes. The narrow reading is unsound, because a *total* type-level function applied to a *partial value* reties the negative knot that strict positivity exists to forbid.
//!
//! **(V) is not seeded here**, and the difference is the point. (T) asks a *syntactic* question — which written positions are types — and its aggressive reading deliberately answers more generously than any typing judgment would: it seeds the body of every definition whose type ends in a sort, which is how it reaches through `/std/BigNat/Canonical` into `is_trimmed`, a dependency no annotation records. A walk can answer that completely. (V) asks a *typing* question — which terms are propositions — and a walk can only re-derive it from the finished term, incompletely; elaboration already decided it for every term in the program. So (V) is seeded where that answer is known, by [`Context::record_checked`](crate::Context), and arrives here as positions rather than being reconstructed. See [`super::checked_proof_positions`].
//!
//! Neither obligation subsumes the other. The type-level `rec` route violates only (T); the partial-carrier and `exit` routes violate only (V).
//!
//! The closure follows a definition into its **body**, not only its type. That is easy to overlook and load-bearing, for the same `Canonical` reason.
//!
//! At the replay site the module in hand is the user suffix alone, so a prelude definition is a sink of this relation. That is sound for the same reason it is sound for positivity — prelude items cannot mention user code — and it is why partiality persists on the definition rather than being recomputed.

use {
    curios_core::{
        Bound, Definition, Func, FuncType, Global, Item, Let, Match, Module, Prim, Rec, RecMember,
        Struct, Subterm, Telescope, Term, Variant,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// A term the erased half of the program must be total in, and what a diagnostic should call it.
///
/// The obligations are stated over *terms*, not over the names those terms mention, because a `rec` written inline in an erased position mentions no name at all. Reporting the reached definitions alone would let `rec Bad : Type = Sink(Bad)`, written as a local binding, satisfy every closure while retying exactly the knot the closure exists to forbid.
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
/// "Type position" is read syntactically and generously: a declared type, a binder annotation, a match motive, a declaration telescope, a nominal type former, a primitive type former, and the *body* of any definition whose own type ends in a sort — the last being what reaches through `/std/BigNat/Canonical` into `is_trimmed`.
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

    // The entrypoint expression and its annotation are not items, so nothing above reaches them. An exploit needs only a local `rec` and one construction, both of which fit in the trailing expression.
    if let Some(type_) = &module.type_ {
        push(&mut positions, "the entrypoint's type", type_);
        annotations(type_, "the entrypoint's type", &mut positions);
    }
    annotations(&module.body, "the entrypoint", &mut positions);

    // A declaration's telescopes are types by construction, and its parameter and field types can name anything.
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

/// Close `seeds` over the definitions they name, following each into both its type and its body.
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
            // Outside the module under analysis: a replayed prelude definition, whose own closure was settled when the archive was built.
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

/// Whether this type's terminal, after peeling every arrow, is a sort — the test for "this definition denotes a type or a proposition".
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

/// Walk a term and mark every type written inside it: binder annotations, match motives, `let` and `rec` declared types, nominal and primitive type formers.
#[allow(clippy::mutable_key_type)]
fn annotations(term: &Term, site: &str, positions: &mut Vec<Position>) {
    // Iterative, and deduplicated on node identity, for one reason each. A string literal's UTF-8 derivation threads its scanner state forwards, so link `i` carries a `step(bᵢ₋₁, … step(b₀, lead))` of depth `i`: the chain is `O(n)` distinct nodes but `O(n²)` *paths* through them, and a walk that revisits shared nodes pays the square while recursing one native frame per link. Both were measured — 2.5s of a 3.5s compile at 12KiB, and a stack overflow above 16KiB.
    //
    // Deduplicating is site-preserving because `site` is fixed for the whole walk: every position this pushes carries the site it was called with, so a node reached twice would only ever push the same position twice.
    let mut seen: std::collections::HashSet<Term> = std::collections::HashSet::new();
    let mut pending = vec![term.clone()];

    while let Some(term) = pending.pop() {
        if !seen.insert(term.clone()) {
            continue;
        }
        annotate_node(&term, site, positions, &mut pending);
    }
}

/// One node's contribution to [`annotations`], with its children queued.
fn annotate_node(term: &Term, site: &str, positions: &mut Vec<Position>, pending: &mut Vec<Term>) {
    match &**term {
        // A type former stands for its whole self; nothing inside it is a value position the aggressive reading would treat differently.
        Subterm::FuncType(_)
        | Subterm::TupleType(_)
        | Subterm::InductType(_)
        | Subterm::StructType(_) => {
            push(positions, site, term);
            return;
        }

        Subterm::Func(Func { telescope, .. }) => {
            entries(telescope, site, positions);
        }

        Subterm::Match(Match { motive, .. }) => {
            push(positions, site, motive.body());
        }

        Subterm::Let(Let { bindings, .. }) => {
            for binding in bindings {
                push(positions, site, binding.type_());
            }
        }

        Subterm::Rec(Rec { group, .. }) | Subterm::RecMember(RecMember { group, .. }) => {
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

        // The type formers name their element types.
        Subterm::Prim(Prim::LstType(type_) | Prim::CellType(type_)) => push(positions, site, type_),

        _ => {}
    }

    let subterm: &Subterm = term;
    subterm.any_child_term(&mut |child| {
        pending.push(child.clone());
        false
    });
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
/// A position can fail without naming anything: an inline `rec` that does not descend, or a `Prim::Exit`, is partial on its own account. Reporting only reached names would miss exactly the shapes that need no name.
pub(crate) enum Fault {
    Named(Global),
    Inline,
}
