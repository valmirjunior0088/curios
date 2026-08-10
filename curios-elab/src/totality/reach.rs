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
        Bound, Definition, Enter, Func, FuncType, Global, Intrinsic, Item, Let, Match, Module, Rec,
        Struct, Subterm, Telescope, Term, Totality, Variant,
    },
    std::{
        collections::{BTreeMap, BTreeSet, HashSet},
        rc::Rc,
    },
};

/// A term the erased half of the program must be total in, and what a diagnostic should call it.
///
/// The obligations are stated over *terms*, not over the names those terms mention, because a `rec` written inline in an erased position mentions no name at all. Reporting the reached definitions alone would let `rec Bad : Type = Sink(Bad)`, written as a local binding, satisfy every closure while retying exactly the knot the closure exists to forbid.
///
/// The site is shared rather than owned, which matters because there is one position per *annotated node* and a passing build reads none of them: `report` renders a message only when a fault is found, so an owned label would be an allocation per node built and dropped on every successful compile. One walk carries one site — [`annotations`] says so where it deduplicates — so the label is minted once per call and every position it pushes clones a refcount. This is the shape [`Context::record_checked`](crate::Context) already stores its own sites in.
pub(crate) struct Position {
    pub(crate) term: Term,
    pub(crate) site: Rc<str>,
}

fn push(positions: &mut Vec<Position>, site: &Rc<str>, term: &Term) {
    positions.push(Position {
        term: term.clone(),
        site: Rc::clone(site),
    });
}

/// Every term in a type position.
///
/// "Type position" is read syntactically and generously: a declared type, a binder annotation, a match motive, a declaration telescope, a nominal type former, an intrinsic type former, and the *body* of any definition whose own type ends in a sort — the last being what reaches through `/std/BigNat/Canonical` into `is_trimmed`.
pub(crate) fn type_positions(module: &Module) -> Vec<Position> {
    let mut positions = Vec::new();

    for definition in definitions(module) {
        let name = definition.name.to_string();
        let of_type: Rc<str> = format!("the type of '{name}'").into();
        let own: Rc<str> = format!("'{name}'").into();
        push(&mut positions, &of_type, &definition.type_);
        if ends_in_sort(&definition.type_) {
            let of_body: Rc<str> = format!("the body of '{name}'").into();
            push(&mut positions, &of_body, &definition.body);
        }
        annotations(&definition.body, &own, &mut positions);
        annotations(&definition.type_, &of_type, &mut positions);
    }

    // The entrypoint expression and its annotation are not items, so nothing above reaches them. An exploit needs only a local `rec` and one construction, both of which fit in the trailing expression.
    if let Some(type_) = &module.type_ {
        let site: Rc<str> = "the entrypoint's type".into();
        push(&mut positions, &site, type_);
        annotations(type_, &site, &mut positions);
    }
    if let Some(body) = &module.body {
        annotations(body, &"the entrypoint".into(), &mut positions);
    }

    // A declaration's telescopes are types by construction, and its parameter and field types can name anything.
    for (name, declaration) in &module.induct_decls {
        let site: Rc<str> = format!("a parameter of '{name}'").into();
        entries(&declaration.arity, &site, &mut positions);
        for (tag, constructor) in &declaration.constructors {
            let site: Rc<str> = format!("the payload of '{name}/{tag}'").into();
            entries(&constructor.telescope, &site, &mut positions);
        }
    }
    for (name, declaration) in &module.struct_decls {
        let site: Rc<str> = format!("a parameter of '{name}'").into();
        entries(&declaration.arity, &site, &mut positions);
        let site: Rc<str> = format!("a field of '{name}'").into();
        entries(declaration.fields(), &site, &mut positions);
    }

    // Temporary instrumentation: (T)'s two seedings and (V)'s one are the inputs both obligations walk, and nothing has ever counted them. Remove once answered.
    curios_profile::sample!("totality::type_positions", positions.len());
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
            // Both halves borrowed and filtered straight into the result, rather than cloned, unioned, and filtered — three sets per definition where one suffices, over every definition, once per obligation.
            let named = definition
                .type_
                .free_vars_shared()
                .iter()
                .chain(definition.body.free_vars_shared())
                .filter_map(|free| free.as_global().cloned())
                .collect::<BTreeSet<_>>();
            (definition.name.clone(), named)
        })
        .collect::<BTreeMap<_, _>>();

    let mut reached = seeds;
    let mut frontier = reached.iter().cloned().collect::<Vec<_>>();
    while let Some(name) = frontier.pop() {
        let Some(named) = bodies.get(&name) else {
            // Outside the module under analysis, which means some predecessor unit declares it — the archived prelude, or a unit compiled earlier in this same fold. Either way its own closure was settled when *it* was checked, and `Definition::totality` already folds that closure into the flag the caller reads: partial means something partial is reachable. So not following it is the same answer, not a gap.
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
    module.items.iter().flat_map(Item::definitions)
}

/// Record every definition this term names.
fn mark(term: &Term, seeds: &mut BTreeSet<Global>) {
    // Borrowed, not owned: this runs once per position, and (T) seeds 68,947 of them over the prelude. Taking the owned set would deep-copy every `Free` — each global carrying its qualifier's segments — to read it once and drop it.
    seeds.extend(
        term.free_vars_shared()
            .iter()
            .filter_map(|free| free.as_global())
            .cloned(),
    );
}

/// Take every entry of a telescope.
fn entries<B: Bound>(telescope: &Telescope<B>, site: &Rc<str>, positions: &mut Vec<Position>) {
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

/// Walk a term and mark every type written inside it: binder annotations, match motives, `let` and `rec` declared types, nominal and intrinsic type formers.
#[allow(clippy::mutable_key_type)]
fn annotations(term: &Term, site: &Rc<str>, positions: &mut Vec<Position>) {
    // On the shared `Term::walk` driver, deduplicated on node identity, for one reason each. A string literal's UTF-8 derivation threads its scanner state forwards, so link `i` carries a `step(bᵢ₋₁, … step(b₀, lead))` of depth `i`: the chain is `O(n)` distinct nodes but `O(n²)` *paths* through them, and a walk that revisits shared nodes pays the square while recursing one native frame per link. Both were measured — 2.5s of a 3.5s compile at 12KiB, and a stack overflow above 16KiB.
    //
    // Deduplicating is site-preserving because `site` is fixed for the whole walk: every position this pushes carries the site it was called with, so a node reached twice would only ever push the same position twice.
    let mut state = (HashSet::<Term>::new(), positions);
    term.walk(
        &mut state,
        |state, term| {
            if !state.0.insert(term.clone()) {
                return Enter::Skip(());
            }
            annotate_node(term, site, state.1)
        },
        |_, _, _| (),
    );
}

/// One node's contribution to [`annotations`], and whether the walk descends below it.
fn annotate_node(term: &Term, site: &Rc<str>, positions: &mut Vec<Position>) -> Enter<()> {
    match &**term {
        // A type former stands for its whole self; nothing inside it is a value position the aggressive reading would treat differently.
        Subterm::FuncType(_)
        | Subterm::TupleType(_)
        | Subterm::InductType(_)
        | Subterm::StructType(_) => {
            push(positions, site, term);
            return Enter::Skip(());
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

        Subterm::Rec(Rec { group, .. }) => {
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
        Subterm::Intrinsic(
            Intrinsic::ListType(type_) | Intrinsic::CellType(type_) | Intrinsic::IoType(type_),
        ) => push(positions, site, type_),

        _ => {}
    }

    Enter::Descend
}

/// One reachability answer: which partial definitions the seeds reach.
pub(crate) fn offenders(
    reached: &BTreeSet<Global>,
    classified: &BTreeMap<Global, Totality>,
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
/// A position can fail without naming anything: an inline `rec` that does not descend, or an `Intrinsic::ProcExit`, is partial on its own account. Reporting only reached names would miss exactly the shapes that need no name.
pub(crate) enum Fault {
    Named(Global),
    Inline,
}
