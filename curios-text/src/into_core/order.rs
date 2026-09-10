//! Phase 5: reordering declarations so each one's value dependencies come before it (outer in the fold), since a cyclic name graph means source order is no longer a valid binding order.
//!
//! A stable Kahn pass keeps independent declarations in source order. A genuine value cycle leaves nodes unorderable, and they are emitted in source order for someone above to answer for: a declaration's own name is bound by the group it becomes, so a *self*-referencing witness is repaired by `curios_elab::elaborate_module_let` lowering it into a group of one, and a cycle between two witnesses is refused there by name — neither reaches the kernel as an unbound reference. Nothing else can form a cycle at all: definitions that name one another are one group, `let ... and` states it in the source, and a cycle the source did not declare is refused here by name.
//!
//! **One partition, over the unit's own items.** This pass used to sort the fixed prelude ahead of everything else, because the prelude and the entry arrived as one item list and the prelude half had to be emitted first. They do not arrive together any more: a unit holds its own items and nothing else, so every item here is under one mount and every name from outside it is satisfied by the scope rather than by a node in this graph. The partition that expressed that, and the two panics that guarded the prelude half of it, are gone with it — a cycle is a cycle whoever wrote it, and it is reported as one.

use {
    super::*,
    curios_utilities::{Qualifier, SyntaxRegistry},
    std::collections::{BTreeMap, BTreeSet, HashMap, HashSet},
};

/// The full set of names one node's declaration references: its own free vars, plus (for a declared inductive/struct) its registry entry's free vars. An inductive's declaration is wider than its items: the registry entry's constructor payload and target types are elaborated alongside the type-binding group (`curios_elab::elaborate_module_rec` rebuilds the registry telescopes there), so a node declaring a registered name references everything its registry entry does — those names live nowhere in the type binding's own `type_`/`body`. Struct field types live in the registry too.
pub(super) fn node_reference_names(
    item: &FlatItem,
    declared: &[curios_core::Global],
    induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
    syntax: &SyntaxRegistry,
) -> HashSet<curios_core::Global> {
    let mut names = item.free_vars();
    for name in declared {
        if let Some(induct_decl) = induct_decls.get(name) {
            names.extend(induct_free_vars(induct_decl));
        }
        if let Some(struct_decl) = struct_decls.get(name) {
            names.extend(struct_free_vars(struct_decl));
        }
    }
    names.extend(derived_vocabulary(item, syntax));
    names
}

/// The names a derived witness body will reference once elaboration writes it — the vocabulary of the derivation registered for the concept its signature names, which the `Derive` transient stands for without spelling. Hard edges, as the written body's `Var`s would be: within the prelude's own unit the renderers, method wrappers and string machinery must elaborate before the witness that applies them. Restricted to the signature's concept so one derivation's edges never reach the other's rows, and empty for an item that derives nothing.
///
/// **Every name a body *writes*, and nothing it merely dispatches through.** A concept reached by infix operator is not here and must not be: `&&` in the equality body is rebuilt by `elaborate_infix` as a projection off a resolved witness, never a wrapper `Var`, so it names no global at all. It cannot even reach [`witness_dep_nodes`]' soft edges, whose operator half comes from `FlatItem::infix_ops` — which walks a body that is, at this point, a transient with no children.
fn derived_vocabulary(item: &FlatItem, syntax: &SyntaxRegistry) -> Vec<curios_core::Global> {
    let lets = match item {
        FlatItem::Let(let_) => std::slice::from_ref(let_),
        FlatItem::Rec(lets) => lets.as_slice(),
    };
    let mut vocabulary = Vec::new();
    for let_ in lets.iter().filter(|let_| let_.body.has_derive()) {
        let Some(concept) = witness_concept(let_).and_then(|name| name.qualifier().cloned()) else {
            continue;
        };
        let Some(derivation) = syntax
            .derivations
            .rows()
            .find(|derivation| derivation.concept_field().concept.qualifier() == concept)
        else {
            continue;
        };
        vocabulary.extend(
            derivation
                .vocabulary()
                .into_iter()
                .map(curios_core::Global::Authored),
        );
    }
    vocabulary
}

/// The concept a witness row registers into — the head of its signature's terminal concept application, peeled through the premise telescope. Lowered form only: pre-elaboration the terminal is an `Apply`/`Var` spine, so the head is a free global rather than a `StructType` normal form (kept as a fallback for synthetic inputs).
fn witness_concept(let_: &FlatLet) -> Option<curios_core::Global> {
    fn head_of(term: &curios_core::Term) -> Option<curios_core::Global> {
        match &**term {
            curios_core::Subterm::FuncType(func_type) => {
                let mut telescope = &func_type.telescope;
                loop {
                    match telescope {
                        curios_core::Telescope::Done(body) => return head_of(body),
                        curios_core::Telescope::Cons(_, scope) => telescope = scope.body(),
                    }
                }
            }
            curios_core::Subterm::Apply(apply) => head_of(&apply.head),
            curios_core::Subterm::Instance(inst) => inst
                .head
                .head_name()
                .and_then(|free| free.as_global())
                .cloned(),
            curios_core::Subterm::Var(var) => {
                var.as_free().and_then(|free| free.as_global()).cloned()
            }
            curios_core::Subterm::StructType(struct_type) => Some(struct_type.name.clone()),
            _ => None,
        }
    }

    matches!(let_.kind, curios_core::DefinitionKind::Witness)
        .then(|| head_of(&let_.type_))
        .flatten()
}

/// Method-wrapper name → owning concept, over every item in the compilation: a wrapper referenced from either partition identifies its concept, wherever that concept's witness rows live.
fn wrapper_owners(items: &[FlatItem]) -> HashMap<curios_core::Global, Qualifier> {
    items
        .iter()
        .flat_map(|item| match item {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
        })
        .filter_map(|let_| match &let_.kind {
            curios_core::DefinitionKind::ConceptMethod { owner } => {
                Some((let_.name.clone(), owner.clone()))
            }
            _ => None,
        })
        .collect()
}

/// The witness rows among `nodes`, grouped by the concept they register into.
fn witness_rows(items: &[FlatItem], nodes: &[usize]) -> HashMap<Qualifier, Vec<usize>> {
    let mut rows: HashMap<Qualifier, Vec<usize>> = HashMap::new();
    for &node in nodes {
        let lets = match &items[node] {
            FlatItem::Let(let_) => std::slice::from_ref(let_),
            FlatItem::Rec(lets) => lets.as_slice(),
        };
        for let_ in lets {
            if let Some(concept) = witness_concept(let_)
                && let Some(qualifier) = concept.qualifier()
            {
                rows.entry(qualifier.clone()).or_default().push(node);
            }
        }
    }
    rows
}

/// A witness row is anonymous, so no name can order a concept's use after its registrations — and one class of use needs exactly that order: a dependent type that must unfold through the operation within its own item, where elaboration's deferred-witness store (which covers every value-level use by retrying between items) comes too late. These edges spell what names cannot: an item that dispatches through a concept — by infix operator, or by referencing one of the concept's method wrappers — wants every witness row of that concept emitted first. Deliberately over-approximate, and therefore *soft*: `topological_order` honors them whenever the hard name edges allow and drops them a node at a time when they deadlock, since which row a use actually needs is a typing fact this stage cannot know, and `/std`'s own reference cycles — the operator concepts against the literal machinery that dispatches through them — guarantee some deadlock. A dropped node merely returns to the pre-edge order, which every value-level use tolerates. Postfix `!` contributes no edge: `!` cannot appear in a type, so `Monad`/`Lift` witnesses are never needed within-item, and their edges would only widen the deadlocks.
fn witness_dep_nodes(
    node: usize,
    item: &FlatItem,
    names: &HashSet<curios_core::Global>,
    wrapper_owner: &HashMap<curios_core::Global, Qualifier>,
    rows: &HashMap<Qualifier, Vec<usize>>,
    syntax: &SyntaxRegistry,
) -> HashSet<usize> {
    let mut concepts: HashSet<Qualifier> = item
        .infix_ops()
        .into_iter()
        .map(|op| syntax.operator.concept_field(op).concept.qualifier())
        .collect();
    concepts.extend(
        names
            .iter()
            .filter_map(|name| wrapper_owner.get(name).cloned()),
    );

    concepts
        .iter()
        .flat_map(|concept| rows.get(concept).into_iter().flatten())
        .copied()
        .filter(|&dep| dep != node)
        .collect()
}

/// The nodes a node depends on: those `owner` maps its referenced names to. Self-edges and names `owner` does not map (intrinsics, or items outside the partition `owner` was restricted to) drop out.
fn dep_nodes(
    node: usize,
    names: &HashSet<curios_core::Global>,
    owner: &HashMap<curios_core::Global, usize>,
) -> HashSet<usize> {
    names
        .iter()
        .filter_map(|name| owner.get(name).copied())
        .filter(|&dep| dep != node)
        .collect()
}

/// Owner index (declared name → node) over the given nodes only.
pub(super) fn owner_of(items: &[FlatItem], nodes: &[usize]) -> HashMap<curios_core::Global, usize> {
    nodes
        .iter()
        .flat_map(|&n| items[n].names().into_iter().map(move |name| (name, n)))
        .collect()
}

/// Topologically order `nodes` (assumed ascending, for the lowest-index tiebreak) under `deps` restricted to that set, honoring `soft_deps` — the witness edges — as preferences. Each round emits the lowest-index node whose hard and soft deps are all emitted; when none is fully ready, the lowest hard-ready node gives up its soft constraints (witness edges over-approximate, and the operator concepts' uses against the string-literal references form one genuine cross-root cycle, so someone must — and dropping a *soft* edge only restores the pre-edge order for that node, where an emission that skipped a *name* edge would manufacture an unbound variable). A genuine hard cycle has no order at all, and is handed back as its members: a group of definitions that name one another is declared with `and`, which makes it one node here, so a cycle between nodes is a group the source did not declare.
fn topological_order(
    nodes: &[usize],
    deps: &HashMap<usize, HashSet<usize>>,
    soft_deps: &HashMap<usize, HashSet<usize>>,
) -> Result<Vec<usize>, Vec<usize>> {
    let mut emitted = HashSet::with_capacity(nodes.len());
    let mut order = Vec::with_capacity(nodes.len());

    while order.len() < nodes.len() {
        let ready = nodes
            .iter()
            .copied()
            .find(|&n| {
                !emitted.contains(&n)
                    && deps[&n].iter().all(|dep| emitted.contains(dep))
                    && soft_deps[&n].iter().all(|dep| emitted.contains(dep))
            })
            .or_else(|| {
                nodes.iter().copied().find(|&n| {
                    !emitted.contains(&n) && deps[&n].iter().all(|dep| emitted.contains(dep))
                })
            });

        let Some(ready) = ready else {
            return Err(hard_cycle(nodes, deps, &emitted));
        };
        emitted.insert(ready);
        order.push(ready);
    }

    Ok(order)
}

/// One cycle among the nodes `topological_order` could not emit, in dependency order. Every remaining node waits on a remaining node, so following the lowest unemitted dependency from the lowest remaining node must revisit a node, and the walk from that node's first visit is the cycle.
fn hard_cycle(
    nodes: &[usize],
    deps: &HashMap<usize, HashSet<usize>>,
    emitted: &HashSet<usize>,
) -> Vec<usize> {
    let start = nodes
        .iter()
        .copied()
        .find(|node| !emitted.contains(node))
        .expect("a node remains while order is incomplete");
    let mut path = vec![start];
    loop {
        let current = *path.last().expect("the path starts non-empty");
        let next = deps[&current]
            .iter()
            .copied()
            .filter(|dep| !emitted.contains(dep))
            .min()
            .expect("a node no hard-ready set admits waits on a remaining node");
        if let Some(position) = path.iter().position(|&node| node == next) {
            return path[position..].to_vec();
        }
        path.push(next);
    }
}

/// The names a cycle is reported by: each node's first declared name, spelled as a path.
fn cycle_names(items: &[FlatItem], cycle: &[usize]) -> Vec<String> {
    cycle
        .iter()
        .filter_map(|&node| items[node].names().first().map(curios_core::Global::symbol))
        .collect()
}

pub(super) fn order_flat_items(
    items: Vec<FlatItem>,
    induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
    syntax: &SyntaxRegistry,
) -> Result<Vec<FlatItem>, Error> {
    let nodes = (0..items.len()).collect::<Vec<usize>>();
    let owner = owner_of(&items, &nodes);
    let wrapper_owner = wrapper_owners(&items);
    let rows = witness_rows(&items, &nodes);

    // Only this unit's own items, because only they are in this graph. A name from the scope is owned by no node here, so `dep_nodes` records no edge for it — which is right: the unit it belongs to was emitted whole before this one began.
    let mut deps = HashMap::with_capacity(nodes.len());
    let mut soft_deps = HashMap::with_capacity(nodes.len());
    for &n in &nodes {
        let declared = items[n].names();
        let names = node_reference_names(&items[n], &declared, induct_decls, struct_decls, syntax);
        deps.insert(n, dep_nodes(n, &names, &owner));
        soft_deps.insert(
            n,
            witness_dep_nodes(n, &items[n], &names, &wrapper_owner, &rows, syntax),
        );
    }
    // Refused at the first member's written name, which is what the reader prefixes with `and`: the report names every definition on the cycle, and the source position it needs is one the reader can act on. The written type is the fallback for a definition the compiler named, since function sugar synthesizes a type with no span of its own.
    let order = topological_order(&nodes, &deps, &soft_deps).map_err(|cycle| {
        let error = Error::UndeclaredCycle {
            names: cycle_names(&items, &cycle),
        };
        let first = first_let(&items[cycle[0]]);
        match first.span.clone().or_else(|| first.type_.span()) {
            Some(span) => error.at(span),
            None => error,
        }
    })?;

    let mut slots = items
        .into_iter()
        .map(Some)
        .collect::<Vec<Option<FlatItem>>>();
    Ok(order
        .into_iter()
        .map(|node| slots[node].take().unwrap())
        .collect())
}

fn first_let(item: &FlatItem) -> &FlatLet {
    match item {
        FlatItem::Let(let_) => let_,
        FlatItem::Rec(lets) => lets.first().expect("a group has a member"),
    }
}

/// The external references of an inductive registry entry: every free var of its telescopes. Binder names (parameters, payload binders) are captured by `Telescope::build` and never appear here; the index types' references also live in the type binding's own signature, but are included for robustness.
fn induct_free_vars(induct_decl: &curios_core::InductDecl) -> HashSet<curios_core::Global> {
    induct_decl
        .arity
        .free_vars()
        .into_iter()
        .chain(
            induct_decl
                .constructors
                .iter()
                .flat_map(|(_, param)| param.telescope.free_vars()),
        )
        .filter_map(|name| name.as_global().cloned())
        .collect()
}

/// The external references of a struct registry entry: every free var of its arity — its parameter domains and the field telescope they terminate in. Like `induct_free_vars`, this is what makes a struct's type-former node depend on the (e.g. intrinsic) types its fields mention — they live nowhere in the type-former's own body, which is just the `StructType` normal form.
fn struct_free_vars(struct_decl: &curios_core::StructDecl) -> HashSet<curios_core::Global> {
    struct_decl
        .arity
        .free_vars()
        .into_iter()
        .filter_map(|name| name.as_global().cloned())
        .collect()
}

#[derive(Clone)]
pub(super) struct AliasEdge {
    pub(super) target: curios_core::Global,
    pub(super) dependencies: Option<BTreeSet<curios_core::Global>>,
}

pub(super) fn flat_aliases(items: &[FlatItem]) -> HashMap<curios_core::Global, AliasEdge> {
    let lets = items.iter().flat_map(|item| match item {
        FlatItem::Let(let_) => std::slice::from_ref(let_),
        FlatItem::Rec(lets) => lets.as_slice(),
    });

    lets.filter_map(|let_| {
        // An alias target is a top-level definition. A body that is a bare *local* is not an alias — a discriminant test now, where it used to be a leading-`/` test on the spelling.
        let direct = let_.body.direct_type_alias_target(&let_.type_);
        let target = direct
            .or_else(|| let_.body.transparent_alias_target())
            .and_then(curios_core::Free::as_global)?
            .clone();

        Some((
            let_.name.clone(),
            AliasEdge {
                target,
                dependencies: direct.map(|_| {
                    let_.body
                        .free_vars()
                        .into_iter()
                        .filter_map(|name| name.as_global().cloned())
                        .collect()
                }),
            },
        ))
    })
    .collect()
}
