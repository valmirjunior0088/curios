//! Phase 5: reordering declarations so each one's value dependencies come before it (outer in the fold), since a cyclic name graph means source order is no longer a valid binding order.
//!
//! A stable Kahn pass keeps independent declarations in source order. A genuine value cycle leaves nodes unorderable, and they are emitted in source order for someone above to answer for: a declaration's own name is bound by the group it becomes, so a *self*-referencing witness is repaired by `curios_elab::elaborate_module_let` lowering it into a group of one, and a cycle between two witnesses is refused there by name — neither reaches the kernel as an unbound reference. Nothing else can form a cycle at all: definitions that name one another are one group, `let ... and` states it in the source, and a cycle the source did not declare is refused here by name.
//!
//! The embedded, fixed prelude is every item under a privileged root (`sys`/`syn`/`std` — see `RootKind::is_privileged`), classified structurally rather than off a hardcoded name list. `std` and `syn` genuinely cross-reference each other in both directions (e.g. `/syn/Str`'s `classify` calls `/std/Nat`'s `in_range`, while `/std/Nat` itself uses `/syn/Str`'s `Scan`/`Utf8`), so the three privileged roots are topologically sorted together as *one* graph — there is no valid fixed sys/syn/std emission order to split them into independently. `sys` is not a distinct partition here as a result: it is always internally consistent with `syn`/`std` because all three are elaborated as one prelude block.

use {
    super::*,
    curios_utilities::{Mount, Qualifier, SyntaxRegistry},
    std::collections::{BTreeMap, BTreeSet, HashMap, HashSet},
};

/// The full set of names one node's declaration references: its own free vars, plus (for a declared inductive/struct) its registry entry's free vars. An inductive's declaration is wider than its items: the registry entry's constructor payload and target types are elaborated alongside the type-binding group (`curios_elab::elaborate_module_rec` rebuilds the registry telescopes there), so a node declaring a registered name references everything its registry entry does — those names live nowhere in the type binding's own `type_`/`body`. Struct field types live in the registry too.
fn node_reference_names(
    item: &FlatItem,
    declared: &[curios_core::Global],
    induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
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
    names
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

/// A witness row is anonymous, so no name can order a concept's use after its registrations — and one class of use needs exactly that order: a dependent type that must unfold through the operation within its own item, where elaboration's deferred-witness store (which covers every value-level use by retrying between items) comes too late. These edges spell what names cannot: an item that dispatches through a concept — by infix operator, or by referencing one of the concept's method wrappers — wants every witness row of that concept emitted first. Deliberately over-approximate, and therefore *soft*: `topological_order` honors them whenever the hard name edges allow and drops them a node at a time when they deadlock, since which row a use actually needs is a typing fact this stage cannot know, and the genuine `/syn`↔`/std` reference cycle guarantees some deadlock. A dropped node merely returns to the pre-edge order, which every value-level use tolerates. Postfix `!` contributes no edge: `!` cannot appear in a type, so `Monad`/`Lift` witnesses are never needed within-item, and their edges would only widen the deadlocks.
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
fn owner_of(items: &[FlatItem], nodes: &[usize]) -> HashMap<curios_core::Global, usize> {
    nodes
        .iter()
        .flat_map(|&n| items[n].names().into_iter().map(move |name| (name, n)))
        .collect()
}

/// Topologically order `nodes` (assumed ascending, for the lowest-index tiebreak) under `deps` restricted to that set, honoring `soft_deps` — the witness edges — as preferences. Each round emits the lowest-index node whose hard and soft deps are all emitted; when none is fully ready, the lowest hard-ready node gives up its soft constraints (witness edges over-approximate, and `/syn`'s operator uses against `/std`'s string-literal references form one genuine cross-root cycle, so someone must — and dropping a *soft* edge only restores the pre-edge order for that node, where an emission that skipped a *name* edge would manufacture an unbound variable). A genuine hard cycle has no order at all, and is handed back as its members: a group of definitions that name one another is declared with `and`, which makes it one node here, so a cycle between nodes is a group the source did not declare.
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

/// The prelude's topological order as positions *relative to* `prelude_nodes` (ascending), so the whole fixed-root block can be emitted before user code.
///
/// Also the one place the cross-root backward-reference invariant is checked: a privileged declaration referencing a name `rest_owner` maps (i.e. a name only the entry program declares) can never resolve, since the prelude is always emitted first. This can only mean a bug in the embedded `sys`/ `syn`/`std` source itself — never anything a user's own program can trigger — so it panics rather than surfacing as a normal `Error`. This runs only while constructing the build-scoped prepared prelude.
fn prelude_permutation(
    items: &[FlatItem],
    prelude_nodes: &[usize],
    induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
    rest_owner: &HashMap<curios_core::Global, usize>,
    wrapper_owner: &HashMap<curios_core::Global, Qualifier>,
    syntax: &SyntaxRegistry,
) -> Vec<usize> {
    let owner = owner_of(items, prelude_nodes);
    let rows = witness_rows(items, prelude_nodes);
    let mut deps = HashMap::with_capacity(prelude_nodes.len());
    let mut soft_deps = HashMap::with_capacity(prelude_nodes.len());
    for &n in prelude_nodes {
        let declared = items[n].names();
        let names = node_reference_names(&items[n], &declared, induct_decls, struct_decls);
        if let Some(name) = names
            .iter()
            .find(|name| !owner.contains_key(*name) && rest_owner.contains_key(*name))
        {
            panic!(
                "'{}' (in the standard library) references '{}', which is only declared \
                 in the entry program — the standard library is always compiled before the \
                 entry program, so this is a bug in the embedded prelude source",
                declared
                    .first()
                    .map_or("<anonymous>".to_string(), curios_core::Global::symbol),
                name.symbol(),
            );
        }
        deps.insert(n, dep_nodes(n, &names, &owner));
        soft_deps.insert(
            n,
            witness_dep_nodes(n, &items[n], &names, wrapper_owner, &rows, syntax),
        );
    }

    let relative = prelude_nodes
        .iter()
        .enumerate()
        .map(|(rel, &node)| (node, rel))
        .collect::<HashMap<usize, usize>>();

    // A cycle here is a prelude bug for the same reason a forward cross-root reference is: no user program can put a `let` of its own among these nodes.
    let order = topological_order(prelude_nodes, &deps, &soft_deps).unwrap_or_else(|cycle| {
        panic!(
            "the standard library declares {} as separate definitions that reference each other — \
             a mutually recursive group is declared with `and`, so this is a bug in the embedded \
             prelude source",
            cycle_names(items, &cycle).join(", "),
        )
    });

    order.iter().map(|node| relative[node]).collect()
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
    mounts: &[Mount],
    induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
    syntax: &SyntaxRegistry,
) -> Result<Vec<FlatItem>, Error> {
    let count = items.len();

    let is_prelude = items
        .iter()
        .map(|item| item.in_prelude(mounts))
        .collect::<Vec<bool>>();
    let prelude_nodes = (0..count)
        .filter(|&i| is_prelude[i])
        .collect::<Vec<usize>>();
    let rest = (0..count)
        .filter(|&i| !is_prelude[i])
        .collect::<Vec<usize>>();

    let rest_owner = owner_of(&items, &rest);
    let wrapper_owner = wrapper_owners(&items);

    let mut order = Vec::with_capacity(count);

    if !prelude_nodes.is_empty() {
        let permutation = prelude_permutation(
            &items,
            &prelude_nodes,
            induct_decls,
            struct_decls,
            &rest_owner,
            &wrapper_owner,
            syntax,
        );
        order.extend(permutation.into_iter().map(|rel| prelude_nodes[rel]));
    }

    // Everything else (user code, plus any non-prelude library a custom loader serves): topologically ordered among itself, after the whole prelude. Its dependencies on prelude items are already satisfied by the prefix above, so the owner map (and thus the dep edges) need only cover `rest` — witness edges included: a rest item's needed prelude rows sit in the emitted prefix, and only its own partition's rows still need ordering.
    let rest_rows = witness_rows(&items, &rest);
    let mut rest_deps = HashMap::with_capacity(rest.len());
    let mut rest_soft_deps = HashMap::with_capacity(rest.len());
    for &n in &rest {
        let declared = items[n].names();
        let names = node_reference_names(&items[n], &declared, induct_decls, struct_decls);
        rest_deps.insert(n, dep_nodes(n, &names, &rest_owner));
        rest_soft_deps.insert(
            n,
            witness_dep_nodes(n, &items[n], &names, &wrapper_owner, &rest_rows, syntax),
        );
    }
    // Refused at the first member's declaration: the report names every definition on the cycle, and the source position it needs is one the reader can act on.
    let rest_order = topological_order(&rest, &rest_deps, &rest_soft_deps).map_err(|cycle| {
        let error = Error::UndeclaredCycle {
            names: cycle_names(&items, &cycle),
        };
        match first_let(&items[cycle[0]]).type_.span() {
            Some(span) => error.at(span.clone()),
            None => error,
        }
    })?;
    order.extend(rest_order);

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
