use {
    super::ModuleInfo,
    crate::{Entrypoint, Error, GroupItem, Module, Name, TopItem, UseGroup},
    curios_base::Entropy,
    curios_core::Qualifier,
    std::{
        collections::{HashMap, HashSet},
        rc::Rc,
    },
};

// The export view of a module: public names only, each pointing at the canonical
// declaration site. Built to a fixed point before any body is elaborated.
#[derive(Clone)]
pub(super) struct PublicInterface {
    pub children: HashMap<String, Entry>,
    pub bindings: HashMap<String, Entry>,
}

impl PublicInterface {
    fn new() -> Self {
        Self {
            children: HashMap::new(),
            bindings: HashMap::new(),
        }
    }
}

#[derive(Clone)]
pub(super) struct Entry {
    pub target: Qualifier,
    pub source: Source,
}

// Provenance of an export entry. A slot may be claimed by at most one source; a
// re-derivation by the same source is idempotent, a different source conflicts.
#[derive(Clone, PartialEq, Eq)]
pub(super) enum Source {
    Direct,
    ReExport(usize),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Ns {
    Module,
    Binding,
}

// A `pub use` lifted out of the syntax tree, tagged with the module it lives in
// and a stable id (its `ReExport` source). Collected once; the fixed point reads
// only these plus the interface map.
struct PubUse {
    module: Qualifier,
    id: usize,
    name: Name,
    group: UseGroup,
}

// Phase 2 + 3 entry point: seed direct public interfaces (including inductive
// constructor modules), then resolve every `pub use` to a fixed point. Also adds
// constructor modules to `table` (the direct-interface view) so phase 4 can
// classify private-vs-missing accesses through them.
pub(super) fn resolve(
    entrypoint: &Entrypoint,
    modules: &HashMap<Qualifier, Rc<Module>>,
    table: &mut HashMap<Qualifier, ModuleInfo>,
) -> Result<HashMap<Qualifier, PublicInterface>, Error> {
    let mut public = HashMap::new();
    let mut pub_uses = Vec::new();
    let counter = Entropy::<usize>::new();

    seed(
        &entrypoint.module.items,
        &Qualifier::empty(),
        modules,
        table,
        &mut public,
        &mut pub_uses,
        &counter,
    )?;

    fixed_point(&mut public, table, &pub_uses)?;
    classify_dead(&public, table, &pub_uses)?;

    Ok(public)
}

// Phase 2. Walk the module tree (mirroring `discover`/`process_items`): for each
// module, seed its `PublicInterface` from the direct interface already in
// `table`; materialize each inductive's constructor module; and collect every
// `pub use`.
fn seed(
    items: &[TopItem],
    prefix: &Qualifier,
    modules: &HashMap<Qualifier, Rc<Module>>,
    table: &mut HashMap<Qualifier, ModuleInfo>,
    public: &mut HashMap<Qualifier, PublicInterface>,
    pub_uses: &mut Vec<PubUse>,
    counter: &Entropy,
) -> Result<(), Error> {
    let mut interface = PublicInterface::new();
    let info = table
        .get(prefix)
        .expect("module info present from discovery");

    for label in info.public_children() {
        let target = prefix.with(&label);
        interface.children.insert(
            label,
            Entry {
                target,
                source: Source::Direct,
            },
        );
    }

    for label in info.public_bindings() {
        let target = prefix.with(&label);
        interface.bindings.insert(
            label,
            Entry {
                target,
                source: Source::Direct,
            },
        );
    }

    public.insert(prefix.clone(), interface);

    for item in items {
        match item {
            TopItem::Use(use_item) if use_item.is_pub => {
                pub_uses.push(PubUse {
                    module: prefix.clone(),
                    id: counter.fresh(),
                    name: use_item.name.clone(),
                    group: use_item.group.clone(),
                });
            }
            TopItem::Induct(group) => {
                for inductive in group {
                    let ctor = prefix.with(&inductive.label);

                    // Constructors are always public *within their nested
                    // module* — its direct info and public interface are both
                    // seeded unconditionally, exactly like an ordinary module
                    // whose bindings are all `pub`. Visibility is governed
                    // entirely by the inductive itself: the child-module flag in
                    // the parent's info gates every walk from outside, so a
                    // constructor is exactly as visible as its inductive and the
                    // declaring module can always use a non-`pub` inductive.
                    let mut direct = ModuleInfo::new();
                    for case in &inductive.cases {
                        direct.insert_binding(case.label.clone(), true)?;
                    }
                    table.insert(ctor.clone(), direct);

                    let mut interface = PublicInterface::new();
                    for case in &inductive.cases {
                        let target = ctor.with(&case.label);
                        interface.bindings.insert(
                            case.label.clone(),
                            Entry {
                                target,
                                source: Source::Direct,
                            },
                        );
                    }
                    public.insert(ctor, interface);
                }
            }
            TopItem::Concept(concept) => {
                // A concept's method wrappers live in a nested namespace, exactly
                // like an inductive's constructors: seed both the direct info and
                // the public interface of that module unconditionally (the fields
                // are always public within it), so `Show/show` resolves. The
                // concept's own visibility gates the walk from outside via the
                // parent's child-module flag.
                let namespace = prefix.with(&concept.label);

                let mut direct = ModuleInfo::new();
                for field in &concept.fields {
                    direct.insert_binding(field.label.clone(), true)?;
                }
                table.insert(namespace.clone(), direct);

                let mut interface = PublicInterface::new();
                for field in &concept.fields {
                    let target = namespace.with(&field.label);
                    interface.bindings.insert(
                        field.label.clone(),
                        Entry {
                            target,
                            source: Source::Direct,
                        },
                    );
                }
                public.insert(namespace, interface);
            }
            TopItem::Mod(mod_item) => {
                let path = prefix.with(&mod_item.label);
                let child = match &mod_item.module {
                    Some(module) => &module.items,
                    None => {
                        &modules
                            .get(&path)
                            .expect("module loaded during discovery")
                            .items
                    }
                };

                seed(child, &path, modules, table, public, pub_uses, counter)?;
            }
            _ => {}
        }
    }

    Ok(())
}

// Phase 3. Repeatedly resolve every `pub use` against the current interface
// graph, inserting whatever is resolvable, until a full round adds nothing.
fn fixed_point(
    public: &mut HashMap<Qualifier, PublicInterface>,
    table: &HashMap<Qualifier, ModuleInfo>,
    pub_uses: &[PubUse],
) -> Result<(), Error> {
    loop {
        let mut changed = false;

        for use_ in pub_uses {
            for (ns, label, target) in resolvable(public, table, use_) {
                let entry = Entry {
                    target,
                    source: Source::ReExport(use_.id),
                };
                changed |= insert(public, &use_.module, ns, label, entry)?;
            }
        }

        if !changed {
            break;
        }
    }

    Ok(())
}

// The selectors of one `pub use` that resolve against the interfaces *as they
// currently stand*. Anything not yet present is skipped (deferred to a later
// round). Never errors — dead entries are classified after the fixed point.
fn resolvable(
    public: &HashMap<Qualifier, PublicInterface>,
    table: &HashMap<Qualifier, ModuleInfo>,
    use_: &PubUse,
) -> Vec<(Ns, String, Qualifier)> {
    let Some(provider) = provider(public, table, &use_.module, &use_.name) else {
        return Vec::new();
    };

    let Some(interface) = public.get(&provider) else {
        return Vec::new();
    };

    let mut out = Vec::new();

    match &use_.group {
        UseGroup::Glob => {
            for (label, entry) in &interface.children {
                out.push((Ns::Module, label.clone(), entry.target.clone()));
            }
            for (label, entry) in &interface.bindings {
                out.push((Ns::Binding, label.clone(), entry.target.clone()));
            }
        }
        UseGroup::Named(items) => {
            for item in items {
                match item {
                    GroupItem::Mod(label) => {
                        if let Some(entry) = interface.children.get(label) {
                            out.push((Ns::Module, label.clone(), entry.target.clone()));
                        }
                    }
                    GroupItem::Let(label) => {
                        if let Some(entry) = interface.bindings.get(label) {
                            out.push((Ns::Binding, label.clone(), entry.target.clone()));
                        }
                    }
                    GroupItem::Both(label) => {
                        if let Some(entry) = interface.children.get(label) {
                            out.push((Ns::Module, label.clone(), entry.target.clone()));
                        }
                        if let Some(entry) = interface.bindings.get(label) {
                            out.push((Ns::Binding, label.clone(), entry.target.clone()));
                        }
                    }
                }
            }
        }
    }

    out
}

// The `Option` view of `resolve_provider`, for callers where non-resolution is
// benign: a selector that does not resolve *yet* during the fixed point, or a
// chain hop that simply does not exist. The terminal `classify_dead` pass calls
// `resolve_provider` directly to surface the precise error instead.
fn provider(
    public: &HashMap<Qualifier, PublicInterface>,
    table: &HashMap<Qualifier, ModuleInfo>,
    module: &Qualifier,
    name: &Name,
) -> Option<Qualifier> {
    resolve_provider(public, table, module, name).ok()
}

// Insert one resolved entry into a slot. Returns whether the map changed.
fn insert(
    public: &mut HashMap<Qualifier, PublicInterface>,
    module: &Qualifier,
    ns: Ns,
    label: String,
    entry: Entry,
) -> Result<bool, Error> {
    let interface = public.get_mut(module).expect("seeded module");
    let slot = match ns {
        Ns::Module => &mut interface.children,
        Ns::Binding => &mut interface.bindings,
    };

    match slot.get(&label) {
        Some(existing) if existing.source == entry.source => Ok(false),
        Some(_) => Err(Error::ExportConflict { label }),
        None => {
            slot.insert(label, entry);
            Ok(true)
        }
    }
}

// Phase 3 post-pass. After convergence any selector still resolving to nothing
// is an error, classified by following its re-export chain: a chain that returns
// to a slot already seen is a cyclic re-export, otherwise the target is missing.
fn classify_dead(
    public: &HashMap<Qualifier, PublicInterface>,
    table: &HashMap<Qualifier, ModuleInfo>,
    pub_uses: &[PubUse],
) -> Result<(), Error> {
    for use_ in pub_uses {
        let provider = resolve_provider(public, table, &use_.module, &use_.name)?;

        let interface = public.get(&provider).expect("seeded module");

        match &use_.group {
            // A glob with a reachable provider always resolves (possibly to no
            // labels); only an unreachable source is an error, handled above.
            UseGroup::Glob => {}
            UseGroup::Named(items) => {
                for item in items {
                    let (label, in_module, in_binding) = match item {
                        GroupItem::Mod(label) => (label, true, false),
                        GroupItem::Let(label) => (label, false, true),
                        GroupItem::Both(label) => (label, true, true),
                    };

                    let module_ok = !in_module || interface.children.contains_key(label);
                    let binding_ok = !in_binding || interface.bindings.contains_key(label);

                    // `{x}` resolves if either namespace filled; `{mod x}` /
                    // `{let x}` require their own namespace.
                    let resolved = match item {
                        GroupItem::Both(_) => {
                            interface.children.contains_key(label)
                                || interface.bindings.contains_key(label)
                        }
                        GroupItem::Mod(_) => module_ok,
                        GroupItem::Let(_) => binding_ok,
                    };

                    if !resolved {
                        let ns = if in_binding { Ns::Binding } else { Ns::Module };
                        return Err(classify_label(
                            public, table, pub_uses, &provider, ns, label,
                        ));
                    }
                }
            }
        }
    }

    Ok(())
}

// Walk the re-export chain for an unresolved `(module, ns, label)` to decide
// whether it is a cycle or a genuine miss.
fn classify_label(
    public: &HashMap<Qualifier, PublicInterface>,
    table: &HashMap<Qualifier, ModuleInfo>,
    pub_uses: &[PubUse],
    module: &Qualifier,
    ns: Ns,
    label: &str,
) -> Error {
    let mut visited = HashSet::new();
    let mut current = module.clone();

    loop {
        if !visited.insert(current.clone()) {
            return Error::CyclicReExport {
                label: label.to_string(),
            };
        }

        match producer(public, table, pub_uses, &current, ns, label) {
            Some(next) => current = next,
            None => {
                return Error::NoSuchUseTarget {
                    label: label.to_string(),
                    parent: module.join(),
                };
            }
        }
    }
}

// The provider module of a `pub use` in `module` that would supply `label` in
// namespace `ns`, if any (named selector match, or a glob whose source is
// reachable). Used only by chain classification.
fn producer(
    public: &HashMap<Qualifier, PublicInterface>,
    table: &HashMap<Qualifier, ModuleInfo>,
    pub_uses: &[PubUse],
    module: &Qualifier,
    ns: Ns,
    label: &str,
) -> Option<Qualifier> {
    for use_ in pub_uses {
        if &use_.module != module {
            continue;
        }

        let names = match &use_.group {
            UseGroup::Glob => true,
            UseGroup::Named(items) => items.iter().any(|item| match item {
                GroupItem::Mod(l) => matches!(ns, Ns::Module) && l == label,
                GroupItem::Let(l) => matches!(ns, Ns::Binding) && l == label,
                GroupItem::Both(l) => l == label,
            }),
        };

        if names && let Some(provider) = provider(public, table, module, &use_.name) {
            return Some(provider);
        }
    }

    None
}

// Walk a `use` source path to its provider module, following re-export targets.
// A relative path's first segment may be the current module's own child of any
// visibility (you are inside it, so its privacy does not apply to itself); every
// later segment, and every segment of an absolute path, must be a public child.
// Each resolved hop is guarded so a non-privileged consumer cannot follow a
// re-export into an internal root (`sys`) by any spelling. On failure, returns
// the precise error at the offending segment, using the direct-interface table
// to tell private from absent; `provider` is the `Option` view for callers
// where that is benign.
fn resolve_provider(
    public: &HashMap<Qualifier, PublicInterface>,
    table: &HashMap<Qualifier, ModuleInfo>,
    module: &Qualifier,
    name: &Name,
) -> Result<Qualifier, Error> {
    let segments = name.qualifier().segments();

    let (mut current, walk) = if name.is_abs() {
        (Qualifier::empty(), segments)
    } else {
        let first = &segments[0];
        let start = match public.get(module).and_then(|i| i.children.get(first)) {
            Some(entry) => entry.target.clone(),
            None => match table.get(module).and_then(|i| i.get_child(first)) {
                // Own direct child (any visibility) is a valid start; only an
                // outright non-child fails here.
                Some(_) => module.with(first),
                None => {
                    return Err(Error::ChildModuleNotFound {
                        segment: first.clone(),
                    });
                }
            },
        };

        (start, &segments[1..])
    };

    super::guard_internal_root(module, current.segments())?;
    for segment in walk {
        match public.get(&current).and_then(|i| i.children.get(segment)) {
            Some(entry) => current = entry.target.clone(),
            None => return Err(segment_error(table, &current, segment)),
        }
        super::guard_internal_root(module, current.segments())?;
    }

    Ok(current)
}

fn segment_error(
    table: &HashMap<Qualifier, ModuleInfo>,
    module: &Qualifier,
    segment: &str,
) -> Error {
    match table.get(module).and_then(|info| info.get_child(segment)) {
        Some(false) => Error::PrivateChildModule {
            segment: segment.to_string(),
        },
        _ => Error::ChildModuleNotFound {
            segment: segment.to_string(),
        },
    }
}
