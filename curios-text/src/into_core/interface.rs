use {
    super::{ModuleInfo, Scoped},
    crate::{Entrypoint, Error, GroupItem, Module, Name, TopItem, UseGroup},
    curios_base::{Mount, Qualifier},
    std::{
        collections::{HashMap, HashSet},
        rc::Rc,
    },
};

// The export view of a module: public names only, each pointing at the canonical declaration site. Built to a fixed point before any body is elaborated.
#[derive(Clone)]
#[curios_archive::archived]
pub(super) struct PublicInterface {
    #[cfg_attr(feature = "archive", rkyv(with = crate::OrderedMap))]
    pub children: HashMap<String, Entry>,
    #[cfg_attr(feature = "archive", rkyv(with = crate::OrderedMap))]
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
#[curios_archive::archived]
pub(super) struct Entry {
    pub target: Qualifier,
    /// The nominal declaration whose representation this export exposes. Kept distinct from `target` so re-exports cannot manufacture representation visibility and aliases can inherit it during the post-lowering audit.
    pub representation: Option<Qualifier>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Ns {
    Module,
    Binding,
}

// A `pub use` lifted out of the syntax tree, tagged with the module it lives in. Collected once; the fixed point reads only these plus the interface map.
struct PubUse {
    module: Qualifier,
    name: Name,
    group: UseGroup,
}

// === Visibility ==============================================================
//
// One rule governs both namespaces: a declaration written without `pub` in module `M` is visible exactly within `M`'s subtree, and a `pub` one is additionally visible wherever `M` itself is. Reachability is therefore the conjunction along a path, which the callers obtain by walking hop by hop.
//
// The public interface is consulted first, so a `pub` declaration and a re-export target resolve identically for every consumer. The subtree fallback covers only what the interface deliberately omits: the module's own non-`pub` declarations, visible to itself and its descendants.
//
// Globs are deliberately *not* relaxed — `resolvable` reads the public interface alone, so `use M/*` imports M's exported surface and `pub use M/*` can never widen a subtree-private item's audience. Reaching a non-`pub` declaration always requires naming it.

/// Who can see each declaration, as a set of subtree roots: a declaration is visible to consumer `C` when `C` lies within any of them.
///
/// The audience of a non-`pub` declaration in `M` is `M` itself. The audience of anything the interface graph exposes at `M` — a `pub` declaration or a re-export target — is `M`'s own audience, so `pub` inside a private module reaches exactly that module's audience and no further. Re-exports can expose one declaration at several unrelated points, which is why an audience is a set rather than a single qualifier.
///
/// Module audiences are a fixed point (a `pub use` chain may cycle, and cyclic module dependencies are supported); binding audiences read off the converged module map in one pass.
pub(super) struct Audiences {
    modules: HashMap<Qualifier, Vec<Qualifier>>,
    bindings: HashMap<Qualifier, Vec<Qualifier>>,
}

impl Audiences {
    pub(super) fn compute(
        public: &Scoped<'_, PublicInterface>,
        table: &Scoped<'_, ModuleInfo>,
    ) -> Self {
        let mut modules: HashMap<Qualifier, Vec<Qualifier>> = HashMap::new();
        // The compilation root is visible to the whole program.
        modules.insert(Qualifier::empty(), vec![Qualifier::empty()]);

        for (module, info) in table.iter() {
            for (label, vis_pub) in info.children() {
                if !vis_pub {
                    widen(&mut modules, module.with(label), module.clone());
                }
            }
        }

        loop {
            let mut changed = false;
            for (module, interface) in public.iter() {
                let exposure = modules.get(module).cloned().unwrap_or_default();
                for entry in interface.children.values() {
                    for root in &exposure {
                        changed |= widen(&mut modules, entry.target.clone(), root.clone());
                    }
                }
            }

            if !changed {
                break;
            }
        }

        let mut bindings: HashMap<Qualifier, Vec<Qualifier>> = HashMap::new();

        for (module, info) in table.iter() {
            for (label, vis_pub) in info.bindings() {
                if !vis_pub {
                    widen(&mut bindings, module.with(label), module.clone());
                }
            }
        }

        for (module, interface) in public.iter() {
            let exposure = modules.get(module).cloned().unwrap_or_default();
            for entry in interface.bindings.values() {
                for root in &exposure {
                    widen(&mut bindings, entry.target.clone(), root.clone());
                }
            }
        }

        Self { modules, bindings }
    }

    /// The audience of the module `qualifier` names. A namespace the interface never mentions (a synthetic one built during lowering) inherits its parent's audience.
    pub(super) fn module(&self, qualifier: &Qualifier) -> Vec<Qualifier> {
        match self.modules.get(qualifier) {
            Some(audience) => audience.clone(),
            None if qualifier.is_root() => vec![Qualifier::empty()],
            None => self.module(&qualifier.without_last()),
        }
    }

    /// The audience of the binding `qualifier` names, falling back to its namespace for compiler-built bindings the interface never registered.
    pub(super) fn binding(&self, qualifier: &Qualifier) -> Vec<Qualifier> {
        match self.bindings.get(qualifier) {
            Some(audience) => audience.clone(),
            None => self.module(&qualifier.without_last()),
        }
    }

    /// Whether everything that can see `audience` can also see `referent`: every root of the exposed audience must lie within some root of the referent's. An empty referent audience is nobody, so nothing covers it.
    pub(super) fn covers(audience: &[Qualifier], referent: &[Qualifier]) -> bool {
        audience
            .iter()
            .all(|root| referent.iter().any(|reach| root.is_within(reach)))
    }
}

// Add `root` to `key`'s audience unless an existing root already contains it, dropping any it subsumes so the set stays an antichain and the fixed point terminates. Returns whether the audience grew.
fn widen(
    audiences: &mut HashMap<Qualifier, Vec<Qualifier>>,
    key: Qualifier,
    root: Qualifier,
) -> bool {
    let audience = audiences.entry(key).or_default();

    if audience.iter().any(|existing| root.is_within(existing)) {
        return false;
    }

    audience.retain(|existing| !existing.is_within(&root));
    audience.push(root);
    true
}

/// The target of `parent`'s child module `label` as seen from `consumer`, or `None` when it is absent or out of view.
pub(super) fn visible_child(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    consumer: &Qualifier,
    parent: &Qualifier,
    label: &str,
) -> Option<Qualifier> {
    if let Some(entry) = public.get(parent).and_then(|i| i.children.get(label)) {
        return Some(entry.target.clone());
    }

    let within = consumer.is_within(parent);
    let declared = table
        .get(parent)
        .and_then(|info| info.get_child(label))
        .is_some();

    (within && declared).then(|| parent.with(label))
}

/// The target of `parent`'s binding `label` as seen from `consumer`, or `None` when it is absent or out of view.
pub(super) fn visible_binding(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    consumer: &Qualifier,
    parent: &Qualifier,
    label: &str,
) -> Option<Qualifier> {
    if let Some(entry) = public.get(parent).and_then(|i| i.bindings.get(label)) {
        return Some(entry.target.clone());
    }

    let within = consumer.is_within(parent);
    let declared = table
        .get(parent)
        .and_then(|info| info.get_binding(label))
        .is_some();

    (within && declared).then(|| parent.with(label))
}

// Phase 2 + 3 entry point: seed direct public interfaces (including inductive constructor modules), then resolve every `pub use` to a fixed point. Also adds constructor modules to `table` (the direct-interface view) so phase 4 can classify private-vs-missing accesses through them. `seed` is a third parallel tree-walk (mirroring `discover`/`process_items`), so it needs the identical explicit-per-root treatment: it reads `table`'s already-correct root-level children (from `Resolved::resolve`'s explicit registration) but its own recursion only ever follows literal `TopItem::Mod` occurrences in the items it's handed — sys/syn/std no longer appear there, so their own content must be seeded from an explicit call, or `public["sys"]` etc. would never exist at all (not even empty), breaking every absolute reference into them.
pub(super) fn resolve<'a>(
    entrypoint: &Entrypoint,
    modules: &HashMap<Qualifier, Rc<Module>>,
    table: &mut Scoped<'_, ModuleInfo>,
    mounts: &[Mount],
) -> Result<Scoped<'a, PublicInterface>, Error> {
    let mut public = Scoped::default();
    let mut pub_uses = Vec::new();

    seed(
        &entrypoint.module.items,
        &Qualifier::empty(),
        modules,
        table,
        &mut public,
        &mut pub_uses,
    )?;

    fixed_point(&mut public, table, mounts, &pub_uses)?;
    classify_dead(&public, table, mounts, &pub_uses)?;

    Ok(public)
}

pub(super) fn resolve_prelude<'a>(
    mounts: &[Mount],
    modules: &HashMap<Qualifier, Rc<Module>>,
    table: &mut Scoped<'_, ModuleInfo>,
) -> Result<Scoped<'a, PublicInterface>, Error> {
    let mut public = Scoped::default();
    let mut pub_uses = Vec::new();

    // Seed the synthetic compilation root as well: its public children are the explicitly mounted `/sys`, `/syn`, and `/std` roots. Absolute references resolve through this interface even though it has no source items.
    seed(
        &[],
        &Qualifier::empty(),
        modules,
        table,
        &mut public,
        &mut pub_uses,
    )?;

    for mount in mounts {
        let content = modules
            .get(&mount.prefix)
            .expect("prelude root loaded during discovery");
        seed(
            &content.items,
            &mount.prefix,
            modules,
            table,
            &mut public,
            &mut pub_uses,
        )?;
    }

    fixed_point(&mut public, table, mounts, &pub_uses)?;
    classify_dead(&public, table, mounts, &pub_uses)?;
    Ok(public)
}

pub(super) fn resolve_with_prelude<'a>(
    entrypoint: &Entrypoint,
    modules: &HashMap<Qualifier, Rc<Module>>,
    table: &mut Scoped<'_, ModuleInfo>,
    mounts: &[Mount],
    prepared: Scoped<'a, PublicInterface>,
) -> Result<Scoped<'a, PublicInterface>, Error> {
    let mut public = prepared;
    let mut pub_uses = Vec::new();

    seed(
        &entrypoint.module.items,
        &Qualifier::empty(),
        modules,
        table,
        &mut public,
        &mut pub_uses,
    )?;
    fixed_point(&mut public, table, mounts, &pub_uses)?;
    classify_dead(&public, table, mounts, &pub_uses)?;
    Ok(public)
}

// Phase 2. Walk the module tree (mirroring `discover`/`process_items`): for each module, seed its `PublicInterface` from the direct interface already in `table`; materialize each inductive's constructor module; and collect every `pub use`.
fn seed(
    items: &[TopItem],
    prefix: &Qualifier,
    modules: &HashMap<Qualifier, Rc<Module>>,
    table: &mut Scoped<'_, ModuleInfo>,
    public: &mut Scoped<'_, PublicInterface>,
    pub_uses: &mut Vec<PubUse>,
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
                representation: None,
            },
        );
    }

    for label in info.public_bindings() {
        let target = prefix.with(&label);
        interface.bindings.insert(
            label,
            Entry {
                target,
                representation: None,
            },
        );
    }

    public.insert(prefix.clone(), interface);

    // Attach declaration provenance to directly exposed nominal bindings. The fixed point copies this bit alongside the canonical target.
    if let Some(interface) = public.get_mut(prefix) {
        for item in items {
            match item {
                TopItem::Struct(item) if item.vis_pub && item.rep_pub => {
                    interface
                        .bindings
                        .get_mut(&item.label)
                        .expect("direct struct binding")
                        .representation = Some(prefix.with(&item.label));
                }
                TopItem::Induct(group) => {
                    for item in group.iter().filter(|item| item.vis_pub && item.rep_pub) {
                        interface
                            .bindings
                            .get_mut(&item.label)
                            .expect("direct inductive binding")
                            .representation = Some(prefix.with(&item.label));
                    }
                }
                TopItem::Concept(item) if item.vis_pub => {
                    interface
                        .bindings
                        .get_mut(&item.label)
                        .expect("direct concept binding")
                        .representation = Some(prefix.with(&item.label));
                }
                _ => {}
            }
        }
    }

    for item in items {
        match item {
            TopItem::Use(use_item) if use_item.vis_pub => {
                pub_uses.push(PubUse {
                    module: prefix.clone(),
                    name: use_item.name.clone(),
                    group: use_item.group.clone(),
                });
            }
            TopItem::Induct(group) => {
                for induct_decl in group {
                    let ctor = prefix.with(&induct_decl.label);

                    // Constructor bindings are public within their synthetic namespace. The parent's child bit, seeded separately as `vis_pub && rep_pub`, gates all external walks while the declaring module retains direct access.
                    let mut direct = ModuleInfo::new();
                    for case in &induct_decl.cases {
                        direct.insert_binding(case.label.clone(), true)?;
                    }
                    table.insert(ctor.clone(), direct);

                    let mut interface = PublicInterface::new();
                    for case in &induct_decl.cases {
                        let target = ctor.with(&case.label);
                        interface.bindings.insert(
                            case.label.clone(),
                            Entry {
                                target,
                                representation: None,
                            },
                        );
                    }
                    public.insert(ctor, interface);
                }
            }
            TopItem::Concept(concept) => {
                // A concept's method wrappers live in a nested namespace, exactly like an inductive's constructors: seed both the direct info and the public interface of that module unconditionally (the fields are always public within it), so `Show/show` resolves. The concept's own visibility gates the walk from outside via the parent's child-module flag.
                let namespace = prefix.with(&concept.label);

                let mut direct = ModuleInfo::new();
                // Superclass fields are anonymous — positional slots with no name to reach them by, and no wrapper (`into_core` filters them out of wrapper generation the same way). Registering their empty labels here is what made two superclasses collide as an empty-named duplicate declaration.
                for field in concept.fields.iter().filter(|field| !field.is_super) {
                    direct.insert_binding(field.label.clone(), true)?;
                }
                table.insert(namespace.clone(), direct);

                let mut interface = PublicInterface::new();
                for field in concept.fields.iter().filter(|field| !field.is_super) {
                    let target = namespace.with(&field.label);
                    interface.bindings.insert(
                        field.label.clone(),
                        Entry {
                            target,
                            representation: None,
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

                seed(child, &path, modules, table, public, pub_uses)?;
            }
            _ => {}
        }
    }

    Ok(())
}

// Phase 3. Repeatedly resolve every `pub use` against the current interface graph, inserting whatever is resolvable, until a full round adds nothing.
fn fixed_point(
    public: &mut Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    mounts: &[Mount],
    pub_uses: &[PubUse],
) -> Result<(), Error> {
    loop {
        let mut changed = false;

        for use_ in pub_uses {
            for (ns, label, target, representation) in resolvable(public, table, mounts, use_) {
                let entry = Entry {
                    target,
                    representation,
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

// The selectors of one `pub use` that resolve against the interfaces *as they currently stand*. Anything not yet present is skipped (deferred to a later round). Never errors — dead entries are classified after the fixed point.
fn resolvable(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    mounts: &[Mount],
    use_: &PubUse,
) -> Vec<(Ns, String, Qualifier, Option<Qualifier>)> {
    let Some(provider) = provider(public, table, mounts, &use_.module, &use_.name) else {
        return Vec::new();
    };

    let Some(interface) = public.get(&provider) else {
        return Vec::new();
    };

    let mut out = Vec::new();

    match &use_.group {
        UseGroup::Glob => {
            for (label, entry) in &interface.children {
                out.push((Ns::Module, label.clone(), entry.target.clone(), None));
            }
            for (label, entry) in &interface.bindings {
                out.push((
                    Ns::Binding,
                    label.clone(),
                    entry.target.clone(),
                    entry.representation.clone(),
                ));
            }
        }
        UseGroup::Named(items) => {
            for item in items {
                match item {
                    GroupItem::Mod(label) => {
                        if let Some(entry) = interface.children.get(label) {
                            out.push((Ns::Module, label.clone(), entry.target.clone(), None));
                        }
                    }
                    GroupItem::Let(label) => {
                        if let Some(entry) = interface.bindings.get(label) {
                            out.push((
                                Ns::Binding,
                                label.clone(),
                                entry.target.clone(),
                                entry.representation.clone(),
                            ));
                        }
                    }
                    GroupItem::Both(label) => {
                        if let Some(entry) = interface.children.get(label) {
                            out.push((Ns::Module, label.clone(), entry.target.clone(), None));
                        }
                        if let Some(entry) = interface.bindings.get(label) {
                            out.push((
                                Ns::Binding,
                                label.clone(),
                                entry.target.clone(),
                                entry.representation.clone(),
                            ));
                        }
                    }
                }
            }
        }
    }

    out
}

// The `Option` view of `resolve_provider`, for callers where non-resolution is benign: a selector that does not resolve *yet* during the fixed point, or a chain hop that simply does not exist. The terminal `classify_dead` pass calls `resolve_provider` directly to surface the precise error instead.
fn provider(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    mounts: &[Mount],
    module: &Qualifier,
    name: &Name,
) -> Option<Qualifier> {
    resolve_provider(public, table, mounts, module, name).ok()
}

// Insert one resolved entry into a slot. Returns whether the map changed.
fn insert(
    public: &mut Scoped<'_, PublicInterface>,
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

    // Conflict is about *what* a slot exports, not which `pub use` claimed it: two selectors (a glob and a named item, or two paths through different re-export chains) that land on the same declaration agree, so re-deriving one is idempotent. Only genuinely divergent targets are ambiguous. Keeping the first entry preserves its representation provenance, which is derived from the target and therefore identical across the agreeing paths.
    match slot.get(&label) {
        Some(existing) if existing.target == entry.target => Ok(false),
        Some(_) => Err(Error::ExportConflict { label }),
        None => {
            slot.insert(label, entry);
            Ok(true)
        }
    }
}

// Phase 3 post-pass. After convergence any selector still resolving to nothing is an error, classified by following its re-export chain: a chain that returns to a slot already seen is a cyclic re-export, otherwise the target is missing.
fn classify_dead(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    mounts: &[Mount],
    pub_uses: &[PubUse],
) -> Result<(), Error> {
    for use_ in pub_uses {
        let provider = resolve_provider(public, table, mounts, &use_.module, &use_.name)?;

        let interface = public.get(&provider).expect("seeded module");

        match &use_.group {
            // A glob with a reachable provider always resolves (possibly to no labels); only an unreachable source is an error, handled above.
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

                    // `{x}` resolves if either namespace filled; `{mod x}` / `{let x}` require their own namespace.
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
                            public, table, mounts, pub_uses, &provider, ns, label,
                        ));
                    }
                }
            }
        }
    }

    Ok(())
}

// Walk the re-export chain for an unresolved `(module, ns, label)` to decide whether it is a cycle or a genuine miss.
fn classify_label(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    mounts: &[Mount],
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

        match producer(public, table, mounts, pub_uses, &current, ns, label) {
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

// The provider module of a `pub use` in `module` that would supply `label` in namespace `ns`, if any (named selector match, or a glob whose source is reachable). Used only by chain classification.
fn producer(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    mounts: &[Mount],
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

        if names && let Some(provider) = provider(public, table, mounts, module, &use_.name) {
            return Some(provider);
        }
    }

    None
}

// Walk a `use` source path to its provider module, following re-export targets. A relative path's first segment may be the current module's own child of any visibility (you are inside it, so its privacy does not apply to itself); every later segment, and every segment of an absolute path, must be a public child. Each resolved hop is guarded so a non-privileged consumer cannot follow a re-export into an internal root (`sys`) by any spelling. On failure, returns the precise error at the offending segment, using the direct-interface table to tell private from absent; `provider` is the `Option` view for callers where that is benign.
fn resolve_provider(
    public: &Scoped<'_, PublicInterface>,
    table: &Scoped<'_, ModuleInfo>,
    mounts: &[Mount],
    module: &Qualifier,
    name: &Name,
) -> Result<Qualifier, Error> {
    let segments = name.qualifier().segments();

    let (mut current, walk) = if name.is_abs() {
        (Qualifier::empty(), segments)
    } else {
        let first = &segments[0];

        // An opaque constructor namespace is never in the public interface, and re-exporting one would widen its audience past the subtree that owns the representation — so it is refused before the subtree fallback can offer it.
        if public
            .get(module)
            .and_then(|i| i.children.get(first))
            .is_none()
            && table
                .get(module)
                .is_some_and(|info| info.is_opaque_constructor_child(first))
        {
            return Err(Error::OpaqueConstructorsCannotBeReExported {
                induct_decl: module.with(first).join(),
            });
        }

        let start = visible_child(public, table, module, module, first).ok_or_else(|| {
            Error::ChildModuleNotFound {
                segment: first.clone(),
            }
        })?;

        (start, &segments[1..])
    };

    super::guard_internal_root(mounts, module, current.segments())?;
    for segment in walk {
        match visible_child(public, table, module, &current, segment) {
            Some(target) => current = target,
            None => return Err(segment_error(table, &current, segment)),
        }
        super::guard_internal_root(mounts, module, current.segments())?;
    }

    Ok(current)
}

fn segment_error(table: &Scoped<'_, ModuleInfo>, module: &Qualifier, segment: &str) -> Error {
    match table.get(module).and_then(|info| info.get_child(segment)) {
        Some(false) => Error::PrivateChildModule {
            segment: segment.to_string(),
        },
        _ => Error::ChildModuleNotFound {
            segment: segment.to_string(),
        },
    }
}
