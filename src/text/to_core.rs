mod context;
use context::*;

mod lower;
use lower::*;

mod interface;

use {
    super::*,
    crate::{Entropy, core, core::Bound},
    std::{
        collections::{BTreeMap, HashMap, HashSet},
        rc::Rc,
    },
};

// Root modules reachable only from the standard library — the trusted primitive
// substrate (`sys`). User code reaches them through their `/std` wrappers; any
// reference that resolves into one from outside is rejected during resolution.
const INTERNAL_ROOTS: &[&str] = &["sys"];
// Consuming roots permitted to reference an internal root: the standard library,
// and the internal roots themselves (so they may reference one another).
const PRIVILEGED_ROOTS: &[&str] = &["std", "sys"];

// Reject a reference that *resolves into* an internal root (`sys`) when the
// consuming module lies outside the privileged roots. `resolved` is the segments
// of the qualifier the reference resolved to — not the raw spelled path — so
// absolute and relative spellings are guarded identically. A non-internal target
// or a privileged consumer passes through.
fn guard_internal_root(consumer: &Qualifier, resolved: &[String]) -> Result<(), Error> {
    let Some(root) = resolved.first() else {
        return Ok(());
    };

    if !is_internal_root(root) {
        return Ok(());
    }

    if privileged(consumer) {
        Ok(())
    } else {
        Err(Error::InternalRootModule {
            segment: root.clone(),
        })
    }
}

// Whether `label` names an internal root: discoverable so the standard library
// can resolve it by absolute path, but unreachable from user code.
fn is_internal_root(label: &str) -> bool {
    INTERNAL_ROOTS.contains(&label)
}

// Whether `consumer` is rooted in a privileged root (the standard library or an
// internal root itself), and so may reference internal roots.
fn privileged(consumer: &Qualifier) -> bool {
    consumer
        .segments()
        .first()
        .is_some_and(|r| PRIVILEGED_ROOTS.contains(&r.as_str()))
}

struct Resolved {
    modules: HashMap<Qualifier, Rc<Module>>,
    table: HashMap<Qualifier, ModuleInfo>,
}

impl Resolved {
    fn new() -> Self {
        Self {
            modules: HashMap::new(),
            table: HashMap::new(),
        }
    }

    fn for_entrypoint(entrypoint: &Entrypoint, loader: &dyn Loader) -> Result<Self, Error> {
        let mut resolved = Self::new();
        resolved.resolve(entrypoint, loader)?;

        Ok(resolved)
    }

    fn resolve(&mut self, entrypoint: &Entrypoint, loader: &dyn Loader) -> Result<(), Error> {
        self.discover(&entrypoint.module.items, &Qualifier::empty(), loader)
    }

    // `mod` declarations only name children, so the module graph is a tree: every
    // qualifier is reached exactly once and no cycles are possible. Hence the walk
    // needs neither a visited-set nor a cache hit-check — just load each file
    // module once and recurse.
    fn discover(
        &mut self,
        items: &[TopItem],
        prefix: &Qualifier,
        loader: &dyn Loader,
    ) -> Result<(), Error> {
        self.table.insert(prefix.clone(), scan_module_info(items)?);

        for item in items {
            if let TopItem::Mod(module_item) = item {
                let path = prefix.with(&module_item.label);

                match &module_item.module {
                    Some(module) => self.discover(&module.items, &path, loader)?,
                    None => {
                        let module =
                            Rc::new(loader.load(&path).map_err(
                                |error| match &module_item.span {
                                    Some(span) => error.at(span.clone()),
                                    None => error,
                                },
                            )?);

                        self.modules.insert(path.clone(), Rc::clone(&module));
                        self.discover(&module.items, &path, loader)?;
                    }
                }
            }
        }

        Ok(())
    }
}

fn scan_module_info(items: &[TopItem]) -> Result<ModuleInfo, Error> {
    let mut info = ModuleInfo::new();

    for item in items {
        match item {
            TopItem::Mod(m) => info.insert_child(m.label.clone(), m.is_pub)?,
            TopItem::Let(l) => info.insert_binding(l.label.clone(), l.is_pub)?,
            TopItem::Rec(ls) => {
                for l in ls {
                    info.insert_binding(l.label.clone(), l.is_pub)?;
                }
            }
            TopItem::Union(unions) => {
                for u in unions {
                    info.insert_child(u.label.clone(), u.is_pub)?;
                    info.insert_binding(u.label.clone(), u.is_pub)?;
                }
            }
            // A struct declares one binding (the type-former), like a `let` —
            // there are no value constructors and no nested namespace, so no
            // child module.
            TopItem::Struct(s) => info.insert_binding(s.label.clone(), s.is_pub)?,
            _ => {}
        }
    }

    Ok(info)
}

// Walk the resolved module tree (mirroring `Resolved::discover`) collecting
// every union's constructor roster. Each constructor's canonical name
// `<module>/<union>/<ctor>` maps to the union's full `(tag, payload arity)` list
// — the arity is just the declared payload count, so no core lowering is needed.
fn scan_union_ctors(
    items: &[TopItem],
    prefix: &Qualifier,
    modules: &HashMap<Qualifier, Rc<Module>>,
    out: &mut HashMap<String, Vec<(String, usize)>>,
) {
    for item in items {
        match item {
            TopItem::Union(unions) => {
                for u in unions {
                    let roster: Vec<(String, usize)> = u
                        .cases
                        .iter()
                        .map(|c| (c.label.clone(), c.payload.len()))
                        .collect();
                    let union_name = prefix.with(&u.label);

                    for c in &u.cases {
                        out.insert(union_name.with(&c.label).join(), roster.clone());
                    }
                }
            }
            TopItem::Mod(m) => {
                let path = prefix.with(&m.label);

                match &m.module {
                    Some(module) => scan_union_ctors(&module.items, &path, modules, out),
                    None => {
                        if let Some(module) = modules.get(&path) {
                            scan_union_ctors(&module.items, &path, modules, out);
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

fn process_items(
    top_items: &[TopItem],
    context: &mut Context,
    flat_items: &mut Vec<FlatItem>,
    inductives: &mut BTreeMap<String, core::Inductive>,
    structures: &mut BTreeMap<String, core::Structure>,
    modules: &HashMap<Qualifier, Rc<Module>>,
) -> Result<(), Error> {
    for top_item in top_items {
        match top_item {
            TopItem::Mod(m) => context.insert_scope(m.label.clone(), context.prefixed(&m.label))?,
            TopItem::Let(l) => {
                context.insert_binding(l.label.clone(), context.prefixed(&l.label))?
            }
            TopItem::Rec(labels) => {
                for l in labels {
                    context.insert_binding(l.label.clone(), context.prefixed(&l.label))?;
                }
            }
            TopItem::Union(unions) => {
                for u in unions {
                    context.insert_scope(u.label.clone(), context.prefixed(&u.label))?;
                    context.insert_binding(u.label.clone(), context.prefixed(&u.label))?;
                }
            }
            // The type-former binding only — like a `let` (no constructor
            // namespace).
            TopItem::Struct(s) => {
                context.insert_binding(s.label.clone(), context.prefixed(&s.label))?
            }
            _ => {}
        }
    }

    for top_item in top_items {
        match top_item {
            TopItem::Mod(mod_item) => match &mod_item.module {
                Some(module) => {
                    process_items(
                        &module.items,
                        &mut context.nested(&mod_item.label),
                        flat_items,
                        inductives,
                        structures,
                        modules,
                    )?;
                }
                None => {
                    let path = context.prefixed(&mod_item.label);
                    // Discovery is exhaustive over this same tree, so every
                    // file-backed module is already cached under this qualifier.
                    let module = modules.get(&path).expect("module loaded during discovery");

                    process_items(
                        &module.items,
                        &mut context.nested(&mod_item.label),
                        flat_items,
                        inductives,
                        structures,
                        modules,
                    )?;
                }
            },
            TopItem::Use(use_item) => {
                // The lexical import effect of `use`/`pub use`: source-ordered,
                // point-of-use scoping. The interface (export) effect of `pub use`
                // is precomputed in the phase-3 fixed point, not here.
                match &use_item.group {
                    UseGroup::Named(items) => {
                        for item in items {
                            let full = use_item.name.with(item.label());

                            match item {
                                GroupItem::Mod(_) => {
                                    context.resolve_module_use(&full)?;
                                }
                                GroupItem::Let(_) => {
                                    context.resolve_binding_use(&full)?;
                                }
                                GroupItem::Both(_) => {
                                    context.resolve_both_use(&full)?;
                                }
                            }
                        }
                    }
                    UseGroup::Glob => {
                        context.resolve_glob(&use_item.name)?;
                    }
                }
            }
            TopItem::Let(let_item) => {
                let lower = Lower::new(context);

                flat_items.push(FlatItem::Let(FlatLet {
                    name: context.prefixed(&let_item.label),
                    type_: lower.term(&let_item.signature.type_())?,
                    body: lower.term(&let_item.signature.body())?,
                }));
            }
            TopItem::Rec(ls) => {
                let items = ls
                    .iter()
                    .map(|let_item| {
                        let lower = Lower::new(context);

                        Ok(FlatLet {
                            name: context.prefixed(&let_item.label),
                            type_: lower.term(&let_item.signature.type_())?,
                            body: lower.term(&let_item.signature.body())?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(FlatItem::Rec(items));
            }
            TopItem::Union(unions) => {
                // Step 1: type bindings as one rec group. A union's type
                // binding wraps a primitive `UnionType` normal form in a
                // `Func` over its type parameters and indices (so
                // `Result(Nat, Bin)` beta-reduces to `UnionType { Result,
                // [Nat, Bin] }` and `Vec(Bin, 3)` to `UnionType { Vec, [Bin],
                // [3] }`), and its shape is recorded in the inductive
                // registry.
                let type_flat_items = unions
                    .iter()
                    .map(|u| {
                        let lower = Lower::new(context);
                        let name = context.prefixed(&u.label).join();

                        let param_tys = u
                            .params
                            .iter()
                            .map(|(p, n, t)| Ok((*p, n.clone(), lower.term(t)?)))
                            .collect::<Result<Vec<_>, Error>>()?;
                        // The registry and the `UnionType` normal form are
                        // positional; plicity matters only on the generated
                        // type-constructor function.
                        let param_tys_unmarked = param_tys
                            .iter()
                            .map(|(_, n, t)| (n.clone(), t.clone()))
                            .collect::<Vec<_>>();

                        let param_vars = u
                            .params
                            .iter()
                            .map(|(_, n, _)| core::Term::var(core::Var::free(n)))
                            .collect::<Vec<_>>();

                        // The head's index telescope. Unnamed entries get a
                        // positional placeholder — the name only matters for
                        // dependency capture among the index types.
                        let index_tys = u
                            .indices
                            .iter()
                            .enumerate()
                            .map(|(i, (n, t))| {
                                let n = n.clone().unwrap_or_else(|| format!("_{i}"));
                                Ok((n, lower.term(t)?))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;

                        let index_vars = index_tys
                            .iter()
                            .map(|(n, _)| core::Term::var(core::Var::free(n)))
                            .collect::<Vec<_>>();

                        // Registry entry: the parameter telescope plus each
                        // constructor's full signature `(params..., payload...)
                        // -> UnionType { name, params, indices }`, where the
                        // terminal's indices are that *case's* target
                        // expressions over its payload binders.
                        // `Telescope::build` captures the parameter and
                        // payload labels in the payload types and the
                        // terminal, mirroring `func_type`.
                        let constructors = u
                            .cases
                            .iter()
                            .map(|c| {
                                let fields = c
                                    .payload
                                    .iter()
                                    .enumerate()
                                    .map(|(i, (_, n, t))| {
                                        let n = n.clone().unwrap_or_else(|| format!("_{i}"));
                                        Ok((n, lower.term(t)?))
                                    })
                                    .collect::<Result<Vec<_>, Error>>()?;

                                let target = c
                                    .target
                                    .iter()
                                    .flatten()
                                    .map(|t| lower.term(t))
                                    .collect::<Result<Vec<_>, Error>>()?;

                                let signature = core::Telescope::build(
                                    param_tys_unmarked.iter().cloned().chain(fields),
                                    core::Term::union_type(&name, param_vars.clone(), target),
                                );

                                Ok((core::Atom::from(c.label.as_str()), signature))
                            })
                            .collect::<Result<BTreeMap<_, _>, Error>>()?;

                        inductives.insert(
                            name.clone(),
                            core::Inductive {
                                params: core::Telescope::build(param_tys_unmarked.clone(), ()),
                                indices: core::Telescope::build(
                                    param_tys_unmarked
                                        .iter()
                                        .cloned()
                                        .chain(index_tys.iter().cloned()),
                                    (),
                                ),
                                constructors,
                            },
                        );

                        let union = core::Term::union_type(&name, param_vars, index_vars);

                        // The type constructor is flat over params then
                        // indices: `Vec : (T : Type, n : Nat) -> Type`. Use
                        // sites never distinguish the two. Parameters keep
                        // their declared marks (`@` makes one implicit at use
                        // sites); indices are always explicit.
                        let binder_tys: Vec<_> = param_tys
                            .iter()
                            .cloned()
                            .chain(
                                index_tys
                                    .iter()
                                    .cloned()
                                    .map(|(n, t)| (core::Plicity::Explicit, n, t)),
                            )
                            .collect();
                        let (type_, body) = if binder_tys.is_empty() {
                            (core::Term::type_(), union)
                        } else {
                            (
                                core::Term::func_type_marked(
                                    binder_tys.clone(),
                                    core::Term::type_(),
                                ),
                                core::Term::func(
                                    binder_tys.into_iter().map(|(_, n, t)| (n, t)),
                                    union,
                                ),
                            )
                        };

                        Ok(FlatLet {
                            name: context.prefixed(&u.label),
                            type_,
                            body,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(FlatItem::Rec(type_flat_items));

                // Step 2: constructor bindings. Each is a function whose body
                // injects the variant as a tagged tuple.
                for u in unions {
                    for c in &u.cases {
                        let lower = Lower::new(context);

                        // Per-case payload binder names: the declared name, or
                        // a positional placeholder.
                        let payload_name = |i: usize, n: &Option<String>| {
                            n.clone().unwrap_or_else(|| format!("_{i}"))
                        };

                        // Output type term `T`, `T(A, ...)`, or — indexed —
                        // the case's full terminal `T(A, ..., target...)`,
                        // elaborated as a name ref applied to the parameters
                        // and the target's index expressions.
                        let output_args: Vec<(core::Plicity, Term)> = u
                            .params
                            .iter()
                            .map(|(p, n, _)| {
                                // Each argument's mark must match its binder
                                // on the type constructor (the two-queue
                                // rule): an `@`-marked parameter is filled
                                // from the implicit queue.
                                (*p, Subterm::Name(Name::from(vec![n.clone()])).into())
                            })
                            .chain(
                                c.target
                                    .iter()
                                    .flatten()
                                    .map(|t| (core::Plicity::Explicit, t.clone())),
                            )
                            .collect();
                        let output_type: Term = if output_args.is_empty() {
                            Subterm::Name(Name::from(vec![u.label.clone()])).into()
                        } else {
                            Subterm::Apply(Apply {
                                head: Subterm::Name(Name::from(vec![u.label.clone()])).into(),
                                params: output_args,
                            })
                            .into()
                        };

                        // Constructor type: (params..., _0 : T_0, ...) -> T.
                        // Every union parameter is implicit at the value
                        // constructor — `Result/success(42)` infers them, the
                        // call-site `@` supplies one positionally — while the
                        // payload binders keep their declared marks (`@m`
                        // makes one implicit; the default is explicit).
                        let param_tys =
                            u.params
                                .iter()
                                .map(|(_, n, t)| {
                                    Ok((core::Plicity::Implicit, n.clone(), lower.term(t)?))
                                })
                                .chain(c.payload.iter().enumerate().map(|(i, (p, n, t))| {
                                    Ok((*p, payload_name(i, n), lower.term(t)?))
                                }))
                                .collect::<Result<Vec<_>, Error>>()?;
                        let ctor_type = core::Term::func_type_marked(
                            param_tys.clone(),
                            lower.term(&output_type)?,
                        );

                        // Constructor body: (params..., _0, ...) => the variant's
                        // injection, a primitive `Variant` normal form.
                        let args: Vec<core::Term> = c
                            .payload
                            .iter()
                            .enumerate()
                            .map(|(i, (_, n, _))| {
                                core::Term::var(core::Var::free(payload_name(i, n)))
                            })
                            .collect();
                        let inject = core::Term::variant(
                            context.prefixed(&u.label).join(),
                            u.params
                                .iter()
                                .map(|(_, n, _)| core::Term::var(core::Var::free(n))),
                            core::Atom::from(c.label.as_str()),
                            args,
                        );
                        // The lambda binds every parameter regardless of mark.
                        let ctor_body =
                            core::Term::func(param_tys.into_iter().map(|(_, n, t)| (n, t)), inject);

                        flat_items.push(FlatItem::Let(FlatLet {
                            name: context.prefixed(&u.label).with(&c.label),
                            type_: ctor_type,
                            body: ctor_body,
                        }));
                    }
                }
            }
            // A struct lowers to a single type-former `let` plus a registry
            // entry — no value-constructor binding (the literal elaborates
            // directly) and no indices.
            TopItem::Struct(s) => {
                let lower = Lower::new(context);

                let name = context.prefixed(&s.label).join();
                // Declaring module: the type-former's qualifier prefix —
                // identical to core's per-item `island` — for the
                // representation-privacy checks.
                let module = match name.rfind('/') {
                    Some(slash) => name[..slash].to_string(),
                    None => String::new(),
                };

                let param_tys = s
                    .params
                    .iter()
                    .map(|(p, n, t)| Ok((*p, n.clone(), lower.term(t)?)))
                    .collect::<Result<Vec<_>, Error>>()?;
                let param_tys_unmarked = param_tys
                    .iter()
                    .map(|(_, n, t)| (n.clone(), t.clone()))
                    .collect::<Vec<_>>();
                let param_vars = s
                    .params
                    .iter()
                    .map(|(_, n, _)| core::Term::var(core::Var::free(n)))
                    .collect::<Vec<_>>();

                // Field types, with declared or positional (`_i`) names so a
                // later field type can depend on an earlier field.
                let field_tys = s
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, (n, t))| {
                        let n = n.clone().unwrap_or_else(|| format!("_{i}"));
                        Ok((n, lower.term(t)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                // Registry entry: the parameter telescope, and the full field
                // telescope (parameter binders first — field types may mention
                // them — then field binders), as in `Inductive::indices`.
                structures.insert(
                    name.clone(),
                    core::Structure {
                        params: core::Telescope::build(param_tys_unmarked.clone(), ()),
                        fields: core::Telescope::build(
                            param_tys_unmarked.iter().cloned().chain(field_tys),
                            (),
                        ),
                        module,
                        rep_public: s.rep_pub,
                    },
                );

                // The type-former: `Pair : (A : Type, B : Type) -> Type` whose
                // body is the `StructType` normal form (the bare node when
                // parameterless), so `Pair(Nat, Bin)` reduces to
                // `StructType { Pair, [Nat, Bin] }`. No value constructor.
                let struct_type = core::Term::struct_type(&name, param_vars);
                let (type_, body) = if param_tys.is_empty() {
                    (core::Term::type_(), struct_type)
                } else {
                    (
                        core::Term::func_type_marked(param_tys.clone(), core::Term::type_()),
                        core::Term::func(
                            param_tys.into_iter().map(|(_, n, t)| (n, t)),
                            struct_type,
                        ),
                    )
                };

                flat_items.push(FlatItem::Let(FlatLet {
                    name: context.prefixed(&s.label),
                    type_,
                    body,
                }));
            }
        }
    }

    Ok(())
}

// Phase 5: reorder declarations so each one's value dependencies come before it
// (outer in the fold), since a cyclic name graph means source order is no longer
// a valid binding order. A stable Kahn pass keeps independent declarations in
// source order; a genuine value cycle leaves nodes unorderable, which are emitted
// in source order and rejected downstream as unbound names — there is nothing to
// repair, as cross-declaration value recursion is unexpressible by construction.
fn order_flat_items(
    items: Vec<FlatItem>,
    referenced: &HashSet<String>,
    library_roots: &HashSet<String>,
    inductives: &BTreeMap<String, core::Inductive>,
    structures: &BTreeMap<String, core::Structure>,
) -> Vec<FlatItem> {
    // Index every declared qualifier to the node that owns it (a rec group owns
    // all its members).
    let owner: HashMap<String, usize> = items
        .iter()
        .enumerate()
        .flat_map(|(node, item)| {
            flat_item_names(item)
                .into_iter()
                .map(move |name| (name, node))
        })
        .collect();

    // A node depends on the nodes declaring its free vars. A union's
    // declaration is wider than its items: the registry entry's constructor
    // payload and target types are elaborated alongside the type-binding
    // group (`core::elaborate_module_rec` rebuilds the registry telescopes
    // there), so a node declaring a registered name references everything its
    // registry entry does, too — those names live nowhere in the type
    // binding's own `type_`/`body`. Self-edges and free vars naming no local
    // declaration (primitives, externals) contribute none.
    let deps: Vec<HashSet<usize>> = items
        .iter()
        .enumerate()
        .map(|(node, item)| {
            let mut names = flat_item_free_vars(item);
            for declared in flat_item_names(item) {
                if let Some(inductive) = inductives.get(&declared) {
                    names.extend(inductive_free_vars(inductive));
                }
                // A struct's field types live in the registry, not the
                // type-former's body, so seed them here too (see
                // `structure_free_vars`) — otherwise the former could be
                // ordered before a type its fields mention is defined.
                if let Some(structure) = structures.get(&declared) {
                    names.extend(structure_free_vars(structure));
                }
            }

            names
                .iter()
                .filter_map(|name| owner.get(name).copied())
                .filter(|&dep| dep != node)
                .collect()
        })
        .collect();

    let count = items.len();

    // Reachability prune: drop library (`sys`/`std`) definitions the program can't
    // reach, so they are never type-checked or lowered (the wasm DCE already removed
    // them downstream — this just stops the wasted work earlier). A node is a
    // prunable library item only if *every* name it declares lives under a loader
    // root; user-authored items are never pruned, so a dead user definition is still
    // type-checked. Seeds: every non-prunable node (kept unconditionally) plus every
    // node owning a name the program references directly; BFS over `deps` keeps the
    // transitive closure.
    let mut keep = vec![false; count];
    let mut stack = Vec::new();

    for (node, slot) in keep.iter_mut().enumerate() {
        if !flat_item_prunable(&items[node], library_roots) {
            *slot = true;
            stack.push(node);
        }
    }

    for name in referenced {
        if let Some(&node) = owner.get(name)
            && !keep[node]
        {
            keep[node] = true;
            stack.push(node);
        }
    }

    while let Some(node) = stack.pop() {
        for &dep in &deps[node] {
            if !keep[dep] {
                keep[dep] = true;
                stack.push(dep);
            }
        }
    }

    let mut emitted = vec![false; count];
    let mut order = Vec::with_capacity(count);

    while order.len() < count {
        // Lowest-index node whose dependencies are all emitted; on a cycle no
        // such node exists, so break the deadlock with the lowest remaining one.
        let ready = (0..count)
            .find(|&node| !emitted[node] && deps[node].iter().all(|dep| emitted[*dep]))
            .or_else(|| (0..count).find(|&node| !emitted[node]))
            .expect("a node remains while order is incomplete");

        emitted[ready] = true;
        order.push(ready);
    }

    let mut slots: Vec<Option<FlatItem>> = items.into_iter().map(Some).collect();
    order
        .into_iter()
        .filter(|&node| keep[node])
        .map(|node| slots[node].take().unwrap())
        .collect()
}

fn flat_item_names(item: &FlatItem) -> Vec<String> {
    match item {
        FlatItem::Let(let_) => vec![let_.name.join()],
        FlatItem::Rec(lets) => lets.iter().map(|let_| let_.name.join()).collect(),
    }
}

fn flat_item_free_vars(item: &FlatItem) -> HashSet<String> {
    let lets = match item {
        FlatItem::Let(let_) => std::slice::from_ref(let_),
        FlatItem::Rec(lets) => lets.as_slice(),
    };

    lets.iter()
        .flat_map(|let_| {
            let_.type_
                .free_vars()
                .into_iter()
                .chain(let_.body.free_vars())
        })
        .collect()
}

/// The external references of an inductive registry entry: every free var of
/// its telescopes. Binder names (parameters, payload binders) are captured by
/// `Telescope::build` and never appear here; the index types' references also
/// live in the type binding's own signature, but are included for robustness.
fn inductive_free_vars(inductive: &core::Inductive) -> HashSet<String> {
    inductive
        .params
        .free_vars()
        .into_iter()
        .chain(inductive.indices.free_vars())
        .chain(
            inductive
                .constructors
                .values()
                .flat_map(|signature| signature.free_vars()),
        )
        .collect()
}

/// The external references of a struct registry entry: every free var of its
/// parameter and field telescopes. Like `inductive_free_vars`, this is what
/// makes a struct's type-former node depend on the (e.g. primitive) types its
/// fields mention — they live nowhere in the type-former's own body, which is
/// just the `StructType` normal form.
fn structure_free_vars(structure: &core::Structure) -> HashSet<String> {
    structure
        .params
        .free_vars()
        .into_iter()
        .chain(structure.fields.free_vars())
        .collect()
}

fn flat_item_prunable(item: &FlatItem, library_roots: &HashSet<String>) -> bool {
    !library_roots.is_empty()
        && flat_item_names(item).iter().all(|name| {
            name.split('/')
                .next()
                .is_some_and(|segment| library_roots.contains(segment))
        })
}

fn flat_let_to_core(let_: FlatLet) -> core::Definition {
    core::Definition {
        name: let_.name.join(),
        type_: let_.type_,
        body: let_.body,
    }
}

fn flat_item_to_core(item: FlatItem) -> core::Item {
    match item {
        FlatItem::Let(let_) => core::Item::Let(flat_let_to_core(let_)),
        FlatItem::Rec(items) => core::Item::Rec(items.into_iter().map(flat_let_to_core).collect()),
    }
}

// A bodyless `pub mod <label>;` declaration. We synthesize one per `Loader::roots`
// entry and prepend it to the entrypoint, so a loader's root modules (`sys`, `std`)
// are discovered, interfaced, and resolvable exactly as if the entrypoint declared
// them — without every entrypoint having to.
fn declaration(label: &str) -> TopItem {
    TopItem::Mod(TopMod {
        span: None,
        is_pub: true,
        label: label.to_string(),
        module: None,
    })
}

/// Lower an [`Entrypoint`] to a [`core::Module`]. Also returns how many
/// metavariable ids were minted for the module's holes: the floor
/// `elaborate_module` needs so the ids it mints for implicit-argument
/// insertion never collide with these.
pub fn to_core(
    entrypoint: &Entrypoint,
    loader: &dyn Loader,
) -> Result<(core::Module, usize), Error> {
    let roots = loader.roots();

    let entrypoint = &Entrypoint {
        module: Module {
            items: roots
                .iter()
                .map(|label| declaration(label))
                .chain(entrypoint.module.items.iter().cloned())
                .collect(),
        },
        type_: entrypoint.type_.clone(),
        tail: entrypoint.tail.clone(),
    };

    let Resolved { mut table, modules } = Resolved::for_entrypoint(entrypoint, loader)?;
    let public = interface::resolve(entrypoint, &modules, &mut table)?;
    let metavars = Entropy::<usize>::new();
    let binders = Entropy::<usize>::new();

    // Precompute every union's constructor roster program-wide (before any body
    // is lowered, so forward references resolve too), keyed by each constructor's
    // canonical name. The pattern-matrix compiler reads it to expand a union-column
    // `_` into the unlisted constructors.
    let mut union_ctors = HashMap::new();
    scan_union_ctors(
        &entrypoint.module.items,
        &Qualifier::empty(),
        &modules,
        &mut union_ctors,
    );

    let mut context = Context::new(&table, &public, &metavars, &binders, &union_ctors);
    let mut flat_items = Vec::new();
    let mut inductives = BTreeMap::new();
    let mut structures = BTreeMap::new();

    process_items(
        &entrypoint.module.items,
        &mut context,
        &mut flat_items,
        &mut inductives,
        &mut structures,
        &modules,
    )?;

    let lower = Lower::new(&context);
    let type_ = entrypoint
        .type_
        .as_ref()
        .map(|type_| lower.term(type_))
        .transpose()?;
    let tail = lower.term(&entrypoint.tail)?;

    // Emit the program as a flat list of named top-level definitions rather than
    // folding it into one N-deep nested `let`/`rec` term (BUG.md). Cross-references
    // (and the references in the entrypoint `body` and its `type_` annotation) stay
    // free `Var`s keyed by the definition's joined name; the core passes `define`
    // each one into the `Context`, so both the body and its annotation reduce
    // through those definitions and agree — no shared binder scope required.
    // The program references these top-level names directly (the entrypoint body and
    // its type annotation); they seed the reachability prune in `order_flat_items`.
    let referenced: HashSet<String> = tail
        .free_vars()
        .into_iter()
        .chain(type_.iter().flat_map(|type_| type_.free_vars()))
        .collect();
    let library_roots: HashSet<String> = roots.iter().cloned().collect();

    let items = order_flat_items(
        flat_items,
        &referenced,
        &library_roots,
        &inductives,
        &structures,
    )
    .into_iter()
    .map(flat_item_to_core)
    .collect();

    Ok((
        core::Module {
            items,
            inductives,
            structures,
            type_,
            body: tail,
        },
        metavars.count(),
    ))
}

#[cfg(test)]
mod tests;
