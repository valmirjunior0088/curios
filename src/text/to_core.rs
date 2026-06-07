mod context;
use context::*;

mod elaborate;
use elaborate::*;

mod interface;

use {
    super::*,
    crate::core,
    std::{
        collections::{BTreeMap, HashMap, HashSet},
        rc::Rc,
    },
};

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
            _ => {}
        }
    }

    Ok(info)
}

fn process_items(
    top_items: &[TopItem],
    context: &mut Context,
    flat_items: &mut Vec<FlatItem>,
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
                let elaborate = Elaborate::new(context);

                flat_items.push(FlatItem::Let(FlatLet {
                    name: context.prefixed(&let_item.label),
                    type_: elaborate.term(&let_item.signature.type_())?,
                    body: elaborate.term(&let_item.signature.body())?,
                }));
            }
            TopItem::Rec(ls) => {
                let items = ls
                    .iter()
                    .map(|let_item| {
                        let elaborate = Elaborate::new(context);

                        Ok(FlatLet {
                            name: context.prefixed(&let_item.label),
                            type_: elaborate.term(&let_item.signature.type_())?,
                            body: elaborate.term(&let_item.signature.body())?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(FlatItem::Rec(items));
            }
            TopItem::Union(unions) => {
                // Step 1: type bindings as one rec group. Each union desugars to a
                // tagged-tuple type, wrapped in a `Func` over its type parameters
                // when present — the capture binds the parameter names referenced
                // in the variant payload telescopes.
                let type_flat_items = unions
                    .iter()
                    .map(|u| {
                        let elaborate = Elaborate::new(context);

                        let variants = u
                            .cases
                            .iter()
                            .map(|c| {
                                let fields = c
                                    .payload_types
                                    .iter()
                                    .map(|t| Ok((String::new(), elaborate.term(t)?)))
                                    .collect::<Result<Vec<(String, core::Term)>, Error>>()?;
                                Ok((
                                    core::Atom::from(c.label.as_str()),
                                    core::Telescope::build(fields, ()),
                                ))
                            })
                            .collect::<Result<BTreeMap<core::Atom, core::Telescope<()>>, Error>>(
                            )?;

                        // Desugar the union type to a tagged-tuple type
                        // `{ tag : '[c_1, ...], match tag { 'c_i => (payload_i...) } }`.
                        // `tuple_type` captures the `tag` binder, shifting any escaping
                        // union-parameter indices in the payload telescopes by one.
                        let atom_type = core::Term::atom_type(variants.keys().cloned());
                        let tag_match: core::Term = core::Subterm::Match(core::Match {
                            head: core::Term::var(core::Var::free("tag")),
                            motive: core::Scope::constant(core::One, core::Term::type_()),
                            cases: variants
                                .into_iter()
                                .map(|(a, telescope)| {
                                    let payload =
                                        core::Subterm::TupleType(core::TupleType { telescope })
                                            .into();
                                    (a, payload)
                                })
                                .collect(),
                        })
                        .into();
                        let union: core::Term =
                            core::Term::tuple_type([("tag", atom_type), ("", tag_match)]);

                        let (type_, body) = if u.params.is_empty() {
                            (core::Term::type_(), union)
                        } else {
                            let param_tys = u
                                .params
                                .iter()
                                .map(|(n, t)| Ok((n.clone(), elaborate.term(t)?)))
                                .collect::<Result<Vec<_>, Error>>()?;
                            (
                                core::Term::func_type(param_tys.clone(), core::Term::type_()),
                                core::Term::func(param_tys, union),
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
                    // Output type term `T` or `T(A, ...)`, elaborated as a name ref.
                    let output_type: Term = if u.params.is_empty() {
                        Subterm::Name(Name::from(vec![u.label.clone()])).into()
                    } else {
                        Subterm::Apply(Apply {
                            head: Subterm::Name(Name::from(vec![u.label.clone()])).into(),
                            params: u
                                .params
                                .iter()
                                .map(|(n, _)| Subterm::Name(Name::from(vec![n.clone()])).into())
                                .collect(),
                        })
                        .into()
                    };

                    for c in &u.cases {
                        let k = c.payload_types.len();
                        let elaborate = Elaborate::new(context);

                        // Constructor type: (params..., _0 : T_0, ...) -> T
                        let param_tys = u
                            .params
                            .iter()
                            .map(|(n, t)| Ok((n.clone(), elaborate.term(t)?)))
                            .chain(
                                c.payload_types
                                    .iter()
                                    .enumerate()
                                    .map(|(i, t)| Ok((format!("_{i}"), elaborate.term(t)?))),
                            )
                            .collect::<Result<Vec<_>, Error>>()?;
                        let ctor_type =
                            core::Term::func_type(param_tys.clone(), elaborate.term(&output_type)?);

                        // Constructor body: (params..., _0, ...) => inject 'c (_0, ...)
                        let args: Vec<core::Term> = (0..k)
                            .map(|i| core::Term::var(core::Var::free(format!("_{i}"))))
                            .collect();
                        // Desugar the injection to a tagged tuple `('c, (args...))`.
                        let inject: core::Term = core::Term::tuple([
                            core::Term::atom(core::Atom::from(c.label.as_str())),
                            core::Term::tuple(args),
                        ]);
                        let ctor_body = core::Term::func(param_tys, inject);

                        flat_items.push(FlatItem::Let(FlatLet {
                            name: context.prefixed(&u.label).with(&c.label),
                            type_: ctor_type,
                            body: ctor_body,
                        }));
                    }
                }
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
) -> Vec<FlatItem> {
    let names = |item: &FlatItem| -> Vec<String> {
        match item {
            FlatItem::Let(let_) => vec![let_.name.join()],
            FlatItem::Rec(lets) => lets.iter().map(|let_| let_.name.join()).collect(),
        }
    };

    let free_vars = |item: &FlatItem| -> HashSet<String> {
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
    };

    // Index every declared qualifier to the node that owns it (a rec group owns
    // all its members).
    let owner: HashMap<String, usize> = items
        .iter()
        .enumerate()
        .flat_map(|(node, item)| names(item).into_iter().map(move |name| (name, node)))
        .collect();

    // A node depends on the nodes declaring its free vars; self-edges and free
    // vars naming no local declaration (primitives, externals) contribute none.
    let deps: Vec<HashSet<usize>> = items
        .iter()
        .enumerate()
        .map(|(node, item)| {
            free_vars(item)
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
    let prunable = |node: usize| -> bool {
        !library_roots.is_empty()
            && names(&items[node]).iter().all(|name| {
                name.split('/')
                    .next()
                    .is_some_and(|segment| library_roots.contains(segment))
            })
    };

    let mut keep = vec![false; count];
    let mut stack = Vec::new();

    for node in 0..count {
        if !prunable(node) {
            keep[node] = true;
            stack.push(node);
        }
    }

    for name in referenced {
        if let Some(&node) = owner.get(name) {
            if !keep[node] {
                keep[node] = true;
                stack.push(node);
            }
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

pub fn to_core(entrypoint: &Entrypoint, loader: &dyn Loader) -> Result<core::Module, Error> {
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
    let metavars = std::cell::Cell::new(0);
    let binders = std::cell::Cell::new(0);
    let mut context = Context::new(&table, &public, &metavars, &binders);
    let mut flat_items = Vec::new();

    process_items(
        &entrypoint.module.items,
        &mut context,
        &mut flat_items,
        &modules,
    )?;

    let elaborate = Elaborate::new(&context);
    let type_ = entrypoint
        .type_
        .as_ref()
        .map(|type_| elaborate.term(type_))
        .transpose()?;
    let tail = elaborate.term(&entrypoint.tail)?;

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

    let items = order_flat_items(flat_items, &referenced, &library_roots)
        .into_iter()
        .map(flat_item_to_core)
        .collect();

    Ok(core::Module {
        items,
        type_,
        body: tail,
    })
}

#[cfg(test)]
mod tests;
