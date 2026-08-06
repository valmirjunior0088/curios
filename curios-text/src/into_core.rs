mod context;
use context::*;
use curios_elab::TermBuilders;

mod lowerer;
use lowerer::*;

mod match_compile;
use match_compile::*;

mod interface;
use interface::*;

#[cfg(test)]
mod tests;

use {
    super::*,
    curios_abi::ForeignStore,
    curios_base::{Entropy, Plicity, Qualifier, RootId, RootKind, SyntaxRegistry},
    curios_core::Bound,
    std::{
        cell::{Cell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        rc::Rc,
    },
};

// Reject a reference that *resolves into* an internal root (`sys`) when the consuming module lies outside the privileged roots. `resolved` is the segments of the qualifier the reference resolved to — not the raw spelled path — so absolute and relative spellings are guarded identically. A non-internal target or a privileged consumer passes through.
fn guard_internal_root(
    table: &HashMap<Qualifier, ModuleInfo>,
    consumer: &Qualifier,
    resolved: &[String],
) -> Result<(), Error> {
    let Some(root) = resolved.first() else {
        return Ok(());
    };

    if !is_internal_root(table, root) {
        return Ok(());
    }

    if privileged(table, consumer) {
        Ok(())
    } else {
        Err(Error::InternalRootModule {
            segment: root.clone(),
        })
    }
}

// Whether `label` names an internal root: discoverable so the standard library can resolve it by absolute path, but unreachable from user code.
fn is_internal_root(table: &HashMap<Qualifier, ModuleInfo>, label: &str) -> bool {
    table
        .get(&Qualifier::from([label]))
        .is_some_and(|info| info.root.kind() == RootKind::Internal)
}

// Whether `consumer` is rooted in a privileged root (the standard library or an internal root itself), and so may reference internal roots.
fn privileged(table: &HashMap<Qualifier, ModuleInfo>, consumer: &Qualifier) -> bool {
    table
        .get(consumer)
        .is_some_and(|info| info.root.kind().is_privileged())
}

struct Resolved {
    modules: HashMap<Qualifier, Rc<Module>>,
    table: HashMap<Qualifier, ModuleInfo>,
}

/// Build-time source set for the fixed compilation roots. The prelude owner supplies already parsed modules keyed by canonical qualifier; `curios-text` retains no embedded `/syn` or `/std` source table.
pub struct PreludeModules {
    roots: Vec<(String, RootId)>,
    modules: BTreeMap<Qualifier, Module>,
}

impl PreludeModules {
    pub fn new() -> Self {
        Self {
            roots: Vec::new(),
            modules: BTreeMap::new(),
        }
    }

    pub fn insert_root(&mut self, name: impl Into<String>, root: RootId, module: Module) {
        let name = name.into();
        assert!(
            root != RootId::Entry,
            "a prepared root cannot be the entry root"
        );
        assert!(
            !self.roots.iter().any(|(existing, _)| existing == &name),
            "prelude root '{name}' is already registered"
        );
        self.modules.insert(Qualifier::from([name.clone()]), module);
        self.roots.push((name, root));
    }

    pub fn insert_module(&mut self, path: Qualifier, module: Module) {
        assert!(!path.is_root(), "a prelude module path cannot be the root");
        assert!(
            self.modules.insert(path.clone(), module).is_none(),
            "prelude module '{}' is already registered",
            path.join()
        );
    }

    fn roots(&self) -> Vec<(String, RootId)> {
        self.roots.clone()
    }

    fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        self.modules
            .get(qualifier)
            .cloned()
            .ok_or_else(|| Error::ModuleNotFound {
                path: qualifier.join(),
            })
    }
}

impl Default for PreludeModules {
    fn default() -> Self {
        Self::new()
    }
}

/// Opaque fixed Text state restored from the build-scoped prelude artifact.
#[derive(Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct PreparedPrelude {
    roots: Vec<(String, RootId)>,
    table: BTreeMap<Qualifier, ModuleInfo>,
    public: BTreeMap<Qualifier, PublicInterface>,
    core: curios_core::Module,
    metavariable_floor: usize,
    binder_floor: usize,
    witness_floor: usize,
    universe_floor: usize,
}

impl PreparedPrelude {
    pub fn core(&self) -> &curios_core::Module {
        &self.core
    }

    /// This prepared prelude with its lowered module hash-consed against `sharing`. Pass the same table used for the elaborated module so equal structures collapse across the two snapshots, not merely within each.
    ///
    /// The rest of a `PreparedPrelude` is resolution metadata and floors — no terms — so the lowered module is the whole of what there is to share.
    pub fn shared(self, sharing: &curios_core::Sharing) -> Self {
        Self {
            core: self.core.shared(sharing),
            ..self
        }
    }

    pub fn metavariable_floor(&self) -> usize {
        self.metavariable_floor
    }

    pub fn binder_floor(&self) -> usize {
        self.binder_floor
    }

    /// One past the highest witness identity the fixed prelude minted. Entry lowering resumes strictly above it: a replayed witness's identity was fixed in an earlier compiler run, and a fresh mint that aliased one would silently rebind a coherence-table entry.
    pub fn witness_floor(&self) -> usize {
        self.witness_floor
    }

    pub fn universe_floor(&self) -> usize {
        self.universe_floor
    }
}

impl Resolved {
    fn new() -> Self {
        Self {
            modules: HashMap::new(),
            table: HashMap::new(),
        }
    }

    fn for_entrypoint(entrypoint: &Entrypoint, loader: &RootSource) -> Result<Self, Error> {
        let mut resolved = Self::new();
        resolved.resolve(entrypoint, loader, &[])?;

        Ok(resolved)
    }

    // No synthesized `mod sys;`-style declarations here: the entry program's own `ModuleInfo` is built directly from its own raw items, then sys/syn/std are registered as its children *explicitly* — a deliberate fact, not something recovered later by pattern-matching a qualifier's leading string segment. `insert_child` (hardened to reject any collision, not just pub/pub) is what catches a user's own `mod std` colliding with this registration, in either direction.
    fn resolve(
        &mut self,
        entrypoint: &Entrypoint,
        loader: &RootSource,
        mounted_roots: &[(String, RootId)],
    ) -> Result<(), Error> {
        let mut root_info = scan_module_info(&entrypoint.module.items, RootId::Entry)?;

        for (name, _) in mounted_roots {
            root_info.insert_child(name.clone(), true)?;
        }

        self.table.insert(Qualifier::empty(), root_info);

        self.discover_children(
            &entrypoint.module.items,
            &Qualifier::empty(),
            loader,
            RootId::Entry,
        )
    }

    // `mod` declarations only name children, so the module graph is a tree: every qualifier is reached exactly once and no cycles are possible. Hence the walk needs neither a visited-set nor a cache hit-check — just load each file module once and recurse. `root` is inherited unchanged through the whole recursion — set once by the caller (`resolve`, at one of the four real roots), never re-derived from `prefix`'s string content here.
    fn discover(
        &mut self,
        items: &[TopItem],
        prefix: &Qualifier,
        loader: &RootSource,
        root: RootId,
    ) -> Result<(), Error> {
        self.table
            .insert(prefix.clone(), scan_module_info(items, root)?);
        self.discover_children(items, prefix, loader, root)
    }

    // The child-recursion half of `discover`, split out so `resolve` can build the entry root's `ModuleInfo` itself (with sys/syn/std pre-registered as children) and recurse into its children without a second, unconditional `scan_module_info` call clobbering that registration.
    fn discover_children(
        &mut self,
        items: &[TopItem],
        prefix: &Qualifier,
        loader: &RootSource,
        root: RootId,
    ) -> Result<(), Error> {
        for item in items {
            if let TopItem::Mod(module_item) = item {
                let path = prefix.with(&module_item.label);

                match &module_item.module {
                    Some(module) => self.discover(&module.items, &path, loader, root)?,
                    None => {
                        let module =
                            Rc::new(loader.load(&path).map_err(
                                |error| match &module_item.span {
                                    Some(span) => error.at(span.clone()),
                                    None => error,
                                },
                            )?);

                        self.modules.insert(path.clone(), Rc::clone(&module));
                        self.discover(&module.items, &path, loader, root)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn for_prelude(input: &PreludeModules) -> Result<(Self, Vec<(String, RootId)>), Error> {
        let mut resolved = Self::new();
        let roots = input.roots();
        let mut root_info = ModuleInfo::new(RootId::Entry);
        for (name, _) in &roots {
            root_info.insert_child(name.clone(), true)?;
        }
        resolved.table.insert(Qualifier::empty(), root_info);

        for (name, root) in &roots {
            let path = Qualifier::empty().with(name);
            let content = Rc::new(input.load(&path)?);
            resolved.modules.insert(path.clone(), Rc::clone(&content));
            resolved.discover_input(&content.items, &path, input, *root)?;
        }

        Ok((resolved, roots))
    }

    fn discover_input(
        &mut self,
        items: &[TopItem],
        prefix: &Qualifier,
        input: &PreludeModules,
        root: RootId,
    ) -> Result<(), Error> {
        self.table
            .insert(prefix.clone(), scan_module_info(items, root)?);
        for item in items {
            if let TopItem::Mod(module_item) = item {
                let path = prefix.with(&module_item.label);
                match &module_item.module {
                    Some(module) => self.discover_input(&module.items, &path, input, root)?,
                    None => {
                        let module =
                            Rc::new(input.load(&path).map_err(
                                |error| match &module_item.span {
                                    Some(span) => error.at(span.clone()),
                                    None => error,
                                },
                            )?);
                        self.modules.insert(path.clone(), Rc::clone(&module));
                        self.discover_input(&module.items, &path, input, root)?;
                    }
                }
            }
        }
        Ok(())
    }
}

fn scan_module_info(items: &[TopItem], root: RootId) -> Result<ModuleInfo, Error> {
    let mut info = ModuleInfo::new(root);

    for item in items {
        match item {
            TopItem::Mod(m) => info.insert_child(m.label.clone(), m.vis_pub)?,
            TopItem::Let(l) => info.insert_binding(l.label.clone(), l.vis_pub)?,
            TopItem::Rec(ls) => {
                for l in ls {
                    info.insert_binding(l.label.clone(), l.vis_pub)?;
                }
            }
            TopItem::Induct(group) => {
                for u in group {
                    info.insert_induct_child(u.label.clone(), u.vis_pub, u.rep_pub)?;
                    info.insert_binding(u.label.clone(), u.vis_pub)?;
                }
            }
            // A struct declares one binding (the type-former), like a `let` — there are no value constructors and no nested namespace, so no child module.
            TopItem::Struct(s) => info.insert_binding(s.label.clone(), s.vis_pub)?,
            // A concept declares the type-former binding *and* a nested namespace (its method wrappers), like an inductive.
            TopItem::Concept(c) => {
                info.insert_child(c.label.clone(), c.vis_pub)?;
                info.insert_binding(c.label.clone(), c.vis_pub)?;
            }
            // A witness is anonymous: it declares no binding and occupies no lexical scope — its backing definition gets a compiler name.
            TopItem::Witness(_) => {}
            // A `foreign` declaration is an ordinary binding, like a `let` — it has no body of its own, but it is called the same way.
            TopItem::Foreign(f) => info.insert_binding(f.label.clone(), f.vis_pub)?,
            _ => {}
        }
    }

    Ok(info)
}

// The surface concept application `C(p₁, …)` for a method wrapper's `use w` binder: the concept name applied to its parameters, each carrying the parameter's declared plicity so the application matches the type-former.
fn concept_application(label: &str, params: &[(Plicity, String, Term)]) -> Term {
    let head: Term = Subterm::Name(Name::from(vec![label.to_string()])).into();
    if params.is_empty() {
        return head;
    }

    Subterm::Apply(Apply {
        head,
        params: params
            .iter()
            .map(|(plicity, label, _)| {
                (
                    *plicity,
                    Subterm::Name(Name::from(vec![label.clone()])).into(),
                )
            })
            .collect(),
    })
    .into()
}

// The surface concept application `C(args)` for a witness's declared type: the witnessed concept applied to the annotation's arguments (as written, so explicit).
fn witness_concept_application(concept: &Name, args: &[Term]) -> Term {
    let head: Term = Subterm::Name(concept.clone()).into();
    if args.is_empty() {
        return head;
    }

    Subterm::Apply(Apply {
        head,
        params: args
            .iter()
            .map(|arg| (Plicity::Explicit, arg.clone()))
            .collect(),
    })
    .into()
}

impl Term {
    // The head name of a concept-application term (a path, optionally applied) — used to read the super concept off a `use`-marked field's type. `None` if the type is not shaped like a concept application. This is into_core-specific vocabulary (concept applications are a `into_core` pass concept), so it lives here rather than on `Term`'s own `impl` in `term.rs`.
    fn concept_app_head(&self) -> Option<Name> {
        match self.as_subterm() {
            Subterm::Name(name) => Some(name.clone()),
            Subterm::Apply(apply) => apply.head.concept_app_head(),
            _ => None,
        }
    }
}

// Resolve a super concept's head to its qualified core name — the same rule `Lowerer`'s term-reference arm uses, minus the local-binder shadowing (a declaration-site super edge has no enclosing value scope).
fn resolve_concept_head(context: &Context, name: &Name) -> Result<curios_core::Global, Error> {
    let qualifier = if name.is_abs() || !name.is_single() {
        context.resolve_term_name(name)?
    } else {
        match context.bindings().get(name.head()) {
            Some(qualifier) => qualifier.clone(),
            None => Qualifier::from([name.head()]),
        }
    };
    Ok(curios_core::Global::Authored(qualifier))
}

#[allow(clippy::too_many_arguments)]
fn process_items(
    top_items: &[TopItem],
    context: &mut Context,
    flat_items: &mut Vec<FlatItem>,
    induct_decls: &mut BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &mut BTreeMap<curios_core::Global, curios_core::StructDecl>,
    concepts: &mut BTreeMap<curios_core::Global, curios_core::ConceptDecl>,
    witnesses: &mut BTreeSet<curios_core::Global>,
    foreigns: &mut ForeignStore,
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
            TopItem::Induct(group) => {
                for u in group {
                    context.insert_scope(u.label.clone(), context.prefixed(&u.label))?;
                    context.insert_binding(u.label.clone(), context.prefixed(&u.label))?;
                }
            }
            // The type-former binding only — like a `let` (no constructor namespace).
            TopItem::Struct(s) => {
                context.insert_binding(s.label.clone(), context.prefixed(&s.label))?
            }
            // A concept declares its type-former binding and a nested namespace for the method wrappers, like an inductive.
            TopItem::Concept(c) => {
                context.insert_scope(c.label.clone(), context.prefixed(&c.label))?;
                context.insert_binding(c.label.clone(), context.prefixed(&c.label))?;
            }
            // A witness is anonymous — no binding, no scope entry.
            TopItem::Witness(_) => {}
            TopItem::Foreign(f) => {
                context.insert_binding(f.label.clone(), context.prefixed(&f.label))?
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
                        induct_decls,
                        struct_decls,
                        concepts,
                        witnesses,
                        foreigns,
                        modules,
                    )?;
                }
                None => {
                    let path = context.prefixed(&mod_item.label);
                    // Discovery is exhaustive over this same tree, so every file-backed module is already cached under this qualifier.
                    let module = modules.get(&path).expect("module loaded during discovery");

                    process_items(
                        &module.items,
                        &mut context.nested(&mod_item.label),
                        flat_items,
                        induct_decls,
                        struct_decls,
                        concepts,
                        witnesses,
                        foreigns,
                        modules,
                    )?;
                }
            },
            TopItem::Use(use_item) => {
                // The lexical import effect of `use`/`pub use`: source-ordered, point-of-use scoping. The interface (export) effect of `pub use` is precomputed in the phase-3 fixed point, not here.
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
                let lower = Lowerer::new(context);
                let type_ = lower.term(&let_item.signature.type_())?;
                flat_items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::Authored,
                    name: curios_core::Global::Authored(context.prefixed(&let_item.label)),
                    island: context.island(),
                    root: context.root(),
                    type_,
                    body: lower.value(&let_item.signature.body())?,
                }));
            }
            TopItem::Foreign(f) => {
                // All FFI-specific bookkeeping (the `ForeignFunction`, its registration, and `host_fn`'s wire-typed signature shape) stays inside `prelude`; from here a `foreign` declaration lowers exactly like an ordinary `TopItem::Let`.
                let path = context.prefixed(&f.label);
                let signature = foreign_signature(f, foreigns, path.join());

                let lower = Lowerer::new(context);
                let type_ = lower.term(&signature.type_())?;
                flat_items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::Authored,
                    name: curios_core::Global::Authored(path),
                    island: context.island(),
                    root: context.root(),
                    type_,
                    body: lower.value(&signature.body())?,
                }));
            }
            TopItem::Rec(ls) => {
                let items = ls
                    .iter()
                    .map(|let_item| {
                        let lower = Lowerer::new(context);
                        let type_ = lower.term(&let_item.signature.type_())?;
                        Ok(FlatLet {
                            kind: curios_core::DefinitionKind::Authored,
                            name: curios_core::Global::Authored(context.prefixed(&let_item.label)),
                            island: context.island(),
                            root: context.root(),
                            type_,
                            body: lower.value(&let_item.signature.body())?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(FlatItem::Rec(items));
            }
            TopItem::Induct(group) => {
                // Step 1: type bindings as one rec group. An inductive's type binding wraps a primitive `InductType` normal form in a `Func` over its type parameters and indices (so `Result(Nat, Bin)` beta-reduces to `InductType { Result, [Nat, Bin] }` and `Vec(Bin, 3)` to `InductType { Vec, [Bin], [3] }`), and its shape is recorded in the inductive registry.
                let type_flat_items = group
                    .iter()
                    .map(|u| {
                        let lower = Lowerer::new(context);
                        let name = curios_core::Global::Authored(context.prefixed(&u.label));

                        // Parameters and indices are minted before any of their types is lowered, and each type sees the binders before it — a later index type naming an earlier parameter must mean *that* binder.
                        let head_binders =
                            lower.mint(u.params.iter().map(|(_, n, _)| n.clone()).chain(
                                u.indices.iter().enumerate().map(|(i, (n, _))| {
                                    n.clone().unwrap_or_else(|| format!("_{i}"))
                                }),
                            ));
                        let (param_binders, index_binders) = head_binders.split_at(u.params.len());

                        let param_tys = u
                            .params
                            .iter()
                            .enumerate()
                            .map(|(i, (p, _, t))| {
                                let ty = lower.bound(&head_binders[..i], || lower.input_type(t))?;
                                Ok((*p, param_binders[i].1.clone(), ty))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;
                        // The registry and the `InductType` normal form are positional; plicity matters only on the generated type-constructor function.
                        let param_tys_unmarked = param_tys
                            .iter()
                            .map(|(_, n, t)| (n.clone(), t.clone()))
                            .collect::<Vec<_>>();

                        let param_vars = param_binders
                            .iter()
                            .map(|(_, id)| {
                                curios_core::Term::var(curios_core::Var::free(id.clone()))
                            })
                            .collect::<Vec<_>>();

                        // The head's index telescope. Unnamed entries got a positional placeholder above — the name only matters for dependency capture among the index types.
                        let index_tys = u
                            .indices
                            .iter()
                            .enumerate()
                            .map(|(i, (_, t))| {
                                let seen = u.params.len() + i;
                                let ty =
                                    lower.bound(&head_binders[..seen], || lower.input_type(t))?;
                                Ok((index_binders[i].1.clone(), ty))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;

                        let index_vars = index_binders
                            .iter()
                            .map(|(_, id)| {
                                curios_core::Term::var(curios_core::Var::free(id.clone()))
                            })
                            .collect::<Vec<_>>();

                        // Registry entry: the parameter telescope plus each constructor's full signature `(params..., payload...) -> InductType { name, params, indices }`, where the terminal's indices are that *case's* target expressions over its payload binders. `Telescope::build` captures the parameter and payload labels in the payload types and the terminal, mirroring `func_type`.
                        let constructors = u
                            .cases
                            .iter()
                            .map(|c| {
                                let payload_binders =
                                    lower.mint(c.payload.iter().enumerate().map(|(i, param)| {
                                        param.label.clone().unwrap_or_else(|| format!("_{i}"))
                                    }));
                                let mut scope = param_binders.to_vec();
                                let fields = c
                                    .payload
                                    .iter()
                                    .enumerate()
                                    .map(|(i, param)| {
                                        let ty = lower
                                            .bound(&scope, || lower.input_type(&param.type_))?;
                                        scope.push(payload_binders[i].clone());
                                        Ok((payload_binders[i].1.clone(), ty))
                                    })
                                    .collect::<Result<Vec<_>, Error>>()?;

                                let target = lower.bound(&scope, || {
                                    c.target
                                        .iter()
                                        .flatten()
                                        .map(|t| lower.term(t))
                                        .collect::<Result<Vec<_>, Error>>()
                                })?;

                                // The signature terminates in the index targets alone: the family and its parameters are fixed by the declaration, so a terminal carries nothing else.
                                let telescope = curios_core::Telescope::build(
                                    param_tys_unmarked.iter().cloned().chain(fields),
                                    target,
                                );

                                // The value constructor's calling convention: every leading declaration parameter is implicit, each payload keeps its declared mark — the same source `ctor_type` uses.
                                let plicities = u
                                    .params
                                    .iter()
                                    .map(|_| Plicity::Implicit)
                                    .chain(c.payload.iter().map(|param| param.plicity))
                                    .collect::<Vec<_>>();

                                Ok((
                                    curios_core::Atom::from(c.label.as_str()),
                                    curios_core::InductParam {
                                        telescope,
                                        plicities,
                                    },
                                ))
                            })
                            // Collected in written order: a constructor's position here is the runtime tag `erase` gives it (`InductDecl::constructors`), so the sequence is the declaration's, not a collation of its labels.
                            .collect::<Result<Vec<_>, Error>>()?;

                        // The declared result sort (`Type`/`Prop`) — closed, so it lowers in the base context. It is both the registry entry's sort and the type-constructor's codomain.
                        let result_sort = lower.term(&u.result_sort)?;

                        induct_decls.insert(
                            name.clone(),
                            curios_core::InductDecl {
                                universe_context: curios_core::UniverseContext::empty(),
                                arity: curios_core::Telescope::build(
                                    param_tys_unmarked.clone(),
                                    curios_core::Telescope::build(index_tys.iter().cloned(), ()),
                                ),
                                constructors,
                                result_sort: result_sort.clone(),
                                module: context.island(),
                                root: context.root(),
                                rep_public: u.rep_pub,
                                // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                                polarities: Vec::new(),
                            },
                        );

                        let induct_decl =
                            curios_core::Term::induct_type(name.clone(), param_vars, index_vars);

                        // The type constructor is flat over params then indices: `Vec : (T : Type, n : Nat) -> Type`. Use sites never distinguish the two. Parameters keep their declared marks (`@` makes one implicit at use sites); indices are always explicit.
                        let binder_tys: Vec<_> = param_tys
                            .iter()
                            .cloned()
                            .chain(
                                index_tys
                                    .iter()
                                    .cloned()
                                    .map(|(n, t)| (Plicity::Explicit, n, t)),
                            )
                            .collect();
                        let (type_, body) = if binder_tys.is_empty() {
                            (result_sort, induct_decl)
                        } else {
                            (
                                curios_core::Term::func_type_marked(
                                    binder_tys.clone(),
                                    result_sort,
                                ),
                                curios_core::Term::func_marked(binder_tys, induct_decl),
                            )
                        };
                        Ok(FlatLet {
                            kind: curios_core::DefinitionKind::InductiveType,
                            name: curios_core::Global::Authored(context.prefixed(&u.label)),
                            island: context.island(),
                            root: context.root(),
                            type_,
                            body,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(FlatItem::Rec(type_flat_items));

                // Step 2: constructor bindings. Each is a function whose body injects the variant as a tagged tuple.
                for u in group {
                    for c in &u.cases {
                        let lower = Lowerer::new(context);

                        // Per-case payload binder names: the declared name, or a positional placeholder.
                        let payload_name = |i: usize, n: &Option<String>| {
                            n.clone().unwrap_or_else(|| format!("_{i}"))
                        };

                        // Output type term `T`, `T(A, ...)`, or — indexed — the case's full terminal `T(A, ..., target...)`, elaborated as a name ref applied to the parameters and the target's index expressions.
                        let output_args: Vec<(Plicity, Term)> = u
                            .params
                            .iter()
                            .map(|(p, n, _)| {
                                // Each argument's mark must match its binder on the type constructor (the two-queue rule): an `@`-marked parameter is filled from the implicit queue.
                                (*p, Subterm::Name(Name::from(vec![n.clone()])).into())
                            })
                            .chain(
                                c.target
                                    .iter()
                                    .flatten()
                                    .map(|t| (Plicity::Explicit, t.clone())),
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

                        // Constructor type: (params..., _0 : T_0, ...) -> T. Every inductive parameter is implicit at the value constructor — `Result/success(42)` infers them, the call-site `@` supplies one positionally — while the payload binders keep their declared marks (`@m` makes one implicit; the default is explicit).
                        let binders = lower.mint(
                            u.params.iter().map(|(_, n, _)| n.clone()).chain(
                                c.payload
                                    .iter()
                                    .enumerate()
                                    .map(|(i, param)| payload_name(i, &param.label)),
                            ),
                        );
                        let plicities = u
                            .params
                            .iter()
                            .map(|_| Plicity::Implicit)
                            .chain(c.payload.iter().map(|param| param.plicity))
                            .collect::<Vec<_>>();
                        let written = u
                            .params
                            .iter()
                            .map(|(_, _, t)| t)
                            .chain(c.payload.iter().map(|param| &param.type_))
                            .collect::<Vec<_>>();
                        let param_tys = written
                            .iter()
                            .enumerate()
                            .map(|(i, t)| {
                                let ty = lower.bound(&binders[..i], || lower.input_type(t))?;
                                Ok((plicities[i], binders[i].1.clone(), ty))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;
                        let payload_binders = &binders[u.params.len()..];
                        let param_binders = &binders[..u.params.len()];
                        // Erasure is sort-driven: `erase_func` drops the same proof/type payload params that `erase_variant` drops from the tuple — the constructor function's arity and its injected variant's arity stay in lockstep.
                        let ctor_type = curios_core::Term::func_type_marked(
                            param_tys.clone(),
                            lower.bound(&binders, || lower.term(&output_type))?,
                        );
                        // Constructor body: (params..., _0, ...) => the variant's injection, a primitive `Variant` normal form.
                        let args: Vec<curios_core::Term> = payload_binders
                            .iter()
                            .map(|(_, id)| {
                                curios_core::Term::var(curios_core::Var::free(id.clone()))
                            })
                            .collect();
                        let inject = curios_core::Term::variant(
                            curios_core::Global::Authored(context.prefixed(&u.label)),
                            param_binders.iter().map(|(_, id)| {
                                curios_core::Term::var(curios_core::Var::free(id.clone()))
                            }),
                            curios_core::Atom::from(c.label.as_str()),
                            args,
                        );
                        // The value constructor carries the same calling convention as `ctor_type`: every inductive parameter is implicit, each payload keeps its declared mark.
                        let ctor_body = curios_core::Term::func_marked(param_tys, inject);

                        flat_items.push(FlatItem::Let(FlatLet {
                            kind: curios_core::DefinitionKind::InductiveConstructor {
                                owner: context.prefixed(&u.label),
                                tag: curios_core::Atom::from(c.label.as_str()),
                            },
                            name: curios_core::Global::Authored(
                                context.prefixed(&u.label).with(&c.label),
                            ),
                            island: context.island(),
                            root: context.root(),
                            type_: ctor_type,
                            body: ctor_body,
                        }));
                    }
                }
            }
            // A struct lowers to a single type-former `let` plus a registry entry — no value-constructor binding (the literal elaborates directly) and no indices.
            TopItem::Struct(s) => {
                let lower = Lowerer::new(context);

                let name = curios_core::Global::Authored(context.prefixed(&s.label));
                // Declaring module: the type-former's qualifier prefix — identical to core's per-item `island` — for the representation-privacy checks.
                let module = context.prefixed(&s.label).without_last();
                let root = context.root();

                let param_binders = lower.mint(s.params.iter().map(|(_, n, _)| n.clone()));
                let param_tys = s
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, (p, _, t))| {
                        let ty = lower.bound(&param_binders[..i], || lower.input_type(t))?;
                        Ok((*p, param_binders[i].1.clone(), ty))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                let param_tys_unmarked = param_tys
                    .iter()
                    .map(|(_, n, t)| (n.clone(), t.clone()))
                    .collect::<Vec<_>>();
                let param_vars = param_binders
                    .iter()
                    .map(|(_, id)| curios_core::Term::var(curios_core::Var::free(id.clone())))
                    .collect::<Vec<_>>();

                // Field types, with declared or positional (`_i`) names so a later field type can depend on an earlier field. The signature sugar `f(params) -> T` is undone here.
                let field_binders = lower.mint(
                    s.fields
                        .iter()
                        .enumerate()
                        .map(|(i, param)| param.label.clone().unwrap_or_else(|| format!("_{i}"))),
                );
                let mut field_scope = param_binders.clone();
                let field_tys = s
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, param)| {
                        let ty = lower
                            .bound(&field_scope, || lower.input_type(&param.desugared_type()))?;
                        field_scope.push(field_binders[i].clone());
                        Ok((field_binders[i].1.clone(), ty))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                // Registry entry: the parameter telescope, and the full field telescope (parameter binders first — field types may mention them — then field binders), as in `Inductive::indices`. The declared result sort (`Type`/`Prop`) — closed; both the registry entry's sort and the type-former's codomain.
                let result_sort = lower.term(&s.result_sort)?;

                struct_decls.insert(
                    name.clone(),
                    curios_core::StructDecl {
                        universe_context: curios_core::UniverseContext::empty(),
                        arity: curios_core::Telescope::build(
                            param_tys_unmarked.clone(),
                            curios_core::Telescope::build(field_tys, ()),
                        ),
                        result_sort: result_sort.clone(),
                        module,
                        root,
                        rep_public: s.rep_pub,
                        // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                        polarities: Vec::new(),
                    },
                );

                // The type-former: `Pair : (A : Type, B : Type) -> Type` whose body is the `StructType` normal form (the bare node when parameterless), so `Pair(Nat, Bin)` reduces to `StructType { Pair, [Nat, Bin] }`. No value constructor.
                let struct_type = curios_core::Term::struct_type(name.clone(), param_vars);
                let (type_, body) = if param_tys.is_empty() {
                    (result_sort, struct_type)
                } else {
                    (
                        curios_core::Term::func_type_marked(param_tys.clone(), result_sort),
                        curios_core::Term::func_marked(param_tys, struct_type),
                    )
                };

                flat_items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::StructType,
                    name: curios_core::Global::Authored(context.prefixed(&s.label)),
                    island: context.island(),
                    root: context.root(),
                    type_,
                    body,
                }));
            }
            // A concept lowers to a representation-public nominal `StructDecl` and its type-former `let` — plus a concept-registry entry (field labels, superclass edges, the parameter telescope) and one method-wrapper `let` per field, synthed into the concept's own namespace.
            TopItem::Concept(concept) => {
                let name = curios_core::Global::Authored(context.prefixed(&concept.label));
                let module = context.prefixed(&concept.label).without_last();
                let root = context.root();

                let lower = Lowerer::new(context);
                let param_binders = lower.mint(concept.params.iter().map(|(_, n, _)| n.clone()));
                let param_tys = concept
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, (p, _, t))| {
                        let ty = lower.bound(&param_binders[..i], || lower.input_type(t))?;
                        Ok((*p, param_binders[i].1.clone(), ty))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                let param_tys_unmarked = param_tys
                    .iter()
                    .map(|(_, n, t)| (n.clone(), t.clone()))
                    .collect::<Vec<_>>();
                let param_vars = param_binders
                    .iter()
                    .map(|(_, id)| curios_core::Term::var(curios_core::Var::free(id.clone())))
                    .collect::<Vec<_>>();

                // Superclass fields are anonymous in the surface syntax; mint a unique internal label per super so the record telescope and the registry's field list stay well-formed. The name is never surfaced — a superclass is reached by resolution, keyed by index, and never projected or wrapped by name.
                let field_labels = concept
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, field)| {
                        if field.is_super {
                            format!("_super{i}")
                        } else {
                            field.label.clone()
                        }
                    })
                    .collect::<Vec<_>>();

                // Field types, lowered under the parameter scope (a method field's label is the binder for later fields; a super field's minted label is inert). The signature sugar `f(params) -> T` is undone here.
                let field_binders = lower.mint(field_labels.iter().cloned());
                let mut field_scope = param_binders.clone();
                let field_tys = concept
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, field)| {
                        let ty = lower
                            .bound(&field_scope, || lower.input_type(&field.desugared_type()))?;
                        field_scope.push(field_binders[i].clone());
                        Ok((field_binders[i].1.clone(), ty))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                let result_sort = lower.term(&concept.result_sort)?;

                // The record shape drives struct literals and projections.
                struct_decls.insert(
                    name.clone(),
                    curios_core::StructDecl {
                        universe_context: curios_core::UniverseContext::empty(),
                        arity: curios_core::Telescope::build(
                            param_tys_unmarked.clone(),
                            curios_core::Telescope::build(field_tys, ()),
                        ),
                        result_sort: result_sort.clone(),
                        module,
                        root,
                        rep_public: concept.rep_pub,
                        // Positivity has not run yet: `curios-elab` computes each declaration's parameter polarities after elaboration and writes them back here.
                        polarities: Vec::new(),
                    },
                );

                // Superclass edges: each `use`-marked field names a super concept by its (resolved, qualified) head.
                let supers = concept
                    .fields
                    .iter()
                    .enumerate()
                    .filter(|(_, field)| field.is_super)
                    .map(|(idx, field)| {
                        let head = field.type_.concept_app_head().ok_or_else(|| {
                            Error::MalformedSuperField {
                                concept: concept.label.clone(),
                            }
                        })?;
                        Ok((idx, resolve_concept_head(context, &head)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                concepts.insert(
                    name.clone(),
                    curios_core::ConceptDecl {
                        universe_context: curios_core::UniverseContext::empty(),
                        params: curios_core::Telescope::build(param_tys_unmarked.clone(), ()),
                        fields: field_labels.clone(),
                        supers,
                        root,
                    },
                );

                // The type-former, exactly like a representation-public struct's.
                let struct_type = curios_core::Term::struct_type(name.clone(), param_vars);
                let (type_, body) = if param_tys.is_empty() {
                    (result_sort, struct_type)
                } else {
                    (
                        curios_core::Term::func_type_marked(param_tys.clone(), result_sort),
                        curios_core::Term::func_marked(param_tys, struct_type),
                    )
                };
                flat_items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::ConceptType,
                    name: curios_core::Global::Authored(context.prefixed(&concept.label)),
                    island: context.island(),
                    root: context.root(),
                    type_,
                    body,
                }));

                // Method wrappers: for each *method* field `f : F`, pub let C/f(@p₁ : P₁, …, use w : C(p₁, …)) -> F = w.f; Built as surface AST and lowered through `Lowerer`, so binder scoping and de-Bruijn capture are handled uniformly. Superclass fields are anonymous and get no wrapper: an instance of the outer concept already yields the inner one by resolution.
                let concept_app = concept_application(&concept.label, &concept.params);
                for field in concept.fields.iter().filter(|field| !field.is_super) {
                    let mut params = concept
                        .params
                        .iter()
                        .map(|(_, label, type_)| FuncSugarParam {
                            plicity: Plicity::Implicit,
                            label: Pattern::Binder(Some(label.clone())),
                            type_: type_.clone(),
                        })
                        .collect::<Vec<_>>();
                    params.push(FuncSugarParam {
                        plicity: Plicity::Witness,
                        label: Pattern::Binder(Some("w".to_string())),
                        type_: concept_app.clone(),
                    });

                    let signature = LetSignature::Func {
                        params,
                        output: field.desugared_type(),
                        body: Subterm::Proj(Proj {
                            head: Subterm::Name(Name::from(vec!["w".to_string()])).into(),
                            field: Field::Label(field.label.clone()),
                        })
                        .into(),
                    };

                    let lower = Lowerer::new(context);
                    flat_items.push(FlatItem::Let(FlatLet {
                        kind: curios_core::DefinitionKind::ConceptMethod {
                            owner: context.prefixed(&concept.label),
                        },
                        name: curios_core::Global::Authored(
                            context.prefixed(&concept.label).with(&field.label),
                        ),
                        island: context.island(),
                        root: context.root(),
                        // The wrapper re-lowers the field type in its *output* position, but that type's written-`Type` spans already seeded universes in the record pass above, under `input_type`'s lexical `Generalizable`. The span-keyed seeds are shared across the two lowerings — the wrapper must speak the concept's inherited levels — and `fresh_universe` asserts the roles agree, so the whole signature lowers `Generalizable`: the record's reading, not the output-position default that panicked on a field whose result spine spells `Type`.
                        type_: context
                            .with_universe_role(curios_core::UniverseRole::Generalizable, || {
                                lower.term(&signature.type_())
                            })?,
                        body: lower.value(&signature.body())?,
                    }));
                }
            }
            // A witness desugars to an anonymous top-level definition satisfy (tele) -> C(args) = C(args) { f = e, … }; and marks it for registration in the program-wide witness table. It gets an *identity*, not a manufactured name: a `satisfy` block has no name a programmer wrote, and the module a diagnostic reports for it comes from `Definition::island`.
            TopItem::Witness(witness) => {
                let name = curios_core::Global::Witness(context.fresh_witness());

                let concept_app = witness_concept_application(&witness.concept, &witness.args);
                let body: Term = Subterm::StructLit(StructLit {
                    head: witness.concept.clone(),
                    params: witness.args.clone(),
                    entries: witness
                        .entries
                        .iter()
                        .map(|entry| match entry {
                            WitnessEntry::Field(field) => StructLitEntry::Field(TupleField {
                                label: Some(field.label.clone()),
                                func_params: field.func_params.clone(),
                                value: field.value.clone(),
                            }),
                            WitnessEntry::Use(term) => StructLitEntry::Use(term.clone()),
                        })
                        .collect(),
                })
                .into();

                let signature = if witness.params.is_empty() {
                    LetSignature::Name {
                        type_: Some(concept_app),
                        body,
                    }
                } else {
                    LetSignature::Func {
                        params: witness.params.clone(),
                        output: concept_app,
                        body,
                    }
                };

                let lower = Lowerer::new(context);
                flat_items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::Witness,
                    name: name.clone(),
                    island: context.island(),
                    root: context.root(),
                    type_: lower.term(&signature.type_())?,
                    body: lower.value(&signature.body())?,
                }));
                witnesses.insert(name);
            }
        }
    }

    Ok(())
}

// Phase 5: reorder declarations so each one's value dependencies come before it (outer in the fold), since a cyclic name graph means source order is no longer a valid binding order. A stable Kahn pass keeps independent declarations in source order; a genuine value cycle leaves nodes unorderable, which are emitted in source order and rejected downstream as unbound names — there is nothing to repair, as cross-declaration value recursion is unexpressible by construction.
//
// The embedded, fixed prelude is every item under a privileged root (`sys`/`syn`/`std` — see `RootKind::is_privileged`), classified structurally rather than off a hardcoded name list. `std` and `syn` genuinely cross-reference each other in both directions (e.g. `/syn/Str`'s `classify` calls `/std/Nat`'s `in_range`, while `/std/Nat` itself uses `/syn/Str`'s `Scan`/`Utf8`), so the three privileged roots are topologically sorted together as *one* graph — there is no valid fixed sys/syn/std emission order to split them into independently. `sys` is not a distinct partition here as a result: it is always internally consistent with `syn`/`std` because all three are elaborated as one prelude block.
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

/// The nodes a node depends on: those `owner` maps its referenced names to. Self-edges and names `owner` does not map (primitives, or items outside the partition `owner` was restricted to) drop out.
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

/// Topologically order `nodes` (assumed ascending, for the lowest-index tiebreak) under `deps` restricted to that set: lowest-index node whose deps are all emitted; on a cycle, the lowest remaining one breaks the deadlock.
fn topological_order(nodes: &[usize], deps: &HashMap<usize, HashSet<usize>>) -> Vec<usize> {
    let mut emitted = HashSet::with_capacity(nodes.len());
    let mut order = Vec::with_capacity(nodes.len());

    while order.len() < nodes.len() {
        let ready = nodes
            .iter()
            .copied()
            .find(|&n| !emitted.contains(&n) && deps[&n].iter().all(|dep| emitted.contains(dep)))
            .or_else(|| nodes.iter().copied().find(|&n| !emitted.contains(&n)))
            .expect("a node remains while order is incomplete");

        emitted.insert(ready);
        order.push(ready);
    }

    order
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
) -> Vec<usize> {
    let owner = owner_of(items, prelude_nodes);
    let deps = prelude_nodes
        .iter()
        .map(|&n| {
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
            (n, dep_nodes(n, &names, &owner))
        })
        .collect::<HashMap<usize, HashSet<usize>>>();

    let relative = prelude_nodes
        .iter()
        .enumerate()
        .map(|(rel, &node)| (node, rel))
        .collect::<HashMap<usize, usize>>();

    topological_order(prelude_nodes, &deps)
        .iter()
        .map(|node| relative[node])
        .collect()
}

fn order_flat_items(
    items: Vec<FlatItem>,
    induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
) -> Vec<FlatItem> {
    let count = items.len();

    let is_prelude = items
        .iter()
        .map(FlatItem::in_prelude)
        .collect::<Vec<bool>>();
    let prelude_nodes = (0..count)
        .filter(|&i| is_prelude[i])
        .collect::<Vec<usize>>();
    let rest = (0..count)
        .filter(|&i| !is_prelude[i])
        .collect::<Vec<usize>>();

    let rest_owner = owner_of(&items, &rest);

    let mut order = Vec::with_capacity(count);

    if !prelude_nodes.is_empty() {
        let permutation = prelude_permutation(
            &items,
            &prelude_nodes,
            induct_decls,
            struct_decls,
            &rest_owner,
        );
        order.extend(permutation.into_iter().map(|rel| prelude_nodes[rel]));
    }

    // Everything else (user code, plus any non-prelude library a custom loader serves): topologically ordered among itself, after the whole prelude. Its dependencies on prelude items are already satisfied by the prefix above, so the owner map (and thus the dep edges) need only cover `rest`.
    let rest_deps = rest
        .iter()
        .map(|&n| {
            let declared = items[n].names();
            let names = node_reference_names(&items[n], &declared, induct_decls, struct_decls);
            (n, dep_nodes(n, &names, &rest_owner))
        })
        .collect::<HashMap<usize, HashSet<usize>>>();
    order.extend(topological_order(&rest, &rest_deps));

    let mut slots = items
        .into_iter()
        .map(Some)
        .collect::<Vec<Option<FlatItem>>>();
    order
        .into_iter()
        .map(|node| slots[node].take().unwrap())
        .collect()
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

/// The external references of a struct registry entry: every free var of its arity — its parameter domains and the field telescope they terminate in. Like `induct_free_vars`, this is what makes a struct's type-former node depend on the (e.g. primitive) types its fields mention — they live nowhere in the type-former's own body, which is just the `StructType` normal form.
fn struct_free_vars(struct_decl: &curios_core::StructDecl) -> HashSet<curios_core::Global> {
    struct_decl
        .arity
        .free_vars()
        .into_iter()
        .filter_map(|name| name.as_global().cloned())
        .collect()
}

#[derive(Clone)]
struct AliasEdge {
    target: curios_core::Global,
    dependencies: Option<BTreeSet<curios_core::Global>>,
}

fn flat_aliases(items: &[FlatItem]) -> HashMap<curios_core::Global, AliasEdge> {
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

/// Follow a directly attached representation provenance or a chain of bare, transparent type aliases to the underlying nominal registry entry.
fn exposed_nominal(
    entry: &Entry,
    aliases: &HashMap<curios_core::Global, AliasEdge>,
    induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
) -> Option<(curios_core::Global, Vec<AliasEdge>)> {
    let mut current = curios_core::Global::Authored(
        entry
            .representation
            .as_ref()
            .unwrap_or(&entry.target)
            .clone(),
    );
    let mut seen = HashSet::new();
    let mut traversed = Vec::new();

    loop {
        if induct_decls.contains_key(&current) || struct_decls.contains_key(&current) {
            return Some((current, traversed));
        }
        if !seen.insert(current.clone()) {
            return None;
        }
        let edge = aliases.get(&current)?.clone();
        current = edge.target.clone();
        traversed.push(edge);
    }
}

/// Invert the alias map to its transitive closure: for each canonical name, the bare transparent aliases that reach it. A name is as visible as the widest alias that stands for it, so an exported alias carries its target's audience even when the target itself is never exported.
fn alias_sources(
    aliases: &HashMap<curios_core::Global, AliasEdge>,
) -> HashMap<curios_core::Global, HashSet<curios_core::Global>> {
    let mut sources: HashMap<curios_core::Global, HashSet<curios_core::Global>> = HashMap::new();

    for (name, edge) in aliases {
        sources
            .entry(edge.target.clone())
            .or_default()
            .insert(name.clone());
    }

    loop {
        let mut changed = false;
        let pairs: Vec<(curios_core::Global, Vec<curios_core::Global>)> = sources
            .iter()
            .map(|(target, names)| (target.clone(), names.iter().cloned().collect()))
            .collect();

        for (target, names) in pairs {
            for name in names {
                let Some(indirect) = sources.get(&name).cloned() else {
                    continue;
                };
                let direct = sources.entry(target.clone()).or_default();
                for hop in indirect {
                    changed |= direct.insert(hop);
                }
            }
        }

        if !changed {
            break;
        }
    }

    sources
}

/// The top-level definitions among `names`. A binder is nobody's dependency: it is introduced and discharged inside the very signature being audited.
fn globals(
    names: impl IntoIterator<Item = curios_core::Free>,
) -> impl Iterator<Item = curios_core::Global> {
    names
        .into_iter()
        .filter_map(|name| name.as_global().cloned())
}

/// Everyone who can see `referent`, whether by its own name or through a transparent alias that stands for it.
fn referent_audience(
    audiences: &Audiences,
    sources: &HashMap<curios_core::Global, HashSet<curios_core::Global>>,
    referent: &curios_core::Global,
) -> Vec<Qualifier> {
    let Some(qualifier) = referent.qualifier() else {
        return Vec::new();
    };
    let mut audience = audiences.binding(qualifier);

    // A hop is matched by identity, and its qualifier is read off the name rather than split back out of a rendering.
    for alias in sources.get(referent).into_iter().flatten() {
        let Some(qualifier) = alias.qualifier() else {
            continue;
        };
        audience.extend(audiences.binding(qualifier));
    }

    audience
}

/// Every consumer of `item` — an item exposed to `exposure` — must be able to see everything `item`'s signature names. Checked against audiences rather than the declaration path, so an item re-exported out of a private module counts as visible exactly where the re-export puts it.
fn audit_dependencies(
    audiences: &Audiences,
    sources: &HashMap<curios_core::Global, HashSet<curios_core::Global>>,
    exposure: &[Qualifier],
    item: &str,
    dependencies: impl IntoIterator<Item = curios_core::Global>,
) -> Result<(), Error> {
    for referent in dependencies {
        let reach = referent_audience(audiences, sources, &referent);
        if !Audiences::covers(exposure, &reach) {
            return Err(Error::PrivateItemInPublicInterface {
                item: item.to_string(),
                referent: referent.symbol(),
            });
        }
    }

    Ok(())
}

/// Audit every declared signature and every exposed representation against the audience of the item carrying it. This runs after lowering because registry telescopes contain the complete signatures and transparent aliases have become canonical free-variable references. Re-export entries retain their representation provenance through the fixed point, so no `pub use` can upgrade an opaque declaration.
///
/// The declared type of every definition is audited here rather than during lowering: only the converged interface graph knows where a name ends up visible, so a signature naming an item re-exported out of a private child is accepted, while one naming something its own consumers cannot reach is not.
fn audit_public_exposures(
    public: &HashMap<Qualifier, PublicInterface>,
    table: &HashMap<Qualifier, ModuleInfo>,
    items: &[FlatItem],
    induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
) -> Result<(), Error> {
    let aliases = flat_aliases(items);
    let sources = alias_sources(&aliases);
    let audiences = Audiences::compute(public, table);

    for let_ in items.iter().flat_map(|item| match item {
        FlatItem::Let(let_) => std::slice::from_ref(let_),
        FlatItem::Rec(lets) => lets.as_slice(),
    }) {
        // Only definitions the source actually wrote. A member synthesized into a nested namespace — an inductive's constructor, a concept's method wrapper — sits below its declaring module rather than in it, and its signature is the declaration's business, not an interface the author wrote: a constructor facade may legitimately hand out values of a type the consumer cannot name.
        //
        // A witness has no authored path at all, which is the same answer arrived at structurally: "who can see this by its name" is not a question an anonymous declaration has. Its reach is the coherence table's, governed by the orphan rule at registration.
        let Some(path) = let_.name.qualifier() else {
            continue;
        };
        if path.without_last() != let_.island {
            continue;
        }

        let exposure = audiences.binding(path);
        audit_dependencies(
            &audiences,
            &sources,
            &exposure,
            &let_.name.symbol(),
            globals(let_.type_.free_vars()),
        )?;
    }

    for (module, interface) in public {
        for (label, entry) in &interface.bindings {
            let Some((nominal, traversed)) =
                exposed_nominal(entry, &aliases, induct_decls, struct_decls)
            else {
                continue;
            };
            let item = module.with(label).join();
            let exposure = audiences.module(module);

            for alias in traversed {
                if let Some(dependencies) = alias.dependencies {
                    audit_dependencies(&audiences, &sources, &exposure, &item, dependencies)?;
                }
            }

            if let Some(induct_decl) = induct_decls.get(&nominal) {
                let nominal_dependencies = globals(induct_decl.arity.free_vars());
                audit_dependencies(&audiences, &sources, &exposure, &item, nominal_dependencies)?;

                if induct_decl.rep_public {
                    audit_dependencies(
                        &audiences,
                        &sources,
                        &exposure,
                        &item,
                        globals(
                            induct_decl
                                .constructors
                                .iter()
                                .flat_map(|(_, case)| case.telescope.free_vars()),
                        ),
                    )?;
                }
            } else if let Some(struct_decl) = struct_decls.get(&nominal) {
                // The parameter domains alone: `arity.free_vars()` would reach the fields it terminates in, and the two are audited under different rules — parameters belong to the nominal type's public face, fields to its representation.
                let mut walk = &struct_decl.arity;
                let mut param_dependencies = Vec::new();
                while let curios_core::Telescope::Cons(domain, rest) = walk {
                    param_dependencies.extend(domain.free_vars());
                    walk = rest.body();
                }
                audit_dependencies(
                    &audiences,
                    &sources,
                    &exposure,
                    &item,
                    globals(param_dependencies),
                )?;

                if struct_decl.rep_public {
                    audit_dependencies(
                        &audiences,
                        &sources,
                        &exposure,
                        &item,
                        globals(struct_decl.fields().free_vars()),
                    )?;
                }
            }
        }
    }

    Ok(())
}

/// Lower an [`Entrypoint`] to a [`curios_core::Module`]. Also returns how many metavariable ids were minted for the module's holes: the floor `elaborate_module` needs so the ids it mints for implicit-argument insertion never collide with these.
pub fn into_core(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    syntax: &SyntaxRegistry,
) -> Result<(curios_core::Module, usize, usize, ForeignStore), Error> {
    curios_profile::profile!("into_core");
    let Resolved { mut table, modules } = Resolved::for_entrypoint(entrypoint, loader)?;
    let public = interface::resolve(entrypoint, &modules, &mut table)?;
    let metavars = Entropy::<usize>::new();
    let universes = Entropy::<usize>::new();
    let universe_role = Cell::new(curios_core::UniverseRole::Flexible);
    let universe_seeds = RefCell::new(Vec::new());
    let universe_allocations = RefCell::new(HashMap::new());
    let binders = Entropy::<usize>::new();
    let witness_ids = Entropy::<usize>::new();

    let mut context = Context::new(
        &table,
        &public,
        RootId::Entry,
        &metavars,
        &universes,
        &universe_role,
        &universe_seeds,
        &universe_allocations,
        &binders,
        &witness_ids,
        syntax,
    );

    let mut flat_items = Vec::new();
    let mut induct_decls = BTreeMap::new();
    let mut struct_decls = BTreeMap::new();
    // Concept resolution metadata and witness registration markers, populated as `concept`/`witness` items lower.
    let mut concepts = BTreeMap::new();
    let mut witnesses = BTreeSet::new();
    // `foreign` declarations found anywhere in this compilation's module graph (discovery above is already exhaustive over it) — separate from, and never merged with, the built-in `host_ops()` store the caller's prelude loader was built from.
    let mut foreigns = ForeignStore::new();

    process_items(
        &entrypoint.module.items,
        &mut context,
        &mut flat_items,
        &mut induct_decls,
        &mut struct_decls,
        &mut concepts,
        &mut witnesses,
        &mut foreigns,
        &modules,
    )?;

    let lower = Lowerer::new(&context);
    let type_ = entrypoint
        .type_
        .as_ref()
        .map(|type_| lower.term(type_))
        .transpose()?;
    let tail = lower.value(&entrypoint.tail)?;

    audit_public_exposures(&public, &table, &flat_items, &induct_decls, &struct_decls)?;

    // Emit the program as a flat list of named top-level definitions rather than folding it into one N-deep nested `let`/`rec` term. Cross-references (and the references in the entrypoint `body` and its `type_` annotation) stay free `Var`s keyed by the definition's joined name; the core passes `define` each one into the `Context`, so both the body and its annotation reduce through those definitions and agree — no shared binder scope required.
    let items = order_flat_items(flat_items, &induct_decls, &struct_decls)
        .into_iter()
        .map(FlatItem::into_core)
        .collect();

    Ok((
        curios_core::Module {
            items,
            universe_seeds: universe_seeds.into_inner(),
            induct_decls,
            struct_decls,
            concepts,
            witnesses,
            binder_floor: binders.count(),
            type_,
            body: tail,
        },
        metavars.count(),
        universes.count(),
        foreigns,
    ))
}

/// Resolve and lower the fixed roots once for build-time archival.
pub fn prepare_prelude(
    input: &PreludeModules,
    syntax: &SyntaxRegistry,
) -> Result<PreparedPrelude, Error> {
    curios_profile::profile!("prepare_prelude");
    let (Resolved { mut table, modules }, roots) = Resolved::for_prelude(input)?;
    let public = interface::resolve_prelude(&roots, &modules, &mut table)?;
    let metavars = Entropy::<usize>::new();
    let universes = Entropy::<usize>::new();
    let universe_role = Cell::new(curios_core::UniverseRole::Flexible);
    let universe_seeds = RefCell::new(Vec::new());
    let universe_allocations = RefCell::new(HashMap::new());
    let binders = Entropy::<usize>::new();
    let witness_ids = Entropy::<usize>::new();
    let mut context = Context::new(
        &table,
        &public,
        RootId::Entry,
        &metavars,
        &universes,
        &universe_role,
        &universe_seeds,
        &universe_allocations,
        &binders,
        &witness_ids,
        syntax,
    );
    for (name, _) in &roots {
        context.insert_scope(name.clone(), Qualifier::empty().with(name))?;
    }

    let mut flat_items = Vec::new();
    let mut induct_decls = BTreeMap::new();
    let mut struct_decls = BTreeMap::new();
    let mut concepts = BTreeMap::new();
    let mut witnesses = BTreeSet::new();
    let mut foreigns = ForeignStore::new();

    for (name, root) in &roots {
        let path = Qualifier::empty().with(name);
        let content = modules
            .get(&path)
            .expect("prelude root loaded during discovery");
        process_items(
            &content.items,
            &mut context.nested_root(name, *root),
            &mut flat_items,
            &mut induct_decls,
            &mut struct_decls,
            &mut concepts,
            &mut witnesses,
            &mut foreigns,
            &modules,
        )?;
    }

    audit_public_exposures(&public, &table, &flat_items, &induct_decls, &struct_decls)?;
    let items = order_flat_items(flat_items, &induct_decls, &struct_decls)
        .into_iter()
        .map(FlatItem::into_core)
        .collect();
    let core = curios_core::Module {
        items,
        universe_seeds: universe_seeds.into_inner(),
        induct_decls,
        struct_decls,
        concepts,
        witnesses,
        binder_floor: binders.count(),
        type_: None,
        body: curios_core::Term::prim(curios_core::Prim::Nat(curios_core::Nat::Zero)),
    };

    Ok(PreparedPrelude {
        roots,
        table: table.into_iter().collect(),
        public: public.into_iter().collect(),
        core,
        metavariable_floor: metavars.count(),
        binder_floor: binders.count(),
        witness_floor: witness_ids.count(),
        universe_floor: universes.count(),
    })
}

/// Lower only entry-owned modules and merge them onto a restored fixed prefix.
pub fn into_core_with_prelude(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    prepared: &PreparedPrelude,
    syntax: &SyntaxRegistry,
) -> Result<(curios_core::Module, usize, usize, ForeignStore), Error> {
    curios_profile::profile!("into_core_with_prelude");
    let mut resolved = Resolved {
        modules: HashMap::new(),
        table: prepared.table.clone().into_iter().collect(),
    };
    resolved.resolve(entrypoint, loader, &prepared.roots)?;
    let Resolved { mut table, modules } = resolved;
    let public = interface::resolve_with_prelude(
        entrypoint,
        &modules,
        &mut table,
        prepared.public.clone().into_iter().collect(),
    )?;

    let metavars = Entropy::<usize>::new();
    metavars.seed(prepared.metavariable_floor);
    let universes = Entropy::<usize>::new();
    universes.seed(prepared.universe_floor);
    let universe_role = Cell::new(curios_core::UniverseRole::Flexible);
    let universe_seeds = RefCell::new(prepared.core.universe_seeds.clone());
    let universe_allocations = RefCell::new(HashMap::new());
    let binders = Entropy::<usize>::new();
    binders.seed(prepared.binder_floor);
    let witness_ids = Entropy::<usize>::new();
    witness_ids.seed(prepared.witness_floor);
    let mut context = Context::new(
        &table,
        &public,
        RootId::Entry,
        &metavars,
        &universes,
        &universe_role,
        &universe_seeds,
        &universe_allocations,
        &binders,
        &witness_ids,
        syntax,
    );
    for (name, _) in &prepared.roots {
        context.insert_scope(name.clone(), Qualifier::empty().with(name))?;
    }

    let mut flat_items = Vec::new();
    let mut induct_decls = prepared.core.induct_decls.clone();
    let mut struct_decls = prepared.core.struct_decls.clone();
    let mut concepts = prepared.core.concepts.clone();
    let mut witnesses = prepared.core.witnesses.clone();
    let mut foreigns = ForeignStore::new();
    process_items(
        &entrypoint.module.items,
        &mut context,
        &mut flat_items,
        &mut induct_decls,
        &mut struct_decls,
        &mut concepts,
        &mut witnesses,
        &mut foreigns,
        &modules,
    )?;

    let lower = Lowerer::new(&context);
    let type_ = entrypoint
        .type_
        .as_ref()
        .map(|type_| lower.term(type_))
        .transpose()?;
    let body = lower.value(&entrypoint.tail)?;
    audit_public_exposures(&public, &table, &flat_items, &induct_decls, &struct_decls)?;

    let mut items = prepared.core.items.clone();
    items.extend(
        order_flat_items(flat_items, &induct_decls, &struct_decls)
            .into_iter()
            .map(FlatItem::into_core),
    );

    Ok((
        curios_core::Module {
            items,
            universe_seeds: universe_seeds.into_inner(),
            induct_decls,
            struct_decls,
            concepts,
            witnesses,
            binder_floor: binders.count(),
            type_,
            body,
        },
        metavars.count(),
        universes.count(),
        foreigns,
    ))
}
