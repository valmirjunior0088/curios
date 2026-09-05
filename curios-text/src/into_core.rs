mod audit;
use audit::*;

mod context;
use context::*;
use curios_elab::TermBuilders;

mod lowerer;
use lowerer::*;

mod match_compile;
use match_compile::*;

mod order;
use order::*;

mod interface;
use interface::*;

mod lint;
use lint::*;

mod scoped;
use scoped::*;

mod document;
use curios_document::Documentation;
use document::document;

#[cfg(test)]
mod binding_tests;
#[cfg(test)]
mod exposure_tests;
#[cfg(test)]
mod foreign_tests;
#[cfg(test)]
mod lint_tests;
#[cfg(test)]
mod lower_tests;
#[cfg(test)]
mod ordering_tests;
#[cfg(test)]
mod re_export_tests;
#[cfg(test)]
mod sys_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod universe_tests;
#[cfg(test)]
mod use_tests;
#[cfg(test)]
mod visibility_tests;

use {
    super::*,
    curios_abi::ForeignStore,
    curios_core::Bound,
    curios_utilities::{Entropy, Mount, Plicity, Qualifier, RootKind, Span, SyntaxRegistry},
    std::{
        cell::{Cell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        rc::Rc,
    },
};

// Reject a reference that *resolves into* an internal root (`sys`) when the consuming module lies outside the privileged roots. `resolved` is the segments of the qualifier the reference resolved to — not the raw spelled path — so absolute and relative spellings are guarded identically. A non-internal target or a privileged consumer passes through.
fn guard_internal_root(
    mounts: &[Mount],
    consumer: &Qualifier,
    resolved: &[String],
) -> Result<(), Error> {
    let Some(root) = resolved.first() else {
        return Ok(());
    };

    if !is_internal_root(mounts, root) {
        return Ok(());
    }

    if Mount::privileged(mounts, consumer) {
        Ok(())
    } else {
        Err(Error::InternalRootModule {
            segment: root.clone(),
        })
    }
}

// Whether `label` names an internal root: discoverable so the standard library can resolve it by absolute path, but unreachable from user code. Asked of the mount itself rather than of the name it owns, because only a whole mount is internal — a module inside one is reachable exactly as far as its mount is.
fn is_internal_root(mounts: &[Mount], label: &str) -> bool {
    let prefix = Qualifier::from([label]);

    mounts
        .iter()
        .any(|mount| mount.prefix == prefix && mount.kind == RootKind::Internal)
}

/// The compilation root's children: the prefix each mount in `mounts` claims.
///
/// **The root is the only namespace a mount implies, and that is a consequence of a name being one word.** A prefix is a single segment — the entry's is empty, which is what makes it the entry — so nothing lies between the root and a mount for a mount to bring into existence. Every other namespace in a compilation is one somebody declared. A set rather than a sequence, because a scope's mounts and the unit's own may name the same prefix and neither declared it.
fn mounted_children<'m>(mounts: impl Iterator<Item = &'m Mount>) -> BTreeSet<String> {
    mounts
        .flat_map(|mount| mount.prefix.iter())
        .map(str::to_string)
        .collect()
}

/// Everything the unit being compiled claims a prefix for, each paired with how a refusal should name it.
///
/// Its mounts, and — when it is the entry — the top-level modules it declares, because `mod myorg` claims `/myorg` against every other unit exactly as a mount would. The entry's own mount is the empty prefix, which claims nothing: every name lies within the compilation root.
fn claims(source: &UnitSource<'_>, own: &[Mount]) -> Vec<(String, Qualifier)> {
    let mounts = own
        .iter()
        .filter(|mount| !mount.prefix.is_root())
        .map(|mount| (format!("`{}`", mount.prefix.join()), mount.prefix.clone()));

    let modules = source.root_items().iter().filter_map(|item| match item {
        TopItem::Mod(declaration) => Some((
            format!("`mod {}` in the entry program", declaration.label),
            Qualifier::from([declaration.label.clone()]),
        )),
        _ => None,
    });

    mounts.chain(modules).collect()
}

struct Resolved<'a> {
    modules: HashMap<Qualifier, Rc<Module>>,
    /// Where each `mod` the unit declares was written, by the module it declares — what the `unused-declaration` lint underlines for a whole dead module.
    mod_spans: HashMap<Qualifier, Span>,
    /// The entry's own module graph, over whatever a prepared prelude already established. Every insertion below targets a module the entry declares; reads cross the boundary, which is why this is layered rather than copied. See [`Scoped`].
    table: Scoped<'a, ModuleInfo>,
}

/// Opaque fixed Text state restored from the build-scoped prelude artifact.
#[derive(Clone)]
#[curios_archive::archived]
pub struct PreparedText {
    mounts: Vec<Mount>,
    /// The `foreign` rows this unit declares. Collected by the same walk that lowers its items, and carried here rather than dropped: a unit that declares one has to reach the link, and the prelude declaring none is a fact about the prelude, not about the shape.
    foreigns: ForeignStore,
    table: BTreeMap<Qualifier, ModuleInfo>,
    public: BTreeMap<Qualifier, PublicInterface>,
    core: curios_core::Module,
    metavariable_floor: usize,
    binder_floor: usize,
    universe_floor: usize,
    /// Every bare name that resolved to nothing, by the binder it lowered to, with what it could have meant — see `Context::unbound_binder`. Empty for any unit that compiles, the prelude included.
    unbound: BTreeMap<curios_core::Free, Vec<Qualifier>>,
    /// Every binding a `use` brought into scope in this unit, with the spelling a reader wrote it under and, per definition, the ones in scope where it was written — see `Context::imports`. What a goal report's candidate pool reaches beyond the names the program already mentions, and the spelling each such candidate is displayed under.
    imports: curios_core::Imports,
    /// Every lint the lowering found, in reading order — see [`Lint`]. Carried with the unit because a lint depends on exactly what the unit's identity in the store depends on: its own sources and its scope's interfaces.
    lints: Vec<Lint>,
    /// The prefix of every mount some reference of this unit resolved into — see `Context::reached`.
    reached: BTreeSet<Qualifier>,
    /// This unit's interface for its consumers, when its resolver marked a mount as documented — built here, as the last thing the lowering does, so it travels with the unit. See `document`.
    documentation: Option<Documentation>,
}

impl PreparedText {
    pub fn lints(&self) -> &[Lint] {
        &self.lints
    }

    pub fn reached(&self) -> &BTreeSet<Qualifier> {
        &self.reached
    }

    pub fn core(&self) -> &curios_core::Module {
        &self.core
    }

    /// This unit's interface for its consumers, or `None` for a unit whose resolver marked nothing as documented — an entry program, or a prelude mount nobody reaches for by name.
    pub fn documentation(&self) -> Option<&Documentation> {
        self.documentation.as_ref()
    }

    /// The `foreign` rows this unit declares.
    pub fn foreigns(&self) -> &ForeignStore {
        &self.foreigns
    }

    /// This prepared prelude with its lowered module hash-consed against `sharing`. Pass the same table used for the elaborated module so equal structures collapse across the two snapshots, not merely within each.
    ///
    /// The rest of a `PreparedText` is resolution metadata and floors — no terms — so the lowered module is the whole of what there is to share.
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

    pub fn universe_floor(&self) -> usize {
        self.universe_floor
    }

    /// What each unresolved bare name could have meant, by the binder it lowered to — the table `curios-elab`'s `unbound variable` report reads its suggestion from.
    pub fn unbound(&self) -> &BTreeMap<curios_core::Free, Vec<Qualifier>> {
        &self.unbound
    }

    /// What each `use` brought into scope, where, and under which spelling — the table `curios-elab`'s goal suggestions draw imported candidates from and spell them by.
    pub fn imports(&self) -> &curios_core::Imports {
        &self.imports
    }
}

impl<'a> Resolved<'a> {
    /// Discover every module `source` declares, over what `scope` already established.
    ///
    /// **One walk, where the entry and a mounted unit each had their own.** They differed in where a root's items came from and in which prefixes the compilation root lists as children, both of which are answered below rather than duplicated: two copies of a tree walk agree by being read, which is the shape every configuration-dependent defect in this stage has had.
    ///
    /// No synthesized `mod sys;`-style declarations here: the compilation root's own `ModuleInfo` is built from the entry's raw items, then every mounted prefix is registered as its child *explicitly* — a deliberate fact, not something recovered later by pattern-matching a qualifier's leading string segment. `insert_child` (hardened to reject any collision, not just pub/pub) is what catches a user's own `mod std` colliding with that registration, in either direction.
    fn of(
        source: &UnitSource<'_>,
        scope: &'a [&'a BTreeMap<Qualifier, ModuleInfo>],
        scope_mounts: &[Mount],
        own: &[Mount],
    ) -> Result<Self, Error> {
        let mut resolved = Resolved {
            modules: HashMap::new(),
            mod_spans: HashMap::new(),
            table: Scoped::over(scope),
        };

        // The compilation root: the entry's own module when the entry is what is being lowered, and otherwise a synthetic one belonging to no unit — which is why its children are *every* mounted prefix rather than only this unit's.
        //
        // Writing it lands in this unit's own layer, which shadows whatever the scope's layer said, so listing only `own` here silently hides the scope's mounts from a unit being compiled against them. That is what made `/std` unreachable from a mounted unit, and the test that says a unit reaches a mounted name is what caught it.
        let mut root_info = scan_module_info(source.root_items())?;
        for child in mounted_children(scope_mounts.iter().chain(own)) {
            root_info.insert_child(child, true)?;
        }
        resolved.table.insert(Qualifier::empty(), root_info);

        // The entry's children hang off the root, whose `ModuleInfo` was just built by hand; a mounted unit has no root items, so this recurses over nothing for it.
        resolved.discover_children(source.root_items(), &Qualifier::empty(), source)?;

        for mount in own.iter().filter(|mount| !mount.prefix.is_root()) {
            let header = Rc::new(source.source.load(&mount.prefix)?);
            resolved
                .modules
                .insert(mount.prefix.clone(), Rc::clone(&header));
            resolved.discover(&header.items, &mount.prefix, source)?;
        }

        Ok(resolved)
    }

    // `mod` declarations only name children, so the module graph is a tree: every qualifier is reached exactly once and no cycles are possible. Hence the walk needs neither a visited-set nor a cache hit-check — just load each file module once and recurse.
    fn discover(
        &mut self,
        items: &[TopItem],
        prefix: &Qualifier,
        source: &UnitSource<'_>,
    ) -> Result<(), Error> {
        self.table.insert(prefix.clone(), scan_module_info(items)?);
        self.discover_children(items, prefix, source)
    }

    // The child-recursion half of `discover`, split out so `of` can build the compilation root's `ModuleInfo` itself (with every mounted prefix pre-registered as a child) and recurse into its children without a second, unconditional `scan_module_info` call clobbering that registration.
    fn discover_children(
        &mut self,
        items: &[TopItem],
        prefix: &Qualifier,
        source: &UnitSource<'_>,
    ) -> Result<(), Error> {
        for item in items {
            if let TopItem::Mod(module_item) = item {
                let path = prefix.with(&module_item.label);
                if let Some(span) = module_item.label.span() {
                    self.mod_spans.insert(path.clone(), span.clone());
                }

                match &module_item.module {
                    Some(module) => self.discover(&module.items, &path, source)?,
                    None => {
                        let module = Rc::new(source.source.load(&path).map_err(|error| {
                            match &module_item.span {
                                Some(span) => error.at(span.clone()),
                                None => error,
                            }
                        })?);

                        self.modules.insert(path.clone(), Rc::clone(&module));
                        self.discover(&module.items, &path, source)?;
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
            TopItem::Mod(m) => info.insert_child(m.label.to_string(), m.vis_pub)?,
            TopItem::Let(ls) => {
                for l in ls {
                    info.insert_binding(l.label.to_string(), l.vis_pub)?;
                }
            }
            TopItem::Induct(group) => {
                for u in group {
                    info.insert_induct_child(u.label.to_string(), u.vis_pub, u.rep_pub)?;
                    info.insert_binding(u.label.to_string(), u.vis_pub)?;
                }
            }
            // A struct declares one binding (the type-former), like a `let` — there are no value constructors and no nested namespace, so no child module.
            TopItem::Struct(group) => {
                for s in group {
                    info.insert_binding(s.label.to_string(), s.vis_pub)?;
                }
            }
            // A concept declares the type-former binding *and* a nested namespace (its method wrappers), like an inductive.
            TopItem::Concept(group) => {
                for c in group {
                    info.insert_child(c.label.to_string(), c.vis_pub)?;
                    info.insert_binding(c.label.to_string(), c.vis_pub)?;
                }
            }
            // A witness is anonymous: it declares no binding and occupies no lexical scope — its backing definition gets a compiler name.
            TopItem::Witness(_) => {}
            // A `foreign` declaration is an ordinary binding, like a `let` — it has no body of its own, but it is called the same way.
            TopItem::Foreign(f) => info.insert_binding(f.label.to_string(), f.vis_pub)?,
            // A test binds its name privately — referable within its subtree, colliding with a like-named sibling, never `pub`.
            TopItem::Test(t) => info.insert_binding(t.label.to_string(), false)?,
            _ => {}
        }
    }

    Ok(info)
}

// The surface concept application `C(args)` for a witness's declared type: the witnessed concept applied to the annotation's arguments (as written, so explicit).
//
// **Spanned over the written `C(args)`, because this is the only thing a `satisfy` refusal can point at.** A witness is anonymous, so nothing else in its declaration names it: `elaborate_module_let` locates a registration failure with `error.at_opt(def.type_.span())`, and this synthesized node is that type. Left spanless, every duplicate, orphan, unkeyable and irregular-premise refusal arrived with no line at all — and a duplicate between two witnesses of one module named that module twice and nothing else, which a reader cannot act on. The head name and each argument carry the spans this joins; `Term::with_span` keeps an innermost span already present, so the arguments are untouched.
fn witness_concept_application(concept: &Name, args: &[Term]) -> Term {
    let head: Term = Subterm::Name(concept.clone()).into();
    let written = |head: Term, last: Option<&Span>| match (concept.span(), last) {
        (Some(open), Some(close)) => {
            head.with_span(Span::new(open.source.clone(), open.start, close.end))
        }
        (Some(open), None) => head.with_span(open.clone()),
        (None, _) => head,
    };

    if args.is_empty() {
        return written(head, None);
    }

    let applied: Term = Subterm::Apply(Apply {
        head,
        arguments: args
            .iter()
            .map(|arg| Argument {
                term: arg.clone(),
                plicity: Plicity::Explicit,
            })
            .collect(),
    })
    .into();

    // The last argument's own span closes the range. An argument the parser gave none — a desugar's product — leaves the head's span alone, which still names the declaration.
    written(applied, args.last().and_then(Term::span))
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
            Some(qualifier) => {
                context.note_binding_use(name.head());
                qualifier.clone()
            }
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
    tests: &mut Vec<curios_core::Global>,
    foreigns: &mut ForeignStore,
    modules: &HashMap<Qualifier, Rc<Module>>,
) -> Result<(), Error> {
    for top_item in top_items {
        match top_item {
            TopItem::Mod(m) => {
                context.insert_scope(m.label.to_string(), context.prefixed(&m.label))?
            }
            TopItem::Let(labels) => {
                for l in labels {
                    context.insert_binding(l.label.to_string(), context.prefixed(&l.label))?;
                }
            }
            TopItem::Induct(group) => {
                for u in group {
                    context.insert_scope(u.label.to_string(), context.prefixed(&u.label))?;
                    context.insert_binding(u.label.to_string(), context.prefixed(&u.label))?;
                }
            }
            // The type-former binding only — like a `let` (no constructor namespace).
            TopItem::Struct(group) => {
                for s in group {
                    context.insert_binding(s.label.to_string(), context.prefixed(&s.label))?;
                }
            }
            // A concept declares its type-former binding and a nested namespace for the method wrappers, like an inductive.
            TopItem::Concept(group) => {
                for c in group {
                    context.insert_scope(c.label.to_string(), context.prefixed(&c.label))?;
                    context.insert_binding(c.label.to_string(), context.prefixed(&c.label))?;
                }
            }
            // A witness is anonymous — no binding, no scope entry.
            TopItem::Witness(_) => {}
            TopItem::Foreign(f) => {
                context.insert_binding(f.label.to_string(), context.prefixed(&f.label))?
            }
            TopItem::Test(t) => {
                context.insert_binding(t.label.to_string(), context.prefixed(&t.label))?
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
                        tests,
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
                        tests,
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
                            context.open_site(UseSite {
                                span: item.label().span().cloned(),
                                what: UseSiteKind::Selector(item.label().clone()),
                                exported: use_item.vis_pub,
                                used: false,
                            });

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
                            context.close_site();
                        }
                    }
                    UseGroup::Glob => {
                        context.open_site(UseSite {
                            span: use_item.span.clone(),
                            what: UseSiteKind::Glob(format!("{}/*", use_item.name.join())),
                            exported: use_item.vis_pub,
                            used: false,
                        });
                        context.resolve_glob(&use_item.name)?;
                        context.close_site();
                    }
                }
            }
            // A `let` item is a group of one or more definitions. It lowers to a `rec` item when it is a declared group or when its one member names itself — read off the lowered terms, so a definition's own name is in scope of its type and body without anything said in the source — and to a plain `let` item otherwise. The kernel needs the distinction and the programmer does not: a `Rec` binds its members' names, a `Let` leaves a self-reference unbound.
            TopItem::Let(ls) => {
                let mut items = ls
                    .iter()
                    .map(|let_item| {
                        context.record_import_scope(Some(&context.prefixed(&let_item.label)));
                        let lower = Lowerer::new(context);
                        lower.enter_signature(&let_item.signature);
                        let type_ = lower.term(&let_item.signature.type_())?;
                        Ok(FlatLet {
                            kind: curios_core::DefinitionKind::Authored,
                            span: let_item.label.span().cloned(),
                            name: curios_core::Global::Authored(context.prefixed(&let_item.label)),
                            island: context.island(),
                            type_,
                            body: lower.value(&let_item.signature.body())?,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                let recursive = items.len() > 1 || items.iter().any(|let_| let_.mentions_itself());
                flat_items.push(match recursive {
                    true => FlatItem::Rec(items),
                    false => FlatItem::Let(items.pop().expect("a `let` item has a member")),
                });
            }
            // A test is the function sugar its parentheses spell — a `let`'s `LetSignature::Func` shape with the output fixed. Its declared type is `(params) -> /syn/Test`: the output emitted as core directly off the registry slot, since a synthesized `Var` carries the resolved identity and nothing here depends on `/syn` being importable, closed under the written telescope. Its body is the authored body under the lambda binding every parameter. Empty parentheses give the harness's nullary test; a telescope gives a property the synthesized tail closes through `Test/property`.
            TopItem::Test(test) => {
                context.record_import_scope(Some(&context.prefixed(&test.label)));
                let lower = Lowerer::new(context);
                lower.enter_sugar();
                let output = curios_core::Term::var(curios_core::Var::free(
                    curios_core::Free::global(context.syntax().test.test_type.qualifier()),
                ));
                let type_ =
                    lower.func_type_under(&func_sugar_type_params(&test.params), || Ok(output))?;
                let body = func_sugar_lambda(&test.params, &test.body);
                let name = curios_core::Global::Authored(context.prefixed(&test.label));
                tests.push(name.clone());
                flat_items.push(FlatItem::Let(FlatLet {
                    kind: curios_core::DefinitionKind::Test,
                    span: None,
                    name,
                    island: context.island(),
                    type_,
                    body: lower.value(&body)?,
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
                    span: f.label.span().cloned(),
                    name: curios_core::Global::Authored(path),
                    island: context.island(),
                    type_,
                    body: lower.value(&signature.body())?,
                }));
            }
            TopItem::Induct(group) => {
                // Step 1: type bindings as one rec group. An inductive's type binding wraps an intrinsic `InductType` normal form in a `Func` over its type parameters and indices (so `Result(Bin, Nat)` beta-reduces to `InductType { Result, [Bin, Nat] }` and `Vec(Bin, 3)` to `InductType { Vec, [Bin], [3] }`), and its shape is recorded in the inductive registry.
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
                                    curios_core::InductParam::new(telescope, plicities),
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
                            span: u.label.span().cloned(),
                            name: curios_core::Global::Authored(context.prefixed(&u.label)),
                            island: context.island(),
                            type_,
                            body,
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                // Unconditionally, where a `struct`, `concept` or `witness` group tests `formers.len()` and a `let` group tests `mentions_itself` besides — so a lone non-recursive `induct` carries a `rec` group of one that means nothing, and every occurrence of `Option`, `Result` or `Bool` reduces through it.
                //
                // Making this conditional is not the two-line change the neighbours make it look like, and both halves were measured rather than reasoned. `mentions_itself` reads `free_vars_shared()`, but an inductive's recursion lives in its *registry entry* — `InductType` holds `name: Global` as a field, not a `Var` — so the test is false for every inductive, recursive ones included, and reaching for it lowers `/std/Nat/Le/Ind` as a `Let` that loses `elaborate_module_rec`'s `context.assume` and leaves its own name unbound while the registry telescopes rebuild. Reading the reach through `order::induct_free_vars` instead clears that and then fails at `/std/Async/Future/State` with `universe instance has 1 arguments but its scheme expects 0`: the group is also where a universe-polymorphic inductive's levels are generalized, and as a `Let` the scheme comes out monomorphic while occurrences still pass a level.
                //
                // So the wrapper does double duty, and dropping it needs `elaborate_module_let` to do both jobs. Nothing depends on that today — a folded spelling reduces correctly since `unfold_rec_apply` learned to apply a group that dissolved to its member's value — which leaves this an optimization with no measurement behind it.
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
                        let output_args: Vec<Argument> = u
                            .params
                            .iter()
                            .map(|(p, n, _)| Argument {
                                term: Subterm::Name(Name::from(vec![n.clone()])).into(),
                                // Each argument's mark must match its binder on the type constructor (the two-queue rule): an `@`-marked parameter is filled from the implicit queue.
                                plicity: *p,
                            })
                            .chain(c.target.iter().flatten().map(|t| Argument {
                                term: t.clone(),
                                plicity: Plicity::Explicit,
                            }))
                            .collect();
                        let output_type: Term = if output_args.is_empty() {
                            Subterm::Name(Name::from(vec![u.label.clone()])).into()
                        } else {
                            Subterm::Apply(Apply {
                                head: Subterm::Name(Name::from(vec![u.label.clone()])).into(),
                                arguments: output_args,
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
                        // Constructor body: (params..., _0, ...) => the variant's injection, an intrinsic `Variant` normal form.
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
                            span: None,
                            kind: curios_core::DefinitionKind::InductiveConstructor {
                                owner: context.prefixed(&u.label),
                                tag: curios_core::Atom::from(c.label.as_str()),
                            },
                            name: curios_core::Global::Authored(
                                context.prefixed(&u.label).with(&c.label),
                            ),
                            island: context.island(),
                            type_: ctor_type,
                            body: ctor_body,
                        }));
                    }
                }
            }
            // A struct lowers to a single type-former `let` plus a registry entry — no value-constructor binding (the literal elaborates directly) and no indices.
            // A group of structures lowers its formers into one `rec` item, as an `induct` group does, so each member's fields may name the others; a lone structure stays a `let`, its own name reached through its registry telescope.
            TopItem::Struct(group) => {
                let mut formers = Vec::with_capacity(group.len());
                for s in group {
                    let lower = Lowerer::new(context);

                    let name = curios_core::Global::Authored(context.prefixed(&s.label));
                    // Declaring module: the type-former's qualifier prefix — identical to core's per-item `island` — for the representation-privacy checks.
                    let module = context.prefixed(&s.label).without_last();

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
                    let field_binders =
                        lower.mint(s.fields.iter().enumerate().map(|(i, field)| {
                            field.param.label.clone().unwrap_or_else(|| format!("_{i}"))
                        }));
                    let mut field_scope = param_binders.clone();
                    let field_tys = s
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            let ty = lower.bound(&field_scope, || {
                                lower.input_type(&field.param.desugared_type())
                            })?;
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

                    formers.push(FlatLet {
                        kind: curios_core::DefinitionKind::StructType,
                        span: s.label.span().cloned(),
                        name: curios_core::Global::Authored(context.prefixed(&s.label)),
                        island: context.island(),
                        type_,
                        body,
                    });
                }
                flat_items.push(match formers.len() {
                    1 => FlatItem::Let(formers.pop().expect("a `struct` item has a member")),
                    _ => FlatItem::Rec(formers),
                });
            }
            // A concept lowers to a representation-public nominal `StructDecl` and its type-former `let` — plus a concept-registry entry (field labels, superclass edges, the parameter telescope) and one method-wrapper `let` per field, synthed into the concept's own namespace.
            // A concept group lowers its formers into one `rec` item as a struct group does; the method wrappers stay `let` items of their own, since each names only its former.
            TopItem::Concept(group) => {
                let mut formers = Vec::with_capacity(group.len());
                for concept in group {
                    let name = curios_core::Global::Authored(context.prefixed(&concept.label));
                    let module = context.prefixed(&concept.label).without_last();

                    let lower = Lowerer::new(context);
                    let param_binders =
                        lower.mint(concept.params.iter().map(|(_, n, _)| n.clone()));
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
                            let ty = lower.bound(&field_scope, || {
                                lower.input_type(&field.desugared_type())
                            })?;
                            field_scope.push(field_binders[i].clone());
                            Ok((field_binders[i].1.clone(), ty))
                        })
                        .collect::<Result<Vec<_>, Error>>()?;

                    let result_sort = lower.term(&concept.result_sort)?;

                    // The record shape drives struct literals, projections, and — through `field_type_from` below — the declared type of every method wrapper.
                    let arity = curios_core::Telescope::build(
                        param_tys_unmarked.clone(),
                        curios_core::Telescope::build(field_tys, ()),
                    );
                    struct_decls.insert(
                        name.clone(),
                        curios_core::StructDecl {
                            universe_context: curios_core::UniverseContext::empty(),
                            arity: arity.clone(),
                            result_sort: result_sort.clone(),
                            module,
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
                                    concept: concept.label.to_string(),
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
                        },
                    );

                    // The type-former, exactly like a representation-public struct's.
                    let struct_type =
                        curios_core::Term::struct_type(name.clone(), param_vars.clone());
                    let (type_, body) = if param_tys.is_empty() {
                        (result_sort, struct_type)
                    } else {
                        (
                            curios_core::Term::func_type_marked(param_tys.clone(), result_sort),
                            curios_core::Term::func_marked(param_tys.clone(), struct_type),
                        )
                    };
                    formers.push(FlatLet {
                        kind: curios_core::DefinitionKind::ConceptType,
                        span: concept.label.span().cloned(),
                        name: curios_core::Global::Authored(context.prefixed(&concept.label)),
                        island: context.island(),
                        type_,
                        body,
                    });

                    // Method wrappers: for each *method* field `f`, pub let C/f(@p₁ : P₁, …, use w : C(p₁, …)) -> F = w.f;
                    //
                    // Built in core rather than as surface AST, because `F` is not the field's *written* type: the record telescope above binds each field's label for the fields after it, so a field type may name the fields before it, and the wrapper has to state it with every such name opened at its own projection off `w`. Restating the written type instead leaves those names bound by nothing — well-formed only while no concept has a dependent field telescope, which is why it survived. Reading it out of the telescope also means the wrapper inherits the record's universe metas by construction, rather than by re-lowering the same spans under a role forced to match.
                    //
                    // Type and body are constructed together so both close over the one `w`, and both index the field positionally. Superclass fields are anonymous and get no wrapper: an instance of the outer concept already yields the inner one by resolution.
                    let param_refs = param_vars.iter().collect::<Vec<_>>();
                    for (index, field) in concept
                        .fields
                        .iter()
                        .enumerate()
                        .filter(|(_, field)| !field.is_super)
                    {
                        // `index` is the field's position in the *whole* telescope, superclass slots included. Counting only the fields that get wrappers would read every method after a superclass one slot early.
                        let witness_id = lower.mint(["w".to_string()]).remove(0).1;
                        let witness =
                            curios_core::Term::var(curios_core::Var::free(witness_id.clone()));

                        let params = param_tys
                            .iter()
                            .map(|(_, binder, type_)| {
                                (Plicity::Implicit, binder.clone(), type_.clone())
                            })
                            .chain(std::iter::once((
                                Plicity::Witness,
                                witness_id,
                                curios_core::Term::struct_type(name.clone(), param_vars.clone()),
                            )))
                            .collect::<Vec<_>>();

                        let field_type = arity
                            .open(&param_refs)
                            .field_type_from(&witness, index)
                            .expect("a concept's own field index is within its record telescope");

                        flat_items.push(FlatItem::Let(FlatLet {
                            span: None,
                            kind: curios_core::DefinitionKind::ConceptMethod {
                                owner: context.prefixed(&concept.label),
                            },
                            name: curios_core::Global::Authored(
                                context.prefixed(&concept.label).with(&field.label),
                            ),
                            island: context.island(),
                            type_: curios_core::Term::func_type_marked(params.clone(), field_type),
                            body: curios_core::Term::func_marked(
                                params,
                                curios_core::Term::proj(witness, index),
                            ),
                        }));
                    }
                }
                flat_items.push(match formers.len() {
                    1 => FlatItem::Let(formers.pop().expect("a `concept` item has a member")),
                    _ => FlatItem::Rec(formers),
                });
            }
            // A witness desugars to an anonymous top-level definition satisfy (tele) -> C(args) = C(args) { f = e, … }; and marks it for registration in the program-wide witness table. It gets an *identity*, not a manufactured name: a `satisfy` block has no name a programmer wrote, and the module a diagnostic reports for it comes from `Definition::island`.
            // A group `satisfy … and …` lowers to one `rec` item, so its members' anonymous names are bound in one another; a lone witness stays a `let` item, and may still resolve through its own entry — that repair is elaboration's, since a witness references itself by resolution rather than by name.
            TopItem::Witness(group) => {
                let mut items = group
                    .iter()
                    .map(|witness| {
                        let name = curios_core::Global::Witness(context.fresh_witness());

                        let concept_app =
                            witness_concept_application(&witness.concept, &witness.args);
                        // A written body is the concept literal; a body-less one is the `Derive` transient, spanned at the concept application so a refusal lands on the declaration. Either way the telescope below wraps it identically.
                        let body: Term = match &witness.body {
                            Some(entries) => Subterm::StructLit(StructLit {
                                head: witness.concept.clone(),
                                params: witness.args.clone(),
                                entries: entries
                                    .iter()
                                    .map(|entry| match entry {
                                        WitnessEntry::Field(field) => {
                                            StructLitEntry::Field(TupleField {
                                                label: Some(field.label.clone()),
                                                func_params: field.func_params.clone(),
                                                value: field.value.clone(),
                                            })
                                        }
                                        WitnessEntry::Use(term) => {
                                            StructLitEntry::Use(term.clone())
                                        }
                                    })
                                    .collect(),
                            })
                            .into(),
                            None => {
                                let derive: Term = Subterm::Derive.into();
                                match witness.args.first().and_then(|arg| arg.span()) {
                                    Some(span) => derive.with_span(span.clone()),
                                    None => derive,
                                }
                            }
                        };

                        // Kept across the move into the signature: under a telescope the declared type is a `FuncType` built here, and a refusal about the *witness* — duplicate, orphan, unkeyable, irregular premise — belongs on the concept application it wraps rather than nowhere.
                        let declared = concept_app.span().cloned();
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

                        // A no-op without a telescope, where `type_()` hands back the already-spanned application: `with_span` keeps the innermost span.
                        let declared_type = match declared {
                            Some(span) => signature.type_().with_span(span),
                            None => signature.type_(),
                        };

                        let lower = Lowerer::new(context);
                        lower.enter_signature(&signature);
                        let item = FlatLet {
                            kind: curios_core::DefinitionKind::Witness,
                            span: None,
                            name: name.clone(),
                            island: context.island(),
                            type_: lower.term(&declared_type)?,
                            body: lower.value(&signature.body())?,
                        };
                        witnesses.insert(name);
                        Ok(item)
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                flat_items.push(match items.len() {
                    1 => FlatItem::Let(items.pop().expect("a `satisfy` item has a member")),
                    _ => FlatItem::Rec(items),
                });
            }
        }
    }

    Ok(())
}

/// What a unit is lowered from: the modules under the prefixes it claims, and — for the one unit that has one — its entrypoint.
///
/// **One resolver, where there were two arms.** The two ways a tree used to arrive here — parsed from a file graph as the entry program is, handed over already parsed as the fixed prelude is — differed in nothing but where a module body came from, which is exactly the question a [`RootSource`] answers. What survives is the one genuine difference: an executable carries a tail expression and owns the empty prefix, and a library does neither.
pub struct UnitSource<'a> {
    entrypoint: Option<&'a Entrypoint>,
    source: &'a RootSource,
}

impl<'a> UnitSource<'a> {
    /// The entry program, its own modules resolved through `source`.
    pub fn entry(entrypoint: &'a Entrypoint, source: &'a RootSource) -> Self {
        Self {
            entrypoint: Some(entrypoint),
            source,
        }
    }

    /// A unit with no entrypoint, under the prefixes `source` claims.
    pub fn mounted(source: &'a RootSource) -> Self {
        Self {
            entrypoint: None,
            source,
        }
    }

    /// The prefixes this source claims.
    ///
    /// The entry claims the empty one and nothing else, and that is stated here rather than read off the resolver: owning the empty prefix is what *makes* a unit the entry, so it cannot be something the way its files were found decided.
    fn mounts(&self) -> Vec<Mount> {
        match self.entrypoint {
            Some(_) => vec![Mount::new(Qualifier::empty(), RootKind::Ordinary)],
            None => self.source.mounts(),
        }
    }

    /// The directories this unit's modules are read from. See [`RootSource::directories`].
    pub fn directories(&self) -> Vec<&std::path::Path> {
        self.source.directories()
    }

    /// Every file this unit has read. See [`RootSource::reads`].
    pub fn reads(&self) -> Vec<(std::path::PathBuf, std::rc::Rc<curios_utilities::Source>)> {
        self.source.reads()
    }

    /// The prefixes this unit claims, which decide how its names are spelled and so which unit it is.
    pub fn claims(&self) -> Vec<Mount> {
        self.mounts()
    }

    /// The prefix this unit claims, as a name to report it by — `/json` for a mounted package.
    ///
    /// The root for the entry, which owns the empty prefix: a caller that wants to *name* the entry knows what was asked for and this does not, so it supplies its own.
    ///
    /// A [`Qualifier`] rather than the text of one, so a reporting caller renders the leading `/` where every other name renders it instead of receiving it already spelled.
    pub fn prefix(&self) -> Qualifier {
        self.mounts()
            .first()
            .map(|mount| mount.prefix.clone())
            .unwrap_or_default()
    }

    /// The mount this unit documents, with its description, when its resolver marked one. An entry documents nothing: a program has no consumer.
    fn documented(&self) -> Option<(Qualifier, Option<String>)> {
        match self.entrypoint {
            Some(_) => None,
            None => self.source.documented_mount(),
        }
    }

    /// The entrypoint this source carries, for the one unit that has one.
    fn entrypoint(&self) -> Option<&Entrypoint> {
        self.entrypoint
    }

    /// The items of the compilation root: the entry's own, and none for a unit with no entrypoint — whose headers sit under its own prefixes, the root belonging to no unit at all.
    fn root_items(&self) -> &'a [TopItem] {
        self.entrypoint
            .map_or(&[], |entrypoint| &entrypoint.module.items)
    }
}

/// Lower one unit against the units already lowered.
///
/// **This is the whole of what used to be three functions.** They differed in where their items sat, whether anything was already in scope, and where four counters started — every one of which is an argument here. `into_core` was the no-scope entry spelling, kept for `curios-text`'s own tests; `prepare_prelude` was the no-scope mounted spelling; `into_core_with_prelude` was the entry spelling with one predecessor. Three copies of one walk agreed by being read, which is the shape every configuration-dependent defect in this stage has had.
///
/// `scope` is in dependency order. Reads span it and the unit's own; writes only ever touch the unit's own, which is what makes a layer sufficient where a copy was used.
pub fn into_core_unit(
    source: &UnitSource<'_>,
    scope: &[&PreparedText],
    syntax: &SyntaxRegistry,
) -> Result<PreparedText, Error> {
    curios_profile::profile!("into_core_unit");
    curios_utilities::grown(|| into_core_unit_within(source, scope, syntax))
}

fn into_core_unit_within(
    source: &UnitSource<'_>,
    scope: &[&PreparedText],
    syntax: &SyntaxRegistry,
) -> Result<PreparedText, Error> {
    let scope_tables = scope.iter().map(|unit| &unit.table).collect::<Vec<_>>();
    let scope_public = scope.iter().map(|unit| &unit.public).collect::<Vec<_>>();
    let scope_cores = scope.iter().map(|unit| &unit.core).collect::<Vec<_>>();
    let scope_mounts = scope
        .iter()
        .flat_map(|unit| unit.mounts.iter().cloned())
        .collect::<Vec<_>>();

    // The prefixes this unit claims. The entry claims the empty one and nothing else; a mounted unit claims what its source does.
    let own = source.mounts();

    // Claimed prefixes must be distinct, and this is decided before discovery — otherwise the collision surfaces from `insert_child` as an ordinary duplicate declaration, which names the label but not what else claimed it.
    //
    // Distinctness *is* disjointness here, because a prefix is one segment: no mount can lie beneath another, and the entry's own `mod json` claims `/json` against a mounted package of that name exactly as a second mount would. The empty prefix takes no part — every name lies within the compilation root by construction, which is what makes it the root.
    //
    // Mount-set disjointness is what `Scoped`'s shadowing rule, the registries' duplicate-key rejection and the `ffi` import namespace all rest on, so it is checked once here rather than assumed three times.
    for (claim, prefix) in claims(source, &own) {
        if let Some(earlier) = scope_mounts
            .iter()
            .find(|earlier| !earlier.prefix.is_root() && earlier.prefix == prefix)
        {
            return Err(Error::MountCollision {
                claim,
                claimed: earlier.prefix.join(),
                claimant: "a unit already in scope".to_string(),
            });
        }
    }

    let Resolved {
        mut table,
        modules,
        mod_spans,
    } = Resolved::of(source, &scope_tables, &scope_mounts, &own)?;

    // Every prefix this compilation mounts — the scope's, then this unit's. Resolution asks the whole set; the lowered module records only `own`, because a module states what its own unit provides.
    let mounts = scope_mounts
        .iter()
        .cloned()
        .chain(own.iter().cloned())
        .collect::<Vec<_>>();

    let public = interface::resolve_unit(
        source,
        &own,
        &modules,
        &mut table,
        &mounts,
        Scoped::over(&scope_public),
    )?;

    // Each counter resumes above every predecessor's, so an identity minted here can alias none already in scope. A floor is a bound: combining by maximum can only widen.
    let floor = |of: fn(&PreparedText) -> usize| scope.iter().copied().map(of).max().unwrap_or(0);
    let metavars = Entropy::<usize>::new();
    metavars.seed(floor(PreparedText::metavariable_floor));
    let universes = Entropy::<usize>::new();
    universes.seed(floor(PreparedText::universe_floor));
    let binders = Entropy::<usize>::new();
    binders.seed(floor(PreparedText::binder_floor));
    // No floor: an ordinal is scoped to its mount now, and this unit's mounts are disjoint from every predecessor's, so nothing it mints can collide with anything already stored.
    let witness_ids = RefCell::new(BTreeMap::new());
    let unbound = RefCell::new(BTreeMap::new());
    let imports = RefCell::new(curios_core::Imports::default());
    let sites = RefCell::new(Vec::new());
    let reached = RefCell::new(BTreeSet::new());
    let lints = RefCell::new(Vec::new());

    let universe_role = Cell::new(curios_core::UniverseRole::Flexible);
    // The scope's seed table. A module carries the *cumulative* table from index zero rather than its own slice — `universe_floor` is asserted equal to its length — so the scope's table is the last unit's, already containing every earlier one. Concatenating them counts each predecessor once per successor, which is what the floor assertion catches.
    let universe_seeds = RefCell::new(
        scope_cores
            .last()
            .map(|core| core.universe_seeds.clone())
            .unwrap_or_default(),
    );
    let universe_allocations = RefCell::new(HashMap::new());

    let mut context = Context::new(
        &table,
        &public,
        &mounts,
        &metavars,
        &universes,
        &universe_role,
        &universe_seeds,
        &universe_allocations,
        &binders,
        &witness_ids,
        &unbound,
        &imports,
        &sites,
        &reached,
        &lints,
        syntax,
    );
    // Every named prefix in the compilation binds its own one-segment name. No two can repeat it: the disjointness check above refuses a unit claiming what the scope already holds, and the scope's own mounts were pairwise disjoint when each was compiled. The entry's prefix is the empty one, which has no name to bind.
    for mount in &mounts {
        if !mount.prefix.is_root() {
            context.insert_scope(mount.prefix.head().to_string(), mount.prefix.clone())?;
        }
    }

    let mut flat_items = Vec::new();
    // This unit's own, never the scope's extended in place. What the scope declares is *scope*, and the one pass here that asks a scope question — the public-exposure audit, whose alias walk may land on a predecessor's type — takes it as a base to query rather than as entries copied into these maps. The dependency sort below never needed it: it looks a declaration up only for names an item itself declares.
    let mut induct_decls = BTreeMap::new();
    let mut struct_decls = BTreeMap::new();
    let mut concepts = BTreeMap::new();
    let mut witnesses = BTreeSet::new();
    let mut tests = Vec::new();
    let mut foreigns = ForeignStore::new();

    // The compilation root's own items — the entry's, and none for a unit with no entrypoint — then one pass per prefix this unit claims. Exactly one of the two does any work, because owning the empty prefix is what makes a unit the entry.
    process_items(
        source.root_items(),
        &mut context,
        &mut flat_items,
        &mut induct_decls,
        &mut struct_decls,
        &mut concepts,
        &mut witnesses,
        &mut tests,
        &mut foreigns,
        &modules,
    )?;

    for mount in own.iter().filter(|mount| !mount.prefix.is_root()) {
        let content = modules
            .get(&mount.prefix)
            .expect("a mounted prefix was loaded during discovery");

        let mut nested = context.nested(mount.prefix.head());

        process_items(
            &content.items,
            &mut nested,
            &mut flat_items,
            &mut induct_decls,
            &mut struct_decls,
            &mut concepts,
            &mut witnesses,
            &mut tests,
            &mut foreigns,
            &modules,
        )?;
    }

    // The entrypoint, for the one unit that has one. Its tail closes the root body, so the imports in scope there are the last the root saw.
    context.record_import_scope(None);
    let entry = {
        // Scoped so the lowerer drops, and reports its lints, before the tables the context borrows are moved into the result below.
        let lower = Lowerer::new(&context);
        match source.entrypoint() {
            Some(entrypoint) => Some(curios_core::Entrypoint {
                body: lower.value(&entrypoint.tail)?,
                type_: entrypoint
                    .type_
                    .as_ref()
                    .map(|type_| lower.term(type_))
                    .transpose()?,
            }),
            None => None,
        }
    };

    audit_public_exposures(
        &public,
        &table,
        &flat_items,
        NominalScope::new(&scope_cores, &induct_decls, &struct_decls),
    )?;

    let dead = unused_declarations(&Declarations {
        items: &flat_items,
        entry: entry.as_ref(),
        table: &table,
        public: &public,
        own: &own,
        mod_spans: &mod_spans,
        induct_decls: &induct_decls,
        struct_decls: &struct_decls,
        syntax,
    });

    // This unit's own items alone. A predecessor reaches later stages as an *environment* they are seeded from — `Globals` at the certifier, a replayed context at elaboration and erasure — and copying its items into every compilation only ever existed so those stages could then skip them again by index. See `documentation/design/toolchain/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md`.
    let items = order_flat_items(flat_items, &mounts, &induct_decls, &struct_decls, syntax)?
        .into_iter()
        .map(FlatItem::into_core)
        .collect();

    // Read last, when the export view is final and every definition's import scope has been recorded, and before the tables below are taken out of their scoped views.
    let documentation = source.documented().map(|(prefix, description)| {
        document(
            &modules,
            &table,
            &public,
            &imports.borrow(),
            &prefix,
            description,
        )
    });

    Ok(PreparedText {
        mounts: own.clone(),
        foreigns,
        table: table.into_own().into_iter().collect(),
        public: public.into_own().into_iter().collect(),
        core: curios_core::Module {
            items,
            mounts: own,
            universe_seeds: universe_seeds.into_inner(),
            induct_decls,
            struct_decls,
            concepts,
            witnesses,
            tests,
            binder_floor: binders.count(),
            entry,
        },
        metavariable_floor: metavars.count(),
        binder_floor: binders.count(),
        universe_floor: universes.count(),
        unbound: unbound.into_inner(),
        imports: imports.into_inner(),
        lints: ordered(
            unused_imports(sites.into_inner())
                .into_iter()
                .chain(lints.into_inner())
                .chain(dead)
                .collect(),
        ),
        reached: reached.into_inner(),
        documentation,
    })
}

/// Lower a whole [`Entrypoint`] with nothing in scope, as `curios-text`'s own stage tests do.
pub fn into_core(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    syntax: &SyntaxRegistry,
) -> Result<(curios_core::Module, usize, usize, ForeignStore), Error> {
    let unit = into_core_unit(&UnitSource::entry(entrypoint, loader), &[], syntax)?;

    Ok((
        unit.core,
        unit.metavariable_floor,
        unit.universe_floor,
        unit.foreigns,
    ))
}

/// Resolve and lower the fixed roots once for build-time archival.
pub fn prepare_prelude(input: &RootSource, syntax: &SyntaxRegistry) -> Result<PreparedText, Error> {
    into_core_unit(&UnitSource::mounted(input), &[], syntax)
}

/// The entry program lowered: its module, the floors elaboration's counters start above, its `foreign` rows, the unresolved-name table its `unbound variable` reports read from, and its lints.
pub struct LoweredEntry {
    pub core: curios_core::Module,
    pub metavariable_floor: usize,
    pub universe_floor: usize,
    pub foreigns: ForeignStore,
    /// See [`PreparedText::unbound`].
    pub unbound: BTreeMap<curios_core::Free, Vec<Qualifier>>,
    /// See [`PreparedText::imports`].
    pub imports: curios_core::Imports,
    /// See [`PreparedText::lints`].
    pub lints: Vec<Lint>,
    /// See [`PreparedText::reached`].
    pub reached: BTreeSet<Qualifier>,
}

/// The `unused-import` lints: every site no reference resolved through, a re-export excepted.
fn unused_imports(sites: Vec<UseSite>) -> Vec<Lint> {
    sites
        .into_iter()
        .filter(|site| !site.used && !site.exported)
        .map(|site| match &site.what {
            UseSiteKind::Selector(label) => Lint::unused_import(label),
            UseSiteKind::Glob(path) => Lint::unused_glob(site.span.as_ref(), path),
        })
        .collect()
}

/// Lower the entry program against the units already lowered.
pub fn into_core_with_prelude(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    scope: &[&PreparedText],
    syntax: &SyntaxRegistry,
) -> Result<LoweredEntry, Error> {
    let unit = into_core_unit(&UnitSource::entry(entrypoint, loader), scope, syntax)?;

    Ok(LoweredEntry {
        core: unit.core,
        metavariable_floor: unit.metavariable_floor,
        universe_floor: unit.universe_floor,
        foreigns: unit.foreigns,
        unbound: unit.unbound,
        imports: unit.imports,
        lints: unit.lints,
        reached: unit.reached,
    })
}
