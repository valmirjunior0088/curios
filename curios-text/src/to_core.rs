mod context;
use context::*;

mod lower;
use lower::*;

mod interface;

use {
    super::*,
    curios_base::Entropy,
    curios_core::Bound,
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        rc::Rc,
    },
};

// Root modules reachable only from the standard library — the trusted primitive
// substrate (`sys`). User code reaches them through their `/std` wrappers; any
// reference that resolves into one from outside is rejected during resolution.
// `syn` (the desugar-target library) is deliberately NOT internal: string-literal
// desugaring emits references to `/syn/Str`, so the name must be reachable from any
// module the way an ordinary library is.
const INTERNAL_ROOTS: &[&str] = &["sys"];
// Consuming roots permitted to reference an internal root: the standard library, the
// internal roots themselves, and `syn` (which reaches `/sys` prims via `/std` re-exports).
const PRIVILEGED_ROOTS: &[&str] = &["std", "sys", "syn"];

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
            TopItem::Induct(group) => {
                for u in group {
                    info.insert_child(u.label.clone(), u.is_pub)?;
                    info.insert_binding(u.label.clone(), u.is_pub)?;
                }
            }
            // A struct declares one binding (the type-former), like a `let` —
            // there are no value constructors and no nested namespace, so no
            // child module.
            TopItem::Struct(s) => info.insert_binding(s.label.clone(), s.is_pub)?,
            // A concept declares the type-former binding *and* a nested namespace
            // (its method wrappers), like an inductive.
            TopItem::Concept(c) => {
                info.insert_child(c.label.clone(), c.is_pub)?;
                info.insert_binding(c.label.clone(), c.is_pub)?;
            }
            // A witness declares one binding (its backing definition), like a `let`.
            TopItem::Witness(w) => info.insert_binding(w.label.clone(), w.is_pub)?,
            _ => {}
        }
    }

    Ok(info)
}

// The surface concept application `C(p₁, …)` for a method wrapper's `use w`
// binder: the concept name applied to its parameters, each carrying the
// parameter's declared plicity so the application matches the type-former.
fn concept_application(label: &str, params: &[ConceptParam]) -> Term {
    let head: Term = Subterm::Name(Name::from(vec![label.to_string()])).into();
    if params.is_empty() {
        return head;
    }

    Subterm::Apply(Apply {
        head,
        params: params
            .iter()
            .map(|param| {
                (
                    param.plicity,
                    Subterm::Name(Name::from(vec![param.label.clone()])).into(),
                )
            })
            .collect(),
    })
    .into()
}

// The surface concept application `C(args)` for a witness's declared type: the
// witnessed concept applied to the annotation's arguments (as written, so
// explicit).
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

// The head name of a concept-application term (a path, optionally applied) —
// used to read the super concept off a `use`-marked field's type. `None` if the
// type is not shaped like a concept application.
fn concept_app_head(term: &Term) -> Option<Name> {
    match term.as_subterm() {
        Subterm::Name(name) => Some(name.clone()),
        Subterm::Apply(apply) => concept_app_head(&apply.head),
        _ => None,
    }
}

// Resolve a super concept's head to its qualified core name — the same rule
// `Lower`'s term-reference arm uses, minus the local-binder shadowing (a
// declaration-site super edge has no enclosing value scope).
fn resolve_concept_head(context: &Context, name: &Name) -> Result<String, Error> {
    if name.is_abs() || !name.is_single() {
        Ok(context.resolve_term_name(name)?.join())
    } else {
        match context.bindings().get(name.head()) {
            Some(qualifier) => Ok(qualifier.join()),
            None => Ok(name.head().to_string()),
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn process_items(
    top_items: &[TopItem],
    context: &mut Context,
    flat_items: &mut Vec<FlatItem>,
    inductives: &mut BTreeMap<String, curios_core::Inductive>,
    structures: &mut BTreeMap<String, curios_core::Structure>,
    concepts: &mut BTreeMap<String, curios_core::Concept>,
    witnesses: &mut BTreeSet<String>,
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
            // The type-former binding only — like a `let` (no constructor
            // namespace).
            TopItem::Struct(s) => {
                context.insert_binding(s.label.clone(), context.prefixed(&s.label))?
            }
            // A concept declares its type-former binding and a nested namespace
            // for the method wrappers, like an inductive.
            TopItem::Concept(c) => {
                context.insert_scope(c.label.clone(), context.prefixed(&c.label))?;
                context.insert_binding(c.label.clone(), context.prefixed(&c.label))?;
            }
            // A witness declares its backing definition's binding, like a `let`.
            TopItem::Witness(w) => {
                context.insert_binding(w.label.clone(), context.prefixed(&w.label))?
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
                        concepts,
                        witnesses,
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
                        concepts,
                        witnesses,
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
            TopItem::Induct(group) => {
                // Step 1: type bindings as one rec group. An inductive's type
                // binding wraps a primitive `InductiveType` normal form in a
                // `Func` over its type parameters and indices (so
                // `Result(Nat, Bin)` beta-reduces to `InductiveType { Result,
                // [Nat, Bin] }` and `Vec(Bin, 3)` to `InductiveType { Vec, [Bin],
                // [3] }`), and its shape is recorded in the inductive
                // registry.
                let type_flat_items = group
                    .iter()
                    .map(|u| {
                        let lower = Lower::new(context);
                        let name = context.prefixed(&u.label).join();

                        let param_tys = u
                            .params
                            .iter()
                            .map(|(p, n, t)| Ok((*p, n.clone(), lower.term(t)?)))
                            .collect::<Result<Vec<_>, Error>>()?;
                        // The registry and the `InductiveType` normal form are
                        // positional; plicity matters only on the generated
                        // type-constructor function.
                        let param_tys_unmarked = param_tys
                            .iter()
                            .map(|(_, n, t)| (n.clone(), t.clone()))
                            .collect::<Vec<_>>();

                        let param_vars = u
                            .params
                            .iter()
                            .map(|(_, n, _)| curios_core::Term::var(curios_core::Var::free(n)))
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
                            .map(|(n, _)| curios_core::Term::var(curios_core::Var::free(n)))
                            .collect::<Vec<_>>();

                        // Registry entry: the parameter telescope plus each
                        // constructor's full signature `(params..., payload...)
                        // -> InductiveType { name, params, indices }`, where the
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
                                    .map(|(i, param)| {
                                        let n =
                                            param.label.clone().unwrap_or_else(|| format!("_{i}"));
                                        Ok((n, lower.term(&param.type_)?))
                                    })
                                    .collect::<Result<Vec<_>, Error>>()?;

                                let target = c
                                    .target
                                    .iter()
                                    .flatten()
                                    .map(|t| lower.term(t))
                                    .collect::<Result<Vec<_>, Error>>()?;

                                let telescope = curios_core::Telescope::build(
                                    param_tys_unmarked.iter().cloned().chain(fields),
                                    curios_core::Term::inductive_type(
                                        &name,
                                        param_vars.clone(),
                                        target,
                                    ),
                                );

                                Ok((
                                    curios_core::Atom::from(c.label.as_str()),
                                    curios_core::InductiveParam { telescope },
                                ))
                            })
                            .collect::<Result<BTreeMap<_, _>, Error>>()?;

                        // The declared result sort (`Type`/`Prop`) — closed, so
                        // it lowers in the base context. It is both the registry
                        // entry's sort and the type-constructor's codomain.
                        let result_sort = lower.term(&u.result_sort)?;

                        inductives.insert(
                            name.clone(),
                            curios_core::Inductive {
                                params: curios_core::Telescope::build(
                                    param_tys_unmarked.clone(),
                                    (),
                                ),
                                indices: curios_core::Telescope::build(
                                    param_tys_unmarked
                                        .iter()
                                        .cloned()
                                        .chain(index_tys.iter().cloned()),
                                    (),
                                ),
                                constructors,
                                result_sort: result_sort.clone(),
                            },
                        );

                        let inductive =
                            curios_core::Term::inductive_type(&name, param_vars, index_vars);

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
                                    .map(|(n, t)| (curios_core::Plicity::Explicit, n, t)),
                            )
                            .collect();
                        let (type_, body) = if binder_tys.is_empty() {
                            (result_sort, inductive)
                        } else {
                            (
                                curios_core::Term::func_type_marked(
                                    binder_tys.clone(),
                                    result_sort,
                                ),
                                curios_core::Term::func(
                                    binder_tys.into_iter().map(|(_, n, t)| (n, t)),
                                    inductive,
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
                for u in group {
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
                        let output_args: Vec<(curios_core::Plicity, Term)> = u
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
                                    .map(|t| (curios_core::Plicity::Explicit, t.clone())),
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
                        // Every inductive parameter is implicit at the value
                        // constructor — `Result/success(42)` infers them, the
                        // call-site `@` supplies one positionally — while the
                        // payload binders keep their declared marks (`@m`
                        // makes one implicit; the default is explicit).
                        let param_tys = u
                            .params
                            .iter()
                            .map(|(_, n, t)| {
                                Ok((curios_core::Plicity::Implicit, n.clone(), lower.term(t)?))
                            })
                            .chain(c.payload.iter().enumerate().map(|(i, param)| {
                                Ok((
                                    param.plicity,
                                    payload_name(i, &param.label),
                                    lower.term(&param.type_)?,
                                ))
                            }))
                            .collect::<Result<Vec<_>, Error>>()?;
                        // Erasure is sort-driven: `erase_func` drops the same
                        // proof/type payload params that `erase_variant` drops
                        // from the tuple — the constructor function's arity and its
                        // injected variant's arity stay in lockstep.
                        let ctor_type = curios_core::Term::func_type_marked(
                            param_tys.clone(),
                            lower.term(&output_type)?,
                        );

                        // Constructor body: (params..., _0, ...) => the variant's
                        // injection, a primitive `Variant` normal form.
                        let args: Vec<curios_core::Term> = c
                            .payload
                            .iter()
                            .enumerate()
                            .map(|(i, param)| {
                                curios_core::Term::var(curios_core::Var::free(payload_name(
                                    i,
                                    &param.label,
                                )))
                            })
                            .collect();
                        let inject = curios_core::Term::variant(
                            context.prefixed(&u.label).join(),
                            u.params
                                .iter()
                                .map(|(_, n, _)| curios_core::Term::var(curios_core::Var::free(n))),
                            curios_core::Atom::from(c.label.as_str()),
                            args,
                        );
                        // The lambda binds every parameter regardless of mark.
                        let ctor_body = curios_core::Term::func(
                            param_tys.into_iter().map(|(_, n, t)| (n, t)),
                            inject,
                        );

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
                    .map(|(_, n, _)| curios_core::Term::var(curios_core::Var::free(n)))
                    .collect::<Vec<_>>();

                // Field types, with declared or positional (`_i`) names so a
                // later field type can depend on an earlier field.
                let field_tys = s
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, param)| {
                        let n = param.label.clone().unwrap_or_else(|| format!("_{i}"));
                        Ok((n, lower.term(&param.type_)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                // Registry entry: the parameter telescope, and the full field
                // telescope (parameter binders first — field types may mention
                // them — then field binders), as in `Inductive::indices`.
                // The declared result sort (`Type`/`Prop`) — closed; both the
                // registry entry's sort and the type-former's codomain.
                let result_sort = lower.term(&s.result_sort)?;

                structures.insert(
                    name.clone(),
                    curios_core::Structure {
                        params: curios_core::Telescope::build(param_tys_unmarked.clone(), ()),
                        fields: curios_core::Telescope::build(
                            param_tys_unmarked.iter().cloned().chain(field_tys),
                            (),
                        ),
                        result_sort: result_sort.clone(),
                        module,
                        rep_public: s.rep_pub,
                    },
                );

                // The type-former: `Pair : (A : Type, B : Type) -> Type` whose
                // body is the `StructType` normal form (the bare node when
                // parameterless), so `Pair(Nat, Bin)` reduces to
                // `StructType { Pair, [Nat, Bin] }`. No value constructor.
                let struct_type = curios_core::Term::struct_type(&name, param_vars);
                let (type_, body) = if param_tys.is_empty() {
                    (result_sort, struct_type)
                } else {
                    (
                        curios_core::Term::func_type_marked(param_tys.clone(), result_sort),
                        curios_core::Term::func(
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
            // A concept lowers to exactly what a `record` lowers to — a nominal
            // `Structure` with `rep_public = true` and its type-former `let` —
            // plus a concept-registry entry (field labels, superclass edges, the
            // parameter telescope) and one method-wrapper `let` per field, synthed
            // into the concept's own namespace (§4.1).
            TopItem::Concept(concept) => {
                let name = context.prefixed(&concept.label).join();
                let module = match name.rfind('/') {
                    Some(slash) => name[..slash].to_string(),
                    None => String::new(),
                };

                let param_tys = {
                    let lower = Lower::new(context);
                    concept
                        .params
                        .iter()
                        .map(|p| Ok((p.plicity, p.label.clone(), lower.term(&p.type_)?)))
                        .collect::<Result<Vec<_>, Error>>()?
                };
                let param_tys_unmarked = param_tys
                    .iter()
                    .map(|(_, n, t)| (n.clone(), t.clone()))
                    .collect::<Vec<_>>();
                let param_vars = concept
                    .params
                    .iter()
                    .map(|p| curios_core::Term::var(curios_core::Var::free(&p.label)))
                    .collect::<Vec<_>>();

                // The input mask: every position not marked `out` keys the
                // witness table. Marking every parameter `out` leaves nothing
                // to key on — rejected here, where the marker lives.
                let inputs = concept
                    .params
                    .iter()
                    .enumerate()
                    .filter(|(_, p)| !p.is_out)
                    .map(|(position, _)| position)
                    .collect::<Vec<_>>();
                if !concept.params.is_empty() && inputs.is_empty() {
                    return Err(Error::ConceptWithoutInputs {
                        label: concept.label.clone(),
                    });
                }

                // Field types, lowered under the parameter scope (concept fields
                // are always named, so the label is the binder for later fields).
                let field_tys = {
                    let lower = Lower::new(context);
                    concept
                        .fields
                        .iter()
                        .map(|field| Ok((field.label.clone(), lower.term(&field.type_)?)))
                        .collect::<Result<Vec<_>, Error>>()?
                };

                let result_sort = {
                    let lower = Lower::new(context);
                    lower.term(&concept.result_sort)?
                };

                // The record shape drives struct literals and projections.
                structures.insert(
                    name.clone(),
                    curios_core::Structure {
                        params: curios_core::Telescope::build(param_tys_unmarked.clone(), ()),
                        fields: curios_core::Telescope::build(
                            param_tys_unmarked.iter().cloned().chain(field_tys),
                            (),
                        ),
                        result_sort: result_sort.clone(),
                        module,
                        rep_public: true,
                    },
                );

                // Superclass edges: each `use`-marked field names a super concept
                // by its (resolved, qualified) head.
                let supers = concept
                    .fields
                    .iter()
                    .enumerate()
                    .filter(|(_, field)| field.is_super)
                    .map(|(idx, field)| {
                        let head = concept_app_head(&field.type_).ok_or_else(|| {
                            Error::MalformedSuperField {
                                label: field.label.clone(),
                            }
                        })?;
                        Ok((idx, resolve_concept_head(context, &head)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                concepts.insert(
                    name.clone(),
                    curios_core::Concept {
                        params: curios_core::Telescope::build(param_tys_unmarked.clone(), ()),
                        fields: concept.fields.iter().map(|f| f.label.clone()).collect(),
                        supers,
                        inputs,
                    },
                );

                // The type-former, exactly like a `record`'s.
                let struct_type = curios_core::Term::struct_type(&name, param_vars);
                let (type_, body) = if param_tys.is_empty() {
                    (result_sort, struct_type)
                } else {
                    (
                        curios_core::Term::func_type_marked(param_tys.clone(), result_sort),
                        curios_core::Term::func(
                            param_tys.iter().cloned().map(|(_, n, t)| (n, t)),
                            struct_type,
                        ),
                    )
                };
                flat_items.push(FlatItem::Let(FlatLet {
                    name: context.prefixed(&concept.label),
                    type_,
                    body,
                }));

                // Method wrappers: for each field `f : F` at index `idx`,
                //   pub let C/f(@p₁ : P₁, …, use w : C(p₁, …)) -> F = w.f;
                // Built as surface AST and lowered through `Lower`, so binder
                // scoping and de-Bruijn capture are handled uniformly.
                let concept_app = concept_application(&concept.label, &concept.params);
                for field in &concept.fields {
                    let mut params = concept
                        .params
                        .iter()
                        .map(|p| FuncSugarParam {
                            plicity: Plicity::Implicit,
                            label: p.label.clone(),
                            type_: p.type_.clone(),
                        })
                        .collect::<Vec<_>>();
                    params.push(FuncSugarParam {
                        plicity: Plicity::Witness,
                        label: "w".to_string(),
                        type_: concept_app.clone(),
                    });

                    let signature = LetSignature::Func {
                        params,
                        output: field.type_.clone(),
                        body: Subterm::Proj(Proj {
                            head: Subterm::Name(Name::from(vec!["w".to_string()])).into(),
                            field: Field::Label(field.label.clone()),
                        })
                        .into(),
                    };

                    let lower = Lower::new(context);
                    flat_items.push(FlatItem::Let(FlatLet {
                        name: context.prefixed(&concept.label).with(&field.label),
                        type_: lower.term(&signature.type_())?,
                        body: lower.term(&signature.body())?,
                    }));
                }
            }
            // A witness desugars to an ordinary definition
            //   let w(tele) -> C(args) = C(args) { f = e, … };
            // and marks `w` for registration in the program-wide witness table.
            TopItem::Witness(witness) => {
                let concept_app = witness_concept_application(&witness.concept, &witness.args);
                let body: Term = Subterm::StructLit(StructLit {
                    head: witness.concept.clone(),
                    params: witness.args.clone(),
                    fields: witness
                        .fields
                        .iter()
                        .map(|(label, value)| (Some(label.clone()), value.clone()))
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

                let lower = Lower::new(context);
                flat_items.push(FlatItem::Let(FlatLet {
                    name: context.prefixed(&witness.label),
                    type_: lower.term(&signature.type_())?,
                    body: lower.term(&signature.body())?,
                }));
                witnesses.insert(context.prefixed(&witness.label).join());
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
// The roots of the embedded, fixed prelude (the `SysLoader`/`SynLoader`/
// `StdLoader` that `text::prelude` wraps every loader with). An item under one
// of them is part of the program-independent prelude prefix, whose topological
// order is cached. This is deliberately *not* the full set of loader roots a
// custom loader may add — only the fixed embedded ones, so the cache stays
// valid across programs.
const PRELUDE_ROOTS: [&str; 3] = ["std", "syn", "sys"];

thread_local! {
    // The fixed prelude's topological order, as a *relative permutation* of its
    // declaration order: emit position `j` of the prelude is the prelude item at
    // relative declaration index `permutation[j]`. Program-independent — the
    // embedded prelude is fixed and always lowered in the same relative order —
    // so the dep-graph build + O(N²) topo-sort happen once, and every compile
    // just indexes through it (no `free_vars`, no name hashing, no sort): the
    // `order_flat_items` hot path was ~⅔ of `to_core` (samply). A
    // `RefCell<Option<_>>`, not `OnceCell`: a `to_core` call without a prelude (a
    // bare-loader test) must not poison the cache, and a prelude of a different
    // size — the only way a custom loader could change the order — refreshes it.
    static PRELUDE_PERMUTATION: RefCell<Option<Vec<usize>>> = const { RefCell::new(None) };
}

/// Whether a flat item belongs to the fixed embedded prelude. Checked on the
/// *structured* qualifier's root segment, before names are flattened to strings.
fn flat_item_in_prelude(item: &FlatItem) -> bool {
    let lets = match item {
        FlatItem::Let(let_) => std::slice::from_ref(let_),
        FlatItem::Rec(lets) => lets.as_slice(),
    };

    !lets.is_empty()
        && lets.iter().all(|let_| {
            let_.name
                .segments()
                .first()
                .is_some_and(|root| PRELUDE_ROOTS.contains(&root.as_str()))
        })
}

/// The nodes a node depends on: those declaring its free vars (and, for a
/// declared inductive/struct, its registry entry's free vars — see the note
/// inline). Self-edges and names `owner` does not map (primitives, or items in
/// the other partition, when `owner` is restricted to one) drop out.
fn dep_nodes(
    node: usize,
    item: &FlatItem,
    declared: &[String],
    inductives: &BTreeMap<String, curios_core::Inductive>,
    structures: &BTreeMap<String, curios_core::Structure>,
    owner: &HashMap<String, usize>,
) -> HashSet<usize> {
    // An inductive's declaration is wider than its items: the registry entry's
    // constructor payload and target types are elaborated alongside the
    // type-binding group (`curios_core::elaborate_module_rec` rebuilds the registry
    // telescopes there), so a node declaring a registered name references
    // everything its registry entry does — those names live nowhere in the type
    // binding's own `type_`/`body`. Struct field types live in the registry too.
    let mut names = flat_item_free_vars(item);
    for name in declared {
        if let Some(inductive) = inductives.get(name) {
            names.extend(inductive_free_vars(inductive));
        }
        if let Some(structure) = structures.get(name) {
            names.extend(structure_free_vars(structure));
        }
    }

    names
        .iter()
        .filter_map(|name| owner.get(name).copied())
        .filter(|&dep| dep != node)
        .collect()
}

/// Owner index (declared name → node) over the given nodes only.
fn owner_of(items: &[FlatItem], nodes: &[usize]) -> HashMap<String, usize> {
    nodes
        .iter()
        .flat_map(|&n| {
            flat_item_names(&items[n])
                .into_iter()
                .map(move |name| (name, n))
        })
        .collect()
}

/// Topologically order `nodes` (assumed ascending, for the lowest-index
/// tiebreak) under `deps` restricted to that set: lowest-index node whose deps
/// are all emitted; on a cycle, the lowest remaining one breaks the deadlock.
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

/// The prelude's topological order as positions *relative to* `prelude_nodes`
/// (ascending), so it can be replayed against a later compile's prelude block
/// wherever it lands. Run once, behind the [`PRELUDE_PERMUTATION`] cache.
fn prelude_permutation(
    items: &[FlatItem],
    prelude_nodes: &[usize],
    inductives: &BTreeMap<String, curios_core::Inductive>,
    structures: &BTreeMap<String, curios_core::Structure>,
) -> Vec<usize> {
    let owner = owner_of(items, prelude_nodes);
    let deps = prelude_nodes
        .iter()
        .map(|&n| {
            let declared = flat_item_names(&items[n]);
            (
                n,
                dep_nodes(n, &items[n], &declared, inductives, structures, &owner),
            )
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
    inductives: &BTreeMap<String, curios_core::Inductive>,
    structures: &BTreeMap<String, curios_core::Structure>,
) -> Vec<FlatItem> {
    let count = items.len();

    let is_prelude = items
        .iter()
        .map(flat_item_in_prelude)
        .collect::<Vec<bool>>();
    let prelude_nodes = (0..count)
        .filter(|&i| is_prelude[i])
        .collect::<Vec<usize>>();
    let rest = (0..count)
        .filter(|&i| !is_prelude[i])
        .collect::<Vec<usize>>();

    let mut order = Vec::with_capacity(count);

    // Prelude prefix: replay the cached relative permutation — pure indexing,
    // no name handling. Prelude items depend only on each other, so emitting the
    // whole block (in that order) ahead of everything else is always valid.
    PRELUDE_PERMUTATION.with(|cell| {
        let mut slot = cell.borrow_mut();
        if slot
            .as_ref()
            .is_none_or(|perm| perm.len() != prelude_nodes.len())
            && !prelude_nodes.is_empty()
        {
            *slot = Some(prelude_permutation(
                &items,
                &prelude_nodes,
                inductives,
                structures,
            ));
        }
        if let Some(perm) = slot.as_ref() {
            order.extend(perm.iter().map(|&rel| prelude_nodes[rel]));
        }
    });

    // Everything else (user code, plus any non-prelude library a custom loader
    // serves): topologically ordered among itself, after the whole prelude. Its
    // dependencies on prelude items are already satisfied by the prefix above,
    // so the owner map (and thus the dep edges) need only cover `rest`.
    let rest_owner = owner_of(&items, &rest);
    let rest_deps = rest
        .iter()
        .map(|&n| {
            let declared = flat_item_names(&items[n]);
            (
                n,
                dep_nodes(n, &items[n], &declared, inductives, structures, &rest_owner),
            )
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
            // Construction head names (`Struct`/`Variant`/type-former normal forms)
            // are reachability edges too — a body that *builds* a struct (the
            // string-literal meta-emitter's `/syn/Str/Str`) must keep its backing
            // type-former and field-type definitions alive even though no `Var`
            // names them. See `Subterm::construction_names`.
            let_.type_
                .free_vars()
                .into_iter()
                .chain(let_.body.free_vars())
                .chain(let_.type_.construction_names())
                .chain(let_.body.construction_names())
        })
        .collect()
}

/// The external references of an inductive registry entry: every free var of
/// its telescopes. Binder names (parameters, payload binders) are captured by
/// `Telescope::build` and never appear here; the index types' references also
/// live in the type binding's own signature, but are included for robustness.
fn inductive_free_vars(inductive: &curios_core::Inductive) -> HashSet<String> {
    inductive
        .params
        .free_vars()
        .into_iter()
        .chain(inductive.indices.free_vars())
        .chain(
            inductive
                .constructors
                .values()
                .flat_map(|param| param.telescope.free_vars()),
        )
        .collect()
}

/// The external references of a struct registry entry: every free var of its
/// parameter and field telescopes. Like `inductive_free_vars`, this is what
/// makes a struct's type-former node depend on the (e.g. primitive) types its
/// fields mention — they live nowhere in the type-former's own body, which is
/// just the `StructType` normal form.
fn structure_free_vars(structure: &curios_core::Structure) -> HashSet<String> {
    structure
        .params
        .free_vars()
        .into_iter()
        .chain(structure.fields.free_vars())
        .collect()
}

fn flat_let_to_core(let_: FlatLet) -> curios_core::Definition {
    curios_core::Definition {
        name: let_.name.join(),
        type_: let_.type_,
        body: let_.body,
    }
}

fn flat_item_to_core(item: FlatItem) -> curios_core::Item {
    match item {
        FlatItem::Let(let_) => curios_core::Item::Let(flat_let_to_core(let_)),
        FlatItem::Rec(items) => {
            curios_core::Item::Rec(items.into_iter().map(flat_let_to_core).collect())
        }
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

/// Lower an [`Entrypoint`] to a [`curios_core::Module`]. Also returns how many
/// metavariable ids were minted for the module's holes: the floor
/// `elaborate_module` needs so the ids it mints for implicit-argument
/// insertion never collide with these.
pub fn to_core(
    entrypoint: &Entrypoint,
    loader: &dyn Loader,
) -> Result<(curios_core::Module, usize), Error> {
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

    let mut context = Context::new(&table, &public, &metavars, &binders);
    let mut flat_items = Vec::new();
    let mut inductives = BTreeMap::new();
    let mut structures = BTreeMap::new();
    // Concept resolution metadata and witness registration markers, populated as
    // `concept`/`witness` items lower.
    let mut concepts = BTreeMap::new();
    let mut witnesses = BTreeSet::new();

    process_items(
        &entrypoint.module.items,
        &mut context,
        &mut flat_items,
        &mut inductives,
        &mut structures,
        &mut concepts,
        &mut witnesses,
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
    let items = order_flat_items(flat_items, &inductives, &structures)
        .into_iter()
        .map(flat_item_to_core)
        .collect();

    Ok((
        curios_core::Module {
            items,
            inductives,
            structures,
            concepts,
            witnesses,
            type_,
            body: tail,
        },
        metavars.count(),
    ))
}

#[cfg(test)]
mod tests;
