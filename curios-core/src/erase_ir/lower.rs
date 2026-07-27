//! The lowering driver: the entry point, the expression walk, and the binding
//! forms.
//!
//! The walk mirrors the legacy recursive erasure's control structure (its
//! stack behavior is the no-regression baseline) but produces operands under
//! the operand law instead of terms: [`Outcome::Emitted`] carries the atom a
//! subexpression erased to, [`Outcome::Diverged`] carries the terminator that
//! seals the innermost block when the subexpression provably never yields a
//! value. Every non-atomic computation is bound by the builder at the point
//! the walk reaches it, so evaluation order is statement order by
//! construction.

use {
    super::{
        Binding, Bound, Context, Environment, Error, InductDecl, Item, Let, Module, Subterm, Term,
        emitted, prim,
    },
    crate::{
        Concept, Definition, Free, Global, InductParam, RecItem, StructDecl,
        project_erased_universes, validate_bound_universes, validate_universes,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// What one expression erased to. See the module documentation.
#[derive(Debug)]
pub(super) enum Outcome {
    Emitted(curios_ersd::Atom),
    Diverged(curios_ersd::Terminator),
}

/// The erasure state: the checked builder constructing the module and the
/// environment mapping Core names to their operands.
#[derive(Default)]
pub(super) struct Lowering {
    pub(super) builder: curios_ersd::ErsdBuilder,
    pub(super) environment: Environment,
    /// Dropped binder labels referenced from a retained position. Consumed by
    /// the function-body collapse: a proof-valued body that dangles a binder
    /// its own lambda dropped is replaced by the unit constant.
    pub(super) dangled: BTreeSet<Free>,
}

pub(super) struct UniverseErased<T>(T);

impl<T> UniverseErased<T> {
    pub(super) fn into_inner(self) -> T {
        self.0
    }
}

impl UniverseErased<Term> {
    fn project(term: &Term) -> Result<Self, Error> {
        validate_bound_universes(term, 0, "erasure expected type")?;
        Ok(Self(project_erased_universes(term)))
    }
}

impl UniverseErased<Module> {
    pub(super) fn project(module: &Module) -> Result<Self, Error> {
        validate_universes(module)?;
        Ok(Self(project_module(module)))
    }

    /// Project a module whose universes were already validated by the boundary
    /// that produced it, skipping the check rather than repeating it.
    ///
    /// The archived prelude is the case this exists for: `curios-prelude`
    /// validates it as it restores, which is the point where untrusted bytes
    /// become a `Module`, and the value is immutable from then on. Re-validating
    /// at every use walked the whole standard library a second time per
    /// compilation — inside the erasure context's reduction deadline, at that —
    /// to re-derive an answer the restore already had.
    pub(super) fn project_validated(module: &Module) -> Self {
        Self(project_module(module))
    }

    /// Project a module that extends an already-projected `prefix`, validating
    /// and projecting only what it adds.
    ///
    /// `elaborate_module_with_prelude` returns the prelude's declarations
    /// concatenated with the user's, so the merged module's leading items *are*
    /// the prelude's — cloned, not re-elaborated. Projecting it whole therefore
    /// walked the entire standard library a second time to produce terms
    /// [`Lowering::erase_items`] then skips. Splicing the prefix back in keeps
    /// the item indices the skip count relies on while validating and rebuilding
    /// only the suffix.
    pub(super) fn project_extending(prefix: &Self, module: &Module) -> Result<Self, Error> {
        let prefix = &prefix.0;
        let residual = Module {
            items: module.items[prefix.items.len()..].to_vec(),
            universe_seeds: module.universe_seeds.clone(),
            induct_decls: added(&module.induct_decls, &prefix.induct_decls),
            struct_decls: added(&module.struct_decls, &prefix.struct_decls),
            concepts: added(&module.concepts, &prefix.concepts),
            witnesses: module.witnesses.clone(),
            binder_floor: module.binder_floor,
            type_: module.type_.clone(),
            body: module.body.clone(),
        };
        validate_universes(&residual)?;
        let residual = project_module(&residual);

        Ok(Self(Module {
            items: prefix.items.iter().cloned().chain(residual.items).collect(),
            universe_seeds: residual.universe_seeds,
            induct_decls: merged(&prefix.induct_decls, residual.induct_decls),
            struct_decls: merged(&prefix.struct_decls, residual.struct_decls),
            concepts: merged(&prefix.concepts, residual.concepts),
            witnesses: residual.witnesses,
            binder_floor: residual.binder_floor,
            type_: residual.type_,
            body: residual.body,
        }))
    }
}

/// The entries `module` declares that `prefix` does not — the user's own, given
/// that the merged module's tables are the prelude's extended in place.
fn added<T: Clone>(
    module: &BTreeMap<Global, T>,
    prefix: &BTreeMap<Global, T>,
) -> BTreeMap<Global, T> {
    module
        .iter()
        .filter(|(name, _)| !prefix.contains_key(*name))
        .map(|(name, value)| (name.clone(), value.clone()))
        .collect()
}

/// `prefix` extended by `added`, restoring what [`added`] split apart.
fn merged<T: Clone>(
    prefix: &BTreeMap<Global, T>,
    added: BTreeMap<Global, T>,
) -> BTreeMap<Global, T> {
    let mut merged = prefix.clone();
    merged.extend(added);
    merged
}

fn project_definition(definition: &Definition) -> Definition {
    Definition {
        name: definition.name.clone(),
        kind: definition.kind.clone(),
        universe_context: Default::default(),
        island: definition.island.clone(),
        root: definition.root,
        type_: project_erased_universes(&definition.type_),
        body: project_erased_universes(&definition.body),
    }
}

/// Build the representation sealed by [`UniverseErased<Module>`]. Universe
/// arguments have no runtime identity; projecting them once also prevents
/// reduction from repeatedly specializing polymorphic Core bodies while
/// lowering them.
fn project_module(module: &Module) -> Module {
    Module {
        items: module
            .items
            .iter()
            .map(|item| match item {
                Item::Let(definition) => Item::Let(project_definition(definition)),
                Item::Rec(rec) => Item::Rec(RecItem::new(
                    rec.definitions().iter().map(project_definition).collect(),
                )),
            })
            .collect(),
        universe_seeds: Vec::new(),
        induct_decls: module
            .induct_decls
            .iter()
            .map(|(name, declaration)| {
                (
                    name.clone(),
                    InductDecl {
                        universe_context: Default::default(),
                        params: project_erased_universes(&declaration.params),
                        indices: project_erased_universes(&declaration.indices),
                        constructors: declaration
                            .constructors
                            .iter()
                            .map(|(tag, constructor)| {
                                (
                                    tag.clone(),
                                    InductParam {
                                        telescope: project_erased_universes(&constructor.telescope),
                                        plicities: constructor.plicities.clone(),
                                    },
                                )
                            })
                            .collect(),
                        result_sort: project_erased_universes(&declaration.result_sort),
                        module: declaration.module.clone(),
                        root: declaration.root,
                        rep_public: declaration.rep_public,
                    },
                )
            })
            .collect(),
        struct_decls: module
            .struct_decls
            .iter()
            .map(|(name, declaration)| {
                (
                    name.clone(),
                    StructDecl {
                        universe_context: Default::default(),
                        params: project_erased_universes(&declaration.params),
                        fields: project_erased_universes(&declaration.fields),
                        result_sort: project_erased_universes(&declaration.result_sort),
                        module: declaration.module.clone(),
                        root: declaration.root,
                        rep_public: declaration.rep_public,
                    },
                )
            })
            .collect(),
        concepts: module
            .concepts
            .iter()
            .map(|(name, concept)| {
                (
                    name.clone(),
                    Concept {
                        universe_context: Default::default(),
                        params: project_erased_universes(&concept.params),
                        fields: concept.fields.clone(),
                        supers: concept.supers.clone(),
                        root: concept.root,
                    },
                )
            })
            .collect(),
        witnesses: module.witnesses.clone(),
        binder_floor: module.binder_floor,
        type_: module.type_.as_ref().map(project_erased_universes),
        body: project_erased_universes(&module.body),
    }
}

/// Erase a whole meta-free [`Module`] into a verified arena
/// [`Module`]. Top-level items are erased in dominance order as the
/// module's item chain; the entrypoint body becomes the entry block, checked
/// against `expected`.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn erase_module_to_ir(
    context: &mut Context,
    module: &Module,
    expected: &Term,
) -> Result<curios_ersd::Module, Error> {
    let module = UniverseErased::<Module>::project(module)?.into_inner();
    let expected = UniverseErased::<Term>::project(expected)?.into_inner();
    // Erasure is re-derivation of elaborated terms, never surface elaboration,
    // so the representation-privacy checks are suppressed for the whole walk.
    context.with_suppressed_privacy(|context| {
        // Erasure runs with its own `Context`; seed the registries the
        // re-derived types consult before any item does.
        for (name, induct_decl) in &module.induct_decls {
            context.register_induct(name, induct_decl.clone())?;
        }
        for (name, struct_decl) in &module.struct_decls {
            context.register_struct(name, struct_decl.clone())?;
        }

        let mut lowering = Lowering::default();
        lowering.erase_items(context, &module, 0)?;

        lowering.builder.open_block();
        let outcome = lowering.walk(context, &module.body, &expected, None)?;
        let entry = lowering.seal(outcome);
        lowering.builder.set_entry(entry);

        // The verifier is the rejection point for the recursion classes the
        // language does not admit (a computed-only evaluation cycle); any other
        // failure here is an erasure bug, indistinguishable at this boundary.
        lowering
            .builder
            .finalize()
            .map_err(|error| Error::erased_module_invalid(error.to_string()))
    })
}

impl Lowering {
    /// Seal the innermost open block: a computed value returns, a divergence
    /// keeps its own terminator.
    pub(super) fn seal(&mut self, outcome: Outcome) -> curios_ersd::BlockId {
        match outcome {
            Outcome::Emitted(atom) => self
                .builder
                .seal_block(curios_ersd::Terminator::Return(atom)),
            Outcome::Diverged(terminator) => self.builder.seal_block(terminator),
        }
    }

    /// Bind a compound right-hand side in the innermost open block (or as a
    /// top-level item) and hand back its result operand.
    pub(super) fn bind(&mut self, hint: Option<&str>, rhs: curios_ersd::Rhs) -> Outcome {
        let result = self.builder.let_value(hint.map(str::to_string), rhs);
        Outcome::Emitted(curios_ersd::Atom::Value(result))
    }

    /// The unit constant — the value of a retained-but-erased slot.
    pub(super) fn unit(&mut self) -> curios_ersd::Atom {
        curios_ersd::Atom::Constant(self.builder.constant(curios_ersd::Constant::Unit))
    }

    /// Erase one expression to an operand. `expected` is the type the
    /// expression was checked against, consumed where a runtime shape must be
    /// read off it; `hint` names the statement when this expression binds one.
    pub(super) fn walk(
        &mut self,
        context: &mut Context,
        term: &Term,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        // Attach this term's span to any error from erasing it, exactly like
        // the legacy wrapper.
        let result = self.walk_subterm(context, term, expected, hint);
        match term.span() {
            Some(span) => result.map_err(|error| error.at(span)),
            None => result,
        }
    }

    fn walk_subterm(
        &mut self,
        context: &mut Context,
        term: &Term,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        match &**term {
            Subterm::Prim(primitive) => prim::erase_prim(self, context, primitive, expected, hint),
            // Type formers carry nothing to lower; their value is the unit of
            // a retained-but-erased slot.
            Subterm::Type(_)
            | Subterm::Prop
            | Subterm::FuncType(_)
            | Subterm::TupleType(_)
            | Subterm::InductType(_)
            | Subterm::StructType(_) => Ok(Outcome::Emitted(self.unit())),
            Subterm::UniverseInst(_) => {
                unreachable!("UniverseInst survived the UniverseErased<Module> projection")
            }
            Subterm::Var(var) => {
                let name = var.unwrap();
                match self.environment.lookup(name) {
                    Some(Binding::Atom(atom)) => Ok(Outcome::Emitted(atom)),
                    Some(Binding::Dropped) => {
                        self.dangled.insert(name.clone());
                        Ok(Outcome::Emitted(self.unit()))
                    }
                    None => unreachable!("erase_ir: unbound variable {name}"),
                }
            }
            Subterm::Let(binding) => self.erase_let(context, binding, expected, hint),
            Subterm::Match(m) => self.erase_match(context, m, hint),
            Subterm::Variant(variant) => self.erase_variant(context, variant, hint),
            Subterm::Struct(value) => self.erase_struct(context, value, hint),
            Subterm::Tuple(tuple) => self.erase_tuple(context, tuple, expected, hint),
            Subterm::Proj(proj) => self.erase_proj(context, proj, hint),
            Subterm::Func(func) => self.erase_func(context, func, expected, hint),
            Subterm::Apply(apply) => self.erase_apply(context, apply, hint),
            Subterm::Rec(rec) => self.erase_rec(context, rec, expected, hint),
            Subterm::RecMember(member) => self.erase_rec_member(context, member, expected, hint),
            // Erasure runs downstream of zonking and elaboration.
            Subterm::Metavar(_) => unreachable!("metavariable survived zonking into erase_ir"),
            Subterm::Infix(_) => unreachable!("infix node survived elaboration into erase_ir"),
            Subterm::NumLit(_) => {
                unreachable!("numeric-literal node survived elaboration into erase_ir")
            }
        }
    }

    /// Erase a let block binding for binding, in written order: each value is
    /// erased once (the operand law), defined in the Core context so dependent
    /// types reduce through it, and mapped to its operand; then the tail.
    fn erase_let(
        &mut self,
        context: &mut Context,
        binding: &Let,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        context.with_frame(|context| {
            let mut label_terms = Vec::<Term>::with_capacity(binding.bindings.len());

            for (index, local) in binding.bindings.iter().enumerate() {
                let (type_, value) = {
                    let refs = label_terms.iter().collect::<Vec<_>>();
                    (local.type_().release(&refs), local.value().release(&refs))
                };

                // The arena identity uniquifies by index, so the hint stays
                // the clean source label; the `#`-uniquified fresh name is
                // only the Core context key.
                let label = binding.tail.hint_iter().nth(index).flatten();
                let hint = label.map(str::to_string);
                let name = context.fresh(label);
                // A proof- or type-valued binding is walked, not collapsed: a
                // written binding evaluates under call-by-value even when its
                // *result* is erased, so an effectful never-returning body
                // (`let _ = /std/proc/exit(3); …`) still runs. The erased
                // residue a proof body can produce — projections of erased
                // fields, dropped binders, applications of erased content —
                // collapses to the unit constant at its own site (see
                // `erase_apply` and `erase_proj`).
                let outcome = self.walk(context, &value, &type_, hint.as_deref())?;
                let atom = emitted!(outcome);
                context.define_assuming(&name, &type_, &value, None);
                self.environment.bind(&name, atom);
                label_terms.push(Term::free_var(&name));
            }

            let tail = binding.tail.open(&label_terms.iter().collect::<Vec<_>>());
            self.walk(context, &tail, expected, hint)
        })
    }
}

/// A replayable prefix: the fixed prelude erased into an unfinished arena
/// module (items only, no entry), together with the erasure environment that
/// maps prelude Core names to their arena operands. Archived by
/// `curios-prelude` behind the `archive` feature and restored there once per
/// thread; every production compile consumes an owned clone, so the prelude
/// is never re-erased from source.
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize),
    rkyv(
        serialize_bounds(__S: rkyv::ser::Writer + rkyv::ser::Allocator + rkyv::ser::Sharing, __S::Error: rkyv::rancor::Source),
        deserialize_bounds(__D: rkyv::de::Pooling, __D::Error: rkyv::rancor::Source),
        bytecheck(bounds(__C: rkyv::validation::ArchiveContext + rkyv::validation::shared::SharedContext, __C::Error: rkyv::rancor::Source))
    )
)]
pub struct ErasedPrelude {
    #[cfg_attr(feature = "archive", rkyv(omit_bounds))]
    module: curios_ersd::Module,
    environment: Environment,
}

impl ErasedPrelude {
    /// Whether the prefix holds any erased items — the freshness probe the
    /// archive tests use.
    pub fn is_empty(&self) -> bool {
        self.module.items().is_empty()
    }
}

/// Erase the fixed prelude's items into a replayable prefix. The prelude
/// module carries no entrypoint of its own; only its item chain is erased.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn erase_prelude_to_ir_prefix(
    context: &mut Context,
    prelude: &Module,
) -> Result<ErasedPrelude, Error> {
    let prelude = UniverseErased::<Module>::project(prelude)?.into_inner();
    // Re-derivation, not surface elaboration (see `erase_module_to_ir`).
    context.with_suppressed_privacy(|context| {
        for (name, induct_decl) in &prelude.induct_decls {
            context.register_induct(name, induct_decl.clone())?;
        }
        for (name, struct_decl) in &prelude.struct_decls {
            context.register_struct(name, struct_decl.clone())?;
        }
        let mut lowering = Lowering::default();
        lowering.erase_items(context, &prelude, 0)?;
        Ok(ErasedPrelude {
            module: lowering.builder.into_module(),
            environment: lowering.environment,
        })
    })
}

/// Replay an erased prelude prefix and erase only the user suffix and
/// entrypoint body. The Core context is re-seeded with the prelude's
/// definitions (so the suffix's re-derived types reduce through them), the
/// builder resumes over the restored arenas, and the suffix items erase in
/// dominance order among themselves — every prelude reference is already
/// bound.
///
/// Two things about `prelude` are the caller's to guarantee, and both hold for
/// the archived prelude that `curios-prelude` restores — the only prelude this
/// is called with. Its universes are taken as already validated, at the restore
/// boundary where the bytes became a `Module`; and `module` must be it extended
/// in place, its items the prelude's own followed by the user's, which is what
/// `elaborate_module_with_prelude` returns. Both are what let this skip
/// re-deriving the standard library on every compilation.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn erase_module_with_prelude_to_ir(
    context: &mut Context,
    prelude: &Module,
    module: &Module,
    expected: &Term,
    prefix: ErasedPrelude,
) -> Result<curios_ersd::Module, Error> {
    let prelude = UniverseErased::<Module>::project_validated(prelude);
    let module = UniverseErased::<Module>::project_extending(&prelude, module)?.into_inner();
    let prelude = prelude.into_inner();
    let expected = UniverseErased::<Term>::project(expected)?.into_inner();
    // Re-derivation, not surface elaboration (see `erase_module_to_ir`).
    context.with_suppressed_privacy(|context| {
        for (name, induct_decl) in &module.induct_decls {
            context.register_induct(name, induct_decl.clone())?;
        }
        for (name, struct_decl) in &module.struct_decls {
            context.register_struct(name, struct_decl.clone())?;
        }

        // Re-seed the Core context with the prelude's definitions, mirroring
        // the legacy replay: later items and the entrypoint reduce through
        // them.
        for item in &prelude.items {
            match item {
                super::Item::Let(definition) => {
                    context.define_assuming_scheme(
                        &Free::from(&definition.name),
                        &definition.type_,
                        &definition.body,
                        Some(&definition.kind),
                        definition.universe_context.clone(),
                    );
                }
                super::Item::Rec(rec) => {
                    let definitions = rec.definitions();
                    for definition in &definitions {
                        let name = Free::from(&definition.name);
                        context.assume(&name, &definition.type_);
                        context.set_assumption_universe_context(
                            &name,
                            rec.group.universe_context().clone(),
                        );
                    }
                    for (index, definition) in definitions.iter().enumerate() {
                        context.define(
                            &Free::from(&definition.name),
                            &Term::rec_member(rec.group.clone(), index),
                            Some(&definition.kind),
                        );
                    }
                }
            }
        }

        let mut lowering = Lowering {
            builder: curios_ersd::ErsdBuilder::resume(prefix.module),
            environment: prefix.environment,
            dangled: Default::default(),
        };
        lowering.erase_items(context, &module, prelude.items.len())?;

        lowering.builder.open_block();
        let outcome = lowering.walk(context, &module.body, &expected, None)?;
        let entry = lowering.seal(outcome);
        lowering.builder.set_entry(entry);

        lowering
            .builder
            .finalize()
            .map_err(|error| Error::erased_module_invalid(error.to_string()))
    })
}
