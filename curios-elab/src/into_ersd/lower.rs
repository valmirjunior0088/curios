//! The lowering driver: the entry point, the expression walk, and the binding forms.
//!
//! The walk mirrors the legacy recursive erasure's control structure (its stack behavior is the no-regression baseline) but produces operands under the operand law instead of terms: [`Outcome::Emitted`] carries the atom a subexpression erased to, [`Outcome::Diverged`] carries the terminator that seals the innermost block when the subexpression provably never yields a value. Every non-atomic computation is bound by the builder at the point the walk reaches it, so evaluation order is statement order by construction.

use {
    super::Resumed,
    super::{
        Binding, Bound, Context, Environment, Error, InductDecl, Intrinsic, Let, Subterm,
        Telescope, Term, emitted, intrinsic, reduce_with,
    },
    crate::{validate_entry_universes, validate_universes},
    curios_core::{
        Certification, ConceptDecl, Definition, Entrypoint, Free, Global, InductParam, Item,
        Module, Operand, Program, StructDecl, Zonked, foreign_operands, project_erased_universes,
    },
    curios_utilities::{Span, grown},
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// What one expression erased to. See the module documentation.
#[derive(Debug)]
pub(super) enum Outcome {
    Emitted(curios_ersd::Atom),
    Diverged(curios_ersd::Terminator),
}

/// The erasure state: the checked builder constructing the module and the environment mapping Core names to their operands.
#[derive(Default)]
pub(super) struct Lowering {
    pub(super) builder: curios_ersd::ErsdBuilder,
    pub(super) environment: Environment,
    /// Dropped binder labels referenced from a retained position. Consumed by the function-body collapse: a proof-valued body that dangles a binder its own lambda dropped is replaced by the unit constant.
    pub(super) dangled: BTreeSet<Free>,
    /// The definition each emitted function descends from, innermost last, paired with how many anonymous ones have been minted under it. See [`Lowering::derived_hint`].
    owners: Vec<(String, usize)>,
    /// The family identity of each inductive whose row is being registered right now. An inductive is legitimately recursive, so classifying its fields reaches itself; the identity is minted before that walk begins, so the recursive lookup answers from here rather than re-entering the registration.
    pub(super) pending_families: BTreeMap<Global, curios_ersd::FamilyId>,
    /// The structures whose row is being registered right now. A self-referential structure is uninhabited but elaborates, and unlike an inductive its schema is only decided *after* its fields are classified — so the cycle is cut by declining to name a schema rather than by naming one early.
    pub(super) in_flight: BTreeSet<Global>,
    /// The span of each computed recursive member's initializer, by the identity it was minted as. The erased verifier refuses a member by that identity after the module holding it is gone, so the frame for the refusal is kept here rather than in the representation, whose nodes are the stored-unit format and carry no spans.
    pub(super) spans: BTreeMap<curios_ersd::ValueId, Span>,
}

pub(super) struct UniverseErased<T>(T);

impl<T> UniverseErased<T> {
    pub(super) fn into_inner(self) -> T {
        self.0
    }
}

impl UniverseErased<Zonked<Module>> {
    /// The composed evidence for the unit being erased: the input arrived zonked, and this projection removed its universe data — each layer justifying one family of impossible states in the walk below, no `Metavar` and no `Instance`. The outer layer claims the rewrite preserves the inner one, which `Zonked::map` re-validates in debug builds.
    pub(super) fn project(module: &Zonked<Module>) -> Result<Self, Error> {
        validate_universes(module.as_module())?;
        Ok(Self(module.clone().map(|module| project_module(&module))))
    }
}

impl UniverseErased<Module> {
    /// Project a module whose universes were already validated by the boundary that produced it, skipping the check rather than repeating it.
    ///
    /// The archived prelude is the case this exists for: `curios-prelude` validates it as it restores, which is the point where untrusted bytes become a `Module`, and the value is immutable from then on. Re-validating at every use walked the whole standard library a second time per compilation — inside the erasure context's step budget, at that — to re-derive an answer the restore already had.
    pub(super) fn project_validated(module: &Module) -> Self {
        Self(project_module(module))
    }
}

fn project_definition(definition: &Definition) -> Definition {
    Definition {
        name: definition.name,
        kind: definition.kind.clone(),
        universe_context: Default::default(),
        island: definition.island,
        // Carried as every field is, and read by nothing below: the erased functions' termination flags are marked from the certifier's record where the sealed program meets it (see `curios_ersd::Function::total`), never from this stamp.
        totality: definition.totality,
        type_: project_erased_universes(&definition.type_),
        body: project_erased_universes(&definition.body),
    }
}

/// Build the representation sealed by [`UniverseErased<Module>`]. Universe arguments have no runtime identity; projecting them once also prevents reduction from repeatedly specializing polymorphic Core bodies while lowering them.
fn project_module(module: &Module) -> Module {
    Module {
        items: module
            .items
            .iter()
            .map(|item| match item {
                Item::Let(definition) => Item::Let(project_definition(definition)),
                // Projected in place. Opening the group and re-closing it rebuilds every node twice over and discards every memoized derivation on the way, where the rewrite itself is uniform under the group's own binders and needs neither.
                Item::Rec(rec) => Item::Rec(rec.projected()),
            })
            .collect(),
        mounts: module.mounts.clone(),
        universe_seeds: Vec::new(),
        induct_decls: module
            .induct_decls
            .iter()
            .map(|(name, declaration)| {
                (
                    *name,
                    InductDecl {
                        universe_context: Default::default(),
                        arity: project_erased_universes(&declaration.arity),
                        constructors: declaration
                            .constructors
                            .iter()
                            .map(|(tag, constructor)| {
                                (
                                    tag.clone(),
                                    InductParam::new(
                                        project_erased_universes(&constructor.telescope),
                                        constructor.plicities().to_vec(),
                                    ),
                                )
                            })
                            .collect(),
                        result_sort: project_erased_universes(&declaration.result_sort),
                        module: declaration.module,
                        rep_public: declaration.rep_public,
                        // Polarity is elaboration-only metadata, projected out here alongside the universe context.
                        polarities: Vec::new(),
                    },
                )
            })
            .collect(),
        struct_decls: module
            .struct_decls
            .iter()
            .map(|(name, declaration)| {
                (
                    *name,
                    StructDecl {
                        universe_context: Default::default(),
                        arity: project_erased_universes(&declaration.arity),
                        result_sort: project_erased_universes(&declaration.result_sort),
                        module: declaration.module,
                        rep_public: declaration.rep_public,
                        // Polarity is elaboration-only metadata, projected out here alongside the universe context.
                        polarities: Vec::new(),
                    },
                )
            })
            .collect(),
        concepts: module
            .concepts
            .iter()
            .map(|(name, concept)| {
                (
                    *name,
                    ConceptDecl {
                        universe_context: Default::default(),
                        params: project_erased_universes(&concept.params),
                        fields: concept.fields.clone(),
                        supers: concept.supers.clone(),
                    },
                )
            })
            .collect(),
        witnesses: module.witnesses.clone(),
        tests: module.tests.clone(),
    }
}

/// Seed the erasure context's registries with `module`'s declarations — the re-derived types every item consults — for each scope unit and then the unit being erased.
fn seed_registries(context: &mut Context, module: &Module) -> Result<(), Error> {
    for (name, induct_decl) in &module.induct_decls {
        context.register_induct(name, induct_decl.clone())?;
    }
    for (name, struct_decl) in &module.struct_decls {
        context.register_struct(name, struct_decl.clone())?;
    }
    Ok(())
}

/// Erase the entrypoint body into the entry block and finalize the arena — [`erase_unit`]'s tail for the unit that is the entry. The program's own body owns what it mints, the same way an item owns what its body mints: the entry is emitted as `func/main`, so that is the name its lifted lambdas descend from. The verifier is the rejection point for the recursion classes the language does not admit — an evaluation cycle, an initializer that performs an effect — and hands those back as the refusal; a malformed arena is an erasure fault, which the verifier panics on rather than returning here.
fn seal_entry(
    mut lowering: Lowering,
    context: &mut Context,
    body: &Term,
    expected: &Term,
) -> Result<ErasedArena, Error> {
    lowering.builder.open_block();
    let outcome = lowering.with_owner("main".to_string(), |lowering| {
        lowering.walk(context, body, expected, None)
    })?;
    let outcome = force_entry(&mut lowering, context, expected, outcome)?;
    let entry = lowering.seal(outcome);
    lowering.builder.set_entry(entry);

    let Lowering {
        builder,
        environment,
        spans,
        ..
    } = lowering;

    Ok(ErasedArena {
        module: builder
            .finalize()
            .map_err(|error| Error::refused_by_verifier(error, &spans))?,
        environment,
    })
}

/// The entrypoint boundary: an `Io(T)` tail is a *description*, and the emitted `func/main` is the one place anything forces one.
///
/// Nothing else in the language may: there is no eliminator from `Io(T)` to `T`, which is what makes every term of non-`Io` type pure by typing. The force is type-directed rather than unconditional so a non-`Io` tail still erases as it always did — the erasure unit tests state such tails directly. What makes it mandatory in production is `curios-pipeline`, which checks the tail against `Io({})`, so the payload the force yields there is already unit and the entry discards nothing an author wrote. The runtime ignores `func/main`'s result either way: a program's meaning is the effects its description performs.
fn force_entry(
    lowering: &mut Lowering,
    context: &mut Context,
    expected: &Term,
    outcome: Outcome,
) -> Result<Outcome, Error> {
    let Outcome::Emitted(description) = outcome else {
        return Ok(outcome);
    };
    if !matches!(
        &*reduce_with(context, expected)?,
        Subterm::Intrinsic(Intrinsic::IoType(_))
    ) {
        return Ok(Outcome::Emitted(description));
    }

    let _forced = lowering.bind(
        None,
        curios_ersd::Rhs::Apply {
            callee: description,
            arguments: Vec::new(),
        },
    );

    Ok(Outcome::Emitted(lowering.unit()))
}

impl Lowering {
    /// Erase `body` with `name` standing as the definition every function minted inside it descends from.
    pub(super) fn with_owner<R>(&mut self, name: String, body: impl FnOnce(&mut Self) -> R) -> R {
        self.owners.push((name, 0));
        let result = body(self);
        self.owners.pop();
        result
    }

    /// A name for a function nothing else names.
    ///
    /// A lambda in argument position binds no statement, so `walk` has no hint to pass and the function it lifts to would print as a bare `~fN` — and its closure as a bare `$clsr/N`. `naming-scheme-law` spells an emitted name `kind/{uniquifier}$hint`, so an absent hint is a hole in it, and one a reader of a module dump or a profile pays for. The owner's own name qualified by which anonymous function this is fills it: `/sys/Handle/write/1`, and `/sys/Handle/write/1/1` one level in. The separator is `/` because `$` is reserved for the hint boundary itself.
    pub(super) fn derived_hint(&mut self) -> Option<String> {
        let (owner, minted) = self.owners.last_mut()?;
        *minted += 1;
        Some(format!("{owner}/{minted}"))
    }
    /// Seal the innermost open block: a computed value returns, a divergence keeps its own terminator.
    pub(super) fn seal(&mut self, outcome: Outcome) -> curios_ersd::BlockId {
        match outcome {
            Outcome::Emitted(atom) => self
                .builder
                .seal_block(curios_ersd::Terminator::Return(atom)),
            Outcome::Diverged(terminator) => self.builder.seal_block(terminator),
        }
    }

    /// Bind a compound right-hand side in the innermost open block (or as a top-level item) and hand back its result operand.
    pub(super) fn bind(&mut self, hint: Option<&str>, rhs: curios_ersd::Rhs) -> Outcome {
        let result = self.builder.let_value(hint.map(str::to_string), rhs);
        Outcome::Emitted(curios_ersd::Atom::Value(result))
    }

    /// The unit constant — the value of a retained-but-erased slot.
    pub(super) fn unit(&mut self) -> curios_ersd::Atom {
        curios_ersd::Atom::Constant(self.builder.constant(curios_ersd::Constant::Unit))
    }

    /// Erase each value against its telescope domain under `mask`, opening the telescope with the un-erased value so later dependent domains stay correct. Erasable slots are dropped entirely; kept slots erase through [`kept_operand`](Self::kept_operand).
    ///
    /// The one walk that consumes a signature mask, shared by every site that fills a telescope: struct, variant, and tuple construction, and the argument list of an application. Its slot-for-slot agreement with [`erasure_mask`](super::erasure_mask) is what fixes a function's runtime arity, so it must stay a single implementation.
    pub(super) fn masked_fields<B: Bound>(
        &mut self,
        context: &mut Context,
        mask: &[bool],
        telescope: Telescope<B>,
        values: &[Term],
    ) -> Result<Result<Vec<curios_ersd::Atom>, Outcome>, Error> {
        let mut atoms = Vec::with_capacity(values.len());
        let mut cursor = telescope.cursor();
        for (index, value) in values.iter().enumerate() {
            // Only a kept slot reads its domain, so an erased one steps past without opening it.
            if !mask[index] {
                let (_, type_) = cursor.entry().expect("erase: arity checked by elaborate");
                match self.kept_operand(context, value, &type_, None)? {
                    Outcome::Emitted(atom) => atoms.push(atom),
                    diverged => return Ok(Err(diverged)),
                }
            }
            cursor.advance(value.clone());
        }
        Ok(Ok(atoms))
    }

    /// Erase one expression to an operand. `expected` is the type the expression was checked against, consumed where a runtime shape must be read off it; `hint` names the statement when this expression binds one.
    pub(super) fn walk(
        &mut self,
        context: &mut Context,
        term: &Term,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        // Attach this term's span to any error from erasing it, exactly like the legacy wrapper.
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
            Subterm::Intrinsic(intrinsic) => {
                intrinsic::erase_intrinsic(self, context, intrinsic, hint)
            }
            // A host call: each operand erases against the demand elaboration checked it with, `foreign_operands`' reading of the same row. A diverging call's type operand erases with every other type.
            Subterm::Foreign(function, arguments) => {
                let mut atoms = Vec::with_capacity(arguments.len());
                for (argument, demand) in arguments.iter().zip(foreign_operands(function)) {
                    match demand {
                        Operand::At(type_) => {
                            atoms.push(emitted!(self.walk(context, argument, &type_, None)?));
                        }
                        Operand::IsType => {}
                        Operand::Function { .. } => {
                            unreachable!("a host call demands no function operand")
                        }
                    }
                }
                let foreign = self.builder.foreign(Arc::clone(function));
                let described = format!("io/{}", function.name());

                // A diverging call never yields a value, so the thunk's block is sealed by the terminator rather than by a return. Code after the *force* is dead; code after the construction is not.
                if function.diverges() {
                    return self.thunk(hint.or(Some(described.as_str())), move |_| {
                        Ok(Outcome::Diverged(curios_ersd::Terminator::Halt {
                            foreign,
                            operands: atoms,
                        }))
                    });
                }
                self.thunk(hint.or(Some(described.as_str())), move |lowering| {
                    Ok(lowering.bind(
                        None,
                        curios_ersd::Rhs::Foreign {
                            foreign,
                            operands: atoms,
                        },
                    ))
                })
            }
            // Type formers carry nothing to lower; their value is the unit of a retained-but-erased slot.
            Subterm::Type(_)
            | Subterm::Prop
            | Subterm::FuncType(_)
            | Subterm::TupleType(_)
            | Subterm::InductType(_)
            | Subterm::StructType(_) => Ok(Outcome::Emitted(self.unit())),
            Subterm::Instance(_) => {
                unreachable!("Instance survived the UniverseErased<Module> projection")
            }
            Subterm::Var(var) => {
                let name = var.unwrap();
                match self.environment.lookup(name) {
                    Some(Binding::Atom(atom)) => Ok(Outcome::Emitted(atom)),
                    Some(Binding::Dropped) => {
                        self.dangled.insert(*name);
                        Ok(Outcome::Emitted(self.unit()))
                    }
                    None => unreachable!("erase: unbound variable {name}"),
                }
            }
            Subterm::Let(binding) => self.erase_let(context, binding, expected, hint),
            Subterm::Match(m) => self.erase_match(context, m, hint),
            Subterm::Variant(variant) => self.erase_variant(context, variant, hint),
            Subterm::Struct(value) => self.erase_struct(context, value, hint),
            Subterm::Tuple(tuple) => self.erase_tuple(context, tuple, expected, hint),
            Subterm::Proj(proj) => self.erase_proj(context, proj, hint),
            Subterm::Func(func) => self.erase_func(context, func, expected, hint),
            Subterm::Apply(apply) => self.erase_apply(context, apply, expected, hint),
            Subterm::Rec(rec) => self.erase_rec(context, rec, expected, hint),
            // Erasure runs downstream of zonking and elaboration.
            Subterm::Metavar(_) => unreachable!("metavariable survived zonking into erasure"),
            Subterm::Transient(_) => {
                unreachable!("transient node survived elaboration into erasure")
            }
        }
    }

    /// Erase a let block binding for binding, in written order: each value is erased once (the operand law), defined in the Core context so dependent types reduce through it, and mapped to its operand; then the tail.
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

                // The arena identity uniquifies by index, so the hint stays the clean source label; the `#`-uniquified fresh name is only the Core context key.
                let label = binding.tail.hint_iter().nth(index).flatten();
                let hint = label.map(str::to_string);
                let name = context.fresh(label);
                // A binding is a kept slot: a value is erased once and bound, and a proof or a type is bound to its stand-in without being computed (see `kept_operand`). `let _ = /std/proc/exit(3); …` is neither — an exit is an `Io`, a description, which is built here and performed only where it is bound into the program.
                let outcome = self.kept_operand(context, &value, &type_, hint.as_deref())?;
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

/// What one unit's erasure provides to its successors: its items in an erased module, together with the environment mapping its Core names to erased operands. Archived behind the `archive` feature and restored once per thread; every production compile consumes an owned clone, so a stored unit is never re-erased from source.
///
/// `Default` is the empty scope, and it is what makes "erase the first unit" the same call as "erase a later one": `ErsdBuilder::resume` over an empty module reindexes nothing and yields exactly a fresh builder.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived(recursive)]
pub struct ErasedArena {
    #[archived_omit_bounds]
    module: curios_ersd::Module,
    environment: Environment,
}

impl ErasedArena {
    /// Whether this holds any erased items — the freshness probe the archive tests use.
    pub fn is_empty(&self) -> bool {
        self.module.items().is_empty()
    }

    /// Compact the erased arena, rewriting the environment that indexes into it.
    ///
    /// Both halves move together or neither does: the module owns the arenas and the environment owns the only identities held outside them, and they are archived as one value. Run before serialization so a stored image never carries tombstones a consumer would then walk on every compile.
    pub fn compact(&mut self) {
        let compaction = self.module.compact();
        self.environment.remap(&compaction);
    }

    /// Mark every function a definition `certification` classifies `Total` erased to as total — see [`curios_ersd::Function::total`]. Each is found through the name→operand binding erasure made for it, so a definition that erased to anything but a function has nothing to mark, and a function no record names keeps the conservative `false`.
    ///
    /// Run on the sealed program, once per record of what it was erased from: every unit's in scope and the entry's own. The arena is cumulative, so one arena holds every unit's functions, and nothing reads the flag before the back half lowers what this leaves.
    pub fn mark_total(&mut self, certification: &Certification) {
        for (name, totality) in certification.iter() {
            if totality.is_total()
                && let Some(Binding::Atom(curios_ersd::Atom::Function(id))) =
                    self.environment.lookup(&Free::from(name))
            {
                self.module.mark_total(id);
            }
        }
    }

    /// The finished erased module, for a unit whose entrypoint was sealed — what the back half of the pipeline lowers. A unit without one is a scope rather than a program, and its arena is resumed over instead.
    pub fn into_module(self) -> curios_ersd::Module {
        self.module
    }
}

/// Erase one unit's items onto what its scope already erased, leaving the arena open for its successors to resume over.
///
/// The Core context is re-seeded with the scope's definitions (so the unit's re-derived types reduce through them), the builder resumes over the restored arenas, and the items erase in dominance order among themselves — every reference into the scope is already bound.
///
/// **A unit and a program are erased by one walk, and only a program's is sealed.** One function used to erase the fixed prelude's item chain with no entry to seal and another erased a program and sealed one — two copies of one walk. The entry is not a unit's to carry: a program is its module and the term it closes with ([`Program`]), so [`erase_program`] runs this same walk and seals the entry at the type it states, which [`Zonked`] holds.
///
/// Nothing here is the caller's to guarantee any more, which is the point. Two contracts used to sit on this signature and neither was checked: that `module` was the prelude *extended in place*, discharged when the unit stopped carrying the prelude's items; and that the scope's Core and its erased arena described the same program, discharged by [`Resumed`] pairing them. What survives is a property of the archive rather than of a caller — its universes were validated at the restore boundary, where untrusted bytes became a `Module`.
pub fn erase_unit(
    context: &mut Context,
    resumed: Resumed<'_>,
    module: &Zonked<Module>,
) -> Result<ErasedArena, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| erase_within(context, resumed, module, None))
}

/// [`erase_unit`] for a program: its module erased onto its scope, then its entry sealed at the type it states — the arena whose [`ErasedArena::into_module`] the back half lowers.
pub fn erase_program(
    context: &mut Context,
    resumed: Resumed<'_>,
    program: &Zonked<Program>,
) -> Result<ErasedArena, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| erase_within(context, resumed, &program.module(), Some(program.entry())))
}

fn erase_within(
    context: &mut Context,
    resumed: Resumed<'_>,
    module: &Zonked<Module>,
    entry: Option<(&Term, &Term)>,
) -> Result<ErasedArena, Error> {
    let scope = resumed.projected_cores();
    // The entry's universes are held to its module's schemes before projection erases them.
    let entry = entry
        .map(|(body, type_)| {
            validate_entry_universes(
                module.as_module(),
                &Entrypoint {
                    body: body.clone(),
                    type_: Some(type_.clone()),
                },
            )?;
            Ok::<_, Error>((
                project_erased_universes(body),
                project_erased_universes(type_),
            ))
        })
        .transpose()?;
    let module = UniverseErased::<Zonked<Module>>::project(module)?
        .into_inner()
        .into_module();
    // Erasure is re-derivation of elaborated terms, never surface elaboration, so the representation-privacy checks are suppressed for the whole walk.
    context.with_suppressed_privacy(|context| {
        // Every half: `module` declares only its own, so each scope unit's nominal entries reach the context from that unit itself. They are disjoint by mount — no unit can reuse another's name — which is why `register_*` rejecting a duplicate key is not a constraint here.
        for unit in &scope {
            seed_registries(context, unit)?;
        }
        seed_registries(context, &module)?;

        // Re-seed the Core context with the scope's definitions, in dependency order: later items and the entrypoint reduce through them.
        for item in scope.iter().flat_map(|unit| &unit.items) {
            match item {
                Item::Let(definition) => {
                    context.define_assuming_scheme(
                        &Free::from(&definition.name),
                        &definition.type_,
                        &definition.body,
                        Some(&definition.kind),
                        definition.universe_context.clone(),
                    );
                }
                Item::Rec(rec) => {
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
                            &Term::rec_proj(rec.group.clone(), index),
                            Some(&definition.kind),
                        );
                    }
                }
            }
        }

        let prefix = resumed.into_arena();
        let mut lowering = Lowering {
            builder: curios_ersd::ErsdBuilder::resume(prefix.module),
            environment: prefix.environment,
            dangled: Default::default(),
            owners: Default::default(),
            pending_families: Default::default(),
            in_flight: Default::default(),
            spans: Default::default(),
        };
        lowering.erase_items(context, &module)?;

        match entry {
            Some((body, type_)) => seal_entry(lowering, context, &body, &type_),
            // No entrypoint: the arena stays open, which is exactly what a successor resumes over. The hand-off still checks every rule a prefix can satisfy, so an image reaches the archive walked rather than merely constructed.
            None => Ok(ErasedArena {
                module: lowering
                    .builder
                    .into_module()
                    .map_err(|error| Error::refused_by_verifier(error, &lowering.spans))?,
                environment: lowering.environment,
            }),
        }
    })
}
