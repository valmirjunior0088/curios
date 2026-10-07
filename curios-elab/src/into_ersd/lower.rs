//! The lowering driver: the entry point, the expression walk, and the binding forms.
//!
//! The walk produces operands under the operand law rather than terms: [`Outcome::Emitted`] carries the atom a subexpression erased to, [`Outcome::Diverged`] carries the terminator that seals the innermost block when the subexpression provably never yields a value. Every non-atomic computation is bound by the builder at the point the walk reaches it, so evaluation order is statement order by construction.

use {
    super::{
        Binding, Bound, Environment, ErasedItem, ErasedUnit, Erasures, Error, Export, InductDecl,
        Intrinsic, Let, Retyping, Subterm, Telescope, Term, emitted, intrinsic, link,
    },
    crate::{validate_entry_universes, validate_universes},
    curios_core::{
        Certification, ConceptDecl, Definition, Entrypoint, Free, Global, InductParam, Item,
        Module, Operand, Program, StructDecl, Zonked, foreign_operands, project_erased_universes,
    },
    curios_utilities::{Span, SyntaxRegistry, grown},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        sync::Arc,
    },
};

/// What one expression erased to. See the module documentation.
#[derive(Debug)]
pub(super) enum Outcome {
    Emitted(curios_ersd::Atom),
    Diverged(curios_ersd::Terminator),
}

/// How one item's erasure reads the erased form of another: by a name the other declares.
pub(super) type Externals<'s> = &'s dyn Fn(&Global) -> Option<&'s ErasedItem>;

/// One item's erasure state: the checked builder constructing its fragment and the environment mapping Core names to their operands there.
pub(super) struct Lowering<'s> {
    pub(super) builder: curios_ersd::ErsdBuilder,
    pub(super) environment: Environment,
    /// The erased forms of the items this one may name.
    externals: Externals<'s>,
    /// The name each value the fragment imports stands for, in the order the builder minted them.
    values: Vec<Global>,
    /// The name each function the fragment imports stands for, in the order the builder minted them.
    functions: Vec<Global>,
    /// Dropped binder labels referenced from a retained position. Consumed by the function-body collapse: a proof-valued body that dangles a binder its own lambda dropped is replaced by the unit constant.
    pub(super) dangled: BTreeSet<Free>,
    /// The definition each emitted function descends from, innermost last, paired with how many anonymous ones have been minted under it. See [`Lowering::derived_hint`].
    owners: Vec<(String, usize)>,
    /// The family identity of each inductive whose row is being registered right now. An inductive is legitimately recursive, so classifying its fields reaches itself; the identity is minted before that walk begins, so the recursive lookup answers from here rather than re-entering the registration.
    pub(super) pending_families: BTreeMap<Global, curios_ersd::FamilyId>,
    /// The structures whose row is being registered right now. A self-referential structure is uninhabited but elaborates, and unlike an inductive its schema is only decided *after* its fields are classified — so the cycle is cut by declining to name a schema rather than by naming one early.
    pub(super) in_flight: BTreeSet<Global>,
    /// The type each term was read at ([`Lowering::type_of`]), for the item being erased. Dropped at every top-level item and before the entry, which is what bounds it.
    pub(super) types: HashMap<Term, Term>,
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
    /// The archived prelude is the case this exists for: `curios-prelude` validates it as it restores, which is the point where untrusted bytes become a `Module`, and the value is immutable from then on. Re-validating at every use would walk the whole standard library a second time per compilation — inside the erasure context's step budget, at that — to re-derive an answer the restore already has.
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
        variances: Vec::new(),
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
                                    InductParam {
                                        telescope: project_erased_universes(&constructor.telescope),
                                    },
                                )
                            })
                            .collect(),
                        result_sort: project_erased_universes(&declaration.result_sort),
                        module: declaration.module,
                        rep_public: declaration.rep_public,
                        // Polarity is elaboration-only metadata, projected out here alongside the universe context.
                        polarities: Vec::new(),
                        variances: Vec::new(),
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
                        variances: Vec::new(),
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
fn seed_registries(context: &mut Retyping, module: &Module) -> Result<(), Error> {
    for (name, induct_decl) in &module.induct_decls {
        context.register_induct(name, induct_decl.clone())?;
    }
    for (name, struct_decl) in &module.struct_decls {
        context.register_struct(name, struct_decl.clone())?;
    }
    Ok(())
}

/// Erase the entrypoint body into the entry block of a fragment of its own — [`erase_program`]'s tail. The program's own body owns what it mints, the same way an item owns what its body mints: the entry is emitted as `func/main`, so that is the name its lifted lambdas descend from. The verifier is the rejection point for the recursion classes the language does not admit — an evaluation cycle, an initializer that performs an effect — and hands those back as the refusal; a malformed fragment is an erasure fault, which the verifier panics on rather than returning here.
fn seal_entry(
    mut lowering: Lowering<'_>,
    context: &mut Retyping,
    body: &Term,
    expected: &Term,
) -> Result<ErasedItem, Error> {
    lowering.types.clear();
    lowering.builder.open_block();
    let outcome = lowering.with_owner("main".to_string(), |lowering| {
        lowering.walk(context, body, expected, None)
    })?;
    let outcome = force_entry(&mut lowering, context, expected, outcome)?;
    let entry = lowering.seal(outcome);
    lowering.builder.set_entry(entry);

    lowering.into_erased(&[])
}

/// The entrypoint boundary: an `Io(T)` tail is a *description*, and the emitted `func/main` is the one place anything forces one.
///
/// Nothing else in the language may: there is no eliminator from `Io(T)` to `T`, which is what makes every term of non-`Io` type pure by typing. The force is type-directed rather than unconditional so a non-`Io` tail erases unforced — the erasure unit tests state such tails directly. What makes it mandatory in production is `curios-pipeline`, which checks the tail against `Io({})`, so the payload the force yields there is already unit and the entry discards nothing an author wrote. The runtime ignores `func/main`'s result either way: a program's meaning is the effects its description performs.
fn force_entry(
    lowering: &mut Lowering<'_>,
    context: &mut Retyping,
    expected: &Term,
    outcome: Outcome,
) -> Result<Outcome, Error> {
    let Outcome::Emitted(description) = outcome else {
        return Ok(outcome);
    };
    if !matches!(
        &*context.reduce_with(expected)?,
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

impl<'s> Lowering<'s> {
    /// A fresh erasure, of one item, reading other items' erased forms through `externals`.
    pub(super) fn over(externals: Externals<'s>) -> Self {
        Self {
            builder: curios_ersd::ErsdBuilder::new(),
            environment: Environment::default(),
            externals,
            values: Vec::new(),
            functions: Vec::new(),
            dangled: BTreeSet::new(),
            owners: Vec::new(),
            pending_families: BTreeMap::new(),
            in_flight: BTreeSet::new(),
            types: HashMap::new(),
            spans: BTreeMap::new(),
        }
    }

    /// Close the fragment and hand the item's erased form over: the fragment verified with no entry, its recursion rules included, and of the environment the bindings of `declared`, the names the item declares.
    pub(super) fn into_erased(self, declared: &[&Global]) -> Result<ErasedItem, Error> {
        let Self {
            builder,
            environment,
            values,
            functions,
            spans,
            ..
        } = self;
        let module = builder
            .into_module()
            .map_err(|error| Error::refused_by_verifier(error, &spans))?;

        Ok(ErasedItem::new(
            module,
            environment.exporting(declared),
            values,
            functions,
            &spans,
        ))
    }

    /// The erased form of the item declaring `name`, where another item does.
    pub(super) fn declaring(&self, name: &Global) -> Option<&'s ErasedItem> {
        (self.externals)(name)
    }

    /// Bind `name`, which another item declares, as this fragment reads it: an erased binding as none, a constant as the constant, and a value or a function as an import, minted the first time the name is used.
    ///
    /// A value whose own initializer reads it is refused here: its item could not tell that anything would read it, and this read is what forces it into itself. So the recursion rules are settled for every fragment where it is erased, and a unit that reads such a value of another is refused where it is compiled.
    fn external(&mut self, name: &Free) -> Result<Option<Binding>, Error> {
        let Some(global) = name.as_global() else {
            return Ok(None);
        };
        let Some(export) = (self.externals)(global).and_then(|erased| erased.export(global)) else {
            return Ok(None);
        };
        let binding = match export {
            Export::Unforced { member, span } => {
                return Err(Error::evaluates_itself(member.to_string()).at_opt(span.cloned()));
            }
            Export::Dropped => Binding::Dropped,
            Export::Constant(constant) => Binding::Atom(curios_ersd::Atom::Constant(
                self.builder.constant(constant.clone()),
            )),
            Export::Value => {
                self.values.push(*global);
                Binding::Atom(curios_ersd::Atom::Value(
                    self.builder.import_value(Some(global.symbol())),
                ))
            }
            Export::Function { arity, interface } => {
                self.functions.push(*global);
                Binding::Atom(curios_ersd::Atom::Function(self.builder.import_function(
                    Some(global.symbol()),
                    arity,
                    interface,
                )))
            }
        };
        match binding {
            Binding::Atom(atom) => self.environment.bind(name, atom),
            Binding::Dropped => self.environment.bind_dropped(name),
        }

        Ok(Some(binding))
    }

    /// Erase `body` with `name` standing as the definition every function minted inside it descends from.
    pub(super) fn with_owner<R>(&mut self, name: String, body: impl FnOnce(&mut Self) -> R) -> R {
        self.owners.push((name, 0));
        let result = body(self);
        self.owners.pop();
        result
    }

    /// A name for a function nothing else names.
    ///
    /// A lambda in argument position binds no statement, so `walk` has no hint to pass and the function it lifts to would print as a bare `~fN` — and its closure as a bare `$clsr/N`. The shared naming scheme (`documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md`) spells an emitted name `kind/{uniquifier}$hint`, so an absent hint is a hole in it, and one a reader of a module dump or a profile pays for. The owner's own name qualified by which anonymous function this is fills it: `/sys/Handle/write/1`, and `/sys/Handle/write/1/1` one level in. The separator is `/` because `$` is reserved for the hint boundary itself.
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
        context: &mut Retyping,
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
        context: &mut Retyping,
        term: &Term,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        // Attach this term's span to any error from erasing it.
        let result = self.walk_subterm(context, term, expected, hint);
        match term.span() {
            Some(span) => result.map_err(|error| error.at(span)),
            None => result,
        }
    }

    fn walk_subterm(
        &mut self,
        context: &mut Retyping,
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
                let binding = match self.environment.lookup(name) {
                    Some(binding) => binding,
                    None => self
                        .external(name)?
                        .unwrap_or_else(|| unreachable!("erase: unbound variable {name}")),
                };
                match binding {
                    Binding::Atom(atom) => Ok(Outcome::Emitted(atom)),
                    Binding::Dropped => {
                        self.dangled.insert(*name);
                        Ok(Outcome::Emitted(self.unit()))
                    }
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
        context: &mut Retyping,
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

/// A program erased and linked: one module holding what its entry reaches, and where each name linked into it landed.
#[derive(Debug)]
pub struct ErasedProgram {
    module: curios_ersd::Module,
    bound: BTreeMap<Global, curios_ersd::Atom>,
}

impl ErasedProgram {
    pub(super) fn new(
        module: curios_ersd::Module,
        bound: BTreeMap<Global, curios_ersd::Atom>,
    ) -> Self {
        Self { module, bound }
    }

    /// Mark every function a definition `certification` classifies `Total` erased to as total — see [`curios_ersd::Function::total`]. Each is found through where its name was linked, so a definition that erased to anything but a function, or that the program does not reach, has nothing to mark, and a function no record names keeps the conservative `false`.
    ///
    /// Run on the linked program, once per record of what it was erased from: every unit's in scope and the entry's own. Nothing reads the flag before the back half lowers what this leaves.
    pub fn mark_total(&mut self, certification: &Certification) {
        for (name, totality) in certification.iter() {
            if totality.is_total()
                && let Some(curios_ersd::Atom::Function(id)) = self.bound.get(name)
            {
                self.module.mark_total(*id);
            }
        }
    }

    /// The finished erased module — what the back half of the pipeline lowers.
    pub fn into_module(self) -> curios_ersd::Module {
        self.module
    }
}

/// Erase one unit's items, each into a fragment of its own, over what its scope erased to.
///
/// The reader erasure reads through is its own, made here on `budget` and handed in by no caller (`Retyping`). It is seeded with the scope's definitions and the unit's own, so every re-derived type reduces through them, and the items erase in the order the module stores them, every item after the items it reaches — so the erased form of every name an item uses is in hand when it is asked for.
///
/// **A unit and a program are erased by one walk, and only a program is linked.** The entry is not a unit's to carry: a program is its module and the term it closes with ([`Program`]), so [`erase_program`] runs this same walk, erases the entry at the type it states, which [`Zonked`] holds, and links what the entry reaches.
pub fn erase_unit(
    budget: u64,
    syntax: SyntaxRegistry,
    scope: Erasures<'_>,
    module: &Zonked<Module>,
) -> Result<ErasedUnit, Error> {
    erase_unit_in(&mut Retyping::new(budget, syntax), scope, module)
}

/// [`erase_unit`] through a reader its caller holds.
pub(crate) fn erase_unit_in(
    context: &mut Retyping,
    scope: Erasures<'_>,
    module: &Zonked<Module>,
) -> Result<ErasedUnit, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = seed(context, scope, module)?;
        erase_items(context, scope, &module, |_| None)
    })
}

/// [`erase_unit`] for a unit compiled over a baseline: an item none of whose names `redone` holds keeps the erased form `baseline` has for it, and every other is erased.
///
/// What is redone is closed under what an item's erasure reads — the types and registry entries of the names it reaches, and how each of them is bound — which is the caller's to guarantee, as it is for the items elaborated again.
pub fn erase_unit_over(
    budget: u64,
    syntax: SyntaxRegistry,
    scope: Erasures<'_>,
    module: &Zonked<Module>,
    baseline: &ErasedUnit,
    redone: &dyn Fn(&Global) -> bool,
) -> Result<ErasedUnit, Error> {
    curios_profile::profile!("erase_unit");
    let context = &mut Retyping::new(budget, syntax);
    grown(|| {
        let module = seed(context, scope, module)?;
        erase_items(context, scope, &module, |item| {
            let names = item.declared_names();
            match names.iter().any(|name| redone(name)) {
                true => None,
                false => baseline.find(names.first()?).cloned(),
            }
        })
    })
}

/// [`erase_unit`] for a program: its module erased over its scope, its entry erased at the type it states, and the two linked with what they reach of the scope — the module whose [`ErasedProgram::into_module`] the back half lowers.
pub fn erase_program(
    budget: u64,
    syntax: SyntaxRegistry,
    scope: Erasures<'_>,
    program: &Zonked<Program>,
) -> Result<ErasedProgram, Error> {
    erase_program_in(&mut Retyping::new(budget, syntax), scope, program)
}

/// [`erase_program`] through a reader its caller holds.
pub(crate) fn erase_program_in(
    context: &mut Retyping,
    scope: Erasures<'_>,
    program: &Zonked<Program>,
) -> Result<ErasedProgram, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let (body, type_) = program.entry();
        // The entry's universes are held to its module's schemes before projection erases them.
        validate_entry_universes(
            program.module().as_module(),
            &Entrypoint {
                body: body.clone(),
                type_: Some(type_.clone()),
            },
        )?;
        let (body, type_) = (
            project_erased_universes(body),
            project_erased_universes(type_),
        );

        let module = seed(context, scope, &program.module())?;
        let own = erase_items(context, scope, &module, |_| None)?;
        let entry = {
            let externals = |name: &Global| own.find(name).or_else(|| scope.find(name));
            seal_entry(Lowering::over(&externals), context, &body, &type_)?
        };

        link(scope, &own, &entry)
    })
}

/// Seed the erasure context with what every item's re-derived types reduce through — the scope's registries and definitions, then the unit's own — and hand back the unit projected out of its universes.
fn seed(
    context: &mut Retyping,
    scope: Erasures<'_>,
    module: &Zonked<Module>,
) -> Result<Module, Error> {
    let predecessors = scope.projected_cores();
    let module = UniverseErased::<Zonked<Module>>::project(module)?
        .into_inner()
        .into_module();
    // Every half: `module` declares only its own, so each scope unit's nominal entries reach the context from that unit itself. They are disjoint by mount — no unit can reuse another's name — which is why `register_*` rejecting a duplicate key is not a constraint here.
    for unit in &predecessors {
        seed_registries(context, unit)?;
    }
    seed_registries(context, &module)?;

    // Every definition, the unit's own with the scope's: an item is erased in a fragment of its own, and which of the unit's items are erased at all is the caller's to say, so none of them is defined by the erasure of the one before it.
    for item in predecessors
        .iter()
        .flat_map(|unit| &unit.items)
        .chain(&module.items)
    {
        define(context, item);
    }

    Ok(module)
}

/// Define `item`'s names in the Core context: a definition at its type and body, a group's members each at its type and its selection.
fn define(context: &mut Retyping, item: &Item) {
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
                context
                    .set_assumption_universe_context(&name, rec.group.universe_context().clone());
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

/// Erase `module`'s items in the order it stores them, each into a fragment of its own, but for those `reused` hands an erased form for.
fn erase_items(
    context: &mut Retyping,
    scope: Erasures<'_>,
    module: &Module,
    reused: impl Fn(&Item) -> Option<ErasedItem>,
) -> Result<ErasedUnit, Error> {
    let mut unit = ErasedUnit::default();
    for item in &module.items {
        let erased = match reused(item) {
            Some(erased) => erased,
            None => {
                let externals = |name: &Global| unit.find(name).or_else(|| scope.find(name));
                erase_item(context, &externals, item)?
            }
        };
        unit.push(item, erased);
    }

    Ok(unit)
}

/// Erase one item into a fragment of its own, on a budget restored for it.
fn erase_item(
    context: &mut Retyping,
    externals: Externals<'_>,
    item: &Item,
) -> Result<ErasedItem, Error> {
    context.restore_budget();
    let mut lowering = Lowering::over(externals);
    match item {
        Item::Let(definition) => lowering.erase_definition(context, definition)?,
        Item::Rec(rec) => lowering.erase_rec_item(context, rec)?,
    }
    // The row of each registry entry the item declares is registered here, whether or not anything of the item constructs or matches it: every other item holds the row as an import of this one.
    for name in item.declared_names() {
        if context.struct_decl(name).is_some() {
            lowering.struct_row(context, name)?;
        }
        if context.induct_decl(name).is_some() {
            lowering.induct_row(context, name)?;
        }
    }

    lowering.into_erased(&item.declared_names())
}
