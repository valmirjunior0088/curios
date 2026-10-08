//! The lowering driver: the entry point, the expression walk, and the binding forms.
//!
//! The walk produces operands under the operand law rather than terms: [`Outcome::Emitted`] carries the atom a subexpression erased to, [`Outcome::Diverged`] carries the terminator that seals the innermost block when the subexpression provably never yields a value. Every non-atomic computation is bound by the builder at the point the walk reaches it, so evaluation order is statement order by construction.

use {
    super::{
        Binding, Bound, Environment, ErasedBaseline, ErasedItem, ErasedOver, ErasedUnit, Erasures,
        Error, Export, Intrinsic, Let, Projected, Retyping, Subterm, Telescope, Term, emitted,
        intrinsic, link,
    },
    crate::{Traces, validate_entry_universes, validate_universes, validate_universes_of},
    curios_core::{
        Certification, Entrypoint, Extent, Free, Global, Item, Module, Operand, Program, Zonked,
        foreign_operands, project_erased_universes,
    },
    curios_utilities::{SyntaxRegistry, grown},
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashMap},
        rc::Rc,
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
    /// The definition each emitted function descends from, innermost last, paired with how many anonymous ones have been minted under it. See [`Lowering::derived_hint`].
    owners: Vec<(String, usize)>,
    /// The family identity of each inductive whose row is being registered right now. An inductive is legitimately recursive, so classifying its fields reaches itself; the identity is minted before that walk begins, so the recursive lookup answers from here rather than re-entering the registration.
    pub(super) pending_families: BTreeMap<Global, curios_ersd::FamilyId>,
    /// The structures whose row is being registered right now. A self-referential structure is uninhabited but elaborates, and unlike an inductive its schema is only decided *after* its fields are classified — so the cycle is cut by declining to name a schema rather than by naming one early.
    pub(super) in_flight: BTreeSet<Global>,
    /// The type each term was read at ([`Lowering::type_of`]), for the item being erased. Dropped at every top-level item and before the entry, which is what bounds it.
    pub(super) types: HashMap<Term, Term>,
    /// The extent of each computed recursive member's initializer, by the identity it was minted as. The erased verifier refuses a member by that identity after the module holding it is gone, so the frame for the refusal is kept here rather than in the representation, whose nodes are the stored-unit format and carry no extents.
    pub(super) extents: BTreeMap<curios_ersd::ValueId, Extent>,
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
            owners: Vec::new(),
            pending_families: BTreeMap::new(),
            in_flight: BTreeSet::new(),
            types: HashMap::new(),
            extents: BTreeMap::new(),
        }
    }

    /// Close the fragment and hand the item's erased form over: the fragment verified with no entry, its recursion rules included, and of the environment the bindings of `declared`, the names the item declares.
    pub(super) fn into_erased(self, declared: &[&Global]) -> Result<ErasedItem, Error> {
        let Self {
            builder,
            environment,
            values,
            functions,
            extents,
            ..
        } = self;
        let module = builder
            .into_module()
            .map_err(|error| Error::refused_by_verifier(error, &extents))?;

        Ok(ErasedItem::new(
            module,
            environment.exporting(declared),
            values,
            functions,
            &extents,
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
            Export::Unforced { member, extent } => {
                return Err(Error::evaluates_itself(member.to_string()).at_opt(extent.cloned()));
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
    /// The one walk that consumes a mask, which only a nominal declaration has: a structure's fields and a constructor's payload, dropped by the declaration's own signature, read once where its row is registered. A structural type has no declaration to read and drops nothing ([`Lowering::slots`]).
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

    /// Erase each value into a slot of its own, against its telescope domain: the arguments of an application and the fields of an anonymous tuple, whose runtime shape is the telescope's length whatever its domains are read as.
    pub(super) fn slots<B: Bound>(
        &mut self,
        context: &mut Retyping,
        telescope: Telescope<B>,
        values: &[Term],
    ) -> Result<Result<Vec<curios_ersd::Atom>, Outcome>, Error> {
        self.masked_fields(context, &vec![false; values.len()], telescope, values)
    }

    /// Erase one expression to an operand. `expected` is the type the expression was checked against, consumed where a runtime shape must be read off it; `hint` names the statement when this expression binds one.
    pub(super) fn walk(
        &mut self,
        context: &mut Retyping,
        term: &Term,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        // Attach this term's extent to any error from erasing it.
        let result = self.walk_subterm(context, term, expected, hint);
        match term.extent() {
            Some(extent) => result.map_err(|error| error.at(extent)),
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
            | Subterm::Prop(_)
            | Subterm::FuncType(_)
            | Subterm::TupleType(_)
            | Subterm::InductType(_)
            | Subterm::StructType(_) => Ok(Outcome::Emitted(self.unit())),
            Subterm::Instance(_) => {
                unreachable!("an instance survived the projection out of universes")
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
                    Binding::Dropped => Ok(Outcome::Emitted(self.unit())),
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
/// The reader erasure reads through is its own, made here on `budget` over the unit and its scope out of their universes (`Retyping`, `Projected`) and handed in by no caller. Nothing is seeded: a declaration is projected when an item's erasure first reads it or when it is erased, so a unit pays for what its items read. The items erase in the order the module stores them, every item after the items it reaches — so the erased form of every name an item uses is in hand when it is asked for.
///
/// **A unit and a program are erased by one walk, and only a program is linked.** The entry is not a unit's to carry: a program is its module and the term it closes with ([`Program`]), so [`erase_program`] runs this same walk, erases the entry at the type it states, which [`Zonked`] holds, and links what the entry reaches.
pub fn erase_unit(
    budget: u64,
    syntax: SyntaxRegistry,
    scope: Erasures<'_>,
    module: &Zonked<Module>,
) -> Result<ErasedUnit, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = module.as_module();
        let view = projected(scope, module)?;
        let context = &mut Retyping::over(budget, syntax, &view);
        erase_items(context, &view, scope, module, None)
    })
}

/// [`erase_unit`] for a unit compiled over a baseline: an item keeps the erased form `baseline` has for it, the baseline's own and no copy of it, where the elaborator kept its declaration and everything its erasure asked there is answered as it was (`over`), and every other is erased.
///
/// The universes held to their schemes are those of the items the elaborator did not keep, and of any the baseline holds no erased form for: a kept declaration's were held where the baseline was compiled, and what it reads of a scheme stands, or it would not have been kept.
pub fn erase_unit_over(
    budget: u64,
    syntax: SyntaxRegistry,
    scope: Erasures<'_>,
    module: &Zonked<Module>,
    baseline: ErasedBaseline<'_>,
) -> Result<ErasedUnit, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = module.as_module();
        validate_universes_of(module, &|name| {
            !baseline.kept.contains(name) || baseline.erased.held(name).is_none()
        })?;
        let view = Projected::over(scope.cores(), module);
        let context = &mut Retyping::over(budget, syntax, &view);
        erase_items(
            context,
            &view,
            scope,
            module,
            Some(ErasedOver::new(baseline, module)),
        )
    })
}

/// [`erase_unit`] for a program: its module erased over its scope, its entry erased at the type it states, and the two linked with what they reach of the scope — the module whose [`ErasedProgram::into_module`] the back half lowers.
pub fn erase_program(
    budget: u64,
    syntax: SyntaxRegistry,
    scope: Erasures<'_>,
    program: &Zonked<Program>,
) -> Result<ErasedProgram, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = &program.as_program().module;
        let (body, type_) = program.entry();
        // The entry's universes are held to its module's schemes before projection erases them.
        validate_entry_universes(
            module,
            &Entrypoint {
                body: body.clone(),
                type_: Some(type_.clone()),
            },
        )?;
        let (body, type_) = (
            project_erased_universes(body),
            project_erased_universes(type_),
        );

        let view = projected(scope, module)?;
        let context = &mut Retyping::over(budget, syntax, &view);
        let own = erase_items(context, &view, scope, module, None)?;
        let entry = {
            let externals = |name: &Global| own.find(name).or_else(|| scope.find(name));
            seal_entry(Lowering::over(&externals), context, &body, &type_)?
        };

        link(scope, &own, &entry)
    })
}

/// `module` and its scope as erasure reads them, out of their universes, with `module`'s universes held to its schemes first: what the projection erases is validated before it goes. The scope's were validated where its units were stored, which is where untrusted bytes become a module.
pub(super) fn projected<'a>(
    scope: Erasures<'a>,
    module: &'a Module,
) -> Result<Projected<'a>, Error> {
    validate_universes(module)?;

    Ok(Projected::over(scope.cores(), module))
}

/// Erase `module`'s items in the order it stores them, each into a fragment of its own and each read out of its universes through `view`, but for those `over` keeps the baseline's erased form of, which are never projected. What erasing each item asked is kept with the unit: of the declarations around it, through the reader's doors, and of each erased form it read how a name is bound in.
pub(super) fn erase_items<'s>(
    context: &mut Retyping<'s>,
    view: &'s Projected<'_>,
    scope: Erasures<'_>,
    module: &Module,
    mut over: Option<ErasedOver<'_>>,
) -> Result<ErasedUnit, Error> {
    let mut unit = ErasedUnit::default();
    let mut asked = Traces::default();
    for (place, item) in module.items.iter().enumerate() {
        let names = item.declared_names();
        let kept = over.as_mut().and_then(|over| over.keep(item, &unit));
        let (erased, trace) = match kept {
            Some(kept) => kept,
            None => {
                let read = RefCell::new(Vec::new());
                let externals = |name: &Global| {
                    read.borrow_mut().push(*name);
                    unit.find(name).or_else(|| scope.find(name))
                };
                context.begin_trace(&names);
                let erased = erase_item(context, &externals, view.own(place))?;
                for name in read.take() {
                    context.asks_erased(&name);
                }
                let trace = context.take_trace();
                // One sample for each item erased, with how much its erasure asked.
                curios_profile::sample!("erase::asked", trace.len());

                (Rc::new(erased), Rc::new(trace))
            }
        };
        if let Some(name) = names.first() {
            asked.share(**name, trace);
        }
        unit.push(item, erased);
    }

    Ok(unit.asking(asked))
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
