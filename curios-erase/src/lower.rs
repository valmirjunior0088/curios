//! The lowering driver: the entry point, the expression walk, and the binding forms.
//!
//! The walk produces operands under the operand law rather than terms: [`Outcome::Emitted`] carries the atom a subexpression erased to, [`Outcome::Diverged`] carries the terminator that seals the innermost block when the subexpression provably never yields a value. Every non-atomic computation is bound by the builder at the point the walk reaches it, so evaluation order is statement order by construction.

use {
    crate::{
        AtSorts, Binding, Environment, ErasedBaseline, ErasedForms, ErasedItem, ErasedOver,
        ErasedUnit, Erasures, Error, Export, Sorts, declared, emitted, evaluate, link, reduced,
        register_shape,
    },
    curios_cert::Reader,
    curios_core::Bound,
    curios_elab::{Answered, Foreign, Parts, Question, Readers, TraceRecord, Traces},
    curios_utilities::{grown, recurse},
    std::{
        borrow::Cow,
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        rc::Rc,
        sync::Arc,
    },
};

mod binding;

mod function;

mod aggregate;

mod eliminate;

mod recursion;

mod intrinsic;
use intrinsic::*;

mod suspension;
use suspension::*;

mod crossing;

mod typed;

/// What one expression erased to. See the module documentation.
#[derive(Debug)]
pub enum Outcome {
    Emitted(curios_anf::Atom),
    Diverged(curios_anf::Terminator),
}

/// How one item's erasure reads the erased form of another: by a name the other declares, at the sorts it is read at.
pub type Externals<'s> = &'s dyn Fn(&curios_core::Global, Sorts) -> Option<&'s ErasedItem>;

/// How one item's erasure reads a `let` definition off the item that declares it, its own unit's or a unit in scope's: what kind of definition a name is, and its body, which the kernel's environment does not say.
pub type Declared<'s> = &'s dyn Fn(&curios_core::Global) -> Option<&'s curios_core::Definition>;

/// One item's erasure state: the checked builder constructing its fragment and the environment mapping Core names to their operands there.
pub struct Lowering<'s> {
    builder: curios_anf::Builder,
    environment: Environment,
    /// The erased forms of the items this one may name.
    externals: Externals<'s>,
    /// The `let` definitions this one may name, as their items declare them.
    declared: Declared<'s>,
    /// The names the item declares, and the sorts this form of it is read at: an occurrence of one at those sorts is the item's own, bound in this fragment at none.
    own: (Vec<curios_core::Global>, Sorts),
    /// The name and sorts each value the fragment imports stands for, in the order the builder minted them.
    values: Vec<AtSorts>,
    /// The name and sorts each function the fragment imports stands for, in the order the builder minted them.
    functions: Vec<AtSorts>,
    /// The definition each emitted function descends from, innermost last, paired with how many anonymous ones have been minted under it. See [`Lowering::derived_hint`].
    owners: Vec<(String, usize)>,
    /// The family identity of each inductive whose row is being registered right now. An inductive is legitimately recursive, so classifying its fields reaches itself; the identity is minted before that walk begins, so the recursive lookup answers from here rather than re-entering the registration.
    pending_families: BTreeMap<AtSorts, curios_anf::FamilyId>,
    /// The structures whose row is being registered right now. A self-referential structure is uninhabited but elaborates, and unlike an inductive its schema is only decided *after* its fields are classified — so the cycle is cut by declining to name a schema rather than by naming one early.
    in_flight: BTreeSet<AtSorts>,
    /// The type each term was read at ([`Lowering::type_of`]), for the item being erased and the arm being walked. Dropped at every top-level item and before the entry, which is what bounds it, and set aside for each arm.
    types: HashMap<curios_core::Term, curios_core::Term>,
    /// The extent of each computed recursive member's initializer, by the identity it was minted as. The erased verifier refuses a member by that identity after the module holding it is gone, so the frame for the refusal is kept here rather than in the representation, whose nodes are the stored-unit format and carry no extents.
    extents: BTreeMap<curios_anf::ValueId, curios_core::Extent>,
    /// The name each member of a group and each import was minted for, for the same refusal: it knows identities alone.
    names: ErasedNames,
}

/// The names a fragment's identities were minted for, by identity: what a refusal of the erased verifier, which holds identities and debug names alone, names a member and a callee by.
#[derive(Debug, Default)]
pub struct ErasedNames {
    pub values: BTreeMap<curios_anf::ValueId, curios_core::Free>,
    pub functions: BTreeMap<curios_anf::FunctionId, curios_core::Free>,
}

/// Erase the entrypoint body into the entry block of a fragment of its own — [`erase_program`]'s tail. The program's own body owns what it mints, the same way an item owns what its body mints: the entry is emitted as `func/main`, so that is the name its lifted lambdas descend from. The verifier is the rejection point for the recursion classes the language does not admit — an evaluation cycle, an initializer that performs an effect — and hands those back as the refusal; a malformed fragment is an erasure fault, which the verifier panics on rather than returning here.
fn seal_entry(
    mut lowering: Lowering<'_>,
    context: &mut Reader,
    body: &curios_core::Term,
    expected: &curios_core::Term,
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

/// The entrypoint boundary: an `Io(T)` tail is a *description*, and the emitted `func/main` is the one place anything forces one, handing back what it yields — or the suspension it answers, surfaced for the host to wait on and advance ([`Lowering::yield_to_host`]).
///
/// Nothing else in the language may: there is no eliminator from `Io(T)` to `T`, which is what makes every term of non-`Io` type pure by typing. The force is type-directed rather than unconditional so a non-`Io` tail erases unforced — the erasure unit tests state such tails directly. What makes it mandatory in production is `curios-pipeline`, which checks an authored tail against `Io({})` and a test program's against `Io(Bool)`. A program's meaning is the effects its description performs, so the unit it yields is read by no host; a test program's yield is its verdict, which the runner reads ([`ENTRY`](curios_abi::ENTRY)).
fn force_entry(
    lowering: &mut Lowering<'_>,
    context: &mut Reader,
    expected: &curios_core::Term,
    outcome: Outcome,
) -> Result<Outcome, Error> {
    let Outcome::Emitted(description) = outcome else {
        return Ok(outcome);
    };
    if !matches!(
        &*reduced(context, expected)?,
        curios_core::Subterm::Intrinsic(curios_core::Intrinsic::IoType(_))
    ) {
        return Ok(Outcome::Emitted(description));
    }

    let answer = emitted!(lowering.bind(
        None,
        curios_anf::Rhs::Apply {
            callee: description,
            arguments: Vec::new(),
        },
    ));

    let polled = lowering.suspended_poll(context)?;
    Ok(Outcome::Emitted(lowering.yield_to_host(answer, polled)))
}

impl<'s> Lowering<'s> {
    /// A fresh erasure of the form at `sorts` of the item declaring `own`, reading other items' erased forms through `externals` and their `let` definitions through `declared`.
    pub fn over(
        externals: Externals<'s>,
        declared: Declared<'s>,
        (own, sorts): (&[&curios_core::Global], Sorts),
    ) -> Self {
        Self {
            builder: curios_anf::Builder::new(),
            environment: Environment::default(),
            externals,
            declared,
            own: (own.iter().map(|name| **name).collect(), sorts),
            values: Vec::new(),
            functions: Vec::new(),
            owners: Vec::new(),
            pending_families: BTreeMap::new(),
            in_flight: BTreeSet::new(),
            types: HashMap::new(),
            extents: BTreeMap::new(),
            names: ErasedNames::default(),
        }
    }

    /// Close the fragment and hand the item's erased form over: the fragment verified with no entry, its recursion rules included, and of the environment the bindings of `declared`, the names the item declares.
    pub fn into_erased(self, declared: &[&curios_core::Global]) -> Result<ErasedItem, Error> {
        let Self {
            builder,
            environment,
            values,
            functions,
            extents,
            names,
            ..
        } = self;
        let module = builder
            .into_module()
            .map_err(|error| Error::refused_by_verifier(error, &extents, &names))?;

        Ok(ErasedItem::new(
            module,
            environment.exporting(declared),
            values,
            functions,
        ))
    }

    /// The erased form at `name`'s sorts of the item declaring `name`, where another item does.
    pub fn declaring(&self, (name, sorts): AtSorts) -> Option<&'s ErasedItem> {
        (self.externals)(&name, sorts)
    }

    /// The `let` definition `name` as its item declares it, where a `let` declares it.
    pub fn declared(&self, name: &curios_core::Global) -> Option<&'s curios_core::Definition> {
        (self.declared)(name)
    }

    /// `name` at `sorts` as this fragment names it: one the item declares, at the sorts this form is read at, is the item's own, at none.
    pub fn named(&self, name: curios_core::Global, sorts: Sorts) -> AtSorts {
        let (own, at) = &self.own;
        match sorts == *at && own.contains(&name) {
            true => (name, Sorts::NONE),
            false => (name, sorts),
        }
    }

    /// The sorts a declaration this fragment names at `sorts` is read at: the form's own, for one the item declares, named at none.
    pub fn read_at(&self, (name, sorts): AtSorts) -> Sorts {
        let (own, at) = &self.own;
        match sorts.is_none() && own.contains(&name) {
            true => *at,
            false => sorts,
        }
    }

    /// Bind `name`, which another item declares, as this fragment reads it: an erased binding as none, a constant as the constant, and a value or a function as an import, minted the first time the name is used.
    ///
    /// A value whose own initializer reads it is refused here, where it is read: its item could not tell that anything would read it, and this read is what forces it into itself. So the recursion rules are settled for every fragment where it is erased, and a unit that reads such a value of another is refused where it is compiled, the report noting where the value is declared.
    fn external(
        &mut self,
        name: &curios_core::Free,
        sorts: Sorts,
    ) -> Result<Option<Binding>, Error> {
        let Some(global) = name.as_global() else {
            return Ok(None);
        };
        let Some(export) = (self.externals)(global, sorts).and_then(|erased| erased.export(global))
        else {
            return Ok(None);
        };
        let hint = format!("{}{sorts}", global.faithful());
        let binding = match export {
            Export::Unforced => return Err(Error::evaluates_itself(*name)),
            Export::Dropped => Binding::Dropped,
            Export::Constant(constant) => Binding::Atom(curios_anf::Atom::Constant(
                self.builder.constant(constant.clone()),
            )),
            Export::Value => {
                self.values.push((*global, sorts));
                Binding::Atom(curios_anf::Atom::Value(
                    self.builder.import_value(Some(hint)),
                ))
            }
            Export::Function { arity, interface } => {
                self.functions.push((*global, sorts));
                let function = self.builder.import_function(Some(hint), arity, interface);
                self.names.functions.insert(function, *name);
                Binding::Atom(curios_anf::Atom::Function(function))
            }
        };
        match binding {
            Binding::Atom(atom) => self.environment.bind_at(name, sorts, atom),
            Binding::Dropped => self.environment.bind_dropped_at(name, sorts),
        }

        Ok(Some(binding))
    }

    /// Erase `body` with `name` standing as the definition every function minted inside it descends from.
    pub fn with_owner<R>(&mut self, name: String, body: impl FnOnce(&mut Self) -> R) -> R {
        self.owners.push((name, 0));
        let result = body(self);
        self.owners.pop();
        result
    }

    /// A name for a function nothing else names.
    ///
    /// A lambda in argument position binds no statement, so `walk` has no hint to pass and the function it lifts to would print as a bare `~fN` — and its closure as a bare `$clsr/N`. The shared naming scheme (`documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md`) spells an emitted name `kind/{uniquifier}$hint`, so an absent hint is a hole in it, and one a reader of a module dump or a profile pays for. The owner's own name qualified by which anonymous function this is fills it: `/sys/Handle/write/1`, and `/sys/Handle/write/1/1` one level in. The separator is `/` because `$` is reserved for the hint boundary itself.
    pub fn derived_hint(&mut self) -> Option<String> {
        let (owner, minted) = self.owners.last_mut()?;
        *minted += 1;
        Some(format!("{owner}/{minted}"))
    }
    /// Seal the innermost open block: a computed value returns, a divergence keeps its own terminator.
    pub fn seal(&mut self, outcome: Outcome) -> curios_anf::BlockId {
        match outcome {
            Outcome::Emitted(atom) => self
                .builder
                .seal_block(curios_anf::Terminator::Return(atom)),
            Outcome::Diverged(terminator) => self.builder.seal_block(terminator),
        }
    }

    /// Bind a compound right-hand side in the innermost open block (or as a top-level item) and hand back its result operand.
    pub fn bind(&mut self, hint: Option<&str>, rhs: curios_anf::Rhs) -> Outcome {
        let result = self.builder.let_value(hint.map(str::to_string), rhs);
        Outcome::Emitted(curios_anf::Atom::Value(result))
    }

    /// [`bind`](Self::bind) a right-hand side whose producer names no carrier, stating on the result the register carrier its type `type_` names.
    pub fn bind_stated(
        &mut self,
        context: &mut Reader,
        hint: Option<&str>,
        rhs: curios_anf::Rhs,
        type_: &curios_core::Term,
    ) -> Result<Outcome, Error> {
        debug_assert!(
            !rhs.names_its_carrier(),
            "a carrier is stated only where the producer names none"
        );
        let result = self.builder.let_value(hint.map(str::to_string), rhs);
        self.state(context, result, type_)?;
        Ok(Outcome::Emitted(curios_anf::Atom::Value(result)))
    }

    /// State on `value`, a binder whose producer names no carrier, the register carrier its type `type_` names, where it names one.
    pub fn state(
        &mut self,
        context: &mut Reader,
        value: curios_anf::ValueId,
        type_: &curios_core::Term,
    ) -> Result<(), Error> {
        if let Some(register) = register_shape(self, context, type_)? {
            self.builder.state(value, register);
        }
        Ok(())
    }

    /// The unit constant — the value of a retained-but-erased slot.
    pub fn unit(&mut self) -> curios_anf::Atom {
        curios_anf::Atom::Constant(self.builder.constant(curios_anf::Constant::Unit))
    }

    /// Erase each value against its telescope domain under `mask`, opening the telescope with the un-erased value so later dependent domains stay correct. Erasable slots are dropped entirely; kept slots erase through [`kept_operand`](Self::kept_operand).
    ///
    /// The one walk that consumes a mask, which only a nominal declaration has: a structure's fields and a constructor's payload, dropped by the declaration's own signature, read once where its row is registered. A structural type has no declaration to read and drops nothing ([`Lowering::slots`]).
    pub fn masked_fields<B: Bound>(
        &mut self,
        context: &mut Reader,
        mask: &[bool],
        telescope: curios_core::Telescope<B>,
        values: &[curios_core::Term],
    ) -> Result<Result<Vec<curios_anf::Atom>, Outcome>, Error> {
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
    pub fn slots<B: Bound>(
        &mut self,
        context: &mut Reader,
        telescope: curios_core::Telescope<B>,
        values: &[curios_core::Term],
    ) -> Result<Result<Vec<curios_anf::Atom>, Outcome>, Error> {
        self.masked_fields(context, &vec![false; values.len()], telescope, values)
    }

    /// Erase one expression to an operand. `expected` is the type the expression was checked against, consumed where a runtime shape must be read off it; `hint` names the statement when this expression binds one.
    pub fn walk(
        &mut self,
        context: &mut Reader,
        term: &curios_core::Term,
        expected: &curios_core::Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        // With room to recurse: an operand is walked inside the expression that holds it, so a nesting of calls is a nesting of walks.
        let result = recurse(|| self.walk_subterm(context, term, expected, hint));
        // Attach this term's extent to any error from erasing it.
        match term.extent() {
            Some(extent) => result.map_err(|error| error.at(extent)),
            None => result,
        }
    }

    fn walk_subterm(
        &mut self,
        context: &mut Reader,
        term: &curios_core::Term,
        expected: &curios_core::Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        match &**term {
            curios_core::Subterm::Intrinsic(intrinsic) => {
                self.erase_intrinsic(context, intrinsic, hint)
            }
            // A host call: each operand erases against the demand elaboration checked it with, `foreign_operands`' reading of the same row. A diverging call's type operand erases with every other type.
            curios_core::Subterm::Foreign(function, _, arguments) => {
                let signature = function.signature();
                let results = signature.returns.types();
                // An option crosses as its element's list would, so one the row takes or answers is converted beside the call, at the instance the row states for it.
                let crossed = results
                    .iter()
                    .map(|wire| self.crossed(context, wire))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut wires = signature.params.iter();
                let mut atoms = Vec::with_capacity(arguments.len());
                let syntax = context.syntax();
                for (argument, demand) in arguments
                    .iter()
                    .zip(curios_core::foreign_operands(function, &syntax))
                {
                    match demand {
                        curios_core::Operand::At(type_) => {
                            let atom = emitted!(self.walk(context, argument, &type_, None)?);
                            let wire = wires
                                .next()
                                .expect("an operand the row demands is one of its parameters");

                            atoms.push(match self.crossed(context, wire)? {
                                Some(polled) => self.option_crossing(atom, polled),
                                None => atom,
                            });
                        }
                        curios_core::Operand::IsType => {}
                        curios_core::Operand::Function { .. } => {
                            unreachable!("a host call demands no function operand")
                        }
                    }
                }
                let foreign = self.builder.foreign(Arc::clone(function));
                let described = format!("io/{}", function.name());

                // A diverging call never yields a value, so the thunk's block is sealed by the terminator rather than by a return. Code after the *force* is dead; code after the construction is not.
                if function.diverges() {
                    return self.thunk(hint.or(Some(described.as_str())), move |_| {
                        Ok(Outcome::Diverged(curios_anf::Terminator::Halt {
                            foreign,
                            operands: atoms,
                        }))
                    });
                }
                Ok(Outcome::Emitted(self.lane_call(
                    foreign,
                    atoms,
                    &signature.params,
                    &results,
                    &crossed,
                    hint.unwrap_or(described.as_str()),
                )))
            }
            // Type formers carry nothing to lower; their value is the unit of a retained-but-erased slot.
            curios_core::Subterm::Sort(..)
            | curios_core::Subterm::FuncType(_)
            | curios_core::Subterm::TupleType(_)
            | curios_core::Subterm::InductType(_)
            | curios_core::Subterm::StructType(_) => Ok(Outcome::Emitted(self.unit())),
            // An instance is its head at the sorts it states: a level carries nothing at run time, and a declaration that leaves sorts open is bound at each of them apart. A group's member reads its group, which leaves no sort of its own.
            curios_core::Subterm::Instance(instance) => match &instance.head {
                curios_core::InstanceHead::Var(var) => {
                    self.erase_name(var.unwrap(), Sorts::of(&instance.universes.qualities))
                }
                curios_core::InstanceHead::RecProj(group, index) => self.walk(
                    context,
                    &curios_core::Term::rec_proj(group.clone(), *index),
                    expected,
                    hint,
                ),
            },
            curios_core::Subterm::Var(var) => self.erase_name(var.unwrap(), Sorts::NONE),
            curios_core::Subterm::Let(binding) => self.erase_let(context, binding, expected, hint),
            curios_core::Subterm::Match(m) => self.erase_match(context, m, hint),
            curios_core::Subterm::Variant(variant) => self.erase_variant(context, variant, hint),
            curios_core::Subterm::Struct(value) => self.erase_struct(context, value, hint),
            curios_core::Subterm::Tuple(tuple) => self.erase_tuple(context, tuple, expected, hint),
            curios_core::Subterm::Proj(proj) => self.erase_proj(context, proj, expected, hint),
            curios_core::Subterm::Func(func) => self.erase_func(context, func, expected, hint),
            curios_core::Subterm::Apply(apply) => self.erase_apply(context, apply, expected, hint),
            curios_core::Subterm::Rec(rec) => self.erase_rec(context, rec, expected, hint),
            // Erasure runs downstream of zonking and elaboration.
            curios_core::Subterm::Metavar(_) => {
                unreachable!("metavariable survived zonking into erasure")
            }
            curios_core::Subterm::Transient(_) => {
                unreachable!("transient node survived elaboration into erasure")
            }
        }
    }

    /// The operand `name`, read at `sorts`, is bound to: a binder's or one the item declares, and otherwise what another item binds it to, imported the first time it is read.
    fn erase_name(&mut self, name: &curios_core::Free, sorts: Sorts) -> Result<Outcome, Error> {
        let sorts = match name.as_global() {
            Some(global) => self.named(*global, sorts).1,
            None => sorts,
        };
        let binding = match self.environment.lookup(name, sorts) {
            Some(binding) => binding,
            None => self.external(name, sorts)?.unwrap_or_else(|| {
                unreachable!("erase: unbound variable {}{sorts}", name.faithful())
            }),
        };

        Ok(Outcome::Emitted(match binding {
            Binding::Atom(atom) => atom,
            Binding::Dropped => self.unit(),
        }))
    }

    /// Erase a let block binding for binding, in written order: each value is erased once (the operand law), defined in the Core context so dependent types reduce through it, and mapped to its operand; then the tail.
    fn erase_let(
        &mut self,
        context: &mut Reader,
        binding: &curios_core::Let,
        expected: &curios_core::Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        context.scoped(|context| {
            let mut label_terms = Vec::<curios_core::Term>::with_capacity(binding.bindings.len());

            for (index, local) in binding.bindings.iter().enumerate() {
                let (type_, value) = {
                    let refs = label_terms.iter().collect::<Vec<_>>();
                    (local.type_().release(&refs), local.value().release(&refs))
                };

                // The arena identity uniquifies by index, so the hint stays the clean source label; the `#`-uniquified fresh name is only the Core context key.
                let label = binding.tail.hint_iter().nth(index).flatten();
                let hint = label.map(str::to_string);
                let name = context.fresh(label);
                // A binding is a kept slot: a value is erased once and bound, and a proof or a type is bound to its stand-in without being computed (see `kept_operand`). `let _ = /std/Process/exit(3); …` is neither — an exit is an `Io`, a description, which is built here and performed only where it is bound into the program.
                let outcome = self.kept_operand(context, &value, &type_, hint.as_deref())?;
                let atom = emitted!(outcome);
                context.bind(&name, &type_, &value);
                self.environment.bind(&name, atom);
                label_terms.push(curios_core::Term::free_var(&name));
            }

            let tail = binding.tail.open(&label_terms.iter().collect::<Vec<_>>());
            self.walk(context, &tail, expected, hint)
        })
    }
}

/// A program erased and linked: one module holding what its entry reaches, and where the function each linked declaration defines landed.
#[derive(Debug)]
pub struct ErasedProgram {
    module: curios_anf::Module,
    /// The function each name that defines one is bound to, at the sorts of the form it was linked as. A function an item computed is bound under a name and defined by none, so it is not among them.
    defined: BTreeMap<AtSorts, curios_anf::FunctionId>,
}

impl ErasedProgram {
    pub(super) fn new(
        module: curios_anf::Module,
        defined: BTreeMap<AtSorts, curios_anf::FunctionId>,
    ) -> Self {
        Self { module, defined }
    }

    /// Mark every function a definition `certification` classifies `Total` erased to as total, at every form — see [`curios_anf::Function::total`]. The kernel judged the definition at each assignment of its sorts, so its totality holds of each form. Each is found through where its name was linked, so a definition that erased to anything but a function, or that the program does not reach, has nothing to mark, and a function no record names keeps the conservative `false`: a closure a top-level value was computed to among them, which its name is bound to and no declaration defines.
    ///
    /// Run on the linked program, once per record of what it was erased from: every unit's in scope and the entry's own. Nothing reads the flag before the back half lowers what this leaves.
    ///
    /// **Asked by the names the program linked, never walked.** A record classifies every definition of its unit and a program links a few of them, so each linked name is asked of the record, and the record is read for what the program took of its unit.
    pub fn mark_total(&mut self, certification: &curios_core::Certification) {
        let total = self
            .defined
            .iter()
            .filter(|((name, _), _)| {
                certification
                    .totality(name)
                    .is_some_and(|totality| totality.is_total())
            })
            .map(|(_, function)| *function)
            .collect::<Vec<_>>();
        for id in total {
            self.module.mark_total(id);
        }
    }

    /// The finished erased module — what the back half of the pipeline lowers.
    pub fn into_module(self) -> curios_anf::Module {
        self.module
    }
}

/// Erase one unit's items through `kernel`, the kernel that certified the unit, each into its forms, over what its scope erased to.
///
/// What every reading reads is the kernel's: the unit's definitions as its walk defined them and the units in scope it mounted, each item read at each assignment of the sorts it leaves open, as the walk judged it. The items erase in the order the module stores them, every item after the items it reaches — so the erased form of every name an item uses is in hand when it is asked for.
///
/// **A unit and a program are erased by one walk, and only a program is linked.** The entry is not a unit's to carry: a program is its module and the term it closes with ([`Program`](curios_core::Program)), so [`erase_program`] runs this same walk, erases the entry at the type it states, which [`Zonked`](curios_core::Zonked) holds, and links what the entry reaches.
pub fn erase_unit(
    kernel: &mut curios_cert::Kernel<'_>,
    scope: Erasures<'_>,
    module: &curios_core::Zonked<curios_core::Module>,
) -> Result<ErasedUnit, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = module.as_module();
        let places = curios_core::Places::of(module);
        kernel.read(|reader| erase_items(reader, scope, (module, &places), None))
    })
}

/// [`erase_unit`] for a unit compiled over a baseline: an item keeps the erased forms `baseline` has for it, the baseline's own and no copy of them, where the elaborator kept its declaration and everything its erasure asked there is answered as it was (`over`), and every other is erased. The walk visits the items an edit could reach and no other; where the module is a version of the baseline's, the unit is a version of the baseline's unit.
pub fn erase_unit_over(
    kernel: &mut curios_cert::Kernel<'_>,
    scope: Erasures<'_>,
    module: &curios_core::Zonked<curios_core::Module>,
    baseline: ErasedBaseline<'_>,
) -> Result<ErasedUnit, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = module.as_module();
        let over = ErasedOver::new(baseline, module, scope);
        kernel.read(|reader| erase_items(reader, scope, (module, baseline.now), Some(over)))
    })
}

/// [`erase_unit`] for a program: its module erased over its scope through `kernel`, the kernel that certified the program, its entry erased at the type it states, and the two linked with what they reach of the scope — the module whose `ErasedProgram::into_module` the back half lowers.
pub fn erase_program(
    kernel: &mut curios_cert::Kernel<'_>,
    scope: Erasures<'_>,
    program: &curios_core::Zonked<curios_core::Program>,
) -> Result<ErasedProgram, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = &program.as_program().module;
        let places = curios_core::Places::of(module);
        let (body, type_) = program.entry();
        kernel.read(|reader| {
            let own = erase_items(reader, scope, (module, &places), None)?;
            let entry = {
                let externals = |name: &curios_core::Global, sorts: Sorts| {
                    own.find(name, sorts).or_else(|| scope.find(name, sorts))
                };
                let declared = |name: &curios_core::Global| {
                    declared(module, &places, name).or_else(|| scope.definition(name))
                };
                reader.restore_budget();
                seal_entry(
                    Lowering::over(&externals, &declared, (&[], Sorts::NONE)),
                    reader,
                    body,
                    type_,
                )?
            };

            link(scope, &own, &entry)
        })
    })
}

/// Erase `module`'s items in the order it stores them, each into its forms, but for those `over` keeps the baseline's erased forms of, which are not read. `places` says where `module` holds each name. What erasing each item asked is kept with the unit: of the declarations around it, through the kernel's reads, of each erased form it read how a name is bound in, and of each erased form its evaluation ran, the form whole.
///
/// **Over a baseline whose module this one is a version of, the unit begins as the baseline's**, every item standing where the baseline holds it with what its erasure asked, and the walk visits the items an edit could reach alone ([`ErasedOver`]); over any other, it is made item by item.
pub fn erase_items(
    reader: &mut Reader,
    scope: Erasures<'_>,
    (module, places): (&curios_core::Module, &curios_core::Places),
    mut over: Option<ErasedOver<'_>>,
) -> Result<ErasedUnit, Error> {
    let shared = over.as_ref().and_then(|over| {
        over.changes()
            .map(|changes| (over.baseline(), changes.clone()))
    });
    let (mut unit, mut asked) = match &shared {
        Some((baseline, changes)) => {
            // What the baseline held where the module holds another item or none is taken out, names and all: every item the module holds anew is one the walk visits, made again by the elaborator.
            let mut unit = baseline.erased.version();
            for place in changes.replaced.iter().chain(&changes.removed) {
                unit.forget(*place, &baseline.core.items.names(*place));
            }

            (unit, baseline.erased.traces().clone())
        }
        None => (ErasedUnit::default(), Traces::default()),
    };
    if shared.is_some()
        && let Some(over) = &over
    {
        for name in over.lost() {
            asked.forget(name);
        }
    }
    let walk = match (&over, &shared) {
        (Some(over), Some(_)) => over.visited().collect::<Vec<_>>(),
        _ => module.items.places().collect(),
    };
    // The first name of each item erased here, whose trace is its own, and every other name such an item declares.
    let (mut made, mut others) = (Vec::new(), Vec::new());
    for place in walk {
        let item = &module.items[place];
        let names = item.declared_names();
        let kept = over.as_mut().and_then(|over| over.keep(place, item, &unit));
        let taken = match kept {
            Some(_) => None,
            None => over
                .as_ref()
                .and_then(|over| over.taken(place, item, &unit)),
        };
        let record = match (kept, taken) {
            (Some((held, trace)), _) => {
                unit.keep(place, item, held);
                if let Some(name) = names.first() {
                    asked.share(**name, trace);
                }
                continue;
            }
            (None, Some((held, record))) => {
                curios_profile::sample!("erase::taken", record.len());
                unit.keep(place, item, held);

                record
            }
            (None, None) => {
                let (erased, trace) = erase_forms(reader, scope, &unit, (module, places), item)?;
                // One sample for each item erased, with how much its erasure asked.
                curios_profile::sample!("erase::asked", trace.len());
                unit.push(place, item, erased);

                Rc::new(TraceRecord::new(trace))
            }
        };
        made.extend(names.first().map(|name| **name));
        // A trace is filed under the first name, so one the baseline filed under another is a declaration this one now holds the name of.
        for name in names.iter().skip(1) {
            asked.forget(name);
            others.push(**name);
        }
        if let Some(name) = names.first() {
            asked.share(**name, record);
        }
    }
    // Who asked what of the unit's traces: the baseline's, with the edges of every item erased again, or no longer held, replaced.
    let readers = over.as_ref().map(|over| {
        let earlier = over.earlier();
        let mut readers = earlier.readers().clone();
        for name in made.iter().chain(&others).chain(over.lost()) {
            readers.replace(
                *name,
                earlier.asked(name).map(|held| &***held),
                asked.held(name).map(|held| &***held),
            );
        }

        readers
    });
    match (&over, &readers) {
        (Some(over), Some(readers)) => {
            revise_foreign(&mut asked, &unit, scope, over, &made, &others, readers);
        }
        _ => note_foreign(&mut asked, &unit, scope),
    }
    let unit = unit.asking(asked);

    Ok(match readers {
        Some(readers) => unit.reading(readers),
        None => unit,
    })
}

/// Record what erasing `unit`'s items read of declarations the unit does not hold, each part and each binding by the digest it has in `scope`: what a later erasure over this unit asks a read of another unit again by (`Foreign`). What will not digest is recorded nowhere, and a read with no record does not stand.
fn note_foreign(asked: &mut Traces, unit: &ErasedUnit, scope: Erasures<'_>) {
    let mut read = Foreign::default();
    for trace in asked.each() {
        for part in foreign_parts(trace, unit, scope) {
            if read.digest(&part).is_none()
                && let Ok(digest) = scope.part_digest(&part)
            {
                read.record(part, digest);
            }
        }
    }
    asked.read(Rc::new(read));
}

/// [`note_foreign`] for an erasure over a baseline, as elaboration revises its own: the baseline's table, revised where an item was erased again or dropped ([`Foreign::revised`]). A part a kept form's erasure read of a unit that moved digests to what it did, or the form would have been erased again, so its record stands; and an erasure that erased and dropped nothing, in a scope that did not move, holds the baseline's table.
fn revise_foreign(
    asked: &mut Traces,
    unit: &ErasedUnit,
    scope: Erasures<'_>,
    over: &ErasedOver<'_>,
    made: &[curios_core::Global],
    others: &[curios_core::Global],
    readers: &Readers,
) {
    let (earlier, baseline) = (over.earlier(), over.baseline());
    if scope.settled() && made.is_empty() && over.lost().is_empty() {
        asked.read(Rc::clone(earlier.foreign()));
        return;
    }

    let read = made
        .iter()
        .filter_map(|name| asked.held(name))
        .flat_map(|trace| foreign_parts(trace, unit, scope))
        .collect::<Vec<_>>();
    let dropped = made
        .iter()
        .chain(others)
        .chain(over.lost())
        .filter_map(|name| earlier.asked(name))
        .flat_map(|trace| foreign_parts(trace, unit, scope))
        .collect::<Vec<_>>();
    // A name the unit declares now and the baseline did not is read of the unit, no other.
    let declared = baseline
        .made
        .iter()
        .filter(|name| baseline.places.place(name).is_none())
        .flat_map(|name| Parts::ALL.questions(*name));
    let revised = earlier.foreign().revised(
        read,
        dropped,
        declared,
        |part| part.subject().is_some_and(|name| !scope.unmoved(name)),
        |part| scope.part_digest(part),
        |part| readers.asks(part),
    );
    asked.read(Rc::new(revised));
}

/// The parts `trace` reads of declarations `unit` does not hold, as the table of them records each: every part asked; none of a declaration of a fixed unit.
fn foreign_parts<'t>(
    trace: &'t curios_core::Trace<Question, Answered>,
    unit: &'t ErasedUnit,
    scope: Erasures<'t>,
) -> impl Iterator<Item = Question> + 't {
    trace
        .asked()
        .iter()
        .filter_map(move |(question, answered)| {
            let name = question.subject()?;

            (*answered == Answered::Part && unit.place(name).is_none() && scope.recorded(name))
                .then_some(*question)
        })
}

/// Erase `item` into a form at each assignment of the sorts it leaves to its caller, each read at its assignment as the kernel judged it, with what erasing them asked between them.
fn erase_forms(
    reader: &mut Reader,
    scope: Erasures<'_>,
    unit: &ErasedUnit,
    (module, places): (&curios_core::Module, &curios_core::Places),
    item: &curios_core::Item,
) -> Result<(ErasedForms, curios_core::Trace<Question, Answered>), Error> {
    let names = item.declared_names();
    let mut asked = Asked::of(&names);
    let mut forms = Vec::new();
    for sorts in Sorts::every(left_open(item)) {
        let (erased_read, declared_read) = (RefCell::new(Vec::new()), RefCell::new(Vec::new()));
        let externals = |name: &curios_core::Global, sorts: Sorts| {
            erased_read.borrow_mut().push(*name);
            unit.find(name, sorts).or_else(|| scope.find(name, sorts))
        };
        let declared = |name: &curios_core::Global| {
            declared_read.borrow_mut().push(*name);
            declared(module, places, name).or_else(|| scope.definition(name))
        };
        reader.restore_budget();
        reader.take_reads();
        let erased = erase_item(
            reader,
            Lowering::over(&externals, &declared, (&names, sorts)),
            &at_sorts(item, sorts),
        )?;
        asked.read(reader.take_reads(), declared_read.take());
        for name in erased_read.take() {
            asked.note(Question::Erased(name));
        }
        // What the item's top-level statements compute is computed here, and its trace says what that ran whether or not it could be finished.
        let erased = match evaluate(scope, unit, (&erased, sorts)) {
            Some(evaluation) => {
                for name in &evaluation.ran {
                    asked.note(Question::Ran(*name));
                }
                match evaluation.erased {
                    Some(evaluated) => {
                        for (name, _) in evaluated.environment().imported_rows() {
                            asked.note(Question::Erased(*name));
                        }

                        evaluated
                    }
                    None => erased,
                }
            }
            None => erased,
        };
        forms.push(erased);
    }
    let forms = ErasedForms::of(forms)
        .unwrap_or_else(|error| panic!("an erased form failed to archive: {error}"));

    Ok((forms, asked.trace))
}

/// How many sorts `item` leaves to its caller: its definition's or its group's.
fn left_open(item: &curios_core::Item) -> usize {
    universe_context(item).quality_count
}

/// The universe context `item` is judged under.
fn universe_context(item: &curios_core::Item) -> &curios_core::UniverseContext {
    match item {
        curios_core::Item::Let(definition) => &definition.universe_context,
        curios_core::Item::Rec(rec) => rec.group.universe_context(),
    }
}

/// `item` at the assignment `sorts` of the sorts it leaves open, as the kernel judges it there: every term it holds read at the assignment, and no sort left. An item that leaves none, as it stands.
fn at_sorts(item: &curios_core::Item, sorts: Sorts) -> Cow<'_, curios_core::Item> {
    if sorts.is_none() {
        return Cow::Borrowed(item);
    }
    let qualities = sorts.qualities();

    Cow::Owned(match item {
        curios_core::Item::Let(definition) => {
            let mut at = curios_core::Definition::clone(definition);
            at.type_ = curios_core::instantiate_qualities(&at.type_, &qualities);
            at.body = curios_core::instantiate_qualities(&at.body, &qualities);
            at.value_sort = at
                .value_sort
                .map(|sort| curios_core::instantiate_qualities(&sort, &qualities));
            at.universe_context.quality_count = 0;

            curios_core::Item::Let(Box::new(at))
        }
        curios_core::Item::Rec(rec) => curios_core::Item::Rec(curios_core::RecItem {
            definitions: rec.definitions.clone(),
            group: rec.group.at_qualities(&qualities),
        }),
    })
}

/// What erasing an item's forms asked, each question once, in the order first asked: the kernel's reads of the declarations around it read as the parts they are, the kinds and bodies erasure read off their items, and the erased forms it read. Nothing asked of a name the item declares is kept, as the item's own.
struct Asked<'a> {
    own: &'a [&'a curios_core::Global],
    noted: HashSet<Question>,
    trace: curios_core::Trace<Question, Answered>,
}

impl<'a> Asked<'a> {
    fn of(own: &'a [&'a curios_core::Global]) -> Self {
        Self {
            own,
            noted: HashSet::new(),
            trace: curios_core::Trace::default(),
        }
    }

    fn note(&mut self, question: Question) {
        let own = question
            .subject()
            .is_some_and(|name| self.own.contains(&name));
        if !own && self.noted.insert(question) {
            self.trace.record(question, Answered::Part);
        }
    }

    /// What the kernel read while erasure read through it, and the definitions erasure read the kind and body of. The kernel's record says a declaration's signature or its body was read, coarser than the parts a recompile digests, so a signature read asks every part a reading of one could have met.
    fn read(&mut self, reads: curios_core::Reads, declared: Vec<curios_core::Global>) {
        for name in reads.signatures {
            for question in [
                Question::Signature(name),
                Question::Sort(name),
                Question::Kind(name),
                Question::Induct(name),
                Question::Struct(name),
            ] {
                self.note(question);
            }
        }
        for name in reads.bodies {
            self.note(Question::Body(name));
        }
        for name in declared {
            self.note(Question::Kind(name));
            self.note(Question::Body(name));
        }
    }
}

/// Erase one item, at one assignment of its sorts, into a form of its own: on a budget restored for it and under the constraints its universe context states.
fn erase_item(
    reader: &mut Reader,
    mut lowering: Lowering<'_>,
    item: &curios_core::Item,
) -> Result<ErasedItem, Error> {
    reader.assume_universes(universe_context(item));
    match item {
        curios_core::Item::Let(definition) => lowering.erase_definition(reader, definition)?,
        curios_core::Item::Rec(rec) => lowering.erase_rec_item(reader, rec)?,
    }
    // The row of each registry entry the item declares is registered here, whether or not anything of the item constructs or matches it: every other item holds the row as an import of this one.
    for name in item.declared_names() {
        if reader.struct_decl(name).is_some() {
            lowering.struct_row(reader, (*name, Sorts::NONE))?;
        }
        if reader.induct_decl(name).is_some() {
            lowering.induct_family(reader, (*name, Sorts::NONE))?;
        }
    }

    lowering.into_erased(&item.declared_names())
}
