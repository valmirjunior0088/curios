//! The expression walk: the eraser's state, its dispatch over every form, the names and lets it binds, and the builder plumbing each family of forms shares.
//!
//! The walk produces operands under the operand law rather than terms: [`Outcome::Emitted`] carries the atom a subexpression erased to, [`Outcome::Diverged`] carries the terminator that seals the innermost block when the subexpression provably never yields a value. Every non-atomic computation is bound by the builder at the point the walk reaches it, so evaluation order is statement order by construction.

use {
    super::Eraser,
    crate::{
        AtSorts, Binding, Environment, ErasedItem, Error, Export, Sorts, emitted, register_shape,
    },
    curios_cert::Reader,
    curios_core::Bound,
    curios_utilities::recurse,
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        sync::Arc,
    },
};

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

/// The names a fragment's identities were minted for, by identity: what a refusal of the erased verifier, which holds identities and debug names alone, names a member and a callee by.
#[derive(Debug, Default)]
pub struct ErasedNames {
    pub values: BTreeMap<curios_anf::ValueId, curios_core::Free>,
    pub functions: BTreeMap<curios_anf::FunctionId, curios_core::Free>,
}

impl<'s> Eraser<'s> {
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
    /// The one walk that consumes a mask, which only a nominal declaration has: a structure's fields and a constructor's payload, dropped by the declaration's own signature, read once where its row is registered. A structural type has no declaration to read and drops nothing ([`Eraser::slots`]).
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
            // An instance is its head at the sorts it states: a level carries nothing at run time, and a declaration that leaves sorts open is bound at each of them apart.
            curios_core::Subterm::Instance(instance) => self.erase_name(
                instance.head.unwrap(),
                Sorts::of(&instance.universes.qualities),
            ),
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
                // A binding is a kept slot: a value is erased once and bound, and a proof or a type is bound to its stand-in without being computed (see `kept_operand`). `let _ = /std/Proc/exit(3); …` is neither — an exit is an `Io`, a description, which is built here and performed only where it is bound into the program.
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
