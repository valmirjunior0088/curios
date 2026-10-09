//! Taking the fragment linked last back out of a program.
//!
//! Linking writes a fragment into a program under fresh identities; this writes what stands past a [`Mark`] into a module of its own under fresh identities, by the same walk. What is past the mark is the fragment linked there with everything appended for it since, which is what an evaluation of its items leaves: the constructions and the copied closures it materialized stand after the fragment, and nothing before the mark was rewritten.
//!
//! **What it names of the rest of the program is an import.** A value or a function before the mark is one another fragment bound. Whoever takes the module says which of them a module may name, and under what interface a function is applied; a suffix that names any other is not taken, since nothing a module holds could stand for it.
//!
//! **A schema is held as an import holds one**: a product at its width, a family with each constructor at its width, and nothing of a field, which is the row's declaration to say. So a suffix that declares a row is not what this takes; whoever calls it knows which fragments declare one.

#[cfg(test)]
mod tests;

use {
    super::{Landing, Linked, Mark},
    crate::{
        Atom, BlockId, CellOperation, ChannelOperation, ConstantId, ConstructorId, FamilyId, Field,
        ForeignId, FunctionId, Interface, Module, ProductId, ProductSchema, Rhs, Statement, Tables,
        Terminator, ValueId,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// A module taken out of a program — see [`Module::taken`].
#[derive(Debug)]
pub struct Taken {
    pub module: Module,
    /// Where each identity of the program the module holds landed in it: what stood past the mark, what it imports, and the constants and schemas it names.
    pub landed: Linked,
    /// The program's value each value the module imports stands for, in the module's order.
    pub values: Vec<ValueId>,
    /// The program's function each function the module imports stands for, in the module's order.
    pub functions: Vec<FunctionId>,
}

/// Identities in the order each was first named.
struct Firsts<I> {
    order: Vec<I>,
    seen: BTreeSet<I>,
}

impl<I> Default for Firsts<I> {
    fn default() -> Self {
        Self {
            order: Vec::new(),
            seen: BTreeSet::new(),
        }
    }
}

impl<I: Ord + Copy> Firsts<I> {
    fn note(&mut self, id: I) {
        if self.seen.insert(id) {
            self.order.push(id);
        }
    }
}

/// What a suffix names of the module it stands in: the values and functions before its mark, and the constants, foreign rows and schemas of the module's tables.
#[derive(Default)]
struct Named {
    values: Firsts<ValueId>,
    functions: Firsts<FunctionId>,
    constants: Firsts<ConstantId>,
    foreigns: Firsts<ForeignId>,
    products: Firsts<ProductId>,
    families: Firsts<FamilyId>,
}

impl Named {
    fn atom(&mut self, mark: &Mark, atom: Atom) {
        match atom {
            Atom::Value(value) if value.index() < mark.values => self.values.note(value),
            Atom::Function(function) if function.index() < mark.functions => {
                self.functions.note(function);
            }
            Atom::Constant(constant) => self.constants.note(constant),
            Atom::Value(_) | Atom::Function(_) => {}
        }
    }

    fn constructor(&mut self, module: &Module, constructor: ConstructorId) -> Option<()> {
        self.families.note(module.constructor(constructor)?.family);

        Some(())
    }

    /// The tables' identities `rhs` holds. Every variant is spelled, so one that gains an identity is met here as [`Remap::rhs`](crate::Remap::rhs) meets it.
    fn tables(&mut self, module: &Module, rhs: &Rhs) -> Option<()> {
        match rhs {
            Rhs::Product { schema, .. } | Rhs::Project { schema, .. } => {
                self.products.note(*schema);
            }
            Rhs::Construct { constructor, .. } => self.constructor(module, *constructor)?,
            Rhs::MatchVariant { family, arms, .. } => {
                self.families.note(*family);
                for arm in arms {
                    self.constructor(module, arm.constructor)?;
                }
            }
            Rhs::Cell { operation, .. } => match *operation {
                CellOperation::Poll { some, none } => {
                    self.constructor(module, some)?;
                    self.constructor(module, none)?;
                }
                CellOperation::New | CellOperation::Fill => {}
            },
            Rhs::Channel { operation, .. } => match *operation {
                ChannelOperation::Push {
                    taken,
                    full,
                    closed,
                } => {
                    self.constructor(module, taken)?;
                    self.constructor(module, full)?;
                    self.constructor(module, closed)?;
                }
                ChannelOperation::Take { item, empty, ended } => {
                    self.constructor(module, item)?;
                    self.constructor(module, empty)?;
                    self.constructor(module, ended)?;
                }
                ChannelOperation::New
                | ChannelOperation::Close
                | ChannelOperation::Closed
                | ChannelOperation::Count
                | ChannelOperation::Capacity => {}
            },
            Rhs::Foreign { foreign, .. } => self.foreigns.note(*foreign),
            Rhs::Alias(_)
            | Rhs::Apply { .. }
            | Rhs::Operation { .. }
            | Rhs::Sequence { .. }
            | Rhs::SwitchBool { .. }
            | Rhs::SwitchNat { .. }
            | Rhs::FoldNat { .. }
            | Rhs::FoldSequence { .. }
            | Rhs::UnconsSequence { .. }
            | Rhs::Intrinsic { .. } => {}
        }

        Some(())
    }
}

impl Module {
    /// What this module names past `mark`, or `None` where something past it is owned by what stands before it: a suffix is taken whole, so a statement, a block, a binder or a group it holds is its own.
    fn named(&self, mark: &Mark, exports: &[Atom]) -> Option<Named> {
        let mut named = Named::default();
        let value = |value: &ValueId| value.index() >= mark.values;
        let block = |block: &BlockId| block.index() >= mark.blocks;
        let function = |function: &FunctionId| function.index() >= mark.functions;

        for statement in self.statements().iter().skip(mark.statements).flatten() {
            let own = match statement {
                Statement::Let { result, rhs } => {
                    for atom in rhs.operands() {
                        named.atom(mark, atom);
                    }
                    named.tables(self, rhs)?;

                    value(result)
                        && rhs.binders().iter().all(value)
                        && rhs.sub_blocks().iter().all(block)
                }
                Statement::Functions { functions } => functions.iter().all(function),
                Statement::Rec { group } => group.index() >= mark.rec_groups,
            };
            if !own {
                return None;
            }
        }
        for held in self.blocks().iter().skip(mark.blocks).flatten() {
            for atom in held.terminator.atoms() {
                named.atom(mark, atom);
            }
            if let Terminator::Halt { foreign, .. } = &held.terminator {
                named.foreigns.note(*foreign);
            }
            if !held
                .statements
                .iter()
                .all(|statement| statement.index() >= mark.statements)
            {
                return None;
            }
        }
        for held in self.functions().iter().skip(mark.functions).flatten() {
            if !held.params.iter().all(value) || !block(&held.body) {
                return None;
            }
        }
        for group in self.rec_groups().iter().skip(mark.rec_groups).flatten() {
            let own = group.functions.iter().all(function)
                && group
                    .values
                    .iter()
                    .all(|member| value(&member.value) && block(&member.init));
            if !own {
                return None;
            }
        }
        if !self.items()[mark.items..]
            .iter()
            .all(|item| item.index() >= mark.statements)
        {
            return None;
        }
        for &atom in exports {
            named.atom(mark, atom);
        }

        Some(named)
    }

    /// What `atom` stands for through the aliases among the items past `mark`: a value one of them binds as an alias of another atom is that atom, and so on to an atom none of them binds as one.
    ///
    /// What an evaluation leaves of an item it finished is an alias of what the item computed, and whoever names the item's value means that.
    pub fn aliased(&self, mark: &Mark, atom: Atom) -> Atom {
        let aliases = |value: ValueId| {
            self.items()[mark.items..]
                .iter()
                .find_map(|&item| match self.statement(item) {
                    Some(Statement::Let {
                        result,
                        rhs: Rhs::Alias(target),
                    }) if *result == value => Some(*target),
                    _ => None,
                })
        };

        let mut atom = atom;
        // A value is bound once and an alias names what is bound before it, so the chain ends.
        while let Atom::Value(value) = atom
            && let Some(target) = aliases(value)
        {
            atom = target;
        }

        atom
    }

    /// What this module holds at or past `mark`, as a module of its own under fresh identities: its items those past the mark, in their order, every value and function before the mark that it names an import, and every schema it names held as an import holds one. `exports` are atoms of this module the one taken is to be able to name besides, which its names are bound to.
    ///
    /// `value` says whether a value before the mark is one a module may import, and `function` under what interface a function before the mark is applied, `None` where it is none a module may import. `None` where the suffix names a value or a function that cannot be imported, or holds something that what stands before the mark owns.
    ///
    /// The module is not verified here: whoever takes it holds it to what a fragment is held to ([`Module::verify_fragment`]), as whoever builds one does.
    pub fn taken(
        &self,
        mark: &Mark,
        exports: &[Atom],
        value: impl Fn(ValueId) -> bool,
        function: impl Fn(FunctionId) -> Option<Interface>,
    ) -> Option<Taken> {
        let named = self.named(mark, exports)?;
        let mut module = Module::new();

        let mut substitution = BTreeMap::new();
        for &imported in &named.values.order {
            if !value(imported) {
                return None;
            }
            let debug_name = self.value(imported)?.debug_name.clone();
            substitution.insert(imported, Atom::Value(module.import_value(debug_name)));
        }
        let mut functions = BTreeMap::new();
        for &imported in &named.functions.order {
            let held = self.function(imported)?;
            let stub = module.import_function(
                held.debug_name.clone(),
                held.params.len(),
                function(imported)?,
            );
            functions.insert(imported, stub);
        }

        let mut tables = Tables::default();
        for &constant in &named.constants.order {
            let interned = module.intern_constant(self.constant(constant)?.clone());
            tables.constants.insert(constant, interned);
        }
        for &foreign in &named.foreigns.order {
            let interned = module.intern_foreign(Arc::clone(self.foreign(foreign)?));
            tables.foreigns.insert(foreign, interned);
        }
        for &product in &named.products.order {
            let held = self.product(product)?;
            let registered = module.add_product(ProductSchema {
                debug_name: held.debug_name.clone(),
                fields: vec![Field::opaque(None); held.width()],
                shared: held.shared,
            });
            tables.products.insert(product, registered);
        }
        for &family in &named.families.order {
            let held = self.family(family)?;
            let registered = module.add_family(held.debug_name.clone());
            for &constructor in &held.constructors {
                let row = self.constructor(constructor)?;
                let landed = module.add_constructor(
                    registered,
                    row.debug_name.clone(),
                    vec![Field::opaque(None); row.width()],
                );
                tables.constructors.insert(constructor, landed);
            }
            tables.families.insert(family, registered);
        }

        // Every identity first, in this module's order, and contents after, as a fragment is linked.
        let mut values = BTreeMap::new();
        for (index, slot) in self.values().iter().enumerate().skip(mark.values) {
            if let Some(definition) = slot {
                let minted = module.add_value(definition.debug_name.clone());
                values.insert(ValueId(index as u32), minted);
            }
        }
        for (index, slot) in self.functions().iter().enumerate().skip(mark.functions) {
            if slot.is_some() {
                functions.insert(FunctionId(index as u32), module.reserve_function());
            }
        }
        let mut blocks = BTreeMap::new();
        for (index, slot) in self.blocks().iter().enumerate().skip(mark.blocks) {
            if slot.is_some() {
                blocks.insert(BlockId(index as u32), module.reserve_block());
            }
        }

        module.write(
            self,
            mark,
            &Landing {
                values: &values,
                blocks: &blocks,
                functions: &functions,
                substitution: &substitution,
                tables: &tables,
            },
        );

        let Tables {
            constants,
            products,
            families,
            ..
        } = tables;

        Some(Taken {
            module,
            landed: Linked {
                values,
                imported: substitution,
                functions,
                constants,
                products,
                families,
            },
            values: named.values.order,
            functions: named.functions.order,
        })
    }
}
