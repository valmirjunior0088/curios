//! Linking: one module appended to another under fresh identities, its imports resolved.
//!
//! A declaration is erased into a module of its own, a *fragment*, whose identities count from zero and whose references to what other declarations bind are [`Imports`](crate::Imports). A program is the fragments it reaches appended to one module in the order their items evaluate, each import standing for where the fragment that defines it landed. Nothing of a fragment is rewritten in place and no index is patched: every identity it holds is minted again in the program, and its contents are written through the old-to-new maps, as a deep copy's are.
//!
//! **What a module keeps outside its arenas is interned, not appended.** A constant, a foreign row and a schema are each one in a program however many fragments spell them. Constants and rows intern by their own identity. A schema has none a module can read — two structures of one shape are two schemas — so whoever links says which of a fragment's schemas the program already holds ([`Resolution::products`], [`Resolution::families`]), and every other is registered as new.
//!
//! **An imported function's stub is not linked.** Its parameters and its body exist so that the fragment verifies alone; in a program the identity is the function it stands for.
//!
//! **A fragment's schemas can be linked without it** ([`Module::link_rows`]): a program that constructs or matches a declaration's values holds the declaration's row, and has no use for what the declaration's own item computes unless something reaches it.
//!
//! **The fragment linked last can be taken back out** ([`Module::taken`]): a program's arenas only grow, so what stands past the place it was linked at ([`Mark`]) is its own, with whatever was appended for it since.

#[cfg(test)]
mod tests;

mod taken;
pub use taken::*;

use {
    crate::{
        Atom, BlockId, ConstantId, ConstructorId, FamilyId, Field, FieldShape, ForeignId,
        FunctionId, Module, ProductId, ProductSchema, RecGroupId, Remap, StatementId, Tables,
        ValueId, lookup,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// What a fragment's imports, and the schemas it shares with the program, stand for in the module it is linked into.
#[derive(Debug, Default)]
pub struct Resolution {
    /// The atom each imported value stands for, in the order the fragment imported them.
    pub values: Vec<Atom>,
    /// The function each imported function stands for, in the order the fragment imported them.
    pub functions: Vec<FunctionId>,
    /// The fragment's product schemas the program already holds, each with where.
    pub products: BTreeMap<ProductId, ProductId>,
    /// The fragment's variant families the program already holds, each with where. A family's constructors are its positions, so they follow it.
    pub families: BTreeMap<FamilyId, FamilyId>,
}

/// Where a linked fragment's identities landed: what the fragment exports is read through these, and so is each schema it registered, for the fragments linked after it.
#[derive(Debug, Default)]
pub struct Linked {
    values: BTreeMap<ValueId, ValueId>,
    /// What each value the fragment imported stands for: a fragment may hand an import on under a name of its own.
    imported: BTreeMap<ValueId, Atom>,
    functions: BTreeMap<FunctionId, FunctionId>,
    constants: BTreeMap<ConstantId, ConstantId>,
    products: BTreeMap<ProductId, ProductId>,
    families: BTreeMap<FamilyId, FamilyId>,
}

impl Linked {
    /// Where an atom the fragment bound landed.
    pub fn atom(&self, atom: Atom) -> Atom {
        match atom {
            Atom::Value(value) => match self.imported.get(&value) {
                Some(&standing) => standing,
                None => Atom::Value(lookup(&self.values, value)),
            },
            Atom::Function(function) => Atom::Function(lookup(&self.functions, function)),
            Atom::Constant(constant) => Atom::Constant(lookup(&self.constants, constant)),
        }
    }

    /// Where a function the fragment defined landed.
    pub fn function(&self, function: FunctionId) -> FunctionId {
        lookup(&self.functions, function)
    }

    /// Where a product schema of the fragment landed, resolved or registered.
    pub fn product(&self, product: ProductId) -> ProductId {
        lookup(&self.products, product)
    }

    /// Where a variant family of the fragment landed, resolved or registered.
    pub fn family(&self, family: FamilyId) -> FamilyId {
        lookup(&self.families, family)
    }

    /// Each product schema of the fragment with where it landed.
    pub fn products(&self) -> impl Iterator<Item = (ProductId, ProductId)> {
        self.products.iter().map(|(&held, &landed)| (held, landed))
    }

    /// Each variant family of the fragment with where it landed.
    pub fn families(&self) -> impl Iterator<Item = (FamilyId, FamilyId)> {
        self.families.iter().map(|(&held, &landed)| (held, landed))
    }
}

/// Where a module's arenas and its item list stood: what is at or past it was added since. The default is where an empty module stands, which every module is past.
#[derive(Debug, Clone, Copy, Default)]
pub struct Mark {
    pub(crate) values: usize,
    pub(crate) functions: usize,
    pub(crate) blocks: usize,
    pub(crate) statements: usize,
    pub(crate) rec_groups: usize,
    pub(crate) items: usize,
}

/// Where the identities of a module go in the one its contents are written into: its values, blocks and functions each by a place of its own, the values that stand for an atom of the other, and its tables.
struct Landing<'a> {
    values: &'a BTreeMap<ValueId, ValueId>,
    blocks: &'a BTreeMap<BlockId, BlockId>,
    functions: &'a BTreeMap<FunctionId, FunctionId>,
    substitution: &'a BTreeMap<ValueId, Atom>,
    tables: &'a Tables,
}

impl Module {
    /// Where this module stands now.
    pub fn mark(&self) -> Mark {
        Mark {
            values: self.values().len(),
            functions: self.functions().len(),
            blocks: self.blocks().len(),
            statements: self.statements().len(),
            rec_groups: self.rec_groups().len(),
            items: self.items().len(),
        }
    }

    /// Write what `source` holds at or past `mark` into this module through `landing`: its recursive groups, statements, blocks and functions, and its items after this module's own. A block and a function are written where `landing` gives the block a place, which leaves out an imported function's stub.
    fn write(&mut self, source: &Module, mark: &Mark, landing: &Landing<'_>) {
        // A group names functions, values and blocks, a statement names groups besides, and a block names statements: each arena is written once the ones its nodes name are mapped.
        let mut rec_groups = BTreeMap::new();
        let mut statements = BTreeMap::new();
        for (index, slot) in source.rec_groups().iter().enumerate().skip(mark.rec_groups) {
            let Some(group) = slot else { continue };
            let remap = Remap {
                values: landing.values,
                blocks: landing.blocks,
                functions: landing.functions,
                rec_groups: &rec_groups,
                statements: &statements,
                substitution: landing.substitution,
                redirect: None,
                tables: Some(landing.tables),
            };
            let written = self.add_rec_group(remap.rec_group(group));
            rec_groups.insert(RecGroupId(index as u32), written);
        }
        for (index, slot) in source.statements().iter().enumerate().skip(mark.statements) {
            let Some(statement) = slot else { continue };
            let remap = Remap {
                values: landing.values,
                blocks: landing.blocks,
                functions: landing.functions,
                rec_groups: &rec_groups,
                statements: &statements,
                substitution: landing.substitution,
                redirect: None,
                tables: Some(landing.tables),
            };
            let written = self.add_statement(remap.statement(statement));
            statements.insert(StatementId(index as u32), written);
        }

        let remap = Remap {
            values: landing.values,
            blocks: landing.blocks,
            functions: landing.functions,
            rec_groups: &rec_groups,
            statements: &statements,
            substitution: landing.substitution,
            redirect: None,
            tables: Some(landing.tables),
        };
        for (index, slot) in source.blocks().iter().enumerate().skip(mark.blocks) {
            let id = BlockId(index as u32);
            if let (Some(block), Some(&written)) = (slot, landing.blocks.get(&id)) {
                self.define_block(written, remap.block_body(block));
            }
        }
        for (index, slot) in source.functions().iter().enumerate().skip(mark.functions) {
            let id = FunctionId(index as u32);
            let Some(function) = slot else { continue };
            if landing.blocks.contains_key(&function.body) {
                self.define_function(landing.functions[&id], remap.function(function));
            }
        }
        for &item in &source.items()[mark.items..] {
            self.push_item(remap.statement_id(item));
        }
    }

    /// Append `fragment` to this module under fresh identities, every import of it standing for what `resolution` gives, and hand back where its identities landed.
    ///
    /// Its items follow this module's, in their order, and its entry block, where it has one, becomes this module's. The result is what a producer that had erased both into one module would have built, up to the numbering of identities: nothing is verified here, since a program is verified once, whole, when the last fragment is in.
    pub fn link(&mut self, fragment: &Module, resolution: &Resolution) -> Linked {
        let imports = fragment.imports();
        assert_eq!(
            (imports.values().len(), imports.functions().len()),
            (resolution.values.len(), resolution.functions.len()),
            "a resolution answers each import of the fragment it resolves, in order",
        );

        // The stubs: what an imported function owns in the fragment and nothing in a program stands for.
        let mut stub_values = BTreeSet::new();
        let mut stub_blocks = BTreeSet::new();
        let mut functions = BTreeMap::new();
        for (import, &target) in imports.functions().iter().zip(&resolution.functions) {
            let stub = fragment
                .function(import.function)
                .expect("an imported function is live in its fragment");
            stub_values.extend(stub.params.iter().copied());
            stub_blocks.insert(stub.body);
            functions.insert(import.function, target);
        }
        let substitution: BTreeMap<ValueId, Atom> = imports
            .values()
            .iter()
            .copied()
            .zip(resolution.values.iter().copied())
            .collect();

        let tables = self.link_tables(fragment, resolution);

        // Every identity first, in the fragment's own order, and contents after: a node may name any arena.
        let mut values = BTreeMap::new();
        for (index, slot) in fragment.values().iter().enumerate() {
            let id = ValueId(index as u32);
            let Some(definition) = slot else { continue };
            if stub_values.contains(&id) || substitution.contains_key(&id) {
                continue;
            }
            values.insert(id, self.add_value(definition.debug_name.clone()));
        }
        for (index, slot) in fragment.functions().iter().enumerate() {
            let id = FunctionId(index as u32);
            if slot.is_some() && !functions.contains_key(&id) {
                functions.insert(id, self.reserve_function());
            }
        }
        let mut blocks = BTreeMap::new();
        for (index, slot) in fragment.blocks().iter().enumerate() {
            let id = BlockId(index as u32);
            if slot.is_some() && !stub_blocks.contains(&id) {
                blocks.insert(id, self.reserve_block());
            }
        }

        self.write(
            fragment,
            &Mark::default(),
            &Landing {
                values: &values,
                blocks: &blocks,
                functions: &functions,
                substitution: &substitution,
                tables: &tables,
            },
        );
        if let Some(entry) = fragment.entry() {
            self.set_entry(lookup(&blocks, entry));
        }

        let Tables {
            constants,
            products,
            families,
            ..
        } = tables;

        Linked {
            values,
            imported: substitution,
            functions,
            constants,
            products,
            families,
        }
    }

    /// Register `fragment`'s schemas in this module and nothing else of it, each where `resolution` says this module holds it, or as a new one, and hand back where they landed.
    pub fn link_rows(&mut self, fragment: &Module, resolution: &Resolution) -> Linked {
        let mut tables = Tables::default();
        self.link_schemas(fragment, resolution, &mut tables);
        let Tables {
            products, families, ..
        } = tables;

        Linked {
            products,
            families,
            ..Default::default()
        }
    }

    /// Intern what `fragment` keeps outside its arenas into this module's tables: its constants and foreign rows by their own identity, and each schema where `resolution` says this module holds it, or as a new one.
    fn link_tables(&mut self, fragment: &Module, resolution: &Resolution) -> Tables {
        let mut tables = Tables::default();
        for (index, constant) in fragment.constants().iter().enumerate() {
            let linked = self.intern_constant(constant.clone());
            tables.constants.insert(ConstantId(index as u32), linked);
        }
        for (index, foreign) in fragment.foreigns().iter().enumerate() {
            let linked = self.intern_foreign(Arc::clone(foreign));
            tables.foreigns.insert(ForeignId(index as u32), linked);
        }
        self.link_schemas(fragment, resolution, &mut tables);

        tables
    }

    /// The schema half of [`link_tables`](Self::link_tables), written into `tables`.
    fn link_schemas(&mut self, fragment: &Module, resolution: &Resolution, tables: &mut Tables) {
        // Every schema's place before any schema's fields: a field's shape names a schema, its own among them.
        let mut registered = Vec::new();
        for (index, schema) in fragment.products().iter().enumerate() {
            let id = ProductId(index as u32);
            let linked = match resolution.products.get(&id) {
                Some(&held) => held,
                None => {
                    let linked = self.add_product(ProductSchema {
                        debug_name: schema.debug_name.clone(),
                        fields: Vec::new(),
                        shared: schema.shared,
                    });
                    registered.push((schema, linked));
                    linked
                }
            };
            tables.products.insert(id, linked);
        }
        for (index, family) in fragment.families().iter().enumerate() {
            let id = FamilyId(index as u32);
            let linked = match resolution.families.get(&id) {
                Some(&held) => {
                    let constructors = &self
                        .family(held)
                        .expect("a family a resolution names is this module's")
                        .constructors;
                    assert_eq!(
                        constructors.len(),
                        family.constructors.len(),
                        "one family has one list of constructors wherever it is spelled",
                    );
                    for (&constructor, &held) in family.constructors.iter().zip(constructors) {
                        tables.constructors.insert(constructor, held);
                    }
                    held
                }
                None => self.add_family(family.debug_name.clone()),
            };
            tables.families.insert(id, linked);
        }

        let field = |field: &Field, tables: &Tables| Field {
            debug_name: field.debug_name.clone(),
            shape: match field.shape {
                FieldShape::Product(product) => {
                    FieldShape::Product(lookup(&tables.products, product))
                }
                FieldShape::Family(family) => FieldShape::Family(lookup(&tables.families, family)),
                shape => shape,
            },
        };
        // A new family's constructors in the order the fragment registered them, which is each family's own.
        for (index, constructor) in fragment.constructors().iter().enumerate() {
            let id = ConstructorId(index as u32);
            if tables.constructors.contains_key(&id) {
                continue;
            }
            let fields = constructor
                .fields
                .iter()
                .map(|entry| field(entry, tables))
                .collect();
            let linked = self.add_constructor(
                tables.families[&constructor.family],
                constructor.debug_name.clone(),
                fields,
            );
            tables.constructors.insert(id, linked);
        }
        for (schema, linked) in registered {
            let fields = schema
                .fields
                .iter()
                .map(|entry| field(entry, tables))
                .collect();
            self.set_product_fields(linked, fields);
        }
    }
}
