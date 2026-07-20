//! The module: crate-private arenas, checked accessors, and the low-level
//! construction and removal primitives the checked builder drives.
//!
//! Arena slots never move and identities are never reused: removal writes
//! `None` (a tombstone) and compaction, when its consumer lands, is an
//! explicit deterministic pass. Constants are interned by their exact bitwise
//! identity, so equal constants share one identity; the interning index is a
//! derived lookup structure, never iterated, and identity order is insertion
//! order — construction is deterministic, so identities are too.
//!
//! The module's top level is an ordered item list plus an entry block: items
//! are ordinary statements at module scope, evaluated eagerly in order (the
//! record pruning's granularity and the effect contract depend on), and the
//! entry block computes the program's result after them.

use {
    super::{
        Block, BlockId, Constant, ConstantId, Constructor, ConstructorId, FamilyId, ForeignId,
        Function, FunctionId, ProductId, ProductSchema, RecGroup, RecGroupId, Statement,
        StatementId, ValueId, VariantFamily,
    },
    curios_abi::ForeignFunction,
    std::{collections::HashMap, sync::Arc},
};

/// A value's definition record. Uses, shape, and freeness are derived by
/// analysis; only the optional debug name is stored, and it never affects
/// identity or behavior.
#[derive(Debug, Clone)]
pub struct ValueDef {
    pub debug_name: Option<String>,
}

/// The erased program as flat, first-order data. See the crate's `ir` module
/// documentation for the representation contract.
#[derive(Debug, Clone, Default)]
pub struct ErasedModule {
    values: Vec<Option<ValueDef>>,
    functions: Vec<Option<Function>>,
    blocks: Vec<Option<Block>>,
    statements: Vec<Option<Statement>>,
    rec_groups: Vec<Option<RecGroup>>,
    constants: Vec<Constant>,
    products: Vec<ProductSchema>,
    families: Vec<VariantFamily>,
    constructors: Vec<Constructor>,
    foreigns: Vec<Arc<ForeignFunction>>,
    items: Vec<StatementId>,
    entry: Option<BlockId>,
    constant_index: HashMap<Constant, ConstantId>,
}

impl ErasedModule {
    pub fn new() -> Self {
        Self::default()
    }

    /// The ordered top-level items: statements at module scope, evaluated
    /// eagerly in this order before the entry block.
    pub fn items(&self) -> &[StatementId] {
        &self.items
    }

    /// The entry block, evaluated after the items to produce the program's
    /// result.
    pub fn entry(&self) -> Option<BlockId> {
        self.entry
    }

    pub fn values(&self) -> &[Option<ValueDef>] {
        &self.values
    }

    pub fn functions(&self) -> &[Option<Function>] {
        &self.functions
    }

    pub fn blocks(&self) -> &[Option<Block>] {
        &self.blocks
    }

    pub fn statements(&self) -> &[Option<Statement>] {
        &self.statements
    }

    pub fn rec_groups(&self) -> &[Option<RecGroup>] {
        &self.rec_groups
    }

    pub fn constants(&self) -> &[Constant] {
        &self.constants
    }

    pub fn products(&self) -> &[ProductSchema] {
        &self.products
    }

    pub fn families(&self) -> &[VariantFamily] {
        &self.families
    }

    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    pub fn foreigns(&self) -> &[Arc<ForeignFunction>] {
        &self.foreigns
    }

    /// The live function identities, in identity order — the external handle
    /// surface for consumers that hold a module but cannot mint identities.
    pub fn function_ids(&self) -> impl Iterator<Item = FunctionId> + '_ {
        self.functions
            .iter()
            .enumerate()
            .filter(|(_, slot)| slot.is_some())
            .map(|(index, _)| FunctionId(index as u32))
    }

    pub fn value(&self, id: ValueId) -> Option<&ValueDef> {
        self.values.get(id.index()).and_then(Option::as_ref)
    }

    pub fn function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id.index()).and_then(Option::as_ref)
    }

    pub fn block(&self, id: BlockId) -> Option<&Block> {
        self.blocks.get(id.index()).and_then(Option::as_ref)
    }

    pub fn statement(&self, id: StatementId) -> Option<&Statement> {
        self.statements.get(id.index()).and_then(Option::as_ref)
    }

    pub fn rec_group(&self, id: RecGroupId) -> Option<&RecGroup> {
        self.rec_groups.get(id.index()).and_then(Option::as_ref)
    }

    pub fn constant(&self, id: ConstantId) -> Option<&Constant> {
        self.constants.get(id.index())
    }

    pub fn product(&self, id: ProductId) -> Option<&ProductSchema> {
        self.products.get(id.index())
    }

    pub fn family(&self, id: FamilyId) -> Option<&VariantFamily> {
        self.families.get(id.index())
    }

    pub fn constructor(&self, id: ConstructorId) -> Option<&Constructor> {
        self.constructors.get(id.index())
    }

    pub fn foreign(&self, id: ForeignId) -> Option<&Arc<ForeignFunction>> {
        self.foreigns.get(id.index())
    }

    pub(crate) fn add_value(&mut self, debug_name: Option<String>) -> ValueId {
        let id = ValueId(self.values.len() as u32);
        self.values.push(Some(ValueDef { debug_name }));
        id
    }

    /// Mint a function identity whose definition follows later, so recursive
    /// bodies can reference their own or a sibling's identity. A reserved slot
    /// is `None` until [`define_function`](Self::define_function) fills it;
    /// finalization requires every reservation to have been defined.
    pub(crate) fn reserve_function(&mut self) -> FunctionId {
        let id = FunctionId(self.functions.len() as u32);
        self.functions.push(None);
        id
    }

    pub(crate) fn define_function(&mut self, id: FunctionId, function: Function) {
        let slot = &mut self.functions[id.index()];
        debug_assert!(slot.is_none(), "function {id} defined twice");
        *slot = Some(function);
    }

    pub(crate) fn add_block(&mut self, block: Block) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(Some(block));
        id
    }

    pub(crate) fn add_statement(&mut self, statement: Statement) -> StatementId {
        let id = StatementId(self.statements.len() as u32);
        self.statements.push(Some(statement));
        id
    }

    pub(crate) fn add_rec_group(&mut self, group: RecGroup) -> RecGroupId {
        let id = RecGroupId(self.rec_groups.len() as u32);
        self.rec_groups.push(Some(group));
        id
    }

    /// Intern a constant by its exact bitwise identity, returning the shared
    /// identity of an equal constant already present.
    pub(crate) fn intern_constant(&mut self, constant: Constant) -> ConstantId {
        if let Some(&id) = self.constant_index.get(&constant) {
            return id;
        }
        let id = ConstantId(self.constants.len() as u32);
        self.constants.push(constant.clone());
        self.constant_index.insert(constant, id);
        id
    }

    pub(crate) fn add_product(&mut self, schema: ProductSchema) -> ProductId {
        let id = ProductId(self.products.len() as u32);
        self.products.push(schema);
        id
    }

    pub(crate) fn add_family(&mut self, debug_name: Option<String>) -> FamilyId {
        let id = FamilyId(self.families.len() as u32);
        self.families.push(VariantFamily {
            debug_name,
            constructors: Vec::new(),
        });
        id
    }

    /// Register the next constructor of `family`, in declaration order; its
    /// position in the family is its discriminant. The cross-links are correct
    /// by construction.
    pub(crate) fn add_constructor(
        &mut self,
        family: FamilyId,
        debug_name: Option<String>,
        fields: Vec<Option<String>>,
    ) -> ConstructorId {
        let id = ConstructorId(self.constructors.len() as u32);
        self.constructors.push(Constructor {
            debug_name,
            family,
            fields,
        });
        self.families[family.index()].constructors.push(id);
        id
    }

    /// Intern a canonical foreign row by its wire identity
    /// (`namespace`/`name`), returning the identity of an equal row already
    /// present. Foreign sets are small; the scan is linear and deterministic.
    pub(crate) fn intern_foreign(&mut self, foreign: Arc<ForeignFunction>) -> ForeignId {
        if let Some(index) = self.foreigns.iter().position(|row| **row == *foreign) {
            return ForeignId(index as u32);
        }
        let id = ForeignId(self.foreigns.len() as u32);
        self.foreigns.push(foreign);
        id
    }

    pub(crate) fn push_item(&mut self, item: StatementId) {
        self.items.push(item);
    }

    pub(crate) fn set_entry(&mut self, entry: BlockId) {
        self.entry = Some(entry);
    }
}
