//! The module: crate-private arenas, checked accessors, and the low-level construction and removal intrinsics the checked builder drives.
//!
//! Between compactions, arena slots never move and identities are never reused: removal writes `None` (a tombstone). [`Module::compact`] is the one explicit, deterministic pass that renumbers, and it reports where every identity a caller may hold went. Constants are interned by their exact bitwise identity, so equal constants share one identity; the interning index is a derived lookup structure, never iterated, and identity order is insertion order — construction is deterministic, so identities are too.
//!
//! The module's top level is an ordered item list plus an entry block: items are ordinary statements at module scope, evaluated eagerly in order (the record pruning's granularity and the effect contract depend on), and the entry block computes the program's result after them.

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, Block, BlockId, Constant, ConstantId, Constructor, ConstructorId, FamilyId, Field,
        ForeignId, Function, FunctionId, ProductId, ProductSchema, RecGroup, RecGroupId, Statement,
        StatementId, Terminator, ValueId, VariantFamily,
    },
    curios_abi::ForeignFunction,
    curios_utilities::Arena,
    std::{
        collections::{BTreeSet, HashMap},
        sync::Arc,
    },
};

/// Where compaction moved the identities a caller outside the module may still hold.
///
/// Only the two an [`Atom`] can carry. A consumer that stored atoms — `curios-elab`'s erased environment is the one that does — must rewrite them through these before using the module again.
#[derive(Debug, Clone)]
pub struct Compaction {
    pub values: std::collections::BTreeMap<ValueId, ValueId>,
    pub functions: std::collections::BTreeMap<FunctionId, FunctionId>,
}

/// A value's definition record. Uses, shape, and freeness are derived by analysis; only the optional debug name is stored, and it never affects identity or behavior.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct ValueDef {
    pub debug_name: Option<String>,
}

/// The erased program as flat, first-order data. See the crate documentation for the representation contract.
///
/// Archived (behind the `archive` feature) as the fixed prelude's replayable prefix. The constant interning index is skipped — it is exactly the inverse of the `constants` arena, rebuilt by `reindex_constants` on restore — so the serialized bytes stay deterministic (a hash map's iteration order is not).
#[derive(Debug, Clone, Default)]
#[curios_archive::archived(recursive)]
pub struct Module {
    values: Arena<ValueId, ValueDef>,
    functions: Arena<FunctionId, Function>,
    blocks: Arena<BlockId, Block>,
    statements: Arena<StatementId, Statement>,
    rec_groups: Arena<RecGroupId, RecGroup>,
    constants: Vec<Constant>,
    products: Vec<ProductSchema>,
    families: Vec<VariantFamily>,
    constructors: Vec<Constructor>,
    #[archived_omit_bounds]
    foreigns: Vec<Arc<ForeignFunction>>,
    items: Vec<StatementId>,
    entry: Option<BlockId>,
    #[archived_with(curios_archive::Skip)]
    constant_index: HashMap<Constant, ConstantId>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    /// The ordered top-level items: statements at module scope, evaluated eagerly in this order before the entry block.
    pub fn items(&self) -> &[StatementId] {
        &self.items
    }

    /// The entry block, evaluated after the items to produce the program's result.
    pub fn entry(&self) -> Option<BlockId> {
        self.entry
    }

    pub fn values(&self) -> &[Option<ValueDef>] {
        self.values.slots()
    }

    pub fn functions(&self) -> &[Option<Function>] {
        self.functions.slots()
    }

    pub fn blocks(&self) -> &[Option<Block>] {
        self.blocks.slots()
    }

    pub fn statements(&self) -> &[Option<Statement>] {
        self.statements.slots()
    }

    pub fn rec_groups(&self) -> &[Option<RecGroup>] {
        self.rec_groups.slots()
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

    /// The live function identities, in identity order — the external handle surface for consumers that hold a module but cannot mint identities.
    pub fn function_ids(&self) -> impl Iterator<Item = FunctionId> + '_ {
        self.functions.live_ids()
    }

    pub fn value(&self, id: ValueId) -> Option<&ValueDef> {
        self.values.get(id)
    }

    pub fn function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id)
    }

    pub fn block(&self, id: BlockId) -> Option<&Block> {
        self.blocks.get(id)
    }

    pub fn statement(&self, id: StatementId) -> Option<&Statement> {
        self.statements.get(id)
    }

    pub fn rec_group(&self, id: RecGroupId) -> Option<&RecGroup> {
        self.rec_groups.get(id)
    }

    /// Whether `member` is referenced anywhere in the module outside its own initializer's subtree (control blocks and nested function regions included on both sides). An unreferenced member's init never runs.
    pub(crate) fn member_used_outside_init(&self, member: ValueId, init: BlockId) -> bool {
        // The init's own subtree: its blocks, plus the regions of functions bound inside it.
        let mut inside_blocks = BTreeSet::new();
        let mut inside_functions = BTreeSet::new();
        let mut function_work: Vec<FunctionId> = Vec::new();
        let mut block_work = vec![init];
        loop {
            if let Some(block) = block_work.pop() {
                if !inside_blocks.insert(block) {
                    continue;
                }
                let Some(block) = self.block(block) else {
                    continue;
                };
                for &statement in &block.statements {
                    match self.statement(statement) {
                        Some(Statement::Let { rhs, .. }) => block_work.extend(rhs.sub_blocks()),
                        Some(Statement::Functions { functions }) => {
                            function_work.extend(functions.iter().copied());
                        }
                        Some(Statement::Rec { group }) => {
                            if let Some(group) = self.rec_group(*group) {
                                function_work.extend(group.functions.iter().copied());
                                block_work.extend(group.values.iter().map(|m| m.init));
                            }
                        }
                        None => {}
                    }
                }
                continue;
            }
            let Some(function) = function_work.pop() else {
                break;
            };
            if !inside_functions.insert(function) {
                continue;
            }
            if let Some(function) = self.function(function) {
                block_work.push(function.body);
            }
        }

        // Any reference from a block outside the subtree is a use.
        for (index, slot) in self.blocks().iter().enumerate() {
            let Some(block) = slot else { continue };
            if inside_blocks.contains(&BlockId(index as u32)) {
                continue;
            }
            for &statement in &block.statements {
                if let Some(Statement::Let { rhs, .. }) = self.statement(statement)
                    && rhs.operands().contains(&Atom::Value(member))
                {
                    return true;
                }
            }
            if block.terminator.atom() == Some(Atom::Value(member)) {
                return true;
            }
        }
        // So is one from a top-level item, which lives in the item list and in no block: a `let` after the group that reads the member is the read that forces it, and a member dropped for want of seeing it lowered to a value the arena lacked.
        for &item in self.items() {
            if let Some(Statement::Let { rhs, .. }) = self.statement(item)
                && rhs.operands().contains(&Atom::Value(member))
            {
                return true;
            }
        }
        false
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
        self.values.mint(ValueDef { debug_name })
    }

    /// Mint a function identity whose definition follows later, so recursive bodies can reference their own or a sibling's identity. A reserved slot is `None` until [`define_function`](Self::define_function) fills it; finalization requires every reservation to have been defined.
    pub(crate) fn reserve_function(&mut self) -> FunctionId {
        self.functions.reserve()
    }

    pub(crate) fn define_function(&mut self, id: FunctionId, function: Function) {
        self.functions.define(id, function);
    }

    pub(crate) fn add_block(&mut self, block: Block) -> BlockId {
        self.blocks.mint(block)
    }

    pub(crate) fn add_statement(&mut self, statement: Statement) -> StatementId {
        self.statements.mint(statement)
    }

    pub(crate) fn add_rec_group(&mut self, group: RecGroup) -> RecGroupId {
        self.rec_groups.mint(group)
    }

    /// Rebuild the skipped interning index from the constants arena — the restore path's counterpart to the `archive` skip above.
    pub(crate) fn reindex_constants(&mut self) {
        self.constant_index = self
            .constants
            .iter()
            .enumerate()
            .map(|(index, constant)| (constant.clone(), ConstantId(index as u32)))
            .collect();
    }

    /// Intern a constant by its exact bitwise identity, returning the shared identity of an equal constant already present.
    pub(crate) fn intern_constant(&mut self, constant: Constant) -> ConstantId {
        if let Some(&id) = self.constant_index.get(&constant) {
            return id;
        }
        let id = ConstantId(u32::try_from(self.constants.len()).expect("constant arena exhausted"));
        self.constants.push(constant.clone());
        self.constant_index.insert(constant, id);
        id
    }

    pub(crate) fn add_product(&mut self, schema: ProductSchema) -> ProductId {
        let id = ProductId(u32::try_from(self.products.len()).expect("product arena exhausted"));
        self.products.push(schema);
        id
    }

    pub(crate) fn add_family(&mut self, debug_name: Option<String>) -> FamilyId {
        let id = FamilyId(u32::try_from(self.families.len()).expect("family arena exhausted"));
        self.families.push(VariantFamily {
            debug_name,
            constructors: Vec::new(),
        });
        id
    }

    /// Replace the top-level item list — the prune's retained, order-preserved subset. Tombstoning what the dropped items owned is the caller's responsibility.
    pub(crate) fn set_items(&mut self, items: Vec<StatementId>) {
        self.items = items;
    }

    /// Rewrite one live statement in place — partial evaluation's install intrinsic (a folded call becomes an alias or a residual).
    pub(crate) fn set_statement(&mut self, id: StatementId, statement: Statement) {
        self.statements.set(id, statement);
    }

    /// Replace one live block's statement list — the splice intrinsic for materialized reifications and taken match arms.
    pub(crate) fn set_block_statements(&mut self, id: BlockId, statements: Vec<StatementId>) {
        let block = self.blocks.get_mut(id).expect("a spliced block is live");
        block.statements = statements;
    }

    /// Replace one live block's terminator — the monoid rebase redirects a leaf tail block's return through the threaded accumulator.
    pub(crate) fn set_block_terminator(&mut self, id: BlockId, terminator: Terminator) {
        let block = self.blocks.get_mut(id).expect("a rewritten block is live");
        block.terminator = terminator;
    }

    /// Redefine one live function — the spine specializer's parameter-drop edit on a freshly minted copy.
    pub(crate) fn set_function(&mut self, id: FunctionId, function: Function) {
        self.functions.set(id, function);
    }

    /// Mint a block identity whose definition follows — the deep copy reserves the whole region before rewriting references into it.
    pub(crate) fn reserve_block(&mut self) -> BlockId {
        self.blocks.reserve()
    }

    pub(crate) fn define_block(&mut self, id: BlockId, block: Block) {
        self.blocks.define(id, block);
    }

    /// Drop every tombstone from every arena and rewrite each stored identity to where it moved.
    ///
    /// The one pass that invalidates identities, so it reports the two spaces anything *outside* a module can hold: [`Atom`] carries a `ValueId`, a `FunctionId`, or a `ConstantId`, and constants are interned in a plain vector this never touches. Blocks, statements and recursive groups are named only from within, so they are compacted and not reported.
    ///
    /// Valid only on a finished module — a reserved-but-undefined slot reads as a tombstone here and would be dropped. Callers verify afterwards: a remap gap rewrites nothing and says nothing, and a stale index still addresses a live slot, so the walk cannot be trusted to fail loudly on its own.
    pub fn compact(&mut self) -> Compaction {
        // Every map first, before any content is rewritten: a node's identities may name any arena, so a rewrite needs all five settled.
        let values = self.values.compact();
        let functions = self.functions.compact();
        let blocks = self.blocks.compact();
        let statements = self.statements.compact();
        let rec_groups = self.rec_groups.compact();

        let remap = crate::remap::Remap {
            values: &values,
            blocks: &blocks,
            functions: &functions,
            rec_groups: &rec_groups,
            statements: &statements,
            substitution: &std::collections::BTreeMap::new(),
            redirect: None,
        };

        // The arenas hold their entries at fresh indices now; their *contents* still name the old ones.
        let rewritten: Vec<_> = self
            .statements
            .iter_live()
            .map(|(id, statement)| (id, remap.statement(statement)))
            .collect();
        for (id, statement) in rewritten {
            self.statements.set(id, statement);
        }

        let rewritten: Vec<_> = self
            .blocks
            .iter_live()
            .map(|(id, block)| (id, remap.block_body(block)))
            .collect();
        for (id, block) in rewritten {
            self.blocks.set(id, block);
        }

        let rewritten: Vec<_> = self
            .functions
            .iter_live()
            .map(|(id, function)| (id, remap.function(function)))
            .collect();
        for (id, function) in rewritten {
            self.functions.set(id, function);
        }

        let rewritten: Vec<_> = self
            .rec_groups
            .iter_live()
            .map(|(id, group)| (id, remap.rec_group(group)))
            .collect();
        for (id, group) in rewritten {
            self.rec_groups.set(id, group);
        }

        // `ValueDef` holds only a debug name, so the values arena needs no content rewrite.
        self.items = self
            .items
            .iter()
            .map(|&item| remap.statement_id(item))
            .collect();
        self.entry = self.entry.map(|entry| remap.block(entry));

        // Named one by one rather than folded into a loop: the list is the claim. A sixth arena added to this module and not compacted above is the shape this catches, and it catches it only by being written out here too.
        self.values.assert_packed("values");
        self.functions.assert_packed("functions");
        self.blocks.assert_packed("blocks");
        self.statements.assert_packed("statements");
        self.rec_groups.assert_packed("recursive group");

        Compaction { values, functions }
    }

    /// Tombstone every function outside `keep`. Identities are never reused.
    pub(crate) fn retain_functions(&mut self, keep: &std::collections::BTreeSet<FunctionId>) {
        self.functions.retain(keep);
    }

    /// Tombstone every block outside `keep`.
    pub(crate) fn retain_blocks(&mut self, keep: &std::collections::BTreeSet<BlockId>) {
        self.blocks.retain(keep);
    }

    /// Tombstone every statement outside `keep`.
    pub(crate) fn retain_statements(&mut self, keep: &std::collections::BTreeSet<StatementId>) {
        self.statements.retain(keep);
    }

    /// Tombstone every value outside `keep`.
    pub(crate) fn retain_values(&mut self, keep: &std::collections::BTreeSet<ValueId>) {
        self.values.retain(keep);
    }

    /// Tombstone every recursive group outside `keep`.
    pub(crate) fn retain_rec_groups(&mut self, keep: &std::collections::BTreeSet<RecGroupId>) {
        self.rec_groups.retain(keep);
    }

    /// Register the next constructor of `family`, in declaration order; its position in the family is its discriminant. The cross-links are correct by construction.
    pub(crate) fn add_constructor(
        &mut self,
        family: FamilyId,
        debug_name: Option<String>,
        fields: Vec<Field>,
    ) -> ConstructorId {
        let id = ConstructorId(
            u32::try_from(self.constructors.len()).expect("constructor arena exhausted"),
        );
        self.constructors.push(Constructor {
            debug_name,
            family,
            fields,
        });
        self.families[family.index()].constructors.push(id);
        id
    }

    /// Intern a canonical foreign row by its wire identity (`namespace`/`name`), returning the identity of an equal row already present. Foreign sets are small; the scan is linear and deterministic.
    pub(crate) fn intern_foreign(&mut self, foreign: Arc<ForeignFunction>) -> ForeignId {
        if let Some(index) = self.foreigns.iter().position(|row| **row == *foreign) {
            return ForeignId(index as u32);
        }
        let id = ForeignId(u32::try_from(self.foreigns.len()).expect("foreign arena exhausted"));
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
