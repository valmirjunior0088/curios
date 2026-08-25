//! The checked builder — the construction surface of the representation.
//!
//! Every producer (erasure, the transformations' reification paths, and the tests) constructs modules through this type: it owns identity allocation, the open-block discipline that gives the operand law its home ("bound as a statement in the innermost open block"), schema and recursion registration, constant and foreign interning, and finalization, which runs the module verifier. The arena mutators on [`Module`] stay crate-private behind it.
//!
//! The module's item list is the outermost block: `let_value`, `let_functions`, and `let_rec` emit into the innermost open block, and with no block open they append to the top-level item list — one uniform emission path, so a producer walking an expression never cares whether it is lowering an item body or a nested position. The `item_*` forms are the explicit top-level surface (they assert no block is open). Blocks nest as a stack — [`open_block`](ErsdBuilder::open_block) pushes, [`seal_block`](ErsdBuilder::seal_block) pops and freezes the block with its terminator.

#[cfg(test)]
mod tests;

use {
    super::{
        Block, BlockId, Constant, ConstantId, ConstructorId, FamilyId, Field, ForeignId, Function,
        FunctionId, Module, ProductId, ProductSchema, RecGroup, RecGroupId, RecValue, Rhs,
        Statement, StatementId, Terminator, ValueId, VerifyError,
    },
    curios_abi::ForeignFunction,
    std::{collections::BTreeSet, sync::Arc},
};

/// A checked module under construction. See the module documentation.
#[derive(Debug, Default)]
pub struct ErsdBuilder {
    module: Module,
    open_blocks: Vec<Vec<StatementId>>,
    /// Identities handed out by [`reserve_function`](Self::reserve_function) and not yet defined.
    ///
    /// Tracked here rather than read back off the arena, because an empty slot does not say *why* it is empty: `Arena::reserve` and `Arena::remove` both write `None`, so an unfilled reservation and a pruned-away function are the same value. Reading finalization's question off the arena therefore answers a different one — it reports the first empty slot whatever put it there, and a module carrying a tombstone would be rejected for a reservation nobody made. Reservation is a lifecycle of *construction*, opening and closing inside one builder, where a tombstone outlives the builder; keeping the two apart is what lets a pruned module be finalized at all.
    reserved_functions: BTreeSet<FunctionId>,
}

impl ErsdBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Resume construction over a restored prefix module: the archived prelude's arenas continue growing under the user suffix. The skipped constant-interning index is rebuilt here, and no block is open.
    pub fn resume(mut module: Module) -> Self {
        module.reindex_constants();
        Self {
            module,
            open_blocks: Vec::new(),
            // A resumed prefix has no outstanding reservations: `into_module` is the only way one is handed over, and it takes a finished builder.
            reserved_functions: BTreeSet::new(),
        }
    }

    /// Read-only access to the module under construction.
    pub fn module(&self) -> &Module {
        &self.module
    }

    /// Hand the module over as a replayable prefix: no entry block, and every other rule checked. [`resume`](Self::resume) is the inverse.
    ///
    /// Verified through [`Module::verify_prefix`], which is [`finalize`](Self::finalize)'s check minus the one clause a prefix cannot satisfy. This hand-off used to skip verification entirely, and it is the hand-off whose result is *serialized*: the fixed prelude's image reached disk without any walk over it, so a fault in erasure or compaction was first reported against whichever program later compiled on top of it.
    pub fn into_module(self) -> Result<Module, VerifyError> {
        assert!(
            self.open_blocks.is_empty(),
            "a prefix hand-off leaves no open block"
        );
        self.module.verify_prefix()?;
        Ok(self.module)
    }

    /// Mint a fresh value identity — a parameter, arm binder, or fold binder. Statement results are minted by the emission methods themselves.
    pub fn value(&mut self, debug_name: Option<String>) -> ValueId {
        self.module.add_value(debug_name)
    }

    /// Intern a constant by exact bitwise identity.
    pub fn constant(&mut self, constant: Constant) -> ConstantId {
        self.module.intern_constant(constant)
    }

    /// Intern a canonical foreign row by wire identity.
    pub fn foreign(&mut self, foreign: Arc<ForeignFunction>) -> ForeignId {
        self.module.intern_foreign(foreign)
    }

    /// Register a product schema.
    pub fn product(&mut self, schema: ProductSchema) -> ProductId {
        self.module.add_product(schema)
    }

    /// Register a variant family; its constructors follow through [`constructor`](Self::constructor), in declaration order.
    pub fn family(&mut self, debug_name: Option<String>) -> FamilyId {
        self.module.add_family(debug_name)
    }

    /// Register the next constructor of `family`; its position in the family is its discriminant.
    pub fn constructor(
        &mut self,
        family: FamilyId,
        debug_name: Option<String>,
        fields: Vec<Field>,
    ) -> ConstructorId {
        self.module.add_constructor(family, debug_name, fields)
    }

    /// Mint a function identity whose definition follows later, so recursive bodies can reference their own or a sibling's identity. Finalization rejects a reservation that was never defined.
    pub fn reserve_function(&mut self) -> FunctionId {
        let id = self.module.reserve_function();
        self.reserved_functions.insert(id);
        id
    }

    /// Define a previously reserved function.
    pub fn define_function(
        &mut self,
        id: FunctionId,
        debug_name: Option<String>,
        params: Vec<ValueId>,
        body: BlockId,
    ) {
        self.reserved_functions.remove(&id);
        self.module.define_function(
            id,
            Function {
                debug_name,
                params,
                body,
            },
        );
    }

    /// Group a set of computed recursive-group members with their initializer blocks into a registered group.
    pub fn rec_group(
        &mut self,
        functions: Vec<FunctionId>,
        values: Vec<(ValueId, BlockId)>,
    ) -> RecGroupId {
        self.module.add_rec_group(RecGroup {
            functions,
            values: values
                .into_iter()
                .map(|(value, init)| RecValue { value, init })
                .collect(),
        })
    }

    /// Open a nested block; subsequent `let_*` statements land in it until [`seal_block`](Self::seal_block) closes it.
    pub fn open_block(&mut self) {
        self.open_blocks.push(Vec::new());
    }

    /// Seal the innermost open block with its terminator and return its identity.
    pub fn seal_block(&mut self, terminator: Terminator) -> BlockId {
        let statements = self
            .open_blocks
            .pop()
            .expect("seal_block requires an open block");
        self.module.add_block(Block {
            statements,
            terminator,
        })
    }

    /// Bind `rhs` to a fresh value in the innermost open block, or as the next top-level item when no block is open.
    pub fn let_value(&mut self, debug_name: Option<String>, rhs: Rhs) -> ValueId {
        let result = self.module.add_value(debug_name);
        let statement = self.module.add_statement(Statement::Let { result, rhs });
        self.emit(statement);
        result
    }

    /// Introduce a function group in the innermost open block.
    pub fn let_functions(&mut self, functions: Vec<FunctionId>) {
        let statement = self
            .module
            .add_statement(Statement::Functions { functions });
        self.emit(statement);
    }

    /// Introduce a mixed recursive group in the innermost open block.
    pub fn let_rec(&mut self, group: RecGroupId) {
        let statement = self.module.add_statement(Statement::Rec { group });
        self.emit(statement);
    }

    /// Bind `rhs` to a fresh value as the next top-level item.
    pub fn item_value(&mut self, debug_name: Option<String>, rhs: Rhs) -> ValueId {
        self.require_top_level("item_value");
        let result = self.module.add_value(debug_name);
        let statement = self.module.add_statement(Statement::Let { result, rhs });
        self.module.push_item(statement);
        result
    }

    /// Introduce a function group as the next top-level item.
    pub fn item_functions(&mut self, functions: Vec<FunctionId>) {
        self.require_top_level("item_functions");
        let statement = self
            .module
            .add_statement(Statement::Functions { functions });
        self.module.push_item(statement);
    }

    /// Introduce a mixed recursive group as the next top-level item.
    pub fn item_rec(&mut self, group: RecGroupId) {
        self.require_top_level("item_rec");
        let statement = self.module.add_statement(Statement::Rec { group });
        self.module.push_item(statement);
    }

    /// Set the entry block, evaluated after the items to produce the program's result.
    pub fn set_entry(&mut self, entry: BlockId) {
        self.module.set_entry(entry);
    }

    /// Finish construction: reject leftover open blocks and dangling function reservations, then run the module verifier and hand the module over. (An empty function slot is a construction error here; after removal lands with its consumer, the same slot is an ordinary tombstone to the verifier.)
    pub fn finalize(self) -> Result<Module, VerifyError> {
        if !self.open_blocks.is_empty() {
            return Err(VerifyError(format!(
                "finalize with {} unsealed open block(s)",
                self.open_blocks.len()
            )));
        }
        if let Some(&id) = self.reserved_functions.iter().next() {
            return Err(VerifyError(format!(
                "function {id} was reserved but never defined"
            )));
        }
        self.module.verify()?;
        Ok(self.module)
    }

    fn emit(&mut self, statement: StatementId) {
        match self.open_blocks.last_mut() {
            Some(block) => block.push(statement),
            None => self.module.push_item(statement),
        }
    }

    fn require_top_level(&self, operation: &str) {
        assert!(
            self.open_blocks.is_empty(),
            "{operation} requires no open block"
        );
    }
}
