//! The checked builder — the construction surface of the representation.
//!
//! Every producer (erasure, the transformations' reification paths, and the tests) constructs modules through this type: it owns identity allocation, the open-block discipline that gives the operand law its home ("bound as a statement in the innermost open block"), schema and recursion registration, constant and foreign interning, and finalization, which runs the module verifier. The arena mutators on [`Module`] stay crate-private behind it.
//!
//! The module's item list is the outermost block: `let_value`, `let_functions`, and `let_rec` emit into the innermost open block, and with no block open they append to the top-level item list — one uniform emission path, so a producer walking an expression never cares whether it is lowering an item body or a nested position. The `item_*` forms are the explicit top-level surface (they assert no block is open). Blocks nest as a stack — [`open_block`](Builder::open_block) pushes, [`seal_block`](Builder::seal_block) pops and freezes the block with its terminator.

#[cfg(test)]
mod tests;

use {
    super::{
        Block, BlockId, Constant, ConstantId, ConstructorId, FamilyId, Field, ForeignId, Function,
        FunctionId, Interface, Module, ProductId, ProductSchema, RecGroup, RecGroupId, RecValue,
        RegisterShape, Rhs, Role, Statement, StatementId, StructuralFault, StructuralRule,
        Terminator, ValueId, VerifyError,
    },
    curios_abi::ForeignFunction,
    curios_common::{Lineage, Qualifier},
    std::{collections::BTreeMap, sync::Arc},
};

/// A checked module under construction. See the module documentation.
#[derive(Debug, Default)]
pub struct Builder {
    module: Module,
    open_blocks: Vec<Vec<StatementId>>,
    /// Identities handed out by [`reserve_function`](Self::reserve_function) and not yet defined, each with the lineage it was reserved under.
    ///
    /// Tracked here rather than read back off the arena, because an empty slot does not say *why* it is empty: `Arena::reserve` and `Arena::remove` both write `None`, so an unfilled reservation and a pruned-away function are the same value. Reading finalization's question off the arena therefore answers a different one — it reports the first empty slot whatever put it there, and a module carrying a tombstone would be rejected for a reservation nobody made. Reservation is a lifecycle of *construction*, opening and closing inside one builder, where a tombstone outlives the builder; keeping the two apart is what lets a pruned module be finalized at all.
    reserved_functions: BTreeMap<FunctionId, Option<Lineage>>,
    /// The declaration the functions reserved from here on are minted for, with how many of its functions are minted already ([`Builder::declaring`]).
    declaring: Declaring,
}

/// The declaration a builder mints functions for, with how many it has minted: what [`Builder::declaring`] swaps in, and hands back for its caller to put back or to resume later.
#[derive(Debug, Default)]
pub struct Declaring(Option<(Qualifier, u32)>);

impl Declaring {
    /// Minting for `declaration`, none of whose functions is minted yet, or for no declaration.
    pub fn of(declaration: Option<Qualifier>) -> Self {
        Self(declaration.map(|declaration| (declaration, 0)))
    }
}

impl Builder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Read-only access to the module under construction.
    pub fn module(&self) -> &Module {
        &self.module
    }

    /// Hand the module over as a fragment: no entry block required, and every other rule checked.
    ///
    /// Verified through [`Module::verify_fragment`], which is [`finalize`](Self::finalize)'s check minus the one clause a fragment cannot satisfy; why a stored fragment is walked at all is that method's.
    pub fn into_module(self) -> Result<Module, VerifyError> {
        assert!(
            self.open_blocks.is_empty(),
            "a fragment hand-off leaves no open block"
        );
        self.module.verify_fragment()?;
        Ok(self.module)
    }

    /// Mint a fresh value identity — a parameter or an arm binder. Statement results are minted by the emission methods themselves.
    pub fn value(&mut self, debug_name: Option<String>) -> ValueId {
        self.module.add_value(debug_name)
    }

    /// State on `value` the register carrier its type names ([`RegisterShape`]). The verifier refuses one on a binding whose producer names its own carrier.
    pub fn state(&mut self, value: ValueId, register: RegisterShape) {
        self.module.state_register(value, register);
    }

    /// Mint a value another module binds — see [`Imports`](super::Imports). It is in scope from the first item on, and the order imports are minted in is the order linking resolves them by.
    pub fn import_value(&mut self, debug_name: Option<String>) -> ValueId {
        self.module.import_value(debug_name)
    }

    /// Mint a function another module defines, of `arity` parameters and the interface that module computed for it ([`Module::interface`]).
    pub fn import_function(
        &mut self,
        debug_name: Option<String>,
        arity: usize,
        interface: Interface,
    ) -> FunctionId {
        self.module.import_function(debug_name, arity, interface)
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

    /// Import a product schema another module declares, at its width — see [`Imports`](super::Imports).
    pub fn import_product(&mut self, debug_name: Option<String>, width: usize) -> ProductId {
        self.module.import_product(debug_name, width)
    }

    /// Import a variant family another module declares, of `count` constructors; each this module names follows through [`import_constructor`](Self::import_constructor).
    pub fn import_family(&mut self, debug_name: Option<String>, count: usize) -> FamilyId {
        self.module.import_family(debug_name, count)
    }

    /// Import the constructor at `position` of the imported `family`, of `width` payload fields.
    pub fn import_constructor(
        &mut self,
        family: FamilyId,
        position: usize,
        debug_name: Option<String>,
        width: usize,
    ) -> ConstructorId {
        self.module
            .import_constructor(family, position, debug_name, width)
    }

    /// Mint a function identity whose definition follows later, so recursive bodies can reference their own or a sibling's identity. Finalization rejects a reservation that was never defined.
    ///
    /// Its lineage is drawn here rather than where it is defined: a function is reserved before its body is walked and defined after, so drawing here numbers a declaration's functions in the order its text writes them, the outer before the inner.
    pub fn reserve_function(&mut self) -> FunctionId {
        let id = self.module.reserve_function();
        let lineage = self.minted();
        self.reserved_functions.insert(id, lineage);
        id
    }

    /// Mint each function reserved from here on as `declaring` says, its lineage the next of the declaration's ([`Lineage`]), and hand back what was being declared before: the caller puts it back once the declaration is built, keeping what it swapped out where the declaration's functions are reserved in more than one stretch.
    pub fn declaring(&mut self, declaring: Declaring) -> Declaring {
        std::mem::replace(&mut self.declaring, declaring)
    }

    /// The lineage of the next function reserved, counting it among the declaration's.
    fn minted(&mut self) -> Option<Lineage> {
        let (declaration, minted) = self.declaring.0.as_mut()?;
        let lineage = Lineage {
            declaration: *declaration,
            function: *minted,
        };
        *minted += 1;

        Some(lineage)
    }

    /// Define a previously reserved function, of the lineage it was reserved under ([`Builder::declaring`]).
    pub fn define_function(
        &mut self,
        id: FunctionId,
        debug_name: Option<String>,
        params: Vec<ValueId>,
        body: BlockId,
    ) {
        let lineage = self
            .reserved_functions
            .remove(&id)
            .expect("a function is defined once, after it is reserved");
        self.module.define_function(
            id,
            Function {
                lineage,
                debug_name,
                params,
                body,
                role: Role::Evaluation,
                total: false,
            },
        );
    }

    /// Stamp a defined function with what it is part of — see [`Role`]. Called where a function is born that is no part of its definition's evaluation: the erasure's `thunk` and `description` for a performance, and the builders of what runs them for machinery.
    pub fn assign_role(&mut self, id: FunctionId, role: Role) {
        if let Some(function) = self.module.function(id).cloned() {
            self.module.set_function(id, Function { role, ..function });
        }
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
        let (result, statement) = self.module.let_value(debug_name, rhs);
        self.emit(statement);
        result
    }

    /// Introduce a function group in the innermost open block.
    pub fn let_functions(&mut self, functions: Vec<FunctionId>) {
        let statement = self.module.add_functions(functions);
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
        let (result, statement) = self.module.let_value(debug_name, rhs);
        self.module.push_item(statement);
        result
    }

    /// Introduce a function group as the next top-level item.
    pub fn item_functions(&mut self, functions: Vec<FunctionId>) {
        self.require_top_level("item_functions");
        let statement = self.module.add_functions(functions);
        self.module.push_item(statement);
    }

    /// Introduce a function group as a top-level item now, ahead of whatever item is still being built: for functions closed over nothing but other items, which that item's body names and which need not live inside it. A function left local to a body is out of reach of everything outside it, the closed evaluator's copies of a description among them.
    pub fn hoist_functions(&mut self, functions: Vec<FunctionId>) {
        let statement = self.module.add_functions(functions);
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

    /// Finish construction and hand the module over. A block left open, a function reserved and never defined, or a malformed module is a fault in the producer and panics; a broken recursion rule is the refusal returned.
    pub fn finalize(self) -> Result<Module, VerifyError> {
        if let Some(fault) = self.construction_fault() {
            fault.raise();
        }
        self.module.verify()?;
        Ok(self.module)
    }

    /// The first fault in how this builder was driven, which no walk of the module can see: a block left open, or a reserved function never defined. What [`finalize`](Self::finalize) panics on, handed back so a test can read which.
    pub(crate) fn construction_fault(&self) -> Option<StructuralFault> {
        if !self.open_blocks.is_empty() {
            return Some(StructuralFault {
                rule: StructuralRule::UnsealedBlock,
                detail: format!(
                    "finalize with {} unsealed open block(s)",
                    self.open_blocks.len()
                ),
            });
        }
        self.reserved_functions
            .keys()
            .next()
            .map(|&id| StructuralFault {
                rule: StructuralRule::DanglingReservation,
                detail: format!("function {id} was reserved but never defined"),
            })
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
