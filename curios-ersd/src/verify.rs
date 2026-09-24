//! The module verifier — the finalize/test gate of every construction and transformation, checking exactly the language contract.
//!
//! Structural rules: every referenced identity resolves to a live slot of its kind; every value use is dominated by its unique binding in the lexical scope structure; every live block, statement, value, function, and recursive group has exactly one structural owner (an unowned or doubly owned one is an error, which also rejects ownership cycles); operand counts agree with each operation's own arity; products, constructors, and matches agree with their registered schemas; matches are exhaustive or defaulted.
//!
//! Recursion admission mirrors the language: recursion through functions is unrestricted, and a group's computed members are forced by need, so a member may reference any other, in any order — what no order can satisfy is an *evaluation cycle*, an initializer that evaluates itself directly or through the functions it applies, and [`eager`] refuses that by summarizing what each reachable function reads and applies. A reference from inside a function body constructed during initialization is dormant; what the summary cannot see through stays dormant too, and its module states the limit. The other rule by need rests on is purity: an initializer performs no effect, so forcing it later, or never, is unobservable — [`eager`] holds it to that as well. Together these admit the corpus's value-recursion idioms (a `join_all`-shaped knot whose initializer calls a group function, a value-only self-referential lazy value whose knot closes through a constructed closure, a parser built over a member declared after it) while refusing exactly what forcing could not make right. Every rule is corpus-certified: a rule that rejects a supported program is a bug in the rule.
//!
//! The two halves differ in whose fault a violation is. A broken structural rule is a malformed module, which only a faulty producer makes, so it panics — [`StructuralFault`] names which rule, and is a value only so this crate's tests can read one without the panic. A broken recursion rule is something a program can do, so it is the [`VerifyError`] handed back, naming members and calls by their source names for the refusal the elaborator renders.
//!
//! The walk recurses over the module's block structure inside [`recurse`], so a deep module diagnoses on the default test-thread stack instead of overflowing it. It used to drive an explicit task stack, which reified three things the call stack already provides — sibling ordering, scope entry and exit, and unwinding — into a `Task` enum, a reversing `push_sequence`, and a driver loop that abandoned its pending work on the error path.

mod eager;

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, Block, BlockId, CellOperation, ChannelOperation, Constructor, ConstructorId,
        FamilyId, Function, FunctionId, Intrinsic, Module, ProductId, ProductSchema, RecGroup,
        RecGroupId, Rhs, SequenceArity, Statement, StatementId, Terminator, ValueId, VariantFamily,
        spell_function, spell_value,
    },
    curios_utilities::{grown, recurse},
    std::collections::HashSet,
};

/// A recursion rule a program broke: what forcing computed members by need could not make right. Members and calls are named by their source names, resolved while the module is still in hand, because a refused module does not survive the refusal; each member also carries the identity it was minted as, which is what the producer that minted it can still look it up by.
#[derive(Debug, Clone)]
pub enum VerifyError {
    /// Computed members whose initializers evaluate one another, directly or through the functions they apply, so no forcing order satisfies them. A single step is a member evaluating itself.
    EvaluationCycle { steps: Vec<CycleStep> },
    /// An initializer that performs an effect, directly or through the named call, which forcing it later, or never, could not keep in its place.
    InitializerPerformsEffect {
        member: String,
        value: ValueId,
        through: Option<String>,
    },
}

/// One link of an evaluation cycle: a member, the identity it was minted as, and the function it reads the next member through, when that read goes through a call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CycleStep {
    pub member: String,
    pub value: ValueId,
    pub through: Option<String>,
}

impl std::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EvaluationCycle { steps } => {
                let shape = match steps.len() {
                    1 => "a computed group member evaluates itself",
                    _ => "computed group members evaluate each other",
                };
                write!(f, "{shape}, which no forcing order can satisfy:")?;
                for step in steps {
                    match &step.through {
                        Some(callee) => write!(f, " {} evaluates, through {callee},", step.member)?,
                        None => write!(f, " {} evaluates", step.member)?,
                    }
                }
                match steps.first() {
                    Some(first) => write!(f, " {}", first.member),
                    None => Ok(()),
                }
            }
            Self::InitializerPerformsEffect {
                member, through, ..
            } => {
                write!(
                    f,
                    "the initializer of computed group member {member} performs an effect"
                )?;
                if let Some(callee) = through {
                    write!(f, " through a call to {callee}")?;
                }
                write!(f, ", which forcing it by need could not keep in its place")
            }
        }
    }
}

impl std::error::Error for VerifyError {}

/// A structural rule a malformed module broke. Only a faulty producer makes one, so [`Module::verify`] panics on it; it is a value here only so this crate's tests can say which rule fired.
#[derive(Debug, Clone)]
pub(crate) struct StructuralFault {
    pub(crate) rule: StructuralRule,
    pub(crate) detail: String,
}

impl StructuralFault {
    /// Abort on the fault, as a fault in the compiler rather than a refusal of the program it compiled.
    pub(crate) fn raise(&self) -> ! {
        panic!(
            "the erased module is malformed ({:?}), a fault in the compiler rather than the program: {}",
            self.rule, self.detail
        )
    }
}

/// The structural rules, one identity per distinct check. A dead identity and a broken schema link each break one documented invariant stated at several sites, so each shares an identity across them.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StructuralRule {
    NoEntry,
    BlockOwnedTwice,
    StatementOwnedTwice,
    GroupOwnedTwice,
    BlockUnowned,
    StatementUnowned,
    FunctionUnbound,
    ValueUndefined,
    GroupUnowned,
    EmptyFunctions,
    EmptyGroup,
    ConstantCallee,
    ApplicationArity,
    OperationArity,
    SequenceOperationArity,
    CellArity,
    ChannelArity,
    ForeignArity,
    IntrinsicArity,
    MapperArity,
    ProductWidth,
    ConstructorWidth,
    ProjectionWidth,
    ArmBindingWidth,
    ArmOfOtherFamily,
    DuplicateArm,
    NonExhaustiveMatch,
    DuplicateCaseKey,
    DeadIdentity,
    ValueOutOfScope,
    FunctionOutOfScope,
    ValueDefinedTwice,
    FunctionBoundTwice,
    BrokenSchemaLink,
    UnsealedBlock,
    DanglingReservation,
}

fn fault(rule: StructuralRule, detail: String) -> StructuralFault {
    StructuralFault { rule, detail }
}

impl Module {
    /// Check the module against the representation contract and the recursion rules a program is held to. A broken structural rule is a malformed module — a fault in whatever produced it — and panics; a broken recursion rule is the refusal handed back. Deterministic: the same module always reports the same.
    pub fn verify(&self) -> Result<(), VerifyError> {
        curios_profile::profile!("verify_module");
        self.assert_structure(Entry::Required);
        self.check_recursion()
    }

    /// [`verify`](Self::verify) for a *prefix* — a unit that carries no entrypoint, so there is no entry block and its absence is not a fault.
    ///
    /// Every other rule applies unchanged: an entry contributes no rule of its own, it is one more block walked after the items, so what a prefix cannot be checked against is exactly the one clause that asks for it. That is worth a second entry point rather than a silent skip, because the prefix is the thing that gets *stored*: the fixed prelude's image is erased, compacted and serialized without ever passing through [`ErsdBuilder::finalize`](crate::ErsdBuilder::finalize), and a compaction that misses an identity rewrites nothing and reports nothing — the stale index still addresses a live slot, just the wrong entity. Without this the first walk over those bytes is a later program's own `finalize`, which reports the fault against that program.
    ///
    /// The prelude's own build profile prices it: **76.1 ms against `erase_unit`'s 2344.9 ms** on 2026-08-25, debug, one call each. Retake with `cargo build --package curios-prelude --features profile` and read `verify_prefix` out of the `OUT_DIR/profile.tsv` it announces.
    pub fn verify_prefix(&self) -> Result<(), VerifyError> {
        curios_profile::profile!("verify_prefix");
        self.assert_structure(Entry::Absent);
        self.check_recursion()
    }

    /// The first structural rule the module breaks as `entry` describes it, if any — what [`verify`](Self::verify) and [`verify_prefix`](Self::verify_prefix) panic on, handed back so a test can read which rule fired.
    pub(crate) fn structure_fault(&self, entry: Entry) -> Option<StructuralFault> {
        grown(|| Verifier::new(self).run(entry)).err()
    }

    fn assert_structure(&self, entry: Entry) {
        if let Some(fault) = self.structure_fault(entry) {
            fault.raise();
        }
    }

    /// Every recursive group's computed members, against the two rules forcing by need rests on. It runs once the structure holds, so every live group it reads off the arena is one the module owns; it is not inside the walk because the walk is the half that panics.
    fn check_recursion(&self) -> Result<(), VerifyError> {
        for group in self.rec_groups().iter().flatten() {
            eager::check_group(self, &group.values)?;
        }
        Ok(())
    }
}

/// Whether the module under check is a finished program, which owes an entry block, or a prefix, which does not.
#[derive(Clone, Copy)]
pub(crate) enum Entry {
    Required,
    Absent,
}

struct Verifier<'m> {
    module: &'m Module,
    values_in_scope: HashSet<ValueId>,
    functions_in_scope: HashSet<FunctionId>,
    defined_values: HashSet<ValueId>,
    bound_functions: HashSet<FunctionId>,
    visited_blocks: HashSet<BlockId>,
    visited_statements: HashSet<StatementId>,
    visited_groups: HashSet<RecGroupId>,
}

impl<'m> Verifier<'m> {
    fn new(module: &'m Module) -> Self {
        Self {
            module,
            values_in_scope: HashSet::new(),
            functions_in_scope: HashSet::new(),
            defined_values: HashSet::new(),
            bound_functions: HashSet::new(),
            visited_blocks: HashSet::new(),
            visited_statements: HashSet::new(),
            visited_groups: HashSet::new(),
        }
    }

    fn run(mut self, entry: Entry) -> Result<(), StructuralFault> {
        self.check_schema_links()?;

        let block = match (self.module.entry(), entry) {
            (Some(block), _) => Some(block),
            (None, Entry::Absent) => None,
            (None, Entry::Required) => {
                return Err(fault(
                    StructuralRule::NoEntry,
                    "the module has no entry block".into(),
                ));
            }
        };

        // The module's top level is a virtual block: items in order, then the entry block, with item bindings ambient for everything after them.
        let items = self.module.items().to_vec();
        for item in items {
            self.check_statement(item)?;
        }
        if let Some(block) = block {
            self.enter_block(block)?;
        }

        self.check_ownership_complete()
    }

    /// Walk a block: its statements in order, its terminator, then the shallow bindings it introduced going back out of scope.
    ///
    /// The recursion point of the whole verifier — a block's statements open blocks of their own — so this is where [`recurse`] sits. Depth is the module's block nesting, which erasure generates rather than anyone writing.
    ///
    /// An error propagates before the unbinding, exactly as the task-stack spelling abandoned its pending work: the walk is over, and a scope left standing cannot be observed.
    fn enter_block(&mut self, id: BlockId) -> Result<(), StructuralFault> {
        recurse(|| {
            let statements = self.block(id)?.statements.clone();
            if !self.visited_blocks.insert(id) {
                return Err(fault(
                    StructuralRule::BlockOwnedTwice,
                    format!("block {id} has more than one owner"),
                ));
            }

            // Shallow bindings of this block, taken out of scope when it ends.
            let mut bound_values = Vec::new();
            let mut bound_functions = Vec::new();
            for &statement in &statements {
                match self.statement(statement)? {
                    Statement::Let { result, .. } => bound_values.push(*result),
                    Statement::Functions { functions } => bound_functions.extend(functions),
                    Statement::Rec { group } => {
                        let group = self.rec_group(*group)?;
                        bound_functions.extend(&group.functions);
                        bound_values.extend(group.values.iter().map(|member| member.value));
                    }
                }
            }

            for statement in statements {
                self.check_statement(statement)?;
            }
            self.check_terminator(id)?;

            for value in bound_values {
                self.values_in_scope.remove(&value);
            }
            for function in bound_functions {
                self.functions_in_scope.remove(&function);
            }

            Ok(())
        })
    }

    /// Bind `values`, walk `block` under them, and take them back out of scope — the bracket every arm binder, fold binder and function parameter enters its body through.
    fn scoped_block(&mut self, values: &[ValueId], block: BlockId) -> Result<(), StructuralFault> {
        for &value in values {
            self.bind_value(value)?;
        }
        self.enter_block(block)?;
        for value in values {
            self.values_in_scope.remove(value);
        }
        Ok(())
    }

    fn check_statement(&mut self, id: StatementId) -> Result<(), StructuralFault> {
        if !self.visited_statements.insert(id) {
            return Err(fault(
                StructuralRule::StatementOwnedTwice,
                format!("statement {id} has more than one owner"),
            ));
        }
        match self.statement(id)?.clone() {
            Statement::Let { result, rhs } => self.check_let(id, result, rhs),
            Statement::Functions { functions } => {
                if functions.is_empty() {
                    return Err(fault(
                        StructuralRule::EmptyFunctions,
                        format!("statement {id} binds no functions"),
                    ));
                }
                for &function in &functions {
                    self.bind_function(function)?;
                }
                for &function in &functions {
                    self.walk_function(function)?;
                }
                Ok(())
            }
            Statement::Rec { group: group_id } => {
                if !self.visited_groups.insert(group_id) {
                    return Err(fault(
                        StructuralRule::GroupOwnedTwice,
                        format!("recursive group {group_id} has more than one owner"),
                    ));
                }
                let group = self.rec_group(group_id)?.clone();
                if group.functions.is_empty() && group.values.is_empty() {
                    return Err(fault(
                        StructuralRule::EmptyGroup,
                        format!("recursive group {group_id} has no members"),
                    ));
                }
                for &function in &group.functions {
                    self.bind_function(function)?;
                }
                for member in &group.values {
                    self.bind_value(member.value)?;
                }
                for &function in &group.functions {
                    self.walk_function(function)?;
                }
                for member in &group.values {
                    self.enter_block(member.init)?;
                }
                Ok(())
            }
        }
    }

    /// Walk one function definition, entered at its binding site: params bound around the body.
    fn walk_function(&mut self, function: FunctionId) -> Result<(), StructuralFault> {
        let definition = self.function(function)?;
        let params = definition.params.clone();
        let body = definition.body;
        self.scoped_block(&params, body)
    }

    fn check_let(
        &mut self,
        id: StatementId,
        result: ValueId,
        rhs: Rhs,
    ) -> Result<(), StructuralFault> {
        match rhs {
            Rhs::Alias(atom) => self.check_atom(id, atom)?,
            Rhs::Apply { callee, arguments } => {
                self.check_atom(id, callee)?;
                if let Atom::Constant(_) = callee {
                    return Err(fault(
                        StructuralRule::ConstantCallee,
                        format!("statement {id} applies a constant callee"),
                    ));
                }
                if let Atom::Function(function) = callee {
                    let arity = self.function(function)?.params.len();
                    if arguments.len() != arity {
                        return Err(fault(
                            StructuralRule::ApplicationArity,
                            format!(
                                "statement {id} applies {} with {} arguments; \
                                 its arity is {arity}",
                                spell_function(self.module, function),
                                arguments.len()
                            ),
                        ));
                    }
                }
                for atom in arguments {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Operation {
                operation,
                operands,
            } => {
                if operands.len() != operation.arity() {
                    return Err(fault(
                        StructuralRule::OperationArity,
                        format!(
                            "statement {id} gives {operation:?} {} operands; its arity is {}",
                            operands.len(),
                            operation.arity()
                        ),
                    ));
                }
                for atom in operands {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Sequence {
                operation,
                operands,
            } => {
                if let SequenceArity::Exactly(arity) = operation.arity()
                    && operands.len() != arity
                {
                    return Err(fault(
                        StructuralRule::SequenceOperationArity,
                        format!(
                            "statement {id} gives {operation:?} {} operands; its arity is {arity}",
                            operands.len()
                        ),
                    ));
                }
                for atom in operands {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Product { schema, fields } => {
                let width = self.product(schema)?.width();
                if fields.len() != width {
                    return Err(fault(
                        StructuralRule::ProductWidth,
                        format!(
                            "statement {id} constructs {schema} with {} fields; its width is {width}",
                            fields.len()
                        ),
                    ));
                }
                for atom in fields {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Construct {
                constructor,
                fields,
            } => {
                let width = self.constructor(constructor)?.width();
                if fields.len() != width {
                    return Err(fault(
                        StructuralRule::ConstructorWidth,
                        format!(
                            "statement {id} constructs {constructor} with {} fields; \
                             its payload width is {width}",
                            fields.len()
                        ),
                    ));
                }
                for atom in fields {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Project {
                schema,
                product,
                field,
            } => {
                let width = self.product(schema)?.width();
                if field as usize >= width {
                    return Err(fault(
                        StructuralRule::ProjectionWidth,
                        format!(
                            "statement {id} projects field {field} of {schema}; its width is {width}"
                        ),
                    ));
                }
                self.check_atom(id, product)?;
            }
            Rhs::MatchVariant {
                family,
                scrutinee,
                arms,
                default,
            } => {
                self.check_atom(id, scrutinee)?;
                let constructors = self.family(family)?.constructors.clone();
                let mut covered = HashSet::new();
                for arm in &arms {
                    let constructor = self.constructor(arm.constructor)?;
                    if constructor.family != family {
                        return Err(fault(
                            StructuralRule::ArmOfOtherFamily,
                            format!(
                                "statement {id} matches {family} but arm constructor {} \
                                 belongs to {}",
                                arm.constructor, constructor.family
                            ),
                        ));
                    }
                    if !covered.insert(arm.constructor) {
                        return Err(fault(
                            StructuralRule::DuplicateArm,
                            format!(
                                "statement {id} has two arms for constructor {}",
                                arm.constructor
                            ),
                        ));
                    }
                    if arm.bindings.len() != constructor.width() {
                        return Err(fault(
                            StructuralRule::ArmBindingWidth,
                            format!(
                                "statement {id} binds {} payload fields of {}; \
                                 its payload width is {}",
                                arm.bindings.len(),
                                arm.constructor,
                                constructor.width()
                            ),
                        ));
                    }
                }
                if default.is_none()
                    && let Some(missing) = constructors
                        .iter()
                        .find(|constructor| !covered.contains(constructor))
                {
                    return Err(fault(
                        StructuralRule::NonExhaustiveMatch,
                        format!(
                            "statement {id} matches {family} without arm or default \
                             for constructor {missing}"
                        ),
                    ));
                }
                for arm in arms {
                    self.scoped_block(&arm.bindings, arm.block)?;
                }
                if let Some(default) = default {
                    self.enter_block(default)?;
                }
            }
            Rhs::SwitchBool {
                scrutinee,
                if_false,
                if_true,
            } => {
                self.check_atom(id, scrutinee)?;
                self.enter_block(if_false)?;
                self.enter_block(if_true)?;
            }
            Rhs::SwitchNat {
                scrutinee,
                cases,
                default,
            } => {
                self.check_atom(id, scrutinee)?;
                let mut keys = HashSet::new();
                for case in &cases {
                    if !keys.insert(case.key) {
                        return Err(fault(
                            StructuralRule::DuplicateCaseKey,
                            format!("statement {id} has two cases for key {}", case.key),
                        ));
                    }
                }
                for case in cases {
                    self.enter_block(case.block)?;
                }
                self.enter_block(default)?;
            }
            Rhs::FoldNat {
                scrutinee,
                zero,
                step,
            } => {
                self.check_atom(id, scrutinee)?;
                self.enter_block(zero)?;
                self.scoped_block(&[step.predecessor, step.hypothesis], step.block)?;
            }
            Rhs::FoldSequence {
                grain: _,
                scrutinee,
                empty,
                step,
            } => {
                self.check_atom(id, scrutinee)?;
                self.enter_block(empty)?;
                self.scoped_block(&[step.element, step.suffix, step.accumulator], step.block)?;
            }
            Rhs::UnconsSequence {
                grain: _,
                scrutinee,
                empty,
                cons,
            } => {
                self.check_atom(id, scrutinee)?;
                self.enter_block(empty)?;
                self.scoped_block(&[cons.element, cons.suffix], cons.block)?;
            }
            Rhs::Cell {
                operation,
                operands,
            } => {
                if let CellOperation::Poll { some, none } = operation {
                    self.outcome(some, 1)?;
                    self.outcome(none, 0)?;
                }
                if operands.len() != operation.arity() {
                    return Err(fault(
                        StructuralRule::CellArity,
                        format!(
                            "statement {id} gives {operation:?} {} operands; its arity is {}",
                            operands.len(),
                            operation.arity()
                        ),
                    ));
                }
                for atom in operands {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Channel {
                operation,
                operands,
            } => {
                match operation {
                    ChannelOperation::Push {
                        taken,
                        full,
                        closed,
                    } => {
                        for constructor in [taken, full, closed] {
                            self.outcome(constructor, 0)?;
                        }
                    }
                    ChannelOperation::Take { item, empty, ended } => {
                        self.outcome(item, 1)?;
                        self.outcome(empty, 0)?;
                        self.outcome(ended, 0)?;
                    }
                    _ => {}
                }
                if operands.len() != operation.arity() {
                    return Err(fault(
                        StructuralRule::ChannelArity,
                        format!(
                            "statement {id} gives {operation:?} {} operands; its arity is {}",
                            operands.len(),
                            operation.arity()
                        ),
                    ));
                }
                for atom in operands {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Foreign { foreign, operands } => {
                let row = self.module.foreign(foreign).ok_or_else(|| {
                    fault(
                        StructuralRule::DeadIdentity,
                        format!("statement {id} references dead {foreign}"),
                    )
                })?;
                let arity = row.signature.params.len();
                if operands.len() != arity {
                    return Err(fault(
                        StructuralRule::ForeignArity,
                        format!(
                            "statement {id} calls {}/{} with {} operands; its wire arity is {arity}",
                            row.namespace,
                            row.name,
                            operands.len()
                        ),
                    ));
                }
                for atom in operands {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Intrinsic {
                intrinsic,
                operands,
            } => {
                if operands.len() != intrinsic.arity() {
                    return Err(fault(
                        StructuralRule::IntrinsicArity,
                        format!(
                            "statement {id} gives {intrinsic:?} {} operands; its arity is {}",
                            operands.len(),
                            intrinsic.arity()
                        ),
                    ));
                }
                // The intrinsic's operand order is the list first, then the mapper.
                if let Intrinsic::ListMap = intrinsic
                    && let [_, Atom::Function(mapper)] = operands[..]
                {
                    let arity = self.function(mapper)?.params.len();
                    if arity != 1 {
                        return Err(fault(
                            StructuralRule::MapperArity,
                            format!(
                                "statement {id} maps with {} of arity {arity}; \
                                 a mapper takes one element",
                                spell_function(self.module, mapper)
                            ),
                        ));
                    }
                }
                for atom in operands {
                    self.check_atom(id, atom)?;
                }
            }
        }
        // The result comes into scope only after every sub-block has been walked, so a block of this statement cannot see it.
        self.bind_value(result)
    }

    fn check_terminator(&mut self, id: BlockId) -> Result<(), StructuralFault> {
        match &self.block(id)?.terminator {
            Terminator::Return(atom) | Terminator::Exit(atom) => {
                self.check_atom_at(*atom, || format!("the terminator of block {id}"))
            }
            Terminator::Unreachable => Ok(()),
        }
    }

    fn check_atom(&self, statement: StatementId, atom: Atom) -> Result<(), StructuralFault> {
        self.check_atom_at(atom, || format!("statement {statement}"))
    }

    fn check_atom_at(&self, atom: Atom, site: impl Fn() -> String) -> Result<(), StructuralFault> {
        match atom {
            Atom::Value(value) => {
                if self.module.value(value).is_none() {
                    return Err(fault(
                        StructuralRule::DeadIdentity,
                        format!("{} references dead {value}", site()),
                    ));
                }
                if !self.values_in_scope.contains(&value) {
                    return Err(fault(
                        StructuralRule::ValueOutOfScope,
                        format!(
                            "{} references {} out of scope",
                            site(),
                            spell_value(self.module, value)
                        ),
                    ));
                }
                Ok(())
            }
            Atom::Function(function) => {
                if self.module.function(function).is_none() {
                    return Err(fault(
                        StructuralRule::DeadIdentity,
                        format!("{} references dead {function}", site()),
                    ));
                }
                if !self.functions_in_scope.contains(&function) {
                    return Err(fault(
                        StructuralRule::FunctionOutOfScope,
                        format!(
                            "{} references {} out of scope",
                            site(),
                            spell_function(self.module, function)
                        ),
                    ));
                }
                Ok(())
            }
            Atom::Constant(constant) => {
                if self.module.constant(constant).is_none() {
                    return Err(fault(
                        StructuralRule::DeadIdentity,
                        format!("{} references dead {constant}", site()),
                    ));
                }
                Ok(())
            }
        }
    }

    fn bind_value(&mut self, id: ValueId) -> Result<(), StructuralFault> {
        if self.module.value(id).is_none() {
            return Err(fault(
                StructuralRule::DeadIdentity,
                format!("dead {id} is bound"),
            ));
        }
        if !self.defined_values.insert(id) {
            return Err(fault(
                StructuralRule::ValueDefinedTwice,
                format!("{id} is defined more than once"),
            ));
        }
        self.values_in_scope.insert(id);
        Ok(())
    }

    fn bind_function(&mut self, id: FunctionId) -> Result<(), StructuralFault> {
        self.function(id)?;
        if !self.bound_functions.insert(id) {
            return Err(fault(
                StructuralRule::FunctionBoundTwice,
                format!("{id} is bound more than once"),
            ));
        }
        self.functions_in_scope.insert(id);
        Ok(())
    }

    /// Cross-check the registered schema links: every family lists exactly the constructors that back-link to it, each exactly once.
    fn check_schema_links(&self) -> Result<(), StructuralFault> {
        let mut listed = vec![0usize; self.module.constructors().len()];
        for (index, family) in self.module.families().iter().enumerate() {
            let id = FamilyId(index as u32);
            for &constructor in &family.constructors {
                let Some(definition) = self.module.constructor(constructor) else {
                    return Err(fault(
                        StructuralRule::BrokenSchemaLink,
                        format!("family {id} lists dead constructor {constructor}"),
                    ));
                };
                if definition.family != id {
                    return Err(fault(
                        StructuralRule::BrokenSchemaLink,
                        format!(
                            "family {id} lists {constructor}, which belongs to {}",
                            definition.family
                        ),
                    ));
                }
                listed[constructor.index()] += 1;
            }
        }
        for (index, count) in listed.iter().enumerate() {
            if *count != 1 {
                let id = ConstructorId(index as u32);
                return Err(fault(
                    StructuralRule::BrokenSchemaLink,
                    format!("constructor {id} is listed {count} times by its family"),
                ));
            }
        }
        for (index, constructor) in self.module.constructors().iter().enumerate() {
            if self.module.family(constructor.family).is_none() {
                let id = ConstructorId(index as u32);
                return Err(fault(
                    StructuralRule::BrokenSchemaLink,
                    format!("constructor {id} references dead {}", constructor.family),
                ));
            }
        }
        Ok(())
    }

    /// After the walk: every live slot in a tombstoned arena was owned exactly once (the walk already rejected double ownership), so anything unvisited is leaked.
    fn check_ownership_complete(&self) -> Result<(), StructuralFault> {
        for (index, slot) in self.module.blocks().iter().enumerate() {
            let id = BlockId(index as u32);
            if slot.is_some() && !self.visited_blocks.contains(&id) {
                return Err(fault(
                    StructuralRule::BlockUnowned,
                    format!("block {id} has no owner"),
                ));
            }
        }
        for (index, slot) in self.module.statements().iter().enumerate() {
            let id = StatementId(index as u32);
            if slot.is_some() && !self.visited_statements.contains(&id) {
                return Err(fault(
                    StructuralRule::StatementUnowned,
                    format!("statement {id} has no owner"),
                ));
            }
        }
        for (index, slot) in self.module.functions().iter().enumerate() {
            let id = FunctionId(index as u32);
            if slot.is_some() && !self.bound_functions.contains(&id) {
                return Err(fault(
                    StructuralRule::FunctionUnbound,
                    format!("function {id} is never bound"),
                ));
            }
        }
        for (index, slot) in self.module.values().iter().enumerate() {
            let id = ValueId(index as u32);
            if slot.is_some() && !self.defined_values.contains(&id) {
                return Err(fault(
                    StructuralRule::ValueUndefined,
                    format!("value {id} is never defined"),
                ));
            }
        }
        for (index, slot) in self.module.rec_groups().iter().enumerate() {
            let id = RecGroupId(index as u32);
            if slot.is_some() && !self.visited_groups.contains(&id) {
                return Err(fault(
                    StructuralRule::GroupUnowned,
                    format!("recursive group {id} has no owner"),
                ));
            }
        }
        Ok(())
    }

    fn block(&self, id: BlockId) -> Result<&'m Block, StructuralFault> {
        self.module.block(id).ok_or_else(|| {
            fault(
                StructuralRule::DeadIdentity,
                format!("dead block {id} is referenced"),
            )
        })
    }

    fn statement(&self, id: StatementId) -> Result<&'m Statement, StructuralFault> {
        self.module.statement(id).ok_or_else(|| {
            fault(
                StructuralRule::DeadIdentity,
                format!("dead statement {id} is referenced"),
            )
        })
    }

    fn function(&self, id: FunctionId) -> Result<&'m Function, StructuralFault> {
        self.module.function(id).ok_or_else(|| {
            fault(
                StructuralRule::DeadIdentity,
                format!("dead {id} is referenced"),
            )
        })
    }

    fn rec_group(&self, id: RecGroupId) -> Result<&'m RecGroup, StructuralFault> {
        self.module.rec_group(id).ok_or_else(|| {
            fault(
                StructuralRule::DeadIdentity,
                format!("dead {id} is referenced"),
            )
        })
    }

    fn product(&self, id: ProductId) -> Result<&'m ProductSchema, StructuralFault> {
        self.module.product(id).ok_or_else(|| {
            fault(
                StructuralRule::DeadIdentity,
                format!("dead {id} is referenced"),
            )
        })
    }

    fn family(&self, id: FamilyId) -> Result<&'m VariantFamily, StructuralFault> {
        self.module.family(id).ok_or_else(|| {
            fault(
                StructuralRule::DeadIdentity,
                format!("dead {id} is referenced"),
            )
        })
    }

    fn outcome(&self, id: ConstructorId, payload: usize) -> Result<(), StructuralFault> {
        let width = self.constructor(id)?.width();
        if width != payload {
            return Err(fault(
                StructuralRule::ConstructorWidth,
                format!(
                    "operation constructs {id} with {payload} fields; its payload width is {width}"
                ),
            ));
        }
        Ok(())
    }

    fn constructor(&self, id: ConstructorId) -> Result<&'m Constructor, StructuralFault> {
        self.module.constructor(id).ok_or_else(|| {
            fault(
                StructuralRule::DeadIdentity,
                format!("dead {id} is referenced"),
            )
        })
    }
}
