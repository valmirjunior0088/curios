//! The module verifier — the finalize/test gate of every construction and transformation, checking exactly the language contract.
//!
//! Structural rules: every referenced identity resolves to a live slot of its kind; every value use is dominated by its unique binding in the lexical scope structure; every live block, statement, value, function, and recursive group has exactly one structural owner (an unowned or doubly owned one is an error, which also rejects ownership cycles); operand counts agree with each operation's own arity; products, constructors, and matches agree with their registered schemas; matches are exhaustive or defaulted.
//!
//! Recursion admission mirrors the language: recursion through functions is unrestricted, and a group's computed member may *evaluate* only earlier computed members — a reference from inside a function body constructed during initialization is dormant and unrestricted. That single rule admits the corpus's value-recursion idioms (a `join_all`-shaped knot whose initializer calls a group function, a value-only self-referential lazy value whose knot closes through a constructed closure) while rejecting exactly the computed-only *evaluation* cycles no initialization order can satisfy. Every rule is corpus-certified: a rule that rejects a supported program is a bug in the rule.
//!
//! The walk recurses over the module's block structure inside [`recurse`], so a deep module diagnoses on the default test-thread stack instead of overflowing it. It used to drive an explicit task stack, which reified three things the call stack already provides — sibling ordering, scope entry and exit, and unwinding — into a `Task` enum, a reversing `push_sequence`, and a driver loop that abandoned its pending work on the error path.

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, Block, BlockId, Constructor, ConstructorId, FamilyId, Function, FunctionId,
        Intrinsic, Module, ProductId, ProductSchema, RecGroup, RecGroupId, Rhs, SequenceArity,
        Statement, StatementId, Terminator, ValueId, VariantFamily,
    },
    curios_base::recurse,
    std::collections::HashSet,
};

/// A verification failure: the first rule violation found, as a rendered diagnostic.
#[derive(Debug, Clone)]
pub struct VerifyError(pub String);

impl std::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for VerifyError {}

impl Module {
    /// Check the module against the representation contract, reporting the first violation. Deterministic: the same module always reports the same diagnostic.
    pub fn verify(&self) -> Result<(), VerifyError> {
        curios_profile::profile!("verify_module");
        Verifier::new(self).run()
    }
}

/// The recursion-admission context of a group's eager initializers: its computed members in order, the index currently being initialized, and the function depth at entry (a use at a greater depth is dormant).
///
/// One per group rather than one per member, since only `limit` moves between them.
struct InitContext {
    computed: Vec<ValueId>,
    limit: usize,
    function_depth: usize,
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
    init_contexts: Vec<InitContext>,
    function_depth: usize,
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
            init_contexts: Vec::new(),
            function_depth: 0,
        }
    }

    fn run(mut self) -> Result<(), VerifyError> {
        self.check_schema_links()?;

        let Some(entry) = self.module.entry() else {
            return Err(VerifyError("the module has no entry block".into()));
        };

        // The module's top level is a virtual block: items in order, then the entry block, with item bindings ambient for everything after them.
        let items = self.module.items().to_vec();
        for item in items {
            self.check_statement(item)?;
        }
        self.enter_block(entry)?;

        self.check_ownership_complete()
    }

    /// Walk a block: its statements in order, its terminator, then the shallow bindings it introduced going back out of scope.
    ///
    /// The recursion point of the whole verifier — a block's statements open blocks of their own — so this is where [`recurse`] sits. Depth is the module's block nesting, which erasure generates rather than anyone writing.
    ///
    /// An error propagates before the unbinding, exactly as the task-stack spelling abandoned its pending work: the walk is over, and a scope left standing cannot be observed.
    fn enter_block(&mut self, id: BlockId) -> Result<(), VerifyError> {
        recurse(|| {
            let statements = self.block(id)?.statements.clone();
            if !self.visited_blocks.insert(id) {
                return Err(VerifyError(format!("block {id} has more than one owner")));
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
    fn scoped_block(&mut self, values: &[ValueId], block: BlockId) -> Result<(), VerifyError> {
        for &value in values {
            self.bind_value(value)?;
        }
        self.enter_block(block)?;
        for value in values {
            self.values_in_scope.remove(value);
        }
        Ok(())
    }

    fn check_statement(&mut self, id: StatementId) -> Result<(), VerifyError> {
        if !self.visited_statements.insert(id) {
            return Err(VerifyError(format!(
                "statement {id} has more than one owner"
            )));
        }
        match self.statement(id)?.clone() {
            Statement::Let { result, rhs } => self.check_let(id, result, rhs),
            Statement::Functions { functions } => {
                if functions.is_empty() {
                    return Err(VerifyError(format!("statement {id} binds no functions")));
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
                    return Err(VerifyError(format!(
                        "recursive group {group_id} has more than one owner"
                    )));
                }
                let group = self.rec_group(group_id)?.clone();
                if group.functions.is_empty() && group.values.is_empty() {
                    return Err(VerifyError(format!(
                        "recursive group {group_id} has no members"
                    )));
                }
                for &function in &group.functions {
                    self.bind_function(function)?;
                }
                let computed: Vec<_> = group.values.iter().map(|member| member.value).collect();
                for &value in &computed {
                    self.bind_value(value)?;
                }
                for &function in &group.functions {
                    self.walk_function(function)?;
                }
                // One context for the whole group, its `limit` advanced per member. Only `limit` differs between members, so pushing a fresh context each time cloned the member list once per member — the same list, `n` times over.
                self.init_contexts.push(InitContext {
                    computed,
                    limit: 0,
                    function_depth: self.function_depth,
                });
                for (index, member) in group.values.iter().enumerate() {
                    self.init_contexts
                        .last_mut()
                        .expect("this group's context is live for every one of its members")
                        .limit = index;
                    self.enter_block(member.init)?;
                }
                self.init_contexts.pop();
                Ok(())
            }
        }
    }

    /// Walk one function definition, entered at its binding site: params bound around the body, dormant depth incremented for the extent of it.
    fn walk_function(&mut self, function: FunctionId) -> Result<(), VerifyError> {
        let definition = self.function(function)?;
        let params = definition.params.clone();
        let body = definition.body;

        self.function_depth += 1;
        self.scoped_block(&params, body)?;
        self.function_depth -= 1;

        Ok(())
    }

    fn check_let(&mut self, id: StatementId, result: ValueId, rhs: Rhs) -> Result<(), VerifyError> {
        match rhs {
            Rhs::Alias(atom) => self.check_atom(id, atom)?,
            Rhs::Apply { callee, arguments } => {
                self.check_atom(id, callee)?;
                if let Atom::Constant(_) = callee {
                    return Err(VerifyError(format!(
                        "statement {id} applies a constant callee"
                    )));
                }
                if let Atom::Function(function) = callee {
                    let arity = self.function(function)?.params.len();
                    if arguments.len() != arity {
                        return Err(VerifyError(format!(
                            "statement {id} applies {function} with {} arguments; \
                             its arity is {arity}",
                            arguments.len()
                        )));
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
                    return Err(VerifyError(format!(
                        "statement {id} gives {operation:?} {} operands; its arity is {}",
                        operands.len(),
                        operation.arity()
                    )));
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
                    return Err(VerifyError(format!(
                        "statement {id} gives {operation:?} {} operands; its arity is {arity}",
                        operands.len()
                    )));
                }
                for atom in operands {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Product { schema, fields } => {
                let width = self.product(schema)?.width();
                if fields.len() != width {
                    return Err(VerifyError(format!(
                        "statement {id} constructs {schema} with {} fields; its width is {width}",
                        fields.len()
                    )));
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
                    return Err(VerifyError(format!(
                        "statement {id} constructs {constructor} with {} fields; \
                         its payload width is {width}",
                        fields.len()
                    )));
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
                    return Err(VerifyError(format!(
                        "statement {id} projects field {field} of {schema}; its width is {width}"
                    )));
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
                        return Err(VerifyError(format!(
                            "statement {id} matches {family} but arm constructor {} \
                             belongs to {}",
                            arm.constructor, constructor.family
                        )));
                    }
                    if !covered.insert(arm.constructor) {
                        return Err(VerifyError(format!(
                            "statement {id} has two arms for constructor {}",
                            arm.constructor
                        )));
                    }
                    if arm.bindings.len() != constructor.width() {
                        return Err(VerifyError(format!(
                            "statement {id} binds {} payload fields of {}; \
                             its payload width is {}",
                            arm.bindings.len(),
                            arm.constructor,
                            constructor.width()
                        )));
                    }
                }
                if default.is_none()
                    && let Some(missing) = constructors
                        .iter()
                        .find(|constructor| !covered.contains(constructor))
                {
                    return Err(VerifyError(format!(
                        "statement {id} matches {family} without arm or default \
                         for constructor {missing}"
                    )));
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
                        return Err(VerifyError(format!(
                            "statement {id} has two cases for key {}",
                            case.key
                        )));
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
            Rhs::Cell {
                operation,
                operands,
            } => {
                if operands.len() != operation.arity() {
                    return Err(VerifyError(format!(
                        "statement {id} gives {operation:?} {} operands; its arity is {}",
                        operands.len(),
                        operation.arity()
                    )));
                }
                for atom in operands {
                    self.check_atom(id, atom)?;
                }
            }
            Rhs::Foreign { foreign, operands } => {
                let row = self.module.foreign(foreign).ok_or_else(|| {
                    VerifyError(format!("statement {id} references dead {foreign}"))
                })?;
                let arity = row.signature.params.len();
                if operands.len() != arity {
                    return Err(VerifyError(format!(
                        "statement {id} calls {}/{} with {} operands; its wire arity is {arity}",
                        row.namespace,
                        row.name,
                        operands.len()
                    )));
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
                    return Err(VerifyError(format!(
                        "statement {id} gives {intrinsic:?} {} operands; its arity is {}",
                        operands.len(),
                        intrinsic.arity()
                    )));
                }
                // The intrinsic's operand order is the list first, then the mapper.
                if let Intrinsic::ListMap = intrinsic
                    && let [_, Atom::Function(mapper)] = operands[..]
                {
                    let arity = self.function(mapper)?.params.len();
                    if arity != 1 {
                        return Err(VerifyError(format!(
                            "statement {id} maps with {mapper} of arity {arity}; \
                             a mapper takes one element"
                        )));
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

    fn check_terminator(&mut self, id: BlockId) -> Result<(), VerifyError> {
        match &self.block(id)?.terminator {
            Terminator::Return(atom) | Terminator::Exit(atom) => {
                self.check_atom_at(*atom, || format!("the terminator of block {id}"))
            }
            Terminator::Unreachable => Ok(()),
        }
    }

    fn check_atom(&self, statement: StatementId, atom: Atom) -> Result<(), VerifyError> {
        self.check_atom_at(atom, || format!("statement {statement}"))
    }

    fn check_atom_at(&self, atom: Atom, site: impl Fn() -> String) -> Result<(), VerifyError> {
        match atom {
            Atom::Value(value) => {
                if self.module.value(value).is_none() {
                    return Err(VerifyError(format!("{} references dead {value}", site())));
                }
                if !self.values_in_scope.contains(&value) {
                    return Err(VerifyError(format!(
                        "{} references {value} out of scope",
                        site()
                    )));
                }
                // Recursion admission: inside an eager initializer (and not inside a function constructed since entering it), a computed member of the group may only be an earlier one — or the member being initialized itself. A self-knot is admitted because the language accepts it: unused, the lowering drops it (mirroring the legacy path); used, the lowering rejects it with a diagnostic. Forward references stay unsatisfiable.
                for context in &self.init_contexts {
                    if self.function_depth == context.function_depth
                        && let Some(position) =
                            context.computed.iter().position(|&member| member == value)
                        && position > context.limit
                    {
                        return Err(VerifyError(format!(
                            "{} evaluates computed group member {value} before \
                             its initialization",
                            site()
                        )));
                    }
                }
                Ok(())
            }
            Atom::Function(function) => {
                if self.module.function(function).is_none() {
                    return Err(VerifyError(format!(
                        "{} references dead {function}",
                        site()
                    )));
                }
                if !self.functions_in_scope.contains(&function) {
                    return Err(VerifyError(format!(
                        "{} references {function} out of scope",
                        site()
                    )));
                }
                Ok(())
            }
            Atom::Constant(constant) => {
                if self.module.constant(constant).is_none() {
                    return Err(VerifyError(format!(
                        "{} references dead {constant}",
                        site()
                    )));
                }
                Ok(())
            }
        }
    }

    fn bind_value(&mut self, id: ValueId) -> Result<(), VerifyError> {
        if self.module.value(id).is_none() {
            return Err(VerifyError(format!("dead {id} is bound")));
        }
        if !self.defined_values.insert(id) {
            return Err(VerifyError(format!("{id} is defined more than once")));
        }
        self.values_in_scope.insert(id);
        Ok(())
    }

    fn bind_function(&mut self, id: FunctionId) -> Result<(), VerifyError> {
        self.function(id)?;
        if !self.bound_functions.insert(id) {
            return Err(VerifyError(format!("{id} is bound more than once")));
        }
        self.functions_in_scope.insert(id);
        Ok(())
    }

    /// Cross-check the registered schema links: every family lists exactly the constructors that back-link to it, each exactly once.
    fn check_schema_links(&self) -> Result<(), VerifyError> {
        let mut listed = vec![0usize; self.module.constructors().len()];
        for (index, family) in self.module.families().iter().enumerate() {
            for &constructor in &family.constructors {
                let Some(definition) = self.module.constructor(constructor) else {
                    return Err(VerifyError(format!(
                        "family ~d{index} lists dead constructor {constructor}"
                    )));
                };
                if definition.family.index() != index {
                    return Err(VerifyError(format!(
                        "family ~d{index} lists {constructor}, which belongs to {}",
                        definition.family
                    )));
                }
                listed[constructor.index()] += 1;
            }
        }
        for (index, count) in listed.iter().enumerate() {
            if *count != 1 {
                return Err(VerifyError(format!(
                    "constructor ~t{index} is listed {count} times by its family"
                )));
            }
        }
        for (index, constructor) in self.module.constructors().iter().enumerate() {
            if self.module.family(constructor.family).is_none() {
                return Err(VerifyError(format!(
                    "constructor ~t{index} references dead {}",
                    constructor.family
                )));
            }
        }
        Ok(())
    }

    /// After the walk: every live slot in a tombstoned arena was owned exactly once (the walk already rejected double ownership), so anything unvisited is leaked.
    fn check_ownership_complete(&self) -> Result<(), VerifyError> {
        for (index, slot) in self.module.blocks().iter().enumerate() {
            if slot.is_some() && !self.visited_blocks.contains(&BlockId(index as u32)) {
                return Err(VerifyError(format!("block ~b{index} has no owner")));
            }
        }
        for (index, slot) in self.module.statements().iter().enumerate() {
            if slot.is_some() && !self.visited_statements.contains(&StatementId(index as u32)) {
                return Err(VerifyError(format!("statement ~s{index} has no owner")));
            }
        }
        for (index, slot) in self.module.functions().iter().enumerate() {
            if slot.is_some() && !self.bound_functions.contains(&FunctionId(index as u32)) {
                return Err(VerifyError(format!("function ~f{index} is never bound")));
            }
        }
        for (index, slot) in self.module.values().iter().enumerate() {
            if slot.is_some() && !self.defined_values.contains(&ValueId(index as u32)) {
                return Err(VerifyError(format!("value ~v{index} is never defined")));
            }
        }
        for (index, slot) in self.module.rec_groups().iter().enumerate() {
            if slot.is_some() && !self.visited_groups.contains(&RecGroupId(index as u32)) {
                return Err(VerifyError(format!(
                    "recursive group ~r{index} has no owner"
                )));
            }
        }
        Ok(())
    }

    fn block(&self, id: BlockId) -> Result<&'m Block, VerifyError> {
        self.module
            .block(id)
            .ok_or_else(|| VerifyError(format!("dead block {id} is referenced")))
    }

    fn statement(&self, id: StatementId) -> Result<&'m Statement, VerifyError> {
        self.module
            .statement(id)
            .ok_or_else(|| VerifyError(format!("dead statement {id} is referenced")))
    }

    fn function(&self, id: FunctionId) -> Result<&'m Function, VerifyError> {
        self.module
            .function(id)
            .ok_or_else(|| VerifyError(format!("dead {id} is referenced")))
    }

    fn rec_group(&self, id: RecGroupId) -> Result<&'m RecGroup, VerifyError> {
        self.module
            .rec_group(id)
            .ok_or_else(|| VerifyError(format!("dead {id} is referenced")))
    }

    fn product(&self, id: ProductId) -> Result<&'m ProductSchema, VerifyError> {
        self.module
            .product(id)
            .ok_or_else(|| VerifyError(format!("dead {id} is referenced")))
    }

    fn family(&self, id: FamilyId) -> Result<&'m VariantFamily, VerifyError> {
        self.module
            .family(id)
            .ok_or_else(|| VerifyError(format!("dead {id} is referenced")))
    }

    fn constructor(&self, id: ConstructorId) -> Result<&'m Constructor, VerifyError> {
        self.module
            .constructor(id)
            .ok_or_else(|| VerifyError(format!("dead {id} is referenced")))
    }
}
