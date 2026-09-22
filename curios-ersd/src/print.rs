//! Deterministic printing — the `Stage::Ersd`/`Stage::ErsdOptm` observer and the diffing surface.
//!
//! The erased IR is A-normal-form Curios — `node.rs` calls it the direct-style ANF structure, `Statement::Functions` is a `let … and …` group, and `Terminator::Return` is a let-chain's tail — so it prints as Curios rather than as a statement dump. That keeps the rung continuous with `text`, `core` and `core-elab`, which are the same program in the same notation progressively decorated, and it retires a `return` that meant "this block's value" and read as a function return inside a nested arm. A function's body prints *at its binding site*, because that is the only placement a `let f(x) = body;` has: the flat section this printer used to emit had to split one binding into a declaration and a definition, which put `~v11$m` in a body whose binder was nowhere in view. Nesting restores the scope, so no capture list is needed. Why the whole grammar changed is `documentation/design/toolchain/the-erased-ir-prints-as-anf-curios.md`.
//!
//! **The elision rule.** Print every declaration something else refers to, state each fact once at its definition site, and spell every reference as short as is unambiguous. Applied to identities, an identity prints exactly when something else can refer to it: `~v`, `~f`, `~p`, `~d`, `~t` and `~x` print, and `~b`, `~s`, `~c` and `~r` are structural and never do. Applied to the header, only the schemas, families and foreign rows the program actually reaches are declared — a dense `products()` arena is never pruned, so a five-line program otherwise carries the whole standard library's 151 rows. There is deliberately no flag for the full header: a reader who needs the unreached rows has the module in front of them in a checkout.
//!
//! **The dump is faithful**, which fixes the boundary the rule stops at: faithful forbids *hiding*, not *not repeating*. Nothing here inlines a single-use binding, folds a constant, reorders a statement or drops dead code — a binding nothing reads prints as `let _ = …;`, which is how dead code announces itself rather than how it disappears. Two things are not printed, and both are absences rather than elisions: `Function::description` is a copy-budget annotation read by one optimizer pass, by no verifier, by no semantics and by no lowering (it does not exist in `curios-cont`), so it is not part of the program; and a `_` binder is what a hintless value with no uses *is*, following `curios-core`'s own rule for an unreferenced unnameable binder.
//!
//! Identities are spelled by the naming scheme shared with `curios-cont` and `curios-wasm` — see `documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md` — `~{kind}{index}`, with the stored debug name appended as `$hint`. The scheme requires the hint at definition sites; this printer also repeats it at every reference, deliberately, so a use far from its binder stays self-describing in a dump nobody scrolls back through. Constants print inline as literals, carrier and all (`2`, `2:int`, `7:byte`), for the reason scalar operations print named (`Nat/add`, never `+`): Ersd keeps its shapes distinct and has no types left to recover a carrier from, so the spelling is where the carrier lives.
//!
//! Equal modules print byte-identically, so a diff reads without cross-referencing arenas. The renderer walks with an explicit job stack and prints a deep module on the default test-thread stack; indentation saturates at a fixed depth so the output stays linear in module size instead of quadratic in nesting depth.

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, BlockId, CellOperation, Constant, ConstructorId, FamilyId, Field, FieldShape,
        ForeignId, FunctionId, Intrinsic, Module, Operation, ProductId, Rhs, SequenceGrain,
        SequenceOp, Statement, StatementId, Terminator, ValueId,
    },
    curios_num::{Grain, Rounding},
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt,
    },
};

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reach = Reach::of(self);
        let uses = super::Analysis::analyze(self);
        Printer {
            module: self,
            reach: &reach,
            uses: &uses,
            out: f,
        }
        .print()
    }
}

/// The nesting depth beyond which indentation stops growing, keeping printed size linear in module size.
const INDENT_SATURATION: usize = 64;

// === What the header declares ================================================

/// The nominal declarations the program reaches, and the functions some statement binds.
///
/// Reachability is a fixpoint rather than one sweep: a reached schema's fields may name further schemas through [`FieldShape::Product`] and [`FieldShape::Family`], and those rows have to be declared too or the header would describe a shape in terms of an identity it never introduced.
struct Reach {
    products: BTreeSet<ProductId>,
    families: BTreeSet<FamilyId>,
    foreigns: BTreeSet<ForeignId>,
    /// Every function some `Functions` or `Rec` statement binds — the ones whose bodies print at that binding site.
    bound: BTreeSet<FunctionId>,
    /// Field names carried by more than one reached row, which a projection must qualify to name unambiguously.
    ambiguous_fields: BTreeSet<String>,
}

impl Reach {
    fn of(module: &Module) -> Self {
        let mut reach = Self {
            products: BTreeSet::new(),
            families: BTreeSet::new(),
            foreigns: BTreeSet::new(),
            bound: BTreeSet::new(),
            ambiguous_fields: BTreeSet::new(),
        };

        let mut blocks: Vec<BlockId> = module.entry().into_iter().collect();
        let mut statements: Vec<StatementId> = module.items().to_vec();
        let mut seen_blocks = BTreeSet::new();
        loop {
            if let Some(id) = statements.pop() {
                match module.statement(id) {
                    Some(Statement::Let { rhs, .. }) => {
                        reach.name_rhs(module, rhs);
                        blocks.extend(rhs.sub_blocks());
                    }
                    Some(Statement::Functions { functions }) => {
                        for &function in functions {
                            if reach.bound.insert(function)
                                && let Some(definition) = module.function(function)
                            {
                                blocks.push(definition.body);
                            }
                        }
                    }
                    Some(Statement::Rec { group }) => {
                        if let Some(group) = module.rec_group(*group) {
                            for &function in &group.functions {
                                if reach.bound.insert(function)
                                    && let Some(definition) = module.function(function)
                                {
                                    blocks.push(definition.body);
                                }
                            }
                            blocks.extend(group.values.iter().map(|member| member.init));
                        }
                    }
                    None => {}
                }
                continue;
            }
            let Some(block) = blocks.pop() else { break };
            if !seen_blocks.insert(block) {
                continue;
            }
            if let Some(definition) = module.block(block) {
                statements.extend(definition.statements.iter().copied());
            }
        }

        reach.close_over_shapes(module);
        reach.find_ambiguous_fields(module);
        reach
    }

    /// The nominal identities one right-hand side names.
    ///
    /// Exhaustive with no wildcard arm on purpose: a new [`Rhs`] carrying a schema must not be able to reach the printer while quietly missing this, and the missing arm is what says so at compile time.
    fn name_rhs(&mut self, module: &Module, rhs: &Rhs) {
        match rhs {
            Rhs::Product { schema, .. } | Rhs::Project { schema, .. } => {
                self.products.insert(*schema);
            }
            Rhs::Construct { constructor, .. } => self.name_constructor(module, *constructor),
            Rhs::MatchVariant { family, arms, .. } => {
                self.families.insert(*family);
                for arm in arms {
                    self.name_constructor(module, arm.constructor);
                }
            }
            Rhs::Foreign { foreign, .. } => {
                self.foreigns.insert(*foreign);
            }
            Rhs::Alias(_)
            | Rhs::Apply { .. }
            | Rhs::Operation { .. }
            | Rhs::Sequence { .. }
            | Rhs::SwitchBool { .. }
            | Rhs::SwitchNat { .. }
            | Rhs::FoldNat { .. }
            | Rhs::FoldSequence { .. }
            | Rhs::UnconsSequence { .. }
            | Rhs::Cell { .. }
            | Rhs::Intrinsic { .. } => {}
        }
    }

    fn name_constructor(&mut self, module: &Module, constructor: ConstructorId) {
        if let Some(definition) = module.constructor(constructor) {
            self.families.insert(definition.family);
        }
    }

    /// Pull in every row a reached row's field shapes name, until nothing new appears.
    fn close_over_shapes(&mut self, module: &Module) {
        loop {
            let mut rows: Vec<&[Field]> = Vec::new();
            for &product in &self.products {
                if let Some(schema) = module.products().get(product.0 as usize) {
                    rows.push(&schema.fields);
                }
            }
            for &family in &self.families {
                let Some(definition) = module.families().get(family.0 as usize) else {
                    continue;
                };
                for &constructor in &definition.constructors {
                    if let Some(constructor) = module.constructor(constructor) {
                        rows.push(&constructor.fields);
                    }
                }
            }

            let mut products = Vec::new();
            let mut families = Vec::new();
            for row in rows {
                for field in row {
                    match field.shape {
                        FieldShape::Product(schema) => products.push(schema),
                        FieldShape::Family(family) => families.push(family),
                        _ => {}
                    }
                }
            }

            let mut grew = false;
            for schema in products {
                grew |= self.products.insert(schema);
            }
            for family in families {
                grew |= self.families.insert(family);
            }
            if !grew {
                return;
            }
        }
    }

    /// A field name carried by two distinct reached rows cannot name a field on its own, so a projection through it spells its schema.
    fn find_ambiguous_fields(&mut self, module: &Module) {
        let mut owners: BTreeMap<&str, ProductId> = BTreeMap::new();
        for &product in &self.products {
            let Some(schema) = module.products().get(product.0 as usize) else {
                continue;
            };
            for field in &schema.fields {
                let Some(name) = field.debug_name.as_deref() else {
                    continue;
                };
                match owners.get(name) {
                    Some(&owner) if owner != product => {
                        self.ambiguous_fields.insert(name.to_string());
                    }
                    Some(_) => {}
                    None => {
                        owners.insert(name, product);
                    }
                }
            }
        }
    }
}

// === The renderer ============================================================

/// One rendering step; sequences are pushed in output order.
enum Job {
    /// An indented line of text.
    Line(String),
    /// A block's statements followed by its tail, one level deeper. `suffix` closes the tail line — `;` where a binding owns the body, nothing where a match arm does.
    Body(BlockId, &'static str),
    /// One statement, expanded into lines and nested bodies. A module-scope item breaks after its `=` even where a statement inside a block would ride the line, so every top-level declaration has the same shape.
    Statement {
        id: StatementId,
        top: bool,
    },
    Indent,
    Dedent,
}

struct Printer<'m, 'a, 'f, 'o> {
    module: &'m Module,
    reach: &'a Reach,
    uses: &'a super::Analysis,
    out: &'f mut fmt::Formatter<'o>,
}

impl Printer<'_, '_, '_, '_> {
    fn print(&mut self) -> fmt::Result {
        let mut jobs = Vec::new();

        for (index, schema) in self.module.products().iter().enumerate() {
            let id = ProductId(index as u32);
            if !self.reach.products.contains(&id) {
                continue;
            }
            let shared = if schema.shared { " shared" } else { "" };
            jobs.push(Job::Line(format!(
                "product {id}{}({}){shared}",
                hint(&schema.debug_name),
                fields(&schema.fields)
            )));
        }
        for (index, family) in self.module.families().iter().enumerate() {
            let id = FamilyId(index as u32);
            if !self.reach.families.contains(&id) {
                continue;
            }
            let constructors = family
                .constructors
                .iter()
                .map(|&constructor| {
                    let definition = self.module.constructor(constructor).expect("live");
                    format!(
                        "{constructor}{}({})",
                        hint(&definition.debug_name),
                        fields(&definition.fields)
                    )
                })
                .collect::<Vec<_>>()
                .join(" ");
            jobs.push(Job::Line(format!(
                "family {id}{} {{ {constructors} }}",
                hint(&family.debug_name)
            )));
        }
        for (index, row) in self.module.foreigns().iter().enumerate() {
            let id = ForeignId(index as u32);
            if !self.reach.foreigns.contains(&id) {
                continue;
            }
            jobs.push(Job::Line(format!(
                "foreign {id} {}/{}",
                row.namespace, row.name
            )));
        }

        for &item in self.module.items() {
            jobs.push(Job::Statement {
                id: item,
                top: true,
            });
        }
        if let Some(entry) = self.module.entry() {
            jobs.push(Job::Line("entry =".into()));
            jobs.push(Job::Body(entry, ";"));
        }

        // A live function no statement binds has nowhere to nest, which `verify.rs` rules out — but this printer is also how a malformed module is read, so an orphan is shown rather than dropped.
        for (index, slot) in self.module.functions().iter().enumerate() {
            let id = FunctionId(index as u32);
            let Some(function) = slot else { continue };
            if self.reach.bound.contains(&id) {
                continue;
            }
            jobs.push(Job::Line(format!(
                "unbound {}{} =",
                self.function(id),
                self.params(&function.params)
            )));
            jobs.push(Job::Body(function.body, ";"));
        }

        jobs.reverse();
        let mut stack = jobs;
        let mut indent = 0usize;
        while let Some(job) = stack.pop() {
            match job {
                Job::Line(line) => {
                    for _ in 0..indent.min(INDENT_SATURATION) {
                        self.out.write_str("    ")?;
                    }
                    self.out.write_str(&line)?;
                    self.out.write_str("\n")?;
                }
                Job::Indent => indent += 1,
                Job::Dedent => indent -= 1,
                Job::Statement { id, top } => self.expand_statement(&mut stack, id, top),
                Job::Body(block, suffix) => {
                    let contents = self.module.block(block).expect("live block");
                    let mut sequence = Vec::with_capacity(contents.statements.len() + 3);
                    sequence.push(Job::Indent);
                    sequence.extend(
                        contents
                            .statements
                            .iter()
                            .map(|&id| Job::Statement { id, top: false }),
                    );
                    sequence.push(Job::Line(format!(
                        "{}{suffix}",
                        self.tail(&contents.terminator)
                    )));
                    sequence.push(Job::Dedent);
                    push_sequence(&mut stack, sequence);
                }
            }
        }
        Ok(())
    }

    /// How a block leaves, as the tail term of its let-chain. `exit` and `unreachable` have no surface spelling; everything else is the atom the chain evaluates to.
    fn tail(&self, terminator: &Terminator) -> String {
        match terminator {
            Terminator::Return(atom) => self.atom(*atom),
            Terminator::Exit(atom) => format!("exit {}", self.atom(*atom)),
            Terminator::Unreachable => "unreachable".into(),
        }
    }

    /// A statement-free block's tail, for an arm that rides its own arrow rather than opening a body.
    fn rides(&self, block: BlockId) -> Option<String> {
        let contents = self.module.block(block)?;
        contents
            .statements
            .is_empty()
            .then(|| self.tail(&contents.terminator))
    }

    /// An arm: its pattern and either the tail it rides or the body it opens.
    fn arm(&self, sequence: &mut Vec<Job>, pattern: String, block: BlockId) {
        match self.rides(block) {
            Some(tail) => sequence.push(Job::Line(format!("| {pattern} => {tail}"))),
            None => {
                sequence.push(Job::Line(format!("| {pattern} =>")));
                sequence.push(Job::Body(block, ""));
            }
        }
    }

    /// A `let … and …` group: one member per keyword, the last closing the binding.
    fn group(
        &self,
        sequence: &mut Vec<Job>,
        first: &'static str,
        functions: &[FunctionId],
        values: &[super::RecValue],
    ) {
        let total = functions.len() + values.len();
        let mut emitted = 0;
        let header = |emitted: usize| if emitted == 0 { first } else { "and" };

        for &function in functions {
            let Some(definition) = self.module.function(function) else {
                continue;
            };
            let keyword = header(emitted);
            sequence.push(Job::Line(format!(
                "{keyword} {}{} =",
                self.function(function),
                self.params(&definition.params)
            )));
            emitted += 1;
            sequence.push(Job::Body(
                definition.body,
                if emitted == total { ";" } else { "" },
            ));
        }
        for member in values {
            let keyword = header(emitted);
            sequence.push(Job::Line(format!(
                "{keyword} {} =",
                self.binder(member.value)
            )));
            emitted += 1;
            sequence.push(Job::Body(
                member.init,
                if emitted == total { ";" } else { "" },
            ));
        }
    }

    fn expand_statement(&self, stack: &mut Vec<Job>, id: StatementId, top: bool) {
        let statement = self.module.statement(id).expect("live statement");
        let mut sequence = Vec::new();
        match statement {
            Statement::Functions { functions } => {
                self.group(&mut sequence, "let", functions, &[]);
            }
            Statement::Rec { group } => {
                let definition = self.module.rec_group(*group).expect("live group");
                self.group(
                    &mut sequence,
                    "let rec",
                    &definition.functions,
                    &definition.values,
                );
            }
            Statement::Let { result, rhs } => {
                let bound = self.binder(*result);
                // One match over every form, so a new `Rhs` cannot reach the printer without an arm: the one-line forms fill `simple` and are laid out below, the eliminators push their own `match … end;` here.
                let mut simple = None;
                match rhs {
                    Rhs::Alias(atom) => simple = Some(self.atom(*atom)),
                    Rhs::Apply { callee, arguments } => {
                        simple = Some(format!("{}({})", self.atom(*callee), self.atoms(arguments)));
                    }
                    Rhs::Operation {
                        operation,
                        operands,
                    } => {
                        simple = Some(format!(
                            "{}({})",
                            operation_name(*operation),
                            self.atoms(operands)
                        ));
                    }
                    Rhs::Sequence {
                        operation,
                        operands,
                    } => {
                        simple = Some(format!(
                            "{}({})",
                            sequence_name(*operation),
                            self.atoms(operands)
                        ));
                    }
                    Rhs::Product { schema, fields } => {
                        simple = Some(format!(
                            "{}{}",
                            self.product(*schema),
                            self.row(*schema, fields)
                        ));
                    }
                    Rhs::Construct {
                        constructor,
                        fields,
                    } => {
                        simple = Some(format!(
                            "{}({})",
                            self.constructor(*constructor),
                            self.atoms(fields)
                        ));
                    }
                    Rhs::Project {
                        schema,
                        product,
                        field,
                    } => {
                        simple = Some(format!(
                            "{}.{}",
                            self.atom(*product),
                            self.field(*schema, *field)
                        ));
                    }
                    Rhs::Cell {
                        operation,
                        operands,
                    } => {
                        simple = Some(format!(
                            "{}({})",
                            cell_name(*operation),
                            self.atoms(operands)
                        ));
                    }
                    Rhs::Foreign { foreign, operands } => {
                        let row = self.module.foreign(*foreign).expect("live foreign");
                        simple = Some(format!(
                            "{}/{}({})",
                            row.namespace,
                            row.name,
                            self.atoms(operands)
                        ));
                    }
                    Rhs::Intrinsic {
                        intrinsic,
                        operands,
                    } => {
                        simple = Some(format!(
                            "{}({})",
                            intrinsic_name(*intrinsic),
                            self.atoms(operands)
                        ));
                    }

                    // The eliminators: `let x =`, then an indented `match … end;`.
                    Rhs::MatchVariant {
                        scrutinee,
                        arms,
                        default,
                        ..
                    } => {
                        sequence.push(Job::Line(format!("let {bound} =")));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Line(format!("match {}", self.atom(*scrutinee))));
                        for arm in arms {
                            let binders = arm
                                .bindings
                                .iter()
                                .map(|&binder| self.binder(binder))
                                .collect::<Vec<_>>()
                                .join(", ");
                            self.arm(
                                &mut sequence,
                                format!("{}({binders})", self.constructor(arm.constructor)),
                                arm.block,
                            );
                        }
                        if let Some(default) = default {
                            self.arm(&mut sequence, "_".into(), *default);
                        }
                        sequence.push(Job::Line("end;".into()));
                        sequence.push(Job::Dedent);
                    }
                    Rhs::SwitchBool {
                        scrutinee,
                        if_false,
                        if_true,
                    } => {
                        sequence.push(Job::Line(format!("let {bound} =")));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Line(format!("match {}", self.atom(*scrutinee))));
                        // True first, as the surface writes a `Bool` match.
                        self.arm(&mut sequence, "true".into(), *if_true);
                        self.arm(&mut sequence, "false".into(), *if_false);
                        sequence.push(Job::Line("end;".into()));
                        sequence.push(Job::Dedent);
                    }
                    Rhs::SwitchNat {
                        scrutinee,
                        cases,
                        default,
                    } => {
                        sequence.push(Job::Line(format!("let {bound} =")));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Line(format!("match {}", self.atom(*scrutinee))));
                        for case in cases {
                            self.arm(&mut sequence, format!("{}", case.key), case.block);
                        }
                        self.arm(&mut sequence, "_".into(), *default);
                        sequence.push(Job::Line("end;".into()));
                        sequence.push(Job::Dedent);
                    }
                    Rhs::FoldNat {
                        scrutinee,
                        zero,
                        step,
                    } => {
                        sequence.push(Job::Line(format!("let {bound} =")));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Line(format!("match {}", self.atom(*scrutinee))));
                        self.arm(&mut sequence, "0".into(), *zero);
                        self.arm(
                            &mut sequence,
                            format!(
                                "{} + 1; {}",
                                self.binder(step.predecessor),
                                self.binder(step.hypothesis)
                            ),
                            step.block,
                        );
                        sequence.push(Job::Line("end;".into()));
                        sequence.push(Job::Dedent);
                    }
                    Rhs::FoldSequence {
                        grain,
                        scrutinee,
                        empty,
                        step,
                    } => {
                        let g = grain_prefix(grain);
                        sequence.push(Job::Line(format!("let {bound} =")));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Line(format!("match {}", self.atom(*scrutinee))));
                        self.arm(&mut sequence, format!("{g}[]"), *empty);
                        self.arm(
                            &mut sequence,
                            format!(
                                "{g}[{}, ..{}]; {}",
                                self.binder(step.element),
                                self.binder(step.suffix),
                                self.binder(step.accumulator)
                            ),
                            step.block,
                        );
                        sequence.push(Job::Line("end;".into()));
                        sequence.push(Job::Dedent);
                    }
                    Rhs::UnconsSequence {
                        grain,
                        scrutinee,
                        empty,
                        cons,
                    } => {
                        let g = grain_prefix(grain);
                        sequence.push(Job::Line(format!("let {bound} =")));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Line(format!("match {}", self.atom(*scrutinee))));
                        self.arm(&mut sequence, format!("{g}[]"), *empty);
                        self.arm(
                            &mut sequence,
                            format!(
                                "{g}[{}, ..{}]",
                                self.binder(cons.element),
                                self.binder(cons.suffix)
                            ),
                            cons.block,
                        );
                        sequence.push(Job::Line("end;".into()));
                        sequence.push(Job::Dedent);
                    }
                }
                // A one-line form rides its `=` inside a block, and breaks under it at module scope, so every top-level declaration opens the same way.
                if let Some(text) = simple {
                    if top {
                        sequence.push(Job::Line(format!("let {bound} =")));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Line(format!("{text};")));
                        sequence.push(Job::Dedent);
                    } else {
                        sequence.push(Job::Line(format!("let {bound} = {text};")));
                    }
                }
            }
        }
        push_sequence(stack, sequence);
    }

    // === Spellings ===========================================================

    /// A binding position. A value with a hint spells it; a hintless one nothing reads spells `_`, since that is what it is.
    fn binder(&self, id: ValueId) -> String {
        match self.module.value(id) {
            Some(definition) => match &definition.debug_name {
                Some(name) => format!("{id}${name}"),
                None if self.uses.value_uses(id) == 0 => "_".into(),
                None => format!("{id}"),
            },
            None => format!("{id}"),
        }
    }

    fn params(&self, params: &[ValueId]) -> String {
        format!(
            "({})",
            params
                .iter()
                .map(|&param| self.binder(param))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    fn atom(&self, atom: Atom) -> String {
        match atom {
            Atom::Value(value) => spell_value(self.module, value),
            Atom::Function(function) => self.function(function),
            Atom::Constant(constant) => {
                render_constant(self.module.constant(constant).expect("live constant"))
            }
        }
    }

    fn atoms(&self, atoms: &[Atom]) -> String {
        atoms
            .iter()
            .map(|&atom| self.atom(atom))
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn function(&self, id: FunctionId) -> String {
        spell_function(self.module, id)
    }

    fn product(&self, id: ProductId) -> String {
        match self.module.products().get(id.0 as usize) {
            Some(schema) => format!("{id}{}", hint(&schema.debug_name)),
            None => format!("{id}"),
        }
    }

    fn constructor(&self, id: ConstructorId) -> String {
        match self.module.constructor(id) {
            Some(definition) => format!("{id}{}", hint(&definition.debug_name)),
            None => format!("{id}"),
        }
    }

    /// A product literal's field row, named as the schema names its entries.
    fn row(&self, schema: ProductId, fields: &[Atom]) -> String {
        if fields.is_empty() {
            return " {}".into();
        }
        // Never qualified: a literal names its schema in front of the row, so the entry names are already unambiguous.
        let entries = fields
            .iter()
            .enumerate()
            .map(|(index, &atom)| {
                format!(
                    "{} = {}",
                    self.field_label(schema, index as u32),
                    self.atom(atom)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!(" {{ {entries} }}")
    }

    /// One field of a schema, by name where it has one and by position otherwise.
    fn field_label(&self, schema: ProductId, index: u32) -> String {
        let Some(definition) = self.module.products().get(schema.0 as usize) else {
            return format!("{index}");
        };
        match definition
            .fields
            .get(index as usize)
            .and_then(|field| field.debug_name.as_deref())
        {
            Some(name) => name.to_string(),
            None => format!("{index}"),
        }
    }

    /// The field of a projection. Nothing else on the line names the schema, so a name some other reached row also carries takes its own — `build_shorten`'s shortest-unambiguous rule, applied to fields.
    fn field(&self, schema: ProductId, index: u32) -> String {
        let label = self.field_label(schema, index);
        if self.reach.ambiguous_fields.contains(&label) {
            return format!("{schema}/{label}");
        }
        label
    }
}

/// A value as the printer spells it — its id, then `$hint` when it carries a source name — which is also how a structural fault names it, since that reader works from `wonder stage ersd`, where `~v155$table` locates a definition and `~v155` does not. A refusal a program earns names a member by its source name alone, because its reader never sees the stage. A dead id spells bare, so a message about one can still be formed.
pub(crate) fn spell_value(module: &Module, id: ValueId) -> String {
    match module.value(id) {
        Some(definition) => format!("{id}{}", hint(&definition.debug_name)),
        None => format!("{id}"),
    }
}

pub(crate) fn spell_function(module: &Module, id: FunctionId) -> String {
    match module.function(id) {
        Some(definition) => format!("{id}{}", hint(&definition.debug_name)),
        None => format!("{id}"),
    }
}

/// Push a job sequence given in output order.
fn push_sequence(stack: &mut Vec<Job>, sequence: Vec<Job>) {
    stack.extend(sequence.into_iter().rev());
}

fn hint(debug_name: &Option<String>) -> String {
    match debug_name {
        Some(name) => format!("${name}"),
        None => String::new(),
    }
}

/// A schema's field row — a constructor's payload or a product's entries: the field name (or its index), with every recorded carrier shape marked so the erasure-side classification is legible in a dump. `Opaque` prints bare, since it is the absence of a claim.
fn fields(row: &[Field]) -> String {
    row.iter()
        .enumerate()
        .map(|(index, field)| {
            let name = match &field.debug_name {
                Some(name) => name.clone(),
                None => format!("{index}"),
            };
            match field.shape {
                FieldShape::Immediate => format!("{name}:immediate"),
                FieldShape::Number => format!("{name}:number"),
                FieldShape::Flt => format!("{name}:flt"),
                FieldShape::Packed(Grain::X) => format!("{name}:bytes"),
                FieldShape::Packed(Grain::B) => format!("{name}:bits"),
                FieldShape::List => format!("{name}:list"),
                FieldShape::Closure(arity) => format!("{name}:closure/{arity}"),
                FieldShape::Product(schema) => format!("{name}:product/{schema}"),
                FieldShape::Family(family) => format!("{name}:family/{family}"),
                FieldShape::Opaque => name,
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// The pattern prefix that selects a sequence carrier, exactly as the surface writes a packed fold: bare for `List`, `b` for `Bits`, `x` for `Bytes`.
fn grain_prefix(grain: &SequenceGrain) -> &'static str {
    match grain {
        SequenceGrain::List => "",
        SequenceGrain::Bin(Grain::B) => "b",
        SequenceGrain::Bin(Grain::X) => "x",
    }
}

/// The carrier a sequence operation belongs to, as `/sys` modules it.
fn grain_carrier(grain: Grain) -> &'static str {
    match grain {
        Grain::B => "Bits",
        Grain::X => "Bytes",
    }
}

// === Operation names =========================================================
//
// The table follows `/sys`'s own spelling, which is where a reader met these operations — `Nat/add`, `Byte/to_nat`, `Flt/of_le_bytes`. `curios-core`'s printer hardcodes the same paths for the same reason: this is display text, not a name the compiler emits, so it reaches no `SyntaxRegistry` slot and no prelude declaration. A handful of operations are lowering-internal and have no `/sys` declaration to follow — `Bool/neq`, which the surface desugars through `Bool/xor`, and `List/build`, which is what a list literal lowers to — and those are spelled in the same style rather than left to leak a Rust variant name.

fn operation_name(operation: Operation) -> String {
    let name = match operation {
        Operation::BoolAnd => "Bool/and",
        Operation::BoolOr => "Bool/or",
        Operation::BoolXor => "Bool/xor",
        Operation::BoolEql => "Bool/eql",
        Operation::BoolNeq => "Bool/neq",
        Operation::NatEql => "Nat/eql",
        Operation::NatNeq => "Nat/neq",
        Operation::NatAdd => "Nat/add",
        Operation::NatSub => "Nat/sub",
        Operation::NatMul => "Nat/mul",
        Operation::NatLt => "Nat/lt",
        Operation::NatDiv => "Nat/div",
        Operation::NatRem => "Nat/rem",
        Operation::NatLe => "Nat/le",
        Operation::NatAnd => "Nat/and",
        Operation::NatOr => "Nat/or",
        Operation::NatXor => "Nat/xor",
        Operation::NatShl => "Nat/shl",
        Operation::NatShr => "Nat/shr",
        Operation::ByteToNat => "Byte/to_nat",
        Operation::NatToByte => "Nat/to_byte",
        Operation::IntEql => "Int/eql",
        Operation::IntNeq => "Int/neq",
        Operation::IntAdd => "Int/add",
        Operation::IntSub => "Int/sub",
        Operation::IntMul => "Int/mul",
        Operation::IntDiv => "Int/div",
        Operation::IntRem => "Int/rem",
        Operation::IntLt => "Int/lt",
        Operation::IntLe => "Int/le",
        Operation::IntAnd => "Int/and",
        Operation::IntOr => "Int/or",
        Operation::IntXor => "Int/xor",
        Operation::IntShl => "Int/shl",
        Operation::IntShr => "Int/shr",
        Operation::FltAdd(rounding) => return flt_rounded(rounding, "add"),
        Operation::FltSub(rounding) => return flt_rounded(rounding, "sub"),
        Operation::FltMul(rounding) => return flt_rounded(rounding, "mul"),
        Operation::FltDiv(rounding) => return flt_rounded(rounding, "div"),
        Operation::FltFma(rounding) => return flt_rounded(rounding, "fma"),
        Operation::FltRem => "Flt/rem",
        Operation::FltEql => "Flt/eql",
        Operation::FltNeq => "Flt/neq",
        Operation::FltLt => "Flt/lt",
        Operation::FltLe => "Flt/le",
        Operation::FltMin => "Flt/min",
        Operation::FltMax => "Flt/max",
        Operation::FltCopysign => "Flt/copysign",
        Operation::FltNeg => "Flt/neg",
        Operation::FltAbs => "Flt/abs",
        Operation::FltSqrt(rounding) => return flt_rounded(rounding, "sqrt"),
        Operation::FltRoundIntegral(rounding) => {
            return format!("Flt/{}", rounding.integral_label());
        }
        Operation::NatToInt => "Nat/to_int",
        Operation::NatToFlt(Rounding::TiesToEven) => "Nat/to_flt",
        Operation::NatToFlt(rounding) => return flt_rounded(rounding, "of_nat"),
        Operation::IntToNat => "Int/to_nat",
        Operation::IntToFlt(Rounding::TiesToEven) => "Int/to_flt",
        Operation::IntToFlt(rounding) => return flt_rounded(rounding, "of_int"),
        Operation::FltToNat => "Flt/to_nat",
        Operation::FltToInt => "Flt/to_int",
        Operation::FltToLeBytes => "Flt/to_le_bytes",
        Operation::FltOfLeBytes => "Flt/of_le_bytes",
    };

    name.to_string()
}

/// The `/sys` path of a float operation rounded in `rounding`: `Flt/add` in the default direction, `Flt/toward_zero/add` in another.
fn flt_rounded(rounding: Rounding, operation: &str) -> String {
    match rounding {
        Rounding::TiesToEven => format!("Flt/{operation}"),
        rounding => format!("Flt/{}/{operation}", rounding.label()),
    }
}

fn sequence_name(operation: SequenceOp) -> String {
    let (carrier, name) = match operation {
        SequenceOp::BinLen(grain) => (grain_carrier(grain), "len"),
        SequenceOp::BinEql(grain) => (grain_carrier(grain), "eql"),
        SequenceOp::BinGet(grain) => (grain_carrier(grain), "get"),
        SequenceOp::BinSlice(grain) => (grain_carrier(grain), "slice"),
        SequenceOp::BinAppend(grain) => (grain_carrier(grain), "append"),
        SequenceOp::BinConcat(grain) => (grain_carrier(grain), "concat"),
        SequenceOp::BinReplicate(grain) => (grain_carrier(grain), "replicate"),
        // The grain names the operand, so the spelling is the direction it is read out of.
        SequenceOp::BinReinterp(grain) => (
            grain_carrier(grain),
            match grain {
                Grain::X => "to_bits",
                Grain::B => "to_bytes",
            },
        ),
        SequenceOp::BinAnd(grain) => (grain_carrier(grain), "and"),
        SequenceOp::BinOr(grain) => (grain_carrier(grain), "or"),
        SequenceOp::BinXor(grain) => (grain_carrier(grain), "xor"),
        SequenceOp::ListBuild => ("List", "build"),
        SequenceOp::ListLen => ("List", "len"),
        SequenceOp::ListGet => ("List", "get"),
        SequenceOp::ListSlice => ("List", "slice"),
        SequenceOp::ListAppend => ("List", "append"),
        SequenceOp::ListConcat => ("List", "concat"),
    };
    format!("{carrier}/{name}")
}

fn cell_name(operation: CellOperation) -> &'static str {
    match operation {
        CellOperation::New => "Cell/new",
        CellOperation::Get => "Cell/get",
        CellOperation::Set => "Cell/set",
    }
}

fn intrinsic_name(intrinsic: Intrinsic) -> &'static str {
    match intrinsic {
        Intrinsic::ListMap => "List/map",
    }
}

fn render_constant(constant: &Constant) -> String {
    match constant {
        Constant::Unit => "unit".into(),
        Constant::Bool(value) => format!("{value}"),
        Constant::Nat(value) => format!("{value}"),
        Constant::Int(value) => format!("{value}:int"),
        Constant::Flt(value) => {
            let float = f64::from(*value);
            if float.is_finite() {
                format!("{float:?}:flt")
            } else {
                format!("flt:0x{:016x}", float.to_bits())
            }
        }
        Constant::Byte(value) => format!("{value}:byte"),
        Constant::Bin(grain, bits) => {
            let rendered = bits
                .to_packed_bytes()
                .iter()
                .map(|byte| format!("{byte:02x}"))
                .collect::<String>();
            match grain {
                Grain::B => format!("b\"{rendered}\"/{}", bits.bit_length()),
                Grain::X => format!("x\"{rendered}\""),
            }
        }
        Constant::Handle(token) => format!("io:{token}"),
    }
}
