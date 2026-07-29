//! Deterministic printing — the `Stage::Ersd`/`Stage::ErsdOptm` observer and
//! the diffing surface.
//!
//! Identities are spelled by the unified naming scheme — `~{kind}{index}`,
//! with the stored debug name appended as `$hint` when present — and constants
//! print inline as literals, so equal modules print byte-identically and a
//! diff reads without cross-referencing arenas. Function bodies print in one
//! flat section in identity order (a function's binding statement names it,
//! its body does not nest), so nesting depth comes only from control blocks;
//! the renderer walks those with an explicit job stack and prints a deep
//! module on the default test-thread stack. Indentation saturates at a fixed
//! depth so the output stays linear in module size instead of quadratic in
//! nesting depth.

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, BlockId, Constant, FunctionId, Module, Rhs, Statement, StatementId, Terminator,
        ValueId,
    },
    curios_base::Grain,
    std::fmt,
};

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Printer {
            module: self,
            out: f,
        }
        .print()
    }
}

/// The nesting depth beyond which indentation stops growing, keeping printed
/// size linear in module size.
const INDENT_SATURATION: usize = 64;

/// One rendering step; sequences are pushed in output order.
enum Job {
    /// An indented line of text.
    Line(String),
    /// One statement, expanded into lines and nested blocks.
    Statement(StatementId),
    /// A labeled nested block: `label {`, the block's contents, `}`.
    Blocked(String, BlockId),
    /// A block's terminator line.
    Terminator(BlockId),
    Indent,
    Dedent,
}

struct Printer<'m, 'f, 'o> {
    module: &'m Module,
    out: &'f mut fmt::Formatter<'o>,
}

impl Printer<'_, '_, '_> {
    fn print(&mut self) -> fmt::Result {
        let mut jobs = Vec::new();

        for (index, schema) in self.module.products().iter().enumerate() {
            jobs.push(Job::Line(format!(
                "product ~p{index}{}({})",
                hint(&schema.debug_name),
                fields(&schema.fields)
            )));
        }
        for (index, family) in self.module.families().iter().enumerate() {
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
                "family ~d{index}{} {{ {constructors} }}",
                hint(&family.debug_name)
            )));
        }
        for (index, row) in self.module.foreigns().iter().enumerate() {
            jobs.push(Job::Line(format!(
                "foreign ~x{index} {}/{}",
                row.namespace, row.name
            )));
        }

        for &item in self.module.items() {
            jobs.push(Job::Statement(item));
        }
        if let Some(entry) = self.module.entry() {
            jobs.push(Job::Blocked("entry".into(), entry));
        }
        for (index, slot) in self.module.functions().iter().enumerate() {
            let Some(function) = slot else { continue };
            let params = function
                .params
                .iter()
                .map(|&param| self.value(param))
                .collect::<Vec<_>>()
                .join(", ");
            jobs.push(Job::Blocked(
                format!("function ~f{index}{}({params})", hint(&function.debug_name)),
                function.body,
            ));
        }

        jobs.reverse();
        let mut stack = jobs;
        let mut indent = 0usize;
        while let Some(job) = stack.pop() {
            match job {
                Job::Line(line) => {
                    for _ in 0..indent.min(INDENT_SATURATION) {
                        self.out.write_str("  ")?;
                    }
                    self.out.write_str(&line)?;
                    self.out.write_str("\n")?;
                }
                Job::Indent => indent += 1,
                Job::Dedent => indent -= 1,
                Job::Statement(statement) => self.expand_statement(&mut stack, statement),
                Job::Blocked(label, block) => {
                    let contents = self.module.block(block).expect("live block");
                    let mut sequence = Vec::with_capacity(contents.statements.len() + 4);
                    sequence.push(Job::Line(format!("{label} {{")));
                    sequence.push(Job::Indent);
                    sequence.extend(contents.statements.iter().map(|&id| Job::Statement(id)));
                    sequence.push(Job::Terminator(block));
                    sequence.push(Job::Dedent);
                    sequence.push(Job::Line("}".into()));
                    push_sequence(&mut stack, sequence);
                }
                Job::Terminator(block) => {
                    let line = match &self.module.block(block).expect("live block").terminator {
                        Terminator::Return(atom) => format!("return {}", self.atom(*atom)),
                        Terminator::Exit(atom) => format!("exit {}", self.atom(*atom)),
                        Terminator::Unreachable => "unreachable".into(),
                    };
                    for _ in 0..indent.min(INDENT_SATURATION) {
                        self.out.write_str("  ")?;
                    }
                    self.out.write_str(&line)?;
                    self.out.write_str("\n")?;
                }
            }
        }
        Ok(())
    }

    fn expand_statement(&self, stack: &mut Vec<Job>, id: StatementId) {
        let statement = self.module.statement(id).expect("live statement");
        let mut sequence = Vec::new();
        match statement {
            Statement::Functions { functions } => {
                let members = functions
                    .iter()
                    .map(|&function| self.function(function))
                    .collect::<Vec<_>>()
                    .join(" ");
                sequence.push(Job::Line(format!("functions {members}")));
            }
            Statement::Rec { group } => {
                let definition = self.module.rec_group(*group).expect("live group");
                sequence.push(Job::Line(format!("rec {group} {{")));
                sequence.push(Job::Indent);
                if !definition.functions.is_empty() {
                    let members = definition
                        .functions
                        .iter()
                        .map(|&function| self.function(function))
                        .collect::<Vec<_>>()
                        .join(" ");
                    sequence.push(Job::Line(format!("functions {members}")));
                }
                for member in &definition.values {
                    sequence.push(Job::Blocked(
                        format!("{} = init", self.value(member.value)),
                        member.init,
                    ));
                }
                sequence.push(Job::Dedent);
                sequence.push(Job::Line("}".into()));
            }
            Statement::Let { result, rhs } => {
                let bound = self.value(*result);
                match rhs {
                    Rhs::Alias(atom) => {
                        sequence.push(Job::Line(format!("{bound} = alias {}", self.atom(*atom))));
                    }
                    Rhs::Apply { callee, arguments } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = apply {}({})",
                            self.atom(*callee),
                            self.atoms(arguments)
                        )));
                    }
                    Rhs::Operation {
                        operation,
                        operands,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = {operation:?}({})",
                            self.atoms(operands)
                        )));
                    }
                    Rhs::Sequence {
                        operation,
                        operands,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = {operation:?}({})",
                            self.atoms(operands)
                        )));
                    }
                    Rhs::Product { schema, fields } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = product {schema}({})",
                            self.atoms(fields)
                        )));
                    }
                    Rhs::Construct {
                        constructor,
                        fields,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = construct {constructor}({})",
                            self.atoms(fields)
                        )));
                    }
                    Rhs::Project {
                        schema,
                        product,
                        field,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = project {schema}.{field} {}",
                            self.atom(*product)
                        )));
                    }
                    Rhs::MatchVariant {
                        family,
                        scrutinee,
                        arms,
                        default,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = match {family} {} {{",
                            self.atom(*scrutinee)
                        )));
                        sequence.push(Job::Indent);
                        for arm in arms {
                            let binders = arm
                                .bindings
                                .iter()
                                .map(|&binder| self.value(binder))
                                .collect::<Vec<_>>()
                                .join(", ");
                            sequence.push(Job::Blocked(
                                format!("{}({binders}) =>", arm.constructor),
                                arm.block,
                            ));
                        }
                        if let Some(default) = default {
                            sequence.push(Job::Blocked("default =>".into(), *default));
                        }
                        sequence.push(Job::Dedent);
                        sequence.push(Job::Line("}".into()));
                    }
                    Rhs::SwitchBool {
                        scrutinee,
                        if_false,
                        if_true,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = switch-bool {} {{",
                            self.atom(*scrutinee)
                        )));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Blocked("false =>".into(), *if_false));
                        sequence.push(Job::Blocked("true =>".into(), *if_true));
                        sequence.push(Job::Dedent);
                        sequence.push(Job::Line("}".into()));
                    }
                    Rhs::SwitchNat {
                        scrutinee,
                        cases,
                        default,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = switch-nat {} {{",
                            self.atom(*scrutinee)
                        )));
                        sequence.push(Job::Indent);
                        for case in cases {
                            sequence.push(Job::Blocked(format!("{} =>", case.key), case.block));
                        }
                        sequence.push(Job::Blocked("default =>".into(), *default));
                        sequence.push(Job::Dedent);
                        sequence.push(Job::Line("}".into()));
                    }
                    Rhs::FoldNat {
                        scrutinee,
                        zero,
                        step,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = fold-nat {} {{",
                            self.atom(*scrutinee)
                        )));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Blocked("zero =>".into(), *zero));
                        sequence.push(Job::Blocked(
                            format!(
                                "step({}, {}) =>",
                                self.value(step.predecessor),
                                self.value(step.hypothesis)
                            ),
                            step.block,
                        ));
                        sequence.push(Job::Dedent);
                        sequence.push(Job::Line("}".into()));
                    }
                    Rhs::FoldSequence {
                        grain,
                        scrutinee,
                        empty,
                        step,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = fold-seq{} {} {{",
                            sequence_grain(grain),
                            self.atom(*scrutinee)
                        )));
                        sequence.push(Job::Indent);
                        sequence.push(Job::Blocked("empty =>".into(), *empty));
                        sequence.push(Job::Blocked(
                            format!(
                                "step({}, {}, {}) =>",
                                self.value(step.element),
                                self.value(step.suffix),
                                self.value(step.accumulator)
                            ),
                            step.block,
                        ));
                        sequence.push(Job::Dedent);
                        sequence.push(Job::Line("}".into()));
                    }
                    Rhs::Cell {
                        operation,
                        operands,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = cell {operation:?}({})",
                            self.atoms(operands)
                        )));
                    }
                    Rhs::Foreign { foreign, operands } => {
                        let row = self.module.foreign(*foreign).expect("live foreign");
                        sequence.push(Job::Line(format!(
                            "{bound} = foreign {}/{}({})",
                            row.namespace,
                            row.name,
                            self.atoms(operands)
                        )));
                    }
                    Rhs::Intrinsic {
                        intrinsic,
                        operands,
                    } => {
                        sequence.push(Job::Line(format!(
                            "{bound} = intrinsic {intrinsic:?}({})",
                            self.atoms(operands)
                        )));
                    }
                }
            }
        }
        push_sequence(stack, sequence);
    }

    fn atom(&self, atom: Atom) -> String {
        match atom {
            Atom::Value(value) => self.value(value),
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

    fn value(&self, id: ValueId) -> String {
        let definition = self.module.value(id).expect("live value");
        format!("{id}{}", hint(&definition.debug_name))
    }

    fn function(&self, id: FunctionId) -> String {
        match self.module.function(id) {
            Some(definition) => format!("{id}{}", hint(&definition.debug_name)),
            None => format!("{id}"),
        }
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

fn fields(row: &[Option<String>]) -> String {
    row.iter()
        .enumerate()
        .map(|(index, field)| match field {
            Some(name) => name.clone(),
            None => format!("{index}"),
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn sequence_grain(grain: &super::SequenceGrain) -> &'static str {
    match grain {
        super::SequenceGrain::List => "[lst]",
        super::SequenceGrain::Bin(Grain::B) => "[bin:b]",
        super::SequenceGrain::Bin(Grain::X) => "[bin:x]",
    }
}

fn render_constant(constant: &Constant) -> String {
    match constant {
        Constant::Unit => "unit".into(),
        Constant::Bool(value) => format!("{value}"),
        Constant::Nat(value) => format!("{value}"),
        Constant::Int(value) => format!("{value}:int"),
        Constant::Flt(value) => {
            let float = value.to_f32();
            if float.is_finite() {
                format!("{float:?}:flt")
            } else {
                format!("flt:0x{:08x}", float.to_bits())
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
