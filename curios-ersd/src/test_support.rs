//! Scaffolding for tests that pin what this crate built: a structural digest of a module, and assertions over the door's sequence-usage census.
//!
//! A namespace rather than a root export, for `curios-runtime`'s `test_support` reason: nothing here collides with anything, but `curios_ersd::test_support::shape` says at its use site that the caller reached for scaffolding rather than product API. The census's admission is spelling-sensitive by necessity — a spread of a field's value is structurally indistinguishable from the builder-accumulator cliff — so a respelling can silently unmark a field, and the cost surfaces only in a profile. This surface is the loud guard: a test states the verdict it relies on, by name, and a breaking respelling fails it instead of quietly regressing.

use {
    super::{
        Atom, BlockId, Constant, FunctionId, Module, RecGroupId, Rhs, Statement, StatementId,
        Terminator,
        into_cont::sequence_census,
        print::{spell_function, spell_value},
    },
    curios_utilities::ArenaId,
};

/// Whether the census marks `constructor`'s field indexed-only — the verdict under which the door settles every store into it. Names are the schema's debug names — the family's is its qualified spelling (`/std/Map/Node`, `/Box` for an entrypoint's own) — and an unknown name panics with the known ones listed, since a silent `false` here would defeat the loudness this surface exists for.
pub fn census_settles_constructor_field(
    module: &Module,
    family: &str,
    constructor: &str,
    field: &str,
) -> bool {
    let family_id = module
        .families()
        .iter()
        .position(|candidate| candidate.debug_name.as_deref() == Some(family))
        .unwrap_or_else(|| {
            panic!(
                "no family named `{family}`; known: {:?}",
                module
                    .families()
                    .iter()
                    .filter_map(|f| f.debug_name.as_deref())
                    .collect::<Vec<_>>(),
            )
        });

    let (constructor_id, row) = module
        .constructors()
        .iter()
        .enumerate()
        .find(|(_, candidate)| {
            candidate.family.index() == family_id
                && candidate.debug_name.as_deref() == Some(constructor)
        })
        .unwrap_or_else(|| {
            panic!(
                "family `{family}` has no constructor named `{constructor}`; known: {:?}",
                module
                    .constructors()
                    .iter()
                    .filter(|c| c.family.index() == family_id)
                    .filter_map(|c| c.debug_name.as_deref())
                    .collect::<Vec<_>>(),
            )
        });

    let position = row
        .fields
        .iter()
        .position(|candidate| candidate.debug_name.as_deref() == Some(field))
        .unwrap_or_else(|| {
            panic!(
                "constructor `{constructor}` has no field named `{field}`; known: {:?}",
                row.fields
                    .iter()
                    .filter_map(|f| f.debug_name.as_deref())
                    .collect::<Vec<_>>(),
            )
        });

    sequence_census(module).indexed_only_constructor(ArenaId::from_index(constructor_id), position)
}

/// A structural digest of `module`: one line per node, bodies nested at the statement that introduces them, every node named by the Rust variant holding it.
///
/// The oracle for a test whose subject is what a stage *built*. [`Module`]'s `Display` answers a different question — how a reader of `wonder stage ersd` sees the program — so a test in another crate that pins it turns a presentation change into a behavioral failure there: one layout rework broke twenty-one tests across `curios-elab` and `curios-pipeline` without a single erasure changing. This spelling reaches no user and cannot drift from the enums it names, so it moves when the representation moves, which is exactly when an erasure test should be re-read.
///
/// Identities keep the `~v0$hint` scheme the printer spells them with, because that names an arena slot rather than a layout. The header is not rendered: a schema is referred to by identity here, and a test whose subject is the schema itself asks [`Module::products`] or [`Module::families`], where the answer is the arena rather than a rendering of it.
///
/// Nesting recurses. That is sound for the programs a test writes and this is not a lowering path; the default-stack guarantee belongs to `Display` and to the stages.
pub fn shape(module: &Module) -> String {
    let mut out = String::new();

    if !module.items().is_empty() {
        out.push_str("items\n");
        for &item in module.items() {
            statement(module, &mut out, 1, item);
        }
    }

    match module.entry() {
        Some(entry) => {
            out.push_str("entry\n");
            block(module, &mut out, 1, entry);
        }
        None => out.push_str("no entry\n"),
    }

    out
}

fn line(out: &mut String, depth: usize, text: &str) {
    for _ in 0..depth {
        out.push_str("  ");
    }
    out.push_str(text);
    out.push('\n');
}

fn statement(module: &Module, out: &mut String, depth: usize, id: StatementId) {
    let Some(statement) = module.statement(id) else {
        line(out, depth, &format!("{id} tombstoned"));
        return;
    };

    match statement {
        Statement::Let { result, rhs } => {
            line(
                out,
                depth,
                &format!(
                    "Let {} = {}",
                    spell_value(module, *result),
                    head(module, rhs)
                ),
            );
            bodies(module, out, depth + 1, rhs);
        }
        Statement::Functions { functions } => {
            line(out, depth, "Functions");
            for &id in functions {
                function(module, out, depth + 1, id);
            }
        }
        Statement::Rec { group } => rec_group(module, out, depth, *group),
    }
}

fn rec_group(module: &Module, out: &mut String, depth: usize, id: RecGroupId) {
    let Some(group) = module.rec_group(id) else {
        line(out, depth, &format!("{id} tombstoned"));
        return;
    };

    line(out, depth, "Rec");
    for &id in &group.functions {
        function(module, out, depth + 1, id);
    }
    for member in &group.values {
        line(
            out,
            depth + 1,
            &format!("value {}", spell_value(module, member.value)),
        );
        block(module, out, depth + 2, member.init);
    }
}

fn function(module: &Module, out: &mut String, depth: usize, id: FunctionId) {
    let Some(function) = module.function(id) else {
        line(out, depth, &format!("{id} tombstoned"));
        return;
    };

    let params = function
        .params
        .iter()
        .map(|&param| spell_value(module, param))
        .collect::<Vec<_>>()
        .join(", ");
    line(
        out,
        depth,
        &format!("function {}({params})", spell_function(module, id)),
    );
    block(module, out, depth + 1, function.body);
}

fn block(module: &Module, out: &mut String, depth: usize, id: BlockId) {
    let Some(block_) = module.block(id) else {
        line(out, depth, &format!("{id} tombstoned"));
        return;
    };

    for &id in &block_.statements {
        statement(module, out, depth, id);
    }

    let terminator = match &block_.terminator {
        Terminator::Return(atom) => format!("Return {}", atom_(module, atom)),
        Terminator::Exit(atom) => format!("Exit {}", atom_(module, atom)),
        Terminator::Unreachable => "Unreachable".to_string(),
    };
    line(out, depth, &terminator);
}

/// The one-line head of a right-hand side: its variant, the operands it holds directly, and the schema or operation identity that selects its meaning. Blocks it owns are [`bodies`]'s job.
fn head(module: &Module, rhs: &Rhs) -> String {
    match rhs {
        Rhs::Alias(atom) => format!("Alias {}", atom_(module, atom)),
        Rhs::Apply { callee, arguments } => format!(
            "Apply {} {}",
            atom_(module, callee),
            atoms(module, arguments)
        ),
        Rhs::Operation {
            operation,
            operands,
        } => format!("Operation {operation:?} {}", atoms(module, operands)),
        Rhs::Sequence {
            operation,
            operands,
        } => format!("Sequence {operation:?} {}", atoms(module, operands)),
        Rhs::Product { schema, fields } => format!("Product {schema} {}", atoms(module, fields)),
        Rhs::Construct {
            constructor,
            fields,
        } => format!("Construct {constructor} {}", atoms(module, fields)),
        Rhs::Project {
            schema,
            product,
            field,
        } => format!("Project {schema}.{field} {}", atom_(module, product)),
        Rhs::MatchVariant {
            family, scrutinee, ..
        } => format!("MatchVariant {family} {}", atom_(module, scrutinee)),
        Rhs::SwitchBool { scrutinee, .. } => format!("SwitchBool {}", atom_(module, scrutinee)),
        Rhs::SwitchNat { scrutinee, .. } => format!("SwitchNat {}", atom_(module, scrutinee)),
        Rhs::FoldNat { scrutinee, .. } => format!("FoldNat {}", atom_(module, scrutinee)),
        Rhs::FoldSequence {
            grain, scrutinee, ..
        } => format!("FoldSequence {grain:?} {}", atom_(module, scrutinee)),
        Rhs::UnconsSequence {
            grain, scrutinee, ..
        } => format!("UnconsSequence {grain:?} {}", atom_(module, scrutinee)),
        Rhs::Channel {
            operation,
            operands,
        } => format!("Channel {operation:?} {}", atoms(module, operands)),
        Rhs::Cell {
            operation,
            operands,
        } => format!("Cell {operation:?} {}", atoms(module, operands)),
        Rhs::Foreign { foreign, operands } => {
            format!("Foreign {foreign} {}", atoms(module, operands))
        }
        Rhs::Intrinsic {
            intrinsic,
            operands,
        } => format!("Intrinsic {intrinsic:?} {}", atoms(module, operands)),
    }
}

/// The blocks a right-hand side owns, each under the arm label that selects it and the binders that arm introduces.
fn bodies(module: &Module, out: &mut String, depth: usize, rhs: &Rhs) {
    match rhs {
        Rhs::MatchVariant { arms, default, .. } => {
            for arm in arms {
                let bindings = arm
                    .bindings
                    .iter()
                    .map(|&binding| spell_value(module, binding))
                    .collect::<Vec<_>>()
                    .join(", ");
                line(out, depth, &format!("arm {}({bindings})", arm.constructor));
                block(module, out, depth + 1, arm.block);
            }
            if let Some(default) = default {
                line(out, depth, "default");
                block(module, out, depth + 1, *default);
            }
        }
        Rhs::SwitchBool {
            if_false, if_true, ..
        } => {
            line(out, depth, "false");
            block(module, out, depth + 1, *if_false);
            line(out, depth, "true");
            block(module, out, depth + 1, *if_true);
        }
        Rhs::SwitchNat { cases, default, .. } => {
            for case in cases {
                line(out, depth, &format!("case {}", case.key));
                block(module, out, depth + 1, case.block);
            }
            line(out, depth, "default");
            block(module, out, depth + 1, *default);
        }
        Rhs::FoldNat { zero, step, .. } => {
            line(out, depth, "zero");
            block(module, out, depth + 1, *zero);
            line(
                out,
                depth,
                &format!(
                    "step({}, {})",
                    spell_value(module, step.predecessor),
                    spell_value(module, step.hypothesis)
                ),
            );
            block(module, out, depth + 1, step.block);
        }
        Rhs::FoldSequence { empty, step, .. } => {
            line(out, depth, "empty");
            block(module, out, depth + 1, *empty);
            line(
                out,
                depth,
                &format!(
                    "step({}, {}, {})",
                    spell_value(module, step.element),
                    spell_value(module, step.suffix),
                    spell_value(module, step.accumulator)
                ),
            );
            block(module, out, depth + 1, step.block);
        }
        Rhs::UnconsSequence { empty, cons, .. } => {
            line(out, depth, "empty");
            block(module, out, depth + 1, *empty);
            line(
                out,
                depth,
                &format!(
                    "cons({}, {})",
                    spell_value(module, cons.element),
                    spell_value(module, cons.suffix)
                ),
            );
            block(module, out, depth + 1, cons.block);
        }
        Rhs::Alias(_)
        | Rhs::Apply { .. }
        | Rhs::Operation { .. }
        | Rhs::Sequence { .. }
        | Rhs::Product { .. }
        | Rhs::Construct { .. }
        | Rhs::Project { .. }
        | Rhs::Cell { .. }
        | Rhs::Channel { .. }
        | Rhs::Foreign { .. }
        | Rhs::Intrinsic { .. } => {}
    }
}

fn atoms(module: &Module, atoms: &[Atom]) -> String {
    let rendered = atoms
        .iter()
        .map(|atom| atom_(module, atom))
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{rendered}]")
}

fn atom_(module: &Module, atom: &Atom) -> String {
    match atom {
        Atom::Value(id) => spell_value(module, *id),
        Atom::Function(id) => spell_function(module, *id),
        Atom::Constant(id) => match module.constant(*id) {
            Some(constant) => constant_(constant),
            None => format!("{id}"),
        },
    }
}

/// A constant by its variant and its value. The numeric payloads print through `Display` — a constant's value is not a layout choice, and `Debug` on an unbounded carrier is not readable.
fn constant_(constant: &Constant) -> String {
    match constant {
        Constant::Unit => "Unit".to_string(),
        Constant::Bool(value) => format!("Bool({value})"),
        Constant::Nat(value) => format!("Nat({value})"),
        Constant::Int(value) => format!("Int({value})"),
        Constant::Flt(value) => format!("Flt({value})"),
        Constant::Byte(value) => format!("Byte({value})"),
        Constant::Bin(grain, bits) => {
            format!("Bin({grain:?}, {} units)", bits.len(*grain))
        }
        Constant::Handle(token) => format!("Handle({token})"),
    }
}
