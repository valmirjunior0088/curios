use super::*;

fn i32() -> curios_wasm::ValType {
    curios_wasm::ValType::Num(curios_wasm::NumType::I32)
}

fn i64() -> curios_wasm::ValType {
    curios_wasm::ValType::Num(curios_wasm::NumType::I64)
}

fn get(name: &str) -> curios_wasm::Instr {
    curios_wasm::Instr::LocalGet {
        local_name: curios_wasm::LocalName::from(name),
    }
}

fn set(name: &str) -> curios_wasm::Instr {
    curios_wasm::Instr::LocalSet {
        local_name: curios_wasm::LocalName::from(name),
    }
}

fn constant(value: i32) -> curios_wasm::Instr {
    curios_wasm::Instr::I32Const { value }
}

fn looped(instructions: Vec<curios_wasm::Instr>) -> curios_wasm::Instr {
    curios_wasm::Instr::Loop {
        label_name: curios_wasm::LabelName::from("loop"),
        block_type: curios_wasm::BlockType::Empty,
        instructions,
    }
}

/// A function of `params` over `locals`, each named and typed, running `instrs`.
fn func(
    params: &[&str],
    locals: &[(&str, curios_wasm::ValType)],
    instrs: Vec<curios_wasm::Instr>,
) -> curios_wasm::Func {
    curios_wasm::Func {
        type_name: curios_wasm::TypeName::from("type"),
        params: params
            .iter()
            .map(|name| curios_wasm::LocalName::from(*name))
            .collect(),
        locals: locals
            .iter()
            .map(|(name, val_type)| (curios_wasm::LocalName::from(*name), val_type.clone()))
            .collect(),
        expr: curios_wasm::Expr::from(instrs),
    }
}

/// The local each `local.get`, `local.set` and `local.tee` names, in order, through every block.
fn named(instrs: &[curios_wasm::Instr]) -> Vec<String> {
    let mut names = Vec::new();
    for instr in instrs {
        match instr {
            curios_wasm::Instr::Block { instructions, .. }
            | curios_wasm::Instr::Loop { instructions, .. } => names.extend(named(instructions)),
            curios_wasm::Instr::If {
                then_instructions,
                else_instructions,
                ..
            } => {
                names.extend(named(then_instructions));
                names.extend(named(else_instructions));
            }
            curios_wasm::Instr::LocalGet { local_name }
            | curios_wasm::Instr::LocalSet { local_name }
            | curios_wasm::Instr::LocalTee { local_name } => names.push(local_name.as_string()),
            _ => {}
        }
    }
    names
}

/// The declared locals' names.
fn declared(func: &curios_wasm::Func) -> Vec<String> {
    func.locals
        .iter()
        .map(|(name, _)| name.as_string())
        .collect()
}

#[test]
fn two_values_of_one_type_that_never_live_together_share_a_local() {
    let mut f = func(
        &[],
        &[("a", i32()), ("b", i32())],
        vec![
            constant(1),
            set("a"),
            get("a"),
            set("b"),
            get("b"),
            curios_wasm::Instr::Drop,
        ],
    );
    allocate(&mut f);

    assert_eq!(declared(&f), ["a"]);
    assert_eq!(named(&f.expr.instrs), ["a", "a", "a", "a"]);
}

#[test]
fn two_values_live_together_keep_two_locals() {
    let mut f = func(
        &[],
        &[("a", i32()), ("b", i32())],
        vec![
            constant(1),
            set("a"),
            constant(2),
            set("b"),
            get("a"),
            get("b"),
            curios_wasm::Instr::I32Add,
            curios_wasm::Instr::Drop,
        ],
    );
    allocate(&mut f);

    assert_eq!(declared(&f), ["a", "b"]);
}

#[test]
fn values_of_two_types_never_share_a_local() {
    let mut f = func(
        &[],
        &[("a", i32()), ("b", i64())],
        vec![
            constant(1),
            set("a"),
            get("a"),
            curios_wasm::Instr::Drop,
            curios_wasm::Instr::I64Const { value: 2 },
            set("b"),
            get("b"),
            curios_wasm::Instr::Drop,
        ],
    );
    allocate(&mut f);

    assert_eq!(declared(&f), ["a", "b"]);
}

/// A value written before a loop and read inside it is read again on the next round: a value written and read inside the loop after that read does not take its local.
#[test]
fn a_value_read_inside_a_loop_it_was_written_before_lives_through_the_loop() {
    let mut f = func(
        &[],
        &[("outer", i32()), ("inner", i32())],
        vec![
            constant(1),
            set("outer"),
            looped(vec![
                get("outer"),
                curios_wasm::Instr::Drop,
                constant(2),
                set("inner"),
                get("inner"),
                curios_wasm::Instr::BrIf {
                    label_name: curios_wasm::LabelName::from("loop"),
                },
            ]),
        ],
    );
    allocate(&mut f);

    assert_eq!(declared(&f), ["outer", "inner"]);
}

/// Live through the last of two loops it is read in, not the first.
#[test]
fn a_value_read_in_two_loops_lives_through_the_later() {
    let mut f = func(
        &[],
        &[("outer", i32()), ("inner", i32())],
        vec![
            constant(1),
            set("outer"),
            looped(vec![get("outer"), curios_wasm::Instr::Drop]),
            looped(vec![
                get("outer"),
                curios_wasm::Instr::Drop,
                constant(2),
                set("inner"),
                get("inner"),
                curios_wasm::Instr::Drop,
            ]),
        ],
    );
    allocate(&mut f);

    assert_eq!(declared(&f), ["outer", "inner"]);
}

/// A local read before any write is read at its default: nothing else is written to it, before the read or after, though a write of its own follows the read.
#[test]
fn a_local_read_before_it_is_written_keeps_its_default() {
    let mut f = func(
        &[],
        &[("early", i32()), ("defaulted", i32())],
        vec![
            constant(1),
            set("early"),
            get("early"),
            curios_wasm::Instr::Drop,
            get("defaulted"),
            curios_wasm::Instr::Drop,
            constant(2),
            set("defaulted"),
            get("defaulted"),
            curios_wasm::Instr::Drop,
        ],
    );
    allocate(&mut f);

    assert_eq!(declared(&f), ["defaulted", "early"]);
    assert_eq!(
        named(&f.expr.instrs),
        ["early", "early", "defaulted", "defaulted", "defaulted"]
    );
}

/// A write in one arm of an `if` initializes nothing in the other.
#[test]
fn a_write_in_one_arm_shows_nothing_in_the_other() {
    let mut f = func(
        &[],
        &[("then", i32()), ("elsewhere", i32())],
        vec![
            constant(1),
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("if"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![constant(2), set("then"), constant(3), set("elsewhere")],
                else_instructions: vec![get("elsewhere"), curios_wasm::Instr::Drop],
            },
        ],
    );
    allocate(&mut f);

    // `elsewhere` is read in the arm no write of it stands in, so it keeps its own local from the start; `then` takes another.
    assert_eq!(declared(&f), ["elsewhere", "then"]);
}

#[test]
fn a_parameter_keeps_its_place() {
    let mut f = func(
        &["p"],
        &[("a", i32())],
        vec![get("p"), set("a"), get("a"), set("p")],
    );
    allocate(&mut f);

    assert_eq!(named(&f.expr.instrs), ["p", "a", "a", "p"]);
    assert_eq!(declared(&f), ["a"]);
}

#[test]
fn a_local_never_named_is_dropped() {
    let mut f = func(
        &[],
        &[("unused", i32())],
        vec![constant(1), curios_wasm::Instr::Drop],
    );
    allocate(&mut f);

    assert!(declared(&f).is_empty());
}
