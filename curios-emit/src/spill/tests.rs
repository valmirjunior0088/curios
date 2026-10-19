use super::*;

/// An envelope small enough to cross with a handful of fields, parameters and results.
const SMALL: Envelope = Envelope {
    fields: 3,
    params: 2,
    results: 2,
};

fn i32() -> curios_wasm::ValType {
    curios_wasm::ValType::Num(curios_wasm::NumType::I32)
}

fn name(text: &str) -> curios_wasm::TypeName {
    curios_wasm::TypeName::from(text)
}

/// A struct type `type_name` of `width` immutable `i32` fields, named by position.
fn wide_struct(type_name: &str, width: usize) -> curios_wasm::RecType {
    curios_wasm::RecType::from([(
        name(type_name),
        final_struct((0..width).map(|index| (field(index), i32()))),
    )])
}

/// A function type of `params` `i32` parameters and `results` `i32` results.
fn signature(type_name: &str, params: usize, results: usize) -> curios_wasm::RecType {
    curios_wasm::RecType::from([(
        name(type_name),
        curios_wasm::SubType {
            is_final: true,
            super_types: vec![],
            comp_type: curios_wasm::CompType::Func(curios_wasm::FuncType {
                inputs: curios_wasm::ResultType::from((0..params).map(|_| i32())),
                outputs: curios_wasm::ResultType::from((0..results).map(|_| i32())),
            }),
        },
    )])
}

/// A function `func_name` of `type_name`, its parameters `p0`, `p1`, …, running `instrs`.
fn function(
    module: &mut curios_wasm::Module,
    func_name: &str,
    type_name: &str,
    params: usize,
    instrs: Vec<curios_wasm::Instr>,
) {
    module.add_func(
        curios_wasm::FuncName::from(func_name),
        curios_wasm::Func {
            type_name: name(type_name),
            params: (0..params)
                .map(|index| curios_wasm::LocalName::from(format!("p{index}")))
                .collect(),
            locals: vec![],
            expr: curios_wasm::Expr::from(instrs),
        },
    );
}

fn constant(value: i32) -> curios_wasm::Instr {
    curios_wasm::Instr::I32Const { value }
}

fn get(type_name: &str, index: usize) -> curios_wasm::Instr {
    curios_wasm::Instr::StructGet {
        type_name: name(type_name),
        field_name: field(index),
    }
}

/// Each instruction of the function `func_name`, as a word and its operands.
fn described(module: &mut curios_wasm::Module, func_name: &str) -> Vec<String> {
    let (_, func) = module
        .funcs_mut()
        .find(|(name, _)| name.as_str() == func_name)
        .expect("the function is defined");
    func.expr
        .instrs
        .iter()
        .map(|instr| match instr {
            curios_wasm::Instr::StructNew { type_name } => format!("new {type_name}"),
            curios_wasm::Instr::StructGet {
                type_name,
                field_name,
            } => format!("get {type_name} {field_name}"),
            curios_wasm::Instr::Call { func_name } => format!("call {func_name}"),
            curios_wasm::Instr::CallIndirect { type_name, .. } => {
                format!("call_indirect {type_name}")
            }
            curios_wasm::Instr::Return => "return".to_string(),
            curios_wasm::Instr::LocalGet { local_name } => format!("local.get {local_name}"),
            curios_wasm::Instr::LocalSet { local_name } => format!("local.set {local_name}"),
            curios_wasm::Instr::I32Const { value } => format!("{value}"),
            curios_wasm::Instr::Drop => "drop".to_string(),
            other => format!("{other:?}"),
        })
        .collect()
}

/// The declared fields of `type_name`, by name.
fn fields(module: &curios_wasm::Module, type_name: &str) -> Vec<String> {
    let curios_wasm::CompType::Struct(struct_type) = &module
        .get_type(&name(type_name))
        .expect("the type is declared")
        .comp_type
    else {
        panic!("`{type_name}` is a struct");
    };
    struct_type
        .fields
        .iter()
        .map(|(field_name, _)| field_name.as_string())
        .collect()
}

/// A struct past the limit keeps one field fewer than it and nests the rest, built tail first from the top of the stack and read through the `spill` field.
#[test]
fn a_wide_struct_keeps_its_first_fields_and_nests_the_rest() {
    let mut module = curios_wasm::Module::new("module");
    module.add_types(wide_struct("t", 5));
    module.add_types(signature("f", 0, 0));
    function(
        &mut module,
        "main",
        "f",
        0,
        (0..5)
            .map(constant)
            .chain([
                curios_wasm::Instr::StructNew {
                    type_name: name("t"),
                },
                get("t", 4),
                curios_wasm::Instr::Drop,
            ])
            .collect(),
    );
    spill_wide_shapes(&mut module, SMALL);

    assert_eq!(fields(&module, "t"), ["0", "1", "spill"]);
    assert_eq!(fields(&module, "t/spill/1"), ["2", "3", "4"]);
    assert_eq!(
        described(&mut module, "main"),
        [
            "0",
            "1",
            "2",
            "3",
            "4",
            "new t/spill/1",
            "new t",
            "get t spill",
            "get t/spill/1 4",
            "drop"
        ]
    );
}

/// A struct past the limit twice over nests a tail in its tail, and a field kept in the first struct is read as it was.
#[test]
fn a_struct_past_the_limit_twice_over_nests_a_tail_in_its_tail() {
    let mut module = curios_wasm::Module::new("module");
    module.add_types(wide_struct("t", 8));
    module.add_types(signature("f", 0, 0));
    function(
        &mut module,
        "main",
        "f",
        0,
        vec![
            constant(0),
            get("t", 7),
            curios_wasm::Instr::Drop,
            constant(0),
            get("t", 1),
            curios_wasm::Instr::Drop,
        ],
    );
    spill_wide_shapes(&mut module, SMALL);

    assert_eq!(fields(&module, "t/spill/1"), ["2", "3", "spill"]);
    assert_eq!(fields(&module, "t/spill/2"), ["4", "5", "spill"]);
    assert_eq!(fields(&module, "t/spill/3"), ["6", "7"]);
    assert_eq!(
        described(&mut module, "main"),
        [
            "0",
            "get t spill",
            "get t/spill/1 spill",
            "get t/spill/2 spill",
            "get t/spill/3 7",
            "drop",
            "0",
            "get t 1",
            "drop"
        ]
    );
}

/// Parameters past the limit are packed by the caller from the top of the stack and spread into the callee's locals at its entry.
#[test]
fn parameters_past_the_limit_pass_as_one_struct() {
    let mut module = curios_wasm::Module::new("module");
    module.add_types(signature("wide", 4, 0));
    module.add_types(signature("f", 0, 0));
    function(
        &mut module,
        "callee",
        "wide",
        4,
        vec![
            curios_wasm::Instr::LocalGet {
                local_name: curios_wasm::LocalName::from("p3"),
            },
            curios_wasm::Instr::Drop,
        ],
    );
    function(
        &mut module,
        "main",
        "f",
        0,
        (0..4)
            .map(constant)
            .chain([curios_wasm::Instr::Call {
                func_name: curios_wasm::FuncName::from("callee"),
            }])
            .collect(),
    );
    spill_wide_shapes(&mut module, SMALL);

    assert_eq!(
        described(&mut module, "main"),
        ["0", "1", "2", "3", "new wide/params", "call callee"]
    );
    assert_eq!(
        described(&mut module, "callee"),
        [
            "local.get spill/params",
            "get wide/params 0",
            "local.set p1",
            "local.get spill/params",
            "get wide/params 1",
            "local.set p2",
            "local.get spill/params",
            "get wide/params 2",
            "local.set p3",
            "local.get p3",
            "drop"
        ]
    );
}

/// Results past the limit are packed at each return and at the function's end, and spread back onto the caller's stack after the call.
#[test]
fn results_past_the_limit_return_as_one_struct() {
    let mut module = curios_wasm::Module::new("module");
    module.add_types(signature("wide", 0, 3));
    module.add_types(signature("f", 0, 0));
    function(
        &mut module,
        "callee",
        "wide",
        0,
        vec![
            constant(0),
            constant(1),
            constant(2),
            curios_wasm::Instr::Return,
        ],
    );
    function(
        &mut module,
        "main",
        "f",
        0,
        vec![
            curios_wasm::Instr::Call {
                func_name: curios_wasm::FuncName::from("callee"),
            },
            curios_wasm::Instr::Drop,
            curios_wasm::Instr::Drop,
            curios_wasm::Instr::Drop,
        ],
    );
    spill_wide_shapes(&mut module, SMALL);

    assert_eq!(
        described(&mut module, "callee"),
        [
            "0",
            "1",
            "2",
            "new wide/results",
            "return",
            "new wide/results"
        ]
    );
    assert_eq!(
        described(&mut module, "main"),
        [
            "call callee",
            "local.set spill/0",
            "local.get spill/0",
            "get wide/results 0",
            "local.get spill/0",
            "get wide/results 1",
            "drop",
            "drop",
            "drop"
        ]
    );
}

/// An indirect call's table index is on top of the arguments, so it goes aside while they are packed.
#[test]
fn an_indirect_call_sets_its_index_aside_while_its_arguments_are_packed() {
    let mut module = curios_wasm::Module::new("module");
    module.add_types(signature("wide", 3, 0));
    module.add_types(signature("f", 0, 0));
    function(
        &mut module,
        "main",
        "f",
        0,
        vec![
            constant(0),
            constant(1),
            constant(2),
            constant(7),
            curios_wasm::Instr::CallIndirect {
                table_name: curios_wasm::TableName::from("table"),
                type_name: name("wide"),
            },
        ],
    );
    spill_wide_shapes(&mut module, SMALL);

    assert_eq!(
        described(&mut module, "main"),
        [
            "0",
            "1",
            "2",
            "7",
            "local.set spill/0",
            "new wide/params",
            "local.get spill/0",
            "call_indirect wide"
        ]
    );
}

/// The tails of a signature are declared before it, in recursion groups of their own.
#[test]
fn a_signature_s_tails_are_declared_before_it() {
    let mut module = curios_wasm::Module::new("module");
    module.add_types(signature("wide", 3, 3));
    spill_wide_shapes(&mut module, SMALL);

    let declared = module
        .types_mut()
        .iter()
        .map(|group| {
            group
                .sub_types
                .iter()
                .map(|(type_name, _)| type_name.as_string())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        declared,
        [vec!["wide/params"], vec!["wide/results"], vec!["wide"]]
    );
}
