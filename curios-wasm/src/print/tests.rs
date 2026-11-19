use crate::*;

/// Code nests as deep as the program it was emitted from, and its text is built a level at a time: ten thousand nested blocks print on the default test thread.
#[test]
fn a_function_nested_ten_thousand_blocks_deep_prints() {
    let mut module = Module::new("module");
    let type_name = TypeName::from("type");
    module.add_type(
        type_name.clone(),
        SubType {
            is_final: true,
            super_types: vec![],
            comp_type: CompType::Func(FuncType {
                inputs: ResultType::from([]),
                outputs: ResultType::from([]),
            }),
        },
    );
    let mut instrs = vec![Instr::Nop];
    for _ in 0..10_000 {
        instrs = vec![Instr::Block {
            label_name: LabelName::from("block"),
            block_type: BlockType::Empty,
            instructions: instrs,
        }];
    }
    module.add_func(
        FuncName::from("func"),
        Func {
            type_name,
            params: vec![],
            locals: vec![],
            expr: Expr::from(instrs),
        },
    );

    assert!(module.to_string().contains("nop"));
}

/// A module of one empty function type, a function of it under each name in `names`, and a passive data segment.
fn named(names: &[&str]) -> Module {
    let mut module = Module::new("module");
    let type_name = TypeName::from("type");
    module.add_type(
        type_name.clone(),
        SubType {
            is_final: true,
            super_types: vec![],
            comp_type: CompType::Func(FuncType {
                inputs: ResultType::from([]),
                outputs: ResultType::from([]),
            }),
        },
    );
    for name in names {
        module.add_func(
            FuncName::from(*name),
            Func {
                type_name: type_name.clone(),
                params: vec![],
                locals: vec![],
                expr: Expr::from(vec![Instr::Nop]),
            },
        );
    }
    module.add_data(
        DataName::from("data"),
        DataSegment {
            mode: DataMode::Passive,
            bytes: vec![0],
        },
    );

    module
}

/// A hint may hold what an identifier cannot — a brace, a parenthesis, a space, a quote — so such a name prints quoted, `$"…"`, its quote and backslash escaped as their bytes, and reads back as the name it was; one an identifier can hold stays bare.
#[test]
fn a_name_an_identifier_cannot_hold_prints_quoted_and_reads_back() {
    let names = [
        "row/1$/sys/Option{Type}",
        "func/2$/std/Monad/Lift(Io, /std/Try/Try)/1",
        "say \"hi\" \\ there",
        "func/3$/count",
    ];
    let text = named(&names).to_string();

    assert!(
        text.contains(r#"(func $"row/1$/sys/Option{Type}""#),
        "{text}"
    );
    assert!(
        text.contains(r#"(func $"say \22hi\22 \5c there""#),
        "{text}"
    );
    assert!(text.contains("(func $func/3$/count "), "{text}");
    let read = text
        .parse::<Module>()
        .expect("the printed module reads back");
    assert!(
        read.funcs().iter().map(|(name, _)| name.as_str()).eq(names),
        "{text}"
    );
}

/// The text is the format's own, so a tool reading WebAssembly text reads it: an indirect call names its signature as a type use, `(type $t)`, and a passive segment names no mode.
#[test]
fn an_indirect_call_names_a_type_use_and_a_passive_segment_no_mode() {
    let mut module = named(&["func"]);
    module.add_func(
        FuncName::from("caller"),
        Func {
            type_name: TypeName::from("type"),
            params: vec![],
            locals: vec![],
            expr: Expr::from(vec![
                Instr::I32Const { value: 0 },
                Instr::CallIndirect {
                    table_name: TableName::from("table"),
                    type_name: TypeName::from("type"),
                },
            ]),
        },
    );
    let text = module.to_string();

    assert!(text.contains("call_indirect $table (type $type)"), "{text}");
    assert!(text.contains(r#"(data $data "\00")"#), "{text}");
}
