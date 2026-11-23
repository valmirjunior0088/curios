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

/// A NaN prints as the text format spells one, by its sign and its payload — `nan` alone for the canonical payload, `nan:0x…` for any other — so a tool reading WebAssembly text reads the bits the module holds, and the text reads back to them; an infinity and a finite value print as Rust spells them, which the format reads alike.
#[test]
fn a_nan_prints_its_sign_and_payload_and_reads_back_to_its_bits() {
    let singles = [
        f32::NAN,
        -f32::NAN,
        f32::from_bits(0x7f80_0001),
        f32::from_bits(0xffc0_0001),
        f32::INFINITY,
        -1.5,
    ];
    let doubles = [
        f64::NAN,
        -f64::NAN,
        f64::from_bits(0x7ff0_0000_0000_0001),
        f64::NEG_INFINITY,
        0.1,
    ];
    let mut module = named(&["func"]);
    module.add_func(
        FuncName::from("floats"),
        Func {
            type_name: TypeName::from("type"),
            params: vec![],
            locals: vec![],
            expr: Expr::from(
                singles
                    .map(|value| Instr::F32Const { value })
                    .into_iter()
                    .chain(doubles.map(|value| Instr::F64Const { value })),
            ),
        },
    );
    let text = module.to_string();
    let spelled = |head: &str| {
        text.split(head)
            .skip(1)
            .map(|rest| {
                rest.split(|char: char| char.is_whitespace() || char == ')')
                    .next()
                    .unwrap_or_default()
                    .to_string()
            })
            .collect::<Vec<_>>()
    };

    assert_eq!(
        spelled("f32.const "),
        ["nan", "-nan", "nan:0x1", "-nan:0x400001", "inf", "-1.5"],
        "{text}"
    );
    assert_eq!(
        spelled("f64.const "),
        ["nan", "-nan", "nan:0x1", "-inf", "0.1"],
        "{text}"
    );
    let read = text
        .parse::<Module>()
        .expect("the printed module reads back");
    let (_, floats) = read
        .funcs()
        .iter()
        .find(|(name, _)| name.as_str() == "floats")
        .expect("the function reads back");
    let bits = floats
        .expr
        .instrs
        .iter()
        .filter_map(|instr| match instr {
            Instr::F32Const { value } => Some(u64::from(value.to_bits())),
            Instr::F64Const { value } => Some(value.to_bits()),
            _ => None,
        })
        .collect::<Vec<_>>();
    let written = singles
        .map(|value| u64::from(value.to_bits()))
        .into_iter()
        .chain(doubles.map(f64::to_bits))
        .collect::<Vec<_>>();
    assert_eq!(bits, written, "{text}");
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
