use crate::*;

#[test]
fn from_str() {
    let module = r#"
        (module $from_str
            (type $id (func (param i32) (result i32)))
            (type $point (struct (field $x i32) (field $y (mut i32))))
            (type $bytes (array (mut i8)))
            (import "env" "ext_add" (func $ext_add (type $id)))
            (func $demo (type $id) (param $x i32) (result i32)
                (local $tmp i32)
                i32.const 41
                local.set $tmp
                local.get $x
                i32.const 1
                i32.add
                i32.const 2
                struct.new $point
                drop
                i32.const 3
                array.new_fixed $bytes 1
                drop
                local.get $tmp)
            (global $answer (mut i32)
                i32.const 41)
            (data $greeting passive "\68\65\6c\6c\6f")
            (export "demo" (func $demo))
            (export "answer" (global $answer)))
    "#
    .parse::<Module>()
    .expect("expected module");

    assert_eq!(module.name(), "from_str");
    assert_eq!(module.types().len(), 3);

    let id_type = module
        .get_type(&TypeName::from("id"))
        .expect("expected id type");

    assert!(id_type.is_final);
    assert!(id_type.super_types.is_empty());

    let func_type = id_type.func_type().expect("expected func type");
    assert_eq!(func_type.inputs(), &[ValType::Num(NumType::I32)]);
    assert_eq!(func_type.outputs(), &[ValType::Num(NumType::I32)]);

    let point_type = module
        .get_type(&TypeName::from("point"))
        .expect("expected point type");

    let struct_type = point_type.struct_type().expect("expected struct type");

    let [(x, x_type), (y, y_type)] = struct_type.fields.as_slice() else {
        panic!("expected two struct fields");
    };

    assert_eq!(x.as_str(), "x");

    assert!(matches!(
        x_type.storage_type,
        StorageType::Val(ValType::Num(NumType::I32))
    ));

    assert!(matches!(x_type.mutability, Mutability::Const));

    assert_eq!(y.as_str(), "y");

    assert!(matches!(
        y_type.storage_type,
        StorageType::Val(ValType::Num(NumType::I32))
    ));

    assert!(matches!(y_type.mutability, Mutability::Var));

    let bytes_type = module
        .get_type(&TypeName::from("bytes"))
        .expect("expected bytes type");

    let CompType::Array(array_type) = &bytes_type.comp_type else {
        panic!("expected array type");
    };

    assert!(matches!(
        array_type.field_type.storage_type,
        StorageType::Packed(PackedType::I8)
    ));

    assert!(matches!(array_type.field_type.mutability, Mutability::Var));

    let [(module_name, import_name, import)] = module.imports() else {
        panic!("expected one import");
    };

    assert_eq!(module_name, "env");
    assert_eq!(import_name, "ext_add");

    assert!(matches!(
        import,
        Import::Func {
            func_name,
            type_name
        } if func_name.as_str() == "ext_add" && type_name.as_str() == "id"
    ));

    let [(func_name, func)] = module.funcs() else {
        panic!("expected one func");
    };

    assert_eq!(func_name.as_str(), "demo");
    assert_eq!(func.type_name.as_str(), "id");
    assert_eq!(func.params.len(), 1);
    assert_eq!(func.params[0].as_str(), "x");

    let [(local_name, local_type)] = func.locals.as_slice() else {
        panic!("expected one local");
    };

    assert_eq!(local_name.as_str(), "tmp");
    assert_eq!(local_type, &ValType::Num(NumType::I32));
    assert_eq!(func.expr.instrs.len(), 12);
    assert!(matches!(func.expr.instrs[0], Instr::I32Const { value: 41 }));

    assert!(matches!(
        &func.expr.instrs[1],
        Instr::LocalSet { local_name } if local_name.as_str() == "tmp"
    ));

    assert!(matches!(
        &func.expr.instrs[2],
        Instr::LocalGet { local_name } if local_name.as_str() == "x"
    ));

    assert!(matches!(func.expr.instrs[3], Instr::I32Const { value: 1 }));
    assert!(matches!(func.expr.instrs[4], Instr::I32Add));
    assert!(matches!(func.expr.instrs[5], Instr::I32Const { value: 2 }));

    assert!(matches!(
        &func.expr.instrs[6],
        Instr::StructNew { type_name } if type_name.as_str() == "point"
    ));

    assert!(matches!(func.expr.instrs[7], Instr::Drop));
    assert!(matches!(func.expr.instrs[8], Instr::I32Const { value: 3 }));

    assert!(matches!(
        &func.expr.instrs[9],
        Instr::ArrayNewFixed { type_name, length }
            if type_name.as_str() == "bytes" && *length == 1
    ));

    assert!(matches!(func.expr.instrs[10], Instr::Drop));

    assert!(matches!(
        &func.expr.instrs[11],
        Instr::LocalGet { local_name } if local_name.as_str() == "tmp"
    ));

    let [(global_name, global)] = module.globals() else {
        panic!("expected one global");
    };

    assert_eq!(global_name.as_str(), "answer");

    assert!(matches!(
        global.global_type.val_type,
        ValType::Num(NumType::I32)
    ));

    assert!(matches!(global.global_type.mutability, Mutability::Var));
    assert_eq!(global.expr.instrs.len(), 1);

    assert!(matches!(
        global.expr.instrs[0],
        Instr::I32Const { value: 41 }
    ));

    assert!(
        module.exports().iter().any(|(name, export)| matches!(
            (name.as_str(), export),
            ("demo", Export::Func(func_name)) if func_name.as_str() == "demo"
        )),
        "expected func export"
    );

    assert!(
        module.exports().iter().any(|(name, export)| matches!(
            (name.as_str(), export),
            ("answer", Export::Global(global_name)) if global_name.as_str() == "answer"
        )),
        "expected global export"
    );

    let [(data_name, data_segment)] = module.datas() else {
        panic!("expected one data segment");
    };

    assert_eq!(data_name.as_str(), "greeting");
    assert_eq!(data_segment.bytes, b"hello");
}
