//! Entry ABI selection is checked before Binaryen and exercised through both native compilation paths.

use {
    super::compile_raw_program,
    curios_native::{Compiled, ForeignBindings, MockHost},
    curios_wasm::to_bytes,
};

#[test]
fn residual_entry_capability_selects_the_abi_and_preserves_execution() {
    for (source, turns, status, output, errors) in [
        ("/std/Io/pure(())", false, 0, b"".as_slice(), b"".as_slice()),
        (
            r#"/std/print("hello")"#,
            true,
            0,
            b"hello".as_slice(),
            b"".as_slice(),
        ),
        (
            "/std/Io/wait_filled([])",
            true,
            1,
            curios_abi::DEADLOCK.as_bytes(),
            curios_abi::DEADLOCK.as_bytes(),
        ),
    ] {
        let (module, foreigns) = compile_raw_program(source);
        let text = module.to_string();
        assert_eq!(text.contains("(export \"func/advance\""), turns, "{source}");
        assert_eq!(
            text.contains("(export \"func/main\" (func $io/main))"),
            turns,
            "{source}"
        );
        let raw = to_bytes(&module);
        for bytes in [raw.clone(), curios_binaryen::optimize(raw, false, |_| {})] {
            let code =
                curios_cranelift::compile(&bytes).expect("selected ABI validates and compiles");
            let compiled = Compiled::of(code, foreigns.clone());
            let (host, io) = MockHost::builder().build();
            assert_eq!(
                compiled
                    .run(host, ForeignBindings::empty())
                    .expect("selected ABI executes"),
                status,
                "{source}"
            );
            assert_eq!(io.output(), output, "{source}");
            assert_eq!(io.errors(), errors, "{source}");
        }
    }
}

#[test]
fn an_internally_consumed_suspension_executes_with_the_direct_abi() {
    let mut module = curios_cont::Module::new();
    let row = module.suspension_row();
    let producer = module.reserve_function();
    let producer_ret = module.reserve_continuation();
    let value = module.add_value(None);
    let body = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: producer_ret,
        args: vec![curios_cont::Atom::Value(value)],
    }));
    let body = module.add_node(curios_cont::Node::LetValue {
        result: value,
        value: curios_cont::ValueExpr::Row(
            row,
            vec![curios_cont::Atom::Filler; module.row(row).width()],
        ),
        next: body,
    });
    module.define_function(
        producer,
        curios_cont::Function {
            debug_name: None,
            params: vec![],
            return_cont: producer_ret,
            body,
            droppable: false,
        },
    );
    let entry = module.reserve_function();
    let ret = module.reserve_continuation();
    let ignored = module.add_value(None);
    let resume = module.reserve_continuation();
    let body = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: ret,
        args: vec![curios_cont::Atom::Filler],
    }));
    module.define_continuation(
        resume,
        curios_cont::Continuation {
            debug_name: None,
            params: vec![ignored],
            body,
        },
    );
    let call = module.add_node(curios_cont::Node::ApplyFun {
        callee: curios_cont::Callee::Known(producer),
        args: vec![],
        return_to: resume,
    });
    let body = module.add_node(curios_cont::Node::LetCont {
        continuations: vec![resume],
        body: call,
    });
    let body = module.add_node(curios_cont::Node::LetFun {
        functions: vec![producer],
        body,
    });
    module.define_function(
        entry,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: ret,
            body,
            droppable: false,
        },
    );
    module.set_entry(entry);
    let (wasm, foreigns) = curios_emit::into_wasm(&module);
    assert!(module.suspension().is_some());
    assert!(!wasm.to_string().contains("(export \"func/advance\""));
    let raw = to_bytes(&wasm);
    for bytes in [raw.clone(), curios_binaryen::optimize(raw, false, |_| {})] {
        let code = curios_cranelift::compile(&bytes).expect("internal suspension module validates");
        let compiled = Compiled::of(code, foreigns.clone());
        let (host, io) = MockHost::builder().build();
        assert_eq!(
            compiled
                .run(host, ForeignBindings::empty())
                .expect("internal suspension is consumed"),
            0
        );
        assert!(io.output().is_empty());
        assert!(io.errors().is_empty());
    }
}
