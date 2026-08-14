//! The wire-ABI `Bytes` bridge: a tiny GC module giving JavaScript accessors over the compiler's `$bytes` heap type — the flat payload every object-language `Bytes` value crosses the host boundary as. JS cannot touch wasm-GC arrays directly, so the harness instantiates this module and reads/builds byte strings through its exports. It declares the compiler's own payload shape (`curios_cont::bytes_sub_type`) — wasm-GC canonicalizes structural types, so the refs it produces and consumes are interchangeable with a compiled program's, no matter that the two modules were instantiated separately.

use {
    curios_cont::bytes_sub_type,
    curios_wasm::{
        BlockType, CompType, Export, Expr, Func, FuncName, FuncType, HeapType, Instr, LabelName,
        LocalName, Module, NumType, RefType, ResultType, SubType, TypeName, ValType, to_bytes,
    },
};

/// One accessor's name, parameters, outputs, and array op.
type Accessor = (
    &'static str,
    Vec<(&'static str, ValType)>,
    Vec<ValType>,
    Instr,
);

/// A `block $done (loop $continue ...)` pair whose body runs `step` for `i` from 0 to `len` (both preexisting locals): the copy loop shared by the two bulk transfers.
fn counted_loop(i: &LocalName, len: &LocalName, step: Vec<Instr>) -> Instr {
    let done = LabelName::from("done");
    let continue_ = LabelName::from("continue");

    let mut body = vec![
        get(i),
        get(len),
        Instr::I32GeU,
        Instr::BrIf {
            label_name: done.clone(),
        },
    ];
    body.extend(step);
    body.extend([
        get(i),
        Instr::I32Const { value: 1 },
        Instr::I32Add,
        set(i),
        Instr::Br {
            label_name: continue_.clone(),
        },
    ]);

    Instr::Block {
        label_name: done,
        block_type: BlockType::Empty,
        instructions: vec![Instr::Loop {
            label_name: continue_,
            block_type: BlockType::Empty,
            instructions: body,
        }],
    }
}

fn get(local: &LocalName) -> Instr {
    Instr::LocalGet {
        local_name: local.clone(),
    }
}

fn set(local: &LocalName) -> Instr {
    Instr::LocalSet {
        local_name: local.clone(),
    }
}

/// The bridge as a `curios_wasm::Module`: the canonical `bytes` type, the four accessor exports (`bytes_len`, `bytes_get`, `bytes_new`, `bytes_set` — each body its parameters' `local.get`s followed by one array op), and the bulk lane — the exported memory plus `bytes_load`/`bytes_store`, which copy a whole byte string between a `bytes` array and the memory at offset 0 so JS pays one boundary call per string instead of one per byte.
pub(crate) fn bridge_module() -> Module {
    let mut module = Module::new("bridge");

    let bytes = TypeName::from("bytes");

    module.add_type(bytes.clone(), bytes_sub_type());

    let bytes_ref = ValType::Ref(RefType {
        is_nullable: false,
        heap_type: HeapType::Concrete(bytes.clone()),
    });

    let i32_val = ValType::Num(NumType::I32);

    let accessors: [Accessor; 4] = [
        (
            "bytes_len",
            vec![("b", bytes_ref.clone())],
            vec![i32_val.clone()],
            Instr::ArrayLen,
        ),
        (
            "bytes_get",
            vec![("b", bytes_ref.clone()), ("i", i32_val.clone())],
            vec![i32_val.clone()],
            Instr::ArrayGetU {
                type_name: bytes.clone(),
            },
        ),
        (
            "bytes_new",
            vec![("n", i32_val.clone())],
            vec![bytes_ref.clone()],
            Instr::ArrayNewDefault {
                type_name: bytes.clone(),
            },
        ),
        (
            "bytes_set",
            vec![
                ("b", bytes_ref.clone()),
                ("i", i32_val.clone()),
                ("v", i32_val.clone()),
            ],
            vec![],
            Instr::ArraySet {
                type_name: bytes.clone(),
            },
        ),
    ];

    for (name, params, outputs, array_op) in accessors {
        let type_name = TypeName::from(name);
        let func_name = FuncName::from(name);

        module.add_type(
            type_name.clone(),
            SubType {
                is_final: true,
                super_types: vec![],
                comp_type: CompType::Func(FuncType {
                    inputs: ResultType::from(params.iter().map(|(_, val_type)| val_type.clone())),
                    outputs: ResultType::from(outputs),
                }),
            },
        );

        module.add_func(
            func_name.clone(),
            Func {
                type_name,
                params: params
                    .iter()
                    .map(|(param, _)| LocalName::from(*param))
                    .collect(),
                locals: vec![],
                expr: Expr::from(
                    params
                        .iter()
                        .map(|(param, _)| Instr::LocalGet {
                            local_name: LocalName::from(*param),
                        })
                        .chain([array_op]),
                ),
            },
        );

        module.add_export(name, Export::Func(func_name));
    }

    module.add_export("memory", Export::Memory);

    let func_type = |inputs: Vec<ValType>, outputs: Vec<ValType>| SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Func(FuncType {
            inputs: ResultType::from(inputs),
            outputs: ResultType::from(outputs),
        }),
    };

    let b = LocalName::from("b");
    let i = LocalName::from("i");
    let len = LocalName::from("len");
    let out = LocalName::from("out");

    let bytes_nullable = ValType::Ref(RefType {
        is_nullable: true,
        heap_type: HeapType::Concrete(bytes.clone()),
    });

    // bytes_load(b): memory[0..len] = b[0..len]; returns len. The caller grows the memory to at least len bytes first.
    let load_name = FuncName::from("bytes_load");
    module.add_type(
        TypeName::from("bytes_load"),
        func_type(vec![bytes_ref.clone()], vec![i32_val.clone()]),
    );
    module.add_func(
        load_name.clone(),
        Func {
            type_name: TypeName::from("bytes_load"),
            params: vec![b.clone()],
            locals: vec![(i.clone(), i32_val.clone()), (len.clone(), i32_val.clone())],
            expr: Expr::from([
                get(&b),
                Instr::ArrayLen,
                set(&len),
                counted_loop(
                    &i,
                    &len,
                    vec![
                        get(&i),
                        get(&b),
                        get(&i),
                        Instr::ArrayGetU {
                            type_name: bytes.clone(),
                        },
                        Instr::I32Store8,
                    ],
                ),
                get(&len),
            ]),
        },
    );
    module.add_export("bytes_load", Export::Func(load_name));

    // bytes_store(len): returns a fresh bytes array filled from memory[0..len]. The caller wrote the bytes into the memory first.
    let store_name = FuncName::from("bytes_store");
    module.add_type(
        TypeName::from("bytes_store"),
        func_type(vec![i32_val.clone()], vec![bytes_ref.clone()]),
    );
    module.add_func(
        store_name.clone(),
        Func {
            type_name: TypeName::from("bytes_store"),
            params: vec![len.clone()],
            locals: vec![(i.clone(), i32_val.clone()), (out.clone(), bytes_nullable)],
            expr: Expr::from([
                get(&len),
                Instr::ArrayNewDefault {
                    type_name: bytes.clone(),
                },
                set(&out),
                counted_loop(
                    &i,
                    &len,
                    vec![
                        get(&out),
                        get(&i),
                        get(&i),
                        Instr::I32Load8U,
                        Instr::ArraySet {
                            type_name: bytes.clone(),
                        },
                    ],
                ),
                get(&out),
                Instr::RefAsNonNull,
            ]),
        },
    );
    module.add_export("bytes_store", Export::Func(store_name));

    module
}

/// The encoded bridge, ready for `WebAssembly.instantiate`.
pub(crate) fn bridge_bytes() -> Vec<u8> {
    to_bytes(&bridge_module())
}
