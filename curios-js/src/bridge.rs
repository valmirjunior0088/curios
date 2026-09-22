//! The wire-ABI bridge: a tiny GC module giving JavaScript accessors over the compiler's `$bytes` heap type — the flat payload every object-language `Bytes` value crosses the host boundary as — over the `$elems` array a list of references crosses as, and over the `$longs` and `$words` arrays a list of scalars crosses as, one `i64` per `Nat` or `Int` and one word per `Bool`. JS cannot touch wasm-GC arrays directly, so the harness instantiates this module and reads/builds byte strings and lists through its exports. It declares the compiler's own payload shapes (`curios_emit::bytes_sub_type`, `curios_emit::elems_sub_type`, `curios_emit::longs_sub_type`, `curios_emit::words_sub_type`) — wasm-GC canonicalizes structural types, so the refs it produces and consumes are interchangeable with a compiled program's, no matter that the two modules were instantiated separately.

use {
    curios_emit::{bytes_sub_type, elems_sub_type, longs_sub_type, words_sub_type},
    curios_wasm::{
        AbsHeapType, AddressType, BlockType, CompType, Export, Expr, Func, FuncName, FuncType,
        HeapType, Instr, LabelName, Limits, LocalName, MemArg, MemName, MemType, Module, NumType,
        RefType, ResultType, SubType, TypeName, ValType, to_bytes,
    },
};

/// One accessor's name, parameters, outputs, and the ops that follow its parameters' `local.get`s — one array op each.
type Accessor = (
    &'static str,
    Vec<(&'static str, ValType)>,
    Vec<ValType>,
    Vec<Instr>,
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

/// The type a bridge function is declared against: final, no supertypes, and the given signature. Every function this module adds is declared through this, so the exports cannot end up built two ways.
fn func_type(
    inputs: impl IntoIterator<Item = ValType>,
    outputs: impl IntoIterator<Item = ValType>,
) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Func(FuncType {
            inputs: ResultType::from(inputs),
            outputs: ResultType::from(outputs),
        }),
    }
}

/// The bridge as a `curios_wasm::Module`: the canonical `bytes` type with its four accessor exports (`bytes_len`, `bytes_get`, `bytes_new`, `bytes_set`), the canonical `elems` list type with its four (`list_len`, `list_get`, `list_new`, `list_set`), the canonical `longs` type a list of `Nat` or `Int` crosses as with its four (`longs_len`, `longs_get`, `longs_new`, `longs_set`), the canonical `words` type a list of `Bool` crosses as with its four (`words_len`, `words_get`, `words_new`, `words_set`) — each body its parameters' `local.get`s followed by its ops — and the bulk lane — a memory this module declares and exports, plus `bytes_load`/`bytes_store`, which copy a whole byte string between a `bytes` array and the memory at offset 0 so JS pays one boundary call per string instead of one per byte. The memory is declared here because it is this module's, and nothing in `curios-wasm` supplies one: a compiled program declares none and carries no memory section at all.
pub(crate) fn bridge_module() -> Module {
    let mut module = Module::new("bridge");

    let bytes = TypeName::from("bytes");

    module.add_type(bytes.clone(), bytes_sub_type());

    let bytes_ref = ValType::Ref(RefType {
        is_nullable: false,
        heap_type: HeapType::Concrete(bytes.clone()),
    });

    let i32_val = ValType::Num(NumType::I32);

    // The list-of-references payload: `(mut (ref null any))` elements, matching the codegen's `list_type` and the native adapter's `anyref_array_type`.
    let any_ref = ValType::Ref(RefType {
        is_nullable: true,
        heap_type: HeapType::Abstract(AbsHeapType::Any),
    });
    let elems = TypeName::from("elems");

    module.add_type(elems.clone(), elems_sub_type(any_ref.clone()));

    let elems_ref = ValType::Ref(RefType {
        is_nullable: false,
        heap_type: HeapType::Concrete(elems.clone()),
    });

    // The list-of-`Nat`-or-`Int` payload: one `(mut i64)` per element, matching the codegen's `$longs` and the native adapter's `longs_array_type`. The guest narrows and boxes each element, so JS reads and writes plain `BigInt`s.
    let longs = TypeName::from("longs");

    module.add_type(longs.clone(), longs_sub_type());

    let longs_ref = ValType::Ref(RefType {
        is_nullable: false,
        heap_type: HeapType::Concrete(longs.clone()),
    });
    let i64_val = ValType::Num(NumType::I64);

    // The list-of-`Bool` payload: one `(mut i32)` word per element, matching the codegen's `$words` and the native adapter's `words_array_type`.
    let words = TypeName::from("words");

    module.add_type(words.clone(), words_sub_type());

    let words_ref = ValType::Ref(RefType {
        is_nullable: false,
        heap_type: HeapType::Concrete(words.clone()),
    });

    let accessors: [Accessor; 16] = [
        (
            "bytes_len",
            vec![("b", bytes_ref.clone())],
            vec![i32_val.clone()],
            vec![Instr::ArrayLen],
        ),
        (
            "bytes_get",
            vec![("b", bytes_ref.clone()), ("i", i32_val.clone())],
            vec![i32_val.clone()],
            vec![Instr::ArrayGetU {
                type_name: bytes.clone(),
            }],
        ),
        (
            "bytes_new",
            vec![("n", i32_val.clone())],
            vec![bytes_ref.clone()],
            vec![Instr::ArrayNewDefault {
                type_name: bytes.clone(),
            }],
        ),
        (
            "bytes_set",
            vec![
                ("b", bytes_ref.clone()),
                ("i", i32_val.clone()),
                ("v", i32_val.clone()),
            ],
            vec![],
            vec![Instr::ArraySet {
                type_name: bytes.clone(),
            }],
        ),
        (
            "list_len",
            vec![("l", elems_ref.clone())],
            vec![i32_val.clone()],
            vec![Instr::ArrayLen],
        ),
        (
            "list_get",
            vec![("l", elems_ref.clone()), ("i", i32_val.clone())],
            vec![any_ref.clone()],
            vec![Instr::ArrayGet {
                type_name: elems.clone(),
            }],
        ),
        (
            "list_new",
            vec![("n", i32_val.clone())],
            vec![elems_ref.clone()],
            vec![Instr::ArrayNewDefault {
                type_name: elems.clone(),
            }],
        ),
        (
            "list_set",
            vec![
                ("l", elems_ref.clone()),
                ("i", i32_val.clone()),
                ("v", any_ref.clone()),
            ],
            vec![],
            vec![Instr::ArraySet {
                type_name: elems.clone(),
            }],
        ),
        (
            "longs_len",
            vec![("l", longs_ref.clone())],
            vec![i32_val.clone()],
            vec![Instr::ArrayLen],
        ),
        (
            "longs_get",
            vec![("l", longs_ref.clone()), ("i", i32_val.clone())],
            vec![i64_val.clone()],
            vec![Instr::ArrayGet {
                type_name: longs.clone(),
            }],
        ),
        (
            "longs_new",
            vec![("n", i32_val.clone())],
            vec![longs_ref.clone()],
            vec![Instr::ArrayNewDefault {
                type_name: longs.clone(),
            }],
        ),
        (
            "longs_set",
            vec![
                ("l", longs_ref.clone()),
                ("i", i32_val.clone()),
                ("v", i64_val.clone()),
            ],
            vec![],
            vec![Instr::ArraySet {
                type_name: longs.clone(),
            }],
        ),
        (
            "words_len",
            vec![("w", words_ref.clone())],
            vec![i32_val.clone()],
            vec![Instr::ArrayLen],
        ),
        (
            "words_get",
            vec![("w", words_ref.clone()), ("i", i32_val.clone())],
            vec![i32_val.clone()],
            vec![Instr::ArrayGet {
                type_name: words.clone(),
            }],
        ),
        (
            "words_new",
            vec![("n", i32_val.clone())],
            vec![words_ref.clone()],
            vec![Instr::ArrayNewDefault {
                type_name: words.clone(),
            }],
        ),
        (
            "words_set",
            vec![
                ("w", words_ref.clone()),
                ("i", i32_val.clone()),
                ("v", i32_val.clone()),
            ],
            vec![],
            vec![Instr::ArraySet {
                type_name: words.clone(),
            }],
        ),
    ];

    for (name, params, outputs, ops) in accessors {
        let type_name = TypeName::from(name);
        let func_name = FuncName::from(name);

        module.add_type(
            type_name.clone(),
            func_type(params.iter().map(|(_, val_type)| val_type.clone()), outputs),
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
                        .chain(ops),
                ),
            },
        );

        module.add_export(name, Export::Func(func_name));
    }

    // Empty and unbounded: the harness grows it to whatever the byte string in flight needs.
    let memory = MemName::from("memory");

    module.add_memory(
        memory.clone(),
        MemType {
            address_type: AddressType::I32,
            limits: Limits { min: 0, max: None },
        },
    );

    module.add_export("memory", Export::Memory(memory.clone()));

    // Byte-granular access at the copy loop's running index, which is the address itself.
    let byte_at = || MemArg {
        mem_name: memory.clone(),
        align: 0,
        offset: 0,
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
                        Instr::I32Store8 { mem_arg: byte_at() },
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
                        Instr::I32Load8U { mem_arg: byte_at() },
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
