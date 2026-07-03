//! The shared rope helper functions — the only module-level functions the
//! emitter mints beyond the program's own (everything else is inlined at its
//! use site). Two families:
//!
//! - `$<carrier>/force` flattens a rope to its payload array: the leaf answers
//!   its payload, a cached node answers its cache, and an uncached node fills
//!   a fresh payload by an *iterative* tree walk (an explicit `$elems`
//!   worklist, grown by doubling), so a 100k-deep concat chain never touches
//!   the wasm call stack. Only the entry node memoizes — intermediates are
//!   usually garbage the moment the walk passes them.
//! - `$<carrier>/wrap` boxes a host-built flat payload into a fresh leaf on
//!   re-entry.
//!
//! The `arr/bin` variants are the host boundary's deep forms: an `Arr(Bin)` /
//! `Arr(Io)` wire value carries `Bin`-shaped *elements*, which the host lifts
//! and lowers as raw `$bytes` — so params force each element too, and results
//! wrap each element back.

use {
    super::{RopeData, Table},
    curios_wasm::{
        BlockType, CompType, FieldName, Func, FuncName, FuncType, HeapType, Instr, LabelName,
        LocalName, Module, NumType, RefType, ResultType, SubType, TypeName, ValType,
    },
};

fn concrete_ref(type_name: TypeName, is_nullable: bool) -> RefType {
    RefType {
        is_nullable,
        heap_type: HeapType::Concrete(type_name),
    }
}

fn concrete_val(type_name: TypeName, is_nullable: bool) -> ValType {
    ValType::Ref(concrete_ref(type_name, is_nullable))
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

fn cast(type_name: &TypeName) -> Instr {
    Instr::RefCast {
        ref_type: concrete_ref(type_name.clone(), false),
    }
}

fn field_get(type_name: &TypeName, field_name: &FieldName) -> Instr {
    Instr::StructGet {
        type_name: type_name.clone(),
        field_name: field_name.clone(),
    }
}

fn field_set(type_name: &TypeName, field_name: &FieldName) -> Instr {
    Instr::StructSet {
        type_name: type_name.clone(),
        field_name: field_name.clone(),
    }
}

fn null(type_name: &TypeName) -> Instr {
    Instr::RefNull {
        heap_type: HeapType::Concrete(type_name.clone()),
    }
}

#[derive(Debug)]
pub struct RopeEmitter<'a, 'b> {
    table: &'a Table<'a>,
    module: &'b mut Module,
}

impl<'a, 'b> RopeEmitter<'a, 'b> {
    pub fn new(table: &'a Table<'a>, module: &'b mut Module) -> Self {
        Self { table, module }
    }

    /// Declare one helper: a final func type named after the function, plus
    /// the function itself. Helpers are called by name, never `ref.func`'d or
    /// exported, so no declaration beyond the pair is needed.
    fn add_helper(
        &mut self,
        func_name: FuncName,
        param: (LocalName, ValType),
        result: ValType,
        locals: Vec<(LocalName, ValType)>,
        instrs: Vec<Instr>,
    ) {
        let type_name = TypeName::from(func_name.as_str());

        self.module.add_type(
            type_name.clone(),
            SubType {
                is_final: true,
                super_types: vec![],
                comp_type: CompType::Func(FuncType {
                    inputs: ResultType::from([param.1]),
                    outputs: ResultType::from([result]),
                }),
            },
        );

        self.module.add_func(
            func_name,
            Func {
                type_name,
                params: vec![param.0],
                locals,
                expr: instrs.into(),
            },
        );
    }

    /// `$<carrier>/force (ref <base>) -> (ref <payload>)`.
    ///
    /// ```wat
    /// if r.tag == 0            → r.payload            ;; leaf
    /// if r.cache != null       → r.cache              ;; already forced
    /// out   := array.new_default <payload> r.len
    /// stack := array.new_default $elems 32            ;; explicit worklist
    /// cur   := r
    /// loop:                                           ;; per leaf-like chunk
    ///   descend: while cur is an uncached node,
    ///     push cur.right (growing the stack by doubling), cur := cur.left
    ///   copy the chunk (leaf payload or node cache) into out at offset
    ///   pop cur from the stack; repeat until empty
    /// r.cache := out; r.left := null; r.right := null ;; memoize, release tree
    /// ```
    pub fn emit_force_func(&mut self, rope: &RopeData, func_name: FuncName) {
        let elems = self.table.elems_type();

        let r = LocalName::from("r");
        let node = LocalName::from("node");
        let out = LocalName::from("out");
        let stack = LocalName::from("stack");
        let grown = LocalName::from("grown");
        let sp = LocalName::from("sp");
        let offset = LocalName::from("offset");
        let cur = LocalName::from("cur");
        let payload = LocalName::from("payload");

        let i32_val = ValType::Num(NumType::I32);
        let locals = vec![
            (node.clone(), concrete_val(rope.node.clone(), true)),
            (out.clone(), concrete_val(rope.payload.clone(), true)),
            (stack.clone(), concrete_val(elems.clone(), true)),
            (grown.clone(), concrete_val(elems.clone(), true)),
            (sp.clone(), i32_val.clone()),
            (offset.clone(), i32_val.clone()),
            (cur.clone(), concrete_val(rope.base.clone(), true)),
            (payload.clone(), concrete_val(rope.payload.clone(), true)),
        ];

        let mut instrs = Vec::new();

        // Leaf: answer the payload directly.
        instrs.extend([
            get(&r),
            field_get(&rope.base, &rope.tag_field),
            Instr::I32Eqz,
            Instr::If {
                label_name: LabelName::from("leaf"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    get(&r),
                    cast(&rope.leaf),
                    field_get(&rope.leaf, &rope.payload_field),
                    Instr::Return,
                ],
                else_instructions: vec![],
            },
        ]);

        // Cached node: answer the memo.
        instrs.extend([
            get(&r),
            cast(&rope.node),
            set(&node),
            get(&node),
            field_get(&rope.node, &rope.cache_field),
            Instr::RefIsNull,
            Instr::I32Eqz,
            Instr::If {
                label_name: LabelName::from("cached"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    get(&node),
                    field_get(&rope.node, &rope.cache_field),
                    Instr::RefAsNonNull,
                    Instr::Return,
                ],
                else_instructions: vec![],
            },
        ]);

        // out = array.new_default <payload> r.len; stack = 32-slot worklist.
        instrs.extend([
            get(&r),
            field_get(&rope.base, &rope.len_field),
            Instr::ArrayNewDefault {
                type_name: rope.payload.clone(),
            },
            set(&out),
            Instr::I32Const { value: 32 },
            Instr::ArrayNewDefault {
                type_name: elems.clone(),
            },
            set(&stack),
            get(&r),
            set(&cur),
        ]);

        // The descent body: classify `cur`, either resolving a leaf-like
        // payload (exit to `$emit`) or pushing right and descending left.
        let descend_label = LabelName::from("descend");
        let mut descend = vec![
            // Leaf: payload = cur.payload.
            get(&cur),
            field_get(&rope.base, &rope.tag_field),
            Instr::I32Eqz,
            Instr::If {
                label_name: LabelName::from("at_leaf"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    get(&cur),
                    cast(&rope.leaf),
                    field_get(&rope.leaf, &rope.payload_field),
                    set(&payload),
                    Instr::Br {
                        label_name: LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
            // Cached node: payload = cur.cache.
            get(&cur),
            cast(&rope.node),
            field_get(&rope.node, &rope.cache_field),
            Instr::RefIsNull,
            Instr::I32Eqz,
            Instr::If {
                label_name: LabelName::from("at_cached"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    get(&cur),
                    cast(&rope.node),
                    field_get(&rope.node, &rope.cache_field),
                    set(&payload),
                    Instr::Br {
                        label_name: LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
            // Uncached node: grow the worklist if full…
            get(&sp),
            get(&stack),
            Instr::ArrayLen,
            Instr::I32Eq,
            Instr::If {
                label_name: LabelName::from("grow"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    get(&sp),
                    Instr::I32Const { value: 1 },
                    Instr::I32Shl,
                    Instr::ArrayNewDefault {
                        type_name: elems.clone(),
                    },
                    set(&grown),
                    get(&grown),
                    Instr::I32Const { value: 0 },
                    get(&stack),
                    Instr::I32Const { value: 0 },
                    get(&sp),
                    Instr::ArrayCopy {
                        source_name: elems.clone(),
                        target_name: elems.clone(),
                    },
                    get(&grown),
                    set(&stack),
                ],
                else_instructions: vec![],
            },
        ];
        // …push cur.right, descend into cur.left.
        descend.extend([
            get(&stack),
            get(&sp),
            get(&cur),
            cast(&rope.node),
            field_get(&rope.node, &rope.right_field),
            Instr::ArraySet {
                type_name: elems.clone(),
            },
            get(&sp),
            Instr::I32Const { value: 1 },
            Instr::I32Add,
            set(&sp),
            get(&cur),
            cast(&rope.node),
            field_get(&rope.node, &rope.left_field),
            set(&cur),
            Instr::Br {
                label_name: descend_label.clone(),
            },
        ]);

        // The walk: descend to a chunk, copy it at the running offset, pop.
        let walk_label = LabelName::from("walk");
        instrs.push(Instr::Loop {
            label_name: walk_label.clone(),
            block_type: BlockType::Empty,
            instructions: vec![
                Instr::Block {
                    label_name: LabelName::from("emit"),
                    block_type: BlockType::Empty,
                    instructions: vec![Instr::Loop {
                        label_name: descend_label,
                        block_type: BlockType::Empty,
                        instructions: descend,
                    }],
                },
                // array.copy out[offset..] <- payload[0..len(payload)]
                get(&out),
                get(&offset),
                get(&payload),
                Instr::I32Const { value: 0 },
                get(&payload),
                Instr::ArrayLen,
                Instr::ArrayCopy {
                    source_name: rope.payload.clone(),
                    target_name: rope.payload.clone(),
                },
                get(&offset),
                get(&payload),
                Instr::ArrayLen,
                Instr::I32Add,
                set(&offset),
                // Pop the next pending subtree; fall out when empty.
                get(&sp),
                Instr::If {
                    label_name: LabelName::from("pop"),
                    block_type: BlockType::Empty,
                    then_instructions: vec![
                        get(&sp),
                        Instr::I32Const { value: 1 },
                        Instr::I32Sub,
                        set(&sp),
                        get(&stack),
                        get(&sp),
                        Instr::ArrayGet {
                            type_name: elems.clone(),
                        },
                        cast(&rope.base),
                        set(&cur),
                        Instr::Br {
                            label_name: walk_label,
                        },
                    ],
                    else_instructions: vec![],
                },
            ],
        });

        // Memoize the entry node and release its tree.
        instrs.extend([
            get(&node),
            get(&out),
            field_set(&rope.node, &rope.cache_field),
            get(&node),
            null(&rope.base),
            field_set(&rope.node, &rope.left_field),
            get(&node),
            null(&rope.base),
            field_set(&rope.node, &rope.right_field),
            get(&out),
            Instr::RefAsNonNull,
        ]);

        self.add_helper(
            func_name,
            (r, concrete_val(rope.base.clone(), false)),
            concrete_val(rope.payload.clone(), false),
            locals,
            instrs,
        );
    }

    /// `$arr/bin/force (ref $arr) -> (ref $elems)`: force the outer rope, then
    /// force every element through `$bin/force` into a *fresh* payload (the
    /// shallow force of a leaf answers its live payload, which must not be
    /// element-rewritten in place).
    pub fn emit_force_arr_bin_func(&mut self, func_name: FuncName) {
        let elems = self.table.elems_type();
        let bin = self.table.bin_rope();

        let r = LocalName::from("r");
        let flat = LocalName::from("flat");
        let fresh = LocalName::from("fresh");
        let idx = LocalName::from("idx");
        let count = LocalName::from("count");

        let i32_val = ValType::Num(NumType::I32);
        let locals = vec![
            (flat.clone(), concrete_val(elems.clone(), true)),
            (fresh.clone(), concrete_val(elems.clone(), true)),
            (idx.clone(), i32_val.clone()),
            (count.clone(), i32_val),
        ];

        let loop_label = LabelName::from("fill");
        let done_label = LabelName::from("done");

        let instrs = vec![
            get(&r),
            Instr::Call {
                func_name: self.table.force_arr_func(),
            },
            set(&flat),
            get(&flat),
            Instr::ArrayLen,
            set(&count),
            get(&count),
            Instr::ArrayNewDefault {
                type_name: elems.clone(),
            },
            set(&fresh),
            Instr::Block {
                label_name: done_label.clone(),
                block_type: BlockType::Empty,
                instructions: vec![Instr::Loop {
                    label_name: loop_label.clone(),
                    block_type: BlockType::Empty,
                    instructions: vec![
                        get(&idx),
                        get(&count),
                        Instr::I32GeU,
                        Instr::BrIf {
                            label_name: done_label,
                        },
                        get(&fresh),
                        get(&idx),
                        get(&flat),
                        get(&idx),
                        Instr::ArrayGet {
                            type_name: elems.clone(),
                        },
                        cast(&bin.base),
                        Instr::Call {
                            func_name: self.table.force_bin_func(),
                        },
                        Instr::ArraySet {
                            type_name: elems.clone(),
                        },
                        get(&idx),
                        Instr::I32Const { value: 1 },
                        Instr::I32Add,
                        set(&idx),
                        Instr::Br {
                            label_name: loop_label,
                        },
                    ],
                }],
            },
            get(&fresh),
            Instr::RefAsNonNull,
        ];

        self.add_helper(
            func_name,
            (r, concrete_val(self.table.arr_type(), false)),
            concrete_val(elems, false),
            locals,
            instrs,
        );
    }

    /// `$<carrier>/wrap (ref <payload>) -> (ref <base>)`: one fresh leaf.
    pub fn emit_wrap_func(&mut self, rope: &RopeData, func_name: FuncName) {
        let b = LocalName::from("b");

        let instrs = vec![
            Instr::I32Const { value: 0 },
            get(&b),
            Instr::ArrayLen,
            get(&b),
            Instr::StructNew {
                type_name: rope.leaf.clone(),
            },
        ];

        self.add_helper(
            func_name,
            (b, concrete_val(rope.payload.clone(), false)),
            concrete_val(rope.base.clone(), false),
            vec![],
            instrs,
        );
    }

    /// `$arr/bin/wrap (ref $elems) -> (ref $arr)`: wrap each raw `$bytes`
    /// element into a `$bin/leaf` in place — the host-built array is fresh,
    /// nothing else aliases it — then the outer array into an `$arr/leaf`.
    pub fn emit_wrap_arr_bin_func(&mut self, func_name: FuncName) {
        let elems = self.table.elems_type();
        let bin = self.table.bin_rope();
        let arr = self.table.arr_rope();

        let e = LocalName::from("e");
        let idx = LocalName::from("idx");
        let count = LocalName::from("count");
        let bytes = LocalName::from("bytes");

        let i32_val = ValType::Num(NumType::I32);
        let locals = vec![
            (idx.clone(), i32_val.clone()),
            (count.clone(), i32_val),
            (bytes.clone(), concrete_val(bin.payload.clone(), true)),
        ];

        let loop_label = LabelName::from("fill");
        let done_label = LabelName::from("done");

        let instrs = vec![
            get(&e),
            Instr::ArrayLen,
            set(&count),
            Instr::Block {
                label_name: done_label.clone(),
                block_type: BlockType::Empty,
                instructions: vec![Instr::Loop {
                    label_name: loop_label.clone(),
                    block_type: BlockType::Empty,
                    instructions: vec![
                        get(&idx),
                        get(&count),
                        Instr::I32GeU,
                        Instr::BrIf {
                            label_name: done_label,
                        },
                        get(&e),
                        get(&idx),
                        Instr::ArrayGet {
                            type_name: elems.clone(),
                        },
                        cast(&bin.payload),
                        set(&bytes),
                        get(&e),
                        get(&idx),
                        Instr::I32Const { value: 0 },
                        get(&bytes),
                        Instr::ArrayLen,
                        get(&bytes),
                        Instr::RefAsNonNull,
                        Instr::StructNew {
                            type_name: bin.leaf.clone(),
                        },
                        Instr::ArraySet {
                            type_name: elems.clone(),
                        },
                        get(&idx),
                        Instr::I32Const { value: 1 },
                        Instr::I32Add,
                        set(&idx),
                        Instr::Br {
                            label_name: loop_label,
                        },
                    ],
                }],
            },
            Instr::I32Const { value: 0 },
            get(&count),
            get(&e),
            Instr::StructNew {
                type_name: arr.leaf.clone(),
            },
        ];

        self.add_helper(
            func_name,
            (e, concrete_val(elems, false)),
            concrete_val(arr.base.clone(), false),
            locals,
            instrs,
        );
    }
}
