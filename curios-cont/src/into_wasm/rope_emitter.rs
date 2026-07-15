//! The shared rope helper functions — the only module-level functions the
//! emitter mints beyond the program's own (everything else is inlined at its
//! use site — straight-line sequences only; anything the emitter lowers to a
//! *loop* lives here, so its mutable scratch locals are zeroed by the fresh
//! activation instead of leaking across executions of one call site). Seven
//! families:
//!
//! - `$<carrier>/force` flattens a byte- or element-grain rope to its payload array: the leaf answers
//!   its payload, a cached node answers its cache, and everything else fills
//!   a fresh payload by an *iterative* tree walk (an explicit `$elems`
//!   worklist, grown by doubling), so a 100k-deep concat chain never touches
//!   the wasm call stack. Only an entry *node* memoizes — intermediates are
//!   usually garbage the moment the walk passes them, and a view's fill is a
//!   single window copy of exactly its own size.
//! - `$<carrier>/embed` places a host-built flat payload into a fresh leaf on
//!   re-entry.
//! - `$<carrier>/slice` builds the O(1) window: bounds-check, the trivial
//!   windows answer an empty leaf or the rope itself, a view collapses (so
//!   windows never stack), and an uncached node base is forced first — which
//!   memoizes — so every `view` in existence has a *flat-available* base (a
//!   leaf or a cached node).
//! - `$<carrier>/read` answers one element: off a leaf payload, *through* a
//!   view's window without forcing (the invariant above makes that O(1)), or
//!   via `force` on a node.
//! - `$bits/force` performs the same iterative walk in logical bit units,
//!   filling a zeroed packed payload and memoizing it on an entry node.
//! - `$bytes/eql` compares two `Bytes` ropes bytewise: unequal lengths answer without
//!   forcing, equal lengths force both payloads once and walk them.
//! - `$lst/map` applies a unary closure to every element of the forced
//!   payload, filling a fresh leaf.
//!
//! The `lst/bin` variants are the host boundary's deep forms: an `Lst(Bin)` /
//! `Lst(Io)` wire value carries `Bin`-shaped *elements*, which the host lifts
//! and lowers as raw `$bytes` — so params force each element too, and results
//! embed each element back.

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
pub(super) struct RopeEmitter<'a, 'b> {
    table: &'a Table<'a>,
    module: &'b mut Module,
}

impl<'a, 'b> RopeEmitter<'a, 'b> {
    pub(super) fn new(table: &'a Table<'a>, module: &'b mut Module) -> Self {
        Self { table, module }
    }

    /// Declare one helper: a final func type named after the function, plus
    /// the function itself. Helpers are called by name, never `ref.func`'d or
    /// exported, so no declaration beyond the pair is needed.
    fn add_helper(
        &mut self,
        func_name: FuncName,
        params: Vec<(LocalName, ValType)>,
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
                    inputs: ResultType::from(params.iter().map(|(_, val_type)| val_type.clone())),
                    outputs: ResultType::from([result]),
                }),
            },
        );

        self.module.add_func(
            func_name,
            Func {
                type_name,
                params: params.into_iter().map(|(name, _)| name).collect(),
                locals,
                expr: instrs.into(),
            },
        );
    }

    /// `$<carrier>/force (ref <base>) -> (ref <payload>)`.
    ///
    /// ```wat
    /// if r.tag == 0                  → r.payload      ;; leaf
    /// if r.tag == 1 && r.cache != null → r.cache      ;; already forced
    /// out   := array.new_default <payload> r.len
    /// stack := array.new_default $elems 32            ;; explicit worklist
    /// cur   := r
    /// loop:                                           ;; per leaf-like chunk
    ///   descend: while cur is an uncached node,
    ///     push cur.right (growing the stack by doubling), cur := cur.left
    ///   copy the chunk — a leaf payload, a node cache, or a view's window
    ///   over its flat-available base — into out at offset
    ///   pop cur from the stack; repeat until empty
    /// if r.tag == 1:                                  ;; memoize, release tree
    ///   r.cache := out; r.left := null; r.right := null
    /// ```
    ///
    /// An entry *view* is not memoized (its fields are immutable): its fill is
    /// one window copy of exactly its own size, so there is nothing quadratic
    /// to fence off.
    pub(super) fn emit_force_func(&mut self, rope: &RopeData, func_name: FuncName) {
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
        let src_off = LocalName::from("src_off");
        let count = LocalName::from("count");
        let sb = LocalName::from("sb");

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
            (src_off.clone(), i32_val.clone()),
            (count.clone(), i32_val.clone()),
            (sb.clone(), concrete_val(rope.base.clone(), true)),
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

        // Cached node: answer the memo. (A view skips straight to the walk.)
        instrs.extend([
            get(&r),
            field_get(&rope.base, &rope.tag_field),
            Instr::I32Const { value: 1 },
            Instr::I32Eq,
            Instr::If {
                label_name: LabelName::from("entry_node"),
                block_type: BlockType::Empty,
                then_instructions: vec![
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
        // chunk — payload + source window — (exit to `$emit`) or pushing
        // right and descending left.
        let descend_label = LabelName::from("descend");
        let mut descend = vec![
            // Leaf: the whole payload.
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
                    Instr::I32Const { value: 0 },
                    set(&src_off),
                    get(&payload),
                    Instr::ArrayLen,
                    set(&count),
                    Instr::Br {
                        label_name: LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
            // View: its window over the base's flat payload — a leaf's payload
            // or a cached node's cache (non-null by the slice invariant; the
            // null trap in `array.copy` is its enforcement).
            get(&cur),
            field_get(&rope.base, &rope.tag_field),
            Instr::I32Const { value: 2 },
            Instr::I32Eq,
            Instr::If {
                label_name: LabelName::from("at_view"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    get(&cur),
                    cast(&rope.view),
                    field_get(&rope.view, &rope.base_field),
                    set(&sb),
                    get(&cur),
                    cast(&rope.view),
                    field_get(&rope.view, &rope.offset_field),
                    set(&src_off),
                    get(&cur),
                    field_get(&rope.base, &rope.len_field),
                    set(&count),
                    get(&sb),
                    field_get(&rope.base, &rope.tag_field),
                    Instr::I32Eqz,
                    Instr::If {
                        label_name: LabelName::from("view_base"),
                        block_type: BlockType::Empty,
                        then_instructions: vec![
                            get(&sb),
                            cast(&rope.leaf),
                            field_get(&rope.leaf, &rope.payload_field),
                            set(&payload),
                        ],
                        else_instructions: vec![
                            get(&sb),
                            cast(&rope.node),
                            field_get(&rope.node, &rope.cache_field),
                            set(&payload),
                        ],
                    },
                    Instr::Br {
                        label_name: LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
            // Cached node: the whole cache.
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
                    Instr::I32Const { value: 0 },
                    set(&src_off),
                    get(&payload),
                    Instr::ArrayLen,
                    set(&count),
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
                // array.copy out[offset..] <- payload[src_off..src_off+count]
                get(&out),
                get(&offset),
                get(&payload),
                get(&src_off),
                get(&count),
                Instr::ArrayCopy {
                    source_name: rope.payload.clone(),
                    target_name: rope.payload.clone(),
                },
                get(&offset),
                get(&count),
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

        // Memoize a node entry and release its tree (`node` was set at entry
        // exactly when the tag is 1); a view entry has nowhere to memoize.
        instrs.extend([
            get(&r),
            field_get(&rope.base, &rope.tag_field),
            Instr::I32Const { value: 1 },
            Instr::I32Eq,
            Instr::If {
                label_name: LabelName::from("memoize"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    get(&node),
                    get(&out),
                    field_set(&rope.node, &rope.cache_field),
                    get(&node),
                    null(&rope.base),
                    field_set(&rope.node, &rope.left_field),
                    get(&node),
                    null(&rope.base),
                    field_set(&rope.node, &rope.right_field),
                ],
                else_instructions: vec![],
            },
            get(&out),
            Instr::RefAsNonNull,
        ]);

        self.add_helper(
            func_name,
            vec![(r, concrete_val(rope.base.clone(), false))],
            concrete_val(rope.payload.clone(), false),
            locals,
            instrs,
        );
    }

    /// `$bits/force (ref $rope/bin) -> (ref $bytes)`: flatten a bit-grain rope into
    /// a packed LSB-first payload. The tree walk is iterative, like
    /// [`Self::emit_force_func`], but chunk windows and offsets are measured in
    /// bits and the destination has `ceil(len / 8)` zeroed bytes. Copying only
    /// logical bits keeps the unused high padding of the final byte zero.
    pub(super) fn emit_bits_force_func(&mut self, func_name: FuncName) {
        let rope = self.table.bin_rope();
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
        let src_off = LocalName::from("src_off");
        let count = LocalName::from("count");
        let sb = LocalName::from("sb");
        let copy_i = LocalName::from("copy_i");
        let src_bit = LocalName::from("src_bit");
        let dst_bit = LocalName::from("dst_bit");

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
            (src_off.clone(), i32_val.clone()),
            (count.clone(), i32_val.clone()),
            (sb.clone(), concrete_val(rope.base.clone(), true)),
            (copy_i.clone(), i32_val.clone()),
            (src_bit.clone(), i32_val.clone()),
            (dst_bit.clone(), i32_val.clone()),
        ];

        let mut instrs = vec![
            // A packed leaf is already flat.
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
            // A cached node answers its packed memo.
            get(&r),
            field_get(&rope.base, &rope.tag_field),
            Instr::I32Const { value: 1 },
            Instr::I32Eq,
            Instr::If {
                label_name: LabelName::from("entry_node"),
                block_type: BlockType::Empty,
                then_instructions: vec![
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
                ],
                else_instructions: vec![],
            },
            // ceil(r.len / 8) zeroed destination bytes.
            get(&r),
            field_get(&rope.base, &rope.len_field),
            Instr::I32Const { value: 7 },
            Instr::I32Add,
            Instr::I32Const { value: 3 },
            Instr::I32ShrU,
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
        ];

        let descend_label = LabelName::from("descend");
        let mut descend = vec![
            // Leaf: copy exactly its logical bit length, not its padding.
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
                    Instr::I32Const { value: 0 },
                    set(&src_off),
                    get(&cur),
                    field_get(&rope.base, &rope.len_field),
                    set(&count),
                    Instr::Br {
                        label_name: LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
            // A view is a logical-bit window over a leaf or cached node.
            get(&cur),
            field_get(&rope.base, &rope.tag_field),
            Instr::I32Const { value: 2 },
            Instr::I32Eq,
            Instr::If {
                label_name: LabelName::from("at_view"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    get(&cur),
                    cast(&rope.view),
                    field_get(&rope.view, &rope.base_field),
                    set(&sb),
                    get(&cur),
                    cast(&rope.view),
                    field_get(&rope.view, &rope.offset_field),
                    set(&src_off),
                    get(&cur),
                    field_get(&rope.base, &rope.len_field),
                    set(&count),
                    get(&sb),
                    field_get(&rope.base, &rope.tag_field),
                    Instr::I32Eqz,
                    Instr::If {
                        label_name: LabelName::from("view_base"),
                        block_type: BlockType::Empty,
                        then_instructions: vec![
                            get(&sb),
                            cast(&rope.leaf),
                            field_get(&rope.leaf, &rope.payload_field),
                            set(&payload),
                        ],
                        else_instructions: vec![
                            get(&sb),
                            cast(&rope.node),
                            field_get(&rope.node, &rope.cache_field),
                            set(&payload),
                        ],
                    },
                    Instr::Br {
                        label_name: LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
            // A cached node contributes its logical length from bit zero.
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
                    Instr::I32Const { value: 0 },
                    set(&src_off),
                    get(&cur),
                    field_get(&rope.base, &rope.len_field),
                    set(&count),
                    Instr::Br {
                        label_name: LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
            // Grow the explicit worklist by doubling when it is full.
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

        let walk_label = LabelName::from("walk");
        let copy_label = LabelName::from("copy");
        let copy_done = LabelName::from("copy_done");
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
                // Scratch locals are reused per chunk, so reset the cursor.
                Instr::I32Const { value: 0 },
                set(&copy_i),
                Instr::Block {
                    label_name: copy_done.clone(),
                    block_type: BlockType::Empty,
                    instructions: vec![Instr::Loop {
                        label_name: copy_label.clone(),
                        block_type: BlockType::Empty,
                        instructions: vec![
                            get(&copy_i),
                            get(&count),
                            Instr::I32GeU,
                            Instr::BrIf {
                                label_name: copy_done,
                            },
                            get(&src_off),
                            get(&copy_i),
                            Instr::I32Add,
                            set(&src_bit),
                            get(&offset),
                            get(&copy_i),
                            Instr::I32Add,
                            set(&dst_bit),
                            // out[dst/8] |= ((payload[src/8] >> src%8) & 1)
                            //               << dst%8
                            get(&out),
                            get(&dst_bit),
                            Instr::I32Const { value: 3 },
                            Instr::I32ShrU,
                            get(&out),
                            get(&dst_bit),
                            Instr::I32Const { value: 3 },
                            Instr::I32ShrU,
                            Instr::ArrayGetU {
                                type_name: rope.payload.clone(),
                            },
                            get(&payload),
                            get(&src_bit),
                            Instr::I32Const { value: 3 },
                            Instr::I32ShrU,
                            Instr::ArrayGetU {
                                type_name: rope.payload.clone(),
                            },
                            get(&src_bit),
                            Instr::I32Const { value: 7 },
                            Instr::I32And,
                            Instr::I32ShrU,
                            Instr::I32Const { value: 1 },
                            Instr::I32And,
                            get(&dst_bit),
                            Instr::I32Const { value: 7 },
                            Instr::I32And,
                            Instr::I32Shl,
                            Instr::I32Or,
                            Instr::ArraySet {
                                type_name: rope.payload.clone(),
                            },
                            get(&copy_i),
                            Instr::I32Const { value: 1 },
                            Instr::I32Add,
                            set(&copy_i),
                            Instr::Br {
                                label_name: copy_label,
                            },
                        ],
                    }],
                },
                get(&offset),
                get(&count),
                Instr::I32Add,
                set(&offset),
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

        instrs.extend([
            get(&r),
            field_get(&rope.base, &rope.tag_field),
            Instr::I32Const { value: 1 },
            Instr::I32Eq,
            Instr::If {
                label_name: LabelName::from("memoize"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    get(&node),
                    get(&out),
                    field_set(&rope.node, &rope.cache_field),
                    get(&node),
                    null(&rope.base),
                    field_set(&rope.node, &rope.left_field),
                    get(&node),
                    null(&rope.base),
                    field_set(&rope.node, &rope.right_field),
                ],
                else_instructions: vec![],
            },
            get(&out),
            Instr::RefAsNonNull,
        ]);

        self.add_helper(
            func_name,
            vec![(r, concrete_val(rope.base.clone(), false))],
            concrete_val(rope.payload, false),
            locals,
            instrs,
        );
    }

    /// `$<carrier>/slice (ref <base>, i32, i32) -> (ref <base>)`.
    ///
    /// ```wat
    /// if s > e || e > r.len   → unreachable            ;; the eager bounds trap
    /// n := e - s
    /// if n == 0               → fresh empty leaf
    /// if s == 0 && n == r.len → r                      ;; whole-window alias
    /// if r.tag == 2           → view{2, n, r.base, r.offset + s}   ;; collapse
    /// if r.tag == 1 && r.cache == null → call force(r) (drop)     ;; memoizes
    /// view{2, n, r, s}
    /// ```
    ///
    /// The node arm's force is what maintains the read-through invariant:
    /// every `view` base is flat-available from birth, and stays so (a cache is
    /// written once, never cleared).
    pub(super) fn emit_slice_func(
        &mut self,
        rope: &RopeData,
        func_name: FuncName,
        force_func: Option<FuncName>,
    ) {
        let r = LocalName::from("r");
        let s = LocalName::from("s");
        let e = LocalName::from("e");
        let n = LocalName::from("n");

        let i32_val = ValType::Num(NumType::I32);
        let locals = vec![(n.clone(), i32_val.clone())];

        let mut instrs = Vec::new();

        // Bounds: the pre-window trap `slice` always had (an out-of-range
        // window must not become a deferred — or never-taken — trap).
        instrs.extend([
            get(&s),
            get(&e),
            Instr::I32GtU,
            get(&e),
            get(&r),
            field_get(&rope.base, &rope.len_field),
            Instr::I32GtU,
            Instr::I32Or,
            Instr::If {
                label_name: LabelName::from("bounds"),
                block_type: BlockType::Empty,
                then_instructions: vec![Instr::Unreachable],
                else_instructions: vec![],
            },
        ]);

        // n = e - s; the empty window is a fresh empty leaf.
        instrs.extend([
            get(&e),
            get(&s),
            Instr::I32Sub,
            set(&n),
            get(&n),
            Instr::I32Eqz,
            Instr::If {
                label_name: LabelName::from("empty"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    Instr::I32Const { value: 0 },
                    Instr::I32Const { value: 0 },
                    Instr::I32Const { value: 0 },
                    Instr::ArrayNewDefault {
                        type_name: rope.payload.clone(),
                    },
                    Instr::StructNew {
                        type_name: rope.leaf.clone(),
                    },
                    Instr::Return,
                ],
                else_instructions: vec![],
            },
        ]);

        // The whole window is the rope itself.
        instrs.extend([
            get(&s),
            Instr::I32Eqz,
            get(&n),
            get(&r),
            field_get(&rope.base, &rope.len_field),
            Instr::I32Eq,
            Instr::I32And,
            Instr::If {
                label_name: LabelName::from("whole"),
                block_type: BlockType::Empty,
                then_instructions: vec![get(&r), Instr::Return],
                else_instructions: vec![],
            },
        ]);

        // A view collapses onto its own base, so windows never stack.
        instrs.extend([
            get(&r),
            field_get(&rope.base, &rope.tag_field),
            Instr::I32Const { value: 2 },
            Instr::I32Eq,
            Instr::If {
                label_name: LabelName::from("collapse"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    Instr::I32Const { value: 2 },
                    get(&n),
                    get(&r),
                    cast(&rope.view),
                    field_get(&rope.view, &rope.base_field),
                    get(&r),
                    cast(&rope.view),
                    field_get(&rope.view, &rope.offset_field),
                    get(&s),
                    Instr::I32Add,
                    Instr::StructNew {
                        type_name: rope.view.clone(),
                    },
                    Instr::Return,
                ],
                else_instructions: vec![],
            },
        ]);

        // An uncached node is forced first — memoized in place — so the view
        // below reads through its cache.
        if let Some(force_func) = force_func {
            instrs.extend([
                get(&r),
                field_get(&rope.base, &rope.tag_field),
                Instr::I32Const { value: 1 },
                Instr::I32Eq,
                Instr::If {
                    label_name: LabelName::from("node"),
                    block_type: BlockType::Empty,
                    then_instructions: vec![
                        get(&r),
                        cast(&rope.node),
                        field_get(&rope.node, &rope.cache_field),
                        Instr::RefIsNull,
                        Instr::If {
                            label_name: LabelName::from("settle"),
                            block_type: BlockType::Empty,
                            then_instructions: vec![
                                get(&r),
                                Instr::Call {
                                    func_name: force_func,
                                },
                                Instr::Drop,
                            ],
                            else_instructions: vec![],
                        },
                    ],
                    else_instructions: vec![],
                },
            ]);
        }

        instrs.extend([
            Instr::I32Const { value: 2 },
            get(&n),
            get(&r),
            get(&s),
            Instr::StructNew {
                type_name: rope.view.clone(),
            },
        ]);

        self.add_helper(
            func_name,
            vec![
                (r, concrete_val(rope.base.clone(), false)),
                (s, i32_val.clone()),
                (e, i32_val),
            ],
            concrete_val(rope.base.clone(), false),
            locals,
            instrs,
        );
    }

    /// `$<carrier>/read (ref <base>, i32) -> <element>`.
    ///
    /// ```wat
    /// if r.tag == 0 → r.payload[i]                    ;; leaf
    /// if r.tag == 2 →                                 ;; view: read through
    ///   (r.base.tag == 0 ? r.base.payload : r.base.cache)[r.offset + i]
    /// force(r)[i]                                     ;; node (memoized)
    /// ```
    ///
    /// Binary-sequence elements are packed bytes (`array.get_u`, an `i32` result);
    /// `Lst` elements are the top type (`array.get`).
    pub(super) fn emit_read_func(
        &mut self,
        rope: &RopeData,
        func_name: FuncName,
        force_func: FuncName,
    ) {
        let packed = rope.payload == self.table.bytes_type();

        let r = LocalName::from("r");
        let i = LocalName::from("i");
        let p = LocalName::from("p");
        let j = LocalName::from("j");
        let sb = LocalName::from("sb");

        let i32_val = ValType::Num(NumType::I32);
        let locals = vec![
            (p.clone(), concrete_val(rope.payload.clone(), true)),
            (j.clone(), i32_val.clone()),
            (sb.clone(), concrete_val(rope.base.clone(), true)),
        ];

        let get_elem = if packed {
            Instr::ArrayGetU {
                type_name: rope.payload.clone(),
            }
        } else {
            Instr::ArrayGet {
                type_name: rope.payload.clone(),
            }
        };

        let result = if packed {
            i32_val.clone()
        } else {
            Table::top_type(true)
        };

        let instrs = vec![
            get(&i),
            set(&j),
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
                    set(&p),
                ],
                else_instructions: vec![
                    get(&r),
                    field_get(&rope.base, &rope.tag_field),
                    Instr::I32Const { value: 2 },
                    Instr::I32Eq,
                    Instr::If {
                        label_name: LabelName::from("view"),
                        block_type: BlockType::Empty,
                        then_instructions: vec![
                            get(&r),
                            cast(&rope.view),
                            field_get(&rope.view, &rope.offset_field),
                            get(&i),
                            Instr::I32Add,
                            set(&j),
                            get(&r),
                            cast(&rope.view),
                            field_get(&rope.view, &rope.base_field),
                            set(&sb),
                            get(&sb),
                            field_get(&rope.base, &rope.tag_field),
                            Instr::I32Eqz,
                            Instr::If {
                                label_name: LabelName::from("view_base"),
                                block_type: BlockType::Empty,
                                then_instructions: vec![
                                    get(&sb),
                                    cast(&rope.leaf),
                                    field_get(&rope.leaf, &rope.payload_field),
                                    set(&p),
                                ],
                                else_instructions: vec![
                                    get(&sb),
                                    cast(&rope.node),
                                    field_get(&rope.node, &rope.cache_field),
                                    set(&p),
                                ],
                            },
                        ],
                        else_instructions: vec![
                            get(&r),
                            Instr::Call {
                                func_name: force_func,
                            },
                            set(&p),
                        ],
                    },
                ],
            },
            get(&p),
            get(&j),
            get_elem,
        ];

        self.add_helper(
            func_name,
            vec![(r, concrete_val(rope.base.clone(), false)), (i, i32_val)],
            result,
            locals,
            instrs,
        );
    }

    /// Read one logical bit from a packed rope. Leaves and settled windows read
    /// their packed payload directly. An uncached node is forced once; later
    /// reads take its packed cache without walking the tree.
    pub(super) fn emit_bits_read_func(&mut self, func_name: FuncName, force_func: FuncName) {
        let rope = self.table.bin_rope();
        let r = LocalName::from("r");
        let i = LocalName::from("i");
        let p = LocalName::from("p");
        let j = LocalName::from("j");
        let sb = LocalName::from("sb");
        let i32_val = ValType::Num(NumType::I32);

        let instrs = vec![
            get(&i),
            get(&r),
            field_get(&rope.base, &rope.len_field),
            Instr::I32GeU,
            Instr::If {
                label_name: LabelName::from("bounds"),
                block_type: BlockType::Empty,
                then_instructions: vec![Instr::Unreachable],
                else_instructions: vec![],
            },
            get(&i),
            set(&j),
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
                    set(&p),
                ],
                else_instructions: vec![
                    get(&r),
                    field_get(&rope.base, &rope.tag_field),
                    Instr::I32Const { value: 2 },
                    Instr::I32Eq,
                    Instr::If {
                        label_name: LabelName::from("view"),
                        block_type: BlockType::Empty,
                        then_instructions: vec![
                            get(&r),
                            cast(&rope.view),
                            field_get(&rope.view, &rope.offset_field),
                            get(&i),
                            Instr::I32Add,
                            set(&j),
                            get(&r),
                            cast(&rope.view),
                            field_get(&rope.view, &rope.base_field),
                            set(&sb),
                            get(&sb),
                            field_get(&rope.base, &rope.tag_field),
                            Instr::I32Eqz,
                            Instr::If {
                                label_name: LabelName::from("view_base"),
                                block_type: BlockType::Empty,
                                then_instructions: vec![
                                    get(&sb),
                                    cast(&rope.leaf),
                                    field_get(&rope.leaf, &rope.payload_field),
                                    set(&p),
                                ],
                                else_instructions: vec![
                                    get(&sb),
                                    cast(&rope.node),
                                    field_get(&rope.node, &rope.cache_field),
                                    set(&p),
                                ],
                            },
                        ],
                        else_instructions: vec![
                            get(&r),
                            cast(&rope.node),
                            field_get(&rope.node, &rope.cache_field),
                            set(&p),
                            get(&p),
                            Instr::RefIsNull,
                            Instr::If {
                                label_name: LabelName::from("settle"),
                                block_type: BlockType::Empty,
                                then_instructions: vec![
                                    get(&r),
                                    Instr::Call {
                                        func_name: force_func,
                                    },
                                    set(&p),
                                ],
                                else_instructions: vec![],
                            },
                        ],
                    },
                ],
            },
            get(&p),
            get(&j),
            Instr::I32Const { value: 3 },
            Instr::I32ShrU,
            Instr::ArrayGetU {
                type_name: rope.payload.clone(),
            },
            get(&j),
            Instr::I32Const { value: 7 },
            Instr::I32And,
            Instr::I32ShrU,
            Instr::I32Const { value: 1 },
            Instr::I32And,
        ];

        self.add_helper(
            func_name,
            vec![(r, concrete_val(rope.base.clone(), false)), (i, i32_val)],
            ValType::Num(NumType::I32),
            vec![
                (p, concrete_val(rope.payload, true)),
                (j, ValType::Num(NumType::I32)),
                (sb, concrete_val(rope.base, true)),
            ],
            instrs,
        );
    }

    /// `$lst/bin/force (ref $rope/lst) -> (ref $elems)`: force the outer rope,
    /// then force every element through `$bytes/force` into a *fresh* payload (the
    /// shallow force of a leaf answers its live payload, which must not be
    /// element-rewritten in place).
    pub(super) fn emit_lst_bin_force_func(&mut self, func_name: FuncName) {
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
                func_name: self.table.lst_force_func(),
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
                            func_name: self.table.bytes_force_func(),
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
            vec![(r, concrete_val(self.table.lst_rope_type(), false))],
            concrete_val(elems, false),
            locals,
            instrs,
        );
    }

    /// `$bytes/eql (ref $rope/bin, ref $rope/bin) -> i32`.
    ///
    /// ```wat
    /// if l.len != r.len → 0            ;; rope lengths answer without forcing
    /// lb := force(l); rb := force(r)
    /// loop:
    ///   if i ≥ lb.len → 1              ;; every byte matched
    ///   if lb[i] != rb[i] → 0
    ///   i += 1
    /// ```
    pub(super) fn emit_eql_func(
        &mut self,
        rope: &RopeData,
        func_name: FuncName,
        force_func: FuncName,
    ) {
        let l = LocalName::from("l");
        let r = LocalName::from("r");
        let lb = LocalName::from("lb");
        let rb = LocalName::from("rb");
        let i = LocalName::from("i");
        let eq = LocalName::from("eq");

        let i32_val = ValType::Num(NumType::I32);
        let locals = vec![
            (lb.clone(), concrete_val(rope.payload.clone(), true)),
            (rb.clone(), concrete_val(rope.payload.clone(), true)),
            (i.clone(), i32_val.clone()),
            (eq.clone(), i32_val.clone()),
        ];

        let done = LabelName::from("done");
        let bytes = LabelName::from("bytes");

        let loop_instrs = vec![
            // Every byte matched: eq = 1, exit.
            get(&i),
            get(&lb),
            Instr::ArrayLen,
            Instr::I32GeU,
            Instr::If {
                label_name: LabelName::from("hit"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    Instr::I32Const { value: 1 },
                    set(&eq),
                    Instr::Br {
                        label_name: done.clone(),
                    },
                ],
                else_instructions: vec![],
            },
            // Mismatch: exit with eq still 0.
            get(&lb),
            get(&i),
            Instr::ArrayGetU {
                type_name: rope.payload.clone(),
            },
            get(&rb),
            get(&i),
            Instr::ArrayGetU {
                type_name: rope.payload.clone(),
            },
            Instr::I32Ne,
            Instr::BrIf {
                label_name: done.clone(),
            },
            get(&i),
            Instr::I32Const { value: 1 },
            Instr::I32Add,
            set(&i),
            Instr::Br {
                label_name: bytes.clone(),
            },
        ];

        let instrs = vec![
            Instr::Block {
                label_name: done.clone(),
                block_type: BlockType::Empty,
                instructions: vec![
                    get(&l),
                    field_get(&rope.base, &rope.len_field),
                    get(&r),
                    field_get(&rope.base, &rope.len_field),
                    Instr::I32Ne,
                    Instr::BrIf { label_name: done },
                    get(&l),
                    Instr::Call {
                        func_name: force_func.clone(),
                    },
                    set(&lb),
                    get(&r),
                    Instr::Call {
                        func_name: force_func,
                    },
                    set(&rb),
                    Instr::Loop {
                        label_name: bytes,
                        block_type: BlockType::Empty,
                        instructions: loop_instrs,
                    },
                ],
            },
            get(&eq),
        ];

        self.add_helper(
            func_name,
            vec![
                (l, concrete_val(rope.base.clone(), false)),
                (r, concrete_val(rope.base.clone(), false)),
            ],
            i32_val,
            locals,
            instrs,
        );
    }

    /// Logical equality for packed bits. The loop is bounded by the rope's
    /// bit length, so unused high padding in the final payload byte is never
    /// observed.
    pub(super) fn emit_bits_eql_func(&mut self, func_name: FuncName, read_func: FuncName) {
        let rope = self.table.bin_rope();
        let l = LocalName::from("l");
        let r = LocalName::from("r");
        let i = LocalName::from("i");
        let eq = LocalName::from("eq");
        let done = LabelName::from("done");
        let bits = LabelName::from("bits");
        let i32_val = ValType::Num(NumType::I32);

        let loop_instrs = vec![
            get(&i),
            get(&l),
            field_get(&rope.base, &rope.len_field),
            Instr::I32GeU,
            Instr::If {
                label_name: LabelName::from("hit"),
                block_type: BlockType::Empty,
                then_instructions: vec![
                    Instr::I32Const { value: 1 },
                    set(&eq),
                    Instr::Br {
                        label_name: done.clone(),
                    },
                ],
                else_instructions: vec![],
            },
            get(&l),
            get(&i),
            Instr::Call {
                func_name: read_func.clone(),
            },
            get(&r),
            get(&i),
            Instr::Call {
                func_name: read_func,
            },
            Instr::I32Ne,
            Instr::BrIf {
                label_name: done.clone(),
            },
            get(&i),
            Instr::I32Const { value: 1 },
            Instr::I32Add,
            set(&i),
            Instr::Br {
                label_name: bits.clone(),
            },
        ];

        let instrs = vec![
            Instr::Block {
                label_name: done.clone(),
                block_type: BlockType::Empty,
                instructions: vec![
                    get(&l),
                    field_get(&rope.base, &rope.len_field),
                    get(&r),
                    field_get(&rope.base, &rope.len_field),
                    Instr::I32Ne,
                    Instr::BrIf { label_name: done },
                    Instr::Loop {
                        label_name: bits,
                        block_type: BlockType::Empty,
                        instructions: loop_instrs,
                    },
                ],
            },
            get(&eq),
        ];

        self.add_helper(
            func_name,
            vec![
                (l, concrete_val(rope.base.clone(), false)),
                (r, concrete_val(rope.base, false)),
            ],
            i32_val.clone(),
            vec![(i, i32_val.clone()), (eq, i32_val)],
            instrs,
        );
    }

    /// `$lst/map (ref $rope/lst, ref $envr/1) -> (ref $rope/lst)`.
    ///
    /// ```wat
    /// selems := force(src); count := selems.len
    /// out    := array.new_default <payload> count
    /// loop: while i < count, out[i] := f(selems[i]), i += 1
    /// leaf { tag 0, count, out }
    /// ```
    ///
    /// `f` is a unary closure `(A) -> B`, called by the arity-1 convention:
    /// the environment as the self argument, the funcref from its special
    /// field.
    pub(super) fn emit_map_func(&mut self, func_name: FuncName, force_func: FuncName) {
        let rope = self.table.lst_rope();
        let envr_type = self.table.find_envr_type(1);
        let clsr_type = self.table.find_clsr_type(1);
        let special_field = self.table.special_field();

        let src = LocalName::from("src");
        let f = LocalName::from("f");
        let selems = LocalName::from("selems");
        let out = LocalName::from("out");
        let count = LocalName::from("count");
        let i = LocalName::from("i");

        let i32_val = ValType::Num(NumType::I32);
        let locals = vec![
            (selems.clone(), concrete_val(rope.payload.clone(), true)),
            (out.clone(), concrete_val(rope.payload.clone(), true)),
            (count.clone(), i32_val.clone()),
            (i.clone(), i32_val),
        ];

        let step = LabelName::from("step");
        let slots = LabelName::from("slots");

        // out[i] = f(selems[i]); i += 1
        let step_instrs = vec![
            get(&out),
            get(&i),
            get(&f),
            get(&selems),
            get(&i),
            Instr::ArrayGet {
                type_name: rope.payload.clone(),
            },
            Instr::RefAsNonNull,
            get(&f),
            field_get(&envr_type, &special_field),
            Instr::RefAsNonNull,
            Instr::CallRef {
                type_name: clsr_type,
            },
            Instr::ArraySet {
                type_name: rope.payload.clone(),
            },
            get(&i),
            Instr::I32Const { value: 1 },
            Instr::I32Add,
            set(&i),
            Instr::Br {
                label_name: slots.clone(),
            },
        ];

        let instrs = vec![
            get(&src),
            Instr::Call {
                func_name: force_func,
            },
            set(&selems),
            get(&selems),
            Instr::ArrayLen,
            set(&count),
            get(&count),
            Instr::ArrayNewDefault {
                type_name: rope.payload.clone(),
            },
            set(&out),
            Instr::Loop {
                label_name: slots,
                block_type: BlockType::Empty,
                instructions: vec![
                    get(&i),
                    get(&count),
                    Instr::I32LtU,
                    Instr::If {
                        label_name: step,
                        block_type: BlockType::Empty,
                        then_instructions: step_instrs,
                        else_instructions: vec![],
                    },
                ],
            },
            // Seal the filled payload into a fresh leaf.
            Instr::I32Const { value: 0 },
            get(&count),
            get(&out),
            Instr::RefAsNonNull,
            Instr::StructNew {
                type_name: rope.leaf.clone(),
            },
        ];

        self.add_helper(
            func_name,
            vec![
                (src, concrete_val(rope.base.clone(), false)),
                (f, concrete_val(envr_type, false)),
            ],
            concrete_val(rope.base, false),
            locals,
            instrs,
        );
    }

    /// `$<carrier>/embed (ref <payload>) -> (ref <base>)`: one fresh leaf.
    pub(super) fn emit_embed_func(&mut self, rope: &RopeData, func_name: FuncName) {
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
            vec![(b, concrete_val(rope.payload.clone(), false))],
            concrete_val(rope.base.clone(), false),
            vec![],
            instrs,
        );
    }

    /// `$lst/bin/embed (ref $elems) -> (ref $rope/lst)`: embed each raw `$bytes`
    /// element into a `$rope/bin/leaf` in place — the host-built array is fresh,
    /// nothing else aliases it — then embed the outer array into a `$rope/lst/leaf`.
    pub(super) fn emit_lst_bin_embed_func(&mut self, func_name: FuncName) {
        let elems = self.table.elems_type();
        let bin = self.table.bin_rope();
        let lst = self.table.lst_rope();

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
                type_name: lst.leaf.clone(),
            },
        ];

        self.add_helper(
            func_name,
            vec![(e, concrete_val(elems, false))],
            concrete_val(lst.base.clone(), false),
            locals,
            instrs,
        );
    }
}
