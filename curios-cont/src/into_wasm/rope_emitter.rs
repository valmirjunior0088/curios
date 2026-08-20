//! The shared rope helper functions — the only module-level functions the emitter mints beyond the program's own (everything else is inlined at its use site — straight-line sequences only; anything the emitter lowers to a *loop* lives here, so its mutable scratch locals are zeroed by the fresh activation instead of leaking across executions of one call site). Seven families:
//!
//! - `$<carrier>/force` flattens a byte- or element-grain rope to its payload array: the leaf answers its payload, a cached node answers its cache, and everything else fills a fresh payload by an *iterative* tree walk (an explicit `$elems` worklist, grown by doubling), so a 100k-deep concat chain never touches the wasm call stack. Only an entry *node* memoizes — intermediates are usually garbage the moment the walk passes them, and a view's fill is a single window copy of exactly its own size.
//! - `$<carrier>/embed` places a host-built flat payload into a fresh leaf on re-entry.
//! - `$<carrier>/slice` builds the O(1) window: bounds-check, the trivial windows answer an empty leaf or the rope itself, a view collapses (so windows never stack), and an uncached node base is forced first — which memoizes — so every `view` in existence has a *flat-available* base (a leaf or a cached node).
//! - `$<carrier>/read` answers one element: off a leaf payload, *through* a view's window without forcing (the invariant above makes that O(1)), or via `force` on a node.
//! - `$bits/force` performs the same iterative walk in logical bit units, filling a zeroed packed payload and memoizing it on an entry node.
//! - `$bytes/eql` compares two `Bytes` ropes bytewise: unequal lengths answer without forcing, equal lengths force both payloads once and walk them.
//! - `$list/map` applies a unary closure to every element of the forced payload, filling a fresh leaf.
//!
//! The `list/bytes` variants are the host boundary's deep forms: a `List(Bytes)` / `List(Handle)` wire value carries `Bytes`-shaped *elements*, which the host lifts and lowers as raw `$bytes` — so params force each element too, and results embed each element back.

use {
    super::{RopeData, Table},
    curios_utilities::Grain,
};

fn concrete_ref(type_name: curios_wasm::TypeName, is_nullable: bool) -> curios_wasm::RefType {
    curios_wasm::RefType {
        is_nullable,
        heap_type: curios_wasm::HeapType::Concrete(type_name),
    }
}

fn concrete_val(type_name: curios_wasm::TypeName, is_nullable: bool) -> curios_wasm::ValType {
    curios_wasm::ValType::Ref(concrete_ref(type_name, is_nullable))
}

fn get(local: &curios_wasm::LocalName) -> curios_wasm::Instr {
    curios_wasm::Instr::LocalGet {
        local_name: local.clone(),
    }
}

fn set(local: &curios_wasm::LocalName) -> curios_wasm::Instr {
    curios_wasm::Instr::LocalSet {
        local_name: local.clone(),
    }
}

fn cast(type_name: &curios_wasm::TypeName) -> curios_wasm::Instr {
    curios_wasm::Instr::RefCast {
        ref_type: concrete_ref(type_name.clone(), false),
    }
}

fn field_get(
    type_name: &curios_wasm::TypeName,
    field_name: &curios_wasm::FieldName,
) -> curios_wasm::Instr {
    curios_wasm::Instr::StructGet {
        type_name: type_name.clone(),
        field_name: field_name.clone(),
    }
}

fn field_set(
    type_name: &curios_wasm::TypeName,
    field_name: &curios_wasm::FieldName,
) -> curios_wasm::Instr {
    curios_wasm::Instr::StructSet {
        type_name: type_name.clone(),
        field_name: field_name.clone(),
    }
}

/// One packed grain's immediate layout: where the length lives, what masks the payload, how many payload bytes can be occupied, and how many length units one byte holds.
fn immediate_layout(grain: Grain) -> (i32, i32, i32, i32) {
    match grain {
        Grain::X => (29, 0x00FF_FFFF, 3, 1),
        Grain::B => (26, 0x03FF_FFFF, 4, 8),
    }
}

fn null(type_name: &curios_wasm::TypeName) -> curios_wasm::Instr {
    curios_wasm::Instr::RefNull {
        heap_type: curios_wasm::HeapType::Concrete(type_name.clone()),
    }
}

/// The walk machinery shared verbatim by [`RopeEmitter::emit_force_func`] and [`RopeEmitter::emit_bits_force_func`]: one scratch-local roster plus the invariant-bearing instruction blocks — entry shortcuts, view resolution, worklist growth and descent, pop, memoization — so the two grains cannot silently diverge on the discipline. Each emitter inlines only what genuinely differs: destination sizing, the chunk-count source, and the copy body.
struct ForceWalk<'r> {
    rope: &'r RopeData,
    elems: curios_wasm::TypeName,
    r: curios_wasm::LocalName,
    node: curios_wasm::LocalName,
    out: curios_wasm::LocalName,
    stack: curios_wasm::LocalName,
    grown: curios_wasm::LocalName,
    sp: curios_wasm::LocalName,
    offset: curios_wasm::LocalName,
    cur: curios_wasm::LocalName,
    payload: curios_wasm::LocalName,
    src_off: curios_wasm::LocalName,
    count: curios_wasm::LocalName,
    sb: curios_wasm::LocalName,
}

impl<'r> ForceWalk<'r> {
    fn new(rope: &'r RopeData, elems: curios_wasm::TypeName) -> Self {
        Self {
            rope,
            elems,
            r: curios_wasm::LocalName::from("r"),
            node: curios_wasm::LocalName::from("node"),
            out: curios_wasm::LocalName::from("out"),
            stack: curios_wasm::LocalName::from("stack"),
            grown: curios_wasm::LocalName::from("grown"),
            sp: curios_wasm::LocalName::from("sp"),
            offset: curios_wasm::LocalName::from("offset"),
            cur: curios_wasm::LocalName::from("cur"),
            payload: curios_wasm::LocalName::from("payload"),
            src_off: curios_wasm::LocalName::from("src_off"),
            count: curios_wasm::LocalName::from("count"),
            sb: curios_wasm::LocalName::from("sb"),
        }
    }

    /// The shared scratch-local roster; the bit-grain emitter appends its copy-loop extras.
    fn locals(&self) -> Vec<(curios_wasm::LocalName, curios_wasm::ValType)> {
        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        vec![
            (
                self.node.clone(),
                concrete_val(self.rope.node.clone(), true),
            ),
            (
                self.out.clone(),
                concrete_val(self.rope.payload.clone(), true),
            ),
            (self.stack.clone(), concrete_val(self.elems.clone(), true)),
            (self.grown.clone(), concrete_val(self.elems.clone(), true)),
            (self.sp.clone(), i32_val.clone()),
            (self.offset.clone(), i32_val.clone()),
            (self.cur.clone(), concrete_val(self.rope.base.clone(), true)),
            (
                self.payload.clone(),
                concrete_val(self.rope.payload.clone(), true),
            ),
            (self.src_off.clone(), i32_val.clone()),
            (self.count.clone(), i32_val),
            (self.sb.clone(), concrete_val(self.rope.base.clone(), true)),
        ]
    }

    /// The flat entries answer without walking: a leaf its payload, a cached node its memo (setting `node` on the way for the memoize epilogue). A view skips straight to the walk.
    fn entry_shortcuts(&self) -> Vec<curios_wasm::Instr> {
        vec![
            get(&self.r),
            field_get(&self.rope.base, &self.rope.tag_field),
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("leaf"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&self.r),
                    cast(&self.rope.leaf),
                    field_get(&self.rope.leaf, &self.rope.payload_field),
                    curios_wasm::Instr::Return,
                ],
                else_instructions: vec![],
            },
            get(&self.r),
            field_get(&self.rope.base, &self.rope.tag_field),
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Eq,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("entry_node"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&self.r),
                    cast(&self.rope.node),
                    set(&self.node),
                    get(&self.node),
                    field_get(&self.rope.node, &self.rope.cache_field),
                    curios_wasm::Instr::RefIsNull,
                    curios_wasm::Instr::I32Eqz,
                    curios_wasm::Instr::If {
                        label_name: curios_wasm::LabelName::from("cached"),
                        block_type: curios_wasm::BlockType::Empty,
                        then_instructions: vec![
                            get(&self.node),
                            field_get(&self.rope.node, &self.rope.cache_field),
                            curios_wasm::Instr::RefAsNonNull,
                            curios_wasm::Instr::Return,
                        ],
                        else_instructions: vec![],
                    },
                ],
                else_instructions: vec![],
            },
        ]
    }

    /// A fresh 32-slot worklist, the walk starting at the entry.
    fn init_worklist(&self) -> Vec<curios_wasm::Instr> {
        vec![
            curios_wasm::Instr::I32Const { value: 32 },
            curios_wasm::Instr::ArrayNewDefault {
                type_name: self.elems.clone(),
            },
            set(&self.stack),
            get(&self.r),
            set(&self.cur),
        ]
    }

    /// The view descent arm: its window over the base's flat payload — a leaf's payload or a cached node's cache (non-null by the slice invariant; the null trap at the copy is its enforcement).
    fn resolve_view_chunk(&self) -> Vec<curios_wasm::Instr> {
        vec![
            get(&self.cur),
            field_get(&self.rope.base, &self.rope.tag_field),
            curios_wasm::Instr::I32Const { value: 2 },
            curios_wasm::Instr::I32Eq,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("at_view"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&self.cur),
                    cast(&self.rope.view),
                    field_get(&self.rope.view, &self.rope.base_field),
                    set(&self.sb),
                    get(&self.cur),
                    cast(&self.rope.view),
                    field_get(&self.rope.view, &self.rope.offset_field),
                    set(&self.src_off),
                    get(&self.cur),
                    field_get(&self.rope.base, &self.rope.len_field),
                    set(&self.count),
                    get(&self.sb),
                    field_get(&self.rope.base, &self.rope.tag_field),
                    curios_wasm::Instr::I32Eqz,
                    curios_wasm::Instr::If {
                        label_name: curios_wasm::LabelName::from("view_base"),
                        block_type: curios_wasm::BlockType::Empty,
                        then_instructions: vec![
                            get(&self.sb),
                            cast(&self.rope.leaf),
                            field_get(&self.rope.leaf, &self.rope.payload_field),
                            set(&self.payload),
                        ],
                        else_instructions: vec![
                            get(&self.sb),
                            cast(&self.rope.node),
                            field_get(&self.rope.node, &self.rope.cache_field),
                            set(&self.payload),
                        ],
                    },
                    curios_wasm::Instr::Br {
                        label_name: curios_wasm::LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
        ]
    }

    /// The uncached-node descent tail: grow the worklist by doubling when full, push `cur.right`, descend into `cur.left`.
    fn push_uncached_node(
        &self,
        descend_label: &curios_wasm::LabelName,
    ) -> Vec<curios_wasm::Instr> {
        vec![
            get(&self.sp),
            get(&self.stack),
            curios_wasm::Instr::ArrayLen,
            curios_wasm::Instr::I32Eq,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("grow"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&self.sp),
                    curios_wasm::Instr::I32Const { value: 1 },
                    curios_wasm::Instr::I32Shl,
                    curios_wasm::Instr::ArrayNewDefault {
                        type_name: self.elems.clone(),
                    },
                    set(&self.grown),
                    get(&self.grown),
                    curios_wasm::Instr::I32Const { value: 0 },
                    get(&self.stack),
                    curios_wasm::Instr::I32Const { value: 0 },
                    get(&self.sp),
                    curios_wasm::Instr::ArrayCopy {
                        target_name: self.elems.clone(),
                        source_name: self.elems.clone(),
                    },
                    get(&self.grown),
                    set(&self.stack),
                ],
                else_instructions: vec![],
            },
            get(&self.stack),
            get(&self.sp),
            get(&self.cur),
            cast(&self.rope.node),
            field_get(&self.rope.node, &self.rope.right_field),
            curios_wasm::Instr::ArraySet {
                type_name: self.elems.clone(),
            },
            get(&self.sp),
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Add,
            set(&self.sp),
            get(&self.cur),
            cast(&self.rope.node),
            field_get(&self.rope.node, &self.rope.left_field),
            set(&self.cur),
            curios_wasm::Instr::Br {
                label_name: descend_label.clone(),
            },
        ]
    }

    /// Pop the next pending subtree; fall out of the walk when the worklist is empty.
    fn pop_or_exit(&self, walk_label: &curios_wasm::LabelName) -> Vec<curios_wasm::Instr> {
        vec![
            get(&self.sp),
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("pop"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&self.sp),
                    curios_wasm::Instr::I32Const { value: 1 },
                    curios_wasm::Instr::I32Sub,
                    set(&self.sp),
                    get(&self.stack),
                    get(&self.sp),
                    curios_wasm::Instr::ArrayGet {
                        type_name: self.elems.clone(),
                    },
                    cast(&self.rope.base),
                    set(&self.cur),
                    curios_wasm::Instr::Br {
                        label_name: walk_label.clone(),
                    },
                ],
                else_instructions: vec![],
            },
        ]
    }

    /// Memoize a node entry and release its tree (`node` was set at entry exactly when the tag is 1); a view entry has nowhere to memoize. Answers the filled payload.
    fn memoize_entry_node(&self) -> Vec<curios_wasm::Instr> {
        vec![
            get(&self.r),
            field_get(&self.rope.base, &self.rope.tag_field),
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Eq,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("memoize"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&self.node),
                    get(&self.out),
                    field_set(&self.rope.node, &self.rope.cache_field),
                    get(&self.node),
                    null(&self.rope.base),
                    field_set(&self.rope.node, &self.rope.left_field),
                    get(&self.node),
                    null(&self.rope.base),
                    field_set(&self.rope.node, &self.rope.right_field),
                ],
                else_instructions: vec![],
            },
            get(&self.out),
            curios_wasm::Instr::RefAsNonNull,
        ]
    }
}

#[derive(Debug)]
pub(crate) struct RopeEmitter<'a, 'b> {
    table: &'a Table<'a>,
    module: &'b mut curios_wasm::Module,
}

impl<'a, 'b> RopeEmitter<'a, 'b> {
    pub(crate) fn new(table: &'a Table<'a>, module: &'b mut curios_wasm::Module) -> Self {
        Self { table, module }
    }

    /// Declare one helper: a final func type named after the function, plus the function itself. Helpers are called by name, never `ref.func`'d or exported, so no declaration beyond the pair is needed.
    fn add_helper(
        &mut self,
        func_name: curios_wasm::FuncName,
        params: Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
        result: curios_wasm::ValType,
        locals: Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
        instrs: Vec<curios_wasm::Instr>,
    ) {
        let type_name = curios_wasm::TypeName::from(func_name.as_str());

        self.module.add_type(
            type_name.clone(),
            curios_wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: curios_wasm::CompType::Func(curios_wasm::FuncType {
                    inputs: curios_wasm::ResultType::from(
                        params.iter().map(|(_, val_type)| val_type.clone()),
                    ),
                    outputs: curios_wasm::ResultType::from([result]),
                }),
            },
        );

        self.module.add_func(
            func_name,
            curios_wasm::Func {
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
    /// An entry *view* is not memoized (its fields are immutable): its fill is one window copy of exactly its own size, so there is nothing quadratic to fence off.
    pub(crate) fn emit_force_func(&mut self, rope: &RopeData, func_name: curios_wasm::FuncName) {
        let walk = ForceWalk::new(rope, self.table.elems_type());

        let mut instrs = walk.entry_shortcuts();

        // out = array.new_default <payload> r.len, then the worklist.
        instrs.extend([
            get(&walk.r),
            field_get(&rope.base, &rope.len_field),
            curios_wasm::Instr::ArrayNewDefault {
                type_name: rope.payload.clone(),
            },
            set(&walk.out),
        ]);
        instrs.extend(walk.init_worklist());

        // The descent body: classify `cur`, either resolving a leaf-like chunk — payload + source window — (exit to `$emit`) or pushing right and descending left.
        let descend_label = curios_wasm::LabelName::from("descend");
        let mut descend = vec![
            // Leaf: the whole payload.
            get(&walk.cur),
            field_get(&rope.base, &rope.tag_field),
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("at_leaf"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&walk.cur),
                    cast(&rope.leaf),
                    field_get(&rope.leaf, &rope.payload_field),
                    set(&walk.payload),
                    curios_wasm::Instr::I32Const { value: 0 },
                    set(&walk.src_off),
                    get(&walk.payload),
                    curios_wasm::Instr::ArrayLen,
                    set(&walk.count),
                    curios_wasm::Instr::Br {
                        label_name: curios_wasm::LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
        ];
        descend.extend(walk.resolve_view_chunk());
        // Cached node: the whole cache.
        descend.extend([
            get(&walk.cur),
            cast(&rope.node),
            field_get(&rope.node, &rope.cache_field),
            curios_wasm::Instr::RefIsNull,
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("at_cached"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&walk.cur),
                    cast(&rope.node),
                    field_get(&rope.node, &rope.cache_field),
                    set(&walk.payload),
                    curios_wasm::Instr::I32Const { value: 0 },
                    set(&walk.src_off),
                    get(&walk.payload),
                    curios_wasm::Instr::ArrayLen,
                    set(&walk.count),
                    curios_wasm::Instr::Br {
                        label_name: curios_wasm::LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
        ]);
        descend.extend(walk.push_uncached_node(&descend_label));

        // The walk: descend to a chunk, copy it at the running offset, pop.
        let walk_label = curios_wasm::LabelName::from("walk");
        let mut walk_body = vec![
            curios_wasm::Instr::Block {
                label_name: curios_wasm::LabelName::from("emit"),
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![curios_wasm::Instr::Loop {
                    label_name: descend_label,
                    block_type: curios_wasm::BlockType::Empty,
                    instructions: descend,
                }],
            },
            // array.copy out[offset..] <- payload[src_off..src_off+count]
            get(&walk.out),
            get(&walk.offset),
            get(&walk.payload),
            get(&walk.src_off),
            get(&walk.count),
            curios_wasm::Instr::ArrayCopy {
                target_name: rope.payload.clone(),
                source_name: rope.payload.clone(),
            },
            get(&walk.offset),
            get(&walk.count),
            curios_wasm::Instr::I32Add,
            set(&walk.offset),
        ];
        walk_body.extend(walk.pop_or_exit(&walk_label));
        instrs.push(curios_wasm::Instr::Loop {
            label_name: walk_label,
            block_type: curios_wasm::BlockType::Empty,
            instructions: walk_body,
        });

        instrs.extend(walk.memoize_entry_node());

        self.add_helper(
            func_name,
            vec![(walk.r.clone(), concrete_val(rope.base.clone(), false))],
            concrete_val(rope.payload.clone(), false),
            walk.locals(),
            instrs,
        );
    }

    /// `$bits/force (ref $rope/bin) -> (ref $bytes)`: flatten a bit-grain rope into a packed LSB-first payload. The tree walk is iterative, like [`Self::emit_force_func`], but chunk windows and offsets are measured in bits and the destination has `ceil(len / 8)` zeroed bytes. Copying only logical bits keeps the unused high padding of the final byte zero.
    pub(crate) fn emit_bits_force_func(&mut self, func_name: curios_wasm::FuncName) {
        let rope = self.table.bin_rope();
        let walk = ForceWalk::new(&rope, self.table.elems_type());

        let copy_i = curios_wasm::LocalName::from("copy_i");
        let src_bit = curios_wasm::LocalName::from("src_bit");
        let dst_bit = curios_wasm::LocalName::from("dst_bit");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let mut locals = walk.locals();
        locals.extend([
            (copy_i.clone(), i32_val.clone()),
            (src_bit.clone(), i32_val.clone()),
            (dst_bit.clone(), i32_val),
        ]);

        let mut instrs = walk.entry_shortcuts();

        // ceil(r.len / 8) zeroed destination bytes, then the worklist.
        instrs.extend([
            get(&walk.r),
            field_get(&rope.base, &rope.len_field),
            curios_wasm::Instr::I32Const { value: 7 },
            curios_wasm::Instr::I32Add,
            curios_wasm::Instr::I32Const { value: 3 },
            curios_wasm::Instr::I32ShrU,
            curios_wasm::Instr::ArrayNewDefault {
                type_name: rope.payload.clone(),
            },
            set(&walk.out),
        ]);
        instrs.extend(walk.init_worklist());

        let descend_label = curios_wasm::LabelName::from("descend");
        let mut descend = vec![
            // Leaf: copy exactly its logical bit length, not its padding.
            get(&walk.cur),
            field_get(&rope.base, &rope.tag_field),
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("at_leaf"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&walk.cur),
                    cast(&rope.leaf),
                    field_get(&rope.leaf, &rope.payload_field),
                    set(&walk.payload),
                    curios_wasm::Instr::I32Const { value: 0 },
                    set(&walk.src_off),
                    get(&walk.cur),
                    field_get(&rope.base, &rope.len_field),
                    set(&walk.count),
                    curios_wasm::Instr::Br {
                        label_name: curios_wasm::LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
        ];
        descend.extend(walk.resolve_view_chunk());
        // A cached node contributes its logical length from bit zero.
        descend.extend([
            get(&walk.cur),
            cast(&rope.node),
            field_get(&rope.node, &rope.cache_field),
            curios_wasm::Instr::RefIsNull,
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("at_cached"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&walk.cur),
                    cast(&rope.node),
                    field_get(&rope.node, &rope.cache_field),
                    set(&walk.payload),
                    curios_wasm::Instr::I32Const { value: 0 },
                    set(&walk.src_off),
                    get(&walk.cur),
                    field_get(&rope.base, &rope.len_field),
                    set(&walk.count),
                    curios_wasm::Instr::Br {
                        label_name: curios_wasm::LabelName::from("emit"),
                    },
                ],
                else_instructions: vec![],
            },
        ]);
        descend.extend(walk.push_uncached_node(&descend_label));

        let walk_label = curios_wasm::LabelName::from("walk");
        let copy_label = curios_wasm::LabelName::from("copy");
        let copy_done = curios_wasm::LabelName::from("copy_done");
        let mut walk_body = vec![
            curios_wasm::Instr::Block {
                label_name: curios_wasm::LabelName::from("emit"),
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![curios_wasm::Instr::Loop {
                    label_name: descend_label,
                    block_type: curios_wasm::BlockType::Empty,
                    instructions: descend,
                }],
            },
            // Scratch locals are reused per chunk, so reset the cursor.
            curios_wasm::Instr::I32Const { value: 0 },
            set(&copy_i),
            curios_wasm::Instr::Block {
                label_name: copy_done.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![curios_wasm::Instr::Loop {
                    label_name: copy_label.clone(),
                    block_type: curios_wasm::BlockType::Empty,
                    instructions: vec![
                        get(&copy_i),
                        get(&walk.count),
                        curios_wasm::Instr::I32GeU,
                        curios_wasm::Instr::BrIf {
                            label_name: copy_done,
                        },
                        get(&walk.src_off),
                        get(&copy_i),
                        curios_wasm::Instr::I32Add,
                        set(&src_bit),
                        get(&walk.offset),
                        get(&copy_i),
                        curios_wasm::Instr::I32Add,
                        set(&dst_bit),
                        // out[dst/8] |= ((payload[src/8] >> src%8) & 1) << dst%8
                        get(&walk.out),
                        get(&dst_bit),
                        curios_wasm::Instr::I32Const { value: 3 },
                        curios_wasm::Instr::I32ShrU,
                        get(&walk.out),
                        get(&dst_bit),
                        curios_wasm::Instr::I32Const { value: 3 },
                        curios_wasm::Instr::I32ShrU,
                        curios_wasm::Instr::ArrayGetU {
                            type_name: rope.payload.clone(),
                        },
                        get(&walk.payload),
                        get(&src_bit),
                        curios_wasm::Instr::I32Const { value: 3 },
                        curios_wasm::Instr::I32ShrU,
                        curios_wasm::Instr::ArrayGetU {
                            type_name: rope.payload.clone(),
                        },
                        get(&src_bit),
                        curios_wasm::Instr::I32Const { value: 7 },
                        curios_wasm::Instr::I32And,
                        curios_wasm::Instr::I32ShrU,
                        curios_wasm::Instr::I32Const { value: 1 },
                        curios_wasm::Instr::I32And,
                        get(&dst_bit),
                        curios_wasm::Instr::I32Const { value: 7 },
                        curios_wasm::Instr::I32And,
                        curios_wasm::Instr::I32Shl,
                        curios_wasm::Instr::I32Or,
                        curios_wasm::Instr::ArraySet {
                            type_name: rope.payload.clone(),
                        },
                        get(&copy_i),
                        curios_wasm::Instr::I32Const { value: 1 },
                        curios_wasm::Instr::I32Add,
                        set(&copy_i),
                        curios_wasm::Instr::Br {
                            label_name: copy_label,
                        },
                    ],
                }],
            },
            get(&walk.offset),
            get(&walk.count),
            curios_wasm::Instr::I32Add,
            set(&walk.offset),
        ];
        walk_body.extend(walk.pop_or_exit(&walk_label));
        instrs.push(curios_wasm::Instr::Loop {
            label_name: walk_label,
            block_type: curios_wasm::BlockType::Empty,
            instructions: walk_body,
        });

        instrs.extend(walk.memoize_entry_node());

        self.add_helper(
            func_name,
            vec![(walk.r.clone(), concrete_val(rope.base.clone(), false))],
            concrete_val(rope.payload.clone(), false),
            locals,
            instrs,
        );
    }

    /// `$<carrier>/slice (ref <base>, i32, i32) -> (ref <base>)`, taking a start and a *count*.
    ///
    /// ```wat
    /// if s > r.len || n > r.len - s → unreachable      ;; the eager bounds trap
    /// if n == 0               → fresh empty leaf
    /// if s == 0 && n == r.len → r                      ;; whole-window alias
    /// if r.tag == 2           → view{2, n, r.base, r.offset + s}   ;; collapse
    /// if r.tag == 1 && r.cache == null → call force(r) (drop)     ;; memoizes
    /// view{2, n, r, s}
    /// ```
    ///
    /// The count arrives as an operand, so nothing here computes one — and a reversed window, which the `(start, end)` form had to reject, cannot be spelled.
    ///
    /// The node arm's force is what maintains the read-through invariant: every `view` base is flat-available from birth, and stays so (a cache is written once, never cleared).
    pub(crate) fn emit_slice_func(
        &mut self,
        rope: &RopeData,
        func_name: curios_wasm::FuncName,
        force_func: curios_wasm::FuncName,
    ) {
        let r = curios_wasm::LocalName::from("r");
        let s = curios_wasm::LocalName::from("s");
        let n = curios_wasm::LocalName::from("n");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = Vec::new();

        let mut instrs = Vec::new();

        // Bounds: the pre-window trap `slice` always had (an out-of-range window must not become a deferred — or never-taken — trap). A window is `(start, count)`, so the reversed range it also used to reject cannot be spelled, and only running past the end is left.
        //
        // Spelled `s > len || n > len - s` rather than `s + n > len` because the sum is i32 arithmetic and would wrap. The subtraction underflows when `s > len`, but the first test has already decided that case and both are evaluated before the `or`.
        instrs.extend([
            get(&s),
            get(&r),
            field_get(&rope.base, &rope.len_field),
            curios_wasm::Instr::I32GtU,
            get(&n),
            get(&r),
            field_get(&rope.base, &rope.len_field),
            get(&s),
            curios_wasm::Instr::I32Sub,
            curios_wasm::Instr::I32GtU,
            curios_wasm::Instr::I32Or,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("bounds"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![curios_wasm::Instr::Unreachable],
                else_instructions: vec![],
            },
        ]);

        // The empty window is a fresh empty leaf. The count arrives as an operand, so nothing computes it.
        instrs.extend([
            get(&n),
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("empty"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    curios_wasm::Instr::I32Const { value: 0 },
                    curios_wasm::Instr::I32Const { value: 0 },
                    curios_wasm::Instr::I32Const { value: 0 },
                    curios_wasm::Instr::ArrayNewDefault {
                        type_name: rope.payload.clone(),
                    },
                    curios_wasm::Instr::StructNew {
                        type_name: rope.leaf.clone(),
                    },
                    curios_wasm::Instr::Return,
                ],
                else_instructions: vec![],
            },
        ]);

        // The whole window is the rope itself.
        instrs.extend([
            get(&s),
            curios_wasm::Instr::I32Eqz,
            get(&n),
            get(&r),
            field_get(&rope.base, &rope.len_field),
            curios_wasm::Instr::I32Eq,
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("whole"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![get(&r), curios_wasm::Instr::Return],
                else_instructions: vec![],
            },
        ]);

        // A view collapses onto its own base, so windows never stack.
        instrs.extend([
            get(&r),
            field_get(&rope.base, &rope.tag_field),
            curios_wasm::Instr::I32Const { value: 2 },
            curios_wasm::Instr::I32Eq,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("collapse"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    curios_wasm::Instr::I32Const { value: 2 },
                    get(&n),
                    get(&r),
                    cast(&rope.view),
                    field_get(&rope.view, &rope.base_field),
                    get(&r),
                    cast(&rope.view),
                    field_get(&rope.view, &rope.offset_field),
                    get(&s),
                    curios_wasm::Instr::I32Add,
                    curios_wasm::Instr::StructNew {
                        type_name: rope.view.clone(),
                    },
                    curios_wasm::Instr::Return,
                ],
                else_instructions: vec![],
            },
        ]);

        // An uncached node is forced first — memoized in place — so the view below reads through its cache.
        instrs.extend([
            get(&r),
            field_get(&rope.base, &rope.tag_field),
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Eq,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("node"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&r),
                    cast(&rope.node),
                    field_get(&rope.node, &rope.cache_field),
                    curios_wasm::Instr::RefIsNull,
                    curios_wasm::Instr::If {
                        label_name: curios_wasm::LabelName::from("settle"),
                        block_type: curios_wasm::BlockType::Empty,
                        then_instructions: vec![
                            get(&r),
                            curios_wasm::Instr::Call {
                                func_name: force_func,
                            },
                            curios_wasm::Instr::Drop,
                        ],
                        else_instructions: vec![],
                    },
                ],
                else_instructions: vec![],
            },
        ]);

        instrs.extend([
            curios_wasm::Instr::I32Const { value: 2 },
            get(&n),
            get(&r),
            get(&s),
            curios_wasm::Instr::StructNew {
                type_name: rope.view.clone(),
            },
        ]);

        self.add_helper(
            func_name,
            vec![
                (r, concrete_val(rope.base.clone(), false)),
                (s, i32_val.clone()),
                (n, i32_val),
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
    /// (r.cache ?? force(r))[i]                        ;; node: answer the memo, fill it once
    /// ```
    ///
    /// The node arm probes the cache before reaching for `force`: a cache is written once and never cleared, so on every walk over an already-forced rope the probe halves the serial call chain a per-element read costs — which is most of what a hot descent pays, per the map-wall decomposition.
    ///
    /// Binary-sequence elements are packed bytes (`array.get_u`, an `i32` result); `List` elements are the top type (`array.get`).
    pub(crate) fn emit_read_func(
        &mut self,
        rope: &RopeData,
        func_name: curios_wasm::FuncName,
        force_func: curios_wasm::FuncName,
    ) {
        let packed = rope.payload == self.table.bytes_type();

        let r = curios_wasm::LocalName::from("r");
        let i = curios_wasm::LocalName::from("i");
        let p = curios_wasm::LocalName::from("p");
        let j = curios_wasm::LocalName::from("j");
        let sb = curios_wasm::LocalName::from("sb");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (p.clone(), concrete_val(rope.payload.clone(), true)),
            (j.clone(), i32_val.clone()),
            (sb.clone(), concrete_val(rope.base.clone(), true)),
        ];

        let get_elem = if packed {
            curios_wasm::Instr::ArrayGetU {
                type_name: rope.payload.clone(),
            }
        } else {
            curios_wasm::Instr::ArrayGet {
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
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("leaf"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&r),
                    cast(&rope.leaf),
                    field_get(&rope.leaf, &rope.payload_field),
                    set(&p),
                ],
                else_instructions: vec![
                    get(&r),
                    field_get(&rope.base, &rope.tag_field),
                    curios_wasm::Instr::I32Const { value: 2 },
                    curios_wasm::Instr::I32Eq,
                    curios_wasm::Instr::If {
                        label_name: curios_wasm::LabelName::from("view"),
                        block_type: curios_wasm::BlockType::Empty,
                        then_instructions: vec![
                            get(&r),
                            cast(&rope.view),
                            field_get(&rope.view, &rope.offset_field),
                            get(&i),
                            curios_wasm::Instr::I32Add,
                            set(&j),
                            get(&r),
                            cast(&rope.view),
                            field_get(&rope.view, &rope.base_field),
                            set(&sb),
                            get(&sb),
                            field_get(&rope.base, &rope.tag_field),
                            curios_wasm::Instr::I32Eqz,
                            curios_wasm::Instr::If {
                                label_name: curios_wasm::LabelName::from("view_base"),
                                block_type: curios_wasm::BlockType::Empty,
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
                            curios_wasm::Instr::RefIsNull,
                            curios_wasm::Instr::If {
                                label_name: curios_wasm::LabelName::from("settle"),
                                block_type: curios_wasm::BlockType::Empty,
                                then_instructions: vec![
                                    get(&r),
                                    curios_wasm::Instr::Call {
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

    /// Read one logical bit from a packed rope. Leaves and settled windows read their packed payload directly. An uncached node is forced once; later reads take its packed cache without walking the tree.
    pub(crate) fn emit_bits_read_func(
        &mut self,
        func_name: curios_wasm::FuncName,
        force_func: curios_wasm::FuncName,
    ) {
        let rope = self.table.bin_rope();
        let r = curios_wasm::LocalName::from("r");
        let i = curios_wasm::LocalName::from("i");
        let p = curios_wasm::LocalName::from("p");
        let j = curios_wasm::LocalName::from("j");
        let sb = curios_wasm::LocalName::from("sb");
        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);

        let instrs = vec![
            get(&i),
            get(&r),
            field_get(&rope.base, &rope.len_field),
            curios_wasm::Instr::I32GeU,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("bounds"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![curios_wasm::Instr::Unreachable],
                else_instructions: vec![],
            },
            get(&i),
            set(&j),
            get(&r),
            field_get(&rope.base, &rope.tag_field),
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("leaf"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&r),
                    cast(&rope.leaf),
                    field_get(&rope.leaf, &rope.payload_field),
                    set(&p),
                ],
                else_instructions: vec![
                    get(&r),
                    field_get(&rope.base, &rope.tag_field),
                    curios_wasm::Instr::I32Const { value: 2 },
                    curios_wasm::Instr::I32Eq,
                    curios_wasm::Instr::If {
                        label_name: curios_wasm::LabelName::from("view"),
                        block_type: curios_wasm::BlockType::Empty,
                        then_instructions: vec![
                            get(&r),
                            cast(&rope.view),
                            field_get(&rope.view, &rope.offset_field),
                            get(&i),
                            curios_wasm::Instr::I32Add,
                            set(&j),
                            get(&r),
                            cast(&rope.view),
                            field_get(&rope.view, &rope.base_field),
                            set(&sb),
                            get(&sb),
                            field_get(&rope.base, &rope.tag_field),
                            curios_wasm::Instr::I32Eqz,
                            curios_wasm::Instr::If {
                                label_name: curios_wasm::LabelName::from("view_base"),
                                block_type: curios_wasm::BlockType::Empty,
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
                            curios_wasm::Instr::RefIsNull,
                            curios_wasm::Instr::If {
                                label_name: curios_wasm::LabelName::from("settle"),
                                block_type: curios_wasm::BlockType::Empty,
                                then_instructions: vec![
                                    get(&r),
                                    curios_wasm::Instr::Call {
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
            curios_wasm::Instr::I32Const { value: 3 },
            curios_wasm::Instr::I32ShrU,
            curios_wasm::Instr::ArrayGetU {
                type_name: rope.payload.clone(),
            },
            get(&j),
            curios_wasm::Instr::I32Const { value: 7 },
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32ShrU,
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32And,
        ];

        self.add_helper(
            func_name,
            vec![(r, concrete_val(rope.base.clone(), false)), (i, i32_val)],
            curios_wasm::ValType::Num(curios_wasm::NumType::I32),
            vec![
                (p, concrete_val(rope.payload, true)),
                (j, curios_wasm::ValType::Num(curios_wasm::NumType::I32)),
                (sb, concrete_val(rope.base, true)),
            ],
            instrs,
        );
    }

    /// `$list/bytes/force (ref $rope/list) -> (ref $elems)`: force the outer rope, then force every element through `$bytes/force` into a *fresh* payload (the shallow force of a leaf answers its live payload, which must not be element-rewritten in place).
    pub(crate) fn emit_list_bytes_force_func(&mut self, func_name: curios_wasm::FuncName) {
        let elems = self.table.elems_type();

        let r = curios_wasm::LocalName::from("r");
        let flat = curios_wasm::LocalName::from("flat");
        let fresh = curios_wasm::LocalName::from("fresh");
        let idx = curios_wasm::LocalName::from("idx");
        let count = curios_wasm::LocalName::from("count");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (flat.clone(), concrete_val(elems.clone(), true)),
            (fresh.clone(), concrete_val(elems.clone(), true)),
            (idx.clone(), i32_val.clone()),
            (count.clone(), i32_val),
        ];

        let loop_label = curios_wasm::LabelName::from("fill");
        let done_label = curios_wasm::LabelName::from("done");

        let instrs = vec![
            get(&r),
            curios_wasm::Instr::Call {
                func_name: self.table.list_force_func(),
            },
            set(&flat),
            get(&flat),
            curios_wasm::Instr::ArrayLen,
            set(&count),
            get(&count),
            curios_wasm::Instr::ArrayNewDefault {
                type_name: elems.clone(),
            },
            set(&fresh),
            curios_wasm::Instr::Block {
                label_name: done_label.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![curios_wasm::Instr::Loop {
                    label_name: loop_label.clone(),
                    block_type: curios_wasm::BlockType::Empty,
                    instructions: vec![
                        get(&idx),
                        get(&count),
                        curios_wasm::Instr::I32GeU,
                        curios_wasm::Instr::BrIf {
                            label_name: done_label,
                        },
                        get(&fresh),
                        get(&idx),
                        get(&flat),
                        get(&idx),
                        curios_wasm::Instr::ArrayGet {
                            type_name: elems.clone(),
                        },
                        // An element is small-canonical, so an immediate is boxed before the deep force — the box is the cast this arm used to make, plus the materialisation.
                        curios_wasm::Instr::Call {
                            func_name: self.table.bytes_box_func(),
                        },
                        curios_wasm::Instr::Call {
                            func_name: self.table.bytes_force_func(),
                        },
                        curios_wasm::Instr::ArraySet {
                            type_name: elems.clone(),
                        },
                        get(&idx),
                        curios_wasm::Instr::I32Const { value: 1 },
                        curios_wasm::Instr::I32Add,
                        set(&idx),
                        curios_wasm::Instr::Br {
                            label_name: loop_label,
                        },
                    ],
                }],
            },
            get(&fresh),
            curios_wasm::Instr::RefAsNonNull,
        ];

        self.add_helper(
            func_name,
            vec![(r, concrete_val(self.table.list_rope_type(), false))],
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
    pub(crate) fn emit_eql_func(
        &mut self,
        rope: &RopeData,
        func_name: curios_wasm::FuncName,
        force_func: curios_wasm::FuncName,
    ) {
        let l = curios_wasm::LocalName::from("l");
        let r = curios_wasm::LocalName::from("r");
        let lb = curios_wasm::LocalName::from("lb");
        let rb = curios_wasm::LocalName::from("rb");
        let i = curios_wasm::LocalName::from("i");
        let eq = curios_wasm::LocalName::from("eq");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (lb.clone(), concrete_val(rope.payload.clone(), true)),
            (rb.clone(), concrete_val(rope.payload.clone(), true)),
            (i.clone(), i32_val.clone()),
            (eq.clone(), i32_val.clone()),
        ];

        let done = curios_wasm::LabelName::from("done");
        let bytes = curios_wasm::LabelName::from("bytes");

        let loop_instrs = vec![
            // Every byte matched: eq = 1, exit.
            get(&i),
            get(&lb),
            curios_wasm::Instr::ArrayLen,
            curios_wasm::Instr::I32GeU,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("hit"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    curios_wasm::Instr::I32Const { value: 1 },
                    set(&eq),
                    curios_wasm::Instr::Br {
                        label_name: done.clone(),
                    },
                ],
                else_instructions: vec![],
            },
            // Mismatch: exit with eq still 0.
            get(&lb),
            get(&i),
            curios_wasm::Instr::ArrayGetU {
                type_name: rope.payload.clone(),
            },
            get(&rb),
            get(&i),
            curios_wasm::Instr::ArrayGetU {
                type_name: rope.payload.clone(),
            },
            curios_wasm::Instr::I32Ne,
            curios_wasm::Instr::BrIf {
                label_name: done.clone(),
            },
            get(&i),
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Add,
            set(&i),
            curios_wasm::Instr::Br {
                label_name: bytes.clone(),
            },
        ];

        let instrs = vec![
            curios_wasm::Instr::Block {
                label_name: done.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![
                    get(&l),
                    field_get(&rope.base, &rope.len_field),
                    get(&r),
                    field_get(&rope.base, &rope.len_field),
                    curios_wasm::Instr::I32Ne,
                    curios_wasm::Instr::BrIf { label_name: done },
                    get(&l),
                    curios_wasm::Instr::Call {
                        func_name: force_func.clone(),
                    },
                    set(&lb),
                    get(&r),
                    curios_wasm::Instr::Call {
                        func_name: force_func,
                    },
                    set(&rb),
                    curios_wasm::Instr::Loop {
                        label_name: bytes,
                        block_type: curios_wasm::BlockType::Empty,
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

    /// Logical equality for packed bits. The loop is bounded by the rope's bit length, so unused high padding in the final payload byte is never observed.
    pub(crate) fn emit_bits_eql_func(
        &mut self,
        func_name: curios_wasm::FuncName,
        read_func: curios_wasm::FuncName,
    ) {
        let rope = self.table.bin_rope();
        let l = curios_wasm::LocalName::from("l");
        let r = curios_wasm::LocalName::from("r");
        let i = curios_wasm::LocalName::from("i");
        let eq = curios_wasm::LocalName::from("eq");
        let done = curios_wasm::LabelName::from("done");
        let bits = curios_wasm::LabelName::from("bits");
        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);

        let loop_instrs = vec![
            get(&i),
            get(&l),
            field_get(&rope.base, &rope.len_field),
            curios_wasm::Instr::I32GeU,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("hit"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    curios_wasm::Instr::I32Const { value: 1 },
                    set(&eq),
                    curios_wasm::Instr::Br {
                        label_name: done.clone(),
                    },
                ],
                else_instructions: vec![],
            },
            get(&l),
            get(&i),
            curios_wasm::Instr::Call {
                func_name: read_func.clone(),
            },
            get(&r),
            get(&i),
            curios_wasm::Instr::Call {
                func_name: read_func,
            },
            curios_wasm::Instr::I32Ne,
            curios_wasm::Instr::BrIf {
                label_name: done.clone(),
            },
            get(&i),
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Add,
            set(&i),
            curios_wasm::Instr::Br {
                label_name: bits.clone(),
            },
        ];

        let instrs = vec![
            curios_wasm::Instr::Block {
                label_name: done.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![
                    get(&l),
                    field_get(&rope.base, &rope.len_field),
                    get(&r),
                    field_get(&rope.base, &rope.len_field),
                    curios_wasm::Instr::I32Ne,
                    curios_wasm::Instr::BrIf { label_name: done },
                    curios_wasm::Instr::Loop {
                        label_name: bits,
                        block_type: curios_wasm::BlockType::Empty,
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

    /// `$list/map (ref $rope/list, ref $envr/1) -> (ref $rope/list)`.
    ///
    /// ```wat
    /// selems := force(src); count := selems.len
    /// out    := array.new_default <payload> count
    /// loop: while i < count, out[i] := f(selems[i]), i += 1
    /// leaf { tag 0, count, out }
    /// ```
    ///
    /// `f` is a unary closure `(A) -> B`, called by the arity-1 convention: the environment as the self argument, the table index from its special field.
    pub(crate) fn emit_map_func(
        &mut self,
        func_name: curios_wasm::FuncName,
        force_func: curios_wasm::FuncName,
    ) {
        let rope = self.table.list_rope();
        let envr_type = self.table.find_envr_type(1);
        let clsr_type = self.table.find_clsr_type(1);
        let special_field = self.table.special_field();

        let src = curios_wasm::LocalName::from("src");
        let f = curios_wasm::LocalName::from("f");
        let selems = curios_wasm::LocalName::from("selems");
        let out = curios_wasm::LocalName::from("out");
        let count = curios_wasm::LocalName::from("count");
        let i = curios_wasm::LocalName::from("i");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (selems.clone(), concrete_val(rope.payload.clone(), true)),
            (out.clone(), concrete_val(rope.payload.clone(), true)),
            (count.clone(), i32_val.clone()),
            (i.clone(), i32_val),
        ];

        let step = curios_wasm::LabelName::from("step");
        let slots = curios_wasm::LabelName::from("slots");

        // out[i] = f(selems[i]); i += 1
        let step_instrs = vec![
            get(&out),
            get(&i),
            get(&f),
            get(&selems),
            get(&i),
            curios_wasm::Instr::ArrayGet {
                type_name: rope.payload.clone(),
            },
            curios_wasm::Instr::RefAsNonNull,
            get(&f),
            field_get(&envr_type, &special_field),
            curios_wasm::Instr::CallIndirect {
                table_name: self.table.clsr_table(1),
                type_name: clsr_type,
            },
            curios_wasm::Instr::ArraySet {
                type_name: rope.payload.clone(),
            },
            get(&i),
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Add,
            set(&i),
            curios_wasm::Instr::Br {
                label_name: slots.clone(),
            },
        ];

        let instrs = vec![
            get(&src),
            curios_wasm::Instr::Call {
                func_name: force_func,
            },
            set(&selems),
            get(&selems),
            curios_wasm::Instr::ArrayLen,
            set(&count),
            get(&count),
            curios_wasm::Instr::ArrayNewDefault {
                type_name: rope.payload.clone(),
            },
            set(&out),
            curios_wasm::Instr::Loop {
                label_name: slots,
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![
                    get(&i),
                    get(&count),
                    curios_wasm::Instr::I32LtU,
                    curios_wasm::Instr::If {
                        label_name: step,
                        block_type: curios_wasm::BlockType::Empty,
                        then_instructions: step_instrs,
                        else_instructions: vec![],
                    },
                ],
            },
            // Seal the filled payload into a fresh leaf.
            curios_wasm::Instr::I32Const { value: 0 },
            get(&count),
            get(&out),
            curios_wasm::Instr::RefAsNonNull,
            curios_wasm::Instr::StructNew {
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

    /// `$bytes/box` / `$bits/box (ref null any) -> (ref $rope/bin)`: a small-canonical packed value as a rope. An immediate — the byte grain's length in the top 2 payload bits over up to 3 bytes, the bit grain's in the top 5 over up to 26 bits, both LSB-first — is materialised into a fresh exact leaf; anything else casts to the rope it must be, trapping on null exactly as the cast this call replaced did. The payload is masked before byte extraction so the length field can never bleed into a stored byte.
    pub(crate) fn emit_box_func(&mut self, grain: Grain, func_name: curios_wasm::FuncName) {
        let (len_shift, payload_mask, slots, unit) = immediate_layout(grain);
        let rope = self.table.bin_rope();
        let r = curios_wasm::LocalName::from("r");
        let v = curios_wasm::LocalName::from("v");
        let len = curios_wasm::LocalName::from("len");
        let arr = curios_wasm::LocalName::from("arr");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (v.clone(), i32_val.clone()),
            (len.clone(), i32_val),
            (arr.clone(), concrete_val(rope.payload.clone(), true)),
        ];

        // arr[i] = ((v & payload_mask) >> 8i) & 0xFF, for each payload byte the length says is occupied — the grain's unit converts a byte slot back into the length's own units.
        let set_byte = |index: i32| {
            let mut instrs = vec![
                get(&len),
                curios_wasm::Instr::I32Const {
                    value: index * unit,
                },
                curios_wasm::Instr::I32GtU,
            ];
            let mut body = vec![
                get(&arr),
                curios_wasm::Instr::I32Const { value: index },
                get(&v),
                curios_wasm::Instr::I32Const {
                    value: payload_mask,
                },
                curios_wasm::Instr::I32And,
            ];
            if index != 0 {
                body.push(curios_wasm::Instr::I32Const { value: index * 8 });
                body.push(curios_wasm::Instr::I32ShrU);
            }
            body.push(curios_wasm::Instr::I32Const { value: 0xFF });
            body.push(curios_wasm::Instr::I32And);
            body.push(curios_wasm::Instr::ArraySet {
                type_name: rope.payload.clone(),
            });
            instrs.push(curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("occupied"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: body,
                else_instructions: vec![],
            });
            instrs
        };

        let mut immediate = vec![
            get(&r),
            curios_wasm::Instr::RefCast {
                ref_type: Table::int_type(false),
            },
            curios_wasm::Instr::I31GetU,
            set(&v),
            get(&v),
            curios_wasm::Instr::I32Const { value: len_shift },
            curios_wasm::Instr::I32ShrU,
            set(&len),
        ];
        // The payload array holds the length's ceiling in bytes: the length itself at the byte grain, `(len + 7) / 8` at the bit grain.
        immediate.push(get(&len));
        if unit != 1 {
            immediate.extend([
                curios_wasm::Instr::I32Const { value: 7 },
                curios_wasm::Instr::I32Add,
                curios_wasm::Instr::I32Const { value: 3 },
                curios_wasm::Instr::I32ShrU,
            ]);
        }
        immediate.extend([
            curios_wasm::Instr::ArrayNewDefault {
                type_name: rope.payload.clone(),
            },
            set(&arr),
        ]);
        for index in 0..slots {
            immediate.extend(set_byte(index));
        }
        immediate.extend([
            curios_wasm::Instr::I32Const { value: 0 },
            get(&len),
            get(&arr),
            curios_wasm::Instr::RefAsNonNull,
            curios_wasm::Instr::StructNew {
                type_name: rope.leaf.clone(),
            },
            curios_wasm::Instr::Return,
        ]);

        let instrs = vec![
            get(&r),
            curios_wasm::Instr::RefTest {
                ref_type: Table::int_type(false),
            },
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("immediate"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: immediate,
                else_instructions: vec![],
            },
            get(&r),
            cast(&rope.base),
        ];

        self.add_helper(
            func_name,
            vec![(r, Table::top_type(true))],
            concrete_val(rope.base.clone(), false),
            locals,
            instrs,
        );
    }

    /// `$bytes/norm` / `$bits/norm (ref $rope/bin) -> (ref any)`: the canonical form of a packed rope — inside its grain's envelope it packs into the i31, and anything longer answers itself. Called at every producer's exit for its grain, which is what makes a mixed-representation pair unrepresentable and the immediate equality one instruction. The bit grain's force already zeroes final-byte padding, so the bytes OR in clean.
    pub(crate) fn emit_norm_func(
        &mut self,
        grain: Grain,
        func_name: curios_wasm::FuncName,
        force_func: curios_wasm::FuncName,
    ) {
        let (len_shift, _, slots, unit) = immediate_layout(grain);
        let envelope = match grain {
            Grain::X => 3,
            Grain::B => 26,
        };
        let rope = self.table.bin_rope();
        let r = curios_wasm::LocalName::from("r");
        let p = curios_wasm::LocalName::from("p");
        let len = curios_wasm::LocalName::from("len");
        let v = curios_wasm::LocalName::from("v");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (p.clone(), concrete_val(rope.payload.clone(), true)),
            (len.clone(), i32_val.clone()),
            (v.clone(), i32_val),
        ];

        // v |= p[i] << 8i, for each payload byte the length says is occupied.
        let or_byte = |index: i32| {
            let mut body = vec![
                get(&v),
                get(&p),
                curios_wasm::Instr::I32Const { value: index },
                curios_wasm::Instr::ArrayGetU {
                    type_name: rope.payload.clone(),
                },
            ];
            if index != 0 {
                body.push(curios_wasm::Instr::I32Const { value: index * 8 });
                body.push(curios_wasm::Instr::I32Shl);
            }
            body.push(curios_wasm::Instr::I32Or);
            body.push(set(&v));
            vec![
                get(&len),
                curios_wasm::Instr::I32Const {
                    value: index * unit,
                },
                curios_wasm::Instr::I32GtU,
                curios_wasm::Instr::If {
                    label_name: curios_wasm::LabelName::from("occupied"),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: body,
                    else_instructions: vec![],
                },
            ]
        };

        let mut instrs = vec![
            get(&r),
            field_get(&rope.base, &rope.len_field),
            set(&len),
            get(&len),
            curios_wasm::Instr::I32Const { value: envelope },
            curios_wasm::Instr::I32GtU,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("wide"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![get(&r), curios_wasm::Instr::Return],
                else_instructions: vec![],
            },
            get(&r),
            curios_wasm::Instr::Call {
                func_name: force_func,
            },
            set(&p),
            get(&len),
            curios_wasm::Instr::I32Const { value: len_shift },
            curios_wasm::Instr::I32Shl,
            set(&v),
        ];
        for index in 0..slots {
            instrs.extend(or_byte(index));
        }
        instrs.extend([get(&v), curios_wasm::Instr::RefI31]);

        self.add_helper(
            func_name,
            vec![(r, concrete_val(rope.base.clone(), false))],
            Table::top_type(false),
            locals,
            instrs,
        );
    }

    /// `$<carrier>/embed (ref <payload>) -> (ref <base>)`: one fresh leaf.
    pub(crate) fn emit_embed_func(&mut self, rope: &RopeData, func_name: curios_wasm::FuncName) {
        let b = curios_wasm::LocalName::from("b");

        let instrs = vec![
            curios_wasm::Instr::I32Const { value: 0 },
            get(&b),
            curios_wasm::Instr::ArrayLen,
            get(&b),
            curios_wasm::Instr::StructNew {
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

    /// `$list/bytes/embed (ref $elems) -> (ref $rope/list)`: embed each raw `$bytes` element into a `$rope/bin/leaf` in place — the host-built array is fresh, nothing else aliases it — then embed the outer array into a `$rope/list/leaf`.
    pub(crate) fn emit_list_bytes_embed_func(&mut self, func_name: curios_wasm::FuncName) {
        let elems = self.table.elems_type();
        let bin = self.table.bin_rope();
        let list = self.table.list_rope();

        let e = curios_wasm::LocalName::from("e");
        let idx = curios_wasm::LocalName::from("idx");
        let count = curios_wasm::LocalName::from("count");
        let bytes = curios_wasm::LocalName::from("bytes");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (idx.clone(), i32_val.clone()),
            (count.clone(), i32_val),
            (bytes.clone(), concrete_val(bin.payload.clone(), true)),
        ];

        let loop_label = curios_wasm::LabelName::from("fill");
        let done_label = curios_wasm::LabelName::from("done");

        let instrs = vec![
            get(&e),
            curios_wasm::Instr::ArrayLen,
            set(&count),
            curios_wasm::Instr::Block {
                label_name: done_label.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![curios_wasm::Instr::Loop {
                    label_name: loop_label.clone(),
                    block_type: curios_wasm::BlockType::Empty,
                    instructions: vec![
                        get(&idx),
                        get(&count),
                        curios_wasm::Instr::I32GeU,
                        curios_wasm::Instr::BrIf {
                            label_name: done_label,
                        },
                        get(&e),
                        get(&idx),
                        curios_wasm::Instr::ArrayGet {
                            type_name: elems.clone(),
                        },
                        cast(&bin.payload),
                        set(&bytes),
                        get(&e),
                        get(&idx),
                        curios_wasm::Instr::I32Const { value: 0 },
                        get(&bytes),
                        curios_wasm::Instr::ArrayLen,
                        get(&bytes),
                        curios_wasm::Instr::RefAsNonNull,
                        curios_wasm::Instr::StructNew {
                            type_name: bin.leaf.clone(),
                        },
                        // A host-built element enters the guest world canonical: a small `Bytes` becomes the i31 here. A `Handle` element is always four bytes, so the call answers it unchanged.
                        curios_wasm::Instr::Call {
                            func_name: self.table.bytes_norm_func(),
                        },
                        curios_wasm::Instr::ArraySet {
                            type_name: elems.clone(),
                        },
                        get(&idx),
                        curios_wasm::Instr::I32Const { value: 1 },
                        curios_wasm::Instr::I32Add,
                        set(&idx),
                        curios_wasm::Instr::Br {
                            label_name: loop_label,
                        },
                    ],
                }],
            },
            curios_wasm::Instr::I32Const { value: 0 },
            get(&count),
            get(&e),
            curios_wasm::Instr::StructNew {
                type_name: list.leaf.clone(),
            },
        ];

        self.add_helper(
            func_name,
            vec![(e, concrete_val(elems, false))],
            concrete_val(list.base.clone(), false),
            locals,
            instrs,
        );
    }
}
