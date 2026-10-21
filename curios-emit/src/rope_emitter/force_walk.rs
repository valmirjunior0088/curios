//! The iterative flattening walk a `force` emitter runs.

use super::{
    Chunk, RopeData, br, branch, cast, concrete_val, field_get, field_set, get, i32_const,
    i64_const, i64_type, null, set, tee, wasm, when,
};

/// The walk machinery of [`RopeEmitter::emit_force_func`](super::RopeEmitter::emit_force_func): one scratch-local roster plus the invariant-bearing instruction blocks — entry shortcuts, the destination its first piece decides, view resolution, worklist growth and descent, pop, memoization. The emitter inlines the chunk-count source and the copy body.
pub(super) struct ForceWalk<'r> {
    rope: &'r RopeData,
    elems: curios_wasm::TypeName,
    /// What a reference popped off the worklist enters the base through: `Table::enter` for `rope`'s base.
    enter: curios_wasm::Instr,
    pub(super) r: curios_wasm::LocalName,
    node: curios_wasm::LocalName,
    pub(super) out: curios_wasm::LocalName,
    room: curios_wasm::LocalName,
    wide: curios_wasm::LocalName,
    stack: curios_wasm::LocalName,
    grown: curios_wasm::LocalName,
    sp: curios_wasm::LocalName,
    pub(super) offset: curios_wasm::LocalName,
    pub(super) cur: curios_wasm::LocalName,
    pub(super) payload: curios_wasm::LocalName,
    pub(super) src_off: curios_wasm::LocalName,
    pub(super) count: curios_wasm::LocalName,
    sb: curios_wasm::LocalName,
}

impl<'r> ForceWalk<'r> {
    /// The walk of a rope whose run occupies as many payload slots as it is long.
    pub(super) fn new(
        rope: &'r RopeData,
        elems: curios_wasm::TypeName,
        enter: curios_wasm::Instr,
    ) -> Self {
        Self {
            rope,
            elems,
            enter,
            r: curios_wasm::LocalName::from("r"),
            node: curios_wasm::LocalName::from("node"),
            out: curios_wasm::LocalName::from("out"),
            room: curios_wasm::LocalName::from("room"),
            wide: curios_wasm::LocalName::from("wide"),
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

    /// The walk's scratch-local roster.
    pub(super) fn locals(&self) -> Vec<(curios_wasm::LocalName, curios_wasm::ValType)> {
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
            (
                self.room.clone(),
                concrete_val(self.rope.room.clone(), true),
            ),
            (self.wide.clone(), i64_type()),
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
    pub(super) fn entry_shortcuts(&self) -> Vec<curios_wasm::Instr> {
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

    /// Where the walk's first piece puts the run, ahead of that piece's copy, which `copied` closes: every later piece finds `out` set and is copied after the pieces before it.
    ///
    /// ```wat
    /// if out == null:                                    ;; the first piece
    ///   if r.tag == 1 && cur.tag == 1:                   ;; a node entry, a flattened first piece
    ///     room := cur.room
    ///     if room != null && room.extent == cur.len && payload.len >= r.len:
    ///       out := payload; room.extent := r.len; br copied   ;; the claim
    ///   if r.tag == 1 && cur.tag != 0:                   ;; read, grown and read again
    ///     out := new min(2 r.len, u32::MAX); room := room{r.len}
    ///   else:
    ///     out := new r.len; room := null
    /// ```
    ///
    /// A claim writes past the extent only, where no slot is written, and every other holder of the payload reads below the extent its run ends at, which no slot it reads is past: so the rule asks nothing about who else holds the payload, and the extent, which only grows, is all that is shared.
    pub(super) fn place_first(&self) -> Vec<curios_wasm::Instr> {
        let run_len = || {
            vec![
                get(&self.r),
                field_get(&self.rope.base, &self.rope.len_field),
            ]
        };
        let tag_is = |local: &curios_wasm::LocalName, tag: i32| {
            vec![
                get(local),
                field_get(&self.rope.base, &self.rope.tag_field),
                i32_const(tag),
                curios_wasm::Instr::I32Eq,
            ]
        };

        let fits = wasm![
            get(&self.room),
            field_get(&self.rope.room, &self.rope.extent_field),
            get(&self.cur),
            field_get(&self.rope.base, &self.rope.len_field),
            curios_wasm::Instr::I32Eq,
            get(&self.payload),
            curios_wasm::Instr::ArrayLen,
            run_len(),
            curios_wasm::Instr::I32GeU,
            curios_wasm::Instr::I32And,
        ];
        let claim = wasm![
            get(&self.payload),
            set(&self.out),
            get(&self.room),
            run_len(),
            field_set(&self.rope.room, &self.rope.extent_field),
            br("copied"),
        ];
        let claimable = wasm![
            get(&self.cur),
            cast(&self.rope.node),
            field_get(&self.rope.node, &self.rope.room_field),
            set(&self.room),
            get(&self.room),
            curios_wasm::Instr::RefIsNull,
            curios_wasm::Instr::I32Eqz,
            when(wasm![fits, when(claim)]),
        ];

        // Twice the run, the product taken wide and clamped to the largest length a run holds.
        let doubled = wasm![
            run_len(),
            curios_wasm::Instr::I64ExtendI32U,
            i64_const(1),
            curios_wasm::Instr::I64Shl,
            tee(&self.wide),
            i64_const(u32::MAX.into()),
            get(&self.wide),
            i64_const(u32::MAX.into()),
            curios_wasm::Instr::I64LtU,
            curios_wasm::Instr::Select {
                val_types: vec![i64_type()],
            },
            curios_wasm::Instr::I32WrapI64,
        ];
        let roomy = wasm![
            doubled,
            curios_wasm::Instr::ArrayNewDefault {
                type_name: self.rope.payload.clone(),
            },
            set(&self.out),
            run_len(),
            curios_wasm::Instr::StructNew {
                type_name: self.rope.room.clone(),
            },
            set(&self.room),
        ];
        let exact = wasm![
            run_len(),
            curios_wasm::Instr::ArrayNewDefault {
                type_name: self.rope.payload.clone(),
            },
            set(&self.out),
            null(&self.rope.room),
            set(&self.room),
        ];

        vec![
            get(&self.out),
            curios_wasm::Instr::RefIsNull,
            when(wasm![
                tag_is(&self.r, 1),
                tag_is(&self.cur, 1),
                curios_wasm::Instr::I32And,
                when(claimable),
                tag_is(&self.r, 1),
                get(&self.cur),
                field_get(&self.rope.base, &self.rope.tag_field),
                curios_wasm::Instr::I32Eqz,
                curios_wasm::Instr::I32Eqz,
                curios_wasm::Instr::I32And,
                branch(roomy, exact),
            ]),
        ]
    }

    /// A fresh 32-slot worklist, the walk starting at the entry.
    pub(super) fn init_worklist(&self) -> Vec<curios_wasm::Instr> {
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
    pub(super) fn resolve_view_chunk(&self) -> Vec<curios_wasm::Instr> {
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
    pub(super) fn push_uncached_node(
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
    pub(super) fn pop_or_exit(
        &self,
        walk_label: &curios_wasm::LabelName,
    ) -> Vec<curios_wasm::Instr> {
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
                    self.enter.clone(),
                    set(&self.cur),
                    curios_wasm::Instr::Br {
                        label_name: walk_label.clone(),
                    },
                ],
                else_instructions: vec![],
            },
        ]
    }

    /// Memoize a node entry, its payload and that payload's room record, and release its tree (`node` was set at entry exactly when the tag is 1); a view entry has nowhere to memoize. Answers the filled payload.
    pub(super) fn memoize_entry_node(&self) -> Vec<curios_wasm::Instr> {
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
                    get(&self.room),
                    field_set(&self.rope.node, &self.rope.room_field),
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
