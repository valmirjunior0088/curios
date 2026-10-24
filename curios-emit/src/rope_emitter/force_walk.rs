//! The flattening a `force` emitter runs: where the run goes, decided by its first piece before anything is written, and the `write` helper that puts every piece at the offset the lengths before it give.

use super::{
    Chunk, RopeData, br, branch, call, cast, concrete_val, either, field_get, field_set, get,
    i32_const, i32_type, i64_const, i64_type, null, repeat, set, tee, wasm, when,
};

/// The flattening of [`RopeEmitter::emit_force_func`](super::RopeEmitter::emit_force_func) and the `write` it calls: the names of both functions' parameters and locals, and the instruction blocks that bear the invariants — the entry shortcuts, the first piece and where it puts the run, the write, memoization.
pub(super) struct ForceWalk<'r> {
    rope: &'r RopeData,
    pub(super) r: curios_wasm::LocalName,
    node: curios_wasm::LocalName,
    out: curios_wasm::LocalName,
    room: curios_wasm::LocalName,
    wide: curios_wasm::LocalName,
    first: curios_wasm::LocalName,
    payload: curios_wasm::LocalName,
    claimed: curios_wasm::LocalName,
    piece: curios_wasm::LocalName,
    at: curios_wasm::LocalName,
    flat: curios_wasm::LocalName,
    dst: curios_wasm::LocalName,
    src_off: curios_wasm::LocalName,
    sb: curios_wasm::LocalName,
    left: curios_wasm::LocalName,
    right: curios_wasm::LocalName,
    left_len: curios_wasm::LocalName,
}

impl<'r> ForceWalk<'r> {
    /// The flattening of a rope whose run occupies as many payload slots as it is long.
    pub(super) fn new(rope: &'r RopeData) -> Self {
        let local = curios_wasm::LocalName::from;
        Self {
            rope,
            r: local("r"),
            node: local("node"),
            out: local("out"),
            room: local("room"),
            wide: local("wide"),
            first: local("first"),
            payload: local("payload"),
            claimed: local("claimed"),
            piece: local("piece"),
            at: local("at"),
            flat: local("flat"),
            dst: local("dst"),
            src_off: local("src_off"),
            sb: local("sb"),
            left: local("left"),
            right: local("right"),
            left_len: local("left_len"),
        }
    }

    fn base(&self) -> curios_wasm::ValType {
        concrete_val(self.rope.base.clone(), true)
    }

    fn payload_type(&self) -> curios_wasm::ValType {
        concrete_val(self.rope.payload.clone(), true)
    }

    /// The force's locals beside its parameter `r`.
    pub(super) fn force_locals(&self) -> Vec<(curios_wasm::LocalName, curios_wasm::ValType)> {
        vec![
            (
                self.node.clone(),
                concrete_val(self.rope.node.clone(), true),
            ),
            (self.out.clone(), self.payload_type()),
            (
                self.room.clone(),
                concrete_val(self.rope.room.clone(), true),
            ),
            (self.wide.clone(), i64_type()),
            (self.first.clone(), self.base()),
            (self.payload.clone(), self.payload_type()),
            (self.claimed.clone(), i32_type()),
        ]
    }

    /// The write's parameters: the piece, the payload it is written into, the offset it starts at, and whether the run's first piece is claimed, already in place at offset zero.
    pub(super) fn write_params(&self) -> Vec<(curios_wasm::LocalName, curios_wasm::ValType)> {
        vec![
            (self.piece.clone(), self.base()),
            (self.out.clone(), self.payload_type()),
            (self.at.clone(), i32_type()),
            (self.claimed.clone(), i32_type()),
        ]
    }

    /// The write's locals.
    pub(super) fn write_locals(&self) -> Vec<(curios_wasm::LocalName, curios_wasm::ValType)> {
        vec![
            (self.flat.clone(), self.base()),
            (self.dst.clone(), i32_type()),
            (self.payload.clone(), self.payload_type()),
            (self.src_off.clone(), i32_type()),
            (self.sb.clone(), self.base()),
            (self.left.clone(), self.base()),
            (self.right.clone(), self.base()),
            (self.left_len.clone(), i32_type()),
        ]
    }

    /// The flat entries answer without walking: a leaf its payload, a cached node its memo (setting `node` on the way for the memoize epilogue). A view goes on to the walk.
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

    /// Whether the rope in `of` is a node never flattened, an i32 on the stack: the one shape the walk descends into.
    fn uncached(&self, of: &curios_wasm::LocalName) -> Vec<curios_wasm::Instr> {
        wasm![
            get(of),
            field_get(&self.rope.base, &self.rope.tag_field),
            i32_const(1),
            curios_wasm::Instr::I32Eq,
            either(
                i32_type(),
                wasm![
                    get(of),
                    cast(&self.rope.node),
                    field_get(&self.rope.node, &self.rope.cache_field),
                    curios_wasm::Instr::RefIsNull
                ],
                vec![i32_const(0)],
            ),
        ]
    }

    /// The run's first piece into `first`: down the left children from the entry, through every node never flattened.
    pub(super) fn find_first(&self) -> Vec<curios_wasm::Instr> {
        wasm![
            get(&self.r),
            set(&self.first),
            repeat(
                "first",
                wasm![
                    self.uncached(&self.first),
                    when(wasm![
                        get(&self.first),
                        cast(&self.rope.node),
                        field_get(&self.rope.node, &self.rope.left_field),
                        set(&self.first),
                        br("first")
                    ]),
                ]
            ),
        ]
    }

    /// Where the run goes, decided by its first piece before anything is written: the first piece's payload where the claim holds (`claimed` set), and a fresh one otherwise.
    ///
    /// ```wat
    /// if r.tag == 1 && first.tag == 1:                   ;; a node entry, a flattened first piece
    ///   room := first.room
    ///   if room != null && room.extent == first.len && first.cache.len >= r.len:
    ///     out := first.cache; room.extent := r.len; claimed := 1   ;; the claim
    /// if !claimed:
    ///   if r.tag == 1 && first.tag != 0:                 ;; read, grown and read again
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
            get(&self.first),
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
            i32_const(1),
            set(&self.claimed),
        ];
        let claimable = wasm![
            get(&self.first),
            cast(&self.rope.node),
            field_get(&self.rope.node, &self.rope.cache_field),
            set(&self.payload),
            get(&self.first),
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

        wasm![
            tag_is(&self.r, 1),
            tag_is(&self.first, 1),
            curios_wasm::Instr::I32And,
            when(claimable),
            get(&self.claimed),
            curios_wasm::Instr::I32Eqz,
            when(wasm![
                tag_is(&self.r, 1),
                get(&self.first),
                field_get(&self.rope.base, &self.rope.tag_field),
                curios_wasm::Instr::I32Eqz,
                curios_wasm::Instr::I32Eqz,
                curios_wasm::Instr::I32And,
                branch(roomy, exact),
            ]),
        ]
    }

    /// The entry written whole into `out` from offset zero.
    pub(super) fn write_entry(&self, write: &curios_wasm::FuncName) -> Vec<curios_wasm::Instr> {
        vec![
            get(&self.r),
            get(&self.out),
            i32_const(0),
            get(&self.claimed),
            call(write),
        ]
    }

    /// Copy the flat piece in `flat` into `out` at `dst`: a leaf's payload, a view's window over its flat base, or a flattened node's cache. Not the claimed first piece, already in place: the only piece at offset zero once a claim holds, since a claimed piece is a node and no node is empty.
    fn place(&self) -> Vec<curios_wasm::Instr> {
        let leaf_payload = |of: &curios_wasm::LocalName| {
            wasm![
                get(of),
                cast(&self.rope.leaf),
                field_get(&self.rope.leaf, &self.rope.payload_field),
                set(&self.payload)
            ]
        };
        let node_cache = |of: &curios_wasm::LocalName| {
            wasm![
                get(of),
                cast(&self.rope.node),
                field_get(&self.rope.node, &self.rope.cache_field),
                set(&self.payload)
            ]
        };
        // A view's window over its base's flat payload — a leaf's payload or a cached node's cache, non-null by the slice invariant, the null trap at the copy its enforcement.
        let view = wasm![
            get(&self.flat),
            cast(&self.rope.view),
            field_get(&self.rope.view, &self.rope.base_field),
            set(&self.sb),
            get(&self.flat),
            cast(&self.rope.view),
            field_get(&self.rope.view, &self.rope.offset_field),
            set(&self.src_off),
            get(&self.sb),
            field_get(&self.rope.base, &self.rope.tag_field),
            curios_wasm::Instr::I32Eqz,
            branch(leaf_payload(&self.sb), node_cache(&self.sb)),
        ];
        wasm![
            get(&self.flat),
            field_get(&self.rope.base, &self.rope.tag_field),
            curios_wasm::Instr::I32Eqz,
            branch(
                wasm![leaf_payload(&self.flat), i32_const(0), set(&self.src_off)],
                wasm![
                    get(&self.flat),
                    field_get(&self.rope.base, &self.rope.tag_field),
                    i32_const(2),
                    curios_wasm::Instr::I32Eq,
                    branch(
                        view,
                        wasm![node_cache(&self.flat), i32_const(0), set(&self.src_off)]
                    ),
                ]
            ),
            get(&self.claimed),
            get(&self.dst),
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32Eqz,
            when(wasm![
                get(&self.out),
                get(&self.dst),
                get(&self.payload),
                get(&self.src_off),
                get(&self.flat),
                field_get(&self.rope.base, &self.rope.len_field),
                curios_wasm::Instr::ArrayCopy {
                    target_name: self.rope.payload.clone(),
                    source_name: self.rope.payload.clone(),
                },
            ]),
        ]
    }

    /// The write's body: `piece` written into `out` from `at`, each flat piece copied where the lengths before it put it.
    ///
    /// ```wat
    /// loop:
    ///   if piece is a node never flattened:
    ///     left, right := piece.left, piece.right
    ///     if both are nodes never flattened:
    ///       write the shorter at its offset, then piece := the longer   ;; recursion halves the run
    ///     elif left is one: copy right at at + left.len; piece := left
    ///     else: copy left at at; piece := right; at += left.len
    ///     continue
    ///   copy piece at at
    /// ```
    ///
    /// **Recursion into the shorter half bounds the depth.** A call is made only where both children still need walking, on the shorter, at most half the run, so no chain of calls is longer than the run's length has bits — 31 for any run — whatever the rope's depth, and a lean of any depth is a loop. Nothing is allocated: an offset is the lengths before a piece, so no worklist holds what is pending.
    pub(super) fn write_body(&self, write: &curios_wasm::FuncName) -> Vec<curios_wasm::Instr> {
        let child = |field: &curios_wasm::FieldName, into: &curios_wasm::LocalName| {
            wasm![
                get(&self.piece),
                cast(&self.rope.node),
                field_get(&self.rope.node, field),
                set(into)
            ]
        };
        let past_left = || {
            wasm![
                get(&self.at),
                get(&self.left_len),
                curios_wasm::Instr::I32Add
            ]
        };
        let place_at = |of: &curios_wasm::LocalName, offset: Vec<curios_wasm::Instr>| {
            wasm![
                get(of),
                set(&self.flat),
                offset,
                set(&self.dst),
                self.place()
            ]
        };
        let recurse = |of: &curios_wasm::LocalName, offset: Vec<curios_wasm::Instr>| {
            wasm![
                get(of),
                get(&self.out),
                offset,
                get(&self.claimed),
                call(write)
            ]
        };
        let node = wasm![
            child(&self.rope.left_field, &self.left),
            child(&self.rope.right_field, &self.right),
            get(&self.left),
            field_get(&self.rope.base, &self.rope.len_field),
            set(&self.left_len),
            self.uncached(&self.left),
            self.uncached(&self.right),
            curios_wasm::Instr::I32And,
            when(wasm![
                get(&self.left_len),
                get(&self.piece),
                field_get(&self.rope.base, &self.rope.len_field),
                get(&self.left_len),
                curios_wasm::Instr::I32Sub,
                curios_wasm::Instr::I32LeU,
                branch(
                    wasm![
                        recurse(&self.left, vec![get(&self.at)]),
                        get(&self.right),
                        set(&self.piece),
                        past_left(),
                        set(&self.at)
                    ],
                    wasm![
                        recurse(&self.right, past_left()),
                        get(&self.left),
                        set(&self.piece)
                    ],
                ),
                br("descend"),
            ]),
            self.uncached(&self.left),
            when(wasm![
                place_at(&self.right, past_left()),
                get(&self.left),
                set(&self.piece),
                br("descend")
            ]),
            place_at(&self.left, vec![get(&self.at)]),
            get(&self.right),
            set(&self.piece),
            past_left(),
            set(&self.at),
            br("descend"),
        ];

        vec![repeat(
            "descend",
            wasm![
                self.uncached(&self.piece),
                when(node),
                place_at(&self.piece, vec![get(&self.at)]),
            ],
        )]
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
