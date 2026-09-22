//! The shared rope helper functions — the only module-level functions the emitter mints beyond the program's own (everything else is inlined at its use site — straight-line sequences only; anything the emitter lowers to a *loop* lives here, so its mutable scratch locals are zeroed by the fresh activation instead of leaking across executions of one call site). Seven rows:
//!
//! - `$<carrier>/force` flattens a byte- or element-grain rope to its payload array: the leaf answers its payload, a cached node answers its cache, and everything else fills a fresh payload by an *iterative* tree walk (an explicit `$elems` worklist, grown by doubling), so a 100k-deep concat chain never touches the wasm call stack. Only an entry *node* memoizes — intermediates are usually garbage the moment the walk passes them, and a view's fill is a single window copy of exactly its own size.
//! - `$<carrier>/embed` places a host-built flat payload into a fresh leaf on re-entry.
//! - `$<carrier>/slice` builds the O(1) window: bounds-check, the trivial windows answer an empty leaf or the rope itself, a view collapses (so windows never stack), and an uncached node base is forced first — which memoizes — so every `view` in existence has a *flat-available* base (a leaf or a cached node).
//! - `$<carrier>/read` answers one element: off a leaf payload, *through* a view's window without forcing (the invariant above makes that O(1)), or via `force` on a node.
//! - `$bits/force` performs the same iterative walk in logical bit units, filling a zeroed packed payload and memoizing it on an entry node.
//! - `$bytes/eql` compares two `Bytes` ropes bytewise: unequal lengths answer without forcing, equal lengths force both payloads once and walk them.
//! - `$list/map` applies a unary closure to every element of the forced payload, filling a fresh leaf.
//!
//! The `list/bytes` variants are the host boundary's deep forms: a `List(Bytes)` / `List(Handle)` wire value carries `Bytes`-shaped *elements*, which the host lifts and lowers as raw `$bytes` — so params force each element too, and results embed each element back. A list of scalars crosses flat, one element per value — `$longs` for a `Nat` or `Int`, `$words` for a `Bool`: `$list/<leaf>/to_<payload>` narrows each element on the way out and `$list/<leaf>/of_<payload>` boxes each on the way back, so a host never builds or reads a guest box.

mod force_walk;
use force_walk::*;

use {
    super::{
        BigHelper, ImmediateLayout, RopeData, Table, block, br, br_if, call, cast, concrete_val,
        either, field_get, field_set, get, i32_const, null, repeat, set,
    },
    curios_abi::{WireLeaf, WireType},
    curios_num::Grain,
};

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
                then_instructions: self.table.refuse_instrs(curios_cont::Panic::OutOfBounds),
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
    /// if i >= r.len         → unreachable             ;; the eager bounds trap
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
            // The eager bounds trap, as the bit grain's twin opens with. A leaf would trap in the engine on its own payload, but a *view* reads `base.payload[offset + i]`, and a position past the window is a position the base array still holds — so without this a read past the end answers a neighbouring element instead of refusing. What makes every read well-placed is the proof its caller discharged; this is the backstop for a wrong erasure or a wrong checker, and the one place the three carriers' reads did not agree.
            get(&i),
            get(&r),
            field_get(&rope.base, &rope.len_field),
            curios_wasm::Instr::I32GeU,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("bounds"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: self.table.refuse_instrs(curios_cont::Panic::OutOfBounds),
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
                then_instructions: self.table.refuse_instrs(curios_cont::Panic::OutOfBounds),
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

    /// `$list/<grain>/force (ref $rope/list) -> (ref $elems)`: force the outer rope, then force every element through the grain's own force into a *fresh* payload (the shallow force of a leaf answers its live payload, which must not be element-rewritten in place).
    pub(crate) fn emit_list_bin_force_func(
        &mut self,
        grain: Grain,
        func_name: curios_wasm::FuncName,
    ) {
        let elems = self.table.elems_type();

        let box_func = match grain {
            Grain::X => self.table.bytes_box_func(),
            Grain::B => self.table.bits_box_func(),
        };
        let force_func = match grain {
            Grain::X => self.table.bytes_force_func(),
            Grain::B => self.table.bits_force_func(),
        };

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
                            func_name: box_func,
                        },
                        curios_wasm::Instr::Call {
                            func_name: force_func,
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
    /// `$bin/and` / `/or` / `/xor (ref $payload) (ref $payload) (i32 len) -> (ref $rope/bin)`: two forced payloads combined byte for byte, sealed at `len`.
    ///
    /// **Three helpers rather than six, because a payload does not know its grain.** The operands arrive already forced, so what is left is a walk over two byte arrays — the same walk whether the generators are bits or bytes — and the grain survives only in which `force` the caller reached for and which length it hands over. The `eql` pair above is two functions for the opposite reason: equality compares *logical* lengths, and a bit grain's final byte carries padding a comparison must not read.
    ///
    /// **The padding needs no mask**, which is `Binary::pointwise`'s argument one rung down: `force` fills a zeroed payload and copies only the logical run, so every padding bit enters as zero, and `and`, `or` and `xor` each take `(0, 0)` to `0`.
    ///
    /// `len` is the run's own rather than the payload's extent: at the byte grain the two agree, while at the bit grain the payload is `ceil(len/8)` bytes and the leaf has to carry the bit count. Forcing at the call site is what keeps this grain-free, and it is allowed there because a pair of calls is a straight-line sequence — the loop, which is not, stays here. Equal operand lengths are the type's to guarantee; the bound is discharged above erasure and nothing here re-checks it.
    pub(crate) fn emit_pointwise_func(
        &mut self,
        func_name: curios_wasm::FuncName,
        combine: curios_wasm::Instr,
    ) {
        let rope = self.table.bin_rope();

        let lb = curios_wasm::LocalName::from("lb");
        let rb = curios_wasm::LocalName::from("rb");
        let len = curios_wasm::LocalName::from("len");
        let out = curios_wasm::LocalName::from("out");
        let count = curios_wasm::LocalName::from("count");
        let i = curios_wasm::LocalName::from("i");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (out.clone(), concrete_val(rope.payload.clone(), true)),
            (count.clone(), i32_val.clone()),
            (i.clone(), i32_val.clone()),
        ];

        let step = curios_wasm::LabelName::from("step");
        let slots = curios_wasm::LabelName::from("slots");

        // out[i] = lb[i] ⊕ rb[i]; i += 1
        let step_instrs = vec![
            get(&out),
            get(&i),
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
            combine,
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
            get(&lb),
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
            // Seal the filled payload into a fresh leaf at the run's logical length.
            curios_wasm::Instr::I32Const { value: 0 },
            get(&len),
            get(&out),
            curios_wasm::Instr::RefAsNonNull,
            curios_wasm::Instr::StructNew {
                type_name: rope.leaf.clone(),
            },
        ];

        self.add_helper(
            func_name,
            vec![
                (lb, concrete_val(rope.payload.clone(), true)),
                (rb, concrete_val(rope.payload, true)),
                (len, i32_val),
            ],
            concrete_val(rope.base, false),
            locals,
            instrs,
        );
    }

    /// `$bytes/to_bits` / `$bits/to_bytes (ref $rope/bin) -> (ref $rope/bin)`: one run resealed at the other grain.
    ///
    /// **The payload crosses untouched and the length does not.** Eight bits are a byte in the stored bytes either way, so there is nothing to repack — but a rope carries its length in the grain's *own* units at every node, so the one number that has to change is the one the leaf is sealed at: eight times going to bits, an eighth going back. That is the whole of the conversion, and the whole of why it is not the identity.
    ///
    /// The force is what makes it one number rather than a walk. A node tree holds a length per node, and rescaling each would be a rewrite of the spine; flattening first leaves exactly one to scale. The alignment the bit-to-byte direction needs is the caller's, proved above erasure and gone by here.
    pub(crate) fn emit_reinterp_func(
        &mut self,
        grain: Grain,
        func_name: curios_wasm::FuncName,
        force_func: curios_wasm::FuncName,
    ) {
        let rope = self.table.bin_rope();

        let r = curios_wasm::LocalName::from("r");
        let out = curios_wasm::LocalName::from("out");
        let len = curios_wasm::LocalName::from("len");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (out.clone(), concrete_val(rope.payload.clone(), true)),
            (len.clone(), i32_val),
        ];

        let mut instrs = vec![get(&r), field_get(&rope.base, &rope.len_field)];
        instrs.extend(match grain {
            // Bytes to bits: eight of the target's units to each of the source's.
            Grain::X => [
                curios_wasm::Instr::I32Const { value: 3 },
                curios_wasm::Instr::I32Shl,
            ],
            // Bits to bytes: the reverse, exact because the caller proved the run holds whole bytes.
            Grain::B => [
                curios_wasm::Instr::I32Const { value: 3 },
                curios_wasm::Instr::I32ShrU,
            ],
        });
        instrs.extend([
            set(&len),
            get(&r),
            curios_wasm::Instr::Call {
                func_name: force_func,
            },
            set(&out),
            curios_wasm::Instr::I32Const { value: 0 },
            get(&len),
            get(&out),
            curios_wasm::Instr::RefAsNonNull,
            curios_wasm::Instr::StructNew {
                type_name: rope.leaf.clone(),
            },
        ]);

        self.add_helper(
            func_name,
            vec![(r, concrete_val(rope.base.clone(), false))],
            concrete_val(rope.base, false),
            locals,
            instrs,
        );
    }

    /// `$<carrier>/replicate (i32 count) (i32 atom) -> (ref $rope/bin)`: `count` copies of one generator, as one flat leaf.
    ///
    /// **Split by grain where the pointwise emitter is not**, for the one difference that survives to the payload: a byte grain fills every slot with the generator and is finished, while a bit grain fills with all-ones and then has to *unset* the bits past the length. That is the mask `Binary::replicate` carries and the only place a fill needs one — `cmp` and `hash` read the stored bytes and trust the padding to be zero, so an all-ones tail would leave a run comparing unequal to itself packed any other way.
    ///
    /// No loop either way. `array.new` is the fill, and the bit grain's partial tail is one store after it: `fill & ((1 << (count & 7)) - 1)`, which needs no second branch on the generator because a clear fill masks to zero regardless. The fill itself is `0 - atom` rather than a select, which is all-ones for the set bit and zero for the clear one, and the payload packs to `i8` so the store keeps the low byte.
    pub(crate) fn emit_replicate_func(&mut self, grain: Grain, func_name: curios_wasm::FuncName) {
        let rope = self.table.bin_rope();

        let count = curios_wasm::LocalName::from("count");
        let atom = curios_wasm::LocalName::from("atom");
        let out = curios_wasm::LocalName::from("out");
        let fill = curios_wasm::LocalName::from("fill");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (out.clone(), concrete_val(rope.payload.clone(), true)),
            (fill.clone(), i32_val),
        ];

        let mut instrs = match grain {
            Grain::X => vec![
                get(&atom),
                get(&count),
                curios_wasm::Instr::ArrayNew {
                    type_name: rope.payload.clone(),
                },
                set(&out),
            ],
            Grain::B => vec![
                curios_wasm::Instr::I32Const { value: 0 },
                get(&atom),
                curios_wasm::Instr::I32Sub,
                set(&fill),
                get(&fill),
                get(&count),
                curios_wasm::Instr::I32Const { value: 7 },
                curios_wasm::Instr::I32Add,
                curios_wasm::Instr::I32Const { value: 3 },
                curios_wasm::Instr::I32ShrU,
                curios_wasm::Instr::ArrayNew {
                    type_name: rope.payload.clone(),
                },
                set(&out),
                // A partial final byte keeps only the bits the length claims.
                get(&count),
                curios_wasm::Instr::I32Const { value: 7 },
                curios_wasm::Instr::I32And,
                curios_wasm::Instr::If {
                    label_name: curios_wasm::LabelName::from("tail"),
                    block_type: curios_wasm::BlockType::Empty,
                    then_instructions: vec![
                        get(&out),
                        get(&count),
                        curios_wasm::Instr::I32Const { value: 3 },
                        curios_wasm::Instr::I32ShrU,
                        get(&fill),
                        curios_wasm::Instr::I32Const { value: 1 },
                        get(&count),
                        curios_wasm::Instr::I32Const { value: 7 },
                        curios_wasm::Instr::I32And,
                        curios_wasm::Instr::I32Shl,
                        curios_wasm::Instr::I32Const { value: 1 },
                        curios_wasm::Instr::I32Sub,
                        curios_wasm::Instr::I32And,
                        curios_wasm::Instr::ArraySet {
                            type_name: rope.payload.clone(),
                        },
                    ],
                    else_instructions: vec![],
                },
            ],
        };

        // Seal the filled payload into a fresh leaf at the count it was asked for.
        instrs.extend([
            curios_wasm::Instr::I32Const { value: 0 },
            get(&count),
            get(&out),
            curios_wasm::Instr::RefAsNonNull,
            curios_wasm::Instr::StructNew {
                type_name: rope.leaf.clone(),
            },
        ]);

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        self.add_helper(
            func_name,
            vec![(count, i32_val.clone()), (atom, i32_val)],
            concrete_val(rope.base, false),
            locals,
            instrs,
        );
    }

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
        let layout = ImmediateLayout::of(grain);
        let (len_shift, payload_mask, slots, unit) = (
            layout.len_shift,
            layout.payload_mask(),
            layout.slots(),
            layout.unit(),
        );
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

    /// `$flt/rem (f64, f64) -> f64`: exact `fmod` over binary64, the one scalar helper among the rope ones — it shares their declaration path and nothing else. Long division by exponent-scaled subtraction, in f64 instructions alone: `t` starts at `|y|` and doubles while `2t ≤ |x|` (past the largest finite value the doubling gives `inf`, which fails the test and stops); then while `|x| ≥ |y|`, `t` halves until it no longer exceeds `|x|` and is subtracted. Each halving stays at or above `|y|`, so it is exact even in the subnormal range, and each subtraction has `t ≤ |x| < 2t`, which is Sterbenz's condition for exactness — so the result is the exact remainder `fmod` computes. The sign is the dividend's, as C defines it. Checked bit-for-bit against `fmod` over two million random pairs and the NaN, zero, infinity and extreme-magnitude grid before it was written down here; the worst case, the largest finite value against the smallest subnormal, takes a few hundred iterations.
    pub(crate) fn emit_flt_rem_func(&mut self, func_name: curios_wasm::FuncName) {
        let x = curios_wasm::LocalName::from("x");
        let y = curios_wasm::LocalName::from("y");
        let ax = curios_wasm::LocalName::from("ax");
        let ay = curios_wasm::LocalName::from("ay");
        let t = curios_wasm::LocalName::from("t");
        let f64_val = curios_wasm::ValType::Num(curios_wasm::NumType::F64);
        fn label(name: &str) -> curios_wasm::LabelName {
            curios_wasm::LabelName::from(name)
        }
        let f64_const = |value: f64| curios_wasm::Instr::F64Const { value };
        let not = || curios_wasm::Instr::I32Eqz;
        let br_if = |name: &str| curios_wasm::Instr::BrIf {
            label_name: label(name),
        };
        let br = |name: &str| curios_wasm::Instr::Br {
            label_name: label(name),
        };
        let return_if = |condition: Vec<curios_wasm::Instr>, result: Vec<curios_wasm::Instr>| {
            let mut instrs = condition;
            instrs.push(curios_wasm::Instr::If {
                label_name: label("return"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: result
                    .into_iter()
                    .chain([curios_wasm::Instr::Return])
                    .collect(),
                else_instructions: vec![],
            });
            instrs
        };

        let mut instrs = Vec::new();
        // A NaN operand, an infinite dividend or a zero divisor has no remainder: NaN.
        instrs.extend(return_if(
            vec![
                get(&x),
                get(&x),
                curios_wasm::Instr::F64Ne,
                get(&y),
                get(&y),
                curios_wasm::Instr::F64Ne,
                curios_wasm::Instr::I32Or,
                get(&x),
                curios_wasm::Instr::F64Abs,
                f64_const(f64::INFINITY),
                curios_wasm::Instr::F64Eq,
                curios_wasm::Instr::I32Or,
                get(&y),
                f64_const(0.0),
                curios_wasm::Instr::F64Eq,
                curios_wasm::Instr::I32Or,
            ],
            vec![f64_const(f64::NAN)],
        ));
        // An infinite divisor or a zero dividend leaves the dividend as it is, its sign included.
        instrs.extend(return_if(
            vec![
                get(&y),
                curios_wasm::Instr::F64Abs,
                f64_const(f64::INFINITY),
                curios_wasm::Instr::F64Eq,
                get(&x),
                f64_const(0.0),
                curios_wasm::Instr::F64Eq,
                curios_wasm::Instr::I32Or,
            ],
            vec![get(&x)],
        ));
        instrs.extend([
            get(&x),
            curios_wasm::Instr::F64Abs,
            set(&ax),
            get(&y),
            curios_wasm::Instr::F64Abs,
            set(&ay),
        ]);
        // A dividend below the divisor is its own remainder.
        instrs.extend(return_if(
            vec![get(&ax), get(&ay), curios_wasm::Instr::F64Lt],
            vec![get(&x)],
        ));
        // t = |y| · 2^k, the largest such at or below |x|.
        instrs.extend([
            get(&ay),
            set(&t),
            curios_wasm::Instr::Block {
                label_name: label("scaled"),
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![curios_wasm::Instr::Loop {
                    label_name: label("double"),
                    block_type: curios_wasm::BlockType::Empty,
                    instructions: vec![
                        get(&t),
                        f64_const(2.0),
                        curios_wasm::Instr::F64Mul,
                        get(&ax),
                        curios_wasm::Instr::F64Le,
                        not(),
                        br_if("scaled"),
                        get(&t),
                        f64_const(2.0),
                        curios_wasm::Instr::F64Mul,
                        set(&t),
                        br("double"),
                    ],
                }],
            },
        ]);
        // while |x| ≥ |y| { while t > |x| { t /= 2 }; |x| -= t }
        instrs.push(curios_wasm::Instr::Block {
            label_name: label("reduced"),
            block_type: curios_wasm::BlockType::Empty,
            instructions: vec![curios_wasm::Instr::Loop {
                label_name: label("subtract"),
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![
                    get(&ax),
                    get(&ay),
                    curios_wasm::Instr::F64Ge,
                    not(),
                    br_if("reduced"),
                    curios_wasm::Instr::Block {
                        label_name: label("aligned"),
                        block_type: curios_wasm::BlockType::Empty,
                        instructions: vec![curios_wasm::Instr::Loop {
                            label_name: label("halve"),
                            block_type: curios_wasm::BlockType::Empty,
                            instructions: vec![
                                get(&t),
                                get(&ax),
                                curios_wasm::Instr::F64Gt,
                                not(),
                                br_if("aligned"),
                                get(&t),
                                f64_const(0.5),
                                curios_wasm::Instr::F64Mul,
                                set(&t),
                                br("halve"),
                            ],
                        }],
                    },
                    get(&ax),
                    get(&t),
                    curios_wasm::Instr::F64Sub,
                    set(&ax),
                    br("subtract"),
                ],
            }],
        });
        instrs.extend([get(&ax), get(&x), curios_wasm::Instr::F64Copysign]);

        self.add_helper(
            func_name,
            vec![(x, f64_val.clone()), (y, f64_val.clone())],
            f64_val.clone(),
            vec![(ax, f64_val.clone()), (ay, f64_val.clone()), (t, f64_val)],
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
        let layout = ImmediateLayout::of(grain);
        let (len_shift, slots, unit, envelope) = (
            layout.len_shift,
            layout.slots(),
            layout.unit(),
            layout.envelope,
        );
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

    /// `$list/<leaf>/to_<payload> (ref $rope/list) -> (ref $<payload>)`: a list of scalars as a host reads it, one flat element per value. The list is forced to its elements and each is narrowed as a host argument of its type is — a `Nat` or `Int` into `$longs`, read off its i31 and widened or handed to its wire helper, which refuses one past the wire — and a `Bool` into `$words`, as its word.
    pub(crate) fn emit_scalars_force_func(
        &mut self,
        leaf: WireLeaf,
        func_name: curios_wasm::FuncName,
    ) {
        let rope = self.table.list_rope();
        let payload = self.table.scalars_type(leaf);
        let r = curios_wasm::LocalName::from("r");
        let elems = curios_wasm::LocalName::from("elems");
        let out = curios_wasm::LocalName::from("out");
        let i = curios_wasm::LocalName::from("i");
        let x = curios_wasm::LocalName::from("x");
        let i31 = || curios_wasm::Instr::RefCast {
            ref_type: Table::int_type(false),
        };

        let narrow = match leaf {
            WireLeaf::Bool => vec![get(&x), i31(), curios_wasm::Instr::I31GetS],
            WireLeaf::Nat | WireLeaf::Int => {
                let wire = self.table.big_func(match leaf {
                    WireLeaf::Nat => BigHelper::NatWire,
                    _ => BigHelper::IntWire,
                });
                vec![
                    get(&x),
                    curios_wasm::Instr::RefTest {
                        ref_type: Table::int_type(false),
                    },
                    either(
                        curios_wasm::ValType::Num(curios_wasm::NumType::I64),
                        vec![
                            get(&x),
                            i31(),
                            curios_wasm::Instr::I31GetS,
                            curios_wasm::Instr::I64ExtendI32S,
                        ],
                        vec![get(&x), call(&wire)],
                    ),
                ]
            }
            WireLeaf::Bytes | WireLeaf::Bits | WireLeaf::Handle => {
                unreachable!("a list of `{leaf:?}` crosses as its payloads, never as scalars")
            }
        };

        let body = [
            get(&out),
            get(&i),
            get(&elems),
            get(&i),
            curios_wasm::Instr::ArrayGet {
                type_name: rope.payload.clone(),
            },
            set(&x),
        ]
        .into_iter()
        .chain(narrow)
        .chain([curios_wasm::Instr::ArraySet {
            type_name: payload.clone(),
        }])
        .collect();

        let instrs = [
            get(&r),
            call(&self.table.list_force_func()),
            set(&elems),
            get(&elems),
            curios_wasm::Instr::ArrayLen,
            curios_wasm::Instr::ArrayNewDefault {
                type_name: payload.clone(),
            },
            set(&out),
        ]
        .into_iter()
        .chain(each(&i, &elems, body))
        .chain([get(&out), curios_wasm::Instr::RefAsNonNull])
        .collect();

        self.add_helper(
            func_name,
            vec![(r, concrete_val(rope.base.clone(), false))],
            concrete_val(payload.clone(), false),
            vec![
                (elems, concrete_val(rope.payload.clone(), true)),
                (out, concrete_val(payload, true)),
                (i, curios_wasm::ValType::Num(curios_wasm::NumType::I32)),
                (x, Table::top_type(true)),
            ],
            instrs,
        );
    }

    /// `$list/<leaf>/of_<payload> (ref $<payload>) -> (ref $rope/list)`: a host's list of scalars back as a list, each element boxed as a scalar result is (`Table::box_word_instrs`) into a fresh payload the list's own embed places in a leaf.
    pub(crate) fn emit_scalars_embed_func(
        &mut self,
        leaf: WireLeaf,
        func_name: curios_wasm::FuncName,
    ) {
        let rope = self.table.list_rope();
        let payload = self.table.scalars_type(leaf);
        let w = curios_wasm::LocalName::from("w");
        let elems = curios_wasm::LocalName::from("elems");
        let i = curios_wasm::LocalName::from("i");

        let body = [
            get(&elems),
            get(&i),
            get(&w),
            get(&i),
            curios_wasm::Instr::ArrayGet {
                type_name: payload.clone(),
            },
        ]
        .into_iter()
        .chain(self.table.box_word_instrs(&WireType::from(leaf)))
        .chain([curios_wasm::Instr::ArraySet {
            type_name: rope.payload.clone(),
        }])
        .collect();

        let instrs = [
            get(&w),
            curios_wasm::Instr::ArrayLen,
            curios_wasm::Instr::ArrayNewDefault {
                type_name: rope.payload.clone(),
            },
            set(&elems),
        ]
        .into_iter()
        .chain(each(&i, &w, body))
        .chain([
            get(&elems),
            curios_wasm::Instr::RefAsNonNull,
            call(&self.table.list_embed_func()),
        ])
        .collect();

        self.add_helper(
            func_name,
            vec![(w, concrete_val(payload, false))],
            concrete_val(rope.base.clone(), false),
            vec![
                (elems, concrete_val(rope.payload.clone(), true)),
                (i, curios_wasm::ValType::Num(curios_wasm::NumType::I32)),
            ],
            instrs,
        );
    }

    /// `$bits/embed (ref $bytes) -> (ref $rope/bin)`: one fresh leaf sealed at eight times the payload's byte count.
    ///
    /// **The length is the payload's, scaled — not a bit count the host sent**, because the wire has no slot for one: a `Bits` row means "read these bytes as 8n bits", so a host with a 20-bit datum sends its own length in band. One `struct.new`, the same the byte grain pays; only the scale of the length field differs.
    pub(crate) fn emit_bits_embed_func(&mut self, func_name: curios_wasm::FuncName) {
        let rope = self.table.bin_rope();
        let b = curios_wasm::LocalName::from("b");

        let instrs = vec![
            curios_wasm::Instr::I32Const { value: 0 },
            get(&b),
            curios_wasm::Instr::ArrayLen,
            curios_wasm::Instr::I32Const { value: 3 },
            curios_wasm::Instr::I32Shl,
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

    /// `$list/<grain>/embed (ref $elems) -> (ref $rope/list)`: embed each raw `$bytes` element into a `$rope/bin/leaf` in place — the host-built array is fresh, nothing else aliases it — then embed the outer array into a `$rope/list/leaf`. Each element leaf is sealed at the grain's reading of its payload, exactly as `$bytes/embed` and `$bits/embed` seal a bare one.
    pub(crate) fn emit_list_bin_embed_func(
        &mut self,
        grain: Grain,
        func_name: curios_wasm::FuncName,
    ) {
        let elems = self.table.elems_type();
        let bin = self.table.bin_rope();
        let list = self.table.list_rope();

        let norm_func = match grain {
            Grain::X => self.table.bytes_norm_func(),
            Grain::B => self.table.bits_norm_func(),
        };

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

        // The leaf's length field is logical: the payload's byte count at the byte grain, eight times it at the bit grain.
        let leaf_len = match grain {
            Grain::X => vec![get(&bytes), curios_wasm::Instr::ArrayLen],
            Grain::B => vec![
                get(&bytes),
                curios_wasm::Instr::ArrayLen,
                curios_wasm::Instr::I32Const { value: 3 },
                curios_wasm::Instr::I32Shl,
            ],
        };

        let step = [
            get(&idx),
            get(&count),
            curios_wasm::Instr::I32GeU,
            curios_wasm::Instr::BrIf {
                label_name: done_label.clone(),
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
        ]
        .into_iter()
        .chain(leaf_len)
        .chain([
            get(&bytes),
            curios_wasm::Instr::RefAsNonNull,
            curios_wasm::Instr::StructNew {
                type_name: bin.leaf.clone(),
            },
            // A host-built element enters the guest world canonical: a small `Bytes`, `Bits` or `Handle` becomes the i31 here.
            curios_wasm::Instr::Call {
                func_name: norm_func,
            },
            curios_wasm::Instr::ArraySet {
                type_name: elems.clone(),
            },
            get(&idx),
            curios_wasm::Instr::I32Const { value: 1 },
            curios_wasm::Instr::I32Add,
            set(&idx),
            curios_wasm::Instr::Br {
                label_name: loop_label.clone(),
            },
        ])
        .collect::<Vec<_>>();

        let instrs = vec![
            get(&e),
            curios_wasm::Instr::ArrayLen,
            set(&count),
            curios_wasm::Instr::Block {
                label_name: done_label,
                block_type: curios_wasm::BlockType::Empty,
                instructions: vec![curios_wasm::Instr::Loop {
                    label_name: loop_label,
                    block_type: curios_wasm::BlockType::Empty,
                    instructions: step,
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

/// Run `body` once for every index `i` below the length of the array in `over`.
fn each(
    i: &curios_wasm::LocalName,
    over: &curios_wasm::LocalName,
    body: Vec<curios_wasm::Instr>,
) -> Vec<curios_wasm::Instr> {
    let step = [
        get(i),
        get(over),
        curios_wasm::Instr::ArrayLen,
        curios_wasm::Instr::I32GeU,
        br_if("done"),
    ]
    .into_iter()
    .chain(body)
    .chain([
        get(i),
        i32_const(1),
        curios_wasm::Instr::I32Add,
        set(i),
        br("each"),
    ])
    .collect();

    vec![
        i32_const(0),
        set(i),
        block("done", vec![repeat("each", step)]),
    ]
}
