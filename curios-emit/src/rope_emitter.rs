//! The shared rope helper functions. Straight-line sequences are inlined at their use site; anything the emitter lowers to a *loop* is a helper here instead, so its mutable scratch locals are zeroed by the fresh activation rather than leaking across executions of one call site. Among them:
//!
//! - `$<carrier>/force` flattens a `Bytes` or `List` rope to its payload array: the leaf answers its payload, a cached node answers its cache, and everything else is filled by an *iterative* tree walk (an explicit `$elems` worklist, grown by doubling), so a 100k-deep concat chain never touches the wasm call stack. The walk's first piece decides where the run goes: a flattened node whose run ends at its payload's written extent hands over that payload's room, and the walk writes the other pieces past the extent; any other first piece leaves a fresh payload, with room twice the run where the entry is a node and the piece no leaf. Only an entry *node* memoizes — intermediates are usually garbage the moment the walk passes them, and a view's fill is a single window copy of exactly its own size.
//! - `$<carrier>/exact` answers a run as a payload exactly its length — the forced payload where its array is the run's, a copy of the run otherwise — for what leaves the rope as a flat array: a leaf sealed over a forced payload, and a host operand.
//! - `$<carrier>/embed` places a host-built flat payload into a fresh leaf on re-entry.
//! - `$<carrier>/slice` builds the window, O(1) over a flat-available base: bounds-check, the trivial windows answer an empty leaf or the rope itself, a view collapses (so windows never stack), and an uncached node base is forced first — O(n) once, which memoizes — so every `view` in existence has a *flat-available* base (a leaf or a cached node).
//! - `$<carrier>/read` answers one element: off a leaf payload, *through* a view's window without forcing (the invariant above makes that O(1)), off a flattened node's cache, and at a node never flattened from the child that holds the element where that child is flat-available — so a read of what was just appended copies nothing — with `force` only where the child is not.
//! - `$bytes/eql` compares two `Bytes` ropes bytewise: unequal lengths answer without forcing, equal lengths force both payloads once and walk them.
//! - `$list/map` applies a unary closure to every element of the forced payload, filling a fresh leaf.
//! - `$reply/masks` and `$reply/bools` walk a host's flat reply for the guest's checks of it (`context/reply.rs`): every mask byte within the bits a row allows, every `List(Bool)` word `0` or `1`. Each answers whether its values hold, and the call site refuses.
//!
//! The `list/bytes` variants are the host boundary's deep forms: a `List(Bytes)` / `List(Handle)` wire value carries `Bytes`-shaped *elements*, which the host lifts and lowers as raw `$bytes` — so params force each element too, and results embed each element back. A list of scalars crosses flat, one element per value — `$longs` for a `Nat` or `Int`, `$words` for a `Bool`: `$list/<leaf>/to_<payload>` narrows each element on the way out and `$list/<leaf>/of_<payload>` boxes each on the way back, so a host never builds or reads a guest box.

mod force_walk;
use force_walk::*;

use {
    super::{
        BigHelper, Chunk, ImmediateLayout, RopeData, Scope, Table, block, br, br_if, branch, call,
        cast, concrete_val, declare_helper, either, field_get, field_set, get, i32_const, i32_type,
        i64_const, i64_type, null, repeat, set, tee, wasm, when,
    },
    curios_abi::{WireLeaf, WireType},
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

    /// Declare one helper with these parameters and locals, answering `result`.
    fn add_helper(
        &mut self,
        func_name: curios_wasm::FuncName,
        params: Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
        result: curios_wasm::ValType,
        locals: Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
        instrs: Vec<curios_wasm::Instr>,
    ) {
        declare_helper(
            self.module,
            func_name,
            Scope { params, locals },
            result,
            instrs,
        );
    }

    /// `$<carrier>/force (ref <base>) -> (ref <payload>)`.
    ///
    /// ```wat
    /// if r.tag == 0                  → r.payload      ;; leaf
    /// if r.tag == 1 && r.cache != null → r.cache      ;; already forced
    /// stack := array.new_default $elems 32            ;; explicit worklist
    /// cur   := r
    /// loop:                                           ;; per leaf-like chunk
    ///   descend: while cur is an uncached node,
    ///     push cur.right (growing the stack by doubling), cur := cur.left
    ///   at the first chunk, out := its payload's room claimed, or a fresh one
    ///   copy the chunk — a leaf payload, a node cache, or a view's window
    ///   over its flat-available base — into out at offset, unless claimed
    ///   pop cur from the stack; repeat until empty
    /// if r.tag == 1:                                  ;; memoize, release tree
    ///   r.cache := out; r.room := room; r.left := null; r.right := null
    /// ```
    ///
    /// The first chunk decides where the run goes ([`ForceWalk::place_first`]): a flattened node whose run ends at its payload's written extent hands the run its payload, the copy then writing only the pieces after it, and every other first chunk leaves the run a fresh payload. An entry *view* is not memoized (its fields are immutable): its fill is one window copy of exactly its own size, so there is nothing quadratic to fence off.
    pub(crate) fn emit_force_func(&mut self, rope: &RopeData, func_name: curios_wasm::FuncName) {
        let walk = ForceWalk::new(rope, self.table.elems_type());

        let mut instrs = walk.entry_shortcuts();
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
        // Cached node: its run, the first `len` slots of its cache.
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

        // The walk: descend to a chunk, place the run at the first, copy the chunk at the running offset, pop.
        let walk_label = curios_wasm::LabelName::from("walk");
        let mut copy = walk.place_first();
        // array.copy out[offset..] <- payload[src_off..src_off+count]
        copy.extend([
            get(&walk.out),
            get(&walk.offset),
            get(&walk.payload),
            get(&walk.src_off),
            get(&walk.count),
            curios_wasm::Instr::ArrayCopy {
                target_name: rope.payload.clone(),
                source_name: rope.payload.clone(),
            },
        ]);
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
            block("copied", copy),
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

    /// `$<carrier>/exact (ref <base>) -> (ref <payload>)`: a run as a payload exactly its length.
    ///
    /// ```wat
    /// p := force(r)
    /// if p.len == r.len → p                  ;; the payload is the run
    /// out := array.new_default <payload> r.len
    /// array.copy out[0..] <- p[0..r.len]
    /// ```
    ///
    /// A flattened node's payload may hold room past its run, which a reader of the rope never sees, since every read is bounded by a `len`. What leaves the rope as a flat array is read by its array's length instead — a leaf, whose inline read leaves its bounds to the engine's trap on the array, and a host, which lifts the whole array — so it takes the run exactly.
    pub(crate) fn emit_exact_func(
        &mut self,
        rope: &RopeData,
        func_name: curios_wasm::FuncName,
        force_func: curios_wasm::FuncName,
    ) {
        let r = curios_wasm::LocalName::from("r");
        let p = curios_wasm::LocalName::from("p");
        let n = curios_wasm::LocalName::from("n");
        let out = curios_wasm::LocalName::from("out");

        let instrs = wasm![
            get(&r),
            call(&force_func),
            set(&p),
            get(&r),
            field_get(&rope.base, &rope.len_field),
            set(&n),
            get(&p),
            curios_wasm::Instr::ArrayLen,
            get(&n),
            curios_wasm::Instr::I32Eq,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("exact"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(&p),
                    curios_wasm::Instr::RefAsNonNull,
                    curios_wasm::Instr::Return,
                ],
                else_instructions: vec![],
            },
            get(&n),
            curios_wasm::Instr::ArrayNewDefault {
                type_name: rope.payload.clone(),
            },
            set(&out),
            get(&out),
            i32_const(0),
            get(&p),
            i32_const(0),
            get(&n),
            curios_wasm::Instr::ArrayCopy {
                target_name: rope.payload.clone(),
                source_name: rope.payload.clone(),
            },
            get(&out),
            curios_wasm::Instr::RefAsNonNull,
        ];

        self.add_helper(
            func_name,
            vec![(r, concrete_val(rope.base.clone(), false))],
            concrete_val(rope.payload.clone(), false),
            vec![
                (p, concrete_val(rope.payload.clone(), true)),
                (n, i32_type()),
                (out, concrete_val(rope.payload.clone(), true)),
            ],
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
    /// The count arrives as an operand, so nothing here computes one — and a reversed window cannot be spelled.
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

        // Bounds: the eager trap (an out-of-range window must not become a deferred — or never-taken — trap). A window is `(start, count)`, so a reversed range cannot be spelled, and only running past the end is left.
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

    /// What a read does at a node that was never flattened: it answers from the child that holds `i` where that child is flat-available — a leaf, a window, a flattened node — and flattens the node only where the child is not. On the way out `p` holds the payload to read and `j` the index within it.
    ///
    /// ```wat
    /// child := r.left
    /// if i >= child.len → j := i - child.len; child := r.right
    /// child.tag == 0 → p := child.payload
    /// child.tag == 2 → j := child.offset + j; p := the payload or cache of child.base
    /// otherwise      → p := child.cache ?? (j := i; force(r))
    /// ```
    ///
    /// **One level, and no further.** A child that is itself unflattened would be read by a descent, and a walk over a deep chain that was never read would pay its depth at every read, where flattening the node makes each later read one array access. So an append then a read of the part just appended copies nothing, which is what a stream's reader and a push-then-peek loop do, and an alternation that reads the older part still flattens, at every second append — a flattening that copies only what was appended since, into the room its first piece's payload holds ([`Self::emit_force_func`]).
    fn unflattened_read_instrs(
        rope: &RopeData,
        force_func: curios_wasm::FuncName,
        r: &curios_wasm::LocalName,
        i: &curios_wasm::LocalName,
        j: &curios_wasm::LocalName,
        p: &curios_wasm::LocalName,
        sb: &curios_wasm::LocalName,
    ) -> Vec<curios_wasm::Instr> {
        let flat_base = curios_wasm::Instr::If {
            label_name: curios_wasm::LabelName::from("child_view_base"),
            block_type: curios_wasm::BlockType::Empty,
            then_instructions: vec![
                get(sb),
                cast(&rope.leaf),
                field_get(&rope.leaf, &rope.payload_field),
                set(p),
            ],
            else_instructions: vec![
                get(sb),
                cast(&rope.node),
                field_get(&rope.node, &rope.cache_field),
                set(p),
            ],
        };
        let through_view = vec![
            get(sb),
            cast(&rope.view),
            field_get(&rope.view, &rope.offset_field),
            get(j),
            curios_wasm::Instr::I32Add,
            set(j),
            get(sb),
            cast(&rope.view),
            field_get(&rope.view, &rope.base_field),
            set(sb),
            get(sb),
            field_get(&rope.base, &rope.tag_field),
            curios_wasm::Instr::I32Eqz,
            flat_base,
        ];
        let node_or_force = vec![
            get(sb),
            cast(&rope.node),
            field_get(&rope.node, &rope.cache_field),
            set(p),
            get(p),
            curios_wasm::Instr::RefIsNull,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("child_node"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(r),
                    curios_wasm::Instr::Call {
                        func_name: force_func,
                    },
                    set(p),
                    get(i),
                    set(j),
                ],
                else_instructions: vec![],
            },
        ];

        vec![
            get(r),
            cast(&rope.node),
            field_get(&rope.node, &rope.left_field),
            set(sb),
            get(i),
            get(sb),
            field_get(&rope.base, &rope.len_field),
            curios_wasm::Instr::I32GeU,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("right"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(i),
                    get(sb),
                    field_get(&rope.base, &rope.len_field),
                    curios_wasm::Instr::I32Sub,
                    set(j),
                    get(r),
                    cast(&rope.node),
                    field_get(&rope.node, &rope.right_field),
                    set(sb),
                ],
                else_instructions: vec![],
            },
            get(sb),
            field_get(&rope.base, &rope.tag_field),
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("child_leaf"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    get(sb),
                    cast(&rope.leaf),
                    field_get(&rope.leaf, &rope.payload_field),
                    set(p),
                ],
                else_instructions: vec![
                    get(sb),
                    field_get(&rope.base, &rope.tag_field),
                    curios_wasm::Instr::I32Const { value: 2 },
                    curios_wasm::Instr::I32Eq,
                    curios_wasm::Instr::If {
                        label_name: curios_wasm::LabelName::from("child_view"),
                        block_type: curios_wasm::BlockType::Empty,
                        then_instructions: through_view,
                        else_instructions: node_or_force,
                    },
                ],
            },
        ]
    }

    /// `$<carrier>/read (ref <base>, i32) -> <element>`.
    ///
    /// ```wat
    /// if i >= r.len         → unreachable             ;; the eager bounds trap
    /// if r.tag == 0 → r.payload[i]                    ;; leaf
    /// if r.tag == 2 →                                 ;; view: read through
    ///   (r.base.tag == 0 ? r.base.payload : r.base.cache)[r.offset + i]
    /// r.cache != null → r.cache[i]                    ;; node: answer the memo
    /// otherwise → the child that holds i, or force(r) ;; `unflattened_read_instrs`
    /// ```
    ///
    /// The node arm probes the cache first: a cache is written once and never cleared, so on every walk over an already-forced rope the probe halves the serial call chain a per-element read costs — which is most of what a hot descent pays.
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
            // The eager bounds trap. A leaf would trap in the engine on its own payload, but a *view* reads `base.payload[offset + i]`, and a position past the window is a position the base array still holds — so without this a read past the end answers a neighbouring element instead of refusing. What makes every read well-placed is the proof its caller discharged; this is the backstop for a wrong erasure or a wrong checker, and what makes the three carriers' reads agree.
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
                                then_instructions: Self::unflattened_read_instrs(
                                    rope, force_func, &r, &i, &j, &p, &sb,
                                ),
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

    /// `$list/bytes/force (ref $rope/list) -> (ref $elems)`: force the outer rope, then take every element exact through `$bytes/exact` into a *fresh* payload of the run's length (the shallow force of a leaf answers its live payload, which must not be element-rewritten in place).
    pub(crate) fn emit_list_bytes_force_func(&mut self, func_name: curios_wasm::FuncName) {
        let elems = self.table.elems_type();
        let list = self.table.list_rope();

        let box_func = self.table.bytes_box_func();
        let exact_func = self.table.bytes_exact_func();

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
            get(&r),
            field_get(&list.base, &list.len_field),
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
                        // An element is small-canonical, so an immediate is boxed before the deep force — the box is a cast plus the materialisation.
                        curios_wasm::Instr::Call {
                            func_name: box_func,
                        },
                        curios_wasm::Instr::Call {
                            func_name: exact_func,
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

    /// `$bytes/eql (ref $rope/bytes, ref $rope/bytes) -> i32`.
    ///
    /// ```wat
    /// if l.len != r.len → 0            ;; rope lengths answer without forcing
    /// lb := force(l); rb := force(r)
    /// loop:
    ///   if i ≥ l.len → 1               ;; every byte of the run matched
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
            // Every byte of the run matched: eq = 1, exit.
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

    /// `$bytes/and` / `/or` / `/xor (ref $payload) (ref $payload) (i32 len) (i32 count) -> (ref $rope/bytes)`: the first `count` bytes of two forced payloads combined byte for byte, sealed at `len`.
    ///
    /// **Three helpers, because a payload is all they read.** The operands arrive already forced, so what is left is a walk over two byte arrays.
    ///
    /// Both lengths are the run's own rather than the payloads' extent, which may be longer: `len`, for the leaf to carry, and `count`, the bytes the run occupies. Forcing at the call site is what keeps this a walk over payloads, and it is allowed there because a pair of calls is a straight-line sequence — the loop, which is not, stays here. Equal operand lengths are the type's to guarantee; the bound is discharged above erasure and nothing here re-checks it.
    pub(crate) fn emit_pointwise_func(
        &mut self,
        func_name: curios_wasm::FuncName,
        combine: curios_wasm::Instr,
    ) {
        let rope = self.table.bytes_rope();

        let lb = curios_wasm::LocalName::from("lb");
        let rb = curios_wasm::LocalName::from("rb");
        let len = curios_wasm::LocalName::from("len");
        let out = curios_wasm::LocalName::from("out");
        let count = curios_wasm::LocalName::from("count");
        let i = curios_wasm::LocalName::from("i");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (out.clone(), concrete_val(rope.payload.clone(), true)),
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
                (len, i32_val.clone()),
                (count, i32_val),
            ],
            concrete_val(rope.base, false),
            locals,
            instrs,
        );
    }

    /// `$<carrier>/replicate (i32 count) (i32 atom) -> (ref $rope/bytes)`: `count` copies of one generator, as one flat leaf.
    ///
    /// No loop: `array.new` is the fill, every slot the generator.
    pub(crate) fn emit_replicate_func(&mut self, func_name: curios_wasm::FuncName) {
        let rope = self.table.bytes_rope();

        let count = curios_wasm::LocalName::from("count");
        let atom = curios_wasm::LocalName::from("atom");
        let out = curios_wasm::LocalName::from("out");

        let locals = vec![(out.clone(), concrete_val(rope.payload.clone(), true))];

        let mut instrs = vec![
            get(&atom),
            get(&count),
            curios_wasm::Instr::ArrayNew {
                type_name: rope.payload.clone(),
            },
            set(&out),
        ];

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

    /// `$list/map (ref $rope/list, ref $envr/1) -> (ref $rope/list)`.
    ///
    /// ```wat
    /// selems := force(src); count := src.len
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
            get(&src),
            field_get(&rope.base, &rope.len_field),
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

    /// `$bytes/box (ref null any) -> (ref $rope/bytes)`: a small-canonical packed value as a rope. An immediate — the length in the top 2 payload bits over up to 3 bytes, LSB-first — is materialised into a fresh exact leaf; anything else casts to the rope it must be, trapping on null as a bare cast would. The payload is masked before byte extraction so the length field can never bleed into a stored byte.
    pub(crate) fn emit_box_func(&mut self, func_name: curios_wasm::FuncName) {
        let layout = ImmediateLayout::new();
        let (len_shift, payload_mask, slots) =
            (layout.len_shift, layout.payload_mask(), layout.slots());
        let rope = self.table.bytes_rope();
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

        // arr[i] = ((v & payload_mask) >> 8i) & 0xFF, for each payload byte the length says is occupied.
        let set_byte = |index: i32| {
            let mut instrs = vec![
                get(&len),
                curios_wasm::Instr::I32Const { value: index },
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
        // The payload array holds the length in bytes.
        immediate.push(get(&len));
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

    /// `$bytes/norm (ref $rope/bytes) -> (ref any)`: the canonical form of a packed rope — inside its envelope it packs into the i31, and anything longer answers itself. Called at every producer's exit, which is what makes a mixed-representation pair unrepresentable and the immediate equality one instruction.
    pub(crate) fn emit_norm_func(
        &mut self,
        func_name: curios_wasm::FuncName,
        force_func: curios_wasm::FuncName,
    ) {
        let layout = ImmediateLayout::new();
        let (len_shift, slots, envelope) = (layout.len_shift, layout.slots(), layout.envelope);
        let rope = self.table.bytes_rope();
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
                curios_wasm::Instr::I32Const { value: index },
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
            WireLeaf::Bytes | WireLeaf::Handle => {
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

        let length = vec![get(&r), field_get(&rope.base, &rope.len_field)];
        let instrs = [get(&r), call(&self.table.list_force_func()), set(&elems)]
            .into_iter()
            .chain(length.clone())
            .chain([
                curios_wasm::Instr::ArrayNewDefault {
                    type_name: payload.clone(),
                },
                set(&out),
            ])
            .chain(each(&i, length, body))
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
        .chain(each(&i, vec![get(&w), curios_wasm::Instr::ArrayLen], body))
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

    /// `$list/bytes/embed (ref $elems) -> (ref $rope/list)`: embed each raw `$bytes` element into a `$rope/bytes/leaf` in place — the host-built array is fresh, nothing else aliases it — then embed the outer array into a `$rope/list/leaf`. Each element leaf is sealed at its payload's length, exactly as `$bytes/embed` seals a bare one.
    pub(crate) fn emit_list_bytes_embed_func(&mut self, func_name: curios_wasm::FuncName) {
        let elems = self.table.elems_type();
        let rope = self.table.bytes_rope();
        let list = self.table.list_rope();

        let norm_func = self.table.bytes_norm_func();

        let e = curios_wasm::LocalName::from("e");
        let idx = curios_wasm::LocalName::from("idx");
        let count = curios_wasm::LocalName::from("count");
        let bytes = curios_wasm::LocalName::from("bytes");

        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let locals = vec![
            (idx.clone(), i32_val.clone()),
            (count.clone(), i32_val),
            (bytes.clone(), concrete_val(rope.payload.clone(), true)),
        ];

        let loop_label = curios_wasm::LabelName::from("fill");
        let done_label = curios_wasm::LabelName::from("done");

        // The leaf's length field is the payload's byte count.
        let leaf_len = vec![get(&bytes), curios_wasm::Instr::ArrayLen];

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
            cast(&rope.payload),
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
                type_name: rope.leaf.clone(),
            },
            // A host-built element enters the guest world canonical: a small `Bytes` or `Handle` becomes the i31 here.
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

/// Run `body` once for every index `i` below the count `bound` pushes, which is re-read each turn.
fn each(
    i: &curios_wasm::LocalName,
    bound: Vec<curios_wasm::Instr>,
    body: Vec<curios_wasm::Instr>,
) -> Vec<curios_wasm::Instr> {
    let step = [get(i)]
        .into_iter()
        .chain(bound)
        .chain([curios_wasm::Instr::I32GeU, br_if("done")])
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
