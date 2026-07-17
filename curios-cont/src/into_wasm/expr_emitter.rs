use {
    crate::{
        BlockData, CodeEmitter, Context, EmissionBlockName, EmissionBody, EmissionCallTarget,
        EmissionClosureName, EmissionData, EmissionHostTarget, EmissionTail, EmissionValue,
        EmissionValueName, Frame, LoadAs, LocalData, Table,
    },
    curios_base::Grain,
    std::collections::{BTreeSet, HashMap, HashSet, VecDeque},
};

#[derive(Debug)]
pub(crate) struct ExprEmitter<'a, 'b> {
    context: Context<'a, 'b>,
    module: &'b mut curios_wasm::Module,
    expr: &'b mut curios_wasm::Expr,
}

impl<'a, 'b> ExprEmitter<'a, 'b> {
    pub(crate) fn new(
        context: Context<'a, 'b>,
        module: &'b mut curios_wasm::Module,
        expr: &'b mut curios_wasm::Expr,
    ) -> Self {
        Self {
            context,
            module,
            expr,
        }
    }

    fn emit_instr(&mut self, instr: curios_wasm::Instr) {
        if let Some(frame) = self.context.this_frame() {
            frame.instrs.push(instr);
        } else {
            self.expr.push(instr);
        }
    }

    fn emit_instrs<I>(&mut self, instrs: I)
    where
        I: IntoIterator<Item = curios_wasm::Instr>,
    {
        if let Some(frame) = self.context.this_frame() {
            frame.instrs.extend(instrs);
        } else {
            self.expr.extend(instrs);
        }
    }

    fn leave_last_frame(&mut self) {
        let instrs = self.context.leave_frame();

        if self.context.this_frame().is_some() {
            panic!("`ExprEmitter` expected empty frame stack after leaving root");
        }

        self.expr.extend(instrs);
    }

    pub(crate) fn emit_data(&mut self, value_name: &'a EmissionValueName, value: &'a EmissionData) {
        match value {
            &EmissionData::Nat(value) => {
                if value >> 31 != 0 {
                    panic!("Nat literal {value} exceeds i31ref range");
                }

                self.emit_instrs([
                    curios_wasm::Instr::I32Const {
                        value: value as i32,
                    },
                    curios_wasm::Instr::RefI31,
                ])
            }
            &EmissionData::Int(value) => {
                // In-range iff bit 30 agrees with the sign bit — the signed
                // analogue of the `Nat` check above; `RefI31` would otherwise
                // silently wrap the literal to 31 bits.
                if value >> 30 != value >> 31 {
                    panic!("Int literal {value} exceeds i31ref range");
                }

                self.emit_instrs([
                    curios_wasm::Instr::I32Const { value },
                    curios_wasm::Instr::RefI31,
                ])
            }
            &EmissionData::Flt(value) => self.emit_instrs([
                curios_wasm::Instr::F32Const { value },
                curios_wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                },
            ]),
            EmissionData::Lst(elems) => {
                let rope = self.context.table().lst_rope();

                // A literal is a leaf: tag 0, the static length, the payload.
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::I32Const {
                    value: elems.len() as i32,
                });

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(curios_wasm::Instr::ArrayNewFixed {
                    type_name: rope.payload,
                    length: elems.len() as u32,
                });
                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: rope.leaf,
                });
            }
            EmissionData::Tpl(elems) => {
                let tpl_n_type = self.context.table().find_tpl_type(elems.len());

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: tpl_n_type,
                });
            }
            EmissionData::Bin(grain, value) => {
                let rope = self.context.table().bin_rope();
                let bytes = match grain {
                    Grain::B => value.to_packed_bytes(),
                    Grain::X => value
                        .to_bytes()
                        .expect("X literals are always byte-aligned"),
                };
                let payload_length = bytes.len() as i32;
                let length = value.len(*grain) as i32;
                let data_name = curios_wasm::DataName::from(format!(
                    "{}${}",
                    value_name.as_string(),
                    self.module.datas().len()
                ));
                self.module
                    .add_data(data_name.clone(), curios_wasm::DataSegment { bytes });
                // A literal is a leaf: tag 0, the static length, the payload.
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::I32Const { value: length });
                self.emit_instr(curios_wasm::Instr::I32Const { value: 0 });
                self.emit_instr(curios_wasm::Instr::I32Const {
                    value: payload_length,
                });
                self.emit_instr(curios_wasm::Instr::ArrayNewData {
                    type_name: rope.payload,
                    data_name,
                });
                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: rope.leaf,
                });
            }
            EmissionData::Closure(target, fields) => {
                let clsr_data = self.context.table().find_clsr(target);
                let envr_type = clsr_data.envr_type();

                self.emit_instr(curios_wasm::Instr::RefFunc {
                    func_name: clsr_data.func_name(),
                });

                for field in fields {
                    self.emit_instrs(self.context.load_value_instrs(field, LoadAs::Null));
                }

                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: envr_type,
                });
            }
        }
    }

    fn emit_closure_shell(
        &mut self,
        value_name: &'a EmissionValueName,
        target: &'a EmissionClosureName,
    ) {
        self.emit_instr(curios_wasm::Instr::StructNewDefault {
            type_name: self.context.table().find_clsr(target).envr_type(),
        });

        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_let_pure(&mut self, value_name: &'a EmissionValueName, value: &'a EmissionData) {
        self.emit_data(value_name, value);

        self.emit_instr(curios_wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_backpatch_clsr(
        &mut self,
        value_name: &'a EmissionValueName,
        target: &'a EmissionClosureName,
        fields: &'a [EmissionValueName],
    ) {
        let clsr_data = self.context.table().find_clsr(target);
        let envr_type = clsr_data.envr_type();

        self.emit_instrs(
            self.context
                .load_value_instrs(value_name, LoadAs::Concrete(envr_type.clone())),
        );
        self.emit_instr(curios_wasm::Instr::RefFunc {
            func_name: clsr_data.func_name(),
        });

        self.emit_instr(curios_wasm::Instr::StructSet {
            type_name: envr_type.clone(),
            field_name: self.context.table().special_field(),
        });

        for (field, field_name) in fields.iter().zip(clsr_data.fields()) {
            self.emit_instrs(
                self.context
                    .load_value_instrs(value_name, LoadAs::Concrete(envr_type.clone())),
            );

            self.emit_instrs(self.context.load_value_instrs(field, LoadAs::Null));

            self.emit_instr(curios_wasm::Instr::StructSet {
                type_name: envr_type.clone(),
                field_name,
            });
        }
    }

    /// Allocate a fresh wasm local for `value_name` and record it in the current frame, so
    /// subsequent `find_local` lookups resolve to it. Called at the point a name is introduced
    /// — a shell or a fresh value — never for a fill, whose local its shell already owns.
    fn declare_local(&mut self, value_name: &'a EmissionValueName) {
        let local_name = self
            .context
            .push_local(value_name.as_str(), Table::top_type(true));

        self.context
            .this_frame()
            .expect("`ExprEmitter` lacks a current frame")
            .values
            .insert(value_name, local_name);
    }

    fn emit_shells(&mut self, shells: &'a [(EmissionValueName, EmissionClosureName)]) {
        for (value_name, target) in shells {
            self.declare_local(value_name);
            self.emit_closure_shell(value_name, target);
        }
    }

    fn emit_let_values(&mut self, values: &'a [(EmissionValueName, EmissionValue)]) {
        for (value_name, value) in values {
            // An acyclic aggregate has no back-edge, so every field is already bound: build it
            // directly with a single `struct.new` / `array.new_fixed` (via `emit_data`). Only a
            // shell'd closure shell — a recursive capture reusing its own local — takes the
            // `new_default` + per-field `struct.set` backpatch path. Tuples and arrays are never
            // shell'd (cyclic ones are rejected in `into_cont`), so they always build directly.
            match value {
                EmissionValue::Pure(value @ (EmissionData::Lst(_) | EmissionData::Tpl(_))) => {
                    self.declare_local(value_name);
                    self.emit_let_pure(value_name, value);
                }
                EmissionValue::Pure(value @ EmissionData::Closure(target, fields)) => {
                    if self.context.is_shell(value_name) {
                        self.emit_backpatch_clsr(value_name, target, fields);
                    } else {
                        self.declare_local(value_name);
                        self.emit_let_pure(value_name, value);
                    }
                }
                EmissionValue::Pure(value) => {
                    self.declare_local(value_name);
                    self.emit_let_pure(value_name, value);
                }
                EmissionValue::Eval(op) => {
                    self.declare_local(value_name);
                    CodeEmitter::new(&mut self.context).emit(value_name, op);
                }
            }
        }
    }

    fn emit_let_blocks(
        &mut self,
        bloink_local: curios_wasm::LocalName,
        bloink_label: curios_wasm::LabelName,
        blocks: Vec<(&'a EmissionBlockName, BlockData<'a>)>,
        tail: &'a EmissionTail,
    ) {
        let regions = blocks
            .iter()
            .map(|(_, block_data)| {
                self.emit_region(block_data.params_map(), block_data.region);

                (block_data.label_name.clone(), self.context.leave_frame())
            })
            .collect();

        self.emit_instrs(
            self.context
                .bloink_instrs(bloink_local, bloink_label, regions, tail),
        );
    }

    fn emit_region(
        &mut self,
        params: HashMap<&'a EmissionValueName, LocalData>,
        region: &'a EmissionBody,
    ) {
        // Locals are allocated lazily, at the point each name is introduced (see `emit_shells`
        // / `emit_let_values`), so the frame starts with no values and is filled as emission
        // proceeds. Allocating after `enter_frame` is what lets a single `is_shell` check
        // distinguish a fresh value from a fill — the current region's shells are now in scope.
        let shells = region.shells.iter().map(|(name, _)| name).collect();
        let acyclic_order = acyclic_block_order(region);
        let natural_loop_plan = natural_loop_plan(region);

        match region.blocks.as_slice() {
            [] => {
                self.context.enter_frame(Frame::new(params, shells, vec![]));
                self.emit_shells(&region.shells);
                self.emit_let_values(&region.values);
                self.emit_instrs(self.context.tail_instrs(&region.tail));
            }
            [(block_name, block)]
                if region_targets_block(&block.region, block_name)
                    && matches!(
                        &region.tail,
                        EmissionTail::Jump(target) if &target.target == block_name
                    ) =>
            {
                let block_params = block
                    .params
                    .iter()
                    .map(|value_name| {
                        let local_name = self
                            .context
                            .push_local(value_name.as_str(), Table::top_type(true));
                        (value_name, LocalData::new(local_name, true))
                    })
                    .collect::<Vec<_>>();
                let block_data =
                    BlockData::new_natural_loop(block_name, block_params, &block.region);
                self.context.enter_frame(Frame::new(
                    params,
                    shells,
                    vec![(block_name, block_data.clone())],
                ));
                self.emit_shells(&region.shells);
                self.emit_let_values(&region.values);
                self.emit_natural_loop(block_data, &region.tail);
            }
            // A region with a single block whose body never targets it has no
            // back-edge: control only ever flows *forward* into the block, so the
            // trampolining `loop` + `br_table` + `-1` seed collapse to a plain
            // `block` the entry branches out of. See `emit_direct_block`.
            [(block_name, block)] if !region_targets_block(&block.region, block_name) => {
                let block_params = block
                    .params
                    .iter()
                    .map(|value_name| {
                        let local_name = self
                            .context
                            .push_local(value_name.as_str(), Table::top_type(true));

                        (value_name, LocalData::new(local_name, true))
                    })
                    .collect::<Vec<_>>();

                let block_data = BlockData::new_direct(block_name, block_params, &block.region);

                self.context.enter_frame(Frame::new(
                    params,
                    shells,
                    vec![(block_name, block_data.clone())],
                ));

                self.emit_shells(&region.shells);
                self.emit_let_values(&region.values);
                self.emit_direct_block(block_data, &region.tail);
            }
            _ if acyclic_order.is_some() => {
                self.emit_acyclic_blocks(params, shells, region, acyclic_order.unwrap());
            }
            _ if natural_loop_plan.is_some() => {
                self.emit_structured_loop(params, shells, region, natural_loop_plan.unwrap());
            }
            _ => {
                let bloink_local = self
                    .context
                    .push_local("", curios_wasm::ValType::Num(curios_wasm::NumType::I32));

                let bloink_label =
                    curios_wasm::LabelName::from(format!("region${}", bloink_local.as_str()));

                let blocks = region
                    .blocks
                    .iter()
                    .enumerate()
                    .map(|(index, (block_name, block))| {
                        let block_params = block
                            .params
                            .iter()
                            .map(|value_name| {
                                let local_name = self
                                    .context
                                    .push_local(value_name.as_str(), Table::top_type(true));

                                (value_name, LocalData::new(local_name, true))
                            })
                            .collect::<Vec<_>>();

                        let block_data = BlockData::new(
                            bloink_label.clone(),
                            bloink_local.clone(),
                            index,
                            block_name,
                            block_params,
                            &block.region,
                        );

                        (block_name, block_data)
                    })
                    .collect::<Vec<_>>();

                let frame_blocks = blocks
                    .iter()
                    .map(|(block_name, block_data)| (*block_name, block_data.clone()))
                    .collect::<Vec<_>>();

                self.context
                    .enter_frame(Frame::new(params, shells, frame_blocks));

                self.emit_shells(&region.shells);
                self.emit_let_values(&region.values);
                self.emit_let_blocks(bloink_local, bloink_label, blocks, &region.tail);
            }
        }
    }

    /// Emit a single-target region (one block, no back-edge). The block body is
    /// laid out *after* a `block` wrapping the region's tail, so the tail — the
    /// entry — runs first and reaches the body by branching forward out of the
    /// block label, with no dispatcher loop:
    ///
    /// ```wat
    /// (block $b  ;; the region's tail (entry); `br $b` exits here
    ///   <tail>)
    /// <body>     ;; the block body
    /// ```
    fn emit_direct_block(&mut self, block_data: BlockData<'a>, tail: &'a EmissionTail) {
        self.emit_region(block_data.params_map(), block_data.region);
        let body = self.context.leave_frame();

        let entry = self.context.tail_instrs(tail);

        self.emit_instr(curios_wasm::Instr::Block {
            label_name: block_data.label_name.clone(),
            block_type: curios_wasm::BlockType::Empty,
            instructions: entry,
        });
        self.emit_instrs(body);
    }

    fn emit_natural_loop(&mut self, block_data: BlockData<'a>, tail: &'a EmissionTail) {
        let EmissionTail::Jump(target) = tail else {
            unreachable!()
        };
        for value in &target.params {
            self.emit_instrs(self.context.load_value_instrs(value, LoadAs::NonNull));
        }
        self.emit_instrs(block_data.bind(target.params.len()));

        self.emit_region(block_data.params_map(), block_data.region);
        let body = self.context.leave_frame();
        self.emit_instr(curios_wasm::Instr::Loop {
            label_name: block_data.label_name,
            block_type: curios_wasm::BlockType::Empty,
            instructions: body,
        });
        self.emit_instr(curios_wasm::Instr::Unreachable);
    }

    fn emit_acyclic_blocks(
        &mut self,
        params: HashMap<&'a EmissionValueName, LocalData>,
        shells: HashSet<&'a EmissionValueName>,
        region: &'a EmissionBody,
        order: Vec<usize>,
    ) {
        let blocks = region
            .blocks
            .iter()
            .map(|(block_name, block)| {
                let block_params = block
                    .params
                    .iter()
                    .map(|value_name| {
                        let local_name = self
                            .context
                            .push_local(value_name.as_str(), Table::top_type(true));
                        (value_name, LocalData::new(local_name, true))
                    })
                    .collect::<Vec<_>>();
                (
                    block_name,
                    BlockData::new_direct(block_name, block_params, &block.region),
                )
            })
            .collect::<Vec<_>>();
        self.context.enter_frame(Frame::new(
            params,
            shells,
            blocks
                .iter()
                .map(|(block_name, block)| (*block_name, block.clone()))
                .collect(),
        ));
        self.emit_shells(&region.shells);
        self.emit_let_values(&region.values);

        let bodies = order
            .iter()
            .map(|&index| {
                let block = &blocks[index].1;
                self.emit_region(block.params_map(), block.region);
                self.context.leave_frame()
            })
            .collect::<Vec<_>>();
        let mut instructions = self.context.tail_instrs(&region.tail);
        for (&index, body) in order.iter().zip(bodies) {
            instructions = std::iter::once(curios_wasm::Instr::Block {
                label_name: blocks[index].1.label_name.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions,
            })
            .chain(body)
            .collect();
        }
        self.emit_instrs(instructions);
    }

    fn emit_structured_loop(
        &mut self,
        params: HashMap<&'a EmissionValueName, LocalData>,
        shells: HashSet<&'a EmissionValueName>,
        region: &'a EmissionBody,
        plan: NaturalLoopPlan,
    ) {
        let blocks = region
            .blocks
            .iter()
            .enumerate()
            .map(|(index, (block_name, block))| {
                let block_params = block
                    .params
                    .iter()
                    .map(|value_name| {
                        let local_name = self
                            .context
                            .push_local(value_name.as_str(), Table::top_type(true));
                        (value_name, LocalData::new(local_name, true))
                    })
                    .collect::<Vec<_>>();
                let block = if index == plan.header {
                    BlockData::new_natural_loop(block_name, block_params, &block.region)
                } else {
                    BlockData::new_direct(block_name, block_params, &block.region)
                };
                (block_name, block)
            })
            .collect::<Vec<_>>();
        self.context.enter_frame(Frame::new(
            params,
            shells,
            blocks
                .iter()
                .map(|(block_name, block)| (*block_name, block.clone()))
                .collect(),
        ));
        self.emit_shells(&region.shells);
        self.emit_let_values(&region.values);

        let bodies = blocks
            .iter()
            .map(|(_, block)| {
                self.emit_region(block.params_map(), block.region);
                self.context.leave_frame()
            })
            .collect::<Vec<_>>();
        let EmissionTail::Jump(entry) = &region.tail else {
            unreachable!()
        };
        let mut prefix = entry
            .params
            .iter()
            .flat_map(|value| self.context.load_value_instrs(value, LoadAs::NonNull))
            .collect::<Vec<_>>();
        prefix.extend(blocks[plan.header].1.bind(entry.params.len()));

        let mut loop_body = bodies[plan.header].clone();
        for &block in &plan.body_order {
            loop_body = std::iter::once(curios_wasm::Instr::Block {
                label_name: blocks[block].1.label_name.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions: loop_body,
            })
            .chain(bodies[block].clone())
            .collect();
        }
        let mut instructions = vec![
            curios_wasm::Instr::Loop {
                label_name: blocks[plan.header].1.label_name.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions: loop_body,
            },
            curios_wasm::Instr::Unreachable,
        ];
        for &block in &plan.exit_order {
            instructions = std::iter::once(curios_wasm::Instr::Block {
                label_name: blocks[block].1.label_name.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions,
            })
            .chain(bodies[block].clone())
            .collect();
        }
        self.emit_instrs(prefix.into_iter().chain(instructions));
    }

    pub(crate) fn emit_root_region(&mut self, region: &'a EmissionBody) {
        self.emit_region(self.context.params(), region);
        self.leave_last_frame();
    }
}

/// Whether `region`, or any region nested inside its blocks, branches into
/// `block_name` — via a jump, a match arm, or a call/host resume. A single-block
/// region whose block is *not* targeted from within its own body has no back-edge,
/// so it can be emitted with `emit_direct_block` instead of the dispatcher loop.
fn region_targets_block(region: &EmissionBody, block_name: &EmissionBlockName) -> bool {
    fn tail_targets(tail: &EmissionTail, block_name: &EmissionBlockName) -> bool {
        match tail {
            EmissionTail::Jump(target) => &target.target == block_name,
            EmissionTail::Match(target) => {
                target.cases.values().any(|jump| &jump.target == block_name)
                    || target
                        .default
                        .as_ref()
                        .is_some_and(|jump| &jump.target == block_name)
            }
            EmissionTail::Call(EmissionCallTarget::Direct { resume, .. })
            | EmissionTail::Call(EmissionCallTarget::Indirect { resume, .. }) => {
                resume == block_name
            }
            EmissionTail::Host(EmissionHostTarget::Foreign { resume, .. }) => resume == block_name,
            EmissionTail::Host(EmissionHostTarget::IoExit { .. }) => false,
            EmissionTail::Cell(cell) => cell.resume() == block_name,
            EmissionTail::Unreachable => false,
        }
    }

    tail_targets(&region.tail, block_name)
        || region
            .blocks
            .iter()
            .any(|(_, block)| region_targets_block(&block.region, block_name))
}

fn acyclic_block_order(region: &EmissionBody) -> Option<Vec<usize>> {
    let successors = block_graph(region);
    topological_order(&successors, &(0..region.blocks.len()).collect(), None)
}

#[derive(Debug)]
struct NaturalLoopPlan {
    header: usize,
    body_order: Vec<usize>,
    exit_order: Vec<usize>,
}

fn natural_loop_plan(region: &EmissionBody) -> Option<NaturalLoopPlan> {
    let EmissionTail::Jump(entry) = &region.tail else {
        return None;
    };
    let header = region
        .blocks
        .iter()
        .position(|(block, _)| block == &entry.target)?;
    let successors = block_graph(region);
    let mut reachable = vec![vec![false; successors.len()]; successors.len()];
    for start in 0..successors.len() {
        let mut work = vec![start];
        while let Some(block) = work.pop() {
            if reachable[start][block] {
                continue;
            }
            reachable[start][block] = true;
            work.extend(successors[block].iter().copied());
        }
    }

    let mut remaining = (0..successors.len()).collect::<BTreeSet<_>>();
    let mut cyclic = Vec::new();
    while let Some(&seed) = remaining.first() {
        let component = remaining
            .iter()
            .copied()
            .filter(|&block| reachable[seed][block] && reachable[block][seed])
            .collect::<BTreeSet<_>>();
        for block in &component {
            remaining.remove(block);
        }
        if component.len() > 1 || successors[seed].contains(&seed) {
            cyclic.push(component);
        }
    }
    let [loop_blocks] = cyclic.as_slice() else {
        return None;
    };
    if !loop_blocks.contains(&header) {
        return None;
    }
    for (source, targets) in successors.iter().enumerate() {
        if loop_blocks.contains(&source) {
            continue;
        }
        if targets
            .iter()
            .any(|target| loop_blocks.contains(target) && *target != header)
        {
            return None;
        }
    }

    let body_order = topological_order(&successors, loop_blocks, Some(header))?;
    if body_order.first() != Some(&header) {
        return None;
    }
    let exits = (0..successors.len())
        .filter(|block| !loop_blocks.contains(block))
        .collect::<BTreeSet<_>>();
    let exit_order = topological_order(&successors, &exits, None)?;
    Some(NaturalLoopPlan {
        header,
        body_order: body_order.into_iter().skip(1).collect(),
        exit_order,
    })
}

fn block_graph(region: &EmissionBody) -> Vec<BTreeSet<usize>> {
    region
        .blocks
        .iter()
        .map(|(_, block)| {
            region
                .blocks
                .iter()
                .enumerate()
                .filter_map(|(target, (block_name, _))| {
                    region_targets_block(&block.region, block_name).then_some(target)
                })
                .collect()
        })
        .collect()
}

fn topological_order(
    successors: &[BTreeSet<usize>],
    members: &BTreeSet<usize>,
    ignored_target: Option<usize>,
) -> Option<Vec<usize>> {
    let mut indegree = vec![0usize; successors.len()];
    for &source in members {
        for &target in &successors[source] {
            if members.contains(&target) && Some(target) != ignored_target {
                indegree[target] += 1;
            }
        }
    }

    let mut ready = members
        .iter()
        .copied()
        .filter(|&block| indegree[block] == 0)
        .collect::<VecDeque<_>>();
    let mut order = Vec::with_capacity(members.len());
    while let Some(block) = ready.pop_front() {
        order.push(block);
        for &successor in &successors[block] {
            if members.contains(&successor) && Some(successor) != ignored_target {
                indegree[successor] -= 1;
            }
            if members.contains(&successor)
                && Some(successor) != ignored_target
                && indegree[successor] == 0
            {
                ready.push_back(successor);
            }
        }
    }
    (order.len() == members.len()).then_some(order)
}

#[cfg(test)]
mod tests {
    use {
        super::{acyclic_block_order, natural_loop_plan},
        crate::{
            EmissionBlock, EmissionBlockName, EmissionBody, EmissionJumpTarget,
            EmissionMatchTarget, EmissionTail, EmissionValueName,
        },
        std::collections::BTreeMap,
    };

    fn body(tail: EmissionTail) -> EmissionBody {
        EmissionBody {
            shells: vec![],
            values: vec![],
            blocks: vec![],
            tail,
        }
    }

    fn jump(target: &EmissionBlockName) -> EmissionTail {
        EmissionTail::Jump(EmissionJumpTarget {
            target: target.clone(),
            params: vec![],
        })
    }

    #[test]
    fn orders_acyclic_machine_blocks_without_a_dispatcher() {
        let first = EmissionBlockName::from("first");
        let second = EmissionBlockName::from("second");
        let region = EmissionBody {
            shells: vec![],
            values: vec![],
            blocks: vec![
                (
                    first.clone(),
                    EmissionBlock {
                        params: vec![],
                        region: body(jump(&second)),
                    },
                ),
                (
                    second,
                    EmissionBlock {
                        params: vec![],
                        region: body(EmissionTail::Unreachable),
                    },
                ),
            ],
            tail: jump(&first),
        };

        assert_eq!(acyclic_block_order(&region), Some(vec![0, 1]));
        assert!(natural_loop_plan(&region).is_none());
    }

    #[test]
    fn finds_a_single_entry_loop_and_keeps_its_exit_outside() {
        let header = EmissionBlockName::from("header");
        let repeat = EmissionBlockName::from("repeat");
        let exit = EmissionBlockName::from("exit");
        let region = EmissionBody {
            shells: vec![],
            values: vec![],
            blocks: vec![
                (
                    header.clone(),
                    EmissionBlock {
                        params: vec![],
                        region: body(EmissionTail::Match(EmissionMatchTarget {
                            operand: EmissionValueName::from("condition"),
                            cases: BTreeMap::from([(
                                0,
                                EmissionJumpTarget {
                                    target: exit.clone(),
                                    params: vec![],
                                },
                            )]),
                            default: Some(EmissionJumpTarget {
                                target: repeat.clone(),
                                params: vec![],
                            }),
                        })),
                    },
                ),
                (
                    repeat,
                    EmissionBlock {
                        params: vec![],
                        region: body(jump(&header)),
                    },
                ),
                (
                    exit,
                    EmissionBlock {
                        params: vec![],
                        region: body(EmissionTail::Unreachable),
                    },
                ),
            ],
            tail: jump(&header),
        };

        let plan = natural_loop_plan(&region).expect("loop is reducible");
        assert_eq!(plan.header, 0);
        assert_eq!(plan.body_order, vec![1]);
        assert_eq!(plan.exit_order, vec![2]);
    }

    #[test]
    fn leaves_a_multi_entry_cycle_for_local_dispatch() {
        let left = EmissionBlockName::from("left");
        let right = EmissionBlockName::from("right");
        let region = EmissionBody {
            shells: vec![],
            values: vec![],
            blocks: vec![
                (
                    left.clone(),
                    EmissionBlock {
                        params: vec![],
                        region: body(jump(&right)),
                    },
                ),
                (
                    right.clone(),
                    EmissionBlock {
                        params: vec![],
                        region: body(jump(&left)),
                    },
                ),
            ],
            tail: EmissionTail::Match(EmissionMatchTarget {
                operand: EmissionValueName::from("entry"),
                cases: BTreeMap::from([(
                    0,
                    EmissionJumpTarget {
                        target: left,
                        params: vec![],
                    },
                )]),
                default: Some(EmissionJumpTarget {
                    target: right,
                    params: vec![],
                }),
            }),
        };

        assert!(acyclic_block_order(&region).is_none());
        assert!(natural_loop_plan(&region).is_none());
    }
}
