use {
    super::{BlockData, CodeEmitter, Context, Frame, LoadAs, LocalData},
    crate as cont, curios_wasm as wasm,
    std::collections::HashMap,
};

#[derive(Debug)]
pub struct ExprEmitter<'a, 'b> {
    context: Context<'a, 'b>,
    module: &'b mut wasm::Module,
    expr: &'b mut wasm::Expr,
}

impl<'a, 'b> ExprEmitter<'a, 'b> {
    pub fn new(
        context: Context<'a, 'b>,
        module: &'b mut wasm::Module,
        expr: &'b mut wasm::Expr,
    ) -> Self {
        Self {
            context,
            module,
            expr,
        }
    }

    fn emit_instr(&mut self, instr: wasm::Instr) {
        if let Some(frame) = self.context.this_frame() {
            frame.instrs.push(instr);
        } else {
            self.expr.push(instr);
        }
    }

    fn emit_instrs<I>(&mut self, instrs: I)
    where
        I: IntoIterator<Item = wasm::Instr>,
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

    pub fn emit_data(&mut self, value_name: &'a cont::ValueName, value: &'a cont::Data) {
        match value {
            &cont::Data::Nat(value) => {
                if value >> 31 != 0 {
                    panic!("Nat literal {value} exceeds i31ref range");
                }

                self.emit_instrs([
                    wasm::Instr::I32Const {
                        value: value as i32,
                    },
                    wasm::Instr::RefI31,
                ])
            }
            &cont::Data::Int(value) => {
                // In-range iff bit 30 agrees with the sign bit — the signed
                // analogue of the `Nat` check above; `RefI31` would otherwise
                // silently wrap the literal to 31 bits.
                if value >> 30 != value >> 31 {
                    panic!("Int literal {value} exceeds i31ref range");
                }

                self.emit_instrs([wasm::Instr::I32Const { value }, wasm::Instr::RefI31])
            }
            &cont::Data::Flt(value) => self.emit_instrs([
                wasm::Instr::F32Const { value },
                wasm::Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                },
            ]),
            cont::Data::Arr(elems) => {
                let arr_type = self.context.table().arr_type();

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(wasm::Instr::ArrayNewFixed {
                    type_name: arr_type,
                    length: elems.len() as u32,
                });
            }
            cont::Data::Tpl(elems) => {
                let tpl_n_type = self.context.table().find_tpl_type(elems.len());

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: tpl_n_type,
                });
            }
            cont::Data::Bin(bytes) => {
                let bin_type = self.context.table().bin_type();
                let data_name = wasm::DataName::from(format!(
                    "{}${}",
                    value_name.as_string(),
                    self.module.datas().len()
                ));
                self.module.add_data(
                    data_name.clone(),
                    wasm::DataSegment {
                        bytes: bytes.clone(),
                    },
                );
                self.emit_instr(wasm::Instr::I32Const { value: 0 });
                self.emit_instr(wasm::Instr::I32Const {
                    value: bytes.len() as i32,
                });
                self.emit_instr(wasm::Instr::ArrayNewData {
                    type_name: bin_type,
                    data_name,
                });
            }
            cont::Data::Clsr(target, fields) => {
                let clsr_data = self.context.table().find_clsr(target);
                let envr_type = clsr_data.envr_type();

                self.emit_instr(wasm::Instr::RefFunc {
                    func_name: clsr_data.func_name(),
                });

                for field in fields {
                    self.emit_instrs(self.context.load_value_instrs(field, LoadAs::Null));
                }

                self.emit_instr(wasm::Instr::StructNew {
                    type_name: envr_type,
                });
            }
        }
    }

    fn emit_preallocate_clsr(
        &mut self,
        value_name: &'a cont::ValueName,
        target: &'a cont::ClsrName,
    ) {
        self.emit_instr(wasm::Instr::StructNewDefault {
            type_name: self.context.table().find_clsr(target).envr_type(),
        });

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_let_pure(&mut self, value_name: &'a cont::ValueName, value: &'a cont::Data) {
        self.emit_data(value_name, value);

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_backpatch_clsr(
        &mut self,
        value_name: &'a cont::ValueName,
        target: &'a cont::ClsrName,
        fields: &'a [cont::ValueName],
    ) {
        let clsr_data = self.context.table().find_clsr(target);
        let envr_type = clsr_data.envr_type();

        self.emit_instrs(
            self.context
                .load_value_instrs(value_name, LoadAs::Concrete(envr_type.clone())),
        );
        self.emit_instr(wasm::Instr::RefFunc {
            func_name: clsr_data.func_name(),
        });

        self.emit_instr(wasm::Instr::StructSet {
            type_name: envr_type.clone(),
            field_name: self.context.table().special_field(),
        });

        for (field, field_name) in fields.iter().zip(clsr_data.fields()) {
            self.emit_instrs(
                self.context
                    .load_value_instrs(value_name, LoadAs::Concrete(envr_type.clone())),
            );

            self.emit_instrs(self.context.load_value_instrs(field, LoadAs::Null));

            self.emit_instr(wasm::Instr::StructSet {
                type_name: envr_type.clone(),
                field_name,
            });
        }
    }

    fn emit_let_alias(&mut self, value_name: &'a cont::ValueName, source: &'a cont::ValueName) {
        self.emit_instrs(self.context.load_value_instrs(source, LoadAs::Null));

        self.emit_instr(wasm::Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    /// Allocate a fresh wasm local for `value_name` and record it in the current frame, so
    /// subsequent `find_local` lookups resolve to it. Called at the point a name is introduced
    /// — a shell or a fresh value — never for a fill, whose local its prealloc already owns.
    fn declare_local(&mut self, value_name: &'a cont::ValueName) {
        let local_name = self
            .context
            .push_local(value_name.as_str(), self.context.table().top_type(true));

        self.context
            .this_frame()
            .expect("`ExprEmitter` lacks a current frame")
            .values
            .insert(value_name, local_name);
    }

    fn emit_preallocs(&mut self, preallocs: &'a [(cont::ValueName, cont::ClsrName)]) {
        for (value_name, target) in preallocs {
            self.declare_local(value_name);
            self.emit_preallocate_clsr(value_name, target);
        }
    }

    fn emit_let_values(&mut self, values: &'a [(cont::ValueName, cont::Value)]) {
        for (value_name, value) in values {
            // An acyclic aggregate has no back-edge, so every field is already bound: build it
            // directly with a single `struct.new` / `array.new_fixed` (via `emit_data`). Only a
            // prealloc'd closure shell — a recursive capture reusing its own local — takes the
            // `new_default` + per-field `struct.set` backpatch path. Tuples and arrays are never
            // prealloc'd (cyclic ones are rejected in `to_cont`), so they always build directly.
            match value {
                cont::Value::Pure(value @ (cont::Data::Arr(_) | cont::Data::Tpl(_))) => {
                    self.declare_local(value_name);
                    self.emit_let_pure(value_name, value);
                }
                cont::Value::Pure(value @ cont::Data::Clsr(target, fields)) => {
                    if self.context.is_prealloc(value_name) {
                        self.emit_backpatch_clsr(value_name, target, fields);
                    } else {
                        self.declare_local(value_name);
                        self.emit_let_pure(value_name, value);
                    }
                }
                cont::Value::Pure(value) => {
                    self.declare_local(value_name);
                    self.emit_let_pure(value_name, value);
                }
                cont::Value::Eval(op) => {
                    self.declare_local(value_name);
                    CodeEmitter::new(&mut self.context).emit(value_name, op);
                }
                cont::Value::Alias(source) => {
                    self.declare_local(value_name);
                    self.emit_let_alias(value_name, source);
                }
            }
        }
    }

    fn emit_let_blocks(
        &mut self,
        bloink_local: wasm::LocalName,
        bloink_label: wasm::LabelName,
        blocks: Vec<(&'a cont::BlockName, BlockData<'a>)>,
        tail: &'a cont::Tail,
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
        params: HashMap<&'a cont::ValueName, LocalData>,
        region: &'a cont::Region,
    ) {
        // Locals are allocated lazily, at the point each name is introduced (see `emit_preallocs`
        // / `emit_let_values`), so the frame starts with no values and is filled as emission
        // proceeds. Allocating after `enter_frame` is what lets a single `is_prealloc` check
        // distinguish a fresh value from a fill — the current region's shells are now in scope.
        let preallocs = region.preallocs.iter().map(|(name, _)| name).collect();

        match region.blocks.as_slice() {
            [] => {
                self.context
                    .enter_frame(Frame::new(params, preallocs, vec![]));
                self.emit_preallocs(&region.preallocs);
                self.emit_let_values(&region.values);
                self.emit_instrs(self.context.tail_instrs(&region.tail));
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
                            .push_local(value_name.as_str(), self.context.table().top_type(true));

                        (value_name, LocalData::new(local_name, true))
                    })
                    .collect::<Vec<_>>();

                let block_data = BlockData::new_direct(block_name, block_params, &block.region);

                self.context.enter_frame(Frame::new(
                    params,
                    preallocs,
                    vec![(block_name, block_data.clone())],
                ));

                self.emit_preallocs(&region.preallocs);
                self.emit_let_values(&region.values);
                self.emit_direct_block(block_data, &region.tail);
            }
            _ => {
                let bloink_local = self
                    .context
                    .push_local("", wasm::ValType::Num(wasm::NumType::I32));

                let bloink_label =
                    wasm::LabelName::from(format!("region${}", bloink_local.as_str()));

                let blocks = region
                    .blocks
                    .iter()
                    .enumerate()
                    .map(|(index, (block_name, block))| {
                        let block_params = block
                            .params
                            .iter()
                            .map(|value_name| {
                                let local_name = self.context.push_local(
                                    value_name.as_str(),
                                    self.context.table().top_type(true),
                                );

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
                    .enter_frame(Frame::new(params, preallocs, frame_blocks));

                self.emit_preallocs(&region.preallocs);
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
    fn emit_direct_block(&mut self, block_data: BlockData<'a>, tail: &'a cont::Tail) {
        self.emit_region(block_data.params_map(), block_data.region);
        let body = self.context.leave_frame();

        let entry = self.context.tail_instrs(tail);

        self.emit_instr(wasm::Instr::Block {
            label_name: block_data.label_name.clone(),
            block_type: wasm::BlockType::Empty,
            instructions: entry,
        });
        self.emit_instrs(body);
    }

    pub fn emit_root_region(&mut self, region: &'a cont::Region) {
        self.emit_region(self.context.params(), region);
        self.leave_last_frame();
    }
}

/// Whether `region`, or any region nested inside its blocks, branches into
/// `block_name` — via a jump, a match arm, or a call/host resume. A single-block
/// region whose block is *not* targeted from within its own body has no back-edge,
/// so it can be emitted with `emit_direct_block` instead of the dispatcher loop.
fn region_targets_block(region: &cont::Region, block_name: &cont::BlockName) -> bool {
    fn tail_targets(tail: &cont::Tail, block_name: &cont::BlockName) -> bool {
        match tail {
            cont::Tail::Jump(target) => &target.target == block_name,
            cont::Tail::Match(target) => {
                target.cases.values().any(|jump| &jump.target == block_name)
                    || target
                        .default
                        .as_ref()
                        .is_some_and(|jump| &jump.target == block_name)
            }
            cont::Tail::Call(cont::CallTarget::Direct { resume, .. })
            | cont::Tail::Call(cont::CallTarget::Indirect { resume, .. }) => resume == block_name,
            cont::Tail::Host(host) => host.resume() == block_name,
            cont::Tail::Cell(cell) => cell.resume() == block_name,
            cont::Tail::Unreachable => false,
        }
    }

    tail_targets(&region.tail, block_name)
        || region
            .blocks
            .iter()
            .any(|(_, block)| region_targets_block(&block.region, block_name))
}
