use {
    super::{BlockData, CodeEmitter, Context, Frame, LoadAs, LocalData, Table},
    curios_wasm::{
        BlockType, DataName, DataSegment, Expr, Instr, LabelName, LocalName, Module, NumType,
        ValType,
    },
    std::collections::HashMap,
};

#[derive(Debug)]
pub(super) struct ExprEmitter<'a, 'b> {
    context: Context<'a, 'b>,
    module: &'b mut Module,
    expr: &'b mut Expr,
}

impl<'a, 'b> ExprEmitter<'a, 'b> {
    pub(super) fn new(
        context: Context<'a, 'b>,
        module: &'b mut Module,
        expr: &'b mut Expr,
    ) -> Self {
        Self {
            context,
            module,
            expr,
        }
    }

    fn emit_instr(&mut self, instr: Instr) {
        if let Some(frame) = self.context.this_frame() {
            frame.instrs.push(instr);
        } else {
            self.expr.push(instr);
        }
    }

    fn emit_instrs<I>(&mut self, instrs: I)
    where
        I: IntoIterator<Item = Instr>,
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

    pub(super) fn emit_data(&mut self, value_name: &'a crate::ValueName, value: &'a crate::Data) {
        match value {
            &crate::Data::Nat(value) => {
                if value >> 31 != 0 {
                    panic!("Nat literal {value} exceeds i31ref range");
                }

                self.emit_instrs([
                    Instr::I32Const {
                        value: value as i32,
                    },
                    Instr::RefI31,
                ])
            }
            &crate::Data::Int(value) => {
                // In-range iff bit 30 agrees with the sign bit — the signed
                // analogue of the `Nat` check above; `RefI31` would otherwise
                // silently wrap the literal to 31 bits.
                if value >> 30 != value >> 31 {
                    panic!("Int literal {value} exceeds i31ref range");
                }

                self.emit_instrs([Instr::I32Const { value }, Instr::RefI31])
            }
            &crate::Data::Flt(value) => self.emit_instrs([
                Instr::F32Const { value },
                Instr::StructNew {
                    type_name: self.context.table().flt_type(),
                },
            ]),
            crate::Data::Lst(elems) => {
                let rope = self.context.table().lst_rope();

                // A literal is a leaf: tag 0, the static length, the payload.
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instr(Instr::I32Const {
                    value: elems.len() as i32,
                });

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(Instr::ArrayNewFixed {
                    type_name: rope.payload,
                    length: elems.len() as u32,
                });
                self.emit_instr(Instr::StructNew {
                    type_name: rope.leaf,
                });
            }
            crate::Data::Tpl(elems) => {
                let tpl_n_type = self.context.table().find_tpl_type(elems.len());

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(Instr::StructNew {
                    type_name: tpl_n_type,
                });
            }
            crate::Data::Bin(bytes) => {
                let rope = self.context.table().bin_rope();
                let data_name = DataName::from(format!(
                    "{}${}",
                    value_name.as_string(),
                    self.module.datas().len()
                ));
                self.module.add_data(
                    data_name.clone(),
                    DataSegment {
                        bytes: bytes.clone(),
                    },
                );
                // A literal is a leaf: tag 0, the static length, the payload.
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instr(Instr::I32Const {
                    value: bytes.len() as i32,
                });
                self.emit_instr(Instr::I32Const { value: 0 });
                self.emit_instr(Instr::I32Const {
                    value: bytes.len() as i32,
                });
                self.emit_instr(Instr::ArrayNewData {
                    type_name: rope.payload,
                    data_name,
                });
                self.emit_instr(Instr::StructNew {
                    type_name: rope.leaf,
                });
            }
            crate::Data::Clsr(target, fields) => {
                let clsr_data = self.context.table().find_clsr(target);
                let envr_type = clsr_data.envr_type();

                self.emit_instr(Instr::RefFunc {
                    func_name: clsr_data.func_name(),
                });

                for field in fields {
                    self.emit_instrs(self.context.load_value_instrs(field, LoadAs::Null));
                }

                self.emit_instr(Instr::StructNew {
                    type_name: envr_type,
                });
            }
        }
    }

    fn emit_preallocate_clsr(
        &mut self,
        value_name: &'a crate::ValueName,
        target: &'a crate::ClsrName,
    ) {
        self.emit_instr(Instr::StructNewDefault {
            type_name: self.context.table().find_clsr(target).envr_type(),
        });

        self.emit_instr(Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_let_pure(&mut self, value_name: &'a crate::ValueName, value: &'a crate::Data) {
        self.emit_data(value_name, value);

        self.emit_instr(Instr::LocalSet {
            local_name: self
                .context
                .find_local(value_name)
                .map(|local_data| local_data.local_name)
                .unwrap_or_else(|| panic!("`ExprEmitter` lacks local `{}`", value_name)),
        });
    }

    fn emit_backpatch_clsr(
        &mut self,
        value_name: &'a crate::ValueName,
        target: &'a crate::ClsrName,
        fields: &'a [crate::ValueName],
    ) {
        let clsr_data = self.context.table().find_clsr(target);
        let envr_type = clsr_data.envr_type();

        self.emit_instrs(
            self.context
                .load_value_instrs(value_name, LoadAs::Concrete(envr_type.clone())),
        );
        self.emit_instr(Instr::RefFunc {
            func_name: clsr_data.func_name(),
        });

        self.emit_instr(Instr::StructSet {
            type_name: envr_type.clone(),
            field_name: self.context.table().special_field(),
        });

        for (field, field_name) in fields.iter().zip(clsr_data.fields()) {
            self.emit_instrs(
                self.context
                    .load_value_instrs(value_name, LoadAs::Concrete(envr_type.clone())),
            );

            self.emit_instrs(self.context.load_value_instrs(field, LoadAs::Null));

            self.emit_instr(Instr::StructSet {
                type_name: envr_type.clone(),
                field_name,
            });
        }
    }

    fn emit_let_alias(&mut self, value_name: &'a crate::ValueName, source: &'a crate::ValueName) {
        self.emit_instrs(self.context.load_value_instrs(source, LoadAs::Null));

        self.emit_instr(Instr::LocalSet {
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
    fn declare_local(&mut self, value_name: &'a crate::ValueName) {
        let local_name = self
            .context
            .push_local(value_name.as_str(), Table::top_type(true));

        self.context
            .this_frame()
            .expect("`ExprEmitter` lacks a current frame")
            .values
            .insert(value_name, local_name);
    }

    fn emit_preallocs(&mut self, preallocs: &'a [(crate::ValueName, crate::ClsrName)]) {
        for (value_name, target) in preallocs {
            self.declare_local(value_name);
            self.emit_preallocate_clsr(value_name, target);
        }
    }

    fn emit_let_values(&mut self, values: &'a [(crate::ValueName, crate::Value)]) {
        for (value_name, value) in values {
            // An acyclic aggregate has no back-edge, so every field is already bound: build it
            // directly with a single `struct.new` / `array.new_fixed` (via `emit_data`). Only a
            // prealloc'd closure shell — a recursive capture reusing its own local — takes the
            // `new_default` + per-field `struct.set` backpatch path. Tuples and arrays are never
            // prealloc'd (cyclic ones are rejected in `into_cont`), so they always build directly.
            match value {
                crate::Value::Pure(value @ (crate::Data::Lst(_) | crate::Data::Tpl(_))) => {
                    self.declare_local(value_name);
                    self.emit_let_pure(value_name, value);
                }
                crate::Value::Pure(value @ crate::Data::Clsr(target, fields)) => {
                    if self.context.is_prealloc(value_name) {
                        self.emit_backpatch_clsr(value_name, target, fields);
                    } else {
                        self.declare_local(value_name);
                        self.emit_let_pure(value_name, value);
                    }
                }
                crate::Value::Pure(value) => {
                    self.declare_local(value_name);
                    self.emit_let_pure(value_name, value);
                }
                crate::Value::Eval(op) => {
                    self.declare_local(value_name);
                    CodeEmitter::new(&mut self.context).emit(value_name, op);
                }
                crate::Value::Alias(source) => {
                    self.declare_local(value_name);
                    self.emit_let_alias(value_name, source);
                }
            }
        }
    }

    fn emit_let_blocks(
        &mut self,
        bloink_local: LocalName,
        bloink_label: LabelName,
        blocks: Vec<(&'a crate::BlockName, BlockData<'a>)>,
        tail: &'a crate::Tail,
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
        params: HashMap<&'a crate::ValueName, LocalData>,
        region: &'a crate::Region,
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
                            .push_local(value_name.as_str(), Table::top_type(true));

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
                let bloink_local = self.context.push_local("", ValType::Num(NumType::I32));

                let bloink_label = LabelName::from(format!("region${}", bloink_local.as_str()));

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
    fn emit_direct_block(&mut self, block_data: BlockData<'a>, tail: &'a crate::Tail) {
        self.emit_region(block_data.params_map(), block_data.region);
        let body = self.context.leave_frame();

        let entry = self.context.tail_instrs(tail);

        self.emit_instr(Instr::Block {
            label_name: block_data.label_name.clone(),
            block_type: BlockType::Empty,
            instructions: entry,
        });
        self.emit_instrs(body);
    }

    pub(super) fn emit_root_region(&mut self, region: &'a crate::Region) {
        self.emit_region(self.context.params(), region);
        self.leave_last_frame();
    }
}

/// Whether `region`, or any region nested inside its blocks, branches into
/// `block_name` — via a jump, a match arm, or a call/host resume. A single-block
/// region whose block is *not* targeted from within its own body has no back-edge,
/// so it can be emitted with `emit_direct_block` instead of the dispatcher loop.
fn region_targets_block(region: &crate::Region, block_name: &crate::BlockName) -> bool {
    fn tail_targets(tail: &crate::Tail, block_name: &crate::BlockName) -> bool {
        match tail {
            crate::Tail::Jump(target) => &target.target == block_name,
            crate::Tail::Match(target) => {
                target.cases.values().any(|jump| &jump.target == block_name)
                    || target
                        .default
                        .as_ref()
                        .is_some_and(|jump| &jump.target == block_name)
            }
            crate::Tail::Call(crate::CallTarget::Direct { resume, .. })
            | crate::Tail::Call(crate::CallTarget::Indirect { resume, .. }) => resume == block_name,
            crate::Tail::Host(host) => host.resume() == block_name,
            crate::Tail::Cell(cell) => cell.resume() == block_name,
            crate::Tail::Unreachable => false,
        }
    }

    tail_targets(&region.tail, block_name)
        || region
            .blocks
            .iter()
            .any(|(_, block)| region_targets_block(&block.region, block_name))
}
