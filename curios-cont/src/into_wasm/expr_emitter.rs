use {
    super::{
        BlockData, CodeEmitter, Context, EmissionBlockName, EmissionBody, EmissionClosureName,
        EmissionData, EmissionValue, EmissionValueName, Frame, LayoutItem, LoadAs, LocalData,
        region_layout,
    },
    curios_utilities::Grain,
    std::collections::{BTreeMap, HashMap, HashSet},
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
                // A folded u32 value the i31 carrier cannot box traps at its materialization point — the same backend boundary where the checked runtime computation of the value would have trapped.
                if value >> 31 != 0 {
                    self.emit_instrs([curios_wasm::Instr::Unreachable]);
                    return;
                }

                self.emit_instrs([
                    curios_wasm::Instr::I32Const {
                        value: value as i32,
                    },
                    curios_wasm::Instr::RefI31,
                ])
            }
            &EmissionData::Int(value) => {
                // In-range iff bit 30 agrees with the sign bit — the signed analogue of the `Nat` check above; out of range traps at the materialization point instead of silently wrapping to 31 bits.
                if value >> 30 != value >> 31 {
                    self.emit_instrs([curios_wasm::Instr::Unreachable]);
                    return;
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
            EmissionData::List(elems) => {
                let rope = self.context.table().list_rope();

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
            EmissionData::Tuple(elems) => {
                let tuple_n_type = self.context.table().find_tuple_type(elems.len());

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: tuple_n_type,
                });
            }
            EmissionData::Variant(family, elems) => {
                let family_type = self.context.table().find_family_type(*family);

                for elem in elems {
                    self.emit_instrs(self.context.load_value_instrs(elem, LoadAs::Null));
                }

                self.emit_instr(curios_wasm::Instr::StructNew {
                    type_name: family_type,
                });
            }
            EmissionData::Bin(grain, value) => {
                // A small packed literal is its canonical immediate, computed here at compile time: the byte grain's length in the top 2 payload bits over up to 3 bytes, the bit grain's in the top 5 over up to 26 bits, both LSB-first.
                let envelope = match grain {
                    Grain::X => 3,
                    Grain::B => 26,
                };
                if value.len(*grain) <= envelope {
                    let len_shift = match grain {
                        Grain::X => 29,
                        Grain::B => 26,
                    };
                    let bytes = value.to_packed_bytes();
                    let packed = bytes.iter().enumerate().fold(
                        (value.len(*grain) as i32) << len_shift,
                        |packed, (index, &byte)| packed | (byte as i32) << (8 * index),
                    );
                    self.emit_instrs([
                        curios_wasm::Instr::I32Const { value: packed },
                        curios_wasm::Instr::RefI31,
                    ]);
                    return;
                }

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
                self.module.add_data(
                    data_name.clone(),
                    curios_wasm::DataSegment {
                        mode: curios_wasm::DataMode::Passive,
                        bytes,
                    },
                );
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

                // The code field is the body's table index — an ordinary `i32`, so constructing a closure pays no `ref.func` materialization and no per-store funcref intern.
                self.emit_instr(curios_wasm::Instr::I32Const {
                    value: clsr_data.index(),
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

    /// Bind a constructed value to its local, materialising it in a register where that local is one.
    ///
    /// The range checks survive being moved onto this path, and that is deliberate: they are what keeps every register-held `Nat` inside the i31 envelope, so *boxing* one later — at a call argument, a constructor field, a jump to a boxed parameter — is a bare `ref.i31` that never has to re-check. Dropping them here would move the trap to the boxing coercion and change which programs trap.
    fn emit_let_pure(&mut self, value_name: &'a EmissionValueName, value: &'a EmissionData) {
        match (self.context.table().raw_carrier(value_name), value) {
            (Some(_), &EmissionData::Nat(value)) => match value >> 31 {
                0 => self.emit_instr(curios_wasm::Instr::I32Const {
                    value: value as i32,
                }),
                _ => self.emit_instr(curios_wasm::Instr::Unreachable),
            },
            (Some(_), &EmissionData::Int(value)) => match value >> 30 == value >> 31 {
                true => self.emit_instr(curios_wasm::Instr::I32Const { value }),
                false => self.emit_instr(curios_wasm::Instr::Unreachable),
            },
            (Some(_), &EmissionData::Flt(value)) => {
                self.emit_instr(curios_wasm::Instr::F32Const { value })
            }
            // No other shape is ever offered a register, so this arm builds the reference and reads it straight back down. It exists to keep the match total rather than to be taken.
            (Some(carrier), value) => {
                self.emit_data(value_name, value);
                let load = LoadAs::of(&carrier);
                self.emit_instrs(self.context.load_as_instrs(load, false));
            }
            (None, value) => self.emit_data(value_name, value),
        }

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
        self.emit_instr(curios_wasm::Instr::I32Const {
            value: clsr_data.index(),
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

    /// Allocate a fresh wasm local for `value_name` and record it in the current frame, so subsequent `find_local` lookups resolve to it. Called at the point a name is introduced — a shell or a fresh value — never for a fill, whose local its shell already owns.
    fn declare_local(&mut self, value_name: &'a EmissionValueName) {
        let val_type = self.context.table().local_type(value_name);
        let local_name = self.context.push_local(value_name.as_str(), val_type);

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
            // An acyclic aggregate has no back-edge, so every field is already bound: build it directly with a single `struct.new` / `array.new_fixed` (via `emit_data`). Only a shell'd closure shell — a recursive capture reusing its own local — takes the `new_default` + per-field `struct.set` backpatch path. Tuples and arrays are never shell'd (cyclic ones are rejected in `into_cont`), so they always build directly.
            match value {
                EmissionValue::Pure(value @ (EmissionData::List(_) | EmissionData::Tuple(_))) => {
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

    /// Emit one region: its bindings, then its blocks laid out as a structured nesting of `loop`, forward `block`, and localized-dispatcher scopes derived from the region's control-flow analysis ([`region_layout`]). Enters a frame the caller is responsible for leaving.
    fn emit_region(
        &mut self,
        params: HashMap<&'a EmissionValueName, LocalData>,
        region: &'a EmissionBody,
    ) {
        let shells = region.shells.iter().map(|(name, _)| name).collect();
        let aggregates: HashSet<_> = region
            .values
            .iter()
            .filter(|(_, value)| {
                matches!(
                    value,
                    EmissionValue::Pure(
                        EmissionData::Tuple(_) | EmissionData::List(_) | EmissionData::Variant(..)
                    )
                )
            })
            .map(|(name, _)| name)
            .collect();

        // A region with no join blocks is straight-line code ending in its tail; there is nothing to structure.
        if region.blocks.is_empty() {
            self.context
                .enter_frame(Frame::new(params, shells, aggregates, vec![]));
            self.emit_shells(&region.shells);
            self.emit_let_values(&region.values);
            let tail = self.context.tail_instrs(&region.tail);
            self.emit_instrs(tail);
            return;
        }

        let layout = region_layout(region);
        let block_params = self.block_param_locals(region);
        let mut dispatch = BTreeMap::new();
        self.collect_dispatch(&layout, region, &mut dispatch);

        // The region frame registers the forward-entry block of every top-level item, so the tail and earlier items resolve their forward branches.
        let registrations = scope_registrations(&layout, region, &block_params, &dispatch);
        self.context
            .enter_frame(Frame::new(params, shells, aggregates, registrations));
        self.emit_shells(&region.shells);
        self.emit_let_values(&region.values);

        // The tail is the innermost code: every block wraps it, so the tail can branch forward to any of them.
        let entry = self.context.tail_instrs(&region.tail);
        let body = self.fold_items(entry, &layout, region, &block_params, &dispatch);
        self.emit_instrs(body);
    }

    /// Reserve one wasm local per block parameter, keyed by block index. A branch binds these before jumping and the block body reads them, so a block referenced from more than one scope shares the same locals.
    fn block_param_locals(
        &mut self,
        region: &'a EmissionBody,
    ) -> Vec<Vec<(&'a EmissionValueName, LocalData)>> {
        region
            .blocks
            .iter()
            .map(|(_, block)| {
                block
                    .params
                    .iter()
                    .map(|name| {
                        let val_type = self.context.table().local_type(name);
                        let local_name = self.context.push_local(name.as_str(), val_type);
                        (name, LocalData::new(local_name, true))
                    })
                    .collect()
            })
            .collect()
    }

    /// Allocate a dispatcher plan — its index local, enter and loop labels, and per-member indices — for every irreducible component in the layout, keyed by its least member. Recurses through loops so nested dispatchers are covered too.
    fn collect_dispatch(
        &mut self,
        items: &[LayoutItem],
        region: &'a EmissionBody,
        out: &mut BTreeMap<usize, DispatchPlan>,
    ) {
        for item in items {
            match item {
                LayoutItem::Block(_) => {}
                LayoutItem::Loop { body, .. } => self.collect_dispatch(body, region, out),
                LayoutItem::Dispatch { members } => {
                    let index_local = self
                        .context
                        .push_local("", curios_wasm::ValType::Num(curios_wasm::NumType::I32));
                    let anchor = &region.blocks[members[0]].0;
                    let enter_label =
                        curios_wasm::LabelName::from(format!("$dispatch/enter/{anchor}"));
                    let dispatch_label =
                        curios_wasm::LabelName::from(format!("$dispatch/{anchor}"));
                    let index_of = members
                        .iter()
                        .enumerate()
                        .map(|(index, &member)| (member, index))
                        .collect();
                    out.insert(
                        members[0],
                        DispatchPlan {
                            enter_label,
                            dispatch_label,
                            index_local,
                            index_of,
                        },
                    );
                }
            }
        }
    }

    /// Fold each item's body around `entry`, wrapping every item in a forward `block` labeled by its entry point. Because the items are in a topological order, item `i` is nested inside every later item, so a forward branch to a later item exits outward as a plain `br`.
    fn fold_items(
        &mut self,
        entry: Vec<curios_wasm::Instr>,
        items: &[LayoutItem],
        region: &'a EmissionBody,
        block_params: &[Vec<(&'a EmissionValueName, LocalData)>],
        dispatch: &BTreeMap<usize, DispatchPlan>,
    ) -> Vec<curios_wasm::Instr> {
        let mut result = entry;
        for item in items {
            let body = self.emit_item(item, region, block_params, dispatch);
            let label = item_enter_label(item, region, dispatch);
            result = std::iter::once(curios_wasm::Instr::Block {
                label_name: label,
                block_type: curios_wasm::BlockType::Empty,
                instructions: result,
            })
            .chain(body)
            .collect();
        }
        result
    }

    /// Emit one layout item's body: a plain block's code, a `loop` around a reducible component's interior, or a localized dispatcher.
    fn emit_item(
        &mut self,
        item: &LayoutItem,
        region: &'a EmissionBody,
        block_params: &[Vec<(&'a EmissionValueName, LocalData)>],
        dispatch: &BTreeMap<usize, DispatchPlan>,
    ) -> Vec<curios_wasm::Instr> {
        match item {
            LayoutItem::Block(block) => {
                let params = block_params[*block].iter().cloned().collect();
                self.emit_region(params, &region.blocks[*block].1.region);
                self.context.leave_frame()
            }
            LayoutItem::Loop { header, body } => {
                let anchor = &region.blocks[*header].0;
                let loop_label = curios_wasm::LabelName::from(format!("$loop/{anchor}"));
                let (head, rest) = body
                    .split_first()
                    .expect("a loop layout body is never empty");
                debug_assert!(
                    matches!(head, LayoutItem::Block(first) if first == header),
                    "a loop layout leads with its header block",
                );

                // The loop frame shadows the header's forward-entry registration with the loop label, so back edges resolve to `br $loop`, and registers the interior's own forward-entry blocks.
                let mut registrations = vec![(
                    &region.blocks[*header].0,
                    BlockData::new_loop(loop_label.clone(), block_params[*header].clone()),
                )];
                registrations.extend(scope_registrations(rest, region, block_params, dispatch));
                self.context.enter_frame(Frame::structural(registrations));

                let header_params = block_params[*header].iter().cloned().collect();
                self.emit_region(header_params, &region.blocks[*header].1.region);
                let header_body = self.context.leave_frame();
                let interior = self.fold_items(header_body, rest, region, block_params, dispatch);
                self.context.leave_frame();

                vec![
                    curios_wasm::Instr::Loop {
                        label_name: loop_label,
                        block_type: curios_wasm::BlockType::Empty,
                        instructions: interior,
                    },
                    curios_wasm::Instr::Unreachable,
                ]
            }
            LayoutItem::Dispatch { members } => {
                self.emit_dispatch(members, region, block_params, dispatch)
            }
        }
    }

    /// Emit a localized dispatcher for an irreducible component: a `loop` whose `br_table` selects a member by index, each member wrapped in its own block. Entries from outside set the index and fall into the loop; cross edges inside set the index and branch back to the loop.
    fn emit_dispatch(
        &mut self,
        members: &[usize],
        region: &'a EmissionBody,
        block_params: &[Vec<(&'a EmissionValueName, LocalData)>],
        dispatch: &BTreeMap<usize, DispatchPlan>,
    ) -> Vec<curios_wasm::Instr> {
        let plan = &dispatch[&members[0]];

        // Inside the dispatcher a member is reached by branching to the loop with its index set.
        let registrations = members
            .iter()
            .map(|member| {
                let name = &region.blocks[*member].0;
                (
                    name,
                    BlockData::new(
                        plan.dispatch_label.clone(),
                        plan.index_local.clone(),
                        plan.index_of[member],
                        name,
                        block_params[*member].clone(),
                    ),
                )
            })
            .collect();
        self.context.enter_frame(Frame::structural(registrations));

        let member_bodies = members
            .iter()
            .map(|member| {
                let params = block_params[*member].iter().cloned().collect();
                self.emit_region(params, &region.blocks[*member].1.region);
                let label = curios_wasm::LabelName::from(format!("${}", region.blocks[*member].0));
                (label, self.context.leave_frame())
            })
            .collect::<Vec<_>>();
        self.context.leave_frame();

        let member_labels = member_bodies
            .iter()
            .map(|(label, _)| label.clone())
            .collect::<Vec<_>>();
        // The indices are always in range, so the default is never taken; the first member's label is a valid, enclosing target for it.
        let default = member_labels[0].clone();
        let inner = vec![
            curios_wasm::Instr::LocalGet {
                local_name: plan.index_local.clone(),
            },
            curios_wasm::Instr::BrTable {
                label_names: member_labels,
                label_name: default,
            },
        ];
        let folded = member_bodies
            .into_iter()
            .rev()
            .fold(inner, |instructions, (label, body)| {
                std::iter::once(curios_wasm::Instr::Block {
                    label_name: label,
                    block_type: curios_wasm::BlockType::Empty,
                    instructions,
                })
                .chain(body)
                .collect()
            });

        vec![
            curios_wasm::Instr::Loop {
                label_name: plan.dispatch_label.clone(),
                block_type: curios_wasm::BlockType::Empty,
                instructions: folded,
            },
            curios_wasm::Instr::Unreachable,
        ]
    }

    pub(crate) fn emit_root_region(&mut self, region: &'a EmissionBody) {
        self.emit_region(self.context.params(), region);
        self.leave_last_frame();
    }
}

/// A localized dispatcher's shared state: the index local read by its `br_table`, the label a cross edge branches to and the label an outside entry falls in through, and each member's dispatch index.
struct DispatchPlan {
    enter_label: curios_wasm::LabelName,
    dispatch_label: curios_wasm::LabelName,
    index_local: curios_wasm::LocalName,
    index_of: BTreeMap<usize, usize>,
}

/// The forward-entry registrations for a scope's items: a plain block or a loop's header enter as a forward `block`, and every member of an irreducible component enters through its dispatcher.
fn scope_registrations<'a>(
    items: &[LayoutItem],
    region: &'a EmissionBody,
    block_params: &[Vec<(&'a EmissionValueName, LocalData)>],
    dispatch: &BTreeMap<usize, DispatchPlan>,
) -> Vec<(&'a EmissionBlockName, BlockData<'a>)> {
    let mut registrations = Vec::new();
    for item in items {
        match item {
            LayoutItem::Block(block) | LayoutItem::Loop { header: block, .. } => {
                let name = &region.blocks[*block].0;
                registrations.push((
                    name,
                    BlockData::new_direct(name, block_params[*block].clone()),
                ));
            }
            LayoutItem::Dispatch { members } => {
                let plan = &dispatch[&members[0]];
                for member in members {
                    let name = &region.blocks[*member].0;
                    registrations.push((
                        name,
                        BlockData::new_dispatch_enter(
                            plan.enter_label.clone(),
                            plan.index_local.clone(),
                            plan.index_of[member],
                            name,
                            block_params[*member].clone(),
                        ),
                    ));
                }
            }
        }
    }
    registrations
}

/// The label a forward branch to an item enters through: a block or loop enters at its header's own label; an irreducible component through its dispatcher's enter block.
fn item_enter_label(
    item: &LayoutItem,
    region: &EmissionBody,
    dispatch: &BTreeMap<usize, DispatchPlan>,
) -> curios_wasm::LabelName {
    match item {
        LayoutItem::Block(block) | LayoutItem::Loop { header: block, .. } => {
            curios_wasm::LabelName::from(format!("${}", region.blocks[*block].0))
        }
        LayoutItem::Dispatch { members } => dispatch[&members[0]].enter_label.clone(),
    }
}
