//! Guest coordination instructions. Every attempt finishes inline before its continuation runs; taking a channel item clears its array slot, so a drained channel retains no removed payloads. Scratch locals are assigned on every path that reads them, including when one call site is executed repeatedly in a loop.

use {
    super::{Context, LoadAs},
    crate::into_wasm::{
        BigHelper, EmissionBlockName, EmissionCellTarget, EmissionChannelTarget, Table, branch,
        call, concrete_val, either, field_get, field_set, get, i32_const, set,
    },
};

impl<'a> Context<'a, '_> {
    fn coordination_resume(
        &self,
        output: &mut Vec<curios_wasm::Instr>,
        resume: &EmissionBlockName,
        arity: usize,
    ) {
        if self.is_resume(resume) {
            output.push(curios_wasm::Instr::Return);
        } else {
            output.extend(self.resume_instrs(resume, arity));
        }
    }

    pub(super) fn cell_instrs(&self, cell: &'a EmissionCellTarget) -> Vec<curios_wasm::Instr> {
        let type_name = self.table().cell_type();
        let field = self.table().special_field();
        let mut output = Vec::new();
        let (resume, arity) = match cell {
            EmissionCellTarget::Reserve { resume } => {
                output.push(curios_wasm::Instr::StructNewDefault { type_name });
                (resume, 1)
            }
            EmissionCellTarget::Fill {
                cell,
                value,
                resume,
            } => {
                let load = self.load_value_instrs(cell, LoadAs::Concrete(type_name.clone()));
                output.extend(load.clone());
                output.push(field_get(&type_name, &field));
                output.push(curios_wasm::Instr::RefIsNull);
                let mut fill = load;
                fill.extend(self.load_value_instrs(value, LoadAs::NonNull));
                fill.push(field_set(&type_name, &field));
                fill.push(i32_const(1));
                output.push(either(
                    curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                    fill,
                    vec![i32_const(0)],
                ));
                output.push(curios_wasm::Instr::RefI31);
                (resume, 1)
            }
            EmissionCellTarget::Poll { cell, resume } => {
                let load = self.load_value_instrs(cell, LoadAs::Concrete(type_name.clone()));
                output.extend(load.clone());
                output.push(field_get(&type_name, &field));
                output.push(curios_wasm::Instr::RefIsNull);
                output.push(curios_wasm::Instr::I32Eqz);
                output.push(curios_wasm::Instr::RefI31);
                output.extend(load);
                output.push(field_get(&type_name, &field));
                (resume, 2)
            }
        };
        self.coordination_resume(&mut output, resume, arity);
        output
    }

    pub(super) fn channel_instrs(
        &mut self,
        target: &'a EmissionChannelTarget,
    ) -> Vec<curios_wasm::Instr> {
        let data = self.table().channel();
        let word = curios_wasm::ValType::Num(curios_wasm::NumType::I32);
        let mut output = Vec::new();
        if target.op == curios_cont::ChannelOp::New {
            output.extend(self.load_value_instrs(&target.args[0], LoadAs::Nat));
            output.push(curios_wasm::Instr::ArrayNewDefault {
                type_name: data.payload,
            });
            output.extend([i32_const(0), i32_const(0), i32_const(0)]);
            output.push(curios_wasm::Instr::StructNew {
                type_name: data.channel,
            });
            self.coordination_resume(&mut output, &target.resume, 1);
            return output;
        }
        let channel = self.push_local("channel", concrete_val(data.channel.clone(), true));
        output.extend(
            self.load_value_instrs(&target.args[0], LoadAs::Concrete(data.channel.clone())),
        );
        output.push(set(&channel));
        let read = |field| vec![get(&channel), field_get(&data.channel, field)];
        let capacity = || {
            let mut instrs = read(&data.payload_field);
            instrs.push(curios_wasm::Instr::ArrayLen);
            instrs
        };
        match target.op {
            curios_cont::ChannelOp::New => unreachable!("creation was emitted above"),
            curios_cont::ChannelOp::Close => {
                output.extend([
                    get(&channel),
                    i32_const(1),
                    field_set(&data.channel, &data.closed_field),
                ]);
            }
            curios_cont::ChannelOp::Closed => {
                output.extend(read(&data.closed_field));
                output.push(curios_wasm::Instr::RefI31);
            }
            curios_cont::ChannelOp::Count | curios_cont::ChannelOp::Capacity => {
                output.extend(if target.op == curios_cont::ChannelOp::Count {
                    read(&data.count_field)
                } else {
                    capacity()
                });
                output.push(curios_wasm::Instr::I64ExtendI32U);
                output.push(call(&self.table().big_func(BigHelper::OfU64)));
            }
            curios_cont::ChannelOp::Push => {
                let mut insert = read(&data.payload_field);
                // Widen the index sum so a head near the array bound cannot wrap before the remainder.
                insert.extend(read(&data.head_field));
                insert.push(curios_wasm::Instr::I64ExtendI32U);
                insert.extend(read(&data.count_field));
                insert.extend([
                    curios_wasm::Instr::I64ExtendI32U,
                    curios_wasm::Instr::I64Add,
                ]);
                insert.extend(capacity());
                insert.extend([
                    curios_wasm::Instr::I64ExtendI32U,
                    curios_wasm::Instr::I64RemU,
                    curios_wasm::Instr::I32WrapI64,
                ]);
                insert.extend(self.load_value_instrs(&target.args[1], LoadAs::NonNull));
                insert.push(curios_wasm::Instr::ArraySet {
                    type_name: data.payload.clone(),
                });
                insert.push(get(&channel));
                insert.extend(read(&data.count_field));
                insert.extend([
                    i32_const(1),
                    curios_wasm::Instr::I32Add,
                    field_set(&data.channel, &data.count_field),
                    i32_const(0),
                ]);
                let mut open = read(&data.count_field);
                open.extend(capacity());
                open.push(curios_wasm::Instr::I32Eq);
                open.push(either(word.clone(), vec![i32_const(1)], insert));
                output.extend(read(&data.closed_field));
                output.push(either(word, vec![i32_const(2)], open));
                output.push(curios_wasm::Instr::RefI31);
            }
            curios_cont::ChannelOp::Take => {
                let payload = self.push_local("channel_item", Table::top_type(true));
                let status = self.push_local("channel_status", word.clone());
                let mut take = read(&data.payload_field);
                take.extend(read(&data.head_field));
                take.push(curios_wasm::Instr::ArrayGet {
                    type_name: data.payload.clone(),
                });
                take.push(set(&payload));
                take.extend(read(&data.payload_field));
                take.extend(read(&data.head_field));
                take.push(curios_wasm::Instr::RefNull {
                    heap_type: curios_wasm::HeapType::Abstract(curios_wasm::AbsHeapType::Any),
                });
                take.push(curios_wasm::Instr::ArraySet {
                    type_name: data.payload.clone(),
                });
                take.push(get(&channel));
                take.extend(read(&data.head_field));
                take.extend([i32_const(1), curios_wasm::Instr::I32Add]);
                take.extend(capacity());
                take.push(curios_wasm::Instr::I32RemU);
                take.push(field_set(&data.channel, &data.head_field));
                take.push(get(&channel));
                take.extend(read(&data.count_field));
                take.extend([
                    i32_const(1),
                    curios_wasm::Instr::I32Sub,
                    field_set(&data.channel, &data.count_field),
                    i32_const(0),
                    set(&status),
                ]);
                let mut empty = vec![
                    curios_wasm::Instr::RefNull {
                        heap_type: curios_wasm::HeapType::Abstract(curios_wasm::AbsHeapType::Any),
                    },
                    set(&payload),
                ];
                empty.extend(read(&data.closed_field));
                empty.push(either(word, vec![i32_const(2)], vec![i32_const(1)]));
                empty.push(set(&status));
                output.extend(read(&data.count_field));
                output.push(branch(take, empty));
                output.extend([get(&status), curios_wasm::Instr::RefI31, get(&payload)]);
            }
        }
        self.coordination_resume(&mut output, &target.resume, target.op.result_arity());
        output
    }
}
