//! Guest coordination instructions. Every attempt finishes inline before its continuation runs. Scratch locals are assigned on every path that reads them, including when one call site is executed repeatedly in a loop.

use {
    super::{Context, LoadAs},
    crate::{
        EmissionBlockName, EmissionBoxTarget, EmissionCellTarget, EmissionFrameTarget, either,
        field_get, field_set, i32_const,
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

    /// A frame-budget operation on `$frames/used`. Entering admits a call of `weight` where what the budget has spent leaves it room, spending it, and answers the admission as the `0`/`1` word every boolean resumes with; leaving hands the weight back and answers nothing.
    pub(super) fn frame_instrs(&self, frame: &'a EmissionFrameTarget) -> Vec<curios_wasm::Instr> {
        let used = self.table().frames_used_global();
        let get = || curios_wasm::Instr::GlobalGet {
            global_name: used.clone(),
        };
        let set = || curios_wasm::Instr::GlobalSet {
            global_name: used.clone(),
        };
        let mut output = Vec::new();
        let arity = match frame.op {
            curios_cps::IntrinsicCall::FrameEnter { weight } => {
                let room = curios_cps::FRAME_BUDGET - weight;
                output.push(get());
                output.push(i32_const(
                    i32::try_from(room).expect("the frame budget is a word"),
                ));
                output.push(curios_wasm::Instr::I32LeU);
                output.push(either(
                    curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                    vec![
                        get(),
                        i32_const(i32::try_from(weight).expect("a weight is a word")),
                        curios_wasm::Instr::I32Add,
                        set(),
                        i32_const(1),
                    ],
                    vec![i32_const(0)],
                ));
                output.push(curios_wasm::Instr::RefI31);
                1
            }
            curios_cps::IntrinsicCall::FrameLeave { weight } => {
                output.push(get());
                output.push(i32_const(
                    i32::try_from(weight).expect("a weight is a word"),
                ));
                output.push(curios_wasm::Instr::I32Sub);
                output.push(set());
                0
            }
            op @ (curios_cps::IntrinsicCall::ListMap | curios_cps::IntrinsicCall::HeapApply) => {
                unreachable!("{op:?} is no frame-budget operation")
            }
        };
        self.coordination_resume(&mut output, &frame.resume, arity);
        output
    }

    /// A box operation, inline: `New` makes the box holding its operand, `Get` answers what the box holds, and `Set` makes it hold its second operand, answering nothing. The field admits null, as every reference position does, so a filler stored in a box reads back as it was stored.
    pub(super) fn box_instrs(&self, target: &'a EmissionBoxTarget) -> Vec<curios_wasm::Instr> {
        let type_name = self.table().box_type();
        let field = self.table().special_field();
        let mut output = Vec::new();
        match target.op {
            curios_cps::BoxOp::New => {
                output.extend(self.load_value_instrs(&target.args[0], LoadAs::Null));
                output.push(curios_wasm::Instr::StructNew { type_name });
            }
            curios_cps::BoxOp::Get => {
                output.extend(
                    self.load_value_instrs(&target.args[0], LoadAs::Concrete(type_name.clone())),
                );
                output.push(field_get(&type_name, &field));
            }
            curios_cps::BoxOp::Set => {
                output.extend(
                    self.load_value_instrs(&target.args[0], LoadAs::Concrete(type_name.clone())),
                );
                output.extend(self.load_value_instrs(&target.args[1], LoadAs::Null));
                output.push(field_set(&type_name, &field));
            }
        }
        self.coordination_resume(&mut output, &target.resume, target.op.result_arity());
        output
    }
}
