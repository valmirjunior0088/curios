//! The turns of a program that can suspend: the entry and the advance, each answering what the program did — the value it yielded, or the suspension it waits in, taken apart for the host.
//!
//! A turn is the five values [`ADVANCE`] states. The suspension that reaches a turn is one the entry surfaced, with no frames and a rest that resumes everything beneath it, so the host needs nothing of the row beyond its wait, its abandon and that rest, and advances either description through the same export.

use {
    super::{Table, cast, concrete_ref, concrete_val, field_get, get, i32_type, set},
    curios_abi::{ADVANCE, ENTRY},
};

#[derive(Debug)]
pub(crate) struct TurnEmitter<'a, 'b> {
    table: &'a Table<'a>,
    module: &'b mut curios_wasm::Module,
}

impl<'a, 'b> TurnEmitter<'a, 'b> {
    pub(crate) fn new(table: &'a Table<'a>, module: &'b mut curios_wasm::Module) -> Self {
        Self { table, module }
    }

    /// Export the entry's turn and the advance. `entry` is the program's own entry function, answering what its forced description answered; `row` is the suspension's.
    pub(crate) fn emit_turns(&mut self, entry: curios_wasm::FuncName, row: curios_cont::RowId) {
        let turn = self.emit_turn(row);

        let main = self.add_func(
            "async/main",
            Vec::new(),
            Vec::new(),
            vec![
                curios_wasm::Instr::Call { func_name: entry },
                curios_wasm::Instr::Call {
                    func_name: turn.clone(),
                },
            ],
        );
        self.module
            .add_export(ENTRY, curios_wasm::Export::Func(main));

        // A description is a closure of no arguments, dispatched through that arity's table as any indirect call is.
        let description = curios_wasm::LocalName::from("description");
        let envr = self.table.find_envr_type(0);
        let advance = self.add_func(
            "async/advance",
            vec![(description.clone(), Table::top_type(true))],
            Vec::new(),
            vec![
                get(&description),
                curios_wasm::Instr::RefAsNonNull,
                get(&description),
                cast(&envr),
                field_get(&envr, &self.table.special_field()),
                curios_wasm::Instr::CallIndirect {
                    table_name: self.table.clsr_table(0),
                    type_name: self.table.find_clsr_type(0),
                },
                curios_wasm::Instr::Call { func_name: turn },
            ],
        );
        self.module
            .add_export(ADVANCE, curios_wasm::Export::Func(advance));
    }

    /// `$async/turn (anyref) -> turn`: what a forced description answered, as a turn. The handles cross as a `List(Handle)` operand of a host row does.
    fn emit_turn(&mut self, row: curios_cont::RowId) -> curios_wasm::FuncName {
        let row_type = self.table.find_row_type(row);
        let answer = curios_wasm::LocalName::from("answer");
        let suspension = curios_wasm::LocalName::from("suspension");
        let null = || curios_wasm::Instr::RefNull {
            heap_type: curios_wasm::HeapType::Abstract(curios_wasm::AbsHeapType::None),
        };
        let slot = |index| {
            [
                get(&suspension),
                field_get(&row_type, &Table::tuple_field(index)),
            ]
        };

        let mut instrs = vec![
            get(&answer),
            curios_wasm::Instr::RefTest {
                ref_type: concrete_ref(row_type.clone(), false),
            },
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::If {
                label_name: curios_wasm::LabelName::from("value"),
                block_type: curios_wasm::BlockType::Empty,
                then_instructions: vec![
                    curios_wasm::Instr::I32Const { value: 0 },
                    get(&answer),
                    null(),
                    null(),
                    null(),
                    curios_wasm::Instr::Return,
                ],
                else_instructions: Vec::new(),
            },
            get(&answer),
            cast(&row_type),
            set(&suspension),
            curios_wasm::Instr::I32Const { value: 1 },
            null(),
        ];
        instrs.extend(slot(0));
        instrs.push(curios_wasm::Instr::RefAsNonNull);
        instrs.push(curios_wasm::Instr::Call {
            func_name: self.table.list_bytes_force_func(),
        });
        instrs.extend(slot(1));
        instrs.extend(slot(2));

        self.add_func(
            "async/turn",
            vec![(answer, Table::top_type(true))],
            vec![(suspension, concrete_val(row_type.clone(), true))],
            instrs,
        )
    }

    /// Declare one function answering a turn: a final func type of its own name, and the function.
    fn add_func(
        &mut self,
        name: &str,
        params: Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
        locals: Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
        instrs: Vec<curios_wasm::Instr>,
    ) -> curios_wasm::FuncName {
        let func_name = curios_wasm::FuncName::from(name);
        let type_name = curios_wasm::TypeName::from(name);
        let outputs = curios_wasm::ResultType::from([
            i32_type(),
            Table::top_type(true),
            concrete_val(self.table.elems_type(), true),
            Table::top_type(true),
            Table::top_type(true),
        ]);

        self.module.add_type(
            type_name.clone(),
            curios_wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: curios_wasm::CompType::Func(curios_wasm::FuncType {
                    inputs: curios_wasm::ResultType::from(
                        params.iter().map(|(_, val_type)| val_type.clone()),
                    ),
                    outputs,
                }),
            },
        );
        self.module.add_func(
            func_name.clone(),
            curios_wasm::Func {
                type_name,
                params: params.into_iter().map(|(name, _)| name).collect(),
                locals,
                expr: instrs.into(),
            },
        );

        func_name
    }
}
