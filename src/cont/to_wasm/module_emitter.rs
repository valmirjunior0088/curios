use {
    super::{Context, ExprEmitter, Table},
    crate::{cont, wasm},
    std::iter,
};

#[derive(Debug)]
pub struct ModuleEmitter<'a, 'b> {
    table: &'a Table<'a>,
    start_expr: wasm::Expr,
    module: &'b mut wasm::Module,
}

impl<'a, 'b> ModuleEmitter<'a, 'b> {
    pub fn new(table: &'a Table<'a>, module: &'b mut wasm::Module) -> Self {
        Self {
            table,
            start_expr: Default::default(),
            module,
        }
    }

    fn emit_bin_type(&mut self) {
        self.module.add_type(
            self.table.bin_type(),
            wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: wasm::CompType::Array(wasm::ArrayType::from(wasm::FieldType {
                    storage_type: wasm::StorageType::Packed(wasm::PackedType::I8),
                    mutability: wasm::Mutability::Var,
                })),
            },
        );
    }

    fn emit_to_str_imports(&mut self) {
        let bin_ref = wasm::ValType::Ref(wasm::RefType {
            is_nullable: false,
            heap_type: wasm::HeapType::Concrete(self.table.bin_type()),
        });

        let i32_to_bin_type = wasm::SubType {
            is_final: true,
            super_types: vec![],
            comp_type: wasm::CompType::Func(wasm::FuncType {
                inputs: wasm::ResultType::from([wasm::ValType::Num(wasm::NumType::I32)]),
                outputs: wasm::ResultType::from([bin_ref.clone()]),
            }),
        };

        if self.table.nat_to_str_used() {
            let nat_to_str_type = wasm::TypeName::from("nat_to_str_type");
            self.module
                .add_type(nat_to_str_type.clone(), i32_to_bin_type.clone());
            self.module.add_import(
                "env",
                "nat_to_str",
                wasm::Import::Func {
                    func_name: self.table.nat_to_str_func().clone(),
                    type_name: nat_to_str_type,
                },
            );
        }

        if self.table.int_to_str_used() {
            let int_to_str_type = wasm::TypeName::from("int_to_str_type");
            self.module
                .add_type(int_to_str_type.clone(), i32_to_bin_type);
            self.module.add_import(
                "env",
                "int_to_str",
                wasm::Import::Func {
                    func_name: self.table.int_to_str_func().clone(),
                    type_name: int_to_str_type,
                },
            );
        }

        if self.table.flt_to_str_used() {
            let flt_to_str_type = wasm::TypeName::from("flt_to_str_type");
            self.module.add_type(
                flt_to_str_type.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([wasm::ValType::Num(wasm::NumType::F32)]),
                        outputs: wasm::ResultType::from([bin_ref]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "flt_to_str",
                wasm::Import::Func {
                    func_name: self.table.flt_to_str_func().clone(),
                    type_name: flt_to_str_type,
                },
            );
        }
    }

    fn emit_sys_imports(&mut self) {
        if self.table.sys_print_used() {
            let bin_ref = wasm::ValType::Ref(wasm::RefType {
                is_nullable: false,
                heap_type: wasm::HeapType::Concrete(self.table.bin_type()),
            });
            let sys_print_type = wasm::TypeName::from("sys_print_type");
            self.module.add_type(
                sys_print_type.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([bin_ref]),
                        outputs: wasm::ResultType::from([]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "sys_print",
                wasm::Import::Func {
                    func_name: self.table.sys_print_func().clone(),
                    type_name: sys_print_type,
                },
            );
        }
    }

    fn emit_arr_type(&mut self) {
        self.module.add_type(
            self.table.arr_type(),
            wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: wasm::CompType::Array(wasm::ArrayType::from(wasm::FieldType {
                    storage_type: wasm::StorageType::Val(self.table.top_type(true)),
                    mutability: wasm::Mutability::Var,
                })),
            },
        );
    }

    fn emit_flt_type(&mut self) {
        self.module.add_type(
            self.table.flt_type(),
            wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: wasm::CompType::Struct(wasm::StructType::from([(
                    self.table.special_field(),
                    wasm::FieldType {
                        storage_type: wasm::StorageType::Val(wasm::ValType::Num(
                            wasm::NumType::F32,
                        )),
                        mutability: wasm::Mutability::Const,
                    },
                )])),
            },
        );
    }

    fn emit_tpl_types(&mut self) {
        for (arity, type_name) in self.table.tpl_types() {
            let super_types = match arity {
                0 => vec![],
                n => vec![self.table.find_tpl_type(n - 1)],
            };

            self.module.add_type(
                type_name,
                wasm::SubType {
                    is_final: false,
                    super_types,
                    comp_type: wasm::CompType::Struct(wasm::StructType::from((0..arity).map(
                        |index| {
                            (
                                self.table.tpl_field(index),
                                wasm::FieldType {
                                    storage_type: wasm::StorageType::Val(self.table.top_type(true)),
                                    mutability: wasm::Mutability::Var,
                                },
                            )
                        },
                    ))),
                },
            );
        }
    }

    fn emit_envr_arity_types(&mut self) {
        for (arity, type_name) in self.table.envr_types() {
            self.module.add_type(
                type_name,
                wasm::SubType {
                    is_final: false,
                    super_types: vec![],
                    comp_type: wasm::CompType::Struct(wasm::StructType::from([(
                        self.table.special_field(),
                        wasm::FieldType {
                            storage_type: wasm::StorageType::Val(wasm::ValType::Ref(
                                wasm::RefType {
                                    is_nullable: true,
                                    heap_type: wasm::HeapType::Concrete(
                                        self.table.find_clsr_type(arity),
                                    ),
                                },
                            )),
                            mutability: wasm::Mutability::Var,
                        },
                    )])),
                },
            );
        }
    }

    fn emit_clsr_types(&mut self) {
        for data in self.table.clsrs() {
            self.module.add_type(
                data.envr_type(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![self.table.find_envr_type(data.arity())],
                    comp_type: wasm::CompType::Struct(wasm::StructType::from(
                        iter::once((
                            self.table.special_field(),
                            wasm::FieldType {
                                storage_type: wasm::StorageType::Val(wasm::ValType::Ref(
                                    wasm::RefType {
                                        is_nullable: true,
                                        heap_type: wasm::HeapType::Concrete(
                                            self.table.find_clsr_type(data.arity()),
                                        ),
                                    },
                                )),
                                mutability: wasm::Mutability::Var,
                            },
                        ))
                        .chain(data.fields().map(|field_name| {
                            (
                                field_name,
                                wasm::FieldType {
                                    storage_type: wasm::StorageType::Val(self.table.top_type(true)),
                                    mutability: wasm::Mutability::Var,
                                },
                            )
                        })),
                    )),
                },
            );
        }
    }

    fn emit_clsr_arity_types(&mut self) {
        for (arity, type_name) in self.table.clsr_types() {
            self.module.add_type(
                type_name,
                wasm::SubType {
                    is_final: false,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from(
                            iter::once(self.table.top_type(false))
                                .chain((0..arity).map(|_| self.table.top_type(false))),
                        ),
                        outputs: wasm::ResultType::from([self.table.top_type(false)]),
                    }),
                },
            );
        }
    }

    fn emit_clsr_named_types(&mut self) {
        for data in self.table.clsrs() {
            self.module.add_type(
                data.clsr_type(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![self.table.find_clsr_type(data.arity())],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from(
                            iter::once(self.table.top_type(false))
                                .chain((0..data.arity()).map(|_| self.table.top_type(false))),
                        ),
                        outputs: wasm::ResultType::from([self.table.top_type(false)]),
                    }),
                },
            );
        }
    }

    fn emit_func_types(&mut self) {
        for (arity, type_name) in self.table.func_types() {
            self.module.add_type(
                type_name,
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from(
                            (0..arity).map(|_| self.table.top_type(false)),
                        ),
                        outputs: wasm::ResultType::from([self.table.top_type(false)]),
                    }),
                },
            );
        }
    }

    fn emit_let_bin_data(&mut self, name: &'a cont::ValueName, bytes: &[u8]) {
        let bin_type = self.table.bin_type();
        let global_name = self.table.find_const(name);
        let data_name = wasm::DataName::from(name.as_string());

        self.module.add_data(
            data_name.clone(),
            wasm::DataSegment {
                bytes: bytes.to_vec(),
            },
        );

        let mut init_expr: wasm::Expr = Default::default();
        init_expr.push(wasm::Instr::I32Const { value: 0 });
        init_expr.push(wasm::Instr::ArrayNewDefault {
            type_name: bin_type.clone(),
        });

        self.module.add_global(
            global_name.clone(),
            wasm::Global {
                global_type: wasm::GlobalType {
                    val_type: wasm::ValType::Ref(wasm::RefType {
                        is_nullable: false,
                        heap_type: wasm::HeapType::Concrete(bin_type.clone()),
                    }),
                    mutability: wasm::Mutability::Var,
                },
                expr: init_expr,
            },
        );

        self.module.add_export(
            global_name.as_string(),
            wasm::Export::Global(global_name.clone()),
        );

        self.start_expr.push(wasm::Instr::I32Const { value: 0 });
        self.start_expr.push(wasm::Instr::I32Const {
            value: bytes.len() as i32,
        });
        self.start_expr.push(wasm::Instr::ArrayNewData {
            type_name: bin_type,
            data_name,
        });
        self.start_expr.push(wasm::Instr::GlobalSet { global_name });
    }

    fn emit_let_data(&mut self, name: &'a cont::ValueName, value: &'a cont::Data) {
        if let cont::Data::Bin(bytes) = value {
            self.emit_let_bin_data(name, bytes);
            return;
        }

        let mut expr = Default::default();

        ExprEmitter::new(Context::new_const(self.table), self.module, &mut expr)
            .emit_data(name, value);

        self.module.add_global(
            self.table.find_const(name),
            wasm::Global {
                global_type: wasm::GlobalType {
                    val_type: self.table.top_type(false),
                    mutability: wasm::Mutability::Const,
                },
                expr,
            },
        );

        self.module.add_export(
            self.table.find_const(name).as_string(),
            wasm::Export::Global(self.table.find_const(name)),
        );
    }

    fn emit_let_clsr(&mut self, name: &'a cont::ClsrName, clsr: &'a cont::Clsr) {
        let mut locals = Default::default();
        let mut expr = Default::default();

        ExprEmitter::new(
            Context::new_clsr(self.table, self.table.find_clsr(name), &mut locals),
            self.module,
            &mut expr,
        )
        .emit_root_region(&clsr.region);

        self.module.add_func(
            self.table.find_clsr(name).func_name(),
            wasm::Func {
                type_name: self.table.find_clsr(name).clsr_type(),
                params: iter::once(self.table.special_local())
                    .chain(clsr.params.iter().map(|value_name| {
                        self.table
                            .find_clsr(name)
                            .find_param(value_name)
                            .unwrap_or_else(|| panic!("`ClsrData` lacks param `{}`", value_name))
                    }))
                    .collect(),
                locals,
                expr,
            },
        );

        self.module.add_export(
            self.table.find_clsr(name).func_name().as_string(),
            wasm::Export::Func(self.table.find_clsr(name).func_name()),
        );
    }

    fn emit_let_func(&mut self, name: &'a cont::FuncName, func: &'a cont::Func) {
        let mut locals = Default::default();
        let mut expr = Default::default();

        ExprEmitter::new(
            Context::new_func(self.table, self.table.find_func(name), &mut locals),
            self.module,
            &mut expr,
        )
        .emit_root_region(&func.region);

        self.module.add_func(
            self.table.find_func(name).func_name(),
            wasm::Func {
                type_name: self.table.find_func_type(func.params.len()),
                params: func
                    .params
                    .iter()
                    .map(|value_name| {
                        self.table
                            .find_func(name)
                            .find_param(value_name)
                            .unwrap_or_else(|| panic!("`FuncData` lacks param `{}`", value_name))
                    })
                    .collect(),
                locals,
                expr,
            },
        );

        self.module.add_export(
            self.table.find_func(name).func_name().as_string(),
            wasm::Export::Func(self.table.find_func(name).func_name()),
        );
    }

    pub fn emit_module(&mut self, module: &'a cont::Module) {
        self.emit_flt_type();
        self.emit_bin_type();
        self.emit_arr_type();
        self.emit_tpl_types();
        self.emit_clsr_arity_types();
        self.emit_clsr_named_types();
        self.emit_envr_arity_types();
        self.emit_clsr_types();
        self.emit_func_types();

        for (name, value) in module.consts() {
            self.emit_let_data(name, value);
        }

        for (name, clsr) in module.clsrs() {
            self.emit_let_clsr(name, clsr);
        }

        for (name, func) in module.funcs() {
            self.emit_let_func(name, func);
        }

        self.emit_to_str_imports();
        self.emit_sys_imports();

        let start_type_name = wasm::TypeName::from("start");

        self.module.add_type(
            start_type_name.clone(),
            wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: wasm::CompType::Func(wasm::FuncType {
                    inputs: wasm::ResultType::from([]),
                    outputs: wasm::ResultType::from([]),
                }),
            },
        );

        let start_func_name = wasm::FuncName::from("start");

        self.module.add_func(
            start_func_name.clone(),
            wasm::Func {
                type_name: start_type_name,
                params: vec![],
                locals: vec![],
                expr: self.start_expr.clone(),
            },
        );

        self.module.set_start(start_func_name);
    }
}
