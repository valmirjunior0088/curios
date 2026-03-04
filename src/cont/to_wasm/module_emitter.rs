use {
    super::{Context, ExprEmitter, Table},
    crate::{cont, wasm},
    std::iter,
};

#[derive(Debug)]
pub struct ModuleEmitter<'a, 'b> {
    table: &'a Table<'a>,
    module: &'b mut wasm::Module,
}

impl<'a, 'b> ModuleEmitter<'a, 'b> {
    pub fn new(table: &'a Table<'a>, module: &'b mut wasm::Module) -> Self {
        Self { table, module }
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

    fn emit_unit_type(&mut self) {
        self.module.add_type(
            self.table.unit_type(),
            wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: wasm::CompType::Struct(wasm::StructType::from([])),
            },
        );
    }

    fn emit_tpl2_type(&mut self) {
        self.module.add_type(
            self.table.tpl2_type(),
            wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: wasm::CompType::Struct(wasm::StructType::from([
                    (
                        self.table.proj_fst_field(),
                        wasm::FieldType {
                            storage_type: wasm::StorageType::Val(self.table.obj_val_type(true)),
                            mutability: wasm::Mutability::Var,
                        },
                    ),
                    (
                        self.table.proj_snd_field(),
                        wasm::FieldType {
                            storage_type: wasm::StorageType::Val(self.table.obj_val_type(true)),
                            mutability: wasm::Mutability::Var,
                        },
                    ),
                ])),
            },
        );
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
                                    storage_type: wasm::StorageType::Val(
                                        self.table.obj_val_type(true),
                                    ),
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
                            iter::once(self.table.obj_val_type(false))
                                .chain((0..arity).map(|_| self.table.obj_val_type(false))),
                        ),
                        outputs: wasm::ResultType::from([self.table.obj_val_type(false)]),
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
                            iter::once(self.table.obj_val_type(false))
                                .chain((0..data.arity()).map(|_| self.table.obj_val_type(false))),
                        ),
                        outputs: wasm::ResultType::from([self.table.obj_val_type(false)]),
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
                            (0..arity).map(|_| self.table.obj_val_type(false)),
                        ),
                        outputs: wasm::ResultType::from([self.table.obj_val_type(false)]),
                    }),
                },
            );
        }
    }

    fn emit_let_const(&mut self, name: &'a cont::ValueName, value: &'a cont::ConstValue) {
        let mut expr = Default::default();

        ExprEmitter::new(Context::new_const(self.table), &mut expr).emit_const_value(value);

        self.module.add_global(
            self.table.find_const(name),
            wasm::Global {
                global_type: wasm::GlobalType {
                    val_type: self.table.obj_val_type(false),
                    mutability: wasm::Mutability::Const,
                },
                expr,
            },
        );

        self.module.add_export(
            self.table.find_const(name).string,
            wasm::Export::Global(self.table.find_const(name)),
        );
    }

    fn emit_let_clsr(&mut self, name: &'a cont::ClsrName, clsr: &'a cont::Clsr) {
        let mut locals = Default::default();
        let mut expr = Default::default();

        ExprEmitter::new(
            Context::new_clsr(self.table, self.table.find_clsr(name), &mut locals),
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
                            .expect(&format!("`ClsrData` lacks param `{}`", value_name.string))
                    }))
                    .collect(),
                locals,
                expr,
            },
        );

        self.module.add_export(
            self.table.find_clsr(name).func_name().string,
            wasm::Export::Func(self.table.find_clsr(name).func_name()),
        );
    }

    fn emit_let_func(&mut self, name: &'a cont::FuncName, func: &'a cont::Func) {
        let mut locals = Default::default();
        let mut expr = Default::default();

        ExprEmitter::new(
            Context::new_func(self.table, self.table.find_func(name), &mut locals),
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
                            .expect(&format!("`FuncData` lacks param `{}`", value_name.string))
                    })
                    .collect(),
                locals,
                expr,
            },
        );

        self.module.add_export(
            self.table.find_func(name).func_name().string,
            wasm::Export::Func(self.table.find_func(name).func_name()),
        );
    }

    pub fn emit_module(&mut self, module: &'a cont::Module) {
        self.emit_unit_type();
        self.emit_flt_type();
        self.emit_tpl2_type();
        self.emit_clsr_arity_types();
        self.emit_clsr_named_types();
        self.emit_envr_arity_types();
        self.emit_clsr_types();
        self.emit_func_types();

        for (name, value) in module.consts() {
            self.emit_let_const(name, value);
        }

        for (name, clsr) in module.clsrs() {
            self.emit_let_clsr(name, clsr);
        }

        for (name, func) in module.funcs() {
            self.emit_let_func(name, func);
        }
    }
}
