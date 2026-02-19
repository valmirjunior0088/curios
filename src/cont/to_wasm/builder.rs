use {
    super::{Context, Emitter, ModuleData},
    crate::{cont, wasm},
    std::iter,
};

pub struct Builder<'a, 'b> {
    metadata: &'a ModuleData<'a>,
    module: &'b mut wasm::Module,
}

impl<'a, 'b> Builder<'a, 'b> {
    pub fn new(metadata: &'a ModuleData<'a>, module: &'b mut wasm::Module) -> Self {
        Self { metadata, module }
    }

    pub fn emit_obj_type(&mut self) {
        self.module.add_type(
            self.metadata.obj_type(),
            wasm::SubType {
                is_final: false,
                super_types: vec![],
                comp_type: wasm::CompType::Struct(wasm::StructType::from([])),
            },
        );
    }

    pub fn emit_int_type(&mut self) {
        self.module.add_type(
            self.metadata.int_type(),
            wasm::SubType {
                is_final: true,
                super_types: vec![self.metadata.obj_type()],
                comp_type: wasm::CompType::Struct(wasm::StructType::from([(
                    self.metadata.special_field(),
                    wasm::FieldType {
                        storage_type: wasm::StorageType::Val(wasm::ValType::Num(
                            wasm::NumType::I32,
                        )),
                        mutability: wasm::Mutability::Const,
                    },
                )])),
            },
        );
    }

    pub fn emit_flt_type(&mut self) {
        self.module.add_type(
            self.metadata.flt_type(),
            wasm::SubType {
                is_final: true,
                super_types: vec![self.metadata.obj_type()],
                comp_type: wasm::CompType::Struct(wasm::StructType::from([(
                    self.metadata.special_field(),
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

    pub fn emit_envr_type(&mut self) {
        self.module.add_type(
            self.metadata.envr_type(),
            wasm::SubType {
                is_final: false,
                super_types: vec![self.metadata.obj_type()],
                comp_type: wasm::CompType::Struct(wasm::StructType::from([(
                    self.metadata.special_field(),
                    wasm::FieldType {
                        storage_type: wasm::StorageType::Val(wasm::ValType::Ref(wasm::RefType {
                            is_nullable: false,
                            heap_type: wasm::HeapType::Abstract(wasm::AbsHeapType::Func),
                        })),
                        mutability: wasm::Mutability::Const,
                    },
                )])),
            },
        );
    }

    pub fn emit_clsr_envr_types(&mut self) {
        for data in self.metadata.clsrs() {
            self.module.add_type(
                data.envr_type(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![self.metadata.envr_type()],
                    comp_type: wasm::CompType::Struct(wasm::StructType::from(
                        iter::once((
                            self.metadata.special_field(),
                            wasm::FieldType {
                                storage_type: wasm::StorageType::Val(wasm::ValType::Ref(
                                    wasm::RefType {
                                        is_nullable: false,
                                        heap_type: wasm::HeapType::Abstract(
                                            wasm::AbsHeapType::Func,
                                        ),
                                    },
                                )),
                                mutability: wasm::Mutability::Const,
                            },
                        ))
                        .chain(data.fields().map(|field_name| {
                            (
                                field_name,
                                wasm::FieldType {
                                    storage_type: wasm::StorageType::Val(
                                        self.metadata.obj_val_type(false),
                                    ),
                                    mutability: wasm::Mutability::Const,
                                },
                            )
                        })),
                    )),
                },
            );
        }
    }

    pub fn emit_clsr_types(&mut self) {
        for (arity, type_name) in self.metadata.clsr_types() {
            self.module.add_type(
                type_name,
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from(
                            iter::once(self.metadata.envr_val_type())
                                .chain((0..arity).map(|_| self.metadata.obj_val_type(false))),
                        ),
                        outputs: wasm::ResultType::from([self.metadata.obj_val_type(false)]),
                    }),
                },
            );
        }
    }

    pub fn emit_func_types(&mut self) {
        for (arity, type_name) in self.metadata.func_types() {
            self.module.add_type(
                type_name,
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from(
                            (0..arity).map(|_| self.metadata.obj_val_type(false)),
                        ),
                        outputs: wasm::ResultType::from([self.metadata.obj_val_type(false)]),
                    }),
                },
            );
        }
    }

    pub fn emit_let_const(&mut self, name: &'a cont::ValueName, value: &'a cont::ConstValue) {
        let mut expr = Default::default();

        Emitter::new(Context::new_const(self.metadata), &mut expr).emit_const_value(value);

        self.module.add_global(
            self.metadata.find_const(name),
            wasm::Global {
                global_type: wasm::GlobalType {
                    val_type: self.metadata.obj_val_type(false),
                    mutability: wasm::Mutability::Const,
                },
                expr,
            },
        );

        self.module.add_export(
            self.metadata.find_const(name).string,
            wasm::Export::Global(self.metadata.find_const(name)),
        );
    }

    pub fn emit_let_clsr(&mut self, name: &'a cont::ClsrName, clsr: &'a cont::Clsr) {
        let mut locals = Default::default();
        let mut expr = Default::default();

        Emitter::new(
            Context::new_clsr(self.metadata, self.metadata.find_clsr(name), &mut locals),
            &mut expr,
        )
        .emit_root_region(&clsr.region);

        self.module.add_func(
            self.metadata.find_clsr(name).func_name(),
            wasm::Func {
                type_name: self.metadata.find_clsr_type(clsr.params.len()),
                params: iter::once(self.metadata.special_local())
                    .chain(clsr.params.iter().map(|value_name| {
                        self.metadata
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
            self.metadata.find_clsr(name).func_name().string,
            wasm::Export::Func(self.metadata.find_clsr(name).func_name()),
        );
    }

    pub fn emit_let_func(&mut self, name: &'a cont::FuncName, func: &'a cont::Func) {
        let mut locals = Default::default();
        let mut expr = Default::default();

        Emitter::new(
            Context::new_func(self.metadata, self.metadata.find_func(name), &mut locals),
            &mut expr,
        )
        .emit_root_region(&func.region);

        self.module.add_func(
            self.metadata.find_func(name).func_name(),
            wasm::Func {
                type_name: self.metadata.find_func_type(func.params.len()),
                params: func
                    .params
                    .iter()
                    .map(|value_name| {
                        self.metadata
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
            self.metadata.find_func(name).func_name().string,
            wasm::Export::Func(self.metadata.find_func(name).func_name()),
        );
    }

    pub fn emit_module(&mut self, module: &'a cont::Module) {
        self.emit_obj_type();
        self.emit_int_type();
        self.emit_flt_type();
        self.emit_envr_type();
        self.emit_clsr_envr_types();
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
