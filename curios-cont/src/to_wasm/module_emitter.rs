use {
    super::{Context, ExprEmitter, Table},
    crate as cont, curios_abi as abi, curios_wasm as wasm,
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
                comp_type: wasm::CompType::Array(wasm::ArrayType {
                    field_type: wasm::FieldType {
                        storage_type: wasm::StorageType::Packed(wasm::PackedType::I8),
                        mutability: wasm::Mutability::Var,
                    },
                }),
            },
        );
    }

    /// The wasm-level type of a host-import *parameter* of the given wire
    /// type: scalars cross as raw `i32` (the call site unboxes the i31 carrier
    /// via `LoadAs::Nat`/`LoadAs::Int`), references as their concrete
    /// non-nullable heap type (a handle is its `Bin` token).
    fn wire_param_type(&self, wire_type: abi::WireType) -> wasm::ValType {
        match wire_type {
            abi::WireType::Nat | abi::WireType::Bln | abi::WireType::Int => {
                wasm::ValType::Num(wasm::NumType::I32)
            }
            abi::WireType::Bin | abi::WireType::Io => wasm::ValType::Ref(wasm::RefType {
                is_nullable: false,
                heap_type: wasm::HeapType::Concrete(self.table.bin_type()),
            }),
            abi::WireType::Arr(_) => wasm::ValType::Ref(wasm::RefType {
                is_nullable: false,
                heap_type: wasm::HeapType::Concrete(self.table.arr_type()),
            }),
        }
    }

    /// The wasm-level type of a host-import *result*: scalars re-enter
    /// pre-boxed as i31 refs so they land directly in anyref block params
    /// (no host op returns an `Int` today; mapping it like `Nat` keeps the
    /// function total), references exactly as in parameter position.
    fn wire_result_type(&self, wire_type: abi::WireType) -> wasm::ValType {
        match wire_type {
            abi::WireType::Nat | abi::WireType::Bln | abi::WireType::Int => {
                wasm::ValType::Ref(self.table.int_type(false))
            }
            reference => self.wire_param_type(reference),
        }
    }

    /// Declare a host import: a final func type plus an `("env", name)` import
    /// bound to `func_name`.
    fn add_host_import(
        &mut self,
        name: &str,
        type_name: wasm::TypeName,
        func_name: wasm::FuncName,
        inputs: wasm::ResultType,
        outputs: wasm::ResultType,
    ) {
        self.module.add_type(
            type_name.clone(),
            wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: wasm::CompType::Func(wasm::FuncType { inputs, outputs }),
            },
        );
        self.module.add_import(
            "env",
            name,
            wasm::Import::Func {
                func_name,
                type_name,
            },
        );
    }

    fn emit_sys_imports(&mut self) {
        let i32_val = wasm::ValType::Num(wasm::NumType::I32);

        // The table-described imports, in `ALL` (= `/sys/Io` prelude) order —
        // only the functions whose call sites interned their table cell.
        for function in abi::HostFunction::ALL {
            if !self.table.host_func_used(function) {
                continue;
            }

            let signature = function.signature();

            self.add_host_import(
                function.name(),
                wasm::TypeName::from(function.name()),
                self.table.host_func(function).clone(),
                wasm::ResultType::from(
                    signature
                        .params
                        .iter()
                        .map(|(_, wire_type)| self.wire_param_type(*wire_type)),
                ),
                wasm::ResultType::from(
                    signature
                        .results
                        .iter()
                        .map(|(_, wire_type)| self.wire_result_type(*wire_type)),
                ),
            );
        }

        if self.table.io_exit_used() {
            self.add_host_import(
                "io_exit",
                wasm::TypeName::from("io_exit"),
                self.table.io_exit_func().clone(),
                wasm::ResultType::from([i32_val.clone()]),
                wasm::ResultType::from([]),
            );
        }
    }

    fn emit_arr_type(&mut self) {
        self.module.add_type(
            self.table.arr_type(),
            wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: wasm::CompType::Array(wasm::ArrayType {
                    field_type: wasm::FieldType {
                        storage_type: wasm::StorageType::Val(self.table.top_type(true)),
                        mutability: self.table.arr_field_mutability(),
                    },
                }),
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

    fn emit_cell_type(&mut self) {
        self.module.add_type(
            self.table.cell_type(),
            wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: wasm::CompType::Struct(wasm::StructType::from([(
                    self.table.special_field(),
                    wasm::FieldType {
                        storage_type: wasm::StorageType::Val(self.table.top_type(true)),
                        mutability: wasm::Mutability::Var,
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
                                    mutability: self.table.tpl_field_mutability(),
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
                            mutability: self.table.envr_special_mutability(arity),
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
                                // Must agree with the shared `envr/N` special field above.
                                mutability: self.table.envr_special_mutability(data.arity()),
                            },
                        ))
                        .chain(data.fields().map(|field_name| {
                            (
                                field_name,
                                wasm::FieldType {
                                    storage_type: wasm::StorageType::Val(self.table.top_type(true)),
                                    // Payload captures are back-patched only when this closure
                                    // is itself a recursive shell; otherwise they're immutable.
                                    mutability: self.table.envr_payload_mutability(data.name()),
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
        let data_name = wasm::DataName::from(format!(
            "{}${}",
            name.as_string(),
            self.module.datas().len()
        ));

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

    /// Emit a module-level const. Every global is declared mutable so that
    /// aggregate (`Tpl`/`Arr`/`Clsr`) consts can `global.get` their dependencies
    /// inside the start function — wasm constant expressions can only read
    /// immutable globals. Scalars (`Nat`/`Int`/`Flt`) keep a self-contained
    /// const initializer (mutability is harmless when the init is constant);
    /// `Bin` and aggregates declare a placeholder init and build the real value
    /// in the start function. `Bin` is special-cased via [`Self::emit_let_bin_data`]
    /// because its payload comes from a data segment.
    fn emit_let_data(&mut self, name: &'a cont::ValueName, value: &'a cont::Data) {
        match value {
            cont::Data::Bin(bytes) => {
                self.emit_let_bin_data(name, bytes);
            }
            cont::Data::Nat(_) | cont::Data::Int(_) | cont::Data::Flt(_) => {
                let mut expr = Default::default();
                ExprEmitter::new(Context::new_const(self.table), self.module, &mut expr)
                    .emit_data(name, value);
                self.module.add_global(
                    self.table.find_const(name),
                    wasm::Global {
                        global_type: wasm::GlobalType {
                            val_type: self.table.top_type(false),
                            mutability: wasm::Mutability::Var,
                        },
                        expr,
                    },
                );
            }
            cont::Data::Tpl(_) | cont::Data::Arr(_) | cont::Data::Clsr(_, _) => {
                let global_name = self.table.find_const(name);

                let mut init_expr: wasm::Expr = Default::default();
                init_expr.push(wasm::Instr::I32Const { value: 0 });
                init_expr.push(wasm::Instr::RefI31);

                self.module.add_global(
                    global_name.clone(),
                    wasm::Global {
                        global_type: wasm::GlobalType {
                            val_type: self.table.top_type(false),
                            mutability: wasm::Mutability::Var,
                        },
                        expr: init_expr,
                    },
                );

                ExprEmitter::new(
                    Context::new_const(self.table),
                    self.module,
                    &mut self.start_expr,
                )
                .emit_data(name, value);
                self.start_expr.push(wasm::Instr::GlobalSet { global_name });
            }
        }
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
                    .chain(clsr.params.iter().map(|param| {
                        self.table
                            .find_clsr(name)
                            .find_param(&param.name)
                            .unwrap_or_else(|| panic!("`ClsrData` lacks param `{}`", param.name))
                    }))
                    .collect(),
                locals,
                expr,
            },
        );

        // Closures are referenced by `ref.func` when their values are built, so
        // they must be declared even though they are not exported.
        self.module
            .declare_func(self.table.find_clsr(name).func_name());
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
                    .map(|param| {
                        self.table
                            .find_func(name)
                            .find_param(&param.name)
                            .unwrap_or_else(|| panic!("`FuncData` lacks param `{}`", param.name))
                    })
                    .collect(),
                locals,
                expr,
            },
        );
    }

    pub fn emit_module(&mut self, module: &'a cont::Module) {
        self.emit_flt_type();
        self.emit_bin_type();
        self.emit_arr_type();
        self.emit_cell_type();
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

        // The entrypoint is the module's sole export — the value the host invokes.
        // Every other function, closure, and const is reached only internally.
        if let Some(name) = module.entry() {
            let func_name = self.table.find_func(name).func_name();
            self.module
                .add_export(func_name.as_string(), wasm::Export::Func(func_name));
        }

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
