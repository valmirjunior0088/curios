use {
    super::{Context, ExprEmitter, Table},
    curios_abi::WireType,
    curios_wasm::{
        ArrayType, CompType, DataName, DataSegment, Export, Expr, FieldType, Func, FuncName,
        FuncType, Global, GlobalType, HeapType, Import, Instr, Module, Mutability, NumType,
        PackedType, RefType, ResultType, StorageType, StructType, SubType, TypeName, ValType,
    },
    std::iter,
};

#[derive(Debug)]
pub struct ModuleEmitter<'a, 'b> {
    table: &'a Table<'a>,
    start_expr: Expr,
    module: &'b mut Module,
}

impl<'a, 'b> ModuleEmitter<'a, 'b> {
    pub fn new(table: &'a Table<'a>, module: &'b mut Module) -> Self {
        Self {
            table,
            start_expr: Default::default(),
            module,
        }
    }

    fn emit_bin_type(&mut self) {
        self.module.add_type(
            self.table.bin_type(),
            SubType {
                is_final: true,
                super_types: vec![],
                comp_type: CompType::Array(ArrayType {
                    field_type: FieldType {
                        storage_type: StorageType::Packed(PackedType::I8),
                        mutability: Mutability::Var,
                    },
                }),
            },
        );
    }

    /// The wasm-level type of a host-import *parameter* of the given wire
    /// type: scalars cross as raw `i32` (the call site unboxes the i31 carrier
    /// via `LoadAs::Nat`/`LoadAs::Int`), references as their concrete
    /// non-nullable heap type (a handle is its `Bin` token).
    fn wire_param_type(&self, wire_type: &WireType) -> ValType {
        match wire_type {
            WireType::Nat | WireType::Bln | WireType::Int => ValType::Num(NumType::I32),
            WireType::Bin | WireType::Io => ValType::Ref(RefType {
                is_nullable: false,
                heap_type: HeapType::Concrete(self.table.bin_type()),
            }),
            WireType::Arr(_) => ValType::Ref(RefType {
                is_nullable: false,
                heap_type: HeapType::Concrete(self.table.arr_type()),
            }),
        }
    }

    /// The wasm-level type of a host-import *result*: scalars re-enter
    /// pre-boxed as i31 refs so they land directly in anyref block params
    /// (no host op returns an `Int` today; mapping it like `Nat` keeps the
    /// function total), references exactly as in parameter position.
    fn wire_result_type(&self, wire_type: &WireType) -> ValType {
        match wire_type {
            WireType::Nat | WireType::Bln | WireType::Int => {
                ValType::Ref(self.table.int_type(false))
            }
            reference => self.wire_param_type(reference),
        }
    }

    /// Declare a host import: a final func type plus an `("env", name)` import
    /// bound to `func_name`.
    fn add_host_import(
        &mut self,
        name: &str,
        type_name: TypeName,
        func_name: FuncName,
        inputs: ResultType,
        outputs: ResultType,
    ) {
        self.module.add_type(
            type_name.clone(),
            SubType {
                is_final: true,
                super_types: vec![],
                comp_type: CompType::Func(FuncType { inputs, outputs }),
            },
        );
        self.module.add_import(
            "env",
            name,
            Import::Func {
                func_name,
                type_name,
            },
        );
    }

    fn emit_sys_imports(&mut self) {
        let i32_val = ValType::Num(NumType::I32);

        // The store-described imports — exactly the functions whose call sites
        // recorded themselves in the table, in import-name order.
        for function in self.table.host_funcs() {
            let signature = &function.signature;

            self.add_host_import(
                &function.name,
                TypeName::from(function.name.as_str()),
                self.table.host_func(&function),
                ResultType::from(
                    signature
                        .params
                        .iter()
                        .map(|(_, wire_type)| self.wire_param_type(wire_type)),
                ),
                ResultType::from(
                    signature
                        .results
                        .iter()
                        .map(|(_, wire_type)| self.wire_result_type(wire_type)),
                ),
            );
        }

        if self.table.io_exit_used() {
            self.add_host_import(
                "io_exit",
                TypeName::from("io_exit"),
                self.table.io_exit_func().clone(),
                ResultType::from([i32_val.clone()]),
                ResultType::from([]),
            );
        }
    }

    fn emit_arr_type(&mut self) {
        self.module.add_type(
            self.table.arr_type(),
            SubType {
                is_final: true,
                super_types: vec![],
                comp_type: CompType::Array(ArrayType {
                    field_type: FieldType {
                        storage_type: StorageType::Val(self.table.top_type(true)),
                        mutability: self.table.arr_field_mutability(),
                    },
                }),
            },
        );
    }

    fn emit_flt_type(&mut self) {
        self.module.add_type(
            self.table.flt_type(),
            SubType {
                is_final: true,
                super_types: vec![],
                comp_type: CompType::Struct(StructType::from([(
                    self.table.special_field(),
                    FieldType {
                        storage_type: StorageType::Val(ValType::Num(NumType::F32)),
                        mutability: Mutability::Const,
                    },
                )])),
            },
        );
    }

    fn emit_cell_type(&mut self) {
        self.module.add_type(
            self.table.cell_type(),
            SubType {
                is_final: true,
                super_types: vec![],
                comp_type: CompType::Struct(StructType::from([(
                    self.table.special_field(),
                    FieldType {
                        storage_type: StorageType::Val(self.table.top_type(true)),
                        mutability: Mutability::Var,
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
                SubType {
                    is_final: false,
                    super_types,
                    comp_type: CompType::Struct(StructType::from((0..arity).map(|index| {
                        (
                            self.table.tpl_field(index),
                            FieldType {
                                storage_type: StorageType::Val(self.table.top_type(true)),
                                mutability: self.table.tpl_field_mutability(),
                            },
                        )
                    }))),
                },
            );
        }
    }

    fn emit_envr_arity_types(&mut self) {
        for (arity, type_name) in self.table.envr_types() {
            self.module.add_type(
                type_name,
                SubType {
                    is_final: false,
                    super_types: vec![],
                    comp_type: CompType::Struct(StructType::from([(
                        self.table.special_field(),
                        FieldType {
                            storage_type: StorageType::Val(ValType::Ref(RefType {
                                is_nullable: true,
                                heap_type: HeapType::Concrete(self.table.find_clsr_type(arity)),
                            })),
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
                SubType {
                    is_final: true,
                    super_types: vec![self.table.find_envr_type(data.arity())],
                    comp_type: CompType::Struct(StructType::from(
                        iter::once((
                            self.table.special_field(),
                            FieldType {
                                storage_type: StorageType::Val(ValType::Ref(RefType {
                                    is_nullable: true,
                                    heap_type: HeapType::Concrete(
                                        self.table.find_clsr_type(data.arity()),
                                    ),
                                })),
                                // Must agree with the shared `envr/N` special field above.
                                mutability: self.table.envr_special_mutability(data.arity()),
                            },
                        ))
                        .chain(data.fields().map(|field_name| {
                            (
                                field_name,
                                FieldType {
                                    storage_type: StorageType::Val(self.table.top_type(true)),
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
                SubType {
                    is_final: false,
                    super_types: vec![],
                    comp_type: CompType::Func(FuncType {
                        inputs: ResultType::from(
                            iter::once(self.table.top_type(false))
                                .chain((0..arity).map(|_| self.table.top_type(false))),
                        ),
                        outputs: ResultType::from([self.table.top_type(false)]),
                    }),
                },
            );
        }
    }

    fn emit_clsr_named_types(&mut self) {
        for data in self.table.clsrs() {
            self.module.add_type(
                data.clsr_type(),
                SubType {
                    is_final: true,
                    super_types: vec![self.table.find_clsr_type(data.arity())],
                    comp_type: CompType::Func(FuncType {
                        inputs: ResultType::from(
                            iter::once(self.table.top_type(false))
                                .chain((0..data.arity()).map(|_| self.table.top_type(false))),
                        ),
                        outputs: ResultType::from([self.table.top_type(false)]),
                    }),
                },
            );
        }
    }

    fn emit_func_types(&mut self) {
        for (arity, type_name) in self.table.func_types() {
            self.module.add_type(
                type_name,
                SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: CompType::Func(FuncType {
                        inputs: ResultType::from((0..arity).map(|_| self.table.top_type(false))),
                        outputs: ResultType::from([self.table.top_type(false)]),
                    }),
                },
            );
        }
    }

    fn emit_let_bin_data(&mut self, name: &'a crate::ValueName, bytes: &[u8]) {
        let bin_type = self.table.bin_type();
        let global_name = self.table.find_const(name);
        let data_name = DataName::from(format!(
            "{}${}",
            name.as_string(),
            self.module.datas().len()
        ));

        self.module.add_data(
            data_name.clone(),
            DataSegment {
                bytes: bytes.to_vec(),
            },
        );

        let mut init_expr: Expr = Default::default();
        init_expr.push(Instr::I32Const { value: 0 });
        init_expr.push(Instr::ArrayNewDefault {
            type_name: bin_type.clone(),
        });

        self.module.add_global(
            global_name.clone(),
            Global {
                global_type: GlobalType {
                    val_type: ValType::Ref(RefType {
                        is_nullable: false,
                        heap_type: HeapType::Concrete(bin_type.clone()),
                    }),
                    mutability: Mutability::Var,
                },
                expr: init_expr,
            },
        );

        self.module
            .add_export(global_name.as_string(), Export::Global(global_name.clone()));

        self.start_expr.push(Instr::I32Const { value: 0 });
        self.start_expr.push(Instr::I32Const {
            value: bytes.len() as i32,
        });
        self.start_expr.push(Instr::ArrayNewData {
            type_name: bin_type,
            data_name,
        });
        self.start_expr.push(Instr::GlobalSet { global_name });
    }

    /// Emit a module-level const. Every global is declared mutable so that
    /// aggregate (`Tpl`/`Arr`/`Clsr`) consts can `global.get` their dependencies
    /// inside the start function — wasm constant expressions can only read
    /// immutable globals. Scalars (`Nat`/`Int`/`Flt`) keep a self-contained
    /// const initializer (mutability is harmless when the init is constant);
    /// `Bin` and aggregates declare a placeholder init and build the real value
    /// in the start function. `Bin` is special-cased via [`Self::emit_let_bin_data`]
    /// because its payload comes from a data segment.
    fn emit_let_data(&mut self, name: &'a crate::ValueName, value: &'a crate::Data) {
        match value {
            crate::Data::Bin(bytes) => {
                self.emit_let_bin_data(name, bytes);
            }
            crate::Data::Nat(_) | crate::Data::Int(_) | crate::Data::Flt(_) => {
                let mut expr = Default::default();
                ExprEmitter::new(Context::new_const(self.table), self.module, &mut expr)
                    .emit_data(name, value);
                self.module.add_global(
                    self.table.find_const(name),
                    Global {
                        global_type: GlobalType {
                            val_type: self.table.top_type(false),
                            mutability: Mutability::Var,
                        },
                        expr,
                    },
                );
            }
            crate::Data::Tpl(_) | crate::Data::Arr(_) | crate::Data::Clsr(_, _) => {
                let global_name = self.table.find_const(name);

                let mut init_expr: Expr = Default::default();
                init_expr.push(Instr::I32Const { value: 0 });
                init_expr.push(Instr::RefI31);

                self.module.add_global(
                    global_name.clone(),
                    Global {
                        global_type: GlobalType {
                            val_type: self.table.top_type(false),
                            mutability: Mutability::Var,
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
                self.start_expr.push(Instr::GlobalSet { global_name });
            }
        }
    }

    fn emit_let_clsr(&mut self, name: &'a crate::ClsrName, clsr: &'a crate::Clsr) {
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
            Func {
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

    fn emit_let_func(&mut self, name: &'a crate::FuncName, func: &'a crate::Func) {
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
            Func {
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

    pub fn emit_module(&mut self, module: &'a crate::Module) {
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
                .add_export(func_name.as_string(), Export::Func(func_name));
        }

        self.emit_sys_imports();

        let start_type_name = TypeName::from("start");

        self.module.add_type(
            start_type_name.clone(),
            SubType {
                is_final: true,
                super_types: vec![],
                comp_type: CompType::Func(FuncType {
                    inputs: ResultType::from([]),
                    outputs: ResultType::from([]),
                }),
            },
        );

        let start_func_name = FuncName::from("start");

        self.module.add_func(
            start_func_name.clone(),
            Func {
                type_name: start_type_name,
                params: vec![],
                locals: vec![],
                expr: self.start_expr.clone(),
            },
        );

        self.module.set_start(start_func_name);
    }
}
