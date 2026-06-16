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
                comp_type: wasm::CompType::Array(wasm::ArrayType {
                    field_type: wasm::FieldType {
                        storage_type: wasm::StorageType::Packed(wasm::PackedType::I8),
                        mutability: wasm::Mutability::Var,
                    },
                }),
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
                        outputs: wasm::ResultType::from([bin_ref.clone()]),
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

        if self.table.flt_to_le_bin_used() {
            let flt_to_le_bin_type = wasm::TypeName::from("flt_to_le_bin_type");
            self.module.add_type(
                flt_to_le_bin_type.clone(),
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
                "flt_to_le_bin",
                wasm::Import::Func {
                    func_name: self.table.flt_to_le_bin_func().clone(),
                    type_name: flt_to_le_bin_type,
                },
            );
        }
    }

    fn emit_sys_imports(&mut self) {
        let bin_ref = wasm::ValType::Ref(wasm::RefType {
            is_nullable: false,
            heap_type: wasm::HeapType::Concrete(self.table.bin_type()),
        });

        let i32_val = wasm::ValType::Num(wasm::NumType::I32);
        // Scalar *results* cross the boundary pre-boxed as i31 refs so they
        // can land directly in anyref block params; scalar *params* stay raw
        // i32 (the call site unboxes via `LoadAs::Nat` as usual).
        let status_ref = wasm::ValType::Ref(self.table.int_type(false));

        if self.table.io_read_used() {
            let io_read = wasm::TypeName::from("io_read");
            self.module.add_type(
                io_read.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([i32_val.clone(), i32_val.clone()]),
                        outputs: wasm::ResultType::from([status_ref.clone(), bin_ref.clone()]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_read",
                wasm::Import::Func {
                    func_name: self.table.io_read_func().clone(),
                    type_name: io_read,
                },
            );
        }

        if self.table.io_write_used() {
            let io_write = wasm::TypeName::from("io_write");
            self.module.add_type(
                io_write.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([i32_val.clone(), bin_ref.clone()]),
                        outputs: wasm::ResultType::from([status_ref.clone()]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_write",
                wasm::Import::Func {
                    func_name: self.table.io_write_func().clone(),
                    type_name: io_write,
                },
            );
        }

        if self.table.io_open_used() {
            let io_open = wasm::TypeName::from("io_open");
            self.module.add_type(
                io_open.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([bin_ref, i32_val.clone()]),
                        outputs: wasm::ResultType::from([status_ref.clone(), status_ref]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_open",
                wasm::Import::Func {
                    func_name: self.table.io_open_func().clone(),
                    type_name: io_open,
                },
            );
        }

        if self.table.io_connect_used() {
            // `bin_ref`/`status_ref` were moved by the `io_open` block above, so
            // recompute them (the `io_connect` signature is `(host, port,
            // connect_timeout, read_timeout, write_timeout) -> (status, handle)`).
            let bin_ref = wasm::ValType::Ref(wasm::RefType {
                is_nullable: false,
                heap_type: wasm::HeapType::Concrete(self.table.bin_type()),
            });
            let status_ref = wasm::ValType::Ref(self.table.int_type(false));
            let io_connect = wasm::TypeName::from("io_connect");
            self.module.add_type(
                io_connect.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([
                            bin_ref,
                            i32_val.clone(),
                            i32_val.clone(),
                            i32_val.clone(),
                            i32_val.clone(),
                        ]),
                        outputs: wasm::ResultType::from([status_ref.clone(), status_ref]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_connect",
                wasm::Import::Func {
                    func_name: self.table.io_connect_func().clone(),
                    type_name: io_connect,
                },
            );
        }

        if self.table.io_close_used() {
            let io_close = wasm::TypeName::from("io_close");
            self.module.add_type(
                io_close.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([i32_val]),
                        outputs: wasm::ResultType::from([]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_close",
                wasm::Import::Func {
                    func_name: self.table.io_close_func().clone(),
                    type_name: io_close,
                },
            );
        }

        if self.table.io_listen_used() {
            // `(host, port) -> (status, handle)` — the same shape as `io_open`.
            // Recompute the reference types, moved by the blocks above.
            let bin_ref = wasm::ValType::Ref(wasm::RefType {
                is_nullable: false,
                heap_type: wasm::HeapType::Concrete(self.table.bin_type()),
            });
            let status_ref = wasm::ValType::Ref(self.table.int_type(false));
            let io_listen = wasm::TypeName::from("io_listen");
            self.module.add_type(
                io_listen.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([
                            bin_ref,
                            wasm::ValType::Num(wasm::NumType::I32),
                        ]),
                        outputs: wasm::ResultType::from([status_ref.clone(), status_ref]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_listen",
                wasm::Import::Func {
                    func_name: self.table.io_listen_func().clone(),
                    type_name: io_listen,
                },
            );
        }

        if self.table.io_accept_used() {
            // `(handle) -> (status, handle)`: one i32 in, a status record out.
            let status_ref = wasm::ValType::Ref(self.table.int_type(false));
            let io_accept = wasm::TypeName::from("io_accept");
            self.module.add_type(
                io_accept.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([wasm::ValType::Num(wasm::NumType::I32)]),
                        outputs: wasm::ResultType::from([status_ref.clone(), status_ref]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_accept",
                wasm::Import::Func {
                    func_name: self.table.io_accept_func().clone(),
                    type_name: io_accept,
                },
            );
        }

        // `bin_ref`/`status_ref` above are moved by the `io_open` block, so the
        // clock/random imports recompute their own i31 and bin reference types.
        let i31_ref = wasm::ValType::Ref(self.table.int_type(false));
        let bin_out = wasm::ValType::Ref(wasm::RefType {
            is_nullable: false,
            heap_type: wasm::HeapType::Concrete(self.table.bin_type()),
        });

        if self.table.io_clock_wall_used() {
            let io_clock_wall = wasm::TypeName::from("io_clock_wall");
            self.module.add_type(
                io_clock_wall.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([]),
                        outputs: wasm::ResultType::from([
                            i31_ref.clone(),
                            i31_ref.clone(),
                            i31_ref.clone(),
                        ]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_clock_wall",
                wasm::Import::Func {
                    func_name: self.table.io_clock_wall_func().clone(),
                    type_name: io_clock_wall,
                },
            );
        }

        if self.table.io_clock_mono_used() {
            let io_clock_mono = wasm::TypeName::from("io_clock_mono");
            self.module.add_type(
                io_clock_mono.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([]),
                        outputs: wasm::ResultType::from([i31_ref.clone(), i31_ref.clone()]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_clock_mono",
                wasm::Import::Func {
                    func_name: self.table.io_clock_mono_func().clone(),
                    type_name: io_clock_mono,
                },
            );
        }

        if self.table.io_random_used() {
            let io_random = wasm::TypeName::from("io_random");
            self.module.add_type(
                io_random.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([wasm::ValType::Num(wasm::NumType::I32)]),
                        outputs: wasm::ResultType::from([bin_out.clone()]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_random",
                wasm::Import::Func {
                    func_name: self.table.io_random_func().clone(),
                    type_name: io_random,
                },
            );
        }

        if self.table.io_args_used() {
            // argv crosses as the module's uniform `Arr(Bin)` (emitted before
            // imports), each element a `bin_ref`.
            let arr_ref = wasm::ValType::Ref(wasm::RefType {
                is_nullable: false,
                heap_type: wasm::HeapType::Concrete(self.table.arr_type()),
            });
            let io_args = wasm::TypeName::from("io_args");
            self.module.add_type(
                io_args.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([]),
                        outputs: wasm::ResultType::from([arr_ref]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_args",
                wasm::Import::Func {
                    func_name: self.table.io_args_func().clone(),
                    type_name: io_args,
                },
            );
        }

        if self.table.io_env_used() {
            let io_env = wasm::TypeName::from("io_env");
            self.module.add_type(
                io_env.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([bin_out.clone()]),
                        outputs: wasm::ResultType::from([i31_ref.clone(), bin_out.clone()]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_env",
                wasm::Import::Func {
                    func_name: self.table.io_env_func().clone(),
                    type_name: io_env,
                },
            );
        }

        if self.table.io_exit_used() {
            let io_exit = wasm::TypeName::from("io_exit");
            self.module.add_type(
                io_exit.clone(),
                wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: wasm::CompType::Func(wasm::FuncType {
                        inputs: wasm::ResultType::from([wasm::ValType::Num(wasm::NumType::I32)]),
                        outputs: wasm::ResultType::from([]),
                    }),
                },
            );
            self.module.add_import(
                "env",
                "io_exit",
                wasm::Import::Func {
                    func_name: self.table.io_exit_func().clone(),
                    type_name: io_exit,
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
