use {
    super::{
        Context, EmissionClosure, EmissionClosureName, EmissionData, EmissionFunction,
        EmissionFunctionName, EmissionModule, EmissionValueName, ExprEmitter, RopeEmitter, Table,
        bytes_sub_type, cell_sub_type, elems_sub_type, flt_sub_type, rope_base_sub_type,
        rope_leaf_sub_type, rope_node_sub_type, rope_view_sub_type,
    },
    crate::CpsSlot,
    curios_abi::{Namespace, WireType},
    curios_utilities::{Grain, PackedBin},
    std::iter,
};

#[derive(Debug)]
pub(crate) struct ModuleEmitter<'a, 'b> {
    table: &'a Table<'a>,
    start_expr: curios_wasm::Expr,
    module: &'b mut curios_wasm::Module,
}

impl<'a, 'b> ModuleEmitter<'a, 'b> {
    pub(crate) fn new(table: &'a Table<'a>, module: &'b mut curios_wasm::Module) -> Self {
        Self {
            table,
            start_expr: Default::default(),
            module,
        }
    }

    /// The binary rope row: the flat `$bytes` payload (the host-boundary shape), the `$rope/bin` base, and its `leaf`/`node`/`view` subtypes. Each is its own singleton recursion group — `$bytes` must canonicalize equal to the type curios-js's bridge declares standalone, and a subtype may reference any *earlier* group.
    fn emit_bin_rope_types(&mut self) {
        let rope = self.table.bin_rope();

        self.module.add_type(rope.payload.clone(), bytes_sub_type());
        self.module.add_type(
            rope.base.clone(),
            rope_base_sub_type(rope.tag_field.clone(), rope.len_field.clone()),
        );
        self.module.add_type(
            rope.leaf.clone(),
            rope_leaf_sub_type(
                rope.base.clone(),
                rope.tag_field.clone(),
                rope.len_field.clone(),
                rope.payload_field.clone(),
                rope.payload.clone(),
            ),
        );
        self.module.add_type(
            rope.node.clone(),
            rope_node_sub_type(
                rope.base.clone(),
                rope.tag_field.clone(),
                rope.len_field.clone(),
                rope.left_field,
                rope.right_field,
                rope.cache_field,
                rope.payload,
            ),
        );
        self.module.add_type(
            rope.view.clone(),
            rope_view_sub_type(
                rope.base,
                rope.tag_field,
                rope.len_field,
                rope.base_field,
                rope.offset_field,
            ),
        );
    }

    /// The wasm-level type of a host-import *parameter* of the given wire type: scalars cross as raw `i32` (the call site unboxes the i31 carrier via `LoadAs::Nat`/`LoadAs::Int`), references as their concrete non-nullable heap type (a handle is its `Bytes` token).
    fn wire_param_type(&self, wire_type: &WireType) -> curios_wasm::ValType {
        match wire_type {
            WireType::Nat | WireType::Bool | WireType::Int => {
                curios_wasm::ValType::Num(curios_wasm::NumType::I32)
            }
            WireType::Bytes | WireType::Handle => curios_wasm::ValType::Ref(curios_wasm::RefType {
                is_nullable: false,
                heap_type: curios_wasm::HeapType::Concrete(self.table.bytes_type()),
            }),
            WireType::List(_) => curios_wasm::ValType::Ref(curios_wasm::RefType {
                is_nullable: false,
                heap_type: curios_wasm::HeapType::Concrete(self.table.elems_type()),
            }),
        }
    }

    /// The wasm-level type of a host-import *result*: scalars re-enter pre-boxed as i31 refs so they land directly in anyref block params (no host op returns an `Int` today; mapping it like `Nat` keeps the function total), references exactly as in parameter position.
    fn wire_result_type(&self, wire_type: &WireType) -> curios_wasm::ValType {
        match wire_type {
            WireType::Nat | WireType::Bool | WireType::Int => {
                curios_wasm::ValType::Ref(Table::int_type(false))
            }
            reference => self.wire_param_type(reference),
        }
    }

    /// Declare a host import: a final func type plus a `(namespace, name)` import bound to `func_name`.
    fn add_host_import(
        &mut self,
        namespace: &str,
        name: &str,
        type_name: curios_wasm::TypeName,
        func_name: curios_wasm::FuncName,
        inputs: curios_wasm::ResultType,
        outputs: curios_wasm::ResultType,
    ) {
        self.module.add_type(
            type_name.clone(),
            curios_wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: curios_wasm::CompType::Func(curios_wasm::FuncType { inputs, outputs }),
            },
        );
        self.module.add_import(
            namespace,
            name,
            curios_wasm::Import::Func {
                func_name,
                type_name,
            },
        );
    }

    fn emit_sys_imports(&mut self) {
        let i32_val = curios_wasm::ValType::Num(curios_wasm::NumType::I32);

        // The store-described imports — exactly the functions whose call sites recorded themselves in the table, in minted-name order. Each function's own `namespace` (stamped at declaration time — see `ForeignFunction`) is the wasm namespace it imports under, so codegen neither rebuilds `host_ops()` to re-derive membership nor chooses a namespace itself.
        for function in self.table.host_funcs() {
            let signature = &function.signature;
            let func_name = self.table.host_func(&function);

            self.add_host_import(
                function.namespace.as_str(),
                &function.name,
                curios_wasm::TypeName::from(func_name.as_str()),
                func_name.clone(),
                curios_wasm::ResultType::from(
                    signature
                        .params
                        .iter()
                        .map(|(_, wire_type)| self.wire_param_type(wire_type)),
                ),
                curios_wasm::ResultType::from(
                    signature
                        .results
                        .iter()
                        .map(|(_, wire_type)| self.wire_result_type(wire_type)),
                ),
            );
        }

        if self.table.exit_used() {
            self.add_host_import(
                Namespace::Sys.as_str(),
                "exit",
                curios_wasm::TypeName::from("exit"),
                self.table.exit_func().clone(),
                curios_wasm::ResultType::from([i32_val.clone()]),
                curios_wasm::ResultType::from([]),
            );
        }
    }

    /// The `List` mirror of [`emit_bin_rope_types`](Self::emit_bin_rope_types).
    fn emit_list_rope_types(&mut self) {
        let rope = self.table.list_rope();

        self.module
            .add_type(rope.payload.clone(), elems_sub_type(Table::top_type(true)));
        self.module.add_type(
            rope.base.clone(),
            rope_base_sub_type(rope.tag_field.clone(), rope.len_field.clone()),
        );
        self.module.add_type(
            rope.leaf.clone(),
            rope_leaf_sub_type(
                rope.base.clone(),
                rope.tag_field.clone(),
                rope.len_field.clone(),
                rope.payload_field.clone(),
                rope.payload.clone(),
            ),
        );
        self.module.add_type(
            rope.node.clone(),
            rope_node_sub_type(
                rope.base.clone(),
                rope.tag_field.clone(),
                rope.len_field.clone(),
                rope.left_field,
                rope.right_field,
                rope.cache_field,
                rope.payload,
            ),
        );
        self.module.add_type(
            rope.view.clone(),
            rope_view_sub_type(
                rope.base,
                rope.tag_field,
                rope.len_field,
                rope.base_field,
                rope.offset_field,
            ),
        );
    }

    fn emit_flt_type(&mut self) {
        self.module.add_type(
            self.table.flt_type(),
            flt_sub_type(self.table.special_field()),
        );
    }

    fn emit_cell_type(&mut self) {
        self.module.add_type(
            self.table.cell_type(),
            cell_sub_type(self.table.special_field(), Table::top_type(true)),
        );
    }

    fn emit_tuple_types(&mut self) {
        for (arity, type_name) in self.table.tuple_types() {
            self.module.add_type(
                type_name,
                curios_wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from(
                        (0..arity).map(|index| {
                            (
                                Table::tuple_field(index),
                                curios_wasm::FieldType {
                                    storage_type: curios_wasm::StorageType::Val(Table::top_type(
                                        true,
                                    )),
                                    mutability: curios_wasm::Mutability::Const,
                                },
                            )
                        }),
                    )),
                },
            );
        }
    }

    /// One final struct per nominal row: for a family, slot zero the tag and the rest the payload slots its constructors share; for a product, the schema's row outright. Final and unrelated to every other type, so a read of one is an exact cast — the reason rows are keyed here rather than by arity — and each field is declared at the carrier [`CpsSlot`] names rather than uniformly `anyref`, which is what lets a scalar payload live in a register and a list payload arrive already at its rope base.
    ///
    /// A family's tag is `i8`. Its constructor count is bounded by its declaration and no corpus family approaches the byte, so the discriminant packs into one and reads back through `struct.get_u` with no unboxing at all — the store side is the raw index, where a uniform slot wrote an `i31` reference.
    fn emit_row_types(&mut self) {
        let fields: Vec<_> = self
            .table
            .row_types()
            .map(|(_, type_name, slots)| {
                let fields: Vec<_> = slots
                    .iter()
                    .enumerate()
                    .map(|(index, slot)| {
                        (
                            Table::tuple_field(index),
                            curios_wasm::FieldType {
                                storage_type: self.slot_storage_type(*slot),
                                mutability: curios_wasm::Mutability::Const,
                            },
                        )
                    })
                    .collect();
                (type_name, fields)
            })
            .collect();

        // One recursion group for the whole roster: a row's slot may name another row, and a self-referential declaration names its own, so a forward reference is only well-formed inside a shared group.
        self.module
            .add_types(curios_wasm::RecType::from(fields.into_iter().map(
                |(type_name, fields)| {
                    (
                        type_name,
                        curios_wasm::SubType {
                            is_final: true,
                            super_types: vec![],
                            comp_type: curios_wasm::CompType::Struct(
                                curios_wasm::StructType::from(fields),
                            ),
                        },
                    )
                },
            )));
    }

    /// The wasm storage type one row slot is declared at.
    fn slot_storage_type(&self, slot: CpsSlot) -> curios_wasm::StorageType {
        let reference = |type_name| {
            curios_wasm::StorageType::Val(curios_wasm::ValType::Ref(curios_wasm::RefType {
                is_nullable: true,
                heap_type: curios_wasm::HeapType::Concrete(type_name),
            }))
        };
        match slot {
            CpsSlot::Tag => curios_wasm::StorageType::Packed(curios_wasm::PackedType::I8),
            CpsSlot::Nat | CpsSlot::Int => {
                curios_wasm::StorageType::Val(curios_wasm::ValType::Num(curios_wasm::NumType::I32))
            }
            CpsSlot::Flt => {
                curios_wasm::StorageType::Val(curios_wasm::ValType::Num(curios_wasm::NumType::F32))
            }
            CpsSlot::List => reference(self.table.list_rope().base.clone()),
            CpsSlot::Closure(arity) => reference(self.table.find_envr_type(arity)),
            CpsSlot::Row(row) => reference(self.table.find_row_type(row)),
            CpsSlot::Opaque => curios_wasm::StorageType::Val(Table::top_type(true)),
        }
    }

    fn emit_envr_arity_types(&mut self) {
        for type_name in self.table.envr_types() {
            self.module.add_type(
                type_name,
                curios_wasm::SubType {
                    is_final: false,
                    super_types: vec![],
                    comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([(
                        self.table.special_field(),
                        curios_wasm::FieldType {
                            // The code field is the body's index in the shared funcref table, not a funcref: writing an `i32` skips the engine's per-store funcref-to-GC-heap intern at every construction.
                            storage_type: curios_wasm::StorageType::Val(curios_wasm::ValType::Num(
                                curios_wasm::NumType::I32,
                            )),
                            mutability: curios_wasm::Mutability::Const,
                        },
                    )])),
                },
            );
        }
    }

    fn emit_clsr_types(&mut self, module: &'a EmissionModule) {
        for data in module
            .clsrs()
            .iter()
            .map(|(name, _)| self.table.find_clsr(name))
        {
            self.module.add_type(
                data.envr_type(),
                curios_wasm::SubType {
                    is_final: true,
                    super_types: vec![self.table.find_envr_type(data.arity())],
                    comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from(
                        iter::once((
                            self.table.special_field(),
                            curios_wasm::FieldType {
                                storage_type: curios_wasm::StorageType::Val(
                                    curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                                ),
                                mutability: curios_wasm::Mutability::Const,
                            },
                        ))
                        .chain(data.fields().map(|field_name| {
                            (
                                field_name,
                                curios_wasm::FieldType {
                                    storage_type: curios_wasm::StorageType::Val(Table::top_type(
                                        true,
                                    )),
                                    mutability: curios_wasm::Mutability::Const,
                                },
                            )
                        })),
                    )),
                },
            );
        }
    }

    /// One final func type per closure arity: every body of that arity is declared at it, and its arity's table is typed by it, so the `call_indirect` signature check is satisfied statically rather than at runtime.
    fn emit_clsr_arity_types(&mut self) {
        for (arity, type_name) in self.table.clsr_types() {
            self.module.add_type(
                type_name,
                curios_wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: curios_wasm::CompType::Func(curios_wasm::FuncType {
                        inputs: curios_wasm::ResultType::from(
                            iter::once(Table::top_type(false))
                                .chain((0..arity).map(|_| Table::top_type(false))),
                        ),
                        outputs: curios_wasm::ResultType::from([Table::top_type(false)]),
                    }),
                },
            );
        }
    }

    fn emit_func_types(&mut self) {
        for ((parameters, results), type_name) in self.table.func_types() {
            self.module.add_type(
                type_name,
                curios_wasm::SubType {
                    is_final: true,
                    super_types: vec![],
                    comp_type: curios_wasm::CompType::Func(curios_wasm::FuncType {
                        inputs: curios_wasm::ResultType::from(
                            (0..parameters).map(|_| Table::top_type(false)),
                        ),
                        outputs: curios_wasm::ResultType::from(
                            (0..results).map(|_| Table::top_type(false)),
                        ),
                    }),
                },
            );
        }
    }

    fn emit_let_bin_data(&mut self, name: &'a EmissionValueName, grain: Grain, value: &PackedBin) {
        // A small packed constant is its canonical immediate — a self-contained constant initializer, no data segment, no start-function code — exactly as the inline literal path emits it. Leaving this path on the leaf would mint the one non-canonical small value in the program, and the immediate equality would answer false against it.
        let envelope = match grain {
            Grain::X => 3,
            Grain::B => 26,
        };
        if value.len(grain) <= envelope {
            let len_shift = match grain {
                Grain::X => 29,
                Grain::B => 26,
            };
            let bytes = value.to_packed_bytes();
            let packed = bytes.iter().enumerate().fold(
                (value.len(grain) as i32) << len_shift,
                |packed, (index, &byte)| packed | (byte as i32) << (8 * index),
            );
            let mut init_expr: curios_wasm::Expr = Default::default();
            init_expr.push(curios_wasm::Instr::I32Const { value: packed });
            init_expr.push(curios_wasm::Instr::RefI31);
            self.module.add_global(
                self.table.find_const(name),
                curios_wasm::Global {
                    global_type: curios_wasm::GlobalType {
                        val_type: Table::top_type(false),
                        mutability: curios_wasm::Mutability::Var,
                    },
                    expr: init_expr,
                },
            );
            return;
        }

        let bytes = value.to_packed_bytes();
        let payload_length = bytes.len() as i32;
        let length = value.len(grain) as i32;
        let rope = self.table.bin_rope();
        let global_name = self.table.find_const(name);
        let data_name = curios_wasm::DataName::from(format!(
            "{}${}",
            name.as_string(),
            self.module.datas().len()
        ));

        self.module.add_data(
            data_name.clone(),
            curios_wasm::DataSegment {
                mode: curios_wasm::DataMode::Passive,
                bytes,
            },
        );

        // The placeholder init is an empty leaf — a wasm constant expression cannot read a data segment (or call), so the real payload is built in the start function below.
        let mut init_expr: curios_wasm::Expr = Default::default();
        init_expr.push(curios_wasm::Instr::I32Const { value: 0 });
        init_expr.push(curios_wasm::Instr::I32Const { value: 0 });
        init_expr.push(curios_wasm::Instr::I32Const { value: 0 });
        init_expr.push(curios_wasm::Instr::ArrayNewDefault {
            type_name: rope.payload.clone(),
        });
        init_expr.push(curios_wasm::Instr::StructNew {
            type_name: rope.leaf.clone(),
        });

        self.module.add_global(
            global_name.clone(),
            curios_wasm::Global {
                global_type: curios_wasm::GlobalType {
                    val_type: curios_wasm::ValType::Ref(curios_wasm::RefType {
                        is_nullable: false,
                        heap_type: curios_wasm::HeapType::Concrete(rope.base),
                    }),
                    mutability: curios_wasm::Mutability::Var,
                },
                expr: init_expr,
            },
        );

        self.start_expr
            .push(curios_wasm::Instr::I32Const { value: 0 });
        self.start_expr
            .push(curios_wasm::Instr::I32Const { value: length });
        self.start_expr
            .push(curios_wasm::Instr::I32Const { value: 0 });
        self.start_expr.push(curios_wasm::Instr::I32Const {
            value: payload_length,
        });
        self.start_expr.push(curios_wasm::Instr::ArrayNewData {
            type_name: rope.payload,
            data_name,
        });
        self.start_expr.push(curios_wasm::Instr::StructNew {
            type_name: rope.leaf,
        });
        self.start_expr
            .push(curios_wasm::Instr::GlobalSet { global_name });
    }

    /// Emit a module-level const. Every global is declared mutable so that aggregate (`Tuple`/`List`/`EmissionClosure`) consts can `global.get` their dependencies inside the start function — wasm constant expressions can only read immutable globals. Scalars (`Nat`/`Int`/`Flt`) keep a self-contained const initializer (mutability is harmless when the init is constant); `Bin` and aggregates declare a placeholder init and build the real value in the start function. `Bin` is special-cased via [`Self::emit_let_bin_data`] because its payload comes from a data segment.
    fn emit_let_data(&mut self, name: &'a EmissionValueName, value: &'a EmissionData) {
        match value {
            EmissionData::Bin(grain, value) => {
                self.emit_let_bin_data(name, *grain, value);
            }
            EmissionData::Nat(_) | EmissionData::Int(_) | EmissionData::Flt(_) => {
                let mut expr = Default::default();
                ExprEmitter::new(Context::new_const(self.table), self.module, &mut expr)
                    .emit_data(name, value);
                self.module.add_global(
                    self.table.find_const(name),
                    curios_wasm::Global {
                        global_type: curios_wasm::GlobalType {
                            val_type: Table::top_type(false),
                            mutability: curios_wasm::Mutability::Var,
                        },
                        expr,
                    },
                );
            }
            EmissionData::Tuple(_)
            | EmissionData::Row(..)
            | EmissionData::List(_)
            | EmissionData::Closure(_, _) => {
                let global_name = self.table.find_const(name);

                let mut init_expr: curios_wasm::Expr = Default::default();
                init_expr.push(curios_wasm::Instr::I32Const { value: 0 });
                init_expr.push(curios_wasm::Instr::RefI31);

                self.module.add_global(
                    global_name.clone(),
                    curios_wasm::Global {
                        global_type: curios_wasm::GlobalType {
                            val_type: Table::top_type(false),
                            mutability: curios_wasm::Mutability::Var,
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
                self.start_expr
                    .push(curios_wasm::Instr::GlobalSet { global_name });
            }
        }
    }

    fn emit_let_clsr(&mut self, name: &'a EmissionClosureName, clsr: &'a EmissionClosure) {
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
            curios_wasm::Func {
                // Declared at the arity type itself, not a named subtype: the body's `ref.func` then types as `(ref $clsr/N)`, which is exactly what its arity's table holds.
                type_name: self
                    .table
                    .find_clsr_type(self.table.find_clsr(name).arity()),
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
    }

    /// One dispatch table per closure arity, typed `(ref null $clsr/N)` and filled by one active element segment in the module's ordered closure walk, so [`ClsrData::index`](super::ClsrData::index) is reproducible. The element type is what deletes the `call_indirect` signature check: the call site expects exactly the table's element type, so the engine proves the match statically instead of comparing type indices per dispatch. Slot 0 stays null — a shell's zeroed index field must trap, and a null entry does exactly that under `call_indirect`. A table exists for every arity the module dispatches at, because an indirect call site can survive the inlining-away of every closure definition of its arity.
    fn emit_clsr_tables(&mut self, module: &'a EmissionModule) {
        for (arity, type_name) in self.table.clsr_types() {
            let exprs: Vec<curios_wasm::Expr> = module
                .clsrs()
                .iter()
                .filter(|(_, clsr)| clsr.params.len() == arity)
                .map(|(name, _)| {
                    let mut expr: curios_wasm::Expr = Default::default();
                    expr.push(curios_wasm::Instr::RefFunc {
                        func_name: self.table.find_clsr(name).func_name(),
                    });
                    expr
                })
                .collect();

            let slots = exprs.len() as u64 + 1;
            self.module.add_table(
                self.table.clsr_table(arity),
                curios_wasm::Table {
                    table_type: curios_wasm::TableType {
                        address_type: curios_wasm::AddressType::I32,
                        ref_type: curios_wasm::RefType {
                            is_nullable: true,
                            heap_type: curios_wasm::HeapType::Concrete(type_name.clone()),
                        },
                        limits: curios_wasm::Limits {
                            min: slots,
                            max: Some(slots),
                        },
                    },
                    expr: None,
                },
            );

            if exprs.is_empty() {
                continue;
            }

            let mut offset: curios_wasm::Expr = Default::default();
            offset.push(curios_wasm::Instr::I32Const { value: 1 });

            self.module.add_elem(
                curios_wasm::ElemName::from(format!("clsr/{}", arity)),
                curios_wasm::ElemSegment {
                    mode: curios_wasm::ElemMode::Active {
                        table_name: self.table.clsr_table(arity),
                        offset,
                    },
                    list: curios_wasm::ElemList::Exprs(
                        curios_wasm::RefType {
                            is_nullable: false,
                            heap_type: curios_wasm::HeapType::Concrete(type_name),
                        },
                        exprs,
                    ),
                },
            );
        }
    }

    fn emit_let_func(&mut self, name: &'a EmissionFunctionName, func: &'a EmissionFunction) {
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
            curios_wasm::Func {
                type_name: self.table.find_func_type((func.params.len(), func.results)),
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

    /// Add the rope helpers the emitted code referenced. Helpers whose bodies call other helpers go first: *building* a body references its callees through the table, so the callee used-flags must settle before they are read — deep host-boundary forms, then everything else whose body calls `force` (`norm`, `box`, `eql`, `map`, `slice`, `read`), then `force`/`embed`.
    fn emit_rope_funcs(&mut self) {
        let mut ropes = RopeEmitter::new(self.table, self.module);

        if self.table.list_bytes_force_used() {
            ropes.emit_list_bytes_force_func(self.table.list_bytes_force_func());
        }

        if self.table.list_bytes_embed_used() {
            ropes.emit_list_bytes_embed_func(self.table.list_bytes_embed_func());
        }

        if self.table.bytes_norm_used() {
            ropes.emit_norm_func(
                Grain::X,
                self.table.bytes_norm_func(),
                self.table.bytes_force_func(),
            );
        }

        if self.table.bits_norm_used() {
            ropes.emit_norm_func(
                Grain::B,
                self.table.bits_norm_func(),
                self.table.bits_force_func(),
            );
        }

        if self.table.bytes_box_used() {
            ropes.emit_box_func(Grain::X, self.table.bytes_box_func());
        }

        if self.table.flt_rem_used() {
            ropes.emit_flt_rem_func(self.table.flt_rem_func());
        }

        if self.table.bits_box_used() {
            ropes.emit_box_func(Grain::B, self.table.bits_box_func());
        }

        if self.table.bytes_eql_used() {
            ropes.emit_eql_func(
                &self.table.bin_rope(),
                self.table.bytes_eql_func(),
                self.table.bytes_force_func(),
            );
        }

        if self.table.bits_eql_used() {
            ropes.emit_bits_eql_func(self.table.bits_eql_func(), self.table.bits_read_func());
        }

        if self.table.list_map_used() {
            ropes.emit_map_func(self.table.list_map_func(), self.table.list_force_func());
        }

        if self.table.bytes_slice_used() {
            ropes.emit_slice_func(
                &self.table.bin_rope(),
                self.table.bytes_slice_func(),
                self.table.bytes_force_func(),
            );
        }

        if self.table.bits_slice_used() {
            ropes.emit_slice_func(
                &self.table.bin_rope(),
                self.table.bits_slice_func(),
                self.table.bits_force_func(),
            );
        }

        if self.table.list_slice_used() {
            ropes.emit_slice_func(
                &self.table.list_rope(),
                self.table.list_slice_func(),
                self.table.list_force_func(),
            );
        }

        if self.table.bytes_read_used() {
            ropes.emit_read_func(
                &self.table.bin_rope(),
                self.table.bytes_read_func(),
                self.table.bytes_force_func(),
            );
        }

        if self.table.bits_read_used() {
            ropes.emit_bits_read_func(self.table.bits_read_func(), self.table.bits_force_func());
        }

        if self.table.list_read_used() {
            ropes.emit_read_func(
                &self.table.list_rope(),
                self.table.list_read_func(),
                self.table.list_force_func(),
            );
        }

        if self.table.bytes_force_used() {
            ropes.emit_force_func(&self.table.bin_rope(), self.table.bytes_force_func());
        }

        if self.table.bits_force_used() {
            ropes.emit_bits_force_func(self.table.bits_force_func());
        }

        if self.table.list_force_used() {
            ropes.emit_force_func(&self.table.list_rope(), self.table.list_force_func());
        }

        if self.table.bytes_embed_used() {
            ropes.emit_embed_func(&self.table.bin_rope(), self.table.bytes_embed_func());
        }

        if self.table.list_embed_used() {
            ropes.emit_embed_func(&self.table.list_rope(), self.table.list_embed_func());
        }
    }

    pub(crate) fn emit_module(&mut self, module: &'a EmissionModule) {
        self.emit_flt_type();
        self.emit_bin_rope_types();
        self.emit_list_rope_types();
        self.emit_cell_type();
        self.emit_tuple_types();
        self.emit_clsr_arity_types();
        self.emit_envr_arity_types();
        self.emit_row_types();
        self.emit_clsr_types(module);
        self.emit_func_types();

        for (name, value) in module.consts() {
            self.emit_let_data(name, value);
        }

        for (name, clsr) in module.clsrs() {
            self.emit_let_clsr(name, clsr);
        }

        self.emit_clsr_tables(module);

        for (name, func) in module.funcs() {
            self.emit_let_func(name, func);
        }

        // The entrypoint is the module's sole export — the value the host invokes. Every other function, closure, and const is reached only internally.
        if let Some(name) = module.entry() {
            let func_name = self.table.find_func(name).func_name();
            self.module
                .add_export(func_name.as_string(), curios_wasm::Export::Func(func_name));
        }

        self.emit_rope_funcs();
        self.emit_sys_imports();

        let start_type_name = curios_wasm::TypeName::from("start");

        self.module.add_type(
            start_type_name.clone(),
            curios_wasm::SubType {
                is_final: true,
                super_types: vec![],
                comp_type: curios_wasm::CompType::Func(curios_wasm::FuncType {
                    inputs: curios_wasm::ResultType::from([]),
                    outputs: curios_wasm::ResultType::from([]),
                }),
            },
        );

        let start_func_name = curios_wasm::FuncName::from("start");

        self.module.add_func(
            start_func_name.clone(),
            curios_wasm::Func {
                type_name: start_type_name,
                params: vec![],
                locals: vec![],
                expr: self.start_expr.clone(),
            },
        );

        self.module.set_start(start_func_name);
    }
}
