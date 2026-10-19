//! Shapes past the engine's envelope, spilled into trailing structs once the module is emitted: a struct type past the engine's field limit, and a function type past its parameter or result limit, with every instruction that builds, reads or calls through one.
//!
//! **A struct keeps its first fields and nests the rest.** A struct type past the limit keeps one field fewer than the limit, then a last, `spill`, holding a struct of the fields past them — spilled again where it is itself past the limit — final, and declared in the recursion group of the type it spills, whose members it may name. A struct is built by `struct.new` from its fields in order on the stack, so the fields a tail holds are the last pushed: the tail is built first, from the top of the stack, and no value moves. A field past the kept ones is read through the `spill` field of each struct before it. The kept fields are the type's own, in their places, so a supertype that declares a prefix of them is still one.
//!
//! **A signature keeps its first parameters and results and passes the rest as one struct.** A function type past either limit keeps one fewer than it and takes, or hands back, a last reference to a struct of the rest. A caller builds the parameters' struct from the arguments on top of the stack, a function's entry spreads it into the locals its parameters were, a function builds its results' struct at each return and at its end, and a caller spreads that back onto the stack after the call through one local. Each tail is a final struct in a recursion group of its own, declared before the signature: two signatures that spill one list of results spill it into one type, as the engine canonicalizes types, so a tail call between them hands the struct through.
//!
//! **What the emitter never does with a wide shape is refused.** A wide struct is built whole and never written after (`struct.new`, never `struct.new_default` or `struct.set`), no block takes a function type, and no import or export is past the limits: a spilled type met any of those ways is the emitter's broken contract rather than a program's fault, and panics.

#[cfg(test)]
mod tests;

use {curios_utilities::recurse, std::collections::HashMap};

/// The engine's limits a module stays inside: the most fields a struct type declares, and the most parameters and results a function type does.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Envelope {
    pub(crate) fields: usize,
    pub(crate) params: usize,
    pub(crate) results: usize,
}

impl Envelope {
    /// Wasmtime's, as its validator states them: `wasmparser`'s `MAX_WASM_STRUCT_FIELDS`, `MAX_WASM_FUNCTION_PARAMS` and `MAX_WASM_FUNCTION_RETURNS`.
    pub(crate) const ENGINE: Self = Self {
        fields: 10_000,
        params: 1_000,
        results: 1_000,
    };
}

/// The field a spilled struct holds its tail in.
const SPILL: &str = "spill";

/// Spill every shape of `module` past `envelope`. See the module documentation.
pub(crate) fn spill_wide_shapes(module: &mut curios_wasm::Module, envelope: Envelope) {
    curios_profile::profile!("spill_wide_shapes");
    let signatures = spill_signatures(module.types_mut(), envelope);
    let wides = spill_structs(module.types_mut(), envelope);
    if signatures.is_empty() && wides.is_empty() {
        return;
    }

    let mut callees = HashMap::new();
    for (_, _, import) in module.imports() {
        if let curios_wasm::Import::Func {
            func_name,
            type_name,
        } = import
        {
            assert!(
                !signatures.contains_key(type_name),
                "a host function `{func_name}` past the engine's limits"
            );
            callees.insert(func_name.clone(), type_name.clone());
        }
    }
    for (func_name, func) in module.funcs_mut() {
        callees.insert(func_name.clone(), func.type_name.clone());
    }
    for (_, export) in module.exports() {
        if let curios_wasm::Export::Func(func_name) = export {
            assert!(
                callees
                    .get(func_name)
                    .is_none_or(|type_name| !signatures.contains_key(type_name)),
                "an exported function `{func_name}` past the engine's limits"
            );
        }
    }

    let rewrite = Rewrite {
        signatures: &signatures,
        wides: &wides,
        callees: &callees,
    };
    for (_, func) in module.funcs_mut() {
        rewrite.func(func);
    }
    for global in module.globals_mut() {
        let instrs = std::mem::take(&mut global.expr.instrs);
        let mut scratch = Scratch::none();
        global.expr.instrs = rewrite.instrs(instrs, &mut scratch);
    }
}

/// A list of parameters or results past its limit: how many are kept, and the struct the rest are passed in.
#[derive(Debug)]
struct Tail {
    kept: usize,
    type_name: curios_wasm::TypeName,
    overflow: Vec<curios_wasm::ValType>,
}

/// What a spilled function type passes in a tail.
#[derive(Debug, Default)]
struct Signature {
    params: Option<Tail>,
    results: Option<Tail>,
}

/// The structs a spilled struct type's fields are held in, itself first, and the one each of its fields is in.
#[derive(Debug)]
struct Wide {
    segments: Vec<curios_wasm::TypeName>,
    segment_of: HashMap<curios_wasm::FieldName, usize>,
}

/// Spill each function type of `types` past `envelope`, declaring each tail in a recursion group of its own just before the type's.
fn spill_signatures(
    types: &mut Vec<curios_wasm::RecType>,
    envelope: Envelope,
) -> HashMap<curios_wasm::TypeName, Signature> {
    let mut signatures = HashMap::new();
    let mut index = 0;
    while index < types.len() {
        let mut tails = Vec::new();
        for (type_name, sub_type) in &mut types[index].sub_types {
            let curios_wasm::CompType::Func(func_type) = &mut sub_type.comp_type else {
                continue;
            };
            let signature = Signature {
                params: spill_list(
                    &mut func_type.inputs.val_types,
                    envelope.params,
                    curios_wasm::TypeName::from(format!("{type_name}/params")),
                    &mut tails,
                ),
                results: spill_list(
                    &mut func_type.outputs.val_types,
                    envelope.results,
                    curios_wasm::TypeName::from(format!("{type_name}/results")),
                    &mut tails,
                ),
            };
            if signature.params.is_some() || signature.results.is_some() {
                signatures.insert(type_name.clone(), signature);
            }
        }
        let declared = tails.len();
        types.splice(index..index, tails);
        index += declared + 1;
    }
    signatures
}

/// Keep `list` to `limit`, its last entry a reference to a struct of what it held past one fewer: the struct's declaration goes to `tails`.
fn spill_list(
    list: &mut Vec<curios_wasm::ValType>,
    limit: usize,
    type_name: curios_wasm::TypeName,
    tails: &mut Vec<curios_wasm::RecType>,
) -> Option<Tail> {
    if list.len() <= limit {
        return None;
    }
    let kept = limit - 1;
    let overflow = list.split_off(kept);
    tails.push(curios_wasm::RecType::from([(
        type_name.clone(),
        final_struct(
            overflow
                .iter()
                .enumerate()
                .map(|(index, val_type)| (field(index), val_type.clone())),
        ),
    )]));
    list.push(reference(&type_name));
    Some(Tail {
        kept,
        type_name,
        overflow,
    })
}

/// Spill each struct type of `types` past `envelope`, its tails declared in its own recursion group.
fn spill_structs(
    types: &mut [curios_wasm::RecType],
    envelope: Envelope,
) -> HashMap<curios_wasm::TypeName, Wide> {
    let mut wides = HashMap::new();
    for group in types {
        let mut tails = Vec::new();
        for (type_name, sub_type) in &mut group.sub_types {
            let curios_wasm::CompType::Struct(struct_type) = &mut sub_type.comp_type else {
                continue;
            };
            if struct_type.fields.len() <= envelope.fields {
                continue;
            }
            assert!(
                struct_type
                    .fields
                    .iter()
                    .all(|(field_name, _)| field_name.as_str() != SPILL),
                "a wide struct `{type_name}` declares a field named `{SPILL}`"
            );

            // Each struct but the last holds one field fewer than the limit and the next; the last holds the rest.
            let mut rest = std::mem::take(&mut struct_type.fields);
            let mut chunks = Vec::new();
            while rest.len() > envelope.fields {
                let tail = rest.split_off(envelope.fields - 1);
                chunks.push(rest);
                rest = tail;
            }
            chunks.push(rest);

            let segments = (0..chunks.len())
                .map(|segment| match segment {
                    0 => type_name.clone(),
                    _ => curios_wasm::TypeName::from(format!("{type_name}/spill/{segment}")),
                })
                .collect::<Vec<_>>();
            let segment_of = chunks
                .iter()
                .enumerate()
                .flat_map(|(segment, chunk)| {
                    chunk
                        .iter()
                        .map(move |(field_name, _)| (field_name.clone(), segment))
                })
                .collect();
            for (segment, mut chunk) in chunks.into_iter().enumerate() {
                if let Some(next) = segments.get(segment + 1) {
                    chunk.push((
                        curios_wasm::FieldName::from(SPILL),
                        curios_wasm::FieldType {
                            storage_type: curios_wasm::StorageType::Val(reference(next)),
                            mutability: curios_wasm::Mutability::Const,
                        },
                    ));
                }
                match segment {
                    0 => struct_type.fields = chunk,
                    _ => tails.push((
                        segments[segment].clone(),
                        curios_wasm::SubType {
                            is_final: true,
                            super_types: vec![],
                            comp_type: curios_wasm::CompType::Struct(
                                curios_wasm::StructType::from(chunk),
                            ),
                        },
                    )),
                }
            }
            wides.insert(
                type_name.clone(),
                Wide {
                    segments,
                    segment_of,
                },
            );
        }
        group.sub_types.extend(tails);
    }
    wides
}

/// The instructions of a module rewritten for its spilled shapes.
struct Rewrite<'a> {
    signatures: &'a HashMap<curios_wasm::TypeName, Signature>,
    wides: &'a HashMap<curios_wasm::TypeName, Wide>,
    /// The function type of each function the module defines or imports, which a call by name passes its arguments by.
    callees: &'a HashMap<curios_wasm::FuncName, curios_wasm::TypeName>,
}

/// The locals a rewrite of one function stashes a value in, one for each value type, and the function's results' tail, which each return builds.
struct Scratch<'a> {
    locals: Option<&'a mut Vec<(curios_wasm::LocalName, curios_wasm::ValType)>>,
    stashes: Vec<(curios_wasm::ValType, curios_wasm::LocalName)>,
    results: Option<curios_wasm::TypeName>,
}

impl<'a> Scratch<'a> {
    /// No locals: a constant expression's, which holds no call and no return.
    fn none() -> Self {
        Self {
            locals: None,
            stashes: Vec::new(),
            results: None,
        }
    }

    /// The local a value of `val_type` is stashed in.
    fn stash(&mut self, val_type: curios_wasm::ValType) -> curios_wasm::LocalName {
        if let Some((_, local)) = self.stashes.iter().find(|(known, _)| *known == val_type) {
            return local.clone();
        }
        let locals = self
            .locals
            .as_deref_mut()
            .expect("a constant expression calls nothing");
        let local = curios_wasm::LocalName::from(format!("spill/{}", self.stashes.len()));
        locals.push((local.clone(), val_type.clone()));
        self.stashes.push((val_type, local.clone()));
        local
    }
}

impl Rewrite<'_> {
    fn func(&self, func: &mut curios_wasm::Func) {
        let signature = self.signatures.get(&func.type_name);
        let mut prologue = Vec::new();
        if let Some(Signature {
            params: Some(tail), ..
        }) = signature
        {
            let spilled = curios_wasm::LocalName::from("spill/params");
            let overflow = func.params.split_off(tail.kept);
            func.params.push(spilled.clone());
            for (index, (local_name, val_type)) in
                overflow.into_iter().zip(&tail.overflow).enumerate()
            {
                func.locals.push((local_name.clone(), val_type.clone()));
                prologue.extend([
                    curios_wasm::Instr::LocalGet {
                        local_name: spilled.clone(),
                    },
                    curios_wasm::Instr::StructGet {
                        type_name: tail.type_name.clone(),
                        field_name: field(index),
                    },
                    curios_wasm::Instr::LocalSet { local_name },
                ]);
            }
        }
        let results = signature
            .and_then(|signature| signature.results.as_ref())
            .map(|tail| tail.type_name.clone());

        let mut instrs = prologue;
        instrs.append(&mut func.expr.instrs);
        let mut scratch = Scratch {
            locals: Some(&mut func.locals),
            stashes: Vec::new(),
            results: results.clone(),
        };
        let mut rewritten = self.instrs(instrs, &mut scratch);
        if let Some(results) = results {
            rewritten.extend(self.expanded(curios_wasm::Instr::StructNew { type_name: results }));
        }
        func.expr.instrs = rewritten;
    }

    /// `instrs` and every block they nest, rewritten.
    fn instrs(
        &self,
        instrs: Vec<curios_wasm::Instr>,
        scratch: &mut Scratch,
    ) -> Vec<curios_wasm::Instr> {
        let mut rewritten = Vec::with_capacity(instrs.len());
        for instr in instrs {
            match instr {
                curios_wasm::Instr::Block {
                    label_name,
                    block_type,
                    instructions,
                } => {
                    self.refuse_block_type(&block_type);
                    rewritten.push(curios_wasm::Instr::Block {
                        label_name,
                        block_type,
                        instructions: recurse(|| self.instrs(instructions, scratch)),
                    });
                }
                curios_wasm::Instr::Loop {
                    label_name,
                    block_type,
                    instructions,
                } => {
                    self.refuse_block_type(&block_type);
                    rewritten.push(curios_wasm::Instr::Loop {
                        label_name,
                        block_type,
                        instructions: recurse(|| self.instrs(instructions, scratch)),
                    });
                }
                curios_wasm::Instr::If {
                    label_name,
                    block_type,
                    then_instructions,
                    else_instructions,
                } => {
                    self.refuse_block_type(&block_type);
                    rewritten.push(curios_wasm::Instr::If {
                        label_name,
                        block_type,
                        then_instructions: recurse(|| self.instrs(then_instructions, scratch)),
                        else_instructions: recurse(|| self.instrs(else_instructions, scratch)),
                    });
                }
                instr => {
                    for called in self.called(instr, scratch) {
                        rewritten.extend(self.expanded(called));
                    }
                }
            }
        }
        rewritten
    }

    fn refuse_block_type(&self, block_type: &curios_wasm::BlockType) {
        if let curios_wasm::BlockType::Concrete(type_name) = block_type {
            assert!(
                !self.signatures.contains_key(type_name),
                "a block typed `{type_name}`, past the engine's limits"
            );
        }
    }

    /// `instr` with what a call or a return through a spilled signature adds around it.
    fn called(&self, instr: curios_wasm::Instr, scratch: &mut Scratch) -> Vec<curios_wasm::Instr> {
        let callee = match &instr {
            curios_wasm::Instr::Call { func_name }
            | curios_wasm::Instr::ReturnCall { func_name } => self.callees.get(func_name),
            curios_wasm::Instr::CallRef { type_name }
            | curios_wasm::Instr::ReturnCallRef { type_name }
            | curios_wasm::Instr::CallIndirect { type_name, .. }
            | curios_wasm::Instr::ReturnCallIndirect { type_name, .. } => Some(type_name),
            curios_wasm::Instr::Return => {
                return match &scratch.results {
                    Some(results) => vec![
                        curios_wasm::Instr::StructNew {
                            type_name: results.clone(),
                        },
                        instr,
                    ],
                    None => vec![instr],
                };
            }
            _ => None,
        };
        let Some(signature) = callee.and_then(|type_name| self.signatures.get(type_name)) else {
            return vec![instr];
        };

        let mut instrs = Vec::new();
        if let Some(params) = &signature.params {
            // What a call takes past its arguments — the function reference or the table index — is on top of them, and goes aside while they are packed.
            let aside = match &instr {
                curios_wasm::Instr::CallRef { type_name }
                | curios_wasm::Instr::ReturnCallRef { type_name } => {
                    Some(curios_wasm::ValType::Ref(curios_wasm::RefType {
                        is_nullable: true,
                        heap_type: curios_wasm::HeapType::Concrete(type_name.clone()),
                    }))
                }
                // The closure tables the emitter dispatches through are addressed by `i32`.
                curios_wasm::Instr::CallIndirect { .. }
                | curios_wasm::Instr::ReturnCallIndirect { .. } => {
                    Some(curios_wasm::ValType::Num(curios_wasm::NumType::I32))
                }
                _ => None,
            }
            .map(|val_type| scratch.stash(val_type));
            if let Some(local_name) = &aside {
                instrs.push(curios_wasm::Instr::LocalSet {
                    local_name: local_name.clone(),
                });
            }
            instrs.push(curios_wasm::Instr::StructNew {
                type_name: params.type_name.clone(),
            });
            if let Some(local_name) = aside {
                instrs.push(curios_wasm::Instr::LocalGet { local_name });
            }
        }
        let returns = matches!(
            instr,
            curios_wasm::Instr::ReturnCall { .. }
                | curios_wasm::Instr::ReturnCallRef { .. }
                | curios_wasm::Instr::ReturnCallIndirect { .. }
        );
        instrs.push(instr);
        // A tail call hands its callee's results through as they are; any other call spreads them back onto the stack.
        if let (Some(results), false) = (&signature.results, returns) {
            let local_name = scratch.stash(reference(&results.type_name));
            instrs.push(curios_wasm::Instr::LocalSet {
                local_name: local_name.clone(),
            });
            for index in 0..results.overflow.len() {
                instrs.extend([
                    curios_wasm::Instr::LocalGet {
                        local_name: local_name.clone(),
                    },
                    curios_wasm::Instr::StructGet {
                        type_name: results.type_name.clone(),
                        field_name: field(index),
                    },
                ]);
            }
        }
        instrs
    }

    /// `instr` built or read through a spilled struct's segments.
    fn expanded(&self, instr: curios_wasm::Instr) -> Vec<curios_wasm::Instr> {
        match instr {
            curios_wasm::Instr::StructNew { type_name } => match self.wides.get(&type_name) {
                Some(wide) => wide
                    .segments
                    .iter()
                    .rev()
                    .map(|segment| curios_wasm::Instr::StructNew {
                        type_name: segment.clone(),
                    })
                    .collect(),
                None => vec![curios_wasm::Instr::StructNew { type_name }],
            },
            curios_wasm::Instr::StructGet {
                type_name,
                field_name,
            } => self.read(type_name, field_name, |type_name, field_name| {
                curios_wasm::Instr::StructGet {
                    type_name,
                    field_name,
                }
            }),
            curios_wasm::Instr::StructGetS {
                type_name,
                field_name,
            } => self.read(type_name, field_name, |type_name, field_name| {
                curios_wasm::Instr::StructGetS {
                    type_name,
                    field_name,
                }
            }),
            curios_wasm::Instr::StructGetU {
                type_name,
                field_name,
            } => self.read(type_name, field_name, |type_name, field_name| {
                curios_wasm::Instr::StructGetU {
                    type_name,
                    field_name,
                }
            }),
            curios_wasm::Instr::StructNewDefault { type_name }
            | curios_wasm::Instr::StructSet { type_name, .. }
                if self.wides.contains_key(&type_name) =>
            {
                panic!("a wide struct `{type_name}` defaulted or written after it was built")
            }
            instr => vec![instr],
        }
    }

    /// A read of `field_name` of a `type_name`, through the segments before the one holding it where the type is spilled.
    fn read(
        &self,
        type_name: curios_wasm::TypeName,
        field_name: curios_wasm::FieldName,
        get: impl Fn(curios_wasm::TypeName, curios_wasm::FieldName) -> curios_wasm::Instr,
    ) -> Vec<curios_wasm::Instr> {
        let Some(wide) = self.wides.get(&type_name) else {
            return vec![get(type_name, field_name)];
        };
        let segment = wide.segment_of[&field_name];
        wide.segments[..segment]
            .iter()
            .map(|segment| curios_wasm::Instr::StructGet {
                type_name: segment.clone(),
                field_name: curios_wasm::FieldName::from(SPILL),
            })
            .chain([get(wide.segments[segment].clone(), field_name)])
            .collect()
    }
}

/// The name a tail gives its `index`th field.
fn field(index: usize) -> curios_wasm::FieldName {
    curios_wasm::FieldName::from(index.to_string())
}

/// A non-null reference to `type_name`: a tail is always built.
fn reference(type_name: &curios_wasm::TypeName) -> curios_wasm::ValType {
    curios_wasm::ValType::Ref(curios_wasm::RefType {
        is_nullable: false,
        heap_type: curios_wasm::HeapType::Concrete(type_name.clone()),
    })
}

/// A final struct of `fields`, each immutable.
fn final_struct(
    fields: impl Iterator<Item = (curios_wasm::FieldName, curios_wasm::ValType)>,
) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![],
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from(fields.map(
            |(field_name, val_type)| {
                (
                    field_name,
                    curios_wasm::FieldType {
                        storage_type: curios_wasm::StorageType::Val(val_type),
                        mutability: curios_wasm::Mutability::Const,
                    },
                )
            },
        ))),
    }
}
