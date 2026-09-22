use {
    super::{
        BigHelper, EmissionBlockName, EmissionBody, EmissionClosure, EmissionClosureName,
        EmissionCode, EmissionData, EmissionFunction, EmissionFunctionName, EmissionModule,
        EmissionValue, EmissionValueName, LoadAs, call, concrete_val, refuse_func_name,
    },
    curios_abi::{ForeignFunction, WireLeaf, WireType},
    std::{
        cell::{OnceCell, RefCell},
        collections::{BTreeMap, HashMap, HashSet},
        sync::Arc,
    },
};

#[derive(Debug, Clone)]
pub(crate) struct FieldData {
    type_name: curios_wasm::TypeName,
    field_name: curios_wasm::FieldName,
}

impl FieldData {
    pub(crate) fn new(
        type_name: curios_wasm::TypeName,
        field_name: curios_wasm::FieldName,
    ) -> Self {
        Self {
            type_name,
            field_name,
        }
    }

    pub(crate) fn type_name(&self) -> curios_wasm::TypeName {
        self.type_name.clone()
    }

    pub(crate) fn field_name(&self) -> curios_wasm::FieldName {
        self.field_name.clone()
    }
}

/// The name bundle for one internal rope carrier (`$rope/bin` or `$rope/list`): the base struct the emitter casts carrier refs to, its `leaf`/`node`/`view` subtypes, the flat payload array, and every field name — one handle to thread through the op emitters so packed `Bits`/`Bytes` and `List` share their lowering code.
#[derive(Debug, Clone)]
pub(crate) struct RopeData {
    pub base: curios_wasm::TypeName,
    pub leaf: curios_wasm::TypeName,
    pub node: curios_wasm::TypeName,
    pub view: curios_wasm::TypeName,
    pub payload: curios_wasm::TypeName,
    pub tag_field: curios_wasm::FieldName,
    pub len_field: curios_wasm::FieldName,
    pub payload_field: curios_wasm::FieldName,
    pub left_field: curios_wasm::FieldName,
    pub right_field: curios_wasm::FieldName,
    pub cache_field: curios_wasm::FieldName,
    pub base_field: curios_wasm::FieldName,
    pub offset_field: curios_wasm::FieldName,
}

/// The name bundle for the boxed form a `Nat` or `Int` outside the i31 takes: the `$big` struct, the `$words` array holding its magnitude, and the struct's two fields — one handle for the big-number helpers and the literal materialization to share.
#[derive(Debug, Clone)]
pub(crate) struct BigData {
    pub big: curios_wasm::TypeName,
    pub words: curios_wasm::TypeName,
    pub sign_field: curios_wasm::FieldName,
    pub limbs_field: curios_wasm::FieldName,
}

#[derive(Debug, Clone)]
pub(crate) struct ClsrData<'a> {
    /// This closure's slot in its arity's dispatch table — the value its environment's special field holds. 1-based: slot 0 stays null, so a zeroed code field dispatches into the null entry and traps rather than into a body.
    index: i32,
    func_name: curios_wasm::FuncName,
    envr_type: curios_wasm::TypeName,
    fields: Vec<(&'a EmissionValueName, curios_wasm::FieldName)>,
    params: HashMap<&'a EmissionValueName, curios_wasm::LocalName>,
    resume: &'a EmissionBlockName,
}

impl<'a> ClsrData<'a> {
    pub(crate) fn new(
        clsr_name: &'a EmissionClosureName,
        clsr: &'a EmissionClosure,
        index: i32,
    ) -> Self {
        Self {
            index,
            func_name: curios_wasm::FuncName::from(format!("clsr/{}", clsr_name)),
            envr_type: curios_wasm::TypeName::from(format!("envr/{}", clsr_name)),
            fields: clsr
                .fields
                .iter()
                .map(|field| {
                    (
                        &field.name,
                        curios_wasm::FieldName::from(format!("${}", field.name)),
                    )
                })
                .collect(),
            params: clsr
                .params
                .iter()
                .map(|param| {
                    (
                        &param.name,
                        curios_wasm::LocalName::from(format!("${}", param.name)),
                    )
                })
                .collect(),
            resume: &clsr.resume,
        }
    }

    pub(crate) fn index(&self) -> i32 {
        self.index
    }

    pub(crate) fn func_name(&self) -> curios_wasm::FuncName {
        self.func_name.clone()
    }

    pub(crate) fn envr_type(&self) -> curios_wasm::TypeName {
        self.envr_type.clone()
    }

    pub(crate) fn fields(&self) -> impl Iterator<Item = curios_wasm::FieldName> {
        self.fields.iter().map(|(_, field_name)| field_name.clone())
    }

    pub(crate) fn find_field(&self, value_name: &EmissionValueName) -> Option<FieldData> {
        self.fields
            .iter()
            .find_map(|(field_name, mapped_field_name)| {
                (value_name == *field_name).then_some(mapped_field_name)
            })
            .cloned()
            .map(|field_name| FieldData::new(self.envr_type(), field_name))
    }

    pub(crate) fn params(&self) -> HashMap<&'a EmissionValueName, curios_wasm::LocalName> {
        self.params.clone()
    }

    pub(crate) fn find_param(
        &self,
        value_name: &EmissionValueName,
    ) -> Option<curios_wasm::LocalName> {
        self.params.get(value_name).cloned()
    }

    pub(crate) fn arity(&self) -> usize {
        self.params.len()
    }

    pub(crate) fn is_resume(&self, block_name: &EmissionBlockName) -> bool {
        self.resume == block_name
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FuncData<'a> {
    func_name: curios_wasm::FuncName,
    params: HashMap<&'a EmissionValueName, curios_wasm::LocalName>,
    resume: &'a EmissionBlockName,
}

impl<'a> FuncData<'a> {
    pub(crate) fn new(func_name: &'a EmissionFunctionName, func: &'a EmissionFunction) -> Self {
        Self {
            // The `func/` prefix is why the exported entrypoint is `func/main`: the entry is always `main`, and the export reuses the function's emitted name.
            func_name: curios_wasm::FuncName::from(format!("func/{}", func_name)),
            params: func
                .params
                .iter()
                .map(|param| {
                    (
                        &param.name,
                        curios_wasm::LocalName::from(format!("${}", param.name)),
                    )
                })
                .collect(),
            resume: &func.resume,
        }
    }

    pub(crate) fn func_name(&self) -> curios_wasm::FuncName {
        self.func_name.clone()
    }

    pub(crate) fn arity(&self) -> usize {
        self.params.len()
    }

    pub(crate) fn params(&self) -> HashMap<&'a EmissionValueName, curios_wasm::LocalName> {
        self.params.clone()
    }

    pub(crate) fn find_param(
        &self,
        value_name: &EmissionValueName,
    ) -> Option<curios_wasm::LocalName> {
        self.params.get(value_name).cloned()
    }

    pub(crate) fn is_resume(&self, block_name: &EmissionBlockName) -> bool {
        self.resume == block_name
    }
}

fn max_tuple_arity(data: &EmissionData) -> usize {
    match data {
        EmissionData::Tuple(fields) => fields.len(),
        _ => 0,
    }
}

fn max_value_tuple_arity(value: &EmissionValue) -> usize {
    match value {
        EmissionValue::Pure(data) => max_tuple_arity(data),
        // Projecting field `index` reads through a tuple type of arity at least `index + 1`, even when no tuple of that arity is ever *built* in the module (e.g. the projected tuple only ever arrives from outside, or the producing array is empty). Sizing the tuple types from constructions alone misses it.
        EmissionValue::Eval(EmissionCode::Intrinsic(
            curios_cont::Intrinsic::TupleGet(index),
            _,
        )) => index + 1,
        _ => 0,
    }
}

fn max_region_tuple_arity(region: &EmissionBody) -> usize {
    let values = region
        .values
        .iter()
        .map(|(_, value)| max_value_tuple_arity(value));

    let blocks = region
        .blocks
        .iter()
        .map(|(_, block)| max_region_tuple_arity(&block.region));

    values.chain(blocks).max().unwrap_or(0)
}

#[derive(Debug)]
pub(crate) struct Table<'a> {
    special_field: curios_wasm::FieldName,
    special_local: curios_wasm::LocalName,
    special_label: curios_wasm::LabelName,
    clsr_tables: BTreeMap<usize, curios_wasm::TableName>,
    flt_type: curios_wasm::TypeName,
    big_type: curios_wasm::TypeName,
    words_type: curios_wasm::TypeName,
    longs_type: curios_wasm::TypeName,
    bin_rope_type: curios_wasm::TypeName,
    list_rope_type: curios_wasm::TypeName,
    bytes_type: curios_wasm::TypeName,
    elems_type: curios_wasm::TypeName,
    bin_rope_leaf_type: curios_wasm::TypeName,
    bin_rope_node_type: curios_wasm::TypeName,
    bin_rope_view_type: curios_wasm::TypeName,
    list_rope_leaf_type: curios_wasm::TypeName,
    list_rope_node_type: curios_wasm::TypeName,
    list_rope_view_type: curios_wasm::TypeName,
    cell_type: curios_wasm::TypeName,
    exit: OnceCell<curios_wasm::FuncName>,
    panic: OnceCell<curios_wasm::FuncName>,
    // One slot per class, in `Panic::ALL`'s order, minted lazily like `exit`: a module declares the refusals its code can reach and no others.
    refuse: [OnceCell<curios_wasm::FuncName>; curios_cont::Panic::ALL.len()],
    // One slot per big-number helper, in `BigHelper::ALL`'s order, minted lazily like `refuse`: a module declares the helpers its code and its other helpers reach and no others.
    big: [OnceCell<curios_wasm::FuncName>; BigHelper::ALL.len()],
    // The shared rope helpers, minted lazily like `exit`: the first call site recorded during emission names the function, and the module emitter then adds exactly the recorded set after the program's own functions (see `emit_rope_funcs`).
    bytes_force: OnceCell<curios_wasm::FuncName>,
    bits_force: OnceCell<curios_wasm::FuncName>,
    list_force: OnceCell<curios_wasm::FuncName>,
    list_bytes_force: OnceCell<curios_wasm::FuncName>,
    list_bits_force: OnceCell<curios_wasm::FuncName>,
    bytes_embed: OnceCell<curios_wasm::FuncName>,
    bits_embed: OnceCell<curios_wasm::FuncName>,
    bytes_box: OnceCell<curios_wasm::FuncName>,
    bytes_norm: OnceCell<curios_wasm::FuncName>,
    flt_rem: OnceCell<curios_wasm::FuncName>,
    bits_box: OnceCell<curios_wasm::FuncName>,
    bits_norm: OnceCell<curios_wasm::FuncName>,
    list_embed: OnceCell<curios_wasm::FuncName>,
    /// The scalar leaves whose list crosses to a host, each through `$list/<leaf>/to_<payload>`.
    scalars_forces: RefCell<Vec<WireLeaf>>,
    /// The scalar leaves whose list a host answers, each through `$list/<leaf>/of_<payload>`.
    scalars_embeds: RefCell<Vec<WireLeaf>>,
    list_bytes_embed: OnceCell<curios_wasm::FuncName>,
    list_bits_embed: OnceCell<curios_wasm::FuncName>,
    bytes_slice: OnceCell<curios_wasm::FuncName>,
    bits_slice: OnceCell<curios_wasm::FuncName>,
    list_slice: OnceCell<curios_wasm::FuncName>,
    bytes_read: OnceCell<curios_wasm::FuncName>,
    bits_read: OnceCell<curios_wasm::FuncName>,
    list_read: OnceCell<curios_wasm::FuncName>,
    bytes_eql: OnceCell<curios_wasm::FuncName>,
    bits_eql: OnceCell<curios_wasm::FuncName>,
    // The pointwise trio takes forced payloads, so one of each serves both grains.
    bin_and: OnceCell<curios_wasm::FuncName>,
    bin_or: OnceCell<curios_wasm::FuncName>,
    bin_xor: OnceCell<curios_wasm::FuncName>,
    bytes_replicate: OnceCell<curios_wasm::FuncName>,
    bits_replicate: OnceCell<curios_wasm::FuncName>,
    bytes_to_bits: OnceCell<curios_wasm::FuncName>,
    bits_to_bytes: OnceCell<curios_wasm::FuncName>,
    list_map: OnceCell<curios_wasm::FuncName>,
    // The foreign functions the emitted code calls, keyed by the minted internal name (see `host_func`). Same lazy used-tracking as the `exit` cell: the first call-site reference during emission records the function's row, and `emit_sys_imports` then declares exactly the recorded set (in minted-name order — wasmtime links by name, so import order is cosmetic).
    host_funcs: RefCell<BTreeMap<String, Arc<ForeignFunction>>>,
    tuple_types: BTreeMap<usize, curios_wasm::TypeName>,
    /// One final struct type per nominal row, keyed by the row's identity rather than by an arity — which is what makes a row read an exact cast and gives Binaryen's closed-world passes distinct types to refine. Widths come from the Cont module's own row table, so a row whose constructions were all optimized away still declares its type (harmless, and a projection can outlive its constructions).
    row_types: BTreeMap<curios_cont::RowId, (curios_wasm::TypeName, Vec<curios_cont::Slot>)>,
    envr_types: BTreeMap<usize, curios_wasm::TypeName>,
    clsr_types: BTreeMap<usize, curios_wasm::TypeName>,
    /// Keyed by the pair a wasm function type actually is — parameter count *and* result count — rather than by parameter count alone, so two functions of the same arity delivering different result shapes cannot collide on one type. The closure supertypes below stay keyed by arity, because a function reached through one is invoked at the uniform shape whatever its own type says.
    func_types: BTreeMap<(usize, usize), curios_wasm::TypeName>,
    consts: HashMap<&'a EmissionValueName, curios_wasm::GlobalName>,
    /// The module consts that are `Tuple`/`List` constructions — the hoisted half of the population `Context::refuse_raw_aggregate` refuses to hand to a register. A closed aggregate is lifted out of its region by `hoist`, so a guard reading region values alone would miss exactly the constant ones.
    const_aggregates: HashSet<&'a EmissionValueName>,
    clsrs: HashMap<&'a EmissionClosureName, ClsrData<'a>>,
    funcs: HashMap<&'a EmissionFunctionName, FuncData<'a>>,
    /// The values the representation analysis decided to hold in a register, and at which carrier. A name absent here is held behind a reference, which is every synthetic name codegen mints for itself.
    raw: &'a HashMap<EmissionValueName, curios_cont::Repr>,
}

impl<'a> Table<'a> {
    pub(crate) fn new(
        module: &'a EmissionModule,
        raw: &'a HashMap<EmissionValueName, curios_cont::Repr>,
    ) -> Self {
        Self {
            raw,
            special_field: curios_wasm::FieldName::from("!"),
            special_local: curios_wasm::LocalName::from("!"),
            special_label: curios_wasm::LabelName::from("!"),
            clsr_tables: module
                .clsr_arities()
                .into_iter()
                .map(|arity| {
                    (
                        arity,
                        curios_wasm::TableName::from(format!("clsr/{}", arity)),
                    )
                })
                .collect(),
            flt_type: curios_wasm::TypeName::from("flt"),
            big_type: curios_wasm::TypeName::from("big"),
            words_type: curios_wasm::TypeName::from("words"),
            longs_type: curios_wasm::TypeName::from("longs"),
            bin_rope_type: curios_wasm::TypeName::from("rope/bin"),
            list_rope_type: curios_wasm::TypeName::from("rope/list"),
            bytes_type: curios_wasm::TypeName::from("bytes"),
            elems_type: curios_wasm::TypeName::from("elems"),
            bin_rope_leaf_type: curios_wasm::TypeName::from("rope/bin/leaf"),
            bin_rope_node_type: curios_wasm::TypeName::from("rope/bin/node"),
            bin_rope_view_type: curios_wasm::TypeName::from("rope/bin/view"),
            list_rope_leaf_type: curios_wasm::TypeName::from("rope/list/leaf"),
            list_rope_node_type: curios_wasm::TypeName::from("rope/list/node"),
            list_rope_view_type: curios_wasm::TypeName::from("rope/list/view"),
            cell_type: curios_wasm::TypeName::from("cell"),
            exit: OnceCell::new(),
            panic: OnceCell::new(),
            refuse: Default::default(),
            big: Default::default(),
            bytes_force: OnceCell::new(),
            bits_force: OnceCell::new(),
            list_force: OnceCell::new(),
            list_bytes_force: OnceCell::new(),
            list_bits_force: OnceCell::new(),
            bytes_embed: OnceCell::new(),
            bits_embed: OnceCell::new(),
            bytes_box: OnceCell::new(),
            bytes_norm: OnceCell::new(),
            flt_rem: OnceCell::new(),
            bits_box: OnceCell::new(),
            bits_norm: OnceCell::new(),
            list_embed: OnceCell::new(),
            scalars_forces: RefCell::new(Vec::new()),
            scalars_embeds: RefCell::new(Vec::new()),
            list_bytes_embed: OnceCell::new(),
            list_bits_embed: OnceCell::new(),
            bytes_slice: OnceCell::new(),
            bits_slice: OnceCell::new(),
            list_slice: OnceCell::new(),
            bytes_read: OnceCell::new(),
            bits_read: OnceCell::new(),
            list_read: OnceCell::new(),
            bytes_eql: OnceCell::new(),
            bits_eql: OnceCell::new(),
            bin_and: OnceCell::new(),
            bin_or: OnceCell::new(),
            bin_xor: OnceCell::new(),
            bytes_replicate: OnceCell::new(),
            bits_replicate: OnceCell::new(),
            bytes_to_bits: OnceCell::new(),
            bits_to_bytes: OnceCell::new(),
            list_map: OnceCell::new(),
            host_funcs: RefCell::new(BTreeMap::new()),
            row_types: module
                .rows()
                .iter()
                .map(|(row, definition)| {
                    let spelled = match &definition.debug_name {
                        Some(hint) => format!("row/{}${hint}", row.index()),
                        None => format!("row/{}", row.index()),
                    };
                    (
                        *row,
                        (
                            curios_wasm::TypeName::from(spelled),
                            definition.slots.clone(),
                        ),
                    )
                })
                .collect(),
            tuple_types: {
                let max = module
                    .consts()
                    .iter()
                    .map(|(_, data)| max_tuple_arity(data))
                    .chain(
                        module
                            .clsrs()
                            .iter()
                            .map(|(_, clsr)| max_region_tuple_arity(&clsr.region)),
                    )
                    .chain(
                        module
                            .funcs()
                            .iter()
                            .map(|(_, func)| max_region_tuple_arity(&func.region)),
                    )
                    .max()
                    .unwrap_or(0);

                (0..=max)
                    .map(|arity| {
                        (
                            arity,
                            curios_wasm::TypeName::from(format!("tuple/{}", arity)),
                        )
                    })
                    .collect()
            },
            envr_types: module
                .clsr_arities()
                .into_iter()
                .map(|arity| {
                    (
                        arity,
                        curios_wasm::TypeName::from(format!("envr/{}", arity)),
                    )
                })
                .collect(),
            clsr_types: module
                .clsr_arities()
                .into_iter()
                .map(|arity| {
                    (
                        arity,
                        curios_wasm::TypeName::from(format!("clsr/{}", arity)),
                    )
                })
                .collect(),
            func_types: module
                .funcs()
                .iter()
                .map(|(_, func)| (func.params.len(), func.results))
                .map(|shape| (shape, Table::func_type_name(shape)))
                .collect(),
            consts: module
                .consts()
                .iter()
                .map(|(const_name, _)| {
                    (
                        const_name,
                        curios_wasm::GlobalName::from(const_name.as_string()),
                    )
                })
                .collect(),
            const_aggregates: module
                .consts()
                .iter()
                .filter(|(_, data)| {
                    matches!(
                        data,
                        EmissionData::List(_) | EmissionData::Tuple(_) | EmissionData::Row(..)
                    )
                })
                .map(|(const_name, _)| const_name)
                .collect(),
            // Table indices come from the module's ordered closure walk — the same order each arity's element segment lists its bodies in — never from this map's iteration. The counter is per arity because each arity dispatches through its own typed table.
            clsrs: {
                let mut next_index = BTreeMap::<usize, i32>::new();
                module
                    .clsrs()
                    .iter()
                    .map(|(clsr_name, clsr)| {
                        let index = next_index.entry(clsr.params.len()).or_insert(0);
                        *index += 1;
                        (clsr_name, ClsrData::new(clsr_name, clsr, *index))
                    })
                    .collect()
            },
            funcs: module
                .funcs()
                .iter()
                .map(|(func_name, func)| (func_name, FuncData::new(func_name, func)))
                .collect(),
        }
    }

    pub(crate) fn special_field(&self) -> curios_wasm::FieldName {
        self.special_field.clone()
    }

    pub(crate) fn special_local(&self) -> curios_wasm::LocalName {
        self.special_local.clone()
    }

    pub(crate) fn special_label(&self) -> curios_wasm::LabelName {
        self.special_label.clone()
    }

    /// The dispatch table holding every closure body of `arity`, indexed by [`ClsrData::index`]. Typed `(ref null $clsr/{arity})` rather than `funcref`, so a `call_indirect` expecting that type is statically known to match and the engine emits no runtime signature check at all.
    pub(crate) fn clsr_table(&self, arity: usize) -> curios_wasm::TableName {
        self.clsr_tables
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks closure table for arity `{}`", arity))
            .clone()
    }

    pub(crate) fn top_type(is_nullable: bool) -> curios_wasm::ValType {
        curios_wasm::ValType::Ref(curios_wasm::RefType {
            is_nullable,
            heap_type: curios_wasm::HeapType::Abstract(curios_wasm::AbsHeapType::Any),
        })
    }

    pub(crate) fn int_type(is_nullable: bool) -> curios_wasm::RefType {
        curios_wasm::RefType {
            is_nullable,
            heap_type: curios_wasm::HeapType::Abstract(curios_wasm::AbsHeapType::I31),
        }
    }

    pub(crate) fn flt_type(&self) -> curios_wasm::TypeName {
        self.flt_type.clone()
    }

    /// The boxed form's name bundle.
    pub(crate) fn big(&self) -> BigData {
        BigData {
            big: self.big_type.clone(),
            words: self.words_type.clone(),
            sign_field: curios_wasm::FieldName::from("sign"),
            limbs_field: curios_wasm::FieldName::from("limbs"),
        }
    }

    /// The big-number helper `helper`, marked for emission by this first use — from the emitted code or from another helper's body, which is why the roster is emitted callers first.
    pub(crate) fn big_func(&self, helper: BigHelper) -> curios_wasm::FuncName {
        self.big[helper.slot()]
            .get_or_init(|| helper.func_name())
            .clone()
    }

    pub(crate) fn big_used(&self, helper: BigHelper) -> bool {
        self.big[helper.slot()].get().is_some()
    }

    pub(crate) fn list_rope_type(&self) -> curios_wasm::TypeName {
        self.list_rope_type.clone()
    }

    /// The wasm-level type a value of the given wire type crosses the host boundary as, in either direction: a `Nat` or `Int` as a raw `i64`, a `Bool` as a raw `i32`, `Flt` as a raw `f64`, references as their concrete non-nullable heap type (a handle is its `Bytes` token).
    ///
    /// **A scalar crosses as the number it is, both ways.** Going out, the call site narrows a `Nat` or `Int` through `LoadAs::WireNat`/`LoadAs::WireInt`, refusing past the wire, reads a `Bool` as its word and an `Flt` out of its box. Coming back, the guest boxes what the host answered (`Context::host_instrs`), because every box is a layout this crate defines: an `Flt`'s `$flt` struct, and a `Nat` or `Int` past the i31 the boxed magnitude. A host that minted either would be a second crate needing that layout, and one that could mint only the i31 would have to refuse a result past it.
    pub(crate) fn wire_type(&self, wire_type: &WireType) -> curios_wasm::ValType {
        match wire_type {
            WireType::Nat | WireType::Int => curios_wasm::ValType::Num(curios_wasm::NumType::I64),
            WireType::Bool => curios_wasm::ValType::Num(curios_wasm::NumType::I32),
            WireType::Flt => curios_wasm::ValType::Num(curios_wasm::NumType::F64),
            WireType::Bytes | WireType::Bits | WireType::Handle => {
                concrete_val(self.bytes_type(), false)
            }
            // A list of scalars is one flat element per value, narrowed and boxed by the guest as a scalar is; a list of references is their flat payloads.
            WireType::List(leaf @ (WireLeaf::Nat | WireLeaf::Int | WireLeaf::Bool)) => {
                concrete_val(self.scalars_type(*leaf), false)
            }
            WireType::List(_) => concrete_val(self.elems_type(), false),
        }
    }

    pub(crate) fn bytes_type(&self) -> curios_wasm::TypeName {
        self.bytes_type.clone()
    }

    /// `$longs`, the flat payload a list of `Nat` or `Int` crosses the host boundary as.
    pub(crate) fn longs_type(&self) -> curios_wasm::TypeName {
        self.longs_type.clone()
    }

    /// The flat payload a list of the scalar `leaf` crosses the host boundary as: `$longs` for a `Nat` or `Int`, whose elements cross as a lone one does, and `$words` for a `Bool`.
    pub(crate) fn scalars_type(&self, leaf: WireLeaf) -> curios_wasm::TypeName {
        match leaf {
            WireLeaf::Nat | WireLeaf::Int => self.longs_type.clone(),
            WireLeaf::Bool => self.words_type.clone(),
            WireLeaf::Bytes | WireLeaf::Bits | WireLeaf::Handle => {
                panic!("a list of `{leaf:?}` crosses as its payloads, never as scalars")
            }
        }
    }

    pub(crate) fn elems_type(&self) -> curios_wasm::TypeName {
        self.elems_type.clone()
    }

    /// The shared packed `Bits`/`Bytes` rope's name bundle.
    pub(crate) fn bin_rope(&self) -> RopeData {
        RopeData {
            base: self.bin_rope_type.clone(),
            leaf: self.bin_rope_leaf_type.clone(),
            node: self.bin_rope_node_type.clone(),
            view: self.bin_rope_view_type.clone(),
            payload: self.bytes_type.clone(),
            tag_field: curios_wasm::FieldName::from("tag"),
            len_field: curios_wasm::FieldName::from("len"),
            payload_field: curios_wasm::FieldName::from("bytes"),
            left_field: curios_wasm::FieldName::from("left"),
            right_field: curios_wasm::FieldName::from("right"),
            cache_field: curios_wasm::FieldName::from("cache"),
            base_field: curios_wasm::FieldName::from("base"),
            offset_field: curios_wasm::FieldName::from("offset"),
        }
    }

    /// The `List` rope's name bundle.
    pub(crate) fn list_rope(&self) -> RopeData {
        RopeData {
            base: self.list_rope_type.clone(),
            leaf: self.list_rope_leaf_type.clone(),
            node: self.list_rope_node_type.clone(),
            view: self.list_rope_view_type.clone(),
            payload: self.elems_type.clone(),
            tag_field: curios_wasm::FieldName::from("tag"),
            len_field: curios_wasm::FieldName::from("len"),
            payload_field: curios_wasm::FieldName::from("elems"),
            left_field: curios_wasm::FieldName::from("left"),
            right_field: curios_wasm::FieldName::from("right"),
            cache_field: curios_wasm::FieldName::from("cache"),
            base_field: curios_wasm::FieldName::from("base"),
            offset_field: curios_wasm::FieldName::from("offset"),
        }
    }

    pub(crate) fn cell_type(&self) -> curios_wasm::TypeName {
        self.cell_type.clone()
    }

    /// The internal binding name of a store-described host function. First use during emission records the function as live; [`host_funcs`](Self::host_funcs) hands the recorded set to `emit_sys_imports`.
    ///
    /// A row's identity is its `(namespace, name)` pair (see [`ForeignFunction`]), and its name is chosen outside the emitter, so the minted name embeds both components under the reserved `host/` row prefix — a foreign name can never collide with a runtime helper, another minted row, or a same-named row from another namespace. The embedding is injective because namespaces are compiler-chosen and never contain `/`.
    pub(crate) fn host_func(&self, function: &Arc<ForeignFunction>) -> curios_wasm::FuncName {
        let func_name =
            curios_wasm::FuncName::from(format!("host/{}/{}", function.namespace, function.name));

        self.host_funcs
            .borrow_mut()
            .entry(func_name.as_string())
            .or_insert_with(|| Arc::clone(function));

        func_name
    }

    /// The foreign functions the emitted code referenced, in minted-name order.
    pub(crate) fn host_funcs(&self) -> Vec<Arc<ForeignFunction>> {
        self.host_funcs.borrow().values().cloned().collect()
    }

    pub(crate) fn exit_func(&self) -> &curios_wasm::FuncName {
        self.exit
            .get_or_init(|| curios_wasm::FuncName::from("exit"))
    }

    pub(crate) fn exit_used(&self) -> bool {
        self.exit.get().is_some()
    }

    /// The `sys.panic` import: a byte string in, no return. Declared by every module, since every module refuses somewhere.
    pub(crate) fn panic_func(&self) -> curios_wasm::FuncName {
        self.panic
            .get_or_init(|| curios_wasm::FuncName::from("panic"))
            .clone()
    }

    /// The instruction sequence a refusal is: a call to the class's helper and the `unreachable` that keeps the block's type. Spelled once for the code emitter, the region context and the rope helpers alike.
    pub(crate) fn refuse_instrs(&self, panic: curios_cont::Panic) -> Vec<curios_wasm::Instr> {
        vec![
            curios_wasm::Instr::Call {
                func_name: self.refuse_func(panic),
            },
            curios_wasm::Instr::Unreachable,
        ]
    }

    /// `$refuse/<class> () -> ()`: build the class's sentence from its data segment, hand it to `sys.panic`, and never return. First use marks it for emission, so nothing is allocated for a refusal until one fires.
    fn refuse_func(&self, panic: curios_cont::Panic) -> curios_wasm::FuncName {
        let slot = curios_cont::Panic::ALL
            .iter()
            .position(|class| *class == panic)
            .expect("`Panic::ALL` holds every class");

        self.refuse[slot]
            .get_or_init(|| refuse_func_name(panic))
            .clone()
    }

    /// The refusals the emitted code referenced, in `Panic::ALL`'s order. Read after every body that can refuse has been built — the rope helpers' included.
    pub(crate) fn refuse_funcs(&self) -> Vec<(curios_cont::Panic, curios_wasm::FuncName)> {
        curios_cont::Panic::ALL
            .into_iter()
            .zip(&self.refuse)
            .filter_map(|(class, slot)| Some((class, slot.get()?.clone())))
            .collect()
    }

    /// `$bytes/force (ref $rope/bin) -> (ref $bytes)`: flatten a `Bytes` rope to its payload, memoizing in the entry node. First use marks it for emission.
    pub(crate) fn bytes_force_func(&self) -> curios_wasm::FuncName {
        self.bytes_force
            .get_or_init(|| curios_wasm::FuncName::from("bytes/force"))
            .clone()
    }

    pub(crate) fn bytes_force_used(&self) -> bool {
        self.bytes_force.get().is_some()
    }

    /// `$bits/force (ref $rope/bin) -> (ref $bytes)`: flatten a bit-grain rope to its packed payload, memoizing in the entry node. First use marks it for emission.
    pub(crate) fn bits_force_func(&self) -> curios_wasm::FuncName {
        self.bits_force
            .get_or_init(|| curios_wasm::FuncName::from("bits/force"))
            .clone()
    }

    pub(crate) fn bits_force_used(&self) -> bool {
        self.bits_force.get().is_some()
    }

    /// `$list/force (ref $rope/list) -> (ref $elems)`: the `List` mirror of [`bytes_force_func`](Self::bytes_force_func).
    pub(crate) fn list_force_func(&self) -> curios_wasm::FuncName {
        self.list_force
            .get_or_init(|| curios_wasm::FuncName::from("list/force"))
            .clone()
    }

    pub(crate) fn list_force_used(&self) -> bool {
        self.list_force.get().is_some()
    }

    /// `$list/bytes/force (ref $rope/list) -> (ref $elems)`: force a `List(Bytes)` / `List(Handle)` host argument *deeply* — the outer rope to a fresh payload whose every element is itself forced to `$bytes`, the element shape the host lifts.
    pub(crate) fn list_bytes_force_func(&self) -> curios_wasm::FuncName {
        self.list_bytes_force
            .get_or_init(|| curios_wasm::FuncName::from("list/bytes/force"))
            .clone()
    }

    pub(crate) fn list_bytes_force_used(&self) -> bool {
        self.list_bytes_force.get().is_some()
    }

    /// `$list/bits/force (ref $rope/list) -> (ref $elems)`: the bit-grain mirror of [`list_bytes_force_func`](Self::list_bytes_force_func) — each element forced through `$bits/force`, so the host reads a `List(Bits)`'s elements as packed `$bytes` too.
    pub(crate) fn list_bits_force_func(&self) -> curios_wasm::FuncName {
        self.list_bits_force
            .get_or_init(|| curios_wasm::FuncName::from("list/bits/force"))
            .clone()
    }

    pub(crate) fn list_bits_force_used(&self) -> bool {
        self.list_bits_force.get().is_some()
    }

    /// `$bytes/embed (ref $bytes) -> (ref $rope/bin)`: embed a host-built flat payload into a fresh leaf on re-entry.
    pub(crate) fn bytes_embed_func(&self) -> curios_wasm::FuncName {
        self.bytes_embed
            .get_or_init(|| curios_wasm::FuncName::from("bytes/embed"))
            .clone()
    }

    pub(crate) fn bytes_embed_used(&self) -> bool {
        self.bytes_embed.get().is_some()
    }

    /// `$bits/embed (ref $bytes) -> (ref $rope/bin)`: the bit-grain mirror of [`bytes_embed_func`](Self::bytes_embed_func) — the same host-built payload sealed at eight times its byte count, which is what a `Bits` wire slot means.
    pub(crate) fn bits_embed_func(&self) -> curios_wasm::FuncName {
        self.bits_embed
            .get_or_init(|| curios_wasm::FuncName::from("bits/embed"))
            .clone()
    }

    pub(crate) fn bits_embed_used(&self) -> bool {
        self.bits_embed.get().is_some()
    }

    /// `$bytes/box (ref null any) -> (ref $rope/bin)`: a small-canonical `Bytes` as a rope — an immediate is materialised into a fresh leaf, a rope passes through. The entry every rope-shaped consumer pays instead of the `ref.cast` that predates the immediate form.
    pub(crate) fn bytes_box_func(&self) -> curios_wasm::FuncName {
        self.bytes_box
            .get_or_init(|| curios_wasm::FuncName::from("bytes/box"))
            .clone()
    }

    pub(crate) fn bytes_box_used(&self) -> bool {
        self.bytes_box.get().is_some()
    }

    /// `$flt/rem (f64, f64) -> f64`: the exact `fmod` every constant folder computes, as a function because WebAssembly has no `f64.rem`. It used to be expanded inline as `x - trunc(x / y) * y`, which rounds at each step and disagrees with `fmod` on roughly half of all finite operand pairs — `1e8 % 3` came out `0` at runtime against the folded `1`.
    pub(crate) fn flt_rem_func(&self) -> curios_wasm::FuncName {
        self.flt_rem
            .get_or_init(|| curios_wasm::FuncName::from("flt/rem"))
            .clone()
    }

    pub(crate) fn flt_rem_used(&self) -> bool {
        self.flt_rem.get().is_some()
    }

    /// `$bytes/norm (ref $rope/bin) -> (ref any)`: the canonical form of a byte rope — at most 3 bytes becomes the i31 (length in the top 2 payload bits, bytes LSB-first below), anything longer passes through. Every byte-grain producer answers through this, which is what makes a mixed-representation pair unrepresentable.
    pub(crate) fn bytes_norm_func(&self) -> curios_wasm::FuncName {
        self.bytes_norm
            .get_or_init(|| curios_wasm::FuncName::from("bytes/norm"))
            .clone()
    }

    pub(crate) fn bytes_norm_used(&self) -> bool {
        self.bytes_norm.get().is_some()
    }

    /// `$bits/box (ref null any) -> (ref $rope/bin)`: the bit-grain mirror of [`bytes_box_func`](Self::bytes_box_func) — length in the top 5 payload bits (0–26), packed bits LSB-first below.
    pub(crate) fn bits_box_func(&self) -> curios_wasm::FuncName {
        self.bits_box
            .get_or_init(|| curios_wasm::FuncName::from("bits/box"))
            .clone()
    }

    pub(crate) fn bits_box_used(&self) -> bool {
        self.bits_box.get().is_some()
    }

    /// `$bits/norm (ref $rope/bin) -> (ref any)`: the bit-grain mirror of [`bytes_norm_func`](Self::bytes_norm_func) — at most 26 bits packs into the i31, anything longer passes through.
    pub(crate) fn bits_norm_func(&self) -> curios_wasm::FuncName {
        self.bits_norm
            .get_or_init(|| curios_wasm::FuncName::from("bits/norm"))
            .clone()
    }

    pub(crate) fn bits_norm_used(&self) -> bool {
        self.bits_norm.get().is_some()
    }

    /// `$list/embed (ref $elems) -> (ref $rope/list)`: the `List` mirror of [`bytes_embed_func`](Self::bytes_embed_func), for scalar-element results.
    pub(crate) fn list_embed_func(&self) -> curios_wasm::FuncName {
        self.list_embed
            .get_or_init(|| curios_wasm::FuncName::from("list/embed"))
            .clone()
    }

    pub(crate) fn list_embed_used(&self) -> bool {
        self.list_embed.get().is_some()
    }

    /// `$list/<leaf>/to_<payload>`, the helper a list of `leaf` crosses to a host through, marked for emission by this first use.
    pub(crate) fn scalars_force_func(&self, leaf: WireLeaf) -> curios_wasm::FuncName {
        record_leaf(&self.scalars_forces, leaf);
        curios_wasm::FuncName::from(format!(
            "list/{}/to_{}",
            scalar_leaf_name(leaf),
            self.scalars_type(leaf)
        ))
    }

    /// The leaves whose `to_<payload>` helper the emitted code referenced.
    pub(crate) fn scalars_forces(&self) -> Vec<WireLeaf> {
        self.scalars_forces.borrow().clone()
    }

    /// `$list/<leaf>/of_<payload>`, the helper a host's list of `leaf` comes back through, marked for emission by this first use.
    pub(crate) fn scalars_embed_func(&self, leaf: WireLeaf) -> curios_wasm::FuncName {
        record_leaf(&self.scalars_embeds, leaf);
        curios_wasm::FuncName::from(format!(
            "list/{}/of_{}",
            scalar_leaf_name(leaf),
            self.scalars_type(leaf)
        ))
    }

    /// The leaves whose `of_<payload>` helper the emitted code referenced.
    pub(crate) fn scalars_embeds(&self) -> Vec<WireLeaf> {
        self.scalars_embeds.borrow().clone()
    }

    /// Box one raw scalar the host answered, alone or as a list's element: a `Nat`'s `i64` read unsigned through `big/of_u64` and an `Int`'s read signed through `big/of_i64`, each answering the i31 below `2³⁰` in magnitude and the boxed magnitude past it; a `Bool` is its own i31. An `Flt` stays the raw `f64` its continuation takes, since `curios-cont` offers that parameter at its carrier.
    pub(crate) fn box_word_instrs(&self, wire_type: &WireType) -> Vec<curios_wasm::Instr> {
        match wire_type {
            WireType::Nat => vec![call(&self.big_func(BigHelper::OfU64))],
            WireType::Int => vec![call(&self.big_func(BigHelper::OfI64))],
            WireType::Bool => vec![curios_wasm::Instr::RefI31],
            _ => vec![],
        }
    }

    /// `$list/bytes/embed (ref $elems) -> (ref $rope/list)`: embed a `List(Bytes)` host result *deeply* — each raw `$bytes` element into a leaf (in place; the host-built array is fresh), then the outer array.
    pub(crate) fn list_bytes_embed_func(&self) -> curios_wasm::FuncName {
        self.list_bytes_embed
            .get_or_init(|| curios_wasm::FuncName::from("list/bytes/embed"))
            .clone()
    }

    pub(crate) fn list_bytes_embed_used(&self) -> bool {
        self.list_bytes_embed.get().is_some()
    }

    /// `$list/bits/embed (ref $elems) -> (ref $rope/list)`: the bit-grain mirror of [`list_bytes_embed_func`](Self::list_bytes_embed_func) — each element sealed at eight times its byte count and normalised through `$bits/norm`.
    pub(crate) fn list_bits_embed_func(&self) -> curios_wasm::FuncName {
        self.list_bits_embed
            .get_or_init(|| curios_wasm::FuncName::from("list/bits/embed"))
            .clone()
    }

    pub(crate) fn list_bits_embed_used(&self) -> bool {
        self.list_bits_embed.get().is_some()
    }

    /// `$bytes/slice (ref $rope/bin, i32, i32) -> (ref $rope/bin)`, taking a start and a *count*: the `Bytes` O(1) view constructor — bounds-check, answer the empty leaf or the whole rope on the trivial windows, collapse a view-of-view, and force an uncached node base so every `view` it builds reads through in O(1).
    pub(crate) fn bytes_slice_func(&self) -> curios_wasm::FuncName {
        self.bytes_slice
            .get_or_init(|| curios_wasm::FuncName::from("bytes/slice"))
            .clone()
    }

    pub(crate) fn bytes_slice_used(&self) -> bool {
        self.bytes_slice.get().is_some()
    }

    pub(crate) fn bits_slice_func(&self) -> curios_wasm::FuncName {
        self.bits_slice
            .get_or_init(|| curios_wasm::FuncName::from("bits/slice"))
            .clone()
    }

    pub(crate) fn bits_slice_used(&self) -> bool {
        self.bits_slice.get().is_some()
    }

    /// `$list/slice (ref $rope/list, i32, i32) -> (ref $rope/list)`: the `List` mirror of [`bytes_slice_func`](Self::bytes_slice_func).
    pub(crate) fn list_slice_func(&self) -> curios_wasm::FuncName {
        self.list_slice
            .get_or_init(|| curios_wasm::FuncName::from("list/slice"))
            .clone()
    }

    pub(crate) fn list_slice_used(&self) -> bool {
        self.list_slice.get().is_some()
    }

    /// `$bytes/read (ref $rope/bin, i32) -> i32`: one byte read — straight off a leaf payload, through a `view`'s window without forcing, and via `$bytes/force` (memoized) on a node.
    pub(crate) fn bytes_read_func(&self) -> curios_wasm::FuncName {
        self.bytes_read
            .get_or_init(|| curios_wasm::FuncName::from("bytes/read"))
            .clone()
    }

    pub(crate) fn bytes_read_used(&self) -> bool {
        self.bytes_read.get().is_some()
    }

    pub(crate) fn bits_read_func(&self) -> curios_wasm::FuncName {
        self.bits_read
            .get_or_init(|| curios_wasm::FuncName::from("bits/read"))
            .clone()
    }

    pub(crate) fn bits_read_used(&self) -> bool {
        self.bits_read.get().is_some()
    }

    /// `$list/read (ref $rope/list, i32) -> anyref`: the `List` mirror of [`bytes_read_func`](Self::bytes_read_func).
    pub(crate) fn list_read_func(&self) -> curios_wasm::FuncName {
        self.list_read
            .get_or_init(|| curios_wasm::FuncName::from("list/read"))
            .clone()
    }

    pub(crate) fn list_read_used(&self) -> bool {
        self.list_read.get().is_some()
    }

    /// `$bytes/eql (ref $rope/bin, ref $rope/bin) -> i32`: whole-value byte equality — unequal rope lengths answer without forcing, equal lengths force both payloads once and compare bytewise.
    pub(crate) fn bytes_eql_func(&self) -> curios_wasm::FuncName {
        self.bytes_eql
            .get_or_init(|| curios_wasm::FuncName::from("bytes/eql"))
            .clone()
    }

    pub(crate) fn bytes_eql_used(&self) -> bool {
        self.bytes_eql.get().is_some()
    }

    pub(crate) fn bits_eql_func(&self) -> curios_wasm::FuncName {
        self.bits_eql
            .get_or_init(|| curios_wasm::FuncName::from("bits/eql"))
            .clone()
    }

    pub(crate) fn bits_eql_used(&self) -> bool {
        self.bits_eql.get().is_some()
    }

    /// `$bin/and (ref $payload, ref $payload, i32) -> (ref $rope/bin)`: two forced payloads combined byte for byte, sealed at the logical length the caller supplies.
    ///
    /// Grain-free, unlike the `eql` and `replicate` pairs either side of it: the operands arrive forced, and a walk over two byte arrays is the same walk whichever generators packed them.
    pub(crate) fn bin_and_func(&self) -> curios_wasm::FuncName {
        self.bin_and
            .get_or_init(|| curios_wasm::FuncName::from("bin/and"))
            .clone()
    }

    pub(crate) fn bin_and_used(&self) -> bool {
        self.bin_and.get().is_some()
    }

    /// The disjunction, as [`bin_and_func`](Self::bin_and_func).
    pub(crate) fn bin_or_func(&self) -> curios_wasm::FuncName {
        self.bin_or
            .get_or_init(|| curios_wasm::FuncName::from("bin/or"))
            .clone()
    }

    pub(crate) fn bin_or_used(&self) -> bool {
        self.bin_or.get().is_some()
    }

    /// The difference, as [`bin_and_func`](Self::bin_and_func).
    pub(crate) fn bin_xor_func(&self) -> curios_wasm::FuncName {
        self.bin_xor
            .get_or_init(|| curios_wasm::FuncName::from("bin/xor"))
            .clone()
    }

    pub(crate) fn bin_xor_used(&self) -> bool {
        self.bin_xor.get().is_some()
    }

    /// `$bytes/replicate (i32 count, i32 atom) -> (ref $rope/bin)`: `count` copies of one byte, as one flat leaf.
    pub(crate) fn bytes_replicate_func(&self) -> curios_wasm::FuncName {
        self.bytes_replicate
            .get_or_init(|| curios_wasm::FuncName::from("bytes/replicate"))
            .clone()
    }

    pub(crate) fn bytes_replicate_used(&self) -> bool {
        self.bytes_replicate.get().is_some()
    }

    /// `$bits/replicate (i32 count, i32 atom) -> (ref $rope/bin)`: the bit grain's fill, which masks the padding its all-ones byte would otherwise leave set.
    pub(crate) fn bits_replicate_func(&self) -> curios_wasm::FuncName {
        self.bits_replicate
            .get_or_init(|| curios_wasm::FuncName::from("bits/replicate"))
            .clone()
    }

    pub(crate) fn bits_replicate_used(&self) -> bool {
        self.bits_replicate.get().is_some()
    }

    /// `$bytes/to_bits (ref $rope/bin) -> (ref $rope/bin)`: the same payload resealed at eight times the length, which is what a byte run is as bits.
    pub(crate) fn bytes_to_bits_func(&self) -> curios_wasm::FuncName {
        self.bytes_to_bits
            .get_or_init(|| curios_wasm::FuncName::from("bytes/to_bits"))
            .clone()
    }

    pub(crate) fn bytes_to_bits_used(&self) -> bool {
        self.bytes_to_bits.get().is_some()
    }

    /// `$bits/to_bytes (ref $rope/bin) -> (ref $rope/bin)`: the reverse, exact because the caller proved the run holds whole bytes.
    pub(crate) fn bits_to_bytes_func(&self) -> curios_wasm::FuncName {
        self.bits_to_bytes
            .get_or_init(|| curios_wasm::FuncName::from("bits/to_bytes"))
            .clone()
    }

    pub(crate) fn bits_to_bytes_used(&self) -> bool {
        self.bits_to_bytes.get().is_some()
    }

    /// `$list/map (ref $rope/list, ref $envr/1) -> (ref $rope/list)`: apply a unary closure to every element of the forced payload, filling a fresh leaf.
    pub(crate) fn list_map_func(&self) -> curios_wasm::FuncName {
        self.list_map
            .get_or_init(|| curios_wasm::FuncName::from("list/map"))
            .clone()
    }

    pub(crate) fn list_map_used(&self) -> bool {
        self.list_map.get().is_some()
    }

    pub(crate) fn tuple_types(&self) -> impl Iterator<Item = (usize, curios_wasm::TypeName)> {
        self.tuple_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    /// Every declared row type, with the carrier of each slot its struct holds.
    pub(crate) fn row_types(
        &self,
    ) -> impl Iterator<
        Item = (
            curios_cont::RowId,
            curios_wasm::TypeName,
            &[curios_cont::Slot],
        ),
    > {
        self.row_types
            .iter()
            .map(|(row, (type_name, slots))| (*row, type_name.clone(), slots.as_slice()))
    }

    /// How a value is loaded to fill `slot`, and how a read of it is coerced back.
    ///
    /// A typed reference slot admits null, because the slots a narrow constructor leaves unwritten hold one; every other carrier is loaded exactly as any position naming it.
    pub(crate) fn slot_load_as(&self, slot: curios_cont::Slot) -> LoadAs {
        match slot {
            curios_cont::Slot::Tag | curios_cont::Slot::Nat => LoadAs::Nat,
            curios_cont::Slot::Flt => LoadAs::Flt,
            curios_cont::Slot::List => LoadAs::ConcreteOrNull(self.list_rope().base.clone()),
            curios_cont::Slot::Closure(arity) => LoadAs::ConcreteOrNull(self.find_envr_type(arity)),
            curios_cont::Slot::Row(row) => LoadAs::ConcreteOrNull(self.find_row_type(row)),
            curios_cont::Slot::Opaque => LoadAs::Null,
        }
    }

    /// The carriers of `row`'s slots.
    pub(crate) fn row_slots(&self, row: curios_cont::RowId) -> &[curios_cont::Slot] {
        &self
            .row_types
            .get(&row)
            .unwrap_or_else(|| panic!("`Table` lacks a type for row `{}`", row))
            .1
    }

    pub(crate) fn find_row_type(&self, row: curios_cont::RowId) -> curios_wasm::TypeName {
        self.row_types
            .get(&row)
            .unwrap_or_else(|| panic!("`Table` lacks a type for row `{}`", row))
            .0
            .clone()
    }

    pub(crate) fn find_tuple_type(&self, arity: usize) -> curios_wasm::TypeName {
        self.tuple_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks tuple type for arity `{}`", arity))
            .clone()
    }

    pub(crate) fn tuple_field(index: usize) -> curios_wasm::FieldName {
        curios_wasm::FieldName::from(index.to_string())
    }

    pub(crate) fn envr_types(&self) -> impl Iterator<Item = curios_wasm::TypeName> {
        self.envr_types.values().cloned()
    }

    pub(crate) fn find_envr_type(&self, arity: usize) -> curios_wasm::TypeName {
        self.envr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks environment type for arity `{}`", arity))
            .clone()
    }

    pub(crate) fn clsr_types(&self) -> impl Iterator<Item = (usize, curios_wasm::TypeName)> {
        self.clsr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub(crate) fn find_clsr_type(&self, arity: usize) -> curios_wasm::TypeName {
        self.clsr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks closure type for arity `{}`", arity))
            .clone()
    }

    /// The emitted name of the function type with this `(parameters, results)` shape.
    ///
    /// A single-result shape keeps the bare `func/{parameters}` spelling it has always had, which is what made keying on the pair a change of the key alone and of no emitted module when it landed. What produces the wider shapes is `cps::protocol`, which hands a class of functions back the leading fields of the construction they used to allocate; that class is a per-tail-call-component decision, because `return_call` requires a callee's results to match its caller's exactly. Closure supertypes are keyed separately, on arity alone, and stay single-result — which is why the decision excludes any function that escapes.
    fn func_type_name(shape: (usize, usize)) -> curios_wasm::TypeName {
        let (parameters, results) = shape;

        match results {
            1 => curios_wasm::TypeName::from(format!("func/{}", parameters)),
            _ => curios_wasm::TypeName::from(format!("func/{}/{}", parameters, results)),
        }
    }

    pub(crate) fn func_types(
        &self,
    ) -> impl Iterator<Item = ((usize, usize), curios_wasm::TypeName)> {
        self.func_types
            .iter()
            .map(|(shape, type_name)| (*shape, type_name.clone()))
    }

    pub(crate) fn find_func_type(&self, shape: (usize, usize)) -> curios_wasm::TypeName {
        self.func_types
            .get(&shape)
            .unwrap_or_else(|| {
                panic!(
                    "`Table` lacks function type for shape `{}` parameters, `{}` results",
                    shape.0, shape.1
                )
            })
            .clone()
    }

    /// Whether this name is a module const holding a `Tuple`/`List`. See `Context::refuse_raw_aggregate`.
    pub(crate) fn is_aggregate_const(&self, const_name: &EmissionValueName) -> bool {
        self.const_aggregates.contains(const_name)
    }

    pub(crate) fn find_const(&self, const_name: &EmissionValueName) -> curios_wasm::GlobalName {
        self.consts
            .get(const_name)
            .unwrap_or_else(|| panic!("`Table` lacks const `{}`", const_name))
            .clone()
    }

    /// Deliberately no iterating accessor: these are `HashMap`s, so iteration order varies per process, and every consumer here emits into the module — where order is load-bearing for a reproducible build. Walk [`EmissionModule`]'s own ordered sequence and resolve each name through this index instead, which is what `curios-utilities`'s `name!` means by carrying an explicit sequence where the order matters.
    /// The carrier this value is held at in a register, or `None` when it is held behind a reference.
    pub(crate) fn raw_carrier(&self, value_name: &EmissionValueName) -> Option<curios_cont::Repr> {
        self.raw.get(value_name).copied()
    }

    /// The wasm type this value's local is declared at: the machine type of its carrier when it is held in a register, and the top reference type otherwise.
    ///
    /// The reference arms are unreachable — the analysis only ever answers a scalar carrier, since those are the only ones a register holds — and they answer `top_type` rather than panicking because a representation that cannot be held raw and a value that was never offered one are the same fact, and this function's job is to state it once.
    pub(crate) fn local_type(&self, value_name: &EmissionValueName) -> curios_wasm::ValType {
        match self.raw_carrier(value_name) {
            Some(curios_cont::Repr::Nat) => curios_wasm::ValType::Num(curios_wasm::NumType::I32),
            Some(curios_cont::Repr::Flt) => curios_wasm::ValType::Num(curios_wasm::NumType::F64),
            Some(
                curios_cont::Repr::Number
                | curios_cont::Repr::Bin(_)
                | curios_cont::Repr::List
                | curios_cont::Repr::Ref,
            )
            | None => Table::top_type(true),
        }
    }

    pub(crate) fn find_clsr(&self, clsr_name: &EmissionClosureName) -> &ClsrData<'a> {
        self.clsrs
            .get(clsr_name)
            .unwrap_or_else(|| panic!("`Table` lacks closure `{}`", clsr_name))
    }

    pub(crate) fn find_func(&self, func_name: &EmissionFunctionName) -> &FuncData<'a> {
        self.funcs
            .get(func_name)
            .unwrap_or_else(|| panic!("`Table` lacks func `{}`", func_name))
    }
}

#[cfg(test)]
mod tests;

/// Record `leaf` once, in first-use order.
fn record_leaf(used: &RefCell<Vec<WireLeaf>>, leaf: WireLeaf) {
    let mut used = used.borrow_mut();
    if !used.contains(&leaf) {
        used.push(leaf);
    }
}

/// How a scalar leaf is spelled in its helpers' names.
fn scalar_leaf_name(leaf: WireLeaf) -> &'static str {
    match leaf {
        WireLeaf::Nat => "nat",
        WireLeaf::Int => "int",
        WireLeaf::Bool => "bool",
        WireLeaf::Bytes | WireLeaf::Bits | WireLeaf::Handle => {
            panic!("a list of `{leaf:?}` crosses as its payloads, never as scalars")
        }
    }
}
