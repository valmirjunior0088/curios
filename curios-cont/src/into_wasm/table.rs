use {
    super::{
        EmissionBlockName, EmissionBody, EmissionClosure, EmissionClosureName, EmissionCode,
        EmissionData, EmissionFunction, EmissionFunctionName, EmissionModule, EmissionValue,
        EmissionValueName,
    },
    crate::{CpsIntrinsic, Repr},
    curios_abi::ForeignFunction,
    std::{
        cell::{OnceCell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
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

#[derive(Debug, Clone)]
pub(crate) struct ClsrData<'a> {
    name: &'a EmissionClosureName,
    /// This closure's slot in the shared funcref table — the value its environment's special field holds. 1-based: slot 0 stays null so a `struct.new_default` shell's zeroed field dispatches into the null entry and traps, as the unfilled funcref did.
    index: i32,
    func_name: curios_wasm::FuncName,
    clsr_type: curios_wasm::TypeName,
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
            name: clsr_name,
            index,
            func_name: curios_wasm::FuncName::from(format!("clsr/{}", clsr_name)),
            clsr_type: curios_wasm::TypeName::from(format!("clsr/{}", clsr_name)),
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

    pub(crate) fn name(&self) -> &'a EmissionClosureName {
        self.name
    }

    pub(crate) fn index(&self) -> i32 {
        self.index
    }

    pub(crate) fn func_name(&self) -> curios_wasm::FuncName {
        self.func_name.clone()
    }

    pub(crate) fn clsr_type(&self) -> curios_wasm::TypeName {
        self.clsr_type.clone()
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
        EmissionValue::Eval(EmissionCode::Intrinsic(CpsIntrinsic::TupleGet(index), _)) => index + 1,
        _ => 0,
    }
}

/// Collect every closure that is reserved as a recursive shell anywhere in `region` (and its nested blocks). These are the only closures whose `envr` fields are back-patched, so they are the only ones whose wasm struct fields must stay mutable.
fn collect_cyclic_clsrs(region: &EmissionBody, out: &mut HashSet<EmissionClosureName>) {
    for (_, clsr) in &region.shells {
        out.insert(clsr.clone());
    }

    for (_, block) in &region.blocks {
        collect_cyclic_clsrs(&block.region, out);
    }
}

fn max_region_tuple_arity(region: &EmissionBody) -> usize {
    // Recursive fallback entries are closure shells only, so they contribute no tuple arity; the arities all come from tuple constructions and projections in `values` (and nested blocks).
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
    clsr_table: curios_wasm::TableName,
    flt_type: curios_wasm::TypeName,
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
    // The shared rope helpers, minted lazily like `exit`: the first call site recorded during emission names the function, and the module emitter then adds exactly the recorded set after the program's own functions (see `emit_rope_funcs`).
    bytes_force: OnceCell<curios_wasm::FuncName>,
    bits_force: OnceCell<curios_wasm::FuncName>,
    list_force: OnceCell<curios_wasm::FuncName>,
    list_bytes_force: OnceCell<curios_wasm::FuncName>,
    bytes_embed: OnceCell<curios_wasm::FuncName>,
    bytes_box: OnceCell<curios_wasm::FuncName>,
    bytes_norm: OnceCell<curios_wasm::FuncName>,
    bits_box: OnceCell<curios_wasm::FuncName>,
    bits_norm: OnceCell<curios_wasm::FuncName>,
    list_embed: OnceCell<curios_wasm::FuncName>,
    list_bytes_embed: OnceCell<curios_wasm::FuncName>,
    bytes_slice: OnceCell<curios_wasm::FuncName>,
    bits_slice: OnceCell<curios_wasm::FuncName>,
    list_slice: OnceCell<curios_wasm::FuncName>,
    bytes_read: OnceCell<curios_wasm::FuncName>,
    bits_read: OnceCell<curios_wasm::FuncName>,
    list_read: OnceCell<curios_wasm::FuncName>,
    bytes_eql: OnceCell<curios_wasm::FuncName>,
    bits_eql: OnceCell<curios_wasm::FuncName>,
    list_map: OnceCell<curios_wasm::FuncName>,
    // The foreign functions the emitted code calls, keyed by the minted internal name (see `host_func`). Same lazy used-tracking as the `exit` cell: the first call-site reference during emission records the function's row, and `emit_sys_imports` then declares exactly the recorded set (in minted-name order — wasmtime links by name, so import order is cosmetic).
    host_funcs: RefCell<BTreeMap<String, Arc<ForeignFunction>>>,
    tuple_types: BTreeMap<usize, curios_wasm::TypeName>,
    envr_types: BTreeMap<usize, curios_wasm::TypeName>,
    clsr_types: BTreeMap<usize, curios_wasm::TypeName>,
    /// Keyed by the pair a wasm function type actually is — parameter count *and* result count — rather than by parameter count alone, so two functions of the same arity delivering different result shapes cannot collide on one type. The closure supertypes below stay keyed by arity, because a function reached through one is invoked at the uniform shape whatever its own type says.
    func_types: BTreeMap<(usize, usize), curios_wasm::TypeName>,
    consts: HashMap<&'a EmissionValueName, curios_wasm::GlobalName>,
    /// The module consts that are `Tuple`/`List` constructions — the hoisted half of the population `Context::refuse_raw_aggregate` refuses to hand to a register. A closed aggregate is lifted out of its region by `hoist`, so a guard reading region values alone would miss exactly the constant ones.
    const_aggregates: HashSet<&'a EmissionValueName>,
    clsrs: HashMap<&'a EmissionClosureName, ClsrData<'a>>,
    funcs: HashMap<&'a EmissionFunctionName, FuncData<'a>>,
    // Closures that are ever shell'd as a recursive shell — their `envr` fields are back-patched (`struct.set`), so those fields must stay mutable. Every other aggregate field is immutable. `cyclic_clsr_arities` carries the same fact at arity granularity, for the shared `envr/N` special field (which must agree across all its subtypes).
    cyclic_clsrs: HashSet<EmissionClosureName>,
    cyclic_clsr_arities: BTreeSet<usize>,
    /// The values the representation analysis decided to hold in a register, and at which carrier. A name absent here is held behind a reference, which is every synthetic name codegen mints for itself.
    raw: &'a HashMap<EmissionValueName, Repr>,
}

impl<'a> Table<'a> {
    pub(crate) fn new(
        module: &'a EmissionModule,
        raw: &'a HashMap<EmissionValueName, Repr>,
    ) -> Self {
        let mut cyclic_clsrs = HashSet::new();
        for (_, clsr) in module.clsrs() {
            collect_cyclic_clsrs(&clsr.region, &mut cyclic_clsrs);
        }
        for (_, func) in module.funcs() {
            collect_cyclic_clsrs(&func.region, &mut cyclic_clsrs);
        }

        let arities = module
            .clsrs()
            .iter()
            .map(|(name, clsr)| (name.clone(), clsr.params.len()))
            .collect::<HashMap<EmissionClosureName, usize>>();

        let cyclic_clsr_arities = cyclic_clsrs
            .iter()
            .filter_map(|name| arities.get(name).copied())
            .collect::<BTreeSet<usize>>();

        Self {
            cyclic_clsrs,
            cyclic_clsr_arities,
            raw,
            special_field: curios_wasm::FieldName::from("!"),
            special_local: curios_wasm::LocalName::from("!"),
            special_label: curios_wasm::LabelName::from("!"),
            clsr_table: curios_wasm::TableName::from("clsr"),
            flt_type: curios_wasm::TypeName::from("flt"),
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
            bytes_force: OnceCell::new(),
            bits_force: OnceCell::new(),
            list_force: OnceCell::new(),
            list_bytes_force: OnceCell::new(),
            bytes_embed: OnceCell::new(),
            bytes_box: OnceCell::new(),
            bytes_norm: OnceCell::new(),
            bits_box: OnceCell::new(),
            bits_norm: OnceCell::new(),
            list_embed: OnceCell::new(),
            list_bytes_embed: OnceCell::new(),
            bytes_slice: OnceCell::new(),
            bits_slice: OnceCell::new(),
            list_slice: OnceCell::new(),
            bytes_read: OnceCell::new(),
            bits_read: OnceCell::new(),
            list_read: OnceCell::new(),
            bytes_eql: OnceCell::new(),
            bits_eql: OnceCell::new(),
            list_map: OnceCell::new(),
            host_funcs: RefCell::new(BTreeMap::new()),
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
                .filter(|(_, data)| matches!(data, EmissionData::List(_) | EmissionData::Tuple(_)))
                .map(|(const_name, _)| const_name)
                .collect(),
            // Table indices come from the module's ordered closure walk — the same order the element segment lists the bodies in — never from this map's iteration.
            clsrs: module
                .clsrs()
                .iter()
                .enumerate()
                .map(|(index, (clsr_name, clsr))| {
                    (clsr_name, ClsrData::new(clsr_name, clsr, index as i32 + 1))
                })
                .collect(),
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

    /// The module-level funcref table holding every closure body, indexed by [`ClsrData::index`].
    pub(crate) fn clsr_table(&self) -> curios_wasm::TableName {
        self.clsr_table.clone()
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

    pub(crate) fn list_rope_type(&self) -> curios_wasm::TypeName {
        self.list_rope_type.clone()
    }

    pub(crate) fn bytes_type(&self) -> curios_wasm::TypeName {
        self.bytes_type.clone()
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
    /// A row's identity is its `(namespace, name)` pair (see [`ForeignFunction`]), and its name is chosen outside the emitter, so the minted name embeds both components under the reserved `host/` family prefix — a foreign name can never collide with a runtime helper, another minted family, or a same-named row from another namespace. The embedding is injective because namespaces are compiler-chosen and never contain `/`.
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

    /// `$bytes/embed (ref $bytes) -> (ref $rope/bin)`: embed a host-built flat payload into a fresh leaf on re-entry.
    pub(crate) fn bytes_embed_func(&self) -> curios_wasm::FuncName {
        self.bytes_embed
            .get_or_init(|| curios_wasm::FuncName::from("bytes/embed"))
            .clone()
    }

    pub(crate) fn bytes_embed_used(&self) -> bool {
        self.bytes_embed.get().is_some()
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

    /// `$list/bytes/embed (ref $elems) -> (ref $rope/list)`: embed a `List(Bytes)` host result *deeply* — each raw `$bytes` element into a leaf (in place; the host-built array is fresh), then the outer array.
    pub(crate) fn list_bytes_embed_func(&self) -> curios_wasm::FuncName {
        self.list_bytes_embed
            .get_or_init(|| curios_wasm::FuncName::from("list/bytes/embed"))
            .clone()
    }

    pub(crate) fn list_bytes_embed_used(&self) -> bool {
        self.list_bytes_embed.get().is_some()
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

    pub(crate) fn find_tuple_type(&self, arity: usize) -> curios_wasm::TypeName {
        self.tuple_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks tuple type for arity `{}`", arity))
            .clone()
    }

    pub(crate) fn tuple_field(index: usize) -> curios_wasm::FieldName {
        curios_wasm::FieldName::from(index.to_string())
    }

    /// Whether this closure is ever reserved as a recursive shell, i.e. its `envr` payload fields are back-patched and so must be declared mutable.
    pub(crate) fn is_cyclic_clsr(&self, name: &EmissionClosureName) -> bool {
        self.cyclic_clsrs.contains(name)
    }

    /// Whether *any* closure of this arity is cyclic. The shared `envr/N` special field (and thus every `envr/<clsr>` of that arity, by subtyping invariance) must be mutable iff so.
    pub(crate) fn arity_has_cyclic_clsr(&self, arity: usize) -> bool {
        self.cyclic_clsr_arities.contains(&arity)
    }

    fn field_mutability(is_mutable: bool) -> curios_wasm::Mutability {
        if is_mutable {
            curios_wasm::Mutability::Var
        } else {
            curios_wasm::Mutability::Const
        }
    }

    pub(crate) fn tuple_field_mutability() -> curios_wasm::Mutability {
        // Tuples are never cyclic (rejected in `into_cont`), so they are never back-patched.
        Self::field_mutability(false)
    }

    pub(crate) fn envr_special_mutability(&self, arity: usize) -> curios_wasm::Mutability {
        Self::field_mutability(self.arity_has_cyclic_clsr(arity))
    }

    pub(crate) fn envr_payload_mutability(
        &self,
        name: &EmissionClosureName,
    ) -> curios_wasm::Mutability {
        Self::field_mutability(self.is_cyclic_clsr(name))
    }

    pub(crate) fn envr_types(&self) -> impl Iterator<Item = (usize, curios_wasm::TypeName)> {
        self.envr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
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
    pub(crate) fn raw_carrier(&self, value_name: &EmissionValueName) -> Option<Repr> {
        self.raw.get(value_name).copied()
    }

    /// The wasm type this value's local is declared at: the machine type of its carrier when it is held in a register, and the top reference type otherwise.
    ///
    /// The reference arms are unreachable — the analysis only ever answers a scalar carrier, since those are the only ones a register holds — and they answer `top_type` rather than panicking because a representation that cannot be held raw and a value that was never offered one are the same fact, and this function's job is to state it once.
    pub(crate) fn local_type(&self, value_name: &EmissionValueName) -> curios_wasm::ValType {
        match self.raw_carrier(value_name) {
            Some(Repr::Nat | Repr::Int) => curios_wasm::ValType::Num(curios_wasm::NumType::I32),
            Some(Repr::Flt) => curios_wasm::ValType::Num(curios_wasm::NumType::F32),
            Some(Repr::Bin(_) | Repr::List | Repr::Ref) | None => Table::top_type(true),
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
mod tests {
    use {
        super::{EmissionModule, Table},
        std::collections::HashMap,
    };

    #[test]
    fn rope_names_match_the_wasm_vocabulary() {
        let module = EmissionModule::new();
        let raw = HashMap::new();
        let table = Table::new(&module, &raw);
        let bin = table.bin_rope();
        let list = table.list_rope();

        assert_eq!(bin.base.as_str(), "rope/bin");
        assert_eq!(bin.leaf.as_str(), "rope/bin/leaf");
        assert_eq!(bin.node.as_str(), "rope/bin/node");
        assert_eq!(bin.view.as_str(), "rope/bin/view");
        assert_eq!(bin.payload.as_str(), "bytes");

        assert_eq!(list.base.as_str(), "rope/list");
        assert_eq!(list.leaf.as_str(), "rope/list/leaf");
        assert_eq!(list.node.as_str(), "rope/list/node");
        assert_eq!(list.view.as_str(), "rope/list/view");
        assert_eq!(list.payload.as_str(), "elems");

        assert_eq!(table.bits_force_func().as_str(), "bits/force");
        assert_eq!(table.bits_slice_func().as_str(), "bits/slice");
        assert_eq!(table.bits_read_func().as_str(), "bits/read");
        assert_eq!(table.bits_eql_func().as_str(), "bits/eql");

        assert_eq!(table.bytes_force_func().as_str(), "bytes/force");
        assert_eq!(table.bytes_embed_func().as_str(), "bytes/embed");
        assert_eq!(table.bytes_slice_func().as_str(), "bytes/slice");
        assert_eq!(table.bytes_read_func().as_str(), "bytes/read");
        assert_eq!(table.bytes_eql_func().as_str(), "bytes/eql");

        assert_eq!(table.list_force_func().as_str(), "list/force");
        assert_eq!(table.list_embed_func().as_str(), "list/embed");
        assert_eq!(table.list_slice_func().as_str(), "list/slice");
        assert_eq!(table.list_read_func().as_str(), "list/read");
        assert_eq!(table.list_map_func().as_str(), "list/map");

        assert_eq!(table.list_bytes_force_func().as_str(), "list/bytes/force");
        assert_eq!(table.list_bytes_embed_func().as_str(), "list/bytes/embed");
    }
}
