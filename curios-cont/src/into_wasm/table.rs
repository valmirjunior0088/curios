use {
    curios_abi::ForeignFunction,
    curios_wasm::{
        AbsHeapType, FieldName, FuncName, GlobalName, HeapType, LabelName, LocalName, Mutability,
        RefType, TypeName, ValType,
    },
    std::{
        cell::{OnceCell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap},
        sync::Arc,
    },
};

#[derive(Debug, Clone)]
pub(super) struct FieldData {
    type_name: TypeName,
    field_name: FieldName,
}

impl FieldData {
    pub(super) fn new(type_name: TypeName, field_name: FieldName) -> Self {
        Self {
            type_name,
            field_name,
        }
    }

    pub(super) fn type_name(&self) -> TypeName {
        self.type_name.clone()
    }

    pub(super) fn field_name(&self) -> FieldName {
        self.field_name.clone()
    }
}

/// The name bundle for one internal rope carrier (`$rope/bin` or `$rope/lst`): the base
/// struct the emitter casts carrier refs to, its `leaf`/`node`/`view` subtypes, the flat
/// payload array, and every field name — one handle to thread through the op
/// emitters so packed `Bits`/`Bytes` and `Lst` share their lowering code.
#[derive(Debug, Clone)]
pub(super) struct RopeData {
    pub base: TypeName,
    pub leaf: TypeName,
    pub node: TypeName,
    pub view: TypeName,
    pub payload: TypeName,
    pub tag_field: FieldName,
    pub len_field: FieldName,
    pub payload_field: FieldName,
    pub left_field: FieldName,
    pub right_field: FieldName,
    pub cache_field: FieldName,
    pub base_field: FieldName,
    pub offset_field: FieldName,
}

#[derive(Debug, Clone)]
pub(super) struct ClsrData<'a> {
    name: &'a crate::ClsrName,
    func_name: FuncName,
    clsr_type: TypeName,
    envr_type: TypeName,
    fields: Vec<(&'a crate::ValueName, FieldName)>,
    params: HashMap<&'a crate::ValueName, LocalName>,
    resume: &'a crate::BlockName,
}

impl<'a> ClsrData<'a> {
    pub(super) fn new(clsr_name: &'a crate::ClsrName, clsr: &'a crate::Clsr) -> Self {
        Self {
            name: clsr_name,
            func_name: FuncName::from(format!("clsr/{}", clsr_name)),
            clsr_type: TypeName::from(format!("clsr/{}", clsr_name)),
            envr_type: TypeName::from(format!("envr/{}", clsr_name)),
            fields: clsr
                .fields
                .iter()
                .map(|field| (&field.name, FieldName::from(format!("${}", field.name))))
                .collect(),
            params: clsr
                .params
                .iter()
                .map(|param| (&param.name, LocalName::from(format!("${}", param.name))))
                .collect(),
            resume: &clsr.resume,
        }
    }

    pub(super) fn name(&self) -> &'a crate::ClsrName {
        self.name
    }

    pub(super) fn func_name(&self) -> FuncName {
        self.func_name.clone()
    }

    pub(super) fn clsr_type(&self) -> TypeName {
        self.clsr_type.clone()
    }

    pub(super) fn envr_type(&self) -> TypeName {
        self.envr_type.clone()
    }

    pub(super) fn fields(&self) -> impl Iterator<Item = FieldName> {
        self.fields.iter().map(|(_, field_name)| field_name.clone())
    }

    pub(super) fn find_field(&self, value_name: &crate::ValueName) -> Option<FieldData> {
        self.fields
            .iter()
            .find_map(|(field_name, mapped_field_name)| {
                (value_name == *field_name).then_some(mapped_field_name)
            })
            .cloned()
            .map(|field_name| FieldData::new(self.envr_type(), field_name))
    }

    pub(super) fn params(&self) -> HashMap<&'a crate::ValueName, LocalName> {
        self.params.clone()
    }

    pub(super) fn find_param(&self, value_name: &crate::ValueName) -> Option<LocalName> {
        self.params.get(value_name).cloned()
    }

    pub(super) fn arity(&self) -> usize {
        self.params.len()
    }

    pub(super) fn is_resume(&self, block_name: &crate::BlockName) -> bool {
        self.resume == block_name
    }
}

#[derive(Debug, Clone)]
pub(super) struct FuncData<'a> {
    func_name: FuncName,
    params: HashMap<&'a crate::ValueName, LocalName>,
    resume: &'a crate::BlockName,
}

impl<'a> FuncData<'a> {
    pub(super) fn new(func_name: &'a crate::FuncName, func: &'a crate::Func) -> Self {
        Self {
            // The `func/` prefix is why the exported entrypoint is
            // `func/main`: the entry is always `main`, and the export reuses
            // the function's emitted name.
            func_name: FuncName::from(format!("func/{}", func_name)),
            params: func
                .params
                .iter()
                .map(|param| (&param.name, LocalName::from(format!("${}", param.name))))
                .collect(),
            resume: &func.resume,
        }
    }

    pub(super) fn func_name(&self) -> FuncName {
        self.func_name.clone()
    }

    pub(super) fn arity(&self) -> usize {
        self.params.len()
    }

    pub(super) fn params(&self) -> HashMap<&'a crate::ValueName, LocalName> {
        self.params.clone()
    }

    pub(super) fn find_param(&self, value_name: &crate::ValueName) -> Option<LocalName> {
        self.params.get(value_name).cloned()
    }

    pub(super) fn is_resume(&self, block_name: &crate::BlockName) -> bool {
        self.resume == block_name
    }
}

fn max_tpl_arity(data: &crate::Data) -> usize {
    match data {
        crate::Data::Tpl(fields) => fields.len(),
        _ => 0,
    }
}

fn max_value_tpl_arity(value: &crate::Value) -> usize {
    match value {
        crate::Value::Pure(data) => max_tpl_arity(data),
        // Projecting field `index` reads through a tuple type of arity at least
        // `index + 1`, even when no tuple of that arity is ever *built* in the module
        // (e.g. the projected tuple only ever arrives from outside, or the producing
        // array is empty). Sizing the tuple types from constructions alone misses it.
        crate::Value::Eval(crate::Code::TplGet(_, index)) => index + 1,
        _ => 0,
    }
}

/// Collect every closure that is reserved as a recursive shell anywhere in `region` (and its
/// nested blocks). These are the only closures whose `envr` fields are back-patched, so they
/// are the only ones whose wasm struct fields must stay mutable.
fn collect_cyclic_clsrs(region: &crate::Region, out: &mut BTreeSet<crate::ClsrName>) {
    for (_, clsr) in &region.preallocs {
        out.insert(clsr.clone());
    }

    for (_, block) in &region.blocks {
        collect_cyclic_clsrs(&block.region, out);
    }
}

fn max_region_tpl_arity(region: &crate::Region) -> usize {
    // Preallocs are closure shells only, so they contribute no tuple arity; the arities all
    // come from tuple constructions and projections in `values` (and nested blocks).
    let values = region
        .values
        .iter()
        .map(|(_, value)| max_value_tpl_arity(value));

    let blocks = region
        .blocks
        .iter()
        .map(|(_, block)| max_region_tpl_arity(&block.region));

    values.chain(blocks).max().unwrap_or(0)
}

#[derive(Debug)]
pub(super) struct Table<'a> {
    special_field: FieldName,
    special_local: LocalName,
    special_label: LabelName,
    flt_type: TypeName,
    bin_rope_type: TypeName,
    lst_rope_type: TypeName,
    bytes_type: TypeName,
    elems_type: TypeName,
    bin_rope_leaf_type: TypeName,
    bin_rope_node_type: TypeName,
    bin_rope_view_type: TypeName,
    lst_rope_leaf_type: TypeName,
    lst_rope_node_type: TypeName,
    lst_rope_view_type: TypeName,
    cell_type: TypeName,
    io_exit: OnceCell<FuncName>,
    // The shared rope helpers, minted lazily like `io_exit`: the first call
    // site recorded during emission names the function, and the module
    // emitter then adds exactly the recorded set after the program's own
    // functions (see `emit_rope_funcs`).
    bytes_force: OnceCell<FuncName>,
    bits_force: OnceCell<FuncName>,
    lst_force: OnceCell<FuncName>,
    lst_bin_force: OnceCell<FuncName>,
    bytes_embed: OnceCell<FuncName>,
    lst_embed: OnceCell<FuncName>,
    lst_bin_embed: OnceCell<FuncName>,
    bytes_slice: OnceCell<FuncName>,
    bits_slice: OnceCell<FuncName>,
    lst_slice: OnceCell<FuncName>,
    bytes_read: OnceCell<FuncName>,
    bits_read: OnceCell<FuncName>,
    lst_read: OnceCell<FuncName>,
    bytes_eql: OnceCell<FuncName>,
    bits_eql: OnceCell<FuncName>,
    lst_map: OnceCell<FuncName>,
    // The foreign functions the emitted code calls, keyed by the minted
    // internal name (see `host_func`). Same lazy used-tracking as the
    // `io_exit` cell: the first call-site reference during emission records
    // the function's row, and `emit_sys_imports` then declares exactly the
    // recorded set (in minted-name order — wasmtime links by name, so
    // import order is cosmetic).
    host_funcs: RefCell<BTreeMap<String, Arc<ForeignFunction>>>,
    tpl_types: BTreeMap<usize, TypeName>,
    envr_types: BTreeMap<usize, TypeName>,
    clsr_types: BTreeMap<usize, TypeName>,
    func_types: BTreeMap<usize, TypeName>,
    consts: HashMap<&'a crate::ValueName, GlobalName>,
    clsrs: HashMap<&'a crate::ClsrName, ClsrData<'a>>,
    funcs: HashMap<&'a crate::FuncName, FuncData<'a>>,
    // Closures that are ever prealloc'd as a recursive shell — their `envr` fields are
    // back-patched (`struct.set`), so those fields must stay mutable. Every other aggregate
    // field is immutable. `cyclic_clsr_arities` carries the same fact at arity granularity,
    // for the shared `envr/N` special field (which must agree across all its subtypes).
    cyclic_clsrs: BTreeSet<crate::ClsrName>,
    cyclic_clsr_arities: BTreeSet<usize>,
}

impl<'a> Table<'a> {
    pub(super) fn new(module: &'a crate::Module) -> Self {
        let mut cyclic_clsrs = BTreeSet::new();
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
            .collect::<HashMap<crate::ClsrName, usize>>();

        let cyclic_clsr_arities = cyclic_clsrs
            .iter()
            .filter_map(|name| arities.get(name).copied())
            .collect::<BTreeSet<usize>>();

        Self {
            cyclic_clsrs,
            cyclic_clsr_arities,
            special_field: FieldName::from("!"),
            special_local: LocalName::from("!"),
            special_label: LabelName::from("!"),
            flt_type: TypeName::from("flt"),
            bin_rope_type: TypeName::from("rope/bin"),
            lst_rope_type: TypeName::from("rope/lst"),
            bytes_type: TypeName::from("bytes"),
            elems_type: TypeName::from("elems"),
            bin_rope_leaf_type: TypeName::from("rope/bin/leaf"),
            bin_rope_node_type: TypeName::from("rope/bin/node"),
            bin_rope_view_type: TypeName::from("rope/bin/view"),
            lst_rope_leaf_type: TypeName::from("rope/lst/leaf"),
            lst_rope_node_type: TypeName::from("rope/lst/node"),
            lst_rope_view_type: TypeName::from("rope/lst/view"),
            cell_type: TypeName::from("cell"),
            io_exit: OnceCell::new(),
            bytes_force: OnceCell::new(),
            bits_force: OnceCell::new(),
            lst_force: OnceCell::new(),
            lst_bin_force: OnceCell::new(),
            bytes_embed: OnceCell::new(),
            lst_embed: OnceCell::new(),
            lst_bin_embed: OnceCell::new(),
            bytes_slice: OnceCell::new(),
            bits_slice: OnceCell::new(),
            lst_slice: OnceCell::new(),
            bytes_read: OnceCell::new(),
            bits_read: OnceCell::new(),
            lst_read: OnceCell::new(),
            bytes_eql: OnceCell::new(),
            bits_eql: OnceCell::new(),
            lst_map: OnceCell::new(),
            host_funcs: RefCell::new(BTreeMap::new()),
            tpl_types: {
                let max = module
                    .consts()
                    .iter()
                    .map(|(_, data)| max_tpl_arity(data))
                    .chain(
                        module
                            .clsrs()
                            .iter()
                            .map(|(_, clsr)| max_region_tpl_arity(&clsr.region)),
                    )
                    .chain(
                        module
                            .funcs()
                            .iter()
                            .map(|(_, func)| max_region_tpl_arity(&func.region)),
                    )
                    .max()
                    .unwrap_or(0);

                (0..=max)
                    .map(|arity| (arity, TypeName::from(format!("tpl/{}", arity))))
                    .collect()
            },
            envr_types: module
                .clsr_arities()
                .into_iter()
                .map(|arity| (arity, TypeName::from(format!("envr/{}", arity))))
                .collect(),
            clsr_types: module
                .clsr_arities()
                .into_iter()
                .map(|arity| (arity, TypeName::from(format!("clsr/{}", arity))))
                .collect(),
            func_types: module
                .funcs()
                .iter()
                .map(|(_, func)| func.params.len())
                .map(|arity| (arity, TypeName::from(format!("func/{}", arity))))
                .collect(),
            consts: module
                .consts()
                .iter()
                .map(|(const_name, _)| (const_name, GlobalName::from(format!("${}", const_name))))
                .collect(),
            clsrs: module
                .clsrs()
                .iter()
                .map(|(clsr_name, clsr)| (clsr_name, ClsrData::new(clsr_name, clsr)))
                .collect(),
            funcs: module
                .funcs()
                .iter()
                .map(|(func_name, func)| (func_name, FuncData::new(func_name, func)))
                .collect(),
        }
    }

    pub(super) fn special_field(&self) -> FieldName {
        self.special_field.clone()
    }

    pub(super) fn special_local(&self) -> LocalName {
        self.special_local.clone()
    }

    pub(super) fn special_label(&self) -> LabelName {
        self.special_label.clone()
    }

    pub(super) fn top_type(is_nullable: bool) -> ValType {
        ValType::Ref(RefType {
            is_nullable,
            heap_type: HeapType::Abstract(AbsHeapType::Any),
        })
    }

    pub(super) fn int_type(is_nullable: bool) -> RefType {
        RefType {
            is_nullable,
            heap_type: HeapType::Abstract(AbsHeapType::I31),
        }
    }

    pub(super) fn flt_type(&self) -> TypeName {
        self.flt_type.clone()
    }

    pub(super) fn bin_rope_type(&self) -> TypeName {
        self.bin_rope_type.clone()
    }

    pub(super) fn lst_rope_type(&self) -> TypeName {
        self.lst_rope_type.clone()
    }

    pub(super) fn bytes_type(&self) -> TypeName {
        self.bytes_type.clone()
    }

    pub(super) fn elems_type(&self) -> TypeName {
        self.elems_type.clone()
    }

    /// The shared packed `Bits`/`Bytes` rope's name bundle.
    pub(super) fn bin_rope(&self) -> RopeData {
        RopeData {
            base: self.bin_rope_type.clone(),
            leaf: self.bin_rope_leaf_type.clone(),
            node: self.bin_rope_node_type.clone(),
            view: self.bin_rope_view_type.clone(),
            payload: self.bytes_type.clone(),
            tag_field: FieldName::from("tag"),
            len_field: FieldName::from("len"),
            payload_field: FieldName::from("bytes"),
            left_field: FieldName::from("left"),
            right_field: FieldName::from("right"),
            cache_field: FieldName::from("cache"),
            base_field: FieldName::from("base"),
            offset_field: FieldName::from("offset"),
        }
    }

    /// The `Lst` rope's name bundle.
    pub(super) fn lst_rope(&self) -> RopeData {
        RopeData {
            base: self.lst_rope_type.clone(),
            leaf: self.lst_rope_leaf_type.clone(),
            node: self.lst_rope_node_type.clone(),
            view: self.lst_rope_view_type.clone(),
            payload: self.elems_type.clone(),
            tag_field: FieldName::from("tag"),
            len_field: FieldName::from("len"),
            payload_field: FieldName::from("elems"),
            left_field: FieldName::from("left"),
            right_field: FieldName::from("right"),
            cache_field: FieldName::from("cache"),
            base_field: FieldName::from("base"),
            offset_field: FieldName::from("offset"),
        }
    }

    pub(super) fn cell_type(&self) -> TypeName {
        self.cell_type.clone()
    }

    /// The internal binding name of a store-described host function. First
    /// use during emission records the function as live;
    /// [`host_funcs`](Self::host_funcs) hands the recorded set to
    /// `emit_sys_imports`.
    ///
    /// A row's identity is its `(namespace, name)` pair (see
    /// [`ForeignFunction`]), and its name is chosen outside the
    /// emitter, so the minted name embeds both components under the reserved
    /// `host/` family prefix — a foreign name can never collide with a
    /// runtime helper, another minted family, or a same-named row from
    /// another namespace. The embedding is injective because namespaces are
    /// compiler-chosen and never contain `/`.
    pub(super) fn host_func(&self, function: &Arc<ForeignFunction>) -> FuncName {
        let func_name = FuncName::from(format!("host/{}/{}", function.namespace, function.name));

        self.host_funcs
            .borrow_mut()
            .entry(func_name.as_string())
            .or_insert_with(|| Arc::clone(function));

        func_name
    }

    /// The foreign functions the emitted code referenced, in minted-name order.
    pub(super) fn host_funcs(&self) -> Vec<Arc<ForeignFunction>> {
        self.host_funcs.borrow().values().cloned().collect()
    }

    pub(super) fn io_exit_func(&self) -> &FuncName {
        self.io_exit.get_or_init(|| FuncName::from("io_exit"))
    }

    pub(super) fn io_exit_used(&self) -> bool {
        self.io_exit.get().is_some()
    }

    /// `$bytes/force (ref $rope/bin) -> (ref $bytes)`: flatten a `Bytes` rope to its
    /// payload, memoizing in the entry node. First use marks it for emission.
    pub(super) fn bytes_force_func(&self) -> FuncName {
        self.bytes_force
            .get_or_init(|| FuncName::from("bytes/force"))
            .clone()
    }

    pub(super) fn bytes_force_used(&self) -> bool {
        self.bytes_force.get().is_some()
    }

    /// `$bits/force (ref $rope/bin) -> (ref $bytes)`: flatten a bit-grain rope to
    /// its packed payload, memoizing in the entry node. First use marks it for
    /// emission.
    pub(super) fn bits_force_func(&self) -> FuncName {
        self.bits_force
            .get_or_init(|| FuncName::from("bits/force"))
            .clone()
    }

    pub(super) fn bits_force_used(&self) -> bool {
        self.bits_force.get().is_some()
    }

    /// `$lst/force (ref $rope/lst) -> (ref $elems)`: the `Lst` mirror of
    /// [`bytes_force_func`](Self::bytes_force_func).
    pub(super) fn lst_force_func(&self) -> FuncName {
        self.lst_force
            .get_or_init(|| FuncName::from("lst/force"))
            .clone()
    }

    pub(super) fn lst_force_used(&self) -> bool {
        self.lst_force.get().is_some()
    }

    /// `$lst/bin/force (ref $rope/lst) -> (ref $elems)`: force an `Lst(Bin)` /
    /// `Lst(Io)` host argument *deeply* — the outer rope to a fresh payload
    /// whose every element is itself forced to `$bytes`, the element shape
    /// the host lifts.
    pub(super) fn lst_bin_force_func(&self) -> FuncName {
        self.lst_bin_force
            .get_or_init(|| FuncName::from("lst/bin/force"))
            .clone()
    }

    pub(super) fn lst_bin_force_used(&self) -> bool {
        self.lst_bin_force.get().is_some()
    }

    /// `$bytes/embed (ref $bytes) -> (ref $rope/bin)`: embed a host-built flat
    /// payload into a fresh leaf on re-entry.
    pub(super) fn bytes_embed_func(&self) -> FuncName {
        self.bytes_embed
            .get_or_init(|| FuncName::from("bytes/embed"))
            .clone()
    }

    pub(super) fn bytes_embed_used(&self) -> bool {
        self.bytes_embed.get().is_some()
    }

    /// `$lst/embed (ref $elems) -> (ref $rope/lst)`: the `Lst` mirror of
    /// [`bytes_embed_func`](Self::bytes_embed_func), for scalar-element results.
    pub(super) fn lst_embed_func(&self) -> FuncName {
        self.lst_embed
            .get_or_init(|| FuncName::from("lst/embed"))
            .clone()
    }

    pub(super) fn lst_embed_used(&self) -> bool {
        self.lst_embed.get().is_some()
    }

    /// `$lst/bin/embed (ref $elems) -> (ref $rope/lst)`: embed an `Lst(Bin)`
    /// host result *deeply* — each raw `$bytes` element into a leaf (in place;
    /// the host-built array is fresh), then the outer array.
    pub(super) fn lst_bin_embed_func(&self) -> FuncName {
        self.lst_bin_embed
            .get_or_init(|| FuncName::from("lst/bin/embed"))
            .clone()
    }

    pub(super) fn lst_bin_embed_used(&self) -> bool {
        self.lst_bin_embed.get().is_some()
    }

    /// `$bytes/slice (ref $rope/bin, i32, i32) -> (ref $rope/bin)`: the `Bytes`
    /// O(1) view constructor — bounds-check, answer the empty leaf or the whole
    /// rope on the trivial windows, collapse a view-of-view, and force an uncached node
    /// base so every `view` it builds reads through in O(1).
    pub(super) fn bytes_slice_func(&self) -> FuncName {
        self.bytes_slice
            .get_or_init(|| FuncName::from("bytes/slice"))
            .clone()
    }

    pub(super) fn bytes_slice_used(&self) -> bool {
        self.bytes_slice.get().is_some()
    }

    pub(super) fn bits_slice_func(&self) -> FuncName {
        self.bits_slice
            .get_or_init(|| FuncName::from("bits/slice"))
            .clone()
    }

    pub(super) fn bits_slice_used(&self) -> bool {
        self.bits_slice.get().is_some()
    }

    /// `$lst/slice (ref $rope/lst, i32, i32) -> (ref $rope/lst)`: the `Lst` mirror of
    /// [`bytes_slice_func`](Self::bytes_slice_func).
    pub(super) fn lst_slice_func(&self) -> FuncName {
        self.lst_slice
            .get_or_init(|| FuncName::from("lst/slice"))
            .clone()
    }

    pub(super) fn lst_slice_used(&self) -> bool {
        self.lst_slice.get().is_some()
    }

    /// `$bytes/read (ref $rope/bin, i32) -> i32`: one byte read — straight off
    /// a leaf payload, through a `view`'s window without forcing, and via
    /// `$bytes/force` (memoized) on a node.
    pub(super) fn bytes_read_func(&self) -> FuncName {
        self.bytes_read
            .get_or_init(|| FuncName::from("bytes/read"))
            .clone()
    }

    pub(super) fn bytes_read_used(&self) -> bool {
        self.bytes_read.get().is_some()
    }

    pub(super) fn bits_read_func(&self) -> FuncName {
        self.bits_read
            .get_or_init(|| FuncName::from("bits/read"))
            .clone()
    }

    pub(super) fn bits_read_used(&self) -> bool {
        self.bits_read.get().is_some()
    }

    /// `$lst/read (ref $rope/lst, i32) -> anyref`: the `Lst` mirror of
    /// [`bytes_read_func`](Self::bytes_read_func).
    pub(super) fn lst_read_func(&self) -> FuncName {
        self.lst_read
            .get_or_init(|| FuncName::from("lst/read"))
            .clone()
    }

    pub(super) fn lst_read_used(&self) -> bool {
        self.lst_read.get().is_some()
    }

    /// `$bytes/eql (ref $rope/bin, ref $rope/bin) -> i32`: whole-value byte
    /// equality — unequal rope lengths answer without forcing, equal lengths
    /// force both payloads once and compare bytewise.
    pub(super) fn bytes_eql_func(&self) -> FuncName {
        self.bytes_eql
            .get_or_init(|| FuncName::from("bytes/eql"))
            .clone()
    }

    pub(super) fn bytes_eql_used(&self) -> bool {
        self.bytes_eql.get().is_some()
    }

    pub(super) fn bits_eql_func(&self) -> FuncName {
        self.bits_eql
            .get_or_init(|| FuncName::from("bits/eql"))
            .clone()
    }

    pub(super) fn bits_eql_used(&self) -> bool {
        self.bits_eql.get().is_some()
    }

    /// `$lst/map (ref $rope/lst, ref $envr/1) -> (ref $rope/lst)`: apply a unary
    /// closure to every element of the forced payload, filling a fresh leaf.
    pub(super) fn lst_map_func(&self) -> FuncName {
        self.lst_map
            .get_or_init(|| FuncName::from("lst/map"))
            .clone()
    }

    pub(super) fn lst_map_used(&self) -> bool {
        self.lst_map.get().is_some()
    }

    pub(super) fn tpl_types(&self) -> impl Iterator<Item = (usize, TypeName)> {
        self.tpl_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub(super) fn find_tpl_type(&self, arity: usize) -> TypeName {
        self.tpl_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks tuple type for arity `{}`", arity))
            .clone()
    }

    pub(super) fn tpl_field(index: usize) -> FieldName {
        FieldName::from(index.to_string())
    }

    /// Whether this closure is ever reserved as a recursive shell, i.e. its `envr` payload
    /// fields are back-patched and so must be declared mutable.
    pub(super) fn is_cyclic_clsr(&self, name: &crate::ClsrName) -> bool {
        self.cyclic_clsrs.contains(name)
    }

    /// Whether *any* closure of this arity is cyclic. The shared `envr/N` special field (and
    /// thus every `envr/<clsr>` of that arity, by subtyping invariance) must be mutable iff so.
    pub(super) fn arity_has_cyclic_clsr(&self, arity: usize) -> bool {
        self.cyclic_clsr_arities.contains(&arity)
    }

    fn field_mutability(is_mutable: bool) -> Mutability {
        if is_mutable {
            Mutability::Var
        } else {
            Mutability::Const
        }
    }

    pub(super) fn tpl_field_mutability() -> Mutability {
        // Tuples are never cyclic (rejected in `into_cont`), so they are never back-patched.
        Self::field_mutability(false)
    }

    pub(super) fn envr_special_mutability(&self, arity: usize) -> Mutability {
        Self::field_mutability(self.arity_has_cyclic_clsr(arity))
    }

    pub(super) fn envr_payload_mutability(&self, name: &crate::ClsrName) -> Mutability {
        Self::field_mutability(self.is_cyclic_clsr(name))
    }

    pub(super) fn envr_types(&self) -> impl Iterator<Item = (usize, TypeName)> {
        self.envr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub(super) fn find_envr_type(&self, arity: usize) -> TypeName {
        self.envr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks environment type for arity `{}`", arity))
            .clone()
    }

    pub(super) fn clsr_types(&self) -> impl Iterator<Item = (usize, TypeName)> {
        self.clsr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub(super) fn find_clsr_type(&self, arity: usize) -> TypeName {
        self.clsr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks closure type for arity `{}`", arity))
            .clone()
    }

    pub(super) fn func_types(&self) -> impl Iterator<Item = (usize, TypeName)> {
        self.func_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub(super) fn find_func_type(&self, arity: usize) -> TypeName {
        self.func_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks function type for arity `{}`", arity))
            .clone()
    }

    pub(super) fn find_const(&self, const_name: &crate::ValueName) -> GlobalName {
        self.consts
            .get(const_name)
            .unwrap_or_else(|| panic!("`Table` lacks const `{}`", const_name))
            .clone()
    }

    pub(super) fn clsrs(&self) -> impl Iterator<Item = &ClsrData<'a>> {
        self.clsrs.values()
    }

    pub(super) fn find_clsr(&self, clsr_name: &crate::ClsrName) -> &ClsrData<'a> {
        self.clsrs
            .get(clsr_name)
            .unwrap_or_else(|| panic!("`Table` lacks closure `{}`", clsr_name))
    }

    pub(super) fn find_func(&self, func_name: &crate::FuncName) -> &FuncData<'a> {
        self.funcs
            .get(func_name)
            .unwrap_or_else(|| panic!("`Table` lacks func `{}`", func_name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rope_names_match_the_wasm_vocabulary() {
        let module = crate::Module::new();
        let table = Table::new(&module);
        let bin = table.bin_rope();
        let lst = table.lst_rope();

        assert_eq!(bin.base.as_str(), "rope/bin");
        assert_eq!(bin.leaf.as_str(), "rope/bin/leaf");
        assert_eq!(bin.node.as_str(), "rope/bin/node");
        assert_eq!(bin.view.as_str(), "rope/bin/view");
        assert_eq!(bin.payload.as_str(), "bytes");

        assert_eq!(lst.base.as_str(), "rope/lst");
        assert_eq!(lst.leaf.as_str(), "rope/lst/leaf");
        assert_eq!(lst.node.as_str(), "rope/lst/node");
        assert_eq!(lst.view.as_str(), "rope/lst/view");
        assert_eq!(lst.payload.as_str(), "elems");

        assert_eq!(table.bits_force_func().as_str(), "bits/force");
        assert_eq!(table.bits_slice_func().as_str(), "bits/slice");
        assert_eq!(table.bits_read_func().as_str(), "bits/read");
        assert_eq!(table.bits_eql_func().as_str(), "bits/eql");

        assert_eq!(table.bytes_force_func().as_str(), "bytes/force");
        assert_eq!(table.bytes_embed_func().as_str(), "bytes/embed");
        assert_eq!(table.bytes_slice_func().as_str(), "bytes/slice");
        assert_eq!(table.bytes_read_func().as_str(), "bytes/read");
        assert_eq!(table.bytes_eql_func().as_str(), "bytes/eql");

        assert_eq!(table.lst_force_func().as_str(), "lst/force");
        assert_eq!(table.lst_embed_func().as_str(), "lst/embed");
        assert_eq!(table.lst_slice_func().as_str(), "lst/slice");
        assert_eq!(table.lst_read_func().as_str(), "lst/read");
        assert_eq!(table.lst_map_func().as_str(), "lst/map");

        assert_eq!(table.lst_bin_force_func().as_str(), "lst/bin/force");
        assert_eq!(table.lst_bin_embed_func().as_str(), "lst/bin/embed");
    }
}
