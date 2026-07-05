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

/// The name bundle for one rope carrier (`Bin` or `Lst`): the base struct the
/// emitter casts carrier refs to, its `leaf`/`node`/`sub` subtypes, the flat
/// payload array, and every field name — one handle to thread through the op
/// emitters so `Bin` and `Lst` share their lowering code.
#[derive(Debug, Clone)]
pub(super) struct RopeData {
    pub base: TypeName,
    pub leaf: TypeName,
    pub node: TypeName,
    pub sub: TypeName,
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
            // `curios_abi::MAIN_EXPORT` (`func/main`): the entry is always
            // `main`, and the export reuses the function's emitted name.
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
        // (e.g. the projected tuple only ever lstives from outside, or the producing
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
    bin_type: TypeName,
    lst_type: TypeName,
    bytes_type: TypeName,
    elems_type: TypeName,
    bin_leaf_type: TypeName,
    bin_node_type: TypeName,
    bin_sub_type: TypeName,
    lst_leaf_type: TypeName,
    lst_node_type: TypeName,
    lst_sub_type: TypeName,
    cell_type: TypeName,
    io_exit: OnceCell<FuncName>,
    // The shared rope helpers, minted lazily like `io_exit`: the first call
    // site recorded during emission names the function, and the module
    // emitter then adds exactly the recorded set after the program's own
    // functions (see `emit_rope_funcs`).
    bin_force: OnceCell<FuncName>,
    lst_force: OnceCell<FuncName>,
    lst_bin_force: OnceCell<FuncName>,
    bin_wrap: OnceCell<FuncName>,
    lst_wrap: OnceCell<FuncName>,
    lst_bin_wrap: OnceCell<FuncName>,
    bin_slice: OnceCell<FuncName>,
    lst_slice: OnceCell<FuncName>,
    bin_read: OnceCell<FuncName>,
    lst_read: OnceCell<FuncName>,
    bin_eql: OnceCell<FuncName>,
    lst_map: OnceCell<FuncName>,
    // The foreign functions the emitted code calls, keyed by import name.
    // Same lazy used-tracking as the `io_exit` cell: the first call-site
    // reference during emission records the function's row, and
    // `emit_sys_imports` then declares exactly the recorded set (in name
    // order — wasmtime links by name, so import order is cosmetic).
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
            bin_type: TypeName::from("bin"),
            lst_type: TypeName::from("lst"),
            bytes_type: TypeName::from("bytes"),
            elems_type: TypeName::from("elems"),
            bin_leaf_type: TypeName::from("bin/leaf"),
            bin_node_type: TypeName::from("bin/node"),
            bin_sub_type: TypeName::from("bin/sub"),
            lst_leaf_type: TypeName::from("lst/leaf"),
            lst_node_type: TypeName::from("lst/node"),
            lst_sub_type: TypeName::from("lst/sub"),
            cell_type: TypeName::from("cell"),
            io_exit: OnceCell::new(),
            bin_force: OnceCell::new(),
            lst_force: OnceCell::new(),
            lst_bin_force: OnceCell::new(),
            bin_wrap: OnceCell::new(),
            lst_wrap: OnceCell::new(),
            lst_bin_wrap: OnceCell::new(),
            bin_slice: OnceCell::new(),
            lst_slice: OnceCell::new(),
            bin_read: OnceCell::new(),
            lst_read: OnceCell::new(),
            bin_eql: OnceCell::new(),
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

    pub(super) fn top_type(&self, is_nullable: bool) -> ValType {
        ValType::Ref(RefType {
            is_nullable,
            heap_type: HeapType::Abstract(AbsHeapType::Any),
        })
    }

    pub(super) fn int_type(&self, is_nullable: bool) -> RefType {
        RefType {
            is_nullable,
            heap_type: HeapType::Abstract(AbsHeapType::I31),
        }
    }

    pub(super) fn flt_type(&self) -> TypeName {
        self.flt_type.clone()
    }

    pub(super) fn bin_type(&self) -> TypeName {
        self.bin_type.clone()
    }

    pub(super) fn lst_type(&self) -> TypeName {
        self.lst_type.clone()
    }

    pub(super) fn bytes_type(&self) -> TypeName {
        self.bytes_type.clone()
    }

    pub(super) fn elems_type(&self) -> TypeName {
        self.elems_type.clone()
    }

    /// The `Bin` rope's name bundle.
    pub(super) fn bin_rope(&self) -> RopeData {
        RopeData {
            base: self.bin_type.clone(),
            leaf: self.bin_leaf_type.clone(),
            node: self.bin_node_type.clone(),
            sub: self.bin_sub_type.clone(),
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
            base: self.lst_type.clone(),
            leaf: self.lst_leaf_type.clone(),
            node: self.lst_node_type.clone(),
            sub: self.lst_sub_type.clone(),
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

    /// The import name of a store-described host function. First use during
    /// emission records the function as live; [`host_funcs`](Self::host_funcs)
    /// hands the recorded set to `emit_sys_imports`.
    pub(super) fn host_func(&self, function: &Arc<ForeignFunction>) -> FuncName {
        self.host_funcs
            .borrow_mut()
            .entry(function.name.clone())
            .or_insert_with(|| Arc::clone(function));

        FuncName::from(function.name.as_str())
    }

    /// The foreign functions the emitted code referenced, in import-name order.
    pub(super) fn host_funcs(&self) -> Vec<Arc<ForeignFunction>> {
        self.host_funcs.borrow().values().cloned().collect()
    }

    pub(super) fn io_exit_func(&self) -> &FuncName {
        self.io_exit.get_or_init(|| FuncName::from("io_exit"))
    }

    pub(super) fn io_exit_used(&self) -> bool {
        self.io_exit.get().is_some()
    }

    /// `$bin/force (ref $bin) -> (ref $bytes)`: flatten a `Bin` rope to its
    /// payload, memoizing in the entry node. First use marks it for emission.
    pub(super) fn bin_force_func(&self) -> FuncName {
        self.bin_force
            .get_or_init(|| FuncName::from("bin/force"))
            .clone()
    }

    pub(super) fn bin_force_used(&self) -> bool {
        self.bin_force.get().is_some()
    }

    /// `$lst/force (ref $lst) -> (ref $elems)`: the `Lst` mirror of
    /// [`bin_force_func`](Self::bin_force_func).
    pub(super) fn lst_force_func(&self) -> FuncName {
        self.lst_force
            .get_or_init(|| FuncName::from("lst/force"))
            .clone()
    }

    pub(super) fn lst_force_used(&self) -> bool {
        self.lst_force.get().is_some()
    }

    /// `$lst/bin/force (ref $lst) -> (ref $elems)`: force an `Lst(Bin)` /
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

    /// `$bin/wrap (ref $bytes) -> (ref $bin)`: wrap a host-built flat payload
    /// into a fresh leaf on re-entry.
    pub(super) fn bin_wrap_func(&self) -> FuncName {
        self.bin_wrap
            .get_or_init(|| FuncName::from("bin/wrap"))
            .clone()
    }

    pub(super) fn bin_wrap_used(&self) -> bool {
        self.bin_wrap.get().is_some()
    }

    /// `$lst/wrap (ref $elems) -> (ref $lst)`: the `Lst` mirror of
    /// [`bin_wrap_func`](Self::bin_wrap_func), for scalar-element results.
    pub(super) fn lst_wrap_func(&self) -> FuncName {
        self.lst_wrap
            .get_or_init(|| FuncName::from("lst/wrap"))
            .clone()
    }

    pub(super) fn lst_wrap_used(&self) -> bool {
        self.lst_wrap.get().is_some()
    }

    /// `$lst/bin/wrap (ref $elems) -> (ref $lst)`: wrap an `Lst(Bin)` host
    /// result *deeply* — each raw `$bytes` element into a leaf (in place; the
    /// host-built array is fresh), then the outer array.
    pub(super) fn lst_bin_wrap_func(&self) -> FuncName {
        self.lst_bin_wrap
            .get_or_init(|| FuncName::from("lst/bin/wrap"))
            .clone()
    }

    pub(super) fn lst_bin_wrap_used(&self) -> bool {
        self.lst_bin_wrap.get().is_some()
    }

    /// `$bin/slice (ref $bin, i32, i32) -> (ref $bin)`: the O(1) window
    /// constructor — bounds-check, answer the empty leaf or the whole rope on
    /// the trivial windows, collapse a sub-of-sub, and force an uncached node
    /// base so every `sub` it builds reads through in O(1).
    pub(super) fn bin_slice_func(&self) -> FuncName {
        self.bin_slice
            .get_or_init(|| FuncName::from("bin/slice"))
            .clone()
    }

    pub(super) fn bin_slice_used(&self) -> bool {
        self.bin_slice.get().is_some()
    }

    /// `$lst/slice (ref $lst, i32, i32) -> (ref $lst)`: the `Lst` mirror of
    /// [`bin_slice_func`](Self::bin_slice_func).
    pub(super) fn lst_slice_func(&self) -> FuncName {
        self.lst_slice
            .get_or_init(|| FuncName::from("lst/slice"))
            .clone()
    }

    pub(super) fn lst_slice_used(&self) -> bool {
        self.lst_slice.get().is_some()
    }

    /// `$bin/read (ref $bin, i32) -> i32`: one element read — straight off a
    /// leaf payload, through a `sub`'s window without forcing, and via
    /// `$bin/force` (memoized) on a node.
    pub(super) fn bin_read_func(&self) -> FuncName {
        self.bin_read
            .get_or_init(|| FuncName::from("bin/read"))
            .clone()
    }

    pub(super) fn bin_read_used(&self) -> bool {
        self.bin_read.get().is_some()
    }

    /// `$lst/read (ref $lst, i32) -> anyref`: the `Lst` mirror of
    /// [`bin_read_func`](Self::bin_read_func).
    pub(super) fn lst_read_func(&self) -> FuncName {
        self.lst_read
            .get_or_init(|| FuncName::from("lst/read"))
            .clone()
    }

    pub(super) fn lst_read_used(&self) -> bool {
        self.lst_read.get().is_some()
    }

    /// `$bin/eql (ref $bin, ref $bin) -> i32`: whole-value byte equality —
    /// unequal rope lengths answer without forcing, equal lengths force both
    /// payloads once and compare bytewise.
    pub(super) fn bin_eql_func(&self) -> FuncName {
        self.bin_eql
            .get_or_init(|| FuncName::from("bin/eql"))
            .clone()
    }

    pub(super) fn bin_eql_used(&self) -> bool {
        self.bin_eql.get().is_some()
    }

    /// `$lst/map (ref $lst, ref $envr/1) -> (ref $lst)`: apply a unary
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

    pub(super) fn tpl_field(&self, index: usize) -> FieldName {
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

    fn field_mutability(&self, is_mutable: bool) -> Mutability {
        if is_mutable {
            Mutability::Var
        } else {
            Mutability::Const
        }
    }

    pub(super) fn tpl_field_mutability(&self) -> Mutability {
        // Tuples are never cyclic (rejected in `to_cont`), so they are never back-patched.
        self.field_mutability(false)
    }

    pub(super) fn envr_special_mutability(&self, arity: usize) -> Mutability {
        self.field_mutability(self.arity_has_cyclic_clsr(arity))
    }

    pub(super) fn envr_payload_mutability(&self, name: &crate::ClsrName) -> Mutability {
        self.field_mutability(self.is_cyclic_clsr(name))
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
