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
pub struct FieldData {
    type_name: TypeName,
    field_name: FieldName,
}

impl FieldData {
    pub fn new(type_name: TypeName, field_name: FieldName) -> Self {
        Self {
            type_name,
            field_name,
        }
    }

    pub fn type_name(&self) -> TypeName {
        self.type_name.clone()
    }

    pub fn field_name(&self) -> FieldName {
        self.field_name.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ClsrData<'a> {
    name: &'a crate::ClsrName,
    func_name: FuncName,
    clsr_type: TypeName,
    envr_type: TypeName,
    fields: Vec<(&'a crate::ValueName, FieldName)>,
    params: HashMap<&'a crate::ValueName, LocalName>,
    resume: &'a crate::BlockName,
}

impl<'a> ClsrData<'a> {
    pub fn new(clsr_name: &'a crate::ClsrName, clsr: &'a crate::Clsr) -> Self {
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

    pub fn name(&self) -> &'a crate::ClsrName {
        self.name
    }

    pub fn func_name(&self) -> FuncName {
        self.func_name.clone()
    }

    pub fn clsr_type(&self) -> TypeName {
        self.clsr_type.clone()
    }

    pub fn envr_type(&self) -> TypeName {
        self.envr_type.clone()
    }

    pub fn fields(&self) -> impl Iterator<Item = FieldName> {
        self.fields.iter().map(|(_, field_name)| field_name.clone())
    }

    pub fn find_field(&self, value_name: &crate::ValueName) -> Option<FieldData> {
        self.fields
            .iter()
            .find_map(|(field_name, mapped_field_name)| {
                (value_name == *field_name).then_some(mapped_field_name)
            })
            .cloned()
            .map(|field_name| FieldData::new(self.envr_type(), field_name))
    }

    pub fn params(&self) -> HashMap<&'a crate::ValueName, LocalName> {
        self.params.clone()
    }

    pub fn find_param(&self, value_name: &crate::ValueName) -> Option<LocalName> {
        self.params.get(value_name).cloned()
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn is_resume(&self, block_name: &crate::BlockName) -> bool {
        self.resume == block_name
    }
}

#[derive(Debug, Clone)]
pub struct FuncData<'a> {
    func_name: FuncName,
    params: HashMap<&'a crate::ValueName, LocalName>,
    resume: &'a crate::BlockName,
}

impl<'a> FuncData<'a> {
    pub fn new(func_name: &'a crate::FuncName, func: &'a crate::Func) -> Self {
        Self {
            func_name: FuncName::from(format!("func/{}", func_name)),
            params: func
                .params
                .iter()
                .map(|param| (&param.name, LocalName::from(format!("${}", param.name))))
                .collect(),
            resume: &func.resume,
        }
    }

    pub fn func_name(&self) -> FuncName {
        self.func_name.clone()
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn params(&self) -> HashMap<&'a crate::ValueName, LocalName> {
        self.params.clone()
    }

    pub fn find_param(&self, value_name: &crate::ValueName) -> Option<LocalName> {
        self.params.get(value_name).cloned()
    }

    pub fn is_resume(&self, block_name: &crate::BlockName) -> bool {
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
pub struct Table<'a> {
    special_field: FieldName,
    special_local: LocalName,
    special_label: LabelName,
    flt_type: TypeName,
    bin_type: TypeName,
    arr_type: TypeName,
    cell_type: TypeName,
    io_exit: OnceCell<FuncName>,
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
    pub fn new(module: &'a crate::Module) -> Self {
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
            arr_type: TypeName::from("arr"),
            cell_type: TypeName::from("cell"),
            io_exit: OnceCell::new(),
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

    pub fn special_field(&self) -> FieldName {
        self.special_field.clone()
    }

    pub fn special_local(&self) -> LocalName {
        self.special_local.clone()
    }

    pub fn special_label(&self) -> LabelName {
        self.special_label.clone()
    }

    pub fn top_type(&self, is_nullable: bool) -> ValType {
        ValType::Ref(RefType {
            is_nullable,
            heap_type: HeapType::Abstract(AbsHeapType::Any),
        })
    }

    pub fn int_type(&self, is_nullable: bool) -> RefType {
        RefType {
            is_nullable,
            heap_type: HeapType::Abstract(AbsHeapType::I31),
        }
    }

    pub fn flt_type(&self) -> TypeName {
        self.flt_type.clone()
    }

    pub fn bin_type(&self) -> TypeName {
        self.bin_type.clone()
    }

    pub fn arr_type(&self) -> TypeName {
        self.arr_type.clone()
    }

    pub fn cell_type(&self) -> TypeName {
        self.cell_type.clone()
    }

    /// The import name of a store-described host function. First use during
    /// emission records the function as live; [`host_funcs`](Self::host_funcs)
    /// hands the recorded set to `emit_sys_imports`.
    pub fn host_func(&self, function: &Arc<ForeignFunction>) -> FuncName {
        self.host_funcs
            .borrow_mut()
            .entry(function.name.clone())
            .or_insert_with(|| Arc::clone(function));

        FuncName::from(function.name.as_str())
    }

    /// The foreign functions the emitted code referenced, in import-name order.
    pub fn host_funcs(&self) -> Vec<Arc<ForeignFunction>> {
        self.host_funcs.borrow().values().cloned().collect()
    }

    pub fn io_exit_func(&self) -> &FuncName {
        self.io_exit.get_or_init(|| FuncName::from("io_exit"))
    }

    pub fn io_exit_used(&self) -> bool {
        self.io_exit.get().is_some()
    }

    pub fn tpl_types(&self) -> impl Iterator<Item = (usize, TypeName)> {
        self.tpl_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub fn find_tpl_type(&self, arity: usize) -> TypeName {
        self.tpl_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks tuple type for arity `{}`", arity))
            .clone()
    }

    pub fn tpl_field(&self, index: usize) -> FieldName {
        FieldName::from(index.to_string())
    }

    /// Whether this closure is ever reserved as a recursive shell, i.e. its `envr` payload
    /// fields are back-patched and so must be declared mutable.
    pub fn is_cyclic_clsr(&self, name: &crate::ClsrName) -> bool {
        self.cyclic_clsrs.contains(name)
    }

    /// Whether *any* closure of this arity is cyclic. The shared `envr/N` special field (and
    /// thus every `envr/<clsr>` of that arity, by subtyping invariance) must be mutable iff so.
    pub fn arity_has_cyclic_clsr(&self, arity: usize) -> bool {
        self.cyclic_clsr_arities.contains(&arity)
    }

    fn field_mutability(&self, is_mutable: bool) -> Mutability {
        if is_mutable {
            Mutability::Var
        } else {
            Mutability::Const
        }
    }

    pub fn tpl_field_mutability(&self) -> Mutability {
        // Tuples are never cyclic (rejected in `to_cont`), so they are never back-patched.
        self.field_mutability(false)
    }

    pub fn arr_field_mutability(&self) -> Mutability {
        // Arrays stay mutable regardless of cyclicity: their primitives (append/concat/slice)
        // build results with `array.new_default` + per-element `array.set`, so the element
        // field must be writable. Only tuples and closures gain immutable fields here.
        self.field_mutability(true)
    }

    pub fn envr_special_mutability(&self, arity: usize) -> Mutability {
        self.field_mutability(self.arity_has_cyclic_clsr(arity))
    }

    pub fn envr_payload_mutability(&self, name: &crate::ClsrName) -> Mutability {
        self.field_mutability(self.is_cyclic_clsr(name))
    }

    pub fn envr_types(&self) -> impl Iterator<Item = (usize, TypeName)> {
        self.envr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub fn find_envr_type(&self, arity: usize) -> TypeName {
        self.envr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks environment type for arity `{}`", arity))
            .clone()
    }

    pub fn clsr_types(&self) -> impl Iterator<Item = (usize, TypeName)> {
        self.clsr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub fn find_clsr_type(&self, arity: usize) -> TypeName {
        self.clsr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks closure type for arity `{}`", arity))
            .clone()
    }

    pub fn func_types(&self) -> impl Iterator<Item = (usize, TypeName)> {
        self.func_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub fn find_func_type(&self, arity: usize) -> TypeName {
        self.func_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks function type for arity `{}`", arity))
            .clone()
    }

    pub fn find_const(&self, const_name: &crate::ValueName) -> GlobalName {
        self.consts
            .get(const_name)
            .unwrap_or_else(|| panic!("`Table` lacks const `{}`", const_name))
            .clone()
    }

    pub fn clsrs(&self) -> impl Iterator<Item = &ClsrData<'a>> {
        self.clsrs.values()
    }

    pub fn find_clsr(&self, clsr_name: &crate::ClsrName) -> &ClsrData<'a> {
        self.clsrs
            .get(clsr_name)
            .unwrap_or_else(|| panic!("`Table` lacks closure `{}`", clsr_name))
    }

    pub fn find_func(&self, func_name: &crate::FuncName) -> &FuncData<'a> {
        self.funcs
            .get(func_name)
            .unwrap_or_else(|| panic!("`Table` lacks func `{}`", func_name))
    }
}
