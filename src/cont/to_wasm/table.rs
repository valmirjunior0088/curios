use {
    crate::{cont, wasm},
    std::{
        cell::OnceCell,
        collections::{BTreeMap, HashMap},
    },
};

#[derive(Debug, Clone)]
pub struct FieldData {
    type_name: wasm::TypeName,
    field_name: wasm::FieldName,
}

impl FieldData {
    pub fn new(type_name: wasm::TypeName, field_name: wasm::FieldName) -> Self {
        Self {
            type_name,
            field_name,
        }
    }

    pub fn type_name(&self) -> wasm::TypeName {
        self.type_name.clone()
    }

    pub fn field_name(&self) -> wasm::FieldName {
        self.field_name.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ClsrData<'a> {
    func_name: wasm::FuncName,
    clsr_type: wasm::TypeName,
    envr_type: wasm::TypeName,
    fields: Vec<(&'a cont::ValueName, wasm::FieldName)>,
    params: HashMap<&'a cont::ValueName, wasm::LocalName>,
    resume: &'a cont::BlockName,
}

impl<'a> ClsrData<'a> {
    pub fn new(clsr_name: &'a cont::ClsrName, clsr: &'a cont::Clsr) -> Self {
        Self {
            func_name: wasm::FuncName::from(format!("clsr/{}", clsr_name)),
            clsr_type: wasm::TypeName::from(format!("clsr/{}", clsr_name)),
            envr_type: wasm::TypeName::from(format!("envr/{}", clsr_name)),
            fields: clsr
                .fields
                .iter()
                .map(|field| {
                    (
                        &field.name,
                        wasm::FieldName::from(format!("${}", field.name)),
                    )
                })
                .collect(),
            params: clsr
                .params
                .iter()
                .map(|param| {
                    (
                        &param.name,
                        wasm::LocalName::from(format!("${}", param.name)),
                    )
                })
                .collect(),
            resume: &clsr.resume,
        }
    }

    pub fn func_name(&self) -> wasm::FuncName {
        self.func_name.clone()
    }

    pub fn clsr_type(&self) -> wasm::TypeName {
        self.clsr_type.clone()
    }

    pub fn envr_type(&self) -> wasm::TypeName {
        self.envr_type.clone()
    }

    pub fn fields(&self) -> impl Iterator<Item = wasm::FieldName> {
        self.fields.iter().map(|(_, field_name)| field_name.clone())
    }

    pub fn find_field(&self, value_name: &cont::ValueName) -> Option<FieldData> {
        self.fields
            .iter()
            .find_map(|(field_name, mapped_field_name)| {
                (value_name == *field_name).then_some(mapped_field_name)
            })
            .cloned()
            .map(|field_name| FieldData::new(self.envr_type(), field_name))
    }

    pub fn params(&self) -> HashMap<&'a cont::ValueName, wasm::LocalName> {
        self.params.clone()
    }

    pub fn find_param(&self, value_name: &cont::ValueName) -> Option<wasm::LocalName> {
        self.params.get(value_name).cloned()
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn is_resume(&self, block_name: &cont::BlockName) -> bool {
        self.resume == block_name
    }
}

#[derive(Debug, Clone)]
pub struct FuncData<'a> {
    func_name: wasm::FuncName,
    params: HashMap<&'a cont::ValueName, wasm::LocalName>,
    resume: &'a cont::BlockName,
}

impl<'a> FuncData<'a> {
    pub fn new(func_name: &'a cont::FuncName, func: &'a cont::Func) -> Self {
        Self {
            func_name: wasm::FuncName::from(format!("func/{}", func_name)),
            params: func
                .params
                .iter()
                .map(|param| {
                    (
                        &param.name,
                        wasm::LocalName::from(format!("${}", param.name)),
                    )
                })
                .collect(),
            resume: &func.resume,
        }
    }

    pub fn func_name(&self) -> wasm::FuncName {
        self.func_name.clone()
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn params(&self) -> HashMap<&'a cont::ValueName, wasm::LocalName> {
        self.params.clone()
    }

    pub fn find_param(&self, value_name: &cont::ValueName) -> Option<wasm::LocalName> {
        self.params.get(value_name).cloned()
    }

    pub fn is_resume(&self, block_name: &cont::BlockName) -> bool {
        self.resume == block_name
    }
}

fn max_tpl_arity(data: &cont::Data) -> usize {
    match data {
        cont::Data::Tpl(fields) => fields.len(),
        _ => 0,
    }
}

fn max_value_tpl_arity(value: &cont::Value) -> usize {
    match value {
        cont::Value::Pure(data) => max_tpl_arity(data),
        // Projecting field `index` reads through a tuple type of arity at least
        // `index + 1`, even when no tuple of that arity is ever *built* in the module
        // (e.g. the projected tuple only ever arrives from outside, or the producing
        // array is empty). Sizing the tuple types from constructions alone misses it.
        cont::Value::Eval(cont::Code::TplGet(_, index)) => index + 1,
        _ => 0,
    }
}

fn max_region_tpl_arity(region: &cont::Region) -> usize {
    let preallocs = region.preallocs.iter().map(|(_, prealloc)| match prealloc {
        cont::Prealloc::Tpl(arity) => *arity,
        _ => 0,
    });

    let values = region
        .values
        .iter()
        .map(|(_, value)| max_value_tpl_arity(value));

    let blocks = region
        .blocks
        .iter()
        .map(|(_, block)| max_region_tpl_arity(&block.region));

    preallocs.chain(values).chain(blocks).max().unwrap_or(0)
}

#[derive(Debug)]
pub struct Table<'a> {
    special_field: wasm::FieldName,
    special_local: wasm::LocalName,
    special_label: wasm::LabelName,
    flt_type: wasm::TypeName,
    bin_type: wasm::TypeName,
    arr_type: wasm::TypeName,
    nat_to_str: OnceCell<wasm::FuncName>,
    int_to_str: OnceCell<wasm::FuncName>,
    flt_to_str: OnceCell<wasm::FuncName>,
    flt_to_le_bin: OnceCell<wasm::FuncName>,
    io_read: OnceCell<wasm::FuncName>,
    io_write: OnceCell<wasm::FuncName>,
    io_open: OnceCell<wasm::FuncName>,
    io_close: OnceCell<wasm::FuncName>,
    io_clock_wall: OnceCell<wasm::FuncName>,
    io_clock_mono: OnceCell<wasm::FuncName>,
    io_random: OnceCell<wasm::FuncName>,
    tpl_types: BTreeMap<usize, wasm::TypeName>,
    envr_types: BTreeMap<usize, wasm::TypeName>,
    clsr_types: BTreeMap<usize, wasm::TypeName>,
    func_types: BTreeMap<usize, wasm::TypeName>,
    consts: HashMap<&'a cont::ValueName, wasm::GlobalName>,
    clsrs: HashMap<&'a cont::ClsrName, ClsrData<'a>>,
    funcs: HashMap<&'a cont::FuncName, FuncData<'a>>,
}

impl<'a> Table<'a> {
    pub fn new(module: &'a cont::Module) -> Self {
        Self {
            special_field: wasm::FieldName::from("!"),
            special_local: wasm::LocalName::from("!"),
            special_label: wasm::LabelName::from("!"),
            flt_type: wasm::TypeName::from("flt"),
            bin_type: wasm::TypeName::from("bin"),
            arr_type: wasm::TypeName::from("arr"),
            nat_to_str: OnceCell::new(),
            int_to_str: OnceCell::new(),
            flt_to_str: OnceCell::new(),
            flt_to_le_bin: OnceCell::new(),
            io_read: OnceCell::new(),
            io_write: OnceCell::new(),
            io_open: OnceCell::new(),
            io_close: OnceCell::new(),
            io_clock_wall: OnceCell::new(),
            io_clock_mono: OnceCell::new(),
            io_random: OnceCell::new(),
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
                    .map(|arity| (arity, wasm::TypeName::from(format!("tpl/{}", arity))))
                    .collect()
            },
            envr_types: module
                .clsrs()
                .iter()
                .map(|(_, clsr)| clsr.params.len())
                .map(|arity| (arity, wasm::TypeName::from(format!("envr/{}", arity))))
                .collect(),
            clsr_types: module
                .clsrs()
                .iter()
                .map(|(_, clsr)| clsr.params.len())
                .map(|arity| (arity, wasm::TypeName::from(format!("clsr/{}", arity))))
                .collect(),
            func_types: module
                .funcs()
                .iter()
                .map(|(_, func)| func.params.len())
                .map(|arity| (arity, wasm::TypeName::from(format!("func/{}", arity))))
                .collect(),
            consts: module
                .consts()
                .iter()
                .map(|(const_name, _)| {
                    (
                        const_name,
                        wasm::GlobalName::from(format!("${}", const_name)),
                    )
                })
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

    pub fn special_field(&self) -> wasm::FieldName {
        self.special_field.clone()
    }

    pub fn special_local(&self) -> wasm::LocalName {
        self.special_local.clone()
    }

    pub fn special_label(&self) -> wasm::LabelName {
        self.special_label.clone()
    }

    pub fn top_type(&self, is_nullable: bool) -> wasm::ValType {
        wasm::ValType::Ref(wasm::RefType {
            is_nullable,
            heap_type: wasm::HeapType::Abstract(wasm::AbsHeapType::Any),
        })
    }

    pub fn int_type(&self, is_nullable: bool) -> wasm::RefType {
        wasm::RefType {
            is_nullable,
            heap_type: wasm::HeapType::Abstract(wasm::AbsHeapType::I31),
        }
    }

    pub fn flt_type(&self) -> wasm::TypeName {
        self.flt_type.clone()
    }

    pub fn bin_type(&self) -> wasm::TypeName {
        self.bin_type.clone()
    }

    pub fn arr_type(&self) -> wasm::TypeName {
        self.arr_type.clone()
    }

    pub fn nat_to_str_func(&self) -> &wasm::FuncName {
        self.nat_to_str
            .get_or_init(|| wasm::FuncName::from("nat_to_str"))
    }

    pub fn int_to_str_func(&self) -> &wasm::FuncName {
        self.int_to_str
            .get_or_init(|| wasm::FuncName::from("int_to_str"))
    }

    pub fn flt_to_str_func(&self) -> &wasm::FuncName {
        self.flt_to_str
            .get_or_init(|| wasm::FuncName::from("flt_to_str"))
    }

    pub fn flt_to_le_bin_func(&self) -> &wasm::FuncName {
        self.flt_to_le_bin
            .get_or_init(|| wasm::FuncName::from("flt_to_le_bin"))
    }

    pub fn nat_to_str_used(&self) -> bool {
        self.nat_to_str.get().is_some()
    }

    pub fn int_to_str_used(&self) -> bool {
        self.int_to_str.get().is_some()
    }

    pub fn flt_to_str_used(&self) -> bool {
        self.flt_to_str.get().is_some()
    }

    pub fn flt_to_le_bin_used(&self) -> bool {
        self.flt_to_le_bin.get().is_some()
    }

    pub fn io_read_func(&self) -> &wasm::FuncName {
        self.io_read.get_or_init(|| wasm::FuncName::from("io_read"))
    }

    pub fn io_read_used(&self) -> bool {
        self.io_read.get().is_some()
    }

    pub fn io_write_func(&self) -> &wasm::FuncName {
        self.io_write
            .get_or_init(|| wasm::FuncName::from("io_write"))
    }

    pub fn io_write_used(&self) -> bool {
        self.io_write.get().is_some()
    }

    pub fn io_open_func(&self) -> &wasm::FuncName {
        self.io_open.get_or_init(|| wasm::FuncName::from("io_open"))
    }

    pub fn io_open_used(&self) -> bool {
        self.io_open.get().is_some()
    }

    pub fn io_close_func(&self) -> &wasm::FuncName {
        self.io_close
            .get_or_init(|| wasm::FuncName::from("io_close"))
    }

    pub fn io_close_used(&self) -> bool {
        self.io_close.get().is_some()
    }

    pub fn io_clock_wall_func(&self) -> &wasm::FuncName {
        self.io_clock_wall
            .get_or_init(|| wasm::FuncName::from("io_clock_wall"))
    }

    pub fn io_clock_wall_used(&self) -> bool {
        self.io_clock_wall.get().is_some()
    }

    pub fn io_clock_mono_func(&self) -> &wasm::FuncName {
        self.io_clock_mono
            .get_or_init(|| wasm::FuncName::from("io_clock_mono"))
    }

    pub fn io_clock_mono_used(&self) -> bool {
        self.io_clock_mono.get().is_some()
    }

    pub fn io_random_func(&self) -> &wasm::FuncName {
        self.io_random
            .get_or_init(|| wasm::FuncName::from("io_random"))
    }

    pub fn io_random_used(&self) -> bool {
        self.io_random.get().is_some()
    }

    pub fn tpl_types(&self) -> impl Iterator<Item = (usize, wasm::TypeName)> {
        self.tpl_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub fn find_tpl_type(&self, arity: usize) -> wasm::TypeName {
        self.tpl_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks tuple type for arity `{}`", arity))
            .clone()
    }

    pub fn tpl_field(&self, index: usize) -> wasm::FieldName {
        wasm::FieldName::from(index.to_string())
    }

    pub fn envr_types(&self) -> impl Iterator<Item = (usize, wasm::TypeName)> {
        self.envr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub fn find_envr_type(&self, arity: usize) -> wasm::TypeName {
        self.envr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks environment type for arity `{}`", arity))
            .clone()
    }

    pub fn clsr_types(&self) -> impl Iterator<Item = (usize, wasm::TypeName)> {
        self.clsr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub fn find_clsr_type(&self, arity: usize) -> wasm::TypeName {
        self.clsr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks closure type for arity `{}`", arity))
            .clone()
    }

    pub fn func_types(&self) -> impl Iterator<Item = (usize, wasm::TypeName)> {
        self.func_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub fn find_func_type(&self, arity: usize) -> wasm::TypeName {
        self.func_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks function type for arity `{}`", arity))
            .clone()
    }

    pub fn find_const(&self, const_name: &cont::ValueName) -> wasm::GlobalName {
        self.consts
            .get(const_name)
            .unwrap_or_else(|| panic!("`Table` lacks const `{}`", const_name))
            .clone()
    }

    pub fn clsrs(&self) -> impl Iterator<Item = &ClsrData<'a>> {
        self.clsrs.values()
    }

    pub fn find_clsr(&self, clsr_name: &cont::ClsrName) -> &ClsrData<'a> {
        self.clsrs
            .get(clsr_name)
            .unwrap_or_else(|| panic!("`Table` lacks closure `{}`", clsr_name))
    }

    pub fn find_func(&self, func_name: &cont::FuncName) -> &FuncData<'a> {
        self.funcs
            .get(func_name)
            .unwrap_or_else(|| panic!("`Table` lacks func `{}`", func_name))
    }
}
