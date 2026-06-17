use {
    crate::{cont, wasm},
    std::{
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, HashMap},
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
    name: &'a cont::ClsrName,
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
            name: clsr_name,
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

    pub fn name(&self) -> &'a cont::ClsrName {
        self.name
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

/// Collect every closure that is reserved as a recursive shell anywhere in `region` (and its
/// nested blocks). These are the only closures whose `envr` fields are back-patched, so they
/// are the only ones whose wasm struct fields must stay mutable.
fn collect_cyclic_clsrs(region: &cont::Region, out: &mut BTreeSet<cont::ClsrName>) {
    for (_, clsr) in &region.preallocs {
        out.insert(clsr.clone());
    }

    for (_, block) in &region.blocks {
        collect_cyclic_clsrs(&block.region, out);
    }
}

fn max_region_tpl_arity(region: &cont::Region) -> usize {
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
    io_resolve: OnceCell<wasm::FuncName>,
    io_socket: OnceCell<wasm::FuncName>,
    io_bind: OnceCell<wasm::FuncName>,
    io_connect: OnceCell<wasm::FuncName>,
    io_listen: OnceCell<wasm::FuncName>,
    io_accept: OnceCell<wasm::FuncName>,
    io_set_nonblocking: OnceCell<wasm::FuncName>,
    io_set_recv_timeout: OnceCell<wasm::FuncName>,
    io_set_send_timeout: OnceCell<wasm::FuncName>,
    io_set_reuseaddr: OnceCell<wasm::FuncName>,
    io_close: OnceCell<wasm::FuncName>,
    io_clock_wall: OnceCell<wasm::FuncName>,
    io_clock_mono: OnceCell<wasm::FuncName>,
    io_random: OnceCell<wasm::FuncName>,
    io_args: OnceCell<wasm::FuncName>,
    io_env: OnceCell<wasm::FuncName>,
    io_exit: OnceCell<wasm::FuncName>,
    tpl_types: BTreeMap<usize, wasm::TypeName>,
    envr_types: BTreeMap<usize, wasm::TypeName>,
    clsr_types: BTreeMap<usize, wasm::TypeName>,
    func_types: BTreeMap<usize, wasm::TypeName>,
    consts: HashMap<&'a cont::ValueName, wasm::GlobalName>,
    clsrs: HashMap<&'a cont::ClsrName, ClsrData<'a>>,
    funcs: HashMap<&'a cont::FuncName, FuncData<'a>>,
    // Closures that are ever prealloc'd as a recursive shell — their `envr` fields are
    // back-patched (`struct.set`), so those fields must stay mutable. Every other aggregate
    // field is immutable. `cyclic_clsr_arities` carries the same fact at arity granularity,
    // for the shared `envr/N` special field (which must agree across all its subtypes).
    cyclic_clsrs: BTreeSet<cont::ClsrName>,
    cyclic_clsr_arities: BTreeSet<usize>,
}

impl<'a> Table<'a> {
    pub fn new(module: &'a cont::Module) -> Self {
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
            .collect::<HashMap<cont::ClsrName, usize>>();

        let cyclic_clsr_arities = cyclic_clsrs
            .iter()
            .filter_map(|name| arities.get(name).copied())
            .collect::<BTreeSet<usize>>();

        Self {
            cyclic_clsrs,
            cyclic_clsr_arities,
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
            io_resolve: OnceCell::new(),
            io_socket: OnceCell::new(),
            io_bind: OnceCell::new(),
            io_connect: OnceCell::new(),
            io_listen: OnceCell::new(),
            io_accept: OnceCell::new(),
            io_set_nonblocking: OnceCell::new(),
            io_set_recv_timeout: OnceCell::new(),
            io_set_send_timeout: OnceCell::new(),
            io_set_reuseaddr: OnceCell::new(),
            io_close: OnceCell::new(),
            io_clock_wall: OnceCell::new(),
            io_clock_mono: OnceCell::new(),
            io_random: OnceCell::new(),
            io_args: OnceCell::new(),
            io_env: OnceCell::new(),
            io_exit: OnceCell::new(),
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

    pub fn io_connect_func(&self) -> &wasm::FuncName {
        self.io_connect
            .get_or_init(|| wasm::FuncName::from("io_connect"))
    }

    pub fn io_connect_used(&self) -> bool {
        self.io_connect.get().is_some()
    }

    pub fn io_listen_func(&self) -> &wasm::FuncName {
        self.io_listen
            .get_or_init(|| wasm::FuncName::from("io_listen"))
    }

    pub fn io_listen_used(&self) -> bool {
        self.io_listen.get().is_some()
    }

    pub fn io_accept_func(&self) -> &wasm::FuncName {
        self.io_accept
            .get_or_init(|| wasm::FuncName::from("io_accept"))
    }

    pub fn io_accept_used(&self) -> bool {
        self.io_accept.get().is_some()
    }

    pub fn io_resolve_func(&self) -> &wasm::FuncName {
        self.io_resolve
            .get_or_init(|| wasm::FuncName::from("io_resolve"))
    }

    pub fn io_resolve_used(&self) -> bool {
        self.io_resolve.get().is_some()
    }

    pub fn io_socket_func(&self) -> &wasm::FuncName {
        self.io_socket
            .get_or_init(|| wasm::FuncName::from("io_socket"))
    }

    pub fn io_socket_used(&self) -> bool {
        self.io_socket.get().is_some()
    }

    pub fn io_bind_func(&self) -> &wasm::FuncName {
        self.io_bind.get_or_init(|| wasm::FuncName::from("io_bind"))
    }

    pub fn io_bind_used(&self) -> bool {
        self.io_bind.get().is_some()
    }

    pub fn io_set_nonblocking_func(&self) -> &wasm::FuncName {
        self.io_set_nonblocking
            .get_or_init(|| wasm::FuncName::from("io_set_nonblocking"))
    }

    pub fn io_set_nonblocking_used(&self) -> bool {
        self.io_set_nonblocking.get().is_some()
    }

    pub fn io_set_recv_timeout_func(&self) -> &wasm::FuncName {
        self.io_set_recv_timeout
            .get_or_init(|| wasm::FuncName::from("io_set_recv_timeout"))
    }

    pub fn io_set_recv_timeout_used(&self) -> bool {
        self.io_set_recv_timeout.get().is_some()
    }

    pub fn io_set_send_timeout_func(&self) -> &wasm::FuncName {
        self.io_set_send_timeout
            .get_or_init(|| wasm::FuncName::from("io_set_send_timeout"))
    }

    pub fn io_set_send_timeout_used(&self) -> bool {
        self.io_set_send_timeout.get().is_some()
    }

    pub fn io_set_reuseaddr_func(&self) -> &wasm::FuncName {
        self.io_set_reuseaddr
            .get_or_init(|| wasm::FuncName::from("io_set_reuseaddr"))
    }

    pub fn io_set_reuseaddr_used(&self) -> bool {
        self.io_set_reuseaddr.get().is_some()
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

    pub fn io_args_func(&self) -> &wasm::FuncName {
        self.io_args.get_or_init(|| wasm::FuncName::from("io_args"))
    }

    pub fn io_args_used(&self) -> bool {
        self.io_args.get().is_some()
    }

    pub fn io_env_func(&self) -> &wasm::FuncName {
        self.io_env.get_or_init(|| wasm::FuncName::from("io_env"))
    }

    pub fn io_env_used(&self) -> bool {
        self.io_env.get().is_some()
    }

    pub fn io_exit_func(&self) -> &wasm::FuncName {
        self.io_exit.get_or_init(|| wasm::FuncName::from("io_exit"))
    }

    pub fn io_exit_used(&self) -> bool {
        self.io_exit.get().is_some()
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

    /// Whether this closure is ever reserved as a recursive shell, i.e. its `envr` payload
    /// fields are back-patched and so must be declared mutable.
    pub fn is_cyclic_clsr(&self, name: &cont::ClsrName) -> bool {
        self.cyclic_clsrs.contains(name)
    }

    /// Whether *any* closure of this arity is cyclic. The shared `envr/N` special field (and
    /// thus every `envr/<clsr>` of that arity, by subtyping invariance) must be mutable iff so.
    pub fn arity_has_cyclic_clsr(&self, arity: usize) -> bool {
        self.cyclic_clsr_arities.contains(&arity)
    }

    fn field_mutability(&self, is_mutable: bool) -> wasm::Mutability {
        if is_mutable {
            wasm::Mutability::Var
        } else {
            wasm::Mutability::Const
        }
    }

    pub fn tpl_field_mutability(&self) -> wasm::Mutability {
        // Tuples are never cyclic (rejected in `to_cont`), so they are never back-patched.
        self.field_mutability(false)
    }

    pub fn arr_field_mutability(&self) -> wasm::Mutability {
        // Arrays stay mutable regardless of cyclicity: their primitives (append/concat/slice)
        // build results with `array.new_default` + per-element `array.set`, so the element
        // field must be writable. Only tuples and closures gain immutable fields here.
        self.field_mutability(true)
    }

    pub fn envr_special_mutability(&self, arity: usize) -> wasm::Mutability {
        self.field_mutability(self.arity_has_cyclic_clsr(arity))
    }

    pub fn envr_payload_mutability(&self, name: &cont::ClsrName) -> wasm::Mutability {
        self.field_mutability(self.is_cyclic_clsr(name))
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
