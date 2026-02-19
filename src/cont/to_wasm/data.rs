use {
    crate::{cont, wasm},
    std::collections::{BTreeMap, HashMap},
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
    envr_type: wasm::TypeName,
    fields: Vec<(&'a cont::ValueName, wasm::FieldName)>,
    params: HashMap<&'a cont::ValueName, wasm::LocalName>,
    resume: &'a cont::BlockName,
}

impl<'a> ClsrData<'a> {
    pub fn new(clsr_name: &'a cont::ClsrName, clsr: &'a cont::Clsr) -> Self {
        Self {
            func_name: wasm::FuncName::from(format!("clsr/{}", clsr_name.string)),
            envr_type: wasm::TypeName::from(format!("envr/{}", clsr_name.string)),
            fields: clsr
                .fields
                .iter()
                .map(|field_name| {
                    (
                        field_name,
                        wasm::FieldName::from(format!("${}", field_name.string)),
                    )
                })
                .collect(),
            params: clsr
                .params
                .iter()
                .map(|param_name| {
                    (
                        param_name,
                        wasm::LocalName::from(format!("${}", param_name.string)),
                    )
                })
                .collect(),
            resume: &clsr.resume,
        }
    }

    pub fn func_name(&self) -> wasm::FuncName {
        self.func_name.clone()
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
            func_name: wasm::FuncName::from(format!("func/{}", func_name.string)),
            params: func
                .params
                .iter()
                .map(|param_name| {
                    (
                        param_name,
                        wasm::LocalName::from(format!("${}", param_name.string)),
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

pub struct ModuleData<'a> {
    special_field: wasm::FieldName,
    special_local: wasm::LocalName,
    special_label: wasm::LabelName,
    obj_type: wasm::TypeName,
    int_type: wasm::TypeName,
    flt_type: wasm::TypeName,
    envr_type: wasm::TypeName,
    clsr_types: BTreeMap<usize, wasm::TypeName>,
    func_types: BTreeMap<usize, wasm::TypeName>,
    consts: HashMap<&'a cont::ValueName, wasm::GlobalName>,
    clsrs: HashMap<&'a cont::ClsrName, ClsrData<'a>>,
    funcs: HashMap<&'a cont::FuncName, FuncData<'a>>,
}

impl<'a> ModuleData<'a> {
    pub fn new(module: &'a cont::Module) -> Self {
        Self {
            special_field: wasm::FieldName::from("!"),
            special_local: wasm::LocalName::from("!"),
            special_label: wasm::LabelName::from("!"),
            obj_type: wasm::TypeName::from("obj"),
            int_type: wasm::TypeName::from("int"),
            flt_type: wasm::TypeName::from("flt"),
            envr_type: wasm::TypeName::from("envr"),
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
                        wasm::GlobalName::from(format!("${}", const_name.string)),
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

    pub fn obj_type(&self) -> wasm::TypeName {
        self.obj_type.clone()
    }

    pub fn obj_val_type(&self, is_nullable: bool) -> wasm::ValType {
        wasm::ValType::Ref(wasm::RefType {
            is_nullable,
            heap_type: wasm::HeapType::Concrete(self.obj_type()),
        })
    }

    pub fn int_type(&self) -> wasm::TypeName {
        self.int_type.clone()
    }

    pub fn flt_type(&self) -> wasm::TypeName {
        self.flt_type.clone()
    }

    pub fn envr_type(&self) -> wasm::TypeName {
        self.envr_type.clone()
    }

    pub fn envr_val_type(&self) -> wasm::ValType {
        wasm::ValType::Ref(wasm::RefType {
            is_nullable: false,
            heap_type: wasm::HeapType::Concrete(self.envr_type()),
        })
    }

    pub fn clsr_types(&self) -> impl Iterator<Item = (usize, wasm::TypeName)> {
        self.clsr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub fn find_clsr_type(&self, arity: usize) -> wasm::TypeName {
        self.clsr_types
            .get(&arity)
            .expect(&format!(
                "`ModuleData` lacks closure type for arity `{}`",
                arity
            ))
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
            .expect(&format!(
                "`ModuleData` lacks function type for arity `{}`",
                arity
            ))
            .clone()
    }

    pub fn find_const(&self, const_name: &cont::ValueName) -> wasm::GlobalName {
        self.consts
            .get(const_name)
            .expect(&format!("`ModuleData` lacks const `{}`", &const_name.string))
            .clone()
    }

    pub fn clsrs(&self) -> impl Iterator<Item = &ClsrData<'a>> {
        self.clsrs.values()
    }

    pub fn find_clsr(&self, clsr_name: &cont::ClsrName) -> &ClsrData<'a> {
        self.clsrs
            .get(clsr_name)
            .expect(&format!("`ModuleData` lacks closure `{}`", &clsr_name.string))
    }

    pub fn find_func(&self, func_name: &cont::FuncName) -> &FuncData<'a> {
        self.funcs
            .get(func_name)
            .expect(&format!("`ModuleData` lacks func `{}`", &func_name.string))
    }
}
