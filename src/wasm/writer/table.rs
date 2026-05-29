use {
    crate::wasm::{
        DataName, FieldName, FuncName, GlobalName, LocalName, Module, TypeName,
    },
    std::collections::HashMap,
};

#[derive(Debug, Clone)]
pub struct Table<'a> {
    types: HashMap<&'a TypeName, usize>,
    fields: HashMap<(&'a TypeName, &'a FieldName), usize>,
    funcs: HashMap<&'a FuncName, usize>,
    locals: HashMap<(&'a FuncName, &'a LocalName), usize>,
    globals: HashMap<&'a GlobalName, usize>,
    datas: HashMap<&'a DataName, usize>,
}

impl<'a> Table<'a> {
    pub fn new(module: &'a Module) -> Self {
        let mut types = HashMap::new();
        let mut fields = HashMap::new();

        for (index, (type_name, sub_type)) in module
            .types()
            .iter()
            .flat_map(|rec_type| rec_type.sub_types.iter())
            .enumerate()
        {
            types.insert(type_name, index);

            if let Some(struct_type) = sub_type.struct_type() {
                for (index, (field_name, _)) in struct_type.fields.iter().enumerate() {
                    fields.insert((type_name, field_name), index);
                }
            }
        }

        let mut funcs = HashMap::new();
        let mut locals = HashMap::new();

        for (index, func_name) in module
            .imports()
            .iter()
            .flat_map(|(_, _, import)| import.func_name())
            .enumerate()
        {
            funcs.insert(func_name, index);
        }

        for (index, (func_name, func)) in (funcs.len()..).zip(module.funcs()) {
            funcs.insert(func_name, index);

            for (index, local_name) in func.local_names().enumerate() {
                locals.insert((func_name, local_name), index);
            }
        }

        let mut globals = HashMap::new();

        for (index, global_name) in module
            .imports()
            .iter()
            .flat_map(|(_, _, import)| import.global_name())
            .enumerate()
        {
            globals.insert(global_name, index);
        }

        for (index, (global_name, _)) in (globals.len()..).zip(module.globals()) {
            globals.insert(global_name, index);
        }

        let mut datas = HashMap::new();

        for (index, (data_name, _)) in module.datas().iter().enumerate() {
            datas.insert(data_name, index);
        }

        Self {
            types,
            fields,
            funcs,
            locals,
            globals,
            datas,
        }
    }

    pub fn resolve_type(&self, name: &'a TypeName) -> usize {
        self.types
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("`Table` lacks type `{}`", name))
    }

    pub fn resolve_field(&self, parent_name: &'a TypeName, name: &'a FieldName) -> usize {
        self.fields
            .get(&(parent_name, name))
            .copied()
            .unwrap_or_else(|| panic!("`Table` lacks field `{}` of type `{}`", name, parent_name))
    }

    pub fn resolve_func(&self, name: &'a FuncName) -> usize {
        self.funcs
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("`Table` lacks func `{}`", name))
    }

    pub fn resolve_local(&self, parent_name: &'a FuncName, name: &'a LocalName) -> usize {
        self.locals
            .get(&(parent_name, name))
            .copied()
            .unwrap_or_else(|| panic!("`Table` lacks local `{}` of func `{}`", name, parent_name))
    }

    pub fn resolve_global(&self, name: &'a GlobalName) -> usize {
        self.globals
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("`Table` lacks global `{}`", name))
    }

    pub fn resolve_data(&self, name: &'a DataName) -> usize {
        self.datas
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("`Table` lacks data `{}`", name))
    }
}
