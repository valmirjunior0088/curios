use {
    crate::{
        DataName, ElemName, FieldName, FuncName, GlobalName, LocalName, MemName, Module, SubType,
        TableName, TypeName,
    },
    std::collections::HashMap,
};

/// Every index space the binary format is defined over, derived once from the module's declaration order with imports leading — the only place a numeric index exists. A name absent from it is a dangling cross-reference, which every resolver reports by panicking with the name rather than encoding a wrong number — and a name declared twice is the same failure from the other side, refused at construction rather than silently resolving every reference to whichever declaration came second. A function's local index space is likewise the format's — its type's inputs followed by its locals — so a param list of any other length is refused here too: numbering from the list instead would put every local of that function in the wrong slot while the module still validates.
#[derive(Debug, Clone)]
pub(super) struct Indices<'a> {
    types: HashMap<&'a TypeName, usize>,
    fields: HashMap<(&'a TypeName, &'a FieldName), usize>,
    funcs: HashMap<&'a FuncName, usize>,
    locals: HashMap<(&'a FuncName, &'a LocalName), usize>,
    tables: HashMap<&'a TableName, usize>,
    mems: HashMap<&'a MemName, usize>,
    globals: HashMap<&'a GlobalName, usize>,
    elems: HashMap<&'a ElemName, usize>,
    datas: HashMap<&'a DataName, usize>,
}

impl<'a> Indices<'a> {
    pub(super) fn new(module: &'a Module) -> Self {
        let mut types = HashMap::new();
        let mut fields = HashMap::new();

        for (index, (type_name, sub_type)) in module
            .types()
            .iter()
            .flat_map(|rec_type| rec_type.sub_types.iter())
            .enumerate()
        {
            assert!(
                types.insert(type_name, index).is_none(),
                "type `{type_name}` is declared twice"
            );

            if let Some(struct_type) = sub_type.struct_type() {
                for (index, (field_name, _)) in struct_type.fields.iter().enumerate() {
                    assert!(
                        fields.insert((type_name, field_name), index).is_none(),
                        "type `{type_name}` declares the field `{field_name}` twice"
                    );
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
            assert!(
                funcs.insert(func_name, index).is_none(),
                "func `{func_name}` is declared twice"
            );
        }

        for (index, (func_name, func)) in (funcs.len()..).zip(module.funcs()) {
            assert!(
                funcs.insert(func_name, index).is_none(),
                "func `{func_name}` is declared twice"
            );

            let inputs = module
                .get_type(&func.type_name)
                .and_then(SubType::func_type)
                .unwrap_or_else(|| {
                    panic!(
                        "func `{func_name}` is declared at `{}`, which is not a declared function type",
                        func.type_name
                    )
                })
                .inputs()
                .len();

            assert!(
                func.params.len() == inputs,
                "func `{func_name}` names {} params for a type of {inputs} inputs",
                func.params.len()
            );

            for (index, local_name) in func.local_names().enumerate() {
                assert!(
                    locals.insert((func_name, local_name), index).is_none(),
                    "func `{func_name}` declares the local `{local_name}` twice"
                );
            }
        }

        let mut tables = HashMap::new();

        for (index, table_name) in module
            .imports()
            .iter()
            .flat_map(|(_, _, import)| import.table_name())
            .enumerate()
        {
            assert!(
                tables.insert(table_name, index).is_none(),
                "table `{table_name}` is declared twice"
            );
        }

        for (index, (table_name, _)) in (tables.len()..).zip(module.tables()) {
            assert!(
                tables.insert(table_name, index).is_none(),
                "table `{table_name}` is declared twice"
            );
        }

        let mut mems = HashMap::new();

        for (index, mem_name) in module
            .imports()
            .iter()
            .flat_map(|(_, _, import)| import.mem_name())
            .enumerate()
        {
            assert!(
                mems.insert(mem_name, index).is_none(),
                "memory `{mem_name}` is declared twice"
            );
        }

        for (index, (mem_name, _)) in (mems.len()..).zip(module.mems()) {
            assert!(
                mems.insert(mem_name, index).is_none(),
                "memory `{mem_name}` is declared twice"
            );
        }

        let mut globals = HashMap::new();

        for (index, global_name) in module
            .imports()
            .iter()
            .flat_map(|(_, _, import)| import.global_name())
            .enumerate()
        {
            assert!(
                globals.insert(global_name, index).is_none(),
                "global `{global_name}` is declared twice"
            );
        }

        for (index, (global_name, _)) in (globals.len()..).zip(module.globals()) {
            assert!(
                globals.insert(global_name, index).is_none(),
                "global `{global_name}` is declared twice"
            );
        }

        let mut elems = HashMap::new();

        for (index, (elem_name, _)) in module.elems().iter().enumerate() {
            assert!(
                elems.insert(elem_name, index).is_none(),
                "elem `{elem_name}` is declared twice"
            );
        }

        let mut datas = HashMap::new();

        for (index, (data_name, _)) in module.datas().iter().enumerate() {
            assert!(
                datas.insert(data_name, index).is_none(),
                "data `{data_name}` is declared twice"
            );
        }

        Self {
            types,
            fields,
            funcs,
            locals,
            tables,
            mems,
            globals,
            elems,
            datas,
        }
    }

    pub(super) fn resolve_type(&self, name: &'a TypeName) -> usize {
        self.types
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("`Indices` lacks type `{}`", name))
    }

    pub(super) fn resolve_field(&self, parent_name: &'a TypeName, name: &'a FieldName) -> usize {
        self.fields
            .get(&(parent_name, name))
            .copied()
            .unwrap_or_else(|| panic!("`Indices` lacks field `{}` of type `{}`", name, parent_name))
    }

    pub(super) fn resolve_func(&self, name: &'a FuncName) -> usize {
        self.funcs
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("`Indices` lacks func `{}`", name))
    }

    pub(super) fn resolve_local(&self, parent_name: &'a FuncName, name: &'a LocalName) -> usize {
        self.locals
            .get(&(parent_name, name))
            .copied()
            .unwrap_or_else(|| panic!("`Indices` lacks local `{}` of func `{}`", name, parent_name))
    }

    pub(super) fn resolve_table(&self, name: &'a TableName) -> usize {
        self.tables
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("`Indices` lacks table `{}`", name))
    }

    pub(super) fn resolve_mem(&self, name: &'a MemName) -> usize {
        self.mems
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("`Indices` lacks memory `{}`", name))
    }

    pub(super) fn resolve_global(&self, name: &'a GlobalName) -> usize {
        self.globals
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("`Indices` lacks global `{}`", name))
    }

    pub(super) fn resolve_elem(&self, name: &'a ElemName) -> usize {
        self.elems
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("`Indices` lacks elem `{}`", name))
    }

    pub(super) fn resolve_data(&self, name: &'a DataName) -> usize {
        self.datas
            .get(name)
            .copied()
            .unwrap_or_else(|| panic!("`Indices` lacks data `{}`", name))
    }
}
