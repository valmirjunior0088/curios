use super::{
    DataName, Expr, FuncName, GlobalName, GlobalType, LocalName, RecType, SubType, TypeName,
    ValType,
};

#[derive(Debug)]
pub enum Import {
    Func {
        func_name: FuncName,
        type_name: TypeName,
    },
    Global {
        global_name: GlobalName,
        global_type: GlobalType,
    },
}

impl Import {
    pub fn func_name(&self) -> Option<&FuncName> {
        match self {
            Import::Func { func_name, .. } => Some(func_name),
            _ => None,
        }
    }

    pub fn global_name(&self) -> Option<&GlobalName> {
        match self {
            Import::Global { global_name, .. } => Some(global_name),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct DataSegment {
    pub bytes: Vec<u8>,
}

#[derive(Debug)]
pub struct Func {
    pub type_name: TypeName,
    pub params: Vec<LocalName>,
    pub locals: Vec<(LocalName, ValType)>,
    pub expr: Expr,
}

impl Func {
    pub fn local_names(&self) -> impl Iterator<Item = &LocalName> {
        self.params
            .iter()
            .chain(self.locals.iter().map(|(local_name, _)| local_name))
    }
}

#[derive(Debug)]
pub struct Global {
    pub global_type: GlobalType,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Export {
    Func(FuncName),
    Global(GlobalName),
}

#[derive(Debug)]
pub struct Module {
    name: String,
    types: Vec<RecType>,
    imports: Vec<(String, String, Import)>,
    funcs: Vec<(FuncName, Func)>,
    globals: Vec<(GlobalName, Global)>,
    datas: Vec<(DataName, DataSegment)>,
    exports: Vec<(String, Export)>,
    start: Option<FuncName>,
    // Functions made eligible for `ref.func` by a declarative element segment,
    // without exporting them. `ref.func $f` validates only if `$f` is declared.
    elems: Vec<FuncName>,
}

impl Module {
    pub fn new<N>(name: N) -> Self
    where
        N: Into<String>,
    {
        Self {
            name: name.into(),
            types: Default::default(),
            imports: Default::default(),
            funcs: Default::default(),
            globals: Default::default(),
            datas: Default::default(),
            exports: Default::default(),
            start: None,
            elems: Default::default(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn types(&self) -> &[RecType] {
        &self.types
    }

    pub fn get_type(&self, target_name: &TypeName) -> Option<&SubType> {
        self.types
            .iter()
            .flat_map(|rec_type| &rec_type.sub_types)
            .find_map(|(type_name, sub_type)| (target_name == type_name).then_some(sub_type))
    }

    pub fn add_types(&mut self, rec_type: RecType) {
        self.types.push(rec_type);
    }

    pub fn add_type(&mut self, type_name: TypeName, sub_type: SubType) {
        self.types.push(RecType::from([(type_name, sub_type)]));
    }

    pub fn imports(&self) -> &[(String, String, Import)] {
        &self.imports
    }

    pub fn add_import<M, N>(&mut self, module_name: M, name: N, import: Import)
    where
        M: Into<String>,
        N: Into<String>,
    {
        self.imports.push((module_name.into(), name.into(), import));
    }

    pub fn funcs(&self) -> &[(FuncName, Func)] {
        &self.funcs
    }

    pub fn funcs_mut(&mut self) -> &mut Vec<(FuncName, Func)> {
        &mut self.funcs
    }

    pub fn add_func(&mut self, func_name: FuncName, func: Func) {
        self.funcs.push((func_name, func));
    }

    pub fn globals(&self) -> &[(GlobalName, Global)] {
        &self.globals
    }

    pub fn add_global(&mut self, global_name: GlobalName, global: Global) {
        self.globals.push((global_name, global));
    }

    pub fn datas(&self) -> &[(DataName, DataSegment)] {
        &self.datas
    }

    pub fn add_data(&mut self, data_name: DataName, data_segment: DataSegment) {
        self.datas.push((data_name, data_segment));
    }

    pub fn exports(&self) -> &[(String, Export)] {
        &self.exports
    }

    pub fn add_export<N>(&mut self, name: N, export: Export)
    where
        N: Into<String>,
    {
        self.exports.push((name.into(), export));
    }

    pub fn start(&self) -> Option<&FuncName> {
        self.start.as_ref()
    }

    pub fn set_start(&mut self, func_name: FuncName) {
        self.start = Some(func_name);
    }

    pub fn elems(&self) -> &[FuncName] {
        &self.elems
    }

    /// Declare a function for `ref.func` use via the declarative element segment.
    pub fn declare_func(&mut self, func_name: FuncName) {
        self.elems.push(func_name);
    }
}
