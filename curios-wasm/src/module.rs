#[cfg(test)]
mod array_tests;
#[cfg(test)]
mod memory_tests;
#[cfg(test)]
mod table_tests;
#[cfg(test)]
mod test_support;

use {
    super::{
        DataName, ElemName, Expr, FuncName, GlobalName, GlobalType, LocalName, MemName, MemType,
        RecType, RefType, SubType, TableName, TableType, TypeName, ValType, print_module,
    },
    curios_print::{run_printer, run_printer_within},
    std::fmt,
};

/// An imported item: a function (typed by reference to a declared func type), a table, a memory, or a global. The name bound here is how the rest of the module refers to the item; the encoder gives imports the leading indices of their respective index spaces.
#[derive(Debug)]
pub enum Import {
    Func {
        func_name: FuncName,
        type_name: TypeName,
    },
    Table {
        table_name: TableName,
        table_type: TableType,
    },
    Memory {
        mem_name: MemName,
        mem_type: MemType,
    },
    Global {
        global_name: GlobalName,
        global_type: GlobalType,
    },
}

impl Import {
    pub(crate) fn func_name(&self) -> Option<&FuncName> {
        match self {
            Import::Func { func_name, .. } => Some(func_name),
            Import::Table { .. } | Import::Memory { .. } | Import::Global { .. } => None,
        }
    }

    pub(crate) fn table_name(&self) -> Option<&TableName> {
        match self {
            Import::Table { table_name, .. } => Some(table_name),
            Import::Func { .. } | Import::Memory { .. } | Import::Global { .. } => None,
        }
    }

    pub(crate) fn mem_name(&self) -> Option<&MemName> {
        match self {
            Import::Memory { mem_name, .. } => Some(mem_name),
            Import::Func { .. } | Import::Table { .. } | Import::Global { .. } => None,
        }
    }

    pub(crate) fn global_name(&self) -> Option<&GlobalName> {
        match self {
            Import::Global { global_name, .. } => Some(global_name),
            Import::Func { .. } | Import::Table { .. } | Import::Memory { .. } => None,
        }
    }
}

/// What instantiation does with a data segment: copy it into a memory at a constant offset (`Active`), or keep it for a later `memory.init`, `array.new_data` or `array.init_data` (`Passive`).
#[derive(Debug)]
pub enum DataMode {
    Passive,
    Active { mem_name: MemName, offset: Expr },
}

/// A data segment: what instantiation does with it, and the raw bytes it holds.
#[derive(Debug)]
pub struct DataSegment {
    pub mode: DataMode,
    pub bytes: Vec<u8>,
}

/// A module-defined function: its signature (a reference to a declared func type), its parameter names in signature order, its extra locals, and its body. Params and locals share one name space per function, which is how the encoder resolves `local.get`/`local.set`/`local.tee` operands to indices.
#[derive(Debug)]
pub struct Func {
    pub type_name: TypeName,
    pub params: Vec<LocalName>,
    pub locals: Vec<(LocalName, ValType)>,
    pub expr: Expr,
}

impl Func {
    pub(crate) fn local_names(&self) -> impl Iterator<Item = &LocalName> {
        self.params
            .iter()
            .chain(self.locals.iter().map(|(local_name, _)| local_name))
    }
}

/// A module-defined global: its type plus the constant initializer expression evaluated at instantiation. Values that cannot be built by a constant expression are instead assigned by the start function via `global.set`.
#[derive(Debug)]
pub struct Global {
    pub global_type: GlobalType,
    pub expr: Expr,
}

/// A module-defined table: its type plus the constant expression every slot starts at. `None` starts them at the element type's default, which only a nullable element type has — a table of a non-defaultable element type must carry the expression, which is the function-references extension to the table section.
#[derive(Debug)]
pub struct Table {
    pub table_type: TableType,
    pub expr: Option<Expr>,
}

/// What instantiation does with an element segment: drop it into a table at a constant offset (`Active`), keep it for a later `table.init` (`Passive`), or neither — `Declarative` contributes no elements at all and exists only to make the functions it lists eligible for `ref.func`.
#[derive(Debug)]
pub enum ElemMode {
    Passive,
    Declarative,
    Active { table_name: TableName, offset: Expr },
}

/// An element segment's contents: functions by name, whose element type is `funcref` implicitly, or constant expressions under an explicit element type — the only form a table of a typed reference admits.
#[derive(Debug)]
pub enum ElemList {
    Funcs(Vec<FuncName>),
    Exprs(RefType, Vec<Expr>),
}

/// An element segment: what instantiation does with it, and what it holds. Named like every other item, because `table.init` and `elem.drop` refer to one.
#[derive(Debug)]
pub struct ElemSegment {
    pub mode: ElemMode,
    pub list: ElemList,
}

/// The internal target of an export — a function, table, memory, or global, by name. The host-visible export string lives alongside it in the module's export list, so the same item can be exported under any name.
#[derive(Debug)]
pub enum Export {
    Func(FuncName),
    Table(TableName),
    Memory(MemName),
    Global(GlobalName),
}

/// An in-memory wasm-GC module under construction — the crate's central type and the emitter's build target. Every item is registered under a symbolic name and every cross-reference is by name; numeric index spaces exist only inside the binary encoder, which derives them from insertion order (imports leading). Render it as WAT-style text via `Display`, parse that text back via `FromStr`, or encode it with [`to_bytes`](crate::to_bytes).
#[derive(Debug)]
pub struct Module {
    name: String,
    types: Vec<RecType>,
    imports: Vec<(String, String, Import)>,
    funcs: Vec<(FuncName, Func)>,
    tables: Vec<(TableName, Table)>,
    mems: Vec<(MemName, MemType)>,
    globals: Vec<(GlobalName, Global)>,
    datas: Vec<(DataName, DataSegment)>,
    exports: Vec<(String, Export)>,
    start: Option<FuncName>,
    elems: Vec<(ElemName, ElemSegment)>,
}

impl Module {
    /// Creates an empty module. The name carries no wasm semantics; it is emitted in the custom name section so tooling can identify the module.
    pub fn new<N>(name: N) -> Self
    where
        N: Into<String>,
    {
        Self {
            name: name.into(),
            types: Default::default(),
            imports: Default::default(),
            funcs: Default::default(),
            tables: Default::default(),
            mems: Default::default(),
            globals: Default::default(),
            datas: Default::default(),
            exports: Default::default(),
            start: None,
            elems: Default::default(),
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn types(&self) -> &[RecType] {
        &self.types
    }

    /// Looks up a declared type by name across all recursion groups — type names form one flat module-wide namespace, so this is how a type's shape is read back out: the printer's spelling of a function's signature, and curios-js's bridge tests checking the accessors' declared shapes.
    pub fn get_type(&self, target_name: &TypeName) -> Option<&SubType> {
        self.types
            .iter()
            .flat_map(|rec_type| &rec_type.sub_types)
            .find_map(|(type_name, sub_type)| (target_name == type_name).then_some(sub_type))
    }

    /// Adds a whole recursion group. Required when the types in it reference each other — a group's members are declared simultaneously, so a forward reference is only well-formed inside one.
    pub fn add_types(&mut self, rec_type: RecType) {
        self.types.push(rec_type);
    }

    /// Adds a single type as its own one-member recursion group. Sufficient whenever the type only references itself or earlier declarations; mutually recursive types must instead share a group, via [`Module::add_types`].
    pub fn add_type(&mut self, type_name: TypeName, sub_type: SubType) {
        self.types.push(RecType::from([(type_name, sub_type)]));
    }

    /// The imports in insertion order, each with its two-level (module, name) resolution key.
    pub fn imports(&self) -> &[(String, String, Import)] {
        &self.imports
    }

    /// Adds an import under the two-level `module_name`/`name` key the host resolves at instantiation. Imported items always take the leading indices of their index spaces, ahead of module-defined ones.
    pub fn add_import<M, N>(&mut self, module_name: M, name: N, import: Import)
    where
        M: Into<String>,
        N: Into<String>,
    {
        self.imports.push((module_name.into(), name.into(), import));
    }

    pub(crate) fn funcs(&self) -> &[(FuncName, Func)] {
        &self.funcs
    }

    /// Adds a module-defined function under `func_name`, the name by which calls, `ref.func`, and exports refer to it.
    pub fn add_func(&mut self, func_name: FuncName, func: Func) {
        self.funcs.push((func_name, func));
    }

    pub(crate) fn tables(&self) -> &[(TableName, Table)] {
        &self.tables
    }

    /// Adds a module-defined table. Its index-space position is its insertion order among defined tables, after all imported ones.
    pub fn add_table(&mut self, table_name: TableName, table: Table) {
        self.tables.push((table_name, table));
    }

    pub(crate) fn mems(&self) -> &[(MemName, MemType)] {
        &self.mems
    }

    /// Adds a module-defined memory. A module that declares none, and imports none, gets no memory section at all — nothing here is emitted on a program's behalf.
    pub fn add_memory(&mut self, mem_name: MemName, mem_type: MemType) {
        self.mems.push((mem_name, mem_type));
    }

    pub(crate) fn globals(&self) -> &[(GlobalName, Global)] {
        &self.globals
    }

    /// Adds a module-defined global. Its index-space position is its insertion order among defined globals, after all imported ones — the encoder works this out from names, so interleaving with `add_import` calls is harmless.
    pub fn add_global(&mut self, global_name: GlobalName, global: Global) {
        self.globals.push((global_name, global));
    }

    /// The data segments in insertion order, which is also their index order in the data section.
    pub fn datas(&self) -> &[(DataName, DataSegment)] {
        &self.datas
    }

    /// Adds a data segment; its insertion order is its index order, which is what `memory.init`, `data.drop`, `array.new_data` and `array.init_data` resolve `data_name` against.
    pub fn add_data(&mut self, data_name: DataName, data_segment: DataSegment) {
        self.datas.push((data_name, data_segment));
    }

    /// The exports in insertion order: each host-visible string name paired with the internal item it exposes.
    pub fn exports(&self) -> &[(String, Export)] {
        &self.exports
    }

    /// Exports a previously added function, table, memory or global under the host-visible string `name`.
    pub fn add_export<N>(&mut self, name: N, export: Export)
    where
        N: Into<String>,
    {
        self.exports.push((name.into(), export));
    }

    pub(crate) fn start(&self) -> Option<&FuncName> {
        self.start.as_ref()
    }

    /// Sets the start function, run automatically when the module is instantiated; the emitter points this at the function that initializes the module's globals.
    pub fn set_start(&mut self, func_name: FuncName) {
        self.start = Some(func_name);
    }

    pub(crate) fn elems(&self) -> &[(ElemName, ElemSegment)] {
        &self.elems
    }

    /// Adds an element segment; its insertion order is its index order, which is what `table.init` and `elem.drop` resolve `elem_name` against.
    pub fn add_elem(&mut self, elem_name: ElemName, elem_segment: ElemSegment) {
        self.elems.push((elem_name, elem_segment));
    }
}

impl fmt::Display for Module {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(print_module(self), formatter, 4)?;

        Ok(())
    }
}

impl Module {
    /// A `Display` adapter rendering the module within `width` columns — long runs (a wide function signature's `param` bindings) break one per line instead of sharing one. The `wonder stage wasm` inspection path; plain `Display` keeps the unbounded flat layout.
    pub fn display_within(&self, width: usize) -> impl fmt::Display + '_ {
        struct Within<'a>(&'a Module, usize);
        impl fmt::Display for Within<'_> {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                run_printer_within(print_module(self.0), formatter, 4, self.1)
            }
        }
        Within(self, width)
    }
}
