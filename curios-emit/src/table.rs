use {
    super::{
        BigHelper, EmissionBlockName, EmissionBody, EmissionClosure, EmissionClosureName,
        EmissionData, EmissionFunction, EmissionFunctionName, EmissionModule, EmissionValue,
        EmissionValueName, FltHelper, LoadAs, call, concrete_val, get, i64_const, refuse_func_name,
        tee, when,
    },
    curios_abi::{Carrier, ForeignFunction, Namespace, WireLeaf, WireType},
    std::{
        cell::{OnceCell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        sync::Arc,
    },
};

#[derive(Debug, Clone)]
pub(crate) struct FieldData {
    type_name: curios_wasm::TypeName,
    field_name: curios_wasm::FieldName,
}

impl FieldData {
    pub(crate) fn new(
        type_name: curios_wasm::TypeName,
        field_name: curios_wasm::FieldName,
    ) -> Self {
        Self {
            type_name,
            field_name,
        }
    }

    pub(crate) fn type_name(&self) -> curios_wasm::TypeName {
        self.type_name.clone()
    }

    pub(crate) fn field_name(&self) -> curios_wasm::FieldName {
        self.field_name.clone()
    }
}

/// The name bundle for one internal rope carrier (`$rope/bytes` or `$rope/list`): the base struct every helper takes, its `leaf`/`node`/`view` subtypes, the block type a reference enters the base through, the flat payload array, the room record both carriers share, and every field name — one handle to thread through the op emitters so packed `Bytes` and `List` share their lowering code.
#[derive(Debug, Clone)]
pub(crate) struct RopeData {
    pub base: curios_wasm::TypeName,
    /// `[anyref] -> [(ref $base)]`, the type of the block `Table::enter` builds.
    pub enter: curios_wasm::TypeName,
    /// `[anyref] -> [(ref null $base)]`, the type of the block `Table::enter` builds where a null passes.
    pub enter_or_null: curios_wasm::TypeName,
    pub leaf: curios_wasm::TypeName,
    pub node: curios_wasm::TypeName,
    pub view: curios_wasm::TypeName,
    pub payload: curios_wasm::TypeName,
    pub room: curios_wasm::TypeName,
    pub tag_field: curios_wasm::FieldName,
    pub len_field: curios_wasm::FieldName,
    pub payload_field: curios_wasm::FieldName,
    pub left_field: curios_wasm::FieldName,
    pub right_field: curios_wasm::FieldName,
    pub cache_field: curios_wasm::FieldName,
    pub room_field: curios_wasm::FieldName,
    pub extent_field: curios_wasm::FieldName,
    pub base_field: curios_wasm::FieldName,
    pub offset_field: curios_wasm::FieldName,
}

/// The bounded channel's storage names. The payload array fixes capacity; mutable head, count, and closed fields describe its live window.
#[derive(Debug, Clone)]
pub(crate) struct ChannelData {
    pub channel: curios_wasm::TypeName,
    pub payload: curios_wasm::TypeName,
    pub payload_field: curios_wasm::FieldName,
    pub head_field: curios_wasm::FieldName,
    pub count_field: curios_wasm::FieldName,
    pub closed_field: curios_wasm::FieldName,
}

/// The name bundle for the boxed form a `Nat` or `Int` outside the i31 takes: the `$big` struct, the `$words` array holding its magnitude, and the struct's two fields — one handle for the big-number helpers and the literal materialization to share.
#[derive(Debug, Clone)]
pub(crate) struct BigData {
    pub big: curios_wasm::TypeName,
    pub words: curios_wasm::TypeName,
    pub sign_field: curios_wasm::FieldName,
    pub limbs_field: curios_wasm::FieldName,
}

#[derive(Debug, Clone)]
pub(crate) struct ClsrData<'a> {
    /// This closure's slot in its arity's dispatch table — the value its environment's special field holds. 1-based: slot 0 stays null, so a zeroed code field dispatches into the null entry and traps rather than into a body.
    index: i32,
    func_name: curios_wasm::FuncName,
    envr_type: curios_wasm::TypeName,
    fields: Vec<(&'a EmissionValueName, curios_wasm::FieldName)>,
    params: HashMap<&'a EmissionValueName, curios_wasm::LocalName>,
    resume: &'a EmissionBlockName,
}

impl<'a> ClsrData<'a> {
    pub(crate) fn new(
        clsr_name: &'a EmissionClosureName,
        clsr: &'a EmissionClosure,
        index: i32,
    ) -> Self {
        Self {
            index,
            func_name: curios_wasm::FuncName::from(format!("clsr/{}", clsr_name)),
            envr_type: curios_wasm::TypeName::from(format!("envr/{}", clsr_name)),
            fields: clsr
                .fields
                .iter()
                .map(|field| {
                    (
                        &field.name,
                        curios_wasm::FieldName::from(format!("${}", field.name)),
                    )
                })
                .collect(),
            params: clsr
                .params
                .iter()
                .map(|param| {
                    (
                        &param.name,
                        curios_wasm::LocalName::from(format!("${}", param.name)),
                    )
                })
                .collect(),
            resume: &clsr.resume,
        }
    }

    pub(crate) fn index(&self) -> i32 {
        self.index
    }

    pub(crate) fn func_name(&self) -> curios_wasm::FuncName {
        self.func_name.clone()
    }

    pub(crate) fn envr_type(&self) -> curios_wasm::TypeName {
        self.envr_type.clone()
    }

    pub(crate) fn fields(&self) -> impl Iterator<Item = curios_wasm::FieldName> {
        self.fields.iter().map(|(_, field_name)| field_name.clone())
    }

    /// The value each field holds, in environment order: the captured value's own name, which the representation decided a carrier for.
    pub(crate) fn field_values(&self) -> impl Iterator<Item = &'a EmissionValueName> + '_ {
        self.fields.iter().map(|(value_name, _)| *value_name)
    }

    pub(crate) fn find_field(&self, value_name: &EmissionValueName) -> Option<FieldData> {
        self.fields
            .iter()
            .find_map(|(field_name, mapped_field_name)| {
                (value_name == *field_name).then_some(mapped_field_name)
            })
            .cloned()
            .map(|field_name| FieldData::new(self.envr_type(), field_name))
    }

    pub(crate) fn params(&self) -> HashMap<&'a EmissionValueName, curios_wasm::LocalName> {
        self.params.clone()
    }

    pub(crate) fn find_param(
        &self,
        value_name: &EmissionValueName,
    ) -> Option<curios_wasm::LocalName> {
        self.params.get(value_name).cloned()
    }

    pub(crate) fn arity(&self) -> usize {
        self.params.len()
    }

    pub(crate) fn is_resume(&self, block_name: &EmissionBlockName) -> bool {
        self.resume == block_name
    }
}

/// What one position of a function's type holds: a reference, or a register at its carrier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Position {
    Reference,
    Word,
    Length,
    Float,
    /// A closure of this arity, declared at the arity's environment type.
    Closure(usize),
}

impl Position {
    /// The position a value held at `carrier` takes — a register's, or `None` for a reference.
    pub(crate) fn of(carrier: Option<curios_cps::Repr>) -> Self {
        match carrier {
            None => Self::Reference,
            Some(curios_cps::Repr::Nat) => Self::Word,
            Some(curios_cps::Repr::Length) => Self::Length,
            Some(curios_cps::Repr::Flt) => Self::Float,
            Some(other) => panic!("a register holds a word, a length or a float, never {other:?}"),
        }
    }

    /// The register carrier this position holds, or `None` for a reference.
    pub(crate) fn carrier(self) -> Option<curios_cps::Repr> {
        match self {
            Self::Reference | Self::Closure(_) => None,
            Self::Word => Some(curios_cps::Repr::Nat),
            Self::Length => Some(curios_cps::Repr::Length),
            Self::Float => Some(curios_cps::Repr::Flt),
        }
    }

    /// The wasm type a value in this position is declared at. A reference admits null, as every reference position a value crosses does.
    pub(crate) fn val_type(self, table: &Table) -> curios_wasm::ValType {
        match self {
            Self::Reference => Table::top_type(true),
            Self::Word | Self::Length => curios_wasm::ValType::Num(curios_wasm::NumType::I32),
            Self::Float => curios_wasm::ValType::Num(curios_wasm::NumType::F64),
            Self::Closure(arity) => concrete_val(table.find_envr_type(arity), true),
        }
    }

    /// How a value is loaded to cross into this position.
    pub(crate) fn load_as(self, table: &Table) -> LoadAs {
        match self {
            Self::Reference => LoadAs::Null,
            Self::Word | Self::Length | Self::Float => {
                LoadAs::of(&self.carrier().expect("a register position has a carrier"))
            }
            Self::Closure(arity) => LoadAs::ConcreteOrNull(table.find_envr_type(arity)),
        }
    }

    fn spelling(self) -> String {
        match self {
            Self::Reference => "r".to_string(),
            Self::Word => "i".to_string(),
            Self::Length => "u".to_string(),
            Self::Float => "f".to_string(),
            Self::Closure(arity) => format!("c{arity}"),
        }
    }
}

/// A function's type: what each parameter and each result is held at.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Signature {
    pub(crate) params: Vec<Position>,
    pub(crate) results: Vec<Position>,
}

impl Signature {
    /// `func`'s type: a parameter is held as the representation decided its value is — in a register `raw` holds, or at the environment type of the closures `closures` knows it holds — and a result as the function hands it back.
    fn of(
        func: &EmissionFunction,
        raw: &HashMap<EmissionValueName, curios_cps::Repr>,
        closures: &HashMap<EmissionValueName, usize>,
    ) -> Self {
        Self {
            params: func
                .params
                .iter()
                .map(|param| match raw.get(&param.name) {
                    Some(&carrier) => Position::of(Some(carrier)),
                    None => closures
                        .get(&param.name)
                        .map_or(Position::Reference, |&arity| Position::Closure(arity)),
                })
                .collect(),
            results: func.results.iter().copied().map(Position::of).collect(),
        }
    }

    /// The emitted name of this type. A type all of references keeps the spelling it had before any position was held in a register, `func/{parameters}` with one result and `func/{parameters}/{results}` with several; any other spells each position, `r` for a reference, `i` for a word, `u` for a length, `f` for a float and `c` with its arity for a closure, and `0` for none.
    ///
    /// What produces several results is `curios-cps`'s return protocol, which hands a class of functions back the leading fields of the construction they would otherwise allocate, and what produces a register position is its representation analysis; both are decided per tail-call class, because `return_call` requires a callee's results to match its caller's exactly. Closure supertypes are keyed separately, on arity alone, and stay uniform and single-result — which is why both decisions exclude any function that escapes.
    fn name(&self) -> curios_wasm::TypeName {
        let uniform = self
            .params
            .iter()
            .chain(&self.results)
            .all(|position| *position == Position::Reference);
        let spelled = |positions: &[Position]| match positions.is_empty() {
            true => "0".to_string(),
            false => positions
                .iter()
                .map(|position| position.spelling())
                .collect(),
        };
        curios_wasm::TypeName::from(match (uniform, self.results.len()) {
            (true, 1) => format!("func/{}", self.params.len()),
            (true, results) => format!("func/{}/{}", self.params.len(), results),
            (false, _) => format!("func/{}/{}", spelled(&self.params), spelled(&self.results)),
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FuncData<'a> {
    func_name: curios_wasm::FuncName,
    params: HashMap<&'a EmissionValueName, curios_wasm::LocalName>,
    signature: Signature,
    resume: &'a EmissionBlockName,
}

impl<'a> FuncData<'a> {
    pub(crate) fn new(
        func_name: &'a EmissionFunctionName,
        func: &'a EmissionFunction,
        raw: &HashMap<EmissionValueName, curios_cps::Repr>,
        closures: &HashMap<EmissionValueName, usize>,
    ) -> Self {
        Self {
            signature: Signature::of(func, raw, closures),
            // The `func/` prefix is why the exported entrypoint is `func/main`: the entry is always `main`, and the export reuses the function's emitted name.
            func_name: curios_wasm::FuncName::from(format!("func/{}", func_name)),
            params: func
                .params
                .iter()
                .map(|param| {
                    (
                        &param.name,
                        curios_wasm::LocalName::from(format!("${}", param.name)),
                    )
                })
                .collect(),
            resume: &func.resume,
        }
    }

    pub(crate) fn func_name(&self) -> curios_wasm::FuncName {
        self.func_name.clone()
    }

    /// What each parameter and each result of this function is held at.
    pub(crate) fn signature(&self) -> &Signature {
        &self.signature
    }

    pub(crate) fn params(&self) -> HashMap<&'a EmissionValueName, curios_wasm::LocalName> {
        self.params.clone()
    }

    pub(crate) fn find_param(
        &self,
        value_name: &EmissionValueName,
    ) -> Option<curios_wasm::LocalName> {
        self.params.get(value_name).cloned()
    }

    pub(crate) fn is_resume(&self, block_name: &EmissionBlockName) -> bool {
        self.resume == block_name
    }
}

/// The arity of the tuple `data` builds, where it builds one.
fn built_tuple_arity(data: &EmissionData) -> Option<usize> {
    match data {
        EmissionData::Tuple(fields) => Some(fields.len()),
        _ => None,
    }
}

/// Note into `arities` the arity of every tuple `region` and the blocks nested in it build.
fn note_built_tuple_arities(region: &EmissionBody, arities: &mut BTreeSet<usize>) {
    for (_, value) in &region.values {
        if let EmissionValue::Pure(data) = value {
            arities.extend(built_tuple_arity(data));
        }
    }
    for (_, block) in &region.blocks {
        note_built_tuple_arities(&block.region, arities);
    }
}

#[derive(Debug)]
pub(crate) struct Table<'a> {
    special_field: curios_wasm::FieldName,
    special_local: curios_wasm::LocalName,
    special_label: curios_wasm::LabelName,
    clsr_tables: BTreeMap<usize, curios_wasm::TableName>,
    flt_type: curios_wasm::TypeName,
    big_type: curios_wasm::TypeName,
    words_type: curios_wasm::TypeName,
    longs_type: curios_wasm::TypeName,
    bytes_rope_type: curios_wasm::TypeName,
    list_rope_type: curios_wasm::TypeName,
    bytes_type: curios_wasm::TypeName,
    elems_type: curios_wasm::TypeName,
    bytes_rope_leaf_type: curios_wasm::TypeName,
    bytes_rope_node_type: curios_wasm::TypeName,
    bytes_rope_view_type: curios_wasm::TypeName,
    list_rope_leaf_type: curios_wasm::TypeName,
    list_rope_node_type: curios_wasm::TypeName,
    list_rope_view_type: curios_wasm::TypeName,
    bytes_rope_enter_type: curios_wasm::TypeName,
    list_rope_enter_type: curios_wasm::TypeName,
    bytes_rope_enter_or_null_type: curios_wasm::TypeName,
    list_rope_enter_or_null_type: curios_wasm::TypeName,
    rope_room_type: curios_wasm::TypeName,
    cell_type: curios_wasm::TypeName,

    panic: OnceCell<curios_wasm::FuncName>,
    // One slot per class, in `Panic::ALL`'s order, minted lazily: a module declares the refusals its code can reach and no others.
    refuse: [OnceCell<curios_wasm::FuncName>; curios_cps::Panic::ALL.len()],
    // One slot per big-number helper, in `BigHelper::ALL`'s order, minted lazily like `refuse`: a module declares the helpers its code and its other helpers reach and no others.
    big: [OnceCell<curios_wasm::FuncName>; BigHelper::ALL.len()],
    // One slot per float helper, in `FltHelper::ALL`'s order, minted lazily like `big`.
    flt: [OnceCell<curios_wasm::FuncName>; FltHelper::ALL.len()],
    // The shared rope helpers, minted lazily: the first call site recorded during emission names the function, and the module emitter then adds exactly the recorded set after the program's own functions (see `emit_rope_funcs`).
    bytes_force: OnceCell<curios_wasm::FuncName>,
    list_force: OnceCell<curios_wasm::FuncName>,
    bytes_exact: OnceCell<curios_wasm::FuncName>,
    list_exact: OnceCell<curios_wasm::FuncName>,
    list_bytes_force: OnceCell<curios_wasm::FuncName>,
    list_empty: OnceCell<curios_wasm::GlobalName>,
    bytes_embed: OnceCell<curios_wasm::FuncName>,
    bytes_box: OnceCell<curios_wasm::FuncName>,
    bytes_norm: OnceCell<curios_wasm::FuncName>,
    list_embed: OnceCell<curios_wasm::FuncName>,
    /// The scalar leaves whose list crosses to a host, each through `$list/<leaf>/to_<payload>`.
    scalars_forces: RefCell<Vec<WireLeaf>>,
    /// The scalar leaves whose list a host answers, each through `$list/<leaf>/of_<payload>`.
    scalars_embeds: RefCell<Vec<WireLeaf>>,
    list_bytes_embed: OnceCell<curios_wasm::FuncName>,
    bytes_slice: OnceCell<curios_wasm::FuncName>,
    list_slice: OnceCell<curios_wasm::FuncName>,
    bytes_read: OnceCell<curios_wasm::FuncName>,
    list_read: OnceCell<curios_wasm::FuncName>,
    bytes_eql: OnceCell<curios_wasm::FuncName>,
    // The pointwise trio takes forced payloads.
    bytes_and: OnceCell<curios_wasm::FuncName>,
    bytes_or: OnceCell<curios_wasm::FuncName>,
    bytes_xor: OnceCell<curios_wasm::FuncName>,
    bytes_replicate: OnceCell<curios_wasm::FuncName>,
    list_map: OnceCell<curios_wasm::FuncName>,
    // The foreign functions the emitted code calls, keyed by the minted internal name (see `host_func`). Lazy used-tracking: the first call-site reference during emission records the function's row, and `emit_sys_imports` then declares exactly the recorded set (in minted-name order — wasmtime links by name, so import order is cosmetic).
    host_funcs: RefCell<BTreeMap<String, Arc<ForeignFunction>>>,
    /// One final struct type for each arity the module builds a tuple at, and the unit a host call resumes with: every tuple a read can meet was built at one of them, since nothing outside the module builds one, and a read tries those wide enough to hold its field.
    tuple_types: BTreeMap<usize, curios_wasm::TypeName>,
    /// One final struct type per nominal row, keyed by the row's identity rather than by an arity — which is what makes a row read an exact cast and gives Binaryen's closed-world passes distinct types to refine. Widths come from the CPS module's own row table, so a row whose constructions were all optimized away still declares its type (harmless, and a projection can outlive its constructions).
    row_types: BTreeMap<curios_cps::RowId, (curios_wasm::TypeName, Vec<curios_cps::Slot>)>,
    envr_types: BTreeMap<usize, curios_wasm::TypeName>,
    clsr_types: BTreeMap<usize, curios_wasm::TypeName>,
    /// Keyed by what a wasm function type actually is — what each parameter and each result is held at — rather than by parameter count alone, so two functions of the same arity delivering different result shapes, or holding a position in different carriers, cannot collide on one type. The closure supertypes below stay keyed by arity, because a function reached through one is invoked at the uniform shape whatever its own type says.
    func_types: BTreeMap<Signature, curios_wasm::TypeName>,
    // No accessor iterates `consts`, `clsrs` or `funcs`: they are `HashMap`s, so iteration order varies per process, and every consumer here emits into the module — where order is load-bearing for a reproducible build. A consumer walks `EmissionModule`'s own ordered sequence and resolves each name through these indices instead, which is what `curios-utilities`'s `name!` means by carrying an explicit sequence where the order matters.
    consts: HashMap<&'a EmissionValueName, curios_wasm::GlobalName>,
    /// The module consts that are `Tuple`/`List` constructions — the hoisted half of the population `Context::refuse_raw_aggregate` refuses to hand to a register. A closed aggregate is lifted out of its region by `hoist`, so a guard reading region values alone would miss exactly the constant ones.
    const_aggregates: HashSet<&'a EmissionValueName>,
    /// The scalar consts a register can hold, each with its carrier and the instruction that is its value there: a `Flt` and a small `Nat`. Hoisting renames a literal to its const whatever reads it, so a register read would otherwise take the global back out of its box.
    const_registers: HashMap<&'a EmissionValueName, (curios_cps::Repr, curios_wasm::Instr)>,
    clsrs: HashMap<&'a EmissionClosureName, ClsrData<'a>>,
    funcs: HashMap<&'a EmissionFunctionName, FuncData<'a>>,
    /// The values the representation analysis decided to hold in a register, and at which carrier. A name absent here is held behind a reference, which is every synthetic name codegen mints for itself.
    raw: &'a HashMap<EmissionValueName, curios_cps::Repr>,
    /// The values the representation analysis knows hold closures of one arity, and that arity: each declared at its arity's environment type.
    closures: &'a HashMap<EmissionValueName, usize>,
    /// The values a closure construction binds, in a body or hoisted, with the closure each builds: each declared at that closure's own environment type.
    constructed: HashMap<&'a EmissionValueName, &'a EmissionClosureName>,
}

impl<'a> Table<'a> {
    pub(crate) fn new(
        module: &'a EmissionModule,
        raw: &'a HashMap<EmissionValueName, curios_cps::Repr>,
        closures: &'a HashMap<EmissionValueName, usize>,
    ) -> Self {
        Self {
            raw,
            closures,
            constructed: module.closure_constructions(),
            special_field: curios_wasm::FieldName::from("!"),
            special_local: curios_wasm::LocalName::from("!"),
            special_label: curios_wasm::LabelName::from("!"),
            clsr_tables: module
                .clsr_arities()
                .into_iter()
                .map(|arity| {
                    (
                        arity,
                        curios_wasm::TableName::from(format!("clsr/{}", arity)),
                    )
                })
                .collect(),
            flt_type: curios_wasm::TypeName::from("flt"),
            big_type: curios_wasm::TypeName::from("big"),
            words_type: curios_wasm::TypeName::from("words"),
            longs_type: curios_wasm::TypeName::from("longs"),
            bytes_rope_type: curios_wasm::TypeName::from("rope/bytes"),
            list_rope_type: curios_wasm::TypeName::from("rope/list"),
            bytes_type: curios_wasm::TypeName::from("bytes"),
            elems_type: curios_wasm::TypeName::from("elems"),
            bytes_rope_leaf_type: curios_wasm::TypeName::from("rope/bytes/leaf"),
            bytes_rope_node_type: curios_wasm::TypeName::from("rope/bytes/node"),
            bytes_rope_view_type: curios_wasm::TypeName::from("rope/bytes/view"),
            list_rope_leaf_type: curios_wasm::TypeName::from("rope/list/leaf"),
            list_rope_node_type: curios_wasm::TypeName::from("rope/list/node"),
            list_rope_view_type: curios_wasm::TypeName::from("rope/list/view"),
            bytes_rope_enter_type: curios_wasm::TypeName::from("rope/bytes/enter"),
            list_rope_enter_type: curios_wasm::TypeName::from("rope/list/enter"),
            bytes_rope_enter_or_null_type: curios_wasm::TypeName::from("rope/bytes/enter_or_null"),
            list_rope_enter_or_null_type: curios_wasm::TypeName::from("rope/list/enter_or_null"),
            rope_room_type: curios_wasm::TypeName::from("rope/room"),
            cell_type: curios_wasm::TypeName::from("cell"),
            panic: OnceCell::new(),
            refuse: Default::default(),
            big: Default::default(),
            flt: Default::default(),
            bytes_force: OnceCell::new(),
            list_force: OnceCell::new(),
            bytes_exact: OnceCell::new(),
            list_exact: OnceCell::new(),
            list_bytes_force: OnceCell::new(),
            list_empty: OnceCell::new(),
            bytes_embed: OnceCell::new(),
            bytes_box: OnceCell::new(),
            bytes_norm: OnceCell::new(),
            list_embed: OnceCell::new(),
            scalars_forces: RefCell::new(Vec::new()),
            scalars_embeds: RefCell::new(Vec::new()),
            list_bytes_embed: OnceCell::new(),
            bytes_slice: OnceCell::new(),
            list_slice: OnceCell::new(),
            bytes_read: OnceCell::new(),
            list_read: OnceCell::new(),
            bytes_eql: OnceCell::new(),
            bytes_and: OnceCell::new(),
            bytes_or: OnceCell::new(),
            bytes_xor: OnceCell::new(),
            bytes_replicate: OnceCell::new(),
            list_map: OnceCell::new(),
            host_funcs: RefCell::new(BTreeMap::new()),
            row_types: module
                .rows()
                .iter()
                .map(|(row, definition)| {
                    let spelled = match &definition.debug_name {
                        Some(hint) => format!("row/{}${hint}", row.index()),
                        None => format!("row/{}", row.index()),
                    };
                    (
                        *row,
                        (
                            curios_wasm::TypeName::from(spelled),
                            definition.slots.clone(),
                        ),
                    )
                })
                .collect(),
            tuple_types: {
                let mut arities = BTreeSet::from([0]);
                arities.extend(
                    module
                        .consts()
                        .iter()
                        .filter_map(|(_, data)| built_tuple_arity(data)),
                );
                for (_, clsr) in module.clsrs() {
                    note_built_tuple_arities(&clsr.region, &mut arities);
                }
                for (_, func) in module.funcs() {
                    note_built_tuple_arities(&func.region, &mut arities);
                }

                arities
                    .into_iter()
                    .map(|arity| {
                        (
                            arity,
                            curios_wasm::TypeName::from(format!("tuple/{}", arity)),
                        )
                    })
                    .collect()
            },
            envr_types: module
                .clsr_arities()
                .into_iter()
                .map(|arity| {
                    (
                        arity,
                        curios_wasm::TypeName::from(format!("envr/{}", arity)),
                    )
                })
                .collect(),
            clsr_types: module
                .clsr_arities()
                .into_iter()
                .map(|arity| {
                    (
                        arity,
                        curios_wasm::TypeName::from(format!("clsr/{}", arity)),
                    )
                })
                .collect(),
            func_types: module
                .funcs()
                .iter()
                .map(|(_, func)| Signature::of(func, raw, closures))
                .map(|signature| {
                    let name = signature.name();
                    (signature, name)
                })
                .collect(),
            consts: module
                .consts()
                .iter()
                .map(|(const_name, _)| {
                    (
                        const_name,
                        curios_wasm::GlobalName::from(const_name.as_string()),
                    )
                })
                .collect(),
            const_registers: module
                .consts()
                .iter()
                .filter_map(|(const_name, data)| {
                    let register = match data {
                        &EmissionData::Flt(value) => (
                            curios_cps::Repr::Flt,
                            curios_wasm::Instr::F64Const { value },
                        ),
                        EmissionData::Nat(value) if curios_cps::nat_is_small(value) => (
                            curios_cps::Repr::Nat,
                            curios_wasm::Instr::I32Const {
                                value: u32::try_from(value).expect("a small `Nat` is a word")
                                    as i32,
                            },
                        ),
                        _ => return None,
                    };
                    Some((const_name, register))
                })
                .collect(),
            const_aggregates: module
                .consts()
                .iter()
                .filter(|(_, data)| {
                    matches!(
                        data,
                        EmissionData::List(_) | EmissionData::Tuple(_) | EmissionData::Row(..)
                    )
                })
                .map(|(const_name, _)| const_name)
                .collect(),
            // Table indices come from the module's ordered closure walk — the same order each arity's element segment lists its bodies in — never from this map's iteration. The counter is per arity because each arity dispatches through its own typed table.
            clsrs: {
                let mut next_index = BTreeMap::<usize, i32>::new();
                module
                    .clsrs()
                    .iter()
                    .map(|(clsr_name, clsr)| {
                        let index = next_index.entry(clsr.params.len()).or_insert(0);
                        *index += 1;
                        (clsr_name, ClsrData::new(clsr_name, clsr, *index))
                    })
                    .collect()
            },
            funcs: module
                .funcs()
                .iter()
                .map(|(func_name, func)| (func_name, FuncData::new(func_name, func, raw, closures)))
                .collect(),
        }
    }

    pub(crate) fn special_field(&self) -> curios_wasm::FieldName {
        self.special_field.clone()
    }

    pub(crate) fn special_local(&self) -> curios_wasm::LocalName {
        self.special_local.clone()
    }

    pub(crate) fn special_label(&self) -> curios_wasm::LabelName {
        self.special_label.clone()
    }

    /// The dispatch table holding every closure body of `arity`, indexed by [`ClsrData::index`]. Typed `(ref null $clsr/{arity})` rather than `funcref`, so a `call_indirect` expecting that type is statically known to match and the engine emits no runtime signature check at all.
    pub(crate) fn clsr_table(&self, arity: usize) -> curios_wasm::TableName {
        self.clsr_tables
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks closure table for arity `{}`", arity))
            .clone()
    }

    pub(crate) fn top_type(is_nullable: bool) -> curios_wasm::ValType {
        curios_wasm::ValType::Ref(curios_wasm::RefType {
            is_nullable,
            heap_type: curios_wasm::HeapType::Abstract(curios_wasm::AbsHeapType::Any),
        })
    }

    pub(crate) fn int_type(is_nullable: bool) -> curios_wasm::RefType {
        curios_wasm::RefType {
            is_nullable,
            heap_type: curios_wasm::HeapType::Abstract(curios_wasm::AbsHeapType::I31),
        }
    }

    pub(crate) fn flt_type(&self) -> curios_wasm::TypeName {
        self.flt_type.clone()
    }

    /// The boxed form's name bundle.
    pub(crate) fn big(&self) -> BigData {
        BigData {
            big: self.big_type.clone(),
            words: self.words_type.clone(),
            sign_field: curios_wasm::FieldName::from("sign"),
            limbs_field: curios_wasm::FieldName::from("limbs"),
        }
    }

    /// The big-number helper `helper`, marked for emission by this first use — from the emitted code or from another helper's body, which is why the roster is emitted callers first.
    pub(crate) fn big_func(&self, helper: BigHelper) -> curios_wasm::FuncName {
        self.big[helper.slot()]
            .get_or_init(|| helper.func_name())
            .clone()
    }

    pub(crate) fn big_used(&self, helper: BigHelper) -> bool {
        self.big[helper.slot()].get().is_some()
    }

    /// The float helper `helper`, marked for emission by this first use.
    pub(crate) fn flt_func(&self, helper: FltHelper) -> curios_wasm::FuncName {
        self.flt[helper.slot()]
            .get_or_init(|| helper.func_name())
            .clone()
    }

    pub(crate) fn flt_used(&self, helper: FltHelper) -> bool {
        self.flt[helper.slot()].get().is_some()
    }

    pub(crate) fn list_rope_type(&self) -> curios_wasm::TypeName {
        self.list_rope_type.clone()
    }

    /// The wasm-level type a value of the given wire type crosses the host boundary as, in either direction: the carrier `WireType::carrier` states, a scalar as its raw number and a reference as the concrete non-nullable heap type of its payload.
    ///
    /// **A scalar crosses as the number it is, both ways.** Going out, the call site narrows a `Nat` or `Int` through `LoadAs::WireNat`/`LoadAs::WireInt`, refusing past the wire, reads a `Bool` as its word and an `Flt` out of its box. Coming back, the guest boxes what the host answered (`Context::host_instrs`), because every box is a layout this crate defines: an `Flt`'s `$flt` struct, and a `Nat` or `Int` past the i31 the boxed magnitude. A host that minted either would be a second crate needing that layout, and one that could mint only the i31 would have to refuse a result past it.
    pub(crate) fn wire_type(&self, wire_type: &WireType) -> curios_wasm::ValType {
        match wire_type.carrier() {
            Carrier::I64 => curios_wasm::ValType::Num(curios_wasm::NumType::I64),
            Carrier::I32 => curios_wasm::ValType::Num(curios_wasm::NumType::I32),
            Carrier::F64 => curios_wasm::ValType::Num(curios_wasm::NumType::F64),
            Carrier::Bytes => concrete_val(self.bytes_type(), false),
            // A list of scalars is one flat element per value, narrowed and boxed by the guest as a scalar is; a list of references is their flat payloads. An option rides its element's list, null where it holds none.
            Carrier::Longs => concrete_val(self.longs_type(), wire_type.admits_null()),
            Carrier::Words => concrete_val(self.words_type.clone(), wire_type.admits_null()),
            Carrier::Elems => concrete_val(self.elems_type(), wire_type.admits_null()),
        }
    }

    pub(crate) fn bytes_type(&self) -> curios_wasm::TypeName {
        self.bytes_type.clone()
    }

    /// `$longs`, the flat payload a list of `Nat` or `Int` crosses the host boundary as.
    pub(crate) fn longs_type(&self) -> curios_wasm::TypeName {
        self.longs_type.clone()
    }

    /// The flat payload a list of the scalar `leaf` crosses the host boundary as: `$longs` for a `Nat` or `Int`, whose elements cross as a lone one does, and `$words` for a `Bool`.
    pub(crate) fn scalars_type(&self, leaf: WireLeaf) -> curios_wasm::TypeName {
        match leaf {
            WireLeaf::Nat | WireLeaf::Int => self.longs_type.clone(),
            WireLeaf::Bool => self.words_type.clone(),
            WireLeaf::Bytes | WireLeaf::Handle => {
                panic!("a list of `{leaf:?}` crosses as its payloads, never as scalars")
            }
        }
    }

    pub(crate) fn elems_type(&self) -> curios_wasm::TypeName {
        self.elems_type.clone()
    }

    /// The packed `Bytes` rope's name bundle.
    pub(crate) fn bytes_rope(&self) -> RopeData {
        RopeData {
            base: self.bytes_rope_type.clone(),
            enter: self.bytes_rope_enter_type.clone(),
            enter_or_null: self.bytes_rope_enter_or_null_type.clone(),
            leaf: self.bytes_rope_leaf_type.clone(),
            node: self.bytes_rope_node_type.clone(),
            view: self.bytes_rope_view_type.clone(),
            payload: self.bytes_type.clone(),
            room: self.rope_room_type.clone(),
            tag_field: curios_wasm::FieldName::from("tag"),
            len_field: curios_wasm::FieldName::from("len"),
            payload_field: curios_wasm::FieldName::from("bytes"),
            left_field: curios_wasm::FieldName::from("left"),
            right_field: curios_wasm::FieldName::from("right"),
            cache_field: curios_wasm::FieldName::from("cache"),
            room_field: curios_wasm::FieldName::from("room"),
            extent_field: curios_wasm::FieldName::from("extent"),
            base_field: curios_wasm::FieldName::from("base"),
            offset_field: curios_wasm::FieldName::from("offset"),
        }
    }

    /// The `List` rope's name bundle.
    pub(crate) fn list_rope(&self) -> RopeData {
        RopeData {
            base: self.list_rope_type.clone(),
            enter: self.list_rope_enter_type.clone(),
            enter_or_null: self.list_rope_enter_or_null_type.clone(),
            leaf: self.list_rope_leaf_type.clone(),
            node: self.list_rope_node_type.clone(),
            view: self.list_rope_view_type.clone(),
            payload: self.elems_type.clone(),
            room: self.rope_room_type.clone(),
            tag_field: curios_wasm::FieldName::from("tag"),
            len_field: curios_wasm::FieldName::from("len"),
            payload_field: curios_wasm::FieldName::from("elems"),
            left_field: curios_wasm::FieldName::from("left"),
            right_field: curios_wasm::FieldName::from("right"),
            cache_field: curios_wasm::FieldName::from("cache"),
            room_field: curios_wasm::FieldName::from("room"),
            extent_field: curios_wasm::FieldName::from("extent"),
            base_field: curios_wasm::FieldName::from("base"),
            offset_field: curios_wasm::FieldName::from("offset"),
        }
    }

    pub(crate) fn channel(&self) -> ChannelData {
        ChannelData {
            channel: curios_wasm::TypeName::from("channel"),
            payload: self.elems_type(),
            payload_field: curios_wasm::FieldName::from("payload"),
            head_field: curios_wasm::FieldName::from("head"),
            count_field: curios_wasm::FieldName::from("count"),
            closed_field: curios_wasm::FieldName::from("closed"),
        }
    }

    pub(crate) fn cell_type(&self) -> curios_wasm::TypeName {
        self.cell_type.clone()
    }

    /// The internal binding name of a store-described host function. First use during emission records the function as live; [`host_funcs`](Self::host_funcs) hands the recorded set to `emit_sys_imports`.
    ///
    /// A row's identity is its import name (see [`ForeignFunction`]), which is chosen outside the emitter, so the minted name embeds it, after the namespace every row imports under, beneath the reserved `host/` row prefix — a foreign name can never collide with a runtime helper or with another minted row.
    pub(crate) fn host_func(&self, function: &Arc<ForeignFunction>) -> curios_wasm::FuncName {
        let func_name =
            curios_wasm::FuncName::from(format!("host/{}/{}", Namespace::Ffi, function.name()));

        self.host_funcs
            .borrow_mut()
            .entry(func_name.as_string())
            .or_insert_with(|| Arc::clone(function));

        func_name
    }

    /// The foreign functions the emitted code referenced, in minted-name order.
    pub(crate) fn host_funcs(&self) -> Vec<Arc<ForeignFunction>> {
        self.host_funcs.borrow().values().cloned().collect()
    }

    /// The `sys.panic` import: a byte string in, no return. Declared by every module, since every module refuses somewhere.
    pub(crate) fn panic_func(&self) -> curios_wasm::FuncName {
        self.panic
            .get_or_init(|| curios_wasm::FuncName::from("panic"))
            .clone()
    }

    /// The instruction sequence a refusal is: a call to the class's helper and the `unreachable` that keeps the block's type. Spelled once for the code emitter, the region context and the rope helpers alike.
    pub(crate) fn refuse_instrs(&self, panic: curios_cps::Panic) -> Vec<curios_wasm::Instr> {
        vec![
            curios_wasm::Instr::Call {
                func_name: self.refuse_func(panic),
            },
            curios_wasm::Instr::Unreachable,
        ]
    }

    /// The reference on the stack as a `target`, a null passing where `nullable`: one `ref.cast` to a final type, and for a rope base the test of each final shape the base holds. Every load at a heap type enters it here.
    pub(crate) fn enter(
        &self,
        target: &curios_wasm::TypeName,
        nullable: bool,
    ) -> curios_wasm::Instr {
        for rope in [self.bytes_rope(), self.list_rope()] {
            if *target == rope.base {
                return self.rope_entry(&rope, nullable);
            }
        }
        curios_wasm::Instr::RefCast {
            ref_type: curios_wasm::RefType {
                is_nullable: nullable,
                heap_type: curios_wasm::HeapType::Concrete(target.clone()),
            },
        }
    }

    /// The reference on the stack as `rope`'s base, by its final shape: a branch out on `leaf`, `node` or `view`, and a refusal past them. Every object is a subtype of the non-final base, so a cast to the base is the engine's `is_subtype` call each time, where a test against a final shape is one inline comparison.
    fn rope_entry(&self, rope: &RopeData, admits_null: bool) -> curios_wasm::Instr {
        let label_name = curios_wasm::LabelName::from("rope_enter");
        let any = curios_wasm::RefType {
            is_nullable: true,
            heap_type: curios_wasm::HeapType::Abstract(curios_wasm::AbsHeapType::Any),
        };
        // A null passes the first test where it is admitted, a nullable target holding it.
        let mut instructions: Vec<_> = [&rope.leaf, &rope.node, &rope.view]
            .into_iter()
            .enumerate()
            .map(|(index, shape)| curios_wasm::Instr::BrOnCast {
                label_name: label_name.clone(),
                source_type: any.clone(),
                target_type: curios_wasm::RefType {
                    is_nullable: admits_null && index == 0,
                    heap_type: curios_wasm::HeapType::Concrete(shape.clone()),
                },
            })
            .collect();
        instructions.extend(self.refuse_instrs(curios_cps::Panic::Invariant));
        let block_type = match admits_null {
            true => rope.enter_or_null.clone(),
            false => rope.enter.clone(),
        };
        curios_wasm::Instr::Block {
            label_name,
            block_type: curios_wasm::BlockType::Concrete(block_type),
            instructions,
        }
    }

    /// A logical length or capacity computed unsigned in 64 bits, checked before it enters a 32-bit field or allocation. `wide` holds the producer once, so the guard neither repeats it nor lets its sum wrap.
    pub(crate) fn length_instrs(
        &self,
        producer: Vec<curios_wasm::Instr>,
        wide: &curios_wasm::LocalName,
    ) -> Vec<curios_wasm::Instr> {
        producer
            .into_iter()
            .chain([
                tee(wide),
                i64_const(u32::MAX.into()),
                curios_wasm::Instr::I64GtU,
                when(self.refuse_instrs(curios_cps::Panic::Capacity)),
                get(wide),
                curios_wasm::Instr::I32WrapI64,
            ])
            .collect()
    }

    /// Two represented lengths added exactly, then checked against the logical length bound before narrowing.
    pub(crate) fn length_sum_instrs(
        &self,
        lhs: Vec<curios_wasm::Instr>,
        rhs: Vec<curios_wasm::Instr>,
        wide: &curios_wasm::LocalName,
    ) -> Vec<curios_wasm::Instr> {
        self.length_instrs(
            lhs.into_iter()
                .chain([curios_wasm::Instr::I64ExtendI32U])
                .chain(rhs)
                .chain([
                    curios_wasm::Instr::I64ExtendI32U,
                    curios_wasm::Instr::I64Add,
                ])
                .collect(),
            wide,
        )
    }

    /// `$refuse/<class> () -> ()`: build the class's sentence from its data segment, hand it to `sys.panic`, and never return. First use marks it for emission, so nothing is allocated for a refusal until one fires.
    fn refuse_func(&self, panic: curios_cps::Panic) -> curios_wasm::FuncName {
        let slot = curios_cps::Panic::ALL
            .iter()
            .position(|class| *class == panic)
            .expect("`Panic::ALL` holds every class");

        self.refuse[slot]
            .get_or_init(|| refuse_func_name(panic))
            .clone()
    }

    /// The refusals the emitted code referenced, in `Panic::ALL`'s order. Read after every body that can refuse has been built — the rope helpers' included.
    pub(crate) fn refuse_funcs(&self) -> Vec<(curios_cps::Panic, curios_wasm::FuncName)> {
        curios_cps::Panic::ALL
            .into_iter()
            .zip(&self.refuse)
            .filter_map(|(class, slot)| Some((class, slot.get()?.clone())))
            .collect()
    }

    /// `$bytes/force (ref $rope/bytes) -> (ref $bytes)`: flatten a `Bytes` rope to its payload, memoizing in the entry node; the payload holds the run in its first `len` slots and may hold room past them. First use marks it for emission.
    pub(crate) fn bytes_force_func(&self) -> curios_wasm::FuncName {
        self.bytes_force
            .get_or_init(|| curios_wasm::FuncName::from("bytes/force"))
            .clone()
    }

    pub(crate) fn bytes_force_used(&self) -> bool {
        self.bytes_force.get().is_some()
    }

    /// `$bytes/write`, which `$bytes/force` calls and is emitted with it.
    pub(crate) fn bytes_write_func() -> curios_wasm::FuncName {
        curios_wasm::FuncName::from("bytes/write")
    }

    /// `$list/force (ref $rope/list) -> (ref $elems)`: the `List` mirror of [`bytes_force_func`](Self::bytes_force_func).
    pub(crate) fn list_force_func(&self) -> curios_wasm::FuncName {
        self.list_force
            .get_or_init(|| curios_wasm::FuncName::from("list/force"))
            .clone()
    }

    pub(crate) fn list_force_used(&self) -> bool {
        self.list_force.get().is_some()
    }

    /// `$list/write`, which `$list/force` calls and is emitted with it.
    pub(crate) fn list_write_func() -> curios_wasm::FuncName {
        curios_wasm::FuncName::from("list/write")
    }

    /// `$bytes/exact (ref $rope/bytes) -> (ref $bytes)`: a `Bytes` run as a payload exactly its length — the forced payload where its array is the run's, a copy of the run where it holds room past it. What leaves the rope as a flat array takes this: a leaf made of a forced payload, and a host operand.
    pub(crate) fn bytes_exact_func(&self) -> curios_wasm::FuncName {
        self.bytes_exact
            .get_or_init(|| curios_wasm::FuncName::from("bytes/exact"))
            .clone()
    }

    pub(crate) fn bytes_exact_used(&self) -> bool {
        self.bytes_exact.get().is_some()
    }

    /// `$list/exact (ref $rope/list) -> (ref $elems)`: the `List` mirror of [`bytes_exact_func`](Self::bytes_exact_func).
    pub(crate) fn list_exact_func(&self) -> curios_wasm::FuncName {
        self.list_exact
            .get_or_init(|| curios_wasm::FuncName::from("list/exact"))
            .clone()
    }

    pub(crate) fn list_exact_used(&self) -> bool {
        self.list_exact.get().is_some()
    }

    /// `$list/bytes/force (ref $rope/list) -> (ref $elems)`: force a `List(Bytes)` / `List(Handle)` host argument *deeply* — the outer rope to a fresh payload whose every element is itself forced to `$bytes`, the element shape the host lifts.
    pub(crate) fn list_bytes_force_func(&self) -> curios_wasm::FuncName {
        self.list_bytes_force
            .get_or_init(|| curios_wasm::FuncName::from("list/bytes/force"))
            .clone()
    }

    pub(crate) fn list_bytes_force_used(&self) -> bool {
        self.list_bytes_force.get().is_some()
    }

    /// `$list/empty`, one empty list every host answer of `none` enters the guest as, so an option a host answers empty allocates nothing.
    pub(crate) fn list_empty_global(&self) -> curios_wasm::GlobalName {
        self.list_empty
            .get_or_init(|| curios_wasm::GlobalName::from("list/empty"))
            .clone()
    }

    pub(crate) fn list_empty_used(&self) -> bool {
        self.list_empty.get().is_some()
    }

    /// `$bytes/embed (ref $bytes) -> (ref $rope/bytes)`: embed a host-built flat payload into a fresh leaf on re-entry.
    pub(crate) fn bytes_embed_func(&self) -> curios_wasm::FuncName {
        self.bytes_embed
            .get_or_init(|| curios_wasm::FuncName::from("bytes/embed"))
            .clone()
    }

    pub(crate) fn bytes_embed_used(&self) -> bool {
        self.bytes_embed.get().is_some()
    }

    /// `$bytes/box (ref null any) -> (ref $rope/bytes)`: a small-canonical `Bytes` as a rope — an immediate is materialised into a fresh leaf, a rope passes through. The entry every rope-shaped consumer pays where a bare `ref.cast` would reject the immediate form.
    pub(crate) fn bytes_box_func(&self) -> curios_wasm::FuncName {
        self.bytes_box
            .get_or_init(|| curios_wasm::FuncName::from("bytes/box"))
            .clone()
    }

    pub(crate) fn bytes_box_used(&self) -> bool {
        self.bytes_box.get().is_some()
    }

    /// `$bytes/norm (ref $rope/bytes) -> (ref any)`: the canonical form of a byte rope — at most 3 bytes becomes the i31 (length in the top 2 payload bits, bytes LSB-first below), anything longer passes through. Every `Bytes` producer answers through this, which is what makes a mixed-representation pair unrepresentable.
    pub(crate) fn bytes_norm_func(&self) -> curios_wasm::FuncName {
        self.bytes_norm
            .get_or_init(|| curios_wasm::FuncName::from("bytes/norm"))
            .clone()
    }

    pub(crate) fn bytes_norm_used(&self) -> bool {
        self.bytes_norm.get().is_some()
    }

    /// `$list/embed (ref $elems) -> (ref $rope/list)`: the `List` mirror of [`bytes_embed_func`](Self::bytes_embed_func), for scalar-element results.
    pub(crate) fn list_embed_func(&self) -> curios_wasm::FuncName {
        self.list_embed
            .get_or_init(|| curios_wasm::FuncName::from("list/embed"))
            .clone()
    }

    pub(crate) fn list_embed_used(&self) -> bool {
        self.list_embed.get().is_some()
    }

    /// `$list/<leaf>/to_<payload>`, the helper a list of `leaf` crosses to a host through, marked for emission by this first use.
    pub(crate) fn scalars_force_func(&self, leaf: WireLeaf) -> curios_wasm::FuncName {
        record_leaf(&self.scalars_forces, leaf);
        curios_wasm::FuncName::from(format!(
            "list/{}/to_{}",
            scalar_leaf_name(leaf),
            self.scalars_type(leaf)
        ))
    }

    /// The leaves whose `to_<payload>` helper the emitted code referenced.
    pub(crate) fn scalars_forces(&self) -> Vec<WireLeaf> {
        self.scalars_forces.borrow().clone()
    }

    /// `$list/<leaf>/of_<payload>`, the helper a host's list of `leaf` comes back through, marked for emission by this first use.
    pub(crate) fn scalars_embed_func(&self, leaf: WireLeaf) -> curios_wasm::FuncName {
        record_leaf(&self.scalars_embeds, leaf);
        curios_wasm::FuncName::from(format!(
            "list/{}/of_{}",
            scalar_leaf_name(leaf),
            self.scalars_type(leaf)
        ))
    }

    /// The leaves whose `of_<payload>` helper the emitted code referenced.
    pub(crate) fn scalars_embeds(&self) -> Vec<WireLeaf> {
        self.scalars_embeds.borrow().clone()
    }

    /// Box one raw scalar the host answered, alone or as a list's element: a `Nat`'s `i64` read unsigned through `big/of_u64` and an `Int`'s read signed through `big/of_i64`, each answering the i31 below `2³⁰` in magnitude and the boxed magnitude past it; a `Bool` is its own i31. An `Flt` stays the raw `f64` its continuation takes, since `curios-cps` offers that parameter at its carrier.
    pub(crate) fn box_word_instrs(&self, wire_type: &WireType) -> Vec<curios_wasm::Instr> {
        match wire_type {
            WireType::Nat => vec![call(&self.big_func(BigHelper::OfU64))],
            WireType::Int => vec![call(&self.big_func(BigHelper::OfI64))],
            WireType::Bool | WireType::Byte => vec![curios_wasm::Instr::RefI31],
            _ => vec![],
        }
    }

    /// `$list/bytes/embed (ref $elems) -> (ref $rope/list)`: embed a `List(Bytes)` host result *deeply* — each raw `$bytes` element into a leaf (in place; the host-built array is fresh), then the outer array.
    pub(crate) fn list_bytes_embed_func(&self) -> curios_wasm::FuncName {
        self.list_bytes_embed
            .get_or_init(|| curios_wasm::FuncName::from("list/bytes/embed"))
            .clone()
    }

    pub(crate) fn list_bytes_embed_used(&self) -> bool {
        self.list_bytes_embed.get().is_some()
    }

    /// `$bytes/slice (ref $rope/bytes, i32, i32) -> (ref $rope/bytes)`, taking a start and a *count*: the `Bytes` view constructor, O(1) over a flat-available base — bounds-check, answer the empty leaf or the whole rope on the trivial windows, collapse a view-of-view, and force an uncached node base, O(n) once and memoized, so every `view` it builds reads through in O(1).
    pub(crate) fn bytes_slice_func(&self) -> curios_wasm::FuncName {
        self.bytes_slice
            .get_or_init(|| curios_wasm::FuncName::from("bytes/slice"))
            .clone()
    }

    pub(crate) fn bytes_slice_used(&self) -> bool {
        self.bytes_slice.get().is_some()
    }

    /// `$list/slice (ref $rope/list, i32, i32) -> (ref $rope/list)`: the `List` mirror of [`bytes_slice_func`](Self::bytes_slice_func).
    pub(crate) fn list_slice_func(&self) -> curios_wasm::FuncName {
        self.list_slice
            .get_or_init(|| curios_wasm::FuncName::from("list/slice"))
            .clone()
    }

    pub(crate) fn list_slice_used(&self) -> bool {
        self.list_slice.get().is_some()
    }

    /// `$bytes/read (ref $rope/bytes, i32) -> i32`: one byte read — straight off a leaf payload, through a `view`'s window without forcing, off a node's cache, and at an uncached node from the child that holds the byte where that child is flat-available, with `$bytes/force` (memoized) only where it is not.
    pub(crate) fn bytes_read_func(&self) -> curios_wasm::FuncName {
        self.bytes_read
            .get_or_init(|| curios_wasm::FuncName::from("bytes/read"))
            .clone()
    }

    pub(crate) fn bytes_read_used(&self) -> bool {
        self.bytes_read.get().is_some()
    }

    /// `$list/read (ref $rope/list, i32) -> anyref`: the `List` mirror of [`bytes_read_func`](Self::bytes_read_func).
    pub(crate) fn list_read_func(&self) -> curios_wasm::FuncName {
        self.list_read
            .get_or_init(|| curios_wasm::FuncName::from("list/read"))
            .clone()
    }

    pub(crate) fn list_read_used(&self) -> bool {
        self.list_read.get().is_some()
    }

    /// `$bytes/eql (ref $rope/bytes, ref $rope/bytes) -> i32`: whole-value byte equality — unequal rope lengths answer without forcing, equal lengths force both payloads once and compare bytewise.
    pub(crate) fn bytes_eql_func(&self) -> curios_wasm::FuncName {
        self.bytes_eql
            .get_or_init(|| curios_wasm::FuncName::from("bytes/eql"))
            .clone()
    }

    pub(crate) fn bytes_eql_used(&self) -> bool {
        self.bytes_eql.get().is_some()
    }

    /// `$bytes/and (ref $payload, ref $payload, i32) -> (ref $rope/bytes)`: two forced payloads combined byte for byte, sealed at the logical length the caller supplies.
    ///
    /// The operands arrive forced, so this is a walk over two byte arrays.
    pub(crate) fn bytes_and_func(&self) -> curios_wasm::FuncName {
        self.bytes_and
            .get_or_init(|| curios_wasm::FuncName::from("bytes/and"))
            .clone()
    }

    pub(crate) fn bytes_and_used(&self) -> bool {
        self.bytes_and.get().is_some()
    }

    /// The disjunction, as [`bytes_and_func`](Self::bytes_and_func).
    pub(crate) fn bytes_or_func(&self) -> curios_wasm::FuncName {
        self.bytes_or
            .get_or_init(|| curios_wasm::FuncName::from("bytes/or"))
            .clone()
    }

    pub(crate) fn bytes_or_used(&self) -> bool {
        self.bytes_or.get().is_some()
    }

    /// The difference, as [`bytes_and_func`](Self::bytes_and_func).
    pub(crate) fn bytes_xor_func(&self) -> curios_wasm::FuncName {
        self.bytes_xor
            .get_or_init(|| curios_wasm::FuncName::from("bytes/xor"))
            .clone()
    }

    pub(crate) fn bytes_xor_used(&self) -> bool {
        self.bytes_xor.get().is_some()
    }

    /// `$bytes/replicate (i32 count, i32 atom) -> (ref $rope/bytes)`: `count` copies of one byte, as one flat leaf.
    pub(crate) fn bytes_replicate_func(&self) -> curios_wasm::FuncName {
        self.bytes_replicate
            .get_or_init(|| curios_wasm::FuncName::from("bytes/replicate"))
            .clone()
    }

    pub(crate) fn bytes_replicate_used(&self) -> bool {
        self.bytes_replicate.get().is_some()
    }

    /// `$list/map (ref $rope/list, ref $envr/1) -> (ref $rope/list)`: apply a unary closure to every element of the forced payload, filling a fresh leaf.
    pub(crate) fn list_map_func(&self) -> curios_wasm::FuncName {
        self.list_map
            .get_or_init(|| curios_wasm::FuncName::from("list/map"))
            .clone()
    }

    pub(crate) fn list_map_used(&self) -> bool {
        self.list_map.get().is_some()
    }

    pub(crate) fn tuple_types(&self) -> impl Iterator<Item = (usize, curios_wasm::TypeName)> {
        self.tuple_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    /// Every declared row type, with the carrier of each slot its struct holds.
    pub(crate) fn row_types(
        &self,
    ) -> impl Iterator<
        Item = (
            curios_cps::RowId,
            curios_wasm::TypeName,
            &[curios_cps::Slot],
        ),
    > {
        self.row_types
            .iter()
            .map(|(row, (type_name, slots))| (*row, type_name.clone(), slots.as_slice()))
    }

    /// How a value is loaded to fill `slot`, and how a read of it is coerced back.
    ///
    /// A typed reference slot admits null, because the slots a narrow constructor leaves unwritten hold one; every other carrier is loaded exactly as any position naming it.
    pub(crate) fn slot_load_as(&self, slot: curios_cps::Slot) -> LoadAs {
        match slot {
            curios_cps::Slot::Tag | curios_cps::Slot::Nat => LoadAs::Nat,
            curios_cps::Slot::Flt => LoadAs::Flt,
            curios_cps::Slot::List => LoadAs::ConcreteOrNull(self.list_rope().base.clone()),
            curios_cps::Slot::Closure(arity) => LoadAs::ConcreteOrNull(self.find_envr_type(arity)),
            curios_cps::Slot::Row(row) => LoadAs::ConcreteOrNull(self.find_row_type(row)),
            curios_cps::Slot::Opaque => LoadAs::Null,
        }
    }

    /// The carriers of `row`'s slots.
    pub(crate) fn row_slots(&self, row: curios_cps::RowId) -> &[curios_cps::Slot] {
        &self
            .row_types
            .get(&row)
            .unwrap_or_else(|| panic!("`Table` lacks a type for row `{}`", row))
            .1
    }

    pub(crate) fn find_row_type(&self, row: curios_cps::RowId) -> curios_wasm::TypeName {
        self.row_types
            .get(&row)
            .unwrap_or_else(|| panic!("`Table` lacks a type for row `{}`", row))
            .0
            .clone()
    }

    pub(crate) fn find_tuple_type(&self, arity: usize) -> curios_wasm::TypeName {
        self.tuple_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks tuple type for arity `{}`", arity))
            .clone()
    }

    pub(crate) fn tuple_field(index: usize) -> curios_wasm::FieldName {
        curios_wasm::FieldName::from(index.to_string())
    }

    pub(crate) fn envr_types(&self) -> impl Iterator<Item = curios_wasm::TypeName> {
        self.envr_types.values().cloned()
    }

    pub(crate) fn find_envr_type(&self, arity: usize) -> curios_wasm::TypeName {
        self.envr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks environment type for arity `{}`", arity))
            .clone()
    }

    pub(crate) fn clsr_types(&self) -> impl Iterator<Item = (usize, curios_wasm::TypeName)> {
        self.clsr_types
            .iter()
            .map(|(arity, type_name)| (*arity, type_name.clone()))
    }

    pub(crate) fn find_clsr_type(&self, arity: usize) -> curios_wasm::TypeName {
        self.clsr_types
            .get(&arity)
            .unwrap_or_else(|| panic!("`Table` lacks closure type for arity `{}`", arity))
            .clone()
    }

    pub(crate) fn func_types(&self) -> impl Iterator<Item = (&Signature, curios_wasm::TypeName)> {
        self.func_types
            .iter()
            .map(|(signature, type_name)| (signature, type_name.clone()))
    }

    pub(crate) fn find_func_type(&self, signature: &Signature) -> curios_wasm::TypeName {
        self.func_types
            .get(signature)
            .unwrap_or_else(|| panic!("`Table` lacks function type for `{signature:?}`"))
            .clone()
    }

    /// Whether this name is a module const holding a `Tuple`/`List`. See `Context::refuse_raw_aggregate`.
    pub(crate) fn is_aggregate_const(&self, const_name: &EmissionValueName) -> bool {
        self.const_aggregates.contains(const_name)
    }

    /// The carrier a scalar const is held at in a register and the instruction that is its value there, or `None` for a const no register holds.
    pub(crate) fn const_register(
        &self,
        const_name: &EmissionValueName,
    ) -> Option<(curios_cps::Repr, curios_wasm::Instr)> {
        self.const_registers.get(const_name).cloned()
    }

    pub(crate) fn find_const(&self, const_name: &EmissionValueName) -> curios_wasm::GlobalName {
        self.consts
            .get(const_name)
            .unwrap_or_else(|| panic!("`Table` lacks const `{}`", const_name))
            .clone()
    }

    /// The carrier this value is held at in a register, or `None` when it is held behind a reference.
    pub(crate) fn raw_carrier(&self, value_name: &EmissionValueName) -> Option<curios_cps::Repr> {
        self.raw.get(value_name).copied()
    }

    /// The wasm type this value's local is declared at: the machine type of its carrier when it is held in a register, and the top reference type otherwise.
    ///
    /// The reference arms are unreachable — the analysis only ever answers a scalar carrier, since those are the only ones a register holds — and they answer `top_type` rather than panicking because a representation that cannot be held raw and a value that was never offered one are the same fact, and this function's job is to state it once.
    pub(crate) fn local_type(&self, value_name: &EmissionValueName) -> curios_wasm::ValType {
        match self.raw_carrier(value_name) {
            Some(curios_cps::Repr::Nat | curios_cps::Repr::Length) => {
                curios_wasm::ValType::Num(curios_wasm::NumType::I32)
            }
            Some(curios_cps::Repr::Flt) => curios_wasm::ValType::Num(curios_wasm::NumType::F64),
            Some(
                curios_cps::Repr::Number
                | curios_cps::Repr::Bytes
                | curios_cps::Repr::List
                | curios_cps::Repr::Ref,
            )
            | None => self
                .static_type(value_name)
                .map_or(Table::top_type(true), |(type_name, _)| {
                    concrete_val(type_name, true)
                }),
        }
    }

    /// The heap type `value_name` is declared at where it is narrower than the uniform reference, with the arity of the closures it holds: a closure construction's own environment, and the arity's environment base for a value the representation analysis knows holds closures of it.
    pub(crate) fn static_type(
        &self,
        value_name: &EmissionValueName,
    ) -> Option<(curios_wasm::TypeName, usize)> {
        if let Some(closure) = self.constructed.get(value_name) {
            let data = self.find_clsr(closure);
            return Some((data.envr_type(), data.arity()));
        }
        self.closures
            .get(value_name)
            .map(|&arity| (self.find_envr_type(arity), arity))
    }

    /// Whether a value declared at `value_name`'s own type is already a `target`, so entering it as one casts nothing: its own environment type, or its arity's base.
    pub(crate) fn is_static(
        &self,
        value_name: &EmissionValueName,
        target: &curios_wasm::TypeName,
    ) -> bool {
        self.static_type(value_name)
            .is_some_and(|(type_name, arity)| {
                type_name == *target || self.find_envr_type(arity) == *target
            })
    }

    pub(crate) fn find_clsr(&self, clsr_name: &EmissionClosureName) -> &ClsrData<'a> {
        self.clsrs
            .get(clsr_name)
            .unwrap_or_else(|| panic!("`Table` lacks closure `{}`", clsr_name))
    }

    pub(crate) fn find_func(&self, func_name: &EmissionFunctionName) -> &FuncData<'a> {
        self.funcs
            .get(func_name)
            .unwrap_or_else(|| panic!("`Table` lacks func `{}`", func_name))
    }
}

#[cfg(test)]
mod tests;

/// Record `leaf` once, in first-use order.
fn record_leaf(used: &RefCell<Vec<WireLeaf>>, leaf: WireLeaf) {
    let mut used = used.borrow_mut();
    if !used.contains(&leaf) {
        used.push(leaf);
    }
}

/// How a scalar leaf is spelled in its helpers' names.
fn scalar_leaf_name(leaf: WireLeaf) -> &'static str {
    match leaf {
        WireLeaf::Nat => "nat",
        WireLeaf::Int => "int",
        WireLeaf::Bool => "bool",
        WireLeaf::Bytes | WireLeaf::Handle => {
            panic!("a list of `{leaf:?}` crosses as its payloads, never as scalars")
        }
    }
}
