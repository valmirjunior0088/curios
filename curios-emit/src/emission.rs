//! The private emission model: the structured program `machine` produces and the emitters consume — bodies of bindings, the labeled join blocks their jumps enter, and the one transfer that ends each.

use {
    super::{EmissionBlockName, EmissionClosureName, EmissionFunctionName, EmissionValueName},
    curios_abi::ForeignFunction,
    curios_num::{Binary, Grain, Integer, Natural},
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// A constructed value: the scalar immediates, packed `Bits`/`Bytes`, `List`/`Tuple` aggregates, a nominal `Row`, and `Closure` naming its definition plus the captures filling its environment. Aggregates are flat — elements are names of already-bound values, never nested `EmissionData` — so constructing one is a single allocation. `EmissionData` is also the payload of module consts, where codegen materialises it into a wasm global.
#[derive(Debug, Clone)]
pub(crate) enum EmissionData {
    Nat(Natural),
    Int(Integer),
    Flt(f64),
    Bin(Grain, Binary),
    List(Vec<EmissionValueName>),
    Tuple(Vec<EmissionValueName>),
    /// A nominal row's construction, at that row's full width. Emitted as the row's own final struct type rather than an arity-keyed `$tuple/N`, which is what makes every read of it an exact cast.
    Row(curios_cont::RowId, Vec<EmissionArg>),
    Closure(EmissionClosureName, Vec<EmissionValueName>),
}

/// One pure intrinsic computation over already-bound values: every variant produces exactly one value and has no effects, anything stateful being an [`EmissionTail`].
#[derive(Debug, Clone)]
pub(crate) enum EmissionCode {
    /// One [`curios_cont::Intrinsic`] over its operands, in the order and arity the op fixes — verified at the CPS boundary, so codegen indexes the vector directly. The scalar rows (`Nat*`/`Int*`/`Flt*`) lower to the wasm ops they mirror one-for-one; the `Bin*`/`List*` rows are rope operations codegen services through shared helper functions.
    Intrinsic(curios_cont::Intrinsic, Vec<EmissionValueName>),
    // `ListMap(src, f)`: map closure `f` over list `src` into a fresh list of the same length. Codegen lowers it to the shared `$list/map` rope helper: one allocation, one fill loop applying `f` per slot via `call_indirect`.
    /// The list-map runtime helper call: list first, mapper second. Not an [`Intrinsic`](Self::Intrinsic) member because it is no [`curios_cont::Intrinsic`] upstream — it runs a closure, so CPS carries it as the call-shaped `curios_cont::IntrinsicCall` with a return continuation, and only structurization collapses it to a pure helper call.
    ListMap(EmissionValueName, EmissionValueName),
}

/// The right-hand side of one binding in the emission body.
#[derive(Debug, Clone)]
pub(crate) enum EmissionValue {
    Pure(EmissionData),
    Eval(EmissionCode),
}

/// A labeled join point inside a region: `params` are its block parameters — the SSA replacement for φ-nodes, bound afresh on every entry from a [`EmissionJumpTarget`] — and `region` its body. Blocks are also how calls continue: a call's `resume` names the block that receives its result, and a backward jump to a loop header block is how the loops contification leaves behind close.
#[derive(Debug, Clone)]
pub(crate) struct EmissionBlock {
    pub(crate) params: Vec<EmissionValueName>,
    pub(crate) region: EmissionBody,
}

/// One branch edge: the destination block plus the arguments feeding its parameters, positionally. This is the IR's φ-mechanism — a block parameter's value is whatever the taken edge passed for it.
#[derive(Debug, Clone)]
pub(crate) struct EmissionJumpTarget {
    pub(crate) target: EmissionBlockName,
    pub(crate) params: Vec<EmissionArg>,
}

/// What one edge passes for one block parameter.
///
/// A named value in every ordinary case. [`EmissionArg::Filler`] is absence: null wherever the destination is a reference, and, where the destination is a parameter the representation analysis holds in a register, that register's zero — the one case a null cannot land, decided here because the carrier is decided after the filler was written.
#[derive(Debug, Clone)]
pub(crate) enum EmissionArg {
    Value(EmissionValueName),
    /// No value: the slot belongs to a wider constructor than this edge's, and is a reference slot, so what the unwritten field holds is null.
    Filler,
}

/// A multi-way branch on a scalar: `operand` is read as an unsigned `u32` (a constructor tag or nat), each case maps one value to its edge, and `default` catches the rest — `None` means the cases are exhaustive, and a match with no cases and no default lowers to a trap. Codegen picks the dispatch shape (a `br_table` for dense-from-zero tags, an `if` for two-way, a binary search otherwise); the IR just states the table.
#[derive(Debug, Clone)]
pub(crate) struct EmissionMatchTarget {
    pub(crate) operand: EmissionValueName,
    pub(crate) cases: BTreeMap<u32, EmissionJumpTarget>,
    pub(crate) default: Option<EmissionJumpTarget>,
}

/// A user-code call in tail position: `Direct` names a known function, `Indirect` invokes a closure value through the shared closure type of its arity (`call_indirect` through the module's closure table). Both name the `resume` block, which receives the callee's results as its parameters — one for a closure call, the callee's result count for a direct one — except when `resume` is the enclosing body's return sentinel, where the call is a genuine tail call and codegen emits `return_call`/`return_call_indirect` instead of branching.
#[derive(Debug, Clone)]
pub(crate) enum EmissionCallTarget {
    Direct {
        target: EmissionFunctionName,
        params: Vec<EmissionArg>,
        resume: EmissionBlockName,
    },
    Indirect {
        target: EmissionValueName,
        params: Vec<EmissionArg>,
        resume: EmissionBlockName,
    },
}

/// A host-provided intrinsic in tail position. Returning foreign calls carry the block that receives their results; a diverging one does not. Purity analysis treats any `EmissionTail::Host` as the impure boundary of its enclosing region tree.
#[derive(Debug, Clone)]
pub(crate) enum EmissionHostTarget {
    /// A store-described host call: `function`'s `WireSignature` fixes the operand order/types and the resume shape — `resume` takes one block parameter per signature result, so a multi-result record arrives as parallel block parameters.
    Foreign {
        function: Arc<ForeignFunction>,
        operands: Vec<EmissionValueName>,
        resume: EmissionBlockName,
    },
    /// A call to a host row that diverges: `function`'s signature fixes the operands, and nothing resumes after it.
    Halt {
        function: Arc<ForeignFunction>,
        operands: Vec<EmissionValueName>,
    },
}

/// A guest write-once cell op in tail position. Same `resume` discipline as `EmissionHostTarget`, but serviced inline in codegen (no host import). Purity analysis treats any `EmissionTail::Cell` as an impure boundary, like `Host`.
#[derive(Debug, Clone)]
pub(crate) enum EmissionCellTarget {
    /// An empty cell, represented by a null field.
    Reserve { resume: EmissionBlockName },
    Fill {
        cell: EmissionValueName,
        value: EmissionValueName,
        resume: EmissionBlockName,
    },
    Poll {
        cell: EmissionValueName,
        resume: EmissionBlockName,
    },
}

impl EmissionCellTarget {
    pub(crate) fn resume(&self) -> &EmissionBlockName {
        match self {
            EmissionCellTarget::Reserve { resume }
            | EmissionCellTarget::Fill { resume, .. }
            | EmissionCellTarget::Poll { resume, .. } => resume,
        }
    }
}

/// A bounded guest channel operation, performed inline and delivering its results to `resume`.
#[derive(Debug, Clone)]
pub(crate) struct EmissionChannelTarget {
    pub(crate) op: curios_cont::ChannelOp,
    pub(crate) args: Vec<EmissionValueName>,
    pub(crate) resume: EmissionBlockName,
}

/// The sole control transfer out of a region — a region never falls through. `Jump` and `Match` stay within the body; `Call` transfers to user code; `Host` and `Cell` are the effectful intrinsics (the only impurity the IR admits — purity analysis marks a region tree impure exactly when one appears in it); `Panic` reports a failure of its class and stops; `Unreachable` traps, marking a path that cannot be taken (an absurd match).
#[derive(Debug, Clone)]
pub(crate) enum EmissionTail {
    Jump(EmissionJumpTarget),
    Match(EmissionMatchTarget),
    Call(EmissionCallTarget),
    Host(EmissionHostTarget),
    Cell(EmissionCellTarget),
    Channel(EmissionChannelTarget),
    Panic(curios_cont::Panic),
    Unreachable,
}

/// One straight-line body fragment and the sub-structure hanging off it: bindings evaluated in order, the labeled join blocks its jumps enter, and the single [`EmissionTail`] transfer that ends it. All control flow in a body lives in its region tree — an [`EmissionValue`] binding never branches.
#[derive(Debug, Clone)]
pub(crate) struct EmissionBody {
    pub(crate) values: Vec<(EmissionValueName, EmissionValue)>,
    pub(crate) blocks: Vec<(EmissionBlockName, EmissionBlock)>,
    pub(crate) tail: EmissionTail,
}

impl EmissionBody {
    /// Collect the arity of every *indirect* call site in this region (and its nested blocks). A closure of that arity is invoked here even when the optimizer has specialized its definition away (a higher-order function's argument inlined, dropping the only closure of that arity while a `call_indirect` in its body survives), so a closure type for the arity is needed even though no closure of it is defined.
    fn collect_indirect_arities(&self, out: &mut BTreeSet<usize>) {
        if let EmissionTail::Call(EmissionCallTarget::Indirect { params, .. }) = &self.tail {
            out.insert(params.len());
        }

        for (_, block) in &self.blocks {
            block.region.collect_indirect_arities(out);
        }
    }
}

/// A function or closure argument in the private emission model.
#[derive(Debug, Clone)]
pub(crate) struct EmissionBinder {
    pub(crate) name: EmissionValueName,
}

impl From<EmissionValueName> for EmissionBinder {
    fn from(name: EmissionValueName) -> Self {
        Self { name }
    }
}

/// A closure definition: `fields` is the captured environment — filled where a `EmissionData::Closure` names this definition, read inside the body like extra bindings — and `params` the call arguments. `resume` is the return sentinel, exactly as on [`EmissionFunction`]. Codegen gives each definition its own environment struct type but files its function under the shared closure type for its arity, which is what lets a [`EmissionCallTarget::Indirect`] call any closure of matching arity.
#[derive(Debug, Clone)]
pub(crate) struct EmissionClosure {
    pub(crate) fields: Vec<EmissionBinder>,
    pub(crate) params: Vec<EmissionBinder>,
    pub(crate) resume: EmissionBlockName,
    pub(crate) region: EmissionBody,
}

/// A top-level function: parameters, body, and its `resume` sentinel — the block name that means "return": a jump to `resume` *is* the function's return, so a call whose resume block is the enclosing sentinel is a tail call by construction (codegen emits `return_call`), no separate tail-position analysis needed.
#[derive(Debug, Clone)]
pub(crate) struct EmissionFunction {
    pub(crate) params: Vec<EmissionBinder>,
    /// How many values this function hands back: one, unless a return protocol delivers a constructor as its fields — which is why the wasm type is keyed on the shape rather than on the parameter count alone.
    pub(crate) results: usize,
    pub(crate) resume: EmissionBlockName,
    pub(crate) region: EmissionBody,
}

/// One whole program in the emission model: module-level consts (hoisted literals), the closure and function definitions, and the blessed entrypoint. Bodies reference consts and each other by name, and definition order is preserved end-to-end — it is the printed order and the wasm emission order. The collections are private to this module: structurization grows a module through the `add_*` methods, and the hoister, a child module, rewrites its bodies in place.
#[derive(Debug, Default, Clone)]
pub(crate) struct EmissionModule {
    consts: Vec<(EmissionValueName, EmissionData)>,
    clsrs: Vec<(EmissionClosureName, EmissionClosure)>,
    funcs: Vec<(EmissionFunctionName, EmissionFunction)>,
    entry: Option<EmissionFunctionName>,
    /// The nominal rows this module constructs and reads, each with its debug name and slot carriers. See [`EmissionData::Row`].
    rows: Vec<(curios_cont::RowId, curios_cont::Row)>,
}

impl EmissionModule {
    /// An empty module: no definitions, no entrypoint.
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn rows(&self) -> &[(curios_cont::RowId, curios_cont::Row)] {
        &self.rows
    }

    pub(crate) fn set_rows(&mut self, rows: Vec<(curios_cont::RowId, curios_cont::Row)>) {
        self.rows = rows;
    }

    pub(crate) fn consts(&self) -> &[(EmissionValueName, EmissionData)] {
        &self.consts
    }

    pub(crate) fn set_consts(&mut self, consts: Vec<(EmissionValueName, EmissionData)>) {
        self.consts = consts;
    }

    /// The closure definitions, in insertion order.
    pub(crate) fn clsrs(&self) -> &[(EmissionClosureName, EmissionClosure)] {
        &self.clsrs
    }

    /// Append a closure definition under its name; `EmissionData::Closure` values cite that name to instantiate it.
    pub(crate) fn add_clsr(&mut self, clsr_name: EmissionClosureName, clsr: EmissionClosure) {
        self.clsrs.push((clsr_name, clsr));
    }

    /// The function definitions, in insertion order — the order they print and emit in.
    pub(crate) fn funcs(&self) -> &[(EmissionFunctionName, EmissionFunction)] {
        &self.funcs
    }

    /// Append a function definition under its name; the position it lands in is the position it keeps.
    pub(crate) fn add_func(&mut self, func_name: EmissionFunctionName, func: EmissionFunction) {
        self.funcs.push((func_name, func));
    }

    /// Every definition's body, the closures' then the functions', each in insertion order — what a pass rewriting bodies in place walks.
    pub(crate) fn regions_mut(&mut self) -> impl Iterator<Item = &mut EmissionBody> {
        self.clsrs
            .iter_mut()
            .map(|(_, clsr)| &mut clsr.region)
            .chain(self.funcs.iter_mut().map(|(_, func)| &mut func.region))
    }

    /// Every closure arity the module needs closure types for: the arities of the surviving closure definitions, unioned with the arities of indirect call sites (whose target definition may have been inlined away). Sizing closure types from definitions alone misses the latter, leaving a surviving `call_indirect` with no declared type for its arity.
    pub(crate) fn clsr_arities(&self) -> BTreeSet<usize> {
        let mut arities = BTreeSet::new();

        // A row slot declared at a closure arity names that arity's environment type, so the roster has to reach it whether or not the module ever builds a closure of that arity.
        for (_, row) in &self.rows {
            for slot in &row.slots {
                if let curios_cont::Slot::Closure(arity) = slot {
                    arities.insert(*arity);
                }
            }
        }

        for (_, clsr) in &self.clsrs {
            arities.insert(clsr.params.len());
            clsr.region.collect_indirect_arities(&mut arities);
        }

        for (_, func) in &self.funcs {
            func.region.collect_indirect_arities(&mut arities);
        }

        arities
    }

    /// The entrypoint function — the program's sole root: the value the host invokes, the only export, and the seed of dead-code reachability. Recorded here so passes consult the module instead of re-deriving a blessed name.
    pub(crate) fn entry(&self) -> Option<&EmissionFunctionName> {
        self.entry.as_ref()
    }

    /// Bless `func_name` as the entrypoint. Set once by `structurize` (always `main`); the module does not check the function exists — the structurizer adds it separately.
    pub(crate) fn set_entry(&mut self, func_name: EmissionFunctionName) {
        self.entry = Some(func_name);
    }
}
