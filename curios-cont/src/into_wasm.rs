use {
    crate::{
        CpsFamilyId, CpsIntrinsic, CpsModule, Repr, cps::represent, machine::lower,
        machine::structurize, machine::value_id, machine::value_name,
    },
    curios_abi::ForeignFunction,
    curios_utilities::{Grain, PackedBin},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        sync::Arc,
    },
};

mod symbols;
pub(crate) use symbols::*;

mod table;
use table::*;

mod frame;
use frame::*;

mod context;
use context::*;

mod code_emitter;
use code_emitter::*;

mod structure;
use structure::*;

mod expr_emitter;
use expr_emitter::*;

mod hoist;
use hoist::*;

mod module_emitter;
use module_emitter::*;

mod rope_emitter;
use rope_emitter::*;

mod types;
pub use types::*;

#[cfg(test)]
mod tests;

/// Lower an optimized CPS module to a wasm-GC module — the pipeline's final stage. The private machine CFG is built and its reducible control structurized into blocks and loops, then a `Table` is computed over the whole module (the name maps, the closure type per `clsr_arities` arity, tuple arities, rope helpers) and `ModuleEmitter` declares the host imports and emits every const, closure, and function, exporting the entry under its emitted name (`func/main` — the entry is always `main`).
pub fn into_wasm(module: &CpsModule) -> curios_wasm::Module {
    curios_profile::profile!("into_wasm");
    let raw = raw_locals(module);
    let machine = lower(module);
    let mut structured = structurize(&machine);
    hoist_consts(&mut structured);
    let mut wasm_module = curios_wasm::Module::new("module");
    ModuleEmitter::new(&Table::new(&structured, &raw), &mut wasm_module).emit_module(&structured);

    wasm_module
}

/// The emission names the representation analysis decided to hold raw, and at which carrier.
///
/// Translated here rather than threaded through the lowerings because both hops are total functions of an index: a machine value *is* its CPS value, and its emission name is that index spelled. Nothing has to be carried along to reconstruct it. A name codegen mints for itself — a hoisted literal, a wrapper argument, a closure shell — is not a CPS value, is therefore absent, and is held behind a reference, which is the correct default.
fn raw_locals(module: &CpsModule) -> HashMap<EmissionValueName, Repr> {
    represent::storage(module)
        .into_iter()
        .filter_map(|(value, storage)| {
            storage
                .raw_carrier()
                .map(|carrier| (value_name(value_id(value)), carrier))
        })
        .collect()
}

/// A constructed value: the scalar immediates, packed `Bits`/`Bytes`, `List`/`Tuple` aggregates, and `EmissionClosure` naming its definition plus the captures filling its environment. Aggregates are flat — elements are names of already-bound values, never nested `EmissionData` — so constructing one is a single allocation. `EmissionData` is also the payload of module consts, where codegen materialises it into a wasm global.
#[derive(Debug, Clone)]
pub(crate) enum EmissionData {
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Grain, PackedBin),
    List(Vec<EmissionValueName>),
    Tuple(Vec<EmissionValueName>),
    /// A variant construction at its family's width. Emitted as the family's own final struct type rather than an arity-keyed `$tuple/N`, which is what makes every read of it an exact cast.
    Variant(CpsFamilyId, Vec<EmissionValueName>),
    Closure(EmissionClosureName, Vec<EmissionValueName>),
}

/// One pure intrinsic computation over already-bound values — the expression vocabulary of the IR. Every variant produces exactly one value and has no effects (anything stateful is a [`EmissionTail`], per the IR's atomicity law), which is what licenses the optimizer to fold, dedupe, hoist, and drop `Eval` bindings freely.
#[derive(Debug, Clone)]
pub(crate) enum EmissionCode {
    /// One [`CpsIntrinsic`] over its operands, in the order and arity the op fixes — verified at the CPS boundary, so codegen indexes the vector directly. The scalar families (`Nat*`/`Int*`/`Flt*`) lower to the wasm ops they mirror one-for-one; the `Bin*`/`List*` families are rope operations codegen services through shared helper functions.
    Intrinsic(CpsIntrinsic, Vec<EmissionValueName>),
    // `ListMap(src, f)`: map closure `f` over list `src` into a fresh list of the same length. Codegen lowers it to the shared `$list/map` rope helper: one allocation, one fill loop applying `f` per slot via `call_indirect`.
    /// The list-map runtime helper call: list first, mapper second. Not an [`Intrinsic`](Self::Intrinsic) member because it is no [`CpsIntrinsic`] upstream — it runs a closure, so CPS carries it as the call-shaped `CpsIntrinsicCall` with a return continuation, and only structurization collapses it to a pure helper call.
    ListMap(EmissionValueName, EmissionValueName),
}

/// The right-hand side of one binding in the emission body.
#[derive(Debug, Clone)]
pub(crate) enum EmissionValue {
    Pure(EmissionData),
    Eval(EmissionCode),
}

/// A labeled join point inside a region: `params` are its block parameters — the SSA replacement for φ-nodes, bound afresh on every entry from a [`EmissionJumpTarget`] — and `region` its body. Blocks are also how calls continue: a call's `resume` names the block that receives its result, and a backward jump to a loop header block is how `tail_recursion`'s loops close.
#[derive(Debug, Clone)]
pub(crate) struct EmissionBlock {
    pub(crate) params: Vec<EmissionValueName>,
    pub(crate) region: EmissionBody,
}

/// One branch edge: the destination block plus the arguments feeding its parameters, positionally. This is the IR's φ-mechanism — a block parameter's value is whatever the taken edge passed for it.
#[derive(Debug, Clone)]
pub(crate) struct EmissionJumpTarget {
    pub(crate) target: EmissionBlockName,
    pub(crate) params: Vec<EmissionJumpArg>,
}

/// What one edge passes for one block parameter.
///
/// A named value in every ordinary case. [`EmissionJumpArg::Filler`] is the exception, and it exists because this is the first point at which the destination parameter's carrier is knowable: an edge coerces each argument to that carrier (see `Context::jump_instrs`), so a filler chosen upstream as a literal would be coerced as one, which is what trapped an `i31` zero standing in a raw `Flt` slot.
#[derive(Debug, Clone)]
pub(crate) enum EmissionJumpArg {
    Value(EmissionValueName),
    /// No value: the slot belongs to a wider constructor than this edge's. Materialised as the zero of whatever carrier the destination parameter is held at.
    Filler,
}

/// A multi-way branch on a scalar: `operand` is read as an unsigned `u32` (a constructor tag or nat), each case maps one value to its edge, and `default` catches the rest — `None` means the cases are exhaustive, and a match with no cases and no default lowers to a trap. Codegen picks the dispatch shape (a `br_table` for dense-from-zero tags, an `if` for two-way, a binary search otherwise); the IR just states the table.
#[derive(Debug, Clone)]
pub(crate) struct EmissionMatchTarget {
    pub(crate) operand: EmissionValueName,
    pub(crate) cases: BTreeMap<u32, EmissionJumpTarget>,
    pub(crate) default: Option<EmissionJumpTarget>,
}

/// A user-code call in tail position: `Direct` names a known function, `Indirect` invokes a closure value through the shared closure type of its arity (`call_indirect` through the module's closure table). Both name the `resume` block, which receives the callee's single result as its one parameter — except when `resume` is the enclosing body's return sentinel, where the call is a genuine tail call and codegen emits `return_call`/`return_call_indirect` instead of branching.
#[derive(Debug, Clone)]
pub(crate) enum EmissionCallTarget {
    Direct {
        target: EmissionFunctionName,
        params: Vec<EmissionValueName>,
        resume: EmissionBlockName,
    },
    Indirect {
        target: EmissionValueName,
        params: Vec<EmissionValueName>,
        resume: EmissionBlockName,
    },
}

/// A host-provided intrinsic in tail position. Returning foreign calls carry the block that receives their results; direct process termination does not. Purity analysis treats any `EmissionTail::Host` as the impure boundary of its enclosing region tree.
#[derive(Debug, Clone)]
pub(crate) enum EmissionHostTarget {
    /// A store-described host call: `function`'s `WireSignature` fixes the operand order/types and the resume shape — `resume` takes one block parameter per signature result (the multi-result records arrive as parallel block parameters, exactly like the per-op variants did).
    ///
    Foreign {
        function: Arc<ForeignFunction>,
        operands: Vec<EmissionValueName>,
        resume: EmissionBlockName,
    },
    /// Terminate the process with exit `code`; this transfer never resumes.
    Exit { code: EmissionValueName },
}

/// A guest mutable-cell op in tail position. Same `resume` discipline as `EmissionHostTarget`, but serviced inline in codegen (no host import). Purity analysis treats any `EmissionTail::Cell` as an impure boundary, like `Host`.
#[derive(Debug, Clone)]
pub(crate) enum EmissionCellTarget {
    New {
        init: EmissionValueName,
        resume: EmissionBlockName,
    },
    Set {
        cell: EmissionValueName,
        value: EmissionValueName,
        resume: EmissionBlockName,
    },
    Get {
        cell: EmissionValueName,
        resume: EmissionBlockName,
    },
}

impl EmissionCellTarget {
    pub(crate) fn resume(&self) -> &EmissionBlockName {
        match self {
            EmissionCellTarget::New { resume, .. }
            | EmissionCellTarget::Set { resume, .. }
            | EmissionCellTarget::Get { resume, .. } => resume,
        }
    }
}

/// The sole control transfer out of a region — a region never falls through. `Jump` and `Match` stay within the body; `Call` transfers to user code; `Host` and `Cell` are the effectful intrinsics (the only impurity the IR admits — purity analysis marks a region tree impure exactly when one appears in it); `Unreachable` traps, marking a path that cannot be taken (an absurd match).
#[derive(Debug, Clone)]
pub(crate) enum EmissionTail {
    Jump(EmissionJumpTarget),
    Match(EmissionMatchTarget),
    Call(EmissionCallTarget),
    Host(EmissionHostTarget),
    Cell(EmissionCellTarget),
    Unreachable,
}

/// One straight-line body fragment and the sub-structure hanging off it: bindings evaluated in order, the labeled join blocks its jumps enter, and the single [`EmissionTail`] transfer that ends it. All control flow in a body lives in its region tree — a [`EmissionValue`] binding never branches, which is what the optimizer's freedom to fold, dedupe, and reorder bindings rests on.
#[derive(Debug, Clone)]
pub(crate) struct EmissionBody {
    /// Closure shells reserved before their captures are filled, so a self- or mutually-recursive capture can name the shell. Only closures need this; cyclic tuples/lists are rejected upstream (`into_cont`), which keeps `tuple`/`list` immutable.
    pub(crate) shells: Vec<(EmissionValueName, EmissionClosureName)>,
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
pub(crate) struct EmissionArgument {
    pub(crate) name: EmissionValueName,
}

impl From<EmissionValueName> for EmissionArgument {
    fn from(name: EmissionValueName) -> Self {
        Self { name }
    }
}

/// A closure definition: `fields` is the captured environment — filled where a `EmissionData::Closure` names this definition, read inside the body like extra bindings — and `params` the call arguments. `resume` is the return sentinel, exactly as on [`EmissionFunction`]. Codegen gives each definition its own environment struct type but files its function under the shared closure type for its arity, which is what lets a [`EmissionCallTarget::Indirect`] call any closure of matching arity.
#[derive(Debug, Clone)]
pub(crate) struct EmissionClosure {
    pub(crate) fields: Vec<EmissionArgument>,
    pub(crate) params: Vec<EmissionArgument>,
    pub(crate) resume: EmissionBlockName,
    pub(crate) region: EmissionBody,
}

/// A top-level function: parameters, body, and its `resume` sentinel — the block name that means "return": a jump to `resume` with one value *is* the function's return, so a call whose resume block is the enclosing sentinel is a tail call by construction (codegen emits `return_call`), no separate tail-position analysis needed.
#[derive(Debug, Clone)]
pub(crate) struct EmissionFunction {
    pub(crate) params: Vec<EmissionArgument>,
    /// How many values this function hands back. One today for every function; the field exists so a protocol delivering a constructor as its fields has somewhere to say so, and so the wasm type is keyed on the shape rather than on the parameter count alone.
    pub(crate) results: usize,
    pub(crate) resume: EmissionBlockName,
    pub(crate) region: EmissionBody,
}

/// One whole program in cont form: module-level consts (hoisted literals), the closure and function definitions, and the blessed entrypoint. Bodies reference consts and each other by name, and definition order is preserved end-to-end — it is the printed order and the wasm emission order. The collections are private: from outside the crate a module only grows through the `add_*` methods, while the mutating views the optimizer rewrites through stay crate-internal.
#[derive(Debug, Default, Clone)]
pub(crate) struct EmissionModule {
    consts: Vec<(EmissionValueName, EmissionData)>,
    clsrs: Vec<(EmissionClosureName, EmissionClosure)>,
    funcs: Vec<(EmissionFunctionName, EmissionFunction)>,
    entry: Option<EmissionFunctionName>,
    /// The variant families this module constructs and reads, each with its debug name and width. See [`EmissionData::Variant`].
    families: Vec<(CpsFamilyId, Option<String>, usize)>,
}

impl EmissionModule {
    /// An empty module: no definitions, no entrypoint.
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn families(&self) -> &[(CpsFamilyId, Option<String>, usize)] {
        &self.families
    }

    pub(crate) fn set_families(&mut self, families: Vec<(CpsFamilyId, Option<String>, usize)>) {
        self.families = families;
    }

    pub(crate) fn consts(&self) -> &[(EmissionValueName, EmissionData)] {
        &self.consts
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

    /// Every closure arity the module needs closure types for: the arities of the surviving closure definitions, unioned with the arities of indirect call sites (whose target definition may have been inlined away). Sizing closure types from definitions alone misses the latter, leaving a surviving `call_indirect` with no declared type for its arity.
    pub(crate) fn clsr_arities(&self) -> BTreeSet<usize> {
        let mut arities = BTreeSet::new();

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
