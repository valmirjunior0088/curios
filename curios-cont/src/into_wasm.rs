use {
    crate::{CpsModule, machine::lower, machine::structurize},
    curios_abi::ForeignFunction,
    curios_base::{Grain, PackedBin},
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

mod symbols;
pub(crate) use symbols::*;

mod table;
pub(crate) use table::*;

mod frame;
pub(crate) use frame::*;

mod context;
pub(crate) use context::*;

mod code_emitter;
pub(crate) use code_emitter::*;

mod structure;
pub(crate) use structure::*;

mod expr_emitter;
pub(crate) use expr_emitter::*;

mod module_emitter;
pub(crate) use module_emitter::*;

mod rope_emitter;
pub(crate) use rope_emitter::*;

mod types;
pub(crate) use types::*;

#[cfg(test)]
mod emit_tests;

/// Lower an optimized CPS module to a wasm-GC module — the pipeline's final stage. The private machine CFG is built and its reducible control structurized into blocks and loops, then a `Table` is computed over the whole module (the name maps, the closure type per `clsr_arities` arity, tuple arities, rope helpers) and `ModuleEmitter` declares the host imports and emits every const, closure, and function, exporting the entry under its emitted name (`func/main` — the entry is always `main`).
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn into_wasm(module: &CpsModule) -> curios_wasm::Module {
    let machine = lower(module);
    let structured = structurize(&machine);
    let mut wasm_module = curios_wasm::Module::new("module");
    ModuleEmitter::new(&Table::new(&structured), &mut wasm_module).emit_module(&structured);

    wasm_module
}

/// A constructed value: the scalar immediates, packed `Bits`/`Bytes`, `Lst`/`Tpl` aggregates, and `EmissionClosure` naming its definition plus the captures filling its environment. Aggregates are flat — elements are names of already-bound values, never nested `EmissionData` — so constructing one is a single allocation. `EmissionData` is also the payload of module consts, where codegen materialises it into a wasm global.
#[derive(Debug, Clone)]
pub(crate) enum EmissionData {
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Grain, PackedBin),
    Lst(Vec<EmissionValueName>),
    Tpl(Vec<EmissionValueName>),
    Closure(EmissionClosureName, Vec<EmissionValueName>),
}

/// One pure primitive computation over already-bound values — the expression vocabulary of the IR. Every variant produces exactly one value and has no effects (anything stateful is a [`EmissionTail`], per the IR's atomicity law), which is what licenses the optimizer to fold, dedupe, hoist, and drop `Eval` bindings freely. The scalar families (`Nat*`/`Int*`/`Flt*`) mirror the wasm ops they lower to one-for-one; the `Bin*`/`Lst*` families are rope operations codegen services through shared helper functions.
#[derive(Debug, Clone)]
pub(crate) enum EmissionCode {
    NatEql(EmissionValueName, EmissionValueName),
    NatNeq(EmissionValueName, EmissionValueName),
    NatAdd(EmissionValueName, EmissionValueName),
    NatSub(EmissionValueName, EmissionValueName),
    NatMul(EmissionValueName, EmissionValueName),
    NatLt(EmissionValueName, EmissionValueName),
    NatDiv(EmissionValueName, EmissionValueName),
    NatRem(EmissionValueName, EmissionValueName),
    NatGt(EmissionValueName, EmissionValueName),
    NatLte(EmissionValueName, EmissionValueName),
    NatGte(EmissionValueName, EmissionValueName),
    NatAnd(EmissionValueName, EmissionValueName),
    NatOr(EmissionValueName, EmissionValueName),
    NatXor(EmissionValueName, EmissionValueName),
    NatShl(EmissionValueName, EmissionValueName),
    NatShr(EmissionValueName, EmissionValueName),
    NatRotl(EmissionValueName, EmissionValueName),
    NatRotr(EmissionValueName, EmissionValueName),
    NatClz(EmissionValueName),
    NatCtz(EmissionValueName),
    NatPopcnt(EmissionValueName),
    NatEqz(EmissionValueName),
    NatToInt(EmissionValueName),
    NatToFlt(EmissionValueName),
    IntEql(EmissionValueName, EmissionValueName),
    IntNeq(EmissionValueName, EmissionValueName),
    IntAdd(EmissionValueName, EmissionValueName),
    IntSub(EmissionValueName, EmissionValueName),
    IntMul(EmissionValueName, EmissionValueName),
    IntDiv(EmissionValueName, EmissionValueName),
    IntRem(EmissionValueName, EmissionValueName),
    IntLt(EmissionValueName, EmissionValueName),
    IntGt(EmissionValueName, EmissionValueName),
    IntLte(EmissionValueName, EmissionValueName),
    IntGte(EmissionValueName, EmissionValueName),
    IntAnd(EmissionValueName, EmissionValueName),
    IntOr(EmissionValueName, EmissionValueName),
    IntXor(EmissionValueName, EmissionValueName),
    IntShl(EmissionValueName, EmissionValueName),
    IntShr(EmissionValueName, EmissionValueName),
    IntRotl(EmissionValueName, EmissionValueName),
    IntRotr(EmissionValueName, EmissionValueName),
    IntClz(EmissionValueName),
    IntCtz(EmissionValueName),
    IntPopcnt(EmissionValueName),
    IntEqz(EmissionValueName),
    IntToNat(EmissionValueName),
    IntToFlt(EmissionValueName),
    FltAdd(EmissionValueName, EmissionValueName),
    FltSub(EmissionValueName, EmissionValueName),
    FltMul(EmissionValueName, EmissionValueName),
    FltDiv(EmissionValueName, EmissionValueName),
    FltRem(EmissionValueName, EmissionValueName),
    FltEql(EmissionValueName, EmissionValueName),
    FltNeq(EmissionValueName, EmissionValueName),
    FltLt(EmissionValueName, EmissionValueName),
    FltGt(EmissionValueName, EmissionValueName),
    FltLte(EmissionValueName, EmissionValueName),
    FltGte(EmissionValueName, EmissionValueName),
    FltMin(EmissionValueName, EmissionValueName),
    FltMax(EmissionValueName, EmissionValueName),
    FltNeg(EmissionValueName),
    FltAbs(EmissionValueName),
    FltSqrt(EmissionValueName),
    FltFloor(EmissionValueName),
    FltCeil(EmissionValueName),
    FltTrunc(EmissionValueName),
    FltNearest(EmissionValueName),
    FltCopysign(EmissionValueName, EmissionValueName),
    FltToNat(EmissionValueName),
    FltToLeBytes(EmissionValueName),
    FltOfLeBytes(EmissionValueName),
    FltToInt(EmissionValueName),
    BinLen(Grain, EmissionValueName),
    BinEql(Grain, EmissionValueName, EmissionValueName),
    BinGet(Grain, EmissionValueName, EmissionValueName),
    BinSlice(
        Grain,
        EmissionValueName,
        EmissionValueName,
        EmissionValueName,
    ),
    BinAppend(Grain, EmissionValueName, EmissionValueName),
    BinConcat(Grain, Vec<EmissionValueName>),
    LstLen(EmissionValueName),
    LstGet(EmissionValueName, EmissionValueName),
    LstSlice(EmissionValueName, EmissionValueName, EmissionValueName),
    LstAppend(EmissionValueName, EmissionValueName),
    LstConcat(Vec<EmissionValueName>),
    // `LstMap(src, f)`: map closure `f` over list `src` into a fresh list of
    // the same length. Codegen lowers it to the shared `$lst/map` rope helper:
    // one allocation, one fill loop applying `f` per slot via `call_ref`.
    /// The list-map runtime helper call: list first, mapper second.
    LstMap(EmissionValueName, EmissionValueName),
    TplGet(EmissionValueName, usize),
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

/// One branch edge: the destination block plus the values feeding its parameters, positionally. This is the IR's φ-mechanism — a block parameter's value is whatever the taken edge passed for it.
#[derive(Debug, Clone)]
pub(crate) struct EmissionJumpTarget {
    pub(crate) target: EmissionBlockName,
    pub(crate) params: Vec<EmissionValueName>,
}

/// A multi-way branch on a scalar: `operand` is read as an unsigned `u32` (a constructor tag or nat), each case maps one value to its edge, and `default` catches the rest — `None` means the cases are exhaustive, and a match with no cases and no default lowers to a trap. Codegen picks the dispatch shape (a `br_table` for dense-from-zero tags, an `if` for two-way, a binary search otherwise); the IR just states the table.
#[derive(Debug, Clone)]
pub(crate) struct EmissionMatchTarget {
    pub(crate) operand: EmissionValueName,
    pub(crate) cases: BTreeMap<u32, EmissionJumpTarget>,
    pub(crate) default: Option<EmissionJumpTarget>,
}

/// A user-code call in tail position: `Direct` names a known function, `Indirect` invokes a closure value through the shared closure type of its arity (`call_ref`). Both name the `resume` block, which receives the callee's single result as its one parameter — except when `resume` is the enclosing body's return sentinel, where the call is a genuine tail call and codegen emits `return_call`/`return_call_ref` instead of branching.
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

/// A host-provided primitive in tail position. Returning foreign calls carry
/// the block that receives their results; direct process termination does not.
/// Purity analysis treats any `EmissionTail::Host` as the impure boundary of
/// its enclosing region tree.
#[derive(Debug, Clone)]
pub(crate) enum EmissionHostTarget {
    /// A store-described host call: `function`'s `WireSignature` fixes the
    /// operand order/types and the resume shape — `resume` takes one block
    /// parameter per signature result (the multi-result records arrive as
    /// parallel block parameters, exactly like the per-op variants did).
    ///
    Foreign {
        function: Arc<ForeignFunction>,
        operands: Vec<EmissionValueName>,
        resume: EmissionBlockName,
    },
    /// Terminate the process with exit `code`; this transfer never resumes.
    Exit { code: EmissionValueName },
}

/// A guest mutable-cell op in tail position. Same `resume` discipline as
/// `EmissionHostTarget`, but serviced inline in codegen (no host import). Purity
/// analysis treats any `EmissionTail::Cell` as an impure boundary, like `Host`.
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

/// The sole control transfer out of a region — a region never falls through. `Jump` and `Match` stay within the body; `Call` transfers to user code; `Host` and `Cell` are the effectful primitives (the only impurity the IR admits — purity analysis marks a region tree impure exactly when one appears in it); `Unreachable` traps, marking a path that cannot be taken (an absurd match).
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
    /// Closure shells reserved before their captures are filled, so a self- or
    /// mutually-recursive capture can name the shell. Only closures need this; cyclic
    /// tuples/lists are rejected upstream (`into_cont`), which keeps `tpl`/`lst` immutable.
    pub(crate) shells: Vec<(EmissionValueName, EmissionClosureName)>,
    pub(crate) values: Vec<(EmissionValueName, EmissionValue)>,
    pub(crate) blocks: Vec<(EmissionBlockName, EmissionBlock)>,
    pub(crate) tail: EmissionTail,
}

impl EmissionBody {
    /// Collect the arity of every *indirect* call site in this region (and its nested blocks).
    /// A closure of that arity is invoked here even when the optimizer has specialized its
    /// definition away (a higher-order function's argument inlined, dropping the only closure
    /// of that arity while a `call_ref` in its body survives), so a closure type for the arity
    /// is needed even though no closure of it is defined.
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
}

impl EmissionModule {
    /// An empty module: no definitions, no entrypoint.
    pub(crate) fn new() -> Self {
        Self::default()
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

    /// Every closure arity the module needs closure types for: the arities of the surviving
    /// closure definitions, unioned with the arities of indirect call sites (whose target
    /// definition may have been inlined away). Sizing closure types from definitions alone
    /// misses the latter, leaving a surviving `call_ref` with no declared type for its arity.
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

    /// The entrypoint function — the program's sole root: the value the host
    /// invokes, the only export, and the seed of dead-code reachability. Recorded
    /// here so passes consult the module instead of re-deriving a blessed name.
    pub(crate) fn entry(&self) -> Option<&EmissionFunctionName> {
        self.entry.as_ref()
    }

    /// Bless `func_name` as the entrypoint. Set once by `structurize` (always `main`); the module does not check the function exists — the structurizer adds it separately.
    pub(crate) fn set_entry(&mut self, func_name: EmissionFunctionName) {
        self.entry = Some(func_name);
    }
}
