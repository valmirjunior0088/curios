//! The float helper functions: what the running program computes where WebAssembly's own `f64` instructions leave the model's answer open.
//!
//! Wasm computes every arithmetic result the model does bit for bit except a NaN's, whose sign and payload it leaves to the engine — x86's default NaN is negative where ARM's is positive. The inline code keeps the hardware instruction and tests its result with `r != r`, which reads no bits; only a NaN takes the call into `flt/nan`, which recomputes the answer the model's NaN rule gives from the operands alone, so whatever the engine produced never reaches the program.
//!
//! Emitted on demand in the `big_emitter` pattern: a call site names a helper through the table, which marks it used, and the module emitter adds the marked set in [`FltHelper::ALL`]'s order.

use super::{Chunk, Scope, declare_helper, f64_type, get, i64_const, i64_type, set, wasm};

/// The bit that makes a NaN quiet, the first of the trailing significand.
const QUIET_BIT: i64 = 1 << 51;
/// The default NaN: positive, quiet and without a payload.
const DEFAULT_NAN: i64 = 0x7ff8_0000_0000_0000;

/// Every float helper, named by what it computes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FltHelper {
    /// `(f64, f64) -> f64`: the NaN the model answers for an operation over these operands whose result is a NaN — the greatest of the NaN operands' quieted patterns, or the default NaN when neither is one. A unary operation passes its operand twice.
    Nan,
}

impl FltHelper {
    /// Every helper, callers before callees.
    pub(crate) const ALL: [FltHelper; 1] = [FltHelper::Nan];

    /// This helper's position in [`FltHelper::ALL`], which is its slot in the table's roster.
    pub(crate) fn slot(self) -> usize {
        FltHelper::ALL
            .iter()
            .position(|helper| *helper == self)
            .expect("`FltHelper::ALL` holds every helper")
    }

    pub(crate) fn func_name(self) -> curios_wasm::FuncName {
        curios_wasm::FuncName::from(match self {
            FltHelper::Nan => "flt/nan",
        })
    }
}

#[derive(Debug)]
pub(crate) struct FltEmitter<'a> {
    module: &'a mut curios_wasm::Module,
}

impl<'a> FltEmitter<'a> {
    pub(crate) fn new(module: &'a mut curios_wasm::Module) -> Self {
        Self { module }
    }

    /// Build `helper` and add it to the module.
    pub(crate) fn emit_func(&mut self, helper: FltHelper) {
        match helper {
            FltHelper::Nan => self.emit_flt_nan(),
        }
    }

    /// `flt/nan`: each operand's pattern quieted when it is a NaN and `0` when it is not — no quieted NaN is `0` — then the greater of the two read unsigned, and the default NaN when both were `0`. Unsigned, so the choice does not read the operands' order and a negative NaN outranks every positive one, as [`curios_num::Floating`] chooses.
    fn emit_flt_nan(&mut self) {
        let mut scope = Scope::default();
        let x = scope.param("x", f64_type());
        let y = scope.param("y", f64_type());
        let qx = scope.local("qx", i64_type());
        let qy = scope.local("qy", i64_type());

        let quieted = |operand: &curios_wasm::LocalName, into: &curios_wasm::LocalName| {
            wasm![
                get(operand),
                curios_wasm::Instr::I64ReinterpretF64,
                i64_const(QUIET_BIT),
                curios_wasm::Instr::I64Or,
                i64_const(0),
                get(operand),
                get(operand),
                curios_wasm::Instr::F64Ne,
                curios_wasm::Instr::Select { val_types: vec![] },
                set(into),
            ]
        };

        let instrs = wasm![
            quieted(&x, &qx),
            quieted(&y, &qy),
            i64_const(DEFAULT_NAN),
            get(&qx),
            get(&qy),
            get(&qx),
            get(&qy),
            curios_wasm::Instr::I64GtU,
            curios_wasm::Instr::Select { val_types: vec![] },
            set(&qx),
            get(&qx),
            get(&qx),
            curios_wasm::Instr::I64Eqz,
            curios_wasm::Instr::Select { val_types: vec![] },
            curios_wasm::Instr::F64ReinterpretI64,
        ];

        declare_helper(
            self.module,
            FltHelper::Nan.func_name(),
            scope,
            f64_type(),
            instrs,
        );
    }
}
