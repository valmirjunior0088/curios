//! The float helper functions: what the running program computes where WebAssembly's own `f64` instructions do not compute the model's answer.
//!
//! Two gaps, and one library for both. Wasm computes every result the model does bit for bit in the default rounding direction except a NaN's, whose sign and payload it leaves to the engine — x86's default NaN is negative where aarch64's is positive. The inline code keeps the hardware instruction and tests its result with `r != r`, which reads no bits; only a NaN takes the call into `flt/nan`, which recomputes the answer the model's NaN rule gives from the operands alone. And Wasm has no rounding direction but ties to even and no fused multiply-add, so those are computed here in integers: each operation reduces its exact result to a 64-bit significand, an exponent and a sticky bit, and `flt/pack` rounds that once, as [`curios_num::Floating`]'s `round` does.
//!
//! **The integer algorithms, by operation.** Every operand is unpacked to a sign, a significand with its top bit at 52 and an exponent — a subnormal normalized below the format's least exponent, so no case is special past the unpacking; `flt/pack` puts the result back on the subnormal grid.
//! - A sum aligns the smaller operand under ten guard bits and folds whatever falls off into the sticky bit. A difference whose subtrahend lost bits borrows one, which is exact because the guard bits below the minuend are zero.
//! - A product multiplies the two significands in 32-bit limbs into 128 bits and keeps the top 64.
//! - A quotient is a restoring long division to 64 quotient bits, the remainder the sticky bit.
//! - A square root takes the hardware's, which is correctly rounded to nearest and so one step at most from every direction's answer, and steps it when the exact square of the root lies on the wrong side, compared in 128-bit integers. No square root is a tie, so the two nearest directions need no correction.
//! - A fused multiply-add aligns the 128-bit product and the addend at 127 bits and sums them exactly but for what falls past the bottom, which is sticky.
//!
//! Operands no rounding applies to — an infinity, a zero, a NaN — are answered by the hardware instruction or directly, since for them it computes exactly what the model does; the NaN it may produce goes through `flt/nan` like any other.
//!
//! Emitted on demand in the `big_emitter` pattern: a call site names a helper through the table, which marks it used, and the module emitter adds the marked set in [`FltHelper::ALL`]'s order — callers before callees.

use {
    super::{
        Chunk, Scope, Table, block, br, br_if, branch, call, declare_helper, either, f64_type, get,
        i32_const, i32_type, i64_const, i64_type, repeat, set, tee, wasm, when,
    },
    curios_num::Rounding,
};

/// The bit that makes a NaN quiet, the first of the trailing significand.
const QUIET_BIT: i64 = 1 << 51;
/// The default NaN: positive, quiet and without a payload.
const DEFAULT_NAN: i64 = 0x7ff8_0000_0000_0000;
/// The infinity's pattern, less its sign.
const INFINITY_BITS: i64 = 0x7ff0_0000_0000_0000;
/// The largest finite magnitude's pattern.
const MAX_FINITE_BITS: i64 = 0x7fef_ffff_ffff_ffff;
/// The stored significand field.
const MANTISSA_MASK: i64 = 0x000f_ffff_ffff_ffff;
/// The hidden bit's weight.
const HIDDEN_BIT: i64 = 1 << 52;
/// The exponent every subnormal has, and the grid no result goes below.
const MIN_EXPONENT: i32 = -1074;
/// What a significand's exponent gains to reach its stored field.
const EXPONENT_BIAS: i32 = 1075;
/// The field an infinity and a NaN share.
const INFINITE_FIELD: i32 = 2047;

/// The `i32` a direction travels as, which is its position in [`Rounding::ALL`].
pub(crate) fn rounding_code(rounding: Rounding) -> i32 {
    match rounding {
        Rounding::TiesToEven => 0,
        Rounding::TiesToAway => 1,
        Rounding::TowardZero => 2,
        Rounding::TowardPositive => 3,
        Rounding::TowardNegative => 4,
    }
}

/// Every float helper, named by what it computes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FltHelper {
    /// `(f64, f64, f64, i32 direction) -> f64`: `a · b + c`, rounded once.
    Fma,
    /// `(f64, f64, i32 direction) -> f64`: the sum. A difference is the sum with its subtrahend negated.
    Add,
    /// `(f64, f64, i32 direction) -> f64`: the product.
    Mul,
    /// `(f64, f64, i32 direction) -> f64`: the quotient.
    Div,
    /// `(f64, i32 direction) -> f64`: the square root.
    Sqrt,
    /// `(f64) -> f64`: the integral value nearest, a tie away from zero — the one direction of round-to-integral Wasm has no instruction for.
    Round,
    /// `(i32 negative, i64 significand, i32 exponent, i32 sticky, i32 direction) -> f64`: the binary64 `(-1)^negative · (significand + ε) · 2^exponent` rounds to, `ε` in `(0, 1)` when `sticky` is `1` and zero when it is `0`. The model's `round`, over a 64-bit significand.
    Pack,
    /// `(f64, f64) -> f64`: the NaN the model answers for an operation over these operands whose result is a NaN — the greatest of the NaN operands' quieted patterns, or the default NaN when neither is one. A unary operation passes its operand twice.
    Nan,
}

impl FltHelper {
    /// Every helper, callers before callees.
    pub(crate) const ALL: [FltHelper; 8] = [
        FltHelper::Fma,
        FltHelper::Add,
        FltHelper::Mul,
        FltHelper::Div,
        FltHelper::Sqrt,
        FltHelper::Round,
        FltHelper::Pack,
        FltHelper::Nan,
    ];

    /// This helper's position in [`FltHelper::ALL`], which is its slot in the table's roster.
    pub(crate) fn slot(self) -> usize {
        FltHelper::ALL
            .iter()
            .position(|helper| *helper == self)
            .expect("`FltHelper::ALL` holds every helper")
    }

    pub(crate) fn func_name(self) -> curios_wasm::FuncName {
        curios_wasm::FuncName::from(match self {
            FltHelper::Fma => "flt/fma",
            FltHelper::Add => "flt/add",
            FltHelper::Mul => "flt/mul",
            FltHelper::Div => "flt/div",
            FltHelper::Sqrt => "flt/sqrt",
            FltHelper::Round => "flt/round",
            FltHelper::Pack => "flt/pack",
            FltHelper::Nan => "flt/nan",
        })
    }
}

type Local = curios_wasm::LocalName;
type Instrs = Vec<curios_wasm::Instr>;

/// A float's bit pattern.
fn bits(value: &Local) -> Instrs {
    wasm![get(value), curios_wasm::Instr::I64ReinterpretF64]
}

/// Whether `value` is finite — neither infinity nor a NaN, which every comparison fails.
fn finite(value: &Local) -> Instrs {
    wasm![
        get(value),
        curios_wasm::Instr::F64Abs,
        curios_wasm::Instr::F64Const {
            value: f64::INFINITY
        },
        curios_wasm::Instr::F64Lt,
    ]
}

/// Whether `value` is a NaN.
fn is_nan(value: &Local) -> Instrs {
    wasm![get(value), get(value), curios_wasm::Instr::F64Ne]
}

/// Whether `value` is a zero of either sign.
fn is_zero(value: &Local) -> Instrs {
    wasm![
        get(value),
        curios_wasm::Instr::F64Const { value: 0.0 },
        curios_wasm::Instr::F64Eq,
    ]
}

/// `value`'s sign bit, as an `i32`.
fn sign(value: &Local) -> Instrs {
    wasm![
        bits(value),
        i64_const(63),
        curios_wasm::Instr::I64ShrU,
        curios_wasm::Instr::I32WrapI64,
    ]
}

/// The `i32` on the stack, moved to the sign bit of an `i64` pattern.
fn sign_bit() -> Instrs {
    wasm![
        curios_wasm::Instr::I64ExtendI32U,
        i64_const(63),
        curios_wasm::Instr::I64Shl,
    ]
}

/// The zero whose sign the `i32` on the stack names.
fn signed_zero() -> Instrs {
    wasm![sign_bit(), curios_wasm::Instr::F64ReinterpretI64]
}

/// Whether the `i32` in `direction` is `rounding`.
fn is_direction(direction: &Local, rounding: Rounding) -> Instrs {
    wasm![
        get(direction),
        i32_const(rounding_code(rounding)),
        curios_wasm::Instr::I32Eq,
    ]
}

/// `condition ? then : otherwise` over three `i32` sequences.
fn pick(then: Instrs, otherwise: Instrs, condition: Instrs) -> Instrs {
    wasm![
        then,
        otherwise,
        condition,
        curios_wasm::Instr::Select { val_types: vec![] }
    ]
}

/// `max(x, y)` over two signed `i32` locals.
fn max_signed(x: &Local, y: &Local) -> Instrs {
    pick(
        vec![get(x)],
        vec![get(y)],
        wasm![get(x), get(y), curios_wasm::Instr::I32GtS],
    )
}

/// `min(x, y)` over two signed `i32` locals.
fn min_signed(x: &Local, y: &Local) -> Instrs {
    pick(
        vec![get(x)],
        vec![get(y)],
        wasm![get(x), get(y), curios_wasm::Instr::I32LtS],
    )
}

/// `local += amount` over `i32`s.
fn bump(local: &Local, amount: Instrs) -> Instrs {
    wasm![get(local), amount, curios_wasm::Instr::I32Add, set(local)]
}

/// Swap two locals of one type through `temp`.
fn swap(first: &Local, second: &Local, temp: &Local) -> Instrs {
    wasm![
        get(first),
        set(temp),
        get(second),
        set(first),
        get(temp),
        set(second)
    ]
}

/// One operand taken apart: its sign as an `i32`, its significand with the top bit at 52, and its exponent — below the least one for a subnormal, which the normalization moved there.
struct Unpacked {
    negative: Local,
    significand: Local,
    exponent: Local,
}

impl Unpacked {
    fn declare(scope: &mut Scope, name: &str) -> Self {
        Self {
            negative: scope.local(&format!("{name}_negative"), i32_type()),
            significand: scope.local(&format!("{name}_significand"), i64_type()),
            exponent: scope.local(&format!("{name}_exponent"), i32_type()),
        }
    }

    /// Take the finite, nonzero `value` apart into these locals.
    fn of(&self, value: &Local) -> Instrs {
        let field = || {
            wasm![
                bits(value),
                i64_const(52),
                curios_wasm::Instr::I64ShrU,
                curios_wasm::Instr::I32WrapI64,
                i32_const(0x7ff),
                curios_wasm::Instr::I32And,
            ]
        };
        let leading = || {
            wasm![
                get(&self.significand),
                curios_wasm::Instr::I64Clz,
                i64_const(11),
                curios_wasm::Instr::I64Sub,
            ]
        };

        wasm![
            sign(value),
            set(&self.negative),
            bits(value),
            i64_const(MANTISSA_MASK),
            curios_wasm::Instr::I64And,
            set(&self.significand),
            field(),
            curios_wasm::Instr::I32Eqz,
            branch(
                // A subnormal: the significand's top bit raised to 52, and the exponent lowered with it.
                wasm![
                    i32_const(MIN_EXPONENT),
                    leading(),
                    curios_wasm::Instr::I32WrapI64,
                    curios_wasm::Instr::I32Sub,
                    set(&self.exponent),
                    get(&self.significand),
                    leading(),
                    curios_wasm::Instr::I64Shl,
                    set(&self.significand),
                ],
                wasm![
                    field(),
                    i32_const(EXPONENT_BIAS),
                    curios_wasm::Instr::I32Sub,
                    set(&self.exponent),
                    get(&self.significand),
                    i64_const(HIDDEN_BIT),
                    curios_wasm::Instr::I64Or,
                    set(&self.significand),
                ],
            ),
        ]
    }
}

/// A 128-bit unsigned value held in two `i64` locals.
struct Wide {
    high: Local,
    low: Local,
}

impl Wide {
    fn declare(scope: &mut Scope, name: &str) -> Self {
        Self {
            high: scope.local(&format!("{name}_high"), i64_type()),
            low: scope.local(&format!("{name}_low"), i64_type()),
        }
    }

    /// Set this to the exact product of two significands under `2^53`, in 32-bit limbs; `middle` is scratch.
    fn product(&self, x: &Local, y: &Local, middle: &Local) -> Instrs {
        let low32 = |v: &Local| wasm![get(v), i64_const(0xffff_ffff), curios_wasm::Instr::I64And];
        let high32 = |v: &Local| wasm![get(v), i64_const(32), curios_wasm::Instr::I64ShrU];

        wasm![
            // The two cross products, each under `2^53`, so their sum cannot carry out.
            low32(x),
            high32(y),
            curios_wasm::Instr::I64Mul,
            high32(x),
            low32(y),
            curios_wasm::Instr::I64Mul,
            curios_wasm::Instr::I64Add,
            set(middle),
            low32(x),
            low32(y),
            curios_wasm::Instr::I64Mul,
            set(&self.low),
            // The carry out of the low half: the sum wrapped exactly when it came out smaller than what it started from.
            get(&self.low),
            get(&self.low),
            get(middle),
            i64_const(32),
            curios_wasm::Instr::I64Shl,
            curios_wasm::Instr::I64Add,
            tee(&self.low),
            curios_wasm::Instr::I64GtU,
            curios_wasm::Instr::I64ExtendI32U,
            high32(x),
            high32(y),
            curios_wasm::Instr::I64Mul,
            curios_wasm::Instr::I64Add,
            get(middle),
            i64_const(32),
            curios_wasm::Instr::I64ShrU,
            curios_wasm::Instr::I64Add,
            set(&self.high),
        ]
    }

    /// Leading zeros of the 128 bits, as an `i32`.
    fn clz(&self) -> Instrs {
        wasm![
            get(&self.low),
            curios_wasm::Instr::I64Clz,
            i64_const(64),
            curios_wasm::Instr::I64Add,
            get(&self.high),
            curios_wasm::Instr::I64Clz,
            get(&self.high),
            curios_wasm::Instr::I64Eqz,
            curios_wasm::Instr::Select { val_types: vec![] },
            curios_wasm::Instr::I32WrapI64,
        ]
    }

    /// Shift left by the `i32` in `count`, which is below 64.
    fn shift_left(&self, count: &Local) -> Instrs {
        let count64 = || wasm![get(count), curios_wasm::Instr::I64ExtendI32U];

        wasm![
            get(count),
            when(wasm![
                get(&self.high),
                count64(),
                curios_wasm::Instr::I64Shl,
                get(&self.low),
                i64_const(64),
                count64(),
                curios_wasm::Instr::I64Sub,
                curios_wasm::Instr::I64ShrU,
                curios_wasm::Instr::I64Or,
                set(&self.high),
                get(&self.low),
                count64(),
                curios_wasm::Instr::I64Shl,
                set(&self.low),
            ]),
        ]
    }

    /// Shift right by the `i32` in `count`, however large, OR-ing every bit that falls off into the `i32` in `sticky`.
    fn shift_right_sticky(&self, count: &Local, sticky: &Local) -> Instrs {
        let count64 = || wasm![get(count), curios_wasm::Instr::I64ExtendI32U];
        let past_64 = || wasm![count64(), i64_const(64), curios_wasm::Instr::I64Sub];
        let absorb = |bits: Instrs| {
            wasm![
                bits,
                i64_const(0),
                curios_wasm::Instr::I64Ne,
                get(sticky),
                curios_wasm::Instr::I32Or,
                set(sticky),
            ]
        };

        let below_64 = wasm![
            absorb(wasm![
                get(&self.low),
                i64_const(64),
                count64(),
                curios_wasm::Instr::I64Sub,
                curios_wasm::Instr::I64Shl,
            ]),
            get(&self.low),
            count64(),
            curios_wasm::Instr::I64ShrU,
            get(&self.high),
            i64_const(64),
            count64(),
            curios_wasm::Instr::I64Sub,
            curios_wasm::Instr::I64Shl,
            curios_wasm::Instr::I64Or,
            set(&self.low),
            get(&self.high),
            count64(),
            curios_wasm::Instr::I64ShrU,
            set(&self.high),
        ];
        // From 64 the low half falls off whole. What falls off the high half is read as the high half less what it keeps, since shifting by `128 - count` would wrap at 64.
        let below_128 = wasm![
            absorb(vec![get(&self.low)]),
            absorb(wasm![
                get(&self.high),
                get(&self.high),
                past_64(),
                curios_wasm::Instr::I64ShrU,
                past_64(),
                curios_wasm::Instr::I64Shl,
                curios_wasm::Instr::I64Xor,
            ]),
            get(&self.high),
            past_64(),
            curios_wasm::Instr::I64ShrU,
            set(&self.low),
            i64_const(0),
            set(&self.high),
        ];
        let beyond = wasm![
            absorb(wasm![
                get(&self.high),
                get(&self.low),
                curios_wasm::Instr::I64Or
            ]),
            i64_const(0),
            set(&self.high),
            i64_const(0),
            set(&self.low),
        ];

        wasm![
            get(count),
            when(wasm![
                get(count),
                i32_const(64),
                curios_wasm::Instr::I32LtU,
                branch(
                    below_64,
                    wasm![
                        get(count),
                        i32_const(128),
                        curios_wasm::Instr::I32LtU,
                        branch(below_128, beyond),
                    ],
                ),
            ]),
        ]
    }

    /// Whether this is above `other`, unsigned, as an `i32`.
    fn above(&self, other: &Wide) -> Instrs {
        wasm![
            get(&self.high),
            get(&other.high),
            curios_wasm::Instr::I64GtU,
            get(&self.low),
            get(&other.low),
            curios_wasm::Instr::I64GtU,
            get(&self.high),
            get(&other.high),
            curios_wasm::Instr::I64Eq,
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32Or,
        ]
    }

    /// Whether the two are equal, as an `i32`.
    fn equals(&self, other: &Wide) -> Instrs {
        wasm![
            get(&self.high),
            get(&other.high),
            curios_wasm::Instr::I64Eq,
            get(&self.low),
            get(&other.low),
            curios_wasm::Instr::I64Eq,
            curios_wasm::Instr::I32And,
        ]
    }

    /// `self += other`; the caller keeps the sum under `2^128`.
    fn add(&self, other: &Wide) -> Instrs {
        wasm![
            get(&self.low),
            get(&other.low),
            curios_wasm::Instr::I64Add,
            tee(&self.low),
            get(&other.low),
            curios_wasm::Instr::I64LtU,
            curios_wasm::Instr::I64ExtendI32U,
            get(&self.high),
            curios_wasm::Instr::I64Add,
            get(&other.high),
            curios_wasm::Instr::I64Add,
            set(&self.high),
        ]
    }

    /// `self -= other + borrow`, where `borrow` is an `i32` of `0` or `1`; the caller keeps the difference non-negative.
    fn subtract(&self, other: &Wide, borrow: &Local) -> Instrs {
        wasm![
            // The high half, less the borrow out of the low: the subtrahend's low half, with the borrow in, exceeds the minuend's.
            get(&self.high),
            get(&other.high),
            curios_wasm::Instr::I64Sub,
            get(&other.low),
            get(&self.low),
            curios_wasm::Instr::I64GtU,
            get(&other.low),
            get(&self.low),
            curios_wasm::Instr::I64Eq,
            get(borrow),
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32Or,
            curios_wasm::Instr::I64ExtendI32U,
            curios_wasm::Instr::I64Sub,
            get(&self.low),
            get(&other.low),
            curios_wasm::Instr::I64Sub,
            get(borrow),
            curios_wasm::Instr::I64ExtendI32U,
            curios_wasm::Instr::I64Sub,
            set(&self.low),
            set(&self.high),
        ]
    }

    /// Swap with `other` through `temp`.
    fn swap(&self, other: &Wide, temp: &Local) -> Instrs {
        wasm![
            swap(&self.high, &other.high, temp),
            swap(&self.low, &other.low, temp),
        ]
    }
}

pub(crate) struct FltEmitter<'a, 'b> {
    table: &'a Table<'a>,
    module: &'b mut curios_wasm::Module,
}

impl<'a, 'b> FltEmitter<'a, 'b> {
    pub(crate) fn new(table: &'a Table<'a>, module: &'b mut curios_wasm::Module) -> Self {
        Self { table, module }
    }

    /// Build `helper` and add it to the module.
    pub(crate) fn emit_func(&mut self, helper: FltHelper) {
        match helper {
            FltHelper::Fma => self.emit_flt_fma(),
            FltHelper::Add => self.emit_flt_add(),
            FltHelper::Mul => self.emit_flt_mul(),
            FltHelper::Div => self.emit_flt_div(),
            FltHelper::Sqrt => self.emit_flt_sqrt(),
            FltHelper::Round => self.emit_flt_round(),
            FltHelper::Pack => self.emit_flt_pack(),
            FltHelper::Nan => self.emit_flt_nan(),
        }
    }

    /// A call to `helper`, marking it for emission.
    fn helper(&self, helper: FltHelper) -> curios_wasm::Instr {
        call(&self.table.flt_func(helper))
    }

    fn declare(&mut self, helper: FltHelper, scope: Scope, instrs: Instrs) {
        declare_helper(self.module, helper.func_name(), scope, f64_type(), instrs);
    }

    /// Return the hardware's `apply` over `operands` — exact wherever this is reached — or the model's NaN when it produced one.
    fn return_exact(
        &self,
        operands: &[&Local],
        apply: curios_wasm::Instr,
        result: &Local,
    ) -> Instrs {
        let pair = match operands {
            [x] => wasm![get(x), get(x)],
            [x, y] => wasm![get(x), get(y)],
            _ => unreachable!("an operation of one or two operands"),
        };

        wasm![
            operands
                .iter()
                .map(|operand| get(operand))
                .collect::<Instrs>(),
            apply,
            set(result),
            is_nan(result),
            either(
                f64_type(),
                wasm![pair, self.helper(FltHelper::Nan)],
                vec![get(result)],
            ),
            curios_wasm::Instr::Return,
        ]
    }

    /// Return `pack(negative, significand, exponent, sticky, direction)`, from locals.
    fn return_packed(
        &self,
        negative: &Local,
        significand: &Local,
        exponent: &Local,
        sticky: &Local,
        direction: &Local,
    ) -> Instrs {
        wasm![
            get(negative),
            get(significand),
            get(exponent),
            get(sticky),
            get(direction),
            self.helper(FltHelper::Pack),
        ]
    }

    /// `flt/add`: exact operands and zeros answered directly; otherwise the two aligned under ten guard bits, the smaller's lost bits sticky, and the sum or difference packed.
    fn emit_flt_add(&mut self) {
        let mut scope = Scope::default();
        let a = scope.param("a", f64_type());
        let b = scope.param("b", f64_type());
        let direction = scope.param("direction", i32_type());
        let r = scope.local("r", f64_type());
        let x = Unpacked::declare(&mut scope, "x");
        let y = Unpacked::declare(&mut scope, "y");
        let distance = scope.local("distance", i32_type());
        let sticky = scope.local("sticky", i32_type());
        let t32 = scope.local("t32", i32_type());
        let t64 = scope.local("t64", i64_type());

        let toward_negative = || is_direction(&direction, Rounding::TowardNegative);
        let distance64 = || wasm![get(&distance), curios_wasm::Instr::I64ExtendI32U];

        let instrs = wasm![
            finite(&a),
            finite(&b),
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32Eqz,
            when(self.return_exact(&[&a, &b], curios_wasm::Instr::F64Add, &r)),
            // Two zeros: the negative one when both are, or when their signs differ and the direction is toward negative (§6.3).
            is_zero(&a),
            is_zero(&b),
            curios_wasm::Instr::I32And,
            when(wasm![
                sign(&a),
                sign(&b),
                curios_wasm::Instr::I32And,
                sign(&a),
                sign(&b),
                curios_wasm::Instr::I32Xor,
                toward_negative(),
                curios_wasm::Instr::I32And,
                curios_wasm::Instr::I32Or,
                signed_zero(),
                curios_wasm::Instr::Return,
            ]),
            is_zero(&a),
            when(wasm![get(&b), curios_wasm::Instr::Return]),
            is_zero(&b),
            when(wasm![get(&a), curios_wasm::Instr::Return]),
            x.of(&a),
            y.of(&b),
            // The larger exponent first.
            get(&x.exponent),
            get(&y.exponent),
            curios_wasm::Instr::I32LtS,
            when(wasm![
                swap(&x.negative, &y.negative, &t32),
                swap(&x.exponent, &y.exponent, &t32),
                swap(&x.significand, &y.significand, &t64),
            ]),
            get(&x.exponent),
            get(&y.exponent),
            curios_wasm::Instr::I32Sub,
            set(&distance),
            get(&x.significand),
            i64_const(10),
            curios_wasm::Instr::I64Shl,
            set(&x.significand),
            get(&x.exponent),
            i32_const(10),
            curios_wasm::Instr::I32Sub,
            set(&x.exponent),
            get(&y.significand),
            i64_const(10),
            curios_wasm::Instr::I64Shl,
            set(&y.significand),
            i32_const(0),
            set(&sticky),
            // The smaller operand aligned: by a whole shift below 64, and entirely sticky past it.
            get(&distance),
            i32_const(64),
            curios_wasm::Instr::I32GeU,
            branch(
                wasm![
                    i32_const(1),
                    set(&sticky),
                    i64_const(0),
                    set(&y.significand)
                ],
                wasm![
                    get(&distance),
                    when(wasm![
                        get(&y.significand),
                        i64_const(64),
                        distance64(),
                        curios_wasm::Instr::I64Sub,
                        curios_wasm::Instr::I64Shl,
                        i64_const(0),
                        curios_wasm::Instr::I64Ne,
                        set(&sticky),
                        get(&y.significand),
                        distance64(),
                        curios_wasm::Instr::I64ShrU,
                        set(&y.significand),
                    ]),
                ],
            ),
            get(&x.negative),
            get(&y.negative),
            curios_wasm::Instr::I32Eq,
            branch(
                wasm![
                    get(&x.significand),
                    get(&y.significand),
                    curios_wasm::Instr::I64Add,
                    set(&x.significand),
                ],
                wasm![
                    // An exact cancellation is `+0`, or `-0` toward negative.
                    get(&x.significand),
                    get(&y.significand),
                    curios_wasm::Instr::I64Eq,
                    get(&sticky),
                    curios_wasm::Instr::I32Eqz,
                    curios_wasm::Instr::I32And,
                    when(wasm![
                        toward_negative(),
                        signed_zero(),
                        curios_wasm::Instr::Return
                    ]),
                    // Only at one exponent can the second be the larger, and then nothing was shifted out of it.
                    get(&y.significand),
                    get(&x.significand),
                    curios_wasm::Instr::I64GtU,
                    branch(
                        wasm![
                            get(&y.negative),
                            set(&x.negative),
                            get(&y.significand),
                            get(&x.significand),
                            curios_wasm::Instr::I64Sub,
                            set(&x.significand),
                        ],
                        wasm![
                            get(&x.significand),
                            get(&y.significand),
                            curios_wasm::Instr::I64Sub,
                            get(&sticky),
                            curios_wasm::Instr::I64ExtendI32U,
                            curios_wasm::Instr::I64Sub,
                            set(&x.significand),
                        ],
                    ),
                ],
            ),
            self.return_packed(
                &x.negative,
                &x.significand,
                &x.exponent,
                &sticky,
                &direction
            ),
        ];

        self.declare(FltHelper::Add, scope, instrs);
    }

    /// `flt/mul`: exact operands answered by the hardware; otherwise the 106-bit product of the significands, its top 64 bits packed with the rest sticky.
    fn emit_flt_mul(&mut self) {
        let mut scope = Scope::default();
        let a = scope.param("a", f64_type());
        let b = scope.param("b", f64_type());
        let direction = scope.param("direction", i32_type());
        let r = scope.local("r", f64_type());
        let x = Unpacked::declare(&mut scope, "x");
        let y = Unpacked::declare(&mut scope, "y");
        let product = Wide::declare(&mut scope, "product");
        let middle = scope.local("middle", i64_type());
        let sticky = scope.local("sticky", i32_type());

        let instrs = wasm![
            finite(&a),
            finite(&b),
            curios_wasm::Instr::I32And,
            is_zero(&a),
            is_zero(&b),
            curios_wasm::Instr::I32Or,
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32Eqz,
            when(self.return_exact(&[&a, &b], curios_wasm::Instr::F64Mul, &r)),
            x.of(&a),
            y.of(&b),
            product.product(&x.significand, &y.significand, &middle),
            // The product lies in `[2^104, 2^106)`: its top 64 bits start at bit 42.
            get(&product.high),
            i64_const(22),
            curios_wasm::Instr::I64Shl,
            get(&product.low),
            i64_const(42),
            curios_wasm::Instr::I64ShrU,
            curios_wasm::Instr::I64Or,
            set(&x.significand),
            get(&product.low),
            i64_const((1 << 42) - 1),
            curios_wasm::Instr::I64And,
            i64_const(0),
            curios_wasm::Instr::I64Ne,
            set(&sticky),
            get(&x.negative),
            get(&y.negative),
            curios_wasm::Instr::I32Xor,
            set(&x.negative),
            bump(
                &x.exponent,
                wasm![get(&y.exponent), i32_const(42), curios_wasm::Instr::I32Add]
            ),
            self.return_packed(
                &x.negative,
                &x.significand,
                &x.exponent,
                &sticky,
                &direction
            ),
        ];

        self.declare(FltHelper::Mul, scope, instrs);
    }

    /// `flt/div`: exact operands answered by the hardware; otherwise a restoring long division of the significands to 64 quotient bits, the remainder sticky.
    fn emit_flt_div(&mut self) {
        let mut scope = Scope::default();
        let a = scope.param("a", f64_type());
        let b = scope.param("b", f64_type());
        let direction = scope.param("direction", i32_type());
        let r = scope.local("r", f64_type());
        let x = Unpacked::declare(&mut scope, "x");
        let y = Unpacked::declare(&mut scope, "y");
        let quotient = scope.local("quotient", i64_type());
        let i = scope.local("i", i32_type());
        let sticky = scope.local("sticky", i32_type());

        let fits = || {
            wasm![
                get(&x.significand),
                get(&y.significand),
                curios_wasm::Instr::I64GeU
            ]
        };

        let instrs = wasm![
            finite(&a),
            finite(&b),
            curios_wasm::Instr::I32And,
            is_zero(&a),
            is_zero(&b),
            curios_wasm::Instr::I32Or,
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32Eqz,
            when(self.return_exact(&[&a, &b], curios_wasm::Instr::F64Div, &r)),
            x.of(&a),
            y.of(&b),
            // The remainder runs in `x`'s significand, below twice the divisor and so under `2^54` before each doubling.
            i64_const(0),
            set(&quotient),
            i32_const(0),
            set(&i),
            block(
                "quotient_done",
                vec![repeat(
                    "quotient_bit",
                    wasm![
                        get(&i),
                        i32_const(64),
                        curios_wasm::Instr::I32GeU,
                        br_if("quotient_done"),
                        get(&quotient),
                        i64_const(1),
                        curios_wasm::Instr::I64Shl,
                        fits(),
                        curios_wasm::Instr::I64ExtendI32U,
                        curios_wasm::Instr::I64Or,
                        set(&quotient),
                        get(&x.significand),
                        get(&y.significand),
                        curios_wasm::Instr::I64Sub,
                        get(&x.significand),
                        fits(),
                        curios_wasm::Instr::Select { val_types: vec![] },
                        i64_const(1),
                        curios_wasm::Instr::I64Shl,
                        set(&x.significand),
                        bump(&i, vec![i32_const(1)]),
                        br("quotient_bit"),
                    ],
                )],
            ),
            get(&x.significand),
            i64_const(0),
            curios_wasm::Instr::I64Ne,
            set(&sticky),
            get(&x.negative),
            get(&y.negative),
            curios_wasm::Instr::I32Xor,
            set(&x.negative),
            get(&x.exponent),
            get(&y.exponent),
            curios_wasm::Instr::I32Sub,
            i32_const(63),
            curios_wasm::Instr::I32Sub,
            set(&x.exponent),
            self.return_packed(&x.negative, &quotient, &x.exponent, &sticky, &direction),
        ];

        self.declare(FltHelper::Div, scope, instrs);
    }

    /// `flt/sqrt`: anything but a positive finite operand answered by the hardware; otherwise the hardware's root, stepped once when its exact square lies on the side the direction excludes.
    fn emit_flt_sqrt(&mut self) {
        let mut scope = Scope::default();
        let a = scope.param("a", f64_type());
        let direction = scope.param("direction", i32_type());
        let root = scope.local("root", f64_type());
        let x = Unpacked::declare(&mut scope, "x");
        let y = Unpacked::declare(&mut scope, "y");
        let square = Wide::declare(&mut scope, "square");
        let target = Wide::declare(&mut scope, "target");
        let middle = scope.local("middle", i64_type());
        let distance = scope.local("distance", i32_type());

        let distance64 = || wasm![get(&distance), curios_wasm::Instr::I64ExtendI32U];
        let step = |by: i64| {
            wasm![
                bits(&root),
                i64_const(by),
                curios_wasm::Instr::I64Add,
                curios_wasm::Instr::F64ReinterpretI64,
            ]
        };

        let instrs = wasm![
            get(&a),
            curios_wasm::Instr::F64Const { value: 0.0 },
            curios_wasm::Instr::F64Gt,
            finite(&a),
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32Eqz,
            when(self.return_exact(&[&a], curios_wasm::Instr::F64Sqrt, &root)),
            get(&a),
            curios_wasm::Instr::F64Sqrt,
            set(&root),
            // No square root is a tie, so rounding to nearest either way is the hardware's answer.
            get(&direction),
            i32_const(rounding_code(Rounding::TowardZero)),
            curios_wasm::Instr::I32LtU,
            when(wasm![get(&root), curios_wasm::Instr::Return]),
            // The root is normal: the least operand's is near `2^-537`.
            x.of(&root),
            y.of(&a),
            square.product(&x.significand, &x.significand, &middle),
            // `root² = square · 2^(2·x)` against `a = y · 2^e`: shift `y` by `e - 2·x`, which the two magnitudes agreeing within an ulp keep in `[51, 54]`.
            get(&y.exponent),
            get(&x.exponent),
            i32_const(1),
            curios_wasm::Instr::I32Shl,
            curios_wasm::Instr::I32Sub,
            set(&distance),
            get(&y.significand),
            i64_const(64),
            distance64(),
            curios_wasm::Instr::I64Sub,
            curios_wasm::Instr::I64ShrU,
            set(&target.high),
            get(&y.significand),
            distance64(),
            curios_wasm::Instr::I64Shl,
            set(&target.low),
            square.equals(&target),
            when(wasm![get(&root), curios_wasm::Instr::Return]),
            is_direction(&direction, Rounding::TowardPositive),
            either(
                f64_type(),
                wasm![
                    square.above(&target),
                    either(f64_type(), vec![get(&root)], step(1)),
                ],
                wasm![
                    square.above(&target),
                    either(f64_type(), step(-1), vec![get(&root)]),
                ],
            ),
        ];

        self.declare(FltHelper::Sqrt, scope, instrs);
    }

    /// `flt/fma`: a NaN operand answered first, then the infinities and the zero product exactly; otherwise the product's 128 bits and the addend aligned at bit 126, summed exactly but for what falls past the bottom, and packed.
    fn emit_flt_fma(&mut self) {
        let mut scope = Scope::default();
        let a = scope.param("a", f64_type());
        let b = scope.param("b", f64_type());
        let c = scope.param("c", f64_type());
        let direction = scope.param("direction", i32_type());
        let r = scope.local("r", f64_type());
        let x = Unpacked::declare(&mut scope, "x");
        let y = Unpacked::declare(&mut scope, "y");
        let z = Unpacked::declare(&mut scope, "z");
        let large = Wide::declare(&mut scope, "large");
        let small = Wide::declare(&mut scope, "small");
        let middle = scope.local("middle", i64_type());
        let count = scope.local("count", i32_type());
        let sticky = scope.local("sticky", i32_type());
        let t32 = scope.local("t32", i32_type());
        let t64 = scope.local("t64", i64_type());

        let nan = || self.helper(FltHelper::Nan);

        let instrs = wasm![
            // A NaN operand answers before anything is invalid, the addend's included.
            is_nan(&a),
            is_nan(&b),
            curios_wasm::Instr::I32Or,
            when(wasm![
                get(&a),
                get(&b),
                nan(),
                set(&r),
                is_nan(&c),
                either(f64_type(), wasm![get(&r), get(&c), nan()], vec![get(&r)]),
                curios_wasm::Instr::Return,
            ]),
            is_nan(&c),
            when(wasm![get(&c), get(&c), nan(), curios_wasm::Instr::Return]),
            // An infinite factor: the product is `0 · ∞`, invalid, or an infinity, and adding exactly is what the hardware does. Any NaN now is an invalid operation's.
            finite(&a),
            finite(&b),
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32Eqz,
            when(wasm![
                get(&a),
                get(&b),
                curios_wasm::Instr::F64Mul,
                get(&c),
                curios_wasm::Instr::F64Add,
                set(&r),
                is_nan(&r),
                either(
                    f64_type(),
                    vec![
                        i64_const(DEFAULT_NAN),
                        curios_wasm::Instr::F64ReinterpretI64
                    ],
                    vec![get(&r)],
                ),
                curios_wasm::Instr::Return,
            ]),
            finite(&c),
            curios_wasm::Instr::I32Eqz,
            when(wasm![get(&c), curios_wasm::Instr::Return]),
            // An exact zero product: the sum with the addend rounds once, and settles two zeros' sign.
            is_zero(&a),
            is_zero(&b),
            curios_wasm::Instr::I32Or,
            when(wasm![
                sign(&a),
                sign(&b),
                curios_wasm::Instr::I32Xor,
                signed_zero(),
                get(&c),
                get(&direction),
                self.helper(FltHelper::Add),
                curios_wasm::Instr::Return,
            ]),
            is_zero(&c),
            when(wasm![
                get(&a),
                get(&b),
                get(&direction),
                self.helper(FltHelper::Mul),
                curios_wasm::Instr::Return,
            ]),
            x.of(&a),
            y.of(&b),
            z.of(&c),
            // The product, with its top bit raised to 126.
            large.product(&x.significand, &y.significand, &middle),
            get(&x.negative),
            get(&y.negative),
            curios_wasm::Instr::I32Xor,
            set(&x.negative),
            large.clz(),
            i32_const(1),
            curios_wasm::Instr::I32Sub,
            set(&count),
            large.shift_left(&count),
            get(&x.exponent),
            get(&y.exponent),
            curios_wasm::Instr::I32Add,
            get(&count),
            curios_wasm::Instr::I32Sub,
            set(&x.exponent),
            // The addend, its top bit at 126 too.
            get(&z.significand),
            i64_const(10),
            curios_wasm::Instr::I64Shl,
            set(&small.high),
            i64_const(0),
            set(&small.low),
            get(&z.exponent),
            i32_const(74),
            curios_wasm::Instr::I32Sub,
            set(&z.exponent),
            // The larger exponent in `large`, whose sign and exponent `x` carries.
            get(&x.exponent),
            get(&z.exponent),
            curios_wasm::Instr::I32LtS,
            when(wasm![
                large.swap(&small, &t64),
                swap(&x.negative, &z.negative, &t32),
                swap(&x.exponent, &z.exponent, &t32),
            ]),
            get(&x.exponent),
            get(&z.exponent),
            curios_wasm::Instr::I32Sub,
            set(&count),
            i32_const(0),
            set(&sticky),
            small.shift_right_sticky(&count, &sticky),
            get(&x.negative),
            get(&z.negative),
            curios_wasm::Instr::I32Eq,
            branch(
                large.add(&small),
                wasm![
                    // An exact cancellation is `+0`, or `-0` toward negative.
                    large.equals(&small),
                    get(&sticky),
                    curios_wasm::Instr::I32Eqz,
                    curios_wasm::Instr::I32And,
                    when(wasm![
                        is_direction(&direction, Rounding::TowardNegative),
                        signed_zero(),
                        curios_wasm::Instr::Return,
                    ]),
                    // Only at one exponent can the smaller-exponent side be the larger, and then nothing was shifted out of it.
                    small.above(&large),
                    when(wasm![
                        large.swap(&small, &t64),
                        get(&z.negative),
                        set(&x.negative),
                    ]),
                    large.subtract(&small, &sticky),
                ],
            ),
            // The top 64 bits of the sum, the rest sticky.
            i32_const(64),
            large.clz(),
            curios_wasm::Instr::I32Sub,
            set(&count),
            pick(
                vec![get(&count)],
                vec![i32_const(0)],
                wasm![get(&count), i32_const(0), curios_wasm::Instr::I32GtS],
            ),
            set(&count),
            large.shift_right_sticky(&count, &sticky),
            bump(&x.exponent, vec![get(&count)]),
            self.return_packed(&x.negative, &large.low, &x.exponent, &sticky, &direction),
        ];

        self.declare(FltHelper::Fma, scope, instrs);
    }

    /// `flt/round`: past `2^52` every float is integral; below it, the truncation stepped away from zero when what it dropped is at least a half. Both steps are exact.
    fn emit_flt_round(&mut self) {
        let mut scope = Scope::default();
        let a = scope.param("a", f64_type());
        let t = scope.local("t", f64_type());

        let instrs = wasm![
            get(&a),
            curios_wasm::Instr::F64Abs,
            curios_wasm::Instr::F64Const {
                value: 4_503_599_627_370_496.0
            },
            curios_wasm::Instr::F64Lt,
            either(
                f64_type(),
                wasm![
                    get(&a),
                    curios_wasm::Instr::F64Trunc,
                    set(&t),
                    get(&a),
                    get(&t),
                    curios_wasm::Instr::F64Sub,
                    curios_wasm::Instr::F64Abs,
                    curios_wasm::Instr::F64Const { value: 0.5 },
                    curios_wasm::Instr::F64Ge,
                    either(
                        f64_type(),
                        wasm![
                            get(&t),
                            curios_wasm::Instr::F64Const { value: 1.0 },
                            get(&a),
                            curios_wasm::Instr::F64Copysign,
                            curios_wasm::Instr::F64Add,
                        ],
                        vec![get(&t)],
                    ),
                ],
                wasm![
                    is_nan(&a),
                    either(
                        f64_type(),
                        wasm![get(&a), get(&a), self.helper(FltHelper::Nan)],
                        vec![get(&a)],
                    ),
                ],
            ),
        ];

        self.declare(FltHelper::Round, scope, instrs);
    }

    /// `flt/pack`, the model's `round` line for line: a value under half the least subnormal decided whole; one that loses no bit normalized and encoded; otherwise the kept significand stepped up when the direction says so, the carry out of the top renormalized, and an overflow sent where the direction sends it.
    fn emit_flt_pack(&mut self) {
        let mut scope = Scope::default();
        let negative = scope.param("negative", i32_type());
        let significand = scope.param("significand", i64_type());
        let exponent = scope.param("exponent", i32_type());
        let sticky = scope.param("sticky", i32_type());
        let direction = scope.param("direction", i32_type());
        let width = scope.local("width", i32_type());
        let grid = scope.local("grid", i32_type());
        let shift = scope.local("shift", i32_type());
        let left = scope.local("left", i32_type());
        let room = scope.local("room", i32_type());
        let dropped = scope.local("dropped", i64_type());
        let half = scope.local("half", i64_type());
        let above = scope.local("above", i32_type());
        let tie = scope.local("tie", i32_type());
        let odd = scope.local("odd", i32_type());
        let inexact = scope.local("inexact", i32_type());

        let shift64 = || wasm![get(&shift), curios_wasm::Instr::I64ExtendI32U];
        let rounds_up = || rounds_up(&direction, &negative, &above, &tie, &odd, &inexact);
        let encode = || encode(&negative, &significand, &exponent, &direction);

        let instrs = wasm![
            get(&significand),
            curios_wasm::Instr::I64Eqz,
            get(&sticky),
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::I32And,
            when(wasm![
                get(&negative),
                signed_zero(),
                curios_wasm::Instr::Return
            ]),
            i32_const(64),
            get(&significand),
            curios_wasm::Instr::I64Clz,
            curios_wasm::Instr::I32WrapI64,
            curios_wasm::Instr::I32Sub,
            set(&width),
            i32_const(MIN_EXPONENT),
            get(&exponent),
            curios_wasm::Instr::I32Sub,
            set(&grid),
            // The whole value, a residue included, lies under half the least subnormal.
            get(&grid),
            get(&width),
            curios_wasm::Instr::I32GtS,
            when(wasm![
                i32_const(0),
                set(&above),
                i32_const(0),
                set(&tie),
                i32_const(0),
                set(&odd),
                i32_const(1),
                set(&inexact),
                get(&negative),
                sign_bit(),
                rounds_up(),
                curios_wasm::Instr::I64ExtendI32U,
                curios_wasm::Instr::I64Or,
                curios_wasm::Instr::F64ReinterpretI64,
                curios_wasm::Instr::Return,
            ]),
            get(&width),
            i32_const(53),
            curios_wasm::Instr::I32Sub,
            set(&shift),
            max_signed(&shift, &grid),
            set(&shift),
            // Nothing dropped: normalize left until the significand is full or the exponent reaches the grid.
            get(&shift),
            i32_const(0),
            curios_wasm::Instr::I32LeS,
            when(wasm![
                i32_const(0),
                get(&shift),
                curios_wasm::Instr::I32Sub,
                set(&left),
                get(&exponent),
                i32_const(-MIN_EXPONENT),
                curios_wasm::Instr::I32Add,
                set(&room),
                min_signed(&left, &room),
                set(&left),
                get(&significand),
                get(&left),
                curios_wasm::Instr::I64ExtendI32U,
                curios_wasm::Instr::I64Shl,
                set(&significand),
                get(&exponent),
                get(&left),
                curios_wasm::Instr::I32Sub,
                set(&exponent),
                encode(),
                curios_wasm::Instr::Return,
            ]),
            // A shift of 64 drops the whole significand, which Wasm's shifts, reduced modulo 64, cannot say.
            get(&shift),
            i32_const(64),
            curios_wasm::Instr::I32GeU,
            branch(
                wasm![
                    get(&significand),
                    set(&dropped),
                    i64_const(0),
                    set(&significand),
                    i64_const(i64::MIN),
                    set(&half),
                ],
                wasm![
                    get(&significand),
                    i64_const(1),
                    shift64(),
                    curios_wasm::Instr::I64Shl,
                    i64_const(1),
                    curios_wasm::Instr::I64Sub,
                    curios_wasm::Instr::I64And,
                    set(&dropped),
                    get(&significand),
                    shift64(),
                    curios_wasm::Instr::I64ShrU,
                    set(&significand),
                    i64_const(1),
                    shift64(),
                    i64_const(1),
                    curios_wasm::Instr::I64Sub,
                    curios_wasm::Instr::I64Shl,
                    set(&half),
                ],
            ),
            // A residue puts the value past the midpoint when `dropped` sits on it.
            get(&dropped),
            get(&half),
            curios_wasm::Instr::I64GtU,
            get(&dropped),
            get(&half),
            curios_wasm::Instr::I64Eq,
            get(&sticky),
            curios_wasm::Instr::I32And,
            curios_wasm::Instr::I32Or,
            set(&above),
            get(&dropped),
            get(&half),
            curios_wasm::Instr::I64Eq,
            get(&sticky),
            curios_wasm::Instr::I32Eqz,
            curios_wasm::Instr::I32And,
            set(&tie),
            get(&significand),
            curios_wasm::Instr::I32WrapI64,
            i32_const(1),
            curios_wasm::Instr::I32And,
            set(&odd),
            get(&sticky),
            get(&dropped),
            i64_const(0),
            curios_wasm::Instr::I64Ne,
            curios_wasm::Instr::I32Or,
            set(&inexact),
            get(&significand),
            rounds_up(),
            curios_wasm::Instr::I64ExtendI32U,
            curios_wasm::Instr::I64Add,
            set(&significand),
            bump(&exponent, vec![get(&shift)]),
            // Rounding up can carry out of the significand, one bit wider.
            get(&significand),
            i64_const(HIDDEN_BIT << 1),
            curios_wasm::Instr::I64Eq,
            when(wasm![
                i64_const(HIDDEN_BIT),
                set(&significand),
                bump(&exponent, vec![i32_const(1)]),
            ]),
            encode(),
        ];

        self.declare(FltHelper::Pack, scope, instrs);
    }

    /// `flt/nan`: each operand's pattern quieted when it is a NaN and `0` when it is not — no quieted NaN is `0` — then the greater of the two read unsigned, and the default NaN when both were `0`. Unsigned, so the choice does not read the operands' order and a negative NaN outranks every positive one, as [`curios_num::Floating`] chooses.
    fn emit_flt_nan(&mut self) {
        let mut scope = Scope::default();
        let x = scope.param("x", f64_type());
        let y = scope.param("y", f64_type());
        let qx = scope.local("qx", i64_type());
        let qy = scope.local("qy", i64_type());

        let quieted = |operand: &Local, into: &Local| {
            wasm![
                bits(operand),
                i64_const(QUIET_BIT),
                curios_wasm::Instr::I64Or,
                i64_const(0),
                is_nan(operand),
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

        self.declare(FltHelper::Nan, scope, instrs);
    }
}

/// Whether a significand that lost bits steps up to the next one, as an `i32`: [`Rounding::rounds_up`] with the direction chosen at run time. `above` says the lost part is past half a unit, `tie` that it is exactly half, `odd` that the kept significand is odd and `inexact` that anything was lost.
fn rounds_up(
    direction: &Local,
    negative: &Local,
    above: &Local,
    tie: &Local,
    odd: &Local,
    inexact: &Local,
) -> Instrs {
    let toward_negative = wasm![get(inexact), get(negative), curios_wasm::Instr::I32And];
    let toward_positive = wasm![
        get(inexact),
        get(negative),
        curios_wasm::Instr::I32Eqz,
        curios_wasm::Instr::I32And,
    ];
    let ties_to_away = wasm![get(above), get(tie), curios_wasm::Instr::I32Or];
    let ties_to_even = wasm![
        get(above),
        get(tie),
        get(odd),
        curios_wasm::Instr::I32And,
        curios_wasm::Instr::I32Or,
    ];

    pick(
        ties_to_even,
        pick(
            ties_to_away,
            pick(
                vec![i32_const(0)],
                pick(
                    toward_positive,
                    toward_negative,
                    is_direction(direction, Rounding::TowardPositive),
                ),
                is_direction(direction, Rounding::TowardZero),
            ),
            is_direction(direction, Rounding::TiesToAway),
        ),
        is_direction(direction, Rounding::TiesToEven),
    )
}

/// The float a significand under `2^53` and its exponent encode: a subnormal when the hidden bit is clear, which leaves the exponent at the grid; otherwise the biased field, past the largest of which an overflow answers the infinity, or the largest finite value where the direction stops short of it (§7.4).
fn encode(negative: &Local, significand: &Local, exponent: &Local, direction: &Local) -> Instrs {
    let field = || {
        wasm![
            get(exponent),
            i32_const(EXPONENT_BIAS),
            curios_wasm::Instr::I32Add
        ]
    };
    // Every direction to nearest reaches the infinity; toward zero never does; toward an infinity only at its own sign.
    let reaches = pick(
        vec![i32_const(1)],
        pick(
            vec![i32_const(0)],
            pick(
                wasm![get(negative), curios_wasm::Instr::I32Eqz],
                vec![get(negative)],
                is_direction(direction, Rounding::TowardPositive),
            ),
            is_direction(direction, Rounding::TowardZero),
        ),
        wasm![
            get(direction),
            i32_const(rounding_code(Rounding::TowardZero)),
            curios_wasm::Instr::I32LtU,
        ],
    );

    wasm![
        get(negative),
        sign_bit(),
        get(significand),
        i64_const(HIDDEN_BIT),
        curios_wasm::Instr::I64LtU,
        either(
            i64_type(),
            vec![get(significand)],
            wasm![
                field(),
                i32_const(INFINITE_FIELD),
                curios_wasm::Instr::I32GeS,
                either(
                    i64_type(),
                    wasm![
                        i64_const(INFINITY_BITS),
                        i64_const(MAX_FINITE_BITS),
                        reaches,
                        curios_wasm::Instr::Select { val_types: vec![] },
                    ],
                    wasm![
                        field(),
                        curios_wasm::Instr::I64ExtendI32U,
                        i64_const(52),
                        curios_wasm::Instr::I64Shl,
                        get(significand),
                        i64_const(HIDDEN_BIT),
                        curios_wasm::Instr::I64Sub,
                        curios_wasm::Instr::I64Or,
                    ],
                ),
            ],
        ),
        curios_wasm::Instr::I64Or,
        curios_wasm::Instr::F64ReinterpretI64,
    ]
}
