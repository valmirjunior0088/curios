//! The big-number helper functions: the boxed half of every `Nat` and `Int` operation, which the inline fast path in `code_emitter` calls whenever an operand is not an i31 or a result leaves it.
//!
//! A `Nat` and an `Int` share one runtime form, which is a reference: an i31 read signed when the value lies in `[-2³⁰, 2³⁰)`, and otherwise a `$big` — a sign and a trimmed little-endian `$words` array of 32-bit limbs — which is never in that range, so every value has one spelling. The helpers are the `big/` library over that form, serving both carriers, with the few whose meaning is one carrier's named after it: `nat/sub` is monus, and `nat/wire` and `int/wire` narrow to the host wire each carrier crosses. Each helper producing a value answers through `big/norm`, which is what keeps the spelling unique. The algorithms are the schoolbook ones over 32-bit limbs with 64-bit intermediates — division is Knuth's algorithm D as Hacker's Delight's `divmnu` spells it — and every one is a loop rather than a recursion, so an operand's size never reaches the wasm stack.
//!
//! The helpers work in two layers. The `big/` layer takes and gives values in the runtime form, reading the sign; it widens an i31 operand to the boxed shape first, which costs an allocation the slow path can afford. The `mag/` layer, in [`magnitude`], takes and gives bare limb arrays and knows nothing of signs.
//!
//! Emitted on demand in the `rope_emitter` pattern: a call site names a helper through the table, which marks it used, and the module emitter adds the marked set in [`BigHelper::ALL`]'s order — callers before callees — so a helper's body is built, marking what it calls, before the callee is looked at.

mod magnitude;

use super::{
    BigData, Chunk, FltHelper, Scope, Table, block, br, br_if, branch, call, cast, concrete_val,
    declare_helper, either, f64_type, field_get, get, i32_const, i32_type, i64_const, i64_type,
    repeat, set, tee, wasm, when,
};

/// Every big-number helper, named by what it computes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BigHelper {
    /// `(anyref, anyref) -> (ref any)`: monus.
    NatSub,
    /// `(anyref, anyref, i32 flip) -> (ref any)`: the sum, or with `flip` the difference.
    Add,
    /// `(anyref, anyref) -> (ref any)`: the product.
    Mul,
    /// `(anyref, anyref) -> (ref any)`: the quotient, truncated toward zero.
    Div,
    /// `(anyref, anyref) -> (ref any)`: the remainder, with the dividend's sign.
    Rem,
    /// `(anyref, anyref) -> i32`: `-1`, `0` or `1` as the first is below, equal to or above the second.
    Cmp,
    /// `(anyref, anyref, i32 op) -> (ref any)`: [`Bitwise`] `op` over the infinite two's complement.
    Bitwise,
    /// `(anyref, i32 count) -> (ref any)`: the value times `2^count`.
    Shl,
    /// `(anyref, i32 count) -> (ref any)`: the floor of the value over `2^count`.
    Shr,
    /// `(anyref, i32 direction) -> f64`: the float the value rounds to in the direction named, through `flt/pack` — which sends a value past the largest finite one where that direction sends an overflow.
    ToF64,
    /// `(f64) -> (ref any)`: the float truncated toward zero, which the caller has made sure is finite.
    OfF64,
    /// `(anyref) -> i32`: a boxed value as a machine word — exact below `2³² - 1`, saturating there.
    Word,
    /// `(anyref) -> i64`: a boxed `Nat` narrowed to the host wire, refusing one at or past `2⁶⁴`.
    NatWire,
    /// `(anyref) -> i64`: a boxed `Int` narrowed to the host wire, refusing one outside `[-2⁶³, 2⁶³)`.
    IntWire,
    /// `(ref null $big, i32 width) -> (ref $words)`: the two's complement of the value in `width` limbs.
    Twos,
    /// `(i64) -> (ref any)`: a machine integer read signed, in the runtime form.
    OfI64,
    /// `(i64) -> (ref any)`: a machine integer read unsigned, in the runtime form.
    OfU64,
    /// `(anyref) -> (ref $big)`: a value in the boxed shape, an i31 widened into a fresh one.
    Widen,
    /// `(i32 sign, ref null $words) -> (ref any)`: the canonical value with this sign and magnitude — an i31 when it fits, a trimmed boxed magnitude otherwise, and zero never negative.
    Norm,
    /// `(ref null $words, ref null $words, i32 want_rem) -> (ref $words)`: the quotient of two magnitudes, or their remainder.
    MagDivRem,
    /// `(ref null $words, ref null $words) -> (ref $words)`: the product of two magnitudes.
    MagMul,
    /// `(ref null $words, ref null $words) -> (ref $words)`: the sum of two magnitudes.
    MagAdd,
    /// `(ref null $words, ref null $words) -> (ref $words)`: the difference of two magnitudes, the first the larger.
    MagSub,
    /// `(ref null $words, i32 count) -> (ref $words)`: a magnitude times `2^count`.
    MagShl,
    /// `(ref null $words) -> (ref $words)`: the array negated in place as a two's-complement number, and handed back.
    MagNegate,
    /// `(ref null $words, ref null $words) -> i32`: the order of two trimmed magnitudes, as [`BigHelper::Cmp`] answers it.
    MagCmp,
}

impl BigHelper {
    /// Every helper, callers before callees: the order the module emitter visits them in, so that a helper is looked at only after every body that could call it has been built.
    pub(crate) const ALL: [BigHelper; 26] = [
        BigHelper::NatSub,
        BigHelper::Add,
        BigHelper::Mul,
        BigHelper::Div,
        BigHelper::Rem,
        BigHelper::Cmp,
        BigHelper::Bitwise,
        BigHelper::Shl,
        BigHelper::Shr,
        BigHelper::ToF64,
        BigHelper::OfF64,
        BigHelper::Word,
        BigHelper::NatWire,
        BigHelper::IntWire,
        BigHelper::Twos,
        BigHelper::OfI64,
        BigHelper::OfU64,
        BigHelper::Widen,
        BigHelper::Norm,
        BigHelper::MagDivRem,
        BigHelper::MagMul,
        BigHelper::MagAdd,
        BigHelper::MagSub,
        BigHelper::MagShl,
        BigHelper::MagNegate,
        BigHelper::MagCmp,
    ];

    /// This helper's position in [`BigHelper::ALL`], which is its slot in the table's roster.
    pub(crate) fn slot(self) -> usize {
        BigHelper::ALL
            .iter()
            .position(|helper| *helper == self)
            .expect("`BigHelper::ALL` holds every helper")
    }

    pub(crate) fn func_name(self) -> curios_wasm::FuncName {
        curios_wasm::FuncName::from(match self {
            BigHelper::NatSub => "nat/sub",
            BigHelper::Add => "big/add",
            BigHelper::Mul => "big/mul",
            BigHelper::Div => "big/div",
            BigHelper::Rem => "big/rem",
            BigHelper::Cmp => "big/cmp",
            BigHelper::Bitwise => "big/bitwise",
            BigHelper::Shl => "big/shl",
            BigHelper::Shr => "big/shr",
            BigHelper::ToF64 => "big/to_f64",
            BigHelper::OfF64 => "big/of_f64",
            BigHelper::Word => "big/word",
            BigHelper::NatWire => "nat/wire",
            BigHelper::IntWire => "int/wire",
            BigHelper::Twos => "big/twos",
            BigHelper::OfI64 => "big/of_i64",
            BigHelper::OfU64 => "big/of_u64",
            BigHelper::Widen => "big/widen",
            BigHelper::Norm => "big/norm",
            BigHelper::MagDivRem => "mag/divrem",
            BigHelper::MagMul => "mag/mul",
            BigHelper::MagAdd => "mag/add",
            BigHelper::MagSub => "mag/sub",
            BigHelper::MagShl => "mag/shl",
            BigHelper::MagNegate => "mag/negate",
            BigHelper::MagCmp => "mag/cmp",
        })
    }
}

/// Which pointwise combination `big/bitwise` computes, handed to it as an `i32` so the three share one two's-complement walk.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Bitwise {
    And,
    Or,
    Xor,
}

impl Bitwise {
    pub(crate) fn code(self) -> i32 {
        match self {
            Bitwise::And => 0,
            Bitwise::Or => 1,
            Bitwise::Xor => 2,
        }
    }
}

/// Count `i` up from wherever the caller left it to `bound`, running `body` each time: the loop every limb walk is. `label` names the pair, so a walk nested in another takes its own.
fn walk(
    i: &curios_wasm::LocalName,
    bound: Vec<curios_wasm::Instr>,
    label: &str,
    body: Vec<curios_wasm::Instr>,
) -> curios_wasm::Instr {
    let done = format!("{label}_done");

    block(
        &done,
        vec![repeat(
            label,
            wasm![
                get(i),
                bound,
                curios_wasm::Instr::I32GeU,
                br_if(&done),
                body,
                get(i),
                i32_const(1),
                curios_wasm::Instr::I32Add,
                set(i),
                br(label),
            ],
        )],
    )
}

/// Return `value` when `condition` holds, and fall through otherwise.
fn return_if(
    condition: Vec<curios_wasm::Instr>,
    value: Vec<curios_wasm::Instr>,
) -> Vec<curios_wasm::Instr> {
    wasm![condition, when(wasm![value, curios_wasm::Instr::Return])]
}

/// The i31 zero, the answer every helper gives for an empty magnitude.
fn zero() -> Vec<curios_wasm::Instr> {
    vec![i32_const(0), curios_wasm::Instr::RefI31]
}

/// How many limbs `limbs` holds.
fn len(limbs: &curios_wasm::LocalName) -> Vec<curios_wasm::Instr> {
    vec![get(limbs), curios_wasm::Instr::ArrayLen]
}

/// The scope of a helper over two integers, `x` and `y`.
fn binary_scope() -> (Scope, curios_wasm::LocalName, curios_wasm::LocalName) {
    let mut scope = Scope::default();
    let x = scope.param("x", Table::top_type(true));
    let y = scope.param("y", Table::top_type(true));

    (scope, x, y)
}

#[derive(Debug)]
pub(crate) struct BigEmitter<'a, 'b> {
    table: &'a Table<'a>,
    module: &'b mut curios_wasm::Module,
    big: BigData,
}

impl<'a, 'b> BigEmitter<'a, 'b> {
    pub(crate) fn new(table: &'a Table<'a>, module: &'b mut curios_wasm::Module) -> Self {
        Self {
            big: table.big(),
            table,
            module,
        }
    }

    /// Build `helper` and add it to the module.
    pub(crate) fn emit_func(&mut self, helper: BigHelper) {
        match helper {
            BigHelper::NatSub => self.emit_nat_sub(),
            BigHelper::Add => self.emit_big_add(),
            BigHelper::Mul => self.emit_big_mul(),
            BigHelper::Div => self.emit_big_div(false),
            BigHelper::Rem => self.emit_big_div(true),
            BigHelper::Cmp => self.emit_big_cmp(),
            BigHelper::Bitwise => self.emit_big_bitwise(),
            BigHelper::Shl => self.emit_big_shl(),
            BigHelper::Shr => self.emit_big_shr(),
            BigHelper::ToF64 => self.emit_big_to_f64(),
            BigHelper::OfF64 => self.emit_big_of_f64(),
            BigHelper::Word => self.emit_big_word(),
            BigHelper::NatWire => self.emit_nat_wire(),
            BigHelper::IntWire => self.emit_int_wire(),
            BigHelper::Twos => self.emit_big_twos(),
            BigHelper::OfI64 => self.emit_big_of_i64(),
            BigHelper::OfU64 => self.emit_big_of_u64(),
            BigHelper::Widen => self.emit_big_widen(),
            BigHelper::Norm => self.emit_big_norm(),
            BigHelper::MagDivRem => self.emit_mag_divrem(),
            BigHelper::MagMul => self.emit_mag_mul(),
            BigHelper::MagAdd => self.emit_mag_add(),
            BigHelper::MagSub => self.emit_mag_sub(),
            BigHelper::MagShl => self.emit_mag_shl(),
            BigHelper::MagNegate => self.emit_mag_negate(),
            BigHelper::MagCmp => self.emit_mag_cmp(),
        }
    }

    /// Declare `helper` over `scope`, answering `result`.
    fn add_helper(
        &mut self,
        helper: BigHelper,
        scope: Scope,
        result: curios_wasm::ValType,
        instrs: Vec<curios_wasm::Instr>,
    ) {
        declare_helper(self.module, helper.func_name(), scope, result, instrs);
    }

    /// A call to `helper`, marking it for emission.
    fn helper(&self, helper: BigHelper) -> curios_wasm::Instr {
        call(&self.table.big_func(helper))
    }

    /// A limb array, admitting null as every local and parameter holding one does.
    fn words_type(&self) -> curios_wasm::ValType {
        concrete_val(self.big.words.clone(), true)
    }

    /// A limb array that is there, as every helper hands one back.
    fn words_result(&self) -> curios_wasm::ValType {
        concrete_val(self.big.words.clone(), false)
    }

    fn big_type(&self) -> curios_wasm::ValType {
        concrete_val(self.big.big.clone(), true)
    }

    fn limb(
        &self,
        limbs: &curios_wasm::LocalName,
        index: Vec<curios_wasm::Instr>,
    ) -> Vec<curios_wasm::Instr> {
        wasm![
            get(limbs),
            index,
            curios_wasm::Instr::ArrayGet {
                type_name: self.big.words.clone(),
            },
        ]
    }

    /// A limb zero-extended, which is how every 64-bit intermediate reads one.
    fn limb64(
        &self,
        limbs: &curios_wasm::LocalName,
        index: Vec<curios_wasm::Instr>,
    ) -> Vec<curios_wasm::Instr> {
        wasm![self.limb(limbs, index), curios_wasm::Instr::I64ExtendI32U]
    }

    fn store(
        &self,
        limbs: &curios_wasm::LocalName,
        index: Vec<curios_wasm::Instr>,
        value: Vec<curios_wasm::Instr>,
    ) -> Vec<curios_wasm::Instr> {
        wasm![
            get(limbs),
            index,
            value,
            curios_wasm::Instr::ArraySet {
                type_name: self.big.words.clone(),
            },
        ]
    }

    /// A fresh zeroed limb array of the given length.
    fn new_limbs(&self, length: Vec<curios_wasm::Instr>) -> Vec<curios_wasm::Instr> {
        wasm![
            length,
            curios_wasm::Instr::ArrayNewDefault {
                type_name: self.big.words.clone(),
            },
        ]
    }

    /// A fixed limb array of the values already on the stack.
    fn fixed_limbs(&self, length: u32) -> curios_wasm::Instr {
        curios_wasm::Instr::ArrayNewFixed {
            type_name: self.big.words.clone(),
            length,
        }
    }

    fn sign(&self, big: &curios_wasm::LocalName) -> Vec<curios_wasm::Instr> {
        vec![get(big), field_get(&self.big.big, &self.big.sign_field)]
    }

    fn magnitude(&self, big: &curios_wasm::LocalName) -> Vec<curios_wasm::Instr> {
        vec![get(big), field_get(&self.big.big, &self.big.limbs_field)]
    }

    /// A boxed value of `sign` over `limbs`, which must leave an array that is there.
    fn boxed(
        &self,
        sign: Vec<curios_wasm::Instr>,
        limbs: Vec<curios_wasm::Instr>,
    ) -> Vec<curios_wasm::Instr> {
        wasm![
            sign,
            limbs,
            curios_wasm::Instr::StructNew {
                type_name: self.big.big.clone(),
            },
        ]
    }

    /// A magnitude of one or two limbs as the `u64` its bits fill, which is every magnitude the host wire carries.
    fn magnitude64(&self, limbs: &curios_wasm::LocalName) -> Vec<curios_wasm::Instr> {
        wasm![
            self.limb64(limbs, vec![i32_const(0)]),
            len(limbs),
            i32_const(2),
            curios_wasm::Instr::I32Eq,
            either(
                i64_type(),
                wasm![
                    self.limb64(limbs, vec![i32_const(1)]),
                    i64_const(32),
                    curios_wasm::Instr::I64Shl,
                ],
                vec![i64_const(0)],
            ),
            curios_wasm::Instr::I64Or,
        ]
    }

    /// The limbs of the nonzero `u64` magnitude in `m`: one when its high half is clear, two otherwise, the high half passing through `high`.
    fn limbs64(
        &self,
        m: &curios_wasm::LocalName,
        high: &curios_wasm::LocalName,
    ) -> Vec<curios_wasm::Instr> {
        wasm![
            get(m),
            i64_const(32),
            curios_wasm::Instr::I64ShrU,
            curios_wasm::Instr::I32WrapI64,
            tee(high),
            curios_wasm::Instr::I32Eqz,
            either(
                self.words_result(),
                vec![get(m), curios_wasm::Instr::I32WrapI64, self.fixed_limbs(1)],
                vec![
                    get(m),
                    curios_wasm::Instr::I32WrapI64,
                    get(high),
                    self.fixed_limbs(2),
                ],
            ),
        ]
    }

    /// Both operands of a binary helper widened to the boxed shape, into `a` and `b`.
    fn widen_pair(
        &self,
        scope: &mut Scope,
        x: &curios_wasm::LocalName,
        y: &curios_wasm::LocalName,
    ) -> (
        curios_wasm::LocalName,
        curios_wasm::LocalName,
        Vec<curios_wasm::Instr>,
    ) {
        let a = scope.local("a", self.big_type());
        let b = scope.local("b", self.big_type());
        let instrs = wasm![
            get(x),
            self.helper(BigHelper::Widen),
            set(&a),
            get(y),
            self.helper(BigHelper::Widen),
            set(&b),
        ];

        (a, b, instrs)
    }

    /// `nat/sub`: zero when the subtrahend is at least the minuend, the difference otherwise.
    fn emit_nat_sub(&mut self) {
        let (scope, x, y) = binary_scope();
        let instrs = wasm![
            return_if(
                wasm![
                    get(&x),
                    get(&y),
                    self.helper(BigHelper::Cmp),
                    i32_const(0),
                    curios_wasm::Instr::I32LeS,
                ],
                zero(),
            ),
            get(&x),
            get(&y),
            i32_const(1),
            self.helper(BigHelper::Add),
        ];

        self.add_helper(BigHelper::NatSub, scope, Table::top_type(false), instrs);
    }

    /// `big/add`: magnitudes of one sign add, and of two signs subtract the smaller from the larger, which keeps the larger's sign.
    fn emit_big_add(&mut self) {
        let (mut scope, x, y) = binary_scope();
        let flip = scope.param("flip", i32_type());
        let (a, b, widen) = self.widen_pair(&mut scope, &x, &y);
        let sa = scope.local("sa", i32_type());
        let sb = scope.local("sb", i32_type());
        let ma = scope.local("ma", self.words_type());
        let mb = scope.local("mb", self.words_type());
        let order = scope.local("order", i32_type());
        let norm = self.helper(BigHelper::Norm);

        let instrs = wasm![
            widen,
            self.sign(&a),
            set(&sa),
            self.sign(&b),
            get(&flip),
            curios_wasm::Instr::I32Xor,
            set(&sb),
            self.magnitude(&a),
            set(&ma),
            self.magnitude(&b),
            set(&mb),
            return_if(
                vec![get(&sa), get(&sb), curios_wasm::Instr::I32Eq],
                vec![
                    get(&sa),
                    get(&ma),
                    get(&mb),
                    self.helper(BigHelper::MagAdd),
                    norm.clone(),
                ],
            ),
            get(&ma),
            get(&mb),
            self.helper(BigHelper::MagCmp),
            set(&order),
            return_if(vec![get(&order), curios_wasm::Instr::I32Eqz], zero()),
            return_if(
                vec![get(&order), i32_const(0), curios_wasm::Instr::I32GtS],
                vec![
                    get(&sa),
                    get(&ma),
                    get(&mb),
                    self.helper(BigHelper::MagSub),
                    norm.clone(),
                ],
            ),
            get(&sb),
            get(&mb),
            get(&ma),
            self.helper(BigHelper::MagSub),
            norm,
        ];

        self.add_helper(BigHelper::Add, scope, Table::top_type(false), instrs);
    }

    /// `big/mul`: the magnitudes' product under the signs' agreement.
    fn emit_big_mul(&mut self) {
        let (mut scope, x, y) = binary_scope();
        let (a, b, widen) = self.widen_pair(&mut scope, &x, &y);
        let instrs = wasm![
            widen,
            self.sign(&a),
            self.sign(&b),
            curios_wasm::Instr::I32Xor,
            self.magnitude(&a),
            self.magnitude(&b),
            self.helper(BigHelper::MagMul),
            self.helper(BigHelper::Norm),
        ];

        self.add_helper(BigHelper::Mul, scope, Table::top_type(false), instrs);
    }

    /// `big/div` and `big/rem`: truncated division, so the quotient takes the signs' agreement and the remainder the dividend's sign — the pair `d · (x / d) + x % d = x` holds of.
    fn emit_big_div(&mut self, remainder: bool) {
        let (mut scope, x, y) = binary_scope();
        let (a, b, widen) = self.widen_pair(&mut scope, &x, &y);
        let (helper, sign) = match remainder {
            true => (BigHelper::Rem, self.sign(&a)),
            false => (
                BigHelper::Div,
                wasm![self.sign(&a), self.sign(&b), curios_wasm::Instr::I32Xor],
            ),
        };
        let instrs = wasm![
            widen,
            sign,
            self.magnitude(&a),
            self.magnitude(&b),
            i32_const(i32::from(remainder)),
            self.helper(BigHelper::MagDivRem),
            self.helper(BigHelper::Norm),
        ];

        self.add_helper(helper, scope, Table::top_type(false), instrs);
    }

    /// `big/cmp`: two signs order by sign, and one sign by magnitude, reversed below zero. Zero widens unsigned, so it is never mistaken for a negative.
    fn emit_big_cmp(&mut self) {
        let (mut scope, x, y) = binary_scope();
        let (a, b, widen) = self.widen_pair(&mut scope, &x, &y);
        let order = scope.local("order", i32_type());
        let instrs = wasm![
            widen,
            return_if(
                wasm![self.sign(&a), self.sign(&b), curios_wasm::Instr::I32Ne],
                wasm![self.sign(&b), self.sign(&a), curios_wasm::Instr::I32Sub],
            ),
            self.magnitude(&a),
            self.magnitude(&b),
            self.helper(BigHelper::MagCmp),
            set(&order),
            i32_const(0),
            get(&order),
            curios_wasm::Instr::I32Sub,
            get(&order),
            self.sign(&a),
            curios_wasm::Instr::Select { val_types: vec![] },
        ];

        self.add_helper(BigHelper::Cmp, scope, i32_type(), instrs);
    }

    /// `big/bitwise`: both operands in two's complement one limb wider than either magnitude — wide enough that the top limb is all sign — combined limb by limb, and read back as a sign and a magnitude.
    fn emit_big_bitwise(&mut self) {
        let (mut scope, x, y) = binary_scope();
        let op = scope.param("op", i32_type());
        let (a, b, widen) = self.widen_pair(&mut scope, &x, &y);
        let la = scope.local("la", i32_type());
        let lb = scope.local("lb", i32_type());
        let width = scope.local("width", i32_type());
        let ta = scope.local("ta", self.words_type());
        let tb = scope.local("tb", self.words_type());
        let r = scope.local("r", self.words_type());
        let i = scope.local("i", i32_type());
        let p = scope.local("p", i32_type());
        let q = scope.local("q", i32_type());
        let negative = scope.local("negative", i32_type());

        let combine = wasm![
            self.limb(&ta, vec![get(&i)]),
            set(&p),
            self.limb(&tb, vec![get(&i)]),
            set(&q),
            self.store(
                &r,
                vec![get(&i)],
                vec![
                    get(&op),
                    curios_wasm::Instr::I32Eqz,
                    either(
                        i32_type(),
                        vec![get(&p), get(&q), curios_wasm::Instr::I32And],
                        vec![
                            get(&op),
                            i32_const(Bitwise::Or.code()),
                            curios_wasm::Instr::I32Eq,
                            either(
                                i32_type(),
                                vec![get(&p), get(&q), curios_wasm::Instr::I32Or],
                                vec![get(&p), get(&q), curios_wasm::Instr::I32Xor],
                            ),
                        ],
                    ),
                ],
            ),
        ];

        let instrs = wasm![
            widen,
            self.magnitude(&a),
            curios_wasm::Instr::ArrayLen,
            set(&la),
            self.magnitude(&b),
            curios_wasm::Instr::ArrayLen,
            set(&lb),
            get(&la),
            get(&lb),
            get(&la),
            get(&lb),
            curios_wasm::Instr::I32GtU,
            curios_wasm::Instr::Select { val_types: vec![] },
            i32_const(1),
            curios_wasm::Instr::I32Add,
            set(&width),
            get(&a),
            get(&width),
            self.helper(BigHelper::Twos),
            set(&ta),
            get(&b),
            get(&width),
            self.helper(BigHelper::Twos),
            set(&tb),
            self.new_limbs(vec![get(&width)]),
            set(&r),
            i32_const(0),
            set(&i),
            walk(&i, vec![get(&width)], "combine", combine),
            self.limb(
                &r,
                vec![get(&width), i32_const(1), curios_wasm::Instr::I32Sub],
            ),
            i32_const(31),
            curios_wasm::Instr::I32ShrU,
            set(&negative),
            get(&negative),
            when(vec![
                get(&r),
                self.helper(BigHelper::MagNegate),
                curios_wasm::Instr::Drop,
            ]),
            get(&negative),
            get(&r),
            self.helper(BigHelper::Norm),
        ];

        self.add_helper(BigHelper::Bitwise, scope, Table::top_type(false), instrs);
    }

    /// `big/twos`: the magnitude copied into `width` limbs, negated there when the sign says so.
    fn emit_big_twos(&mut self) {
        let mut scope = Scope::default();
        let big = scope.param("big", self.big_type());
        let width = scope.param("width", i32_type());
        let t = scope.local("t", self.words_type());
        let m = scope.local("m", self.words_type());
        let instrs = wasm![
            self.new_limbs(vec![get(&width)]),
            set(&t),
            self.magnitude(&big),
            set(&m),
            get(&t),
            i32_const(0),
            get(&m),
            i32_const(0),
            len(&m),
            curios_wasm::Instr::ArrayCopy {
                target_name: self.big.words.clone(),
                source_name: self.big.words.clone(),
            },
            self.sign(&big),
            when(vec![
                get(&t),
                self.helper(BigHelper::MagNegate),
                curios_wasm::Instr::Drop,
            ]),
            get(&t),
            curios_wasm::Instr::RefAsNonNull,
        ];

        self.add_helper(BigHelper::Twos, scope, self.words_result(), instrs);
    }

    /// `big/shl`: the magnitude shifted, the sign kept — multiplying by a power of two moves no sign. Zero answers itself before a count could size an allocation.
    fn emit_big_shl(&mut self) {
        let mut scope = Scope::default();
        let x = scope.param("x", Table::top_type(true));
        let count = scope.param("count", i32_type());
        let a = scope.local("a", self.big_type());
        let m = scope.local("m", self.words_type());
        let instrs = wasm![
            get(&x),
            self.helper(BigHelper::Widen),
            set(&a),
            self.magnitude(&a),
            set(&m),
            return_if(wasm![len(&m), curios_wasm::Instr::I32Eqz], zero()),
            self.sign(&a),
            get(&m),
            get(&count),
            self.helper(BigHelper::MagShl),
            self.helper(BigHelper::Norm),
        ];

        self.add_helper(BigHelper::Shl, scope, Table::top_type(false), instrs);
    }

    /// `big/shr`: the floor of the value over `2^count`. The magnitude shifts right, and a negative value whose shift dropped a set bit rounds its magnitude up — `⌊-x / 2^k⌋ = -⌈x / 2^k⌉`. A count past every limb answers the sign alone.
    fn emit_big_shr(&mut self) {
        let mut scope = Scope::default();
        let x = scope.param("x", Table::top_type(true));
        let count = scope.param("count", i32_type());
        let a = scope.local("a", self.big_type());
        let m = scope.local("m", self.words_type());
        let sa = scope.local("sa", i32_type());
        let la = scope.local("la", i32_type());
        let skip = scope.local("skip", i32_type());
        let bits = scope.local("bits", i32_type());
        let width = scope.local("width", i32_type());
        let r = scope.local("r", self.words_type());
        let i = scope.local("i", i32_type());
        let dropped = scope.local("dropped", i32_type());

        let above = || vec![get(&i), get(&skip), curios_wasm::Instr::I32Add];
        let shift = self.store(
            &r,
            vec![get(&i)],
            wasm![
                above(),
                i32_const(1),
                curios_wasm::Instr::I32Add,
                get(&la),
                curios_wasm::Instr::I32LtU,
                either(
                    i64_type(),
                    self.limb64(&m, wasm![above(), i32_const(1), curios_wasm::Instr::I32Add]),
                    vec![i64_const(0)],
                ),
                i64_const(32),
                curios_wasm::Instr::I64Shl,
                self.limb64(&m, above()),
                curios_wasm::Instr::I64Or,
                get(&bits),
                curios_wasm::Instr::I64ExtendI32U,
                curios_wasm::Instr::I64ShrU,
                curios_wasm::Instr::I32WrapI64,
            ],
        );

        let round_up = wasm![
            self.limb(&m, vec![get(&skip)]),
            i32_const(1),
            get(&bits),
            curios_wasm::Instr::I32Shl,
            i32_const(1),
            curios_wasm::Instr::I32Sub,
            curios_wasm::Instr::I32And,
            set(&dropped),
            i32_const(0),
            set(&i),
            walk(
                &i,
                vec![get(&skip)],
                "sticky",
                wasm![
                    self.limb(&m, vec![get(&i)]),
                    get(&dropped),
                    curios_wasm::Instr::I32Or,
                    set(&dropped),
                ],
            ),
            get(&dropped),
            when(vec![
                get(&r),
                i32_const(1),
                self.fixed_limbs(1),
                self.helper(BigHelper::MagAdd),
                set(&r),
            ]),
        ];

        let instrs = wasm![
            get(&x),
            self.helper(BigHelper::Widen),
            set(&a),
            self.magnitude(&a),
            set(&m),
            self.sign(&a),
            set(&sa),
            len(&m),
            set(&la),
            get(&count),
            i32_const(5),
            curios_wasm::Instr::I32ShrU,
            set(&skip),
            get(&count),
            i32_const(31),
            curios_wasm::Instr::I32And,
            set(&bits),
            return_if(
                vec![get(&skip), get(&la), curios_wasm::Instr::I32GeU],
                vec![
                    i32_const(-1),
                    i32_const(0),
                    get(&sa),
                    curios_wasm::Instr::Select { val_types: vec![] },
                    curios_wasm::Instr::RefI31,
                ],
            ),
            get(&la),
            get(&skip),
            curios_wasm::Instr::I32Sub,
            set(&width),
            self.new_limbs(vec![get(&width)]),
            set(&r),
            i32_const(0),
            set(&i),
            walk(&i, vec![get(&width)], "shift", shift),
            get(&sa),
            when(round_up),
            get(&sa),
            get(&r),
            self.helper(BigHelper::Norm),
        ];

        self.add_helper(BigHelper::Shr, scope, Table::top_type(false), instrs);
    }

    /// `big/to_f64`: an i31 converts exactly; a magnitude of at most 64 bits converts through `f64.convert_i64_u`, which rounds to nearest, ties to even; a wider one converts its top 64 bits with every lower set bit ORed into the last, which keeps a tie a tie only when it is one, and is scaled back by its power of two — exact, since a normal float times a power of two only ever overflows, and overflow is `inf` as rounding would have made it.
    fn emit_big_to_f64(&mut self) {
        let mut scope = Scope::default();
        let x = scope.param("x", Table::top_type(true));
        let direction = scope.param("direction", i32_type());
        let b = scope.local("b", self.big_type());
        let m = scope.local("m", self.words_type());
        let la = scope.local("la", i32_type());
        let width = scope.local("width", i32_type());
        let shift = scope.local("shift", i32_type());
        let skip = scope.local("skip", i32_type());
        let bits = scope.local("bits", i32_type());
        let sticky = scope.local("sticky", i32_type());
        let i = scope.local("i", i32_type());
        let w = scope.local("w", i64_type());
        let i31 = Table::int_type(false);
        let from = |offset: i32| wasm![get(&skip), i32_const(offset), curios_wasm::Instr::I32Add];

        // At most 64 bits: the magnitude is the significand, exact, with nothing sticky.
        let narrow = wasm![
            self.limb64(&m, vec![i32_const(0)]),
            get(&la),
            i32_const(1),
            curios_wasm::Instr::I32GtU,
            either(
                i64_type(),
                wasm![
                    self.limb64(&m, vec![i32_const(1)]),
                    i64_const(32),
                    curios_wasm::Instr::I64Shl,
                ],
                vec![i64_const(0)],
            ),
            curios_wasm::Instr::I64Or,
            set(&w),
            i32_const(0),
            set(&shift),
            i32_const(0),
            set(&sticky),
        ];

        let window = either(
            i64_type(),
            wasm![
                self.limb64(&m, vec![get(&skip)]),
                self.limb64(&m, from(1)),
                i64_const(32),
                curios_wasm::Instr::I64Shl,
                curios_wasm::Instr::I64Or,
            ],
            wasm![
                self.limb64(&m, vec![get(&skip)]),
                get(&bits),
                curios_wasm::Instr::I64ExtendI32U,
                curios_wasm::Instr::I64ShrU,
                self.limb64(&m, from(1)),
                i64_const(32),
                get(&bits),
                curios_wasm::Instr::I64ExtendI32U,
                curios_wasm::Instr::I64Sub,
                curios_wasm::Instr::I64Shl,
                curios_wasm::Instr::I64Or,
                self.limb64(&m, from(2)),
                i64_const(64),
                get(&bits),
                curios_wasm::Instr::I64ExtendI32U,
                curios_wasm::Instr::I64Sub,
                curios_wasm::Instr::I64Shl,
                curios_wasm::Instr::I64Or,
            ],
        );

        let wide = wasm![
            get(&width),
            i32_const(64),
            curios_wasm::Instr::I32Sub,
            set(&shift),
            get(&shift),
            i32_const(5),
            curios_wasm::Instr::I32ShrU,
            set(&skip),
            get(&shift),
            i32_const(31),
            curios_wasm::Instr::I32And,
            set(&bits),
            get(&bits),
            curios_wasm::Instr::I32Eqz,
            window,
            set(&w),
            self.limb(&m, vec![get(&skip)]),
            i32_const(1),
            get(&bits),
            curios_wasm::Instr::I32Shl,
            i32_const(1),
            curios_wasm::Instr::I32Sub,
            curios_wasm::Instr::I32And,
            set(&sticky),
            i32_const(0),
            set(&i),
            walk(
                &i,
                vec![get(&skip)],
                "sticky",
                wasm![
                    self.limb(&m, vec![get(&i)]),
                    get(&sticky),
                    curios_wasm::Instr::I32Or,
                    set(&sticky),
                ],
            ),
            get(&sticky),
            i32_const(0),
            curios_wasm::Instr::I32Ne,
            set(&sticky),
        ];

        let instrs = wasm![
            get(&x),
            curios_wasm::Instr::RefTest {
                ref_type: i31.clone()
            },
            when(vec![
                get(&x),
                curios_wasm::Instr::RefCast { ref_type: i31 },
                curios_wasm::Instr::I31GetS,
                curios_wasm::Instr::F64ConvertI32S,
                curios_wasm::Instr::Return,
            ]),
            get(&x),
            cast(&self.big.big),
            set(&b),
            self.magnitude(&b),
            set(&m),
            len(&m),
            set(&la),
            get(&la),
            i32_const(5),
            curios_wasm::Instr::I32Shl,
            self.limb(&m, vec![get(&la), i32_const(1), curios_wasm::Instr::I32Sub]),
            curios_wasm::Instr::I32Clz,
            curios_wasm::Instr::I32Sub,
            set(&width),
            get(&width),
            i32_const(64),
            curios_wasm::Instr::I32LeU,
            branch(narrow, wide),
            // The top 64 bits of the magnitude at `2^shift`, the rest sticky, rounded once in the direction named.
            self.sign(&b),
            i32_const(0),
            curios_wasm::Instr::I32Ne,
            get(&w),
            get(&shift),
            get(&sticky),
            get(&direction),
            call(&self.table.flt_func(FltHelper::Pack)),
        ];

        self.add_helper(BigHelper::ToF64, scope, f64_type(), instrs);
    }

    /// `big/of_f64`: a truncation below `2⁶³` in magnitude converts through `i64.trunc_f64_s`; a larger one is its 53-bit significand shifted left by its exponent, which is at least 11 there.
    fn emit_big_of_f64(&mut self) {
        let mut scope = Scope::default();
        let f = scope.param("f", f64_type());
        let t = scope.local("t", f64_type());
        let bits = scope.local("bits", i64_type());
        let exponent = scope.local("exponent", i32_type());
        let significand = scope.local("significand", i64_type());
        let instrs = wasm![
            get(&f),
            curios_wasm::Instr::F64Trunc,
            set(&t),
            return_if(
                vec![
                    get(&t),
                    curios_wasm::Instr::F64Abs,
                    curios_wasm::Instr::F64Const {
                        value: 9_223_372_036_854_775_808.0,
                    },
                    curios_wasm::Instr::F64Lt,
                ],
                vec![
                    get(&t),
                    curios_wasm::Instr::I64TruncF64S,
                    self.helper(BigHelper::OfI64),
                ],
            ),
            get(&t),
            curios_wasm::Instr::I64ReinterpretF64,
            set(&bits),
            get(&bits),
            i64_const(52),
            curios_wasm::Instr::I64ShrU,
            i64_const(0x7ff),
            curios_wasm::Instr::I64And,
            curios_wasm::Instr::I32WrapI64,
            i32_const(1075),
            curios_wasm::Instr::I32Sub,
            set(&exponent),
            get(&bits),
            i64_const(0xf_ffff_ffff_ffff),
            curios_wasm::Instr::I64And,
            i64_const(0x10_0000_0000_0000),
            curios_wasm::Instr::I64Or,
            set(&significand),
            get(&bits),
            i64_const(63),
            curios_wasm::Instr::I64ShrU,
            curios_wasm::Instr::I32WrapI64,
            get(&significand),
            curios_wasm::Instr::I32WrapI64,
            get(&significand),
            i64_const(32),
            curios_wasm::Instr::I64ShrU,
            curios_wasm::Instr::I32WrapI64,
            self.fixed_limbs(2),
            get(&exponent),
            self.helper(BigHelper::MagShl),
            self.helper(BigHelper::Norm),
        ];

        self.add_helper(BigHelper::OfF64, scope, Table::top_type(false), instrs);
    }

    /// `big/word`: a boxed value is at least `2³⁰` in magnitude, so one limb and no sign is the whole of what a word holds exactly; every other value saturates to the top word, which no position reaches and no switch key names.
    fn emit_big_word(&mut self) {
        let mut scope = Scope::default();
        let x = scope.param("x", Table::top_type(true));
        let b = scope.local("b", self.big_type());
        let m = scope.local("m", self.words_type());
        let instrs = wasm![
            get(&x),
            cast(&self.big.big),
            set(&b),
            self.magnitude(&b),
            set(&m),
            return_if(
                wasm![
                    self.sign(&b),
                    len(&m),
                    i32_const(1),
                    curios_wasm::Instr::I32Ne,
                    curios_wasm::Instr::I32Or,
                ],
                vec![i32_const(-1)],
            ),
            self.limb(&m, vec![i32_const(0)]),
        ];

        self.add_helper(BigHelper::Word, scope, i32_type(), instrs);
    }

    /// `nat/wire`: the host reads a `Nat` argument as an `i64` read unsigned, so a boxed one crosses when it is at most two limbs, below `2⁶⁴`, and refuses otherwise.
    fn emit_nat_wire(&mut self) {
        let mut scope = Scope::default();
        let x = scope.param("x", Table::top_type(true));
        let b = scope.local("b", self.big_type());
        let m = scope.local("m", self.words_type());
        let instrs = wasm![
            get(&x),
            cast(&self.big.big),
            set(&b),
            self.magnitude(&b),
            set(&m),
            return_if(
                wasm![
                    self.sign(&b),
                    curios_wasm::Instr::I32Eqz,
                    len(&m),
                    i32_const(2),
                    curios_wasm::Instr::I32LeU,
                    curios_wasm::Instr::I32And,
                ],
                self.magnitude64(&m),
            ),
            self.table.refuse_instrs(curios_cont::Panic::NatWire),
        ];

        self.add_helper(BigHelper::NatWire, scope, i64_type(), instrs);
    }

    /// `int/wire`: the host reads an `Int` argument as an `i64`, so a boxed one crosses when it is at most two limbs inside `[-2⁶³, 2⁶³)` and refuses otherwise. The negation wraps at `2⁶³`, whose negative is exactly `i64::MIN`.
    fn emit_int_wire(&mut self) {
        let mut scope = Scope::default();
        let x = scope.param("x", Table::top_type(true));
        let b = scope.local("b", self.big_type());
        let m = scope.local("m", self.words_type());
        let v = scope.local("v", i64_type());
        let instrs = wasm![
            get(&x),
            cast(&self.big.big),
            set(&b),
            self.magnitude(&b),
            set(&m),
            len(&m),
            i32_const(2),
            curios_wasm::Instr::I32LeU,
            when(wasm![
                self.magnitude64(&m),
                set(&v),
                return_if(
                    wasm![
                        self.sign(&b),
                        curios_wasm::Instr::I32Eqz,
                        get(&v),
                        i64_const(0),
                        curios_wasm::Instr::I64GeS,
                        curios_wasm::Instr::I32And,
                    ],
                    vec![get(&v)],
                ),
                return_if(
                    wasm![
                        self.sign(&b),
                        get(&v),
                        i64_const(i64::MIN),
                        curios_wasm::Instr::I64LeU,
                        curios_wasm::Instr::I32And,
                    ],
                    vec![i64_const(0), get(&v), curios_wasm::Instr::I64Sub],
                ),
            ]),
            self.table.refuse_instrs(curios_cont::Panic::IntWire),
        ];

        self.add_helper(BigHelper::IntWire, scope, i64_type(), instrs);
    }

    /// `big/of_i64`: an i31 when the value sign-extends from bit 30, and otherwise a boxed magnitude of one or two limbs. The negation wraps at `i64::MIN`, whose magnitude read unsigned is exactly `2⁶³`.
    fn emit_big_of_i64(&mut self) {
        let mut scope = Scope::default();
        let v = scope.param("v", i64_type());
        let negative = scope.local("negative", i32_type());
        let m = scope.local("m", i64_type());
        let high = scope.local("high", i32_type());
        let instrs = wasm![
            return_if(
                vec![
                    get(&v),
                    i64_const(33),
                    curios_wasm::Instr::I64Shl,
                    i64_const(33),
                    curios_wasm::Instr::I64ShrS,
                    get(&v),
                    curios_wasm::Instr::I64Eq,
                ],
                vec![
                    get(&v),
                    curios_wasm::Instr::I32WrapI64,
                    curios_wasm::Instr::RefI31,
                ],
            ),
            get(&v),
            i64_const(0),
            curios_wasm::Instr::I64LtS,
            set(&negative),
            i64_const(0),
            get(&v),
            curios_wasm::Instr::I64Sub,
            get(&v),
            get(&negative),
            curios_wasm::Instr::Select { val_types: vec![] },
            set(&m),
            self.boxed(vec![get(&negative)], self.limbs64(&m, &high)),
        ];

        self.add_helper(BigHelper::OfI64, scope, Table::top_type(false), instrs);
    }

    /// `big/of_u64`: an i31 when the value is below `2³⁰`, and otherwise a non-negative boxed magnitude of one or two limbs.
    fn emit_big_of_u64(&mut self) {
        let mut scope = Scope::default();
        let v = scope.param("v", i64_type());
        let high = scope.local("high", i32_type());
        let instrs = wasm![
            return_if(
                vec![get(&v), i64_const(1 << 30), curios_wasm::Instr::I64LtU],
                vec![
                    get(&v),
                    curios_wasm::Instr::I32WrapI64,
                    curios_wasm::Instr::RefI31,
                ],
            ),
            self.boxed(vec![i32_const(0)], self.limbs64(&v, &high)),
        ];

        self.add_helper(BigHelper::OfU64, scope, Table::top_type(false), instrs);
    }

    /// `big/widen`: a boxed value is already the shape; an i31 becomes one — zero with no limbs, so a magnitude comparison and a sign test both read it right.
    fn emit_big_widen(&mut self) {
        let mut scope = Scope::default();
        let x = scope.param("x", Table::top_type(true));
        let v = scope.local("v", i32_type());
        let i31 = Table::int_type(false);
        let instrs = wasm![
            get(&x),
            curios_wasm::Instr::RefTest {
                ref_type: i31.clone()
            },
            when(wasm![
                get(&x),
                curios_wasm::Instr::RefCast { ref_type: i31 },
                curios_wasm::Instr::I31GetS,
                set(&v),
                return_if(
                    vec![get(&v), i32_const(0), curios_wasm::Instr::I32LtS],
                    self.boxed(
                        vec![i32_const(1)],
                        vec![
                            i32_const(0),
                            get(&v),
                            curios_wasm::Instr::I32Sub,
                            self.fixed_limbs(1),
                        ],
                    ),
                ),
                return_if(
                    vec![get(&v), curios_wasm::Instr::I32Eqz],
                    self.boxed(vec![i32_const(0)], vec![self.fixed_limbs(0)]),
                ),
                self.boxed(vec![i32_const(0)], vec![get(&v), self.fixed_limbs(1)]),
                curios_wasm::Instr::Return,
            ]),
            get(&x),
            cast(&self.big.big),
        ];

        self.add_helper(
            BigHelper::Widen,
            scope,
            concrete_val(self.big.big.clone(), false),
            instrs,
        );
    }

    /// `big/norm`: trim the magnitude's zero top limbs, answer an i31 when what is left fits one — below `2³⁰` unsigned, at most `2³⁰` negated — and otherwise box a trimmed array, copying only when trimming shortened it.
    fn emit_big_norm(&mut self) {
        let mut scope = Scope::default();
        let sign = scope.param("sign", i32_type());
        let a = scope.param("a", self.words_type());
        let n = scope.local("n", i32_type());
        let m = scope.local("m", i32_type());
        let t = scope.local("t", self.words_type());
        let bound = i32_const(1 << (curios_cont::ENVELOPE_BITS - 1));
        let instrs = wasm![
            len(&a),
            set(&n),
            block(
                "trimmed",
                vec![repeat(
                    "trim",
                    wasm![
                        get(&n),
                        curios_wasm::Instr::I32Eqz,
                        br_if("trimmed"),
                        self.limb(&a, vec![get(&n), i32_const(1), curios_wasm::Instr::I32Sub]),
                        br_if("trimmed"),
                        get(&n),
                        i32_const(1),
                        curios_wasm::Instr::I32Sub,
                        set(&n),
                        br("trim"),
                    ],
                )],
            ),
            return_if(vec![get(&n), curios_wasm::Instr::I32Eqz], zero()),
            get(&n),
            i32_const(1),
            curios_wasm::Instr::I32Eq,
            when(wasm![
                self.limb(&a, vec![i32_const(0)]),
                set(&m),
                return_if(
                    vec![
                        get(&sign),
                        curios_wasm::Instr::I32Eqz,
                        get(&m),
                        bound.clone(),
                        curios_wasm::Instr::I32LtU,
                        curios_wasm::Instr::I32And,
                    ],
                    vec![get(&m), curios_wasm::Instr::RefI31],
                ),
                return_if(
                    vec![
                        get(&sign),
                        get(&m),
                        bound,
                        curios_wasm::Instr::I32LeU,
                        curios_wasm::Instr::I32And,
                    ],
                    vec![
                        i32_const(0),
                        get(&m),
                        curios_wasm::Instr::I32Sub,
                        curios_wasm::Instr::RefI31,
                    ],
                ),
            ]),
            get(&n),
            len(&a),
            curios_wasm::Instr::I32Ne,
            when(wasm![
                self.new_limbs(vec![get(&n)]),
                set(&t),
                get(&t),
                i32_const(0),
                get(&a),
                i32_const(0),
                get(&n),
                curios_wasm::Instr::ArrayCopy {
                    target_name: self.big.words.clone(),
                    source_name: self.big.words.clone(),
                },
                get(&t),
                set(&a),
            ]),
            self.boxed(
                vec![get(&sign)],
                vec![get(&a), curios_wasm::Instr::RefAsNonNull]
            ),
        ];

        self.add_helper(BigHelper::Norm, scope, Table::top_type(false), instrs);
    }
}
