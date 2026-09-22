//! The `mag/` layer: arithmetic over bare magnitudes — `$limbs` arrays, least significant limb first — with no sign anywhere. Each helper allocates the array it answers and never writes one it was handed, which is what lets a literal's array be shared by every use of it.
//!
//! Inputs are trimmed wherever an order is read (`mag/cmp`, and `mag/divrem` through it) and may carry zero top limbs elsewhere; outputs are sized for the worst case and left untrimmed, since the `big/norm` every result passes through trims once.

use super::{
    BigEmitter, BigHelper, Chunk, Scope, block, br, br_if, either, get, i32_const, i32_type,
    i64_const, i64_type, len, repeat, return_if, set, tee, walk, wasm, when,
};

/// The limb base, `2³²`, as the 64-bit intermediates compare against it.
const BASE: i64 = 1 << 32;

impl BigEmitter<'_, '_> {
    fn magnitude_pair(&self) -> (Scope, curios_wasm::LocalName, curios_wasm::LocalName) {
        let mut scope = Scope::default();
        let a = scope.param("a", self.limbs_type());
        let b = scope.param("b", self.limbs_type());

        (scope, a, b)
    }

    /// `mag/cmp`: a longer trimmed magnitude is the larger; equal lengths compare limb by limb from the top.
    pub(super) fn emit_mag_cmp(&mut self) {
        let (mut scope, a, b) = self.magnitude_pair();
        let i = scope.local("i", i32_type());
        let x = scope.local("x", i32_type());
        let y = scope.local("y", i32_type());
        let order = |less: Vec<curios_wasm::Instr>| {
            wasm![
                less,
                either(i32_type(), vec![i32_const(-1)], vec![i32_const(1)]),
            ]
        };
        let instrs = wasm![
            return_if(
                wasm![len(&a), len(&b), curios_wasm::Instr::I32Ne],
                order(wasm![len(&a), len(&b), curios_wasm::Instr::I32LtU]),
            ),
            len(&a),
            set(&i),
            repeat(
                "scan",
                wasm![
                    return_if(
                        vec![get(&i), curios_wasm::Instr::I32Eqz],
                        vec![i32_const(0)]
                    ),
                    get(&i),
                    i32_const(1),
                    curios_wasm::Instr::I32Sub,
                    set(&i),
                    self.limb(&a, vec![get(&i)]),
                    set(&x),
                    self.limb(&b, vec![get(&i)]),
                    set(&y),
                    return_if(
                        vec![get(&x), get(&y), curios_wasm::Instr::I32Ne],
                        order(vec![get(&x), get(&y), curios_wasm::Instr::I32LtU]),
                    ),
                    br("scan"),
                ],
            ),
            curios_wasm::Instr::Unreachable,
        ];

        self.add_helper(BigHelper::MagCmp, scope, i32_type(), instrs);
    }

    /// `mag/add`: one pass over the longer operand, carrying into a limb past it.
    pub(super) fn emit_mag_add(&mut self) {
        let (mut scope, a, b) = self.magnitude_pair();
        let t = scope.local("t", self.limbs_type());
        let la = scope.local("la", i32_type());
        let lb = scope.local("lb", i32_type());
        let i = scope.local("i", i32_type());
        let r = scope.local("r", self.limbs_type());
        let carry = scope.local("carry", i64_type());
        let sum = scope.local("sum", i64_type());
        let instrs = wasm![
            len(&a),
            set(&la),
            len(&b),
            set(&lb),
            get(&la),
            get(&lb),
            curios_wasm::Instr::I32LtU,
            when_swapped(&a, &b, &t, &la, &lb, &i),
            self.new_limbs(vec![get(&la), i32_const(1), curios_wasm::Instr::I32Add]),
            set(&r),
            i32_const(0),
            set(&i),
            i64_const(0),
            set(&carry),
            walk(
                &i,
                vec![get(&la)],
                "add",
                wasm![
                    self.limb64(&a, vec![get(&i)]),
                    get(&i),
                    get(&lb),
                    curios_wasm::Instr::I32LtU,
                    either(
                        i64_type(),
                        self.limb64(&b, vec![get(&i)]),
                        vec![i64_const(0)]
                    ),
                    curios_wasm::Instr::I64Add,
                    get(&carry),
                    curios_wasm::Instr::I64Add,
                    set(&sum),
                    self.store(
                        &r,
                        vec![get(&i)],
                        vec![get(&sum), curios_wasm::Instr::I32WrapI64]
                    ),
                    get(&sum),
                    i64_const(32),
                    curios_wasm::Instr::I64ShrU,
                    set(&carry),
                ],
            ),
            self.store(
                &r,
                vec![get(&la)],
                vec![get(&carry), curios_wasm::Instr::I32WrapI64]
            ),
            get(&r),
            curios_wasm::Instr::RefAsNonNull,
        ];

        self.add_helper(BigHelper::MagAdd, scope, self.limbs_result(), instrs);
    }

    /// `mag/sub`: one pass over the larger operand, borrowing out of the next limb whenever a difference goes negative.
    pub(super) fn emit_mag_sub(&mut self) {
        let (mut scope, a, b) = self.magnitude_pair();
        let la = scope.local("la", i32_type());
        let lb = scope.local("lb", i32_type());
        let i = scope.local("i", i32_type());
        let r = scope.local("r", self.limbs_type());
        let borrow = scope.local("borrow", i64_type());
        let difference = scope.local("difference", i64_type());
        let instrs = wasm![
            len(&a),
            set(&la),
            len(&b),
            set(&lb),
            self.new_limbs(vec![get(&la)]),
            set(&r),
            i32_const(0),
            set(&i),
            i64_const(0),
            set(&borrow),
            walk(
                &i,
                vec![get(&la)],
                "subtract",
                wasm![
                    self.limb64(&a, vec![get(&i)]),
                    get(&i),
                    get(&lb),
                    curios_wasm::Instr::I32LtU,
                    either(
                        i64_type(),
                        self.limb64(&b, vec![get(&i)]),
                        vec![i64_const(0)]
                    ),
                    curios_wasm::Instr::I64Sub,
                    get(&borrow),
                    curios_wasm::Instr::I64Sub,
                    set(&difference),
                    self.store(
                        &r,
                        vec![get(&i)],
                        vec![get(&difference), curios_wasm::Instr::I32WrapI64],
                    ),
                    get(&difference),
                    i64_const(63),
                    curios_wasm::Instr::I64ShrU,
                    set(&borrow),
                ],
            ),
            get(&r),
            curios_wasm::Instr::RefAsNonNull,
        ];

        self.add_helper(BigHelper::MagSub, scope, self.limbs_result(), instrs);
    }

    /// `mag/mul`: the schoolbook product, one row per limb of the first operand. A limb product plus a limb plus a carry is at most `2⁶⁴ - 1`, so a row never overflows its 64-bit accumulator.
    pub(super) fn emit_mag_mul(&mut self) {
        let (mut scope, a, b) = self.magnitude_pair();
        let la = scope.local("la", i32_type());
        let lb = scope.local("lb", i32_type());
        let i = scope.local("i", i32_type());
        let j = scope.local("j", i32_type());
        let r = scope.local("r", self.limbs_type());
        let carry = scope.local("carry", i64_type());
        let factor = scope.local("factor", i64_type());
        let t = scope.local("t", i64_type());
        let at = || vec![get(&i), get(&j), curios_wasm::Instr::I32Add];

        let row = wasm![
            self.limb64(&a, vec![get(&i)]),
            set(&factor),
            i64_const(0),
            set(&carry),
            i32_const(0),
            set(&j),
            walk(
                &j,
                vec![get(&lb)],
                "column",
                wasm![
                    get(&factor),
                    self.limb64(&b, vec![get(&j)]),
                    curios_wasm::Instr::I64Mul,
                    self.limb64(&r, at()),
                    curios_wasm::Instr::I64Add,
                    get(&carry),
                    curios_wasm::Instr::I64Add,
                    set(&t),
                    self.store(&r, at(), vec![get(&t), curios_wasm::Instr::I32WrapI64]),
                    get(&t),
                    i64_const(32),
                    curios_wasm::Instr::I64ShrU,
                    set(&carry),
                ],
            ),
            self.store(
                &r,
                vec![get(&i), get(&lb), curios_wasm::Instr::I32Add],
                vec![get(&carry), curios_wasm::Instr::I32WrapI64],
            ),
        ];

        let instrs = wasm![
            len(&a),
            set(&la),
            len(&b),
            set(&lb),
            self.new_limbs(vec![get(&la), get(&lb), curios_wasm::Instr::I32Add]),
            set(&r),
            i32_const(0),
            set(&i),
            walk(&i, vec![get(&la)], "row", row),
            get(&r),
            curios_wasm::Instr::RefAsNonNull,
        ];

        self.add_helper(BigHelper::MagMul, scope, self.limbs_result(), instrs);
    }

    /// `mag/shl`: whole limbs of the count become an offset and the rest a shift within a limb, each limb spilling its high bits into the next.
    pub(super) fn emit_mag_shl(&mut self) {
        let mut scope = Scope::default();
        let a = scope.param("a", self.limbs_type());
        let count = scope.param("count", i32_type());
        let la = scope.local("la", i32_type());
        let skip = scope.local("skip", i32_type());
        let bits = scope.local("bits", i32_type());
        let i = scope.local("i", i32_type());
        let r = scope.local("r", self.limbs_type());
        let v = scope.local("v", i64_type());
        let at = |offset: i32| {
            vec![
                get(&i),
                get(&skip),
                curios_wasm::Instr::I32Add,
                i32_const(offset),
                curios_wasm::Instr::I32Add,
            ]
        };
        let instrs = wasm![
            len(&a),
            set(&la),
            get(&count),
            i32_const(5),
            curios_wasm::Instr::I32ShrU,
            set(&skip),
            get(&count),
            i32_const(31),
            curios_wasm::Instr::I32And,
            set(&bits),
            self.new_limbs(vec![
                get(&la),
                get(&skip),
                curios_wasm::Instr::I32Add,
                i32_const(1),
                curios_wasm::Instr::I32Add,
            ]),
            set(&r),
            i32_const(0),
            set(&i),
            walk(
                &i,
                vec![get(&la)],
                "shift",
                wasm![
                    self.limb64(&a, vec![get(&i)]),
                    get(&bits),
                    curios_wasm::Instr::I64ExtendI32U,
                    curios_wasm::Instr::I64Shl,
                    set(&v),
                    self.store(
                        &r,
                        at(0),
                        wasm![
                            self.limb(&r, at(0)),
                            get(&v),
                            curios_wasm::Instr::I32WrapI64,
                            curios_wasm::Instr::I32Or,
                        ],
                    ),
                    self.store(
                        &r,
                        at(1),
                        vec![
                            get(&v),
                            i64_const(32),
                            curios_wasm::Instr::I64ShrU,
                            curios_wasm::Instr::I32WrapI64,
                        ],
                    ),
                ],
            ),
            get(&r),
            curios_wasm::Instr::RefAsNonNull,
        ];

        self.add_helper(BigHelper::MagShl, scope, self.limbs_result(), instrs);
    }

    /// `mag/negate`: invert every limb and add one, in place — the two's-complement negation `big/bitwise` reads its operands and its result through.
    pub(super) fn emit_mag_negate(&mut self) {
        let mut scope = Scope::default();
        let a = scope.param("a", self.limbs_type());
        let i = scope.local("i", i32_type());
        let carry = scope.local("carry", i64_type());
        let t = scope.local("t", i64_type());
        let instrs = wasm![
            i32_const(0),
            set(&i),
            i64_const(1),
            set(&carry),
            walk(
                &i,
                len(&a),
                "negate",
                wasm![
                    self.limb(&a, vec![get(&i)]),
                    i32_const(-1),
                    curios_wasm::Instr::I32Xor,
                    curios_wasm::Instr::I64ExtendI32U,
                    get(&carry),
                    curios_wasm::Instr::I64Add,
                    set(&t),
                    self.store(
                        &a,
                        vec![get(&i)],
                        vec![get(&t), curios_wasm::Instr::I32WrapI64]
                    ),
                    get(&t),
                    i64_const(32),
                    curios_wasm::Instr::I64ShrU,
                    set(&carry),
                ],
            ),
            get(&a),
            curios_wasm::Instr::RefAsNonNull,
        ];

        self.add_helper(BigHelper::MagNegate, scope, self.limbs_result(), instrs);
    }

    /// `mag/divrem`: the quotient of two magnitudes, or with `want_rem` their remainder.
    ///
    /// A dividend below the divisor answers at once; a one-limb divisor divides limb by limb from the top, a 64-bit dividend over a 32-bit divisor at each step; and a wider one runs Knuth's algorithm D as Hacker's Delight's `divmnu` spells it: normalize both operands until the divisor's top bit is set, estimate each quotient limb from the top two limbs of the running remainder and correct the estimate by the divisor's second limb — at most twice — then multiply and subtract, adding the divisor back in the rare case the estimate was still one too large. The normalizing shifts run in 64 bits, so a shift of zero needs no case of its own. A zero divisor is refused as the compiler's fault, since every division carries evidence that its divisor is not zero.
    pub(super) fn emit_mag_divrem(&mut self) {
        let (mut scope, a, b) = self.magnitude_pair();
        let want = scope.param("want_rem", i32_type());
        let la = scope.local("la", i32_type());
        let lb = scope.local("lb", i32_type());
        let s = scope.local("s", i32_type());
        let i = scope.local("i", i32_type());
        let j = scope.local("j", i32_type());
        let q = scope.local("q", self.limbs_type());
        let r = scope.local("r", self.limbs_type());
        let vn = scope.local("vn", self.limbs_type());
        let un = scope.local("un", self.limbs_type());
        let d = scope.local("d", i64_type());
        let t = scope.local("t", i64_type());
        let num = scope.local("num", i64_type());
        let qhat = scope.local("qhat", i64_type());
        let rhat = scope.local("rhat", i64_type());
        let k = scope.local("k", i64_type());
        let p = scope.local("p", i64_type());
        let vtop = scope.local("vtop", i64_type());
        let vnext = scope.local("vnext", i64_type());

        let s64 = || vec![get(&s), curios_wasm::Instr::I64ExtendI32U];
        let s64_complement = || {
            vec![
                i64_const(32),
                get(&s),
                curios_wasm::Instr::I64ExtendI32U,
                curios_wasm::Instr::I64Sub,
            ]
        };
        let minus = |local: &curios_wasm::LocalName, offset: i32| {
            vec![get(local), i32_const(offset), curios_wasm::Instr::I32Sub]
        };
        let plus = |x: &curios_wasm::LocalName, y: &curios_wasm::LocalName| {
            vec![get(x), get(y), curios_wasm::Instr::I32Add]
        };
        let top = |offset: i32| {
            vec![
                get(&j),
                get(&lb),
                curios_wasm::Instr::I32Add,
                i32_const(offset),
                curios_wasm::Instr::I32Sub,
            ]
        };

        // `dst[i] = (src[i] << s) | (src[i - 1] >> (32 - s))` for `i` from `from` down to 1, then `dst[0] = src[0] << s`.
        let normalize = |dst: &curios_wasm::LocalName,
                         src: &curios_wasm::LocalName,
                         label: &str|
         -> Vec<curios_wasm::Instr> {
            let done = format!("{label}_done");
            wasm![
                block(
                    &done,
                    vec![repeat(
                        label,
                        wasm![
                            get(&i),
                            curios_wasm::Instr::I32Eqz,
                            br_if(&done),
                            self.store(
                                dst,
                                vec![get(&i)],
                                wasm![
                                    self.limb64(src, vec![get(&i)]),
                                    s64(),
                                    curios_wasm::Instr::I64Shl,
                                    self.limb64(src, minus(&i, 1)),
                                    s64_complement(),
                                    curios_wasm::Instr::I64ShrU,
                                    curios_wasm::Instr::I64Or,
                                    curios_wasm::Instr::I32WrapI64,
                                ],
                            ),
                            minus(&i, 1),
                            set(&i),
                            br(label),
                        ],
                    )],
                ),
                self.store(
                    dst,
                    vec![i32_const(0)],
                    wasm![
                        self.limb(src, vec![i32_const(0)]),
                        get(&s),
                        curios_wasm::Instr::I32Shl
                    ],
                ),
            ]
        };

        let single = wasm![
            self.limb64(&b, vec![i32_const(0)]),
            set(&d),
            self.new_limbs(vec![get(&la)]),
            set(&q),
            i64_const(0),
            set(&t),
            get(&la),
            set(&i),
            repeat(
                "digit",
                wasm![
                    minus(&i, 1),
                    set(&i),
                    get(&t),
                    i64_const(32),
                    curios_wasm::Instr::I64Shl,
                    self.limb64(&a, vec![get(&i)]),
                    curios_wasm::Instr::I64Or,
                    set(&num),
                    self.store(
                        &q,
                        vec![get(&i)],
                        vec![
                            get(&num),
                            get(&d),
                            curios_wasm::Instr::I64DivU,
                            curios_wasm::Instr::I32WrapI64,
                        ],
                    ),
                    get(&num),
                    get(&d),
                    curios_wasm::Instr::I64RemU,
                    set(&t),
                    get(&i),
                    br_if("digit"),
                ],
            ),
            get(&want),
            either(
                self.limbs_result(),
                vec![get(&t), curios_wasm::Instr::I32WrapI64, self.fixed_limbs(1)],
                vec![get(&q), curios_wasm::Instr::RefAsNonNull],
            ),
            curios_wasm::Instr::Return,
        ];

        let estimate = block(
            "estimated",
            vec![repeat(
                "estimate",
                wasm![
                    get(&qhat),
                    i64_const(BASE),
                    curios_wasm::Instr::I64GeU,
                    either(
                        i32_type(),
                        vec![i32_const(1)],
                        wasm![
                            get(&qhat),
                            get(&vnext),
                            curios_wasm::Instr::I64Mul,
                            get(&rhat),
                            i64_const(32),
                            curios_wasm::Instr::I64Shl,
                            self.limb64(&un, top(2)),
                            curios_wasm::Instr::I64Or,
                            curios_wasm::Instr::I64GtU,
                        ],
                    ),
                    curios_wasm::Instr::I32Eqz,
                    br_if("estimated"),
                    get(&qhat),
                    i64_const(1),
                    curios_wasm::Instr::I64Sub,
                    set(&qhat),
                    get(&rhat),
                    get(&vtop),
                    curios_wasm::Instr::I64Add,
                    tee(&rhat),
                    i64_const(BASE),
                    curios_wasm::Instr::I64LtU,
                    br_if("estimate"),
                ],
            )],
        );

        let subtract = wasm![
            i64_const(0),
            set(&k),
            i32_const(0),
            set(&i),
            walk(
                &i,
                vec![get(&lb)],
                "subtract",
                wasm![
                    get(&qhat),
                    self.limb64(&vn, vec![get(&i)]),
                    curios_wasm::Instr::I64Mul,
                    set(&p),
                    self.limb64(&un, plus(&i, &j)),
                    get(&k),
                    curios_wasm::Instr::I64Sub,
                    get(&p),
                    i64_const(0xffff_ffff),
                    curios_wasm::Instr::I64And,
                    curios_wasm::Instr::I64Sub,
                    set(&t),
                    self.store(
                        &un,
                        plus(&i, &j),
                        vec![get(&t), curios_wasm::Instr::I32WrapI64]
                    ),
                    get(&p),
                    i64_const(32),
                    curios_wasm::Instr::I64ShrU,
                    get(&t),
                    i64_const(32),
                    curios_wasm::Instr::I64ShrS,
                    curios_wasm::Instr::I64Sub,
                    set(&k),
                ],
            ),
            self.limb64(&un, top(0)),
            get(&k),
            curios_wasm::Instr::I64Sub,
            set(&t),
            self.store(&un, top(0), vec![get(&t), curios_wasm::Instr::I32WrapI64]),
        ];

        let add_back = wasm![
            get(&qhat),
            i64_const(1),
            curios_wasm::Instr::I64Sub,
            set(&qhat),
            i64_const(0),
            set(&k),
            i32_const(0),
            set(&i),
            walk(
                &i,
                vec![get(&lb)],
                "add_back",
                wasm![
                    self.limb64(&un, plus(&i, &j)),
                    self.limb64(&vn, vec![get(&i)]),
                    curios_wasm::Instr::I64Add,
                    get(&k),
                    curios_wasm::Instr::I64Add,
                    set(&t),
                    self.store(
                        &un,
                        plus(&i, &j),
                        vec![get(&t), curios_wasm::Instr::I32WrapI64]
                    ),
                    get(&t),
                    i64_const(32),
                    curios_wasm::Instr::I64ShrU,
                    set(&k),
                ],
            ),
            self.store(
                &un,
                top(0),
                wasm![
                    self.limb64(&un, top(0)),
                    get(&k),
                    curios_wasm::Instr::I64Add,
                    curios_wasm::Instr::I32WrapI64,
                ],
            ),
        ];

        let quotient = block(
            "quotient_done",
            vec![repeat(
                "quotient",
                wasm![
                    self.limb64(&un, top(0)),
                    i64_const(32),
                    curios_wasm::Instr::I64Shl,
                    self.limb64(&un, top(1)),
                    curios_wasm::Instr::I64Or,
                    set(&num),
                    get(&num),
                    get(&vtop),
                    curios_wasm::Instr::I64DivU,
                    set(&qhat),
                    get(&num),
                    get(&vtop),
                    curios_wasm::Instr::I64RemU,
                    set(&rhat),
                    estimate,
                    subtract,
                    get(&t),
                    i64_const(0),
                    curios_wasm::Instr::I64LtS,
                    when(add_back),
                    self.store(
                        &q,
                        vec![get(&j)],
                        vec![get(&qhat), curios_wasm::Instr::I32WrapI64]
                    ),
                    get(&j),
                    curios_wasm::Instr::I32Eqz,
                    br_if("quotient_done"),
                    minus(&j, 1),
                    set(&j),
                    br("quotient"),
                ],
            )],
        );

        let unnormalize = wasm![
            self.new_limbs(vec![get(&lb)]),
            set(&r),
            i32_const(0),
            set(&i),
            walk(
                &i,
                minus(&lb, 1),
                "unnormalize",
                self.store(
                    &r,
                    vec![get(&i)],
                    wasm![
                        self.limb64(&un, vec![get(&i)]),
                        s64(),
                        curios_wasm::Instr::I64ShrU,
                        self.limb64(&un, vec![get(&i), i32_const(1), curios_wasm::Instr::I32Add]),
                        s64_complement(),
                        curios_wasm::Instr::I64Shl,
                        curios_wasm::Instr::I64Or,
                        curios_wasm::Instr::I32WrapI64,
                    ],
                ),
            ),
            self.store(
                &r,
                minus(&lb, 1),
                wasm![
                    self.limb64(&un, minus(&lb, 1)),
                    s64(),
                    curios_wasm::Instr::I64ShrU,
                    curios_wasm::Instr::I32WrapI64,
                ],
            ),
            get(&r),
            curios_wasm::Instr::RefAsNonNull,
        ];

        let instrs = wasm![
            len(&a),
            set(&la),
            len(&b),
            set(&lb),
            get(&lb),
            curios_wasm::Instr::I32Eqz,
            when(self.table.refuse_instrs(curios_cont::Panic::Invariant)),
            return_if(
                vec![
                    get(&a),
                    get(&b),
                    self.helper(BigHelper::MagCmp),
                    i32_const(0),
                    curios_wasm::Instr::I32LtS,
                ],
                vec![
                    get(&want),
                    either(
                        self.limbs_result(),
                        vec![get(&a), curios_wasm::Instr::RefAsNonNull],
                        self.new_limbs(vec![i32_const(0)]),
                    ),
                ],
            ),
            get(&lb),
            i32_const(1),
            curios_wasm::Instr::I32Eq,
            when(single),
            self.limb(&b, minus(&lb, 1)),
            curios_wasm::Instr::I32Clz,
            set(&s),
            self.new_limbs(vec![get(&lb)]),
            set(&vn),
            self.new_limbs(vec![get(&la), i32_const(1), curios_wasm::Instr::I32Add]),
            set(&un),
            minus(&lb, 1),
            set(&i),
            normalize(&vn, &b, "divisor"),
            self.store(
                &un,
                vec![get(&la)],
                wasm![
                    self.limb64(&a, minus(&la, 1)),
                    s64_complement(),
                    curios_wasm::Instr::I64ShrU,
                    curios_wasm::Instr::I32WrapI64,
                ],
            ),
            minus(&la, 1),
            set(&i),
            normalize(&un, &a, "dividend"),
            self.new_limbs(vec![
                get(&la),
                get(&lb),
                curios_wasm::Instr::I32Sub,
                i32_const(1),
                curios_wasm::Instr::I32Add,
            ]),
            set(&q),
            self.limb64(&vn, minus(&lb, 1)),
            set(&vtop),
            self.limb64(&vn, minus(&lb, 2)),
            set(&vnext),
            get(&la),
            get(&lb),
            curios_wasm::Instr::I32Sub,
            set(&j),
            quotient,
            return_if(
                vec![get(&want), curios_wasm::Instr::I32Eqz],
                vec![get(&q), curios_wasm::Instr::RefAsNonNull],
            ),
            unnormalize,
        ];

        self.add_helper(BigHelper::MagDivRem, scope, self.limbs_result(), instrs);
    }
}

/// Swap `a` and `b`, and their lengths, when the condition on the stack holds — `mag/add` walks the longer operand.
fn when_swapped(
    a: &curios_wasm::LocalName,
    b: &curios_wasm::LocalName,
    spare: &curios_wasm::LocalName,
    la: &curios_wasm::LocalName,
    lb: &curios_wasm::LocalName,
    scratch: &curios_wasm::LocalName,
) -> curios_wasm::Instr {
    when(vec![
        get(a),
        set(spare),
        get(b),
        set(a),
        get(spare),
        set(b),
        get(la),
        set(scratch),
        get(lb),
        set(la),
        get(scratch),
        set(lb),
    ])
}
