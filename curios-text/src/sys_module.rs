//! The roster of `/sys`: every intrinsic type and operation the language provides, built directly as `text` AST and compiled as the prelude's `/sys` unit, so intrinsics participate in the module system like any other binding.
//!
//! Bodies bake `Intrinsic` nodes in directly, so the roster needs no internal name resolution — with one exception, the propositions a bound is stated in and the outcomes an operation answers with, which are `/sys`'s own and are named absolutely so a declaration resolves wherever the roster puts it.
//!
//! Ordinary polymorphic inductives are parsed from declarations so a repeated lowering of each written `Type` retains its universe identity. Intrinsic bodies have no surface spelling and are built structurally; their declarations are assembled beside them.
//!
//! **An entry states its name, its parameters' names and its node; its types are `Intrinsic::signature`'s.** The table is `curios-core`'s and both checkers type a node by it, and a declaration here is that row read into a signature (`projected.rs`), so the roster cannot disagree with the checkers and there is no second statement to keep in step. What is the roster's own is which node a name builds: `gt` is `lt` with its operands the other way round, and `Nat/to_flt` rounds ties to even.
//!
//! This file holds the roster alone. How a declaration is built is `constructors.rs`, how a node's row is read into its types is `projected.rs`, and how a term is built is `helpers.rs`.

mod constructors;
use constructors::*;

mod helpers;
use helpers::*;

mod projected;
use projected::*;

#[cfg(test)]
mod tests;

use {
    super::{
        Doc, Intrinsic, Match, MatchPattern, MatrixArm, Module, Subterm, TopCase, TopInduct,
        TopItem,
    },
    curios_abi::{CodeCarrier, Failure, code_sets, stdio},
    curios_num::{Grain, Natural, Rounding},
    curios_utilities::SyntaxRegistry,
};

// Ordinary declarations use the parser's source identities: every repeated lowering of a written `Type` must share its universe seed, including the family and constructor signatures synthesized from it.
fn option_decl() -> TopItem {
    let module: Module = r#"
        --- A value that may be absent.
        pub induct Option(A: Sort): pub Type
        --- The value is there.
        | some(A)
        --- There is none.
        | none()
        end
    "#
    .parse()
    .expect("the ordinary Option declaration parses");
    module.items.into_iter().next().expect("one declaration")
}

// Parsed for the reason `option_decl` is: repeated lowerings of each written `Type` share their universe seed.
fn result_decl() -> TopItem {
    let module: Module = r#"
        --- Success or failure, the failure's type first.
        pub induct Result(E: Type, A: Type): pub Type
        --- It worked, carrying the value.
        | success(A)
        --- It did not, carrying the error.
        | failure(E)
        end
    "#
    .parse()
    .expect("the ordinary Result declaration parses");
    module.items.into_iter().next().expect("one declaration")
}

// `pub induct True: pub Prop | qed() end` — the trivially true proposition and its proof, which every discharged obligation is answered with.
fn true_prop() -> TopItem {
    TopItem::Induct(vec![TopInduct {
        doc: Some(Doc {
            lines: vec!["The proposition that always holds.".to_string()],
            span: None,
        }),
        vis_pub: true,
        rep_pub: true,
        label: "True".into(),
        params: Vec::new(),
        indices: Vec::new(),
        result_sort: prop(),
        cases: vec![TopCase {
            doc: Some(Doc {
                lines: vec!["Its proof, which anything may produce.".to_string()],
                span: None,
            }),
            label: "qed".into(),
            payload: Vec::new(),
            target: None,
        }],
    }])
}

// `pub induct False: pub Prop end` — no cases, so a value of it is a contradiction and a match on one eliminates into anything.
fn false_prop() -> TopItem {
    TopItem::Induct(vec![TopInduct {
        doc: Some(Doc {
            lines: vec![
                "The proposition with no proof, which anything may be concluded from.".to_string(),
            ],
            span: None,
        }),
        vis_pub: true,
        rep_pub: true,
        label: "False".into(),
        params: Vec::new(),
        indices: Vec::new(),
        result_sort: prop(),
        cases: Vec::new(),
    }])
}

// `pub let Holds(b: Bool) -> Prop = match b | true => True | false => False end;` — the reflection of a decision into a claim, which every decided bound this roster states is built from. A refined comparison reduces the match away, which is what discharges an obligation with nothing written.
fn holds() -> Decl {
    documented(
        &[
            "That `b` is `true`, as a proposition: `True` where it is and `False` where it is not.",
            "",
            "The reflection of a decision into a claim, which is what every decided bound is made of. A literal operand discharges what an intrinsic demands because the decision reduces and this reduces with it.",
        ],
        pub_fn(
            "Holds",
            vec![("b", bool_())],
            prop(),
            Subterm::Match(Match {
                head: name("b"),
                motive: None,
                arms: vec![
                    MatrixArm {
                        pattern: MatchPattern::Bool(true),
                        body: name("True"),
                        span: None,
                    },
                    MatrixArm {
                        pattern: MatchPattern::Bool(false),
                        body: name("False"),
                        span: None,
                    },
                ],
            })
            .into(),
        ),
    )
}

fn nat_succ(syntax: &SyntaxRegistry) -> Decl {
    operation(
        "succ",
        &["a"],
        syntax,
        Intrinsic::Nat(curios_core::Nat::Succ(Natural::from(1usize), name("a"))),
    )
}

fn nat_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(&["One more than `a`."], nat_succ(syntax)),
        documented(
            &["Whether the two are equal."],
            binary("eql", syntax, Intrinsic::NatEql),
        ),
        documented(
            &["Whether the two differ."],
            binary("neq", syntax, Intrinsic::NatNeq),
        ),
        documented(&["Their sum."], binary("add", syntax, Intrinsic::NatAdd)),
        documented(
            &[
                "`b` taken from `a`, and zero where `b` is the larger — a natural is never negative.",
            ],
            binary("sub", syntax, Intrinsic::NatSub),
        ),
        documented(
            &["Their product."],
            binary("mul", syntax, Intrinsic::NatMul),
        ),
        documented(
            &["`a` divided by `b`, rounded down, under the evidence that `b` is not zero."],
            guarded_binary("div", syntax, |dividend, divisor, non_zero| {
                Intrinsic::NatDiv {
                    dividend,
                    divisor,
                    non_zero,
                }
            }),
        ),
        documented(
            &["What `a` leaves after dividing by `b`, under the evidence that `b` is not zero."],
            guarded_binary("rem", syntax, |dividend, divisor, non_zero| {
                Intrinsic::NatRem {
                    dividend,
                    divisor,
                    non_zero,
                }
            }),
        ),
        documented(
            &["Whether `a` is below `b`."],
            binary("lt", syntax, Intrinsic::NatLt),
        ),
        // **`gt` and `ge` are built as their `lt`/`le` mirrors, on every carrier.** A comparison is spelled one way from the moment it enters Core, so a case equation recorded on a guard as written and the same guard met reduced inside a proposition are one term — the reducer's own mirror covers an intrinsic built by hand, but a spelling that never exists cannot be keyed on. See `documentation/design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md`.
        documented(
            &["Whether `a` is above `b`."],
            binary("gt", syntax, |a, b| Intrinsic::NatLt(b, a)),
        ),
        documented(
            &["Whether `a` is below `b` or equal to it."],
            binary("le", syntax, Intrinsic::NatLe),
        ),
        documented(
            &["Whether `a` is above `b` or equal to it."],
            binary("ge", syntax, |a, b| Intrinsic::NatLe(b, a)),
        ),
        documented(
            &["Their bits, kept where both have one."],
            binary("and", syntax, Intrinsic::NatAnd),
        ),
        documented(
            &["Their bits, kept where either has one."],
            binary("or", syntax, Intrinsic::NatOr),
        ),
        documented(
            &["Their bits, kept where exactly one has one."],
            binary("xor", syntax, Intrinsic::NatXor),
        ),
        documented(
            &["The smaller of the two."],
            binary("min", syntax, Intrinsic::NatMin),
        ),
        documented(
            &["The larger of the two."],
            binary("max", syntax, Intrinsic::NatMax),
        ),
        documented(
            &["`a` doubled `b` times."],
            binary("shl", syntax, Intrinsic::NatShl),
        ),
        documented(
            &[
                "`base` to the power `exp`, and `1` where `exp` is zero.",
                "",
                "Conversion decides it as a morphism of its exponent: a sum there is a product of powers, a literal so many factors of `base`, and `shl(1, exp)` is `pow(2, exp)`. It reads nothing of `base` but a literal, so a power of a power and a power of a product are lemmas.",
            ],
            operation(
                "pow",
                &["base", "exp"],
                syntax,
                Intrinsic::NatPow(name("base"), name("exp")),
            ),
        ),
        documented(
            &["`a` halved `b` times, rounded down each time."],
            binary("shr", syntax, Intrinsic::NatShr),
        ),
        documented(
            &["The same number as an `Int`."],
            unary("to_int", syntax, Intrinsic::NatToInt),
        ),
        documented(
            &["The nearest `Flt` to it."],
            unary("to_flt", syntax, |a| {
                Intrinsic::NatToFlt(Rounding::TiesToEven, a)
            }),
        ),
        documented(
            &["The same number as a `Byte`, under the evidence that it is below `256`."],
            guarded_unary("to_byte", syntax, |nat, below| Intrinsic::NatToByte {
                nat,
                below,
            }),
        ),
    ]
}

fn byte_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![documented(
        &["The same number as a `Nat`."],
        unary("to_nat", syntax, Intrinsic::ByteToNat),
    )]
}

// `Bool` rides the same i31ref/u32 carrier as `Nat`, with `false`/`true` as `0`/`1`. `and`/`or`/`xor` are bitwise machine ops on those bits — exact boolean logic — and `eql` is the `Nat` equality op (`i32.eq`) on that single bit, so all four are intrinsics rather than `match` definitions. `not` has no machine instruction; `/std/Bool` defines it as `xor(b, true)`.
fn bool_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(
            &["True when both are."],
            binary("and", syntax, Intrinsic::BoolAnd),
        ),
        documented(
            &["True when either is."],
            binary("or", syntax, Intrinsic::BoolOr),
        ),
        documented(
            &["True when exactly one is."],
            binary("xor", syntax, Intrinsic::BoolXor),
        ),
        documented(
            &["Whether the two are equal."],
            binary("eql", syntax, Intrinsic::BoolEql),
        ),
    ]
}

fn int_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(
            &["Whether the two are equal."],
            binary("eql", syntax, Intrinsic::IntEql),
        ),
        documented(
            &["Whether the two differ."],
            binary("neq", syntax, Intrinsic::IntNeq),
        ),
        documented(&["Their sum."], binary("add", syntax, Intrinsic::IntAdd)),
        documented(
            &["`b` taken from `a`."],
            binary("sub", syntax, Intrinsic::IntSub),
        ),
        documented(
            &["Their product."],
            binary("mul", syntax, Intrinsic::IntMul),
        ),
        documented(
            &["`a` divided by `b`, rounded toward zero."],
            guarded_binary("div", syntax, |dividend, divisor, non_zero| {
                Intrinsic::IntDiv {
                    dividend,
                    divisor,
                    non_zero,
                }
            }),
        ),
        documented(
            &["What `a` leaves after dividing by `b`, taking its sign from `a`."],
            guarded_binary("rem", syntax, |dividend, divisor, non_zero| {
                Intrinsic::IntRem {
                    dividend,
                    divisor,
                    non_zero,
                }
            }),
        ),
        documented(
            &["Whether `a` is below `b`."],
            binary("lt", syntax, Intrinsic::IntLt),
        ),
        documented(
            &["Whether `a` is above `b`."],
            binary("gt", syntax, |a, b| Intrinsic::IntLt(b, a)),
        ),
        documented(
            &["Whether `a` is below `b` or equal to it."],
            binary("le", syntax, Intrinsic::IntLe),
        ),
        documented(
            &["Whether `a` is above `b` or equal to it."],
            binary("ge", syntax, |a, b| Intrinsic::IntLe(b, a)),
        ),
        // Bitwise ops on unbounded two's complement. `and`/`or`/`xor` are exact bit ops; `shl` is `· 2ⁿ` like `Nat/shl`; `shr` is arithmetic (sign-preserving). Both shifts count in `Nat`, as `Nat/shl` does, so a negative count — which the theory never defined — cannot be written. `not` is `/std/Int`'s `xor(x, -1)`.
        documented(
            &["Their bits, kept where both have one."],
            binary("and", syntax, Intrinsic::IntAnd),
        ),
        documented(
            &["Their bits, kept where either has one."],
            binary("or", syntax, Intrinsic::IntOr),
        ),
        documented(
            &["Their bits, kept where exactly one has one."],
            binary("xor", syntax, Intrinsic::IntXor),
        ),
        documented(
            &["The smaller of the two."],
            binary("min", syntax, Intrinsic::IntMin),
        ),
        documented(
            &["The larger of the two."],
            binary("max", syntax, Intrinsic::IntMax),
        ),
        documented(
            &["The magnitude, as a `Nat`."],
            unary("abs", syntax, Intrinsic::IntAbs),
        ),
        documented(
            &["`-1`, `+0` or `+1` by the sign of `a`."],
            unary("sign", syntax, Intrinsic::IntSign),
        ),
        documented(
            &["`a` doubled `b` times."],
            operation(
                "shl",
                &["a", "b"],
                syntax,
                Intrinsic::IntShl(name("a"), name("b")),
            ),
        ),
        documented(
            &["`a` halved `b` times, rounded toward negative — the sign is kept."],
            operation(
                "shr",
                &["a", "b"],
                syntax,
                Intrinsic::IntShr(name("a"), name("b")),
            ),
        ),
        documented(
            &["The same number as a `Nat`, under the evidence that it is not negative."],
            guarded_unary("to_nat", syntax, |int, non_neg| Intrinsic::IntToNat {
                int,
                non_neg,
            }),
        ),
        documented(
            &["The nearest `Flt` to it."],
            unary("to_flt", syntax, |a| {
                Intrinsic::IntToFlt(Rounding::TiesToEven, a)
            }),
        ),
    ]
}

fn flt_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
    let decls = vec![
        documented(
            &["What `a` leaves after dividing by `b`."],
            binary("rem", syntax, Intrinsic::FltRem),
        ),
        documented(
            &["The smaller of the two."],
            binary("min", syntax, Intrinsic::FltMin),
        ),
        documented(
            &["The larger of the two."],
            binary("max", syntax, Intrinsic::FltMax),
        ),
        documented(
            &["Whether the two are equal."],
            binary("eql", syntax, Intrinsic::FltEql),
        ),
        documented(
            &["Whether the two differ."],
            binary("neq", syntax, Intrinsic::FltNeq),
        ),
        documented(
            &["Whether `a` is below `b`."],
            binary("lt", syntax, Intrinsic::FltLt),
        ),
        documented(
            &["Whether `a` is above `b`."],
            binary("gt", syntax, |a, b| Intrinsic::FltLt(b, a)),
        ),
        documented(
            &["Whether `a` is below `b` or equal to it."],
            binary("le", syntax, Intrinsic::FltLe),
        ),
        documented(
            &["Whether `a` is above `b` or equal to it."],
            binary("ge", syntax, |a, b| Intrinsic::FltLe(b, a)),
        ),
        documented(
            &["`a` with its sign flipped."],
            unary("neg", syntax, Intrinsic::FltNeg),
        ),
        documented(
            &["`a` without its sign."],
            unary("abs", syntax, Intrinsic::FltAbs),
        ),
        documented(
            &[
                "The whole number nearest `a`, and the one farther from zero where it falls halfway.",
            ],
            unary("round", syntax, |a| {
                Intrinsic::FltRoundIntegral(Rounding::TiesToAway, a)
            }),
        ),
        documented(
            &["The greatest whole number at or below `a`."],
            unary("floor", syntax, |a| {
                Intrinsic::FltRoundIntegral(Rounding::TowardNegative, a)
            }),
        ),
        documented(
            &["The least whole number at or above `a`."],
            unary("ceil", syntax, |a| {
                Intrinsic::FltRoundIntegral(Rounding::TowardPositive, a)
            }),
        ),
        documented(
            &["`a` with its fraction dropped, toward zero."],
            unary("trunc", syntax, |a| {
                Intrinsic::FltRoundIntegral(Rounding::TowardZero, a)
            }),
        ),
        documented(
            &["The whole number nearest `a`, and the even one where it falls halfway."],
            unary("nearest", syntax, |a| {
                Intrinsic::FltRoundIntegral(Rounding::TiesToEven, a)
            }),
        ),
        documented(
            &["`a` carrying `b`'s sign."],
            binary("copysign", syntax, Intrinsic::FltCopysign),
        ),
        documented(
            &["The whole part of `a` as a `Nat`, under the evidence that it is not negative."],
            guarded_unary("to_nat", syntax, |flt, non_neg| Intrinsic::FltToNat {
                flt,
                non_neg,
            }),
        ),
        documented(
            &["The whole part of `a` as an `Int`, under the evidence that it is finite."],
            guarded_unary("to_int", syntax, |flt, finite| Intrinsic::FltToInt {
                flt,
                finite,
            }),
        ),
        documented(
            &[
                "The signed mantissa of `a`'s exact value `mantissa · 2^exponent`, under the evidence that it is finite: under `2^53` in magnitude, and `0` for a zero of either sign.",
            ],
            guarded_unary("mantissa", syntax, |flt, finite| Intrinsic::FltMantissa {
                flt,
                finite,
            }),
        ),
        documented(
            &[
                "The exponent of `a`'s exact value `mantissa · 2^exponent`, under the evidence that it is finite: from `-1074`, the least subnormal's, and `0` for a zero of either sign.",
            ],
            guarded_unary("exponent", syntax, |flt, finite| Intrinsic::FltExponent {
                flt,
                finite,
            }),
        ),
        documented(
            &["Its eight bytes, least significant first."],
            unary("to_le_bytes", syntax, Intrinsic::FltToLeBytes),
        ),
        documented(
            &["The number those eight bytes spell, least significant first."],
            guarded_unary("of_le_bytes", syntax, |bin, eight_bytes| {
                Intrinsic::FltOfLeBytes { bin, eight_bytes }
            }),
        ),
    ];

    let directed = Rounding::ALL
        .into_iter()
        .filter(|rounding| *rounding != Rounding::TiesToEven)
        .map(|rounding| pub_mod(rounding.label(), items(flt_rounded(rounding, syntax))));

    items(flt_rounded(Rounding::TiesToEven, syntax))
        .into_iter()
        .chain(items(decls))
        .chain(directed)
        .collect()
}

/// The operations IEEE computes exactly and rounds once, in the direction `rounding` names — at the root for the default, ties to even, and in `/sys/Flt/<direction>` for each other — as the packed operations are written once per [`Grain`].
fn flt_rounded(rounding: Rounding, syntax: &SyntaxRegistry) -> Vec<Decl> {
    let mut decls = vec![
        documented(
            &["Their sum."],
            binary("add", syntax, |a, b| Intrinsic::FltAdd(rounding, a, b)),
        ),
        documented(
            &["`b` taken from `a`."],
            binary("sub", syntax, |a, b| Intrinsic::FltSub(rounding, a, b)),
        ),
        documented(
            &["Their product."],
            binary("mul", syntax, |a, b| Intrinsic::FltMul(rounding, a, b)),
        ),
        documented(
            &["`a` divided by `b`."],
            binary("div", syntax, |a, b| Intrinsic::FltDiv(rounding, a, b)),
        ),
        documented(
            &["`a` times `b`, plus `c`, rounded once."],
            ternary("fma", syntax, |a, b, c| {
                Intrinsic::FltFma(rounding, a, b, c)
            }),
        ),
        documented(
            &["The square root of `a`."],
            unary("sqrt", syntax, |a| Intrinsic::FltSqrt(rounding, a)),
        ),
    ];

    // The default direction's conversions are `Nat/to_flt` and `Int/to_flt`, beside the carriers they convert from.
    if rounding != Rounding::TiesToEven {
        decls.extend([
            documented(
                &["The `Flt` a `Nat` rounds to."],
                unary("of_nat", syntax, |a| Intrinsic::NatToFlt(rounding, a)),
            ),
            documented(
                &["The `Flt` an `Int` rounds to."],
                unary("of_int", syntax, |a| Intrinsic::IntToFlt(rounding, a)),
            ),
        ]);
    }

    decls
}

fn bin_ops(grain: Grain, syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(
            &["How many it holds."],
            operation("len", &["b"], syntax, Intrinsic::BinLen(grain, name("b"))),
        ),
        documented(
            &["Whether the two hold the same, in the same order."],
            operation(
                "eql",
                &["a", "b"],
                syntax,
                Intrinsic::BinEql(grain, name("a"), name("b")),
            ),
        ),
        documented(
            &["The element at `i`, under the evidence that `i` is within the length."],
            operation(
                "get",
                &["b", "i", "ok"],
                syntax,
                Intrinsic::BinGet {
                    grain,
                    bin: name("b"),
                    index: name("i"),
                    in_range: name("ok"),
                },
            ),
        ),
        // A window is a start and a *count*, so a reversed one cannot be spelled and an ordering bound has no proposition to state. What survives is that the window ends inside the value.
        documented(
            &[
                "The run from `from` up to but not including `to`, under the evidence that both are within the length and in order.",
            ],
            operation(
                "slice",
                &["b", "s", "l", "within"],
                syntax,
                Intrinsic::BinSlice {
                    grain,
                    bin: name("b"),
                    start: name("s"),
                    length: name("l"),
                    within: name("within"),
                },
            ),
        ),
        documented(
            &["`count` copies of `x`."],
            operation(
                "replicate",
                &["count", "x"],
                syntax,
                Intrinsic::BinReplicate {
                    grain,
                    count: name("count"),
                    atom: name("x"),
                },
            ),
        ),
        documented(
            &[
                "The conjunction of `a` and `b` element by element, under the evidence that they are the same length.",
            ],
            operation(
                "and",
                &["a", "b", "same"],
                syntax,
                Intrinsic::BinAnd {
                    grain,
                    left: name("a"),
                    right: name("b"),
                    same_length: name("same"),
                },
            ),
        ),
        documented(
            &["The disjunction, under the same evidence."],
            operation(
                "or",
                &["a", "b", "same"],
                syntax,
                Intrinsic::BinOr {
                    grain,
                    left: name("a"),
                    right: name("b"),
                    same_length: name("same"),
                },
            ),
        ),
        documented(
            &["The difference, under the same evidence."],
            operation(
                "xor",
                &["a", "b", "same"],
                syntax,
                Intrinsic::BinXor {
                    grain,
                    left: name("a"),
                    right: name("b"),
                    same_length: name("same"),
                },
            ),
        ),
        documented(
            &["`b` with `x` added after its last element."],
            operation(
                "append",
                &["b", "x"],
                syntax,
                Intrinsic::BinAppend {
                    grain,
                    bin: name("b"),
                    element: name("x"),
                },
            ),
        ),
        documented(
            &["The elements of `a`, then those of `b`."],
            operation(
                "concat",
                &["a", "b"],
                syntax,
                Intrinsic::BinConcat {
                    grain,
                    operands: vec![name("a"), name("b")],
                },
            ),
        ),
    ]
}

/// The one operation a packed carrier does not share with its twin: its own run read at the other grain.
///
/// **Declared per module rather than in [`bin_ops`]**, which is the rule that holds at *both* grains and would stop being that with a direction inside it. One intrinsic underneath, two names above — the grain names the operand, so each carrier declares the reading *out* of itself.
///
/// The bound rides one position either way and decides per grain, as the intrinsic's own row does: at the byte grain there is nothing to prove, because eight bits are a byte and no count of bytes leaves a remainder. At the bit grain it is the whole of what makes the reading possible.
///
/// **The round trip needs no guard, at any run.** `to_bytes(to_bits(b))` discharges for a wholly symbolic `b`, through three laws already in the grid rather than anything stated here: the length of a reinterpretation reduces to a shift, a shift by a literal count normalises to a product with that coefficient, and `nat_euclid_split` reads the coefficient and answers the remainder for every value the symbolic factor can take. It is the same reading that makes `(256·x + Byte/to_nat(b)) / 256` reduce to `x`, which is what `Bytes/of_nat` is built on.
fn regrain_op(grain: Grain, syntax: &SyntaxRegistry) -> Vec<Decl> {
    let (label, prose) = match grain {
        Grain::X => (
            "to_bits",
            "The same run read as bits, eight to the byte and least significant first.",
        ),
        Grain::B => (
            "to_bytes",
            "The same run read as bytes, under the evidence that it holds a whole number of them.",
        ),
    };

    vec![documented(
        &[prose],
        operation(
            label,
            &["b", "whole"],
            syntax,
            Intrinsic::BinReinterp {
                grain,
                bin: name("b"),
                aligned: name("whole"),
            },
        ),
    )]
}

fn list_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(
            &["How many it holds."],
            operation(
                "len",
                &["T", "a"],
                syntax,
                Intrinsic::ListLen {
                    element: name("T"),
                    list: name("a"),
                },
            ),
        ),
        documented(
            &["The element at `i`, under the evidence that `i` is within the length."],
            operation(
                "get",
                &["T", "a", "i", "ok"],
                syntax,
                Intrinsic::ListGet {
                    element: name("T"),
                    list: name("a"),
                    index: name("i"),
                    in_range: name("ok"),
                },
            ),
        ),
        // The `List` twin of `Bin/slice`'s window bound; see the comment there for why only one half survives the count.
        documented(
            &[
                "The run from `from` up to but not including `to`, under the evidence that both are within the length and in order.",
            ],
            operation(
                "slice",
                &["T", "a", "s", "l", "within"],
                syntax,
                Intrinsic::ListSlice {
                    element: name("T"),
                    list: name("a"),
                    start: name("s"),
                    length: name("l"),
                    within: name("within"),
                },
            ),
        ),
        documented(
            &["`a` with `x` added after its last element."],
            operation(
                "append",
                &["T", "a", "x"],
                syntax,
                Intrinsic::ListAppend {
                    element: name("T"),
                    list: name("a"),
                    item: name("x"),
                },
            ),
        ),
        documented(
            &["The elements of `a`, then those of `b`."],
            operation(
                "concat",
                &["T", "a", "b"],
                syntax,
                Intrinsic::ListConcat {
                    element: name("T"),
                    operands: vec![name("a"), name("b")],
                },
            ),
        ),
        documented(
            &["Each element with `f` applied to it, in order."],
            operation(
                "map",
                &["A", "B", "a", "f"],
                syntax,
                Intrinsic::ListMap {
                    from: name("A"),
                    to: name("B"),
                    list: name("a"),
                    function: name("f"),
                },
            ),
        ),
        documented(
            &["Left to right with `f(elem, acc)`: `f(x_n, … f(x_1, init))`."],
            operation(
                "fold",
                &["T", "A", "l", "init", "f"],
                syntax,
                Intrinsic::ListFold {
                    element: name("T"),
                    result: name("A"),
                    list: name("l"),
                    init: name("init"),
                    function: name("f"),
                },
            ),
        ),
    ]
}

// Public cells describe allocation and first-write-wins observation; private knot memoization is introduced only below erasure.
fn cell_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(
            &["A new empty cell."],
            operation(
                "new",
                &["T"],
                syntax,
                Intrinsic::Cell { element: name("T") },
            ),
        ),
        documented(
            &["Store `v` only if empty, returning whether this fill won."],
            operation(
                "fill",
                &["T", "c", "v"],
                syntax,
                Intrinsic::CellFill {
                    element: name("T"),
                    cell: name("c"),
                    value: name("v"),
                },
            ),
        ),
        documented(
            &["The first value filled, or none while empty."],
            operation(
                "poll",
                &["T", "c"],
                syntax,
                Intrinsic::CellPoll {
                    element: name("T"),
                    cell: name("c"),
                    universes: curios_core::Universes::default(),
                },
            ),
        ),
    ]
}

fn channel_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
    let outcomes: Module = r#"
        --- The outcome of one nonblocking push.
        pub induct Push: pub Type
        | taken()
        | full()
        | closed()
        end
        --- The outcome of one nonblocking take.
        pub induct Take(A: Sort): pub Type
        | item(A)
        | empty()
        | ended()
        end
    "#
    .parse()
    .expect("ordinary channel outcomes parse");
    let query = |label, description, node| {
        documented(&[description], operation(label, &["T", "c"], syntax, node))
    };
    outcomes.items.into_iter().chain(items(vec![
        documented(&["A bounded FIFO with positive capacity."], operation("new", &["T", "capacity", "positive"], syntax, Intrinsic::Channel { element: name("T"), capacity: name("capacity"), positive: name("positive") })),
        documented(&["Append if open and not full, reporting this attempt's outcome."], operation("push", &["T", "c", "value"], syntax, Intrinsic::ChannelPush { element: name("T"), channel: name("c"), value: name("value") })),
        query("take", "Remove the oldest item; empty while open, ended when closed and drained.", Intrinsic::ChannelTake { element: name("T"), channel: name("c"), universes: curios_core::Universes::default() }),
        query("close", "Prevent later pushes, preserving queued items for draining. Repeated closes do nothing.", Intrinsic::ChannelClose { element: name("T"), channel: name("c") }),
        query("closed", "Whether the channel has been closed.", Intrinsic::ChannelClosed { element: name("T"), channel: name("c") }),
        query("count", "The number of queued items. This observation reserves nothing.", Intrinsic::ChannelCount { element: name("T"), channel: name("c") }),
        query("capacity", "The positive bound supplied at creation.", Intrinsic::ChannelCapacity { element: name("T"), channel: name("c") }),
    ])).collect()
}

// The monad of the `/sys/Io` type, and nothing else: `Io` owns the sequencing, never the operations. An operation belongs with its subject — the one its own store row names, not the type its result wears.
fn io_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(
            &["The description that performs nothing and yields `x`."],
            operation(
                "pure",
                &["T", "x"],
                syntax,
                Intrinsic::IoPure {
                    result: name("T"),
                    value: name("x"),
                },
            ),
        ),
        documented(
            &["The description that performs `m`, then whatever `f` makes of its result."],
            operation(
                "bind",
                &["A", "B", "m", "f"],
                syntax,
                Intrinsic::IoBind {
                    from: name("A"),
                    to: name("B"),
                    action: name("m"),
                    continuation: name("f"),
                },
            ),
        ),
        documented(
            &[
                "The description that suspends until a handle of `on` may be ready or a cell of `filled` is filled, then goes on as `rest`; whoever drops it instead performs `cancel`.",
                "",
                "It waits on nothing itself: whoever advances it decides what a suspension means. The host waits on `on` and advances `rest`; a combinator runs something else first, or drops it after performing `cancel`. A cell is filled by the program alone, so the host, which waits while nothing of the program runs, never reads one: a suspension on no handle that reaches it is a deadlock.",
            ],
            operation(
                "suspend",
                &["T", "on", "filled", "cancel", "rest"],
                syntax,
                Intrinsic::IoSuspend {
                    result: name("T"),
                    on: name("on"),
                    filled: name("filled"),
                    cancel: name("cancel"),
                    rest: name("rest"),
                },
            ),
        ),
        documented(
            &[
                "The description that performs `m` and hands its value to `done`, or, where `m` suspends, its `on`, `filled`, `cancel` and `rest` to `suspended`: the one place a suspension is caught rather than passed out.",
            ],
            operation(
                "advance",
                &["A", "R", "m", "done", "suspended"],
                syntax,
                Intrinsic::IoAdvance {
                    from: name("A"),
                    to: name("R"),
                    action: name("m"),
                    done: name("done"),
                    suspended: name("suspended"),
                },
            ),
        ),
    ]
}

// The values and operations of the `/sys/Handle` type: the three standard streams and the ready token, handle identity, and `host` — the store rows whose subject is the handle itself (`read`, `write`, `poll`, `close`), which join their type module rather than open one of their own.
fn handle_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(
            &["The stream the host feeds the program."],
            operation("stdin", &[], syntax, Intrinsic::Handle(stdio::STDIN)),
        ),
        documented(
            &["The stream the program writes its output to."],
            operation("stdout", &[], syntax, Intrinsic::Handle(stdio::STDOUT)),
        ),
        documented(
            &["The stream the program writes its diagnostics to."],
            operation("stderr", &[], syntax, Intrinsic::Handle(stdio::STDERR)),
        ),
        documented(
            &[
                "The handle that is always ready: a description suspended on it lets what waits beside it run, and goes on.",
            ],
            operation("ready", &[], syntax, Intrinsic::Handle(stdio::READY)),
        ),
    ]
}

/// The wire-code mirror: the guest counterpart of ABI wire codes, so the standard library compares against named constants the host derives from the same source. Each module is one set of `curios-abi`'s table of codes (`code_sets`), named as the set is, and holds each code the table gives a name the guest reads it by, at that name: nothing here spells a code or its name a second time. A set whose codes the guest reads by no name mints no module.
///
/// `/sys/status` holds one value more, `failures`: the code of every failure the table names, in its order. The standard library names each as a case of `/std/Io/Error`, and its own test holds it to the list, so a status the table gains is a case the library is refused until it has.
fn code_modules() -> Vec<SysModule> {
    code_sets()
        .into_iter()
        .filter(|set| !set.codes.is_empty())
        .map(|set| {
            let minted = set.codes.iter().map(|code| match set.carrier {
                CodeCarrier::Nat => pub_let(code.guest, nat(), nat_lit(code.value)),
                CodeCarrier::Byte => pub_let(
                    code.guest,
                    byte(),
                    byte_lit(u8::try_from(code.value).expect("a code read as a byte fits one")),
                ),
            });
            let failures = (set.name == "status").then(|| {
                pub_let(
                    "failures",
                    intrinsic(Intrinsic::ListType(nat())),
                    intrinsic(Intrinsic::List {
                        element: nat(),
                        items: Failure::NAMED
                            .iter()
                            .map(|failure| nat_lit(failure.code()))
                            .collect(),
                    }),
                )
            });

            SysModule::ops(set.name, minted.chain(failures).collect())
        })
        .collect()
}

/// Every module `/sys` declares before the host's rows join them: the carriers in the order the root lists them, each holding its type former and the intrinsic operations over it, and hoisting the type to the root.
fn declared(syntax: &SyntaxRegistry) -> Vec<SysModule> {
    vec![
        SysModule::carrier(
            "Nat",
            &["The whole numbers from zero up, with no highest."],
            operation("Nat", &[], syntax, Intrinsic::NatType),
            items(nat_ops(syntax)),
        ),
        SysModule::carrier(
            "Byte",
            &["A single byte, from zero through 255."],
            operation("Byte", &[], syntax, Intrinsic::ByteType),
            items(byte_ops(syntax)),
        ),
        SysModule::carrier(
            "Int",
            &["The whole numbers, negative and not, with no least and no greatest."],
            operation("Int", &[], syntax, Intrinsic::IntType),
            items(int_ops(syntax)),
        ),
        SysModule::carrier(
            "Flt",
            &["A binary64 floating-point number."],
            operation("Flt", &[], syntax, Intrinsic::FltType),
            flt_ops(syntax),
        ),
        SysModule::carrier(
            "Bits",
            &["A packed run of bits, written `b[…]`."],
            operation("Bits", &[], syntax, Intrinsic::BinType(Grain::B)),
            [
                items(bin_ops(Grain::B, syntax)),
                items(regrain_op(Grain::B, syntax)),
            ]
            .concat(),
        ),
        SysModule::carrier(
            "Bytes",
            &["A packed run of bytes, written `x[…]`."],
            operation("Bytes", &[], syntax, Intrinsic::BinType(Grain::X)),
            [
                items(bin_ops(Grain::X, syntax)),
                items(regrain_op(Grain::X, syntax)),
            ]
            .concat(),
        ),
        // The propositions `/sys`'s own operations state their preconditions in sit beside the decision they reflect: `Holds` is written beside every bound in the roster, and `True` and `False` are what it reduces to.
        SysModule::carrier(
            "Bool",
            &["The two truth values, `true` and `false`."],
            operation("Bool", &[], syntax, Intrinsic::BoolType),
            [
                items(bool_ops(syntax)),
                vec![true_prop(), false_prop(), holds().into_item()],
            ]
            .concat(),
        ),
        SysModule::carrier(
            "Handle",
            &[
                "An open stream the host holds — a file, a socket, or one of the three standard streams.",
            ],
            operation("Handle", &[], syntax, Intrinsic::HandleType),
            items(handle_ops(syntax)),
        ),
        SysModule::carrier(
            "List",
            &["A run of values of one type, written `[…]`."],
            operation("List", &["T"], syntax, Intrinsic::ListType(name("T"))),
            items(list_ops(syntax)),
        ),
        SysModule::carrier(
            "Cell",
            &["An initially empty holder whose first fill wins."],
            operation("Cell", &["T"], syntax, Intrinsic::CellType(name("T"))),
            items(cell_ops(syntax)),
        ),
        SysModule::carrier(
            "Channel",
            &["A bounded FIFO shared within one program instance."],
            operation("Channel", &["T"], syntax, Intrinsic::ChannelType(name("T"))),
            channel_ops(syntax),
        ),
        SysModule::carrier(
            "Io",
            &[
                "A description of something the host does, yielding a `T`.",
                "",
                "Holding one performs nothing: a description runs by being the program's tail, so forcing the same one twice does the work twice, and there is no operation taking an `Io(T)` back to a `T`.",
            ],
            operation("Io", &["T"], syntax, Intrinsic::IoType(name("T"))),
            items(io_ops(syntax)),
        ),
    ]
}

/// Construct the generated `/sys` surface module.
///
/// **One declared roster.** Every `/sys` module is a `SysModule` with a label, written here from the intrinsic table, and then the wire-code mirror. No host operation is declared here: a host's rows are `foreign` declarations of `/std`, in the module that reads each.
///
/// Exposed for the build-time prelude artifact builder; production compilation never lowers it at runtime.
pub fn sys_module(syntax: &SyntaxRegistry) -> Module {
    let items = declared(syntax)
        .into_iter()
        .chain(code_modules())
        .flat_map(SysModule::into_items)
        .chain([option_decl(), result_decl()])
        .collect();

    Module::built(items)
}
