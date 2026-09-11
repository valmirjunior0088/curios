//! The roster of `/sys`: every intrinsic type and operation the language provides, built directly as `text` AST and prepended to every parsed `Entrypoint`, so intrinsics participate in the module system like any other binding.
//!
//! Bodies bake the `text::Intrinsic::*` nodes in directly, so the roster needs no internal name resolution — with one exception, the propositions an operation states as its precondition, which are `/sys`'s own and are named absolutely so a declaration resolves wherever the roster puts it.
//!
//! The propositions a decided bound is stated in are the one part written rather than built, and so the one part parsed — see `flt_bounds` and `holds` below. What separates the two is whether a surface spelling exists: an intrinsic has none and must be constructed, while a proposition over intrinsics is ordinary Curios.
//!
//! **These declarations are a second statement of `Intrinsic::signature`, not a projection of it.** The table is `curios-core`'s; this roster is what a caller actually names, and elaborating a body here checks its operands against that table and unifies its result with the declared one. A declaration disagreeing with the operation its body constructs does not compile, and the prelude build is where that is enforced — so deriving either from the other would make the check compare the roster with itself.
//!
//! This file holds the roster alone. How a declaration is built is `constructors.rs`, how a term is built is `helpers.rs`, and how a host store's wire row becomes one is `host_rows.rs`.

mod constructors;
use constructors::*;

mod helpers;
use helpers::*;

mod host_rows;
// `pub(crate)` rather than plain, and the one place this module widens anything: `foreign_signature` answers `into_core`'s user-written `foreign`, and it reaches the crate root through here.
pub(crate) use host_rows::*;

#[cfg(test)]
mod tests;

use {
    super::{
        Doc, Intrinsic, Match, MatchPattern, MatrixArm, Module, Nat, NatLiteral, Subterm, Term,
        TopCase, TopInduct, TopItem,
    },
    curios_abi::{ForeignStore, event, file_kind, open_mode, status, stdio, stdio_mode},
    curios_num::Integer,
    curios_utilities::{Grain, Plicity, SyntaxRegistry},
};

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
                    },
                    MatrixArm {
                        pattern: MatchPattern::Bool(false),
                        body: name("False"),
                    },
                ],
            })
            .into(),
        ),
    )
}

// The two `Flt` narrowings' domains, stated inside `/sys/Flt` because that is the carrier they are about. Each is a conjunction rather than one comparison, which is why `Intrinsic::signature` names them instead of building them as it builds the rest.
fn flt_bounds(syntax: &SyntaxRegistry) -> Vec<Decl> {
    let and = |left: Term, right: Term| intrinsic(Intrinsic::BoolAnd(left, right));
    let le = |left: Term, right: Term| intrinsic(Intrinsic::FltLe(left, right));
    let lt = |left: Term, right: Term| intrinsic(Intrinsic::FltLt(left, right));

    vec![
        documented(
            &["That `a` is neither infinite nor beyond what `Flt` represents, decided."],
            pub_fn(
                "Finite",
                vec![("a", flt())],
                prop(),
                decided(
                    syntax,
                    and(
                        lt(flt_lit(f64::NEG_INFINITY), name("a")),
                        lt(name("a"), flt_lit(f64::INFINITY)),
                    ),
                ),
            ),
        ),
        documented(
            &["That `a` is zero or above and within range, decided."],
            pub_fn(
                "NonNeg",
                vec![("a", flt())],
                prop(),
                decided(
                    syntax,
                    and(
                        le(flt_lit(0.0), name("a")),
                        lt(name("a"), flt_lit(f64::INFINITY)),
                    ),
                ),
            ),
        ),
    ]
}

fn nat_succ() -> Decl {
    pub_fn(
        "succ",
        vec![("a", nat())],
        nat(),
        intrinsic(Intrinsic::Nat(Nat::Succ(
            NatLiteral::number(1usize),
            name("a"),
        ))),
    )
}

// `0 < b`: a natural is nonzero exactly when zero is below it, so the divisions reuse the bound the accessors already state rather than introducing a second proposition for the same fact.
fn nat_nonzero(syntax: &SyntaxRegistry) -> Term {
    decided(
        syntax,
        applied(
            sys_op(&["sys", "Nat", "lt"]),
            vec![intrinsic(Intrinsic::Nat(Nat::Zero)), name("b")],
        ),
    )
}

fn int_nonzero(syntax: &SyntaxRegistry) -> Term {
    decided(
        syntax,
        applied(
            sys_op(&["sys", "Int", "neq"]),
            vec![name("b"), intrinsic(Intrinsic::Int(Integer::from(0)))],
        ),
    )
}

fn nat_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(&["One more than `a`."], nat_succ()),
        documented(
            &["Whether the two are equal."],
            binary("eql", nat(), bool_(), Intrinsic::NatEql),
        ),
        documented(
            &["Whether the two differ."],
            binary("neq", nat(), bool_(), Intrinsic::NatNeq),
        ),
        documented(
            &["Their sum."],
            binary("add", nat(), nat(), Intrinsic::NatAdd),
        ),
        documented(
            &[
                "`b` taken from `a`, and zero where `b` is the larger — a natural is never negative.",
            ],
            binary("sub", nat(), nat(), Intrinsic::NatSub),
        ),
        documented(
            &["Their product."],
            binary("mul", nat(), nat(), Intrinsic::NatMul),
        ),
        documented(
            &["`a` divided by `b`, rounded down, under the evidence that `b` is not zero."],
            guarded_binary(
                "div",
                nat(),
                nat(),
                nat_nonzero(syntax),
                |dividend, divisor, non_zero| Intrinsic::NatDiv {
                    dividend,
                    divisor,
                    non_zero,
                },
            ),
        ),
        documented(
            &["What `a` leaves after dividing by `b`, under the evidence that `b` is not zero."],
            guarded_binary(
                "rem",
                nat(),
                nat(),
                nat_nonzero(syntax),
                |dividend, divisor, non_zero| Intrinsic::NatRem {
                    dividend,
                    divisor,
                    non_zero,
                },
            ),
        ),
        documented(
            &["Whether `a` is below `b`."],
            binary("lt", nat(), bool_(), Intrinsic::NatLt),
        ),
        // **`gt` and `ge` are built as their `lt`/`le` mirrors, on every carrier.** A comparison is spelled one way from the moment it enters Core, so a case equation recorded on a guard as written and the same guard met reduced inside a proposition are one term — the reducer's own mirror covers an intrinsic built by hand, but a spelling that never exists cannot be keyed on. See `documentation/design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md`.
        documented(
            &["Whether `a` is above `b`."],
            binary("gt", nat(), bool_(), |a, b| Intrinsic::NatLt(b, a)),
        ),
        documented(
            &["Whether `a` is below `b` or equal to it."],
            binary("le", nat(), bool_(), Intrinsic::NatLe),
        ),
        documented(
            &["Whether `a` is above `b` or equal to it."],
            binary("ge", nat(), bool_(), |a, b| Intrinsic::NatLe(b, a)),
        ),
        documented(
            &["Their bits, kept where both have one."],
            binary("and", nat(), nat(), Intrinsic::NatAnd),
        ),
        documented(
            &["Their bits, kept where either has one."],
            binary("or", nat(), nat(), Intrinsic::NatOr),
        ),
        documented(
            &["Their bits, kept where exactly one has one."],
            binary("xor", nat(), nat(), Intrinsic::NatXor),
        ),
        documented(
            &["`a` doubled `b` times."],
            binary("shl", nat(), nat(), Intrinsic::NatShl),
        ),
        documented(
            &["`a` halved `b` times, rounded down each time."],
            binary("shr", nat(), nat(), Intrinsic::NatShr),
        ),
        documented(
            &["The same number as an `Int`."],
            unary("to_int", nat(), int(), Intrinsic::NatToInt),
        ),
        documented(
            &["The nearest `Flt` to it."],
            unary("to_flt", nat(), flt(), Intrinsic::NatToFlt),
        ),
        documented(
            &["The same number as a `Byte`, under the evidence that it is below `256`."],
            guarded_unary(
                "to_byte",
                nat(),
                byte(),
                decided(
                    syntax,
                    applied(sys_op(&["sys", "Nat", "lt"]), vec![name("a"), nat_lit(256)]),
                ),
                |nat, below| Intrinsic::NatToByte { nat, below },
            ),
        ),
    ]
}

fn byte_ops() -> Vec<Decl> {
    vec![documented(
        &["The same number as a `Nat`."],
        unary("to_nat", byte(), nat(), Intrinsic::ByteToNat),
    )]
}

// `Bool` rides the same i31ref/u32 carrier as `Nat`, with `false`/`true` as `0`/`1`. `and`/`or`/`xor` are bitwise machine ops on those bits — exact boolean logic — and `eql` is the `Nat` equality op (`i32.eq`) on that single bit, so all four are intrinsics rather than `match` definitions. `not` has no machine instruction; `/std/Bool` defines it as `xor(b, true)`.
fn bool_ops() -> Vec<Decl> {
    vec![
        documented(
            &["True when both are."],
            binary("and", bool_(), bool_(), Intrinsic::BoolAnd),
        ),
        documented(
            &["True when either is."],
            binary("or", bool_(), bool_(), Intrinsic::BoolOr),
        ),
        documented(
            &["True when exactly one is."],
            binary("xor", bool_(), bool_(), Intrinsic::BoolXor),
        ),
        documented(
            &["Whether the two are equal."],
            binary("eql", bool_(), bool_(), Intrinsic::BoolEql),
        ),
    ]
}

fn int_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(
            &["Whether the two are equal."],
            binary("eql", int(), bool_(), Intrinsic::IntEql),
        ),
        documented(
            &["Whether the two differ."],
            binary("neq", int(), bool_(), Intrinsic::IntNeq),
        ),
        documented(
            &["Their sum."],
            binary("add", int(), int(), Intrinsic::IntAdd),
        ),
        documented(
            &["`b` taken from `a`."],
            binary("sub", int(), int(), Intrinsic::IntSub),
        ),
        documented(
            &["Their product."],
            binary("mul", int(), int(), Intrinsic::IntMul),
        ),
        documented(
            &["`a` divided by `b`, rounded toward zero."],
            guarded_binary(
                "div",
                int(),
                int(),
                int_nonzero(syntax),
                |dividend, divisor, non_zero| Intrinsic::IntDiv {
                    dividend,
                    divisor,
                    non_zero,
                },
            ),
        ),
        documented(
            &["What `a` leaves after dividing by `b`, taking its sign from `a`."],
            guarded_binary(
                "rem",
                int(),
                int(),
                int_nonzero(syntax),
                |dividend, divisor, non_zero| Intrinsic::IntRem {
                    dividend,
                    divisor,
                    non_zero,
                },
            ),
        ),
        documented(
            &["Whether `a` is below `b`."],
            binary("lt", int(), bool_(), Intrinsic::IntLt),
        ),
        documented(
            &["Whether `a` is above `b`."],
            binary("gt", int(), bool_(), |a, b| Intrinsic::IntLt(b, a)),
        ),
        documented(
            &["Whether `a` is below `b` or equal to it."],
            binary("le", int(), bool_(), Intrinsic::IntLe),
        ),
        documented(
            &["Whether `a` is above `b` or equal to it."],
            binary("ge", int(), bool_(), |a, b| Intrinsic::IntLe(b, a)),
        ),
        // Bitwise ops on the signed i31 carrier. `and`/`or`/`xor` are exact bit ops; `shl` refuses a result past the carrier like `Nat/shl`; `shr` is arithmetic (sign-preserving). Both shifts count in `Nat`, as `Nat/shl` does, so a negative count — which the theory never defined — cannot be written. `not` is `/std/Int`'s `xor(x, -1)`.
        documented(
            &["Their bits, kept where both have one."],
            binary("and", int(), int(), Intrinsic::IntAnd),
        ),
        documented(
            &["Their bits, kept where either has one."],
            binary("or", int(), int(), Intrinsic::IntOr),
        ),
        documented(
            &["Their bits, kept where exactly one has one."],
            binary("xor", int(), int(), Intrinsic::IntXor),
        ),
        documented(
            &["`a` doubled `b` times."],
            pub_fn(
                "shl",
                vec![("a", int()), ("b", nat())],
                int(),
                intrinsic(Intrinsic::IntShl(name("a"), name("b"))),
            ),
        ),
        documented(
            &["`a` halved `b` times, rounded toward negative — the sign is kept."],
            pub_fn(
                "shr",
                vec![("a", int()), ("b", nat())],
                int(),
                intrinsic(Intrinsic::IntShr(name("a"), name("b"))),
            ),
        ),
        documented(
            &["The same number as a `Nat`, under the evidence that it is not negative."],
            guarded_unary(
                "to_nat",
                int(),
                nat(),
                decided(
                    syntax,
                    applied(
                        sys_op(&["sys", "Int", "ge"]),
                        vec![name("a"), intrinsic(Intrinsic::Int(Integer::from(0)))],
                    ),
                ),
                |int, non_neg| Intrinsic::IntToNat { int, non_neg },
            ),
        ),
        documented(
            &["The nearest `Flt` to it."],
            unary("to_flt", int(), flt(), Intrinsic::IntToFlt),
        ),
    ]
}

fn flt_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    let items = vec![
        documented(
            &["Their sum."],
            binary("add", flt(), flt(), Intrinsic::FltAdd),
        ),
        documented(
            &["`b` taken from `a`."],
            binary("sub", flt(), flt(), Intrinsic::FltSub),
        ),
        documented(
            &["Their product."],
            binary("mul", flt(), flt(), Intrinsic::FltMul),
        ),
        documented(
            &["`a` divided by `b`."],
            binary("div", flt(), flt(), Intrinsic::FltDiv),
        ),
        documented(
            &["What `a` leaves after dividing by `b`."],
            binary("rem", flt(), flt(), Intrinsic::FltRem),
        ),
        documented(
            &["The smaller of the two."],
            binary("min", flt(), flt(), Intrinsic::FltMin),
        ),
        documented(
            &["The larger of the two."],
            binary("max", flt(), flt(), Intrinsic::FltMax),
        ),
        documented(
            &["Whether the two are equal."],
            binary("eql", flt(), bool_(), Intrinsic::FltEql),
        ),
        documented(
            &["Whether the two differ."],
            binary("neq", flt(), bool_(), Intrinsic::FltNeq),
        ),
        documented(
            &["Whether `a` is below `b`."],
            binary("lt", flt(), bool_(), Intrinsic::FltLt),
        ),
        documented(
            &["Whether `a` is above `b`."],
            binary("gt", flt(), bool_(), |a, b| Intrinsic::FltLt(b, a)),
        ),
        documented(
            &["Whether `a` is below `b` or equal to it."],
            binary("le", flt(), bool_(), Intrinsic::FltLe),
        ),
        documented(
            &["Whether `a` is above `b` or equal to it."],
            binary("ge", flt(), bool_(), |a, b| Intrinsic::FltLe(b, a)),
        ),
        documented(
            &["`a` with its sign flipped."],
            unary("neg", flt(), flt(), Intrinsic::FltNeg),
        ),
        documented(
            &["`a` without its sign."],
            unary("abs", flt(), flt(), Intrinsic::FltAbs),
        ),
        documented(
            &["The square root of `a`."],
            unary("sqrt", flt(), flt(), Intrinsic::FltSqrt),
        ),
        documented(
            &["The greatest whole number at or below `a`."],
            unary("floor", flt(), flt(), Intrinsic::FltFloor),
        ),
        documented(
            &["The least whole number at or above `a`."],
            unary("ceil", flt(), flt(), Intrinsic::FltCeil),
        ),
        documented(
            &["`a` with its fraction dropped, toward zero."],
            unary("trunc", flt(), flt(), Intrinsic::FltTrunc),
        ),
        documented(
            &["The whole number nearest `a`, and the even one where it falls halfway."],
            unary("nearest", flt(), flt(), Intrinsic::FltNearest),
        ),
        documented(
            &["`a` carrying `b`'s sign."],
            binary("copysign", flt(), flt(), Intrinsic::FltCopysign),
        ),
        documented(
            &["The whole part of `a` as a `Nat`, under the evidence that it is not negative."],
            guarded_unary(
                "to_nat",
                flt(),
                nat(),
                applied(registered(syntax.proof.flt_non_neg), vec![name("a")]),
                |flt, non_neg| Intrinsic::FltToNat { flt, non_neg },
            ),
        ),
        documented(
            &["The whole part of `a` as an `Int`, under the evidence that it is finite."],
            guarded_unary(
                "to_int",
                flt(),
                int(),
                applied(registered(syntax.proof.flt_finite), vec![name("a")]),
                |flt, finite| Intrinsic::FltToInt { flt, finite },
            ),
        ),
        documented(
            &["Its eight bytes, least significant first."],
            unary("to_le_bytes", flt(), bin(Grain::X), Intrinsic::FltToLeBytes),
        ),
        documented(
            &["The number those eight bytes spell, least significant first."],
            guarded_unary(
                "of_le_bytes",
                bin(Grain::X),
                flt(),
                decided(
                    syntax,
                    applied(
                        sys_op(&["sys", "Nat", "eql"]),
                        vec![
                            applied(sys_op(&["sys", "Bytes", "len"]), vec![name("a")]),
                            nat_lit(8),
                        ],
                    ),
                ),
                |bin, eight_bytes| Intrinsic::FltOfLeBytes { bin, eight_bytes },
            ),
        ),
    ];

    items.into_iter().chain(flt_bounds(syntax)).collect()
}

fn bin_ops(grain: Grain, syntax: &SyntaxRegistry) -> Vec<Decl> {
    let type_ = bin(grain);
    let atom = match grain {
        Grain::B => bool_(),
        Grain::X => byte(),
    };
    // `i < len(b)`, the bound `at` will not index without. Stated here rather than only on `/std`'s wrapper because `/std` re-exports this module: a precondition the wrapper alone carried would be bypassed by naming the raw operation, which is the defect this obligation exists to close.
    //
    // The length is the sibling `len` rather than the `BinLen` intrinsic its body bakes in, which is the one place this module needs a name resolved rather than a node planted. The two are definitionally equal and that is not enough: a scrutinee refinement is keyed on the term written, so a caller who guards with `i < len(b)` — the only spelling available to them — discharges a goal spelled that way and not one spelled with an intrinsic they cannot write.
    let in_range = decided(
        syntax,
        applied(
            sys_op(&["sys", "Nat", "lt"]),
            vec![name("i"), applied(name("len"), vec![name("b")])],
        ),
    );
    vec![
        documented(
            &["How many it holds."],
            pub_fn(
                "len",
                vec![("b", type_.clone())],
                nat(),
                intrinsic(Intrinsic::BinLen(grain, name("b"))),
            ),
        ),
        documented(
            &["Whether the two hold the same, in the same order."],
            pub_fn(
                "eql",
                vec![("a", type_.clone()), ("b", type_.clone())],
                bool_(),
                intrinsic(Intrinsic::BinEql(grain, name("a"), name("b"))),
            ),
        ),
        documented(
            &["The element at `i`, under the evidence that `i` is within the length."],
            pub_fn_marked(
                "get",
                vec![
                    (Plicity::Explicit, "b", type_.clone()),
                    (Plicity::Explicit, "i", nat()),
                    (Plicity::Implicit, "ok", in_range),
                ],
                atom.clone(),
                intrinsic(Intrinsic::BinGet {
                    grain,
                    bin: name("b"),
                    index: name("i"),
                    in_range: name("ok"),
                }),
            ),
        ),
        // A window is a start and a *count*, so a reversed one cannot be spelled and the ordering half of the old bound has no proposition left to state. What survives is that the window ends inside the value.
        documented(
            &[
                "The run from `from` up to but not including `to`, under the evidence that both are within the length and in order.",
            ],
            pub_fn_marked(
                "slice",
                vec![
                    (Plicity::Explicit, "b", type_.clone()),
                    (Plicity::Explicit, "s", nat()),
                    (Plicity::Explicit, "l", nat()),
                    (
                        Plicity::Implicit,
                        "within",
                        decided(
                            syntax,
                            applied(
                                sys_op(&["sys", "Nat", "le"]),
                                vec![
                                    nat_plus(name("s"), name("l")),
                                    applied(name("len"), vec![name("b")]),
                                ],
                            ),
                        ),
                    ),
                ],
                type_.clone(),
                intrinsic(Intrinsic::BinSlice {
                    grain,
                    bin: name("b"),
                    start: name("s"),
                    length: name("l"),
                    within: name("within"),
                }),
            ),
        ),
        documented(
            &["`b` with `x` added after its last element."],
            pub_fn(
                "append",
                vec![("b", type_.clone()), ("x", atom)],
                type_.clone(),
                intrinsic(Intrinsic::BinAppend {
                    grain,
                    bin: name("b"),
                    element: name("x"),
                }),
            ),
        ),
        documented(
            &["The elements of `a`, then those of `b`."],
            pub_fn(
                "concat",
                vec![("a", type_.clone()), ("b", type_.clone())],
                type_,
                intrinsic(Intrinsic::BinConcat {
                    grain,
                    left: name("a"),
                    right: name("b"),
                }),
            ),
        ),
    ]
}

fn list_ops(syntax: &SyntaxRegistry) -> Vec<Decl> {
    vec![
        documented(
            &["How many it holds."],
            pub_fn_marked(
                "len",
                vec![
                    (Plicity::Implicit, "T", type_()),
                    (Plicity::Explicit, "a", list_of(name("T"))),
                ],
                nat(),
                intrinsic(Intrinsic::ListLen {
                    element: name("T"),
                    list: name("a"),
                }),
            ),
        ),
        documented(
            &["The element at `i`, under the evidence that `i` is within the length."],
            pub_fn_marked(
                "get",
                vec![
                    (Plicity::Implicit, "T", type_()),
                    (Plicity::Explicit, "a", list_of(name("T"))),
                    (Plicity::Explicit, "i", nat()),
                    (
                        Plicity::Implicit,
                        "ok",
                        decided(
                            syntax,
                            applied(
                                sys_op(&["sys", "Nat", "lt"]),
                                vec![name("i"), applied(name("len"), vec![name("a")])],
                            ),
                        ),
                    ),
                ],
                name("T"),
                intrinsic(Intrinsic::ListGet {
                    element: name("T"),
                    list: name("a"),
                    index: name("i"),
                    in_range: name("ok"),
                }),
            ),
        ),
        // The `List` twin of `Bin/slice`'s window bound; see the comment there for why only one half survives the count.
        documented(
            &[
                "The run from `from` up to but not including `to`, under the evidence that both are within the length and in order.",
            ],
            pub_fn_marked(
                "slice",
                vec![
                    (Plicity::Implicit, "T", type_()),
                    (Plicity::Explicit, "a", list_of(name("T"))),
                    (Plicity::Explicit, "s", nat()),
                    (Plicity::Explicit, "l", nat()),
                    (
                        Plicity::Implicit,
                        "within",
                        decided(
                            syntax,
                            applied(
                                sys_op(&["sys", "Nat", "le"]),
                                vec![
                                    nat_plus(name("s"), name("l")),
                                    applied(name("len"), vec![name("a")]),
                                ],
                            ),
                        ),
                    ),
                ],
                list_of(name("T")),
                intrinsic(Intrinsic::ListSlice {
                    element: name("T"),
                    list: name("a"),
                    start: name("s"),
                    length: name("l"),
                    within: name("within"),
                }),
            ),
        ),
        documented(
            &["`a` with `x` added after its last element."],
            pub_fn_marked(
                "append",
                vec![
                    (Plicity::Implicit, "T", type_()),
                    (Plicity::Explicit, "a", list_of(name("T"))),
                    (Plicity::Explicit, "x", name("T")),
                ],
                list_of(name("T")),
                intrinsic(Intrinsic::ListAppend {
                    element: name("T"),
                    list: name("a"),
                    item: name("x"),
                }),
            ),
        ),
        documented(
            &["The elements of `a`, then those of `b`."],
            pub_fn_marked(
                "concat",
                vec![
                    (Plicity::Implicit, "T", type_()),
                    (Plicity::Explicit, "a", list_of(name("T"))),
                    (Plicity::Explicit, "b", list_of(name("T"))),
                ],
                list_of(name("T")),
                intrinsic(Intrinsic::ListConcat {
                    element: name("T"),
                    left: name("a"),
                    right: name("b"),
                }),
            ),
        ),
        documented(
            &["Each element with `f` applied to it, in order."],
            pub_fn_marked(
                "map",
                vec![
                    (Plicity::Implicit, "A", type_()),
                    (Plicity::Implicit, "B", type_()),
                    (Plicity::Explicit, "a", list_of(name("A"))),
                    (Plicity::Explicit, "f", fn_of(name("A"), name("B"))),
                ],
                list_of(name("B")),
                intrinsic(Intrinsic::ListMap {
                    from: name("A"),
                    to: name("B"),
                    list: name("a"),
                    function: name("f"),
                }),
            ),
        ),
    ]
}

// Allocating a cell, reading one, and writing one are all host effects, so all three return descriptions. `Cell/get` is the operation the whole discipline was named for: a scrutinee spelled `Cell/get(c)` denotes a different value before and after a `Cell/set`, and giving it an `Io` result is what makes that spelling ill-typed in scrutinee position rather than something an analysis has to notice.
fn cell_ops() -> Vec<Decl> {
    vec![
        documented(
            &["A new cell holding `x`."],
            pub_fn_marked(
                "new",
                vec![
                    (Plicity::Implicit, "T", type_()),
                    (Plicity::Explicit, "x", name("T")),
                ],
                io_of(cell_of(name("T"))),
                intrinsic(Intrinsic::Cell {
                    element: name("T"),
                    initial: name("x"),
                }),
            ),
        ),
        documented(
            &["Put `v` in the cell."],
            pub_fn_marked(
                "set",
                vec![
                    (Plicity::Implicit, "T", type_()),
                    (Plicity::Explicit, "c", cell_of(name("T"))),
                    (Plicity::Explicit, "v", name("T")),
                ],
                io_of(unit()),
                intrinsic(Intrinsic::CellSet {
                    element: name("T"),
                    cell: name("c"),
                    value: name("v"),
                }),
            ),
        ),
        documented(
            &["What the cell holds."],
            pub_fn_marked(
                "get",
                vec![
                    (Plicity::Implicit, "T", type_()),
                    (Plicity::Explicit, "c", cell_of(name("T"))),
                ],
                io_of(name("T")),
                intrinsic(Intrinsic::CellGet {
                    element: name("T"),
                    cell: name("c"),
                }),
            ),
        ),
    ]
}

// The monad of the `/sys/Io` type, and nothing else: `Io` owns the sequencing, never the operations. An operation belongs with its subject — the one its own store row names, not the type its result wears.
fn io_ops() -> Vec<Decl> {
    vec![
        documented(
            &["The description that performs nothing and yields `x`."],
            pub_fn_marked(
                "pure",
                vec![
                    (Plicity::Implicit, "T", type_()),
                    (Plicity::Explicit, "x", name("T")),
                ],
                io_of(name("T")),
                intrinsic(Intrinsic::IoPure {
                    result: name("T"),
                    value: name("x"),
                }),
            ),
        ),
        documented(
            &["The description that performs `m`, then whatever `f` makes of its result."],
            pub_fn_marked(
                "bind",
                vec![
                    (Plicity::Implicit, "A", type_()),
                    (Plicity::Implicit, "B", type_()),
                    (Plicity::Explicit, "m", io_of(name("A"))),
                    (Plicity::Explicit, "f", fn_of(name("A"), io_of(name("B")))),
                ],
                io_of(name("B")),
                intrinsic(Intrinsic::IoBind {
                    from: name("A"),
                    to: name("B"),
                    action: name("m"),
                    continuation: name("f"),
                }),
            ),
        ),
    ]
}

// The values and operations of the `/sys/Handle` type: the three standard streams, handle identity, and `host` — the store rows whose subject is the handle itself (`read`, `write`, `poll`, `close`), which join their type module rather than open one of their own.
fn handle_ops() -> Vec<Decl> {
    vec![
        documented(
            &["The stream the host feeds the program."],
            pub_let(
                "stdin",
                handle(),
                intrinsic(Intrinsic::Handle(stdio::STDIN)),
            ),
        ),
        documented(
            &["The stream the program writes its output to."],
            pub_let(
                "stdout",
                handle(),
                intrinsic(Intrinsic::Handle(stdio::STDOUT)),
            ),
        ),
        documented(
            &["The stream the program writes its diagnostics to."],
            pub_let(
                "stderr",
                handle(),
                intrinsic(Intrinsic::Handle(stdio::STDERR)),
            ),
        ),
    ]
}

/// The one operation placed by hand rather than by a row: `exit` is `Intrinsic::ProcExit` and traps instead of returning, so no `WireSignature` describes it — but it is a process operation like `args` and `env`, so it joins the module its subject already opened.
///
/// `(@A : Type, n : Nat) -> Io(A)`: the description yields whatever the region wanted, which is sound because `Io` has no eliminator — an inhabitant of `Io(False)` proves nothing — and is what lets an exiting arm end a region of any type instead of only a unit one.
fn proc_exit() -> Decl {
    pub_fn_marked(
        "exit",
        vec![
            (Plicity::Implicit, "A", type_()),
            (Plicity::Explicit, "n", nat()),
        ],
        io_of(name("A")),
        intrinsic(Intrinsic::ProcExit {
            result: name("A"),
            code: name("n"),
        }),
    )
}

/// The wire-code mirror: the guest counterpart of ABI wire codes, so the standard library compares against named constants the host derives from the same source. Each is named by the tag it holds — `status`, `event`, `open_mode`, `file_kind`, `stdio_mode` — as `curios-abi`'s `codes` names them, and all are lowercase because no type backs them.
fn code_modules() -> Vec<SysModule> {
    vec![
        // The wire-code mirror: the guest counterpart of ABI wire codes, so the standard library compares against named constants the host derives from the same source.
        SysModule::ops(
            "status",
            vec![
                pub_let("ok", nat(), nat_lit(status::OK)),
                pub_let("eof", nat(), nat_lit(status::EOF)),
                pub_let("not_found", nat(), nat_lit(status::NOT_FOUND)),
                pub_let(
                    "permission_denied",
                    nat(),
                    nat_lit(status::PERMISSION_DENIED),
                ),
                pub_let("exists", nat(), nat_lit(status::ALREADY_EXISTS)),
                pub_let("refused", nat(), nat_lit(status::CONNECTION_REFUSED)),
                pub_let("would_block", nat(), nat_lit(status::WOULD_BLOCK)),
                pub_let("tls", nat(), nat_lit(status::TLS_ERROR)),
                pub_let("not_empty", nat(), nat_lit(status::NOT_EMPTY)),
                pub_let("is_directory", nat(), nat_lit(status::IS_DIRECTORY)),
                pub_let("not_directory", nat(), nat_lit(status::NOT_DIRECTORY)),
                pub_let("other_base", nat(), nat_lit(status::OTHER_BASE)),
            ],
        ),
        SysModule::ops(
            "event",
            vec![
                pub_let("read", nat(), nat_lit(event::READ)),
                pub_let("write", nat(), nat_lit(event::WRITE)),
                pub_let("err", nat(), nat_lit(event::ERR)),
                pub_let("hup", nat(), nat_lit(event::HUP)),
            ],
        ),
        SysModule::ops(
            "open_mode",
            vec![
                pub_let("read", nat(), nat_lit(open_mode::READ)),
                pub_let("write", nat(), nat_lit(open_mode::WRITE)),
                pub_let("append", nat(), nat_lit(open_mode::APPEND)),
            ],
        ),
        SysModule::ops(
            "file_kind",
            vec![
                pub_let("file", nat(), nat_lit(file_kind::FILE)),
                pub_let("directory", nat(), nat_lit(file_kind::DIRECTORY)),
                pub_let("symlink", nat(), nat_lit(file_kind::SYMLINK)),
                pub_let("other", nat(), nat_lit(file_kind::OTHER)),
            ],
        ),
        SysModule::ops(
            "stdio_mode",
            vec![
                pub_let("inherit", nat(), nat_lit(stdio_mode::INHERIT)),
                pub_let("pipe", nat(), nat_lit(stdio_mode::PIPE)),
                pub_let("null", nat(), nat_lit(stdio_mode::NULL)),
            ],
        ),
    ]
}

/// Every module `/sys` declares before the host's rows join them: the carriers in the order the root lists them, each holding its type former and the intrinsic operations over it and all but the two packed runs hoisting the type to the root, then the one module of operations that no carrier opens.
fn declared(syntax: &SyntaxRegistry) -> Vec<SysModule> {
    vec![
        SysModule::carrier(
            "Nat",
            &["The whole numbers from zero up, with no highest."],
            pub_let("Nat", type_(), nat()),
            nat_ops(syntax),
        ),
        SysModule::carrier(
            "Byte",
            &["A single byte, from zero through 255."],
            pub_let("Byte", type_(), byte()),
            byte_ops(),
        ),
        SysModule::carrier(
            "Int",
            &["The whole numbers, negative and not, with no least and no greatest."],
            pub_let("Int", type_(), int()),
            int_ops(syntax),
        ),
        SysModule::carrier(
            "Flt",
            &["A binary32 floating-point number."],
            pub_let("Flt", type_(), flt()),
            flt_ops(syntax),
        ),
        // The two packed runs share every operation name, so neither type is hoisted: `Bits` and `Bytes` are reached through their own modules.
        SysModule::packed(
            "Bits",
            &["A packed run of bits, written `b[…]`."],
            pub_let("Bits", type_(), bin(Grain::B)),
            bin_ops(Grain::B, syntax),
        ),
        SysModule::packed(
            "Bytes",
            &["A packed run of bytes, written `x[…]`."],
            pub_let("Bytes", type_(), bin(Grain::X)),
            bin_ops(Grain::X, syntax),
        ),
        SysModule::carrier(
            "Bool",
            &["The two truth values, `true` and `false`."],
            pub_let("Bool", type_(), bool_()),
            bool_ops(),
        ),
        // The one label in both inputs: its carrier declarations here, its host rows folded in by `absorb_host_rows` like any other subject's.
        SysModule::carrier(
            "Handle",
            &[
                "An open stream the host holds — a file, a socket, or one of the three standard streams.",
            ],
            pub_let("Handle", type_(), handle()),
            handle_ops(),
        ),
        SysModule::carrier(
            "List",
            &["A run of values of one type, written `[…]`."],
            pub_fn("List", vec![("T", type_())], type_(), list_of(name("T"))),
            list_ops(syntax),
        ),
        SysModule::carrier(
            "Cell",
            &[
                "A mutable holder of one value.",
                "",
                "Reading answers the last value written through any name for the same cell, so two names for one cell are not two cells.",
            ],
            pub_fn("Cell", vec![("T", type_())], type_(), cell_of(name("T"))),
            cell_ops(),
        ),
        SysModule::carrier(
            "Io",
            &[
                "A description of something the host does, yielding a `T`.",
                "",
                "Holding one performs nothing: a description runs by being the program's tail, so forcing the same one twice does the work twice, and there is no operation taking an `Io(T)` back to a `T`.",
            ],
            pub_fn("Io", vec![("T", type_())], type_(), io_of(name("T"))),
            io_ops(),
        ),
        // Declared rather than pushed in after the join, which is what a module of operations is for: `exit` used to be placed by a `find` over the joined roster, so a store that stopped carrying a `proc` row would have dropped it with no error at all. Declaring the module is what makes that unrepresentable — and it puts `exit` ahead of the rows, since a declared module precedes every module the join opens.
        SysModule::ops("proc", vec![proc_exit()]),
    ]
}

/// Construct the generated `/sys` surface module from the authoritative host function store.
///
/// **One keyed pass over one declared roster.** Every `/sys` module is a `SysModule` with a label: the ones written here from the intrinsic table, then each host row joining the module its own subject names — opening one where nothing declared the label, which is how `file`, `socket` and `dns` come to exist, and joining the declaration where something did, which is how `Handle`'s rows come to sit beside its type and `proc`'s beside `exit`. Then the propositions `/sys`'s own preconditions are stated in, and the wire-code mirror.
///
/// Exposed for the build-time prelude artifact builder; production compilation never lowers it at runtime.
pub fn sys_module(foreigns: &ForeignStore, syntax: &SyntaxRegistry) -> Module {
    let mut items = absorb_host_rows(declared(syntax), foreigns)
        .into_iter()
        .flat_map(SysModule::into_items)
        .collect::<Vec<_>>();

    // The propositions `/sys`'s own operations state their preconditions in, at the root rather than in a module of their own: a precondition is about the operation that demands it, and `Holds` is written beside every bound in the roster.
    items.extend([true_prop(), false_prop(), holds().into_item()]);
    items.extend(code_modules().into_iter().flat_map(SysModule::into_items));

    Module { items }
}
