mod helpers;
use helpers::*;

use {
    super::{
        Doc, Intrinsic, LetSignature, Match, MatchPattern, MatrixArm, Module, Nat, NatLiteral,
        Subterm, Term, TopCase, TopForeign, TopInduct, TopItem, TopLet,
    },
    curios_abi::{
        ForeignFunction, ForeignStore, Namespace, ResultShape, WireType, event, file_kind,
        open_mode, status, stdio, stdio_mode,
    },
    curios_num::Integer,
    curios_utilities::{Grain, Plicity, SyntaxRegistry},
    std::sync::Arc,
};

// The `sys` module is the home of every intrinsic type and operation. Its roster is built directly as `text` AST and prepended to every parsed `Entrypoint`, so intrinsics participate in the module system like any other binding. Bodies bake the `text::Intrinsic::*` nodes in directly, so the roster needs no internal name resolution — with one exception, the propositions an operation states as its precondition, which are `/sys`'s own and are named absolutely so a declaration resolves wherever the roster puts it.
//
// The propositions a decided bound is stated in are the one part written rather than built, and so the one part parsed — see `propositions` below. What separates the two is whether a surface spelling exists: an intrinsic has none and must be constructed, while a proposition over intrinsics is ordinary Curios.

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
fn holds() -> TopItem {
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
fn flt_bounds(syntax: &SyntaxRegistry) -> Vec<TopItem> {
    let and = |left: Term, right: Term| intrinsic(Intrinsic::BoolAnd(left, right));
    let le = |left: Term, right: Term| intrinsic(Intrinsic::FltLe(left, right));

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
                        le(flt_lit(f32::MIN), name("a")),
                        le(name("a"), flt_lit(f32::MAX)),
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
                        le(name("a"), flt_lit(f32::MAX)),
                    ),
                ),
            ),
        ),
    ]
}

/// The surface type a host-boundary [`WireType`] denotes — the prelude's reading of the signature, mirrored by `core::wire_term` after lowering.
fn wire_type(type_: &WireType) -> Term {
    match type_ {
        WireType::Nat => nat(),
        WireType::Int => int(),
        WireType::Bool => bool_(),
        WireType::Bytes => bin(Grain::X),
        WireType::Handle => handle(),
        WireType::List(element) => list_of(wire_type(&(*element).into())),
    }
}

/// A host-function declaration generated from a foreign-store row: parameter names/types and the result shape (unit, bare type, named record) come off the `WireSignature`, and the body bakes the generic `Foreign` intrinsic applied to the parameter names. Used both for the builtin store's rows (always `pub`) and, via [`foreign_signature`], for a user's own `foreign` declaration (`vis_pub` follows what they wrote).
///
/// The result is an `Io`, and this one site is what makes that true of every row the store describes — a user's own `foreign` declaration included, since a call across the wire is a host effect whoever declared it. The wire contract does not move: `curios-abi` describes the same shapes and only the guest-facing type changes, so a multi-result row reads `Io({status : Nat, bytes : Bytes})` with the record still inside the wrapper.
///
/// A row with no parameters becomes a *constant* rather than a nullary function. The nullary function was the previous discipline's workaround — a top-level value binding would force-reduce its effectful body where a type-level effect was refused — and a description needs no thunk, being one already.
fn host_fn(function: &Arc<ForeignFunction>, vis_pub: bool) -> TopLet {
    // What the row says of itself, which for a builtin is the roster's own `///` and for a user's `foreign` is nothing — their prose sits on the declaration they wrote.
    let doc = match function.description.is_empty() {
        true => None,
        false => Some(Doc {
            lines: vec![function.description.clone()],
            span: None,
        }),
    };
    let signature = &function.signature;

    let result = match signature.results.shape() {
        ResultShape::Unit => unit(),
        ResultShape::Single(result) => wire_type(&result),
        ResultShape::Record(fields) => record(
            fields
                .into_iter()
                .map(|(label, result)| (label, wire_type(&result)))
                .collect(),
        ),
    };
    let output = io_of(result);

    let body = Term::from(Subterm::Foreign(
        Arc::clone(function),
        signature
            .params
            .iter()
            .map(|(param, _)| name(param))
            .collect(),
    ));

    if signature.params.is_empty() {
        return TopLet {
            doc,
            vis_pub,
            label: function.label.clone().into(),
            signature: LetSignature::Name {
                type_: Some(output),
                body,
            },
        };
    }

    let mut declaration = fn_marked(
        vis_pub,
        &function.label,
        signature
            .params
            .iter()
            .map(|(param, type_)| (Plicity::Explicit, param.as_str(), wire_type(type_)))
            .collect(),
        output,
        body,
    );
    declaration.doc = doc;
    declaration
}

/// Handle one user-written `foreign` declaration: register its [`ForeignFunction`] into the compilation's (non-`host_ops`) foreign store, and return the ordinary [`LetSignature`] `into_core` lowers it as — wire-type bookkeeping and `host_fn`'s shape stay internal to this module, so `into_core` only ever deals with the same `LetSignature` it already knows how to lower for a plain `TopItem::Let`. `name` is the declaration's fully qualified name (leading `/`, the caller's current position while walking the module tree), which becomes the wasm import string under the `ffi` namespace. Qualified names are unique per compilation (a same-scope duplicate is a binding conflict long before lowering reaches this point), so `register`'s duplicate panic stays what it is everywhere else: a construction bug.
pub(crate) fn foreign_signature(
    declaration: &TopForeign,
    foreigns: &mut ForeignStore,
    name: String,
) -> LetSignature {
    let function = ForeignFunction {
        namespace: Namespace::Ffi,
        name,
        subject: None,
        label: declaration.label.to_string(),
        signature: declaration.signature.clone(),
        // A user's `foreign` carries its own `-- |` on the declaration they wrote, which is what a page reads; the row has nothing to add.
        description: String::new(),
    };

    foreigns.register(function.clone());

    host_fn(&Arc::new(function), declaration.vis_pub).signature
}

fn nat_succ() -> TopItem {
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

fn nat_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
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
            &["The same number as a `Byte`."],
            unary("to_byte", nat(), byte(), Intrinsic::NatToByte),
        ),
    ]
}

fn byte_ops() -> Vec<TopItem> {
    vec![
        documented(
            &["The same number as a `Nat`."],
            unary("to_nat", byte(), nat(), Intrinsic::ByteToNat),
        ),
        documented(
            &["Whether the two are equal."],
            binary("eql", byte(), bool_(), Intrinsic::ByteEql),
        ),
        documented(
            &["Whether `a` is below `b`."],
            binary("lt", byte(), bool_(), Intrinsic::ByteLt),
        ),
        documented(
            &["Whether `a` is below `b` or equal to it."],
            binary("le", byte(), bool_(), Intrinsic::ByteLe),
        ),
        documented(
            &["Whether `a` is above `b`."],
            binary("gt", byte(), bool_(), |a, b| Intrinsic::ByteLt(b, a)),
        ),
        documented(
            &["Whether `a` is above `b` or equal to it."],
            binary("ge", byte(), bool_(), |a, b| Intrinsic::ByteLe(b, a)),
        ),
    ]
}

// `Bool` rides the same i31ref/u32 carrier as `Nat`, with `false`/`true` as `0`/`1`. `and`/`or`/`xor` are bitwise machine ops on those bits — exact boolean logic — and `eql` is the `Nat` equality op (`i32.eq`) on that single bit, so all four are intrinsics rather than `match` definitions. `not` has no machine instruction; `/std/Bool` defines it as `xor(b, true)`.
fn bool_ops() -> Vec<TopItem> {
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

fn int_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
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

fn flt_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
    let mut items = vec![
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
            &["Its four bytes, least significant first."],
            unary("to_le_bytes", flt(), bin(Grain::X), Intrinsic::FltToLeBytes),
        ),
        documented(
            &["The number those four bytes spell, least significant first."],
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
                            nat_lit(4),
                        ],
                    ),
                ),
                |bin, four_bytes| Intrinsic::FltOfLeBytes { bin, four_bytes },
            ),
        ),
    ];

    items.extend(flt_bounds(syntax));
    items
}

fn bin_ops(grain: Grain, syntax: &SyntaxRegistry) -> Vec<TopItem> {
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

fn list_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
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
fn cell_ops() -> Vec<TopItem> {
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
fn io_ops() -> Vec<TopItem> {
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
fn handle_ops(mut host: Vec<TopItem>) -> Vec<TopItem> {
    let mut items = vec![
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
    ];

    items.append(&mut host);
    items
}

// Every store-described host op, grouped by the `/sys` module its own row names as its subject — groups in the order their first row appears, rows within a group in store order. The grouping is read off the rows rather than listed here, so a new row lands under its subject with nothing beside the table to update. The 0-arity clocks and `args` are constants rather than nullary functions: the function abstraction existed to keep an effectful intrinsic body unevaluated at definition time, and a description is already unevaluated (see `host_fn`).
fn host_subjects(foreigns: &ForeignStore) -> Vec<(String, Vec<TopItem>)> {
    let mut subjects: Vec<(String, Vec<TopItem>)> = vec![];

    for function in foreigns.iter() {
        let subject = function
            .subject
            .clone()
            .expect("a builtin host operation names its /sys subject");
        let item = TopItem::Let(vec![host_fn(function, true)]);

        match subjects.iter_mut().find(|(label, _)| *label == subject) {
            Some((_, items)) => items.push(item),
            None => subjects.push((subject, vec![item])),
        }
    }

    subjects
}

// Lift one subject's operations out of the grouping, leaving the rest for `host_operations`. `Handle`'s rows are the caller: they belong inside the type module `sys_module` already builds, so they cannot be emitted as a root module of their own.
fn take_subject(subjects: &mut Vec<(String, Vec<TopItem>)>, subject: &str) -> Vec<TopItem> {
    let index = subjects
        .iter()
        .position(|(label, _)| label == subject)
        .unwrap_or_else(|| panic!("no host operation names the '{subject}' subject"));

    subjects.remove(index).1
}

// The remaining host-operation subjects, `exit`, and the wire-code mirror: one `/sys` module per subject, then the code modules, each named by the tag it holds — `status`, `event`, `open_mode`, `file_kind`, `stdio_mode` — as `curios-abi`'s `codes` names them. Every one of these names is lowercase because no type backs it — a capitalized `/sys` module is one whose type the root facade re-exports, and the code modules are `Nat` constants.
fn host_operations(subjects: Vec<(String, Vec<TopItem>)>) -> Vec<TopItem> {
    let mut items = subjects
        .into_iter()
        .map(|(subject, mut ops)| {
            // `exit` is `Intrinsic::ProcExit` rather than a store row — it traps instead of returning, so no `WireSignature` describes it — but it is a process operation like `args` and `env`, so it is placed by hand in the module its subject already opened. `(@A : Type, n : Nat) -> Io(A)`: the description yields whatever the region wanted, which is sound because `Io` has no eliminator — an inhabitant of `Io(False)` proves nothing — and is what lets an exiting arm end a region of any type instead of only a unit one.
            if subject == "proc" {
                ops.push(pub_fn_marked(
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
                ));
            }

            pub_mod(&subject, ops)
        })
        .collect::<Vec<_>>();

    items.extend([
        // The wire-code mirror: the guest counterpart of ABI wire codes, so the standard library compares against named constants the host derives from the same source.
        pub_mod(
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
        pub_mod(
            "event",
            vec![
                pub_let("read", nat(), nat_lit(event::READ)),
                pub_let("write", nat(), nat_lit(event::WRITE)),
                pub_let("err", nat(), nat_lit(event::ERR)),
                pub_let("hup", nat(), nat_lit(event::HUP)),
            ],
        ),
        pub_mod(
            "open_mode",
            vec![
                pub_let("read", nat(), nat_lit(open_mode::READ)),
                pub_let("write", nat(), nat_lit(open_mode::WRITE)),
                pub_let("append", nat(), nat_lit(open_mode::APPEND)),
            ],
        ),
        pub_mod(
            "file_kind",
            vec![
                pub_let("file", nat(), nat_lit(file_kind::FILE)),
                pub_let("directory", nat(), nat_lit(file_kind::DIRECTORY)),
                pub_let("symlink", nat(), nat_lit(file_kind::SYMLINK)),
                pub_let("other", nat(), nat_lit(file_kind::OTHER)),
            ],
        ),
        pub_mod(
            "stdio_mode",
            vec![
                pub_let("inherit", nat(), nat_lit(stdio_mode::INHERIT)),
                pub_let("pipe", nat(), nat_lit(stdio_mode::PIPE)),
                pub_let("null", nat(), nat_lit(stdio_mode::NULL)),
            ],
        ),
    ]);

    items
}

/// Construct the generated `/sys` surface module from the authoritative host function store. Each type module (`Nat`, …, `Handle`, `List`, `Cell`, `Io`) holds its type and operations and hoists the type to the `/sys` root; each host-operation subject the store names becomes a module of its own (`file`, `socket`, `dns`, …) except `Handle`'s, which join their type module; then the code modules. Exposed for the build-time prelude artifact builder; production compilation never lowers it at runtime.
pub fn sys_module(foreigns: &ForeignStore, syntax: &SyntaxRegistry) -> Module {
    let mut subjects = host_subjects(foreigns);
    let handle_host = take_subject(&mut subjects, "Handle");

    let mut items = vec![
        pub_mod(
            "Nat",
            with_type(
                documented(
                    &["The whole numbers from zero up, with no highest."],
                    pub_let("Nat", type_(), nat()),
                ),
                nat_ops(syntax),
            ),
        ),
        pub_use("Nat"),
        pub_mod(
            "Byte",
            with_type(
                documented(
                    &["A single byte, from zero through 255."],
                    pub_let("Byte", type_(), byte()),
                ),
                byte_ops(),
            ),
        ),
        pub_use("Byte"),
        pub_mod(
            "Int",
            with_type(
                documented(
                    &["The whole numbers, negative and not, with no least and no greatest."],
                    pub_let("Int", type_(), int()),
                ),
                int_ops(syntax),
            ),
        ),
        pub_use("Int"),
        pub_mod(
            "Flt",
            with_type(
                documented(
                    &["A binary32 floating-point number."],
                    pub_let("Flt", type_(), flt()),
                ),
                flt_ops(syntax),
            ),
        ),
        pub_use("Flt"),
        pub_mod(
            "Bits",
            with_type(
                documented(
                    &["A packed run of bits, written `b[…]`."],
                    pub_let("Bits", type_(), bin(Grain::B)),
                ),
                bin_ops(Grain::B, syntax),
            ),
        ),
        pub_mod(
            "Bytes",
            with_type(
                documented(
                    &["A packed run of bytes, written `x[…]`."],
                    pub_let("Bytes", type_(), bin(Grain::X)),
                ),
                bin_ops(Grain::X, syntax),
            ),
        ),
        pub_mod(
            "Bool",
            with_type(
                documented(
                    &["The two truth values, `true` and `false`."],
                    pub_let("Bool", type_(), bool_()),
                ),
                bool_ops(),
            ),
        ),
        pub_use("Bool"),
        pub_mod(
            "Handle",
            with_type(
                documented(
                    &[
                        "An open stream the host holds — a file, a socket, or one of the three standard streams.",
                    ],
                    pub_let("Handle", type_(), handle()),
                ),
                handle_ops(handle_host),
            ),
        ),
        pub_use("Handle"),
        pub_mod(
            "List",
            with_type(
                documented(
                    &["A run of values of one type, written `[…]`."],
                    pub_fn("List", vec![("T", type_())], type_(), list_of(name("T"))),
                ),
                list_ops(syntax),
            ),
        ),
        pub_use("List"),
        pub_mod(
            "Cell",
            with_type(
                documented(
                    &[
                        "A mutable holder of one value.",
                        "",
                        "Reading answers the last value written through any name for the same cell, so two names for one cell are not two cells.",
                    ],
                    pub_fn("Cell", vec![("T", type_())], type_(), cell_of(name("T"))),
                ),
                cell_ops(),
            ),
        ),
        pub_use("Cell"),
        pub_mod(
            "Io",
            with_type(
                documented(
                    &[
                        "A description of something the host does, yielding a `T`.",
                        "",
                        "Holding one performs nothing: a description runs by being the program's tail, so forcing the same one twice does the work twice, and there is no operation taking an `Io(T)` back to a `T`.",
                    ],
                    pub_fn("Io", vec![("T", type_())], type_(), io_of(name("T"))),
                ),
                io_ops(),
            ),
        ),
        pub_use("Io"),
        true_prop(),
        false_prop(),
        holds(),
    ];

    items.extend(host_operations(subjects));

    Module { items }
}
