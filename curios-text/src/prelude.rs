use {
    super::{
        Apply, FuncSugarParam, FuncType, FuncTypeParam, GroupItem, Intrinsic, LetSignature, Module,
        Name, Nat, NatLiteral, Pattern, Subterm, Term, TopForeign, TopItem, TopLet, TopMod, TopUse,
        TupleType, TupleTypeParam, UseGroup,
    },
    curios_abi::{ForeignFunction, ForeignStore, WireType, mode, poll, status, stdio},
    curios_utilities::{Grain, Plicity, SyntaxName, SyntaxRegistry},
    std::sync::Arc,
};

// The `sys` module is the home of every intrinsic type and operation. It is built directly as `text` AST (never parsed) and prepended to every parsed `Entrypoint`, so intrinsics participate in the module system like any other binding. Bodies bake the `text::Intrinsic::*` nodes in directly, so the prelude needs no internal name resolution — with one exception, the propositions an operation states as its precondition, which are `/syn` names this module cannot spell and takes from the registry instead.

fn name(label: &str) -> Term {
    Subterm::Name(Name::from([label.to_string()])).into()
}

// A registered `/syn` name, absolute so it resolves against the compilation root rather than whatever module the generated declaration lands in.
fn registered(target: SyntaxName) -> Term {
    Subterm::Name(Name::new(true, target.qualifier())).into()
}

fn applied(head: Term, args: Vec<Term>) -> Term {
    Subterm::Apply(Apply {
        head,
        params: args
            .into_iter()
            .map(|arg| (Plicity::Explicit, arg))
            .collect(),
    })
    .into()
}

fn intrinsic(p: Intrinsic) -> Term {
    Subterm::Intrinsic(p).into()
}

fn nat() -> Term {
    intrinsic(Intrinsic::NatType)
}

fn byte() -> Term {
    intrinsic(Intrinsic::ByteType)
}

// A `Nat` literal value term, built exactly as the parser builds one: `0` is bare `Zero`, anything else is `Succ(n, Zero)`. Used to bake host-owned wire codes (`status`, `poll`, and `mode`) into the `/sys/Handle` constant mirror.
fn nat_lit(n: u32) -> Term {
    match n {
        0 => intrinsic(Intrinsic::Nat(Nat::Zero)),
        n => intrinsic(Intrinsic::Nat(Nat::Succ(
            NatLiteral::number(n),
            intrinsic(Intrinsic::Nat(Nat::Zero)),
        ))),
    }
}

/// `left + right` as a `/sys` term. The window bound is the one precondition stated over arithmetic rather than over an operand, so it is the one that has to build a sum.
fn nat_plus(left: Term, right: Term) -> Term {
    intrinsic(Intrinsic::NatAdd(left, right))
}

fn int() -> Term {
    intrinsic(Intrinsic::IntType)
}

fn flt() -> Term {
    intrinsic(Intrinsic::FltType)
}

fn bin(grain: Grain) -> Term {
    intrinsic(Intrinsic::BinType(grain))
}

fn bool_() -> Term {
    intrinsic(Intrinsic::BoolType)
}

fn handle() -> Term {
    intrinsic(Intrinsic::HandleType)
}

fn unit() -> Term {
    Subterm::TupleType(TupleType { fields: vec![] }).into()
}

fn record(fields: Vec<(&str, Term)>) -> Term {
    Subterm::TupleType(TupleType {
        fields: fields
            .into_iter()
            .map(|(label, type_)| TupleTypeParam {
                label: Some(label.to_string()),
                func_params: None,
                type_,
            })
            .collect(),
    })
    .into()
}

fn list_of(elem: Term) -> Term {
    intrinsic(Intrinsic::ListType(elem))
}

// A single-argument function type `(domain) -> output`, for higher-order intrinsics (the `f` of `List/map`).
fn fn_of(domain: Term, output: Term) -> Term {
    Subterm::FuncType(FuncType {
        params: vec![FuncTypeParam {
            plicity: Plicity::Explicit,
            label: None,
            type_: domain,
        }],
        output,
    })
    .into()
}

fn cell_of(elem: Term) -> Term {
    intrinsic(Intrinsic::CellType(elem))
}

fn io_of(result: Term) -> Term {
    intrinsic(Intrinsic::IoType(result))
}

fn type_() -> Term {
    Subterm::Type.into()
}

fn pub_let(label: &str, type_: Term, body: Term) -> TopItem {
    TopItem::Let(TopLet {
        vis_pub: true,
        label: label.to_string(),
        signature: LetSignature::Name {
            type_: Some(type_),
            body,
        },
    })
}

fn pub_mod(label: &str, items: Vec<TopItem>) -> TopItem {
    TopItem::Mod(TopMod {
        span: None,
        vis_pub: true,
        label: label.to_string(),
        module: Some(Module { items }),
    })
}

// `pub use Label/{let Label}` — the facade re-export that hoists a submodule's own type binding up to the library root, so `/sys/{Label}` names the type.
fn pub_use(label: &str) -> TopItem {
    TopItem::Use(TopUse {
        vis_pub: true,
        name: Name::from([label.to_string()]),
        group: UseGroup::Named(vec![GroupItem::Let(label.to_string())]),
    })
}

// An intrinsic module's items: its type declaration first, then its operations, so the type lives *inside* its module and the root facade re-exports it.
fn with_type(type_decl: TopItem, mut ops: Vec<TopItem>) -> Vec<TopItem> {
    let mut items = vec![type_decl];
    items.append(&mut ops);
    items
}

fn pub_fn(label: &str, params: Vec<(&str, Term)>, output: Term, body: Term) -> TopItem {
    pub_fn_marked(
        label,
        params
            .into_iter()
            .map(|(n, t)| (Plicity::Explicit, n, t))
            .collect(),
        output,
        body,
    )
}

fn pub_fn_marked(
    label: &str,
    params: Vec<(Plicity, &str, Term)>,
    output: Term,
    body: Term,
) -> TopItem {
    TopItem::Let(fn_marked(true, label, params, output, body))
}

fn fn_marked(
    vis_pub: bool,
    label: &str,
    params: Vec<(Plicity, &str, Term)>,
    output: Term,
    body: Term,
) -> TopLet {
    TopLet {
        vis_pub,
        label: label.to_string(),
        signature: LetSignature::Func {
            params: params
                .into_iter()
                .map(|(p, n, t)| FuncSugarParam {
                    plicity: p,
                    label: Pattern::Binder(Some(n.to_string())),
                    type_: t,
                })
                .collect(),
            output,
            body,
        },
    }
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
    let signature = &function.signature;

    let result = match signature.results.as_slice() {
        [] => unit(),
        [(_, result)] => wire_type(result),
        results => record(
            results
                .iter()
                .map(|(label, result)| (label.as_str(), wire_type(result)))
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
            vis_pub,
            label: function.label.clone(),
            signature: LetSignature::Name {
                type_: Some(output),
                body,
            },
        };
    }

    fn_marked(
        vis_pub,
        &function.label,
        signature
            .params
            .iter()
            .map(|(param, type_)| (Plicity::Explicit, param.as_str(), wire_type(type_)))
            .collect(),
        output,
        body,
    )
}

/// Handle one user-written `foreign` declaration: register its [`ForeignFunction`] into the compilation's (non-`host_ops`) foreign store, and return the ordinary [`LetSignature`] `into_core` lowers it as — wire-type bookkeeping and `host_fn`'s shape stay internal to this module, so `into_core` only ever deals with the same `LetSignature` it already knows how to lower for a plain `TopItem::Let`. `name` is the declaration's fully qualified name (leading `/`, the caller's current position while walking the module tree), which becomes the wasm import string under the `ffi` namespace. Qualified names are unique per compilation (a same-scope duplicate is a binding conflict long before lowering reaches this point), so `register`'s duplicate panic stays what it is everywhere else: a construction bug.
pub(crate) fn foreign_signature(
    declaration: &TopForeign,
    foreigns: &mut ForeignStore,
    name: String,
) -> LetSignature {
    let function = ForeignFunction {
        namespace: "ffi",
        name,
        subject: None,
        label: declaration.label.clone(),
        signature: declaration.signature.clone(),
    };

    foreigns.register(function.clone());

    host_fn(&Arc::new(function), declaration.vis_pub).signature
}

fn binary(label: &str, operand: Term, output: Term, ctor: fn(Term, Term) -> Intrinsic) -> TopItem {
    pub_fn(
        label,
        vec![("a", operand.clone()), ("b", operand)],
        output,
        intrinsic(ctor(name("a"), name("b"))),
    )
}

// A binary operation whose second operand carries a precondition — the divisions, whose fold reports rather than answers on a zero divisor. The bound is stated the way source states it, for the reason `bin_ops` gives: a refinement is keyed on the term written, and a caller can only write the operand.
fn guarded_binary(
    label: &str,
    operand: Term,
    output: Term,
    bound: Term,
    ctor: fn(Term, Term, Term) -> Intrinsic,
) -> TopItem {
    pub_fn_marked(
        label,
        vec![
            (Plicity::Explicit, "a", operand.clone()),
            (Plicity::Explicit, "b", operand),
            (Plicity::Implicit, "ok", bound),
        ],
        output,
        // The proof parameter is named in the body, which is what carries the bound into Core: an implicit nothing referenced would be checked here and forgotten, leaving the kernel nothing to re-verify.
        intrinsic(ctor(name("a"), name("b"), name("ok"))),
    )
}

fn guarded_unary(
    label: &str,
    input: Term,
    output: Term,
    bound: Term,
    ctor: fn(Term, Term) -> Intrinsic,
) -> TopItem {
    pub_fn_marked(
        label,
        vec![
            (Plicity::Explicit, "a", input),
            (Plicity::Implicit, "ok", bound),
        ],
        output,
        intrinsic(ctor(name("a"), name("ok"))),
    )
}

fn unary(label: &str, input: Term, output: Term, ctor: fn(Term) -> Intrinsic) -> TopItem {
    pub_fn(
        label,
        vec![("a", input)],
        output,
        intrinsic(ctor(name("a"))),
    )
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
    applied(
        registered(syntax.proof.lt),
        vec![intrinsic(Intrinsic::Nat(Nat::Zero)), name("b")],
    )
}

fn int_nonzero(syntax: &SyntaxRegistry) -> Term {
    applied(registered(syntax.proof.int_non_zero), vec![name("b")])
}

fn nat_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
    vec![
        nat_succ(),
        binary("eql", nat(), bool_(), Intrinsic::NatEql),
        binary("neq", nat(), bool_(), Intrinsic::NatNeq),
        binary("add", nat(), nat(), Intrinsic::NatAdd),
        binary("sub", nat(), nat(), Intrinsic::NatSub),
        binary("mul", nat(), nat(), Intrinsic::NatMul),
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
        binary("lt", nat(), bool_(), Intrinsic::NatLt),
        binary("gt", nat(), bool_(), Intrinsic::NatGt),
        binary("le", nat(), bool_(), Intrinsic::NatLe),
        binary("ge", nat(), bool_(), Intrinsic::NatGe),
        binary("and", nat(), nat(), Intrinsic::NatAnd),
        binary("or", nat(), nat(), Intrinsic::NatOr),
        binary("xor", nat(), nat(), Intrinsic::NatXor),
        binary("shl", nat(), nat(), Intrinsic::NatShl),
        binary("shr", nat(), nat(), Intrinsic::NatShr),
        binary("rotl", nat(), nat(), Intrinsic::NatRotl),
        binary("rotr", nat(), nat(), Intrinsic::NatRotr),
        unary("clz", nat(), nat(), Intrinsic::NatClz),
        unary("ctz", nat(), nat(), Intrinsic::NatCtz),
        unary("popcnt", nat(), nat(), Intrinsic::NatPopcnt),
        unary("to_int", nat(), int(), Intrinsic::NatToInt),
        unary("to_flt", nat(), flt(), Intrinsic::NatToFlt),
        unary("to_byte", nat(), byte(), Intrinsic::NatToByte),
    ]
}

fn byte_ops() -> Vec<TopItem> {
    vec![
        unary("to_nat", byte(), nat(), Intrinsic::ByteToNat),
        binary("eql", byte(), bool_(), Intrinsic::ByteEql),
        binary("lt", byte(), bool_(), Intrinsic::ByteLt),
        binary("le", byte(), bool_(), Intrinsic::ByteLe),
        binary("gt", byte(), bool_(), Intrinsic::ByteGt),
        binary("ge", byte(), bool_(), Intrinsic::ByteGe),
    ]
}

// `Bool` rides the same i31ref/u32 carrier as `Nat`, with `false`/`true` as `0`/`1`. `and`/`or`/`xor` are bitwise machine ops on those bits — exact boolean logic — and `eql` is the `Nat` equality op (`i32.eq`) on that single bit, so all four are intrinsics rather than `match` definitions. `not` has no machine instruction; `/std/Bool` defines it as `xor(b, true)`.
fn bool_ops() -> Vec<TopItem> {
    vec![
        binary("and", bool_(), bool_(), Intrinsic::BoolAnd),
        binary("or", bool_(), bool_(), Intrinsic::BoolOr),
        binary("xor", bool_(), bool_(), Intrinsic::BoolXor),
        binary("eql", bool_(), bool_(), Intrinsic::BoolEql),
    ]
}

fn int_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
    vec![
        binary("eql", int(), bool_(), Intrinsic::IntEql),
        binary("neq", int(), bool_(), Intrinsic::IntNeq),
        binary("add", int(), int(), Intrinsic::IntAdd),
        binary("sub", int(), int(), Intrinsic::IntSub),
        binary("mul", int(), int(), Intrinsic::IntMul),
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
        binary("lt", int(), bool_(), Intrinsic::IntLt),
        binary("gt", int(), bool_(), Intrinsic::IntGt),
        binary("le", int(), bool_(), Intrinsic::IntLe),
        binary("ge", int(), bool_(), Intrinsic::IntGe),
        // Bitwise ops on the signed i31 carrier. `and`/`or`/`xor` are exact bit ops; `shl` truncates into the carrier like `Nat/shl`; `shr` is arithmetic (sign-preserving). `not` is `/std/Int`'s `xor(x, -1)`.
        binary("and", int(), int(), Intrinsic::IntAnd),
        binary("or", int(), int(), Intrinsic::IntOr),
        binary("xor", int(), int(), Intrinsic::IntXor),
        binary("shl", int(), int(), Intrinsic::IntShl),
        binary("shr", int(), int(), Intrinsic::IntShr),
        binary("rotl", int(), int(), Intrinsic::IntRotl),
        binary("rotr", int(), int(), Intrinsic::IntRotr),
        unary("clz", int(), int(), Intrinsic::IntClz),
        unary("ctz", int(), int(), Intrinsic::IntCtz),
        unary("popcnt", int(), int(), Intrinsic::IntPopcnt),
        pub_fn_marked(
            "to_nat",
            vec![
                (Plicity::Explicit, "a", int()),
                (
                    Plicity::Implicit,
                    "ok",
                    applied(registered(syntax.proof.int_non_neg), vec![name("a")]),
                ),
            ],
            nat(),
            intrinsic(Intrinsic::IntToNat(name("a"))),
        ),
        unary("to_flt", int(), flt(), Intrinsic::IntToFlt),
    ]
}

fn flt_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
    vec![
        binary("add", flt(), flt(), Intrinsic::FltAdd),
        binary("sub", flt(), flt(), Intrinsic::FltSub),
        binary("mul", flt(), flt(), Intrinsic::FltMul),
        binary("div", flt(), flt(), Intrinsic::FltDiv),
        binary("rem", flt(), flt(), Intrinsic::FltRem),
        binary("min", flt(), flt(), Intrinsic::FltMin),
        binary("max", flt(), flt(), Intrinsic::FltMax),
        binary("eql", flt(), bool_(), Intrinsic::FltEql),
        binary("neq", flt(), bool_(), Intrinsic::FltNeq),
        binary("lt", flt(), bool_(), Intrinsic::FltLt),
        binary("gt", flt(), bool_(), Intrinsic::FltGt),
        binary("le", flt(), bool_(), Intrinsic::FltLe),
        binary("ge", flt(), bool_(), Intrinsic::FltGe),
        unary("neg", flt(), flt(), Intrinsic::FltNeg),
        unary("abs", flt(), flt(), Intrinsic::FltAbs),
        unary("sqrt", flt(), flt(), Intrinsic::FltSqrt),
        unary("floor", flt(), flt(), Intrinsic::FltFloor),
        unary("ceil", flt(), flt(), Intrinsic::FltCeil),
        unary("trunc", flt(), flt(), Intrinsic::FltTrunc),
        unary("nearest", flt(), flt(), Intrinsic::FltNearest),
        binary("copysign", flt(), flt(), Intrinsic::FltCopysign),
        unary("to_nat", flt(), nat(), Intrinsic::FltToNat),
        unary("to_int", flt(), int(), Intrinsic::FltToInt),
        unary("to_le_bytes", flt(), bin(Grain::X), Intrinsic::FltToLeBytes),
        guarded_unary(
            "of_le_bytes",
            bin(Grain::X),
            flt(),
            applied(registered(syntax.proof.bytes_four), vec![name("a")]),
            |bin, four_bytes| Intrinsic::FltOfLeBytes { bin, four_bytes },
        ),
    ]
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
    let in_range = applied(
        registered(syntax.proof.lt),
        vec![name("i"), applied(name("len"), vec![name("b")])],
    );
    vec![
        pub_fn(
            "len",
            vec![("b", type_.clone())],
            nat(),
            intrinsic(Intrinsic::BinLen(grain, name("b"))),
        ),
        pub_fn(
            "eql",
            vec![("a", type_.clone()), ("b", type_.clone())],
            bool_(),
            intrinsic(Intrinsic::BinEql(grain, name("a"), name("b"))),
        ),
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
        // A window is a start and a *count*, so a reversed one cannot be spelled and the ordering half of the old bound has no proposition left to state. What survives is that the window ends inside the value.
        pub_fn_marked(
            "slice",
            vec![
                (Plicity::Explicit, "b", type_.clone()),
                (Plicity::Explicit, "s", nat()),
                (Plicity::Explicit, "l", nat()),
                (
                    Plicity::Implicit,
                    "within",
                    applied(
                        registered(syntax.proof.le),
                        vec![
                            nat_plus(name("s"), name("l")),
                            applied(name("len"), vec![name("b")]),
                        ],
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
    ]
}

fn list_ops(syntax: &SyntaxRegistry) -> Vec<TopItem> {
    vec![
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
        pub_fn_marked(
            "get",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", list_of(name("T"))),
                (Plicity::Explicit, "i", nat()),
                (
                    Plicity::Implicit,
                    "ok",
                    applied(
                        registered(syntax.proof.lt),
                        vec![name("i"), applied(name("len"), vec![name("a")])],
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
        // The `List` twin of `Bin/slice`'s window bound; see the comment there for why only one half survives the count.
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
                    applied(
                        registered(syntax.proof.le),
                        vec![
                            nat_plus(name("s"), name("l")),
                            applied(name("len"), vec![name("a")]),
                        ],
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
    ]
}

// Allocating a cell, reading one, and writing one are all host effects, so all three return descriptions. `Cell/get` is the operation the whole discipline was named for: a scrutinee spelled `Cell/get(c)` denotes a different value before and after a `Cell/set`, and giving it an `Io` result is what makes that spelling ill-typed in scrutinee position rather than something an analysis has to notice.
fn cell_ops() -> Vec<TopItem> {
    vec![
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
    ]
}

// The monad of the `/sys/Io` type, and nothing else: `Io` owns the sequencing, never the operations. An operation belongs with its subject — the one its own store row names, not the type its result wears.
fn io_ops() -> Vec<TopItem> {
    vec![
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
    ]
}

// The values and operations of the `/sys/Handle` type: the three standard streams, handle identity, and `host` — the store rows whose subject is the handle itself (`read`, `write`, `poll`, `close`), which join their type module rather than open one of their own.
fn handle_ops(mut host: Vec<TopItem>) -> Vec<TopItem> {
    let mut items = vec![
        pub_let(
            "stdin",
            handle(),
            intrinsic(Intrinsic::Handle(stdio::STDIN)),
        ),
        pub_let(
            "stdout",
            handle(),
            intrinsic(Intrinsic::Handle(stdio::STDOUT)),
        ),
        pub_let(
            "stderr",
            handle(),
            intrinsic(Intrinsic::Handle(stdio::STDERR)),
        ),
        pub_fn(
            "eql",
            vec![("a", handle()), ("b", handle())],
            bool_(),
            intrinsic(Intrinsic::HandleEql(name("a"), name("b"))),
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
        let item = TopItem::Let(host_fn(function, true));

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

// The remaining host-operation subjects, `exit`, and the wire-code mirror: one `/sys` module per subject, then the `status`/`poll`/`mode` code modules. Every one of these names is lowercase because no type backs it — a capitalized `/sys` module is one whose type the root facade re-exports, and the code modules are `Nat` constants. The mirror was capitalized only so the `poll` op and the `Poll` module could coexist; nesting moved the op to `/sys/Handle/poll` and retired that reason.
fn host_operations(subjects: Vec<(String, Vec<TopItem>)>) -> Vec<TopItem> {
    let mut items = subjects
        .into_iter()
        .map(|(subject, mut ops)| {
            // `exit` is `Intrinsic::ProcExit` rather than a store row — it traps instead of returning, so no `WireSignature` describes it — but it is a process operation like `args` and `env`, so it is placed by hand in the module its subject already opened. `(n : Nat) -> Io({})`: the result inside the wrapper is unit rather than the caller's choice, because a non-returning term is unsound exactly when it inhabits a type nothing total inhabits — and `{}` is inhabited by `()`, so there is nothing to forge. An `Io`-polymorphic bottom is arguably sound now that no eliminator can extract the result, but it reopens a settled decision for no consumer.
            if subject == "proc" {
                ops.push(pub_fn_marked(
                    "exit",
                    vec![(Plicity::Explicit, "n", nat())],
                    io_of(unit()),
                    intrinsic(Intrinsic::ProcExit(name("n"))),
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
                pub_let("other_base", nat(), nat_lit(status::OTHER_BASE)),
            ],
        ),
        pub_mod(
            "poll",
            vec![
                pub_let("read", nat(), nat_lit(poll::READ)),
                pub_let("write", nat(), nat_lit(poll::WRITE)),
                pub_let("err", nat(), nat_lit(poll::ERR)),
                pub_let("hup", nat(), nat_lit(poll::HUP)),
            ],
        ),
        pub_mod(
            "mode",
            vec![
                pub_let("read", nat(), nat_lit(mode::READ)),
                pub_let("write", nat(), nat_lit(mode::WRITE)),
                pub_let("append", nat(), nat_lit(mode::APPEND)),
            ],
        ),
    ]);

    items
}

/// Construct the generated `/sys` surface module from the authoritative host function store. Each type module (`Nat`, …, `Handle`, `List`, `Cell`, `Io`) holds its type and operations and hoists the type to the `/sys` root; each host-operation subject the store names becomes a module of its own (`file`, `socket`, `dns`, …) except `Handle`'s, which join their type module; then the `status`/`poll`/`mode` code modules. Exposed for the build-time prelude artifact builder; production compilation never lowers it at runtime.
pub fn sys_module(foreigns: &ForeignStore, syntax: &SyntaxRegistry) -> Module {
    let mut subjects = host_subjects(foreigns);
    let handle_host = take_subject(&mut subjects, "Handle");

    let mut items = vec![
        pub_mod(
            "Nat",
            with_type(pub_let("Nat", type_(), nat()), nat_ops(syntax)),
        ),
        pub_use("Nat"),
        pub_mod(
            "Byte",
            with_type(pub_let("Byte", type_(), byte()), byte_ops()),
        ),
        pub_use("Byte"),
        pub_mod(
            "Int",
            with_type(pub_let("Int", type_(), int()), int_ops(syntax)),
        ),
        pub_use("Int"),
        pub_mod(
            "Flt",
            with_type(pub_let("Flt", type_(), flt()), flt_ops(syntax)),
        ),
        pub_use("Flt"),
        pub_mod(
            "Bits",
            with_type(
                pub_let("Bits", type_(), bin(Grain::B)),
                bin_ops(Grain::B, syntax),
            ),
        ),
        pub_mod(
            "Bytes",
            with_type(
                pub_let("Bytes", type_(), bin(Grain::X)),
                bin_ops(Grain::X, syntax),
            ),
        ),
        pub_mod(
            "Bool",
            with_type(pub_let("Bool", type_(), bool_()), bool_ops()),
        ),
        pub_use("Bool"),
        pub_mod(
            "Handle",
            with_type(
                pub_let("Handle", type_(), handle()),
                handle_ops(handle_host),
            ),
        ),
        pub_use("Handle"),
        pub_mod(
            "List",
            with_type(
                pub_fn("List", vec![("T", type_())], type_(), list_of(name("T"))),
                list_ops(syntax),
            ),
        ),
        pub_use("List"),
        pub_mod(
            "Cell",
            with_type(
                pub_fn("Cell", vec![("T", type_())], type_(), cell_of(name("T"))),
                cell_ops(),
            ),
        ),
        pub_use("Cell"),
        pub_mod(
            "Io",
            with_type(
                pub_fn("Io", vec![("T", type_())], type_(), io_of(name("T"))),
                io_ops(),
            ),
        ),
        pub_use("Io"),
    ];

    items.extend(host_operations(subjects));

    Module { items }
}
