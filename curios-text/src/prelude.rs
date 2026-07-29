use {
    super::{
        FuncSugarParam, FuncType, FuncTypeParam, GroupItem, LetSignature, Module, Name, Nat,
        NatLiteral, Pattern, Prim, Subterm, Term, TopForeign, TopItem, TopLet, TopMod, TopUse,
        TupleType, TupleTypeParam, UseGroup,
    },
    curios_abi::{ForeignFunction, ForeignStore, WireType, mode, poll, status, stdio},
    curios_base::{Grain, Plicity},
    std::sync::Arc,
};

// The `sys` module is the home of every primitive type and operation. It is
// built directly as `text` AST (never parsed) and prepended to every parsed
// `Entrypoint`, so primitives participate in the module system like any other
// binding. Bodies bake the `text::Prim::*` nodes in directly, so the prelude
// needs no internal name resolution.

fn name(label: &str) -> Term {
    Subterm::Name(Name::from([label.to_string()])).into()
}

fn prim(p: Prim) -> Term {
    Subterm::Prim(p).into()
}

fn nat() -> Term {
    prim(Prim::NatType)
}

fn byte() -> Term {
    prim(Prim::ByteType)
}

// A `Nat` literal value term, built exactly as the parser builds one: `0` is
// bare `Zero`, anything else is `Succ(n, Zero)`. Used to bake host-owned wire
// codes (`status`, `poll`, and `mode`) into the `/sys/Handle` constant mirror.
fn nat_lit(n: u32) -> Term {
    match n {
        0 => prim(Prim::Nat(Nat::Zero)),
        n => prim(Prim::Nat(Nat::Succ(
            NatLiteral::number(n),
            prim(Prim::Nat(Nat::Zero)),
        ))),
    }
}

fn int() -> Term {
    prim(Prim::IntType)
}

fn flt() -> Term {
    prim(Prim::FltType)
}

fn bin(grain: Grain) -> Term {
    prim(Prim::BinType(grain))
}

fn bool_() -> Term {
    prim(Prim::BoolType)
}

fn handle() -> Term {
    prim(Prim::HandleType)
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

fn lst_of(elem: Term) -> Term {
    prim(Prim::LstType(elem))
}

// A single-argument function type `(domain) -> output`, for higher-order
// primitives (the `f` of `Lst/map`).
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
    prim(Prim::CellType(elem))
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

// `pub use Label/{let Label}` — the facade re-export that hoists a submodule's
// own type binding up to the library root, so `/sys/{Label}` names the type.
fn pub_use(label: &str) -> TopItem {
    TopItem::Use(TopUse {
        vis_pub: true,
        name: Name::from([label.to_string()]),
        group: UseGroup::Named(vec![GroupItem::Let(label.to_string())]),
    })
}

// A primitive module's items: its type declaration first, then its operations,
// so the type lives *inside* its module and the root facade re-exports it.
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

/// The surface type a host-boundary [`WireType`] denotes — the prelude's
/// reading of the signature, mirrored by `core::wire_term` after lowering.
fn wire_type(type_: &WireType) -> Term {
    match type_ {
        WireType::Nat => nat(),
        WireType::Int => int(),
        WireType::Bool => bool_(),
        WireType::Bin => bin(Grain::X),
        WireType::Handle => handle(),
        WireType::Lst(element) => lst_of(wire_type(element)),
    }
}

/// A host-function declaration generated from a foreign-store row: parameter
/// names/types and the result shape (unit, bare type, named record) come off
/// the `WireSignature`, and the body bakes the generic `Foreign` prim applied
/// to the parameter names. Used both for `/sys/Handle`'s rows (always `pub`) and,
/// via [`foreign_signature`], for a user's own `foreign` declaration (`vis_pub`
/// follows what they wrote).
fn host_fn(function: &Arc<ForeignFunction>, vis_pub: bool) -> TopLet {
    let signature = &function.signature;

    let output = match signature.results.as_slice() {
        [] => unit(),
        [(_, result)] => wire_type(result),
        results => record(
            results
                .iter()
                .map(|(label, result)| (label.as_str(), wire_type(result)))
                .collect(),
        ),
    };

    fn_marked(
        vis_pub,
        &function.label,
        signature
            .params
            .iter()
            .map(|(param, type_)| (Plicity::Explicit, param.as_str(), wire_type(type_)))
            .collect(),
        output,
        prim(Prim::Foreign(
            Arc::clone(function),
            signature
                .params
                .iter()
                .map(|(param, _)| name(param))
                .collect(),
        )),
    )
}

/// Handle one user-written `foreign` declaration: register its
/// [`ForeignFunction`] into the compilation's (non-`host_ops`) foreign store,
/// and return the ordinary [`LetSignature`] `into_core` lowers it as — wire-type
/// bookkeeping and `host_fn`'s shape stay internal to this module, so `into_core`
/// only ever deals with the same `LetSignature` it already knows how to lower
/// for a plain `TopItem::Let`. `name` is the declaration's fully qualified
/// name (leading `/`, the caller's current position while walking the module
/// tree), which becomes the wasm import string under the `ffi` namespace.
/// Qualified names are unique per compilation (a same-scope duplicate is a
/// binding conflict long before lowering reaches this point), so `register`'s
/// duplicate panic stays what it is everywhere else: a construction bug.
pub(crate) fn foreign_signature(
    declaration: &TopForeign,
    foreigns: &mut ForeignStore,
    name: String,
) -> LetSignature {
    let function = ForeignFunction {
        namespace: "ffi",
        name,
        label: declaration.label.clone(),
        signature: declaration.signature.clone(),
    };

    foreigns.register(function.clone());

    host_fn(&Arc::new(function), declaration.vis_pub).signature
}

fn binary(label: &str, operand: Term, output: Term, ctor: fn(Term, Term) -> Prim) -> TopItem {
    pub_fn(
        label,
        vec![("a", operand.clone()), ("b", operand)],
        output,
        prim(ctor(name("a"), name("b"))),
    )
}

fn unary(label: &str, input: Term, output: Term, ctor: fn(Term) -> Prim) -> TopItem {
    pub_fn(label, vec![("a", input)], output, prim(ctor(name("a"))))
}

fn nat_succ() -> TopItem {
    pub_fn(
        "succ",
        vec![("a", nat())],
        nat(),
        prim(Prim::Nat(Nat::Succ(NatLiteral::number(1usize), name("a")))),
    )
}

fn nat_ops() -> Vec<TopItem> {
    vec![
        nat_succ(),
        binary("eql", nat(), bool_(), Prim::NatEql),
        binary("neq", nat(), bool_(), Prim::NatNeq),
        binary("add", nat(), nat(), Prim::NatAdd),
        binary("sub", nat(), nat(), Prim::NatSub),
        binary("mul", nat(), nat(), Prim::NatMul),
        binary("div", nat(), nat(), Prim::NatDiv),
        binary("rem", nat(), nat(), Prim::NatRem),
        binary("lt", nat(), bool_(), Prim::NatLt),
        binary("gt", nat(), bool_(), Prim::NatGt),
        binary("lte", nat(), bool_(), Prim::NatLte),
        binary("gte", nat(), bool_(), Prim::NatGte),
        binary("and", nat(), nat(), Prim::NatAnd),
        binary("or", nat(), nat(), Prim::NatOr),
        binary("xor", nat(), nat(), Prim::NatXor),
        binary("shl", nat(), nat(), Prim::NatShl),
        binary("shr", nat(), nat(), Prim::NatShr),
        binary("rotl", nat(), nat(), Prim::NatRotl),
        binary("rotr", nat(), nat(), Prim::NatRotr),
        unary("clz", nat(), nat(), Prim::NatClz),
        unary("ctz", nat(), nat(), Prim::NatCtz),
        unary("popcnt", nat(), nat(), Prim::NatPopcnt),
        unary("to_int", nat(), int(), Prim::NatToInt),
        unary("to_flt", nat(), flt(), Prim::NatToFlt),
        unary("to_byte", nat(), byte(), Prim::NatToByte),
    ]
}

fn byte_ops() -> Vec<TopItem> {
    vec![
        unary("to_nat", byte(), nat(), Prim::ByteToNat),
        binary("eql", byte(), bool_(), Prim::ByteEql),
        binary("lt", byte(), bool_(), Prim::ByteLt),
        binary("lte", byte(), bool_(), Prim::ByteLte),
        binary("gt", byte(), bool_(), Prim::ByteGt),
        binary("gte", byte(), bool_(), Prim::ByteGte),
    ]
}

// `Bool` rides the same i31ref/u32 carrier as `Nat`, with `false`/`true` as
// `0`/`1`. `and`/`or`/`xor` are bitwise machine ops on those bits — exact
// boolean logic — and `eql` is the `Nat` equality op (`i32.eq`) on that single
// bit, so all four are primitives rather than `match` definitions. `not` has no
// machine instruction; `/std/Bool` defines it as `xor(b, true)`.
fn bool_ops() -> Vec<TopItem> {
    vec![
        binary("and", bool_(), bool_(), Prim::BoolAnd),
        binary("or", bool_(), bool_(), Prim::BoolOr),
        binary("xor", bool_(), bool_(), Prim::BoolXor),
        binary("eql", bool_(), bool_(), Prim::BoolEql),
    ]
}

fn int_ops() -> Vec<TopItem> {
    vec![
        binary("eql", int(), bool_(), Prim::IntEql),
        binary("neq", int(), bool_(), Prim::IntNeq),
        binary("add", int(), int(), Prim::IntAdd),
        binary("sub", int(), int(), Prim::IntSub),
        binary("mul", int(), int(), Prim::IntMul),
        binary("div", int(), int(), Prim::IntDiv),
        binary("rem", int(), int(), Prim::IntRem),
        binary("lt", int(), bool_(), Prim::IntLt),
        binary("gt", int(), bool_(), Prim::IntGt),
        binary("lte", int(), bool_(), Prim::IntLte),
        binary("gte", int(), bool_(), Prim::IntGte),
        // Bitwise ops on the signed i31 carrier. `and`/`or`/`xor` are exact bit
        // ops; `shl` truncates into the carrier like `Nat/shl`; `shr` is
        // arithmetic (sign-preserving). `not` is `/std/Int`'s `xor(x, -1)`.
        binary("and", int(), int(), Prim::IntAnd),
        binary("or", int(), int(), Prim::IntOr),
        binary("xor", int(), int(), Prim::IntXor),
        binary("shl", int(), int(), Prim::IntShl),
        binary("shr", int(), int(), Prim::IntShr),
        binary("rotl", int(), int(), Prim::IntRotl),
        binary("rotr", int(), int(), Prim::IntRotr),
        unary("clz", int(), int(), Prim::IntClz),
        unary("ctz", int(), int(), Prim::IntCtz),
        unary("popcnt", int(), int(), Prim::IntPopcnt),
        unary("to_nat", int(), nat(), Prim::IntToNat),
        unary("to_flt", int(), flt(), Prim::IntToFlt),
    ]
}

fn flt_ops() -> Vec<TopItem> {
    vec![
        binary("add", flt(), flt(), Prim::FltAdd),
        binary("sub", flt(), flt(), Prim::FltSub),
        binary("mul", flt(), flt(), Prim::FltMul),
        binary("div", flt(), flt(), Prim::FltDiv),
        binary("rem", flt(), flt(), Prim::FltRem),
        binary("min", flt(), flt(), Prim::FltMin),
        binary("max", flt(), flt(), Prim::FltMax),
        binary("eql", flt(), bool_(), Prim::FltEql),
        binary("neq", flt(), bool_(), Prim::FltNeq),
        binary("lt", flt(), bool_(), Prim::FltLt),
        binary("gt", flt(), bool_(), Prim::FltGt),
        binary("lte", flt(), bool_(), Prim::FltLte),
        binary("gte", flt(), bool_(), Prim::FltGte),
        unary("neg", flt(), flt(), Prim::FltNeg),
        unary("abs", flt(), flt(), Prim::FltAbs),
        unary("sqrt", flt(), flt(), Prim::FltSqrt),
        unary("floor", flt(), flt(), Prim::FltFloor),
        unary("ceil", flt(), flt(), Prim::FltCeil),
        unary("trunc", flt(), flt(), Prim::FltTrunc),
        unary("nearest", flt(), flt(), Prim::FltNearest),
        binary("copysign", flt(), flt(), Prim::FltCopysign),
        unary("to_nat", flt(), nat(), Prim::FltToNat),
        unary("to_int", flt(), int(), Prim::FltToInt),
        unary("to_le_bytes", flt(), bin(Grain::X), Prim::FltToLeBytes),
        unary("of_le_bytes", bin(Grain::X), flt(), Prim::FltOfLeBytes),
    ]
}

fn bin_ops(grain: Grain) -> Vec<TopItem> {
    let type_ = bin(grain);
    let atom = match grain {
        Grain::B => bool_(),
        Grain::X => byte(),
    };
    vec![
        pub_fn(
            "len",
            vec![("b", type_.clone())],
            nat(),
            prim(Prim::BinLen(grain, name("b"))),
        ),
        pub_fn(
            "eql",
            vec![("a", type_.clone()), ("b", type_.clone())],
            bool_(),
            prim(Prim::BinEql(grain, name("a"), name("b"))),
        ),
        pub_fn(
            "get",
            vec![("b", type_.clone()), ("i", nat())],
            atom.clone(),
            prim(Prim::BinGet(grain, name("b"), name("i"))),
        ),
        pub_fn(
            "slice",
            vec![("b", type_.clone()), ("s", nat()), ("e", nat())],
            type_.clone(),
            prim(Prim::BinSlice(grain, name("b"), name("s"), name("e"))),
        ),
        pub_fn(
            "append",
            vec![("b", type_.clone()), ("x", atom)],
            type_.clone(),
            prim(Prim::BinAppend(grain, name("b"), name("x"))),
        ),
        pub_fn(
            "concat",
            vec![("a", type_.clone()), ("b", type_.clone())],
            type_,
            prim(Prim::BinConcat(grain, name("a"), name("b"))),
        ),
    ]
}

fn lst_ops() -> Vec<TopItem> {
    vec![
        pub_fn_marked(
            "len",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", lst_of(name("T"))),
            ],
            nat(),
            prim(Prim::LstLen(name("T"), name("a"))),
        ),
        pub_fn_marked(
            "get",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", lst_of(name("T"))),
                (Plicity::Explicit, "i", nat()),
            ],
            name("T"),
            prim(Prim::LstGet(name("T"), name("a"), name("i"))),
        ),
        pub_fn_marked(
            "slice",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", lst_of(name("T"))),
                (Plicity::Explicit, "s", nat()),
                (Plicity::Explicit, "e", nat()),
            ],
            lst_of(name("T")),
            prim(Prim::LstSlice(name("T"), name("a"), name("s"), name("e"))),
        ),
        pub_fn_marked(
            "append",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", lst_of(name("T"))),
                (Plicity::Explicit, "x", name("T")),
            ],
            lst_of(name("T")),
            prim(Prim::LstAppend(name("T"), name("a"), name("x"))),
        ),
        pub_fn_marked(
            "concat",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", lst_of(name("T"))),
                (Plicity::Explicit, "b", lst_of(name("T"))),
            ],
            lst_of(name("T")),
            prim(Prim::LstConcat(name("T"), name("a"), name("b"))),
        ),
        pub_fn_marked(
            "map",
            vec![
                (Plicity::Implicit, "A", type_()),
                (Plicity::Implicit, "B", type_()),
                (Plicity::Explicit, "a", lst_of(name("A"))),
                (Plicity::Explicit, "f", fn_of(name("A"), name("B"))),
            ],
            lst_of(name("B")),
            prim(Prim::LstMap(name("A"), name("B"), name("a"), name("f"))),
        ),
    ]
}

fn cell_ops() -> Vec<TopItem> {
    vec![
        pub_fn_marked(
            "new",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "x", name("T")),
            ],
            cell_of(name("T")),
            prim(Prim::Cell(name("T"), name("x"))),
        ),
        pub_fn_marked(
            "set",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "c", cell_of(name("T"))),
                (Plicity::Explicit, "v", name("T")),
            ],
            unit(),
            prim(Prim::CellSet(name("T"), name("c"), name("v"))),
        ),
        pub_fn_marked(
            "get",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "c", cell_of(name("T"))),
            ],
            name("T"),
            prim(Prim::CellGet(name("T"), name("c"))),
        ),
    ]
}

// The values and operations of the `/sys/Handle` type: the three standard
// streams and handle identity. The host operations that mint and consume
// handles live flat at the `/sys` root (see `host_operations`), not here.
fn handle_ops() -> Vec<TopItem> {
    vec![
        pub_let("stdin", handle(), prim(Prim::Handle(stdio::STDIN))),
        pub_let("stdout", handle(), prim(Prim::Handle(stdio::STDOUT))),
        pub_let("stderr", handle(), prim(Prim::Handle(stdio::STDERR))),
        pub_fn(
            "eql",
            vec![("a", handle()), ("b", handle())],
            bool_(),
            prim(Prim::HandleEql(name("a"), name("b"))),
        ),
    ]
}

// The host operations, `exit`, and the wire-code mirror, all emitted flat at
// the `/sys` root: every store-described op (in declaration order), then
// `exit`, then the `Status`/`Poll`/`Mode` code modules. Operations are
// lowercase and the code modules capitalized, so the `poll` op and the `Poll`
// module coexist without clashing.
fn host_operations(foreigns: &ForeignStore) -> Vec<TopItem> {
    // Every store-described host op, in store (= declaration) order. Each is a
    // *function*, including the 0-arity clocks/args: a value binding would
    // force-reduce its effectful prim body at definition (the bare prelude is
    // lowered whole, so a top-level value `let` lands in `main`) and trip the
    // host-effect-at-type-level guard, while under the function abstraction the
    // prim stays unevaluated until called.
    let mut ops = foreigns
        .iter()
        .map(|function| TopItem::Let(host_fn(function, true)))
        .collect::<Vec<_>>();

    ops.extend([
        // `(n : Nat) -> {}`: exit ends the process. The result is unit rather
        // than the caller's choice, because a non-returning term is unsound
        // exactly when it inhabits a type nothing total inhabits — and `{}` is
        // inhabited by `()`, so there is nothing to forge.
        pub_fn_marked(
            "exit",
            vec![(Plicity::Explicit, "n", nat())],
            unit(),
            prim(Prim::Exit(name("n"))),
        ),
        // The wire-code mirror: the guest counterpart of ABI wire codes, so the
        // standard library compares against named constants the host derives
        // from the same source.
        pub_mod(
            "Status",
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
            ],
        ),
        pub_mod(
            "Poll",
            vec![
                pub_let("read", nat(), nat_lit(poll::READ)),
                pub_let("write", nat(), nat_lit(poll::WRITE)),
                pub_let("err", nat(), nat_lit(poll::ERR)),
                pub_let("hup", nat(), nat_lit(poll::HUP)),
            ],
        ),
        pub_mod(
            "Mode",
            vec![
                pub_let("read", nat(), nat_lit(mode::READ)),
                pub_let("write", nat(), nat_lit(mode::WRITE)),
                pub_let("append", nat(), nat_lit(mode::APPEND)),
            ],
        ),
    ]);

    ops
}

/// Construct the generated `/sys` surface module from the authoritative host
/// function store. Each type module (`Nat`, …, `Handle`, `Lst`, `Cell`) holds
/// its type and operations and hoists the type to the `/sys` root; the host
/// operations, `exit`, and the `Status`/`Poll`/`Mode` code modules sit flat at
/// the root. Exposed for the build-time prelude artifact builder; production
/// compilation never lowers it at runtime.
pub fn sys_module(foreigns: &ForeignStore) -> Module {
    let mut items = vec![
        pub_mod("Nat", with_type(pub_let("Nat", type_(), nat()), nat_ops())),
        pub_use("Nat"),
        pub_mod(
            "Byte",
            with_type(pub_let("Byte", type_(), byte()), byte_ops()),
        ),
        pub_use("Byte"),
        pub_mod("Int", with_type(pub_let("Int", type_(), int()), int_ops())),
        pub_use("Int"),
        pub_mod("Flt", with_type(pub_let("Flt", type_(), flt()), flt_ops())),
        pub_use("Flt"),
        pub_mod(
            "Bits",
            with_type(pub_let("Bits", type_(), bin(Grain::B)), bin_ops(Grain::B)),
        ),
        pub_mod(
            "Bytes",
            with_type(pub_let("Bytes", type_(), bin(Grain::X)), bin_ops(Grain::X)),
        ),
        pub_mod(
            "Bool",
            with_type(pub_let("Bool", type_(), bool_()), bool_ops()),
        ),
        pub_use("Bool"),
        pub_mod(
            "Handle",
            with_type(pub_let("Handle", type_(), handle()), handle_ops()),
        ),
        pub_use("Handle"),
        pub_mod(
            "Lst",
            with_type(
                pub_fn("Lst", vec![("T", type_())], type_(), lst_of(name("T"))),
                lst_ops(),
            ),
        ),
        pub_use("Lst"),
        pub_mod(
            "Cell",
            with_type(
                pub_fn("Cell", vec![("T", type_())], type_(), cell_of(name("T"))),
                cell_ops(),
            ),
        ),
        pub_use("Cell"),
    ];

    items.extend(host_operations(foreigns));

    Module { items }
}
