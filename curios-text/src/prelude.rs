use {
    super::{
        ConceptField, ConceptParam, Error, FuncSugarParam, FuncType, FuncTypeParam, GroupItem,
        LetSignature, Loader, Module, Name, Nat, NatLiteral, Plicity, Prim, Qualifier, Subterm,
        Term, TopConcept, TopItem, TopLet, TopMod, TopUse, TopWitness, TupleType, TupleTypeParam,
        UseGroup, WitnessEntry, WitnessField,
    },
    curios_abi::{ForeignFunction, ForeignStore, WireType, mode, poll, status, stdio},
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

// A `Nat` literal value term, built exactly as the parser builds one: `0` is
// bare `Zero`, anything else is `Succ(n, Zero)`. Used to bake host-owned wire
// codes (`curios_abi::{status, poll, mode}`) into the `/sys/Io` constant mirror.
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

fn bin() -> Term {
    prim(Prim::BinType)
}

fn bln() -> Term {
    prim(Prim::BlnType)
}

fn io() -> Term {
    prim(Prim::IoType)
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
        is_pub: true,
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
        is_pub: true,
        label: label.to_string(),
        module: Some(Module { items }),
    })
}

// `pub use Label/{let Label}` — the facade re-export that hoists a submodule's
// own type binding up to the library root, so `/sys/{Label}` names the type.
fn pub_use(label: &str) -> TopItem {
    TopItem::Use(TopUse {
        is_pub: true,
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
    TopItem::Let(TopLet {
        is_pub: true,
        label: label.to_string(),
        signature: LetSignature::Func {
            params: params
                .into_iter()
                .map(|(p, n, t)| FuncSugarParam {
                    plicity: p,
                    label: n.to_string(),
                    type_: t,
                })
                .collect(),
            output,
            body,
        },
    })
}

/// The surface type a host-boundary [`WireType`] denotes — the prelude's
/// reading of the signature, mirrored by `core::wire_term` after lowering.
fn wire_type(type_: &WireType) -> Term {
    match type_ {
        WireType::Nat => nat(),
        WireType::Int => int(),
        WireType::Bln => bln(),
        WireType::Bin => bin(),
        WireType::Io => io(),
        WireType::Lst(element) => lst_of(wire_type(element)),
    }
}

/// A `/sys/Io` host-function declaration generated from a foreign-store row:
/// parameter names/types and the result shape (unit, bare type, named record)
/// come off the `WireSignature`, and the body bakes the generic `Foreign` prim
/// applied to the parameter names.
fn host_fn(function: &Arc<ForeignFunction>) -> TopItem {
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

    pub_fn(
        &function.label,
        signature
            .params
            .iter()
            .map(|(param, type_)| (param.as_str(), wire_type(type_)))
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
        binary("eql", nat(), bln(), Prim::NatEql),
        binary("neq", nat(), bln(), Prim::NatNeq),
        binary("add", nat(), nat(), Prim::NatAdd),
        binary("sub", nat(), nat(), Prim::NatSub),
        binary("mul", nat(), nat(), Prim::NatMul),
        binary("div", nat(), nat(), Prim::NatDiv),
        binary("rem", nat(), nat(), Prim::NatRem),
        binary("lt", nat(), bln(), Prim::NatLt),
        binary("gt", nat(), bln(), Prim::NatGt),
        binary("lte", nat(), bln(), Prim::NatLte),
        binary("gte", nat(), bln(), Prim::NatGte),
        binary("and", nat(), nat(), Prim::NatAnd),
        binary("or", nat(), nat(), Prim::NatOr),
        binary("xor", nat(), nat(), Prim::NatXor),
        binary("shl", nat(), nat(), Prim::NatShl),
        binary("shr", nat(), nat(), Prim::NatShr),
        unary("to_int", nat(), int(), Prim::NatToInt),
        unary("to_flt", nat(), flt(), Prim::NatToFlt),
    ]
}

// `Bln` rides the same i31ref/u32 carrier as `Nat`, with `false`/`true` as
// `0`/`1`. `and`/`or`/`xor` are bitwise machine ops on those bits — exact
// boolean logic — and `eql` is the `Nat` equality op (`i32.eq`) on that single
// bit, so all four are primitives rather than `match` definitions. `not` has no
// machine instruction; `/std/Bln` defines it as `xor(b, true)`.
fn bln_ops() -> Vec<TopItem> {
    vec![
        binary("and", bln(), bln(), Prim::BlnAnd),
        binary("or", bln(), bln(), Prim::BlnOr),
        binary("xor", bln(), bln(), Prim::BlnXor),
        binary("eql", bln(), bln(), Prim::BlnEql),
    ]
}

fn int_ops() -> Vec<TopItem> {
    vec![
        binary("eql", int(), bln(), Prim::IntEql),
        binary("neq", int(), bln(), Prim::IntNeq),
        binary("add", int(), int(), Prim::IntAdd),
        binary("sub", int(), int(), Prim::IntSub),
        binary("mul", int(), int(), Prim::IntMul),
        binary("div", int(), int(), Prim::IntDiv),
        binary("rem", int(), int(), Prim::IntRem),
        binary("lt", int(), bln(), Prim::IntLt),
        binary("gt", int(), bln(), Prim::IntGt),
        binary("lte", int(), bln(), Prim::IntLte),
        binary("gte", int(), bln(), Prim::IntGte),
        // Bitwise ops on the signed i31 carrier. `and`/`or`/`xor` are exact bit
        // ops; `shl` truncates into the carrier like `Nat/shl`; `shr` is
        // arithmetic (sign-preserving). `not` is `/std/Int`'s `xor(x, -1)`.
        binary("and", int(), int(), Prim::IntAnd),
        binary("or", int(), int(), Prim::IntOr),
        binary("xor", int(), int(), Prim::IntXor),
        binary("shl", int(), int(), Prim::IntShl),
        binary("shr", int(), int(), Prim::IntShr),
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
        binary("eql", flt(), bln(), Prim::FltEql),
        binary("neq", flt(), bln(), Prim::FltNeq),
        binary("lt", flt(), bln(), Prim::FltLt),
        binary("gt", flt(), bln(), Prim::FltGt),
        binary("lte", flt(), bln(), Prim::FltLte),
        binary("gte", flt(), bln(), Prim::FltGte),
        unary("neg", flt(), flt(), Prim::FltNeg),
        unary("abs", flt(), flt(), Prim::FltAbs),
        unary("sqrt", flt(), flt(), Prim::FltSqrt),
        unary("floor", flt(), flt(), Prim::FltFloor),
        unary("ceil", flt(), flt(), Prim::FltCeil),
        unary("trunc", flt(), flt(), Prim::FltTrunc),
        unary("nearest", flt(), flt(), Prim::FltNearest),
        unary("to_nat", flt(), nat(), Prim::FltToNat),
        unary("to_int", flt(), int(), Prim::FltToInt),
        unary("to_le_bin", flt(), bin(), Prim::FltToLeBin),
    ]
}

fn bin_ops() -> Vec<TopItem> {
    vec![
        unary("len", bin(), nat(), Prim::BinLen),
        binary("eql", bin(), bln(), Prim::BinEql),
        pub_fn(
            "get",
            vec![("b", bin()), ("i", nat())],
            nat(),
            prim(Prim::BinGet(name("b"), name("i"))),
        ),
        pub_fn(
            "slice",
            vec![("b", bin()), ("s", nat()), ("e", nat())],
            bin(),
            prim(Prim::BinSlice(name("b"), name("s"), name("e"))),
        ),
        pub_fn(
            "append",
            vec![("b", bin()), ("x", nat())],
            bin(),
            prim(Prim::BinAppend(name("b"), name("x"))),
        ),
        binary("concat", bin(), bin(), Prim::BinConcat),
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
                (Plicity::Explicit, "f", fn_of(name("A"), name("B"))),
                (Plicity::Explicit, "a", lst_of(name("A"))),
            ],
            lst_of(name("B")),
            prim(Prim::LstMap(name("A"), name("B"), name("f"), name("a"))),
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

fn io_ops(foreigns: &ForeignStore) -> Vec<TopItem> {
    let mut ops = vec![
        pub_let("stdin", io(), prim(Prim::Io(stdio::STDIN))),
        pub_let("stdout", io(), prim(Prim::Io(stdio::STDOUT))),
        pub_let("stderr", io(), prim(Prim::Io(stdio::STDERR))),
        pub_fn(
            "eql",
            vec![("a", io()), ("b", io())],
            bln(),
            prim(Prim::IoEql(name("a"), name("b"))),
        ),
    ];

    // Every store-described host op, in store (= declaration) order. Each is a
    // *function*, including the 0-arity clocks/args: a value binding would
    // force-reduce its effectful prim body at definition (the bare prelude is
    // lowered whole, so a top-level value `let` lands in `main`) and trip the
    // IO-at-type-level guard, while under the function abstraction the prim
    // stays unevaluated until called.
    ops.extend(foreigns.iter().map(host_fn));

    ops.extend([
        // `(@A : Type) -> Nat -> A`: exit never returns, so its result type is
        // whatever the caller wants. `/std/Proc/exit` pins `A := False`.
        pub_fn_marked(
            "exit",
            vec![
                (Plicity::Implicit, "A", type_()),
                (Plicity::Explicit, "n", nat()),
            ],
            name("A"),
            prim(Prim::IoExit(name("A"), name("n"))),
        ),
        // The wire-code mirror: the guest counterpart of `curios_abi::wire`, so the
        // standard library compares against named constants the host derives from
        // the same source. `read`/`write` already name ops here, so each family
        // is a sub-module.
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

// === Operator concepts and their primitive witnesses ========================
//
// Every infix operator except `&&`/`||` dispatches through one of these
// concepts (`+` → `Add/add`, `==` → `Eql/eql`, `<` → `Cmp/lt`, …). They are
// declared at the *top level* of `sys` — not in std surface files — so every
// operator on every primitive type resolves the moment `sys` loads: std uses
// infix pervasively, including type-level (`Lte`'s `a + 1` indices), and
// sys-homed witnesses make that correct by construction, with no ordering
// dependency between std files. Witness fields are eta-reduced references to
// the named wrappers built above (`add = Nat/add` — the wrapper's type is
// exactly the field type), so no infix appears in any witness body.

// The binary-operator field signature `(A, A) -> output` over the concept's
// parameter.
fn operator_field_type(output: Term) -> Term {
    let a = || FuncTypeParam {
        plicity: Plicity::Explicit,
        label: None,
        type_: name("A"),
    };
    Subterm::FuncType(FuncType {
        params: vec![a(), a()],
        output,
    })
    .into()
}

// A single-parameter operator concept:
// `pub concept Label(A : Type) : Type { field : (A, A) -> Out, … }`.
fn operator_concept(label: &str, fields: Vec<(&str, Term)>) -> TopItem {
    TopItem::Concept(TopConcept {
        is_pub: true,
        label: label.to_string(),
        params: vec![ConceptParam {
            plicity: Plicity::Explicit,
            is_out: false,
            label: "A".to_string(),
            type_: type_(),
        }],
        result_sort: type_(),
        fields: fields
            .into_iter()
            .map(|(label, output)| ConceptField {
                is_super: false,
                label: label.to_string(),
                func_params: None,
                type_: operator_field_type(output),
            })
            .collect(),
    })
}

// A primitive operator witness: `satisfy Concept(Head) { field =
// Module/op, … }` — each field the named wrapper reference.
fn operator_witness(concept: &str, head: Term, fields: Vec<(&str, [&str; 2])>) -> TopItem {
    TopItem::Witness(TopWitness {
        params: Vec::new(),
        concept: Name::from([concept.to_string()]),
        args: vec![head],
        entries: fields
            .into_iter()
            .map(|(field, [module, op])| {
                WitnessEntry::Field(WitnessField {
                    label: field.to_string(),
                    func_params: None,
                    value: Subterm::Name(Name::from(vec![module.to_string(), op.to_string()]))
                        .into(),
                })
            })
            .collect(),
    })
}

// The full operator surface, at exact parity with the retired per-type
// overload table: one witness per `(operator, primitive type)` pair it
// accepted, plus `Eql(Bin)` (migrated from std — its wrapper is
// sys-expressible). `&&`/`||` stay hardcoded on `Bln` and have no concept.
fn operator_items() -> Vec<TopItem> {
    let numeric = [(nat as fn() -> Term, "Nat"), (int, "Int"), (flt, "Flt")];
    let mut items = Vec::new();

    for (concept, field) in [
        ("Add", "add"),
        ("Sub", "sub"),
        ("Mul", "mul"),
        ("Div", "div"),
        ("Rem", "rem"),
    ] {
        items.push(operator_concept(concept, vec![(field, name("A"))]));
        for (head, module) in numeric {
            items.push(operator_witness(
                concept,
                head(),
                vec![(field, [module, field])],
            ));
        }
    }

    items.push(operator_concept("Eql", vec![("eql", bln())]));
    for (head, module) in [
        (nat as fn() -> Term, "Nat"),
        (int, "Int"),
        (flt, "Flt"),
        (bln, "Bln"),
        (bin, "Bin"),
    ] {
        items.push(operator_witness(
            "Eql",
            head(),
            vec![("eql", [module, "eql"])],
        ));
    }

    // Four relations per witness: float NaN semantics make them non-derivable
    // from one another, and `Bln` results keep `sys` free of any `Order`
    // inductive (std's richer `Ord` stays separate).
    items.push(operator_concept(
        "Cmp",
        vec![("lt", bln()), ("lte", bln()), ("gt", bln()), ("gte", bln())],
    ));
    for (head, module) in numeric {
        items.push(operator_witness(
            "Cmp",
            head(),
            vec![
                ("lt", [module, "lt"]),
                ("lte", [module, "lte"]),
                ("gt", [module, "gt"]),
                ("gte", [module, "gte"]),
            ],
        ));
    }

    items
}

// The `sys` module body of primitive types and operations, served to discovery by
// `SysLoader` like any other loaded module. The host operations under `Io` come
// off `foreigns` — the compilation's foreign store.
fn sys_module(foreigns: &ForeignStore) -> Module {
    Module {
        items: vec![
            pub_mod("Nat", with_type(pub_let("Nat", type_(), nat()), nat_ops())),
            pub_use("Nat"),
            pub_mod("Int", with_type(pub_let("Int", type_(), int()), int_ops())),
            pub_use("Int"),
            pub_mod("Flt", with_type(pub_let("Flt", type_(), flt()), flt_ops())),
            pub_use("Flt"),
            pub_mod("Bin", with_type(pub_let("Bin", type_(), bin()), bin_ops())),
            pub_use("Bin"),
            pub_mod("Bln", with_type(pub_let("Bln", type_(), bln()), bln_ops())),
            pub_use("Bln"),
            pub_mod(
                "Io",
                with_type(pub_let("Io", type_(), io()), io_ops(foreigns)),
            ),
            pub_use("Io"),
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
        ]
        .into_iter()
        .chain(operator_items())
        .collect(),
    }
}

// A `&L` is itself a `Loader`, so a `&dyn Loader` can be nested inside the prelude
// decorators (which take their inner loader by value) without lifetime gymnastics.
impl<L: Loader + ?Sized> Loader for &L {
    fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        (**self).load(qualifier)
    }

    fn roots(&self) -> Vec<String> {
        (**self).roots()
    }
}

// Serves the `sys` module of primitives, delegating everything else to `inner`. `sys`
// is built directly as `text` AST (never parsed); only `["sys"]` is ever asked for it.
pub struct SysLoader<L> {
    // `sys` is fixed once the foreign store is chosen, so [`prelude`] builds
    // its AST when the loader is assembled and `load` hands out clones —
    // discovery asks for it repeatedly per compile (§ loader cache).
    module: Module,
    inner: L,
}

impl<L: Loader> Loader for SysLoader<L> {
    fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        if qualifier.iter().eq(["sys"]) {
            return Ok(self.module.clone());
        }

        self.inner.load(qualifier)
    }

    // `sys` comes *first*: root order is flat-item lowering order, which is
    // the topological-sort tiebreak — and nothing references witness items by
    // name, so only their position gets the sys operator witnesses emitted
    // (and registered) before any std item that uses infix elaborates.
    // Type-level operator uses (`a + 1` in an inductive index) park
    // conversion goals that must resolve within their own item, so witness
    // deferral cannot paper over a late-sorted witness there.
    fn roots(&self) -> Vec<String> {
        ["sys".to_string()]
            .into_iter()
            .chain(self.inner.roots())
            .collect()
    }
}

// The `std` standard library, authored as real Curios source kept alongside the
// compiler (`std/*.crs`) and embedded in the binary. The `["std"]` entry is the
// manifest of `pub mod`/`pub use` declarations; each leaf is its own module. `std`
// being well-formed is a compiler invariant, so a parse failure is a `panic!`.
const STD: &[(&[&str], &str)] = &[
    (&["std"], include_str!("../std.crs")),
    (&["std", "Lst"], include_str!("../std/Lst.crs")),
    (&["std", "Cell"], include_str!("../std/Cell.crs")),
    (&["std", "Bin"], include_str!("../std/Bin.crs")),
    (&["std", "Nat"], include_str!("../std/Nat.crs")),
    (&["std", "Int"], include_str!("../std/Int.crs")),
    (&["std", "Bln"], include_str!("../std/Bln.crs")),
    (&["std", "Io"], include_str!("../std/Io.crs")),
    (&["std", "Reader"], include_str!("../std/Reader.crs")),
    (&["std", "File"], include_str!("../std/File.crs")),
    (&["std", "Tcp"], include_str!("../std/Tcp.crs")),
    (&["std", "Task"], include_str!("../std/Task.crs")),
    (&["std", "Http"], include_str!("../std/Http.crs")),
    (&["std", "Char"], include_str!("../std/Char.crs")),
    (&["std", "Result"], include_str!("../std/Result.crs")),
    (&["std", "Option"], include_str!("../std/Option.crs")),
    (&["std", "Order"], include_str!("../std/Order.crs")),
    (&["std", "Eql"], include_str!("../std/Eql.crs")),
    (&["std", "Add"], include_str!("../std/Add.crs")),
    (&["std", "Sub"], include_str!("../std/Sub.crs")),
    (&["std", "Mul"], include_str!("../std/Mul.crs")),
    (&["std", "Div"], include_str!("../std/Div.crs")),
    (&["std", "Rem"], include_str!("../std/Rem.crs")),
    (&["std", "Cmp"], include_str!("../std/Cmp.crs")),
    (&["std", "Ord"], include_str!("../std/Ord.crs")),
    (&["std", "Show"], include_str!("../std/Show.crs")),
    (&["std", "Monad"], include_str!("../std/Monad.crs")),
    (&["std", "BigNat"], include_str!("../std/BigNat.crs")),
    (&["std", "Vec"], include_str!("../std/Vec.crs")),
    (&["std", "Eq"], include_str!("../std/Eq.crs")),
    (&["std", "False"], include_str!("../std/False.crs")),
    (&["std", "True"], include_str!("../std/True.crs")),
    (&["std", "Flt"], include_str!("../std/Flt.crs")),
    (&["std", "Str"], include_str!("../std/Str.crs")),
    (&["std", "Parse"], include_str!("../std/Parse.crs")),
    (&["std", "Json"], include_str!("../std/Json.crs")),
    (&["std", "Fmt"], include_str!("../std/Fmt.crs")),
    (&["std", "Time"], include_str!("../std/Time.crs")),
    (&["std", "Rand"], include_str!("../std/Rand.crs")),
    (&["std", "Proc"], include_str!("../std/Proc.crs")),
];

// Serves the embedded `std` modules, delegating everything else to `inner`.
pub struct StdLoader<L> {
    inner: L,
}

thread_local! {
    // Parse every embedded `std` module once per thread; `load` then hands out
    // clones. `std` being well-formed is a compiler invariant, so a parse failure
    // is a `panic!`. Discovery loads the full `std` manifest (and its leaves) on
    // every compile, so without this each compile re-parses all of `std`. `Module`
    // is not `Sync`, so this is thread-local rather than a `static` (§ loader cache).
    static STD_MODULES: Vec<Module> = STD
        .iter()
        .map(|(segments, source)| {
            source.parse::<Module>().unwrap_or_else(|error| {
                panic!("embedded std module {} is malformed: {error:?}", segments.join("/"))
            })
        })
        .collect();
}

impl<L: Loader> Loader for StdLoader<L> {
    fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        let path = qualifier.iter().collect::<Vec<_>>();

        if let Some(index) = STD.iter().position(|(segments, _)| path == **segments) {
            return Ok(STD_MODULES.with(|modules| modules[index].clone()));
        }

        self.inner.load(qualifier)
    }

    fn roots(&self) -> Vec<String> {
        self.inner
            .roots()
            .into_iter()
            .chain(["std".to_string()])
            .collect()
    }
}

// The `syn` library: modules the compiler's desugaring targets, kept alongside the
// compiler (`syn/*.crs`) and embedded in the binary. Unlike `sys`, `syn` is *not*
// internal (it is absent from `to_core::INTERNAL_ROOTS`): desugaring emits absolute
// `/syn/…` references, so the names must be resolvable like any ordinary library —
// they are not walled from user code. `syn` is privileged
// (`to_core::PRIVILEGED_ROOTS`) so it may reach the `/sys` primitives, and in
// practice it is consumed through `/std` re-exports. Well-formedness is a compiler
// invariant, so a parse failure is a `panic!`.
const SYN: &[(&[&str], &str)] = &[
    (&["syn"], include_str!("../syn.crs")),
    (&["syn", "Str"], include_str!("../syn/Str.crs")),
    (&["syn", "Monad"], include_str!("../syn/Monad.crs")),
];

// Serves the embedded `syn` modules, delegating everything else to `inner`.
pub struct SynLoader<L> {
    inner: L,
}

thread_local! {
    // Parse every embedded `syn` module once per thread; `load` hands out clones.
    // Mirrors `STD_MODULES` (§ loader cache).
    static SYN_MODULES: Vec<Module> = SYN
        .iter()
        .map(|(segments, source)| {
            source.parse::<Module>().unwrap_or_else(|error| {
                panic!("embedded syn module {} is malformed: {error:?}", segments.join("/"))
            })
        })
        .collect();
}

impl<L: Loader> Loader for SynLoader<L> {
    fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        let path = qualifier.iter().collect::<Vec<_>>();

        if let Some(index) = SYN.iter().position(|(segments, _)| path == **segments) {
            return Ok(SYN_MODULES.with(|modules| modules[index].clone()));
        }

        self.inner.load(qualifier)
    }

    fn roots(&self) -> Vec<String> {
        self.inner
            .roots()
            .into_iter()
            .chain(["syn".to_string()])
            .collect()
    }
}

/// Wrap a loader so `sys`, `syn`, and `std` resolve from the binary and everything else
/// falls through to `inner`. The wrapped loader also reports those roots from
/// [`Loader::roots`], so `to_core` declares them at the entrypoint root automatically.
/// `foreigns` is the compilation's foreign store — the host operations `/sys/Io`
/// declares; today always `curios_abi::sys_io()`, created per compilation by the
/// pipeline driver.
pub fn prelude<L: Loader>(foreigns: &ForeignStore, inner: L) -> impl Loader {
    SysLoader {
        module: sys_module(foreigns),
        inner: SynLoader {
            inner: StdLoader { inner },
        },
    }
}
