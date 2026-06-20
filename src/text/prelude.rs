use {
    super::{
        Error, FuncSugarParam, LetSignature, Loader, Module, Name, Nat, NatLiteral, Pattern,
        Plicity, Prim, Qualifier, Quantity, Subterm, Term, TopItem, TopLet, TopMod, TupleType,
        TupleTypeParam,
    },
    crate::wire,
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
// codes (`crate::wire`) into the `/sys/Io` constant mirror.
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

fn str() -> Term {
    prim(Prim::StrType)
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
                quantity: Quantity::Omega,
                type_,
            })
            .collect(),
    })
    .into()
}

fn arr_of(elem: Term) -> Term {
    prim(Prim::ArrType(elem))
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
                    quantity: Quantity::Omega,
                    pattern: Pattern::Bind(n.to_string()),
                    type_: t,
                })
                .collect(),
            output,
            body,
        },
    })
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

// A text-rendering conversion. The primitive is intrinsically `_ -> Str`: its
// output is UTF-8 by its (trusted) contract — decimal/ASCII text — the same
// inherent primitive trust as `Bin/len` returning the right length.
fn to_str(input: Term, ctor: fn(Term) -> Prim) -> TopItem {
    pub_fn("to_str", vec![("a", input)], str(), prim(ctor(name("a"))))
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
        to_str(nat(), Prim::NatToStr),
    ]
}

// `Bln` rides the same i31ref/u32 carrier as `Nat`, with `false`/`true` as
// `0`/`1`. `and`/`or`/`xor` are bitwise machine ops on those bits — exact
// boolean logic — so they are primitives rather than `match` definitions. `not`
// has no machine instruction; `/std/Bln` defines it as `xor(b, true)`.
fn bln_ops() -> Vec<TopItem> {
    vec![
        binary("and", bln(), bln(), Prim::BlnAnd),
        binary("or", bln(), bln(), Prim::BlnOr),
        binary("xor", bln(), bln(), Prim::BlnXor),
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
        to_str(int(), Prim::IntToStr),
    ]
}

fn flt_ops() -> Vec<TopItem> {
    vec![
        binary("add", flt(), flt(), Prim::FltAdd),
        binary("sub", flt(), flt(), Prim::FltSub),
        binary("mul", flt(), flt(), Prim::FltMul),
        binary("div", flt(), flt(), Prim::FltDiv),
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
        to_str(flt(), Prim::FltToStr),
        unary("to_le_bin", flt(), bin(), Prim::FltToLeBin),
    ]
}

fn str_ops() -> Vec<TopItem> {
    vec![
        // `Str` shares `Bin`'s runtime representation but not its surface.
        // `to_bin` is the carrier projection; `concat`/`eql` are defined in
        // `/std/Str` on top of it rather than as primitives.
        unary("to_bin", str(), bin(), Prim::StrToBin),
        // The trusted `Bin -> Str` coercion — the raw substrate beneath the
        // checked `/std/Str/of_bin`. Not re-exported into the `/std/Str` API.
        unary("of_bin", bin(), str(), Prim::StrOfBin),
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
        unary("flatten", arr_of(bin()), bin(), Prim::BinFlatten),
    ]
}

fn arr_ops() -> Vec<TopItem> {
    vec![
        pub_fn_marked(
            "len",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", arr_of(name("T"))),
            ],
            nat(),
            prim(Prim::ArrLen(name("T"), name("a"))),
        ),
        pub_fn_marked(
            "get",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", arr_of(name("T"))),
                (Plicity::Explicit, "i", nat()),
            ],
            name("T"),
            prim(Prim::ArrGet(name("T"), name("a"), name("i"))),
        ),
        pub_fn_marked(
            "slice",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", arr_of(name("T"))),
                (Plicity::Explicit, "s", nat()),
                (Plicity::Explicit, "e", nat()),
            ],
            arr_of(name("T")),
            prim(Prim::ArrSlice(name("T"), name("a"), name("s"), name("e"))),
        ),
        pub_fn_marked(
            "append",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", arr_of(name("T"))),
                (Plicity::Explicit, "x", name("T")),
            ],
            arr_of(name("T")),
            prim(Prim::ArrAppend(name("T"), name("a"), name("x"))),
        ),
        pub_fn_marked(
            "concat",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", arr_of(name("T"))),
                (Plicity::Explicit, "b", arr_of(name("T"))),
            ],
            arr_of(name("T")),
            prim(Prim::ArrConcat(name("T"), name("a"), name("b"))),
        ),
        pub_fn_marked(
            "flatten",
            vec![
                (Plicity::Implicit, "T", type_()),
                (Plicity::Explicit, "a", arr_of(arr_of(name("T")))),
            ],
            arr_of(name("T")),
            prim(Prim::ArrFlatten(name("T"), name("a"))),
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

fn io_ops() -> Vec<TopItem> {
    vec![
        pub_let("stdin", io(), prim(Prim::Io(0))),
        pub_let("stdout", io(), prim(Prim::Io(1))),
        pub_let("stderr", io(), prim(Prim::Io(2))),
        pub_fn(
            "eql",
            vec![("a", io()), ("b", io())],
            bln(),
            prim(Prim::IoEql(name("a"), name("b"))),
        ),
        pub_fn(
            "read",
            vec![("h", io()), ("n", nat())],
            record(vec![("status", nat()), ("bytes", bin())]),
            prim(Prim::IoRead(name("h"), name("n"))),
        ),
        pub_fn(
            "write",
            vec![("h", io()), ("b", bin())],
            record(vec![("status", nat()), ("written", nat())]),
            prim(Prim::IoWrite(name("h"), name("b"))),
        ),
        pub_fn(
            "open",
            vec![("path", bin()), ("mode", nat())],
            record(vec![("status", nat()), ("handle", io())]),
            prim(Prim::IoOpen(name("path"), name("mode"))),
        ),
        pub_fn(
            "resolve",
            vec![("host", bin()), ("port", nat())],
            record(vec![("status", nat()), ("addresses", arr_of(bin()))]),
            prim(Prim::IoResolve(name("host"), name("port"))),
        ),
        pub_fn(
            "socket",
            vec![("addr", bin())],
            record(vec![("status", nat()), ("handle", io())]),
            prim(Prim::IoSocket(name("addr"))),
        ),
        pub_fn(
            "bind",
            vec![("h", io()), ("addr", bin())],
            nat(),
            prim(Prim::IoBind(name("h"), name("addr"))),
        ),
        pub_fn(
            "connect",
            vec![("h", io()), ("addr", bin())],
            nat(),
            prim(Prim::IoConnect(name("h"), name("addr"))),
        ),
        pub_fn(
            "listen",
            vec![("h", io()), ("backlog", nat())],
            nat(),
            prim(Prim::IoListen(name("h"), name("backlog"))),
        ),
        pub_fn(
            "accept",
            vec![("h", io())],
            record(vec![("status", nat()), ("handle", io())]),
            prim(Prim::IoAccept(name("h"))),
        ),
        pub_fn(
            "start_tls",
            vec![("h", io()), ("sni", bin())],
            nat(),
            prim(Prim::IoStartTls(name("h"), name("sni"))),
        ),
        pub_fn(
            "tls_server_config",
            vec![("cert", bin()), ("key", bin())],
            record(vec![("status", nat()), ("handle", io())]),
            prim(Prim::IoTlsServerConfig(name("cert"), name("key"))),
        ),
        pub_fn(
            "start_tls_server",
            vec![("h", io()), ("cfg", io())],
            nat(),
            prim(Prim::IoStartTlsServer(name("h"), name("cfg"))),
        ),
        pub_fn(
            "set_nonblocking",
            vec![("h", io()), ("on", bln())],
            nat(),
            prim(Prim::IoSetNonblocking(name("h"), name("on"))),
        ),
        pub_fn(
            "set_recv_timeout",
            vec![("h", io()), ("ms", nat())],
            nat(),
            prim(Prim::IoSetRecvTimeout(name("h"), name("ms"))),
        ),
        pub_fn(
            "set_send_timeout",
            vec![("h", io()), ("ms", nat())],
            nat(),
            prim(Prim::IoSetSendTimeout(name("h"), name("ms"))),
        ),
        pub_fn(
            "set_reuseaddr",
            vec![("h", io()), ("on", bln())],
            nat(),
            prim(Prim::IoSetReuseaddr(name("h"), name("on"))),
        ),
        // The readiness oracle: `handles` and `events` are parallel — `events[i]`
        // is the interest bitmask (`POLL_*`) for `handles[i]` — and the result is
        // parallel too. `timeout : Int` follows `poll(2)`: `< 0` forever, `0`
        // immediate, `> 0` milliseconds.
        pub_fn(
            "poll",
            vec![
                ("handles", arr_of(io())),
                ("events", arr_of(nat())),
                ("timeout", int()),
            ],
            arr_of(nat()),
            prim(Prim::IoPoll(name("handles"), name("events"), name("timeout"))),
        ),
        pub_fn(
            "close",
            vec![("h", io())],
            unit(),
            prim(Prim::IoClose(name("h"))),
        ),
        // Clock/random are ambient (no handle). The clocks are 0-arity
        // *functions* (not value bindings), so each call re-performs the read
        // and the bare effectful-prim body stays under the function abstraction,
        // never force-reduced at definition — like read/write/open/close.
        pub_fn(
            "clock_wall",
            vec![],
            record(vec![
                ("secs_hi", nat()),
                ("secs_lo", nat()),
                ("nanos", nat()),
            ]),
            prim(Prim::IoClockWall),
        ),
        pub_fn(
            "clock_mono",
            vec![],
            record(vec![("secs", nat()), ("nanos", nat())]),
            prim(Prim::IoClockMono),
        ),
        pub_fn(
            "random",
            vec![("n", nat())],
            bin(),
            prim(Prim::IoRandom(name("n"))),
        ),
        // argv is an immutable snapshot, so a shared value binding is correct
        // (and `IoArgs` is inert, so this is not force-reduced into the IO
        // guard). `env` stays a function — lambda-protected, never force-reduced.
        pub_let("args", arr_of(bin()), prim(Prim::IoArgs)),
        pub_fn(
            "env",
            vec![("name", bin())],
            record(vec![("status", nat()), ("value", bin())]),
            prim(Prim::IoEnv(name("name"))),
        ),
        // `(@A : Type) -> Nat -> A`: exit never returns, so its result type is
        // whatever the caller wants. `/std/Proc/exit` pins `A := Void`.
        pub_fn_marked(
            "exit",
            vec![
                (Plicity::Implicit, "A", type_()),
                (Plicity::Explicit, "n", nat()),
            ],
            name("A"),
            prim(Prim::IoExit(name("A"), name("n"))),
        ),
        // The wire-code mirror: the guest counterpart of `crate::wire`, so the
        // standard library compares against named constants the host derives from
        // the same source. `read`/`write` already name ops here, so each family
        // is a sub-module.
        pub_mod(
            "Status",
            vec![
                pub_let("ok", nat(), nat_lit(wire::status::OK)),
                pub_let("eof", nat(), nat_lit(wire::status::EOF)),
                pub_let("not_found", nat(), nat_lit(wire::status::NOT_FOUND)),
                pub_let(
                    "permission_denied",
                    nat(),
                    nat_lit(wire::status::PERMISSION_DENIED),
                ),
                pub_let("exists", nat(), nat_lit(wire::status::ALREADY_EXISTS)),
                pub_let("refused", nat(), nat_lit(wire::status::CONNECTION_REFUSED)),
                pub_let("would_block", nat(), nat_lit(wire::status::WOULD_BLOCK)),
                pub_let("tls", nat(), nat_lit(wire::status::TLS_ERROR)),
            ],
        ),
        pub_mod(
            "Poll",
            vec![
                pub_let("read", nat(), nat_lit(wire::poll::READ)),
                pub_let("write", nat(), nat_lit(wire::poll::WRITE)),
                pub_let("err", nat(), nat_lit(wire::poll::ERR)),
                pub_let("hup", nat(), nat_lit(wire::poll::HUP)),
            ],
        ),
        pub_mod(
            "Mode",
            vec![
                pub_let("read", nat(), nat_lit(wire::mode::READ)),
                pub_let("write", nat(), nat_lit(wire::mode::WRITE)),
                pub_let("append", nat(), nat_lit(wire::mode::APPEND)),
            ],
        ),
    ]
}

// The `sys` module body of primitive types and operations, served to discovery by
// `SysLoader` like any other loaded module.
fn sys_module() -> Module {
    Module {
        items: vec![
            pub_let("Nat", type_(), nat()),
            pub_let("Int", type_(), int()),
            pub_let("Flt", type_(), flt()),
            pub_let("Bin", type_(), bin()),
            pub_let("Str", type_(), str()),
            pub_let("Bln", type_(), bln()),
            pub_let("Io", type_(), io()),
            pub_fn("Arr", vec![("T", type_())], type_(), arr_of(name("T"))),
            pub_fn("Cell", vec![("T", type_())], type_(), cell_of(name("T"))),
            pub_mod("Nat", nat_ops()),
            pub_mod("Bln", bln_ops()),
            pub_mod("Int", int_ops()),
            pub_mod("Flt", flt_ops()),
            pub_mod("Bin", bin_ops()),
            pub_mod("Str", str_ops()),
            pub_mod("Arr", arr_ops()),
            pub_mod("Cell", cell_ops()),
            pub_mod("Io", io_ops()),
        ],
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
    inner: L,
}

thread_local! {
    // `sys` is the same on every load, so build its AST once per thread and hand
    // out clones — discovery asks for it on every compile (§ loader cache). `Module`
    // is not `Sync`, so this is thread-local rather than a `static`.
    static SYS_MODULE: Module = sys_module();
}

impl<L: Loader> Loader for SysLoader<L> {
    fn load(&self, qualifier: &Qualifier) -> Result<Module, Error> {
        if qualifier.iter().eq(["sys"]) {
            return Ok(SYS_MODULE.with(Module::clone));
        }

        self.inner.load(qualifier)
    }

    fn roots(&self) -> Vec<String> {
        self.inner
            .roots()
            .into_iter()
            .chain(["sys".to_string()])
            .collect()
    }
}

// The `std` standard library, authored as real Curios source kept alongside the
// compiler (`std/*.crs`) and embedded in the binary. The `["std"]` entry is the
// manifest of `pub mod`/`pub use` declarations; each leaf is its own module. `std`
// being well-formed is a compiler invariant, so a parse failure is a `panic!`.
const STD: &[(&[&str], &str)] = &[
    (&["std"], include_str!("../../std.crs")),
    (&["std", "Arr"], include_str!("../../std/Arr.crs")),
    (&["std", "Cell"], include_str!("../../std/Cell.crs")),
    (&["std", "Bin"], include_str!("../../std/Bin.crs")),
    (&["std", "Nat"], include_str!("../../std/Nat.crs")),
    (&["std", "Int"], include_str!("../../std/Int.crs")),
    (&["std", "Bln"], include_str!("../../std/Bln.crs")),
    (&["std", "Io"], include_str!("../../std/Io.crs")),
    (&["std", "Reader"], include_str!("../../std/Reader.crs")),
    (&["std", "File"], include_str!("../../std/File.crs")),
    (&["std", "Net"], include_str!("../../std/Net.crs")),
    (&["std", "Task"], include_str!("../../std/Task.crs")),
    (&["std", "Http"], include_str!("../../std/Http.crs")),
    (&["std", "Char"], include_str!("../../std/Char.crs")),
    (&["std", "Result"], include_str!("../../std/Result.crs")),
    (&["std", "Option"], include_str!("../../std/Option.crs")),
    (&["std", "Lst"], include_str!("../../std/Lst.crs")),
    (&["std", "Vec"], include_str!("../../std/Vec.crs")),
    (&["std", "Eq"], include_str!("../../std/Eq.crs")),
    (&["std", "Void"], include_str!("../../std/Void.crs")),
    (&["std", "Flt"], include_str!("../../std/Flt.crs")),
    (&["std", "Str"], include_str!("../../std/Str.crs")),
    (&["std", "Parse"], include_str!("../../std/Parse.crs")),
    (&["std", "Json"], include_str!("../../std/Json.crs")),
    (&["std", "Fmt"], include_str!("../../std/Fmt.crs")),
    (&["std", "Time"], include_str!("../../std/Time.crs")),
    (&["std", "Rand"], include_str!("../../std/Rand.crs")),
    (&["std", "Proc"], include_str!("../../std/Proc.crs")),
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

/// Wrap a loader so `sys` and `std` resolve from the binary and everything else falls
/// through to `inner`. The wrapped loader also reports `sys` and `std` from
/// [`Loader::roots`], so `to_core` declares them at the entrypoint root automatically.
pub fn prelude<L: Loader>(inner: L) -> impl Loader {
    SysLoader {
        inner: StdLoader { inner },
    }
}
