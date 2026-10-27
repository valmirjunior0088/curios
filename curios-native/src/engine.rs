use {
    super::{
        Answer, Handle, HostOps, Lift, Lower, Mode, Plugin, Refusal, Replied, Reply, SerialFlow,
        SerialOp, SerialParity, Settled, StdioMode, anyref_array_type, bool_of, filled,
        i8_array_type, longs_array_type, words_array_type,
    },
    curios_abi::{
        Answered, Carrier, ENTRY, ForeignFunction, ForeignStore, HostOp, LANE_IN, Namespace, PANIC,
        RowReply, WireReturn, WireType, for_each_host_op,
    },
    std::{
        collections::{HashMap, hash_map::Entry},
        error::Error,
        fmt, panic,
        sync::{Arc, LazyLock},
        thread,
    },
    wasmtime::{
        AnyRef, ArrayType, Caller, Config, Engine, FuncType, HeapType, Linker, Module, RefType,
        Rooted, Store, StoreLimits, StoreLimitsBuilder, Val, ValType,
    },
};

#[cfg(feature = "test-support")]
mod test_support;

#[cfg(test)]
mod tests;

mod turns;
use turns::*;

/// Reject a malformed module, against the same engine that will run it.
///
/// Here rather than in the crate that links a compiler: validation is a wasmparser pass over the bytes, not a compilation, so a build with no backend can decide it. That is what lets the check live beside the engine whose feature set decides what counts as valid.
///
/// Returns the failure as a `String` so no `wasmtime` type appears in the signature; callers are free to treat a refusal as fatal, and `curios` does.
pub fn validate(bytes: &[u8]) -> Result<(), String> {
    Module::validate(shared_engine(), bytes).map_err(|error| error.to_string())
}

/// The stack a module's calls nest on, in bytes.
const GUEST_STACK: usize = 1 << 30;

/// The heap a program's live set may grow to, in bytes: the bound every store states to the engine ([`Guest`]), so a program whose live set grows without end stops on the engine's own refusal to grow it. The decision and its reason are this crate's `README.md`, "The heap is sized ahead of its churn".
pub(crate) const GUEST_HEAP: usize = 4 << 30;

/// What a program's store holds beside its instances: the bound its heap, and each memory a module declares, may grow to, which the engine asks of the store's limiter before it grows one.
pub struct Guest {
    limits: StoreLimits,
}

impl Guest {
    /// A store's data whose heap grows to `heap` bytes and no further.
    pub(crate) fn bounded(heap: usize) -> Self {
        Self {
            limits: StoreLimitsBuilder::new().memory_size(heap).build(),
        }
    }
}

/// A store against `engine` whose heap grows to `heap` bytes and no further: every store a module runs in.
pub(crate) fn guest_store(engine: &Engine, heap: usize) -> Store<Guest> {
    let mut store = Store::new(engine, Guest::bounded(heap));
    store.limiter(|guest| &mut guest.limits);

    store
}

/// What a host operation still has to run in when a module calls it from its deepest frame. The engine checks its limit where a module is entered and where it calls, never inside a host operation, so one called from the last frame the limit allows runs past it, and the thread has this much there.
const HOST_RESERVE: usize = 16 << 20;

/// Run `enter` on a thread whose stack is a module's, and answer what it answers.
///
/// **Every entry into a module goes through here**, because the engine's limit bounds a stack and reserves none: where the thread's own stack ends first, the process aborts with no trap. So the limit [`shared_engine`] sets and the stack this thread is built with are one constant, and no caller's thread — a system's main thread, a test's two mebibytes — is what a module runs on. A panic on the thread reaches the caller as itself.
pub(crate) fn on_guest_stack<T: Send>(enter: impl FnOnce() -> T + Send) -> Result<T, String> {
    thread::scope(|scope| {
        let guest = thread::Builder::new()
            .name("guest".to_owned())
            .stack_size(GUEST_STACK + HOST_RESERVE)
            .spawn_scoped(scope, enter)
            .map_err(|error| format!("cannot reserve the program's stack: {error}"))?;

        match guest.join() {
            Ok(answer) => Ok(answer),
            Err(payload) => panic::resume_unwind(payload),
        }
    })
}

/// The one wasm engine for the whole process. Building an `Engine` stands up the Cranelift backend and is expensive, so it is created once and shared; `Engine` is `Send + Sync` (internally reference-counted), so a `static` is sound and a clone is cheap. Every module, store, and type below is created against it, so they stay engine-consistent.
pub fn shared_engine() -> &'static Engine {
    static ENGINE: LazyLock<Engine> = LazyLock::new(|| {
        let mut config = Config::new();
        config.wasm_reference_types(true);
        config.wasm_function_references(true);
        config.wasm_gc(true);
        config.wasm_tail_call(true);
        // The collector is left at `Collector::Auto`: the workspace `wasmtime` dependency compiles in only `gc-copying`, so `Auto` resolves to the copying (semi-space) collector — bump-allocation with an in-wasm fast path, so `struct.new`/`array.new` do not round-trip through the `gc_alloc_raw` libcall the deferred-reference-counting collector would require.

        // Thirty-two mebibytes: the decision and its reason are this crate's `README.md`, "The heap is sized ahead of its churn", and the figures and their retake recipes live with `chain_collection_decomposition`, `spines_collection_decomposition` and `spines_key_width_measurements`.
        config.gc_heap_initial_size(32 * 1024 * 1024);

        // One gibibyte: the decision and its reason are this crate's `README.md`, "A module runs on a stack of its own". The limit bounds a stack and reserves none, so [`on_guest_stack`] is where a thread that has it is built. The engine refuses a limit past the stack it would give an asynchronous call, which nothing here makes, so that size is stated as what the thread has.
        config.max_wasm_stack(GUEST_STACK);
        config.async_stack_size(GUEST_STACK + HOST_RESERVE);

        // Under the `profile` feature, symbolicate emitted code for a sampling profiler: wasmtime writes `/tmp/perf-<pid>.map`, which `samply` and `perf` read to attribute samples to the `$func/<N>$hint` names `curios-emit` emitted. Without it every sample landing in emitted wasm resolves to a bare address, or symbolicates into an unrelated host function's prologue.
        //
        // This is the guest-side half of the same flag `curios-profile` uses for the compiler, so one feature profiles both ends of a compile-and-run. It selects how compiled code is registered with the host rather than how it is compiled, so a `.cwasm` produced without it still deserializes against an engine built with it.
        #[cfg(feature = "profile")]
        config.profiler(wasmtime::ProfilingStrategy::PerfMap);

        Engine::new(&config).expect("failed to create wasm engine")
    });

    &ENGINE
}

/// The wasmtime type of one host import, derived from its `WireSignature`: each type as the carrier `WireType::carrier` states, which is the statement `curios-emit` types the module's import section by, and wasmtime validates the two against each other at instantiation. A scalar is the raw number, and the guest boxes a result; an array carrier is this engine's own registration of the payload type the emitter declares (`bytes_sub_type`, `longs_sub_type`, `words_sub_type`, `elems_sub_type`).
fn host_func_type(engine: &Engine, function: &ForeignFunction) -> FuncType {
    let signature = function.signature();

    wire_func_type(
        engine,
        &signature.imported_params(),
        &signature.imported_results(),
    )
}

/// [`host_func_type`] over the wire types alone: what an import that takes `params` and answers `results` is typed as.
fn wire_func_type(engine: &Engine, params: &[WireType], results: &[WireType]) -> FuncType {
    // An option rides its element's list, and admits null where it holds none.
    let array_ref = |wire_type: &WireType, array_type: ArrayType| {
        ValType::Ref(RefType::new(
            wire_type.admits_null(),
            HeapType::ConcreteArray(array_type),
        ))
    };
    // Raw in both directions: a host hands back a number and the guest boxes it, so nothing here needs to know a layout curios-emit defines.
    let val_type = |wire_type: &WireType| match wire_type.carrier() {
        Carrier::I64 => ValType::I64,
        Carrier::I32 => ValType::I32,
        Carrier::F64 => ValType::F64,
        Carrier::Bytes => array_ref(wire_type, i8_array_type(engine)),
        Carrier::Longs => array_ref(wire_type, longs_array_type(engine)),
        Carrier::Words => array_ref(wire_type, words_array_type(engine)),
        Carrier::Elems => array_ref(wire_type, anyref_array_type(engine)),
    };

    FuncType::new(
        engine,
        params.iter().map(val_type).collect::<Vec<_>>(),
        results.iter().map(val_type).collect::<Vec<_>>(),
    )
}

/// A type-erased implementation of one import: the closure wasmtime calls, already wrapped in its [`Lift`] and [`Reply`] plumbing.
pub(crate) type Trampoline =
    Arc<dyn Fn(&mut Caller<'_, Guest>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync>;

/// Ask an answer for its reply to a call's operands: written into the call's results at once, or the future of one handed back for the host to file.
type Ask = Arc<
    dyn Fn(&mut Caller<'_, Guest>, &[Val], &mut [Val]) -> wasmtime::Result<Option<Answer>>
        + Send
        + Sync,
>;

/// Write what a filed future of an answer settled with into the results of the call that asks for it again.
type Settle =
    Arc<dyn Fn(&mut Caller<'_, Guest>, Settled, &mut [Val]) -> wasmtime::Result<()> + Send + Sync>;

/// How an answer is entered: asked once a call's operands are lifted, and, where it handed back a future, settled from what the future came to.
#[derive(Clone)]
pub(crate) struct Entered {
    ask: Ask,
    settle: Settle,
}

impl Entered {
    /// An answer `trampoline` gives at once, which settles nothing.
    pub(crate) fn now(trampoline: Trampoline) -> Self {
        Self {
            ask: Arc::new(move |caller, params, results| {
                trampoline(caller, params, results).map(|()| None)
            }),
            settle: Arc::new(|_, _, _| {
                Err(wasmtime::Error::msg(
                    "the call asks again for an answer given at once",
                ))
            }),
        }
    }

    /// An answer whose reply `R` comes when `ask` says: the reply it writes at once, or the future of one, which settles as `R` and is written by `reply`.
    fn of<R: Send + 'static>(
        ask: impl Fn(&mut Caller<'_, Guest>, &[Val], &mut [Val]) -> wasmtime::Result<Option<Answer>>
        + Send
        + Sync
        + 'static,
        reply: impl Fn(&mut Caller<'_, Guest>, R, &mut [Val]) -> wasmtime::Result<()>
        + Send
        + Sync
        + 'static,
    ) -> Self {
        Self {
            ask: Arc::new(ask),
            settle: Arc::new(move |caller, settled, results| {
                let settled = settled.downcast::<R>().map_err(|_| {
                    wasmtime::Error::msg("the call asks again for an answer another call left")
                })?;

                reply(caller, *settled, results)
            }),
        }
    }
}

/// `future`, as the host files it: what it comes to, its type erased.
fn filed<R: Send + 'static>(future: impl Future<Output = R> + Send + 'static) -> Answer {
    Box::pin(async move { Box::new(future.await) as Settled })
}

/// What a call comes back with, as the type of what answers it states: the wire types that cross, in order, or nothing ever.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Returned {
    Values(Vec<WireType>),
    Never,
}

impl Returned {
    /// What a declaration states of the same: its results' wire types, their labels being the guest's alone, or that its call does not return.
    pub(crate) fn declared(returns: &WireReturn) -> Self {
        match returns {
            WireReturn::Answers(results) => Returned::Values(results.types()),
            WireReturn::Never => Returned::Never,
        }
    }
}

/// The wire types of a list as a declaration writes them.
fn spelled(types: &[WireType]) -> String {
    types
        .iter()
        .map(WireType::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

impl fmt::Display for Returned {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Returned::Values(types) => write!(formatter, "({})", spelled(types)),
            Returned::Never => formatter.write_str("Never"),
        }
    }
}

/// What an answer takes and comes back with, as its implementation states them: what the link holds to the row.
#[derive(Clone)]
pub(crate) struct Stated {
    params: Vec<WireType>,
    returned: Returned,
}

impl Stated {
    /// What a row's own declaration states: what an answer resolved against the row itself crosses as.
    pub(crate) fn declared(function: &ForeignFunction) -> Self {
        let signature = function.signature();

        Self {
            params: signature.params.clone(),
            returned: Returned::declared(&signature.returns),
        }
    }
}

/// Answers by the name each row is declared under: the one registry a link reads, whoever filled it. An embedder's closures and a package's modules fill one ([`ForeignBindings`]) and a host's own methods another ([`host_rows`]), and nothing tells the two kinds of answer apart but which was asked first.
///
/// Each answer stands beside `T`, how it is entered: an [`Entered`] where a program is run, and nothing where its link is only checked ([`ForeignBindings::check`]). The comparison is the same code either way.
#[derive(Clone)]
pub(crate) struct Answers<T = Entered> {
    rows: HashMap<String, (Stated, T)>,
    /// The names answered more than once, in the order the second answer arrived: what the link refuses, since [`ForeignBindings::define`] refuses nothing.
    twice: Vec<String>,
}

impl<T> Default for Answers<T> {
    fn default() -> Self {
        Self {
            rows: HashMap::new(),
            twice: Vec::new(),
        }
    }
}

impl<T> Answers<T> {
    /// Record the answer to the row declared as `name`: what it states, and how it is entered.
    pub(crate) fn record(&mut self, name: String, stated: Stated, entered: T) {
        match self.rows.entry(name) {
            Entry::Occupied(taken) => self.twice.push(taken.key().clone()),
            Entry::Vacant(free) => {
                free.insert((stated, entered));
            }
        }
    }

    /// Refuse what these answers get wrong about the program whose rows are `foreigns`, whatever it imports: a row answered twice, and an answer under a name the program declares no row for, which would otherwise never be asked and never be missed.
    fn admit(&self, foreigns: &ForeignStore) -> Result<(), String> {
        if let Some(name) = self.twice.first() {
            return Err(format!(
                "`{name}` is answered twice by the bindings of this run"
            ));
        }

        match self
            .rows
            .keys()
            .filter(|name| foreigns.get(name).is_none())
            .min()
        {
            Some(name) => Err(format!(
                "a binding answers `{name}`, which the program declares no foreign row for"
            )),
            None => Ok(()),
        }
    }
}

impl Answers {
    /// Answer the row declared as `name` with a typed closure: its operands and its reply are the closure's own types, which the link holds to the row.
    fn answer<Li, Lo, F>(&mut self, name: String, f: F)
    where
        Li: Lift,
        Lo: Reply,
        F: Fn(Li) -> Lo + Send + Sync + 'static,
    {
        self.record(
            name,
            Stated {
                params: Li::shape(),
                returned: Lo::returned(),
            },
            Entered::now(Arc::new(move |caller, params, results| {
                f(Li::lift(caller, params)?).reply(caller, results)
            })),
        );
    }

    /// Answer the row declared as `name` with a typed closure whose reply comes later, as [`answer`](Self::answer) answers with one given at once.
    fn answer_later<Li, Lo, Fu, F>(&mut self, name: String, f: F)
    where
        Li: Lift,
        Lo: Lower + Send + 'static,
        Fu: Future<Output = Lo> + Send + 'static,
        F: Fn(Li) -> Fu + Send + Sync + 'static,
    {
        self.record(
            name,
            Stated {
                params: Li::shape(),
                returned: Lo::returned(),
            },
            Entered::of(
                move |caller, params, _| Ok(Some(filed(f(Li::lift(caller, params)?)))),
                |caller, reply: Lo, results| reply.reply(caller, results),
            ),
        );
    }

    /// Answer the host's row `op` with its method, `f`: at once or later, as the method's reply says.
    fn row<Li, Ro, F>(&mut self, op: HostOp, f: F)
    where
        Li: Lift,
        Ro: RowReply<Reply: Send + 'static>,
        F: Fn(Li) -> Ro + Send + Sync + 'static,
    {
        self.record(
            op.declared(),
            Stated {
                params: Li::shape(),
                returned: Replied::<Ro::Reply>::returned(),
            },
            Entered::of(
                move |caller, params, results| match f(Li::lift(caller, params)?).answered() {
                    Answered::Now(reply) => {
                        Replied { op, reply }.reply(caller, results).map(|()| None)
                    }
                    Answered::Later(later) => Ok(Some(filed(later))),
                },
                move |caller, reply: Ro::Reply, results| {
                    Replied { op, reply }.reply(caller, results)
                },
            ),
        );
    }
}

/// What answers a program's rows ahead of the host: an embedder's closures and the modules a package declares, each for the row declared under a name. A run links *pull-based* — it walks the program's imports and defines exactly what the program demands, an answer here ahead of the host's own — so an import nothing answers is a clean, named refusal instead of a stranded wasmtime lookup.
///
/// Nothing is checked where an answer is recorded. What it is held to is the program's own declaration of the row, which the program carries ([`Compiled`]), so a disagreement is refused where the two meet, at the link, by one comparison for every answer.
#[derive(Clone, Default)]
pub struct ForeignBindings {
    pub(crate) answers: Answers,
    /// The modules resolved for the program, each instantiated into the program's own store where a run begins, and answering there the rows it was resolved for.
    pub(crate) modules: Vec<Arc<Plugin>>,
}

impl ForeignBindings {
    /// No answers of one's own: every row the program reaches is left to the host, which answers the standard library's. Follow with one [`define`](Self::define) for each row to answer ahead of it.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Answer the row declared as `name`, its fully qualified name (e.g. `/foo/double`), with a typed closure. The closure's operand and reply types state what it takes and comes back with, and a run refuses, naming the row, an answer that crosses otherwise than the program declares, one that names no row of the program, and a row answered twice.
    pub fn define<Li, Lo, F>(&mut self, name: &str, f: F)
    where
        Li: Lift,
        Lo: Reply,
        F: Fn(Li) -> Lo + Send + Sync + 'static,
    {
        self.answers.answer(name.to_string(), f);
    }

    /// Answer the row declared as `name`, as [`define`](Self::define) does, with a closure whose reply comes later: the future it hands back is filed with the host, polled where the row is called and again whenever it wakes, and the call suspends meanwhile, so the rest of the program runs on. The future does its waiting by being woken, so slow work runs elsewhere — [`blocking`](crate::blocking()) runs a blocking call on the runtime's pool, and an embedder's own runtime may run the rest — and the future only learns when it is done. A call dropped meanwhile drops the future.
    pub fn define_async<Li, Lo, Fu, F>(&mut self, name: &str, f: F)
    where
        Li: Lift,
        Lo: Lower + Send + 'static,
        Fu: Future<Output = Lo> + Send + 'static,
        F: Fn(Li) -> Fu + Send + Sync + 'static,
    {
        self.answers.answer_later(name.to_string(), f);
    }

    /// Whether an answer here stands for the row declared as `name`: an embedder's closure, or a module resolved for it.
    pub fn answers(&self, name: &str) -> bool {
        self.answers.rows.contains_key(name)
            || self
                .modules
                .iter()
                .any(|module| module.answers().any(|row| row.name() == name))
    }

    /// Link the program whose rows are `foreigns` as a run under this runtime's host would, entering nothing: every row is held to what stands to answer it, by the comparison a run makes, and what a run would refuse where it begins is refused here. What writes an executable asks this first, since a launcher has these bindings and this host and no other.
    pub fn check(&self, foreigns: &ForeignStore) -> Result<(), String> {
        let mut bindings = Answers::<()> {
            rows: self
                .answers
                .rows
                .iter()
                .map(|(name, (stated, _))| (name.clone(), (stated.clone(), ())))
                .collect(),
            twice: self.answers.twice.clone(),
        };
        for module in &self.modules {
            for row in module.answers() {
                bindings.record(row.name().to_string(), Stated::declared(row), ());
            }
        }
        bindings.admit(foreigns)?;

        let host = host_stated();
        for row in foreigns.iter() {
            answering(foreigns, &bindings, &host, row.name())?;
        }

        Ok(())
    }
}

/// What this runtime's host states of every row it answers, with nothing to enter: the table's own signatures, which [`host_rows`] answers by.
fn host_stated() -> Answers<()> {
    let mut answers = Answers::default();

    for op in HostOp::ALL {
        let signature = op.signature();

        answers.record(
            op.declared(),
            Stated {
                params: signature.params.clone(),
                returned: Returned::declared(&signature.returns),
            },
            (),
        );
    }

    answers
}

/// The row the program declares as `name` and how its answer is entered: the first answer among `bindings`' and the host's, held to the row — the wire types it takes and what it comes back with. Who answers a row is decided here, by what stands to answer, and nothing in the program says which.
fn answering<'a, T>(
    foreigns: &'a ForeignStore,
    bindings: &'a Answers<T>,
    host: &'a Answers<T>,
    name: &str,
) -> Result<(&'a Arc<ForeignFunction>, &'a T), String> {
    let declared = foreigns.get(name).ok_or_else(|| {
        format!("the program imports `{name}`, which the rows it carries do not describe")
    })?;
    let ((stated, entered), answerer) = bindings
        .rows
        .get(name)
        .map(|answer| (answer, "a binding"))
        .or_else(|| host.rows.get(name).map(|answer| (answer, "this host")))
        .ok_or_else(|| {
            format!("no implementation for `{name}`: no module answers it, and this host does not")
        })?;

    let signature = declared.signature();
    let returned = Returned::declared(&signature.returns);

    if signature.params != stated.params || returned != stated.returned {
        return Err(format!(
            "{answerer} answers `{name}` as ({}) -> {}, and the program declares it ({}) -> {returned}",
            spelled(&stated.params),
            stated.returned,
            spelled(&signature.params),
        ));
    }

    Ok((declared, entered))
}

/// `entered`, the answer to the row declared as `name` that answers `results`, entered through the lane every row that returns carries (`curios_abi::LANE_IN`, `curios_abi::LANE_OUT`).
///
/// A first call hands the operands it declares to the answer, and a reply given at once goes back beside no token. A future of one is filed with `host`: the reply goes back where the host found it settled at once, and otherwise fillers go back beside the token it is filed under, which the call suspends on. A call that asks again with the token reads the reply back once it has settled, and fillers beside the token again until then; a token that names no answer the host holds refuses the call. A call that drops its token closes it, which drops the future, and answers fillers, as one that drops nothing does.
fn laned(
    name: &str,
    results: Vec<WireType>,
    entered: Entered,
    host: Arc<dyn HostOps + Send + Sync>,
) -> Trampoline {
    let name = name.to_string();

    Arc::new(move |caller, params, replies| {
        let (operands, lane) = params.split_at(params.len() - LANE_IN.len());
        let (reply, slot) = replies.split_at_mut(replies.len() - 1);
        let pending = Option::<Handle>::lift(caller, &lane[..1])?;
        let dropped = bool_of(lane[1].unwrap_i32())?;

        let settled = match (pending, dropped) {
            (None, false) => {
                (entered.ask)(caller, operands, reply)?.map(|answer| host.later(answer))
            }
            (Some(token), false) => match host.settled(token.clone()) {
                Ok(Some(settled)) => Some(Ok(settled)),
                Ok(None) => Some(Err(token)),
                Err(Refusal(why)) => {
                    return Err(wasmtime::Error::msg(format!("{name}: {why}")));
                }
            },
            (token, true) => {
                if let Some(token) = token {
                    host.handle_close(token);
                }
                filled(caller, &results, reply)?;

                None
            }
        };

        let token = match settled {
            None => None,
            Some(Ok(settled)) => {
                (entered.settle)(caller, settled, reply)?;

                None
            }
            Some(Err(token)) => {
                filled(caller, &results, reply)?;

                Some(token)
            }
        };

        token.lower(caller, slot)
    })
}

/// Define the `ffi` import declared as `name` into `linker`, entered as [`answering`] finds it answered, and filing what it answers later with `host`.
fn link(
    linker: &mut Linker<Guest>,
    engine: &Engine,
    foreigns: &ForeignStore,
    bindings: &Answers,
    answers: &Answers,
    host: &Arc<dyn HostOps + Send + Sync>,
    name: &str,
) -> Result<(), String> {
    let (declared, entered) = answering(foreigns, bindings, answers, name)?;
    let trampoline = match &declared.signature().returns {
        WireReturn::Answers(results) => {
            laned(name, results.types(), entered.clone(), Arc::clone(host))
        }
        // A row that does not return carries no lane, so nothing can answer it later.
        WireReturn::Never => {
            let ask = Arc::clone(&entered.ask);
            let name = name.to_string();

            Arc::new(
                move |caller: &mut Caller<'_, Guest>, params: &[Val], results: &mut [Val]| {
                    match ask(caller, params, results)? {
                        None => Ok(()),
                        Some(_) => Err(wasmtime::Error::msg(format!(
                            "{name}: a call that does not return cannot be answered later"
                        ))),
                    }
                },
            ) as Trampoline
        }
    };

    linker
        .func_new(
            Namespace::Ffi.as_str(),
            name,
            host_func_type(engine, declared),
            move |mut caller, params, results| trampoline(&mut caller, params, results),
        )
        .map(|_| ())
        .map_err(|error| format!("failed to define {name}: {error}"))
}

/// Answer every row of the table under the name the standard library declares it by ([`HostOp::declared`]). Each import lifts its operands as the row types them, calls the method, and lowers the reply through [`Replied`], at once or once it has come. Generated from the rows it binds, so no row goes unanswered, none is answered twice, and no answer's types can differ from the row's.
macro_rules! declare_host_rows {
    ($(
        $(#[doc = $doc:literal])*
        $variant:ident: fn $name:ident($($p:ident: $t:ty),* $(,)?) -> $r:ty { $($contract:tt)* }
    )*) => {
        /// What `host` answers of the rows the standard library declares.
        pub(crate) fn host_rows<H: HostOps + Send + Sync + 'static>(host: Arc<H>) -> Answers {
            let mut answers = Answers::default();

            $(
                answers.row(HostOp::$variant, {
                    let host = Arc::clone(&host);

                    move |($($p,)*): ($($t,)*)| host.$name($($p),*)
                });
            )*

            answers
        }
    };
}

for_each_host_op!(declare_host_rows);

/// A process exit requested via `proc_exit`. Carried out of the wasm call as a trap so it unwinds cleanly; `instantiate` catches it and recovers the code, distinguishing a clean exit from a real trap. Made only when a [`Termination`](curios_abi::Termination) reply is encoded, so a host method answering the row cannot return into the guest.
#[derive(Debug)]
pub(crate) struct ExitTrap(pub(crate) u8);

impl fmt::Display for ExitTrap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "process exited with code {}", self.0)
    }
}

impl Error for ExitTrap {}

/// A refusal the emitted program raised through `sys.panic`: the message it handed over, carried out of the wasm call as a trap exactly as an exit is, and rendered by `instantiate` as `panicked: …` — the sentence is the report, and the frames follow it as context.
#[derive(Debug)]
struct PanicTrap(Vec<u8>);

impl fmt::Display for PanicTrap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "panicked: {}", String::from_utf8_lossy(&self.0))
    }
}

impl Error for PanicTrap {}

/// What a run answers of how a program ended: one reading where its entry returned, of the result it handed back, and one where `proc_exit` unwound it, of the code. A trap is neither, and is every run's error alike.
pub(crate) trait Ended: Sized + Send {
    /// The entry returned `result`, read while the store it lives in is open.
    fn returned(store: &Store<Guest>, result: Option<Rooted<AnyRef>>) -> Result<Self, String>;

    /// `proc_exit` ended the run with `code`.
    fn exited(code: u8) -> Self;
}

/// A program's exit code: the code it exited with, and `0` where its entry returned. What the entry hands back there is the unit every program's description yields, which says nothing and is not read.
impl Ended for u8 {
    fn returned(_: &Store<Guest>, _: Option<Rooted<AnyRef>>) -> Result<Self, String> {
        Ok(0)
    }

    fn exited(code: u8) -> Self {
        code
    }
}

/// How a test program's run ended: with the verdict its entry answered, or by an exit that left it unanswered. An exit is the second whatever its code, so no code is ever read as a verdict.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestEnd {
    /// The entry returned, answering whether what it ran held.
    Answered(bool),
    /// `proc_exit` ended the run with this code before the entry answered.
    Exited(u8),
}

/// A test program's entry answers a `Bool` ([`ENTRY`]), read as the reference it is handed back as, and a reference that is none is refused rather than read as a verdict. The unit a program's entry hands back is the reference `false` is, so a module that is no test program is never read as one whose tests held.
impl Ended for TestEnd {
    fn returned(store: &Store<Guest>, result: Option<Rooted<AnyRef>>) -> Result<Self, String> {
        let refused = || format!("{ENTRY} answered no Bool");
        let word = result
            .ok_or_else(refused)?
            .as_i31(store)
            .map_err(|error| format!("failed to read what {ENTRY} answered: {error}"))?
            .ok_or_else(refused)?;

        bool_of(word.get_i32())
            .map(TestEnd::Answered)
            .map_err(|_| refused())
    }

    fn exited(code: u8) -> Self {
        TestEnd::Exited(code)
    }
}

/// A module's machine code as this engine holds it: what a program is run from and what a declared module is linked from.
///
/// **Holding one is what makes instantiating safe, and making one is where the trust is taken.** Machine code read back from bytes is run on trust, since nothing can check that the bytes are a compiler's output. So there are two ways to one and no third: [`MachineCode::of`] takes the module a compiler made on [`shared_engine`], which is `curios-cranelift`'s to call since this crate links no compiler, and [`MachineCode::deserialize`] reads bytes back, its caller vouching for them.
#[derive(Clone)]
pub struct MachineCode(pub(crate) Module);

impl MachineCode {
    /// The machine code `module` is: an engine made it, so nothing is read back and nothing is vouched for.
    pub fn of(module: Module) -> Self {
        Self(module)
    }

    /// Read back a precompiled module: `.cwasm` bytes a compiler wrote out for this exact wasmtime version and engine configuration.
    ///
    /// # Safety
    ///
    /// `payload` must be unmodified output of this engine's compiler: `Module::deserialize` performs only light validation, so a foreign blob could execute arbitrary code. The keyword is what carries that obligation to each caller, which names its provenance where it takes it on — the launcher its own footer, the compiler what it filed in the project's store.
    pub unsafe fn deserialize(payload: &[u8]) -> Result<Self, String> {
        // SAFETY: the caller's, restated in this function's contract — `payload` is our own precompiled output.
        unsafe { Module::deserialize(shared_engine(), payload) }
            .map(Self)
            .map_err(|error| error.to_string())
    }

    /// The engine's own module, for the crate that links a compiler to write out.
    pub fn module(&self) -> &Module {
        &self.0
    }
}

/// A program's machine code as this engine holds it, ready to run.
///
/// **Holding one is what makes a run safe.** The trust machine code read back from bytes is run on is taken once, where its [`MachineCode`] is made, and a run takes none: a program is either machine code a compiler's crate hands over ([`Compiled::of`]) or read back by [`Compiled::deserialize`], whose caller vouches for the bytes.
///
/// **It carries the rows it imports.** The machine code names each `ffi` import and types it by its carriers alone, which cannot tell a `Nat` from an `Int`: what an import is declared as lives in the compilation, and travels here beside the code, so a run holds whatever answers a row to that row's own declaration and no caller hands the rows over a second time.
pub struct Compiled {
    pub(crate) module: Module,
    pub(crate) foreigns: ForeignStore,
}

impl Compiled {
    /// The program `code` is, beside `foreigns`, the rows the compilation that emitted it handed back.
    pub fn of(code: MachineCode, foreigns: ForeignStore) -> Self {
        Self {
            module: code.0,
            foreigns,
        }
    }

    /// Read back a precompiled program beside `foreigns`, the rows the compilation that made it handed back.
    ///
    /// # Safety
    ///
    /// [`MachineCode::deserialize`]'s: `payload` must be unmodified output of this engine's compiler.
    pub unsafe fn deserialize(payload: &[u8], foreigns: ForeignStore) -> Result<Self, String> {
        // SAFETY: the caller's, restated in this function's contract — `payload` is our own precompiled output.
        unsafe { MachineCode::deserialize(payload) }
            .map(|code| Self::of(code, foreigns))
            .map_err(|error| format!("failed to load wasm module: {error}"))
    }

    /// Run the program under `host`, returning the process exit code: `0` when `main` returns normally, otherwise the code passed to `proc_exit`. `bindings` answers rows ahead of the host, a program's own `foreign` declarations among them (pass [`ForeignBindings::empty`] where the host answers every row the program reaches).
    pub fn run<H: HostOps + Send + Sync + 'static>(
        &self,
        host: H,
        bindings: ForeignBindings,
    ) -> Result<u8, String> {
        let host = Arc::new(host);

        instantiate(
            shared_engine(),
            GUEST_HEAP,
            &self.module,
            &self.foreigns,
            bindings,
            host_rows(Arc::clone(&host)),
            host,
        )
    }

    /// [`Compiled::run`] for a program compiled as a test program, returning how its run ended: the verdict its entry answered, or the exit that left it unanswered.
    pub fn run_test<H: HostOps + Send + Sync + 'static>(
        &self,
        host: H,
        bindings: ForeignBindings,
    ) -> Result<TestEnd, String> {
        let host = Arc::new(host);

        instantiate(
            shared_engine(),
            GUEST_HEAP,
            &self.module,
            &self.foreigns,
            bindings,
            host_rows(Arc::clone(&host)),
            host,
        )
    }
}

/// Instantiate `module` against `engine` in a store whose heap grows to `heap` bytes, wire up the host imports, and run its entrypoint, returning what the run asking reads of how it ended. `foreigns` holds the rows the module's `ffi` imports are declared as, `bindings` the answers a module or an embedder gives, `host` the ones the host gives itself, and `waiter` the host that waits between a suspending program's turns and files what a row answers later.
pub(crate) fn instantiate<E: Ended>(
    engine: &Engine,
    heap: usize,
    module: &Module,
    foreigns: &ForeignStore,
    bindings: ForeignBindings,
    host: Answers,
    waiter: Arc<dyn HostOps + Send + Sync>,
) -> Result<E, String> {
    on_guest_stack(move || entered(engine, heap, module, foreigns, bindings, host, waiter))?
}

/// [`instantiate`], on the thread it is called on.
fn entered<E: Ended>(
    engine: &Engine,
    heap: usize,
    module: &Module,
    foreigns: &ForeignStore,
    bindings: ForeignBindings,
    host: Answers,
    waiter: Arc<dyn HostOps + Send + Sync>,
) -> Result<E, String> {
    let mut store = guest_store(engine, heap);

    // A module is instantiated into the program's own store, ahead of the program, and answers its rows there beside an embedder's.
    let ForeignBindings {
        answers: mut bindings,
        modules,
    } = bindings;
    for module in &modules {
        module.instantiate(&mut store, &mut bindings)?;
    }
    bindings.admit(foreigns)?;

    let mut linker = Linker::new(engine);

    // The namespaces as the emitter spells them — `curios-abi`'s, read rather than restated, so the two ends of the wire cannot drift on the one string they link on.
    const SYS: &str = Namespace::Sys.as_str();
    const FFI: &str = Namespace::Ffi.as_str();

    // Pull-based linking: the module's own import section drives what gets defined, so only the functions the program calls are wired and a demand the registry cannot meet is a named error.
    for import in module.imports() {
        match import.module() {
            SYS => match import.name() {
                // `panic` is the emitter's own refusal, `exit` with a message: the byte string it hands over is lifted as any `Bytes` operand is, and carried out as the trap the caller below renders.
                PANIC => {
                    let bytes_ref = ValType::Ref(RefType::new(
                        false,
                        HeapType::ConcreteArray(i8_array_type(engine)),
                    ));
                    let panic_type = FuncType::new(engine, [bytes_ref], []);

                    linker
                        .func_new(SYS, PANIC, panic_type, move |mut caller, params, _| {
                            let message = Vec::<u8>::lift(&mut caller, params)?;

                            Err(wasmtime::Error::from(PanicTrap(message)))
                        })
                        .map_err(|error| format!("failed to define panic: {error}"))?;
                }
                name => {
                    return Err(format!(
                        "the module imports {SYS}.{name}, and {SYS} holds {PANIC} alone"
                    ));
                }
            },
            FFI => link(
                &mut linker,
                engine,
                foreigns,
                &bindings,
                &host,
                &waiter,
                import.name(),
            )?,
            namespace => {
                return Err(format!(
                    "the module imports {}.{}, but host imports live in {SYS} or {FFI}",
                    namespace,
                    import.name(),
                ));
            }
        }
    }

    let instance = linker
        .instantiate(&mut store, module)
        .map_err(|error| format!("failed to instantiate module: {error}"))?;

    let entry = instance
        .get_func(&mut store, ENTRY)
        .ok_or_else(|| format!("failed to access {ENTRY}: the module exports none"))?;
    // A program that can suspend answers turns, advanced until it yields; one that cannot answers what it yields at once.
    let ended = match entry.ty(&store).results().len() {
        1 => entry
            .typed::<(), Option<Rooted<AnyRef>>>(&store)
            .map_err(|error| format!("failed to access {ENTRY}: {error}"))?
            .call(&mut store, ()),
        _ => run_turns(&mut store, &instance, entry, &*waiter),
    };

    match ended {
        Ok(result) => E::returned(&store, result),
        Err(error) => match error.downcast_ref::<ExitTrap>() {
            Some(ExitTrap(code)) => Ok(E::exited(*code)),
            // The trap is the root of the chain and the backtrace a context wrapped around it, so the error's own `Display` shows only the frames; the cause — a refusal's own sentence, or the engine's for a null reference or an exhausted stack — is what a reader needs first.
            None => match error.downcast_ref::<PanicTrap>() {
                Some(panic) => Err(format!("{panic}\n{error}")),
                None => Err(format!("execution failed: {}\n{error}", error.root_cause())),
            },
        },
    }
}
