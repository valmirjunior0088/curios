use {
    super::{
        Handle, HostOps, Lift, Mode, Poll, Replied, Reply, SerialFlow, SerialOp, SerialParity,
        StdioMode, anyref_array_type, bool_of, i8_array_type, longs_array_type, words_array_type,
    },
    curios_abi::{
        Carrier, ENTRY, ForeignFunction, ForeignStore, HostOp, Namespace, PANIC, WireOperand,
        WireReturn, WireType, for_each_host_op,
    },
    std::{
        collections::{HashMap, hash_map::Entry},
        error::Error,
        fmt, panic,
        sync::{Arc, LazyLock},
        thread,
    },
    wasmtime::{
        AnyRef, Caller, Config, Engine, FuncType, HeapType, Linker, Module, RefType, Rooted, Store,
        Val, ValType,
    },
};

#[cfg(test)]
mod tests;

/// Reject a malformed module, against the same engine that will run it.
///
/// Available without the `cranelift` feature: validation is a wasmparser pass over the bytes, not a compilation, so a runtime-only build can decide it. That is what lets the check live beside the engine whose feature set decides what counts as valid, rather than in whichever crate happens to link a compiler.
///
/// Returns the failure as a `String` so no `wasmtime` type appears in the signature; callers are free to treat a refusal as fatal, and `curios` does.
pub fn validate(bytes: &[u8]) -> Result<(), String> {
    Module::validate(shared_engine(), bytes).map_err(|error| error.to_string())
}

/// The stack a module's calls nest on, in bytes.
const GUEST_STACK: usize = 1 << 30;

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

        // Sixteen mebibytes: the decision and its reason are this crate's `README.md`, "The heap is sized ahead of its churn", and the figures and their retake recipe live with `chain_collection_decomposition` and `spines_collection_decomposition`.
        config.gc_heap_initial_size(16 * 1024 * 1024);

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

    wire_func_type(engine, &signature.params, &signature.returns.types())
}

/// [`host_func_type`] over the wire types alone: what an import that takes `params` and answers `results` is typed as.
fn wire_func_type(engine: &Engine, params: &[WireType], results: &[WireType]) -> FuncType {
    let bytes_ref = ValType::Ref(RefType::new(
        false,
        HeapType::ConcreteArray(i8_array_type(engine)),
    ));
    let list_ref = ValType::Ref(RefType::new(
        false,
        HeapType::ConcreteArray(anyref_array_type(engine)),
    ));
    let longs_ref = ValType::Ref(RefType::new(
        false,
        HeapType::ConcreteArray(longs_array_type(engine)),
    ));
    let words_ref = ValType::Ref(RefType::new(
        false,
        HeapType::ConcreteArray(words_array_type(engine)),
    ));
    // Raw in both directions: a host hands back a number and the guest boxes it, so nothing here needs to know a layout curios-emit defines.
    let val_type = |wire_type: &WireType| match wire_type.carrier() {
        Carrier::I64 => ValType::I64,
        Carrier::I32 => ValType::I32,
        Carrier::F64 => ValType::F64,
        Carrier::Bytes => bytes_ref.clone(),
        Carrier::Longs => longs_ref.clone(),
        Carrier::Words => words_ref.clone(),
        Carrier::Elems => list_ref.clone(),
    };

    FuncType::new(
        engine,
        params.iter().map(val_type).collect::<Vec<_>>(),
        results.iter().map(val_type).collect::<Vec<_>>(),
    )
}

/// A type-erased implementation of one import: the closure wasmtime calls, already wrapped in its [`Lift`] and [`Reply`] plumbing.
type Trampoline =
    Arc<dyn Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync>;

/// What a call comes back with, as the type of what answers it states: the wire types that cross, in order, or nothing ever.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Returned {
    Values(Vec<WireType>),
    Never,
}

impl Returned {
    /// What a declaration states of the same: its results' wire types, their labels being the guest's alone, or that its call does not return.
    fn declared(returns: &WireReturn) -> Self {
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

/// One answer to a row: the wire types it takes and what it comes back with, as its implementation states them, and how it is entered.
#[derive(Clone)]
struct Answer {
    params: Vec<WireType>,
    returned: Returned,
    trampoline: Trampoline,
}

/// Answers by the name each row is declared under: the one registry a run links from, whoever filled it. An embedder's closures and a package's modules fill one ([`ForeignBindings`]) and a host's own methods another ([`host_rows`]), and nothing tells the two kinds of answer apart but which was asked first.
#[derive(Clone, Default)]
pub(crate) struct Answers {
    rows: HashMap<String, Answer>,
    /// The names answered more than once, in the order the second answer arrived: what the link refuses, since [`ForeignBindings::define`] refuses nothing.
    twice: Vec<String>,
}

impl Answers {
    /// Answer the row declared as `name` with a typed closure: its operands and its reply are the closure's own types, which the link holds to the row.
    fn answer<Li, Lo, F>(&mut self, name: String, f: F)
    where
        Li: Lift,
        Lo: Reply,
        F: Fn(Li) -> Lo + Send + Sync + 'static,
    {
        self.answer_raw(
            name,
            Li::shape(),
            Lo::returned(),
            move |mut caller, params, results| {
                f(Li::lift(&mut caller, params)?).reply(&mut caller, results)
            },
        );
    }

    /// Answer the row declared as `name` with a trampoline that marshals for itself, stating what it takes and comes back with.
    fn answer_raw<F>(&mut self, name: String, params: Vec<WireType>, returned: Returned, f: F)
    where
        F: Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync + 'static,
    {
        let answer = Answer {
            params,
            returned,
            trampoline: Arc::new(f),
        };

        match self.rows.entry(name) {
            Entry::Occupied(taken) => self.twice.push(taken.key().clone()),
            Entry::Vacant(free) => {
                free.insert(answer);
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

    /// Replace how the row declared as `name` is entered with a trampoline that marshals for itself — how a test puts a reply in front of the guest that no checked answer would give.
    #[cfg(feature = "test-support")]
    pub(crate) fn replace_raw<F>(&mut self, name: &str, f: F)
    where
        F: Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync + 'static,
    {
        self.rows
            .get_mut(name)
            .unwrap_or_else(|| panic!("nothing here answers `{name}`"))
            .trampoline = Arc::new(f);
    }
}

/// What answers a program's rows ahead of the host: an embedder's closures and the modules a package declares, each for the row declared under a name. A run links *pull-based* — it walks the program's imports and defines exactly what the program demands, an answer here ahead of the host's own — so an import nothing answers is a clean, named refusal instead of a stranded wasmtime lookup.
///
/// Nothing is checked where an answer is recorded. What it is held to is the program's own declaration of the row, which the program carries ([`Compiled`]), so a disagreement is refused where the two meet, at the link, by one comparison for every answer.
#[derive(Clone, Default)]
pub struct ForeignBindings {
    pub(crate) answers: Answers,
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

    /// Answer the row `row` with a trampoline that marshals for itself, crossing as the row declares.
    ///
    /// [`define`](Self::define) is the door for an implementation written against Rust types, where the signature is known where the closure is written and `Lift`/`Lower` pick themselves from it. A plugin's is not: its marshalling is decided by a [`WireSignature`](curios_abi::WireSignature) read at run time, so the closure has to see the raw slots and choose. `pub(crate)` because those slots are wasmtime's vocabulary, which nothing above this crate names.
    pub(crate) fn define_raw<F>(&mut self, row: &ForeignFunction, f: F)
    where
        F: Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync + 'static,
    {
        let signature = row.signature();

        self.answers.answer_raw(
            row.name().to_string(),
            signature.params.clone(),
            Returned::declared(&signature.returns),
            f,
        );
    }
}

/// Define the `ffi` import declared as `name` into `linker`: the row as the program declares it, the first answer among `bindings`' and the host's, and the two held to each other. Who answers a row is decided here, by what was linked, and nothing in the program says which.
fn link(
    linker: &mut Linker<()>,
    engine: &Engine,
    foreigns: &ForeignStore,
    bindings: &Answers,
    host: &Answers,
    name: &str,
) -> Result<(), String> {
    let declared = foreigns.get(name).ok_or_else(|| {
        format!("the program imports `{name}`, which the rows it carries do not describe")
    })?;
    let (answer, answerer) = bindings
        .rows
        .get(name)
        .map(|answer| (answer, "a binding"))
        .or_else(|| host.rows.get(name).map(|answer| (answer, "this host")))
        .ok_or_else(|| {
            format!("no implementation for `{name}`: no module answers it, and this host does not")
        })?;

    let signature = declared.signature();
    let returned = Returned::declared(&signature.returns);

    if signature.params != answer.params || returned != answer.returned {
        return Err(format!(
            "{answerer} answers `{name}` as ({}) -> {}, and the program declares it ({}) -> {returned}",
            spelled(&answer.params),
            answer.returned,
            spelled(&signature.params),
        ));
    }

    let trampoline = Arc::clone(&answer.trampoline);

    linker
        .func_new(
            Namespace::Ffi.as_str(),
            name,
            host_func_type(engine, declared),
            move |caller, params, results| trampoline(caller, params, results),
        )
        .map(|_| ())
        .map_err(|error| format!("failed to define {name}: {error}"))
}

/// Answer every row of the table under the name the standard library declares it by ([`HostOp::declared`]). Each import lifts its operands as the row types them, measures the ones its requirements read, calls the method once they hold, and lowers the reply through [`Replied`]. Generated from the rows it binds, so no row goes unanswered, none is answered twice, and no answer's types can differ from the row's.
macro_rules! declare_host_rows {
    ($(
        $(#[doc = $doc:literal])*
        $variant:ident: fn $name:ident($($p:ident: $t:ty),* $(,)?) -> $r:ty { $($contract:tt)* }
    )*) => {
        /// What `host` answers of the rows the standard library declares.
        pub(crate) fn host_rows<H: HostOps + Send + Sync + 'static>(host: Arc<H>) -> Answers {
            let mut answers = Answers::default();

            $(
                answers.answer(
                    HostOp::$variant.declared(),
                    {
                        let host = Arc::clone(&host);

                        move |($($p,)*): ($($t,)*)| {
                            let operands = [$(WireOperand::measure(&$p)),*];

                            Replied::new(HostOp::$variant, operands, || host.$name($($p),*))
                        }
                    },
                );
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
    fn returned(store: &Store<()>, result: Option<Rooted<AnyRef>>) -> Result<Self, String>;

    /// `proc_exit` ended the run with `code`.
    fn exited(code: u8) -> Self;
}

/// A program's exit code: the code it exited with, and `0` where its entry returned. What the entry hands back there is the unit every program's description yields, which says nothing and is not read.
impl Ended for u8 {
    fn returned(_: &Store<()>, _: Option<Rooted<AnyRef>>) -> Result<Self, String> {
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
    fn returned(store: &Store<()>, result: Option<Rooted<AnyRef>>) -> Result<Self, String> {
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

/// A program's machine code as this engine holds it, ready to run.
///
/// **Holding one is what makes a run safe.** Machine code read back from bytes is run on trust, since nothing can check that the bytes are a compiler's output. So the trust is taken once, where a `Compiled` is made, and a run takes none: a program is either compiled on this engine ([`Compiled::compile`](Self::compile), where a compiler is linked) or read back by [`Compiled::deserialize`], whose caller vouches for the bytes.
///
/// **It carries the rows it imports.** The machine code names each `ffi` import and types it by its carriers alone, which cannot tell a `Nat` from an `Int`: what an import is declared as lives in the compilation, and travels here beside the code, so a run holds whatever answers a row to that row's own declaration and no caller hands the rows over a second time.
pub struct Compiled {
    pub(crate) module: Module,
    pub(crate) foreigns: ForeignStore,
}

impl Compiled {
    /// Read back a precompiled module — `.cwasm` bytes [`precompile`](super::precompile) produced for this exact wasmtime version and engine configuration — beside `foreigns`, the rows the compilation that made it handed back.
    ///
    /// # Safety
    ///
    /// `payload` must be unmodified output of [`precompile`](super::precompile) for this engine: `Module::deserialize` performs only light validation, so a foreign blob could execute arbitrary code. The keyword is what carries that obligation to each caller, which names its provenance where it takes it on — the launcher its own footer, the compiler what it precompiled or filed in the project's store.
    pub unsafe fn deserialize(payload: &[u8], foreigns: ForeignStore) -> Result<Self, String> {
        // SAFETY: the caller's, restated in this function's contract — `payload` is our own precompiled output.
        unsafe { Module::deserialize(shared_engine(), payload) }
            .map(|module| Self { module, foreigns })
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
            &self.module,
            &self.foreigns,
            bindings,
            host_rows(host),
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
            &self.module,
            &self.foreigns,
            bindings,
            host_rows(host),
        )
    }
}

/// Instantiate `module` against `engine`, wire up the host imports, and run its entrypoint, returning what the run asking reads of how it ended. `foreigns` holds the rows the module's `ffi` imports are declared as, `bindings` the answers a module or an embedder gives, and `host` the ones the host gives itself.
pub(crate) fn instantiate<E: Ended>(
    engine: &Engine,
    module: &Module,
    foreigns: &ForeignStore,
    bindings: ForeignBindings,
    host: Answers,
) -> Result<E, String> {
    on_guest_stack(move || entered(engine, module, foreigns, bindings, host))?
}

/// [`instantiate`], on the thread it is called on.
fn entered<E: Ended>(
    engine: &Engine,
    module: &Module,
    foreigns: &ForeignStore,
    bindings: ForeignBindings,
    host: Answers,
) -> Result<E, String> {
    let bindings = bindings.answers;
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

    let mut store = Store::new(engine, ());

    let instance = linker
        .instantiate(&mut store, module)
        .map_err(|error| format!("failed to instantiate module: {error}"))?;

    let function = instance
        .get_typed_func::<(), Option<Rooted<AnyRef>>>(&mut store, ENTRY)
        .map_err(|error| format!("failed to access {ENTRY}: {error}"))?;

    match function.call(&mut store, ()) {
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
