use {
    super::{
        Handle, HostOps, Lift, Lower, Mode, Poll, Replied, SerialFlow, SerialOp, SerialParity,
        StdioMode, anyref_array_type, bool_of, i8_array_type, longs_array_type, words_array_type,
    },
    curios_abi::{
        Carrier, ENTRY, ForeignFunction, ForeignStore, HostOp, Namespace, PANIC, WireOperand,
        WireType, for_each_host_op,
    },
    std::{
        collections::HashMap,
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

/// A type-erased host implementation: the closure wasmtime calls for one import, already wrapped in its [`Lift`]/[`Lower`] plumbing. `Arc`ed so [`ForeignBindings`] can keep the registry while handing wasmtime its own handle.
type Trampoline =
    Arc<dyn Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync>;

/// An embedder's side of a foreign registry: for each [`ForeignFunction`] of a store it answers, the trampoline implementing it. An embedder builds one from the [`ForeignStore`] `compile_entrypoint` returned, `define`-ing each row it wants to supply, and a module's exports are bound through the same door. `instantiate` links *pull-based* — it walks the module's imports and defines exactly what the module demands, a binding here ahead of the host's own answer — so an import nothing answers is a clean, named error instead of a stranded wasmtime lookup.
pub struct ForeignBindings {
    foreigns: ForeignStore,
    trampolines: HashMap<String, Trampoline>,
}

impl ForeignBindings {
    /// An empty registry over the rows of `foreigns`, the [`ForeignStore`] that `compile_entrypoint` returned for the program: follow with one [`define`](Self::define) per row to answer.
    pub fn new(foreigns: ForeignStore) -> Self {
        Self {
            foreigns,
            trampolines: HashMap::new(),
        }
    }

    /// No bindings: every row the program reaches is left to the host, which answers the standard library's.
    pub fn empty() -> Self {
        Self::new(ForeignStore::new())
    }

    /// Implement the store row named `name` with a typed closure. A `foreign` declaration's row is named by its fully qualified name (e.g. `/foo/double`). Every row must be implemented exactly once, only rows can be implemented, and a closure's operand and result types must cross as the row's do — violations are construction bugs, so they panic.
    pub fn define<Li, Lo, F>(&mut self, name: &str, f: F)
    where
        Li: Lift,
        Lo: Lower,
        F: Fn(Li) -> Lo + Send + Sync + 'static,
    {
        let signature = self
            .foreigns
            .get(name)
            .unwrap_or_else(|| panic!("'{name}' is not in the foreign store"))
            .signature();
        let params = &signature.params;
        let results = signature.returns.types();

        assert_eq!(&Li::shape(), params, "'{name}' takes {params:?}");
        assert_eq!(Lo::shape(), results, "'{name}' answers {results:?}");

        let trampoline: Trampoline = Arc::new(move |mut caller, params, results| {
            f(Li::lift(&mut caller, params)?).lower(&mut caller, results)
        });

        assert!(
            self.trampolines
                .insert(name.to_string(), trampoline)
                .is_none(),
            "'{name}' is implemented twice"
        );
    }

    /// Implement the store row named `name` with a trampoline that marshals for itself.
    ///
    /// [`define`](Self::define) is the door for a host implementation written against Rust types, where the signature is known where the closure is written and `Lift`/`Lower` pick themselves from it. A plugin's is not: its marshalling is decided by a [`WireSignature`](curios_abi::WireSignature) read at run time, so the closure has to see the raw slots and choose. `pub(crate)` because those slots are wasmtime's vocabulary, which nothing above this crate names.
    pub(crate) fn define_raw<F>(&mut self, name: &str, f: F)
    where
        F: Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync + 'static,
    {
        assert!(
            self.foreigns.get(name).is_some(),
            "'{name}' is not in the foreign store"
        );

        assert!(
            self.trampolines
                .insert(name.to_string(), Arc::new(f))
                .is_none(),
            "'{name}' is implemented twice"
        );
    }

    /// Whether a binding answers the row named `name`: a module's, or an embedder's.
    fn answers(&self, name: &str) -> bool {
        self.trampolines.contains_key(name)
    }

    /// The row named `name`, as the program's units declared it.
    fn row(&self, name: &str) -> Option<&Arc<ForeignFunction>> {
        self.foreigns.get(name)
    }

    /// Define the import named `name` into `linker` under `namespace`, typing it from its store row — the pull side of the registry, driven by the module's own import section.
    fn link(
        &self,
        linker: &mut Linker<()>,
        engine: &Engine,
        namespace: Namespace,
        name: &str,
    ) -> Result<(), String> {
        let (function, trampoline) = self
            .foreigns
            .get(name)
            .zip(self.trampolines.get(name))
            .ok_or_else(|| format!("no host implementation registered for {namespace}.{name}"))?;

        let trampoline = Arc::clone(trampoline);

        linker
            .func_new(
                namespace.as_str(),
                name,
                host_func_type(engine, function),
                move |caller, params, results| trampoline(caller, params, results),
            )
            .map(|_| ())
            .map_err(|error| format!("failed to define {name}: {error}"))
    }
}

/// What a host answers of the rows a unit declares, by the name each is declared under: the wire types its implementation takes and answers, as its Rust types state them, and its trampoline.
///
/// A program's `ffi` import is linked here where no binding answers it, so a row the standard library declares is answered by the host as a row a package declares is answered by its module. An implementation is held to the program's declaration where it is linked, and a disagreement is a refusal naming the row.
#[derive(Default)]
pub(crate) struct Provided {
    rows: HashMap<String, (Vec<WireType>, Vec<WireType>, Trampoline)>,
}

impl Provided {
    /// Answer the row declared as `name` with a typed closure, as [`ForeignBindings::define`] takes one.
    fn answer<Li, Lo, F>(&mut self, name: String, f: F)
    where
        Li: Lift,
        Lo: Lower,
        F: Fn(Li) -> Lo + Send + Sync + 'static,
    {
        let trampoline: Trampoline = Arc::new(move |mut caller, params, results| {
            f(Li::lift(&mut caller, params)?).lower(&mut caller, results)
        });

        self.rows
            .insert(name, (Li::shape(), Lo::shape(), trampoline));
    }

    /// Replace what answers the row declared as `name` with a trampoline that marshals for itself — how a test puts a reply in front of the guest that no checked answer would give.
    #[cfg(feature = "test-support")]
    pub(crate) fn replace_raw<F>(&mut self, name: &str, f: F)
    where
        F: Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync + 'static,
    {
        let (_, _, trampoline) = self
            .rows
            .get_mut(name)
            .unwrap_or_else(|| panic!("this host answers no `{name}`"));

        *trampoline = Arc::new(f);
    }

    /// Define the `ffi` import named `name` into `linker`, held to `declared` where the program's rows describe it.
    fn link(
        &self,
        linker: &mut Linker<()>,
        engine: &Engine,
        name: &str,
        declared: Option<&Arc<ForeignFunction>>,
    ) -> Result<(), String> {
        let Some((params, results, trampoline)) = self.rows.get(name) else {
            return Err(format!(
                "no implementation for `{name}`: no module answers it, and this host does not"
            ));
        };

        if let Some(function) = declared {
            let signature = function.signature();
            let spelled = |types: &[WireType]| {
                types
                    .iter()
                    .map(WireType::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            };

            if signature.params != *params || signature.returns.types() != *results {
                return Err(format!(
                    "this host answers `{name}` as ({}) -> ({}), and the program declares it ({}) -> ({})",
                    spelled(params),
                    spelled(results),
                    spelled(&signature.params),
                    spelled(&signature.returns.types()),
                ));
            }
        }

        let trampoline = Arc::clone(trampoline);

        linker
            .func_new(
                Namespace::Ffi.as_str(),
                name,
                wire_func_type(engine, params, results),
                move |caller, params, results| trampoline(caller, params, results),
            )
            .map(|_| ())
            .map_err(|error| format!("failed to define {name}: {error}"))
    }
}

/// Answer every row of the table under the name the standard library declares it by ([`HostOp::declared`]). Each import lifts its operands as the row types them, measures the ones its requirements read, calls the method once they hold, and lowers the reply through [`Replied`]. Generated from the rows it binds, so no row goes unanswered, none is answered twice, and no answer's types can differ from the row's.
macro_rules! declare_host_rows {
    ($(
        $(#[doc = $doc:literal])*
        $variant:ident: fn $name:ident($($p:ident: $t:ty),* $(,)?) -> $r:ty { $($contract:tt)* }
    )*) => {
        /// What `host` answers of the rows the standard library declares.
        pub(crate) fn host_rows<H: HostOps + Send + Sync + 'static>(host: Arc<H>) -> Provided {
            let mut provided = Provided::default();

            $(
                provided.answer(
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

            provided
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
pub struct Compiled(pub(crate) Module);

impl Compiled {
    /// Read back a precompiled module — `.cwasm` bytes [`precompile`](super::precompile) produced for this exact wasmtime version and engine configuration.
    ///
    /// # Safety
    ///
    /// `payload` must be unmodified output of [`precompile`](super::precompile) for this engine: `Module::deserialize` performs only light validation, so a foreign blob could execute arbitrary code. The keyword is what carries that obligation to each caller, which names its provenance where it takes it on — the launcher its own footer, the compiler what it precompiled or filed in the project's store.
    pub unsafe fn deserialize(payload: &[u8]) -> Result<Self, String> {
        // SAFETY: the caller's, restated in this function's contract — `payload` is our own precompiled output.
        unsafe { Module::deserialize(shared_engine(), payload) }
            .map(Self)
            .map_err(|error| format!("failed to load wasm module: {error}"))
    }

    /// Run the program under `host`, returning the process exit code: `0` when `main` returns normally, otherwise the code passed to `proc_exit`. `bindings` implements the program's own `foreign` declarations (pass [`ForeignBindings::empty`] for a program that declares none).
    pub fn run<H: HostOps + Send + Sync + 'static>(
        &self,
        host: H,
        bindings: ForeignBindings,
    ) -> Result<u8, String> {
        let host = Arc::new(host);

        instantiate(shared_engine(), &self.0, bindings, host_rows(host))
    }

    /// [`Compiled::run`] for a program compiled as a test program, returning how its run ended: the verdict its entry answered, or the exit that left it unanswered.
    pub fn run_test<H: HostOps + Send + Sync + 'static>(
        &self,
        host: H,
        bindings: ForeignBindings,
    ) -> Result<TestEnd, String> {
        let host = Arc::new(host);

        instantiate(shared_engine(), &self.0, bindings, host_rows(host))
    }
}

/// Instantiate `module` against `engine`, wire up the host imports, and run its entrypoint, returning what the run asking reads of how it ended. `bindings` supplies the rows a module or an embedder answers, and `provided` the ones the host answers itself.
pub(crate) fn instantiate<E: Ended>(
    engine: &Engine,
    module: &Module,
    bindings: ForeignBindings,
    provided: Provided,
) -> Result<E, String> {
    on_guest_stack(move || entered(engine, module, bindings, provided))?
}

/// [`instantiate`], on the thread it is called on.
fn entered<E: Ended>(
    engine: &Engine,
    module: &Module,
    bindings: ForeignBindings,
    provided: Provided,
) -> Result<E, String> {
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
            // A module's or an embedder's answer first, and the host's own where there is none: who answers a row is decided here, by what was linked, and nothing in the program says which.
            FFI => match bindings.answers(import.name()) {
                true => bindings.link(&mut linker, engine, Namespace::Ffi, import.name())?,
                false => provided.link(
                    &mut linker,
                    engine,
                    import.name(),
                    bindings.row(import.name()),
                )?,
            },
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
