use {
    super::{
        Handle, HostOps, Lift, Lower, Mode, Poll,
        lower::{Replied, anyref_array_type, i8_array_type, longs_array_type, words_array_type},
    },
    curios_abi::{
        ENTRY, ForeignFunction, ForeignStore, HostOp, Namespace, PANIC, WireLeaf, WireType,
        host_ops,
    },
    std::{
        collections::HashMap,
        error::Error,
        fmt,
        sync::{Arc, LazyLock},
    },
    wasmtime::{
        AnyRef, Caller, Config, Engine, FuncType, HeapType, Linker, Module, RefType, Rooted, Store,
        Val, ValType,
    },
};

/// Reject a malformed module, against the same engine that will run it.
///
/// Available without the `cranelift` feature: validation is a wasmparser pass over the bytes, not a compilation, so a runtime-only build can decide it. That is what lets the check live beside the engine whose feature set decides what counts as valid, rather than in whichever crate happens to link a compiler.
///
/// Returns the failure as a `String` so no `wasmtime` type appears in the signature; callers are free to treat a refusal as fatal, and `curios` does.
pub fn validate(bytes: &[u8]) -> Result<(), String> {
    Module::validate(shared_engine(), bytes).map_err(|error| error.to_string())
}

/// The one wasm engine for the whole process. Building an `Engine` stands up the Cranelift backend and is expensive, so it is created once and shared; `Engine` is `Send + Sync` (internally reference-counted), so a `static` is sound and a clone is cheap. Every module, store, and type below is created against it, so they stay engine-consistent.
pub fn shared_engine() -> &'static Engine {
    static ENGINE: LazyLock<Engine> = LazyLock::new(|| {
        let mut config = Config::new();
        config.wasm_reference_types(true);
        config.wasm_function_references(true);
        config.wasm_gc(true);
        config.wasm_tail_call(true);
        // The collector is left at `Collector::Auto`: the workspace `wasmtime` dependency compiles in only `gc-copying`, so `Auto` resolves to the copying (semi-space) collector — bump-allocation with an in-wasm fast path, so `struct.new`/`array.new` no longer round-trip through the `gc_alloc_raw` libcall the deferred-reference-counting collector requires.

        // Sixteen mebibytes, because the engine's own growth policy only ever reacts to a single allocation not fitting after a collection — so under death-birth churn the heap parks within a doubling of the live set and the collector recopies the survivors continually. Sixteen is the smallest measured size on the good side of that knee for both churn-class workloads, and the cold-page tax that argues for smaller never registers until far above it; commit is lazy, so a program that allocates little touches little. The figures and the retake recipe live with `chain_collection_decomposition` and `spines_collection_decomposition`; the decision is this crate's `README.md`, "The heap is sized ahead of its churn".
        config.gc_heap_initial_size(16 * 1024 * 1024);

        // Under the `profile` feature, symbolicate emitted code for a sampling profiler: wasmtime writes `/tmp/perf-<pid>.map`, which `samply` and `perf` read to attribute samples to the `$func/<N>$hint` names `curios-emit` emitted. Without it every sample landing in emitted wasm resolves to a bare address — which is what made the first runtime profile of a Curios program unreadable, its two largest buckets symbolicating into an unrelated host function's prologue.
        //
        // This is the guest-side half of the same flag `curios-profile` uses for the compiler, so one feature profiles both ends of a compile-and-run. It selects how compiled code is registered with the host rather than how it is compiled, so a `.cwasm` produced without it still deserializes against an engine built with it.
        #[cfg(feature = "profile")]
        config.profiler(wasmtime::ProfilingStrategy::PerfMap);

        Engine::new(&config).expect("failed to create wasm engine")
    });

    &ENGINE
}

/// The wasmtime type of one host import, derived from its `WireSignature` — the same derivation `curios-emit` applies to the module's import section, so the two ends cannot drift (and wasmtime validates them against each other at instantiation). Scalars cross raw in both directions — `i64` for a `Nat` or `Int`, `i32` for a `Bool` or a `Byte`, `f64` for an `Flt` — and the guest boxes a result; `Bytes`/`Bits`/`Handle` are the concrete i8-array, a list of `Nat` or `Int` the `i64` longs array, a list of `Bool` the `i32` words array, any other `List` the anyref-element array — wasmtime-universe mirrors of curios-emit's `bytes_sub_type`/`longs_sub_type`/`words_sub_type`/`elems_sub_type` (the flat payloads every reference crosses the boundary as); keep the two ends in sync.
fn host_func_type(engine: &Engine, function: &ForeignFunction) -> FuncType {
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
    let val_type = |wire_type: &WireType| match wire_type {
        WireType::Nat | WireType::Int => ValType::I64,
        WireType::Bool | WireType::Byte => ValType::I32,
        WireType::Flt => ValType::F64,
        WireType::Bytes | WireType::Bits | WireType::Handle => bytes_ref.clone(),
        WireType::List(WireLeaf::Nat | WireLeaf::Int) => longs_ref.clone(),
        WireType::List(WireLeaf::Bool) => words_ref.clone(),
        WireType::List(_) => list_ref.clone(),
    };

    let signature = &function.signature();

    FuncType::new(
        engine,
        signature
            .params
            .iter()
            .map(|(_, wire_type)| val_type(wire_type))
            .collect::<Vec<_>>(),
        signature
            .results
            .iter()
            .map(|(_, wire_type)| val_type(&wire_type))
            .collect::<Vec<_>>(),
    )
}

/// A type-erased host implementation: the closure wasmtime calls for one import, already wrapped in its [`Lift`]/[`Lower`] plumbing. `Arc`ed so [`ForeignBindings`] can keep the registry while handing wasmtime its own handle.
type Trampoline =
    Arc<dyn Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync>;

/// The host side of a foreign registry: for each [`ForeignFunction`] in a store, the trampoline implementing it. `instantiate` fills the `sys`-tier one from the `Host` trait, and links *pull-based* — it walks the module's imports and defines exactly what the module demands, so an import with no registered implementation is a clean, named error instead of a stranded wasmtime lookup. An embedder builds its own `ffi`-tier one from a [`ForeignStore`] returned by `compile_entrypoint`, `define`-ing each row it wants to supply.
pub struct ForeignBindings {
    foreigns: ForeignStore,
    trampolines: HashMap<String, Trampoline>,
}

impl ForeignBindings {
    /// An empty registry over the rows of `foreigns`: follow with one [`define`](Self::define) per row the module will import. `instantiate` seeds the `sys`-tier registry this way from `host_ops()`; an embedder seeds the `ffi`-tier one from the [`ForeignStore`] that `compile_entrypoint` returned for the program.
    pub fn new(foreigns: ForeignStore) -> Self {
        Self {
            foreigns,
            trampolines: HashMap::new(),
        }
    }

    /// No bindings — the store every no-FFI caller passes through [`run_bytes`]/`instantiate`, since a program with no `foreign` declarations imports nothing under `ffi`.
    pub fn empty() -> Self {
        Self::new(ForeignStore::new())
    }

    /// Implement the store row named `name` with a typed closure. A `foreign` declaration's row is named by its fully qualified name (e.g. `/foo/double`). Every row must be implemented exactly once, and only rows can be implemented — violations are construction bugs, so they panic.
    pub fn define<Li, Lo, F>(&mut self, name: &str, f: F)
    where
        Li: Lift,
        Lo: Lower,
        F: Fn(Li) -> Lo + Send + Sync + 'static,
    {
        assert!(
            self.foreigns.get(name).is_some(),
            "'{name}' is not in the foreign store"
        );

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

/// Bind every row of the table to its [`HostOps`] method: each import lifts its operands as the row types them, calls the method, and lowers the reply through [`Replied`]. Generated from the rows it binds, so no row goes unbound, none is bound twice, and no binding's types can differ from the row's.
macro_rules! declare_sys_impls {
    ($(
        $(#[doc = $doc:literal])*
        $variant:ident: fn $name:ident($($p:ident: $t:ty),* $(,)?) -> $r:ty as $subject:ident / $label:ident { $($contract:tt)* }
    )*) => {
        /// The registry of builtin implementations: every [`host_ops()`] row bound to its [`HostOps`] method.
        fn sys_impls<H: HostOps + Send + Sync + 'static>(host: Arc<H>) -> ForeignBindings {
            let mut impls = ForeignBindings::new(host_ops());

            $(
                impls.define(HostOp::$variant.name(), {
                    let host = host.clone();

                    move |($($p,)*): ($($t,)*)| Replied(host.$name($($p),*))
                });
            )*

            impls
        }
    };
}

host_ops!(declare_sys_impls);

/// A process exit requested via `proc/exit`. Carried out of the wasm call as a trap so it unwinds cleanly; `instantiate` catches it and recovers the code, distinguishing a clean exit from a real trap. Made only when a [`Termination`](curios_abi::Termination) reply is encoded, so a host method answering the row cannot return into the guest.
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

/// Run a precompiled module — `.cwasm` bytes produced by `Engine::precompile_module` for this exact wasmtime version and engine configuration — returning the process exit code (`0` when `main` returns normally, otherwise the code passed to `proc/exit`).
///
/// # Safety
///
/// `payload` must be unmodified output of [`precompile`](super::precompile) for this engine: `Module::deserialize` performs only light validation, so a foreign blob could execute arbitrary code. The keyword is what carries that obligation to each caller, which names its provenance where it takes it on — the launcher its own footer, the compiler what it just compiled or filed in the project's store.
pub unsafe fn run_bytes<H: HostOps + Send + Sync + 'static>(
    payload: &[u8],
    host: H,
    bindings: ForeignBindings,
) -> Result<u8, String> {
    let engine = shared_engine();

    // SAFETY: the caller's, restated in this function's contract — `payload` is our own precompiled output.
    let module = unsafe { Module::deserialize(engine, payload) }
        .map_err(|error| format!("failed to load wasm module: {error}"))?;

    instantiate(engine, &module, host, bindings)
}

/// Instantiate `module` against `engine`, wire up the host imports, and run its entrypoint, returning the process exit code. `bindings` supplies the `ffi`-tier implementations for the module's own `foreign` declarations (pass [`ForeignBindings::empty`] for a program that declares none). The deserialize/instantiate split [`run_bytes`] factors out.
fn instantiate<H: HostOps + Send + Sync + 'static>(
    engine: &Engine,
    module: &Module,
    host: H,
    bindings: ForeignBindings,
) -> Result<u8, String> {
    let impls = sys_impls(Arc::new(host));
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
                name => impls.link(&mut linker, engine, Namespace::Sys, name)?,
            },
            FFI => bindings.link(&mut linker, engine, Namespace::Ffi, import.name())?,
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
        Ok(_) => Ok(0),
        Err(error) => match error.downcast_ref::<ExitTrap>() {
            Some(ExitTrap(code)) => Ok(*code),
            // The trap is the root of the chain and the backtrace a context wrapped around it, so the error's own `Display` shows only the frames; the cause — a refusal's own sentence, or the engine's for a null reference or an exhausted stack — is what a reader needs first.
            None => match error.downcast_ref::<PanicTrap>() {
                Some(panic) => Err(format!("{panic}\n{error}")),
                None => Err(format!("execution failed: {}\n{error}", error.root_cause())),
            },
        },
    }
}
