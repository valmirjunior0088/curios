use {
    super::{
        Handle, HostOps, Lift, Lower, Mode, Poll,
        lower::{anyref_array_type, i8_array_type},
    },
    curios_abi::{ForeignFunction, ForeignStore, WireType, host_ops},
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

        // Under the `profile` feature, symbolicate emitted code for a sampling profiler: wasmtime writes `/tmp/perf-<pid>.map`, which `samply` and `perf` read to attribute samples to the `$func/<N>$hint` names `curios-cont` emitted. Without it every sample landing in emitted wasm resolves to a bare address — which is what made the first runtime profile of a Curios program unreadable, its two largest buckets symbolicating into an unrelated host function's prologue.
        //
        // This is the guest-side half of the same flag `curios-profile` uses for the compiler, so one feature profiles both ends of a compile-and-run. It selects how compiled code is registered with the host rather than how it is compiled, so a `.cwasm` produced without it still deserializes against an engine built with it.
        #[cfg(feature = "profile")]
        config.profiler(wasmtime::ProfilingStrategy::PerfMap);

        Engine::new(&config).expect("failed to create wasm engine")
    });

    &ENGINE
}

/// The wasmtime type of one host import, derived from its `WireSignature` — the same derivation `cont`'s wasm emitter applies to the module's import section, so the two ends cannot drift (and wasmtime validates them against each other at instantiation). Scalar params cross raw `i32`, scalar results pre-boxed as i31 refs; `Bytes`/`Handle` are the concrete i8-array, `List` the anyref-element array — wasmtime-universe mirrors of curios-cont's `bytes_sub_type`/`elems_sub_type` (the flat rope payloads every reference crosses the boundary as); keep the two ends in sync.
fn host_func_type(engine: &Engine, function: &ForeignFunction) -> FuncType {
    let bytes_ref = ValType::Ref(RefType::new(
        false,
        HeapType::ConcreteArray(i8_array_type(engine)),
    ));
    let list_ref = ValType::Ref(RefType::new(
        false,
        HeapType::ConcreteArray(anyref_array_type(engine)),
    ));
    let i31_ref = ValType::Ref(RefType::new(false, HeapType::I31));

    let val_type = |wire_type: &WireType, is_result: bool| match wire_type {
        WireType::Nat | WireType::Bool | WireType::Int => match is_result {
            true => i31_ref.clone(),
            false => ValType::I32,
        },
        WireType::Bytes | WireType::Handle => bytes_ref.clone(),
        WireType::List(_) => list_ref.clone(),
    };

    let signature = &function.signature;

    FuncType::new(
        engine,
        signature
            .params
            .iter()
            .map(|(_, wire_type)| val_type(wire_type, false))
            .collect::<Vec<_>>(),
        signature
            .results
            .iter()
            .map(|(_, wire_type)| val_type(wire_type, true))
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

    /// Define the import named `name` into `linker` under `namespace`, typing it from its store row — the pull side of the registry, driven by the module's own import section.
    fn link(
        &self,
        linker: &mut Linker<()>,
        engine: &Engine,
        namespace: &str,
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
                namespace,
                name,
                host_func_type(engine, function),
                move |caller, params, results| trampoline(caller, params, results),
            )
            .map(|_| ())
            .map_err(|error| format!("failed to define {name}: {error}"))
    }
}

/// The registry of builtin implementations: every [`host_ops`] row bound to its [`HostOps`] method. The store and the trait are generated from one authored list in `curios-abi`, and these hand-written bindings are cross-checked against both — each `define` name must be a real store row (asserted), and each method call must match the trait (compiler-checked) — so the three stay in agreement without a fourth independent spelling.
fn sys_impls<H: HostOps + Send + Sync + 'static>(host: Arc<H>) -> ForeignBindings {
    let mut impls = ForeignBindings::new(host_ops());

    impls.define("read", {
        let host = host.clone();

        move |(handle, count): (Handle, u32)| host.read(handle, count)
    });

    impls.define("write", {
        let host = host.clone();

        move |(handle, bytes): (Handle, Vec<u8>)| host.write(handle, &bytes)
    });

    impls.define("open", {
        let host = host.clone();

        move |(path, mode): (Vec<u8>, Mode)| host.open(&path, mode)
    });

    impls.define("connect", {
        let host = host.clone();

        move |(handle, addr): (Handle, Vec<u8>)| host.connect(handle, &addr)
    });

    impls.define("start_tls", {
        let host = host.clone();

        move |(handle, sni): (Handle, Vec<u8>)| host.start_tls(handle, &sni)
    });

    impls.define("tls_server_config", {
        let host = host.clone();

        move |(cert, key): (Vec<u8>, Vec<u8>)| host.tls_server_config(&cert, &key)
    });

    impls.define("start_tls_server", {
        let host = host.clone();

        move |(handle, cfg): (Handle, Handle)| host.start_tls_server(handle, cfg)
    });

    impls.define("listen", {
        let host = host.clone();

        move |(handle, backlog): (Handle, u32)| host.listen(handle, backlog)
    });

    impls.define("accept", {
        let host = host.clone();

        move |handle: Handle| host.accept(handle)
    });

    impls.define("lookup", {
        let host = host.clone();

        move |(name, port): (Vec<u8>, u32)| host.lookup(&name, port)
    });

    impls.define("resolve", {
        let host = host.clone();

        move |handle: Handle| host.resolve(handle)
    });

    impls.define("socket", {
        let host = host.clone();

        move |addr: Vec<u8>| host.socket(&addr)
    });

    impls.define("bind", {
        let host = host.clone();

        move |(handle, addr): (Handle, Vec<u8>)| host.bind(handle, &addr)
    });

    impls.define("set_nonblocking", {
        let host = host.clone();

        move |(handle, on): (Handle, u32)| host.set_nonblocking(handle, on)
    });

    impls.define("set_recv_timeout", {
        let host = host.clone();

        move |(handle, ms): (Handle, u32)| host.set_recv_timeout(handle, ms)
    });

    impls.define("set_send_timeout", {
        let host = host.clone();

        move |(handle, ms): (Handle, u32)| host.set_send_timeout(handle, ms)
    });

    impls.define("set_reuseaddr", {
        let host = host.clone();

        move |(handle, on): (Handle, u32)| host.set_reuseaddr(handle, on)
    });

    impls.define("poll", {
        let host = host.clone();

        move |(handles, events, timeout): (Vec<Handle>, Vec<Poll>, i32)| {
            host.poll(&handles, &events, timeout)
        }
    });

    impls.define("close", {
        let host = host.clone();

        move |handle: Handle| host.close(handle)
    });

    impls.define("clock_wall", {
        let host = host.clone();

        move |()| host.clock_wall()
    });

    impls.define("clock_mono", {
        let host = host.clone();

        move |()| host.clock_mono()
    });

    impls.define("random", {
        let host = host.clone();

        move |count: u32| host.random(count)
    });

    impls.define("args", {
        let host = host.clone();

        move |()| host.args()
    });

    impls.define("env", {
        let host = host.clone();

        move |name: Vec<u8>| host.env(&name)
    });

    impls
}

/// A process exit requested via `proc/exit`. Carried out of the wasm call as a trap so it unwinds cleanly; `instantiate_and_run` catches it and recovers the code, distinguishing a clean exit from a real trap.
#[derive(Debug)]
struct ExitTrap(i32);

impl fmt::Display for ExitTrap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "process exited with code {}", self.0)
    }
}

impl Error for ExitTrap {}

/// Run a precompiled module — `.cwasm` bytes produced by `Engine::precompile_module` for this exact wasmtime version and engine configuration — returning the process exit code (`0` when `main` returns normally, otherwise the code passed to `proc/exit`).
///
/// # Safety contract
///
/// `payload` must be unmodified output of `precompile_module` for this engine. Provenance is guaranteed by callers: the launcher reads it from its own trusted footer, and `curios` produces it in-process. `Module::deserialize` performs only light validation, so a foreign blob could execute arbitrary code.
pub fn run_bytes<H: HostOps + Send + Sync + 'static>(
    payload: &[u8],
    host: H,
    bindings: ForeignBindings,
) -> Result<i32, String> {
    let engine = shared_engine();

    // SAFETY: see the contract above — `payload` is our own precompiled output.
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
) -> Result<i32, String> {
    let impls = sys_impls(Arc::new(host));
    let mut linker = Linker::new(engine);

    // Pull-based linking: the module's own import section drives what gets defined, so only the functions the program calls are wired and a demand the registry cannot meet is a named error.
    for import in module.imports() {
        match import.module() {
            "sys" => match import.name() {
                // `exit` never returns: it traps with the code, which the caller below catches. A registry trampoline cannot trap, so it is wired directly, outside the store.
                "exit" => {
                    let exit_type = FuncType::new(engine, [ValType::I32], []);

                    linker
                        .func_new("sys", "exit", exit_type, move |_caller, params, _| {
                            let code = match params.first() {
                                Some(wasmtime::Val::I32(code)) => *code,
                                _ => 0,
                            };

                            Err(wasmtime::Error::from(ExitTrap(code)))
                        })
                        .map_err(|error| format!("failed to define exit: {error}"))?;
                }
                name => impls.link(&mut linker, engine, "sys", name)?,
            },
            "ffi" => bindings.link(&mut linker, engine, "ffi", import.name())?,
            namespace => {
                return Err(format!(
                    "the module imports {}.{}, but host imports live in {} or {}",
                    namespace,
                    import.name(),
                    "sys",
                    "ffi"
                ));
            }
        }
    }

    let mut store = Store::new(engine, ());

    let instance = linker
        .instantiate(&mut store, module)
        .map_err(|error| format!("failed to instantiate module: {error}"))?;

    let function = instance
        .get_typed_func::<(), Rooted<AnyRef>>(&mut store, "func/main")
        .map_err(|error| format!("failed to access func/main: {error}"))?;

    match function.call(&mut store, ()) {
        Ok(_) => Ok(0),
        Err(error) => match error.downcast_ref::<ExitTrap>() {
            Some(ExitTrap(code)) => Ok(*code),
            None => Err(format!("execution failed: {error}")),
        },
    }
}
