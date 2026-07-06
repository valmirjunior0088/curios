use {
    super::{
        Host, Io, Lift, Lower, Mode, Poll,
        lower::{anyref_array_type, i8_array_type},
    },
    curios_abi::{ForeignFunction, ForeignStore, WireType, sys_io},
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

/// The one wasm engine for the whole process. Building an `Engine` stands up the
/// Cranelift backend and is expensive, so it is created once and shared; `Engine`
/// is `Send + Sync` (internally reference-counted), so a `static` is sound and a
/// clone is cheap. Every module, store, and type below is created against it, so
/// they stay engine-consistent.
pub fn shared_engine() -> &'static Engine {
    static ENGINE: LazyLock<Engine> = LazyLock::new(|| {
        let mut config = Config::new();
        config.wasm_reference_types(true);
        config.wasm_function_references(true);
        config.wasm_gc(true);
        config.wasm_tail_call(true);
        // The collector is left at `Collector::Auto`: the workspace `wasmtime`
        // dependency compiles in only `gc-copying`, so `Auto` resolves to the
        // copying (semi-space) collector — bump-allocation with an in-wasm fast
        // path, so `struct.new`/`array.new` no longer round-trip through the
        // `gc_alloc_raw` libcall the deferred-reference-counting collector requires.

        Engine::new(&config).expect("failed to create wasm engine")
    });

    &ENGINE
}

/// The wasmtime type of one host import, derived from its [`WireSignature`] —
/// the same derivation `cont`'s wasm emitter applies to the module's import
/// section, so the two ends cannot drift (and wasmtime validates them against
/// each other at instantiation). Scalar params cross raw `i32`, scalar results
/// pre-boxed as i31 refs; `Bin`/`Io` are the concrete i8-array, `Lst` the
/// anyref-element array — wasmtime-universe mirrors of curios-cont's
/// `bytes_sub_type`/`elems_sub_type` (the flat rope payloads every reference
/// crosses the boundary as); keep the two ends in sync.
///
/// [`WireSignature`]: curios_abi::WireSignature
fn host_func_type(engine: &Engine, function: &ForeignFunction) -> FuncType {
    let bin_ref = ValType::Ref(RefType::new(
        false,
        HeapType::ConcreteArray(i8_array_type(engine)),
    ));
    let lst_ref = ValType::Ref(RefType::new(
        false,
        HeapType::ConcreteArray(anyref_array_type(engine)),
    ));
    let i31_ref = ValType::Ref(RefType::new(false, HeapType::I31));

    let val_type = |wire_type: &WireType, is_result: bool| match wire_type {
        WireType::Nat | WireType::Bln | WireType::Int => match is_result {
            true => i31_ref.clone(),
            false => ValType::I32,
        },
        WireType::Bin | WireType::Io => bin_ref.clone(),
        WireType::Lst(_) => lst_ref.clone(),
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

/// A type-erased host implementation: the closure wasmtime calls for one
/// import, already wrapped in its [`Lift`]/[`Lower`] plumbing. `Arc`ed so
/// [`ForeignBindings`] can keep the registry while handing wasmtime its own
/// handle.
type Trampoline =
    Arc<dyn Fn(Caller<'_, ()>, &[Val], &mut [Val]) -> wasmtime::Result<()> + Send + Sync>;

/// The host side of a foreign registry: for each [`ForeignFunction`] in a
/// store, the trampoline implementing it. `instantiate` fills the `sys`-tier
/// one from the [`Host`] trait, and links *pull-based* — it walks the
/// module's imports and defines exactly what the module demands, so an import
/// with no registered implementation is a clean, named error instead of a
/// stranded wasmtime lookup. An embedder builds its own `env`-tier one from a
/// [`ForeignStore`] returned by `compile_entrypoint`, `define`-ing each row it
/// wants to supply.
pub struct ForeignBindings {
    foreigns: ForeignStore,
    trampolines: HashMap<String, Trampoline>,
}

impl ForeignBindings {
    /// An empty registry over the rows of `foreigns`: follow with one [`define`](Self::define) per row the module will import. `instantiate` seeds the `sys`-tier registry this way from `sys_io()`; an embedder seeds the `env`-tier one from the [`ForeignStore`] that `compile_entrypoint` returned for the program.
    pub fn new(foreigns: ForeignStore) -> Self {
        Self {
            foreigns,
            trampolines: HashMap::new(),
        }
    }

    /// No bindings — the store every no-FFI caller passes through
    /// [`run_bytes`]/`instantiate`, since a program with no `foreign`
    /// declarations imports nothing under `env`.
    pub fn empty() -> Self {
        Self::new(ForeignStore::new())
    }

    /// Implement the store row named `name` with a typed closure. Every row
    /// must be implemented exactly once, and only rows can be implemented —
    /// violations are construction bugs, so they panic.
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

    /// Define the import named `name` into `linker` under `namespace`, typing
    /// it from its store row — the pull side of the registry, driven by the
    /// module's own import section.
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

/// The registry of `/sys/Io` implementations: every [`sys_io`] row bound to
/// its [`Host`] method. The store and the trait evolve together, so a row
/// without a binding here (or vice versa) is caught by `define`'s asserts the
/// first time any program runs.
fn sys_impls<H: Host + Send + Sync + 'static>(host: Arc<H>) -> ForeignBindings {
    let mut impls = ForeignBindings::new(sys_io());

    impls.define("io_read", {
        let host = host.clone();

        move |(io, count): (Io, u32)| host.read(io, count)
    });

    impls.define("io_write", {
        let host = host.clone();

        move |(io, bytes): (Io, Vec<u8>)| host.write(io, &bytes)
    });

    impls.define("io_open", {
        let host = host.clone();

        move |(path, mode): (Vec<u8>, Mode)| host.open(&path, mode)
    });

    impls.define("io_connect", {
        let host = host.clone();

        move |(io, addr): (Io, Vec<u8>)| host.connect(io, &addr)
    });

    impls.define("io_start_tls", {
        let host = host.clone();

        move |(io, sni): (Io, Vec<u8>)| host.start_tls(io, &sni)
    });

    impls.define("io_tls_server_config", {
        let host = host.clone();

        move |(cert, key): (Vec<u8>, Vec<u8>)| host.tls_server_config(&cert, &key)
    });

    impls.define("io_start_tls_server", {
        let host = host.clone();

        move |(io, cfg): (Io, Io)| host.start_tls_server(io, cfg)
    });

    impls.define("io_listen", {
        let host = host.clone();

        move |(io, backlog): (Io, u32)| host.listen(io, backlog)
    });

    impls.define("io_accept", {
        let host = host.clone();

        move |io: Io| host.accept(io)
    });

    impls.define("io_lookup", {
        let host = host.clone();

        move |(name, port): (Vec<u8>, u32)| host.lookup(&name, port)
    });

    impls.define("io_resolve", {
        let host = host.clone();

        move |handle: Io| host.resolve(handle)
    });

    impls.define("io_socket", {
        let host = host.clone();

        move |addr: Vec<u8>| host.socket(&addr)
    });

    impls.define("io_bind", {
        let host = host.clone();

        move |(io, addr): (Io, Vec<u8>)| host.bind(io, &addr)
    });

    impls.define("io_set_nonblocking", {
        let host = host.clone();

        move |(io, on): (Io, u32)| host.set_nonblocking(io, on)
    });

    impls.define("io_set_recv_timeout", {
        let host = host.clone();

        move |(io, ms): (Io, u32)| host.set_recv_timeout(io, ms)
    });

    impls.define("io_set_send_timeout", {
        let host = host.clone();

        move |(io, ms): (Io, u32)| host.set_send_timeout(io, ms)
    });

    impls.define("io_set_reuseaddr", {
        let host = host.clone();

        move |(io, on): (Io, u32)| host.set_reuseaddr(io, on)
    });

    impls.define("io_poll", {
        let host = host.clone();

        move |(handles, events, timeout): (Vec<Io>, Vec<Poll>, i32)| {
            host.poll(&handles, &events, timeout)
        }
    });

    impls.define("io_close", {
        let host = host.clone();

        move |io: Io| host.close(io)
    });

    impls.define("io_clock_wall", {
        let host = host.clone();

        move |()| host.clock_wall()
    });

    impls.define("io_clock_mono", {
        let host = host.clone();

        move |()| host.clock_mono()
    });

    impls.define("io_random", {
        let host = host.clone();

        move |count: u32| host.random(count)
    });

    impls.define("io_args", {
        let host = host.clone();

        move |()| host.args()
    });

    impls.define("io_env", {
        let host = host.clone();

        move |name: Vec<u8>| host.env(&name)
    });

    impls
}

/// A process exit requested via `Proc/exit`. Carried out of the wasm call as a
/// trap so it unwinds cleanly; `instantiate_and_run` catches it and recovers the
/// code, distinguishing a clean exit from a real trap.
#[derive(Debug)]
struct ExitTrap(i32);

impl fmt::Display for ExitTrap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "process exited with code {}", self.0)
    }
}

impl Error for ExitTrap {}

/// Run a precompiled module — `.cwasm` bytes produced by
/// `Engine::precompile_module` for this exact wasmtime version and engine
/// configuration — returning the process exit code (`0` when `main` returns
/// normally, otherwise the code passed to `Proc/exit`).
///
/// # Safety contract
///
/// `payload` must be unmodified output of `precompile_module` for this engine.
/// Provenance is guaranteed by callers: the launcher reads it from its own
/// trusted footer, and `curios` produces it in-process. `Module::deserialize`
/// performs only light validation, so a foreign blob could execute arbitrary code.
pub fn run_bytes<H: Host + Send + Sync + 'static>(
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

/// Instantiate `module` against `engine`, wire up the host imports, and run its
/// entrypoint, returning the process exit code. `bindings` supplies the
/// `env`-tier implementations for the module's own `foreign` declarations
/// (pass [`ForeignBindings::empty`] for a program that declares none). The
/// deserialize/instantiate split [`run_bytes`] factors out.
fn instantiate<H: Host + Send + Sync + 'static>(
    engine: &Engine,
    module: &Module,
    host: H,
    bindings: ForeignBindings,
) -> Result<i32, String> {
    let impls = sys_impls(Arc::new(host));
    let mut linker = Linker::new(engine);

    // Pull-based linking: the module's own import section drives what gets
    // defined, so only the functions the program calls are wired and a demand
    // the registry cannot meet is a named error.
    for import in module.imports() {
        match import.module() {
            curios_abi::NAMESPACE_SYS => match import.name() {
                // `exit` never returns: it traps with the code, which the
                // caller below catches. A registry trampoline cannot trap, so
                // it is wired directly, outside the store.
                "io_exit" => {
                    let io_exit_type = FuncType::new(engine, [ValType::I32], []);

                    linker
                        .func_new(
                            curios_abi::NAMESPACE_SYS,
                            "io_exit",
                            io_exit_type,
                            move |_caller, params, _| {
                                let code = match params.first() {
                                    Some(wasmtime::Val::I32(code)) => *code,
                                    _ => 0,
                                };

                                Err(wasmtime::Error::from(ExitTrap(code)))
                            },
                        )
                        .map_err(|error| format!("failed to define io_exit: {error}"))?;
                }
                name => impls.link(&mut linker, engine, curios_abi::NAMESPACE_SYS, name)?,
            },
            curios_abi::NAMESPACE_ENV => bindings.link(
                &mut linker,
                engine,
                curios_abi::NAMESPACE_ENV,
                import.name(),
            )?,
            namespace => {
                return Err(format!(
                    "the module imports {}.{}, but host imports live in {} or {}",
                    namespace,
                    import.name(),
                    curios_abi::NAMESPACE_SYS,
                    curios_abi::NAMESPACE_ENV
                ));
            }
        }
    }

    let mut store = Store::new(engine, ());

    let instance = linker
        .instantiate(&mut store, module)
        .map_err(|error| format!("failed to instantiate module: {error}"))?;

    let function = instance
        .get_typed_func::<(), Rooted<AnyRef>>(&mut store, curios_abi::MAIN_EXPORT)
        .map_err(|error| format!("failed to access {}: {error}", curios_abi::MAIN_EXPORT))?;

    match function.call(&mut store, ()) {
        Ok(_) => Ok(0),
        Err(error) => match error.downcast_ref::<ExitTrap>() {
            Some(ExitTrap(code)) => Ok(*code),
            None => Err(format!("execution failed: {error}")),
        },
    }
}
