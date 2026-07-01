use {
    super::{Host, Io, Lift, Lower, Mode, Poll},
    curios_abi::{HostFunction, WireType},
    std::{
        error::Error,
        fmt,
        sync::{Arc, LazyLock},
    },
    wasmtime::{
        AnyRef, ArrayType, Config, Engine, FieldType, FuncType, HeapType, Linker, Module,
        Mutability, RefType, Rooted, StorageType, Store, ValType,
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

        Engine::new(&config).expect("failed to create wasm engine")
    });

    &ENGINE
}

pub fn define_import<Li, Lo, F>(
    linker: &mut Linker<()>,
    name: &str,
    ty: FuncType,
    f: F,
) -> Result<(), String>
where
    Li: Lift,
    Lo: Lower,
    F: Fn(Li) -> Lo + Send + Sync + 'static,
{
    linker
        .func_new("env", name, ty, move |mut caller, params, results| {
            f(Li::lift(&mut caller, params)?).lower(&mut caller, results)
        })
        .map(|_| ())
        .map_err(|error| format!("failed to define {name}: {error}"))
}

/// The wasmtime type of one host import, derived from its [`WireSignature`] —
/// the same derivation `cont`'s wasm emitter applies to the module's import
/// section, so the two ends cannot drift (and wasmtime validates them against
/// each other at instantiation). Scalar params cross raw `i32`, scalar results
/// pre-boxed as i31 refs; `Bin`/`Io` are the concrete i8-array, `Arr` the
/// anyref-element array.
///
/// [`WireSignature`]: curios_abi::WireSignature
fn host_func_type(engine: &Engine, function: HostFunction) -> FuncType {
    let bin_array = ArrayType::new(engine, FieldType::new(Mutability::Var, StorageType::I8));
    let bin_ref = ValType::Ref(RefType::new(false, HeapType::ConcreteArray(bin_array)));
    let arr_array = ArrayType::new(
        engine,
        FieldType::new(
            Mutability::Var,
            StorageType::ValType(ValType::Ref(RefType::new(true, HeapType::Any))),
        ),
    );
    let arr_ref = ValType::Ref(RefType::new(false, HeapType::ConcreteArray(arr_array)));
    let i31_ref = ValType::Ref(RefType::new(false, HeapType::I31));

    let val_type = |wire_type: WireType, is_result: bool| match wire_type {
        WireType::Nat | WireType::Bln | WireType::Int => match is_result {
            true => i31_ref.clone(),
            false => ValType::I32,
        },
        WireType::Bin | WireType::Io => bin_ref.clone(),
        WireType::Arr(_) => arr_ref.clone(),
    };

    let signature = function.signature();

    FuncType::new(
        engine,
        signature
            .params
            .iter()
            .map(|(_, wire_type)| val_type(*wire_type, false))
            .collect::<Vec<_>>(),
        signature
            .results
            .iter()
            .map(|(_, wire_type)| val_type(*wire_type, true))
            .collect::<Vec<_>>(),
    )
}

/// [`define_import`] for a table-described host function: the import name and
/// `FuncType` both come off the [`HostFunction`] table.
fn define_host_import<Li, Lo, F>(
    linker: &mut Linker<()>,
    engine: &Engine,
    function: HostFunction,
    f: F,
) -> Result<(), String>
where
    Li: Lift,
    Lo: Lower,
    F: Fn(Li) -> Lo + Send + Sync + 'static,
{
    define_import(linker, function.name(), host_func_type(engine, function), f)
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
pub fn run_bytes<H: Host + Send + Sync + 'static>(payload: &[u8], host: H) -> Result<i32, String> {
    let engine = shared_engine();

    // SAFETY: see the contract above — `payload` is our own precompiled output.
    let module = unsafe { Module::deserialize(engine, payload) }
        .map_err(|error| format!("failed to load wasm module: {error}"))?;

    instantiate(engine, &module, host)
}

/// Instantiate `module` against `engine`, wire up the host imports, and run its
/// entrypoint, returning the process exit code. Shared by [`run_bytes`] and by
/// `curios`'s JIT path (which builds the `Module` via `from_binary`).
pub fn instantiate<H: Host + Send + Sync + 'static>(
    engine: &Engine,
    module: &Module,
    host: H,
) -> Result<i32, String> {
    let io_exit_type = FuncType::new(engine, [ValType::I32], []);

    let mut linker = Linker::new(engine);
    let host = Arc::new(host);

    define_host_import(&mut linker, engine, HostFunction::Read, {
        let host = host.clone();

        move |(io, count): (Io, u32)| host.read(io, count)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Write, {
        let host = host.clone();

        move |(io, bytes): (Io, Vec<u8>)| host.write(io, &bytes)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Open, {
        let host = host.clone();

        move |(path, mode): (Vec<u8>, Mode)| host.open(&path, mode)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Connect, {
        let host = host.clone();

        move |(io, addr): (Io, Vec<u8>)| host.connect(io, &addr)
    })?;

    define_host_import(&mut linker, engine, HostFunction::StartTls, {
        let host = host.clone();

        move |(io, sni): (Io, Vec<u8>)| host.start_tls(io, &sni)
    })?;

    define_host_import(&mut linker, engine, HostFunction::TlsServerConfig, {
        let host = host.clone();

        move |(cert, key): (Vec<u8>, Vec<u8>)| host.tls_server_config(&cert, &key)
    })?;

    define_host_import(&mut linker, engine, HostFunction::StartTlsServer, {
        let host = host.clone();

        move |(io, cfg): (Io, Io)| host.start_tls_server(io, cfg)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Listen, {
        let host = host.clone();

        move |(io, backlog): (Io, u32)| host.listen(io, backlog)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Accept, {
        let host = host.clone();

        move |io: Io| host.accept(io)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Lookup, {
        let host = host.clone();

        move |(name, port): (Vec<u8>, u32)| host.lookup(&name, port)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Resolve, {
        let host = host.clone();

        move |handle: Io| host.resolve(handle)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Socket, {
        let host = host.clone();

        move |addr: Vec<u8>| host.socket(&addr)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Bind, {
        let host = host.clone();

        move |(io, addr): (Io, Vec<u8>)| host.bind(io, &addr)
    })?;

    define_host_import(&mut linker, engine, HostFunction::SetNonblocking, {
        let host = host.clone();

        move |(io, on): (Io, u32)| host.set_nonblocking(io, on)
    })?;

    define_host_import(&mut linker, engine, HostFunction::SetRecvTimeout, {
        let host = host.clone();

        move |(io, ms): (Io, u32)| host.set_recv_timeout(io, ms)
    })?;

    define_host_import(&mut linker, engine, HostFunction::SetSendTimeout, {
        let host = host.clone();

        move |(io, ms): (Io, u32)| host.set_send_timeout(io, ms)
    })?;

    define_host_import(&mut linker, engine, HostFunction::SetReuseaddr, {
        let host = host.clone();

        move |(io, on): (Io, u32)| host.set_reuseaddr(io, on)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Poll, {
        let host = host.clone();

        move |(handles, events, timeout): (Vec<Io>, Vec<Poll>, i32)| {
            host.poll(&handles, &events, timeout)
        }
    })?;

    define_host_import(&mut linker, engine, HostFunction::Close, {
        let host = host.clone();

        move |io: Io| host.close(io)
    })?;

    define_host_import(&mut linker, engine, HostFunction::ClockWall, {
        let host = host.clone();

        move |()| host.clock_wall()
    })?;

    define_host_import(&mut linker, engine, HostFunction::ClockMono, {
        let host = host.clone();

        move |()| host.clock_mono()
    })?;

    define_host_import(&mut linker, engine, HostFunction::Random, {
        let host = host.clone();

        move |count: u32| host.random(count)
    })?;

    define_host_import(&mut linker, engine, HostFunction::Args, {
        let host = host.clone();

        move |()| host.args()
    })?;

    define_host_import(&mut linker, engine, HostFunction::Env, {
        let host = host.clone();

        move |name: Vec<u8>| host.env(&name)
    })?;

    // `exit` never returns: it traps with the code, which `instantiate_and_run`
    // catches. A plain `define_import` cannot trap, so it is wired directly.
    linker
        .func_new("env", "io_exit", io_exit_type, move |_caller, params, _| {
            let code = match params.first() {
                Some(wasmtime::Val::I32(code)) => *code,
                _ => 0,
            };

            Err(wasmtime::Error::from(ExitTrap(code)))
        })
        .map_err(|error| format!("failed to define io_exit: {error}"))?;

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
