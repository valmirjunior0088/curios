use {
    super::{Host, Io, Lift, Lower, Mode, Poll},
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
    let bin_array_type = ArrayType::new(engine, FieldType::new(Mutability::Var, StorageType::I8));
    let bin_ref = ValType::Ref(RefType::new(false, HeapType::ConcreteArray(bin_array_type)));
    let i31_ref = ValType::Ref(RefType::new(false, HeapType::I31));
    let io_clock_wall_type = FuncType::new(
        engine,
        std::iter::empty::<ValType>(),
        [i31_ref.clone(), i31_ref.clone(), i31_ref.clone()],
    );
    let io_clock_mono_type = FuncType::new(
        engine,
        std::iter::empty::<ValType>(),
        [i31_ref.clone(), i31_ref.clone()],
    );
    let io_random_type = FuncType::new(engine, [ValType::I32], [bin_ref.clone()]);
    // argv crosses as an `Arr(Bin)` — an array of `anyref` whose element type
    // `(mut (ref null any))` matches the codegen's uniform `arr_type`.
    let arr_array_type = ArrayType::new(
        engine,
        FieldType::new(
            Mutability::Var,
            StorageType::ValType(ValType::Ref(RefType::new(true, HeapType::Any))),
        ),
    );
    let arr_ref = ValType::Ref(RefType::new(false, HeapType::ConcreteArray(arr_array_type)));
    let io_args_type = FuncType::new(engine, std::iter::empty::<ValType>(), [arr_ref.clone()]);
    let io_env_type = FuncType::new(
        engine,
        [bin_ref.clone()],
        [i31_ref.clone(), bin_ref.clone()],
    );
    let io_exit_type = FuncType::new(engine, [ValType::I32], []);
    // A handle crosses as a `Bin` (`bin_ref`) in both directions now, so every
    // handle operand and every handle result is `bin_ref`; only non-handle
    // scalars stay raw `i32` and only the status results stay `i31_ref`.
    let io_read_type = FuncType::new(
        engine,
        [bin_ref.clone(), ValType::I32],
        [i31_ref.clone(), bin_ref.clone()],
    );
    let io_write_type = FuncType::new(
        engine,
        [bin_ref.clone(), bin_ref.clone()],
        [i31_ref.clone(), i31_ref.clone()],
    );
    let io_connect_type = FuncType::new(
        engine,
        [bin_ref.clone(), bin_ref.clone()],
        [i31_ref.clone()],
    );
    let io_listen_type = FuncType::new(engine, [bin_ref.clone(), ValType::I32], [i31_ref.clone()]);
    let io_accept_type = FuncType::new(
        engine,
        [bin_ref.clone()],
        [i31_ref.clone(), bin_ref.clone()],
    );
    let io_lookup_type = FuncType::new(
        engine,
        [bin_ref.clone(), ValType::I32],
        [i31_ref.clone(), bin_ref.clone()],
    );
    let io_resolve_type = FuncType::new(
        engine,
        [bin_ref.clone()],
        [i31_ref.clone(), arr_ref.clone()],
    );
    let io_socket_type = FuncType::new(
        engine,
        [bin_ref.clone()],
        [i31_ref.clone(), bin_ref.clone()],
    );
    let io_start_tls_type = FuncType::new(
        engine,
        [bin_ref.clone(), bin_ref.clone()],
        [i31_ref.clone()],
    );
    let io_tls_server_config_type = FuncType::new(
        engine,
        [bin_ref.clone(), bin_ref.clone()],
        [i31_ref.clone(), bin_ref.clone()],
    );
    let io_start_tls_server_type = FuncType::new(
        engine,
        [bin_ref.clone(), bin_ref.clone()],
        [i31_ref.clone()],
    );
    let io_bind_type = FuncType::new(
        engine,
        [bin_ref.clone(), bin_ref.clone()],
        [i31_ref.clone()],
    );
    let io_set_nonblocking_type =
        FuncType::new(engine, [bin_ref.clone(), ValType::I32], [i31_ref.clone()]);
    let io_set_recv_timeout_type =
        FuncType::new(engine, [bin_ref.clone(), ValType::I32], [i31_ref.clone()]);
    let io_set_send_timeout_type =
        FuncType::new(engine, [bin_ref.clone(), ValType::I32], [i31_ref.clone()]);
    let io_set_reuseaddr_type =
        FuncType::new(engine, [bin_ref.clone(), ValType::I32], [i31_ref.clone()]);
    let io_open_type = FuncType::new(
        engine,
        [bin_ref.clone(), ValType::I32],
        [i31_ref.clone(), bin_ref.clone()],
    );
    // `(handles : Arr(Io), events : Arr(Nat), timeout : Int) -> revents :
    // Arr(Nat)`: the two array params and the array result all cross as the
    // uniform `arr_ref`; `timeout` is a raw i32 (the signed poll(2) convention).
    let io_poll_type = FuncType::new(
        engine,
        [arr_ref.clone(), arr_ref.clone(), ValType::I32],
        [arr_ref.clone()],
    );
    let io_close_type = FuncType::new(engine, [bin_ref.clone()], []);

    let mut linker = Linker::new(engine);
    let host = Arc::new(host);

    define_import(&mut linker, "io_read", io_read_type, {
        let host = host.clone();

        move |(io, count): (Io, u32)| host.read(io, count)
    })?;

    define_import(&mut linker, "io_write", io_write_type, {
        let host = host.clone();

        move |(io, bytes): (Io, Vec<u8>)| host.write(io, &bytes)
    })?;

    define_import(&mut linker, "io_open", io_open_type, {
        let host = host.clone();

        move |(path, mode): (Vec<u8>, Mode)| host.open(&path, mode)
    })?;

    define_import(&mut linker, "io_connect", io_connect_type, {
        let host = host.clone();

        move |(io, addr): (Io, Vec<u8>)| host.connect(io, &addr)
    })?;

    define_import(&mut linker, "io_start_tls", io_start_tls_type, {
        let host = host.clone();

        move |(io, sni): (Io, Vec<u8>)| host.start_tls(io, &sni)
    })?;

    define_import(
        &mut linker,
        "io_tls_server_config",
        io_tls_server_config_type,
        {
            let host = host.clone();

            move |(cert, key): (Vec<u8>, Vec<u8>)| host.tls_server_config(&cert, &key)
        },
    )?;

    define_import(
        &mut linker,
        "io_start_tls_server",
        io_start_tls_server_type,
        {
            let host = host.clone();

            move |(io, cfg): (Io, Io)| host.start_tls_server(io, cfg)
        },
    )?;

    define_import(&mut linker, "io_listen", io_listen_type, {
        let host = host.clone();

        move |(io, backlog): (Io, u32)| host.listen(io, backlog)
    })?;

    define_import(&mut linker, "io_accept", io_accept_type, {
        let host = host.clone();

        move |io: Io| host.accept(io)
    })?;

    define_import(&mut linker, "io_lookup", io_lookup_type, {
        let host = host.clone();

        move |(name, port): (Vec<u8>, u32)| host.lookup(&name, port)
    })?;

    define_import(&mut linker, "io_resolve", io_resolve_type, {
        let host = host.clone();

        move |handle: Io| host.resolve(handle)
    })?;

    define_import(&mut linker, "io_socket", io_socket_type, {
        let host = host.clone();

        move |addr: Vec<u8>| host.socket(&addr)
    })?;

    define_import(&mut linker, "io_bind", io_bind_type, {
        let host = host.clone();

        move |(io, addr): (Io, Vec<u8>)| host.bind(io, &addr)
    })?;

    define_import(
        &mut linker,
        "io_set_nonblocking",
        io_set_nonblocking_type,
        {
            let host = host.clone();

            move |(io, on): (Io, u32)| host.set_nonblocking(io, on)
        },
    )?;

    define_import(
        &mut linker,
        "io_set_recv_timeout",
        io_set_recv_timeout_type,
        {
            let host = host.clone();

            move |(io, ms): (Io, u32)| host.set_recv_timeout(io, ms)
        },
    )?;

    define_import(
        &mut linker,
        "io_set_send_timeout",
        io_set_send_timeout_type,
        {
            let host = host.clone();

            move |(io, ms): (Io, u32)| host.set_send_timeout(io, ms)
        },
    )?;

    define_import(&mut linker, "io_set_reuseaddr", io_set_reuseaddr_type, {
        let host = host.clone();

        move |(io, on): (Io, u32)| host.set_reuseaddr(io, on)
    })?;

    define_import(&mut linker, "io_poll", io_poll_type, {
        let host = host.clone();

        move |(handles, events, timeout): (Vec<Io>, Vec<Poll>, i32)| {
            host.poll(&handles, &events, timeout)
        }
    })?;

    define_import(&mut linker, "io_close", io_close_type, {
        let host = host.clone();

        move |io: Io| host.close(io)
    })?;

    define_import(&mut linker, "io_clock_wall", io_clock_wall_type, {
        let host = host.clone();

        move |()| host.clock_wall()
    })?;

    define_import(&mut linker, "io_clock_mono", io_clock_mono_type, {
        let host = host.clone();

        move |()| host.clock_mono()
    })?;

    define_import(&mut linker, "io_random", io_random_type, {
        let host = host.clone();

        move |count: u32| host.random(count)
    })?;

    define_import(&mut linker, "io_args", io_args_type, {
        let host = host.clone();

        move |()| host.args()
    })?;

    define_import(&mut linker, "io_env", io_env_type, {
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
