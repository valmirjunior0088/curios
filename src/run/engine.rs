use {
    super::{Host, Lift, Lower},
    crate::wasm,
    std::sync::{Arc, LazyLock},
    wasmtime::{
        AnyRef, ArrayType, Config, Engine, FieldType, FuncType, HeapType, Instance, Linker, Module,
        Mutability, RefType, Rooted, StorageType, Store, ValType,
    },
};

/// The one wasm engine for the whole process. Building an `Engine` stands up the
/// Cranelift backend and is expensive, so it is created once and shared; `Engine`
/// is `Send + Sync` (internally reference-counted), so a `static` is sound and a
/// clone is cheap. Every module, store, and type below is created against it, so
/// they stay engine-consistent.
fn shared_engine() -> &'static Engine {
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

pub fn run_wasm<H: Host + Send + Sync + 'static>(
    module: &wasm::Module,
    host: H,
) -> Result<(), String> {
    instantiate_and_run(module, host).map(|_| ())
}

/// Instantiate `module`, wire up the host imports, and run its entrypoint, returning the
/// store and instance so callers can inspect post-run state.
fn instantiate_and_run<H: Host + Send + Sync + 'static>(
    module: &wasm::Module,
    host: H,
) -> Result<(Store<()>, Instance), String> {
    let engine = shared_engine();

    let bytes = wasm::to_bytes(module);

    #[cfg(feature = "binaryen")]
    let bytes = crate::binaryen::optimize(bytes);

    let module = Module::from_binary(engine, &bytes)
        .map_err(|error| format!("failed to load wasm module: {error}"))?;

    let bin_array_type = ArrayType::new(engine, FieldType::new(Mutability::Var, StorageType::I8));
    let bin_ref = ValType::Ref(RefType::new(false, HeapType::ConcreteArray(bin_array_type)));
    let i32_to_bin_type = FuncType::new(engine, [ValType::I32], [bin_ref.clone()]);
    let f32_to_bin_type = FuncType::new(engine, [ValType::F32], [bin_ref.clone()]);
    let i32_i32_to_bin_type = FuncType::new(
        engine,
        [ValType::I32, ValType::I32],
        [bin_ref.clone()],
    );
    let i32_bin_to_unit_type = FuncType::new(engine, [ValType::I32, bin_ref], []);

    let mut linker: Linker<()> = Linker::new(engine);
    let host = Arc::new(host);

    define_import(&mut linker, "nat_to_str", i32_to_bin_type.clone(), {
        move |value| super::host::nat_to_str(value)
    })?;

    define_import(&mut linker, "int_to_str", i32_to_bin_type, {
        move |value| super::host::int_to_str(value)
    })?;

    define_import(&mut linker, "flt_to_str", f32_to_bin_type.clone(), {
        move |value| super::host::flt_to_str(value)
    })?;

    define_import(&mut linker, "flt_to_le_bin", f32_to_bin_type, {
        let host = host.clone();

        move |value| host.flt_to_le_bin(value)
    })?;

    define_import(&mut linker, "io_read", i32_i32_to_bin_type, {
        let host = host.clone();

        move |(handle, count): (u32, u32)| host.read(handle, count)
    })?;

    define_import(&mut linker, "io_write", i32_bin_to_unit_type, {
        let host = host.clone();

        move |(handle, bytes): (u32, Vec<u8>)| host.write(handle, &bytes)
    })?;

    let mut store = Store::new(engine, ());

    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|error| format!("failed to instantiate module: {error}"))?;

    let function = instance
        .get_typed_func::<(), Rooted<AnyRef>>(&mut store, "func/main")
        .map_err(|error| format!("failed to access func/main: {error}"))?;

    function
        .call(&mut store, ())
        .map_err(|error| format!("execution failed: {error}"))?;

    Ok((store, instance))
}
