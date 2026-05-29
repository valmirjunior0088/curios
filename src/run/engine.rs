use {
    super::{Host, Lift, Lower},
    crate::wasm,
    std::sync::Arc,
    wasmtime::{
        AnyRef, ArrayType, Config, Engine, FieldType, FuncType, HeapType, Linker, Module,
        Mutability, RefType, Rooted, StorageType, Store, ValType,
    },
};

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
    let mut config = Config::new();
    config.wasm_reference_types(true);
    config.wasm_function_references(true);
    config.wasm_gc(true);
    config.wasm_tail_call(true);

    let engine =
        Engine::new(&config).map_err(|error| format!("failed to create engine: {error}"))?;

    let module = Module::from_binary(&engine, &wasm::to_bytes(module))
        .map_err(|error| format!("failed to load wasm module: {error}"))?;

    let bin_array_type = ArrayType::new(&engine, FieldType::new(Mutability::Var, StorageType::I8));
    let bin_ref = ValType::Ref(RefType::new(false, HeapType::ConcreteArray(bin_array_type)));
    let i32_to_bin_type = FuncType::new(&engine, [ValType::I32], [bin_ref.clone()]);
    let f32_to_bin_type = FuncType::new(&engine, [ValType::F32], [bin_ref.clone()]);
    let unit_to_bin_type = FuncType::new(&engine, [], [bin_ref.clone()]);
    let bin_to_unit_type = FuncType::new(&engine, [bin_ref], []);

    let mut linker: Linker<()> = Linker::new(&engine);
    let host = Arc::new(host);

    define_import(&mut linker, "nat_to_str", i32_to_bin_type.clone(), {
        let host = host.clone();

        move |value| host.nat_to_str(value)
    })?;

    define_import(&mut linker, "int_to_str", i32_to_bin_type, {
        let host = host.clone();

        move |value| host.int_to_str(value)
    })?;

    define_import(&mut linker, "flt_to_str", f32_to_bin_type, {
        let host = host.clone();

        move |value| host.flt_to_str(value)
    })?;

    define_import(&mut linker, "io_read", unit_to_bin_type, {
        let host = host.clone();

        move |(): ()| host.read()
    })?;

    define_import(&mut linker, "io_print", bin_to_unit_type, {
        let host = host.clone();

        move |bytes: Vec<u8>| host.print(&bytes)
    })?;

    let mut store = Store::new(&engine, ());

    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|error| format!("failed to instantiate module: {error}"))?;

    let function = instance
        .get_typed_func::<(), Rooted<AnyRef>>(&mut store, "func/main")
        .map_err(|error| format!("failed to access func/main: {error}"))?;

    function
        .call(&mut store, ())
        .map_err(|error| format!("execution failed: {error}"))?;

    Ok(())
}
