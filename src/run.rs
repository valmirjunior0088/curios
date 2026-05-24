mod provider;
pub use provider::*;

use {
    crate::{cont, core, ersd, text, wasm},
    std::{path::Path, sync::Arc, time::Duration},
    wasmtime::{
        AnyRef, ArrayRef, ArrayRefPre, ArrayType, Caller, Config, Engine, FieldType, FuncType,
        HeapType, Linker, Module, Mutability, RefType, Rooted, StorageType, Store, Val, ValType,
    },
};

pub fn run<P: Provider + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    loader: &dyn text::Loader,
    provider: P,
) -> Result<(), String> {
    let term = text::to_core(
        &source
            .parse()
            .map_err(|error| format!("failed to parse source: {error:?}"))?,
        loader,
    );

    let type_ = core::infer(&mut core::Context::new(timeout), &term)
        .map_err(|error| format!("failed to infer type: {error:?}"))?;

    let term = core::erase(&mut core::Context::new(timeout), &term, &type_)
        .map_err(|error| format!("failed to erase term: {error:?}"))?;

    run_wasm(&cont::to_wasm(&ersd::to_cont(&term)), provider)?;

    Ok(())
}

pub fn run_wasm<P: Provider + Send + Sync + 'static>(
    wasm_module: &wasm::Module,
    provider: P,
) -> Result<(), String> {
    let mut config = Config::new();
    config.wasm_reference_types(true);
    config.wasm_function_references(true);
    config.wasm_gc(true);
    config.wasm_tail_call(true);

    let engine =
        Engine::new(&config).map_err(|error| format!("failed to create engine: {error}"))?;

    let module = Module::from_binary(&engine, &wasm::to_bytes(wasm_module))
        .map_err(|error| format!("failed to load wasm module: {error}"))?;

    let bin_array_type = ArrayType::new(&engine, FieldType::new(Mutability::Var, StorageType::I8));

    let bin_ref = ValType::Ref(RefType::new(
        false,
        HeapType::ConcreteArray(bin_array_type.clone()),
    ));

    let i32_to_bin = FuncType::new(&engine, [ValType::I32], [bin_ref.clone()]);
    let f32_to_bin = FuncType::new(&engine, [ValType::F32], [bin_ref.clone()]);

    let provider = Arc::new(provider);
    let mut linker: Linker<()> = Linker::new(&engine);

    {
        let bin_array_type = bin_array_type.clone();

        linker
            .func_new(
                "env",
                "nat_to_str",
                i32_to_bin.clone(),
                move |mut caller: Caller<'_, ()>, params, results| {
                    let value = params[0].unwrap_i32() as u32;
                    let bytes = format!("{value}").into_bytes();
                    let pre = ArrayRefPre::new(&mut caller, bin_array_type.clone());
                    let elems: Vec<Val> = bytes.into_iter().map(|b| Val::I32(b as i32)).collect();
                    results[0] = Val::AnyRef(Some(
                        ArrayRef::new_fixed(&mut caller, &pre, &elems)?.to_anyref(),
                    ));
                    Ok(())
                },
            )
            .map_err(|e| format!("failed to define nat_to_str: {e}"))?;
    }

    {
        let bin_array_type = bin_array_type.clone();

        linker
            .func_new(
                "env",
                "int_to_str",
                i32_to_bin,
                move |mut caller: Caller<'_, ()>, params, results| {
                    let value = params[0].unwrap_i32();
                    let bytes = format!("{value}").into_bytes();
                    let pre = ArrayRefPre::new(&mut caller, bin_array_type.clone());
                    let elems: Vec<Val> = bytes.into_iter().map(|b| Val::I32(b as i32)).collect();
                    results[0] = Val::AnyRef(Some(
                        ArrayRef::new_fixed(&mut caller, &pre, &elems)?.to_anyref(),
                    ));
                    Ok(())
                },
            )
            .map_err(|e| format!("failed to define int_to_str: {e}"))?;
    }

    {
        let bin_array_type = bin_array_type.clone();

        linker
            .func_new(
                "env",
                "flt_to_str",
                f32_to_bin,
                move |mut caller: Caller<'_, ()>, params, results| {
                    let value = params[0].unwrap_f32();
                    let bytes = format!("{value}").into_bytes();
                    let pre = ArrayRefPre::new(&mut caller, bin_array_type.clone());
                    let elems: Vec<Val> = bytes.into_iter().map(|b| Val::I32(b as i32)).collect();
                    results[0] = Val::AnyRef(Some(
                        ArrayRef::new_fixed(&mut caller, &pre, &elems)?.to_anyref(),
                    ));
                    Ok(())
                },
            )
            .map_err(|e| format!("failed to define flt_to_str: {e}"))?;
    }

    {
        let bin_to_unit = FuncType::new(&engine, [bin_ref.clone()], []);
        let provider = provider.clone();

        linker
            .func_new(
                "env",
                "sys_print",
                bin_to_unit,
                move |mut caller: Caller<'_, ()>, params, _results| {
                    let Val::AnyRef(Some(anyref)) = &params[0] else {
                        return Err(wasmtime::Error::msg("sys_print: expected non-null anyref"));
                    };
                    let array_ref = anyref
                        .as_array(&caller)?
                        .ok_or_else(|| wasmtime::Error::msg("sys_print: expected array ref"))?;
                    let len = array_ref.len(&caller)?;
                    let bytes: Vec<u8> = (0..len)
                        .map(|i| array_ref.get(&mut caller, i).map(|v| v.unwrap_i32() as u8))
                        .collect::<Result<Vec<u8>, _>>()?;
                    provider.print(&bytes);
                    Ok(())
                },
            )
            .map_err(|e| format!("failed to define sys_print: {e}"))?;
    }

    {
        let unit_to_bin = FuncType::new(&engine, [], [bin_ref.clone()]);
        let bin_array_type = bin_array_type.clone();
        let provider = provider.clone();

        linker
            .func_new(
                "env",
                "sys_read",
                unit_to_bin,
                move |mut caller: Caller<'_, ()>, _params, results| {
                    let bytes = provider.read();
                    let pre = ArrayRefPre::new(&mut caller, bin_array_type.clone());
                    let elems: Vec<Val> = bytes.into_iter().map(|b| Val::I32(b as i32)).collect();
                    results[0] = Val::AnyRef(Some(
                        ArrayRef::new_fixed(&mut caller, &pre, &elems)?.to_anyref(),
                    ));
                    Ok(())
                },
            )
            .map_err(|e| format!("failed to define sys_read: {e}"))?;
    }

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

pub fn run_text<P: Provider + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    provider: P,
) -> Result<(), String> {
    run(timeout, source, &text::PanicLoader, provider)
}

pub fn run_file<P: Provider + Send + Sync + 'static>(
    timeout: Duration,
    path: &Path,
    provider: P,
) -> Result<(), String> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| format!("failed to read {}: {e}", path.display()))?;

    let base = path.parent().unwrap_or(Path::new(".")).to_path_buf();

    run(timeout, &source, &text::FileLoader::new(base), provider)
}
