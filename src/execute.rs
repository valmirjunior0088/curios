use {
    crate::{
        cont, core, ersd,
        printer::{Printer, flat, indent, pure, sep_flat},
        wasm,
    },
    std::{collections::HashMap, time::Duration},
    wasmtime::{AnyRef, Config, Engine, Instance, Module, Rooted, Store, Val},
};

fn raw_ref_id(store: &mut Store<()>, reference: &Rooted<AnyRef>) -> u32 {
    reference
        .to_raw(&mut *store)
        .unwrap_or_else(|error| panic!("live rooted anyref should expose a raw identity: {error}"))
}

#[derive(Debug, Default)]
struct RefIds {
    ref_ids: HashMap<u32, usize>,
}

impl RefIds {
    fn new() -> Self {
        Self::default()
    }

    fn find(&mut self, store: &mut Store<()>, reference: &Rooted<AnyRef>) -> Option<usize> {
        let raw = raw_ref_id(store, reference);

        self.ref_ids.get(&raw).copied()
    }

    fn push(&mut self, store: &mut Store<()>, reference: &Rooted<AnyRef>) -> usize {
        let id = self.ref_ids.len();
        self.ref_ids.insert(raw_ref_id(store, reference), id);

        id
    }
}

fn print_val(ref_ids: &mut RefIds, store: &mut Store<()>, value: Val) -> Printer<'static> {
    match value {
        Val::I32(value) => pure(format!("i32(bits=0x{:08x}, value={value})", value as u32)),
        Val::I64(value) => pure(format!("i64(bits=0x{:016x}, value={value})", value as u64)),
        Val::F32(bits) => pure(format!(
            "f32(bits=0x{bits:08x}, value={})",
            f32::from_bits(bits)
        )),
        Val::F64(bits) => pure(format!(
            "f64(bits=0x{bits:016x}, value={})",
            f64::from_bits(bits)
        )),
        Val::V128(value) => pure(format!("v128({value:?})")),
        Val::FuncRef(Some(_)) => pure("funcref"),
        Val::FuncRef(None) => pure("null funcref"),
        Val::ExternRef(Some(_)) => pure("externref"),
        Val::ExternRef(None) => pure("null externref"),
        Val::AnyRef(Some(reference)) => print_ref(ref_ids, store, &reference),
        Val::AnyRef(None) => pure("null anyref"),
        Val::ExnRef(Some(_)) => pure("exnref"),
        Val::ExnRef(None) => pure("null exnref"),
        Val::ContRef(Some(_)) => pure("contref"),
        Val::ContRef(None) => pure("null contref"),
    }
}

fn print_ref(
    ref_ids: &mut RefIds,
    store: &mut Store<()>,
    reference: &Rooted<AnyRef>,
) -> Printer<'static> {
    if let Some(id) = ref_ids.find(store, reference) {
        return pure(format!("ref #{id}"));
    }

    let id = ref_ids.push(store, reference);

    if let Some(value) = reference.as_i31(&*store).unwrap_or_else(|error| {
        panic!("anyref i31 probe should succeed for a live rooted value: {error}")
    }) {
        let value = value.get_i32();
        let bits = (value as u32) & 0x7fff_ffff;

        return pure(format!("#{id} = i31(bits=0x{bits:08x}, value={value})"));
    }

    if let Some(struct_ref) = reference.as_struct(&*store).unwrap_or_else(|error| {
        panic!("anyref struct probe should succeed for a live rooted value: {error}")
    }) {
        let field_count = struct_ref
            .ty(&*store)
            .unwrap_or_else(|error| panic!("struct anyref should expose its type: {error}"))
            .fields()
            .len();

        let fields = (0..field_count)
            .map(|index| {
                let value = struct_ref
                    .field(&mut *store, index)
                    .unwrap_or_else(|error| {
                        panic!("struct field {index} should be readable: {error}")
                    });

                flat([pure(format!("{index}: ")), print_val(ref_ids, store, value)])
            })
            .collect::<Vec<_>>();

        return match fields.is_empty() {
            true => pure(format!("#{id} = struct {{}}")),
            false => flat([
                pure(format!("#{id} = struct {{\n")),
                indent(sep_flat(fields, || pure("\n"))),
                pure("\n}"),
            ]),
        };
    }

    if let Some(array_ref) = reference.as_array(&*store).unwrap_or_else(|error| {
        panic!("anyref array probe should succeed for a live rooted value: {error}")
    }) {
        let length = array_ref
            .len(&*store)
            .unwrap_or_else(|error| panic!("array anyref should expose its length: {error}"));
        let elems = (0..length)
            .map(|index| {
                let value = array_ref.get(&mut *store, index).unwrap_or_else(|error| {
                    panic!("array element {index} should be readable: {error}")
                });

                flat([pure(format!("{index}: ")), print_val(ref_ids, store, value)])
            })
            .collect::<Vec<_>>();

        return match elems.is_empty() {
            true => pure(format!("#{id} = array []")),
            false => flat([
                pure(format!("#{id} = array [\n")),
                indent(sep_flat(elems, || pure("\n"))),
                pure("\n]"),
            ]),
        };
    }

    pure(format!("#{id} = anyref"))
}

pub fn execute(timeout: Duration, source: &str) -> Result<String, String> {
    let term = source
        .parse()
        .map_err(|error| format!("failed to parse source: {error:?}"))?;

    let type_ = core::infer(&mut core::Context::new(timeout), &term)
        .map_err(|error| format!("failed to infer type: {error:?}"))?;

    let term = core::erase(&mut core::Context::new(timeout), &term, &type_)
        .map_err(|error| format!("failed to erase term: {error:?}"))?;

    let mut config = Config::new();
    config.wasm_reference_types(true);
    config.wasm_function_references(true);
    config.wasm_gc(true);
    config.wasm_tail_call(true);

    let engine =
        Engine::new(&config).map_err(|error| format!("failed to create engine: {error}"))?;

    let module = Module::from_binary(
        &engine,
        &wasm::to_bytes(&cont::to_wasm(&ersd::to_cont(&term))),
    )
    .map_err(|error| format!("failed to load wasm module: {error}"))?;

    let mut store = Store::new(&engine, ());

    let instance = Instance::new(&mut store, &module, &[])
        .map_err(|error| format!("failed to instantiate module: {error}"))?;

    let function = instance
        .get_typed_func(&mut store, "func/main")
        .map_err(|error| format!("failed to access func/main: {error}"))?;

    let result = function
        .call(&mut store, ())
        .map_err(|error| format!("execution failed: {error}"))?;

    Ok(print_ref(&mut RefIds::new(), &mut store, &result)
        .display()
        .to_string())
}
