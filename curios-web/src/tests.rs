//! Tests for the browser bridge helpers. Program-side `Bytes` is a rope (`$rope/bin/leaf` / `$rope/bin/node` structs); what crosses to a host is always the forced flat payload, which is what the bridge accessors read and write.

use {
    curios_runtime::shared_engine,
    curios_wasm::{CompType, Export, SubType, TypeName},
    wasmtime::{Func, Instance, Linker, Module, Store, Val},
};

fn func_arity(sub_type: &SubType) -> (usize, usize) {
    match &sub_type.comp_type {
        CompType::Func(func_type) => (func_type.inputs().len(), func_type.outputs().len()),
        comp_type => panic!("expected a func type, got {comp_type:?}"),
    }
}

fn instantiate(store: &mut Store<()>, bytes: &[u8]) -> Instance {
    let module = Module::new(store.engine(), bytes).expect("invalid module bytes");

    Linker::new(store.engine())
        .instantiate(&mut *store, &module)
        .expect("instantiation failed")
}

fn export(store: &mut Store<()>, instance: &Instance, name: &str) -> Func {
    instance
        .get_func(&mut *store, name)
        .unwrap_or_else(|| panic!("missing export {name}"))
}

/// Call `func` untyped and return its single result. Untyped calls are the point: they type-check dynamically, which succeeds exactly when the bridge's declared shapes are right.
fn call(store: &mut Store<()>, func: &Func, params: &[Val]) -> Val {
    let mut results = vec![Val::I32(0); 1];

    func.call(&mut *store, params, &mut results)
        .expect("call failed");

    results.remove(0)
}

#[test]
fn accessors_are_exported_with_their_shapes() {
    let module = crate::bridge::bridge_module();

    for (name, inputs, outputs) in [
        ("bytes_len", 1, 1),
        ("bytes_get", 2, 1),
        ("bytes_new", 1, 1),
        ("bytes_set", 3, 0),
        ("bytes_load", 1, 1),
        ("bytes_store", 1, 1),
    ] {
        let export = module
            .exports()
            .iter()
            .find(|(export_name, _)| export_name == name);

        assert!(
            matches!(export, Some((_, Export::Func(_)))),
            "missing func export {name}"
        );

        let sub_type = module
            .get_type(&TypeName::from(name))
            .unwrap_or_else(|| panic!("missing type {name}"));

        assert_eq!(func_arity(sub_type), (inputs, outputs), "{name}");
    }
}

#[test]
fn bridge_bytes_encode_a_wasm_module() {
    assert_eq!(&crate::bridge_bytes()[..4], b"\0asm");
}

#[test]
fn bridge_accessors_roundtrip() {
    let mut store = Store::new(shared_engine(), ());
    let bridge = instantiate(&mut store, &crate::bridge_bytes());

    let bytes_new = export(&mut store, &bridge, "bytes_new");
    let bytes_set = export(&mut store, &bridge, "bytes_set");
    let bytes_get = export(&mut store, &bridge, "bytes_get");
    let bytes_len = export(&mut store, &bridge, "bytes_len");

    let bin = call(&mut store, &bytes_new, &[Val::I32(3)]);

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        bytes_set
            .call(
                &mut store,
                &[bin, Val::I32(index as i32), Val::I32(value)],
                &mut [],
            )
            .expect("bytes_set failed");
    }

    assert_eq!(
        call(&mut store, &bytes_len, std::slice::from_ref(&bin)).unwrap_i32(),
        3
    );

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        assert_eq!(
            call(&mut store, &bytes_get, &[bin, Val::I32(index as i32)]).unwrap_i32(),
            value
        );
    }
}

/// The bulk lane end to end: bytes written into the exported memory become a `bytes` array via `bytes_store`, and `bytes_load` copies them back out.
#[test]
fn bulk_transfers_roundtrip_through_the_memory() {
    let mut store = Store::new(shared_engine(), ());
    let bridge = instantiate(&mut store, &crate::bridge_bytes());

    let memory = bridge
        .get_memory(&mut store, "memory")
        .expect("missing memory export");
    assert_eq!(memory.size(&store), 0, "the memory starts empty");
    memory.grow(&mut store, 1).expect("grow failed");

    let payload = b"bulk lane";
    memory
        .write(&mut store, 0, payload)
        .expect("memory write failed");

    let bytes_store = export(&mut store, &bridge, "bytes_store");
    let bytes_load = export(&mut store, &bridge, "bytes_load");
    let bin = call(&mut store, &bytes_store, &[Val::I32(payload.len() as i32)]);

    memory
        .write(&mut store, 0, &vec![0; payload.len()])
        .expect("memory clear failed");
    assert_eq!(
        call(&mut store, &bytes_load, std::slice::from_ref(&bin)).unwrap_i32(),
        payload.len() as i32
    );

    let mut copied = vec![0; payload.len()];
    memory
        .read(&store, 0, &mut copied)
        .expect("memory read failed");
    assert_eq!(&copied, payload);
}
