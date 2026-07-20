//! Tests for the browser bridge helpers. Program-side `Bytes` is a rope
//! (`$rope/bin/leaf` / `$rope/bin/node` structs); what crosses to a host is always the
//! forced flat payload, which is what the bridge accessors read and write.

use {
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

/// Call `func` untyped and return its single result. Untyped calls are the
/// point: they type-check dynamically, which succeeds exactly when the
/// bridge's declared shapes are right.
fn call(store: &mut Store<()>, func: &Func, params: &[Val]) -> Val {
    let mut results = vec![Val::I32(0); 1];

    func.call(&mut *store, params, &mut results)
        .expect("call failed");

    results.remove(0)
}

/// The bridge's `bytes` declaration matches the compiler's `array (mut i8)`
/// host-boundary shape — the premise of the whole canonicalization scheme.
#[test]
fn bytes_type_is_the_compiler_boundary_shape() {
    assert_eq!(
        crate::bridge::bridge_module().get_type(&TypeName::from("bytes")),
        Some(&crate::bridge::bytes_sub_type())
    );
}

#[test]
fn accessors_are_exported_with_their_shapes() {
    let module = crate::bridge::bridge_module();

    for (name, inputs, outputs) in [
        ("bin_len", 1, 1),
        ("bin_get", 2, 1),
        ("bin_new", 1, 1),
        ("bin_set", 3, 0),
        ("bin_load", 1, 1),
        ("bin_store", 1, 1),
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
    let mut store = Store::new(curios_runtime::shared_engine(), ());
    let bridge = instantiate(&mut store, &crate::bridge_bytes());

    let bin_new = export(&mut store, &bridge, "bin_new");
    let bin_set = export(&mut store, &bridge, "bin_set");
    let bin_get = export(&mut store, &bridge, "bin_get");
    let bin_len = export(&mut store, &bridge, "bin_len");

    let bin = call(&mut store, &bin_new, &[Val::I32(3)]);

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        bin_set
            .call(
                &mut store,
                &[bin, Val::I32(index as i32), Val::I32(value)],
                &mut [],
            )
            .expect("bin_set failed");
    }

    assert_eq!(
        call(&mut store, &bin_len, std::slice::from_ref(&bin)).unwrap_i32(),
        3
    );

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        assert_eq!(
            call(&mut store, &bin_get, &[bin, Val::I32(index as i32)]).unwrap_i32(),
            value
        );
    }
}

/// The bulk lane end to end: bytes written into the exported memory become a
/// `bytes` array via `bin_store`, and `bin_load` copies them back out.
#[test]
fn bulk_transfers_roundtrip_through_the_memory() {
    let mut store = Store::new(curios_runtime::shared_engine(), ());
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

    let bin_store = export(&mut store, &bridge, "bin_store");
    let bin_load = export(&mut store, &bridge, "bin_load");
    let bin = call(&mut store, &bin_store, &[Val::I32(payload.len() as i32)]);

    memory
        .write(&mut store, 0, &vec![0; payload.len()])
        .expect("memory clear failed");
    assert_eq!(
        call(&mut store, &bin_load, std::slice::from_ref(&bin)).unwrap_i32(),
        payload.len() as i32
    );

    let mut copied = vec![0; payload.len()];
    memory
        .read(&store, 0, &mut copied)
        .expect("memory read failed");
    assert_eq!(&copied, payload);
}
