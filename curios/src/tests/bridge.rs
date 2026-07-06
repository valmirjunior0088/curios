//! End-to-end tests of curios-js's Bin bridge: the encoded module must be
//! valid wasm and its accessors must work. A program-side `Bin` is a rope
//! (`$bin/leaf` / `$bin/node` structs); what crosses to a host — and thus to
//! the bridge — is always the forced flat payload, which is what the
//! accessors read and write.

use wasmtime::{Func, Instance, Linker, Module, Store, Val};

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

#[test]
fn bridge_accessors_roundtrip() {
    let mut store = Store::new(curios_rt::shared_engine(), ());
    let bridge = instantiate(&mut store, &curios_js::bridge_bytes());

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
