//! End-to-end tests of curios-js's Bin bridge: the encoded module must be
//! valid wasm, its accessors must work, and — the point of the whole scheme —
//! its `bin` type must canonicalize with the `Bin` type of a compiled
//! program, so refs flow between the two instances.

use {
    crate::cont::{self, to_wasm},
    wasmtime::{Func, Instance, Linker, Module, Store, Val},
};

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
                &[bin.clone(), Val::I32(index as i32), Val::I32(value)],
                &mut [],
            )
            .expect("bin_set failed");
    }

    assert_eq!(call(&mut store, &bin_len, &[bin.clone()]).unwrap_i32(), 3);

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        assert_eq!(
            call(&mut store, &bin_get, &[bin.clone(), Val::I32(index as i32)]).unwrap_i32(),
            value
        );
    }
}

/// The canonicalization proof: a compiled program's `Bin` result, produced in
/// a separate instance, reads back through the bridge's accessors. If the two
/// `array (mut i8)` declarations ever diverged, the `bin_len`/`bin_get` calls
/// below would fail wasmtime's dynamic type check.
#[test]
fn program_bins_flow_through_the_bridge() {
    let mut program = cont::Module::new();
    program.set_entry(cont::FuncName::from("main"));
    program.add_const(
        cont::ValueName::from("BYTES"),
        cont::Data::Bin(vec![7, 8, 9]),
    );

    program.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume: cont::BlockName::from("r"),
            region: cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: cont::Tail::Jump(cont::JumpTarget {
                    target: cont::BlockName::from("r"),
                    params: vec![cont::ValueName::from("BYTES")],
                }),
            },
        },
    );

    let mut store = Store::new(curios_rt::shared_engine(), ());
    let bridge = instantiate(&mut store, &curios_js::bridge_bytes());
    let program = instantiate(&mut store, &crate::wasm::to_bytes(&to_wasm(&program)));

    let main = export(&mut store, &program, crate::abi::MAIN_EXPORT);
    let bin = call(&mut store, &main, &[]);

    let bin_len = export(&mut store, &bridge, "bin_len");
    let bin_get = export(&mut store, &bridge, "bin_get");

    assert_eq!(call(&mut store, &bin_len, &[bin.clone()]).unwrap_i32(), 3);

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        assert_eq!(
            call(&mut store, &bin_get, &[bin.clone(), Val::I32(index as i32)]).unwrap_i32(),
            value
        );
    }
}
