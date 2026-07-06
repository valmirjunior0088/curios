//! End-to-end tests of curios-js's Bin bridge: the encoded module must be
//! valid wasm, its accessors must work, and — the point of the whole scheme —
//! its `bytes` type must canonicalize with the `$bytes` payload type of a
//! compiled program, so refs flow between the two instances. A program-side
//! `Bin` is a rope (`$bin/leaf` / `$bin/node` structs); what crosses to a
//! host — and thus to the bridge — is always the forced flat payload.

use {
    curios_cont::to_wasm,
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

/// The canonicalization proof: a compiled program's `Bin` payload, produced
/// in a separate instance, reads back through the bridge's accessors. If the
/// two `array (mut i8)` declarations ever diverged, the `bin_len`/`bin_get`
/// calls below would fail wasmtime's dynamic type check.
///
/// `main` answers the rope value itself (a `$bin/leaf` here — a literal is
/// always a leaf), so the test projects its payload field first: the same
/// flat array a host call's forced parameter would carry across the wire.
#[test]
fn program_bins_flow_through_the_bridge() {
    let mut program = curios_cont::Module::new();
    program.set_entry(curios_cont::FuncName::from("main"));
    program.add_const(
        curios_cont::ValueName::from("BYTES"),
        curios_cont::Data::Bin(vec![7, 8, 9]),
    );

    program.add_func(
        curios_cont::FuncName::from("main"),
        curios_cont::Func {
            params: vec![],
            resume: curios_cont::BlockName::from("r"),
            region: curios_cont::Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: curios_cont::Tail::Jump(curios_cont::JumpTarget {
                    target: curios_cont::BlockName::from("r"),
                    params: vec![curios_cont::ValueName::from("BYTES")],
                }),
            },
        },
    );

    let mut store = Store::new(curios_rt::shared_engine(), ());
    let bridge = instantiate(&mut store, &curios_js::bridge_bytes());
    let program = instantiate(&mut store, &curios_wasm::to_bytes(&to_wasm(&program)));

    let main = export(&mut store, &program, curios_abi::MAIN_EXPORT);
    let rope = call(&mut store, &main, &[]);

    // Project the leaf's payload (field 2: tag, len, bytes).
    let Val::AnyRef(Some(rope)) = &rope else {
        panic!("expected a non-null rope result");
    };
    let leaf = rope
        .as_struct(&store)
        .expect("rope result is unrooted")
        .expect("expected a struct rope result");
    let bin = leaf.field(&mut store, 2).expect("leaf lacks a payload");

    let bin_len = export(&mut store, &bridge, "bin_len");
    let bin_get = export(&mut store, &bridge, "bin_get");

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
