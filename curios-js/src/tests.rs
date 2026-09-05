//! Tests for the browser bridge helpers and the harness's host table. Program-side `Bytes` is a rope (`$rope/bin/leaf` / `$rope/bin/node` structs); what crosses to a host is always the forced flat payload, which is what the bridge accessors read and write.

use {
    curios_abi::{ENTRY, EXIT, PANIC, host_ops},
    curios_runtime::test_support::{GuestInstance, GuestValue},
    curios_wasm::{CompType, Export, SubType, TypeName},
};

fn func_arity(sub_type: &SubType) -> (usize, usize) {
    match &sub_type.comp_type {
        CompType::Func(func_type) => (func_type.inputs().len(), func_type.outputs().len()),
        comp_type => panic!("expected a func type, got {comp_type:?}"),
    }
}

fn bridge() -> GuestInstance {
    GuestInstance::instantiate(&crate::bridge_bytes()).expect("the bridge module instantiates")
}

/// Call `export` and return its single result. Untyped calls are the point: they type-check dynamically against the module's declared signatures, which succeeds exactly when the bridge's declared shapes are right.
fn call(bridge: &mut GuestInstance, export: &str, args: &[GuestValue]) -> GuestValue {
    let mut results = bridge.call(export, args).expect("call failed");
    assert_eq!(results.len(), 1, "{export} returns exactly one value");

    results.remove(0)
}

/// Call `export` for its effect, asserting it returns nothing.
fn call_void(bridge: &mut GuestInstance, export: &str, args: &[GuestValue]) {
    let results = bridge.call(export, args).expect("call failed");
    assert!(results.is_empty(), "{export} returns nothing");
}

fn i32_of(value: GuestValue) -> i32 {
    value.to_i32().expect("an i32 result")
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
        ("list_len", 1, 1),
        ("list_get", 2, 1),
        ("list_new", 1, 1),
        ("list_set", 3, 0),
        ("nat_box", 1, 1),
        ("nat_unbox", 1, 1),
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
    let mut bridge = bridge();

    // The `bin` this hands back is an opaque guest reference, threaded into every call below — which is why the test-support value type exists rather than a plain `i32`.
    let bin = call(&mut bridge, "bytes_new", &[GuestValue::from_i32(3)]);

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        call_void(
            &mut bridge,
            "bytes_set",
            &[
                bin,
                GuestValue::from_i32(index as i32),
                GuestValue::from_i32(value),
            ],
        );
    }

    assert_eq!(i32_of(call(&mut bridge, "bytes_len", &[bin])), 3);

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        assert_eq!(
            i32_of(call(
                &mut bridge,
                "bytes_get",
                &[bin, GuestValue::from_i32(index as i32)],
            )),
            value
        );
    }
}

/// A list built through the bridge holds what was set in it: an i31-boxed `Nat` comes back through the unbox, and the length is what `list_new` was asked for — the shape `poll` builds its `revents` list in.
#[test]
fn list_accessors_roundtrip_an_i31_element() {
    let mut bridge = bridge();

    let list = call(&mut bridge, "list_new", &[GuestValue::from_i32(2)]);
    assert_eq!(i32_of(call(&mut bridge, "list_len", &[list])), 2);

    let boxed = call(&mut bridge, "nat_box", &[GuestValue::from_i32(5)]);
    call_void(
        &mut bridge,
        "list_set",
        &[list, GuestValue::from_i32(1), boxed],
    );

    let element = call(&mut bridge, "list_get", &[list, GuestValue::from_i32(1)]);
    assert_eq!(i32_of(call(&mut bridge, "nat_unbox", &[element])), 5);
}

/// Every builtin host operation has an entry in `harness.js`'s `sys` import object — every `host_ops!` row, and the two `sys` imports that are not rows, `exit` and `panic`. The harness spells the wire names by hand, like any embedder — so without this check, a new `host_ops!` row keeps the workspace suite green while every browser program touching it dies with a `LinkError` only an actual browser can surface.
#[test]
fn harness_implements_every_host_op() {
    let source = include_str!("harness.js");

    let start = source
        .find("const sysEnv = {")
        .expect("harness.js declares sysEnv");
    let body = &source[start..];
    // The object closes at two-space indentation; every nested closure inside it closes deeper, so this marker is unambiguous.
    let end = body.find("\n  };").expect("sysEnv closes");
    let body = &body[..end];

    for name in host_ops()
        .iter()
        .map(|function| function.name.as_str())
        .chain([EXIT, PANIC])
    {
        assert!(
            body.contains(&format!("\n    {name}:")),
            "harness.js's sysEnv lacks an entry for host op `{name}`"
        );
    }

    // The entry export is the other wire name the harness spells by hand, and it is invoked outside `sysEnv`, so it is pinned on its own.
    assert!(
        source.contains(&format!("exports[\"{ENTRY}\"]")),
        "harness.js does not invoke the entry export `{ENTRY}`"
    );
}

/// The bulk lane end to end: bytes written into the exported memory become a `bytes` array via `bytes_store`, and `bytes_load` copies them back out.
#[test]
fn bulk_transfers_roundtrip_through_the_memory() {
    let mut bridge = bridge();

    assert_eq!(
        bridge.memory_size("memory").expect("a memory export"),
        0,
        "the memory starts empty"
    );
    bridge.memory_grow("memory", 1).expect("grow failed");

    let payload = b"bulk lane";
    bridge
        .memory_write("memory", 0, payload)
        .expect("memory write failed");

    let bin = call(
        &mut bridge,
        "bytes_store",
        &[GuestValue::from_i32(payload.len() as i32)],
    );

    bridge
        .memory_write("memory", 0, &vec![0; payload.len()])
        .expect("memory clear failed");
    assert_eq!(
        i32_of(call(&mut bridge, "bytes_load", &[bin])),
        payload.len() as i32
    );

    let mut copied = vec![0; payload.len()];
    bridge
        .memory_read("memory", 0, &mut copied)
        .expect("memory read failed");
    assert_eq!(&copied, payload);
}
