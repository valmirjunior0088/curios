//! Tests for the browser bridge helpers, the harness's host table and what the harness is configured with. Program-side `Bytes` is a rope (`$rope/bytes/leaf` / `$rope/bytes/node` structs); what crosses to a host is always the forced flat payload, which is what the bridge accessors read and write.

use {
    curios_abi::{DEADLOCK, ENTRY, HostOp, PANIC},
    curios_native::test_support::{GuestInstance, GuestValue},
    curios_utilities::fill,
    curios_wasm::{CompType, Export, SubType, TypeName},
};

fn func_arity(sub_type: &SubType) -> (usize, usize) {
    match &sub_type.comp_type {
        CompType::Func(func_type) => (func_type.inputs().len(), func_type.outputs().len()),
        comp_type => panic!("expected a func type, got {comp_type:?}"),
    }
}

fn bridge() -> GuestInstance {
    let code = curios_cranelift::compile(&crate::bridge_bytes()).expect("the bridge compiles");

    GuestInstance::instantiate(&code).expect("the bridge module instantiates")
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
        ("longs_len", 1, 1),
        ("longs_get", 2, 1),
        ("longs_new", 1, 1),
        ("longs_set", 3, 0),
        ("words_len", 1, 1),
        ("words_get", 2, 1),
        ("words_new", 1, 1),
        ("words_set", 3, 0),
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

    // The `bytes` this hands back is an opaque guest reference, threaded into every call below — which is why the test-support value type exists rather than a plain `i32`.
    let bytes = call(&mut bridge, "bytes_new", &[GuestValue::from_i32(3)]);

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        call_void(
            &mut bridge,
            "bytes_set",
            &[
                bytes,
                GuestValue::from_i32(index as i32),
                GuestValue::from_i32(value),
            ],
        );
    }

    assert_eq!(i32_of(call(&mut bridge, "bytes_len", &[bytes])), 3);

    for (index, value) in [7, 8, 9].into_iter().enumerate() {
        assert_eq!(
            i32_of(call(
                &mut bridge,
                "bytes_get",
                &[bytes, GuestValue::from_i32(index as i32)],
            )),
            value
        );
    }
}

/// A list built through the bridge holds what was set in it: a byte string comes back as the element it went in as, and the length is what `list_new` was asked for — the shape a `List(Bytes)` result is built in.
#[test]
fn list_accessors_roundtrip_an_element() {
    let mut bridge = bridge();

    let list = call(&mut bridge, "list_new", &[GuestValue::from_i32(2)]);
    assert_eq!(i32_of(call(&mut bridge, "list_len", &[list])), 2);

    let bytes = call(&mut bridge, "bytes_new", &[GuestValue::from_i32(3)]);
    call_void(
        &mut bridge,
        "list_set",
        &[list, GuestValue::from_i32(1), bytes],
    );

    let element = call(&mut bridge, "list_get", &[list, GuestValue::from_i32(1)]);
    assert_eq!(i32_of(call(&mut bridge, "bytes_len", &[element])), 3);
}

/// A list of `Nat` or `Int` built through the bridge holds the numbers set in it, whole: one past the 32-bit word and a negative one come back as they went in — the shape a `List(Nat)` or `List(Int)` result is built in, which the guest boxes after the call.
#[test]
fn longs_accessors_roundtrip_a_number_past_the_word() {
    let mut bridge = bridge();

    let longs = call(&mut bridge, "longs_new", &[GuestValue::from_i32(2)]);
    assert_eq!(i32_of(call(&mut bridge, "longs_len", &[longs])), 2);

    for (index, long) in [(0, 1 << 40), (1, -7)] {
        call_void(
            &mut bridge,
            "longs_set",
            &[
                longs,
                GuestValue::from_i32(index),
                GuestValue::from_i64(long),
            ],
        );
        let read = call(
            &mut bridge,
            "longs_get",
            &[longs, GuestValue::from_i32(index)],
        );
        assert_eq!(read.to_i64(), Some(long));
    }
}

/// A list of `Bool` built through the bridge holds the words set in it — the shape a `List(Bool)` result is built in.
#[test]
fn words_accessors_roundtrip_a_word() {
    let mut bridge = bridge();

    let words = call(&mut bridge, "words_new", &[GuestValue::from_i32(2)]);
    assert_eq!(i32_of(call(&mut bridge, "words_len", &[words])), 2);

    for (index, word) in [(0, 1), (1, 0)] {
        call_void(
            &mut bridge,
            "words_set",
            &[
                words,
                GuestValue::from_i32(index),
                GuestValue::from_i32(word),
            ],
        );
        let read = call(
            &mut bridge,
            "words_get",
            &[words, GuestValue::from_i32(index)],
        );
        assert_eq!(i32_of(read), word);
    }
}

/// Every row a host answers has an entry in `harness.mjs`'s `host` object, keyed by the host's method name — every `for_each_host_op!` row, `proc_exit` among them — and the one `sys` import, `panic`, is defined beside it. The harness spells those names by hand, like any embedder, so a row added to the table without one is found here and not by the program that first calls it.
#[test]
fn harness_implements_every_host_op() {
    let source = include_str!("harness.mjs");

    let start = source
        .find("const host = {")
        .expect("harness.mjs declares its host table");
    let body = &source[start..];
    // The object closes at two-space indentation; every nested closure inside it closes deeper, so this marker is unambiguous.
    let end = body.find("\n  };").expect("the host table closes");
    let body = &body[..end];

    for name in HostOp::ALL.iter().map(|op| op.name()) {
        assert!(
            body.contains(&format!("\n    {name}:")),
            "harness.mjs's host table lacks an entry for host op `{name}`"
        );
    }

    assert!(
        source.contains(&fill!("sys: { ${PANIC} }", PANIC)),
        "harness.mjs does not define the `sys` import `{PANIC}`"
    );

    // The entry export is the other wire name the harness spells by hand, and it is invoked outside `sysEnv`, so it is pinned on its own.
    assert!(
        source.contains(&format!("exports[\"{ENTRY}\"]")),
        "harness.mjs does not invoke the entry export `{ENTRY}`"
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

    let bytes = call(
        &mut bridge,
        "bytes_store",
        &[GuestValue::from_i32(payload.len() as i32)],
    );

    bridge
        .memory_write("memory", 0, &vec![0; payload.len()])
        .expect("memory clear failed");
    assert_eq!(
        i32_of(call(&mut bridge, "bytes_load", &[bytes])),
        payload.len() as i32
    );

    let mut copied = vec![0; payload.len()];
    bridge
        .memory_read("memory", 0, &mut copied)
        .expect("memory read failed");
    assert_eq!(&copied, payload);
}

/// The configuration holds what the harness reads of it: every table it takes apart, each status code as the decimal string it reads as a `BigInt`, the deadlock report, and a declared name for every host row.
#[test]
fn the_configuration_holds_what_the_harness_reads() {
    let configuration =
        serde_json::from_str::<serde_json::Value>(&crate::configuration()).expect("JSON");

    for key in ["status", "event", "stdio", "seed", "deadlock", "rows"] {
        assert!(
            configuration.get(key).is_some(),
            "the configuration lacks `{key}`"
        );
    }
    assert!(
        configuration["status"]
            .as_object()
            .expect("a table")
            .values()
            .all(|code| code
                .as_str()
                .is_some_and(|code| code.parse::<u64>().is_ok())),
        "{}",
        configuration["status"]
    );
    assert_eq!(configuration["deadlock"], DEADLOCK);
    for op in HostOp::ALL {
        assert_eq!(configuration["rows"][op.declared().as_str()], op.name());
    }
}

/// JavaScriptCore cannot list the imports of a module that crosses GC types, and refuses with a `TypeError` where asked, so a harness or a loader asking would stop every page in Safari before its program ran. The import object is built from the program's rows instead, and no CI engine is JavaScriptCore, so the question is held absent here.
#[test]
fn neither_the_harness_nor_the_loader_asks_an_engine_for_a_modules_imports() {
    for (file, source) in [
        ("harness.mjs", include_str!("harness.mjs")),
        ("templates/index.js", include_str!("../templates/index.js")),
    ] {
        assert!(
            !source.contains("Module.imports"),
            "{file} asks for a module's imports"
        );
    }
}
