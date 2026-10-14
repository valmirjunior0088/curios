//! The wire codes a JavaScript host needs, surfaced as one JS object — derived from `curios-abi`, so the browser harness cannot drift from the compiler and runtime the way a hand-copied constants file can. The wire *names* (import namespaces, `panic`, the entry export) and the host's methods are spelled directly in `harness.js`, exactly as any embedder spells them.

use {
    crate::set,
    curios_abi::{CodeCarrier, DEADLOCK, Failure, Handle, HostOp, code_sets, errno},
    js_sys::Object,
    wasm_bindgen::JsValue,
};

/// A code table of `BigInt`s: each code is a `Nat`, which crosses as an `i64`, and JavaScript reads and writes an `i64` as a `BigInt` — so a status the harness hands back must be one.
fn codes(entries: &[(&str, u64)]) -> Object {
    let object = Object::new();

    for (key, value) in entries {
        set(&object, key, &JsValue::from(*value));
    }

    object
}

/// A table of plain numbers: the poll masks, which cross as the bytes of a `Bytes` rather than as `Nat`s.
fn masks(entries: &[(&str, u8)]) -> Object {
    let object = Object::new();

    for (key, value) in entries {
        set(&object, key, &JsValue::from_f64(f64::from(*value)));
    }

    object
}

/// The standard streams' tokens as the hex of their exact bytes. A handle crosses as its token's bytes and the harness keys every token on their hex, so a token is only ever the bytes it is: the empty token is not stdin's, and a padded or a long one names no stream.
fn tokens(entries: &[(&str, Handle)]) -> Object {
    let object = Object::new();

    for (key, handle) in entries {
        let hex = handle
            .bytes()
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<String>();

        set(&object, key, &JsValue::from_str(&hex));
    }

    object
}

/// The rows a host answers: the name the standard library declares each under, which is the name a program imports it by, beside the host method that answers it.
fn rows() -> Object {
    let object = Object::new();

    for op in HostOp::ALL {
        set(&object, &op.declared(), &JsValue::from_str(op.name()));
    }

    object
}

/// The wire codes as a JS object: the `status`/`file_kind`/`stdio_mode` code tables as `BigInt`s, the `event` masks as numbers, the `stdio` tokens as hex, `seed`, the first token a host may mint, as a number, `deadlock`, what a host reports of a program suspended on nothing, and `rows`, the name the standard library declares each host row under beside the method that answers it.
pub(crate) fn abi() -> Object {
    let object = Object::new();

    // Each table is a set of `curios-abi`'s table of codes, every code the guest reads under its constant's name, so a code has one value on both sides of the wire by being read from one row.
    for group in code_sets() {
        let entries = group.codes.iter().map(|code| (code.constant, code.value));

        let table = match (group.name, group.carrier) {
            // A failure the harness answers by an errno of its own crosses in the status lane, as any host's does.
            ("status", _) => Some(codes(
                &entries
                    .chain([("EBADF", Failure::Other(errno::EBADF).code())])
                    .collect::<Vec<_>>(),
            )),
            ("file_kind" | "stdio_mode", _) => Some(codes(&entries.collect::<Vec<_>>())),
            (_, CodeCarrier::Byte) => Some(masks(
                &entries
                    .map(|(constant, value)| {
                        let flag = u8::try_from(value).expect("a code read as a byte fits one");

                        (constant, flag)
                    })
                    .collect::<Vec<_>>(),
            )),
            // The harness answers no row that takes or reports a code of any other set.
            _ => None,
        };

        if let Some(table) = table {
            set(&object, group.name, &table);
        }
    }

    set(
        &object,
        "stdio",
        &tokens(&[
            ("STDIN", Handle::Stdin),
            ("STDOUT", Handle::Stdout),
            ("STDERR", Handle::Stderr),
        ]),
    );
    set(
        &object,
        "seed",
        &JsValue::from_f64(f64::from(Handle::HANDLE_SEED)),
    );
    set(&object, "deadlock", &JsValue::from_str(DEADLOCK));
    set(&object, "rows", &rows());

    object
}
