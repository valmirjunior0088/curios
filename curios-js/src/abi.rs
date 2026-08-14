//! The numeric wire codes a JavaScript host needs, surfaced as one JS object — derived from `curios-abi`, so the browser harness cannot drift from the compiler and runtime the way a hand-copied constants file can. The wire *names* (import namespaces, `sys.*` keys, the entry export) are spelled directly in `harness.js`, exactly as any embedder spells them.

use {
    crate::set,
    curios_abi::{status, stdio},
    js_sys::Object,
    wasm_bindgen::JsValue,
};

fn constants(entries: &[(&str, u32)]) -> Object {
    let object = Object::new();

    for (key, value) in entries {
        set(&object, key, &JsValue::from_f64(f64::from(*value)));
    }

    object
}

/// The numeric wire codes as a JS object: the `status`/`stdio` code tables.
pub(crate) fn abi() -> Object {
    let object = Object::new();
    set(
        &object,
        "status",
        &constants(&[
            ("OK", status::OK),
            ("EOF", status::EOF),
            ("NOT_FOUND", status::NOT_FOUND),
            ("PERMISSION_DENIED", status::PERMISSION_DENIED),
            ("ALREADY_EXISTS", status::ALREADY_EXISTS),
            ("CONNECTION_REFUSED", status::CONNECTION_REFUSED),
            ("WOULD_BLOCK", status::WOULD_BLOCK),
            ("TLS_ERROR", status::TLS_ERROR),
        ]),
    );
    set(
        &object,
        "stdio",
        &constants(&[
            ("STDIN", stdio::STDIN),
            ("STDOUT", stdio::STDOUT),
            ("STDERR", stdio::STDERR),
        ]),
    );

    object
}
