//! The wire codes a JavaScript host needs, surfaced as one JS object — derived from `curios-abi`, so the browser harness cannot drift from the compiler and runtime the way a hand-copied constants file can. The wire *names* (import namespaces, `sys.*` keys, the entry export) are spelled directly in `harness.js`, exactly as any embedder spells them.

use {
    crate::set,
    curios_abi::{Handle, Poll, errno, event, file_kind, status, stdio_mode},
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

/// The wire codes as a JS object: the `status`/`file_kind`/`stdio_mode` code tables as `BigInt`s, the `event` masks as numbers, and the `stdio` tokens as hex.
pub(crate) fn abi() -> Object {
    let object = Object::new();
    set(
        &object,
        "status",
        &codes(&[
            ("OK", status::OK),
            ("EOF", status::EOF),
            ("NOT_FOUND", status::NOT_FOUND),
            ("PERMISSION_DENIED", status::PERMISSION_DENIED),
            ("ALREADY_EXISTS", status::ALREADY_EXISTS),
            ("CONNECTION_REFUSED", status::CONNECTION_REFUSED),
            ("WOULD_BLOCK", status::WOULD_BLOCK),
            ("TLS_ERROR", status::TLS_ERROR),
            ("NOT_EMPTY", status::NOT_EMPTY),
            ("IS_DIRECTORY", status::IS_DIRECTORY),
            ("NOT_DIRECTORY", status::NOT_DIRECTORY),
            ("EBADF", status::OTHER_BASE + u64::from(errno::EBADF)),
        ]),
    );
    set(
        &object,
        "file_kind",
        &codes(&[
            ("FILE", file_kind::FILE),
            ("DIRECTORY", file_kind::DIRECTORY),
            ("SYMLINK", file_kind::SYMLINK),
            ("OTHER", file_kind::OTHER),
        ]),
    );
    set(
        &object,
        "stdio_mode",
        &codes(&[
            ("INHERIT", stdio_mode::INHERIT),
            ("PIPE", stdio_mode::PIPE),
            ("NULL", stdio_mode::NULL),
        ]),
    );
    set(
        &object,
        "event",
        &masks(&[
            ("READ", event::READ),
            ("WRITE", event::WRITE),
            ("ERR", event::ERR),
            ("INTEREST", Poll::INTEREST),
        ]),
    );
    set(
        &object,
        "stdio",
        &tokens(&[
            ("STDIN", Handle::Stdin),
            ("STDOUT", Handle::Stdout),
            ("STDERR", Handle::Stderr),
        ]),
    );

    object
}
