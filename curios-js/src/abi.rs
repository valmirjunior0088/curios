//! The ABI facts a JavaScript host needs, surfaced as one JS object — every
//! value derived from `curios-abi`, so the browser harness cannot drift from
//! the compiler and runtime the way a hand-copied constants file can.

use {
    curios_abi::{status, stdio, sys_io},
    js_sys::{Array, Object, Reflect},
    wasm_bindgen::prelude::*,
};

/// The import names of every store-described host operation, in store order.
/// The harness builds its `env` import object from exactly this roster, so a
/// new `sys_io` row that lacks a browser implementation fails loudly.
fn import_names() -> Vec<String> {
    sys_io()
        .iter()
        .map(|function| function.name.clone())
        .collect()
}

pub(crate) fn set(target: &Object, key: &str, value: &JsValue) {
    Reflect::set(target, &JsValue::from_str(key), value).expect("Reflect::set on a plain object");
}

fn constants(entries: &[(&str, u32)]) -> Object {
    let object = Object::new();

    for (key, value) in entries {
        set(&object, key, &JsValue::from_f64(f64::from(*value)));
    }

    object
}

pub(crate) fn abi_object() -> Object {
    let object = Object::new();

    set(
        &object,
        "sysNamespace",
        &JsValue::from_str(curios_abi::NAMESPACE_SYS),
    );
    set(
        &object,
        "envNamespace",
        &JsValue::from_str(curios_abi::NAMESPACE_ENV),
    );
    set(
        &object,
        "mainExport",
        &JsValue::from_str(curios_abi::MAIN_EXPORT),
    );
    set(
        &object,
        "importNames",
        &import_names()
            .iter()
            .map(|name| JsValue::from_str(name))
            .collect::<Array>(),
    );
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

/// The host/guest contract as a JS object: `sysNamespace`, `envNamespace`,
/// `mainExport`, `importNames`, and the `status`/`stdio` code tables.
#[wasm_bindgen]
pub fn abi() -> JsValue {
    abi_object().into()
}

#[cfg(test)]
mod tests {
    use curios_abi::sys_io;

    /// The roster the harness builds its `env` object from is the store, name for
    /// name, in store order. (`abi()` itself is JS-only — the object assembly
    /// can't run on the host — so the roster is pinned here instead.)
    #[test]
    fn import_names_are_the_store_rows() {
        let names = super::import_names();

        assert_eq!(
            names,
            sys_io()
                .iter()
                .map(|function| function.name.clone())
                .collect::<Vec<_>>()
        );
        assert_eq!(names.first().map(String::as_str), Some("io_read"));
        assert_eq!(names.last().map(String::as_str), Some("io_env"));
    }
}
