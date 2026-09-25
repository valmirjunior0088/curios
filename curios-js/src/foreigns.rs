//! A program's own `foreign` rows as plain data: what `compile` hands back beside the module, as the native bundle carries its `ForeignStore` beside its payload, and what `run` holds each hook to. One `{ name, params, results }` per row — `name` the fully qualified name the program imports it under, `params` the operands' types in order, `results` each result's `{ label, type }` — with every type spelled as the declaration spells it (`Nat`, `List(Bool)`), by `WireType`'s own `Display`, which is the spelling an embedder reads in the program's source.

use {
    crate::set,
    curios_abi::ForeignStore,
    js_sys::{Array, Object},
    wasm_bindgen::JsValue,
};

/// Every row of `foreigns`, in declaration order.
pub(crate) fn foreign_rows(foreigns: &ForeignStore) -> Array {
    foreigns
        .iter()
        .map(|function| {
            let signature = function.signature();
            let row = Object::new();

            set(&row, "name", &JsValue::from_str(function.name()));
            set(
                &row,
                "params",
                &signature
                    .params
                    .iter()
                    .map(|(_, wire_type)| JsValue::from_str(&wire_type.to_string()))
                    .collect::<Array>(),
            );
            set(
                &row,
                "results",
                &signature
                    .results
                    .iter()
                    .map(|(label, wire_type)| {
                        let result = Object::new();

                        set(&result, "label", &JsValue::from_str(label));
                        set(&result, "type", &JsValue::from_str(&wire_type.to_string()));

                        JsValue::from(result)
                    })
                    .collect::<Array>(),
            );

            JsValue::from(row)
        })
        .collect()
}
