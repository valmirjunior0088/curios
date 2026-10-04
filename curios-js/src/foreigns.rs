//! A program's own `foreign` rows as plain data: what `compile` hands back beside the module, as the native bundle carries its `ForeignStore` beside its payload, and what `run` holds each hook to. One `{ name, params, results }` per row — `name` the fully qualified name the program imports it under, `params` the operands' types in order, `results` each result's `{ type }`, under its `label` where the row answers a record of them — with every type spelled as the declaration spells it (`Nat`, `List(Bool)`), by `WireType`'s own `Display`, which is the spelling an embedder reads in the program's source.

use {
    crate::set,
    curios_abi::{ForeignStore, ResultShape, WireType},
    js_sys::{Array, Object},
    wasm_bindgen::JsValue,
};

/// One result as the harness reads it: its wire type, under the label a record's field has.
fn result(label: Option<&str>, wire_type: WireType) -> JsValue {
    let result = Object::new();

    if let Some(label) = label {
        set(&result, "label", &JsValue::from_str(label));
    }
    set(&result, "type", &JsValue::from_str(&wire_type.to_string()));

    JsValue::from(result)
}

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
                    .map(|wire_type| JsValue::from_str(&wire_type.to_string()))
                    .collect::<Array>(),
            );
            set(
                &row,
                "results",
                &match signature.results.shape() {
                    ResultShape::Unit => Array::new(),
                    ResultShape::Single(wire_type) => Array::of1(&result(None, wire_type)),
                    ResultShape::Record(fields) => fields
                        .into_iter()
                        .map(|(label, wire_type)| result(Some(label), wire_type))
                        .collect::<Array>(),
                }
                .into(),
            );

            JsValue::from(row)
        })
        .collect()
}
