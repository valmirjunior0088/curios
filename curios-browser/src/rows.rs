//! A program's own `foreign` rows as plain data: what `compile` hands back beside the module, as the native bundle carries its `ForeignStore` beside its payload, and what the harness holds each hook to. One `{ name, params, results }` per row — `name` the fully qualified name the program imports it under, `params` the operands' types in order, `results` each result's `{ type }`, under its `label` where the row answers a record of them, and `never: true` in place of `results` for a row whose call does not return — with every type spelled as the declaration spells it (`Nat`, `List(Bool)`), by `WireType`'s own `Display`, which is the spelling an embedder reads in the program's source. Rendered once, as JSON.

use {
    curios_abi::{ForeignStore, ResultShape, WireReturn, WireType},
    serde::Serialize,
};

/// One row as the harness reads it.
#[derive(Serialize)]
struct Row {
    name: String,
    params: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    results: Option<Vec<Answer>>,
    #[serde(skip_serializing_if = "is_false")]
    never: bool,
}

/// One result as the harness reads it: its wire type, under the label a record's field has.
#[derive(Serialize)]
struct Answer {
    #[serde(skip_serializing_if = "Option::is_none")]
    label: Option<String>,
    #[serde(rename = "type")]
    wire_type: String,
}

/// Whether `value` is false, which a row that returns states by stating nothing.
fn is_false(value: &bool) -> bool {
    !*value
}

fn answer(label: Option<&str>, wire_type: WireType) -> Answer {
    Answer {
        label: label.map(str::to_string),
        wire_type: wire_type.to_string(),
    }
}

/// Every row of `foreigns`, in declaration order, as JSON.
pub fn rows(foreigns: &ForeignStore) -> String {
    let rows = foreigns
        .iter()
        .map(|function| {
            let signature = function.signature();
            let (results, never) = match &signature.returns {
                WireReturn::Never => (None, true),
                WireReturn::Answers(results) => (
                    Some(match results.shape() {
                        ResultShape::Unit => Vec::new(),
                        ResultShape::Single(wire_type) => vec![answer(None, wire_type)],
                        ResultShape::Record(fields) => fields
                            .into_iter()
                            .map(|(label, wire_type)| answer(Some(label), wire_type))
                            .collect(),
                    }),
                    false,
                ),
            };

            Row {
                name: function.name().to_string(),
                params: signature.params.iter().map(ToString::to_string).collect(),
                results,
                never,
            }
        })
        .collect::<Vec<_>>();

    serde_json::to_string(&rows).expect("rows of strings render as JSON")
}
