//! What the harness is configured with that no program changes: the codes it answers with, the standard streams' tokens, the first token it may mint, the report of a program suspended on nothing, and the name each host row is declared under beside the host method that answers it. Everything is read off `curios-abi`, so the browser cannot drift from the compiler and the native host as a hand-copied constants file could, and rendered once, as JSON.

use {
    curios_abi::{CodeCarrier, DEADLOCK, Failure, Handle, HostOp, code_sets, errno},
    serde_json::{Map, Value, json},
};

/// A handle's token as the harness keys it: the hex of its exact bytes. A handle crosses as its token's bytes, so a token is only ever the bytes it is: the empty token is not stdin's, and a padded or a long one names no stream.
fn token(handle: Handle) -> String {
    handle
        .bytes()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}

/// The harness's configuration, as JSON.
///
/// - `status`: every status code under its constant's name, as a decimal string, since a code is a `Nat` the harness answers as a `BigInt` and JSON carries no integer past 2⁵³ exactly.
/// - Every code set a byte carries, the poll masks among them, under the set's name, as numbers.
/// - `stdio`: the standard streams' tokens, each the hex of its exact bytes.
/// - `seed`: the first token the harness may mint.
/// - `deadlock`: what a host reports of a program suspended on nothing.
/// - `rows`: the name the standard library declares each host row under, beside the host method that answers it.
///
/// The code sets the harness answers no row with are left out.
pub fn configuration() -> String {
    let mut configuration = Map::new();

    // Each table is a set of `curios-abi`'s table of codes, every code the guest reads under its constant's name, so a code has one value on both sides of the wire by being read from one row.
    for group in code_sets() {
        let codes = group.codes.iter().map(|code| (code.constant, code.value));

        let table = match (group.name, group.carrier) {
            // A failure the harness answers by an errno of its own crosses in the status lane, as any host's does.
            ("status", _) => codes
                .chain([("EBADF", Failure::Other(errno::EBADF).code())])
                .map(|(constant, value)| (constant.to_string(), Value::from(value.to_string())))
                .collect::<Map<_, _>>(),
            (_, CodeCarrier::Byte) => codes
                .map(|(constant, value)| {
                    let mask = u8::try_from(value).expect("a code read as a byte fits one");

                    (constant.to_string(), Value::from(mask))
                })
                .collect(),
            // The harness answers no row that takes or reports a code of any other set.
            _ => continue,
        };

        configuration.insert(group.name.to_string(), Value::Object(table));
    }

    configuration.insert(
        "stdio".to_string(),
        json!({
            "STDIN": token(Handle::Stdin),
            "STDOUT": token(Handle::Stdout),
            "STDERR": token(Handle::Stderr),
        }),
    );
    configuration.insert("seed".to_string(), Value::from(Handle::HANDLE_SEED));
    configuration.insert("deadlock".to_string(), Value::from(DEADLOCK));
    configuration.insert(
        "rows".to_string(),
        Value::Object(
            HostOp::ALL
                .iter()
                .map(|op| (op.declared(), Value::from(op.name())))
                .collect(),
        ),
    );

    Value::Object(configuration).to_string()
}
