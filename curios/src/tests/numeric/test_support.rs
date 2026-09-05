//! Programs the numeric suites compile and run.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

//! The numeric envelope gates: every constant folder computes in exact `u32`/`i32` (the numeric law), and the i31 backend boundary appears only as a trap in emitted Wasm — an overflowing computation traps, and a folded literal the carrier cannot box traps at its materialization point. The differential half runs each scalar expression twice — fully constant (folded at compile time) and with a runtime-zero perturbation (executed by the emitted Wasm) — and demands identical output, pinning the folders and the backend to one semantics.

use {
    crate::tests::{Compiled, compile},
    curios_runtime::MockHost,
};

/// One program holding every row of a table, the row chosen by the host: its stdin line is two bytes, the first read as the runtime zero `n` (`'A' − 65`) with its `Int` twin `i`, which is what keeps every row out of the folder's reach, and the second naming the row. One compile therefore serves the whole table, and a run costs milliseconds — the reason this is a selector rather than one program per row. With `taint` false, `n` and `i` are literal zeros instead, so every row folds to a literal while the selection alone stays runtime, which is what "folded" asserts.
pub(super) fn table(rows: &[&str], taint: bool) -> String {
    let zero = if taint {
        "Nat/sub(Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0)), 65)"
    } else {
        "0"
    };
    let arms = rows
        .iter()
        .enumerate()
        .map(|(index, row)| format!("| {index} => /std/print({row})"))
        .collect::<Vec<_>>()
        .join("\n        ");
    format!(
        r#"
        use /std/{{Nat, Int, Flt, Byte, Bytes, Str, Option, Io}};
        let bytes = match Io/read(Io/stdin, 16)! : (_) => Bytes
            | chunk(b) => b
            | eof() => x[]
            | error(_) => x[]
            end;
        let n = {zero};
        let i = Nat/to_int(n);
        let to_nat_or(x : Int, d : Nat) -> Nat =
            match x >= +0 | true => Int/to_nat(x) | false => d end;
        let row = Nat/sub(Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 1), 0)), 32);
        match row
        {arms}
        | _ => /std/print("no such row")
        end
        "#
    )
}

/// Run row `index` of a compiled table: the taint byte, then the row byte, printable so the line stays one.
pub(super) fn run_row(compiled: &Compiled, index: usize) -> Result<Vec<u8>, String> {
    let row = u8::try_from(index).expect("a table of under 95 rows") + b' ';
    let (system, io) = MockHost::builder().stdin_lines([[b'A', row]]).build();
    compiled.run(system)?;
    Ok(io.output())
}

/// Compile `rows` closed and tainted — two compiles for the table — and assert each row's folded and executed answers agree byte-for-byte. The executed answers are returned, for a fixture that also pins what they are.
pub(super) fn folded_matches_runtime(rows: &[&str]) -> Vec<Vec<u8>> {
    let folded = compile(&table(rows, false)).expect("the closed table compiles");
    let executed = compile(&table(rows, true)).expect("the tainted table compiles");
    rows.iter()
        .enumerate()
        .map(|(index, row)| {
            let folded = run_row(&folded, index).expect("a folded row runs");
            let executed = run_row(&executed, index).expect("in-envelope expression executes");
            assert_eq!(folded, executed, "fold/runtime disagreement on: {row}");
            executed
        })
        .collect()
}

/// Compile `rows` tainted and assert each traps at the backend boundary when it is the row selected.
pub(super) fn runtime_traps(rows: &[&str]) {
    let executed = compile(&table(rows, true)).expect("the tainted table compiles");
    for (index, row) in rows.iter().enumerate() {
        let error = run_row(&executed, index).expect_err("expression should trap");
        assert!(
            error.contains("execution failed"),
            "expected a runtime trap for {row}, got: {error}"
        );
    }
}
