//! `/std/Byte` as the width-8 word: eight operations over `to_nat` and `of_nat`, each restating the width the runtime carrier will not.

use super::test_support::folded_matches_runtime;

/// **The width is restated at every operation, and these rows are what says the restatement works.** `Byte` rides the same unbounded `Nat` the theory folds and the same `i31` the runtime boxes, so nothing narrows a result to eight bits on its own: `and`, `or` and `xor` inherit the width from their operands, `not` takes the distance to the all-ones byte, and everything built on a shift masks. Each operand and count below is tainted through `n`, the table's runtime zero, so what answers is the emitted instruction rather than the folder — and the helper compiles the table both ways and pins the two to the same bytes, which is what makes a row here a statement about the language rather than about one stage.
///
/// **The two shift rows are the design, not the coverage.** `shl` guards its count instead of masking the product: `Nat` is unbounded, so `0x01 << 300` would build the whole magnitude before a mask could narrow it and refuse at the envelope on the way to an answer that is plainly `0`. The guard computes that zero. `rotl` and `rotr` need no guard for the same reason read differently — a count taken modulo eight carries its bound *in the term*, so the shift under it can never reach far enough to matter, and the rotation by a full width is the identity that says the modulo is really there.
#[test]
fn every_byte_word_operation_answers_at_width_eight() {
    let rows = [
        (
            "Nat/to_str(Byte/to_nat(Byte/and(Byte/of_nat(Nat/and(0xF0 + n, 255)), 0x3C)))",
            "48",
        ),
        (
            "Nat/to_str(Byte/to_nat(Byte/or(Byte/of_nat(Nat/and(0xF0 + n, 255)), 0x0C)))",
            "252",
        ),
        (
            "Nat/to_str(Byte/to_nat(Byte/xor(Byte/of_nat(Nat/and(0xFF + n, 255)), 0x0F)))",
            "240",
        ),
        (
            "Nat/to_str(Byte/to_nat(Byte/not(Byte/of_nat(Nat/and(0x0F + n, 255)))))",
            "240",
        ),
        ("Nat/to_str(Byte/to_nat(Byte/shl(0x81, 1 + n)))", "2"),
        ("Nat/to_str(Byte/to_nat(Byte/shl(0x01, 300 + n)))", "0"),
        ("Nat/to_str(Byte/to_nat(Byte/shr(0x81, 1 + n)))", "64"),
        ("Nat/to_str(Byte/to_nat(Byte/rotl(0x81, 1 + n)))", "3"),
        ("Nat/to_str(Byte/to_nat(Byte/rotr(0x81, 1 + n)))", "192"),
        ("Nat/to_str(Byte/to_nat(Byte/rotl(0xAB, 0 + n)))", "171"),
        ("Nat/to_str(Byte/to_nat(Byte/rotl(0xAB, 8 + n)))", "171"),
        (
            "Nat/to_str(Byte/to_nat(Byte/of_nat(Nat/and(0xF0 + n, 255)) && 0x3C))",
            "48",
        ),
        (
            "Nat/to_str(Byte/to_nat(Byte/of_nat(Nat/and(0xF0 + n, 255)) || 0x0C))",
            "252",
        ),
    ];
    let sources = rows.iter().map(|(row, _)| *row).collect::<Vec<_>>();

    for (index, output) in folded_matches_runtime(&sources).into_iter().enumerate() {
        let (row, expected) = rows[index];
        assert_eq!(
            String::from_utf8_lossy(&output).trim(),
            expected,
            "expected the width-8 answer for {row}",
        );
    }
}
