//! A bounded operation's domain, pinned at its edge: the nearest operand inside compiles, and the nearest outside is refused where it is written, for its bound.
//!
//! **A pair per bound, because `/sys` states none of its own.** Each `/sys` declaration's bound is read off `Intrinsic::signature`, so a row that drew its edge one off — `<=` where `<` was meant — would be copied into the declaration faithfully and admit the operand. A second spelling of the row could only say that two spellings agree; a program on each side of the edge says what the bound means.

use crate::tests::typecheck;

/// One bound at its edge: the operation, as a refusal names it, and the call on each side.
struct Edge {
    operation: &'static str,
    inside: &'static str,
    outside: &'static str,
}

const HEADER: &str =
    "use /std/{Nat, Int, Flt, Bool, Byte, Bytes, Bits, List, Io}; use /std/Io/{Channel};";

const EDGES: &[Edge] = &[
    Edge {
        operation: "Nat/div",
        inside: "let edge: Nat = Nat/div(1, 1);",
        outside: "let edge: Nat = Nat/div(1, 0);",
    },
    Edge {
        operation: "Nat/rem",
        inside: "let edge: Nat = Nat/rem(1, 1);",
        outside: "let edge: Nat = Nat/rem(1, 0);",
    },
    Edge {
        operation: "Int/div",
        inside: "let edge: Int = Int/div(+1, -1);",
        outside: "let edge: Int = Int/div(+1, +0);",
    },
    Edge {
        operation: "Int/rem",
        inside: "let edge: Int = Int/rem(+1, -1);",
        outside: "let edge: Int = Int/rem(+1, +0);",
    },
    Edge {
        operation: "Nat/to_byte",
        inside: "let edge: Byte = Nat/to_byte(255);",
        outside: "let edge: Byte = Nat/to_byte(256);",
    },
    Edge {
        operation: "Int/to_nat",
        inside: "let edge: Nat = Int/to_nat(+0);",
        outside: "let edge: Nat = Int/to_nat(-1);",
    },
    // Negative zero is zero, so the first value below the domain is the first below it in order; the infinity is its other edge.
    Edge {
        operation: "Flt/to_nat",
        inside: "let edge: Nat = Flt/to_nat(-0.0);",
        outside: "let edge: Nat = Flt/to_nat(-1.0);",
    },
    Edge {
        operation: "Flt/to_nat",
        inside: "let edge: Nat = Flt/to_nat(1.0);",
        outside: "let edge: Nat = Flt/to_nat(+inf.0);",
    },
    Edge {
        operation: "Flt/to_int",
        inside: "let edge: Int = Flt/to_int(-1.0);",
        outside: "let edge: Int = Flt/to_int(-inf.0);",
    },
    Edge {
        operation: "Flt/to_int",
        inside: "let edge: Int = Flt/to_int(1.0);",
        outside: "let edge: Int = Flt/to_int(+nan.0);",
    },
    Edge {
        operation: "Flt/mantissa",
        inside: "let edge: Int = Flt/mantissa(1.0);",
        outside: "let edge: Int = Flt/mantissa(+inf.0);",
    },
    Edge {
        operation: "Flt/exponent",
        inside: "let edge: Int = Flt/exponent(1.0);",
        outside: "let edge: Int = Flt/exponent(+inf.0);",
    },
    Edge {
        operation: "Flt/of_le_bytes",
        inside: "let edge: Flt = Flt/of_le_bytes(x[0, 0, 0, 0, 0, 0, 0, 0]);",
        outside: "let edge: Flt = Flt/of_le_bytes(x[0, 0, 0, 0, 0, 0, 0]);",
    },
    Edge {
        operation: "Flt/of_le_bytes",
        inside: "let edge: Flt = Flt/of_le_bytes(x[0, 0, 0, 0, 0, 0, 0, 0]);",
        outside: "let edge: Flt = Flt/of_le_bytes(x[0, 0, 0, 0, 0, 0, 0, 0, 0]);",
    },
    Edge {
        operation: "Bytes/get",
        inside: "let edge: Byte = Bytes/get(x[7], 0);",
        outside: "let edge: Byte = Bytes/get(x[7], 1);",
    },
    Edge {
        operation: "Bits/get",
        inside: "let edge: Bool = Bits/get(b[1], 0);",
        outside: "let edge: Bool = Bits/get(b[1], 1);",
    },
    Edge {
        operation: "List/get",
        inside: "let edge: Nat = List/get([7], 0);",
        outside: "let edge: Nat = List/get([7], 1);",
    },
    // A window may end at the end, and may be empty there: the bound is on where it ends.
    Edge {
        operation: "Bytes/slice",
        inside: "let edge: Bytes = Bytes/slice(x[7], 1, 0);",
        outside: "let edge: Bytes = Bytes/slice(x[7], 1, 1);",
    },
    Edge {
        operation: "Bits/slice",
        inside: "let edge: Bits = Bits/slice(b[1], 1, 0);",
        outside: "let edge: Bits = Bits/slice(b[1], 1, 1);",
    },
    Edge {
        operation: "List/slice",
        inside: "let edge: List(Nat) = List/slice([7], 1, 0);",
        outside: "let edge: List(Nat) = List/slice([7], 1, 1);",
    },
    Edge {
        operation: "Bytes/and",
        inside: "let edge: Bytes = Bytes/and(x[7], x[7]);",
        outside: "let edge: Bytes = Bytes/and(x[7], x[7, 7]);",
    },
    Edge {
        operation: "Bytes/or",
        inside: "let edge: Bytes = Bytes/or(x[7], x[7]);",
        outside: "let edge: Bytes = Bytes/or(x[7, 7], x[7]);",
    },
    Edge {
        operation: "Bytes/xor",
        inside: "let edge: Bytes = Bytes/xor(x[], x[]);",
        outside: "let edge: Bytes = Bytes/xor(x[], x[7]);",
    },
    Edge {
        operation: "Bits/and",
        inside: "let edge: Bits = Bits/and(b[1], b[1]);",
        outside: "let edge: Bits = Bits/and(b[1], b[1, 1]);",
    },
    Edge {
        operation: "Bits/or",
        inside: "let edge: Bits = Bits/or(b[1], b[1]);",
        outside: "let edge: Bits = Bits/or(b[1, 1], b[1]);",
    },
    Edge {
        operation: "Bits/xor",
        inside: "let edge: Bits = Bits/xor(b[], b[]);",
        outside: "let edge: Bits = Bits/xor(b[], b[1]);",
    },
    // A whole number of bytes on one side, one bit short and one bit over on the other.
    Edge {
        operation: "Bits/to_bytes",
        inside: "let edge: Bytes = Bits/to_bytes(b[1, 0, 0, 0, 0, 0, 0, 0]);",
        outside: "let edge: Bytes = Bits/to_bytes(b[1, 0, 0, 0, 0, 0, 0]);",
    },
    Edge {
        operation: "Bits/to_bytes",
        inside: "let edge: Bytes = Bits/to_bytes(b[]);",
        outside: "let edge: Bytes = Bits/to_bytes(b[1, 0, 0, 0, 0, 0, 0, 0, 1]);",
    },
];

fn program(declaration: &str) -> String {
    format!("{HEADER}\n{declaration}\n/std/print(\"\")")
}

// One program holding every inside call, since a program that compiles pays for the whole pipeline and one that is refused stops where it is written. A refusal names the declaration it is in, which is the row's place in `EDGES`.
#[test]
fn every_bounded_operation_compiles_at_the_edge_of_its_domain() {
    let declarations = EDGES
        .iter()
        .enumerate()
        .map(|(row, edge)| {
            edge.inside
                .replacen("let edge:", &format!("let edge_{row}:"), 1)
        })
        .collect::<Vec<_>>()
        .join("\n");

    if let Err(error) = typecheck(&program(&declarations)) {
        panic!("a call inside its operation's domain was refused:\n{error}");
    }
}

// Refused for its bound and nothing else: a control refused for a misspelling would pass as refused, so the refusal must name the operation whose proof nothing supplied.
#[test]
fn every_bounded_operation_is_refused_one_past_the_edge_of_its_domain() {
    for edge in EDGES {
        let error = match typecheck(&program(edge.outside)) {
            Ok(()) => panic!(
                "`{}` is outside `{}`'s domain and compiled",
                edge.outside, edge.operation
            ),
            Err(error) => error,
        };

        assert!(
            error.contains(&format!("of '{}' was not inferred", edge.operation)),
            "`{}` was refused, but not for `{}`'s bound:\n{error}",
            edge.outside,
            edge.operation
        );
    }
}

// A channel's capacity is bounded by `/sys/Channel/new`'s row too, which a program reaches only through the wrapper `/std` puts in front of it; the wrapper restates the bound to hand its proof on, so the refusal names the wrapper.
#[test]
fn a_channel_of_no_capacity_is_refused_and_one_of_capacity_one_compiles() {
    let program = |capacity: u32| {
        format!(
            "{HEADER}\nlet edge: Io({{Channel/Sender(Nat), Channel/Receiver(Nat)}}) = Channel/new(@Nat, {capacity});\n/std/print(\"\")"
        )
    };

    typecheck(&program(1)).expect("a channel may hold one value");

    let error = typecheck(&program(0)).expect_err("a channel holds at least one value");
    assert!(
        error.contains("of 'Channel/new' was not inferred"),
        "{error}"
    );
}
