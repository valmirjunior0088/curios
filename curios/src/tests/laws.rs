//! The definitional-law grid: which equations over the intrinsic carriers the normalizer closes by computation, and which it refuses, stated so that both halves are checked.
//!
//! A held row is a law both checkers decide for every value, so it is stated as an `Eq/refl()` proof and compiled. A refused row is a law the normalizer does *not* take — kept here rather than in prose so the refused set is a record rather than a rumor, and so that taking one later is a row moving, not a test appearing. The refused half cannot be stated as a proof, so both halves are also stated as written goals and read back through the `? ≈ Eq/refl()` candidate line, which is the compiler's own answer to "does refl fit here": a held row must get the line and a refused row must not. The two directions check each other — if the candidate search stopped reporting, every held row would fail the goal test, so a refused row cannot pass it vacuously.
//!
//! Every refused row is a candidate law, not a bug: each needs a rule in `curios-core`'s `reduce::intrinsic`, which both checkers share, so taking one is an addition to the trusted base and is recorded in `documentation/soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md` beside the grid that probes it over values.

use super::typecheck;

/// One carrier's rows: the binders every claim is stated under, the laws `Eq/refl()` closes, and the laws it refuses.
struct Carrier {
    name: &'static str,
    binders: &'static str,
    held: &'static [&'static str],
    refused: &'static [&'static str],
}

const IMPORTS: &str = "use /std/{Nat, Int, Bool, Byte, Bytes, Bits, List, Str, Eq, Io};";

const CARRIERS: &[Carrier] = &[
    Carrier {
        name: "Nat under +",
        binders: "x: Nat, y: Nat, z: Nat",
        held: &[
            "Eq(x + 0, x)",
            "Eq(0 + x, x)",
            "Eq(x + y, y + x)",
            "Eq((x + y) + z, x + (y + z))",
        ],
        refused: &[],
    },
    Carrier {
        name: "Nat under *",
        binders: "x: Nat, y: Nat, z: Nat",
        held: &[
            "Eq((x + 1) * 2, x * 2 + 2)",
            "Eq(3 * (x + 2), 3 * x + 6)",
            "Eq(x * 1, x)",
            "Eq(1 * x, x)",
            "Eq(x * 0, 0)",
            "Eq(0 * x, 0)",
            "Eq(2 * (3 * x), 6 * x)",
            "Eq((x * 2) * 3, x * 6)",
            // A literal coefficient has one side, so the two spellings meet.
            "Eq(x * 2, 2 * x)",
        ],
        refused: &[
            "Eq((x + y) * 2, x * 2 + y * 2)",
            "Eq(x * (y + z), x * y + x * z)",
            "Eq(x * 2 + x * 3, x * 5)",
            "Eq(x * 2, x + x)",
            "Eq(x + x, 2 * x)",
            "Eq(x * y, y * x)",
            "Eq((x * y) * z, x * (y * z))",
        ],
    },
    Carrier {
        name: "Nat under -",
        binders: "x: Nat, y: Nat, z: Nat",
        held: &[
            "Eq(x - 0, x)",
            "Eq(x - x, 0)",
            "Eq((x + y) - y, x)",
            "Eq((x + y) - x, y)",
            "Eq((x + 5) - 3, x + 2)",
            "Eq(0 - x, 0)",
            "Eq((x - y) - z, x - (y + z))",
        ],
        refused: &[],
    },
    Carrier {
        name: "Nat under / and %",
        binders: "x: Nat",
        held: &[
            "Eq((x * 2) / 2, x)",
            "Eq((x * 2) % 2, 0)",
            "Eq((x * 2 + 1) % 2, 1)",
            "Eq(x / 1, x)",
            "Eq(x % 1, 0)",
            "Eq(0 / (x + 1), 0)",
            "Eq(0 % (x + 1), 0)",
            "Eq((x + 1) / (x + 1), 1)",
            "Eq((x + 1) % (x + 1), 0)",
        ],
        refused: &[],
    },
    Carrier {
        name: "Nat comparisons",
        binders: "x: Nat, y: Nat",
        held: &[
            "Eq(x == x, true)",
            "Eq(x != x, false)",
            "Eq(x <= x, true)",
            "Eq(x < x, false)",
            "Eq(x < x + 1, true)",
            "Eq(0 < x + 1, true)",
            "Eq(x + 1 > 0, true)",
            "Eq(x <= x + y, true)",
            "Eq(x + y < x, false)",
        ],
        // Parity: not a law of any monoid here, and not one to take.
        refused: &["Eq(x * 2 + 1 == y * 2, false)"],
    },
    Carrier {
        name: "Nat bitwise and shifts",
        binders: "x: Nat",
        held: &[
            "Eq(Nat/and(x, 0), 0)",
            "Eq(Nat/and(0, x), 0)",
            "Eq(Nat/and(x, x), x)",
            "Eq(Nat/or(x, 0), x)",
            "Eq(Nat/or(0, x), x)",
            "Eq(Nat/or(x, x), x)",
            "Eq(Nat/xor(x, 0), x)",
            "Eq(Nat/xor(0, x), x)",
            "Eq(Nat/xor(x, x), 0)",
            "Eq(Nat/shl(x, 0), x)",
            "Eq(Nat/shr(x, 0), x)",
            "Eq(Nat/shl(0, x), 0)",
            "Eq(Nat/shr(0, x), 0)",
        ],
        // True on the unbounded ℕ the type level folds, false on the truncating carrier the runtime imposes: not one to take.
        refused: &["Eq(Nat/shl(x, 1), x * 2)"],
    },
    Carrier {
        name: "Int",
        binders: "i: Int, j: Int",
        held: &[
            "Eq(i + 0, i)",
            "Eq(0 + i, i)",
            "Eq(i - 0, i)",
            "Eq(i - i, 0)",
            "Eq(i * 1, i)",
            "Eq(1 * i, i)",
            "Eq(i * 0, 0)",
            "Eq(0 * i, 0)",
            "Eq(i == i, true)",
            "Eq(i != i, false)",
        ],
        // Commutativity needs the summand normal form `Nat` has and `Int` does not.
        refused: &["Eq(i + j, j + i)"],
    },
    Carrier {
        name: "Bool",
        binders: "b: Bool, c: Bool",
        held: &[
            "Eq(b && true, b)",
            "Eq(true && b, b)",
            "Eq(b && false, false)",
            "Eq(false && b, false)",
            "Eq(b || false, b)",
            "Eq(false || b, b)",
            "Eq(b || true, true)",
            "Eq(true || b, true)",
            "Eq(b && b, b)",
            "Eq(b || b, b)",
            "Eq(b == b, true)",
            "Eq(b != b, false)",
            "Eq(b == true, b)",
            "Eq(true == b, b)",
            "Eq(b != false, b)",
            "Eq(b == false, Bool/not(b))",
            "Eq(b != true, Bool/not(b))",
            "Eq(Bool/xor(b, false), b)",
            "Eq(Bool/xor(false, b), b)",
            "Eq(Bool/xor(b, b), false)",
            "Eq(Bool/not(Bool/not(b)), b)",
        ],
        // Commutativity: the same normal-form question as `Int`'s.
        refused: &["Eq(b && c, c && b)"],
    },
    Carrier {
        name: "List, the free monoid",
        binders: "xs: List(Nat), ys: List(Nat), zs: List(Nat), a: Nat, f: (Nat) -> Nat",
        held: &[
            "Eq([..xs, ..[]], xs)",
            "Eq([..[], ..xs], xs)",
            "Eq([..[..xs, ..ys], ..zs], [..xs, ..ys, ..zs])",
            "Eq([..xs, ..[..ys, ..zs]], [..xs, ..ys, ..zs])",
            "Eq(List/len(@Nat, []), 0)",
            "Eq(List/len([a, ..xs]), List/len(xs) + 1)",
            "Eq(List/len([..xs, a]), List/len(xs) + 1)",
            "Eq(List/len([..xs, ..ys]), List/len(xs) + List/len(ys))",
            "Eq(List/len([1, 2, ..xs]), List/len(xs) + 2)",
            "Eq(List/map(@Nat, @Nat, [], f), [])",
            "Eq(List/map([a, ..xs], f), [f(a), ..List/map(xs, f)])",
            "Eq(List/map([..xs, ..ys], f), [..List/map(xs, f), ..List/map(ys, f)])",
            "Eq(List/slice(xs, 0, List/len(xs)), xs)",
            "Eq(List/slice(xs, 0, 0), [])",
            "Eq(List/slice([a, ..xs], 1, List/len(xs)), xs)",
            "Eq(List/get([a, ..xs], 0), a)",
            "Eq(List/len(List/map(xs, f)), List/len(xs))",
            "Eq(List/slice([..xs, ..ys], 0, List/len(xs)), xs)",
            "Eq(List/slice([..xs, ..ys], List/len(xs), List/len(ys)), ys)",
            "Eq(List/slice([..xs, ..ys, ..zs], List/len(xs), List/len(ys)), ys)",
            "Eq(List/slice([..xs, ..ys, ..zs], 0, List/len(xs) + List/len(ys)), [..xs, ..ys])",
        ],
        refused: &[
            // Function extensionality in disguise: not one to take.
            "Eq(List/map(xs, (v) => v), xs)",
        ],
    },
    Carrier {
        name: "Bytes, the free monoid",
        binders: "bs: Bytes, cs: Bytes, ds: Bytes, k: Byte",
        held: &[
            "Eq(x[..bs, ..x[]], bs)",
            "Eq(x[..x[], ..bs], bs)",
            "Eq(x[..x[..bs, ..cs], ..ds], x[..bs, ..cs, ..ds])",
            "Eq(Bytes/len(x[]), 0)",
            "Eq(Bytes/len(x[k, ..bs]), Bytes/len(bs) + 1)",
            "Eq(Bytes/len(x[..bs, k]), Bytes/len(bs) + 1)",
            "Eq(Bytes/len(x[..bs, ..cs]), Bytes/len(bs) + Bytes/len(cs))",
            "Eq(Bytes/len(x[1, 2, ..bs]), Bytes/len(bs) + 2)",
            "Eq(Bytes/slice(bs, 0, Bytes/len(bs)), bs)",
            "Eq(Bytes/slice(bs, 0, 0), x[])",
            "Eq(Bytes/slice(x[k, ..bs], 1, Bytes/len(bs)), bs)",
            "Eq(Bytes/get(x[k, ..bs], 0), k)",
            "Eq(Bytes/eql(bs, bs), true)",
            "Eq(bs == bs, true)",
            "Eq(Bytes/slice(x[..bs, ..cs], 0, Bytes/len(bs)), bs)",
            "Eq(Bytes/slice(x[..bs, ..cs], Bytes/len(bs), Bytes/len(cs)), cs)",
            "Eq(Bytes/slice(x[..bs, ..cs, ..ds], Bytes/len(bs), Bytes/len(cs)), cs)",
        ],
        refused: &[],
    },
    Carrier {
        name: "Bits, the free monoid",
        binders: "ts: Bits, us: Bits, v: Bool",
        held: &[
            "Eq(b[..ts, ..b[]], ts)",
            "Eq(b[..b[], ..ts], ts)",
            "Eq(Bits/len(b[v, ..ts]), Bits/len(ts) + 1)",
            "Eq(Bits/len(b[..ts, ..us]), Bits/len(ts) + Bits/len(us))",
            "Eq(Bits/slice(ts, 0, Bits/len(ts)), ts)",
            "Eq(Bits/get(b[v, ..ts], 0), v)",
            "Eq(Bits/slice(b[..ts, ..us], Bits/len(ts), Bits/len(us)), us)",
        ],
        refused: &[],
    },
    Carrier {
        name: "Str, over Bytes",
        binders: "s: Str",
        held: &[
            "Eq(Str/concat(s, \"\"), s)",
            "Eq(Str/concat(\"\", s), s)",
            "Eq(Str/len(\"\"), 0)",
            "Eq(s == s, true)",
        ],
        refused: &[],
    },
];

/// One program stating every `claims` row of `carrier` as an item with `body`, in order, over a unit tail.
fn program(carrier: &Carrier, claims: &[&str], body: &str) -> String {
    let items = claims
        .iter()
        .enumerate()
        .map(|(index, claim)| format!("let law{index}({}) -> {claim} = {body};", carrier.binders))
        .collect::<Vec<_>>()
        .join("\n");
    format!("{IMPORTS}\n{items}\nIo/pure(())")
}

#[test]
fn every_held_law_closes_by_refl() {
    // The admitting direction, stated where both checkers see it: one program per carrier, every held row an `Eq/refl()` proof. A failure names the carrier; the goal test below names the row.
    for carrier in CARRIERS {
        if carrier.held.is_empty() {
            continue;
        }
        let source = program(carrier, carrier.held, "Eq/refl()");
        if let Err(error) = typecheck(&source) {
            panic!("a held law of {} no longer closes: {error}", carrier.name);
        }
    }
}

#[test]
fn every_row_is_on_the_side_the_compiler_puts_it() {
    // Every row — held then refused — as a written goal, read back through the compiler's own refl-fit line. A held row without the line is a regression in the normalizer; a refused row with it is a law that has been taken and must move to the held rows, which is how the refused half stays a record. Every misplaced row is reported at once, since a change to one rule can move several.
    let mut misplaced = Vec::new();
    for carrier in CARRIERS {
        let rows = carrier
            .held
            .iter()
            .chain(carrier.refused)
            .copied()
            .collect::<Vec<_>>();
        let error = typecheck(&program(carrier, &rows, "?"))
            .expect_err("a program of written goals never compiles");
        let reports = error.split("goal `?`").skip(1).collect::<Vec<_>>();
        assert_eq!(
            reports.len(),
            rows.len(),
            "{}: one report per row, got:\n{error}",
            carrier.name
        );
        for (index, (row, report)) in rows.iter().zip(reports).enumerate() {
            let fits = report.contains("? \u{2248} Eq/refl()");
            let held = index < carrier.held.len();
            if fits != held {
                misplaced.push(format!(
                    "{}: `{row}` is {} but the compiler {} close it by refl",
                    carrier.name,
                    if held { "held" } else { "refused" },
                    if fits { "does" } else { "does not" }
                ));
            }
        }
    }
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n"));
}
