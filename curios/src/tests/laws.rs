//! The definitional-law grid: which equations over the intrinsic carriers the normalizer closes by computation, and which it refuses, stated so that both halves are checked.
//!
//! A held row is a law both checkers decide for every value, so it is stated as an `Eq/refl()` proof and compiled. A refused row is a law the normalizer does *not* take — kept here rather than in prose so the refused set is a record rather than a rumor, and so that taking one later is a row moving, not a test appearing. The refused half cannot be stated as a proof, so both halves are also stated as written goals and read back through the `? ≈ Eq/refl()` candidate line, which is the compiler's own answer to "does refl fit here": a held row must get the line and a refused row must not. The two directions check each other — if the candidate search stopped reporting, every held row would fail the goal test, so a refused row cannot pass it vacuously.
//!
//! **State a row at every carrier, and state it first.** A grid stating a law at one carrier and not another passes exactly as a complete one does, and what it hides is invisible from reading the two implementations side by side: the bit grain went without `eql` while its byte twin had it, and `List` went without both seam-index rows while `Bytes` and `Bits` held them. Each was a real incompleteness, and each was found by stating the row rather than by inspection. A row stated before an implementation moves turns the change that follows into a refactor with an oracle.
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
            // A sum is a linear combination: like terms merge, and a literal distributes over a symbolic sum.
            "Eq(x + x, 2 * x)",
            "Eq(x * 2, x + x)",
            "Eq(x * 2 + x * 3, x * 5)",
            "Eq((x + y) * 2, x * 2 + y * 2)",
            "Eq((x + y + 1) + (x + 2), 2 * x + y + 3)",
            // A product of symbols is a monomial with one factor order, and distributes over a sum.
            "Eq(x * y, y * x)",
            "Eq((x * y) * z, x * (y * z))",
            "Eq(x * (y + z), x * y + x * z)",
            "Eq((x + 1) * (y + 2), x * y + 2 * x + y + 2)",
            "Eq(x * y + y * x, 2 * (x * y))",
        ],
        refused: &[],
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
        // Every subject here is bounded below 256 where a `Byte` is built from a `Nat`, never a bare binder: the narrowing states that domain, so an unbounded subject would stop the row elaborating rather than move it between the halves.
        name: "Byte against Nat",
        binders: "b: Byte, q: Nat, x: Nat",
        held: &[
            // The carrier's own bound, which is what the oracle answers for a `Byte` however the value was produced.
            "Eq(Byte/to_nat(b) < 256, true)",
            "Eq(Nat/to_byte(Byte/to_nat(b)), b)",
            // Euclid's seam, which is what `Key(Nat)`'s recombination rests on: a residual carried in a `Byte` divides out of a scaled symbol, and the remainder twin recovers it.
            "Eq((256 * q + Byte/to_nat(b)) / 256, q)",
            "Eq((256 * q + Byte/to_nat(b)) % 256, Byte/to_nat(b))",
            // The control for the pair below: the same split over a bound the oracle reads off the term directly.
            "Eq((16 * q + Nat/and(x, 15)) / 16, q)",
            // The transparency pair, which moved here when the narrowing took its domain. `Nat/to_byte` states `nat < 256`, so the constructor is invertible and `Byte/to_nat` reduces back through it — which is what lets the second row hold: it is the control above with the operand sent through `Byte` and back, the arithmetic identical and the round trip no longer erasing what the oracle could read.
            "Eq(Byte/to_nat(Nat/to_byte(Nat/and(x, 255))), Nat/and(x, 255))",
            "Eq((16 * q + Byte/to_nat(Nat/to_byte(Nat/and(x, 15)))) / 16, q)",
        ],
        refused: &[],
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
            // An append is the concatenation the peel's own law says it is, so a window at its seam locates like any other — held here as it is at `Bytes`.
            "Eq(List/slice([..xs, a], 0, List/len(xs)), xs)",
            // An index at a symbolic seam, where the operand beginning there carries exactly one element. The same walk that locates the window above, asked for a window of one.
            "Eq(List/get([..xs, a, ..ys], List/len(xs)), a)",
            "Eq(List/get([..xs, a], List/len(xs)), a)",
        ],
        refused: &[
            // Function extensionality in disguise: not one to take.
            "Eq(List/map(xs, (v) => v), xs)",
        ],
    },
    Carrier {
        name: "Bytes, the free monoid",
        binders: "bs: Bytes, cs: Bytes, ds: Bytes, k: Byte, s: Nat, l: Nat, ok: Nat/Le(s + l, Bytes/len(bs))",
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
            // An index at a symbolic seam, which the same walk finds as a window of one. The operand beginning there carries a single generator, read out of it directly rather than through an inner `get` that would need a bound nothing states.
            "Eq(Bytes/get(x[..bs, k, ..cs], Bytes/len(bs)), k)",
            "Eq(Bytes/get(x[..bs, k], Bytes/len(bs)), k)",
            // An append is the concatenation the peel's own law says it is, so a window at its seam locates like any other.
            "Eq(Bytes/slice(x[..bs, k], 0, Bytes/len(bs)), bs)",
        ],
        refused: &[
            // A window's length is the count it was cut to. True at every well-typed instance — `slice` takes `s + l <= len(b)` and the node carries the proof — so this is a candidate rather than a bug, and stating it here is what would make taking it a row moving. Not taken: `free_monoid`'s measure reads literal runs and their concatenations and declines everything else, and admitting a window would be a new definitional equation bought for no caller, since an accumulation's spine is literals. It does not open the way to conversion's own decomposition either, which measures a window already but materializes its generators to compare them, where this one counts them without reading any.
            "Eq(Bytes/len(Bytes/slice(bs, s, l, @ok)), l)",
        ],
    },
    Carrier {
        name: "Bits, the free monoid",
        binders: "ts: Bits, us: Bits, ws: Bits, v: Bool",
        held: &[
            // The byte grain's laws, stated again here: one grain's fold arm is not evidence for the other's, and a law held at one grain and unstated at the other is a copy with nothing checking it. The explanations are the byte group's, above.
            "Eq(b[..ts, ..b[]], ts)",
            "Eq(b[..b[], ..ts], ts)",
            "Eq(b[..b[..ts, ..us], ..ws], b[..ts, ..us, ..ws])",
            "Eq(Bits/len(b[]), 0)",
            "Eq(Bits/len(b[v, ..ts]), Bits/len(ts) + 1)",
            "Eq(Bits/len(b[..ts, v]), Bits/len(ts) + 1)",
            "Eq(Bits/len(b[..ts, ..us]), Bits/len(ts) + Bits/len(us))",
            "Eq(Bits/len(b[1, 0, ..ts]), Bits/len(ts) + 2)",
            "Eq(Bits/slice(ts, 0, Bits/len(ts)), ts)",
            "Eq(Bits/slice(ts, 0, 0), b[])",
            "Eq(Bits/slice(b[v, ..ts], 1, Bits/len(ts)), ts)",
            "Eq(Bits/get(b[v, ..ts], 0), v)",
            "Eq(Bits/eql(ts, ts), true)",
            "Eq(ts == ts, true)",
            "Eq(Bits/slice(b[..ts, ..us], 0, Bits/len(ts)), ts)",
            "Eq(Bits/slice(b[..ts, ..us], Bits/len(ts), Bits/len(us)), us)",
            "Eq(Bits/slice(b[..ts, ..us, ..ws], Bits/len(ts), Bits/len(us)), us)",
            "Eq(Bits/get(b[..ts, v, ..us], Bits/len(ts)), v)",
            "Eq(Bits/get(b[..ts, v], Bits/len(ts)), v)",
            "Eq(Bits/slice(b[..ts, v], 0, Bits/len(ts)), ts)",
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
