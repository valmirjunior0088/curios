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
            // A stuck symmetric comparison is spelled with its operands in one order.
            "Eq(x == y, y == x)",
            "Eq(x != y, y != x)",
            // The family is aligned probe-side: a negation reads as its dual, and `<=` as `<` of the successor.
            "Eq(Bool/not(x < y), y <= x)",
            "Eq(Bool/not(x <= y), y < x)",
            "Eq(Bool/not(x == y), x != y)",
            "Eq(Bool/not(x != y), x == y)",
            "Eq(Bool/not(Bool/not(x < y)), x < y)",
            "Eq(x <= y, x < y + 1)",
            "Eq(x >= y, y < x + 1)",
            "Eq(x <= 3, x < 4)",
            "Eq(Bool/not(x < y) && Bool/not(y < x), y <= x && x <= y)",
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
        binders: "i: Int, j: Int, k: Int",
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
            "Eq(i == j, j == i)",
            "Eq(i != j, j != i)",
            // The signed sum normal form: a group, so a subtraction is a negative coefficient and every difference cancels in full.
            "Eq(i + j, j + i)",
            "Eq((i + j) + k, i + (j + k))",
            "Eq((i + j) - j, i)",
            "Eq((i + 1) - 1, i)",
            "Eq(i - (i + 1), -1)",
            "Eq((i - j) + j, i)",
            "Eq(i + i, 2 * i)",
            "Eq(0 - i, -1 * i)",
            // Monomials: one factor order, and distribution past a single monomial in the fold, past two symbolic sums on demand.
            "Eq(i * j, j * i)",
            "Eq(i * (j + k), i * j + i * k)",
            "Eq((i + 1) * (j + 1), i * j + i + j + 1)",
            // Comparisons read through the difference.
            "Eq(i < i + 1, true)",
            "Eq(i + 1 <= i, false)",
            "Eq(i == i + 1, false)",
            "Eq(i + j < i + k, j < k)",
            // The family aligned, as on `Nat`.
            "Eq(Bool/not(i < j), j <= i)",
            "Eq(Bool/not(i <= j), j < i)",
            "Eq(Bool/not(i == j), i != j)",
            "Eq(i <= j, i < j + 1)",
        ],
        refused: &[],
    },
    Carrier {
        name: "Bool",
        binders: "b: Bool, c: Bool, d: Bool",
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
            // `&&` and `||` are semilattices on their leaves, decided as sets by the peel: commuted, reassociated and repeated leaves are one value.
            "Eq(b && c, c && b)",
            "Eq((b && c) && d, b && (c && d))",
            "Eq(b || c, c || b)",
            "Eq((b || c) || d, b || (c || d))",
            "Eq((b && c) && b, c && b)",
            // A stuck symmetric comparison is spelled with its operands in one order.
            "Eq(b == c, c == b)",
            "Eq(b != c, c != b)",
            // A negated equality reads as the inequality.
            "Eq(Bool/not(b == c), b != c)",
            "Eq(Bool/not(b != c), b == c)",
        ],
        // De Morgan and absorption need a normal form past the leaf set, and neither is taken.
        refused: &[
            "Eq(Bool/not(b && c), Bool/not(b) || Bool/not(c))",
            "Eq(b || (b && c), b)",
        ],
    },
    Carrier {
        name: "List, the free monoid",
        binders: "xs: List(Nat), ys: List(Nat), zs: List(Nat), a: Nat, f: (Nat) -> Nat, s: Nat, l: Nat, ok: Nat/Le(s + l, List/len(xs)), first: Nat/Lt(0, l), at: Nat/Lt(s, List/len(xs)), head: Nat/Lt(0, List/len(ys)), into: Nat/Lt(s, List/len(ys)), fits: Nat/Le(l, List/len(ys)), z: Nat, g: (Nat, Nat) -> Nat",
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
            // The syntactic identity lambda is beta, not extensionality: `map` sends every element to itself and the list is returned whole.
            "Eq(List/map(xs, (v) => v), xs)",
            // An index into a map is the function at the index into its argument; the bound is the argument's, since `len(map(xs, f))` is `len(xs)`.
            "Eq(List/get(@Nat, List/map(xs, f), s, @at), f(List/get(@Nat, xs, s, @at)))",
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
            // A window's length is the count it was cut to, and a map moves inside a window with the same bound, since `len(map(xs, f))` is `len(xs)`. Both rest on `slice`'s own precondition, which the binder `ok` states.
            "Eq(List/len(List/slice(@Nat, xs, s, l, @ok)), l)",
            "Eq(List/map(List/slice(@Nat, xs, s, l, @ok), f), List/slice(@Nat, List/map(xs, f), s, l, @ok))",
            // A window past every operand but the last is that window into the last operand. The bound cancels the consumed prefix off both sides, so the same proof places the narrowed operation.
            "Eq(List/get(@Nat, [..xs, ..ys], List/len(xs), @head), List/get(@Nat, ys, 0, @head))",
            "Eq(List/get(@Nat, [..xs, ..ys], List/len(xs) + s, @into), List/get(@Nat, ys, s, @into))",
            "Eq(List/slice(@Nat, [..xs, ..ys], List/len(xs), l, @fits), List/slice(@Nat, ys, 0, l, @fits))",
            // A left fold reduces over the shape: the empty run is the seed, a cons steps once and folds the tail from there, and a concatenation folds its operands in order.
            "Eq(List/fold(@Nat, @Nat, [], z, g), z)",
            "Eq(List/fold([a], z, g), g(a, z))",
            "Eq(List/fold([a, ..xs], z, g), List/fold(xs, g(a, z), g))",
            "Eq(List/fold([..xs, ..ys], z, g), List/fold(ys, List/fold(xs, z, g), g))",
            "Eq(List/fold([..xs, a], z, g), g(a, List/fold(xs, z, g)))",
        ],
        refused: &[
            // Function extensionality in disguise, and not one to take: a function that is the identity pointwise is not the identity lambda, and `map` by it stays stuck.
            "Eq(List/map(xs, (v) => v + 0), xs)",
            // A position inside a window needs a bound on the base that no term in hand proves, and a reducer may not invent one — so these stay stuck, and are stated here so that taking them is a row moving.
            "Eq(List/get(@Nat, List/slice(@Nat, xs, s, l, @ok), 0, @first), List/get(@Nat, xs, s, @at))",
        ],
    },
    Carrier {
        name: "Bytes, the free monoid",
        binders: "bs: Bytes, cs: Bytes, ds: Bytes, k: Byte, s: Nat, l: Nat, ok: Nat/Le(s + l, Bytes/len(bs)), head: Nat/Lt(0, Bytes/len(cs)), into: Nat/Lt(s, Bytes/len(cs)), fits: Nat/Le(l, Bytes/len(cs))",
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
            "Eq(bs == cs, cs == bs)",
            "Eq(Bytes/slice(x[..bs, ..cs], 0, Bytes/len(bs)), bs)",
            "Eq(Bytes/slice(x[..bs, ..cs], Bytes/len(bs), Bytes/len(cs)), cs)",
            "Eq(Bytes/slice(x[..bs, ..cs, ..ds], Bytes/len(bs), Bytes/len(cs)), cs)",
            // An index at a symbolic seam, which the same walk finds as a window of one. The operand beginning there carries a single generator, read out of it directly rather than through an inner `get` that would need a bound nothing states.
            "Eq(Bytes/get(x[..bs, k, ..cs], Bytes/len(bs)), k)",
            "Eq(Bytes/get(x[..bs, k], Bytes/len(bs)), k)",
            // An append is the concatenation the peel's own law says it is, so a window at its seam locates like any other.
            "Eq(Bytes/slice(x[..bs, k], 0, Bytes/len(bs)), bs)",
            // A window's length is the count it was cut to: `slice` takes `s + l <= len(b)`, so the count is the measure at every well-typed instance. Taken where the homomorphism reads the window rather than in `free_monoid`'s measure, which still counts only literal runs.
            "Eq(Bytes/len(Bytes/slice(bs, s, l, @ok)), l)",
            // A window past every operand but the last is that window into the last operand, under the caller's own bound.
            "Eq(Bytes/get(x[..bs, ..cs], Bytes/len(bs), @head), Bytes/get(cs, 0, @head))",
            "Eq(Bytes/get(x[..bs, ..cs], Bytes/len(bs) + s, @into), Bytes/get(cs, s, @into))",
            "Eq(Bytes/slice(x[..bs, ..cs], Bytes/len(bs), l, @fits), Bytes/slice(cs, 0, l, @fits))",
            // A positive segment anywhere in a value decides it against the empty one, and against a value it extends.
            "Eq(x[..bs, k] == x[], false)",
            "Eq(x[..bs, 1] == x[], false)",
            "Eq(x[..bs, k] == bs, false)",
            "Eq(x[..bs, ..cs, 1] == bs, false)",
        ],
        refused: &[],
    },
    Carrier {
        name: "Bits, the free monoid",
        binders: "ts: Bits, us: Bits, ws: Bits, v: Bool, s: Nat, l: Nat, ok: Nat/Le(s + l, Bits/len(ts)), head: Nat/Lt(0, Bits/len(us)), into: Nat/Lt(s, Bits/len(us)), fits: Nat/Le(l, Bits/len(us))",
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
            "Eq(ts == us, us == ts)",
            "Eq(Bits/slice(b[..ts, ..us], 0, Bits/len(ts)), ts)",
            "Eq(Bits/slice(b[..ts, ..us], Bits/len(ts), Bits/len(us)), us)",
            "Eq(Bits/slice(b[..ts, ..us, ..ws], Bits/len(ts), Bits/len(us)), us)",
            "Eq(Bits/get(b[..ts, v, ..us], Bits/len(ts)), v)",
            "Eq(Bits/get(b[..ts, v], Bits/len(ts)), v)",
            "Eq(Bits/slice(b[..ts, v], 0, Bits/len(ts)), ts)",
            "Eq(Bits/len(Bits/slice(ts, s, l, @ok)), l)",
            "Eq(Bits/get(b[..ts, ..us], Bits/len(ts), @head), Bits/get(us, 0, @head))",
            "Eq(Bits/get(b[..ts, ..us], Bits/len(ts) + s, @into), Bits/get(us, s, @into))",
            "Eq(Bits/slice(b[..ts, ..us], Bits/len(ts), l, @fits), Bits/slice(us, 0, l, @fits))",
            "Eq(b[..ts, v] == b[], false)",
            "Eq(b[..ts, 1] == b[], false)",
            "Eq(b[..ts, v] == ts, false)",
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
            // The bytes decide a suffix as they decide a prefix: a string with a character appended is not the empty one.
            "Eq(Str/concat(s, \"a\") == \"\", false)",
            "Eq(Str/concat(\"a\", s) == \"\", false)",
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
