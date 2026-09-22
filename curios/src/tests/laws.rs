//! The definitional-law grid: which equations over the intrinsic carriers the normalizer closes by computation, and which it refuses, stated so that both halves are checked.
//!
//! A held row is a law both checkers decide for every value, so it is stated as an `Eq/refl()` proof and compiled. A refused row is a law the normalizer does *not* take — kept here rather than in prose so the refused set is a record rather than a rumor, and so that taking one later is a row moving, not a test appearing. The refused half cannot be stated as a proof, so both halves are also stated as written goals and read back through the `? ≈ Eq/refl()` candidate line, which is the compiler's own answer to "does refl fit here": a held row must get the line and a refused row must not. The two directions check each other — if the candidate search stopped reporting, every held row would fail the goal test. That alone does not keep a refused row from passing vacuously, because the search can stop *partway*: every goal of one program draws its candidate attempts from a single budget, so a program with enough goals runs it dry and every goal after that point reports no candidate at all — which is exactly what a refused row is expected to show, and the refused rows come last. So each program ends on a sentinel, a row `refl` trivially closes, stated after the refused ones: a sentinel without its line means the search ran dry before the refused rows were reached, and the test says so rather than passing them.
//!
//! **State a row at every carrier, and state it first.** A grid stating a law at one carrier and not another passes exactly as a complete one does, and what it hides is invisible from reading the two implementations side by side: the bit grain went without `eql` while its byte twin had it, and `List` went without both seam-index rows while `Bytes` and `Bits` held them. Each was a real incompleteness, and each was found by stating the row rather than by inspection. A row stated before an implementation moves turns the change that follows into a refactor with an oracle.
//!
//! Every refused row is a candidate law or a control, never a bug. A candidate needs a rule in `curios-core`'s `reduce::intrinsic`, which both checkers share, so taking one is an addition to the trusted base and is recorded in `documentation/soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md` beside the grid that probes it over values. A control is a claim that is *not* a law, stated beside the held rows whose rule must stop short of it and marked as one where it stands: a rule widened past its soundness moves a control to the held side, and the goal test names it.

use super::typecheck;

/// One carrier's rows: the binders every claim is stated under, the laws `Eq/refl()` closes, and the laws it refuses.
struct Carrier {
    name: &'static str,
    binders: &'static str,
    held: &'static [&'static str],
    refused: &'static [&'static str],
}

/// The last goal of every program the goal test states: trivially closed by `refl`, so its candidate line is evidence that the search still had budget when the refused rows before it were answered.
const SENTINEL: &str = "Eq(0, 0)";

const IMPORTS: &str = "use /std/{Nat, Int, Bool, Byte, Bytes, Bits, List, Str, Char, Flt, Eq, Io};";

const CARRIERS: &[Carrier] = &[
    Carrier {
        name: "Nat under +",
        binders: "x: Nat, y: Nat, z: Nat, f: (Nat) -> Nat, g: (Nat) -> Nat",
        held: &[
            "Eq(x + 0, x)",
            "Eq(0 + x, x)",
            "Eq(x + y, y + x)",
            "Eq((x + y) + z, x + (y + z))",
            // Congruence under an opaque head: the arguments are compared as numbers.
            "Eq(f(x + y), f(y + x))",
            // A summand meets its own spelling. Summands pair by identity, and two occurrences of one operator are two terms while the signature holding them is checked — each carries its own witness metavariable, solved and not yet spliced — so this was refused by the elaborator alone, as `x` against `y` under the positional congruence, until the solved ones were substituted before the peel.
            "Eq(f(x + 1) + f(y + 1), f(y + 1) + f(x + 1))",
            // The composition of the two rows above. Cancellation pairs summands by identity and the fold leaves a stuck application's arguments as written, so `f(x + y)` never met `f(y + x)` inside a sum though conversion decides that pair on its own. `Nat::cancel_common` still takes no reducer — what forces the summands' arguments is `Nat::normalize_atoms`, at the conversion site that already normalizes a stuck product and already forces a Boolean tree's leaves, and only once the peel has found nothing to cancel.
            "Eq(f(x + y) + g(y + z), g(z + y) + f(y + x))",
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
        binders: "x: Nat, y: Nat, d: Nat, p: Nat/Lt(0, d)",
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
            // A remainder is below its divisor and a quotient no larger than its dividend, whatever the divisor is, on the division's own proof.
            "Eq(Nat/rem(x, d, @p) < d, true)",
            "Eq(Nat/div(x, d, @p) <= x, true)",
            // Euclid's identity: a remainder beside the divisor times the matching quotient is the dividend, at a literal divisor, at a symbolic one, at multiplicity two beside an unrelated summand, and through the floor law.
            "Eq((x / 10) * 10 + x % 10, x)",
            "Eq(x % 10 + 10 * (x / 10), x)",
            "Eq((x / (y + 1)) * (y + 1) + x % (y + 1), x)",
            "Eq(Nat/div(x, d, @p) * d + Nat/rem(x, d, @p), x)",
            "Eq(2 * ((x / 3) * 3) + y + 2 * (x % 3), 2 * x + y)",
            "Eq(((x + 5) / 3) * 3 + (x + 5) % 3, x + 5)",
        ],
        refused: &[
            // Controls, and none is a law: the multiple without its remainder, the wrong multiple of the quotient, and a remainder of another dividend.
            "Eq((x / 3) * 3, x)",
            "Eq((x / 3) * 2 + x % 3, x)",
            "Eq((x / 3) * 3 + y % 3, x)",
        ],
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
            // A comparison beside its dual computes complementary booleans at every pair.
            "Eq(x < y && y <= x, false)",
            "Eq(x < y || y <= x, true)",
            "Eq(x == y && x != y, false)",
            "Eq(x == y || x != y, true)",
            // A bound met exactly is the non-strict verdict, on either side.
            "Eq(x % 7 <= 6, true)",
            "Eq(x % 7 > 6, false)",
            "Eq(6 >= x % 7, true)",
            // A value against an operand it never exceeds.
            "Eq(x - y <= x, true)",
            "Eq(x / (y + 1) <= x, true)",
            "Eq(x % (y + 1) <= x, true)",
            "Eq(x % (y + 1) < y + 1, true)",
            "Eq(Nat/and(x, y) <= x, true)",
            "Eq(Nat/and(x, y) <= y, true)",
            "Eq(Nat/shr(x, y) <= x, true)",
            "Eq(x < x - y, false)",
            "Eq(x - y <= x + 3, true)",
            // Divisibility: every symbolic summand is a multiple of the gcd of the coefficients, so two floors apart modulo it meet at no value. Equality and its negation decide; the order stays open.
            "Eq(x * 2 + 1 == y * 2, false)",
            "Eq(x * 2 + 1 != y * 2, true)",
            "Eq(x * 4 + 6 == y * 2 + 1, false)",
        ],
        refused: &[
            // Controls, and neither is a law: floors that agree modulo the gcd meet at `x = y + 1`, and a gcd of `1` divides every floor, so these meet at `x = y = 1`.
            "Eq(x * 2 == y * 2 + 2, false)",
            "Eq(x * 2 + 1 == y * 3, false)",
        ],
    },
    Carrier {
        name: "Nat bitwise and shifts",
        binders: "x: Nat, y: Nat",
        held: &[
            "Eq(Nat/and(x, y), Nat/and(y, x))",
            "Eq(Nat/or(x, y), Nat/or(y, x))",
            "Eq(Nat/xor(x, y), Nat/xor(y, x))",
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
            // A left shift by a literal count is the coefficient `2ᵏ`, on the unbounded ℕ the type level folds and at run time alike, since the emitter refuses a shift that leaves the carrier rather than truncating it. The coefficient is charged before it is built, and the product then distributes as any other does.
            "Eq(Nat/shl(x, 1), x * 2)",
            "Eq(Nat/shl(x, 3), 8 * x)",
            "Eq(Nat/shl(x + 1, 2), 4 * x + 4)",
            "Eq(Nat/shl(x, 1) + Nat/shl(y, 1), 2 * (x + y))",
        ],
        refused: &[
            // A symbolic count is no coefficient: `2ˣ` is not a literal, and the normal form has no exponential to hold it.
            "Eq(Nat/shl(2, x), Nat/shl(1, x + 1))",
            // A right shift by a literal count is a quotient by `2ᵏ` and could join the division family through the Euclidean split. It may not do so by building a division node, which carries a proof that the divisor is nonzero, and a reducer may not invent one.
            "Eq(Nat/shr(x * 4, 2), x)",
        ],
    },
    Carrier {
        // Every subject here is bounded below 256 where a `Byte` is built from a `Nat`, never a bare binder: the narrowing states that domain, so an unbounded subject would stop the row elaborating rather than move it between the halves.
        name: "Byte against Nat",
        binders: "b: Byte, q: Nat, x: Nat",
        held: &[
            // The carrier's own bound, which is what the oracle answers for a `Byte` however the value was produced.
            "Eq(Byte/to_nat(b) < 256, true)",
            "Eq(Byte/to_nat(b) <= 255, true)",
            "Eq(Byte/to_nat(b) + 1 <= 256, true)",
            "Eq(Byte/to_nat(b) > 255, false)",
            "Eq(256 >= Byte/to_nat(b) + 1, true)",
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
        binders: "i: Int, j: Int, k: Int, h: (Int) -> Int",
        held: &[
            // A summand meets its own spelling, as on `Nat`.
            "Eq(h(i + 1) + h(j + 1), h(j + 1) + h(i + 1))",
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
            "Eq(i < j && j <= i, false)",
            "Eq(i <= j || j < i, true)",
            // Two comparisons of one relation meet through their difference, whatever cancels: the one-sided spelling, the successor across the `<`/`<=` seam, reversal under negation, and equality.
            "Eq(i < j, +0 < j - i)",
            "Eq(i < j, j - i - 1 >= +0)",
            "Eq(i + 1 <= j, i < j)",
            "Eq(+0 - i < +0 - j, j < i)",
            "Eq(i == j, i - j == +0)",
            // A left shift by a literal count is the coefficient `2ᵏ`, as on `Nat`, and below zero too.
            "Eq(Int/shl(i, 1), i * 2)",
            "Eq(Int/shl(i, 3), 8 * i)",
            "Eq(Int/shl(i + j, 1), 2 * i + 2 * j)",
            // Divisibility, as on `Nat`: the argument needs integers and nothing more.
            "Eq(i * 2 + 1 == j * 2, false)",
            "Eq(i * 2 + 1 != j * 2, true)",
            // Euclid's identity over truncated division, at both signs of the divisor and of the copies taken.
            "Eq((i / 3) * 3 + i % 3, i)",
            "Eq(i % -3 + (i / -3) * -3, i)",
            "Eq(+0 - i % 3 - (i / 3) * 3, +0 - i)",
        ],
        refused: &[
            // The control, and not a law: constants that agree modulo the gcd meet at `i = j + 1`.
            "Eq(i * 2 == j * 2 + 2, false)",
            // A candidate, declined on purpose: true over truncated division, but reading it would rewrite a difference into a remainder where the recombination only ever shrinks a sum, so `i - d · (i / d)` and `i % d` stay two spellings.
            "Eq(i - (i / 3) * 3, i % 3)",
        ],
    },
    Carrier {
        name: "Int against Nat",
        binders: "m: Nat, n: Nat, i: Int, p: Int/NonNeg(Nat/to_int(n)), q: Int/NonNeg(i), r: Int/NonNeg(Nat/to_int(m) + Nat/to_int(n))",
        held: &[
            // The transparency pair, as `Byte against Nat` states its own: `Int/to_nat` states `0 <= int`, so each conversion reduces back through the other, and a widened natural narrows back whatever proof it was handed.
            "Eq(Int/to_nat(Nat/to_int(n), @p), n)",
            "Eq(Nat/to_int(Int/to_nat(i, @q)), i)",
            // `Nat/to_int` is an ordered-semiring embedding: it goes through a sum, a product and a floor, a non-negative combination of widened naturals narrows back to its preimage, and comparing two such combinations is comparing their preimages — which is what makes a widened natural's floor the one `Nat` already has.
            "Eq(Nat/to_int(m + n), Nat/to_int(m) + Nat/to_int(n))",
            "Eq(Nat/to_int(m * n), Nat/to_int(m) * Nat/to_int(n))",
            "Eq(Nat/to_int(m + 3), Nat/to_int(m) + 3)",
            "Eq(Int/to_nat(Nat/to_int(m) + Nat/to_int(n), @r), m + n)",
            "Eq(Nat/to_int(n) >= +0, true)",
            "Eq(Nat/to_int(m) + Nat/to_int(n) + 1 > +0, true)",
            "Eq(Nat/to_int(m) <= Nat/to_int(m * n + m), true)",
            // And a stuck comparison of widened naturals is the `Nat` comparison of the same relation.
            "Eq(Nat/to_int(m) < Nat/to_int(n), m < n)",
            "Eq(Nat/to_int(m) == Nat/to_int(n), m == n)",
        ],
        refused: &[
            // Controls, and none is a law: a widened natural against an integer that may be negative, truncated subtraction as though it were the group's, and a difference of widened naturals as though it were one.
            "Eq(Nat/to_int(n) <= i, true)",
            "Eq(Nat/to_int(m - n), Nat/to_int(m) - Nat/to_int(n))",
            "Eq(Nat/to_int(m) - Nat/to_int(n) >= +0, true)",
        ],
    },
    Carrier {
        name: "Bool",
        binders: "b: Bool, c: Bool, d: Bool, x: Nat, y: Nat, p: (Nat) -> Bool",
        held: &[
            // A leaf meets its own spelling, as a summand does on `Nat`.
            "Eq(p(x + 1) && p(y + 1), p(y + 1) && p(x + 1))",
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
            // The complement law on values: an operand beside its own negation, by cases on `b`.
            "Eq(b && Bool/not(b), false)",
            "Eq(Bool/not(b) && b, false)",
            "Eq(b || Bool/not(b), true)",
            "Eq(b == Bool/not(b), false)",
            "Eq(b != Bool/not(b), true)",
            // The laws that relate one connective to another, which no single node sees and no leaf set holds: decided where two terms are compared, by a truth table over their atoms, so no spelling a guard was keyed on is ever changed. Absorption is the one whose other side is no connective at all.
            "Eq(Bool/not(b && c), Bool/not(b) || Bool/not(c))",
            "Eq(Bool/not(b || c), Bool/not(b) && Bool/not(c))",
            "Eq(b || (b && c), b)",
            "Eq(b && (b || c), b)",
            "Eq(b, b || (b && c))",
            "Eq(b && (c || d), (b && c) || (b && d))",
            "Eq(Bool/xor(b, c), (b || c) && Bool/not(b && c))",
            "Eq((b && c) || Bool/not(b) || Bool/not(c), true)",
            // A comparison and its dual are one atom at two polarities.
            "Eq(Bool/not(x < y && c), y <= x || Bool/not(c))",
        ],
        refused: &[
            // Controls, and none is a law: each pair differs at an assignment, the last by reading a comparison and its dual at one polarity.
            "Eq(b || c, b)",
            "Eq(b && c, b || c)",
            "Eq(x < y && c, y <= x && c)",
        ],
    },
    Carrier {
        name: "Bool, at the table's cap",
        binders: "a0: Bool, a1: Bool, a2: Bool, a3: Bool, a4: Bool, a5: Bool, a6: Bool, a7: Bool, a8: Bool",
        // Absorption over eight atoms, which is the cap.
        held: &["Eq(a0 || (a0 && a1 && a2 && a3 && a4 && a5 && a6 && a7), a0)"],
        // The same law over nine: true, and declined, because a table doubles with every atom and the cap is where the question stops being asked. A candidate for a procedure that does not enumerate.
        refused: &["Eq(a0 || (a0 && a1 && a2 && a3 && a4 && a5 && a6 && a7 && a8), a0)"],
    },
    Carrier {
        name: "List, the free monoid",
        binders: "xs: List(Nat), ys: List(Nat), zs: List(Nat), a: Nat, f: (Nat) -> Nat, s: Nat, l: Nat, ok: Nat/Le(s + l, List/len(xs)), at: Nat/Lt(s, List/len(xs)), head: Nat/Lt(0, List/len(ys)), into: Nat/Lt(s, List/len(ys)), fits: Nat/Le(l, List/len(ys)), z: Nat, g: (Nat, Nat) -> Nat, h: (Nat) -> Nat",
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
            // The identity is beta, not extensionality: `map` sends every element to itself and the list is returned whole. It is recognised by what the function's body reduces to under its binder, as conversion reads it, and not by how the lambda is spelled.
            "Eq(List/map(xs, (v) => v), xs)",
            "Eq(List/map(xs, (v) => v + 0), xs)",
            "Eq(List/map(xs, (v) => ((w: Nat) => w)(v)), xs)",
            "Eq(List/len(List/map(xs, (v) => v * 1)), List/len(xs))",
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
            // Function extensionality, and not one to take: a function that is the identity only pointwise has a stuck match for a body, which reduces to no binder, and nothing short of induction on every element says otherwise.
            "Eq(List/map(xs, (v) => match v | 0 => 0 | k + 1 => k + 1 end), xs)",
            // Fusion: a map of a map is one map by the composite, a law of the functor and not of the monoid. A candidate — a fold would build the composite lambda, and nothing makes the equation inadmissible — that no consumer has asked for.
            "Eq(List/map(List/map(xs, f), h), List/map(xs, (v) => h(f(v))))",
            // Fusion's other half: a fold over a map is one fold whose step applies the function first. The same candidate for the same reason — the fold would build the composite step — and `len(map(xs, f)) = len(xs)`, held above, is the one instance of it a consumer has asked for.
            "Eq(List/fold(List/map(xs, f), z, g), List/fold(xs, z, (v, acc) => g(f(v), acc)))",
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
            // A fill of a successor count is its generator consed onto the shorter fill, which is how two fills meet without either being unrolled; its length is its count, and a positive fill is not the empty one.
            "Eq(Bytes/replicate(l + 1, k), x[k, ..Bytes/replicate(l, k)])",
            "Eq(Bytes/len(Bytes/replicate(l, k)), l)",
            "Eq(Bytes/replicate(l + 1, k) == x[], false)",
        ],
        refused: &[
            // Controls, and neither is a law: a count carrying no floor may be zero, where the fill is empty. The first is the peel's side of that line and the second the equality fold's.
            "Eq(Bytes/replicate(l, k), x[k, ..Bytes/replicate(l - 1, k)])",
            "Eq(Bytes/replicate(l, k) == x[], false)",
            // A candidate: a fill of a successor count ends in its generator as surely as it begins with one, but the peel emits only the leading one.
            "Eq(Bytes/replicate(l + 1, k), x[..Bytes/replicate(l, k), k])",
        ],
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
            "Eq(Bits/replicate(l + 1, v), b[v, ..Bits/replicate(l, v)])",
            "Eq(Bits/len(Bits/replicate(l, v)), l)",
            "Eq(Bits/replicate(l + 1, v) == b[], false)",
        ],
        refused: &[
            // The byte group's controls and candidate.
            "Eq(Bits/replicate(l, v), b[v, ..Bits/replicate(l - 1, v)])",
            "Eq(Bits/replicate(l, v) == b[], false)",
            "Eq(Bits/replicate(l + 1, v), b[..Bits/replicate(l, v), v])",
        ],
    },
    Carrier {
        name: "List, through a window",
        binders: "xs: List(Nat), ys: List(Nat), s: Nat, l: Nat, i: Nat, t: Nat, m: Nat, ok: Nat/Le(s + l, List/len(xs)), first: Nat/Lt(0, l), at: Nat/Lt(s, List/len(xs)), inside: Nat/Lt(i, l), deep: Nat/Lt(s + i, List/len(xs)), next: Nat/Lt(s + 1, List/len(xs)), other: Nat/Lt(s, List/len(ys)), inner: Nat/Le(t + m, l), whole: Nat/Le(s + t + m, List/len(xs))",
        held: &[
            // A position inside a window is the position it names in the base, and a window of a window is the window it names there. Decided where two are compared, never by rewriting one: the rewritten node would owe a bound no term in hand proves, and a comparison builds nothing, so the two bounds are never read. A carrier of its own because these rows take more binders than the monoid's do, and every goal's candidate search pays for the whole scope.
            "Eq(List/get(@Nat, List/slice(@Nat, xs, s, l, @ok), 0, @first), List/get(@Nat, xs, s, @at))",
            "Eq(List/get(@Nat, List/slice(@Nat, xs, s, l, @ok), i, @inside), List/get(@Nat, xs, s + i, @deep))",
            "Eq(List/slice(@Nat, List/slice(@Nat, xs, s, l, @ok), t, m, @inner), List/slice(@Nat, xs, s + t, m, @whole))",
        ],
        refused: &[
            // Controls, and neither is a law: the position one past the window's first, and the same position of another list.
            "Eq(List/get(@Nat, List/slice(@Nat, xs, s, l, @ok), 0, @first), List/get(@Nat, xs, s + 1, @next))",
            "Eq(List/get(@Nat, List/slice(@Nat, xs, s, l, @ok), 0, @first), List/get(@Nat, ys, s, @other))",
        ],
    },
    Carrier {
        name: "Bytes, through a window",
        binders: "bs: Bytes, cs: Bytes, s: Nat, l: Nat, i: Nat, t: Nat, m: Nat, ok: Nat/Le(s + l, Bytes/len(bs)), first: Nat/Lt(0, l), at: Nat/Lt(s, Bytes/len(bs)), inside: Nat/Lt(i, l), deep: Nat/Lt(s + i, Bytes/len(bs)), next: Nat/Lt(s + 1, Bytes/len(bs)), other: Nat/Lt(s, Bytes/len(cs)), inner: Nat/Le(t + m, l), whole: Nat/Le(s + t + m, Bytes/len(bs))",
        held: &[
            // The `List` rows, at the byte grain.
            "Eq(Bytes/get(Bytes/slice(bs, s, l, @ok), 0, @first), Bytes/get(bs, s, @at))",
            "Eq(Bytes/get(Bytes/slice(bs, s, l, @ok), i, @inside), Bytes/get(bs, s + i, @deep))",
            "Eq(Bytes/slice(Bytes/slice(bs, s, l, @ok), t, m, @inner), Bytes/slice(bs, s + t, m, @whole))",
        ],
        refused: &[
            // The `List` controls, at the byte grain.
            "Eq(Bytes/get(Bytes/slice(bs, s, l, @ok), 0, @first), Bytes/get(bs, s + 1, @next))",
            "Eq(Bytes/get(Bytes/slice(bs, s, l, @ok), 0, @first), Bytes/get(cs, s, @other))",
        ],
    },
    Carrier {
        name: "Bits, through a window",
        binders: "ts: Bits, us: Bits, s: Nat, l: Nat, i: Nat, t: Nat, m: Nat, ok: Nat/Le(s + l, Bits/len(ts)), first: Nat/Lt(0, l), at: Nat/Lt(s, Bits/len(ts)), inside: Nat/Lt(i, l), deep: Nat/Lt(s + i, Bits/len(ts)), next: Nat/Lt(s + 1, Bits/len(ts)), other: Nat/Lt(s, Bits/len(us)), inner: Nat/Le(t + m, l), whole: Nat/Le(s + t + m, Bits/len(ts))",
        held: &[
            // The `List` rows, at the bit grain: one grain's arm is not evidence for the other's.
            "Eq(Bits/get(Bits/slice(ts, s, l, @ok), 0, @first), Bits/get(ts, s, @at))",
            "Eq(Bits/get(Bits/slice(ts, s, l, @ok), i, @inside), Bits/get(ts, s + i, @deep))",
            "Eq(Bits/slice(Bits/slice(ts, s, l, @ok), t, m, @inner), Bits/slice(ts, s + t, m, @whole))",
        ],
        refused: &[
            // The `List` controls, at the bit grain.
            "Eq(Bits/get(Bits/slice(ts, s, l, @ok), 0, @first), Bits/get(ts, s + 1, @next))",
            "Eq(Bits/get(Bits/slice(ts, s, l, @ok), 0, @first), Bits/get(us, s, @other))",
        ],
    },
    Carrier {
        name: "Bits against Bytes",
        binders: "bs: Bytes, ts: Bits, a: Bool/Holds(Nat/eql(Nat/rem(Bits/len(ts), 8), 0))",
        held: &[
            // Regrouping moves no bit, so each reinterpretation reduces back through the other, in both orders.
            "Eq(Bits/to_bytes(Bytes/to_bits(bs)), bs)",
            "Eq(Bytes/to_bits(Bits/to_bytes(ts, @a)), ts)",
            // A reinterpretation's length is a shift of its operand's. A left shift by a literal count is a coefficient the Euclidean split reads, which is what discharges the byte-first trip's alignment bound above with no proof beside it; a right shift joins no division, so the other length stays one.
            "Eq(Bits/len(Bytes/to_bits(bs)), 8 * Bytes/len(bs))",
            "Eq(Bytes/len(Bits/to_bytes(ts, @a)), Nat/shr(Bits/len(ts), 3))",
        ],
        refused: &[],
    },
    Carrier {
        name: "Flt against Bytes",
        binders: "f: Flt, b: Bytes, e: Bool/Holds(Nat/eql(Bytes/len(b), 8))",
        held: &[
            // Decoding what `to_le_bytes` wrote is the float it was given. The decoding owes an eight-byte bound, discharged by the length the second row states.
            "Eq(Flt/of_le_bytes(Flt/to_le_bytes(f)), f)",
            "Eq(Bytes/len(Flt/to_le_bytes(f)), 8)",
        ],
        // The control, and not a law: decoding first and encoding back canonicalises a NaN payload the bytes carried, so the pair inverts only from the float's side.
        refused: &["Eq(Flt/to_le_bytes(Flt/of_le_bytes(b, @e)), b)"],
    },
    Carrier {
        name: "Char, over Nat",
        binders: "c: Char, d: Char",
        held: &[
            // An order on characters is the order on their code points once unfolded, so it reduces as one: reflexive cases, the mirrored comparison, and the dual of a negation.
            "Eq(Char/lt(c, c), false)",
            "Eq(Char/le(c, c), true)",
            "Eq(Char/eql(c, d), Char/eql(d, c))",
            "Eq(Char/lt(c, d), Char/gt(d, c))",
            "Eq(Char/le(c, d), Char/ge(d, c))",
            "Eq(Bool/not(Char/lt(c, d)), Char/ge(c, d))",
            "Eq(Bool/not(Char/le(c, d)), Char/gt(c, d))",
            "Eq(Char/lt(c, d) && Char/ge(c, d), false)",
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
    // Every row — held, then refused, then the sentinel — as a written goal, read back through the compiler's own refl-fit line. A held row without the line is a regression in the normalizer; a refused row with it is a law that has been taken and must move to the held rows, which is how the refused half stays a record. Every misplaced row is reported at once, since a change to one rule can move several.
    let mut misplaced = Vec::new();
    for carrier in CARRIERS {
        let rows = carrier
            .held
            .iter()
            .chain(carrier.refused)
            .chain(&[SENTINEL])
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
            if index + 1 == rows.len() {
                if !fits {
                    misplaced.push(format!(
                        "{}: the candidate search ran dry before the sentinel, so the refused rows above it were answered by an empty budget and not by the normalizer — split the carrier",
                        carrier.name
                    ));
                }
                continue;
            }
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
