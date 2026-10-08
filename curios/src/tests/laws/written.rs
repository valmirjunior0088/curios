//! The rows stated by hand: every carrier's laws no family states, the controls beside the laws a rule must stop short of, and the refused candidates with the reason conversion does not take each.

use {
    super::{Binder, IMPORTS, closes, misplaced, misplaced_by_the_kernel, orders},
    crate::tests::typecheck,
};

/// One carrier's rows: the binders every claim is stated under, the laws `Eq/refl()` closes, and the laws it refuses.
struct Carrier {
    name: &'static str,
    binders: &'static str,
    held: &'static [&'static str],
    refused: &'static [&'static str],
}

/// The binders every atom row is stated under. Every reader of the carriers' algebra keys an atom on its spelling, and two atoms that convert without being identical — `f(a + b)` and `f(b + a)`, one call under two proofs of its bound, two functions whose bodies commute a sum — are one only because each checker classes them by its own conversion before any reader sees them. Without that a pair of them falls to the congruence, which compares operands in the order the term holds them, and a verdict turns on the order the binders were declared in; so the rows are stated at every order of the sweep.
const ATOM_BINDERS: &[Binder] = &[
    Binder {
        name: "a",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "b",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "c",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "d",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "n",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "m",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "f",
        type_: "(Nat) -> Nat",
        needs: &[],
    },
    Binder {
        name: "g",
        type_: "(Nat) -> Nat",
        needs: &[],
    },
    Binder {
        name: "i",
        type_: "Int",
        needs: &[],
    },
    Binder {
        name: "j",
        type_: "Int",
        needs: &[],
    },
    Binder {
        name: "k",
        type_: "Int",
        needs: &[],
    },
    Binder {
        name: "l",
        type_: "Int",
        needs: &[],
    },
    Binder {
        name: "h",
        type_: "(Int) -> Int",
        needs: &[],
    },
    Binder {
        name: "e",
        type_: "(Int) -> Int",
        needs: &[],
    },
    Binder {
        name: "p",
        type_: "(Nat) -> Bool",
        needs: &[],
    },
    Binder {
        name: "q",
        type_: "(Nat) -> Bool",
        needs: &[],
    },
    Binder {
        name: "w",
        type_: "(n: Nat, at: Holds(n < 10)) -> Nat",
        needs: &[],
    },
    Binder {
        name: "p1",
        type_: "Holds(a < 10)",
        needs: &["a"],
    },
    Binder {
        name: "p2",
        type_: "Holds(a < 10)",
        needs: &["a"],
    },
    Binder {
        name: "q1",
        type_: "Holds(b < 10)",
        needs: &["b"],
    },
    Binder {
        name: "q2",
        type_: "Holds(b < 10)",
        needs: &["b"],
    },
    Binder {
        name: "u",
        type_: "((Nat) -> Nat) -> Nat",
        needs: &[],
    },
    Binder {
        name: "r",
        type_: "(Nat) -> Flt",
        needs: &[],
    },
    Binder {
        name: "s",
        type_: "(Nat) -> Flt",
        needs: &[],
    },
];

/// How many orders of [`ATOM_BINDERS`] the atom rows are stated at.
const ATOM_ORDERS: usize = 16;

const ATOMS_HELD: &[&str] = &[
    // A product's factors, commuted or not: the factor pairing reads them by identity.
    "Eq()(f(a + b) * g(c + d), g(d + c) * f(b + a))",
    "Eq()(f(a + b) * g(c + d), f(b + a) * g(d + c))",
    // A product inside a sum, and an atom whose argument's own application holds the sum.
    "Eq()(f(a + b) * g(c + d) + 1, 1 + g(d + c) * f(b + a))",
    "Eq()(f(g(a + b)) + g(f(c + d)), g(f(d + c)) + f(g(b + a)))",
    // A comparison's sides, paired by identity where the comparison is symmetric and read through the linear views where it is ordered.
    "Eq()(f(a + b) == g(c + d), g(d + c) == f(b + a))",
    "Eq()(f(a + b) != g(c + d), g(d + c) != f(b + a))",
    "Eq()(f(a + b) * g(c + d) < n, g(d + c) * f(b + a) < n)",
    // The group's cancellation, its monomials and its comparisons.
    "Eq()(h(i + j) + e(k + l), e(l + k) + h(j + i))",
    "Eq()(h(i + j) * e(k + l), e(l + k) * h(j + i))",
    "Eq()(h(i + j) == e(k + l), e(l + k) == h(j + i))",
    "Eq()(h(i + j) + e(k + l) <= i, e(l + k) + h(j + i) <= i)",
    // A connective's leaves as a set, and the truth table's laws over them.
    "Eq()(p(a + b) && q(c), q(c) && p(b + a))",
    "Eq()(p(a + b) || q(c + d), q(d + c) || p(b + a))",
    "Eq()(Bool/xor(p(a + b), q(c)), Bool/xor(q(c), p(b + a)))",
    "Eq()(Bool/not(p(a + b) && q(c)), Bool/not(q(c)) || Bool/not(p(b + a)))",
    "Eq()(p(a + b) || (p(b + a) && q(c)), p(a + b))",
    // Two stuck `match`es whose arms commute a sum: neither is an application, so nothing but a checker's conversion says they are one, and two on a side leave no pair to hand on as a residual.
    "Eq()((match n | 0 => a + b | _ => c end) + (match m | 0 => c + d | _ => a end), (match m | 0 => d + c | _ => a end) + (match n | 0 => b + a | _ => c end))",
    "Eq()((match n | 0 => a + b | _ => c end) * (match m | 0 => c + d | _ => a end), (match m | 0 => d + c | _ => a end) * (match n | 0 => b + a | _ => c end))",
    // One call under two proofs of its bound. The first two rows leave one unpaired factor on a side and the third leaves two: conversion is transitive only where the third holds.
    "Eq()(w(a, p1) * w(b, q1), w(a, p2) * w(b, q1))",
    "Eq()(w(a, p2) * w(b, q1), w(b, q2) * w(a, p2))",
    "Eq()(w(a, p1) * w(b, q1), w(b, q2) * w(a, p2))",
    "Eq()(w(a, p1) + w(b, q1), w(b, q2) + w(a, p2))",
    "Eq()(w(a, p1) == w(b, q1), w(b, q2) == w(a, p2))",
    // Two functions whose bodies commute a sum, which differ under a binder.
    "Eq()(u((x) => x + a) * u((x) => x + b), u((x) => b + x) * u((x) => a + x))",
    "Eq()(u((x) => x + a) + u((x) => x + b), u((x) => b + x) + u((x) => a + x))",
    // An atom whose own atoms need classing, two to a side so no residual hands the inner pair on: a classing asked for inside a classing asks about what stands inside the two atoms being compared, a call of their own head among it.
    "Eq()(f(g(a + b) * f(c + d)) * f(g(c + a) * f(b + d)), f(f(d + b) * g(a + c)) * f(f(d + c) * g(b + a)))",
    "Eq()(f(g(a + b) * f(c + d)) + f(g(c + a) * f(b + d)), f(f(d + b) * g(a + c)) + f(f(d + c) * g(b + a)))",
    // Three spellings of one atom are one monomial, so their coefficients merge.
    "Eq()((match n | 0 => a + b + d | _ => c end) + (match n | 0 => b + a + d | _ => c end) + (match n | 0 => d + b + a | _ => c end), 3 * (match n | 0 => a + b + d | _ => c end))",
    // A fold over two atoms that convert is the fold over one: reduction asks its checker's conversion which atoms of a stuck fold are one, so conversion is a congruence under the fold, and a `match` on it reduces.
    "Eq()(f(a + b) == f(b + a), true)",
    "Eq()(f(a + b) <= f(b + a), true)",
    "Eq()(f(a + b) - f(b + a), 0)",
    "Eq()(w(a, p1) == w(a, p2), true)",
    "Eq()(u((x) => x + a) - u((x) => a + x), 0)",
    "Eq()(h(i + j) == h(j + i), true)",
    "Eq()(match f(a + b) == f(b + a) | true => 0 | false => 1 end, 0)",
    // Euclid's recombination pairs a remainder with its quotient's multiple on their dividend and divisor. The two are atoms of two kinds, which nothing asks about each other, so the dividend and the divisor of each are classed: a remainder beside the multiple of a quotient over a dividend conversion holds the same is recombined, at `Nat` and at `Int`, under a comparison as under an equation.
    "Eq()(2 * (f(a + b) / 2) + f(b + a) % 2, f(a + b))",
    "Eq()(2 * (f(a + b) / 2) + f(a + b) % 2, f(b + a))",
    "Eq()(2 * (f(a + b) / 2) + f(b + a) % 2 <= f(a + b), true)",
    "Eq()(+2 * (h(i + j) / +2) + h(j + i) % +2, h(i + j))",
    // Two atoms one by conversion alone, each holding a quotient or a remainder written apart from the other's. A written `/` takes a universe level of its own, the level of the precondition its concept states, which the quotient's value does not read; a checker asked whether the two atoms are one joins no two levels, and holds the two quotients one by what each reduces to.
    "Eq()((match n | 0 => a + b | _ => c / 2 end) + (match m | 0 => c + d | _ => a % 2 end), (match m | 0 => d + c | _ => a % 2 end) + (match n | 0 => b + a | _ => c / 2 end))",
    "Eq()((match n | 0 => a + b | _ => c / 2 end) == (match m | 0 => c + d | _ => a % 2 end), (match m | 0 => d + c | _ => a % 2 end) == (match n | 0 => b + a | _ => c / 2 end))",
    "Eq()((match n | 0 => a + b | _ => c / 2 end) - (match n | 0 => b + a | _ => c / 2 end), 0)",
    // A float's sum, product, `min` and `max`, and a fused multiply-add's two factors with its addend in place, pair their operands by identity as every symmetric reader does; a difference is the sum it is spelled as.
    "Eq()(r(a + b) + s(c + d), s(d + c) + r(b + a))",
    "Eq()(r(a + b) * s(c + d), s(d + c) * r(b + a))",
    "Eq()(Flt/min(r(a + b), s(c + d)), Flt/min(s(d + c), r(b + a)))",
    "Eq()(Flt/max(r(a + b), s(c + d)), Flt/max(s(d + c), r(b + a)))",
    "Eq()(Flt/fma(r(a + b), s(c + d), r(n)), Flt/fma(s(d + c), r(b + a), r(n)))",
    "Eq()(Flt/fma(r(a + b), s(c + d), r(n + a)), Flt/fma(r(b + a), s(d + c), r(a + n)))",
    "Eq()(r(a + b) - s(c + d), Flt/neg(s(d + c)) + r(b + a))",
    // A sign copied from a value that converts with the magnitude is the magnitude's own, and from that value negated its own flipped: the negation is read through to the value under it.
    "Eq()(Flt/copysign(r(a + b), r(b + a)), r(a + b))",
    "Eq()(Flt/copysign(r(a + b), Flt/neg(r(b + a))), Flt/neg(r(a + b)))",
];

const CARRIERS: &[Carrier] = &[
    Carrier {
        name: "Nat under +",
        binders: "x: Nat, y: Nat, z: Nat, f: (Nat) -> Nat, g: (Nat) -> Nat",
        held: &[
            // Congruence under an opaque head: the arguments are compared as numbers.
            "Eq()(f(x + y), f(y + x))",
            // A summand meets its own spelling. Summands pair by identity, and two occurrences of one operator are two terms while the signature holding them is checked — each carries its own witness metavariable, solved and not yet spliced — so the solved ones are substituted before the peel, without which the elaborator alone would refuse this as `x` against `y` under the positional congruence.
            "Eq()(f(x + 1) + f(y + 1), f(y + 1) + f(x + 1))",
            // The composition of the two rows above. Cancellation pairs summands by identity and the fold leaves a stuck application's arguments as written, so `f(x + y)` would never meet `f(y + x)` inside a sum though conversion decides that pair on its own. `Nat::cancel_common` takes no reducer and asks no judgment: what makes the two summands one is the checker classing the pair's atoms by its own conversion, the conversion chain's one retry for every reader of atoms, asked only once the pair has decided nothing as it stood.
            "Eq()(f(x + y) + g(y + z), g(z + y) + f(y + x))",
        ],
        refused: &[],
    },
    Carrier {
        name: "Nat under *",
        binders: "x: Nat, y: Nat, z: Nat",
        held: &[
            "Eq()((x + 1) * 2, x * 2 + 2)",
            "Eq()(3 * (x + 2), 3 * x + 6)",
            "Eq()(2 * (3 * x), 6 * x)",
            "Eq()((x * 2) * 3, x * 6)",
            // A literal coefficient has one side, so the two spellings meet.
            "Eq()(x * 2, 2 * x)",
            // A sum is a linear combination: like terms merge, and a literal distributes over a symbolic sum.
            "Eq()(x + x, 2 * x)",
            "Eq()(x * 2, x + x)",
            "Eq()(x * 2 + x * 3, x * 5)",
            "Eq()((x + y) * 2, x * 2 + y * 2)",
            "Eq()((x + y + 1) + (x + 2), 2 * x + y + 3)",
            // A product of symbols is a monomial with one factor order, and distributes over a sum.
            "Eq()((x + 1) * (y + 2), x * y + 2 * x + y + 2)",
            "Eq()(x * y + y * x, 2 * (x * y))",
        ],
        refused: &[],
    },
    Carrier {
        name: "Nat under -",
        binders: "x: Nat, y: Nat, z: Nat",
        held: &["Eq()((x + 5) - 3, x + 2)", "Eq()((x - y) - z, x - (y + z))"],
        refused: &[],
    },
    Carrier {
        name: "Nat under / and %",
        binders: "x: Nat, y: Nat, d: Nat, p: Holds(0 < d)",
        held: &[
            "Eq()((x * 2) / 2, x)",
            "Eq()((x * 2) % 2, 0)",
            "Eq()((x * 2 + 1) % 2, 1)",
            "Eq()(x % 1, 0)",
            "Eq()(0 / (x + 1), 0)",
            "Eq()(0 % (x + 1), 0)",
            "Eq()((x + 1) / (x + 1), 1)",
            "Eq()((x + 1) % (x + 1), 0)",
            // A remainder is below its divisor and a quotient no larger than its dividend, whatever the divisor is, on the division's own proof.
            "Eq()(Nat/rem(x, d, @p) < d, true)",
            "Eq()(Nat/div(x, d, @p) <= x, true)",
            // Euclid's identity: a remainder beside the divisor times the matching quotient is the dividend, at a literal divisor, at a symbolic one, at multiplicity two beside an unrelated summand, and through the floor law.
            "Eq()((x / 10) * 10 + x % 10, x)",
            "Eq()(x % 10 + 10 * (x / 10), x)",
            "Eq()((x / (y + 1)) * (y + 1) + x % (y + 1), x)",
            "Eq()(Nat/div(x, d, @p) * d + Nat/rem(x, d, @p), x)",
            "Eq()(2 * ((x / 3) * 3) + y + 2 * (x % 3), 2 * x + y)",
            "Eq()(((x + 5) / 3) * 3 + (x + 5) % 3, x + 5)",
        ],
        refused: &[
            // Controls, and none is a law: the multiple without its remainder, the wrong multiple of the quotient, and a remainder of another dividend.
            "Eq()((x / 3) * 3, x)",
            "Eq()((x / 3) * 2 + x % 3, x)",
            "Eq()((x / 3) * 3 + y % 3, x)",
        ],
    },
    Carrier {
        name: "Nat comparisons",
        binders: "x: Nat, y: Nat",
        held: &[
            "Eq()(x < x + 1, true)",
            "Eq()(0 < x + 1, true)",
            "Eq()(x + 1 > 0, true)",
            "Eq()(x <= x + y, true)",
            "Eq()(x + y < x, false)",
            // The family is aligned probe-side: a negation reads as its dual, and `<=` as `<` of the successor.
            "Eq()(Bool/not(Bool/not(x < y)), x < y)",
            "Eq()(x <= y, x < y + 1)",
            "Eq()(x >= y, y < x + 1)",
            "Eq()(x <= 3, x < 4)",
            // The seam read alike at `Nat` and `Int`: `<=` meets `<` as `<` of its successor with the floor that reading shares cancelled, so a floored `<=` meets the `<` it spells whichever side it stands on, and whatever the fold already cancelled.
            "Eq()(x < y, x + 1 <= y)",
            "Eq()(x + y + 1 <= y + y, x < y)",
            "Eq()(Bool/not(x < y) && Bool/not(y < x), y <= x && x <= y)",
            // A comparison beside its dual computes complementary booleans at every pair.
            "Eq()(x < y && y <= x, false)",
            "Eq()(x < y || y <= x, true)",
            "Eq()(x == y && x != y, false)",
            "Eq()(x == y || x != y, true)",
            // A bound met exactly is the non-strict verdict, on either side.
            "Eq()(x % 7 <= 6, true)",
            "Eq()(x % 7 > 6, false)",
            "Eq()(6 >= x % 7, true)",
            // A value against an operand it never exceeds.
            "Eq()(x - y <= x, true)",
            "Eq()(x / (y + 1) <= x, true)",
            "Eq()(x % (y + 1) <= x, true)",
            "Eq()(x % (y + 1) < y + 1, true)",
            "Eq()(Nat/and(x, y) <= x, true)",
            "Eq()(Nat/and(x, y) <= y, true)",
            "Eq()(Nat/shr(x, y) <= x, true)",
            "Eq()(x < x - y, false)",
            "Eq()(x - y <= x + 3, true)",
            // A remainder is strictly below its divisor, so at most whatever the divisor is at most one past: the two spellings of that across the `<`/`<=` seam decide alike, under whatever stands above a floor.
            "Eq()(x % (y + 1) <= y, true)",
            "Eq()(x % 8 <= y + 7, true)",
            "Eq()(x % 8 <= y + 7, x % 8 < y + 8)",
            // Divisibility: every symbolic summand is a multiple of the gcd of the coefficients, so two floors apart modulo it meet at no value. Equality and its negation decide; the order stays open.
            "Eq()(x * 2 + 1 == y * 2, false)",
            "Eq()(x * 2 + 1 != y * 2, true)",
            "Eq()(x * 4 + 6 == y * 2 + 1, false)",
        ],
        refused: &[
            // Controls, and neither is a law: floors that agree modulo the gcd meet at `x = y + 1`, and a gcd of `1` divides every floor, so these meet at `x = y = 1`.
            "Eq()(x * 2 == y * 2 + 2, false)",
            "Eq()(x * 2 + 1 == y * 3, false)",
            // Controls for the seam, and neither is a law: a floor of two is one step past the successor, and `x + 1 <= y` against `x <= y` differs at `x = y`.
            "Eq()(x + 2 <= y, x < y)",
            "Eq()(x + 1 <= y, x <= y)",
            // The control for a bound against a floor, and not a law: `x % 8` is `7` at `x = 7`, past `y + 6` at zero.
            "Eq()(x % 8 <= y + 6, true)",
        ],
    },
    Carrier {
        // The contract `curios-core`'s `linear` module publishes, clause by clause, each with the complement that keeps it honest: a view that keeps an atom no clause bounds decides nothing.
        name: "What conversion decides about a comparison",
        binders: "x: Nat, y: Nat, i: Int, j: Int, m: Nat",
        held: &[
            // Every atom cancelled, and the constant's sign the answer — at `Nat`, at `Int`, and through the embedding.
            "Eq()(x + y + 2 <= y + x + 3, true)",
            "Eq()(2 * x + y == y + x + x, true)",
            "Eq()(i + j - i < j + 1, true)",
            "Eq()(Nat/to_int(m) + i <= i + Nat/to_int(m), true)",
            // Every monomial of one sign over non-negative atoms, and a constant that does not oppose it: a sum of naturals is at least its constant.
            "Eq()(0 < x + y + 1, true)",
            "Eq()(x < 0, false)",
            "Eq()(Nat/to_int(m) + 1 > +0, true)",
            // A side bounded by a literal, and a side through an operand it never exceeds.
            "Eq()(x % 7 < 7, true)",
            "Eq()(x - y <= x, true)",
            // An equality whose constant the coefficients' gcd does not divide.
            "Eq()(2 * x + 1 == 2 * y, false)",
        ],
        refused: &[
            // Controls, and none is a law: an atom of each sign is left (`x = 0, y = 0`, `i = 0, j = 0`); a natural atom with nothing above it may be zero; an `Int` atom may be negative; an operand an `x - y` falls short of only where `y` is positive; constants the gcd divides.
            "Eq()(x + 1 <= y, true)",
            "Eq()(i < j + 1, true)",
            "Eq()(0 < x, true)",
            "Eq()(i + 1 > +0, true)",
            "Eq()(x - y < x, true)",
            "Eq()(2 * x == 2 * y + 2, false)",
        ],
    },
    Carrier {
        name: "Nat bitwise and shifts",
        binders: "x: Nat, y: Nat, z: Nat",
        held: &[
            // A left shift by a literal count is the coefficient `2ᵏ`, on the unbounded ℕ the type level folds and at run time alike, since the running program grows a shifted value into a boxed magnitude rather than truncating it. The coefficient is charged before it is built, and the product then distributes as any other does.
            "Eq()(Nat/shl(x, 1), x * 2)",
            "Eq()(Nat/shl(x, 3), 8 * x)",
            "Eq()(Nat/shl(x + 1, 2), 4 * x + 4)",
            "Eq()(Nat/shl(x, 1) + Nat/shl(y, 1), 2 * (x + y))",
            // A symbolic count is a power of two as an atom, `pow(2, k)` however it is spelled, the count's literal floor peeled into the coefficient — which is what lets a proof about a shift recurse on its count's successor — and a right shift takes a floored count in two steps.
            "Eq()(Nat/shl(y, x + 1), 2 * Nat/shl(y, x))",
            "Eq()(Nat/shl(y, x + 3), 8 * Nat/shl(y, x))",
            "Eq()(Nat/shl(2, x), Nat/shl(1, x + 1))",
            "Eq()(Nat/shl(y + 1, x), Nat/shl(y, x) + Nat/shl(1, x))",
            "Eq()(Nat/shr(y, x + 1), Nat/shr(Nat/shr(y, x), 1))",
            // The exponent is a morphism from `(ℕ, +)` into `(ℕ, ·)`: each monomial of a count is a power of its own, a literal coefficient that power repeated, and a product of sums in the count distributed first, so a shift of a shift is the shift by the sum, whichever count comes first.
            "Eq()(Nat/shl(1, x + y), Nat/shl(1, x) * Nat/shl(1, y))",
            "Eq()(Nat/shl(1, 2 * x), Nat/shl(1, x) * Nat/shl(1, x))",
            "Eq()(Nat/shl(Nat/shl(x, y), x), Nat/shl(x, y + x))",
            "Eq()(Nat/shl(Nat/shl(z, x), y), Nat/shl(Nat/shl(z, y), x))",
            "Eq()(Nat/shl(1, x * (y + 1)), Nat/shl(1, x * y) * Nat/shl(1, x))",
            "Eq()(Nat/shl(1, (x + 1) * (y + 2)), 4 * Nat/shl(1, x * y) * Nat/shl(1, x) * Nat/shl(1, x) * Nat/shl(1, y))",
            // A power is at least one, so it is positive, and so is what a sum or a product puts over one.
            "Eq()(0 < Nat/shl(1, x), true)",
            "Eq()(Nat/shl(1, x) == 0, false)",
            "Eq()(0 < Nat/shl(x + 1, y), true)",
            "Eq()(1 <= Nat/shl(1, x) * Nat/shl(1, y), true)",
        ],
        refused: &[
            // A candidate no stage moves: a value is at most itself shifted, a product by a factor that is at least one.
            "Eq()(x <= Nat/shl(x, y), true)",
            // The controls, and neither is a law: a product in the count is no product of powers, `2¹` against `2 · 2` at `x = y = 1`; and a factor nothing is known of takes a product over a power to zero.
            "Eq()(Nat/shl(1, x * y), Nat/shl(1, x) * Nat/shl(1, y))",
            "Eq()(0 < x * Nat/shl(1, y), true)",
            // A right shift by a literal count is a quotient by `2ᵏ` and could join the division family through the Euclidean split. It may not do so by building a division node, which carries a proof that the divisor is nonzero, and a reducer may not invent one.
            "Eq()(Nat/shr(x * 4, 2), x)",
        ],
    },
    Carrier {
        // Every subject here is bounded below 256 where a `Byte` is built from a `Nat`, never a bare binder: the narrowing states that domain, so an unbounded subject would stop the row elaborating rather than move it between the halves.
        name: "Byte against Nat",
        binders: "b: Byte, q: Nat, x: Nat",
        held: &[
            // The carrier's own bound, which is what the oracle answers for a `Byte` however the value was produced.
            "Eq()(Byte/to_nat(b) < 256, true)",
            "Eq()(Byte/to_nat(b) <= 255, true)",
            "Eq()(Byte/to_nat(b) + 1 <= 256, true)",
            "Eq()(Byte/to_nat(b) > 255, false)",
            "Eq()(256 >= Byte/to_nat(b) + 1, true)",
            // And against the floor under a symbol, where nothing but the bound decides: a byte's value has no operand it never exceeds.
            "Eq()(Byte/to_nat(b) <= q + 255, true)",
            "Eq()(Nat/to_byte(Byte/to_nat(b)), b)",
            // Euclid's seam, which is what `Key(Nat)`'s recombination rests on: a residual carried in a `Byte` divides out of a scaled symbol, and the remainder twin recovers it.
            "Eq()((256 * q + Byte/to_nat(b)) / 256, q)",
            "Eq()((256 * q + Byte/to_nat(b)) % 256, Byte/to_nat(b))",
            // The control for the pair below: the same split over a bound the oracle reads off the term directly.
            "Eq()((16 * q + Nat/and(x, 15)) / 16, q)",
            // The transparency pair. `Nat/to_byte` states `nat < 256`, so the constructor is invertible and `Byte/to_nat` reduces back through it — which is what lets the second row hold: it is the control above with the operand sent through `Byte` and back, the arithmetic identical and the round trip erasing nothing the oracle could read.
            "Eq()(Byte/to_nat(Nat/to_byte(Nat/and(x, 255))), Nat/and(x, 255))",
            "Eq()((16 * q + Byte/to_nat(Nat/to_byte(Nat/and(x, 15)))) / 16, q)",
        ],
        refused: &[],
    },
    Carrier {
        name: "Int",
        binders: "i: Int, j: Int, k: Int, h: (Int) -> Int, n: Nat",
        held: &[
            // A summand meets its own spelling, as on `Nat`.
            "Eq()(h(i + 1) + h(j + 1), h(j + 1) + h(i + 1))",
            // The signed sum normal form: a group, so a subtraction is a negative coefficient and every difference cancels in full.
            "Eq()((i + 1) - 1, i)",
            "Eq()(i - (i + 1), -1)",
            "Eq()((i - j) + j, i)",
            "Eq()(i + i, 2 * i)",
            "Eq()(0 - i, -1 * i)",
            // Monomials: one factor order, and distribution past a single monomial in the fold, past two symbolic sums on demand.
            "Eq()((i + 1) * (j + 1), i * j + i + j + 1)",
            // Comparisons read through the difference.
            "Eq()(i < i + 1, true)",
            "Eq()(i + 1 <= i, false)",
            "Eq()(i == i + 1, false)",
            "Eq()(i + j < i + k, j < k)",
            // The family aligned, as on `Nat`.
            "Eq()(i <= j, i < j + 1)",
            "Eq()(i < j && j <= i, false)",
            "Eq()(i <= j || j < i, true)",
            // Two comparisons of one relation meet through their difference, whatever cancels: the one-sided spelling, the successor across the `<`/`<=` seam, reversal under negation, and equality.
            "Eq()(i < j, +0 < j - i)",
            "Eq()(i < j, j - i - 1 >= +0)",
            "Eq()(+0 - i < +0 - j, j < i)",
            "Eq()(i == j, i - j == +0)",
            // A left shift by a literal count is the coefficient `2ᵏ`, as on `Nat`, and below zero too.
            "Eq()(Int/shl(i, 1), i * 2)",
            "Eq()(Int/shl(i, 3), 8 * i)",
            "Eq()(Int/shl(i + j, 1), 2 * i + 2 * j)",
            // And by a symbolic count, as on `Nat`: the power as an atom with the count's floor peeled, and a floored right shift in two steps.
            "Eq()(Int/shl(i, n + 1), 2 * Int/shl(i, n))",
            "Eq()(Int/shr(i, n + 1), Int/shr(Int/shr(i, n), 1))",
            // Divisibility, as on `Nat`: the argument needs integers and nothing more.
            "Eq()(i * 2 + 1 == j * 2, false)",
            "Eq()(i * 2 + 1 != j * 2, true)",
            // Euclid's identity over truncated division, at both signs of the divisor and of the copies taken.
            "Eq()((i / 3) * 3 + i % 3, i)",
            "Eq()(i % -3 + (i / -3) * -3, i)",
            "Eq()(+0 - i % 3 - (i / 3) * 3, +0 - i)",
        ],
        refused: &[
            // The control, and not a law: constants that agree modulo the gcd meet at `i = j + 1`.
            "Eq()(i * 2 == j * 2 + 2, false)",
            // A candidate, declined on purpose: true over truncated division, but reading it would rewrite a difference into a remainder where the recombination only ever shrinks a sum, so `i - d · (i / d)` and `i % d` stay two spellings.
            "Eq()(i - (i / 3) * 3, i % 3)",
        ],
    },
    Carrier {
        name: "Int against Nat",
        binders: "m: Nat, n: Nat, i: Int, p: Int/NonNeg(Nat/to_int(n)), q: Int/NonNeg(i), r: Int/NonNeg(Nat/to_int(m) + Nat/to_int(n))",
        held: &[
            // The transparency pair, as `Byte against Nat` states its own: `Int/to_nat` states `0 <= int`, so each conversion reduces back through the other, and a widened natural narrows back whatever proof it was handed.
            "Eq()(Int/to_nat(Nat/to_int(n), @p), n)",
            "Eq()(Nat/to_int(Int/to_nat(i, @q)), i)",
            // `Nat/to_int` is an ordered-semiring embedding: it goes through a sum, a product and a floor, a non-negative combination of widened naturals narrows back to its preimage, and comparing two such combinations is comparing their preimages — which is what makes a widened natural's floor the one `Nat` already has.
            "Eq()(Nat/to_int(m + n), Nat/to_int(m) + Nat/to_int(n))",
            "Eq()(Nat/to_int(m * n), Nat/to_int(m) * Nat/to_int(n))",
            "Eq()(Nat/to_int(m + 3), Nat/to_int(m) + 3)",
            "Eq()(Int/to_nat(Nat/to_int(m) + Nat/to_int(n), @r), m + n)",
            "Eq()(Nat/to_int(n) >= +0, true)",
            "Eq()(Nat/to_int(m) + Nat/to_int(n) + 1 > +0, true)",
            "Eq()(Nat/to_int(m) <= Nat/to_int(m * n + m), true)",
            // And a stuck comparison of widened naturals is the `Nat` comparison of the same relation.
            "Eq()(Nat/to_int(m) < Nat/to_int(n), m < n)",
            "Eq()(Nat/to_int(m) == Nat/to_int(n), m == n)",
            // And across the `<`/`<=` seam, since the two are read through one linear view: ℕ → ℤ preserves and reflects order, and `a + 1 <= b` is `a < b` over ℤ.
            "Eq()(Nat/to_int(m) + 1 <= Nat/to_int(n), m < n)",
            "Eq()(Nat/to_int(m) < Nat/to_int(n) + 1, m <= n)",
            // The shift's exponent law, as on `Nat`: a count's sum splits, the embedding goes through a shift, and the signed power is `Nat`'s widened, so it is positive.
            "Eq()(Int/shl(i, n + m), Int/shl(Int/shl(i, n), m))",
            "Eq()(Int/shl(+1, n + m), Int/shl(+1, n) * Int/shl(+1, m))",
            "Eq()(Nat/to_int(Nat/shl(m, n)), Int/shl(Nat/to_int(m), n))",
            "Eq()(Int/shl(+1, n), Nat/to_int(Nat/shl(1, n)))",
            "Eq()(+0 < Int/shl(+1, n), true)",
            "Eq()(+0 <= Int/shl(Nat/to_int(m), n), true)",
        ],
        refused: &[
            // Controls, and none is a law: a widened natural against an integer that may be negative, truncated subtraction as though it were the group's, and a difference of widened naturals as though it were one.
            "Eq()(Nat/to_int(n) <= i, true)",
            "Eq()(Nat/to_int(m - n), Nat/to_int(m) - Nat/to_int(n))",
            "Eq()(Nat/to_int(m) - Nat/to_int(n) >= +0, true)",
        ],
    },
    Carrier {
        // Every held row holds today through `/std/Nat`'s definitions, each a `match` on a comparison that reduces, and holds after `min` and `max` are operations: stated first, they are what the promotion is held to.
        name: "Nat min and max",
        binders: "x: Nat, y: Nat, z: Nat",
        held: &[
            // An operand the comparison holds at most the other answers for both: the operand itself, a floor apart, a shared sum, zero below everything, a bound against a literal, two literals, and through a nested node. Each held through the library's definitions before `min` and `max` were operations.
            "Eq()(Nat/min(x, x + 1), x)",
            "Eq()(Nat/max(x, x + 1), x + 1)",
            "Eq()(Nat/max(x + 3, x + 1), x + 3)",
            "Eq()(Nat/min(x + y + 1, y + x), x + y)",
            "Eq()(Nat/min(x % 8, 8), x % 8)",
            "Eq()(Nat/min(3, 5), 3)",
            "Eq()(Nat/max(3, 5), 5)",
            "Eq()(Nat/min(Nat/min(x, x + 1), y), Nat/min(x, y))",
            // A literal added to both operands leaves, the least floor standing outside as a sum's does, through a nested node too.
            "Eq()(Nat/min(x + 1, y + 1), Nat/min(x, y) + 1)",
            "Eq()(Nat/max(x + 2, y + 3), Nat/max(x, y + 1) + 2)",
            "Eq()(Nat/min(Nat/min(x, y) + 1, z), Nat/min(x + 1, Nat/min(y + 1, z)))",
            // An operand at most the other where the comparison decides `<=` and not `<`.
            "Eq()(Nat/max(x, x + y), x + y)",
            "Eq()(Nat/min(x - y, x), x - y)",
            // Domination and its mirror, and the lattice's own bounds: at most a `min` is at most both operands, and each absorbs the other.
            "Eq()(Nat/min(x, y) <= x, true)",
            "Eq()(Nat/min(x, y) <= y, true)",
            "Eq()(x <= Nat/max(x, y), true)",
            "Eq()(y <= Nat/max(x, y), true)",
            "Eq()(Nat/min(x, y) <= Nat/max(x, y), true)",
            "Eq()(Nat/max(x, y) <= x + y, true)",
            "Eq()(z <= Nat/min(z + x, z + y), true)",
            "Eq()(Nat/min(x, Nat/max(x, y)), x)",
            "Eq()(Nat/max(x, Nat/min(x, y)), x)",
            // Two trees of one operation under one enclosing sum, and an equality of two that folds: one leaf set is one value.
            "Eq()(Nat/min(x, y) + 1, Nat/min(y, x) + 1)",
            "Eq()(Nat/min(z, Nat/min(y, x)), Nat/min(Nat/min(x, z), y))",
            "Eq()(Nat/min(x, y) == Nat/min(y, x), true)",
            // Two associations whose inner node drops the operand that orders the other two: one value only where the comparison's order is transitive.
            "Eq()(Nat/max(Nat/max(x % 8, 7), y + 7), Nat/max(x % 8, Nat/max(7, y + 7)))",
        ],
        refused: &[
            // Lemmas, which no stage moves: each takes a case split, or shares a symbolic summand, and the equations of a sum with `min` and `max` are not decided whole.
            "Eq()(Nat/min(x, y) + Nat/max(x, y), x + y)",
            "Eq()((x - y) + y, Nat/max(x, y))",
            "Eq()(Nat/min(x + z, y + z), Nat/min(x, y) + z)",
            // Controls, and none is a law: each fails at `x = 1, y = 0`.
            "Eq()(Nat/min(x, y), x)",
            "Eq()(Nat/max(x, y) <= y, true)",
            "Eq()(Nat/min(x, y) + 1, Nat/min(x + 1, y))",
            "Eq()(Nat/min(x, y), Nat/max(x, y))",
        ],
    },
    Carrier {
        // As `Nat`'s, with the least constant standing outside at either sign.
        name: "Int min and max",
        binders: "i: Int, j: Int, k: Int, n: Nat",
        held: &[
            "Eq()(Int/min(i, i + 1), i)",
            "Eq()(Int/max(i, i - 1), i)",
            "Eq()(Int/min(-3, +2), -3)",
            // A literal on both operands of either sign, an operand below the other by a widened natural, and domination with its mirror.
            "Eq()(Int/min(i + 1, j + 1), Int/min(i, j) + 1)",
            "Eq()(Int/max(i - 2, j + 3), Int/max(i, j + 5) - 2)",
            "Eq()(Int/min(i, -5), Int/min(i + 5, +0) - 5)",
            "Eq()(Int/max(i, i + Nat/to_int(n)), i + Nat/to_int(n))",
            "Eq()(Int/min(i, j) <= i, true)",
            "Eq()(j <= Int/max(i, j), true)",
            "Eq()(Int/min(i, j) <= Int/max(i, j), true)",
            "Eq()(Int/min(i, Int/max(i, j)), i)",
            "Eq()(k <= Int/min(k + Nat/to_int(n), k), true)",
        ],
        refused: &[
            // Lemmas: a case split, and a negation through the lattice, which turns one operation into the other.
            "Eq()(Int/min(i, j) + Int/max(i, j), i + j)",
            "Eq()(+0 - Int/min(i, j), Int/max(+0 - i, +0 - j))",
            // The control: it fails at `i = +1, j = +0`.
            "Eq()(Int/min(i, j), i)",
        ],
    },
    Carrier {
        // `abs` and `sign` read their operand through what no substitution undoes: a literal factor of every coefficient, the sign of the one that leads, and a power every monomial holds.
        name: "Int abs and sign",
        binders: "i: Int, j: Int, n: Nat, m: Nat",
        held: &[
            // `abs` inverts the embedding, through a sum, a product and a floor, and below zero where a constant makes the sign certain. Each held through the library's definitions before `abs` and `sign` were operations.
            "Eq()(Int/abs(Nat/to_int(n)), n)",
            "Eq()(Int/abs(Nat/to_int(n) + 1), n + 1)",
            "Eq()(Int/abs(Nat/to_int(n) + Nat/to_int(m)), n + m)",
            "Eq()(Int/abs(Nat/to_int(n) * Nat/to_int(m)), n * m)",
            "Eq()(Int/abs(+0 - Nat/to_int(n) - 1), n + 1)",
            "Eq()(Int/abs(Nat/to_int(Int/abs(i))), Int/abs(i))",
            "Eq()(Nat/to_int(Int/abs(Nat/to_int(n))), Nat/to_int(n))",
            // Widened back, the `abs` of an operand the comparison holds non-negative is the operand, where no natural is its preimage.
            "Eq()(Nat/to_int(Int/abs(Nat/to_int(n) - Nat/to_int(n - m))), Nat/to_int(n) - Nat/to_int(n - m))",
            "Eq()(Int/sign(Nat/to_int(n) + 1), +1)",
            "Eq()(Int/sign(+0 - Nat/to_int(n) - 1), -1)",
            // And a widened power is positive at every count, a power being at least one.
            "Eq()(Int/sign(Nat/to_int(Nat/shl(1, n))), +1)",
            "Eq()(Int/sign(Int/shl(+1, n)), +1)",
            "Eq()(Int/abs(-3), 3)",
            "Eq()(Int/sign(-3), -1)",
            "Eq()(Int/sign(+0), +0)",
            // A negation leaves `abs` and flips `sign`, at a variable, at a widened natural and at a difference, which is read from one end whichever it was written from: so two `abs` of opposite operands are one term, under a sum as alone.
            "Eq()(Int/abs(+0 - i), Int/abs(i))",
            "Eq()(Int/sign(+0 - i), +0 - Int/sign(i))",
            "Eq()(Int/abs(+0 - Nat/to_int(n)), n)",
            "Eq()(Int/abs(i - j), Int/abs(j - i))",
            "Eq()(Int/sign(i - j), +0 - Int/sign(j - i))",
            "Eq()(Int/abs(i - j) + 1, Int/abs(j - i) + 1)",
            // A literal factor leaves by its magnitude and its sign, out of a sum too, and a power as a literal does.
            "Eq()(Int/abs(8 * i), 8 * Int/abs(i))",
            "Eq()(Int/abs(-8 * i), 8 * Int/abs(i))",
            "Eq()(Int/abs(2 * i - 2 * j), 2 * Int/abs(i - j))",
            "Eq()(Int/abs(6 * i + 9), 3 * Int/abs(2 * i + 3))",
            "Eq()(Int/sign(8 * i), Int/sign(i))",
            "Eq()(Int/sign(-8 * i), +0 - Int/sign(i))",
            "Eq()(Int/abs(Int/shl(i, n)), Nat/shl(Int/abs(i), n))",
            "Eq()(Int/sign(Int/shl(i, n)), Int/sign(i))",
            "Eq()(Int/abs(Nat/to_int(Nat/pow(3, n)) * i), Nat/pow(3, n) * Int/abs(i))",
            "Eq()(Int/sign(Nat/to_int(Nat/pow(3, n)) * i), Int/sign(i))",
            // Each read at a product of two sums, the one shape a fold leaves standing: the operand is normalized before it is read, so a law holds of it as written and as distributed.
            "Eq()(Int/abs(+0 - (i + 1) * (j + 1)), Int/abs((i + 1) * (j + 1)))",
            "Eq()(Int/sign(+0 - (i + 1) * (j + 1)), +0 - Int/sign((i + 1) * (j + 1)))",
            "Eq()(Int/abs(8 * ((i + 1) * (j + 1))), 8 * Int/abs((i + 1) * (j + 1)))",
            "Eq()(Int/abs(Int/shl((i + 1) * (j + 1), n)), Nat/shl(Int/abs((i + 1) * (j + 1)), n))",
            "Eq()(Int/abs((i + 1) * (j + 1)), Int/abs(i * j + i + j + 1))",
        ],
        refused: &[
            // Lemmas, which no stage moves: a product reaches either operation distributed, so deciding these takes factoring it; the decomposition is a product of the two; and a comparison of an `abs` with zero is a case split.
            "Eq()(Int/abs(i * j), Int/abs(i) * Int/abs(j))",
            "Eq()(Int/sign(i * j), Int/sign(i) * Int/sign(j))",
            "Eq()(Int/sign(i) * Nat/to_int(Int/abs(i)), i)",
            "Eq()(Int/abs(i) == 0, i == +0)",
            "Eq()(Int/sign(Int/sign(i)), Int/sign(i))",
            // A power of a symbolic base does not leave as one of a literal base does: at a literal exponent it is so many plain factors, which stay inside, so a rule that took it out would fail there.
            "Eq()(Int/abs(Nat/to_int(Nat/pow(m, n)) * i), Nat/pow(m, n) * Int/abs(i))",
            // Controls, and none is a law: the first fails at `i = +1, j = -1`, the second at `i = +1, j = +1`, and the third at `i = -1`.
            "Eq()(Int/abs(i + j), Int/abs(i) + Int/abs(j))",
            "Eq()(Int/sign(i + j), Int/sign(i) + Int/sign(j))",
            "Eq()(Nat/to_int(Int/abs(i)), i)",
        ],
    },
    Carrier {
        // `pow` is declared a morphism from `(ℕ, +)` into `(ℕ, ·)` at a fixed base, and nothing is read of the base but a literal.
        name: "Nat powers",
        binders: "x: Nat, y: Nat, z: Nat",
        held: &[
            // Each held through `/std/Nat/pow`, a recursion by squaring that unfolded at a literal exponent, before `pow` was an operation.
            "Eq()(Nat/pow(x, 0), 1)",
            "Eq()(Nat/pow(x, 1), x)",
            "Eq()(Nat/pow(x, 2), x * x)",
            "Eq()(Nat/pow(x, 3), x * x * x)",
            "Eq()(Nat/pow(2 * x, 2), 4 * x * x)",
            "Eq()(Nat/pow(x + 1, 2), x * x + 2 * x + 1)",
            "Eq()(Nat/pow(2, 10), 1024)",
            "Eq()(Nat/pow(0, 3), 0)",
            "Eq()(Nat/pow(0, 0), 1)",
            "Eq()(Nat/pow(1, 5), 1)",
            // The exponent's sum is a product: a symbolic summand, a floor, a literal coefficient, and a product of sums in the exponent, which is distributed first.
            "Eq()(Nat/pow(x, y + z), Nat/pow(x, y) * Nat/pow(x, z))",
            "Eq()(Nat/pow(x, y + 1), x * Nat/pow(x, y))",
            "Eq()(Nat/pow(2, y + 3), 8 * Nat/pow(2, y))",
            "Eq()(Nat/pow(x, 2 * y), Nat/pow(x, y) * Nat/pow(x, y))",
            "Eq()(Nat/pow(x, y * (z + 1)), Nat/pow(x, y * z) * Nat/pow(x, y))",
            // The bases that decide a power whatever its exponent, and a power of a positive base.
            "Eq()(Nat/pow(1, y), 1)",
            "Eq()(Nat/pow(0, y + 1), 0)",
            "Eq()(0 < Nat/pow(2, y), true)",
            "Eq()(0 < Nat/pow(x + 1, y), true)",
            // The shift read through the power, one atom for the two.
            "Eq()(Nat/shl(x, y), x * Nat/pow(2, y))",
            "Eq()(Nat/shl(1, y), Nat/pow(2, y))",
            // A base is an atom up to conversion: two spellings of one base are one power, a product of two sums among them, which a fold leaves standing.
            "Eq()(Nat/pow(x + z, y), Nat/pow(z + x, y))",
            "Eq()(Nat/pow((x + 1) * (z + 1), y), Nat/pow(x * z + x + z + 1, y))",
            "Eq()(Nat/pow((x + 1) * (z + 1), y + 1), (x + 1) * (z + 1) * Nat/pow((x + 1) * (z + 1), y))",
        ],
        refused: &[
            // Lemmas, which no stage moves: a power of a power beside `pow(x, 2) = x * x` gives a power of a product, which at two literals factors a base, and no finite set of identities is complete for exponentiation. The last compares two powers of one base, a product by a factor that is at least one.
            "Eq()(Nat/pow(Nat/pow(x, y), z), Nat/pow(x, y * z))",
            "Eq()(Nat/pow(x * y, z), Nat/pow(x, z) * Nat/pow(y, z))",
            "Eq()(Nat/pow(6, y), Nat/pow(2, y) * Nat/pow(3, y))",
            "Eq()(Nat/pow(4, y), Nat/pow(2, 2 * y))",
            "Eq()(Nat/pow(2, y) <= Nat/pow(2, y + z), true)",
            // Controls, and none is a law: `2³` is not `3²`; a square of a sum has its cross term; a sum in the exponent is no sum of powers; and `0¹` is zero.
            "Eq()(Nat/pow(x, y), Nat/pow(y, x))",
            "Eq()(Nat/pow(x + y, 2), x * x + y * y)",
            "Eq()(Nat/pow(x, y + z), Nat/pow(x, y) + Nat/pow(x, z))",
            "Eq()(0 < Nat/pow(x, y), true)",
        ],
    },
    Carrier {
        // What `Int` shares with `Nat`, and what it does not: `Int` divides truncating toward zero and shifts right arithmetically.
        name: "Int, the families it shares with Nat",
        binders: "i: Int, j: Int, k: Int, n: Nat, p: Int/NonZero(j)",
        held: &[
            // The division's laws that read no sign, which no family of the law table states: of zero, of the divisor itself, by `-1`, and of an exact multiple, whatever the signs of the coefficient and the divisor. The bitwise identities with `-1` the unit of `and` and the absorber of `or`, the shifts' zero laws, the quotient by one and the product's associativity are the table's rows.
            "Eq()(i % +1, +0)",
            "Eq()(i / -1, +0 - i)",
            "Eq()(i % -1, +0)",
            "Eq()(Int/div(+0, j, @p), +0)",
            "Eq()(Int/rem(+0, j, @p), +0)",
            "Eq()(Int/div(j, j, @p), +1)",
            "Eq()(Int/rem(j, j, @p), +0)",
            "Eq()((i * 2) / 2, i)",
            "Eq()((i * 2) % 2, +0)",
            "Eq()((i * -6) / 2, i * -3)",
            "Eq()((i * 2) / -2, +0 - i)",
            "Eq()((i * 4 + k * 6 + 8) / 2, i * 2 + k * 3 + 4)",
            "Eq()((i * 4 + k * 6 + 8) % -2, +0)",
        ],
        refused: &[
            // A candidate no stage moves: a remainder's magnitude is below its divisor's, a bound through `abs` nothing declares.
            "Eq()(i % 3 < +3, true)",
            // Controls, each a law of `Nat` and none of `Int`, failing at an operand below zero: a remainder takes its dividend's sign, so an odd number's is `-1`; the floor law and the Euclidean split count whole divisors, which a truncated quotient does not; and nothing dominates below zero, where a difference, a bitwise `and`, an arithmetic shift, a quotient and a remainder each pass their operand.
            "Eq()((i * 2 + 1) % 2, +1)",
            "Eq()((i + 2) / 2, 1 + i / 2)",
            "Eq()((2 * i + 1) / 2, i)",
            "Eq()(i - k <= i, true)",
            "Eq()(Int/and(i, k) <= i, true)",
            "Eq()(Int/shr(i, n) <= i, true)",
            "Eq()(i / 2 <= i, true)",
            "Eq()(+0 <= i % 3, true)",
            // And the complement is not the negation: `not(i)` is `-1 - i`.
            "Eq()(Int/xor(i, -1), +0 - i)",
        ],
    },
    Carrier {
        name: "Bool",
        binders: "b: Bool, c: Bool, d: Bool, x: Nat, y: Nat, p: (Nat) -> Bool",
        held: &[
            // A leaf meets its own spelling, as a summand does on `Nat`.
            "Eq()(p(x + 1) && p(y + 1), p(y + 1) && p(x + 1))",
            // An equality against a literal is its operand or the operand's negation, and a negation undoes itself.
            "Eq()(b == false, Bool/not(b))",
            "Eq()(b != true, Bool/not(b))",
            "Eq()(Bool/not(Bool/not(b)), b)",
            // `&&` and `||` are semilattices on their leaves, decided as sets by the peel: commuted, reassociated and repeated leaves are one value.
            "Eq()((b && c) && b, c && b)",
            // The laws that relate one connective to another, which no single node sees and no leaf set holds: decided where two terms are compared, by a truth table over their atoms, so no spelling a guard was keyed on is ever changed. Absorption is the one whose other side is no connective at all.
            "Eq()(Bool/not(b && c), Bool/not(b) || Bool/not(c))",
            "Eq()(Bool/not(b || c), Bool/not(b) && Bool/not(c))",
            "Eq()(b || (b && c), b)",
            "Eq()(b && (b || c), b)",
            "Eq()(b, b || (b && c))",
            "Eq()(Bool/xor(b, c), (b || c) && Bool/not(b && c))",
            "Eq()((b && c) || Bool/not(b) || Bool/not(c), true)",
            // A comparison and its dual are one atom at two polarities.
            "Eq()(Bool/not(x < y && c), y <= x || Bool/not(c))",
        ],
        refused: &[
            // Controls, and none is a law: each pair differs at an assignment, the last by reading a comparison and its dual at one polarity.
            "Eq()(b || c, b)",
            "Eq()(b && c, b || c)",
            "Eq()(x < y && c, y <= x && c)",
        ],
    },
    Carrier {
        name: "Bool, at the table's cap",
        binders: "a0: Bool, a1: Bool, a2: Bool, a3: Bool, a4: Bool, a5: Bool, a6: Bool, a7: Bool, a8: Bool",
        // Absorption over eight atoms, which is the cap.
        held: &["Eq()(a0 || (a0 && a1 && a2 && a3 && a4 && a5 && a6 && a7), a0)"],
        // The same law over nine: true, and declined, because a table doubles with every atom and the cap is where the question stops being asked. A candidate for a procedure that does not enumerate.
        refused: &["Eq()(a0 || (a0 && a1 && a2 && a3 && a4 && a5 && a6 && a7 && a8), a0)"],
    },
    Carrier {
        name: "List, the free monoid",
        binders: "xs: List(Nat), ys: List(Nat), zs: List(Nat), a: Nat, f: (Nat) -> Nat, s: Nat, l: Nat, ok: Holds(s + l <= List/len(xs)), at: Holds(s < List/len(xs)), head: Holds(0 < List/len(ys)), into: Holds(s < List/len(ys)), fits: Holds(l <= List/len(ys)), z: Nat, g: (Nat, Nat) -> Nat, h: (Nat) -> Nat",
        held: &[
            "Eq()([..[..xs, ..ys], ..zs], [..xs, ..ys, ..zs])",
            "Eq()([..xs, ..[..ys, ..zs]], [..xs, ..ys, ..zs])",
            "Eq()(List/len([a, ..xs]), List/len(xs) + 1)",
            "Eq()(List/len([1, 2, ..xs]), List/len(xs) + 2)",
            "Eq()(List/map(@Nat, @Nat, [], f), [])",
            // The identity is beta, not extensionality: `map` sends every element to itself and the list is returned whole. It is recognised by what the function's body reduces to under its binder, as conversion reads it, and not by how the lambda is spelled.
            "Eq()(List/map(xs, (v) => v), xs)",
            "Eq()(List/map(xs, (v) => v + 0), xs)",
            "Eq()(List/map(xs, (v) => ((w: Nat) => w)(v)), xs)",
            "Eq()(List/len(List/map(xs, (v) => v * 1)), List/len(xs))",
            // An index into a map is the function at the index into its argument; the bound is the argument's, since `len(map(xs, f))` is `len(xs)`.
            "Eq()(List/get(@Nat, List/map(xs, f), s, @at), f(List/get(@Nat, xs, s, @at)))",
            "Eq()(List/map([a, ..xs], f), [f(a), ..List/map(xs, f)])",
            "Eq()(List/map([..xs, ..ys], f), [..List/map(xs, f), ..List/map(ys, f)])",
            "Eq()(List/slice(xs, 0, List/len(xs)), xs)",
            "Eq()(List/slice(xs, 0, 0), [])",
            "Eq()(List/slice([a, ..xs], 1, List/len(xs)), xs)",
            "Eq()(List/get([a, ..xs], 0), a)",
            "Eq()(List/len(List/map(xs, f)), List/len(xs))",
            "Eq()(List/slice([..xs, ..ys], 0, List/len(xs)), xs)",
            "Eq()(List/slice([..xs, ..ys], List/len(xs), List/len(ys)), ys)",
            "Eq()(List/slice([..xs, ..ys, ..zs], List/len(xs), List/len(ys)), ys)",
            "Eq()(List/slice([..xs, ..ys, ..zs], 0, List/len(xs) + List/len(ys)), [..xs, ..ys])",
            // An append is the concatenation the peel's own law says it is, so a window at its seam locates like any other — held here as it is at `Bytes`.
            "Eq()(List/slice([..xs, a], 0, List/len(xs)), xs)",
            // An index at a symbolic seam, where the operand beginning there carries exactly one element. The same walk that locates the window above, asked for a window of one.
            "Eq()(List/get([..xs, a, ..ys], List/len(xs)), a)",
            "Eq()(List/get([..xs, a], List/len(xs)), a)",
            // A window's length is the count it was cut to, and a map moves inside a window with the same bound, since `len(map(xs, f))` is `len(xs)`. Both rest on `slice`'s own precondition, which the binder `ok` states.
            "Eq()(List/len(List/slice(@Nat, xs, s, l, @ok)), l)",
            "Eq()(List/map(List/slice(@Nat, xs, s, l, @ok), f), List/slice(@Nat, List/map(xs, f), s, l, @ok))",
            // A window past every operand but the last is that window into the last operand. The bound cancels the consumed prefix off both sides, so the same proof places the narrowed operation.
            "Eq()(List/get(@Nat, [..xs, ..ys], List/len(xs), @head), List/get(@Nat, ys, 0, @head))",
            "Eq()(List/get(@Nat, [..xs, ..ys], List/len(xs) + s, @into), List/get(@Nat, ys, s, @into))",
            "Eq()(List/slice(@Nat, [..xs, ..ys], List/len(xs), l, @fits), List/slice(@Nat, ys, 0, l, @fits))",
            // A left fold reduces over the shape: the empty run is the seed, a cons steps once and folds the tail from there, and a concatenation folds its operands in order.
            "Eq()(List/fold(@Nat, @Nat, [], z, g), z)",
            "Eq()(List/fold([a], z, g), g(a, z))",
            "Eq()(List/fold([a, ..xs], z, g), List/fold(xs, g(a, z), g))",
            "Eq()(List/fold([..xs, ..ys], z, g), List/fold(ys, List/fold(xs, z, g), g))",
            "Eq()(List/fold([..xs, a], z, g), g(a, List/fold(xs, z, g)))",
        ],
        refused: &[
            // Function extensionality, and not one to take: a function that is the identity only pointwise has a stuck match for a body, which reduces to no binder, and nothing short of induction on every element says otherwise.
            "Eq()(List/map(xs, (v) => match v | 0 => 0 | k + 1 => k + 1 end), xs)",
            // Fusion: a map of a map is one map by the composite, a law of the functor and not of the monoid. A candidate — a fold would build the composite lambda, and nothing makes the equation inadmissible — that no consumer has asked for.
            "Eq()(List/map(List/map(xs, f), h), List/map(xs, (v) => h(f(v))))",
            // Fusion's other half: a fold over a map is one fold whose step applies the function first. The same candidate for the same reason — the fold would build the composite step — and `len(map(xs, f)) = len(xs)`, held above, is the one instance of it a consumer has asked for.
            "Eq()(List/fold(List/map(xs, f), z, g), List/fold(xs, z, (v, acc) => g(f(v), acc)))",
        ],
    },
    Carrier {
        name: "Bytes, the free monoid",
        binders: "bs: Bytes, cs: Bytes, ds: Bytes, k: Byte, s: Nat, l: Nat, ok: Holds(s + l <= Bytes/len(bs)), head: Holds(0 < Bytes/len(cs)), into: Holds(s < Bytes/len(cs)), fits: Holds(l <= Bytes/len(cs))",
        held: &[
            "Eq()(x[..x[..bs, ..cs], ..ds], x[..bs, ..cs, ..ds])",
            "Eq()(Bytes/len(x[k, ..bs]), Bytes/len(bs) + 1)",
            "Eq()(Bytes/len(x[1, 2, ..bs]), Bytes/len(bs) + 2)",
            "Eq()(Bytes/slice(bs, 0, Bytes/len(bs)), bs)",
            "Eq()(Bytes/slice(bs, 0, 0), x[])",
            "Eq()(Bytes/slice(x[k, ..bs], 1, Bytes/len(bs)), bs)",
            "Eq()(Bytes/get(x[k, ..bs], 0), k)",
            "Eq()(Bytes/eql(bs, bs), true)",
            "Eq()(Bytes/slice(x[..bs, ..cs], 0, Bytes/len(bs)), bs)",
            "Eq()(Bytes/slice(x[..bs, ..cs], Bytes/len(bs), Bytes/len(cs)), cs)",
            "Eq()(Bytes/slice(x[..bs, ..cs, ..ds], Bytes/len(bs), Bytes/len(cs)), cs)",
            // An index at a symbolic seam, which the same walk finds as a window of one. The operand beginning there carries a single generator, read out of it directly rather than through an inner `get` that would need a bound nothing states.
            "Eq()(Bytes/get(x[..bs, k, ..cs], Bytes/len(bs)), k)",
            "Eq()(Bytes/get(x[..bs, k], Bytes/len(bs)), k)",
            // An append is the concatenation the peel's own law says it is, so a window at its seam locates like any other.
            "Eq()(Bytes/slice(x[..bs, k], 0, Bytes/len(bs)), bs)",
            // A window's length is the count it was cut to: `slice` takes `s + l <= len(b)`, so the count is the measure at every well-typed instance. Taken where the homomorphism reads the window rather than in `free_monoid`'s measure, which counts only literal runs.
            "Eq()(Bytes/len(Bytes/slice(bs, s, l, @ok)), l)",
            // A window past every operand but the last is that window into the last operand, under the caller's own bound.
            "Eq()(Bytes/get(x[..bs, ..cs], Bytes/len(bs), @head), Bytes/get(cs, 0, @head))",
            "Eq()(Bytes/get(x[..bs, ..cs], Bytes/len(bs) + s, @into), Bytes/get(cs, s, @into))",
            "Eq()(Bytes/slice(x[..bs, ..cs], Bytes/len(bs), l, @fits), Bytes/slice(cs, 0, l, @fits))",
            // A positive segment anywhere in a value decides it against the empty one, and against a value it extends.
            "Eq()(x[..bs, k] == x[], false)",
            "Eq()(x[..bs, 1] == x[], false)",
            "Eq()(x[..bs, k] == bs, false)",
            "Eq()(x[..bs, ..cs, 1] == bs, false)",
            // A fill of a successor count is its generator consed onto the shorter fill, which is how two fills meet without either being unrolled; its length is its count, and a positive fill is not the empty one.
            "Eq()(Bytes/replicate(l + 1, k), x[k, ..Bytes/replicate(l, k)])",
            "Eq()(Bytes/len(Bytes/replicate(l, k)), l)",
            "Eq()(Bytes/replicate(l + 1, k) == x[], false)",
        ],
        refused: &[
            // Controls, and neither is a law: a count carrying no floor may be zero, where the fill is empty. The first is the peel's side of that line and the second the equality fold's.
            "Eq()(Bytes/replicate(l, k), x[k, ..Bytes/replicate(l - 1, k)])",
            "Eq()(Bytes/replicate(l, k) == x[], false)",
            // A candidate: a fill of a successor count ends in its generator as surely as it begins with one, but the peel emits only the leading one.
            "Eq()(Bytes/replicate(l + 1, k), x[..Bytes/replicate(l, k), k])",
        ],
    },
    Carrier {
        name: "Bits, the free monoid",
        binders: "ts: Bits, us: Bits, ws: Bits, v: Bool, s: Nat, l: Nat, ok: Holds(s + l <= Bits/len(ts)), head: Holds(0 < Bits/len(us)), into: Holds(s < Bits/len(us)), fits: Holds(l <= Bits/len(us))",
        held: &[
            // The byte grain's laws, stated again here: one grain's fold arm is not evidence for the other's, and a law held at one grain and unstated at the other is a copy with nothing checking it. The explanations are the byte group's, above.
            "Eq()(b[..b[..ts, ..us], ..ws], b[..ts, ..us, ..ws])",
            "Eq()(Bits/len(b[v, ..ts]), Bits/len(ts) + 1)",
            "Eq()(Bits/len(b[1, 0, ..ts]), Bits/len(ts) + 2)",
            "Eq()(Bits/slice(ts, 0, Bits/len(ts)), ts)",
            "Eq()(Bits/slice(ts, 0, 0), b[])",
            "Eq()(Bits/slice(b[v, ..ts], 1, Bits/len(ts)), ts)",
            "Eq()(Bits/get(b[v, ..ts], 0), v)",
            "Eq()(Bits/eql(ts, ts), true)",
            "Eq()(Bits/slice(b[..ts, ..us], 0, Bits/len(ts)), ts)",
            "Eq()(Bits/slice(b[..ts, ..us], Bits/len(ts), Bits/len(us)), us)",
            "Eq()(Bits/slice(b[..ts, ..us, ..ws], Bits/len(ts), Bits/len(us)), us)",
            "Eq()(Bits/get(b[..ts, v, ..us], Bits/len(ts)), v)",
            "Eq()(Bits/get(b[..ts, v], Bits/len(ts)), v)",
            "Eq()(Bits/slice(b[..ts, v], 0, Bits/len(ts)), ts)",
            "Eq()(Bits/len(Bits/slice(ts, s, l, @ok)), l)",
            "Eq()(Bits/get(b[..ts, ..us], Bits/len(ts), @head), Bits/get(us, 0, @head))",
            "Eq()(Bits/get(b[..ts, ..us], Bits/len(ts) + s, @into), Bits/get(us, s, @into))",
            "Eq()(Bits/slice(b[..ts, ..us], Bits/len(ts), l, @fits), Bits/slice(us, 0, l, @fits))",
            "Eq()(b[..ts, v] == b[], false)",
            "Eq()(b[..ts, 1] == b[], false)",
            "Eq()(b[..ts, v] == ts, false)",
            "Eq()(Bits/replicate(l + 1, v), b[v, ..Bits/replicate(l, v)])",
            "Eq()(Bits/len(Bits/replicate(l, v)), l)",
            "Eq()(Bits/replicate(l + 1, v) == b[], false)",
        ],
        refused: &[
            // The byte group's controls and candidate.
            "Eq()(Bits/replicate(l, v), b[v, ..Bits/replicate(l - 1, v)])",
            "Eq()(Bits/replicate(l, v) == b[], false)",
            "Eq()(Bits/replicate(l + 1, v), b[..Bits/replicate(l, v), v])",
        ],
    },
    Carrier {
        name: "List, through a window",
        binders: "xs: List(Nat), ys: List(Nat), s: Nat, l: Nat, i: Nat, t: Nat, m: Nat, ok: Holds(s + l <= List/len(xs)), first: Holds(0 < l), at: Holds(s < List/len(xs)), inside: Holds(i < l), deep: Holds(s + i < List/len(xs)), next: Holds(s + 1 < List/len(xs)), other: Holds(s < List/len(ys)), inner: Holds(t + m <= l), whole: Holds(s + t + m <= List/len(xs))",
        held: &[
            // A position inside a window is the position it names in the base, and a window of a window is the window it names there. Decided where two are compared, never by rewriting one: the rewritten node would owe a bound no term in hand proves, and a comparison builds nothing, so the two bounds are never read. A carrier of its own because these rows take more binders than the monoid's do, and every goal's candidate search pays for the whole scope.
            "Eq()(List/get(@Nat, List/slice(@Nat, xs, s, l, @ok), 0, @first), List/get(@Nat, xs, s, @at))",
            "Eq()(List/get(@Nat, List/slice(@Nat, xs, s, l, @ok), i, @inside), List/get(@Nat, xs, s + i, @deep))",
            "Eq()(List/slice(@Nat, List/slice(@Nat, xs, s, l, @ok), t, m, @inner), List/slice(@Nat, xs, s + t, m, @whole))",
        ],
        refused: &[
            // Controls, and neither is a law: the position one past the window's first, and the same position of another list.
            "Eq()(List/get(@Nat, List/slice(@Nat, xs, s, l, @ok), 0, @first), List/get(@Nat, xs, s + 1, @next))",
            "Eq()(List/get(@Nat, List/slice(@Nat, xs, s, l, @ok), 0, @first), List/get(@Nat, ys, s, @other))",
        ],
    },
    Carrier {
        name: "Bytes, through a window",
        binders: "bs: Bytes, cs: Bytes, s: Nat, l: Nat, i: Nat, t: Nat, m: Nat, ok: Holds(s + l <= Bytes/len(bs)), first: Holds(0 < l), at: Holds(s < Bytes/len(bs)), inside: Holds(i < l), deep: Holds(s + i < Bytes/len(bs)), next: Holds(s + 1 < Bytes/len(bs)), other: Holds(s < Bytes/len(cs)), inner: Holds(t + m <= l), whole: Holds(s + t + m <= Bytes/len(bs))",
        held: &[
            // The `List` rows, at the byte grain.
            "Eq()(Bytes/get(Bytes/slice(bs, s, l, @ok), 0, @first), Bytes/get(bs, s, @at))",
            "Eq()(Bytes/get(Bytes/slice(bs, s, l, @ok), i, @inside), Bytes/get(bs, s + i, @deep))",
            "Eq()(Bytes/slice(Bytes/slice(bs, s, l, @ok), t, m, @inner), Bytes/slice(bs, s + t, m, @whole))",
        ],
        refused: &[
            // The `List` controls, at the byte grain.
            "Eq()(Bytes/get(Bytes/slice(bs, s, l, @ok), 0, @first), Bytes/get(bs, s + 1, @next))",
            "Eq()(Bytes/get(Bytes/slice(bs, s, l, @ok), 0, @first), Bytes/get(cs, s, @other))",
        ],
    },
    Carrier {
        name: "Bits, through a window",
        binders: "ts: Bits, us: Bits, s: Nat, l: Nat, i: Nat, t: Nat, m: Nat, ok: Holds(s + l <= Bits/len(ts)), first: Holds(0 < l), at: Holds(s < Bits/len(ts)), inside: Holds(i < l), deep: Holds(s + i < Bits/len(ts)), next: Holds(s + 1 < Bits/len(ts)), other: Holds(s < Bits/len(us)), inner: Holds(t + m <= l), whole: Holds(s + t + m <= Bits/len(ts))",
        held: &[
            // The `List` rows, at the bit grain: one grain's arm is not evidence for the other's.
            "Eq()(Bits/get(Bits/slice(ts, s, l, @ok), 0, @first), Bits/get(ts, s, @at))",
            "Eq()(Bits/get(Bits/slice(ts, s, l, @ok), i, @inside), Bits/get(ts, s + i, @deep))",
            "Eq()(Bits/slice(Bits/slice(ts, s, l, @ok), t, m, @inner), Bits/slice(ts, s + t, m, @whole))",
        ],
        refused: &[
            // The `List` controls, at the bit grain.
            "Eq()(Bits/get(Bits/slice(ts, s, l, @ok), 0, @first), Bits/get(ts, s + 1, @next))",
            "Eq()(Bits/get(Bits/slice(ts, s, l, @ok), 0, @first), Bits/get(us, s, @other))",
        ],
    },
    Carrier {
        name: "List, through a concatenation",
        binders: "xs: List(Nat), ys: List(Nat), i: Nat, s: Nat, l: Nat, joined: Holds(i < List/len([..xs, ..ys])), left: Holds(i < List/len(xs)), right: Holds(i < List/len(ys)), spans: Holds(s + l <= List/len([..xs, ..ys])), inside: Holds(s + l <= List/len(xs))",
        held: &[
            // A position inside an operand of a concatenation is that operand's position, and so is a window inside one. Decided where two are compared, as a position through a window is: the concatenation's bound reaches past the operand, so rewriting the read would owe a bound no term in hand proves, while the operand's own read is typed by the bound that places the position inside it.
            "Eq()(List/get(@Nat, [..xs, ..ys], i, @joined), List/get(@Nat, xs, i, @left))",
            "Eq()(List/slice(@Nat, [..xs, ..ys], s, l, @spans), List/slice(@Nat, xs, s, l, @inside))",
        ],
        refused: &[
            // Control, and no law: the other operand at the same offset, which begins at `len(xs)` rather than at `0`.
            "Eq()(List/get(@Nat, [..xs, ..ys], i, @joined), List/get(@Nat, ys, i, @right))",
        ],
    },
    Carrier {
        name: "Bytes, through a concatenation",
        binders: "bs: Bytes, cs: Bytes, i: Nat, s: Nat, l: Nat, joined: Holds(i < Bytes/len(x[..bs, ..cs])), left: Holds(i < Bytes/len(bs)), right: Holds(i < Bytes/len(cs)), spans: Holds(s + l <= Bytes/len(x[..bs, ..cs])), inside: Holds(s + l <= Bytes/len(bs))",
        held: &[
            // The `List` rows, at the byte grain.
            "Eq()(Bytes/get(x[..bs, ..cs], i, @joined), Bytes/get(bs, i, @left))",
            "Eq()(Bytes/slice(x[..bs, ..cs], s, l, @spans), Bytes/slice(bs, s, l, @inside))",
        ],
        refused: &[
            // The `List` control, at the byte grain.
            "Eq()(Bytes/get(x[..bs, ..cs], i, @joined), Bytes/get(cs, i, @right))",
        ],
    },
    Carrier {
        name: "Bits, through a concatenation",
        binders: "ts: Bits, us: Bits, i: Nat, s: Nat, l: Nat, joined: Holds(i < Bits/len(b[..ts, ..us])), left: Holds(i < Bits/len(ts)), right: Holds(i < Bits/len(us)), spans: Holds(s + l <= Bits/len(b[..ts, ..us])), inside: Holds(s + l <= Bits/len(ts))",
        held: &[
            // The `List` rows, at the bit grain: one grain's arm is not evidence for the other's.
            "Eq()(Bits/get(b[..ts, ..us], i, @joined), Bits/get(ts, i, @left))",
            "Eq()(Bits/slice(b[..ts, ..us], s, l, @spans), Bits/slice(ts, s, l, @inside))",
        ],
        refused: &[
            // The `List` control, at the bit grain.
            "Eq()(Bits/get(b[..ts, ..us], i, @joined), Bits/get(us, i, @right))",
        ],
    },
    Carrier {
        name: "Bits against Bytes",
        binders: "bs: Bytes, ts: Bits, a: Holds(Nat/eql(Nat/rem(Bits/len(ts), 8), 0))",
        held: &[
            // Regrouping moves no bit, so each reinterpretation reduces back through the other, in both orders.
            "Eq()(Bits/to_bytes(Bytes/to_bits(bs)), bs)",
            "Eq()(Bytes/to_bits(Bits/to_bytes(ts, @a)), ts)",
            // A reinterpretation's length is a shift of its operand's. A left shift by a literal count is a coefficient the Euclidean split reads, which is what discharges the byte-first trip's alignment bound above with no proof beside it; a right shift joins no division, so the other length stays one.
            "Eq()(Bits/len(Bytes/to_bits(bs)), 8 * Bytes/len(bs))",
            "Eq()(Bytes/len(Bits/to_bytes(ts, @a)), Nat/shr(Bits/len(ts), 3))",
        ],
        refused: &[],
    },
    Carrier {
        name: "Flt against Bytes",
        binders: "f: Flt",
        held: &[
            // Decoding what `to_le_bytes` wrote is the float it was given. The decoding owes an eight-byte bound, discharged by the length the second row states; the law table's rows state each direction under a bound handed in.
            "Eq()(Flt/of_le_bytes(Flt/to_le_bytes(f)), f)",
            "Eq()(Bytes/len(Flt/to_le_bytes(f)), 8)",
        ],
        refused: &[],
    },
    Carrier {
        name: "Flt, a fused multiply-add",
        binders: "a: Flt, b: Flt, c: Flt",
        // True of every bit pattern under the model, NaNs included: the NaN rule reads no operand's position, so the two factors stand in either order, in every rounding direction. Decided by pairing the factors in the judgment rather than by sorting them in a fold, which would respell a term a guard's refinement is keyed on. The sums, the products, `min` and `max` are the law table's rows.
        held: &[
            "Eq()(Flt/rounded/fma(Flt/Rounding/ties_to_even(), a, b, c), Flt/rounded/fma(Flt/Rounding/ties_to_even(), b, a, c))",
            "Eq()(Flt/rounded/fma(Flt/Rounding/ties_to_away(), a, b, c), Flt/rounded/fma(Flt/Rounding/ties_to_away(), b, a, c))",
            "Eq()(Flt/rounded/fma(Flt/Rounding/toward_zero(), a, b, c), Flt/rounded/fma(Flt/Rounding/toward_zero(), b, a, c))",
            "Eq()(Flt/rounded/fma(Flt/Rounding/toward_positive(), a, b, c), Flt/rounded/fma(Flt/Rounding/toward_positive(), b, a, c))",
            "Eq()(Flt/rounded/fma(Flt/Rounding/toward_negative(), a, b, c), Flt/rounded/fma(Flt/Rounding/toward_negative(), b, a, c))",
        ],
        refused: &[
            // Candidates no stage moves: `min` and `max` are associative on every pattern, the greatest quieted NaN being the one either association meets, and a law over a multiset of leaves nothing consumes.
            "Eq()(Flt/min(Flt/min(a, b), c), Flt/min(a, Flt/min(b, c)))",
            "Eq()(Flt/max(Flt/max(a, b), c), Flt/max(a, Flt/max(b, c)))",
        ],
    },
    Carrier {
        name: "Flt, sign operations and the difference",
        binders: "a: Flt, b: Flt, c: Flt",
        held: &[
            // The model defines a difference as the sum with its subtrahend negated, so a stuck one is that sum, in every direction.
            "Eq()(Flt/rounded/sub(Flt/Rounding/ties_to_even(), a, b), Flt/rounded/add(Flt/Rounding/ties_to_even(), a, Flt/neg(b)))",
            "Eq()(Flt/rounded/sub(Flt/Rounding/ties_to_away(), a, b), Flt/rounded/add(Flt/Rounding/ties_to_away(), a, Flt/neg(b)))",
            "Eq()(Flt/rounded/sub(Flt/Rounding/toward_zero(), a, b), Flt/rounded/add(Flt/Rounding/toward_zero(), a, Flt/neg(b)))",
            "Eq()(Flt/rounded/sub(Flt/Rounding/toward_positive(), a, b), Flt/rounded/add(Flt/Rounding/toward_positive(), a, Flt/neg(b)))",
            "Eq()(Flt/rounded/sub(Flt/Rounding/toward_negative(), a, b), Flt/rounded/add(Flt/Rounding/toward_negative(), a, Flt/neg(b)))",
            // The sign operations move one bit and read no other, so each law holds of every pattern, a NaN's included.
            "Eq()(Flt/neg(Flt/neg(a)), a)",
            "Eq()(Flt/abs(Flt/abs(a)), Flt/abs(a))",
            "Eq()(Flt/abs(Flt/neg(a)), Flt/abs(a))",
            "Eq()(Flt/copysign(Flt/copysign(a, b), c), Flt/copysign(a, c))",
            "Eq()(Flt/neg(Flt/copysign(a, b)), Flt/copysign(a, Flt/neg(b)))",
            // Every composition of the three is written one way: a magnitude under a sign bit that is clear, set, another value's, or that flipped. A literal is its absolute value under the bit it has.
            "Eq()(Flt/abs(Flt/copysign(a, b)), Flt/abs(a))",
            "Eq()(Flt/copysign(a, a), a)",
            "Eq()(Flt/copysign(Flt/abs(a), b), Flt/copysign(a, b))",
            "Eq()(Flt/copysign(Flt/neg(a), b), Flt/copysign(a, b))",
            "Eq()(Flt/copysign(a, Flt/abs(b)), Flt/abs(a))",
            "Eq()(Flt/copysign(a, Flt/copysign(b, c)), Flt/copysign(a, c))",
            "Eq()(Flt/copysign(a, 1.0), Flt/abs(a))",
            "Eq()(Flt/copysign(-2.0, b), Flt/copysign(2.0, b))",
            "Eq()(Flt/neg(Flt/abs(Flt/neg(a))), Flt/neg(Flt/abs(a)))",
            "Eq()(Flt/copysign(Flt/neg(a), a), a)",
            "Eq()(Flt/copysign(a, Flt/neg(a)), Flt/neg(a))",
            "Eq()(Flt/neg(Flt/copysign(a, Flt/neg(b))), Flt/copysign(a, b))",
            "Eq()(Flt/copysign(a, -0.0), Flt/neg(Flt/abs(a)))",
            // And a difference of a negation is the sum.
            "Eq()(a - Flt/neg(b), a + b)",
        ],
        refused: &[
            // Controls: a sign is not read through an operation that rounds. `0.0 - 0.0` is `+0.0` where the negation is `-0.0`, and `1.0 + -1.0` is `+0.0` in either order where its negation is `-0.0`.
            "Eq()(Flt/neg(a), 0.0 - a)",
            "Eq()(Flt/neg(a + b), Flt/neg(a) + Flt/neg(b))",
            // Of two NaNs a product answers the greater pattern, read with its sign, so clearing the signs first can change which payload it carries.
            "Eq()(Flt/abs(a * b), Flt/abs(a) * Flt/abs(b))",
        ],
    },
    Carrier {
        name: "Flt, controls",
        binders: "a: Flt, b: Flt, c: Flt",
        held: &[],
        // Controls, none a law, each beside its counterexample: a rule that took one would be unsound.
        refused: &[
            // Rounding.
            "Eq()((a + b) + c, a + (b + c))",
            "Eq()((a * b) * c, a * (b * c))",
            "Eq()(a * (b + c), a * b + a * c)",
            // `-0.0 + 0.0` is `+0.0`.
            "Eq()(a + 0.0, a)",
            // A signaling NaN is quieted.
            "Eq()(a * 1.0, a)",
            // An infinity or a NaN.
            "Eq()(a - a, 0.0)",
            // An infinity, a NaN, or a negative `a`'s sign.
            "Eq()(a * 0.0, 0.0)",
            // A NaN.
            "Eq()(a == a, true)",
            // A NaN on either side.
            "Eq()(a < b, Bool/not(a >= b))",
            // A signaling NaN is quieted, so `min` is not idempotent.
            "Eq()(Flt/min(a, a), a)",
            // Two directions are two operations: a sum rounded toward zero is not the swapped sum rounded up.
            "Eq()(Flt/rounded/add(Flt/Rounding/toward_zero(), a, b), Flt/rounded/add(Flt/Rounding/toward_positive(), b, a))",
            // The addend is no factor.
            "Eq()(Flt/rounded/fma(Flt/Rounding/ties_to_even(), a, b, c), Flt/rounded/fma(Flt/Rounding/ties_to_even(), a, c, b))",
            // A sign operation is not the identity, and `copysign` reads its sign from its second operand alone.
            "Eq()(Flt/neg(a), a)",
            "Eq()(Flt/abs(Flt/neg(a)), a)",
            "Eq()(Flt/copysign(a, b), Flt/copysign(b, a))",
        ],
    },
    Carrier {
        name: "Char, over Nat",
        binders: "c: Char, d: Char",
        held: &[
            // An order on characters is the order on their code points once unfolded, so it reduces as one: reflexive cases, the mirrored comparison, and the dual of a negation.
            "Eq()(Char/lt(c, c), false)",
            "Eq()(Char/le(c, c), true)",
            "Eq()(Char/eql(c, d), Char/eql(d, c))",
            "Eq()(Char/lt(c, d), Char/gt(d, c))",
            "Eq()(Char/le(c, d), Char/ge(d, c))",
            "Eq()(Bool/not(Char/lt(c, d)), Char/ge(c, d))",
            "Eq()(Bool/not(Char/le(c, d)), Char/gt(c, d))",
            "Eq()(Char/lt(c, d) && Char/ge(c, d), false)",
        ],
        refused: &[],
    },
    Carrier {
        name: "Str, over Bytes",
        binders: "s: Str",
        held: &[
            "Eq()(Str/concat(s, \"\"), s)",
            "Eq()(Str/concat(\"\", s), s)",
            "Eq()(Str/len(\"\"), 0)",
            "Eq()(s == s, true)",
            // The bytes decide a suffix as they decide a prefix: a string with a character appended is not the empty one.
            "Eq()(Str/concat(s, \"a\") == \"\", false)",
            "Eq()(Str/concat(\"a\", s) == \"\", false)",
        ],
        refused: &[],
    },
];

impl Carrier {
    /// This carrier's rows, each under its binders: the held ones first, then the refused.
    fn rows(&self) -> Vec<(String, String)> {
        self.held
            .iter()
            .chain(self.refused)
            .map(|claim| (self.binders.to_owned(), (*claim).to_owned()))
            .collect()
    }
}

#[test]
fn every_held_law_closes_by_refl() {
    // The admitting direction, stated where both checkers see it: one program per carrier, every held row an `Eq/refl()` proof.
    for carrier in CARRIERS {
        if carrier.held.is_empty() {
            continue;
        }
        let held = &carrier.rows()[..carrier.held.len()];
        if let Err(failures) = closes(IMPORTS, held) {
            panic!(
                "a held law of {} no longer closes:\n{}",
                carrier.name,
                failures.join("\n")
            );
        }
    }
}

#[test]
fn every_row_is_on_the_side_the_compiler_puts_it() {
    // Every row — held, then refused, then the sentinel — as a written goal, read back through the compiler's own refl-fit line. A held row without the line is a regression in the normalizer; a refused row with it is a law that has been taken and must move to the held rows, which is how the refused half stays a record. Every misplaced row is reported at once, since a change to one rule can move several.
    let found = CARRIERS
        .iter()
        .flat_map(|carrier| misplaced(IMPORTS, carrier.name, &carrier.rows(), carrier.held.len()))
        .collect::<Vec<_>>();
    assert!(found.is_empty(), "{}", found.join("\n"));
}

#[test]
fn every_row_is_on_the_side_the_kernel_puts_it_asked_alone() {
    // The same rows put to the kernel with the elaborator's verdict left out: a held row it refuses is the reader's fault, and a refused row it holds is an equation the trusted checker believes and the elaborator has been keeping from every program. Mutation-checked: a held law planted among a carrier's refused rows is named.
    let found = CARRIERS
        .iter()
        .flat_map(|carrier| {
            misplaced_by_the_kernel(IMPORTS, carrier.name, &carrier.rows(), carrier.held.len())
        })
        .collect::<Vec<_>>();
    assert!(found.is_empty(), "{}", found.join("\n"));
}

#[test]
fn every_atom_law_closes_at_every_order_of_its_binders() {
    // The atom rows, apart from the carriers above because their binders are not one telescope but a sweep of them: one program per order, every row an `Eq/refl()` proof both checkers must accept, and at the first order every row read back as a goal as well.
    for (order, binders) in orders(ATOM_BINDERS, ATOM_ORDERS).into_iter().enumerate() {
        let rows = ATOMS_HELD
            .iter()
            .map(|claim| (binders.clone(), (*claim).to_owned()))
            .collect::<Vec<_>>();
        if let Err(failures) = closes(IMPORTS, &rows) {
            panic!(
                "an atom law no longer closes under `{binders}`:\n{}",
                failures.join("\n")
            );
        }
        if order == 0 {
            let found = misplaced(IMPORTS, "Atoms up to conversion", &rows, rows.len());
            assert!(found.is_empty(), "{}", found.join("\n"));
        }
    }
}

/// The five rounding directions, as `/std/Flt/Rounding` spells them.
const DIRECTIONS: [&str; 5] = [
    "ties_to_even",
    "ties_to_away",
    "toward_zero",
    "toward_positive",
    "toward_negative",
];

#[test]
fn a_rounding_to_an_integral_value_is_fixed_by_every_direction() {
    // At every pair of directions: an integral value is one every direction leaves where it is, so the outer rounding reads nothing of which direction the inner one took, and a rounding of a stuck rounding is the inner one.
    let name = "Flt, a rounding of a rounding";
    let rounded = |direction: &str, operand: &str| {
        format!("Flt/rounded/to_integral(Flt/Rounding/{direction}(), {operand})")
    };
    let rows = DIRECTIONS
        .iter()
        .flat_map(|outer| {
            DIRECTIONS.iter().map(move |inner| {
                let once = rounded(inner, "a");
                (
                    "a: Flt".to_owned(),
                    format!("Eq()({}, {once})", rounded(outer, &once)),
                )
            })
        })
        .collect::<Vec<_>>();
    if let Err(failures) = closes(IMPORTS, &rows) {
        panic!("{}", failures.join("\n"));
    }
    let mut found = misplaced(IMPORTS, name, &rows, rows.len());
    found.extend(misplaced_by_the_kernel(IMPORTS, name, &rows, rows.len()));
    assert!(found.is_empty(), "{}", found.join("\n"));
}

/// What the rows over a recursive function are stated under: one whose body multiplies two of its own calls, so two calls that differ unfold to products nothing pairs, whose atoms are the same two calls one step on.
const RECURSIVE: &str = "use /std/{Nat, Eq, Io};\nlet twice(c: Nat, n: Nat) -> Nat = match n == 0 | true => c | false => twice(c, n / 2) * twice(c, n / 2) end;";

#[test]
fn two_calls_of_a_recursive_function_that_differ_are_refused() {
    // Each row compares two calls that differ in an argument, and the last two are each a candidate for the other's goal, which compares them through an implicit. A classing asked for inside a classing leaves apart what stands inside neither atom being compared, so no comparison follows the function down its own unfolding, in either checker.
    let name = "two calls of a recursive function";
    let rows = [
        "Eq()(twice(x, y), twice(2, y))",
        "Eq()(twice(x, y) * x, x * twice(2, y))",
        "Eq()(Nat/shl(x, y), x * twice(2, y))",
        "Eq()(twice(x, y + 1), twice(x, y) * x)",
    ]
    .map(|claim| ("x: Nat, y: Nat".to_owned(), claim.to_owned()));
    let mut found = misplaced(RECURSIVE, name, &rows, 0);
    found.extend(misplaced_by_the_kernel(RECURSIVE, name, &rows, 0));
    assert!(found.is_empty(), "{}", found.join("\n"));

    // The same comparison where the second call's argument is an implicit to solve.
    let solved = format!(
        "{RECURSIVE}\nlet solved(x: Nat, y: Nat, lemma: (@b: Nat, n: Nat, e: Eq()(n, twice(2, b))) -> Nat) -> Nat = lemma(twice(x, y), Eq/refl());\nIo/pure(())"
    );
    assert!(typecheck(&solved).is_err());
}

#[test]
fn a_step_taken_through_an_annotation_is_one_the_kernel_takes_in_one() {
    // The elaborator compares a value's type with a `let`'s annotation and the annotation with the result type, each pair leaving one unpaired factor; the kernel compares the first type with the last, which leaves two. Both accept at every order only where conversion is transitive over them.
    let binders = [
        Binder {
            name: "a",
            type_: "Nat",
            needs: &[],
        },
        Binder {
            name: "b",
            type_: "Nat",
            needs: &[],
        },
        Binder {
            name: "w",
            type_: "(n: Nat, at: Holds(n < 10)) -> Nat",
            needs: &[],
        },
        Binder {
            name: "p1",
            type_: "Holds(a < 10)",
            needs: &["a"],
        },
        Binder {
            name: "p2",
            type_: "Holds(a < 10)",
            needs: &["a"],
        },
        Binder {
            name: "q1",
            type_: "Holds(b < 10)",
            needs: &["b"],
        },
        Binder {
            name: "q2",
            type_: "Holds(b < 10)",
            needs: &["b"],
        },
        Binder {
            name: "v",
            type_: "Vec(Nat, w(a, p1) * w(b, q1))",
            needs: &["a", "b", "w", "p1", "q1"],
        },
    ];
    for binders in orders(&binders, ATOM_ORDERS) {
        let source = format!(
            "use /std/{{Nat, Vec, Io}}; use /std/Bool/{{Holds}};\npub let stepped({binders}) -> Vec(Nat, w(b, q2) * w(a, p2)) =\n    let through: Vec(Nat, w(a, p2) * w(b, q1)) = v;\n    through;\nIo/pure(())"
        );
        if let Err(error) = typecheck(&source) {
            panic!("under `{binders}`: {error}");
        }
    }
}

/// An item whose body calls a function with two implicit arguments standing under a commutative operation, at `a` declared before `b` and after it.
fn implicit_under(operation: &str, result: &str, body: &str) -> Vec<String> {
    ["a: Nat, b: Nat", "b: Nat, a: Nat"]
        .iter()
        .map(|declared| {
            format!(
                "use /std/{{Nat, Vec, Io}};\npub let item({declared}, g: (@x: Nat, @y: Nat, v: Vec(Nat, x {operation} y)) -> Vec(Nat, x), w: Vec(Nat, a {operation} b)) -> {result} =\n    {body};\nIo/pure(())"
            )
        })
        .collect()
}

#[test]
fn an_implicit_with_two_solutions_is_refused_and_never_picked() {
    // `?x ⋆ ?y` against `a ⋆ b` has two solutions where `⋆` commutes, and the order a term holds its operands in is no reason to commit one: under a product it is a hash, which moves with the order `a` and `b` were declared in, so a pick would make `g(w)` a `Vec(Nat, a)` under one telescope and a `Vec(Nat, b)` under the other. The call is refused under both, naming the conversion it could not decide.
    for operation in ["*", "+"] {
        // No expected type reaches the call, so nothing but the argument says what `x` is.
        for source in implicit_under(operation, "{}", "let _ = g(w); ()") {
            let error = typecheck(&source).expect_err("an implicit with two solutions is picked");
            assert!(
                error.contains("cannot decide a postponed conversion"),
                "refused, but not as a conversion left undecided:\n{error}"
            );
        }
    }
}

#[test]
fn an_implicit_with_one_solution_is_solved_through_a_commutative_operation() {
    // With `x` written, one operand of each side pairs by identity and the other two are what is left: `?y` against `b`, whichever way the operands stand.
    for operation in ["*", "+"] {
        for source in implicit_under(operation, "Vec(Nat, a)", "g(@a, w)") {
            if let Err(error) = typecheck(&source) {
                panic!("{source}\n{error}");
            }
        }
    }
}

/// A function whose one implicit stands under `pattern`, applied to a vector of `actual` elements where nothing but that equation says what the implicit is: no type is expected of the call, and the item's own result does not mention it. `body` is what follows the call's binding to `r`.
fn linear_in(pattern: &str, actual: &str, body: &str) -> String {
    format!(
        "use /std/{{Nat, Vec, Io}};\npub let item(x: Nat, y: Nat, z: Nat, g: (@w: Nat, v: Vec(Nat, {pattern})) -> Vec(Nat, w), v: Vec(Nat, {actual})) -> {{}} =\n    let r = g(v);\n    {body};\nIo/pure(())"
    )
}

#[test]
fn a_metavariable_an_equation_is_linear_in_is_solved_however_the_equation_is_written() {
    // One unknown, standing as a factor: the equation is `A * w + B = 0` over its atoms and has the one solution `-B / A`, which is the same term whichever way either side's summands stand, and need not be an atom. No operand is paired by position to find it, and nothing else in the item says what `w` is, so an item that elaborates has had it solved through the equation — to the term a goal over the result then reports.
    for (pattern, actual, solution) in [
        ("w * (y + z)", "x * y + x * z", "x"),
        ("w * (y + z)", "x * z + x * y", "x"),
        ("w * (y + z)", "(y + z) * x", "x"),
        ("w * (y + z)", "(x + 1) * y + (x + 1) * z", "x + 1"),
        ("w * (y + z) + z", "x * y + x * z + z", "x"),
        ("w * (x + y + z)", "y * z + y * y + y * x", "y"),
        ("w * y + w", "x + x * y", "x"),
        ("w * y + w", "2 * y + 2", "2"),
        ("2 * w", "x + x", "x"),
    ] {
        let source = linear_in(pattern, actual, "()");
        if let Err(error) = typecheck(&source) {
            panic!("{source}\n{error}");
        }
        let asked = linear_in(pattern, actual, "let t: ? = r; ()");
        let report = typecheck(&asked).expect_err("a program holding a goal never compiles");
        assert!(
            report.contains(&format!("r : Vec(Nat, {solution})")),
            "{asked}\nsolved, but not to `{solution}`:\n{report}"
        );
    }
}

#[test]
fn a_metavariable_no_division_solves_is_left_undecided() {
    // The controls, each refused as a conversion that cannot be decided and none as a mismatch, since nothing was compared: a division that leaves a remainder, a coefficient that divides nothing, and an unknown at a power, which is not linear though `x` solves it.
    for (pattern, actual) in [
        ("w * (y + z)", "x * y + z"),
        ("2 * w + 1", "2 * x"),
        ("w * w + w * y", "x * x + x * y"),
    ] {
        let source = linear_in(pattern, actual, "()");
        let error = typecheck(&source).expect_err("an equation this does not solve is accepted");
        assert!(
            error.contains("cannot decide a postponed conversion"),
            "{source}\nrefused, but not as a conversion left undecided:\n{error}"
        );
    }
}

#[test]
fn a_mismatch_nothing_compared_says_so() {
    let refusal = |binders: &str, claim: &str| {
        typecheck(&format!(
            "{IMPORTS}\nlet law({binders}) -> {claim} = Eq/refl();\nIo/pure(())"
        ))
        .expect_err("the claim is no law conversion decides")
    };

    // Past the truth table's cap. Each of two contradictions over five atoms converts with `false`, and the two are not compared with each other: their pair holds ten atoms, and the table reads eight.
    let atoms = (0..10)
        .map(|index| format!("a{index}: Bool"))
        .collect::<Vec<_>>()
        .join(", ");
    let contradiction = |from: usize| {
        let names = (from..from + 5)
            .map(|index| format!("a{index}"))
            .collect::<Vec<_>>();
        let negated = names
            .iter()
            .map(|name| format!("Bool/not({name})"))
            .collect::<Vec<_>>();
        format!("(({}) && {})", names.join(" || "), negated.join(" && "))
    };
    let past_cap = refusal(
        &atoms,
        &format!("Eq()({}, {})", contradiction(0), contradiction(5)),
    );
    assert!(
        past_cap.contains("type mismatch") && past_cap.contains("not compared: two Boolean terms"),
        "{past_cap}"
    );

    // Operands nothing pairs: no factor of one product is a factor of the other.
    let unpaired = refusal(
        "a: Nat, b: Nat, f: (Nat) -> Nat, g: (Nat) -> Nat",
        "Eq()(f(a) * g(b), g(a) * f(b))",
    );
    assert!(
        unpaired.contains("type mismatch")
            && unpaired.contains("not compared: no operand of one side"),
        "{unpaired}"
    );

    // The control: two terms that were compared and differ are a mismatch and nothing more.
    let compared = refusal("a: Nat, b: Nat", "Eq()(a, b)");
    assert!(
        compared.contains("type mismatch") && !compared.contains("not compared"),
        "{compared}"
    );
}

// A promoted operation computes from its operands alone and reads no arm. A `min`: under a guard on `a <= b`, and under a hypothesis of it, `Nat/min(a, b)` is the node it was, so `Eq/refl()` does not close it against `a`, and `Nat/min/of_le` over the fact does. So one `min` is one value in every arm it is read in, and commutation and association hold the same under a guard as outside it. The same at `Int`, and for a `max`.
#[test]
fn a_promoted_operation_under_a_guard_reads_no_arm_and_a_lemma_converts_it() {
    let header = "use /std/{Nat, Int, Eq, Io}; use /std/Bool/{True, Holds};";
    let under = |carrier: &str, operation: &str, kept: &str, proof: &str| {
        format!(
            "{header}\npub let row(a: {carrier}, b: {carrier}) -> Nat = match a <= b | true => let _: Eq()({carrier}/{operation}(a, b), {kept}) = {proof}; 0 | false => 1 end;\nIo/pure(())"
        )
    };
    for (carrier, operation, kept, lemma) in [
        ("Nat", "min", "a", "Nat/min/of_le(a, b, True/proved())"),
        ("Nat", "max", "b", "Nat/max/of_le(a, b, True/proved())"),
        ("Int", "min", "a", "Int/min/of_le(a, b, True/proved())"),
        ("Int", "max", "b", "Int/max/of_le(a, b, True/proved())"),
    ] {
        let refused = typecheck(&under(carrier, operation, kept, "Eq/refl()"))
            .expect_err("a `min` or a `max` that read its arm");
        assert!(refused.contains("type mismatch"), "{refused}");
        if let Err(error) = typecheck(&under(carrier, operation, kept, lemma)) {
            panic!("{carrier}/{operation}/of_le does not convert it under the guard:\n{error}");
        }
    }

    // An `abs` widened back and a `sign` read no arm either: under a guard on `+0 <= a` each is the node it was, and `Int/abs/of_non_neg` and `Int/sign/of_not_neg` over the arm's fact are the step.
    let widened = |proof: &str| {
        format!(
            "{header}\npub let row(a: Int) -> Nat = match +0 <= a | true => let _: Eq()(Nat/to_int(Int/abs(a)), a) = {proof}; 0 | false => 1 end;\nIo/pure(())"
        )
    };
    assert!(typecheck(&widened("Eq/refl()")).is_err());
    if let Err(error) = typecheck(&widened("Int/abs/of_non_neg(a, True/proved())")) {
        panic!("Int/abs/of_non_neg does not convert it under the guard:\n{error}");
    }
    let signed = |proof: &str| {
        format!(
            "{header}\npub let row(a: Int) -> Nat = match +0 < a | true => let _: Eq()(Int/sign(a), +1) = {proof}; 0 | false => 1 end;\nIo/pure(())"
        )
    };
    assert!(typecheck(&signed("Eq/refl()")).is_err());
    if let Err(error) = typecheck(&signed("Int/sign/of_pos(a, True/proved())")) {
        panic!("Int/sign/of_pos does not convert it under the guard:\n{error}");
    }

    // Under a hypothesis as under a guard, and commuted there as anywhere.
    let stated = format!(
        "{header}\npub let row(a: Nat, b: Nat, p: Holds(a <= b)) -> Eq()(Nat/min(a, b), a) = Eq/refl();\nIo/pure(())"
    );
    assert!(typecheck(&stated).is_err());
    let commuted = format!(
        "{header}\npub let row(a: Nat, b: Nat, p: Holds(a <= b)) -> Eq()(Nat/min(a, b), Nat/min(b, a)) = Eq/refl();\nIo/pure(())"
    );
    if let Err(error) = typecheck(&commuted) {
        panic!("a `min` does not commute under a hypothesis:\n{error}");
    }
}
