# curios-algebra

The carriers' algebra over abstract atoms: what conversion decides about `Nat` and `Int` — linear combinations, their cancellation, and the strength of what a comparison concludes — stated without the terms it is decided over. Both checkers reach it through `curios-core`, which decides which terms are one atom and rebuilds results from the terms it was given; the algorithms and their contracts belong to the crate rustdoc.

The consolidation that is moving the rest of the carriers' reasoning here — comparison facts, the Boolean laws and the truth table, words and positions — is [Algebra, part 1](../documentation/roadmap/algebra/01-one-owner-spec.md), whose baseline inventory records what has moved and what has not.

## Design

### The mathematics has one owner, and it names no term

**Decision.** This crate depends directly on `curios-num` alone. Its source names no `Term`, no `Intrinsic`, no elaborator context and no kernel type. What it reasons over is an `Atom` — a handle its caller hands out — and a summand carries an origin of its caller's choosing, handed back untouched so the caller rebuilds a result from the terms it came from.

**Rationale.** The mathematics had been interleaved with term reading, reduction demands and reconstruction in `curios-core`, so a rule's meaning was readable only from the folds that applied it, and each carrier's copy of a rule could drift from the other's — which is how `x + 1 <= y` came to meet `x < y` at `Int` and not at `Nat`. Over bare atoms a rule is stated once, tested against concrete values without a checker, and every carrier reaches the same implementation. A term never reaching this crate is also what keeps it from acquiring the rest of the compiler through callbacks: an atom's identity is settled before a handle is handed over, so nothing here asks for conversion or reduction.

**Rejected.** Moving the existing files here wholesale: their term representation, reduction and proof handling belong to Core and the drivers, and a crate that named `Term` could not be tested over independent atoms. A general host trait exposing conversion and reduction to this crate: it would let a mathematical decision be made by the caller on this crate's behalf, which is the ownership this crate exists to end.

### A conclusion carries its strength, and inversion's type cannot carry sufficiency

**Decision.** A comparison concludes a `Deduction` — equal, impossible, an equivalent residual, or undecided — or a `Conclusion`, which adds a residual that is merely sufficient. Inversion is handed `Deduction`s only.

**Rationale.** `x · f = x · g` holds when `f = g`, and at `x = 0` whatever `f` and `g` are, so conversion may check `f = g` to establish the equation and inversion may never deduce it. That restriction used to be enforced by registration — the product-factor peel was simply left out of inversion's entry — and nothing in the result said so. A type with no sufficient variant makes it a fact a caller cannot forget.

### Atom identity is the caller's, and a collision never merges

**Decision.** Two handles are two atoms. What makes two terms one atom — up to universe instances for `Nat` and `Int` — is decided by the caller, and stated once where it is decided (`curios-core`'s `atoms` module).

**Rationale.** Identity is a fact about a carrier's terms, not about arithmetic, and the carriers do not agree on it: numeric atoms project universe instances, while a Boolean leaf and a word chunk are compared as written. Keeping identity out of this crate is what lets one algorithm serve both, and what makes a hash collision in the caller's keys cost a probe rather than a false equation here.
