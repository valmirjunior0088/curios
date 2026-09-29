# curios-algebra

The carriers' algebra over abstract atoms: what conversion decides about the carriers — the arithmetic of `Nat` and `Int`, the Boolean laws, the free monoid a sequence is, the round trips between carriers, and the strength of what a comparison concludes — stated without the terms it is decided over. Both checkers reach it through `curios-core`, which decides which terms are one atom and rebuilds results from the terms it was given; the algorithms and their contracts belong to the crate rustdoc.

Which crate owns which part of the carriers' algebra — this one the mathematics, `curios-core` the terms, `curios-analysis` the chain both checkers run — is the cross-crate decision [One crate owns the carriers' algebra, and the checkers share its strategy](../documentation/design/toolchain/one-crate-owns-the-carriers-algebra-and-the-checkers-share-its-strategy.md), and why the laws are decided in conversion at all is [The carriers' algebra stays in conversion](../documentation/design/language/the-carriers-algebra-stays-in-conversion.md).

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

### An operation's meaning is stated once, and its mapping is Core's

**Decision.** `Operation` names each operation this crate gives a meaning to — sum, truncated or group difference, product, the two halves of a division, the bitwise operations, the shifts, the comparisons, and the conversions between carriers — and what that meaning says is stated here, once per operation: which operands bound its result (`bound_reads`, `upper_bound`), which it never exceeds (`dominators`), which identities it satisfies (`bitwise_identity`, `shift_identity`), and which conversion undoes which (`undoes`, over the `round_trip` table). Which concrete operation is which is `curios-core`'s `Intrinsic::algebra`, an exhaustive match.

**Rationale.** The bound criterion — an operand is read where the result is not antitone in it — used to live in one function's arms, beside the terms it read, so an arm could not be checked against the criterion without reading the term plumbing too. Stated per operation, each arm is a fact about the operation, and an intrinsic joins a law by being declared an operation of that kind rather than by gaining an arm. An intrinsic is declared only where some implemented rule reads it at its carrier — `Int`'s bitwise operations stay opaque, since no identity of theirs is implemented — so declaring membership never enables an identity nothing implements.

### A law is stated once per family, and declared per operation

**Decision.** `Family` names each kind of law conversion decides — unit and absorber at either position, idempotence, self-cancellation, commutativity, associativity, complement, nested cancellation, distribution, dual, successor seam, cancellation, the length homomorphism and the inverse pair — and `laws` states each family's laws once, as equations between `Expr`s over variables and a carrier's `Constant`s. `TABLE` declares, for each operation at each carrier, exactly the families conversion decides for it, with its constants: `And` at `Bool` has the unit `true` and the absorber `false`, and `And` at ℕ the absorber `0` and no unit. `curios`'s `tests::laws::generated` spells every instance as Curios source and holds it through both checkers and at closed values.

**Rationale.** A law true at two carriers used to be stated at each by hand, under a convention to state it at every carrier, and the `<`/`<=` seam slipped past the convention. Declared once and instantiated, the row at every carrier is a consequence, and a family declared where no procedure decides it fails at that carrier instead of going unstated — associativity of `and` at ℕ is the generator's own test of that.

**Rejected.** Declaring a carrier a member of a structure — ℕ a commutative semiring, `Bool` a Boolean algebra — and deriving its laws: membership would enable every law of the structure, and conversion decides only some of them, so the table names families per operation and declares only what a procedure implements. `map`, `fold`, `get`, `slice` and `replicate` are in no family: their laws take a function operand, read a binder or carry a bound that a family's abstract operands do not, so they stay rows written by hand.

### A comparison has one linear form, in one total order

**Decision.** `LinearForm` is a comparison read as the difference of its sides over ℤ: each monomial once, its atoms in rank order, the monomials ordered lexicographically, the constant separated, and the atoms known non-negative recorded beside it. Atoms of equal rank are ordered by the order their caller handed them out in.

**Rationale.** A relation stated two ways — commuted, reassociated, a term moved across — is one form, so a reader outside the converters can ask what conversion decides of a comparison without reading the folds. The second key is the one place a form can depend on how its sums were written, and it is reached only when ranks collide, where it orders two atoms and never merges them.

### A word's measures are its caller's

**Decision.** A `Word` is generic over an `Alphabet`, which supplies the arithmetic and identity of the word's numbers, a chunk's length, how a position is rooted through the windows it is read through, and where an operand begins inside a root. Everything the free monoid decides from those answers is stated here: the normal form, window fusion, the prefix strip and its verdicts, and when two positions are one.

**Rationale.** A window's offset and length are `Nat`s, and their normal form under addition includes Euclid's recombination, which reads which summand is the remainder of which division — terms this crate never sees. Summed here, two windows that fuse or meet today would stop. Read through the alphabet, a number is also read only where it is compared, which is what a word cost before it moved. The alphabet converts and reduces nothing, so the host trait rejected above stays rejected: its sum is the carrier's normal form, its identity is `Nat`'s cancellation under Core's atom identity, and the rest reads how a term is built.

**Rejected.** Offsets and lengths held here as `Combination`s: they would still need the recombining sum from the caller, and would add an atom table per comparison and read and project every window number up front.
