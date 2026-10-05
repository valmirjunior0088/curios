# curios-algebra

The carriers' algebra over abstract atoms: what conversion decides about the carriers — the arithmetic of `Nat` and `Int`, the Boolean laws, the free monoid a sequence is, the round trips between carriers, and the strength of what a comparison concludes — stated without the terms it is decided over. Both checkers reach it through `curios-core`, which decides which terms are one atom and rebuilds results from the terms it was given, and through `curios-analysis`'s conversion chain; the elaborator's packed-literal view and its procedure proving a bound from the facts in scope read its forms directly. Which crate owns which part of the algebra, and why its laws are decided in conversion, is [The carriers' algebra stays in conversion](../documentation/design/arithmetic/the-carriers-algebra-stays-in-conversion.md); the algorithms and their contracts belong to the crate rustdoc.

## Design

### The mathematics has one owner, and it names no term

**Decision.** This crate depends directly on `curios-num` alone, and its source names no `Term`, `Intrinsic`, elaborator context or kernel type. It reasons over an `Atom`, a handle its caller hands out, and a summand carries an origin of the caller's choosing, handed back untouched so the caller rebuilds a result from the terms it came from.

**Rationale.** Over bare atoms a rule is stated once, tested against concrete values without a checker, and reached by every carrier from one implementation — where a rule interleaved with term reading can drift between carriers — `x + 1 <= y` meeting `x < y` at `Int` and not at `Nat`. An atom's identity is settled before its handle is handed over, so nothing here asks for conversion or reduction and the crate cannot acquire the rest of the compiler through callbacks.

**Rejected.** A crate that named `Term`, which could not be tested over independent atoms; a host trait exposing conversion and reduction, which would let the caller make a mathematical decision on this crate's behalf.

### A conclusion carries its strength, and inversion's type cannot carry sufficiency

**Decision.** A comparison concludes a `Deduction` — equal, impossible, an equivalent residual, or undecided — or a `Conclusion`, which adds a residual that is merely sufficient. Inversion is handed `Deduction`s only.

**Rationale.** `x · f = x · g` holds when `f = g` and, at `x = 0`, whatever `f` and `g` are, so conversion may check `f = g` to establish the equation and inversion may never deduce it. Enforced by leaving a rule out of inversion's entry, the restriction would be one nothing in the result says; a type with no sufficient variant makes it one a caller cannot forget.

### Atom identity is the caller's, and a collision never merges

**Decision.** Two handles are two atoms. What makes two terms one atom — up to universe instances for `Nat` and `Int` — is the caller's, stated once in `curios-core`'s `atoms` module.

**Rationale.** Identity is a fact about a carrier's terms, and the carriers disagree on it: numeric atoms project universe instances, while a Boolean leaf and a word chunk are compared as written. Keeping it out lets one algorithm serve both, and makes a hash collision in the caller's keys cost a probe rather than a false equation. A comparison here is transitive over handles, and over a caller's terms only where the caller hands one handle to every two terms that convert: `curios-analysis`'s chain has each checker class a pair's atoms by its own conversion before the pair is read ([A term is one where conversion says so](../documentation/design/arithmetic/a-term-is-one-where-conversion-says-so.md)).

### An operation's meaning is stated once, and its mapping is Core's

**Decision.** `Operation` names each operation this crate gives a meaning to — sum, truncated or group difference, product, the two halves of a division, the bitwise operations, the shifts, the comparisons and the conversions between carriers — and states its meaning once: which operands bound its result (`bound_reads`, `upper_bound`), which it never exceeds (`dominators`), which identities it satisfies (`bitwise_identity`, `shift_identity`), and which conversion undoes which (`undoes`, over `round_trip`). Which concrete operation is which is `curios-core`'s `Intrinsic::algebra`, an exhaustive match. An intrinsic is declared an operation only where an implemented rule reads it at its carrier.

**Rationale.** Stated per operation, the bound criterion — an operand is read where the result is not antitone in it — is a fact checked arm by arm without reading term plumbing, and an intrinsic joins a law by being declared an operation of that kind; declaring only what a rule reads keeps membership from enabling an identity nothing implements.

### A law is stated once per family, and declared per operation

**Decision.** `Family` names each kind of law conversion decides — unit and absorber at either position, idempotence, self-cancellation, commutativity, associativity, complement, nested cancellation, distribution, dual, successor seam, cancellation, the length homomorphism and the inverse pair — and `laws` states each family's laws once, as equations between `Expr`s over variables and a carrier's `Constant`s. `TABLE` declares, for each operation at each carrier, exactly the families conversion decides for it, with its constants: `And` at `Bool` has the unit `true` and the absorber `false`, and at ℕ the absorber `0` and no unit. `curios`'s `tests::laws::generated` spells every instance as Curios source and holds it through both checkers and at closed values.

**Rationale.** Stated at each carrier by hand under a convention, a law true at two carriers slips past the convention — the `<`/`<=` seam is the case. Declared once and instantiated, the row at every carrier is a consequence, and a family declared where no procedure decides it fails there instead of going unstated.

**Rejected.** Declaring a carrier a member of a structure — ℕ a commutative semiring, `Bool` a Boolean algebra — and deriving its laws, which enables every law of the structure where conversion decides some. `map`, `fold`, `get`, `slice` and `replicate` are in no family, since their laws take a function operand, read a binder or carry a bound, and stay rows written by hand.

### A comparison has one linear form, in one total order

**Decision.** `LinearForm` is a comparison read as the difference of its sides over ℤ: each monomial once, its atoms in rank order, the monomials lexicographically ordered, the constant separated, and the atoms known non-negative recorded beside it. Atoms of equal rank are ordered as their caller handed them out.

**Rationale.** A relation stated two ways — commuted, reassociated, a term moved across — is one form, so a reader outside the converters can ask what conversion decides of a comparison. The second key is reached only when ranks collide, where it orders two atoms and never merges them.

### A word's measures are its caller's

**Decision.** A `Word` is generic over an `Alphabet`, which supplies the arithmetic and identity of the word's numbers, a chunk's length, how a position is rooted through the windows it is read through, and where an operand begins inside a root. The free monoid's decisions are stated here: the normal form, window fusion, the prefix strip and its verdicts, and when two positions are one.

**Rationale.** A window's offset and length are `Nat`s whose normal form under addition includes Euclid's recombination, which reads which summand is the remainder of which division — terms this crate never sees — so summed here, windows that fuse would stop fusing. Read through the alphabet, a number is read only where it is compared. The alphabet converts and reduces nothing, so it is not the host trait rejected above.

**Rejected.** Offsets and lengths held here as `Combination`s, which still need the recombining sum and add an atom table per comparison.
