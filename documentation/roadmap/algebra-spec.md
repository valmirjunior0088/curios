# Intrinsic laws as theorems of declared structures

Working specification for how conversion decides equations over the intrinsic carriers. Today each law is code of its own — a fold arm, a peel, or a demand a converter asks for by name — and a law holds wherever those pieces happen to reach. Here each intrinsic operation declares the algebraic structure it belongs to, equality is computed in that structure's free algebra over canonical atoms, and a law is a theorem of the declaration rather than a rule beside it. Both checkers decide that theory with one reference implementation written to be checked, and the search it needs runs in the elaborator alone, its answers checked by the certifier. The same specification collapses `List`, `Bits` and `Bytes` into one sequence carrier inside the compiler, which the word algebra is built over, and adds an elaboration-side procedure that discharges a bound following linearly from the hypotheses in scope. [The `Nat` laws](nat-laws-spec.md), [the `Int` laws](int-laws-spec.md) and [the `Flt` laws](flt-laws-spec.md) depend on the stages below and name the stage each of their items waits on.

## What this builds on

- **The engine.** Intrinsic reduction and the algebra beside it live in `curios-core`: the sum normal form in `nat` and `int`, the free monoid in `free_monoid`, the peels in `spine`, and the folds, the laws beside them and the truth table under `reduce::intrinsic`. Each checker composes them in its own `convert::intrinsic`. What they decide, and the evidence for it, is [Open fold laws and the sum normal form](../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md) and [Intrinsic fold laws and the free-monoid peel](../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md).
- **The evidence.** `curios/src/tests/laws.rs` states each law conversion takes and each it declines as a row both checkers see, and `curios-core`'s `reduce::intrinsic::laws_tests` holds every fold and verdict to ground truth at closed instantiations.
- **The two checkers.** A rule both checkers run is written once in `curios-analysis`, behind `Env`, because a second copy of a pure function is a second run rather than a second opinion ([its README](../../curios-analysis/README.md)); the kernel rechecks what the elaborator emits.
- **`Bits` and `Bytes`.** One internal packed family keyed by `Grain` stands behind two guest types — the arrangement `List` joins below.
- **Coq Modulo Theory**, which put a decidable first-order theory into conversion over the terms of its signature and abstracted every other subterm as an opaque *alien*, with strong normalization and the Church–Rosser property established for strong elimination modulo the theory (Jouannaud and Strub, 2017). Curios's atoms are its aliens; what this specification adds is products, sequences, `Bool`, and a theory stated as declarations rather than as one fixed signature. Its metatheory asks four things of the theory beyond decidability — freeness, non-triviality, completeness on ground terms, and first-orderness over aliens — which *The theory's shape* below adopts.
- **The certifier**, which decides this theory with the reference implementation below and checks the search's certificates rather than running the search, as [the certifier specification](certifier-spec.md) states.

## The gap

**The normal form is an invariant on `Term`, not a type.** A `Nat` is a successor floor over `NatAdd` trees in first-appearance order, a product's factors sit in structural-hash order, and a stuck connective's right operand stays as written. The form is non-canonical in those three places on purpose, because a guard's refinement is keyed on a written spelling and because building a full normal form at every reduction was measured to be ruinous ([A sum is merged when it is forced, not when it is built](../design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md)). So two equal values often have different forms, and every identification the form misses needs code of its own.

**Where that code sits is decided by the representation.** [A law is decided where it neither respells nor invents](../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md) places a law in a fold, a peel or a probe-side decision by what it would do to the representation — respell a guard's key, invent a proof, or cost too much — rather than by anything in the mathematics. Conversion is then a first-answer-wins union: substitute the solved metavariables, normalize the stuck products, ask the truth table, flatten the connective trees, align the comparisons, run eight peels chained so that the first to answer ends the chain, retry with each summand's arguments forced, try the packed-literal view, and fall to the operand congruence. Its correctness depends on the peels' domains staying disjoint, which nothing enforces, and the chain is restated in each checker, where the law grid has already caught one divergence. The algebra is written twice more beside it: `Nat` against `Int` — `Nat::cancel_common` and `int_cancel_common`, `compare_nat` and `compare_int`, `Nat::normalize` and `int_normalize`, `peel_nat` and `peel_int_pair` — and `Bin` against `List` — `peel_bin` and `peel_list`, `peel_first_atom` and `peel_first_elem`.

**What it leaves undecided.** Each of these holds of every value and is refused by `Eq/refl()` today:

- comparisons combined by connectives: `x < y && y < x`, `x < y && y < z && z < x` and `x < y && y < x + 1` against `false`, `x + y == 0` against `x == 0 && y == 0`, and on `Int` totality, `a <= b || b <= a` against `true`;
- an equation with no solution in `ℕ` though the gcd of its coefficients divides its constant, `3 * x + 5 * y == 7` against `false`;
- truncated subtraction: `(3 - x) + x >= 3` and `(x - y) + y >= x`;
- a nonlinear fact: `x <= x * x`, and on `Int` `i * i >= +0` — though `Nat/to_int(x) * Nat/to_int(x) >= +0` holds, because it is decided through the preimage of a widened natural;
- the exponent law of the left shift, `shl(shl(x, m), k)` against `shl(x, m + k)`.

**A law holds on one carrier and not its twin.** `Bytes/len(Bytes/replicate(n, b))` converts with `n` while `List/len(List/replicate(n, b))` does not, since `List/replicate` is library recursion; `List/fold` over a cons or a concatenation reduces by its laws while `Bytes/fold`, a hand-written loop, reduces by neither. Each carrier's intrinsic set is a different part of one structure.

## Permanent decisions

**Structures, not laws.** For each carrier below, the equations its operations satisfy are exactly those of a classical variety, and the variety's free algebra over the atoms is therefore a complete normal form for them (Birkhoff): an equation holds of the carrier exactly when it holds in the free algebra.

| Carrier and operations | Variety | Normal form |
| --- | --- | --- |
| `Nat`: `+`, `*` | commutative semiring | polynomials with natural coefficients, read in ℤ[X] |
| `Int`: `+`, `-`, `*` | commutative ring | ℤ[X] |
| `List`, `Bits`, `Bytes`: concatenation | monoid | words over the atoms, literal runs fused |
| `Bool`: the connectives, `==`, `!=` | Boolean algebra | Boolean functions of the atoms |
| `Nat`: bitwise `and`, `or`, `xor` | Boolean ring without unit | algebraic normal form over GF(2), with no constant term |

Each carrier generates its variety — `ℕ` and `ℤ` are infinite, so a polynomial identity over them is an identity of polynomials; a word alphabet has at least two letters; the two-element Boolean algebra generates them all — so nothing true of the carrier is missing from its normal form. The laws are then not written: they are what the free algebra already satisfies. Comparisons are not operations of these varieties but predicates over them, and truncated subtraction, a quotient and a remainder are not operations of them either; they are the relational layer's.

Two declarations are sound without that completeness, and say so. `pow` brings exponentiation, whose identities over `ℕ` are not finitely axiomatizable — Wilkie exhibited one the school laws do not derive, and Gurevič proved that no finite set derives them all — so it declares the exponent laws, and a sum raised to a symbolic power stays an atom. `Flt`'s `add`, `mul`, `min` and `max` satisfy identities no classical variety describes whole, so they declare the commutativity [the `Flt` laws](flt-laws-spec.md) take. In both, what a declaration omits is declined, never decided wrongly.

**The theory's shape.** The congruence conversion decides is decidable, and it is *free* — two distinct constructors are never equal modulo the theory, and a constructor is injective modulo it, which is what inversion's clashes and pinnings rest on — *non-trivial*, *complete* on ground terms, each of which equals a literal, and *first-order* over opaque atoms. Those are the conditions Coq Modulo Theory's metatheory is established under, and a rule that meets them inherits its argument; a rule that does not is named and argued on its own, or moved out of the theory. Two are known: `map` by a function convertible to the identity looks under a binder, and `fold`'s equations are an eliminator's rather than the theory's.

**What stays out of conversion.** Five constraints survive the rework, because they are true of the calculus rather than of the representation:

- a hypothesis, and a bound's proof, never enters conversion: under an inconsistent context every linear literal is valid, which is how extensional type theory loses normalization and decidable checking, and the refinement store stays a lookup;
- the reducer never invents a proof: a window is compared through its root and absolute position, and rewritten only where the bound it needs is the one handed on;
- a disequality comes only from unsatisfiability for every value of the atoms, since an atom such as a stuck match is not free to take every value;
- a budget that runs out declines, and declining is the refusing direction;
- nonlinear order is incomplete — a polynomial identity is decidable, but polynomials combined with comparisons are Hilbert's tenth problem — so conversion reads nothing nonlinear past the sign facts a declaration states.

**Where each piece lives.**

| Piece | Home |
| --- | --- |
| The structures' canonical forms, the defined operations' definitions, the theory's freeness, and the certificate checker, generic over an atom type — the reference implementation both checkers decide with | `curios-algebra`, depending on `curios-num` alone and naming no `Term` |
| Search: solving linear integer arithmetic and emitting its certificates, and the solver behind unification | the elaborator — `curios-elab`, or a crate of its own if it grows — outside `curios-cert`'s dependency closure |
| The declaration table, purification into atoms, canonical forms, and the folds that read them | `curios-core` |
| The verdicts: intrinsic conversion, and inversion's disequalities | `curios-analysis`, behind `Env` |
| What genuinely differs between the checkers — substituting solved metavariables first, the packed-literal view while solving | each checker |
| Discharging a bound from the hypotheses in scope | elaboration |

The mathematics sits behind a crate boundary so that the compiler enforces its independence from the term language and its property tests stand on their own. The canonicalizer is in `curios-core` because reduction is and must call it; the verdicts are in `curios-analysis` because both checkers run them and neither may keep a copy. The search is outside the certifier's closure because the certifier checks its answers rather than running it, and everything the certifier does run is of the grade [the certifier specification](certifier-spec.md) defines.

**Canonical forms are computed on demand.** Terms keep the spelling they were built with, and the cheap merging the folds do today stays; a canonical form is computed where a comparison, a conversion or a refinement key needs one, and memoized per term. It is the only thing equality and keys read, so the spelled term and its canonical form cannot disagree about a verdict. Atoms are ordered by a total structural order, never by a hash, so no collision can cost canonicity. Canonical forms at construction would pay for every product and every connective tree at every reduction, which is the blow-up the sum decision measured.

**Search is the elaborator's, checking the certifier's.** Solving linear integer arithmetic is search, and it runs in the elaborator alone, emitting a certificate — a Farkas combination, with the case splits a defined operation calls for — for each validity it finds. The certificate travels with the module keyed by its canonical formula, and the certifier checks it with `curios-algebra`'s checker; missing evidence is a refusal, never an acceptance. The checker is the trusted part; the search can be tuned without widening the soundness perimeter.

**A solved form says what it may be used for.** The solver behind unification is the elaborator's search, and it answers either an *equivalent* solved form, whose solutions are exactly the original's, or a *sufficient* one, which is one solution among possibly others; unification may use either. Inversion, which both checkers run, never asks the solver: it deduces from first-order unification over the theory's canonical forms and its freeness, which give equivalent forms by construction. Today the same distinction is carried by which chain a peel is registered in.

**The declaration table is the source of truth.** `Intrinsic::algebra`, like `Intrinsic::signature`, states for each operation the structure it belongs to, its morphisms and its definition, and both checkers and the generated law grid read it. Adding an operation of an existing kind is a row of the table, not code in the engine.

**`Seq` names the sequence carrier.** The carrier families are named by a short type prefix — `NatAdd`, `IntMul`, `FltFma`, `BinLen` — and helpers type-first, so the collapsed carrier is `Seq`: `SeqLen`, `SeqCarrier`, `seq_slice`.

## The structures

`curios-algebra` provides each structure as a canonical form and the operations on it, generic over the atoms it is handed:

- **Polynomials over ℤ**, sparse. `Nat` is the sub-semiring with natural coefficients, its atoms carrying non-negativity. A monomial is a coefficient times atoms each raised to an exponent, and an exponent is itself a polynomial with natural coefficients, so that `pow` at a symbolic exponent has a canonical form; a sum raised to a symbolic exponent is an atom. Literal bases under a symbolic exponent are rewritten over a coprime basis the two sides of a comparison share, computed with gcds alone and never by factoring, while a key removes only perfect powers, so `pow(6, k)` and `pow(2, k) * pow(3, k)` convert although their keys may differ. Sparse Horner form is measured against the monomial form before one is chosen.
- **Words** over atoms, each run either packed at a `Grain` or a vector of terms. The kind of run matters: two packed heads that differ clash, while two term heads are compared, since `[a + b]` and `[b + a]` are one list.
- **Boolean forms**: a reduced ordered binary decision diagram over canonical literals, in the total order of the atoms.
- **Algebraic normal form** over GF(2) without a constant term, for the bitwise operations on `Nat`.
- **Ordered-operand trees** for a commutative magma, whose consumer is `Flt`.
- **Linear integer arithmetic.** A comparison is a canonical literal: `p = 0` or `p ≥ 0` for `p` in ℤ[X], its coefficients divided by their gcd and its constant tightened as the omega test normalizes, and an equality's sign fixed. Validity of a formula over such literals is found by the elaborator's search, with the case splits a defined operation's definition calls for, and each valid verdict carries a certificate the checker here verifies.

## Morphisms and defined operations

The declaration table relates structures through a fixed set of kinds:

- a **homomorphism** between the monoid parts of two structures: `len` from words to `(ℕ, +)`, `map` from words to words, and `fold` as the catamorphism;
- **`pow`**, promoted from `/std/Nat` to `/sys`: a homomorphism from `(ℕ, +)` to `(ℕ, ·)` at every base and from `(ℕ, ·)` to `(ℕ, ·)` at every exponent, with `pow(pow(b, e₁), e₂) = pow(b, e₁ · e₂)`, `pow(b, 0) = 1` and `pow(b, 1) = b`, all of which hold on all of `ℕ` with `pow(0, 0) = 1`. A literal exponent expands into a ring power, so `pow(x + 1, 2)` is `x * x + 2 * x + 1`. The left shift is a derived operation over it, `shl(v, k) = v * pow(2, k)`, so `shl(1, k)` and `pow(2, k)` are one term;
- an **embedding**: `Nat/to_int`, the ordered-semiring embedding of ℕ[X] in ℤ[X], whose image the solver reads back;
- a **retraction** with its domain: `Nat/to_byte` against `Byte/to_nat`, `Int/to_nat` against `Nat/to_int`, and the regrouping between `Bits` and `Bytes`;
- an **isomorphism**, a **projection** and a **derived operation**, whose consumer is `Flt`;
- an operation with a **Presburger definition**: truncated subtraction, the quotient and the remainder by a literal, and the operations [the `Nat` laws](nat-laws-spec.md) and [the `Int` laws](int-laws-spec.md) promote.

A morphism is pushed through a normal form in one generic way, the image of a sum being the sum of the images of its generators. A defined operation stays an atom in its structure and contributes its definition to the relational layer; a symbolic `pow(b, e)` is an atom there too, with the sign facts its declaration states, such as `pow(b, e) ≥ 1` where `b ≥ 1`. Each kind is exercised by the declarations that replace today's engine, and a later operation of an existing kind is a row, which is what lets the laws specifications add theirs without touching the engine.

## The verdicts

- **Conversion** compares canonical forms. Two numeric terms whose canonical forms differ and that hold a defined operation are equal when linear integer arithmetic proves their difference zero for every value of the atoms under the definitions — found by the elaborator's search, and checked by the certifier as evidence; a pure polynomial never reaches that check, since a difference in its canonical form is a difference in its value. Two `Bool` terms are equal when their Boolean forms are equivalent modulo linear integer arithmetic.
- **Reduction** folds a comparison whose literal is valid to `true` and one whose literal is unsatisfiable to `false`, which is what lets a decided bound be discharged by reduction on grounds past cancellation.
- **Unification** asks the solver, which solves a metavariable occurring linearly with a unit coefficient and, inside a monomial, proposes the pairing a factor-by-factor comparison would. On `Nat` a solution is admitted only in ℕ[X].
- **Inversion** reads a disequality only from unsatisfiability for every value of the atoms, and deduces only from equivalent solved forms.
- **A refinement** is recorded and looked up by canonical form. Keys are Boolean-canonical and never reason modulo arithmetic, so a lookup may miss a spelling linear arithmetic would equate, and fails closed when it does; entailment stays out of the store.
- **What stays special**: `map` by a function convertible to the identity, which becomes one generic check that the function converts with `(v) => v`; the division family, with Euclid's identity and the right shift as a floor; and the `Flt` model's folds over literals.

## One `Seq` carrier inside the compiler

`List`, `Bits` and `Bytes` stay the guest's three types, as `Bits` and `Bytes` are already two guest types over one packed family; the compiler collapses them one level up. `Grain` is not extended with a list variant: it is a fact about packed payloads — how many bits a generator takes, which grain a regrouping reads — and a list has no answer to either.

- **`SeqCarrier`** in `curios-core` is `List(Term)`, holding the element type, or `Packed(Grain)`. It answers the element type, so each `Intrinsic::signature` row is written once.
- **`SeqKind`** in `curios-num`, beside `Grain`, is the erased carrier, `List` or `Packed(Grain)`, shared by `curios-ersd`, `curios-cont` and `curios-emit` as `Grain` already is. It replaces `curios-ersd`'s `SequenceGrain`; `SequenceOp`, `SequenceArity` and `SequenceFacts` become `SeqOp`, `SeqArity` and `SeqFacts`, and `curios-elab`'s local carrier enum gives way to `SeqCarrier`.

The intrinsics go from twenty-two to fifteen:

| Today | After |
| --- | --- |
| `BinType(Grain)`, `ListType(T)` | `SeqType(SeqCarrier)` |
| `Bin(Grain, Binary)`, `List { element, items }` | `Seq(Run)`, a run being `Packed(Grain, Binary)` or `Items { element, items }`, so the carrier is read off the run and a list can never hold a packed payload |
| `BinLen` and `ListLen`, `BinGet` and `ListGet`, `BinSlice` and `ListSlice`, `BinAppend` and `ListAppend`, `BinConcat` and `ListConcat` | `SeqLen`, `SeqGet`, `SeqSlice`, `SeqAppend`, `SeqConcat` over a `SeqCarrier` |
| `BinReplicate` | `SeqReplicate`, produced by the packed rows alone for now |
| `ListFold` | `SeqFold`, produced by the `List` row alone for now |
| `ListMap` | `SeqMap { source, target }`, produced by the `List` rows alone for now |
| `BinEql`, `BinAnd`, `BinOr`, `BinXor`, `BinReinterp` | unchanged: packed alone, keyed by `Grain` |

Below Core the same collapse follows: `SeqOp` carries a `SeqKind` for the shared operations, `curios-cont`'s sequence intrinsics likewise, with `Repr::Seq(SeqKind)` keeping the layouts unconfusable as `Repr::Bin(Grain)` does, and the emitter's arms merge onto `RopeData`, which is already generic. What stays separate is what genuinely differs: the two kinds of run, the packed-only operations, the bit-level helpers, the canonical i31 form of a small packed value, and the host's wire types.

**Order.** Bottom-up, so each Core step's lowering is a mapping from `SeqCarrier` to `SeqKind`: `SeqKind`; `SeqOp`; the continuation intrinsics and `Repr`; the emitter's arms; `SeqCarrier` and `SeqType`; the operations one pair at a time; the literals; the algebra's twins, `peel_bin` and `peel_list` becoming one peel that branches on the kind of run; one match elaborator, one literal lowering, and `/sys` rows built from one `seq_ops(carrier)` as `bin_ops(grain)` builds them now; and the totality and positivity arms of `curios-analysis`.

**Risks.**

- The kind of run is a soundness fact: applying a packed head clash to `List` fails `[a + b] ++ xs ~ [b + a] ++ xs` as a false impossibility, which the `List` value grid already catches as a mutation, and the merged peel must keep that row failing.
- `Intrinsic` and `SequenceOp` are archived, so the prelude's images and stored units change format; the prelude rebuilds itself, and the first Core step confirms that a stored unit is invalidated by the compiler's identity.
- `SeqType(List(T))` takes `T`'s level exactly as `ListType(T)` does, and the packed carriers stay at the ground level.
- Every Core step re-elaborates the prelude, so those steps are few.
- `SeqMap` across carriers has no producer yet; its typing reads the result element type off `target`, and the `List` rows exercise it.

**Later.** A guest rework may make the carrier a type-level index, `Seq(c)`, with `List(T)`, `Bits` and `Bytes` as aliases, one `/std` written over `c`, and specialization per carrier at erasure. `SeqCarrier` is shaped for it: that rework adds a variant holding a carrier term rather than rewriting the nodes.

## Bounds from hypotheses

A bound that follows from the guards and decided hypotheses in scope by linear arithmetic is discharged in elaboration, never in conversion. Where an obligation `Holds(t)` does not reduce to `True`, the elaborator runs the relational layer over the literals in scope, and a valid result's certificate — a Farkas combination, with the case splits a defined operation or an integer cut calls for — is turned into a proof term: the hypotheses scaled and summed through `/std`'s order lemmas, with any the certificates need added beside them, the residue closed by the cancellation conversion already performs, and each split a `match` on a decided comparison. The kernel rechecks an ordinary term, so nothing is trusted that is not already. What does not follow linearly still takes a written proof: `tests::matching`'s `a_bound_a_hypothesis_only_implies_is_still_stuck` needs monotonicity of a product and stays refused until a nonlinear certificate exists, which is outside this specification.

## Stages

Stages are named so the laws specifications can cite one. Each lands alone, retires what it names, and runs the old engine beside the new one until the differential under Verification is clean.

- **The metatheory grids.** Conversion held transitive, and stable under substitution — including substitutions that add atoms — over today's engine, so the grids guard every later stage.
- **One `Seq` carrier.** Above. A pure refactor.
- **The algebra crate.** `curios-algebra` as the reference implementation, with the certificate checker, each structure tested on its own against `curios-num`. No integration.
- **The relational layer.** Canonical literals and Boolean forms decide comparisons and `Bool`; a valid or unsatisfiable literal folds; truncated subtraction and the quotient and remainder by a literal carry their definitions; the elaborator's search emits the certificates the certifier checks, which is the certifier specification's *Evidence* stage landing beside this one. Retires `decide_bool`, `normalize_bool`, `align_comparisons`, `peel_bool`, `peel_symmetric` on comparisons and `Bool`, `apart_modulo`, `compare_preimages`, `int_split_by_sign`, and `Bool`'s share of `then_laws`; the truth table's eight-atom cap becomes a priced budget. Needs the algebra crate.
- **Canonical keys.** Refinements recorded and probed by canonical form. Retires the refinement probe's retries under a dual and a successor spelling. Needs the relational layer.
- **The ring.** `Nat` and `Int` as one polynomial ring with its solver, the bitwise operations as algebraic normal form, the sign facts, and equality of numeric terms modulo their defined operations. Retires `Nat::cancel_common` and `int_cancel_common`, `Nat::normalize`, `Nat::normalize_atoms` and `int_normalize`, `classify_nat`, `peel_nat`, `peel_nat_pair`, `peel_nat_terms`, `peel_int_pair` and `peel_monomial`, the bitwise share of `then_laws` and `peel_symmetric`, and the folds' twin merging, which is re-expressed through the crate's polynomials. Extends canonical keys to ring forms. Needs canonical keys.
- **Morphisms and words.** The declared morphisms and the word algebra over `Seq`, beginning with the rows that give every carrier the same operations — `Bytes/fold` and `Bits/fold` over `SeqFold`, replacing their hand-written loops, and `List/replicate` over `SeqReplicate` — since a carrier without one would leave a hole in the generated grid. `pow` is promoted here, its fold charged before it builds as the left shift's is, through a `curios-num` power bounded by an allowance beside `shl_within`, and the isomorphism, projection, derived operation and commutative magma the `Flt` laws declare land here too. Retires `then_coefficient` and `then_power`, `int_of_nat` and `int_preimage` as code of their own, the conversion arms of the retraction pairs, `peel_position`, `rooted`, `seam_window`, `bin_measure`, the merged sequence peel's special cases, and `free_monoid`'s shape readers. Needs one `Seq` carrier and the ring.
- **The theory audited.** Every rule of every stage held against the conditions under *The theory's shape*, its exceptions named and argued or moved out of the theory. Runs beside each stage and closes before the generated grid.
- **The generated grid.** Every axiom of every declared structure instantiated at every declared operation and held to ground truth, the controls kept by hand, and the perimeter consolidated to one entry per structure and one per kind of morphism. Needs morphisms and words.
- **Bounds from hypotheses.** Above. Needs the relational layer.

## Design decisions this overturns

Each is revised in the change that makes its replacement true, not before.

- [A law is decided where it neither respells nor invents](../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md). Its rule places each law by what it would do to the representation; declarations replace the placement question. It stops governing comparisons and `Bool` at the relational layer and is replaced by this specification's decision at the generated grid. Three of its sections change on the way. *Not this decision's* claims `compare_nat` complete for `<` and `<=` with no hypothesis in scope, which holds of one linear comparison and not of the combinations *The gap* lists. *Rejected — a normal form for `Bool`* rejected a reduced ordered diagram as a fold, and canonical keys use one computed on demand. *Deferred — parity as a clash* is resolved by the ring, whose solver finds `x * 2 + 1 = y * 2` unsatisfiable, with [Coverage](../soundness/per-term-rules/coverage.md)'s evidence brought to it.
- [A comparison is spelled one way when it is stuck](../design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md). The mirror at the rows stays. Its rejection of respelling a symmetric comparison's operands, and the probe's retries under a dual and a successor spelling, rest on keys recorded as written, and are overturned at canonical keys.
- [A bound is stated in a decided proposition and discharged by reduction](../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md). Its rejection of "a general arithmetic decision procedure first" deferred the procedure rather than rejecting it, and bounds from hypotheses supplies it where that decision says a fact following from two facts is a proof the compiler asks for. Its account of Coq Modulo Theory is inaccurate and is corrected in the same change: that metatheory covers strong elimination as well (Jouannaud and Strub, 2017), and conversion modulo a context's equations is the Calculus of Congruent Inductive Constructions (Blanqui, Jouannaud and Strub, 2008), not Coq Modulo Theory. *The refinement store is a lookup and not a theory* stands, restated as a lookup modulo canonical form.
- [A sum is merged when it is forced, not when it is built](../design/toolchain/a-sum-is-merged-when-it-is-forced-not-when-it-is-built.md) stands, and is why canonical forms are computed on demand; its rejection of a cheaper monomial representation is revisited against sparse Horner form at the ring.

The two perimeter entries, [Open fold laws and the sum normal form](../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md) and [Intrinsic fold laws and the free-monoid peel](../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md), are rewritten a structure at a time and consolidated at the generated grid. `curios-core`'s and `curios-analysis`'s READMEs take the placement above, `curios-algebra` gets its own, and the READMEs of the stages below Core that describe `Bin` and `List` as twins are revised with the collapse. `CLAUDE.md` gains two invariants — `curios-algebra` depends on `curios-num` alone and names no `Term`, and `Intrinsic::algebra` is the source of truth for what an operation declares — and two change-routing rows: a structure, a declaration or a morphism, reaching `curios-algebra`, the table, the verdicts in `curios-analysis` and the generated grid; and a sequence carrier or its operations, reaching `SeqCarrier`, `SeqKind` and every stage that lowers them.

## Rejected

- **Taking the algebra out of conversion by casts the elaborator inserts**, the extensional-to-intensional translation of Winterhalter, Sozeau and Tabareau, as Lean4Less applies it to Lean. It would take the theory out of the certifier's trusted base, but the algebra is also reduction's, which discharges decided bounds, and matching's, which reads `n + 1` as a successor, and neither role is a cast. It is pervasive where Lean4Less's was not: elaborating `/std` runs `nat::cancel_common` alone 16,230 times, while Lean4Less measured 15.5% more term and 36% more checking time from equalities 6% of constants used. A cast translation stays open as a route for exporting proofs to another checker.
- **Equality saturation over an e-graph**, each law a rewrite rule. The laws would be orthogonal, but saturation guarantees neither termination nor a decision, and an associative and commutative operation blows it up; it suits an elaboration tactic, not a judgment.
- **Rewriting modulo associativity and commutativity with declared rules**, as Lambdapi and the rewrite rules of Agda and Rocq do. The laws become declarative but remain a list to extend, an undirected law needs matching modulo the theory, and confluence and termination become obligations of every rule.
- **Reflection as the mechanism**: the canonicalizers written and proved in `/std` and run by the kernel. It is the strongest account of trust, but a Curios twin run at the type level was measured at ten to thirty-five times a Rust fold for `Flt`'s rounding, and the kernel would run it on every conversion; a twin is kept as an evidence route instead.
- **A private exponential beside `/std/Nat/pow`**, which would leave `pow(2, k)` and `shl(1, k)` two terms conversion cannot relate, and the exponent laws unavailable to a program that writes `pow`.
- **A list variant of `Grain`**, for the reasons under the `Seq` carrier.
- **`Bytes` as `List(Byte)`**: a representation chosen by the element type breaks parametricity, and with types erased it would force two forms of one value at run time, which [A small packed value has one runtime form](../design/toolchain/a-small-packed-value-has-one-runtime-form.md) rejects.
- **Canonical forms at construction**, for the cost the sum decision measured.
- **A hypothesis in conversion**, and **omega over hypotheses in conversion**, for the reason under *What stays out of conversion*.

## Non-goals

A carrier-indexed `Seq(c)` for the guest and a `/std` written once over it; nonlinear decision in conversion past declared sign facts; `Rat` as an intrinsic carrier; and `reverse` as an intrinsic anti-homomorphism, which would give `len(reverse(xs)) = len(xs)` and `reverse(reverse(xs)) = xs` and is a candidate for a later declaration.

## Verification

- **The `Seq` carrier** is a pure refactor: every rung `wonder stage` prints — `core`, `ersd`, `cont` and `wasm` — over `programs/` and `/std` shows no difference from a baseline taken before the first step, and printing keeps today's names, so any difference is a finding.
- **Each engine stage** runs the old engine beside the new one: every old `Equal` stays `Equal`, every old `Clash` stays `Clash`, and every `Equal` the new engine alone finds is reviewed as a candidate law before the old engine is removed.
- **The law grid** never loses a held row. The generated grid instantiates every structure's axioms at every declared operation and holds `curios-algebra`'s reference implementation to them, the controls stay written by hand, and one mutation per structure is run and caught and named in its perimeter entry. Every certificate the elaborator's search emits over the grid is held by the checker.
- **The metatheory grids** hold at every stage.
- **`curios-algebra`** is property-tested against `curios-num`, and its certificate checker against certificates corrupted one step at a time.
- **A promoted operation** — `pow` here, and those the laws specifications promote — folds as it executes over literals across the i31 and past 64 bits, and a cost test holds its fold's charge.
- **Cost.** The prelude's build is measured before and after every Core stage and every engine stage, and reported by the stage it measures.

## Completion criteria

- Every fold, peel and probe the stages above name is retired or re-expressed as a declaration, and each checker's intrinsic conversion is the one verdict in `curios-analysis`.
- Both checkers decide the theory with `curios-algebra`'s reference implementation, every rule meets the conditions under *The theory's shape* or is argued as an exception, and `curios-cert`'s dependency closure contains no search.
- Conversion decides the fragment this specification states: polynomial identity over `Nat` and `Int`, word identity over `Seq`, Boolean identity, the declared morphisms and defined operations, and the validity of quantifier-free linear formulas over the atoms.
- `List`, `Bits` and `Bytes` are one `Seq` carrier inside the compiler, and every carrier has the same free-monoid operations and laws.
- `pow` is a `/sys` operation, and the left shift is read through it.
- A bound following linearly from the hypotheses in scope is discharged without a written proof.
- Adding an operation of an existing kind is a row of `Intrinsic::algebra`, as [the `Nat` laws](nat-laws-spec.md), [the `Int` laws](int-laws-spec.md) and [the `Flt` laws](flt-laws-spec.md) demonstrate.
- Before this specification is deleted, its contracts are recorded in `curios-algebra`'s README and rustdoc, the decision and its rejected alternatives are a design decision replacing the ones overturned above, the perimeter holds one entry per structure, `CLAUDE.md` carries the invariants and routing above, the roadmap entry is a checked summary, and no reference to this filename remains.
