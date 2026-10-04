# What conversion still decides by spelling or by cap

The first capability below, an atom being one where conversion says so, is refined and open, with its stages. The rest is **not refined yet**: this specification preserves what the carriers' algebra does not decide and no consumer has asked for, with the approaches already discussed, and the rows the law grid still refuses, as [the relational layer](08-relational-layer.md) reserves its own. Each of those opens when a consumer needs what it decides, and refinement establishes its fragment, algorithm contract and acceptance criteria before any implementation.

## An atom is one where conversion says so

### What this builds on

- **One chain for both checkers.** `curios-analysis`'s `convert_intrinsics` reads two intrinsics through the carriers' algebra and hands back what it cannot settle — a residual pair, or the congruence's operands — for each checker to discharge its own way; it calls no judgment ([Intrinsic fold laws and the free-monoid peel](../../design/soundness/conversion/intrinsic-fold-laws-and-the-free-monoid-peel.md)).
- **Atom identity is spelling.** `curios-algebra` reasons over handles and leaves identity to its caller; `curios-core`'s `atoms` module hands one handle per term as written, up to universe instances at `Nat` and `Int`. Every reader — cancellation, a monomial's factors, a connective's leaves, the truth table, the linear views, the symmetric peel — pairs atoms by that identity, and hands a single unpaired pair to the checker as a residual.
- **One retry respells.** A pair the readers decide nothing about is read once more with every atom's arguments forced under the rigid heads they are stuck on and the sums and products inside them ordered (`force_atoms`), never under a binder.
- **The congruence compares by position.** What is still undecided is compared operand by operand in the order the term holds them: a sum's summands as they were written, and a product's factors and an equality's sides in the order of their structural hashes.
- **Each checker's own conversion is typed.** A residual reaches the kernel's `compare` or the elaborator's worklist, where an application's arguments are compared at its head's telescope, so two proofs of one proposition convert ([Definitional proof irrelevance](../../design/soundness/conversion/definitional-proof-irrelevance.md)).

### The gap

Two atoms that convert without being identical are two atoms to every reader, and the forcing does not reach them where they differ in a proof argument, under a binder or inside a stuck `match`. With one such atom on each side the pair is a residual and conversion decides it. With two, nothing pairs them and the congruence's order decides.

- **Conversion is not transitive.** With `p1` and `p2` two proofs of `Holds(a < 10)`, and `q1` and `q2` of `Holds(b < 10)`: `f(a, p1) * f(b, q1)` converts with `f(a, p2) * f(b, q1)`, and that with `f(b, q2) * f(a, p2)`, at every binder order, and the first converts with the last, by one `Eq/refl()`, at 1 order of 16. `Eq/trans` over the two steps is accepted.
- **A verdict turns on the order binders were declared in** where operands are held in hash order: product, `==` and `!=` at `Nat` and `Int`. One equation is accepted, refused by the elaborator, or accepted by the elaborator and refused by the kernel.
- **A verdict turns on the order operands were written in** at every other row the law table declares commutative: sum at `Nat` and `Int`, the bitwise `and`, `or` and `xor`, `&&`, `||`, `xor` and `==` at `Bool`, `==` at `Flt` and `Bytes`. Swapped, the pair is refused at every binder order; written alike, it is held at every one.
- **Coefficients do not merge.** Three spellings of one stuck `match`, summed, are refused against three times the first.
- **The checkers part.** A `Vec(Nat, f(a, p1) * f(b, q1))` returned at `Vec(Nat, f(b, q2) * f(a, p2))` through a `let` annotated at `Vec(Nat, f(a, p2) * f(b, q1))` is accepted by the elaborator at every order, which takes the annotation as a step, and refused by the kernel at 12 of 16, which compares the first type with the last.
- **An implicit argument is picked.** With `g : (@x: Nat, @y: Nat, v: Vec(Nat, x * y)) -> Vec(Nat, x)` and `w : Vec(Nat, a * b)`, `g(w)` is a `Vec(Nat, a)` where `a` is declared before `b` and a `Vec(Nat, b)` where `b` is declared first: `?x * ?y` against `a * b` has two solutions, and the congruence commits the one its order pairs.

None admits a false equation: every verdict is `Equal` from a pairing or from a residual the checker itself compares, and `match h end` over a refused row's equation is refused, so conversion calls none of them impossible. An ordered comparison, a difference and a concatenation are unaffected, since position fixes each operand's partner and each pair reaches the checker.

This is the relation Coq Modulo Theory defines — a term's cap compared in the theory, its aliens by conversion, and the whole transitive (Jouannaud and Strub, 2017) — and the identity Mathlib's `AtomM` keeps, an atom being a class of terms up to definitional equality.

### Settled

1. Two atoms are one where the checker's own conversion says so. `curios-algebra` and the `atoms` table do not change: the atoms of a pair are classed before any reader sees them, and each is read as its class's representative.
2. The chain still calls no judgment. Where its readers decide nothing it hands the pair's atoms back, and it is entered again with the partition.
3. `force_atoms` is retired, since the classing subsumes it.
4. A commutative operation's operands are paired, never compared by position. Which operations those are is what the law table's `Family::Commutativity` rows say, read from the table.
5. An implicit argument with more than one solution is refused, naming the equation, and never picked.

### Stages

1. **Atoms classed before the algebra reads them.** `curios-core` offers the atoms of a pair and a pair with each atom replaced by its class's representative, over the traversal the forcing walked, widened to every row the table declares commutative. `convert_intrinsics` returns the atoms where steps 3 to 6 decide nothing and reads again when entered with a partition, and `connectives_agree` likewise. The kernel classes pairwise with its comparison under the active history; the elaborator in the bracket its witness probe uses, a pair being one only where it converts with nothing committed. Only acceptances are added. Check: every row of the protocol below is held by both checkers at every order of the sweep, the grid's atom rows stay held, and the annotated `let` is accepted by the kernel.
2. **A commutative operation pairs its operands.** For a row the table declares commutative the congruence pairs operands by identity in either order: one pair matched hands the other back as a sufficient residual, and none matched is unpaired. The kernel reads unpaired as unequal; the elaborator parks the problem while a metavariable is unsolved and reports a survivor as a postponed conversion. This is the one stage that refuses what was accepted, and a program resting on a picked implicit writes it. Check: `g(w)` is refused at both declaration orders and accepted with `@x`, a single unpaired operand still solves its metavariable at every commutative row, and no verdict and no inferred type moves across the sweep.
3. **A refusal says what was declined.** The chain returns why it declined beside its outcome — operands unpaired, or the truth table past its cap — and the elaborator's mismatch report says so. Check: a diagnostics test for each.

### Verification

The measurement is `counted`, at `67b3e8f86`. Each row is one program on standard input to `cargo run --release --package curios -- wonder diagnostics -`:

```crs
use /std/{Nat, Int, Bool, Flt, Bytes, Vec, Eq, Io}; use /std/Bool/{Holds, True};
pub let law0(<binders>) -> <claim> = <body>;
Io/pure(())
```

An answer is the kernel's refusal where the report says `the kernel refused`, the elaborator's where it says `while elaborating`, and held otherwise. A row is put at several orders of its binders, each a shuffle that keeps a binder after the binders its type names, so a figure is the count over one such draw.

- **Binders.** The proofs: `a: Nat, b: Nat, f: (n: Nat, p: Holds(n < 10)) -> R, p1: Holds(a < 10), p2: Holds(a < 10), q1: Holds(b < 10), q2: Holds(b < 10)`, with `R` the row's carrier. The functions: `a: Nat, b: Nat, h: ((Nat) -> Nat) -> Nat`. The matches: `a`, `b`, `c`, `d`, `n` and `m`, each a `Nat`. Below, `L1 = f(a, p1)`, `L2 = f(b, q1)`, `R1 = f(b, q2)` and `R2 = f(a, p2)`.
- **Transitivity**, over the proofs at `Nat`, by `Eq/refl()`, at 16 orders: `Eq()(L1 * L2, R2 * L2)` and `Eq()(R2 * L2, R1 * R2)` are held at 16; `Eq()(L1 * L2, R1 * R2)` is held at 1, refused by the elaborator at 8 and by the kernel at 7.
- **Every commutative row**, over the proofs, `Eq()(L1 ⋆ L2, R1 ⋆ R2)` by `Eq/refl()`, at 8 orders. `==` and `!=` at `Nat` are each held at 3, refused by the elaborator at 4 and by the kernel at 1; `*` and `==` at `Int` are each refused by the elaborator at 4 and by the kernel at 4. Refused by the elaborator at every order: `+` at `Nat` and `Int`; `Nat/and`, `Nat/or` and `Nat/xor`; `&&`, `||`, `Bool/xor` and `==` at `Bool`; `==` at `Flt` and at `Bytes`. Not measured: `!=` at `Int`, `Bool` and `Flt`, and `==` at `Bits`.
- **Under a binder**, over the functions, at 16 orders: `Eq()(h((x) => x + a) * h((x) => x + b), h((x) => b + x) * h((x) => a + x))` is held at 2, refused by the elaborator at 9 and by the kernel at 5, and the same two calls summed are refused at 16.
- **Stuck matches**, over the matches, at 32 orders, with `M1 = (match n | 0 => a + b | _ => c end)`, `M2 = (match m | 0 => c + d | _ => a end)` and `M1'`, `M2'` the same with `b + a` and `d + c`: `Eq()(M1 + M2, M2' + M1')` is refused at 32, `Eq()(M1 + M2, M1' + M2')` is held at 32, and `Eq()(M1 * M2, M2' * M1')` is held at 9.
- **Coefficients**, over the matches, at 16 orders, with `T1`, `T2`, `T3` the match on `n` whose first arm is `a + b + d`, `b + a + d` and `d + b + a`: `Eq()(T1 + T2 + T3, 3 * T1)` is refused at 16, and `Eq()(M1 + M1', 2 * M1)` is held at 16.
- **The annotated `let`**, over the proofs at `Nat` with `v: Vec(Nat, L1 * L2)` last, the claim `Vec(Nat, R1 * R2)`, at 16 orders: the body `v` is held at 2, refused by the elaborator at 7 and by the kernel at 7; the body `let w: Vec(Nat, R2 * L2) = v; w` is held at 4 and refused by the kernel at 12.
- **The implicit**: `pub let law0(a: Nat, b: Nat, g: (@x: Nat, @y: Nat, v: Vec(Nat, x * y)) -> Vec(Nat, x), w: Vec(Nat, a * b)) -> {} = let r = g(w); let t: ? = r; ();`, whose goal reports `r : Vec(Nat, a)`, and `r : Vec(Nat, b)` with `b` declared before `a`.
- **Controls**, over the proofs, held at every one of 8 orders: `Eq()(L1 < L2, R2 < R1)` and `Eq()(L1 - L2, R2 - R1)` at `Nat`, and `Eq()((L1 ++ L2) ++ L1, R2 ++ (R1 ++ R2))` at `Bytes`. And `match h end` at `/std/Bool/False` over `h` a refused row's equation is refused.

### Rejected

- **Refusing wherever two operands are unpaired, with no classing.** It makes the verdict stable and leaves conversion non-transitive, and it is the one alternative that refuses programs without accepting any.
- **A method on the driver the chain calls mid-way.** The chain would call a judgment, where the elaborator's enqueues and may solve and the kernel's must run under the history of the goal it is inside. Handing the atoms back keeps the chain a function of a pair and a partition, which a test states by hand.
- **Keeping the forcing as a first tier**, which is two mechanisms for one question: the checker's conversion already decides every pair the forcing made identical.
- **A relevance mark on an argument**, so that identity could skip a proof: a second statement of what the parameter's type says, carried through every representation.
- **Normalising atoms under binders**, which misses a proof argument, costs a full normal form, and reduces among loose indices where the forcing declined to.
- **Congruence closure over the terms of a goal**, as Lean's `grind` keeps, which would take case equations too: a different kernel.
- **Normal forms by evaluation with rules on neutral terms** (Allais, McBride and Boutillier, 2013), complete for the monoid and fusion laws of lists: it has no commutativity, and it is a rewrite of both conversions.
- **Picking a solution for an ambiguous implicit.** A product keeps no written order to pick by, and any pick makes a program's meaning follow a declaration order.

### Completion criteria

- Both checkers hold every row of the protocol at every order of the sweep.
- No verdict and no inferred type moves with the order binders are declared in or operands are written in.
- `force_atoms` is gone, and the chain's module documentation states the hand-back.
- An ambiguous implicit is refused with its equation.

### Retirement

Move the rule, its rationale and what it rejected to a decision under `documentation/design/arithmetic/`, the chain's contract to its rustdoc, the classing's argument to the conversion entries of the soundness board, and the rows to the law grid. Replace the clauses of the roadmap line this section owns with a checked summary, and cut this section, leaving the rest of the file.

## What reduction reads by spelling

**Capability wanted.** A refinement key and a fold read an atom as conversion does, so neither turns on incidental term spelling. Today a case equation is recorded under a few spellings of its scrutinee and found by matching them ([Case equations and their key](../../design/soundness/elimination/case-equations-and-their-key.md)), and the fold pairs atoms by the identity the section above leaves behind.

The two checkers do not look for an equation in the same places, and which way they part turns on how a guard is spelled. The elaborator escalates a key it missed to its arguments reduced, which the kernel does not, so under `let n = m + 0; match Nat/in_range(n, 240, 244)` the elaborator accepts `Nat/le/of_in_range(m, 240, 244, True/qed())` and the kernel refuses it; [Case equations and their key](../../design/soundness/elimination/case-equations-and-their-key.md) records that direction as admitting and closed at certification. The other refuses: the kernel records a guard over local definitions with them substituted, while the elaborator settles only the spelling that names them, so under `let n = Byte/to_nat(c); match Nat/in_range(n, 0, 0x7F)` the kernel's reduct answers `Byte/to_nat(c) <= 0x7F` and the elaborator's does not; [the findings](00-findings.md) hold what is not yet known of that direction. The procedure that proves a bound from the facts in scope met both, and writes its proofs over the spellings the keys hold ([A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)); an author writing the same terms still meets them. Settling reduced spellings is also a measured cost of elaboration, and [A shared term costs its size](../05-compilation/01-shared-term-costs.md)'s settlement stage keeps a settled spelling for as long as the frames it rests on stand.

The fold reads atoms by the same identity, so a comparison or a difference of two convertible atoms does not reduce: with `p1` and `p2` two proofs of one bound, `f(a, p1) == f(a, p2)` does not reduce to `true`, nor `f(a, p1) <= f(a, p2)`, and `f(a, p1) - f(a, p2)` is not `0`, at every binder order, though conversion holds `f(a, p1) + b` equal to `b + f(a, p2)`. Proof irrelevance reaches conversion and not reduction. Classing atoms in the chain does not change this, since a fold runs inside reduction, below any chain.

**Previously discussed.** A total atom order, complete normalization within each supported fragment, canonical key construction and caching, and retirement of the spelling probes the keys replace. Identity stays separate from presentation order; hashes alone cannot establish equality.

For the fold, three directions, none investigated. Reduction asking its checker which atoms are one, which is Coq Modulo Theory's reduction modulo the theory and makes a reduct depend on a conversion verdict. An atom key that projects a proof argument away as it projects a universe instance, which needs the head's telescope, since an argument carries its plicity and not its sort. And laws taken probe-side, a comparison against a literal read as its sides' equation.

**Still to refine.** The exact fragments, atom relations, placement of normalization, cache lifetime and invalidation, and the consequences for substitution, universes, proof irrelevance, sharing and cost — and whether reduction may depend on a conversion verdict at all, against the evaluation memo and what a case equation assumes. Canonical comparison views do not by themselves justify changing reduction's spelling; the existing spelling decisions remain in force until a measured replacement lands.

## Boolean and bitwise normal forms

**Capability wanted.** Reason beyond today's local identities and the truth table's bounded agreement — two `Bool` terms with a connective between them are put to a truth table over their atoms, a rung of `curios-analysis`'s conversion chain, which declines past `curios-algebra`'s `BOOL_ATOM_CAP` of eight atoms — including relationships among comparison atoms and further natural bitwise laws.

The cap is also a place conversion is not transitive, by design: with `A` the conjunction of `a0 || a1 || a2 || a3 || a4` and the negation of each of its five atoms, and `C` the same over `a5` to `a9`, `A` converts with `false` and `false` with `C`, and `A` does not convert with `C`, their pair holding ten atoms. A reader can count the atoms, and the refusal says a type mismatch.

**Previously discussed.** Reduced ordered decision diagrams for Boolean reasoning, algebraic normal form for natural bitwise operations, and replacing the truth table's fixed cap with a budget the reduction budget prices.

**Still to refine.** Which representations serve which operations, how arithmetic relationships enter the Boolean procedure, and which complexity limits keep work predictable. The cap is a constant of the language on purpose, so the same two formulas are decided on every target (`curios-algebra`'s `boolean` module), and a priced budget has to keep that. Agreement, impossibility and failure to decide need separate contracts; disagreement over independent opaque atoms is not automatically a realizable counterexample.

## Polynomial unification proposals

**Capability wanted.** Broader polynomial and sign reasoning, and solving proposals beyond today's, so unification can propose a solution where a metavariable sits inside a polynomial equation.

**Previously discussed.** A ring procedure sharing the appropriate mathematics of `Nat` and `Int`, with its solver on the elaborator's side of the certifier's dependency closure; the elaborator chooses solutions and the certifier checks what reaches it.

**Still to refine.** The fragment, the treatment of natural coefficients and integer signs, opaque atoms, and where a proposal ends and a search belongs to [the elaborator's search over the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) instead.

## A reflection law for `Bytes/eql`

`a == b` giving `Eq(a, b)` holds as a lemma for `Nat`, `Byte` and `Int` (`Int/eq_of_eql`). `Bytes/eql` does not reduce over a list, so its reflection would be a new intrinsic law, declared as [the declared operations](02-declared-operations.md) are. Stepping confirms a match without one, so it waits for a consumer that needs the equation rather than the step.

## One internal sequence carrier

**Capability wanted.** One internal sequence carrier and common operations, preserving `List`, `Bits` and `Bytes` for the guest.

**Previously discussed.** A carrier and kind pair, with representation changes through the erased and continuation IRs, emission and stored archives. [The carriers' algebra](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) shares one word algebra, which deliberately works over today's three representations.

**Still to refine.** The representation, carrier and element typing, packed storage, proof operands, intrinsic signatures, stage boundaries, archive compatibility and performance, and whether a reasoning capability needs it or it can proceed on its own.

## Rows the law grid refuses

`tests::laws::written` holds rows no rule decides yet, each a candidate for a family of its own. Parity read as a clash: the gcd test decides `x * 2 + 1 == y * 2` false as a fold, and read by the `Nat` peel as `Impossible` it would let `match h end` close `Eq()(x * 2 + 1, y * 2)`, so it needs its own row and [Coverage](../../design/soundness/elimination/coverage.md)'s evidence before inversion may rely on it. Map fusion, `map(map(xs, f), g) = map(xs, (x) => g(f(x)))`. A shift by a symbolic count's exponent law, and a literal-count right shift's quotient. Boolean agreement past the truth table's cap, above.
