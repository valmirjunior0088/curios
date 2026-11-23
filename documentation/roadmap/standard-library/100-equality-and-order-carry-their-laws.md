# Equality and order carry their laws

**Adopted by the user on 2026-11-22; its next step is stage 1, a ready delivery.** `==` decides equality and `<` a total order wherever a type has them: `Eql` and `Ord` carry their laws as fields, as `Div` carries its precondition, so generic code and its theorems rely on what an operator says rather than taking the law as a hypothesis, and a type that cannot meet the laws has no witness and names its comparisons, as `Flt` names `Flt/eql` and `Flt/identical`. Curios states a partial operation's domain where the operation is, rather than giving it a total face that answers junk or traps ([A partial primitive is totalized by a canonical extension, or it states its domain](../../design/arithmetic/a-partial-primitive-is-totalized-by-a-canonical-extension-or-it-states-its-domain.md)), and a lawless `==` is that face over a concept.

## What is wrong today

- **`Eql` and `Ord` carry no law.** `ops.crs` says a witness of `Eql` "owes no proof that its answer agrees" and `Ord.crs` that "`Ord` itself carries no law"; a theorem that needs one takes it as a hypothesis, `Ord/Flips` in `List/sort`'s two and `List/sort_by`'s six, `Ord/Transitive` in `List/Pairwise`'s two, and each carrier proves its own beside it (`Nat/ord/flips`). Two witnesses of `Eql` decide no `Eq`: `http/Field/Name`'s ignores ASCII case, and `Flt/internal/Dyadic`'s compares the values of a representation it keeps unnormalized on purpose.
- **`!=` is a method of its own.** `Eql`'s `neq` is supplied beside `eql`, and "`!=` is no negation of `eql`" ([the language reference](../../language.md#operators)), so `a != b` and `Bool/not(a == b)` may disagree.
- **`<` needs a second witness that no derivation writes.** `<`, `<=`, `>` and `>=` dispatch through `Cmp`, four methods no law ties to `Ord`, and `{ .. }` derives `Ord` but no `Cmp`: at `82ddfd3b0`, `Date`, `DateTime`, `Time`, `Month`, `Weekday`, `Offset`, `OffsetDateTime`, `Ordering`, `Flt/Rounding`, `{}` and the tuples have an `Ord` and no `Cmp`, so `<` is refused on each, `no witness` of `Cmp` (`Month`, `Ordering`, `{Nat, Nat}` and `{}` probed at `9abc374c9`).

## Goal

- `Eql(A)` holds `eql` and two laws: `eq_of_eql`, that `a == b` gives `Eq()(a, b)`, and `refl`, that `a == a`. `neq` is gone, and `a != b` is `Bool/not(a == b)`.
- `Ord(A)` holds `use Eql(A)`, `ord`, `lt` and `le`, and five laws: `flips`, that `ord(b, a)` is `ord(a, b)` flipped; `transitive`, that `le` carries across a middle element; `eq_of_ord`, that `ord(a, b)` answering `eq` gives `Eq()(a, b)`; and `lt` and `le` agreeing with `ord`.
- `Cmp` is gone: `<` and `<=` dispatch through `Ord`'s `lt` and `le`, and `a > b` and `a >= b` are `b < a` and `b <= a`, their operands evaluated in the order written.
- `Flt` has no comparison operator: `Flt/lt`, `Flt/le`, `Flt/gt` and `Flt/ge`, IEEE's comparisons, are called by name, as `Flt/eql` is.
- `{ .. }` writes the proofs of a derived `Eql`'s and `Ord`'s laws, and each written witness proves its own.
- The theorems take the laws from the witness: `List/sort`, `List/sort_by` and `List/Pairwise` lose their hypotheses, and `Map/Key`'s `refl`, `flips` and `transitive` are `Ord`'s own, read through its superclass.

## Settled

- **The laws are the concepts' own fields.** A lawful companion, as Lean's `LawfulBEq` is beside `BEq` and `TransCmp` beside `Ord`, leaves the operator lawless wherever the companion is absent, the face Curios refuses; Agda's standard library bundles its structures' laws, as Curios does here.
- **A type with a narrower contract states it in its witness, once one needs to.** `Div`'s `Ok(A) -> Prop` is the precedent, and `Flt`'s `Ok(_) = Bool/True` declares its division total in the witness itself. A domain field for `Eql` or `Ord` waits for a consumer, and none exists.
- **`neq` goes.** Its one reason is an instruction, "a carrier with a native disequality instruction names it instead of paying for an equality and a negation" (`curios-common/src/syntax.rs`), and conversion already holds `Bool/not(a == b)` one proposition with `a != b` at `Nat` and `Int` (`align_comparisons`, `curios-core/src/reduce/intrinsic.rs`), so every bound `/std` states over `!=` stands. Rust's Clippy refuses a hand-written `ne` (`partialeq_ne_impl`), and Swift's `Equatable` asks for `==` alone.
- **`lt` and `le` stay methods.** The totality guard reads a `Nat` comparison as the intrinsic it reduces to (`curios-analysis/src/totality/guard.rs`), so `Nat`'s witness names `Nat/lt` and `Nat/le`; read off `ord` instead, `i < n` would be `Ordering/is_lt(Nat/ord(i, n))`, stuck at every bound. A derived witness writes `lt(a, b) = Ordering/is_lt(ord(a, b))`, whose agreement is `Eq/refl()`.
- **`>` and `>=` are `<` and `<=` swapped**, as Haskell's `Ord` documents `x > y = y < x` and Swift's `Comparable` asks for `<` alone; IEEE's comparisons obey the swap, so `Flt`'s named ones keep it.
- **`Ord`'s reflexivity is `flips`'s**: at `a = b`, `ord(a, a)` is its own flip, so `eq`; with it, `eq_of_ord` and `eq_of_eql`, `ord(a, b)` answers `eq` exactly where `a == b`.
- **A derived proof under a hypothesis writes an arm per constructor.** Over `h: Holds(a == b)`, a proof matching `a` and then `b` writes one arm of `b` in each arm of `a`, the pairs of differing constructors closed by the contradiction `h` reduces to there, as a case the facts refute is left unwritten (probed at `9abc374c9`: without `h`, the same match is refused, `no arm matches`).
- **`Hash` takes no law**: once `==` decides `Eq`, equal values encode alike by congruence.
- **`http/Field/Name` is stored lowercase**, as HTTP/2 and HTTP/3 send a name and Rust's `http::HeaderName` stores one, so its `Eql` derives; `Dyadic` drops its `Eql`, `Ord` and `Cmp` for the functions it calls by name.
- **A lawful order on floats is at hand**: `Flt/total_key` and `Flt/total_key_injective` give IEEE's total order as `Nat/ord` reads a key, so a type wrapping `Flt` has an `Ord` from proofs that exist.

**Rejected.** A lawful companion beside each concept, `Ord/Lawful` as Lean splits them. The laws as hypotheses, as today: every theorem restates them, and no generic code can rely on `==`. `neq` kept under a law: an obligation on every witness for one instruction. `<` read off `ord` alone: it blocks every bound over `Nat`. `Cmp` kept beneath `Ord`, as Rust's `PartialOrd` is beneath `Ord`: a second derivation for every ordered type, and a lawless `<=` on `Flt`, false between a NaN and itself and true both ways between `+0.0` and `-0.0`. Decisions returning evidence, Idris's `DecEq` and Agda's `Dec`: `==` stays the `Bool` that conversion folds and bounds are stated in.

## Open questions

- **What the derived proofs of `flips` and `transitive` match**, which hold no hypothesis refuting a pair of differing constructors: an arm per pair, 144 for `Month`, or one per constructor through a law of `Ord/by_tag` taking the tags' equality as a hypothesis the facts refute where the tags differ; erased either way. Settled at stage 3, where `by_tag`'s laws are written.
- **What `Map`'s `Eql` compares**: its entries, as today (`same_entries(to_list(a), to_list(b))`), its law then resting on two sound tries with one list of entries being one trie; or its tries' structure, its law structural. Settled by reading `Map/internal`'s canonicity at stage 2.
- **What the derived proofs cost `/std`'s build**, over the 57 derived witnesses of `Eql` and the 11 of `Ord` at `82ddfd3b0`. Settled by the prelude's build counted before and after stages 2 and 3.

## Stages

**1. `neq` removed.** `!=` lowered to `Bool/not(a == b)`, `neq` dropped from `Eql` and its witnesses, and the totality guard reading a negated comparison as its dual, as conversion does. Check: `/std` builds with its bounds as written; a guard `n != 0` still shows the totality checker that `n` is not zero; `curios`'s bounds and law grids pass.

**2. `Eql`'s laws.** The two fields; their proofs written by the derivation and by the 25 written witnesses; `Name` stored lowercase and `Dyadic`'s witness dropped. Check: a witness whose `eql` breaks either law is refused where it is declared; a derived witness of a recursive family and of one with a proof field is accepted.

**3. `Ord`'s laws, and `Cmp` folded into it.** `lt`, `le` and the five laws; `Cmp` deleted and `>` and `>=` swapped; `Ord/lexicographic`'s and `Ord/by_tag`'s laws proved once, for the derivation and the written lexicographic witnesses; the 22 written witnesses' proofs; `Flt`'s comparisons named. Check: `<` on a derived type compiles; a witness that breaks `flips` is refused where it is declared; `<` on two floats is refused as `==` is.

**4. The theorems take the laws.** `List/sort`, `List/sort_by` and `List/Pairwise` lose their hypotheses, and `Map/Key`'s `refl`, `flips` and `transitive` are removed for `Ord`'s. Check: `/std` builds, and its tests pass as restated.

## Verification and retirement

Each stage's check, then `cargo xtask workspace lint`, `cargo xtask jig lint`, the suite and `cargo xtask docs build`. As the work lands, `documentation/language.md`'s operator table and its sentence on `!=`, [Syntax forms are closed, semantics extend by witness](../../design/surface/syntax-forms-are-closed-semantics-extend-by-witness.md)'s `neq` and `Cmp`, and [A witness body may be written by the compiler](../../design/surface/a-witness-body-may-be-written-by-the-compiler.md)'s `Eql` and `Ord` bodies and its *Entries beside the dots* are restated. Once stage 4 lands, this file is deleted.
