# A declaration leaves a sort to its caller

A definition that serves a proposition and a type alike is written once for each, and nothing lets a declaration say that either will do. This specification states how one leaves a sort open, what checks it, and the stages that land it.

## What this builds on

- **Being a proof is a fact about the term.** No proposition stands where a type is wanted ([Subsumption is a relation, not a traversal order](../../design/theory/subsumption-is-a-relation-not-a-traversal-order.md)), so a type is a proposition under every substitution and every case, or under none.
- **A level is left to a caller already.** A declaration generalizes the levels its constraints leave free and each use instantiates them ([A universe level is implicit, cumulative, and settles by where it stands](../../design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-stands.md)): a `UniverseContext` on the declaration, a vector of levels on each occurrence (`Instance`, `InductType`, `Variant`, `StructType`, `Struct`, `curios-core/src/term/shape.rs`), and `UniverseSolver` (`curios-elab/src/universe_solver.rs`) between them.
- **Erasure keeps a slot for what a caller's variable types.** A function takes a parameter for each binder and a tuple a field for each member, and only a declaration drops one, by its own signature (`curios-elab/src/into_ersd/classify.rs`). A proof handed where the callee knows a variable fills a kept slot.
- **The rules that tell a proof from data by a declaration read its literal result sort**: the clash and injectivity guard of index inversion (`unify_index`, `curios-analysis/src/invert.rs`), the large-elimination guard (`guard_large_elimination`, `curios-cert/src/kernel/infer/eliminate.rs`) and the pinned payload a proof's match fires on (`pinned_instance`).
- **An intrinsic node takes either sort where its row says a type.** `Operand::IsType` (`curios-core/src/intrinsic/signature.rs`) is checked by each checker's type-formation judgment, and `sort_of_intrinsic` (`curios-cert/src/kernel/sort.rs`) lands a former over a proposition at `Type`.
- **Peers.** Rocq's irrelevant `SProp` is no subtype of `Type`, "necessary to use relevance marks in the conversion", and its elaborator allowed the inclusion "during elaboration but not when checking definitions" (Gilbert, Cockx, Sozeau and Tabareau, [*Definitional Proof-Irrelevance without K*](https://doi.org/10.1145/3290316), POPL 2019, §4.5 and §6.1). Werner's proof-irrelevant theory discards `Prop ⊂ Type` ([*On the strength of proof-irrelevant type theories*](https://arxiv.org/abs/0808.3928), LMCS 2008). Lean has no cumulativity and Agda's `Prop` embeds through a record. Rocq answers the duplication with prenex sort polymorphism, a universe being a sort beside a level and every complete instantiation a term of the theory without it (Poiret, Gilbert, Maillard, Pédrot, Sozeau, Tabareau and Tanter, [*All Your Base Are Belong to Us*](https://doi.org/10.1145/3704912), POPL 2025), and then with elimination constraints between sorts for a family whose own sort is open (Rosain, Díaz, Maillard, Sozeau, Tabareau, Tanter and Winterhalter, [*Bounded Sort Polymorphism with Elimination Constraints*](https://doi.org/10.1145/3776732), POPL 2026).

## The gap

**One definition is written once for each sort.** `/std/Eq/subst` carries a proof along an equation and `/std/Eq/transp` a value, with one body (`curios-std/src/Eq.crs`). `/std/Nat/descend` is induction on a measure and `/std/Nat/recurse` the fixpoint, with one body (`curios-std/src/Nat/descend.crs`, `curios-std/src/Nat/recurse.crs`). `/std/Evidence` is `/std/Option` over a proposition (`curios-std/src/Evidence.crs`).

**An equation relates no two proofs, and no list, cell, channel or description holds one.** `/std/Eq` takes `@A: Type`. `/sys` declares each former and each operation over a `Type` (`curios-text/src/sys_module/helpers.rs`) where the node its body builds takes either sort, so the kernel forms a list of proofs that no program can write.

**The sort of a type left to inference is a `Type` that admits a proposition.** An unannotated binder's type, a motive's result and a parked term's are typed at `Type` at a classifier's level (`UniverseSolver::fresh_classifier`), under which `decide_below` (`curios-elab/src/convert.rs`) admits a `Prop`. The classifier states no sort, so every reader takes it for `Type`: `let r = match t | a() => True/qed() | b() => True/qed() end;` over a proposition of two constructors is refused as an elimination into data where the result is a proposition, and admitted with the motive written; a goal solved by a proof is reported at `Type`; and a sort read before the type under it is known is read as `Type` for good.

**A sort has no place to be open.** Core spells a sort as `Subterm::Type(Level)` or `Subterm::Prop(Level)`. The quality cannot ride a term metavariable: with `Context::fresh_classifier_type` answering a hole at a fresh inferred level in place of `Type` at a classifier's, `/std` does not elaborate, 316 declarations reported and 289 of them `type mismatch`, `inferred: ?`, `expected: Type` (counted at `103139151`, from the report of `curios-prelude-archive`'s build script), since `check_is_sort` (`curios-elab/src/typing.rs`) and every reader like it needs a sort's level at once while its quality may wait. It cannot ride the level's metavariable either: a proposition's level and a type's are identified wherever they are compared.

## Decisions

**A sort is a quality beside its level.** A quality is `Prop`, `Type`, a declaration's parameter or, while a declaration elaborates, a metavariable. Core holds one sort former, `Subterm::Sort(Quality, Level)`, so every reader of a sort is written against both parts.

**A top-level declaration leaves a quality open, and each use states it.** A `UniverseContext` counts the qualities its declaration leaves open beside its level parameters, and an occurrence's instance states one quality for each beside its levels. Only a top-level item binds a quality: a local definition and a local group bind none, and may name the item's.

**The kernel checks a declaration at every ground assignment of its qualities, and judges no open sort.** Each check runs on the declaration with its quality parameters substituted, so every term the kernel judges is ground and no store keyed by a term holds one term at two sorts. The scheme is stored as written, and an occurrence instantiates it at the qualities it states. What the kernel accepts is then what it would accept of the 2ⁿ declarations written out, which is the property Rocq proves of its own design as monomorphization.

**The elaborator checks a declaration once, with its open qualities held abstract.** Each of its readers answers for an open quality in the direction that reader calls safe: no proposition for irrelevance and for an implicit taken as a bound, kept for erasure, a proof position for the totality obligation, no field of data for eta. What it accepts the kernel checks at each assignment, so a reader that answers wrongly costs a refusal where the declaration is written.

**A family's own sort is ground.** An `induct` and a `struct` declare `Prop` or `Type`. The rules read off a declaration's result sort stand as they are, and no relation between two open sorts says what one may be eliminated into.

**The sort of a type left to inference is a quality the elaborator solves.** Two sorts compared unify their qualities. A reader that needs the quality and meets it unsolved decides it where only one answer is admitted, the elimination of a proposition no index pins and the field of a `Prop` structure, and takes the safe answer otherwise. A quality still unsolved where its declaration closes is `Type`.

**`Sort` is written where a caller chooses.** It stands in the signature of a top-level `let`, `induct` or `struct`, each occurrence a quality of its own as each `Type` is a level of its own. It is refused in a body, in a local definition and in a `concept` or `satisfy` header. One the declaration itself decides is refused, with the sort it can only be and what decided it.

**A Π takes its codomain's quality, and a tuple type the join of its fields'.** A tuple with a field of data is a type, one of propositions alone a proposition, and one whose fields are propositions and types at one open quality has that quality. A tuple over two open qualities has none, and is refused.

**A mark states what is known.** A binder at an open quality bears no proof mark and a declaration whose result is at one is no marked proof, so each is read as data, which the kernel refuses nowhere.

## Open questions

**What a refusal at one assignment tells a reader who wrote neither.** The kernel names the assignment it refused at. Whether the report restates the declaration at that assignment is not decided.

**Which of the elaborator's readers meet an open quality once `/std` says `Sort`.** Counted where the fifth stage lands, by a note at each.

**Whether a `Sort` reaches a concept.** A witness is found by the key its signature is spelled at, and what a key at an open quality matches is not designed.

## Stages

Each stage is one commit, and its check passes before the next starts.

0. **This specification, and the report of a proposition handed to an implicit type.** `let keep(@A: Type, a: A) -> A = a;` asked as `keep(True/qed())` reports `inferred: True`, `expected: ?`. Check: a fixture in `curios-pipeline`'s `tests::diagnostic_tests`.
1. **A sort is a quality beside its level.** The representation alone: `Quality`, `Subterm::Sort`, and every reader in `curios-core`, `curios-analysis`, `curios-cert`, `curios-elab` and `curios-text`. The kernel refuses a parameter and a metavariable. Check: `cargo xtask clippy`, the five crates' tests, and the corpus compiled to the same bytes.
2. **The sort of a type left to inference is a quality metavariable.** The classifier, `classifies` and `decide_below`'s arm go. Check: the match above as a fixture in `curios`'s `tests::matching`, with its twin into data refused; a goal solved by a proof reported at `Prop`; `cargo xboard report`; the corpus compiled to the same bytes; the notes of `curios_elab::sort` counted again.
3. **A declaration takes quality parameters, in Core and in the kernel.** `quality_count`, an occurrence's qualities, the walk that instantiates both, the check at every assignment. Check: over modules built by hand in `curios-cert`, a definition ill-typed at its `Type` assignment alone is refused, mutation-checked against a check that stops at the first assignment; an occurrence that states too few qualities, or an open one, is refused; two instances of a family at two sorts do not convert. The corpus compiles to the same bytes.
4. **The word, generalization and instantiation.** `Sort` in `curios-text`, in the tree-sitter grammar and in the VS Code grammar; the elaborator's written qualities, its closing of a declaration and its instantiation of a use. Check: `curios`'s `tests::universes::sort_tests` for one definition asked at both sorts, a family over a sort and each refusal; the two run-time tests of a proof handed to a caller's variable restored in `tests::erasure::shape_tests`; in `tests::board::subsumption_tests`, a type that is a proposition in one case of a match refused at `Sort`, and a declaration whose `Type` assignment eliminates a proposition refused by both checkers under one name.
5. **`/sys` and `/std` say `Sort` where either serves.** `/sys`'s formers and operations at every operand its row calls a type; `/std/Eq`; `/std/Eq/subst`, `/std/Nat/descend` and `/std/Option`, with `/std/Eq/transp`, `/std/Nat/recurse` and `/std/Evidence` removed. Check: `cargo xtask clippy`, which elaborates, erases and certifies `/std`, and `curios-std`'s tests. It may move any reading of `xbench` whose loop holds a decision: an `Option` of a proof keeps the slot `Evidence` dropped.
6. **The decision, and this specification retired.** Check: the gate.

## Verification

- **The corpus compiled to the same bytes.** With the compiler `cargo xtask build` builds, each loose program of `programs/` is compiled with `curios compile <program> -o <file>` and the files' SHA-256 digests are compared with those taken at `103139151`. The first three stages change none; the fifth may.
- **The notes of an undecided sort.** A profiling build (`cargo xtask profile programs/hello_world.crs --profile release`) writes `curios-prelude-archive/.artifacts/profile.tsv`; the `V` rows whose message is `level defaulted to 0` are counted by their `site`. At `103139151`, counted: 36 at `unsolved metavariable` and 22 at `neutral type unsynthesizable`.
- **What an eager check would cost were every `Type` a caller's choice.** Over the headers of the top-level `let`, `induct`, `struct`, `concept` and `satisfy` declarations under `curios-std/src` outside `tests/`, the written `Type`s in an input position are counted, a binder's own type and the codomain of a function-typed binder, the declaration's own result left out. At `103139151`, counted: 532 of 2,376 declarations write one, and they have 3,624 ground assignments between them.
- `cargo xboard report` holds every ticket after the second, fourth and fifth stages.
- After the fifth stage, on standard input: one definition used at a number and at a proof runs; `List(True)` and `Option(True)` are certified; `let F(c: Bool) -> Sort = match c | true => Two | false => Nat end;` is refused.

## Rejected

- **`Prop u ≤ Type v`**, which the subsumption decision rejects: it never removed a pair whose own sort differs, and whether a type is a proposition follows the case it is read in.
- **A kernel that judges an open sort**, as Rocq's does. Each of its readers of a sort would need an answer for one not known, safe in a direction of its own: no for irrelevance, yes for the totality of an erased position, data for the elimination guard, no field of data for eta. The two share a term representation, so this stays reachable.
- **A copy of the declaration for each assignment, named before the kernel.** The kernel is untouched, and every stage after it, every registry slot and every report learns an instance's name.
- **Every `Type` in an input position a caller's choice.** No word is added, a signature no longer says whether it takes a proposition, and an eager check costs the count above, so the kernel would judge an open sort or check an instance where a use first asks for it.
- **A box the elaborator inserts where a proposition meets a `Type`.** Two proofs under a box stand apart wherever neither is a literal, and every use of a definition at a proposition runs through a wrap and an unwrap no source wrote.
- **Irrelevance by where a value is used**, with no sort of propositions: a function has no irrelevant result, and a proposition is neither declared nor quantified over.
- **The quality on a term metavariable or on the level's**, by the experiment under *The gap*.

## Non-goals

A mark that is a quality ([A proof is known by its mark](../soundness/200-a-proof-is-known-by-its-mark.md)); a family whose own sort is open, and the elimination constraints it needs; a tuple over two open qualities; a `Sort` in a concept's header or in a local definition.

## Completion and retirement

- `/std` writes each of the three definitions once, `/std/Eq` relates two proofs, and a list, a cell, a channel and a description hold one.
- No elaborated term and no metavariable's type holds a sort that states none.
- The kernel judges no term holding an open quality.

Before this specification is deleted, a design decision states the rule with its rejected alternatives, the subsumption decision no longer describes a classifier, [the reference](../../syntax.md) states where `Sort` is written, `curios-cert`'s README states the check at every assignment, the roadmap entry is a checked summary, and no reference to this filename remains.
