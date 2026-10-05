# Plicity on a telescope's members

Working specification for making a mark mean on data what it means on a function. [Plicity is part of function identity](../../design/theory/plicity-is-part-of-function-identity.md) states the rule a call, a lambda and a concept literal follow: a hidden member — `@` or `use` — takes no position among the members an author writes, and is written only to be named. A function's precondition is therefore stated once: declared in the signature, filled at each call, a fact in the body. An invariant on data is not. A constructor's hidden payload is written at every arm, a structure's field takes no mark and its proofs are written at every literal, and a concept value is read with its edges in the count.

It is independent of every other spec. [Typed patterns](01-typed-patterns.md) later lifts the one restriction this leaves, on the rows of one constructor.

## What this builds on

- **The rule, where it holds.** `align` (`curios-elab/src/elaborate/align.rs`) matches written members to slots: plain members are written, in order, and between two of them the hidden ones are written from the first of their run, the rest left out. A call (`elaborate_apply`) and a concept literal (`elaborate_struct`) call it; a lambda (`elaborate_func_check`) and a spread (`elaborate_struct_spread`) each walk the same rule by hand.
- **A slot left out is filled by its mark.** `insert_auto_argument` (`curios-elab/src/elaborate/apply.rs`) fills an `@` slot by unification, by reduction to `True`, or by `entail` from the facts in scope, and a `use` slot by resolution.
- **A hidden member left out is still in scope.** An unnamed `@` binder's proposition is a fact wherever its telescope is opened: in a definition's body, in an arm, whose payload `assume_payload` assumes, and one level down in a structure- or tuple-typed hypothesis (`Reader::hypothesis`, `curios-elab/src/entailment/facts.rs`). A `use` member is in the witness scope.
- **A constructor's payload declares `@`** (`parse_induct_payload_field`), and construction fills it as any call does, a constructor being a function: `/std/Io/Chunk/chunk(b)` over `chunk(bytes: Bytes, @some: Holds(0 < Bytes/len(bytes)))`.
- **A structure's fields are a telescope.** `StructDecl::arity` is the parameters ending in the fields, and a concept is a structure whose `use` fields are its superclass edges.
- **Core's marks.** A telescope's every entry is a type under a mark (`curios-core/src/scope/telescope.rs`): a `FuncType`, a `Func` and an `InductParam` are their telescopes and nothing beside, and a declaration's parameters and a concept's `use` fields stand under their marks in its arity. What a concept's edge reaches is read off its field's elaborated type where the concept's fields are elaborated (`superclass_targets`, `curios-elab/src/resolve.rs`), so an alias of a concept application is an edge. The marks an author wrote ride on what was written: a field of each `Apply` argument, and a sealed vector beside an `InductArm`'s binders.

## The gap

| | function parameter | constructor payload | structure field | concept edge |
| --- | --- | --- | --- | --- |
| declared hidden | yes | `@` | refused | `use` |
| left out where filled | yes | yes | written at every literal | yes |
| left out where opened | yes | no | takes a position | no |
| in scope there unnamed | yes | yes | yes, one level down | only as a `use` member's own |

Read from the code and from probes of the compiler at `87189c97d`, the counts over `curios-text/std` at that commit:

- **An arm writes every hidden payload.** `elaborate_induct_match` (`curios-elab/src/elaborate/match_.rs`) checks an arm's binder count against the payload's, then each mark, exactly: `| push(head, tail) => …` over `push(@n: Nat, head: T, tail: Sized(T)(n))` reports `constructor 'push' takes 3 argument(s) but the match arm binds 2`. 26 arms write a hidden payload, 18 of them only `@_`.
- **A structure's field takes no mark** (`FIELD_TAKES_NO_MARK`, `curios-text/src/parse/marks.rs`), so the bound a payload fills by itself is a field written at every literal. `/std/Str/At`'s ten literals write twenty proofs; 10 of `/std`'s 77 structures carry a proof field.
- **Reading a concept value counts its edges.** Over `concept Over(A: Type) { use Base(A), over(A) -> Str }`, `let Over { over } = dict;` binds `over` to the `Base(A)` edge, and `dict.0` is the edge, where a literal leaves the edges out of its positional sequence. A `use _` written in that pattern reports a missing `}`.
- **A bound a fact in scope states outright is filled only where it is arithmetic.** `entail` (`curios-elab/src/entailment.rs`) reads `Nat` and `Int` comparisons, so over `opaque(b: Bool, @Holds(b))`, `let same(b: Bool, @Holds(b)) -> Nat = opaque(b);` reports `nothing discharged Holds(b)`. Seven of `/std/Str/At`'s ten `boundary` proofs are a hypothesis, another value's field or a lemma.
- **The kernel's conversion compares marks none of its rules checks**: those of an application's arguments and of an arm's binders ([Soundness: findings](../01-soundness/00-findings.md)).

## Prior art

- **A mark lives on its binder.** Agda's telescope is `Tele (Dom Type)`, each `Dom` holding its `ArgInfo` ([`Agda.Syntax.Internal`](https://github.com/agda/agda/blob/master/src/full/Agda/Syntax/Internal.hs)); Lean's `lam` and `forallE` each carry a `binderInfo` ([`Lean.Expr`](https://github.com/leanprover/lean4/blob/master/src/Lean/Expr.lean)).
- **A pattern is the form that builds the value.** Lean's reference has patterns "a subset of the terms" ([Pattern matching](https://lean-lang.org/doc/reference/latest/Terms/Pattern-Matching/)); Rocq's has implicit arguments "omitted in patterns" by default, as in terms ([`match.rst`](https://github.com/rocq-prover/rocq/blob/master/doc/sphinx/language/extensions/match.rst)); Agda's left-hand-side checker inserts a wildcard for each hidden pattern left out, by the `insertImplicit` its application arguments are aligned by ([`Rules.LHS.Implicit`](https://github.com/agda/agda/blob/master/src/full/Agda/TypeChecking/Rules/LHS/Implicit.hs)).
- **An invariant on data is checked where the value is built and assumed where it is read.** Liquid Haskell turns a refined constructor into a smart constructor ([Refined datatypes](https://ucsd-progsys.github.io/liquidhaskell-tutorial/Tutorial_05_Datatypes.html)); Why3 assumes a type's invariant at function entry and reads a field without restoring it ([Syntax reference](https://www.why3.org/doc/syntaxref.html)).
- **A dictionary in data is the contested case.** GHC stores one at construction and extracts it on a match, and forbids matching such a constructor in a `let` and deriving over it ([Existential quantification](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html)); Scala 3 has the pattern write `given` ([Pattern-bound givens](https://docs.scala-lang.org/scala3/reference/contextual/more-givens.html)).

## Decisions

1. **A hidden member takes no position.** Wherever an author counts — a call's arguments, a lambda's binders, a literal's entries, an arm's binders, a positional pattern field, a written `.N` — the count is of plain members, and a hidden one is written in its run, under its mark, only to be named.
2. **A declaration's marks are its telescopes' entries'.** A mark lives in its telescope's entry and nowhere beside it: a declaration's parameter marks are its arity's, and an index telescope, as a tuple type, is built all-plain.
3. **An application's and an arm's marks are checked where they are typed.** The kernel's application rule holds each argument's mark to its binder's and its match rule each arm's to the payload's, and conversion reads them in neither checker.
4. **A concept's edge is a `use` field of its telescope.** Its position is the field's mark, and what it reaches is read from the elaborated field type where the concept's fields are elaborated, so an alias of a concept application is an edge where it is already a premise. The superclass graph is checked as each concept's edges are recorded, so a cycle is refused where its last edge arrives.
5. **Every site is one of three walks.** A telescope is declared, each domain checked and its member entered by its mark; filled, written members aligned and the rest supplied by mark; or opened, written binders aligned and the rest bound by mark. One function enters a member, and `align` is the one alignment.
6. **An arm aligns in the elaborator.** `elaborate_induct_match` aligns an arm's written binders to the payload and binds each hidden slot left out, hintless. Lowering groups rows by constructor and holds a group's rows to one shape, a check on the text alone, so rows of one constructor write the same hidden members until [typed patterns](01-typed-patterns.md) aligns each row.
7. **A written position is resolved as a label is.** A projection's field is a slot in every elaborated term; a written `.N` and a positional pattern field are a position among plain members, which `project` resolves to the slot once. A struct pattern takes no mark: a hidden field is read by its label.
8. **A structure's field declares `@`**, as `@label: type` or its type alone. A literal leaves it out, and it is filled as a call's omitted `@` argument is; `@label = value`, or `@value` in its run, writes it, and `@_` holds its place. A spread re-infers it and never copies it, and where it cannot be re-inferred the literal is refused, naming the proposition. It takes no part in a derived witness, and `Spell` leaves it out.
9. **A bound a fact in scope states is filled by the fact.** Before its arithmetic search, `entail` puts to the bound each hypothesis and each proof field one level down whose decision converts with the bound's, first in scope order.
10. **Waiting for a consumer.** `use` on data, a payload or a field: no type of `/std` or `programs/` holds a dictionary, a type declared under a premise names its dictionary in its type, and a dictionary packed by hand is a plain payload passed on with `use value`. `@` on a concept's field, and any mark on a tuple type's field. Each is refused by its own rule.

## Stages

1. **A written mark is checked where it is typed.** Decision 3. Check: an argument and an arm binder under another mark than its slot's are each refused by the kernel, and a fixture puts two applications differing in a mark alone to both checkers' conversion.
2. **One declare, one fill, one open.** Decision 5: the lambda's walk and the spread's call `align`, and every site enters a member through one function. No verdict moves.
3. **An arm opens by the rule.** Decision 6. Check: `push(head, tail)`, `refl()`, and `chunk(bytes)` with its bound a fact in the arm; each misalignment by its wording; a group of differing rows refused as today.
4. **A written position counts plain members.** Decision 7. Check: over a concept value `.0` is the first method and `Over { over }` binds it; a mark in a struct pattern is refused by its rule.
5. **A structure's field declares `@`.** Decision 8, through the grammar, the printer and the editors. Check: a fixture per form, the spread both ways, a `Spell` round trip, and a hidden field ahead of a plain one read by pattern and by position.
6. **A bound a fact states is filled by it.** Decision 9. Check: an opaque `Holds(b)` under `@Holds(b)`, a field one level down, and a refusal two levels down.
7. **`/std` reads by the rule.** The arms that write only `@_` drop it, and a structure's proof field becomes `@` where a literal sheds a written proof by it.

## Verification

- Every acceptance criterion above is a fixture, the refusals by their wording.
- `/std`, `programs/` and the test corpus compile at every stage; a program whose verdict changes other than as its stage states is a finding, never a fixture update.
- `cargo xboard`, and `cargo xtask clippy`, whose prelude build elaborates, erases and certifies all of `/std`.
- `curios format` is the identity on every new form, and the grammar's corpus holds each.

## Rejected

- **Padding a group's rows in lowering**, so that rows of one constructor may differ now: the padding is discarded the day typed patterns aligns each row where its signature is known.
- **Leaving arms to typed patterns.** An arm reaches the elaborator with its written marks, and the payload's are there; only a group of differing rows waits.
- **One sealed pair of a telescope and its marks**, behind one door: the correspondence is asserted where it could be unspellable, and a nested arity still needs two vectors. **A telescope generic in its annotation**: a tuple type could then spell no mark, at the price of a type parameter through every function the two checkers share between a tuple's fields and a structure's. **A mark on the binder's label**: a label is no part of a scope's identity, and a rebuild that re-mints its binders drops one.
- **Counting plain members in `project` with no written form.** An elaborated term is elaborated again, and a slot would be read as a position.
- **A struct pattern held to a written mark.** A hidden member takes no position there and a named one is read by its label, so no pattern has a mark to write.
- **A spread that falls back to the base's `@` field** where re-inference fails: one literal elaborated two ways by whether a search succeeded.
- **A search over the facts for a bound that is no comparison** — congruence, or a fact's consequences. A fact that states the bound is found by conversion alone, and anything further is a search with no fragment to be complete for.
- **A `use` field on a structure while a `use` payload is admitted**, or the reverse: a one-constructor family is that structure, so the two are one decision.

## Completion and retirement

Done when no site restates, ignores or refuses a mark its telescope declares, but for what decision 10 names. A design decision titled by decision 1 records the rule, its rationale and what is rejected here; [Plicity is part of function identity](../../design/theory/plicity-is-part-of-function-identity.md) drops its rejection of pattern insertion and links to it; [Telescope instantiation](../../design/soundness/introduction/telescope-instantiation.md) states what its plicity clause rests on; [A bound that follows from the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) takes decision 9; and `documentation/syntax.md` takes every surface form. This spec then shrinks to decision 10, and its roadmap entry to a checked summary beside what waits.
