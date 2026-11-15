# A sequence pattern's operands are patterns

Its benefit is beyond doubt: `/std` writes a run of elements, or a case of an element, as a match inside an arm at fourteen sites, and a run written as a pattern is refused, a flat one in words about another pattern (*What is missing*). Its approach is the one *Settled* states: a cons pattern's head and tail are patterns, and a run nests. Its next step is stage 1, a ready delivery.

## What is missing

**A sequence pattern takes one element apart and binds what it takes.** A `List` or `Bytes` pattern is `[]` or `[head, ..tail]`, and both operands are binders (`curios-surface/src/parse/patterns.rs`, [Inductive patterns](../../language.md#inductive-patterns)), so a run, or a case of an element, is a match inside the arm.

- **Runs.** `/std/List/last` writes `| [h, ..t] => match t | [] => Option/some(h) | _ => last(t) end`; `Cli/parse`, `Html/parse`, `Toml/build`, `http/Framing` and `http/Request` take a tail apart the same way, and `http/Url` a packed one.
- **Elements.** `/std/List/sort_by`'s stack of runs writes `| [top, ..rest] => match top | none() => … | some(older) => … end` at seven sites, in `push`, `flush` and the proofs about them.

**A run is refused, and a flat one in another pattern's words.** `match l | [] => 0 | [a, ..[]] => a | [a, ..t] => f(t) end` is refused at `..[]` with "the head and the tail of a `[head, ..tail]` pattern are binders, never patterns", and `[a]` in the same words, which describe a pattern it does not write.

**A dispatch is written as a chain of comparisons.** Seven headless matches compare one subject with `==` against literals, beside ranges and other conditions, where a dispatch would hold the literals to the rule it holds its arms to, a literal written twice refused as unreachable: `Tui/Input/decode`'s `control`, its key table, its CSI reader and its escape reader, a tag's opening in `Html/parse`, the CSI reader of `Tui/tests/render`, and a duration's unit in `time/iso8601`. A range stands in a dispatch as a guarded arm after the literal arms, `| c, Nat/in_range(c, lo, hi) => …`, and no literal of the seven stands after a range that holds it, so the order the chain tries them in is the dispatch's.

## Settled

- **A head and a tail are patterns, and a run nests.** A sequence pattern is `[]` or `[head, ..tail]`, as now, its head a pattern at the element's type and its tail a sequence pattern, where each is a binder or a wildcard today. A run of one is `[a, ..[]]`, of two `[a, ..[b, ..[]]]`, two and a rest `[a, ..[b, ..t]]`, and `[none(), ..rest]` holds a pattern in its head. No flat form is added, so each pattern has the one spelling Core's written match holds, and the grammar changes only in what stands in an operand; `[a]` is refused where its tail is expected, as any pattern missing one is. A packed pattern nests the same way under `x[`, as `x[h, ..x[]]`; its head is a `Byte`, which takes no literal pattern, a byte being dispatched through `Byte/to_nat` at the head, so it stays a binder or `_`.
- **The matrix compiler peels as it does.** It takes one element apart a column (`peel`, `curios-elab/src/elaborate/matrix.rs`), so a head and a tail are columns of their own, as a variant's payload is, and coverage, the unreachable-arm rule and the rule that arms have no priority read them as they read a payload. Each element a pattern writes is one peel, so a pattern costs what it spells.
- **What the checkers read does not change.** Core's written match (`curios-core/src/term/matrix.rs`) is the form lowering hands elaboration and never survives it, so its `Pattern::Cons` and `Pattern::Atom` holding patterns where they hold a `Slot` reaches the surface parser and printer, the lowering (`curios-module/src/into_core/lowerer/match_compile.rs`), elaboration, Core text (`core_text/matching.rs`, `transient.rs`), `from_core`, and the editors' grammar (`editors/grammar/base/grammar.js`) with the Zed pin, and the certifier is handed the nested case splits it reads today.
- **A natural's predecessor stays a binder.** A `Nat` pattern is `0`, `p + 1` or a dispatch literal, as now.

## Rejected

- **Flat runs, `[a]`, `[a, b]` and `[a, b, ..t]`.** Each is a second spelling of a nested pattern the operands being patterns already admit, and a second spelling is a second form for the printer, the formatter and the grammar to choose between.
- **A successor step past one, `k + c`, with literals beside it.** Its one use in `/std` is the count of continuation bytes the UTF-8 decoders peel, three functions, and to stand beside the literals they write it needs a literal peeled one step at a time in a column that steps, as Lean's match compiler peels one, which makes what a match costs depend on a literal's value rather than on what it spells: `| 0 | 1000 | n + 1`, refused today, would be a thousand nested splits. Haskell 2010 dropped its `n + k` patterns as a special case of very limited usefulness.
- **A suffix, `[..init, last]`.** It takes a sequence apart from its end, an elimination Core does not have, and so reaches the certifier; the languages whose lists are taken apart from the front, the ML family, Haskell and Lean among them, have none.

## Deferred

- **A literal prefix of a packed sequence.** It waits for a consumer: no `/std` site outside its tests asks for one yet.

## Stages

1. **Core and elaboration.** Core's written `Pattern::Cons` and `Pattern::Atom` hold patterns for their head and tail, and the lowering, the matrix compiler, Core text and `from_core` follow. Check: Core-text fixtures of the nested forms read and written again; the matrix compiler's suites; a run or an element case its arms do not cover refused as a missing case; the workspace lint and tests.
2. **The surface.** The parser, the printer and the formatter take a pattern in a head's and in a tail's place, with their packed twins; the editors' grammar and the Zed pin follow; [Inductive patterns](../../language.md#inductive-patterns) states the rule. Check: `[a, ..[]]` accepted where it is refused today, and `[a]` refused where its tail is expected; the formatter's round trip over nested runs and element patterns; the grammar steps and `cargo xtask zed test`.
3. **`/std` writes them.** The fourteen sites above written with runs and element patterns, and the seven chains written as dispatches. Check: `curios test` over the subjects touched; `/std`'s elaborated case trees compared with the ones before, each difference read.

## Retirement

The syntax goes to [Inductive patterns](../../language.md#inductive-patterns), and the rule with what it rejects to [Patterns are compiled where their types are known](../../design/surface/patterns-are-compiled-where-their-types-are-known.md); nothing cites this file, and it is deleted.
