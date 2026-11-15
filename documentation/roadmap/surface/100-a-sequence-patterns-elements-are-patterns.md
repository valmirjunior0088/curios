# A sequence pattern's elements are patterns

Its benefit is beyond doubt: `/std` writes a run of elements, a case of an element or a byte's value as a match inside an arm, or reads it at fixed offsets, at seventeen sites, each of which a [run](../../language.md#list-case-split) or a [byte's literal](../../language.md#natural-number-dispatch) now spells (*What is missing*). Its approach is the one *Settled* states. Its next step is stage 1, a ready delivery.

## What is missing

**`/std` takes its sequences apart a match at a time.**

- **Runs.** `/std/List/last` writes `| [h, ..t] => match t | [] => Option/some(h) | _ => last(t) end`, where the run is `| [h] => Option/some(h) | [_, ..t] => last(t)`; `/std/Cli/parse`'s `read_field`, `/std/Html/parse`'s `content`, `/std/Toml/build`'s `walk` and `/std/http/Request`'s `host_of` take a tail apart the same way, and `/std/Flt/digits`'s `normalize` tests its tail's length.
- **Elements.** `/std/List/sort_by`'s stack of runs writes `| [top, ..rest] => match top | none() => … | some(older) => … end` at seven sites, in `push`, `flush` and the proofs about them.
- **Bytes.** `/std/http/Url`'s `decode_at` dispatches `Byte/to_nat(byte)` and takes its tail apart two elements deep, where its cases are `x['+', ..after]`, `x['%', high, low, ..onward]`, `x['%', .._]` and `x[byte, ..after]`; `/std/http/syntax`'s `version` reads `HTTP/` at fixed offsets with `octets/at`, where `x['H', 'T', 'T', 'P', '/', major, '.', minor]` spells it, and its `indented` reads its first byte by index; `/std/Tui/Input/decode`'s `flush` compares its head with `0x1B`.

## Settled

- **A site states its cases as patterns where a run or a byte's literal spells them**, and keeps what is read at an offset or tested by a guard as it is.

## Stages

1. **`/std` writes them.** The seventeen sites above, by subject. Check: `curios test` over the subjects touched; `/std`'s elaborated case trees compared with the ones before, each difference read.

## Retirement

The syntax and its rule are [Inductive patterns](../../language.md#inductive-patterns), [List case split](../../language.md#list-case-split), [Packed case split](../../language.md#packed-case-split), [Natural-number dispatch](../../language.md#natural-number-dispatch) and [Patterns are compiled where their types are known](../../design/surface/patterns-are-compiled-where-their-types-are-known.md)'s already; nothing cites this file, and once stage 1 lands it is deleted.
