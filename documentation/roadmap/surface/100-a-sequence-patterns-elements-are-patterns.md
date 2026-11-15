# A sequence pattern's elements are patterns

Its benefit is beyond doubt: `/std` writes a run of elements, a case of an element or a byte's value as a match inside an arm at thirteen sites, each of which a [run](../../language.md#list-case-split) now spells but a byte's value, which no pattern can write (*What is missing*). Its approach is the one *Settled* states: a byte takes literal patterns as a natural does, and `/std` writes its runs as patterns. Its next step is stage 1, a ready delivery.

## What is missing

**A byte takes no literal pattern.** A numeral realizes at `Byte`, and the packed literal that builds a value writes its constant bytes as numerals and characters, `x['%', ..rest]`, but no row can write a case of a `Byte` column (`naturals` in `curios-elab/src/elaborate/matrix.rs` resolves a literal's column as a `Nat`), so a byte is dispatched as the natural `Byte/to_nat` gives. `/std/http/Url`'s `decode_at` writes `| x[byte, ..after] => match Byte/to_nat(byte) | '+' => … | '%' => match after | x[] => … | x[high, ..low_on] => match low_on | x[] => … | x[low, ..onward] => … end end | _ => … end`, four matches for the cases `x['+', ..after]`, `x['%', high, low, ..onward]`, `x['%', .._]` and `x[byte, ..after]`.

**`/std` writes its runs as matches inside arms.**

- **Runs.** `/std/List/last` writes `| [h, ..t] => match t | [] => Option/some(h) | _ => last(t) end`, where the run is `| [h] => Option/some(h) | [_, ..t] => last(t)`; `/std/Cli/parse`'s `read_field`, `/std/Html/parse`'s `content`, `/std/Toml/build`'s `walk` and `/std/http/Request`'s `host_of` take a tail apart the same way, and `decode_at` a packed one two elements deep.
- **Elements.** `/std/List/sort_by`'s stack of runs writes `| [top, ..rest] => match top | none() => … | some(older) => … end` at seven sites, in `push`, `flush` and the proofs about them.

## Settled

- **A byte takes literal patterns.** A `Byte` column whose rows write a numeral or a character is a dispatch the match compiler writes on `Byte/to_nat` of the column, held to a natural dispatch's rules: its literals are a byte's, a numeral from `0` through `255` or a character whose code point fits one, as a packed literal's constant entries are; `0` is a literal, a byte having no successor, and `p + 1` is refused; a row that writes a binder takes every other value, and where none does the match is refused with the least byte no row writes. Core and the certifier hold nothing new: the dispatch is a `Switch` on `Byte/to_nat(column)`. A `Char` keeps its dispatch through `Char/to_nat` at the head, as [A literal is realized by its expected type](../../design/surface/a-literal-is-realized-by-its-expected-type.md) states.

## Rejected

- **A byte dispatch closed by its literals.** Core's dispatch is a natural's, whose default always stands, so a match that writes all 256 bytes still writes a binder; closing it would put `Byte/to_nat`'s bound to the certifier, and no program writes every byte.

## Stages

1. **A byte's literals.** The matrix compiler splits a `Byte` column of literals as a dispatch on `Byte/to_nat`, a byte's literals held to its range, in the tree and in a row's own walk; [Natural-number dispatch](../../language.md#natural-number-dispatch) and [Packed case split](../../language.md#packed-case-split) state it, and [A literal is realized by its expected type](../../design/surface/a-literal-is-realized-by-its-expected-type.md) says a literal pattern realizes at its column's type. Check: `match b | 0x0A => … | _ => … end` and `x['%', h, l, ..r]` accepted and run; `256`, a character past a byte and `p + 1` refused at a byte; a byte dispatch with no binder refused with its least missing byte; the certifier's verdict on each; `/std` elaborates to the Core it does today.
2. **`/std` writes them.** The thirteen sites above written with runs and element patterns, `decode_at` with byte literals. Check: `curios test` over the subjects touched; `/std`'s elaborated case trees compared with the ones before, each difference read.

## Retirement

A byte's literal patterns go to [Natural-number dispatch](../../language.md#natural-number-dispatch) and [Packed case split](../../language.md#packed-case-split), their rule to [A literal is realized by its expected type](../../design/surface/a-literal-is-realized-by-its-expected-type.md) and [Patterns are compiled where their types are known](../../design/surface/patterns-are-compiled-where-their-types-are-known.md) with the rejected closing of a byte dispatch; nothing cites this file, and it is deleted.
