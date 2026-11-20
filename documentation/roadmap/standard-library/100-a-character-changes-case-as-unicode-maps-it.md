# A character changes case as Unicode maps it

Adopted on 2026-11-19, since `/std` already writes a name in upper case and gets it wrong: `Char` and `Str` map and fold case as the database the toolchain pins states, by the Unicode Standard's default case algorithms (§3.13), and `Char` answers the case properties under their plain names, so a name a program writes in any script is shouted, lowered and compared as the standard does — and what the compiler reads of a character and what a program reads of it stay one table.

## What is wrong today

- **`/std/Cli` shouts a name in ASCII alone.** Help and a refusal write a positional's name in upper case through `shouted` (`curios-prelude/std/Cli.crs`), which maps each character by `Char/to_ascii_upper`: a command whose positionals are `année` and `straße` prints `Usage: cat <ANNéE> [STRAßE]`, and a missing `année` is refused as `ANNéE` (`Cli/parse.crs`), at `ef6a83175`, where the full mapping writes `ANNÉE` and `STRASSE`. Every other ASCII case `/std` reads is ASCII by its standard: `http` compares field names by RFC 9110 §5.1, `Flt/grammar` reads `inf` and `nan`, and `Url` writes a hex digit.

## What is missing

- **Case is ASCII's alone.** `Char/to_ascii_lower` and `Char/to_ascii_upper` change `A`–`Z` and `a`–`z` alone, and nothing in `Char` or `Str` maps, folds or compares the case of any other character: `é` has no uppercase in `/std`, and `Straße` and `STRASSE` are no caseless match.
- **The case predicates are ASCII's alone.** `Char/is_ascii_lower`, `is_ascii_upper` and `is_ascii_alpha` test `a`–`z` and `A`–`Z`, and no predicate reads what the database states of case.
- **`/sys/unicode` carries classes, and a mapping produces characters.** A packed table is runs of seven bytes, a range and its class's code (`curios-unicode/src/packed.rs`), and `Char`'s private `unicode` module answers a code from one. A mapping answers characters, and [`/std` invents no value where a proof belongs](../../../curios-prelude/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position): each code a table hands back is a `Char` only with the proof that it is a scalar value, which a lookup cannot decide at run time without an arm for a code that is not.

## Settled

- **The full mappings, and no simple ones.** `Char/to_upper`, `Char/to_lower` and `Char/to_casefold` answer the database's `Uppercase_Mapping`, `Lowercase_Mapping` and full `Case_Folding` of one character as a `Str`, `ß` to `SS`; `Str/to_upper`, `Str/to_lower` and `Str/to_casefold` are the default algorithms `toUppercase`, `toLowercase` and `toCasefold` over text; and `Str/eql_ignoring_case` is default caseless matching (D144), the equality of the folds, beside `eql_ignoring_ascii_case`. A one-to-one mapping would be a second answer to what a character's upper case is, and the one `shouted` gets wrong today: Rust's `char::to_uppercase` and Python's `str.upper` answer the full mapping alone.
- **The final sigma is honoured.** `Str/to_lower` lowers `Σ` to `ς` where `Final_Sigma` holds — after a cased character and any case-ignorable ones, and before no cased one past any case-ignorable ones — as Rust's `str::to_lowercase` does; `Char/to_lower` has no context and answers `σ`. It reads `Cased` and `Case_Ignorable`, which `Char` answers as `is_cased` and `is_case_ignorable`.
- **No language's tailoring.** The Turkish, Azeri and Lithuanian mappings are conditioned on a language, which no value of `/std` names; the default algorithms are the language-independent ones.
- **No title case.** `toTitlecase` titlecases the first cased character after each word boundary, which needs UAX #29's word boundaries, which `/std` does not segment; Go deprecated `strings.Title` for drawing them by its own rule.
- **Caseless matching is the folds' equality.** Canonical caseless matching (D145) normalizes before and after folding, and normalization is a capability of its own.
- **A mapping is a table of records, and its codes are proved by a fact reduction decides once.** `curios-unicode` packs each mapping as records of seventeen bytes — the first scalar, the last, the step between the scalars it maps, how many characters each becomes, and three targets — where each scalar `first + k·step` up to `last` becomes the first target moved by `k·step`, then the second and third as written. `Char` holds, of every record of each table, that the characters it produces are scalar values, as a `Bool/Holds(Nat/all_below(…))` reduction decides when `/std` is built, as `Map/digits` holds its table; a lookup reads its record's fact with `Nat/all_below/at` and builds each `Char` from it through the range lemma `Str/utf8` states today, `scalars` and `scalars_within`, which moves to `Char/Valid` for both to read, so no arm stands for a code that is not one. The fact costs about a second of `/std`'s build a table, linear in its records: timed at `ef6a83175`, single readings of `curios run -` over a program holding the 271 records of upper case, 1.1 s with the fact against 0.1 s without, and 2.1 s and 4.1 s for the table doubled and quadrupled.
- **A table of the database's is held to the database at every record's edges.** `curios-unicode`'s `test_support` hands `curios-prelude`'s tests the mappings and properties as the database states them, and a test compares `/std`'s answers with them at every record's first and last scalar and those just outside, as `the_library_reads_every_property_as_the_database_does_at_every_run_s_edges` does today.

## Stages

1. **Upper case.** `UnicodeData.txt`'s simple mappings and `SpecialCasing.txt`'s unconditional ones packed as records, carried by `/sys/unicode` as `upper`, with the record shape `curios-unicode` states; `Char/to_upper` and `Str/to_upper`; `Cli`'s `shouted` restated over `Str/to_upper`. Check: `année` and `straße` shouted `ANNÉE` and `STRASSE` in `Cli`'s render and parse tests; `/std`'s mapping equals the database's at every record's edges; the archive's and a mapping program's growth named.
2. **Lower case**, `lower` packed, `Cased` and `Case_Ignorable` kept and answered as `Char/is_cased` and `Char/is_case_ignorable`, `Char/to_lower` and `Str/to_lower` with the final sigma. Check: `ΟΔΟΣ` lowered to `οδος`, `Σ` alone to `σ`, `ΣΑ` to `σα`, `Σ'` after a letter to `ς'`; edges as stage 1.
3. **Folding and caseless comparison**, `CaseFolding.txt`'s common and full foldings packed as `fold`, `Char/to_casefold`, `Str/to_casefold` and `Str/eql_ignoring_case`. Check: `Straße` and `STRASSE` caselessly equal, `http`'s ASCII comparison unchanged; edges as stage 1.
4. **The case predicates**, `Lowercase`, `Uppercase` and `Alphabetic` answered as `Char/is_lower`, `Char/is_upper` and `Char/is_alpha` — `Alphabetic` the table the identifier rule already reads. Check: each at every run's edges, as the existing properties are.

## Next step

A ready delivery: stage 1.

## Retirement

It retires when `Char` and `Str` map and fold case as the pinned database states, and `Char` answers the case properties under their plain names.
