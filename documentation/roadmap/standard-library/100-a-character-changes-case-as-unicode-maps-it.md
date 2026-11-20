# A character changes case as Unicode maps it

Adopted on 2026-11-19, since `/std` already writes a name in upper case and gets it wrong: `Char` and `Str` map and fold case as the database the toolchain pins states, by the Unicode Standard's default case algorithms (§3.13), and `Char` answers the case properties under their plain names, so a name a program writes in any script is shouted, lowered and compared as the standard does — and what the compiler reads of a character and what a program reads of it stay one table.

## What is missing

- **The case predicates are ASCII's alone, but for `Cased` and `Case_Ignorable`.** `Char/is_ascii_lower`, `is_ascii_upper` and `is_ascii_alpha` test `a`–`z` and `A`–`Z`, and `Char` answers none of the database's `Lowercase`, `Uppercase` and `Alphabetic`.

## Settled

- **The full mappings, and no simple ones.** `Char/to_upper`, `Char/to_lower` and `Char/to_casefold` answer the database's `Uppercase_Mapping`, `Lowercase_Mapping` and full `Case_Folding` of one character as a `Str`; `Str/to_upper`, `Str/to_lower` and `Str/to_casefold` are the default algorithms `toUppercase`, `toLowercase` and `toCasefold` over text; and `Str/eql_ignoring_case` is default caseless matching (D144), the equality of the folds, beside `eql_ignoring_ascii_case`. A one-to-one mapping would be a second answer to what a character's case is: Rust's `char::to_lowercase` and Python's `str.casefold` answer the full mapping alone.
- **No language's tailoring.** The Turkish, Azeri and Lithuanian mappings are conditioned on a language, which no value of `/std` names; the default algorithms are the language-independent ones.
- **No title case.** `toTitlecase` titlecases the first cased character after each word boundary, which needs UAX #29's word boundaries, which `/std` does not segment; Go deprecated `strings.Title` for drawing them by its own rule.
- **Caseless matching is the folds' equality.** Canonical caseless matching (D145) normalizes before and after folding, and normalization is a capability of its own.
- **Each mapping is packed and proved alike.** Each is a table of the records [`curios-unicode` packs a mapping in](../../../curios-unicode/README.md#a-mapping-is-packed-as-records-whose-first-target-moves-with-the-scalar), held by its own `every_record_lands` fact in `Char` and read through `/std/Char`'s `unicode/mapping`. A fact costs about a second of `/std`'s build a table, linear in its records: timed at `ef6a83175`, single readings of `curios run -` over a program holding the 271 records of upper case, 1.1 s with the fact against 0.1 s without, and 2.1 s and 4.1 s for the table doubled and quadrupled.
- **A table of the database's is held to the database at every record's edges**, by `curios-prelude`'s `the_library_maps_case_as_the_database_does_at_every_record_s_edges` for a mapping and `the_library_reads_every_property_as_the_database_does_at_every_run_s_edges` for a property, each against what `curios-unicode`'s `test_support` reads off the database's rows.

## Stages

1. **The case predicates**, `Lowercase`, `Uppercase` and `Alphabetic` answered as `Char/is_lower`, `Char/is_upper` and `Char/is_alpha` — `Alphabetic` the table the identifier rule already reads. Check: each at every run's edges, as the existing properties are.

## Next step

A ready delivery: stage 1.

## Retirement

It retires when `Char` and `Str` map and fold case as the pinned database states, and `Char` answers the case properties under their plain names.
