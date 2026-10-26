# A character changes case as Unicode maps it

**Not refined yet.** This specification reserves case mapping, case folding and the case properties of the Unicode Character Database over `Char` and `Str`. It is not an implementation plan. No module of `/std` consumes it yet: `http` compares field names without case as RFC 9110 §5.1 reads them, over ASCII, and its first consumer would be a program that matches or displays text a person wrote.

## What is missing

- **Case is ASCII's alone.** `Char/to_ascii_lower` and `Char/to_ascii_upper` (`curios-prelude/std/Char.crs`) change `A`–`Z` and `a`–`z` and leave every other character, and nothing in `Char` or `Str` maps, folds or compares the case of any other: `é` has no uppercase in `/std`, and `Straße` and `STRASSE` are no caseless match.
- **The case predicates are ASCII's alone.** `Char/is_ascii_lower`, `is_ascii_upper`, `is_ascii_alpha` and `is_ascii_alphanumeric` test `a`–`z`, `A`–`Z` and the digits, and no predicate reads what the database states of case.
- **The database the toolchain pins holds no mapping.** `curios-unicode` reads five files of Unicode 18.0.0 — `EastAsianWidth.txt`, `emoji/emoji-data.txt`, `auxiliary/GraphemeBreakProperty.txt`, `DerivedCoreProperties.txt` and `auxiliary/GraphemeBreakTest.txt` — and keeps of `DerivedCoreProperties.txt` only `InCB` and `Default_Ignorable_Code_Point`, though the same file states `Lowercase`, `Uppercase` and `Alphabetic`. The mappings are in files it does not read: `UnicodeData.txt` (the simple ones), `SpecialCasing.txt` (the full and the conditional ones) and `CaseFolding.txt`.

## What is known

- **The algorithms are the standard's.** The Unicode Standard's default case algorithms (§3.13) define `toUppercase`, `toLowercase`, `toTitlecase` and `toCasefold` over a string, and caseless matching as equality of the folds. A full mapping can lengthen a string, `ß` to `SS`; one mapping depends on context, a final `Σ` lowering to `ς`; and the Turkish and Lithuanian ones depend on a language, which no value of `/std` names.
- **`/sys/unicode` carries classes, and a mapping is no class.** A packed table is runs of seven bytes, a range's first scalar, its last and its class's code (`curios-unicode/src/packed.rs`), which `Char`'s private `unicode` module searches. A mapping takes a scalar to another, or to several, so a run would carry a difference from its range, as Go's [`unicode.CaseRanges`](https://pkg.go.dev/unicode#CaseRanges) do, with a mark for the ranges where upper and lower alternate.
- **Peers split between simple and full.** Rust's [`char::to_uppercase`](https://doc.rust-lang.org/std/primitive.char.html#method.to_uppercase) yields one to three characters and `str::to_lowercase` handles the final sigma; Go's [`unicode.ToUpper`](https://pkg.go.dev/unicode#ToUpper) maps one rune to one, with `unicode.SpecialCase` for Turkish; Python's [`str.upper`](https://docs.python.org/3/library/stdtypes.html#str.upper) is the full mapping and `str.casefold` the full folding; Haskell's `Data.Char.toUpper` is the simple mapping and `Data.Text.toUpper` the full one; Lean 4's `Char.toUpper` changes ASCII alone.

## Refinement

- **Which mappings.** Simple mappings on `Char`, full ones on `Str`, folding for caseless comparison; whether title case is among them; and whether the final sigma's context is honoured, which reads the characters around the one mapped.
- **Language.** Whether the Turkish and Lithuanian mappings are offered at all, with no locale in `/std` to choose them.
- **The predicates.** `is_lower`, `is_upper` and `is_alpha` are free for the database's `Lowercase`, `Uppercase` and `Alphabetic`, the ASCII ones keeping `ascii` in their names; which of the database's other properties join them.
- **The tables.** How `/sys/unicode` carries a mapping, and what the tables add to the archived prelude and to every program that maps case.
- **Normalization.** Whether caseless matching stays the folds' equality, or reaches canonical caseless matching, which needs normalization and is a capability of its own.

It retires when `Char` and `Str` map and fold case as the pinned database states, and `Char` answers the case properties under their plain names.
