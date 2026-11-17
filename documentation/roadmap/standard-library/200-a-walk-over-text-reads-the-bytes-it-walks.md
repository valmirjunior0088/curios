# A walk over text reads the bytes it walks

This specification makes a walk over text cost what reading its characters costs, in a type and in the erased program alike, where a walk over text builds a position at every character today and a piece of text is matched a character at a time. Its next step is stage 1, a delivery.

## What is missing

**A walk over text builds a position at every character.** `Str/span`, `search/seek` and `search/occurrence` recur through `Str/At/forward`, a descent over positions, and ask `Str/At/try_next` or `skip` for every character they pass: an `Option`, a record and a position each, and the descent's two closures. Counted at `1398dabe8`, a walk over a literal of `n` letters claimed by `Eq/refl()` to reach `n`, read at 32 and 64 letters and divided by the 32 between, each reading the least `--budget` under which `wonder -` accepts the claim: `Str/span` costs 1,319 units a character, a count by `At/try_next` 1,142 and one by `At/skip` 1,005, where `Str/fold` costs 180, and `Parse/bytes/take_while` about 113 a byte. Timed on top of `1398dabe8`, the median and span of five interleaved rounds over about 13 MB of mixed-width text built from the command line, on a machine in use: a walk by `At/try_next` takes 0.719 s [0.704–0.719], `Str/span` 0.626 s [0.622–0.631] and `Str/fold` 0.103 s [0.101–0.107], building the text included. The fold's loop carries what is left of its text as a window, `(base, end, length)`, and allocates nothing; a walk over positions cuts the rest of its text from the whole at every step.

**A step builds the position past twice.** `Str/At/skip` builds a position in the text after `at` and `of_after` builds it again in `s`; `Str/At/next` cuts the rest and reads the character's width itself and once more through `skip`.

**A piece of text is matched a character at a time.** `Str/follows` — through it `Parse/str/literal`, `strip_prefix` and `starts_with` — and `search/meets_back` — `strip_suffix` and `ends_with` — decode both sides at every character, and `search/occurrence` — `index_of_substr`, `has_substr`, `split`, `split_once`, `replace`, `lines` and `Parse/str/until` — confirms each candidate through `meets`. Counted the same way, `Str/follows` costs 1,360 units a character.

**`/std`'s rule for a walk and its walks over text part.** `curios-prelude/README.md`'s first decision states that "a walk stops where its predicate does and is total: it recurses on the bytes it reads and answers how many it took", which `Parse/bytes`, `Str/fold` and `Fmt/parse` do, and in the same paragraph that "a walk over text recurses over positions (`Str/At/forward`, `backward`)", which `Str`'s own walks do.

## What is known

- **UTF-8 lets bytes answer for characters.** Each character has one encoding, and no encoding begins inside another's, so valid text holds a valid piece's characters exactly where it holds its bytes, at a boundary, and goes on past them as valid text. Rust compares a `&str` pattern's bytes (`is_prefix_of` is `haystack.as_bytes().starts_with(self.as_bytes())`) and searches by Two-Way over bytes, its searcher noting that "TwoWaySearcher produces valid *Match* indices that split at char boundaries as long as it does correct matching and that haystack and needle are valid UTF-8" ([`core::str::pattern`](https://doc.rust-lang.org/src/core/str/pattern.rs.html)); Lean 4 searches a `String.Slice` by KMP over its UTF-8 bytes and verifies it ([`Init.Data.String.Pattern.String`](https://lean-lang.org/doc/api/Init/Data/String/Pattern/String.html)); `text` 2 searches its UTF-8 array of `Word8` by Boyer–Moore–Horspool ([`Data.Text.Internal.Search`](https://hackage-content.haskell.org/package/text-2.1.2/docs/src/Data.Text.Internal.Search.html)).
- **A position is an offset and a proof of validity elsewhere too.** Lean 4 completed in 4.27 "the revision of the `String` API, including dependently typed `String.Pos`", a position being "a byte offset in `s` together with a proof that this position is at a UTF-8 character boundary", and removed every use of `String.Iterator` from core in favour of it ([v4.25.0](https://lean-lang.org/doc/reference/latest/releases/v4.25.0/), [v4.26.0](https://lean-lang.org/doc/reference/latest/releases/v4.26.0/), [v4.27.0](https://lean-lang.org/doc/reference/latest/releases/v4.27.0/), [`Init.Data.String.Defs`](https://lean-lang.org/doc/api/Init/Data/String/Defs.html)). Rust's `CharIndices` holds an offset beside a `Chars`, the rest as a slice iterator ([`core::str::iter`](https://doc.rust-lang.org/src/core/str/iter.rs.html)), Megaparsec's `State` the rest beside `stateOffset` ([`Text.Megaparsec.State`](https://hackage.haskell.org/package/megaparsec-9.7.0/docs/src/Text.Megaparsec.State.html)), and Koka's `sslice` a view `next` hands on with the character ([`std/core/sslice`](https://koka-lang.github.io/koka/doc/std_core_sslice.html)): a rest is free to hold where a slice is two words held in place, and a position that holds one here is a row on the heap at every step. What Lean's rework leaves behind is a raw offset that may fall inside a character, where its legacy iterator's character "is `(default : Char)`, similar to `String.get` on an invalid position" ([`Init.Data.String.Iterator`](https://lean-lang.org/doc/api/Init/Data/String/Iterator.html)): the invented value `Str/At`'s boundary has ruled out from the start.
- **A walk the compiler can keep in place is one over the bytes it reads.** `curios-cps` gives a suffix a window only among a loop's parameters (`optimize/fields.rs`), as Erlang keeps one match context for a loop that matches the rest of a binary and builds no sub-binary ([Constructing and Matching Binaries](https://www.erlang.org/doc/system/binaryhandling)).
- **Holding the rest in a position was tried and measured.** On top of `1398dabe8`, `At(s)` holding its offset, the rest as a `Str`, its bound and the cut (the decision of 2026-11-16) checks across `/std` and passes its 316 tests, and costs more on every claim: `Parse/str/take_while` over 32 letters 54,941 units against 50,758, `Str/span` 1,447 a character against 1,319, and a walk by `At/try_next` 0.762 s against 0.721 at about 13 MB, the emitted `Str/At/next` building one row for the position past at every step and still cutting twice. Handing on the tail its decoding cut spared a cut on ASCII and kept the row. A cut is a window in both checkers and in the program wherever it is taken from, so holding the rest spares no work.
- **The decisions below were prototyped.** On top of `1398dabe8`, `/std` with every decision below checks whole and passes its 316 tests; a claim's figure is the least `--budget` its program is accepted under, the parser rows claiming what was read so the read is run, and the last eight `type_level_claim_measurements`' claims:

  | Claim | Today | Prototype |
  | --- | --- | --- |
  | `Parse/str/take_while`, 32 letters | 50,979 | 14,761 |
  | `Parse/str/literal`, 32 letters | 81,404 | 8,014 |
  | `Parse/str/until`, 32 letters | 48,235 | 14,653 |
  | `Str/span`, a character | 1,319 | 222 |
  | `Str/follows`, a character | 1,360 | 69 |
  | a count by `At/try_next`, a character | 1,142 | 1,016 |
  | `Str/split` | 29,766 | 18,633 |
  | `Str/split_once` | 19,015 | 13,139 |
  | `Str/lines` | 27,037 | 15,086 |
  | `Str/replace` | 24,280 | 16,194 |
  | `Str/trim` | 20,364 | 17,411 |
  | `Str/strip_suffix` | 17,374 | 8,642 |
  | `Flt/of_str` | 34,949 | 25,114 |
  | `Str/index_of` | 22,778 | 14,178 |

  Timed as above, today against the prototype: `Str/span` 0.626 s against 0.113 s [0.110–0.115]; `Parse/str/take_while` over the text and then seven million `Parse/str/char` reads 1.281 s against 0.811 s [0.806–0.836]; a million `Parse/str/literal` reads 1.476 s against 0.148 s [0.147–0.346]; `Str/split` at each "→" and a search for an absent needle 11.065 s against 2.401 s [2.333–2.413]. The span's and the search's loops carry a window and allocate nothing per byte, as `Str/fold`'s does; `trim` moves least, its end walked back over positions.

## Decisions

- **A position is an offset and its proofs.** `Str/At` keeps its shape: the bound and the boundary erased, a position erasing to its offset, which is what a cut and a step need.
- **A step builds the position past once.** `skip` writes the position at `at.offset + width` with the boundary `hop` proved of the rest it cut, which conversion reads as the whole's, and `next` cuts and hops once.
- **A walk over text reads the bytes it walks.** `Str/span` is a structural walk over the bytes from its start, which reads each character at its lead byte as `Str/fold` does and answers how many bytes it took. Where it stands rides beside it as proofs alone — its offset within the text and the text from there on — so nothing it passed is carried, and its position is built once, at its end. `search/seek` is the span of the characters its predicate refuses and the character it stops at.
- **A piece of text is matched as bytes.** `follows` compares a piece's bytes with the text's at a position, a suffix is compared where it would begin, and `occurrence` searches for a needle's bytes, testing its lead byte before the rest; a match's two boundaries follow from validity — a valid needle begins with a byte no character continues, and valid bytes that begin with valid bytes go on as valid bytes — and no character is decoded. `search/meets` and `meets_back` go.
- **A walk that stands at positions stays one.** `At/forward` and `backward` remain the recursion of a walk that must stand at a position between characters: a walk back (`back_over`, `At/prev`), which `Bytes` has no pattern to take from its end, and a walk over grapheme clusters, whose breaks read the character before.

Decided on 2026-11-17, in place of the position that held its rest.

## Stages

1. **A step builds the position past once.** `Str/At/skip` and `next`. Check: `/std`'s tests; the counts by `At/try_next` and by `At/skip` read again.
2. **A walk over text reads its bytes.** `Str/span` over a private structural walk, its stop and its step proved by lemmas the walk passes on; `search/seek` over `span`. Check: `parse_primitive_measurements`' `take_while` row and `Str/span`'s count read again; `the_per_character_walk_reads_each_character_whole_without_allocating` (`curios/src/tests/codegen/ladder.rs`) restated for the walk `Str/trim` takes now, its loop carrying a window and allocating nothing. Open in this stage: the prototype's `Str/At/next` answers its record as a pair on the heap where HEAD answers two values, its callers now `seek` and `prev`; the cause is read and fixed, or the claim the test makes of `next` restated with it.
3. **A piece of text is matched as bytes.** `follows`, `strip_suffix` and `ends_with`, and `occurrence` by bytes, with `Str/Valid/after_prefix` and a lemma that a cut's two halves are its bytes; `meets` and `meets_back` deleted. Check: `/std`'s tests and `curios/src/tests/strings`; `parse_primitive_measurements`' `literal` and `until` rows and `type_level_claim_measurements` read again.

## Retirement

Once stage 3 lands, `curios-prelude/README.md`'s first decision states that a walk over text reads the bytes it walks and that a position is for a cut and a step, with the rest-holding position among what it rejects, `/std/Str/At`'s and `Str/search`'s documentation says which walks stand at positions, the two measurements' figures move, and this file is deleted.
