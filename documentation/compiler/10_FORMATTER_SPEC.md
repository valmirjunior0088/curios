# The Curios formatter (`curios format`)

This document specifies the canonical source formatter: zero-configuration, corpus-derived, built as a consumer of machinery that already exists — the comment-capturing parse product, the width-aware printing algebra, and the surface printer once it is converted to that algebra. The prelude is the style's source of truth and its acceptance corpus.

## Laws

- **Semantic identity.** `parse(format(s))` structurally equals `parse(s)`, verified on every run; on mismatch the formatter refuses to write and reports the discrepancy. Formatting can never change a program.
- **Comment conservation.** Every comment span the parse captured appears in the output exactly once, text verbatim.
- **Idempotence and determinism.** `format ∘ format = format`, and equal inputs produce byte-equal outputs.
- **Nothing is ever reordered.** `use` scoping is point-of-use — the prelude itself depends on it (`Option.crs` places `pub use Option/*;` after the `induct` it re-exports) — and item order carries witness-registration ordering, so reordering is semantics-changing in Curios. The formatter formats; it never rearranges declarations, imports, fields, or arms.

## Style constants

- **Width 100** — the same target the goal reports render within, so diagnostics and formatted source read identically.
- **Indent 4**, unified: every printer entry in the workspace (`Display` impls, `display_within`, the stage dumps) moves from step 2 to step 4 alongside the formatter, with the affected test expectations updated. One indentation everywhere.
- **Exactly one blank line** between adjacent top-level items — except between consecutive `use` declarations and between a `mod` and a directly following `use`, which stack with none, as the corpus writes its import heads and its `mod X; use X/{…}` pairs; author grouping is not consulted. Inside bodies, blank lines are not emitted.
- **Trailing commas in every broken comma-list**; flat lists carry none.

## Construct rules

- **Top-level `let`/`rec`**: the signature on one line ending in `=`, the body on the next line at +4 — always, even when the whole binding would fit on one line. A signature overflowing 100 breaks its telescope one binder per line — the same shape goal reports use, deliberately.
- **Local `let`**: inline when it fits, body broken at +4 when it does not.
- **`match`** (and the primitive match/fold forms): never flattened. The shell at its context's indent, arms `|`-aligned with the `match` keyword, arm bodies inline after `=>` when they fit and on the next line at +4 when they do not, `end` aligned with the shell. The motive rides the shell line, breaking after `:` when overflowing.
- **`induct`/`struct`/`concept`**: header line; cases and fields at the header's own column (corpus style — no extra indent); `end` at the header's column.
- **`satisfy`**: brace block with fields at +4, one per line, trailing commas per the broken-list rule.
- **Applications, tuples, list literals, telescopes, `use` groups**: the shared delimited-list shape — flat when the group fits, else one element per line at +4 with a trailing comma, closing delimiter on its own line at the opening's indent.
- **Operator chains**: grouped per precedence chain; when broken, continuation lines lead with the operator.
- **Literals verbatim**: numeric radix spellings, character and string escapes, and packed-literal forms are reprinted as written; strings are never split.
- **The full inventory** — motives, effect brackets `[E] T`, `choose`, postfix `!`, irrefutable binder patterns, `foreign`, module blocks, and the rest of SYNTAX.md's roughly forty forms — each receives one deliberate rule during the printer conversion, recorded in the formatter module's rustdoc as it lands; this document pins the principles and the contentious calls above.

## Comments

One lexical fact does most of the work: a `--` comment runs to end of line, so **a comment is a hard break** — rendered as its text plus a mandatory newline. The fits scan already refuses to flatten a group containing a mandatory break, so any construct holding an interior comment breaks mechanically, with no special-casing.

Attachment is a pass over the captured spans, keyed by position relative to the syntax: an own-line comment attaches as *leading* to the next element and is re-indented to it; a same-line comment attaches as *trailing* to the preceding element and is flushed just before the following break; a comment after the last element of a block attaches as *dangling* to the block's close. Comment text is never rewrapped — the corpus writes long single-line comments, matching the repository's no-hardwrap documentation rule.

## Known consequences, accepted

- **Redundant author parentheses vanish.** The syntax tree has no parenthesis nodes, so grouping parens beyond what precedence requires are reprinted away. Semantic identity holds; the emphasis is lost. A paren-preserving node is the escape hatch if this proves painful.
- **Hand-wrapped layouts are canonicalized**, and the blank-line rule pries apart tightly-clustered one-liners. The first formatting of the prelude will show both — that diff is the acceptance review, not a bug report.

## The wasm dump, width-honest in both directions

Riding alongside as its own milestone, in `curios-wasm`'s printer: the dump currently hard-breaks small things and never breaks some large ones. Convert both directions — struct-type field lists, global initializers, and singleton `rec` groups become groups that flatten when they fit; `br_table` label lists and type-section signatures become groups that break when they overflow. Unlike the earlier output-neutral conversions, flattening changes `Display` output at unbounded width by design; the pinned dump expectations are updated rather than preserved.

## Mechanics

- **The printer conversion** mirrors the core printer's: the surface printer's fixed separators become `line()`/`group()`, output-neutral at unbounded width, so the existing `Display` round-trip tests pass untouched until the indent unification lands.
- **The formatter** lives in `curios-text` as a consumer of the parse product: `(Module, Vec<Span>, source)` in, formatted text out — attachment pass, width-100/indent-4 rendering with comments woven in, then the verification reparse.
- **The CLI**: `curios format <files…>` rewrites in place; `--check` writes nothing and exits nonzero when any file would change; a parse or verification failure reports and exits nonzero without writing. Formatting a goal-bearing program is fine — goals are syntax.
- **The corpus**: `curios-prelude/std` and `curios-prelude/syn` are formatted as the acceptance run and stay formatted thereafter, making style drift visible in review.

## Non-goals

- Configuration of any kind — no options, no style knobs.
- Reordering anything, ever.
- Rewrapping comment text or splitting string literals.
- Preserving redundant parentheses (v1).
- Editor integration — format-on-save is the editor's job over the same CLI.

## Tests

- Per-construct fixtures pinning each rule's flat and broken forms, including trailing commas and operator-led continuations.
- The laws over the corpus: format-reparse equality, idempotence, byte-determinism.
- Comment conservation across leading, trailing, interior, and dangling placements, including a comment forcing its group to break.
- Blank-line normalization; `--check` and failure exit behavior.
- The formatted prelude as a standing corpus test.

## Milestones

- **F1 — printer conversion** (`curios-text`): the surface printer onto the algebra, output-neutral, rules recorded per construct.
- **F2 — comment attachment** (`curios-text`): the pass over the parse product, with placement fixtures.
- **F3 — the formatter core** (`curios-text`): rendering at 100/4 with comments woven, plus the verification law.
- **F4 — the CLI** (`curios`): `format`, in-place and `--check`.
- **F5 — indent unification and the corpus run**: every printer entry to step 4, test expectations updated, then the prelude formatted and reviewed.
- **W1 — the wasm dump conversion** (`curios-wasm`): both directions, dump expectations updated. Independent of F1–F5.

## Retirement criteria

- Before this specification is deleted: the laws and per-construct rules are recorded in the formatter module's documentation and pinned by its tests; the corpus is formatted and standing; the CLI contract is in the README; the wasm dump conversion's expectations are updated in place; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
