# Span "out of source bounds" rendering bug

## Symptom

Errors originating in imported `.crs` files render against the wrong source and emit a placeholder instead of a code snippet:

```
type mismatch
  inferred: …
  expected: …

<span 2302..2305 out of source bounds>
```

Observed reliably when type-checking `examples/crs_json_codec.rs` — the type-mismatch span sits at byte 2302 of `examples/crs/std/Parse.crs` (line 68, inside `Parse/and_drop`), but the renderer is given the ~1.4 KB inline source from the Rust example file.

## Mechanism

`Span` is just two byte offsets, with no file identity:

```rust
// src/span.rs:1–5
pub struct Span {
    pub start: usize,
    pub end: usize,
}
```

`Span::render_snippet(&self, source: &str)` (`src/span.rs:12`) takes one source string, indexes into it, and emits a snippet with line numbers and a caret. If `start > source.len() || end > source.len()`, it bails with `<span N..M out of source bounds>`.

`compile(...)` (`src/run/compile.rs:14`) is handed exactly one inline source — the `term: &str` argument, which in the failing example is the string literal inside `examples/crs_json_codec.rs`. When any pipeline stage returns an error, `compile` calls `error.format(term)` (`src/run/compile.rs:32, 43, 46, 50`), and `format` (`src/core/typing.rs:255`, `src/text/error.rs:25`) hands that single `term` to `render_snippet`.

But errors come from anywhere in the import graph. The `Loader` (`text::FileLoader::new(...)`) loads `examples/crs/std/Parse.crs`, `examples/crs/json/decode.crs`, etc. as separate sources, parses each into a `text::Entrypoint`, and lets `to_core` walk the imports. Spans carried in `text::Spanned` and propagated up via `Error::at(span)` are byte offsets into *those imported files*, not into `compile`'s `term`.

So in the failing example:

- Type checking finds the mismatch inside `Parse/and_drop`, at bytes 2302..2305 of `examples/crs/std/Parse.crs` (`Result/` → the `lt/` substring on line 68).
- The span propagates up wearing no file identity.
- `compile` renders it against `examples/crs_json_codec.rs`'s inline source (length ~1400).
- 2302 > 1400 → out-of-bounds branch → `<span 2302..2305 out of source bounds>`.

If the inline source were longer than 2305 bytes, you'd get an even more confusing failure mode: a plausible-looking snippet from the wrong file, with a caret pointing at unrelated code. The current placeholder is at least honest.

## Why it wasn't noticed before

As long as errors are produced from inline sources (parsing or typing of the `term` string itself) or the imports happen to be much shorter than the inline source, the span lands inside `term`'s bounds and `render_snippet` produces *something* — possibly the wrong line, but not a flagged out-of-bounds. The union-converted examples are the first time we've had errors fire deep inside large imported modules, with spans larger than the inline `compile` source.

## Fix

Three pieces, all small:

1. **Give spans file identity.** Either:
   - Add a `source_id` field to `Span` (a path or a small interned ID), or
   - Wrap `Located` with a richer location: `Error::Located { source_id, span, error }`, keeping `Span` lightweight.

   Every place that creates a `Span` needs to stamp it with the file it's in. That's the `text::Spanned` parsing layer and any hand-built spans. The `Loader` is the natural place to give each loaded file an ID and thread it into the parser for that file.

2. **Render against the right source.** `format` (and `render_snippet`) need access to all the loaded sources, not just `term`. The cleanest shape:
   - The `Loader` (or a small `SourceMap` it populates) keeps a map from source-id → `&str`.
   - `compile` passes the `SourceMap` (or just a `Fn(source_id) -> Option<&str>` closure) to `error.format(...)`.
   - `render_snippet` resolves the span's source-id to the right text. The inline `term` gets its own id (e.g. `"<input>"`).

3. **Remove the bounds-checking placeholder.** `render_snippet` (`src/span.rs:16–18`) currently has an early-return:

   ```rust
   if start > source.len() || end > source.len() {
       return format!("<span {start}..{end} out of source bounds>");
   }
   ```

   This is a defensive stopgap that lets compilation continue when a span doesn't match its source — exactly the silent-failure mode this bug surfaces. Once (1) and (2) are in place, the only way to hit this branch is a genuine internal mistake (wrong source threaded through, span constructed against the wrong file, etc.) — i.e. an invariant violation we want to know about. Drop the `if` and let the slice indexing panic naturally, or replace it with an explicit `unreachable!`/`debug_assert!`. The placeholder being merely "honest about not knowing which file" is no longer useful when the file identity is recorded — at that point an out-of-bounds span means we have a bug elsewhere and should fail loudly rather than emit a placeholder.

The diff is bounded — a field on `Span` or `Located`, a few lines in the parser to thread it in, a small `SourceMap` type, a one-line change at each `format(term)` call site, and the bounds-check deletion in `render_snippet`.

## Second call site to update

`monads/parser.rs:106` formats parser errors via its own path:

```rust
snippet = crate::Span::new(self.offset, self.offset).render_snippet(string)
```

The parser builds a zero-width span at the current offset and renders it against a `string` it was given. When the parser is invoked on imported modules through the `Loader`, the same file-identity problem applies — the offset belongs to whichever file is being parsed, not necessarily the inline `term`. Whatever shape the `Span`/`SourceMap` fix lands on (a `source_id` on `Span` or an outer `Located` wrapper), this call site needs to be updated in lockstep — pass the file id, render against the right source.

## Verification

1. `cargo run --example crs_json_codec` after the union-match fix lands but *before* this fix: confirms the type-mismatch error still mentions a real span, just rendered out-of-bounds. Use this as the baseline.
2. After this fix: same example, induce a type error inside an imported module (any `examples/crs/**/*.crs`), confirm the snippet renders against the right file with correct line numbers and caret.
3. Hand-craft a parser error in an imported file (e.g. a syntax error in `examples/crs/std/Parse.crs`) and verify `monads/parser.rs:106`'s output also picks the right source.
4. Force an internal invariant violation (a span manufactured against the wrong source) and confirm the renderer panics or asserts rather than emitting a placeholder — i.e. the safety net in `render_snippet` is actually gone.

## Relationship to the union-match fix

Independent. The match-soundness fix lives in `core` typing; this one is error-rendering plumbing across `text::Loader` ↔ `Error` ↔ `compile`. They share nothing useful structurally. Worth its own small PR after the match fix lands — at which point the typical error site (out-of-source-bounds today) will start producing correct, file-aware snippets, and pipeline-wide error UX improves noticeably.
