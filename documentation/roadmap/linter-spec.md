# A lint is an exact finding read off the compilation

## Status

Refined; nothing is started.

## Why it exists

Nothing reports an import nothing uses, a binder nothing reads, a private declaration nothing reaches or a dependency nothing names. Every surveyed peer — Lean, Rocq, Agda, GHC, OCaml, elm-review, Gleam, PureScript, Rust, Go — reports the first three, they are the most frequent findings each reports, the one most often a misspelling in disguise, and the one whose fix is a deletion. The fourth is one Gleam had to refuse as undecidable, since an external can reach a dependency invisibly; here a dependency is reached only by naming its prefix, so it is as exact as the other three.

## Decisions

- **`curios lint [TARGET]` is a gate, and a lint is a diagnostic.** The target takes `wonder diagnostics`'s four forms and means the governing package entire by none; the unit asked about is linted and its scope never is. Output is what `wonder diagnostics` prints plus every lint, each a `Report` rendered as `run` renders one; exit 1 when a lint or an error was reported, 2 when only goals were, 0 when nothing. `wonder diagnostics` carries lints under a third severity and the server sends them as warnings; `run`, `compile` and `test` never mention them. *Rejected:* warnings on `run` — a partial program is exactly when a run is wanted, and Go and Zig's hard error and Elm's removal of compiler warnings point the same way; a lint that stops compilation.
- **Every lint is always on, and there is no configuration.** No levels, no rule list, no manifest table, no inline suppression: comments are not syntax, and the formatter set the precedent of one style and no options. Admission is exactness — a lint is a fact of name resolution, never a heuristic — so nothing remains to configure. *Rejected:* allow/warn/deny levels and `expect`, which exist to manage stale suppressions, a problem there is no need to have; a suppression ratchet, which exists for gradual adoption over a legacy corpus, and the corpus here is linted clean before the gate lands; disable comments.
- **Silence is spelled in the program.** A binder or private declaration nothing uses is kept by naming it `_x`; an import nothing uses is deleted. `_x` is already an identifier, no `/std` module spells one, and every peer reads it the same way. The message says how: ``unused binder `n`; name it `_n` to keep it``.
- **Lints are computed where names resolve.** `into_core` resolves every reference to a binder identity, an import or a declaration, and a lint is a zero read off that. Elaboration is not consulted: the only references the compiler inserts, `!`'s bind and witness resolution, reach anonymous binders. *Rejected:* walking elaborated terms — Lean's info trees exist because tactics use variables invisibly, and nothing here does.
- **A declaration holding a written goal is exempt.** Its binders are the goal's scope, listed for the author to use next.

## The lints

| Lint | Fires on | Never on | Kept by |
| --- | --- | --- | --- |
| `unused-import` | a `use` selector or glob no reference resolved through | `pub use` | deleting it |
| `unused-binder` | a named parameter, `let` binder, pattern binder or motive label nothing references, implicit or shadowed included | `_`, `_x`, anonymous `use` binders, a declaration with a goal | `_x` |
| `unused-declaration` | a non-`pub` `let`, `induct`, `struct`, `concept` or `mod` unreachable from the unit's roots: `pub` items, tests, the executable tail and `satisfy` bodies | `pub` items, tests, `satisfy`, constructors | `_x` or `pub` |
| `unused-dependency` | a `[dependencies]` row of the governing package whose prefix no reference in its library or any of its executables resolved into | an umbrella's `catalog` row, which fetches nothing on its own | deleting the row |

Reachability rather than a count, so a private definition used only by itself or by another dead one is dead. A dependency is a fact of the package rather than of a unit, so `unused-dependency` fires only when the target is the package entire, and is reported against the manifest, naming the row, since a manifest row has no span.

## What changes

- `curios-text`: spans on `TopUse`, `GroupItem` and `Pattern::Binder`, none of which carries one today; the lowerer counts resolutions, marks reachability and records which mounts references resolved into; the lowering hands out `Vec<Lint>` and the mounts reached beside the module.
- `curios-pipeline`: `check_with_units` returns them.
- `curios`: `Severity::Lint`; `wonder diagnostics` appends them; `server` maps them to `WARNING`; `Mode::Lint` renders and exits, and over the package entire unions the mounts its units reached against the manifest's rows.
- Tests: a corpus per lint — fires, does not fire, kept; `/std`, `/syn` and `programs/` lint clean as a test.
- `usage.md` gains a Linting section; `syntax.md` states what a `_`-prefixed name means.

## Deferred

`--fix` — a deletion at the reported span, verified by the formatter's reparse; unused constructors; a redundant match default. None changes a decision above.
