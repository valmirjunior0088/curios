# A lint is an exact finding read off the compilation

**Decision.** `curios lint` reports four lints — `unused-import`, `unused-binder`, `unused-declaration` and `unused-dependency` — and nothing else, every one always on, with nothing to configure and no way to suppress one but to change the program: a binder or declaration is kept by naming it `_x`, an import or dependency by deleting it, and each message says which. A lint is decided where names resolve: `into_core` resolves every written reference to a binder identity, an import or a declaration, the first three lints are zeros read off that resolution — a selector nothing resolved through, a binder nothing referenced, a declaration unreachable from the unit's roots — and the fourth is read off the union, over a package's units, of the mounts references resolved into. The one reference no written name makes is credited too: a proof the elaborator writes from the facts in scope reads the hypotheses it sums, so elaboration credits each binder it reads, by its declaration and its place among the declaration's written binders, a key the declaration's own text determines. The findings travel with the lowered unit into the store. A lint is a diagnostic of its own severity: `wonder diagnostics` reports it, the server publishes it as a warning, `run`, `compile` and `test` never mention it, and only `lint` turns it into an exit code ([`lint`](../../usage.md#lint)).

**Rationale.**

- **The unused family is what every peer reports** — Lean, Rocq, Agda, GHC, OCaml, elm-review, Gleam, PureScript, Rust, Go — as the most frequent finding, often a misspelling in disguise, and fixed by a deletion.
- **Exactness leaves nothing to configure.** A fact of name resolution has no false positive, so levels, groups, suppression comments, suppression files and `expect`, which manage heuristic rules over a legacy corpus, have nothing to do. The formatter set the precedent of one style and no options.
- **Only one inserted reference reaches a named binder.** `!`'s bind and witness resolution reach anonymous binders; a proof from the facts in scope reads named hypotheses, so elaboration contributes the binders it reads — the part of Lean's info trees this language needs — and the lowering still decides every lint. A credit keyed by the declaration's text lets a recompile credit a declaration it reuses.
- **Each rule falls out of resolution.** A declaration holding a written goal reports no binder, since its binders are the goal's scope; a definition sugar's parameter is one binder in its Π-type and in its body lambda, so a mention in its result type is a use of it, and a proof written in its type credits it as one written in its body does; reachability decides a declaration, so one used only by itself is dead; a dependency mounts at its name and is reached only by a reference naming it.
- **Stored findings cost nothing to ask for again**, so a cached library still answers and the language server pays nothing per keystroke for a unit it did not touch.

**Rejected.**

- **Warnings on `run`**: a partial program is exactly when a run is wanted, as Go's and Zig's hard errors and Elm's removal of warnings agree.
- **Lint levels and `#[expect]`**, which manage stale suppressions; **a suppression file with a ratchet**, for gradual adoption over a legacy corpus — `/std`, `programs/` and the test corpus lint clean; **disable comments**, since comments are not syntax.
- **Walking elaborated terms instead of the lowering**, which re-derives what resolution already knows.
- **Writing a hypothesis only a proof reads as `_`**, which reports a used binder and loses the name documenting its precondition.
- **A lint that stops compilation**: a lint is not a refusal.
