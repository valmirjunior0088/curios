# A report is built where it is reported

**Not refined yet.** `resolved_for_display` (`curios-elab/src/typing.rs`) is documented as the display form of a report, and [A declaration is a function of what it reads](../../design/compilation/a-declaration-is-a-function-of-what-it-reads.md) decides that what is decided of a declaration spends a budget restored for that judgment. A mismatch's display is built where the error is raised, so a declaration pays, in its own budget, its own identities and its own caches, for every report it discards.

## What is missing

`display_mismatch` normalizes the two sides of a mismatch where its error is raised, and chooses among the specialized reports by reducing them (`sort_decided_twice`, `unembedded_action`, `implicit_at_other_sort`); an error raised under a bracket that rolls back is thrown away with what was built for it. Elaborating `/std` builds 389 such displays, 778 sides normalized, and reports none; with the normalization skipped, `/std` spends 65,514,611 units where it spends 65,545,426 and two declarations commit other terms through the identities the normalization minted. Counted at `68ca73258`. Once [An identity is read by its equality and its age](../arithmetic/101-an-identity-is-read-by-its-equality-and-its-age.md) has landed, what is left is the budget and the cost: a declaration near its budget can be refused for a report it never makes.

## Design

- **What a raise holds is what reading the context gives.** Substituting the solutions that have landed (`zonk_solved_term_metas`), settling qualities and giving recursive names back (`refold_recs`) each take `&Context`; normalizing and choosing a specialized report take `&mut Context`. An error holds its sides as the first three leave them, the term the conversion was about, and the frames it was raised on (`Context::hold_frames`); the constructor takes `&Context`, so no raise site can reach what it would spend.
- **The rest is built where the report is made**: in the drain for parked work, where it is made today, and where a declaration is refused, before its attempt is undone, for every error the refusal holds, a rejected candidate's among them, standing on the frames each error holds.

The peers carry the state an error needs and render it when it is shown: Lean's `MessageData.ofLazy` is "a lazy message. The provided thunk will not be run until it is about to be displayed", under the `(env, mctx, lctx, opts)` a `withContext` holds ([`Lean.Message`](https://lean-lang.org/doc/api/Lean/Message.html)); Rocq raises `PretypeError of env * evar_map * pretype_error`, which `Himsg.explain_pretype_error` turns into text ([`Himsg`](https://ocaml.org/p/rocq-runtime/9.0.0/rocq-runtime.vernac/Himsg/index.html)); Agda's `TypeError` carries the `TCState` it was raised in and a `Closure` of its error ([`Agda.TypeChecking.Monad.Base`](https://hackage-content-origin.haskell.org/package/Agda-2.6.20240714/candidate/docs/Agda-TypeChecking-Monad-Base.html)); GHC reports unsolved constraints once solving is done, zonked then ([`GHC.Tc.Errors`](https://hackage.haskell.org/package/ghc-9.10.3/src/GHC/Tc/Errors.hs)).

**Not read yet**: whether a normalization made once the locals of the raise are closed reads them, and how many errors a refusal holds that were raised under a bracket that rolled back, which hold the bracket's solutions as substituted at the raise and gain any landed since.

**Rejected.** Building the display where it is raised and undoing what it spent, minted and cached: undoing a cache's writes needs a journal of them. Skipping the display inside an oracle: an oracle's error can be the one reported, and every other bracket stays.

## Check

`/std` builds no display, every report in `curios-pipeline/src/tests/` and the corpus is unchanged, and `/std`'s elaborated module is the same. Figure it may move: the budget `/std` spends.
