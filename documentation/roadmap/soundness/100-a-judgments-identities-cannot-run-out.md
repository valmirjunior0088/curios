# A judgment's identities cannot run out

[`curios-cert`'s README](../../../curios-cert/README.md#a-type-is-accepted-by-typing-it-and-reduction-is-total-on-arbitrary-terms) decides that the kernel's judgments are total and that neither shapes nor counts can abort them, and [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md) decides that what a judgment consumes is counted in units no target can move. A judgment's binder identities are counted in a 32-bit space no budget bounds, through a counter whose width is the host's: the kernel panics where the space runs out on a native build, and on wasm32 the counter can wrap and hand out an identity already open.

## What is wrong

**The counter's width is the host's.** `Entropy` (`curios-utilities/src/entropy.rs`) counts in `usize`, which is 32 bits on wasm32, where `curios-js` certifies what the playground compiles. Built `--release`, with no overflow checks, a mint at the top of the space wraps the counter to zero and hands out identities the declaration has open: a capture where the native build panics. A hit mints nothing ([the memo decision](../../../curios-cert/README.md#the-kernel-memoizes-its-own-evaluation-and-a-memo-changes-only-resource-verdicts)), so only binders a judgment opens advance the counter, and none known comes near the top; that it cannot is held by nothing but how much work a judgment does.

**The space is a limit of its own.** A local's index is a `u32` (`Mint`, `curios-core/src/names.rs`) that nothing reads but its comparison and its hash, so it is a second limit beside the budget, which no refusal names: the kernel's `Spend::fresh`, the elaborator's `Context::fresh_for` and `Context::fresh_written` (`curios-elab/src/context.rs`) and the lowering's binders (`curios-text/src/into_core/context.rs`) each `expect` the counter to fit it. The printer cuts the same space in two: a render mints its own binders from `PRINTED`, `1 << 31` (`Minting`, `curios-core/src/print.rs`), and takes every local at or above it for one of its own, so a compilation's identity past `2^31` would be displayed as a printer's.

## Goal

A judgment's identities are counted in a width that is the same on every target and that no judgment that ends can exhaust, so the running out of identities is not a state the code can reach and the only limits a judgment meets are its budget's.

## Decisions

- **An identity is a `u64`, and so is every counter `Entropy` keeps.** The index is only compared and hashed, so its width costs nothing it is read for: `Mint`, `Free` and `Subterm` stay 24, 32 and 176 bytes and their archived forms 31 and 107 for `Free` and `Subterm`, the archived `Mint` alone going from 18 to 22 within `Global`'s 30, counted at `68ca73258` on x86_64. A counter that only a mint advances, one at a time, holds every judgment that ends, as `std::thread::ThreadId`'s `u64` ([`id.rs`](https://github.com/rust-lang/rust/blob/master/library/std/src/thread/id.rs)), Agda's `NameId` (`Word64`, [`Agda.Syntax.Common`](https://github.com/agda/agda/blob/master/src/full/Agda/Syntax/Common.hs)) and GHC's `Unique` (`Word64` since [#22010](https://gitlab.haskell.org/ghc/ghc/-/issues/22010), where `Int` ran out on 32-bit targets) hold theirs. `Entropy` counts in `u64` for every space it mints, as `curios-core`'s cost formulas compute in `u64` regardless of the host's pointer width; an identity that indexes an arena narrows to its arena's index where it is minted (`Mint`, the `id!` macro), as it does today. The printer's binders start at `1 << 63`.

## Rejected

- **A refusal where the space runs out**, a fallible mint through `Reducer::fresh_binder` and `curios-analysis`'s `fresh`, at 48 sites in the kernel and the shared crates and 129 in the elaborator: a refusal for a state the representation can leave out.
- **Pricing a mint in the budget**, which makes the bound the budget's own: typing builds terms the budget prices nowhere, since the budget prices reduction, the one stage a program can drive to arbitrary cost, and a local priced alone would be the one construction typing pays for.
- **Binders the kernel names by position**, de Bruijn indices as Rocq's kernel keeps them ([`constr.mli`](https://github.com/rocq-prover/rocq/blob/master/kernel/constr.mli)), which mints no name, or levels as the elaboration zoo's conversion takes a fresh variable from the context's length ([`02-typecheck-closures-debruijn`](https://github.com/AndrasKovacs/elaboration-zoo/blob/master/02-typecheck-closures-debruijn/Main.hs)): a position recurs once its scope closes, and the kernel's tables over terms that name a local rest on identities that never recur within a declaration, which is why an entry that outlives its binders is unreachable rather than wrong; Lean's kernel keeps unique names for its caches the same way.
- **Capping the budget at what 32 bits hold**: a limit on a program's budget read off a representation's width.

## Stage

**An identity is a `u64`.** `Entropy` counts in one, `Mint::mint` takes one, and the `id!` and `name!` macros, `MetavarId` and `UniverseMetaId` narrow from it; `Mint`'s index, `Free::local`, `Free::local_written`, `Free::local_index` and `Spend::reserve` take one; `PRINTED` is `1 << 63`; and the `expect`s on a `u32` in the kernel, the elaborator and the lowering go with the conversions they guard. Check: `cargo xtask workspace lint`, `format` and `test`, the corpus among them since the macros reach every intermediate representation, and `cargo xtask bundle build`. The browser bundle's memory may move, where `Mint` aligns to eight bytes; the sizes on wasm32 are not measured. Size: small, mechanical across the stages.

## Verification and retirement

No `expect` on an identity's width is left, and the bundle builds. The specification is deleted with the stage, with the references to it.

## What it leaves

[An atom is ranked by where its binder stands](../arithmetic/100-an-atom-is-ranked-by-where-its-binder-stands.md) is what makes freshness the one property a judgment reads off an identity.
