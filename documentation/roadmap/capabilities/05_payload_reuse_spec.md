# Payload reuse

Working implementation specification for reusing the optimized, precompiled `.cwasm` payload across `run` and `compile` invocations of a manifest target, so a program nothing has changed re-executes without recompiling anything.

This document states responsibilities and invariants, not today's internal APIs — those move. Its durable soundness argument belongs in a new entry under `documentation/soundness/admission-without-judgment/`, the store's layout and key in `curios-package`'s rustdoc, the record and probe invariants in the `curios` crate's module documentation and tests, and the user-facing behavior in `usage.md`.

## What this is, and is not

Both subcommands already share the whole compilation, and the unit store already makes the mounted half incremental: a warm dependency is restored rather than re-elaborated. What is never cached today is everything after the units — the entry program's own front half (lowering, elaboration, the kernel recheck, erasure), the whole-module back half (erased optimization, the lowering into Cont, Cont's whole-module fixpoint, wasm emission), and the native tail (validation, Binaryen, Cranelift) — so a warm `run` of an unchanged program still pays all of it. This capability caches the artifact those stages produce, keyed and verified so an unchanged invocation skips them entirely.

It is deliberately whole-artifact reuse on total freshness, not finer incrementality: the back half is whole-program by construction, so any source change still pays it in full. What this buys is the invocation that dominates development — rerunning a program nothing changed.

## Design decision: the cache sits at the payload seam, not the bundle

The unit of storage is the `.cwasm` payload — `to_cwasm`'s output. On a hit, `run` executes the stored payload in-process through the runtime exactly as it executes a fresh one today, with the same argv (the entry path as argv[0]) and the same exit-code passthrough; `compile` appends it to the embedded launcher and writes the bundle. One slot serves both subcommands, so `compile` after `run` is a file write and `run` after `compile` recompiles nothing.

Caching the bundle instead was rejected: `run_wasm`'s contract already states the payload path is byte-identical to what a bundled executable takes, so caching below the bundle preserves `run`'s observable behavior under a property the code already claims; the bundle is the launcher image plus the payload by pure concatenation, and the launcher bytes are inside the binary the address's compiler digest already names; and executing a stored bundle would change `run`'s argv[0] or need an override to hide that it did.

## Design decision: the cached-verdicts scheme, applied one level up

The address names a place and a record decides whether what sits in it may be believed — the split argued in [Cached verdicts](../../soundness/admission-without-judgment/cached-verdicts.md), which this capability extends rather than re-argues. A hit admits without judging: the entry's kernel recheck is skipped on the strength of the record, exactly as a reused unit's is. The new obligations the soundness entry must state are the entry's read-set closure (a module joins the entry only through a `mod` in a header that is itself a recorded read, and the entry's own header is recorded separately), the budget premise below, and machine dependence.

### The address

Its own schema tag, the compiler digest, the ordered predecessor unit slots, the declaring package's and the executable's names, and the engine-compatibility fingerprint. No paths and no file contents, so an edit changes what is verified rather than where it is looked for. The executable's identity is package name plus executable name — the one identity in a compilation that cannot collide, and the reason two executables of one package occupy two slots; a path there would re-import the store growth the unit key's replacement records. Predecessor slots are themselves address-stable across source edits, so the payload address is too: one slot per executable per chain per compiler, overwritten in place, bounded as the unit family is.

### The record

The entry file's digest, taken from the text that was parsed rather than from a re-read of the path — a re-read races an edit between parse and digest, and that race records newer text against an older artifact, the one direction that admits stale. Every file the entry's loader read, by canonical path and digest of the parsed text, through the same `RootSource::reads` seam the unit records use and under the same containment clause that keeps a shared store from admitting across projects. The digest of each predecessor's stored bytes, in fold order. And the digest of the payload itself, so corruption is a miss rather than a runtime error.

Verification is all of it or nothing. The write ordering is record-last, so every state an interrupted run can leave reads as a miss. Writes are best-effort: a store that cannot be written costs the reuse and never the verdict or the run, and the first refusal is reported once.

### The probe

A hit must be decidable without deserializing any unit: walk the unit sources in fold order, computing each slot, checking its record still agrees, and digesting its stored bytes — the verification half of the existing store consultation, shared rather than duplicated. Any stale unit is a payload miss by construction, since it will recompile into bytes the payload record cannot match.

## Design decision: machine dependence enters the key

The payload is the store's first machine-dependent artifact — Cranelift compiles for the host's ISA, which is the one input neither the compiler digest nor any recorded file covers. The engine-compatibility fingerprint therefore joins the address, exposed by `curios-runtime` (the only crate that names wasmtime) as a fold into a caller-supplied stable hasher, keeping the digest dependency out of the runtime and wasmtime out of `curios`. Wasmtime's own version and ISA stamp inside the artifact remains the backstop; with corruption and incompatibility both already misses, a deserialize failure at run time stays a hard error rather than a silent recompile.

## Design decision: the shared store family

The payload files in the store's shared half, beside the units: its key says nothing about which project asked, and content-derived keys are what the shared layer's own rationale admits. With `CURIOS_CACHE` unset the shared half sits beside the project as today, so the choice only changes behavior for users who opted into sharing. The bundle `compile` writes stays in the project-local `bin/` family, whose identity is project-relative.

## Layering

- `curios-pipeline` stays payload-ignorant. Its one change is taking the entry loader by reference through the fold, so the caller that owns it can read the accumulated read log afterward — the pattern the cache handle and its after-the-fold refusal report already use, on a read log that is interior-mutable precisely to be recorded through `&self`.
- `curios-package` owns the payload family's layout and key, beside `unit_slot`; the `curios` crate owns reading and writing through them — the split the unit cache already states for itself.
- `curios-runtime` gains only the compatibility fold; the slim launcher takes no new dependency and `make curios/runtime` stays clean.
- `curios-text` exposes the entry's parsed source alongside `Entrypoint::opened`, since the entry's header is deliberately never read through the loader.

## Flags and forms

- Manifest targets only. A bare file has no project, hence no store and no slot — the declared-versus-bare split unchanged.
- `--print` skips the get and still puts: stage dumps exist only when compilation runs, and filing what a real compilation produced is always safe.
- `--budget` stays out of the key, inheriting the cached-verdicts stance, on a premise the implementation must keep true: a spent budget surfaces as a refusal, never as a different artifact — exhaustion propagates as an error and the erased optimizer takes no budget. The day any pass degrades under budget instead of refusing, the budget enters the record.
- `-o` stays out of the key: the payload is independent of where the bundle lands, and the refuse-to-overwrite-the-input check still runs first.
- The program's own arguments are runtime inputs and appear nowhere.

## Reporting

A hit reports as reuse in the spelling the unit fold already uses, announced after the store is consulted so a reported operation is one actually about to happen. An unwritable store reports one refusal after the fold and stops nothing.

## Acceptance

- A package target run twice with nothing changed reuses on the second invocation, with identical program output and exit code, and the reuse is visible in the report.
- `compile` after `run` wraps the stored payload without recompiling; `run` after `compile` hits the same slot; two executables of one package occupy distinct slots.
- An edit to the entry file, to any file its loader read, or to any dependency source is a miss; the recompile refiles, and the following invocation hits again.
- Adding, removing, or reordering dependencies changes the address rather than mismatching a record.
- A corrupted stored payload is a miss, and every half-written slot state reads as a miss — the write-ordering states enumerated in tests as the unit store's are.
- Two different engine-compatibility fingerprints address two slots.
- A record naming a file outside what the asking target could itself have read is a miss, mirroring the unit store's shared-store aliasing probe.
- An unwritable store reports one refusal while the compile and the run proceed.
- A `--print` invocation compiles, dumps, and still files; the next plain invocation hits.
- A bare-file target never consults or writes the payload family.

## Non-goals

- Caching bare-file compilations — no project, no store, deliberately.
- Caching the entry's front half as a unit, or any finer-grained incrementality inside the back half — the whole-program passes stay whole-program.
- Narrowing the compiler identity for library embedders, an over-invalidation cached verdicts already records as safe.
- An eviction or size policy for the payload family: addresses are stable and slots overwrite in place, so growth is bounded as the unit family's is.
- Any change to the bundle format, the launcher, or `compile`'s output locations.
- Guaranteeing cross-machine payload sharing: two machines share a slot only when their engines' compatibility fingerprints agree, and the artifact's own stamp refuses whatever slips past.

## Verification

After landing, run the repository's full done bar in order — [CLAUDE.md](../../../CLAUDE.md), "Before handing off code changes", which owns the command list. Because the loader-by-reference seam touches `curios-text` and `curios-pipeline` inside the browser compiler's dependency graph, also run `make curios/js` with the exactly version-matched `wasm-bindgen-cli`. The bundle format is untouched, so the ignored end-to-end bundle test is not triggered, and the gate's first step already rebuilds the slim launcher in isolation.

## Retirement criteria

- The soundness entry exists under `documentation/soundness/admission-without-judgment/` with its grade in `soundness.md`'s index, stating the entry read-set closure, the budget premise, and the machine-dependence answer.
- The payload family's layout and key are recorded in `curios-package`'s rustdoc; the record, probe, and write-ordering invariants in the `curios` crate's module documentation and tests.
- `usage.md` documents the reuse and the payload family under "Where things go", and the CLI's module documentation no longer states that `run` compiles unconditionally.
- The roadmap entry is a checked unlinked summary, and no reference to this filename remains.
