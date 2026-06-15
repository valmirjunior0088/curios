Curios is a dependently-typed functional language that compiles to WebAssembly.

# Authoritative references

Consult these on demand when a specific question arises about the language, type system, primitives, idioms, or compiler architecture. Do not read them preemptively at session start to "orient"; open only the one that answers the question in front of you.

- **`README.md`** — project pitch, install via `cargo install`, CLI subcommands (`run`, `check`, `compile`) with `--timeout`, `--print`, and `compile --output-path`, minimal `/std/Io/write` entrypoint.
- **`SYNTAX.md`** — complete language reference: lexical basics, top-level forms (`let`, `rec`, `union`, `mod`, `use`), every term and type form including holes and `let !`/bang sequencing, literals, canonical idioms for sum and recursive types, and an appendix of `/std` operation tables (`Nat`, `Int`, `Flt`, `Bin`, `Arr`, `Bln`, `Io`). `/std` is the user-facing surface; the primitives live in an internal `/sys` module it re-exports, unreachable from user code.
- **`STD.md`** — standard-library reference: every `/std` module's public surface — scalar helpers, `Char`/`Str`, the `Io` buffered reader and `File` brackets, `Option`/`Result`/`Lst`/`Vec`, `Eq`/`Void`, the `Parse` combinators, the `Json` codec, and `Fmt` typed format strings. The `std/*.crs` sources are comment-free by policy; this file carries their documentation.
- **`CRASH_COURSE.md`** — Rust-programmer's introduction: bindings, lambdas, `match` over `Nat`, primitives, tuples, union sum types, Π-types, length-indexed vectors, with Rust and Curios side by side.
- **`PROOFS_101.md`** — proving as a follow-up to the crash course: propositions as types (`/std/Void`, `Not`), equality via `/std/Eq` (`refl`, `sym`/`trans`/`cong`/`subst`), induction as `match` with `ih`, negation by discriminate-and-transport, the one-binder-per-index-position inverter limit, `subst` re-typing data, and a sortedness invariant (`Lte`/`IsSorted` as type-level functions) guarding a `search` precondition; every snippet is pinned by `examples/crs_proofs.rs`.
- **`ARCHITECTURE.md`** — compiler pipeline across eight stages: parsing (`text`), name/module resolution and union desugaring (`text/to_core`), type checking and erasure (`core/`), CPS lowering (`ersd/to_cont`), CPS optimization (`optm/` — monomorphization, devirtualization, DCE), WASM codegen (`cont/to_wasm`), binary serialization (`wasm/writer.rs`), and Binaryen optimization of the emitted binary (`binaryen.rs`, vendored Binaryen in `binaryen/`); also the de Bruijn machinery (`core/scope.rs`), module conventions, the embedded `/sys` + `/std` prelude, WASM value representation, the `Loader` trait, the test suite, and a recommended reading order.
- **`examples/`** — runnable Rust programs that drive the full pipeline end-to-end (parse → typecheck → erase → CPS → WASM → Wasmtime). Four worth knowing:
  - `crs_json_codec.rs` — encodes a `Json` tree to a `Bin`, round-trips through a parser, asserts byte-identical output; exercises the standard library (`std/Json`, `std/Parse`), union values, and arrays.
  - `crs_printf.rs` — reads `"Alice"` from stdin through `/std/Io/read` on the `Io/stdin` handle, trims it, then runs `/std/Fmt/printf("%s is %d")(name)(30)`; also demonstrates the type-safety guarantee — passing a `Bin` where `%d` expects a `Nat` is a compile-time `TypeMismatch`.
  - `crs_eq.rs` — exercises `/std/Eq` (`refl` with its implicit payload, `sym`/`trans`/`cong`/`subst`), then asserts that claiming `Eq(2, 3)` is a compile-time `TypeMismatch`.
  - `crs_proofs.rs` — compiles and runs the complete program from `PROOFS_101.md`, and asserts its three rejections (`MissingArmNotImpossible` for a zero-arm match on `Eq(0, 1)`, `TypeMismatch` for `refl` at unequal indices, `NotATupleType` for claiming `IsSorted` of an unsorted list).

# Project management

A dedicated GitHub Projects board named **Curios** (project `3`, owner `@me`) tracks work. Use `gh project` whenever an instruction involves the board.

- All items are `DraftIssue` — no URL, no number, not commentable until promoted.
- Items carry no GitHub labels; they are organised by three single-select fields whose option spellings must match exactly:
  - **Status** — `Needs refinement`, `Ready to start`, `In progress`, `Finished`.
  - **Stage** — `1. Text → Core`, `2. Core → Ersd`, `3. Ersd → Cont`, `4. Cont → Cont`, `5. Cont → Wasm`, `6. Wasm → ()`, `7. Pre-release`, `8. Release`, `9. Post-release`.
  - **Feature** — `Error Messages`, `Type System`, `Primitive Types`, `Syntax Sugar`, `Optimizations`, `Tooling & Ecosystem`, `Testing & Documentation`, `Core Pipeline`, `IO`.
- **Reads (querying the board):** prefer `gh api graphql` with a query that selects only the fields you need. `gh project item-list ... --format json` fetches every item with every column and `--jq` only filters _after_ that payload has already landed in context, so it does not reduce what you pay for. A targeted GraphQL read that returns just the relevant fields for just the relevant items is the cheaper path and is the preferred one here, the general CLI-first rule notwithstanding. When you do fall back to `item-list`, set `--limit` to the smallest number that covers the board (it defaults to 30; do not pad it to 100 reflexively).
- **Mutations (changing items):** use the plain `gh project` CLI — `item-edit` with field and option IDs. Reach for `gh api graphql` only after confirming, via `--help`, that no CLI subcommand can express the mutation.
- **Cached field and option IDs** (so you can skip the `field-list --format json` call before each `item-edit` — verify once with `gh project field-list 3 --owner @me --format json` if a mutation fails, in case the board schema changed):
  - Project ID: `PVT_kwHOARtF1c4BRKYq`
  - Status field: `PVTSSF_lAHOARtF1c4BRKYqzg_EgXc` — options: Needs refinement `946369b4`, Ready to start `518e4a26`, In progress `55e71cbd`, Finished `bdfec89e`
  - Stage field: `PVTSSF_lAHOARtF1c4BRKYqzg_EgX8` — options: 1. Text → Core `9a0c627a`, 2. Core → Ersd `eb5fc71b`, 3. Ersd → Cont `fdfef4a0`, 4. Cont → Cont `2f43ac5b`, 5. Cont → Wasm `8bacbc1d`, 6. Wasm → () `59a2e3c8`, 7. Pre-release `05f2a624`, 8. Release `5bb06bd7`, 9. Post-release `3f94b80e`
  - Feature field: `PVTSSF_lAHOARtF1c4BRKYqzg_EgYA` — options: Error Messages `c96c33d1`, Type System `1a11eb13`, Primitive Types `f28b7bb6`, Syntax Sugar `c3a05ace`, Optimizations `945065f5`, Tooling & Ecosystem `e07a5f8c`, Testing & Documentation `67528e86`, Core Pipeline `7ac641a4`, IO `30bfe2e2`

# Working rules

1. **Do not change code without an explicit go-ahead.** Never infer that a change is wanted from context; wait for a direct instruction.
2. **Surface design decisions; do not invent workarounds.** When facing ambiguity, obstacles, or architectural choices, ask. Never silently adopt a fallback strategy.
3. **Do not stall in doubt.** The user can resolve any uncertainty — ask directly and immediately rather than speculating, hedging, or deferring to assumptions.
4. **Do not spiral in self-thought.** If reasoning revisits the same considerations without producing new information, stop and ask the user. Recursive deliberation is never a substitute for a question — the user is the one who breaks the loop.
