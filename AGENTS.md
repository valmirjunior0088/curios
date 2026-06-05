Curios is a dependently-typed functional language that compiles to WebAssembly.

# Authoritative references

Consult these on demand when a specific question arises about the language, type system, primitives, idioms, or compiler architecture. Do not read them preemptively at session start to "orient"; open only the one that answers the question in front of you.

- **`README.md`** — project pitch, install via `cargo install`, CLI subcommands (`run`, `check`, `compile`) with `--timeout`, `--print`, and `compile --output-path`, minimal `/sys/Io/print` entrypoint.
- **`SYNTAX.md`** — complete language reference: lexical basics, top-level forms (`let`, `rec`, `union`, `mod`, `use`), every term and type form, literals, the `/sys` prelude (`Nat`, `Int`, `Flt`, `Bin`, `Arr`, `Bln`, `Io`), and canonical idioms for sum and recursive types.
- **`CRASH_COURSE.md`** — Rust-programmer's introduction: bindings, lambdas, `match` over `Nat`, primitives, tuples, atoms, union sum types, Π-types, length-indexed vectors, with Rust and Curios side by side.
- **`ARCHITECTURE.md`** — compiler pipeline across six stages: parsing (`text`), elaboration (`text/to_core`), type checking and erasure (`core/typing.rs`), CPS lowering (`ersd/to_cont`), WASM codegen (`cont/to_wasm`), and binary serialization (`wasm/writer.rs`); also module conventions, WASM value representation, the `Loader` trait, the test suite, and a recommended reading order.
- **`examples/`** — runnable Rust programs that drive the full pipeline end-to-end (parse → typecheck → erase → CPS → WASM → Wasmtime). Two worth knowing:
  - `crs_json_codec.rs` — encodes a `json/Value` tree to a `Bin`, round-trips through a parser, asserts byte-identical output; exercises file-backed modules (`std`, `std/Parse`, `json`), union values, and arrays.
  - `crs_printf.rs` — `fmt/printf("%s is %d")("Alice")(30)`; also demonstrates the type-safety guarantee — passing a `Bin` where `%d` expects a `Nat` is a compile-time `TypeMismatch`.

# Project management

A dedicated GitHub Projects board named **Curios** (project `3`, owner `@me`) tracks work. Use `gh project` whenever an instruction involves the board.

- All items are `DraftIssue` — no URL, no number, not commentable until promoted.
- Items carry no GitHub labels; they are organised by three single-select fields whose option spellings must match exactly:
  - **Status** — `Needs refinement`, `Ready to start`, `In progress`, `Finished`.
  - **Stage** — `1. Text → Core`, `2. Core → Ersd`, `3. Ersd → Cont`, `4. Cont → Cont`, `5. Cont → Wasm`, `6. Wasm → ()`, `7. Pre-release`, `8. Release`, `9. Post-release`.
  - **Feature** — `Algebraic Effects`, `Error Messages`, `Type System`, `Primitive Types`, `Syntax Sugar`, `Optimizations`, `Tooling & Ecosystem`, `Testing & Documentation`, `Core Pipeline`, `IO`.
- **Reads (querying the board):** prefer `gh api graphql` with a query that selects only the fields you need. `gh project item-list ... --format json` fetches every item with every column and `--jq` only filters *after* that payload has already landed in context, so it does not reduce what you pay for. A targeted GraphQL read that returns just the relevant fields for just the relevant items is the cheaper path and is the preferred one here, the general CLI-first rule notwithstanding. When you do fall back to `item-list`, set `--limit` to the smallest number that covers the board (it defaults to 30; do not pad it to 100 reflexively).
- **Mutations (changing items):** use the plain `gh project` CLI — `item-edit` with field and option IDs. Reach for `gh api graphql` only after confirming, via `--help`, that no CLI subcommand can express the mutation.
- **Cached field and option IDs** (so you can skip the `field-list --format json` call before each `item-edit` — verify once with `gh project field-list 3 --owner @me --format json` if a mutation fails, in case the board schema changed):
  - Project ID: `<<FILL IN: project node ID, e.g. PVT_xxx>>`
  - Status field: `<<FILL IN: field ID>>` — options: Needs refinement `<<id>>`, Ready to start `<<id>>`, In progress `<<id>>`, Finished `<<id>>`
  - Stage field: `<<FILL IN: field ID>>` — options: `<<one id per stage 1–9>>`
  - Feature field: `<<FILL IN: field ID>>` — options: `<<one id per feature>>`

# Working rules

1. **Do not change code without an explicit go-ahead.** Never infer that a change is wanted from context; wait for a direct instruction.
2. **Surface design decisions; do not invent workarounds.** When facing ambiguity, obstacles, or architectural choices, ask. Never silently adopt a fallback strategy.
3. **Do not stall in doubt.** The user can resolve any uncertainty — ask directly and immediately rather than speculating, hedging, or deferring to assumptions.
4. **Do not spiral in self-thought.** If reasoning revisits the same considerations without producing new information, stop and ask the user. Recursive deliberation is never a substitute for a question — the user is the one who breaks the loop.
