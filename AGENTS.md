Curios is a dependently-typed functional language that compiles to WebAssembly.

# Authoritative references

Consult these first when questions arise about the language, type system, primitives, idioms, or compiler architecture.

- **`README.md`** — project pitch, install via `cargo install`, CLI flags (`--timeout`, `--check`, `--print`), minimal `/sys/Io/print` entrypoint.
- **`SYNTAX.md`** — complete language reference: lexical basics, top-level forms (`let`, `rec`, `mod`, `use`), every term and type form, literals, the `/sys` prelude (`Nat`, `Int`, `Flt`, `Bin`, `Arr`, `Bln`, `Io`), and canonical idioms for sum and recursive types.
- **`CRASH_COURSE.md`** — Rust-programmer's introduction: bindings, lambdas, `match` over `Nat`, primitives, tuples, atoms, dependent-tuple sum types, Π-types, length-indexed vectors, with Rust and Curios side by side.
- **`ARCHITECTURE.md`** — compiler pipeline across six stages: parsing (`text`), elaboration (`text/to_core`), type checking and erasure (`core/typing.rs`), CPS lowering (`ersd/to_cont`), WASM codegen (`cont/to_wasm`), and binary serialization (`wasm/writer.rs`); also module conventions, WASM value representation, the `Provider` trait, the test suite, and a recommended reading order.
- **`examples/`** — runnable Rust programs that drive the full pipeline end-to-end (parse → typecheck → erase → CPS → WASM → Wasmtime). Two worth knowing:
  - `crs_json_codec.rs` — encodes a `json/Value` tree to a `Bin`, round-trips through a parser, asserts byte-identical output; exercises file-backed modules (`std`, `parser`, `json`), dependent sums, and arrays.
  - `crs_printf.rs` — `fmt/printf("%s is %d")("Alice")(30)`; also demonstrates the type-safety guarantee — passing a `Bin` where `%d` expects a `Nat` is a compile-time `TypeMismatch`.

# Project management

A dedicated GitHub Projects board named **Curios** (project `3`, owner `@me`) tracks work. Use `gh project` whenever an instruction involves the board.

- List items with `gh project item-list 3 --owner @me --limit 100 --format json` and filter with `--jq`. The default tabular output hides bodies and custom fields, and `--limit` defaults to 30.
- All items are `DraftIssue` — no URL, no number, not commentable until promoted.
- Items carry no GitHub labels; they are organised by three single-select fields whose option spellings must match exactly:
  - **Status** — `Needs refinement`, `Ready to start`, `In progress`, `Finished`.
  - **Stage** — `1. Text → Core`, `2. Core → Ersd`, `3. Ersd → Cont`, `4. Cont → Cont`, `5. Cont → Wasm`, `6. Wasm → ()`, `7. Pre-release`, `8. Release`, `9. Post-release`.
  - **Feature** — `Algebraic Effects`, `Error Messages`, `Type System`, `Primitive Types`, `Syntax Sugar`, `Optimizations`, `Tooling & Ecosystem`, `Testing & Documentation`, `Core Pipeline`, `IO`.
- The plain `gh project` CLI covers nearly every read and mutation — use `--format json` + `--jq` for queries, and `item-edit` (with field and option IDs from `field-list --format json`) for changes. Reach for `gh api graphql` only after confirming, via `--help`, that no CLI subcommand can express the operation.

# Working rules

1. **Do not change code without an explicit go-ahead.** Never infer that a change is wanted from context; wait for a direct instruction.
2. **Surface design decisions; do not invent workarounds.** When facing ambiguity, obstacles, or architectural choices, ask. Never silently adopt a fallback strategy.
3. **Do not stall in doubt.** The user can resolve any uncertainty — ask directly and immediately rather than speculating, hedging, or deferring to assumptions.
4. **Do not spiral in self-thought.** If reasoning revisits the same considerations without producing new information, stop and ask the user. Recursive deliberation is never a substitute for a question — the user is the one who breaks the loop.
