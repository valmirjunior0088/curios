//! Recorded figures for the structural shapes, each with the command that retakes it. None asserts.

//! Structural acceptance fixtures. Each test compiles a small `.crs` fixture to the raw, pre-Binaryen wasm module and asserts a structural property of the emitted code — a clean natural loop for a hot kernel, direct recursion, the closure ABI only where a call is genuinely unknown — and that the raw module validates and executes without Binaryen repairing control flow.
//!
//! Emitted function names are `$func/<N>` ids — a module-wide monotonic index over every reachable function, prelude included — optionally suffixed with the source hint as `$func/<N>$hint`. The index carries identity; the hint is only origin annotation. Hot kernels are still located by a distinctive literal constant baked into their arithmetic (`65537` for LCG, `1000003` for trees) or by name-independent structure (self-recursion, the shared `$func/<N>`/`$clsr/<N>` index of a function used both directly and as a closure), never by a source name. A genuine irreducible-cycle dispatcher is the `loop $$dispatch/<anchor>` the emitter names in `into_wasm::expr_emitter`; an ordinary constructor-tag `switch` is not a dispatcher whatever shape it takes — a `br_table` over `$case$N`/`$tail` labels for three or more cases, a plain `if` for the two-way and one-way shapes.

use curios_wasm::to_bytes;

use super::test_support::*;

/// What the closure table index is worth at product level, and where the profile's attribution had already expired.
///
/// Run it with:
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture closure_index_dispatch_measurements
/// ```
///
/// It asserts nothing. The structural claim is [`closures_carry_their_code_as_a_table_index`]'s to make; this prints the static shape of the swap over the corpus — table slots, dispatch sites, environment allocations — so the timings below stay pinned to the modules that produced them.
///
/// # The native timings, taken 2026-08-17
///
/// Native binaries, debug-profile compiler, x86-64 Linux: `target/debug/curios compile <program> -o <path>` at the swap commit (after) and at its parent `1e079440` (before), then `echo <N> | /usr/bin/time -v <bin>`, five runs per arm, `user` seconds, arms interleaved run-by-run to keep thermal drift out of the comparison (a first non-interleaved pass showed parse_digits's before arm drifting 1.14 → 0.80 across five runs; the interleaved figures below are the stable ones). Every pair printed identical output before any figure was read. Max RSS was flat on every pair (~7 MB; trees 134 MB both arms).
///
/// | Program | Input | before | after |
/// | --- | --- | --- | --- |
/// | `monad_io` | 10 000 000 | 1.26 1.26 1.26 1.26 1.29 | 0.22 0.21 0.21 0.22 0.22 |
/// | `parse_digits` | 1 000 000 | 0.80 0.80 0.79 0.78 0.80 | 0.59 0.58 0.59 0.58 0.58 |
/// | `parse_multibyte` | 300 000 | 0.63 0.60 0.60 0.60 0.61 | 0.55 0.54 0.53 0.55 0.54 |
/// | `rng_state` | 10 000 000 | 0.04–0.06 | 0.04–0.06 |
/// | `rng_manual` | 10 000 000 | 0.03 | 0.03 |
/// | `state_monad` | 1 000 000 | 0.00 | 0.00 |
/// | `lcg` | 100 000 000 | 0.31 | 0.31 |
/// | `trees` | 21 | 0.32–0.38 | 0.27–0.36 |
///
/// **The monadic loop the mechanism was priced for moved 5.9×** — `monad_io` binds a description per step, so each iteration built one closure and forced it, and 1.26 s of that was the funcref machinery. The string walks moved too (parse_digits −27%, parse_multibyte −11%): two `call_indirect` per character replaced two funcref constructions' interns. The controls behaved — `lcg` and `rng_manual` build no closures and are flat.
///
/// # The typed tables, 2026-08-20
///
/// Per-arity tables typed `(ref null $clsr/N)` with bodies declared at the arity type, deleting both per-dispatch engine checks: the `call_indirect` signature check compiles away entirely (the table's element type equals the expected type — Wasmtime's `StaticMatch`), and the former named-final-subtype mismatch that took the `is_subtype` libcall on every call has no types left to mismatch. Release-profile compiler this time, native binaries, x86-64 Linux, whole-process wall, min of 5, before = the commit under `5b837023`, outputs identical across arms and every harness anchor reproduced:
///
/// | Program | Input | before | after |
/// | --- | --- | --- | --- |
/// | `monad_io` | 20 000 000 | 317 ms | 238 ms (**−24.9%**) |
/// | `parse_digits` | 1 000 000 | 442 ms | 374 ms (**−15.4%**) |
/// | `state_monad` | 100 000 000 | 185 ms | 180 ms |
/// | `chain` | 1600 | 118 ms | 118 ms |
/// | `spines` | 75 000 | 82 ms | 82 ms |
/// | `lcg` | 100 000 000 | 321 ms | 326 ms |
/// | `trees` | 21 | 352 ms | 346 ms |
/// | `churn` | 75 000 000 | 344 ms | 345 ms |
///
/// The split is the prediction: the two programs whose hot loop dispatches an unknown callee moved, and the five whose loops carry no indirect call — plus `state_monad`, whose binds the specializer absorbs — sat inside ±1.7%. What remains per dispatch is the one `ref.cast (ref $envr/N)` to the non-final environment supertype — deleted since, where the closure arrives from a heap field declared at that type, and standing where it arrives from a parameter.
///
/// # Family keying, 2026-08-20
///
/// A variant family is one final struct at its own width — `CpsValueExpr::Variant`/`CpsIntrinsic::VariantGet`, minted by the Ersd door and padded to the family's width — so a family read is one exact cast where it was a `ref.test` cascade over the arity roster. Same method as above, but **interleaved run-by-run** (before, after, before, after) rather than arm-by-arm, min of 7 each, taken at a one-minute load average under 1.0. Two independent passes, reported together because they agree:
///
/// | Program | before | after | pass 1 | pass 2 |
/// | --- | ---: | ---: | ---: | ---: |
/// | `parse_digits` | 369 ms | 329 ms | **-10.8%** | **-9.8%** |
/// | `chain` | 118 ms | 107 ms | **-9.3%** | **-10.8%** |
/// | `trees` | 329 ms | 305 ms | **-7.3%** | **-7.6%** |
/// | `spines` | 70 ms | 65 ms | **-7.1%** | **-7.5%** |
/// | `churn` | 343 ms | 338 ms | -1.5% | -0.6% |
/// | `monad_io` | 232 ms | 232 ms | +0.0% | +0.0% |
/// | `state_monad` | 181 ms | 182 ms | +0.6% | +0.5% |
/// | `lcg` | 320 ms | 322 ms | +0.6% | +0.3% |
///
/// Statically, over optimized `spines`: the cascade's `ref.test (ref $tuple/N)` falls **58 to 6**, `ref.cast (ref $tuple/N)` **86 to 17**, and the arity roster **5 types to 3**.
///
/// The four movers are exactly the programs whose hot loop walks a heap variant family, and `lcg` — which declares no variant — is flat, so the split is a class rather than a coincidence. **The controls earned their place twice here.** A first pass showed `state_monad` 5.8x *slower* (185 to 1070 ms): the specializer's profitability gates asked whether a body contained a `TupleGet` on its parameter, and once family reads became `VariantGet` they answered no for every variant, so constructor specialization declined silently and `State/bind`'s chain survived as a per-step closure allocation. Teaching both gates the second vocabulary restored it exactly. That is the cost of a second vocabulary, and the reason it is worth paying is the same measurement: a *rebuild* in the wrong vocabulary would have trapped at the next exact cast instead of merely running slower, and the verifier now refuses one.
///
/// One finding for the successor: the per-family type identity does **not** survive Binaryen while every slot is `anyref`. `GlobalTypeOptimization` drops a slot nothing reads, leaving a struct structurally identical to a same-width `$tuple/N`, which closed-world type merging then folds together — the map's fork reads ship as `struct.get $tuple/3`. Exactness is unaffected (the merged type is still final, so the cast is still one compare), but `TypeRefining` still has nothing to refine. Typed slots are what make family structs structurally distinct, which is the next step's subject.
///
/// # Typed slots, 2026-08-20
///
/// Each family slot is now declared at the carrier its recorded shape names rather than uniformly `anyref` — the tag as a packed `i8` read through `struct.get_u`, unsigned and signed immediates as raw `i32`, an `Flt` inline as `f32`, a list at its rope base, a product at its arity's type. Slots are grouped by carrier rather than by field position, so constructors agreeing on a carrier share its slots and only a disagreement costs width; `shapes.rs`'s `slot_layout_probe` is that choice's figure. Same method as family keying above — interleaved run-by-run, min of 7, two passes, load 0.82 at the start and 1.15 at the end — outputs identical across arms and every harness anchor reproduced:
///
/// | Program | before | after | pass 1 | pass 2 |
/// | --- | ---: | ---: | ---: | ---: |
/// | `spines` | 65 ms | 60 ms | **−7.7%** | **−7.7%** |
/// | `trees` | 306 ms | 284 ms | **−7.2%** | **−4.9%** |
/// | `chain` | 110 ms | 107 ms | **−2.7%** | **−2.8%** |
/// | `monad_io` | 232 ms | 234 ms | +0.9% | +0.4% |
/// | `churn` | 340 ms | 343 ms | +0.9% | +0.6% |
/// | `lcg` | 320 ms | 320 ms | +0.0% | −0.3% |
/// | `state_monad` | 182 ms | 182 ms | +0.0% | +0.0% |
/// | `parse_digits` | 332 ms | 339 ms | +1.8% | +2.4% |
///
/// **The split is payload typing, and `parse_digits` is the experiment that shows it.** Its only families are `Option` and `Result`, whose payloads are polymorphic and stay `anyref`, so it is the one program where the tag is typed and nothing else is — and it is the one program that loses. The three that win are exactly the three whose hot loop reads a *typed payload*: `Map/Node`'s `crit`, `Chain/link`'s `Nat`, `trees`' node. So the tag's packing is a small charge and the payload's carrier is what pays for it, which is worth stating because the two arrived in one commit and a single aggregate would have hidden it. The regression was left standing deliberately: it is a fifth the size of what the same mechanism returns elsewhere, and isolating it further would have cost more machine time than the finding is worth.
///
/// Statically the emitted code is uniformly *better* in the losing program too — all 26 of `parse_digits`'s functions shrank or held, `ref.i31` fell 307 to 297 and `ref.cast (ref $tuple/N)` 16 to 10 — so its 2% lives in the engine's object handling rather than in the instruction stream. Over optimized `spines`: `ref.i31` **410 to 363**, `ref.cast (ref $tuple/N)` **17 to 10**, total instructions **8962 to 8758**.
///
/// **The finding family keying left open is closed.** With every slot `anyref` a family struct was structurally identical to a same-width `$tuple/N`, and Binaryen's closed-world type merging folded them together — under family keying alone `spines` shipped exactly one surviving `$row/` type (`/syn/Str/Scan`) and `parse_digits` shipped none, with `/std/Map/Node`'s reads going out as tuple gets. Typed slots make the structs distinct, so the types survive (`spines` 1 to 3, every other program 0–1 to 2–3) and `TypeRefining` has something to work with at last: the emitted module now names `(ref (exact $row/5$/std/Map/Node))` in a signature. The descent loop reads its `crit` straight into `i32.div_u`, with no cast and no unbox between.
///
/// **One negative result, worth as much as the positive ones.** Typing `List` slots at the rope base was meant to bite into the `is_subtype` libcall class, and it did nothing: `ref.cast (ref $rope/N)` is 51 in five programs and 72 in `spines` in *both* arms. Reading where those casts sit says why, and retires the census's framing of them as an opportunity.
///
/// Take the counts before Binaryen, which merges the two rope bases into one and hides the split: **61 casts target `$rope/bin`, 5 target `$rope/list`**, and the remaining 39 target `(sub final …)` subtypes, which short-circuit to inline compares and were never the expensive kind. Every one of `/std/Map/insert1`'s sixteen is a `$rope/bin` — the `Bytes` key path.
///
/// That is the whole answer, and it is a *boundary rather than a backlog*. `Repr::Bin` is sometimes-immediate: since the map wall put a small `Bytes` on the i31 — worth 6.1× on the insert — no local, field, or slot can be declared at `$rope/bin`, because half the population is not a rope at all. The residue that a declared carrier *could* reach is the 5 `$rope/list` sites. So this class is the standing price of the packed immediate, already paid for deliberately and already priced, and not work waiting to be scheduled. What would move it is making the packed carrier always-a-rope, which is the 6.1× being handed back.
///
/// **Where the profile's attribution had expired:** the 2026-08-10 profile named `rng_state` at ~75% interning, but the inline-budget raise recorded in `cps/optimize.rs` has since absorbed that program's `State/bind` chain entirely — its loop is scalar now, closures survive only at its few effect boundaries, and both arms time identically. `state_monad`'s trivial-bind loop specializes the same way. The population the swap re-prices is what the specializers *cannot* reach: the per-step `Io` description and the genuinely unknown per-character step closure — which is exactly the spec's "only re-prices the calls that stay unknown".
///
/// # The two scale questions, answered by the corpus
///
/// The design record owed two measurements: `call_indirect` against many distinct final subtypes in one table, and instantiation of a table at hundreds of entries. The optimized corpus never builds either shape — the largest table any measured program emits is 22 slots (printed below), because dead-code elimination keeps only reachable closure bodies. At that size the answers are subsumed by the product rows above: `monad_io`'s 5.9× is measured *through* a table whose every entry is its own final subtype, and `rng_state`'s startup-dominated 0.04 s is unchanged with the table present, so neither the per-call check nor instantiation is an attributable share at the sizes this compiler emits. A future program with hundreds of live closures re-opens the question; nothing in the corpus can.
///
/// # The constant-closure annex's admission, 2026-08-17
///
/// The final column counts environments materialized once in `$start` — closures whose captures are all interned constants, which the hoister interns now that the code field is an `i32`. Its gate was frequency, and the population is everywhere: at least one per corpus fixture, and 9 of the 19–21 environment constructions in each stdin-driven program (the `/std` description machinery's capture-free thunks are most of them). The rewrite stays.
#[test]
#[ignore = "measurement: records what the closure table costs and saves rather than asserting"]
fn closure_index_dispatch_measurements() {
    const MONAD_IO: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../programs/monad_io.crs"
    ));
    const PARSE_DIGITS: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../programs/parse_digits.crs"
    ));
    const RNG_STATE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../programs/rng_state.crs"
    ));

    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("uncurry", UNCURRY),
        ("string-walk", STRING_WALK),
        ("monad_io", MONAD_IO),
        ("parse_digits", PARSE_DIGITS),
        ("rng_state", RNG_STATE),
    ] {
        let wat = wat(source);
        let slots = wat
            .lines()
            .filter_map(|line| line.trim().strip_prefix("(table $clsr/"))
            .filter_map(|rest| rest.split_whitespace().nth(2))
            .filter_map(|min| min.parse::<usize>().ok())
            .sum::<usize>();
        let dispatches = wat.matches("call_indirect $clsr/").count();
        let environments = wat.matches("struct.new $envr/").count();
        // The constant-closure annex's admission census: environments materialized once at instantiation, each a construction the swap moved out of function code.
        let interned = functions(&wat)
            .iter()
            .find(|function| function.name == "$start")
            .map_or(0, |start| start.body.matches("struct.new $envr/").count());
        println!(
            "{label}: {slots} table slots, {dispatches} dispatch sites, {environments} environment constructions, {interned} interned as consts"
        );
    }
}

/// What the return protocol removes from the corpus, and what that is worth.
///
/// Run it with:
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture split_return_measurements
/// ```
///
/// It asserts nothing. The structural claim is [`a_returned_constructor_is_delivered_as_its_fields`]'s to make and it fails when it stops holding; this only reports how much of the corpus the protocol reaches, which is a question with no right answer to assert against.
///
/// # What it last printed
///
/// Taken **2026-08-12**, **debug**, on the commit that introduced the pass.
///
/// | Fixture | Multi-result types | Allocation sites |
/// | --- | --- | --- |
/// | lcg | 0 | 79 |
/// | trees | 0 | 81 |
/// | higher-order | 0 | 81 |
/// | direct/escaping | 0 | 81 |
/// | function-only | 0 | 79 |
/// | mutual-recursion | 0 | 79 |
/// | split-return | 1 | 79 |
///
/// **The zeroes are not a null result, they are the wrong corpus for the question.** These fixtures take their runtime taint from `proc/args!` and never read stdin, so none of them reaches the UTF-8 decode path where the protocol actually fires. What they do establish is that the pass is inert everywhere it has no candidate — which is most places.
///
/// Across `programs/`, which does read stdin, exactly one function is selected and it is the same one every time: `/syn/Str/classify`. Five return edges, all visible constructions, tagged at index zero, demanded five slots wide — and one of its two call sites sits inside `/std/Str/fold`'s per-character walk. Its emitted body goes from three allocations to none, and `programs/parse_digits.crs` as a whole from 145 to 142.
///
/// **The runtime figure is the one worth reading, and it is small.** Timing `programs/parse_digits.crs` with the pass toggled and nothing else changed, `user` time over repeated runs:
///
/// | Input | Pass off | Pass on |
/// | --- | --- | --- |
/// | 300000 | 0.27, 0.26 | 0.26, 0.25, 0.25 |
/// | 1000000 | 0.95, 0.95 | 0.94, 0.93 |
///
/// Roughly **one to two percent** — consistent in direction across all five pairs and too small to be worth more precision than that. At about 135 ns per character the loop is not allocation-bound on the tuple this removes: it is dominated by the per-character closure call and the transient `Option` that `/std/Nat/of_str`'s lifted fold step allocates, neither of which a return protocol reaches, and both of which belong to the higher-order specialization that succeeds this work.
///
/// **Two things this does not separate.** The allocation counts are taken pre-Binaryen, so some of what the pass now removes earlier, Binaryen may have been removing later — the runtime figure is the only one that accounts for that, and it is the small one. And both binaries come from a debug-profile compiler; whether the gap widens under release is unmeasured.
#[test]
#[ignore = "measurement: reports what the return protocol reaches rather than asserting"]
fn split_return_measurements() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
        ("function-only", FUNCTION_ONLY),
        ("mutual-recursion", MUTUAL_RECURSION),
        ("split-return", SPLIT_RETURN),
    ] {
        let wat = wat(source);
        // A multi-result type is spelled `func/{parameters}/{results}`; the single-result shape keeps the bare `func/{parameters}` and is what every function had before this. Counted off the type *name* in a declaration rather than off slashes in the line, because a function definition names its type too and carries a source hint that is itself full of slashes.
        let split = wat
            .lines()
            .map(str::trim)
            .filter(|line| line.starts_with("(type $func/"))
            .filter(|line| {
                line.split_whitespace()
                    .nth(1)
                    .is_some_and(|name| name.matches('/').count() == 2)
            })
            .count();
        let allocations = wat.matches("struct.new").count() + wat.matches("array.new").count();
        println!("{label}: {split} multi-result types, {allocations} allocation sites");
    }
}

/// What copying more costs, in the two units that can see it.
///
/// Run it with:
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture copy_growth_measurements
/// ```
///
/// It asserts nothing. Lifting the nested-definition refusal lets the inliner and both specializers copy bodies they used to decline, and copying is the one thing that trades size for speed in both directions at once — so the baseline is taken before the change rather than reconstructed after it.
///
/// # Which instrument sees what
///
/// **Peak memory cannot see a transient allocation, and it is not a shortcoming of the measurement.** The return protocol removes roughly one five-field object per character; running `programs/parse_digits.crs` at 1000000 with that pass toggled and nothing else changed gives a maximum resident set of 5 734 400 bytes without it and 5 767 168 bytes with it — flat, and if anything slightly up from the code that replaced it. Transient garbage never accumulates, so its cost is allocation *work* rather than footprint, and that lands on the clock. Retention is the opposite: `trees` holds what it builds, and its resident set moves from 5.77 MB at depth 18 to 271.68 MB at depth 21 on nothing but what it keeps.
///
/// So: **time for a change to transient allocation, resident set for a change to retention, emitted size for a change that copies.** Reaching for the wrong one reports a confident null.
///
/// # The baseline, taken at `82cb8ef7`
///
/// Native binaries built with `cargo run --package curios -- compile <program> -o <path>`, timed with `/usr/bin/time -l`. The binary embeds the runtime launcher, so its absolute size is mostly launcher and only the *difference* between two builds is compiled code.
///
/// | Program | Input | `user` | Max RSS | Binary |
/// | --- | --- | --- | --- | --- |
/// | `parse_digits` | 1000000 | 0.92 s | 5 767 168 B | 3 786 408 B |
/// | `trees` | 21 | 0.23 s | 271 679 488 B | 3 786 504 B |
///
/// What this test itself prints is the third unit — the raw pre-Binaryen module size for each structural fixture, which is where code growth shows up first and without a runtime at all. At the same revision: `lcg` 6708, `trees` 7706, `higher-order` 7160, `direct/escaping` 7174, `function-only` 6632, `mutual-recursion` 6834, `split-return` 8367 bytes.
#[test]
#[ignore = "measurement: reports emitted size rather than asserting"]
fn copy_growth_measurements() {
    for (label, source) in [
        ("lcg", LCG),
        ("trees", TREES),
        ("higher-order", HIGHER_ORDER),
        ("direct/escaping", DIRECT_ESCAPING),
        ("function-only", FUNCTION_ONLY),
        ("mutual-recursion", MUTUAL_RECURSION),
        ("split-return", SPLIT_RETURN),
    ] {
        let module = compile_raw(source);
        let bytes = to_bytes(&module).len();
        println!("{label}: {bytes} bytes");
    }
}
