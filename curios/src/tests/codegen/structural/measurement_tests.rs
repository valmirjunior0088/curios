//! Recorded figures for the structural shapes, each with the command that retakes it. None asserts.

use {super::test_support::*, curios_wasm::to_bytes};

/// What the closure table index is worth at product level.
///
/// Run it with:
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture closure_index_dispatch_measurements
/// ```
///
/// It asserts nothing. The structural claim is [`closures_carry_their_code_as_a_table_index`]'s to make; this prints the static shape of the dispatch over the corpus — table slots, dispatch sites, environment allocations — so the timings below stay pinned to the modules that produced them.
///
/// # What it last printed
///
/// Taken at `447fbb0a1`, x86-64 Linux, debug.
///
/// | Program | Table slots | Dispatch sites | Environment constructions | Interned as consts |
/// | --- | ---: | ---: | ---: | ---: |
/// | `lcg` | 5 | 3 | 2 | 0 |
/// | `trees` | 5 | 3 | 2 | 0 |
/// | higher-order | 7 | 4 | 4 | 2 |
/// | uncurry | 5 | 3 | 2 | 0 |
/// | string-walk | 5 | 3 | 2 | 0 |
/// | `monad_io` | 10 | 25 | 8 | 1 |
/// | `parse_digits` | 8 | 23 | 6 | 1 |
/// | `rng_state` | 8 | 23 | 6 | 1 |
///
/// The closure-free controls sit at five slots and three dispatch sites, which is `/std`'s own plumbing rather than anything the program asked for; the monadic and walking programs are where the table is actually exercised.
///
/// # What the index buys
///
/// A change here is timed as native binaries of two compiler builds, `echo <N> | /usr/bin/time -v <bin>`, arms interleaved run-by-run to keep thermal drift out of the comparison, every pair printing identical output before any figure is read.
///
/// A code field holding an `i32` table index rather than a funcref moves the monadic loop it was priced for: `monad_io` binds a description per step, so each iteration builds one closure and forces it. The string walks move too, since two `call_indirect` per character replace two funcref constructions' interns, and the closure-free controls, `lcg` and `rng_manual`, do not.
///
/// Typing each arity's table `(ref null $clsr/N)`, with bodies declared at the arity type, deletes both per-dispatch engine checks: the `call_indirect` signature check compiles away (the table's element type is the expected type — Wasmtime's `StaticMatch`), and no named-final-subtype mismatch is left to take the `is_subtype` libcall. It moves the two programs whose hot loop dispatches an unknown callee, `monad_io` and `parse_digits`, and leaves the loops carrying no indirect call where they were. What remains per dispatch is one `ref.cast (ref $envr/N)` to the non-final environment supertype, where the closure arrives from a parameter.
///
/// # What keying rows and typing their slots buys
///
/// A family read being one exact cast on the row's own final type, rather than a `ref.test` cascade over the arity roster, moves exactly the programs whose hot loop walks a heap variant family — `parse_digits`, `chain`, `trees`, `spines` — while `lcg`, which declares none, does not move, and over optimized `spines` most of the cascade's `ref.test` and `ref.cast (ref $tuple/N)` are gone.
///
/// Declaring each slot at the carrier its recorded shape names — the tag as a packed `i8` read through `struct.get_u`, a scalar payload raw, an `Flt` inline as `f64`, a list at its rope base, a row at its own type — moves the programs whose hot loop reads a *typed payload*: `spines`, `trees`, `chain`. It costs `parse_digits`, whose only families are `Option` and `Result` with polymorphic payloads that stay `anyref`, a little: the tag's packing is a small charge and the payload's carrier is what pays for it. Typed slots also keep a row's struct distinct from a same-width `$tuple/N`, which Binaryen's closed-world type merging would otherwise fold together, so `TypeRefining` has something to refine — the emitted module names `(ref (exact $row/5$/std/Map/Node))` in a signature, and the descent loop reads its `crit` straight into `i32.div_u`.
///
/// **One negative result.** Typing `List` slots at the rope base moves no `ref.cast (ref $rope/N)`. Counted before Binaryen merges the two rope bases, nearly every such cast targets `$rope/bin` — the `Bytes` key path — and `Repr::Bin` is sometimes-immediate: a small `Bytes` rides the i31, so no local, field or slot can be declared at `$rope/bin`. That class is the standing price of the packed immediate rather than a backlog, and what would move it is making the packed carrier always-a-rope.
///
/// # The two scale questions
///
/// `call_indirect` against many distinct final subtypes in one table, and instantiating a table at hundreds of entries, have no instance in the corpus: the largest table any measured program emits is 22 slots (printed below), because dead-code elimination keeps only reachable closure bodies. At that size neither the per-call check nor instantiation is an attributable share — `monad_io` is measured *through* a table whose every entry is its own final subtype. A program with hundreds of live closures re-opens the question; nothing in the corpus can.
///
/// # Constant closures
///
/// The final column counts environments materialized once in `$start` — closures whose captures are all interned constants, which the hoister interns because the code field is an `i32`. The population is everywhere: at least one per corpus fixture, and 9 of the 19–21 environment constructions in each stdin-driven program, the `/std` description machinery's capture-free thunks most of them.
#[test]
#[ignore = "measurement, counted: records what the closure table costs and saves rather than asserting"]
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
        // Environments materialized once at instantiation, each a construction moved out of function code.
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
/// Taken at `447fbb0a1`, x86-64 Linux, **debug**.
///
/// | Fixture | Multi-result types | Allocation sites |
/// | --- | --- | --- |
/// | lcg | 0 | 91 |
/// | trees | 0 | 92 |
/// | higher-order | 0 | 93 |
/// | direct/escaping | 0 | 92 |
/// | function-only | 0 | 90 |
/// | mutual-recursion | 0 | 90 |
/// | split-return | 1 | 91 |
///
/// **The zeroes are not a null result, they are the wrong corpus for the question.** These fixtures take their runtime taint from `proc/args!` and never read stdin, so none of them reaches the UTF-8 decode path where the protocol actually fires. What they do establish is that the pass is inert everywhere it has no candidate — which is most places.
///
/// The allocation counts are taken pre-Binaryen, so some of what the pass removes earlier, Binaryen may remove later; only a runtime figure accounts for that. Toggling the pass on `programs/parse_digits.crs` and nothing else moved `user` time by one to two percent (debug compiler): the per-character loop is not allocation-bound on the tuple the protocol removes.
#[test]
#[ignore = "measurement, counted: reports what the return protocol reaches rather than asserting"]
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
        // A multi-result type is spelled `func/{parameters}/{results}`; the single-result shape keeps the bare `func/{parameters}`. Counted off the type *name* in a declaration rather than off slashes in the line, because a function definition names its type too and carries a source hint that is itself full of slashes.
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
/// It asserts nothing. The inliner and both specializers copy bodies that nest definitions, and copying is the one thing that trades size for speed in both directions at once — so a change to what they copy is read against a recorded baseline rather than one reconstructed after it.
///
/// # Which instrument sees what
///
/// **Peak memory cannot see a transient allocation, and it is not a shortcoming of the measurement.** The return protocol removes roughly one five-field object per character; running `programs/parse_digits.crs` at 1000000 with that pass toggled and nothing else changed gives a maximum resident set of 5 734 400 bytes without it and 5 767 168 bytes with it — flat. Transient garbage never accumulates, so its cost is allocation *work* rather than footprint, and that lands on the clock. Retention is the opposite: `trees` holds what it builds, and its resident set moves from 5.77 MB at depth 18 to 271.68 MB at depth 21 on nothing but what it keeps.
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
/// What this test itself prints is the third unit — the raw pre-Binaryen module size for each structural fixture, which is where code growth shows up first and without a runtime at all. Taken at `447fbb0a1`: `lcg` 23530, `trees` 24667, `higher-order` 23905, `direct/escaping` 23669, `function-only` 23088, `mutual-recursion` 23259, `split-return` 29800 bytes. The baseline above is `82cb8ef7`'s and has not been retaken, so the two are not read against each other.
#[test]
#[ignore = "measurement, counted: reports emitted size rather than asserting"]
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
