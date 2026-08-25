//! The string-walk ladder: what one character of an idiomatic walk costs, divided across the three programs written to divide it.
//!
//! `programs/parse_digits.crs`, `programs/parse_bindless.crs` and `programs/parse_manual.crs` decode the same digit string the same number of times and differ only in what they pay for it. Each step down the ladder removes exactly one cost:
//!
//! | Rung | UTF-8 scan | Closure per character | Bind per character |
//! | --- | --- | --- | --- |
//! | `parse_digits` | yes | yes | yes |
//! | `parse_bindless` | yes | yes | no |
//! | `parse_manual` | no | no | no |
//!
//! So `digits − bindless` is the bind, `bindless − manual` is the closure plus the scan, and the bottom rung is a *ceiling rather than an equivalent* — it declines work the abstraction performs rather than performing it more cheaply.
//!
//! **The ladder existed for the length of three roadmap items before anything ran it.** These are real programs rather than fixtures precisely because the question is what idiomatic code costs, and a fixture written to be measured tends to answer a question nobody asked.
//!
//! Timings that a walk-directed change produces are recorded here, controls included: a campaign that moves this ladder owes a reading of the two harness workloads beside it, because "moved the column it aimed at" is only a claim if the other columns were looked at.

use {
    super::structural::{compile_raw, functions, user_allocations, wat},
    curios_runtime::{ForeignBindings, MockHost, precompile, run_bytes},
    curios_wasm::to_bytes,
    std::time::Instant,
};

const PARSE_DIGITS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_digits.crs"
));
const PARSE_BINDLESS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_bindless.crs"
));
const PARSE_MANUAL: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_manual.crs"
));
const PARSE_MULTIBYTE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_multibyte.crs"
));

const WALK_MIRROR_BASELINE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_baseline.crs"
));
const WALK_MIRROR_FLAT_ACC: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_flat_acc.crs"
));
const WALK_MIRROR_HELD_SCAN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_held_scan.crs"
));
const WALK_MIRROR_INLINE_STEP: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_inline_step.crs"
));
const WALK_MIRROR_INDEXED: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/walk_mirror_indexed.crs"
));

/// Every static figure this attribution leans on, taken over the three programs above.
///
/// # How to run it
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture string_walk_ladder_measurements
/// ```
///
/// It asserts nothing and cannot fail — a measurement that fails is a measurement with an opinion. Allocation *sites* are counted, not allocations: a site inside the per-character walk runs once per character and a site in the program's setup runs once, and this instrument cannot tell them apart. That is what the timings below are for, and why neither half is quoted without the other.
///
/// # The dynamic half, and how to retake it
///
/// Timings are taken over native binaries rather than in-process, because the in-process module is the raw pre-Binaryen one and the native path is what a user runs:
///
/// ```sh
/// make curios/runtime
/// cargo build --package curios
/// cargo run --package curios -- compile programs/parse_digits.crs -o /tmp/parse_digits
/// echo 1000000 | /usr/bin/time -v /tmp/parse_digits    # and the same for the other two rungs
/// ```
///
/// Check the output, not just the clock: all three programs print the same number for the same input, and a rung that has stopped agreeing is measuring something else.
///
/// # The regime, which is not the one the prose said
///
/// N is read from stdin and the string decoded is `Nat/to_str(n)`, so at N = 1 000 000 these programs decode a **seven-character** string a million times — seven million characters through a seven-link chain, not one walk over a million. Per-call overhead amortizes differently in the two regimes, so a per-character figure derived here does not transfer to a long string without saying so.
///
/// # What it last printed
///
/// Taken **2026-08-13**, debug-profile compiler, native binaries, Linux, at N = 1 000 000.
///
/// ## The division, before anything was changed (`fb1db1ea`)
///
/// | Rung | `user` | Isolates | `Str/fold` envs | slice calls |
/// | --- | --- | --- | --- | --- |
/// | `parse_digits` | 2.31, 2.32, 2.30, 2.31, 2.33 | — | 2 | 17 |
/// | `parse_bindless` | 2.22, 2.25, 2.22 | the bind | 2 | — |
/// | `parse_manual` | 0.17, 0.17, 0.17 | the closure *and* the scan | 2 | — |
///
/// **The bind is about six percent of the gap** — 2.31 to 2.23 — and the whole gap is about fourteenfold. The remaining 2.06 s is the closure and the scan *together*, and this ladder cannot divide that pair: that needs a fourth program, or a change removing one of them, which is what makes a library reformulation an instrument as well as a fix.
///
/// ## After the lowering stopped materializing an unread fold suffix (`513ac6bc`)
///
/// | Rung | `user` | Change |
/// | --- | --- | --- |
/// | `parse_digits` | 2.21, 2.22, 2.21, 2.22, 2.22 | −4%, slice calls 17 to 15 |
/// | `parse_bindless` | 2.05, 2.06, 2.05 | −8% |
/// | `parse_manual` | 0.18, 0.17, 0.17 | unchanged, and expected to be: its hot path never folds |
///
/// ## After the walk threaded its accumulator through the recursion (`06b54ece`)
///
/// | Rung | `user` | Change |
/// | --- | --- | --- |
/// | `parse_digits` | 1.06, 1.06, 1.07, 1.07, 1.07 | −52% |
/// | `parse_bindless` | 1.00 ×5 | −51% |
/// | `parse_manual` | 0.16, 0.16, 0.17 | unchanged |
///
/// The gap is now about sevenfold, from fourteen. **What bought it was removing the chain, not removing an allocation** — the `Str/fold` environments go two to none, while slice calls *rise* to 18, because the reformulated walk materializes the tail it recurses on. The old shape built N closures backward and applied them forward, two passes and an indirect call per character; the new one makes a single forward pass with a direct tail call. A prediction of "one allocation traded for another" was made before this was measured and was wrong about the size for exactly that reason: it counted allocations and the cost was the traversal.
///
/// ## After emptiness stopped being decided by a walk (`493f8503`)
///
/// | Rung | `user` | Change |
/// | --- | --- | --- |
/// | `parse_digits` | 0.89, 0.91, 0.90, 0.90, 0.90 | −16% |
/// | `parse_bindless` | 0.93, 0.92, 0.92, 0.93, 0.93 | −8% |
/// | `parse_manual` | 0.16 ×3 | unchanged |
///
/// `/std/Nat/of_str` tested `Str/len(s) == 0`, and `Str/len` is `count_scalars` — so every parse walked its input twice, once to ask whether it was empty and once to read it. Five other sites in `/std` asked the same question the same way.
///
/// **The bind has inverted, and that retires a suspect.** `parse_digits` now beats `parse_bindless` — 0.89–0.91 against 0.92–0.93, distributions barely overlapping — although it does strictly more work, since `!` is what separates them. The bind measured about six percent at the top of this table and measures nothing now. Read it as an upper bound that has closed rather than as evidence `!` is free: two rungs this close are separated by code layout as much as by work.
///
/// ## What one character still costs
///
/// Counted in the emitted body of `/std/Str/fold`, which has three arms — `lead`, `cont` and `bad` — so exactly one of each runs per character:
///
/// | Per character | Site |
/// | --- | --- |
/// | read the byte | `call $bytes/read` |
/// | **allocate a rope view** for the tail | `call $bytes/slice` |
/// | **allocate the scan state** to hand to `step` | `struct.new $tuple/4` |
/// | the UTF-8 scan | `call $syn/Str/step`, then `classify` |
/// | **indirect call** through `f` | `call_ref $clsr/2` |
/// | **allocate the accumulator tuple** | `struct.new $tuple/2` |
///
/// Three of those six are allocations, and **none is confined to the iteration that builds it**: each crosses the loop's own edge, so the region is the loop and its backedges rather than one lexical iteration. The accumulator tuple is the least obvious and the best example: nobody wrote it, it exists because the fold threads `{A, Nat}`. The indirect call is its own roadmap item. The scan is the work the abstraction performs and the control declines, which is what makes `parse_manual` a ceiling rather than an equivalent.
///
/// **The scan state was missing from this table, and how it was missing is the reusable part.** The row below it reports the scan as *work* — two calls — and an allocation handed **to** a call is not where a reader counting allocation sites looks. It is `Scan/cont(rem, lo, hi)`, destructured out of the loop's own parameter and rebuilt field by field purely to be passed in, so it is a reconstruction rather than a value anyone wrote.
///
/// Retaken **2026-08-13** by reading the emitted body rather than grepping the module, which is what the count above should have done from the start:
///
/// ```sh
/// cargo run --package curios -- wonder stage wasm programs/parse_digits.crs
/// ```
///
/// The raw pre-Binaryen module goes to stderr. `$func/…$/std/Str/fold` then holds five `struct.new $tuple/2` — four in the arms, plus the seed built before the loop — one `struct.new $tuple/4`, three `call $bytes/slice`, three `call $bytes/read` and two `call_ref $clsr/2`. Exactly one arm runs per character, which is what turns those site counts into the per-character row above.
///
/// **The split between those six is unmeasured**, and dividing it needs one instrument each rather than another rung: removing the three allocations measures them, specializing `f` measures the call, and what remains is the scan.
///
/// ## After the scan argument stopped being rebuilt (2026-08-16)
///
/// The `struct.new $tuple/4` row above was a spelling, not a compiler obligation. The cont arm passed `step(h, Scan/cont(rem, lo, hi))` with `sc` — the parameter holding exactly that value — in scope, and match refinement makes the two spellings definitionally equal, so `/std` now passes the held parameter at every such site: `fold`, `at`, `utf8/drop_width`, `utf8/count_scalars`, and their proof twins, which keeps function and proof unfolding in the same spelling. The kernel recertified the prelude over the change, and the walk, `len` and `slice` each lost a per-continuation-byte allocation at once.
///
/// The emitted fold body for `programs/parse_multibyte.crs` now holds five `struct.new $tuple/2`, no `struct.new $tuple/4`, three `call $bytes/slice`, three `call $bytes/read` and two `call_ref $clsr/2`, and the only arity-4 constructions left in that module are the two genuine transitions inside `/syn/Str/step` — the probe now spelled [`the_per_character_walk_carries_its_scan_without_allocating`] held both facts. Timed **2026-08-17** with the native-binary protocol above, five runs each, `user` seconds:
///
/// | Program | before | after |
/// | --- | --- | --- |
/// | `parse_digits` at N = 1 000 000 | 0.90 0.89 0.89 0.89 0.89 | 0.91 0.91 1.07 0.91 0.91 |
/// | `parse_multibyte` at N = 300 000 | 0.77 0.77 0.76 0.77 0.76 | 0.75 0.75 0.76 0.75 0.75 |
///
/// The digit control is unchanged within layout noise — its walk never enters the cont arm — and the multi-byte walk, 16 of whose 28 bytes are continuation bytes, gains about two percent. One heap construction per continuation byte is therefore worth about two percent of this walk, a calibration [`walk_mirror_attribution_measurements`] cross-checks from the outside.
///
/// ## After step's result crossed as fields (M1a, 2026-08-17)
///
/// Interprocedural demand made `split_returns` eligible on `/syn/Str/step`'s component — once `check` stopped capturing the scan into a closure chain — so step returns four fields and constructs nothing. What each caller does with them is the measured story: every resume must rebuild the tuple to jump it into the loop, and dynamic fields admit no interning, so the digit walk that used to receive an interned `lead()` for free now reboxes per character:
///
/// | Program | after the sweep | after M1a |
/// | --- | --- | --- |
/// | `parse_digits` at N = 1 000 000 | 0.91 0.91 1.07 0.91 0.91 | 0.96 ×5 |
/// | `parse_multibyte` at N = 300 000 | 0.75 0.75 0.76 0.75 0.75 | 0.78 0.77 0.77 0.77 0.78 |
///
/// This is the reboxing mode GHC's boxity analysis names, measured live at about five percent, and it is deliberate: the value-lifetime campaign shipped its demand deferral first because it is the cheapest change that measures the fact, and M2's continuation splitting is what turns the relocated construction into flowing fields — the ASCII path then rebuilds nothing, and the cont arm materializes once per multi-byte character at the step call alone.
///
/// The `check` rework in the same change also removes a closure per validated byte from every `of_bytes`: the validator was an induction returning a function of the scan, and now threads the scan by recursion exactly as the fold does.
///
/// ## After the accumulator crossed as fields (M2, 2026-08-17)
///
/// Continuation scalar replacement threads the `{A, Nat}` accumulator through the contified loop as two field parameters, seed included — the emitted fold body holds no `struct.new $tuple/2` at all — and with it the M1a reboxing recedes on every path that stopped carrying the tuple:
///
/// | Program | after M1a | after M2 |
/// | --- | --- | --- |
/// | `parse_digits` at N = 1 000 000 | 0.96 ×5 | 0.88 0.88 0.87 0.87 0.87 |
/// | `parse_multibyte` at N = 300 000 | 0.78 0.77 0.77 0.77 0.78 | 0.65 0.65 0.68 0.65 0.65 |
///
/// The digit walk lands *below* every figure this ladder has recorded for it, and the multi-byte walk gains about thirteen percent against its pre-campaign baseline. The scan rebuild remains — three arity-4 constructions, one per arm — and for a recorded reason: the loop's scan parameter mixes arity-1 interned constants with the resumes' arity-4 rebuilds, the variant-width shape no exact product describes, so its removal awaits either variant-aware splitting or a uniform-width variant lowering, each a measured decision `documentation/design/toolchain/a-value-costs-when-it-is-kept-not-when-it-is-named.md` records. The suffix view is M4's, and it is now the walk's dominant remaining obligation.
///
/// ## After the suffix crossed as a window (M4, 2026-08-17)
///
/// The window split virtualizes the suffix: the loop carries `(base, offset, length)`, the per-character `call $bytes/slice` and the view it allocated are gone together — the half an allocation count does not see — and every read is an index into one payload:
///
/// | Program | after M2 | after M4 |
/// | --- | --- | --- |
/// | `parse_digits` at N = 1 000 000 | 0.88 0.88 0.87 0.87 0.87 | 0.77 0.76 0.77 0.76 0.77 |
/// | `parse_multibyte` at N = 300 000 | 0.65 0.65 0.68 0.65 0.65 | 0.59 0.58 0.59 0.59 0.59 |
///
/// Against the pre-campaign baseline the digit walk is fifteen percent faster and the multi-byte walk twenty-three, with the variant-width scan rebuild the one obligation left on the per-character path. The spec's ordering held: the window was the destination, and the tuple work was its substrate.
///
/// ## After the scan crossed as fields (variant-width, 2026-08-17)
///
/// The last obligation on the per-character path. The loop's scan parameter travels as a discriminant and three payload slots with the interned nullary constructors filled, and `/syn/Str/step` takes those four slots as parameters, so the emitted `/std/Str/fold` body holds no `struct.new` of any kind.
///
/// **This row carries its own control, and every row above it should be read as though it did not.** The rows above compare against figures taken days and several commits apart — the closure-table swap alone moved these walks by 27% and 11% — so a delta read across two of them is not attributable to the change named beside it. Both columns here were built from the same tree at two commits and timed in one interleaved session on one machine: the control is `9e7e81e8`, the campaign's own base.
///
/// | Program | control (`9e7e81e8`) | after variant-width |
/// | --- | --- | --- |
/// | `parse_digits` at N = 1 000 000 | 0.66 0.68 0.67 0.67 0.65 | 0.46 0.47 0.48 0.47 0.47 |
/// | `parse_multibyte` at N = 300 000 | 0.55 ×5 | 0.43 0.42 0.43 0.42 0.42 |
///
/// **The digit walk gains more than the multi-byte one — 29% against 23% — and that inversion is the point.** A digit walk never enters the `cont` arm, so before this change its scan was the interned `lead()` constant, which is exactly what M1a stopped being able to hand back for free: the row above records the digit walk *regressing* five percent when the return protocol reached `step`, because dynamic result fields admit no interning and every resume reboxed. That relocation is what this change collects, and it collects most where the constant used to be free.
///
/// **The two harness workloads are the controls, and both are flat** — same protocol, same session, same control commit, outputs compared before any figure was read:
///
/// | Program | Input | control (`9e7e81e8`) | after variant-width |
/// | --- | --- | --- | --- |
/// | `lcg` | 100 000 000 | 0.31 ×5 | 0.31 ×5 |
/// | `trees` | 21 | 0.26 0.24 0.24 0.25 0.25 | 0.26 0.26 0.26 0.25 0.24 |
///
/// `trees` max RSS was flat too, ~134.5 MB on both arms. Each is flat for its own stated reason rather than by luck. `lcg`'s hot loop matches on `Nat`, an intrinsic riding the i31 carrier, so the program contains no variant family for a width to reach. `trees` contains one — but a node is stored in its parent, so its values *rest*, and a rewrite that only removes objects in flight has nothing to take: the same fact, observed dynamically, that the census's return gate reports statically and that refuses the return milestone.
///
/// ## The static half, and why it divides almost nothing
///
/// Retaken **2026-08-17** after variant-width splitting. The figures it replaces predate the window split and the closure table and had not been retaken, so read the block as a fresh reading rather than as a delta:
///
/// ```text
/// parse_digits:   0 closure sites, 2 env sites, 10 slice calls, 443738 bytes of wat
/// parse_bindless: 0 closure sites, 3 env sites, 10 slice calls, 458940 bytes of wat
/// parse_manual:   0 closure sites, 2 env sites, 10 slice calls, 496933 bytes of wat
/// ```
///
/// Two, three and two env sites span a sixfold spread in runtime, and the ones that remain belong to `/std/Nat/of_str/1` and `/std/Str/trim_bounds/1` in all three programs — `parse_manual` included, because every rung parses its stdin the same way. **A site count cannot see a loop.** It is kept because it is the half that survives a machine change, and because a site appearing or vanishing is a real event — the `Str/fold` environments vanishing is how the walk's chain was confirmed gone. It is never the half that answers "what does this cost".
///
/// **Two comparisons this does not license.** An earlier record in `structural.rs` timed `parse_digits` at 0.92–0.95 s with `/usr/bin/time -l`, which is macOS syntax; these are Linux figures from another machine, so only the ratios transfer. And the roadmap's "roughly eightfold" for this gap has no probe at all — fourteenfold is what this tree and this machine report.
#[test]
#[ignore = "measurement: divides the string-walk gap rather than asserting"]
fn string_walk_ladder_measurements() {
    for (label, source) in [
        ("parse_digits", PARSE_DIGITS),
        ("parse_bindless", PARSE_BINDLESS),
        ("parse_manual", PARSE_MANUAL),
    ] {
        let wat = wat(source);
        // The three allocation kinds `structural.rs` separates. A closure's *environment* is the one that answers this document's question: `$envr/<N>$<hint>` carries the hint of the function whose closure it is, so the walk's own allocations can be named rather than merely counted.
        let closures = user_allocations(&wat, "struct.new $clsr/").len();
        let envs = user_allocations(&wat, "struct.new $envr/");
        // A rope view is allocated *inside* the shared `slice` helper, so no `struct.new` site in this module moves when a caller stops slicing — the call sites are what move. Counted directly rather than through [`user_allocations`], whose `$io/` separation exists for instructions that name their own definition and would report nothing here.
        let slices = wat
            .lines()
            .map(str::trim)
            .filter(|line| {
                line.starts_with("call $bytes/slice") || line.starts_with("call $list/slice")
            })
            .count();
        println!(
            "{label}: {closures} closure sites, {} env sites, {slices} slice calls, {} bytes of wat",
            envs.len(),
            wat.len(),
        );

        // Which environments the string walk itself allocates. A site is static and the walk is a loop, so each of these runs once per character rather than once per program — which is exactly why this half cannot be read without the timings above.
        let mut walk: Vec<&str> = envs
            .iter()
            .filter_map(|line| line.split("struct.new ").nth(1))
            .filter(|name| name.contains("/Str/") || name.contains("/Nat/of_str"))
            .collect();
        walk.sort_unstable();
        for name in walk {
            println!("    {name}");
        }
    }
}

// -- the mirror family -------------------------------------------------------
//
// `programs/walk_mirror_*.crs` are five user-level mirrors of the fold's walk over the same multi-byte text: a faithful baseline, and one program per removed obligation — the accumulator tuple (`flat_acc`), the scan argument reconstruction (`held_scan`), the returned scan state (`inline_step`), and the suffix view (`indexed`). They exist because the fold's own obligations cannot be removed one at a time without the compiler capabilities this family is measuring the case for, and they are bounds rather than equivalents: each removal necessarily reshapes the arms around it, and the mirrors carry no validity witness.

/// The module-wide counts the family's claims are stated over: arity-4 tuple constructions (the scan shape), arity-2 tuple constructions (the accumulator shape), rope-slice calls, and calls to the mirror's own `mstep`.
fn mirror_counts(wat: &str) -> (usize, usize, usize, usize) {
    let lines = |needle: &str| {
        wat.lines()
            .map(str::trim)
            .filter(|line| line.starts_with(needle))
            .count()
    };
    (
        lines("struct.new $tuple/4"),
        lines("struct.new $tuple/2"),
        lines("call $bytes/slice"),
        wat.lines()
            .map(str::trim)
            .filter(|line| line.starts_with("call $func/") && line.contains("mstep"))
            .count(),
    )
}

/// The calls to `needle` lexically inside a `loop`, anywhere in the module: the per-character cost, which a module-wide count cannot tell from the one-time plumbing around the walk. Nesting is read off the printer's indentation, which the emitter keeps exact.
fn calls_inside_loops(wat: &str, needle: &str) -> usize {
    let mut open = Vec::<usize>::new();
    let mut count = 0;
    for line in wat.lines() {
        let indent = line.len() - line.trim_start().len();
        let line = line.trim();
        while open
            .last()
            .is_some_and(|&loop_indent| loop_indent >= indent)
        {
            open.pop();
        }
        if line.starts_with("loop ") {
            open.push(indent);
        } else if line.starts_with(needle) && !open.is_empty() {
            count += 1;
        }
    }
    count
}

/// What the family guards changed when the compiler caught up with it — twice. The two allocation rungs were built to isolate obligations by *removing* them at the source, and continuation scalar replacement (M2, 2026-08-17) erased those obligations from the baseline itself, so `held_scan` compiles to counts identical to the baseline's and `flat_acc` no longer saves a single accumulator construction. That equality is the campaign's headliner delivered for these spellings — naming the value stopped costing — and this guard holds it. Then window virtualization reached the walk in the faithful spelling too (2026-08-21), so `indexed` stopped shedding a slice the baseline pays; the fact that survives is stronger than the comparison it replaces, and is stated below. The step rung keeps its spelling-structural fact: inlining the step removes its calls.
///
/// **The slice claim is per loop, because the module-wide count could not see the event it guards.** The walk is inlined into the entry, so a slice leaving the walk's loop read as 11 → 10 in `mirror_counts` and nothing more — and the same count is what let the family pass for three days while every spelling's walk sliced a fresh rope per character. The reason was order, not capability: `split_windows` took the first admissible region rather than the widest, and ran before the continuation the walk would turn out to flow into existed, so a one-continuation sub-region below the walk claimed the positions and the walk's own region was declined for the rest of the compilation.
#[test]
fn walk_mirror_family_isolates_each_obligation() {
    let baseline_wat = wat(WALK_MIRROR_BASELINE);
    let held_scan_wat = wat(WALK_MIRROR_HELD_SCAN);
    let indexed_wat = wat(WALK_MIRROR_INDEXED);
    let baseline = mirror_counts(&baseline_wat);
    let flat_acc = mirror_counts(&wat(WALK_MIRROR_FLAT_ACC));
    let held_scan = mirror_counts(&held_scan_wat);
    let inline_step = mirror_counts(&wat(WALK_MIRROR_INLINE_STEP));
    let indexed = mirror_counts(&indexed_wat);

    assert_eq!(
        held_scan, baseline,
        "naming the reconstruction no longer costs: the spellings compile identically",
    );
    assert!(
        baseline.1 <= flat_acc.1,
        "the idiomatic accumulator pays no more tuples than the hand-flattened spelling: baseline {baseline:?}, flat_acc {flat_acc:?}",
    );
    assert_eq!(
        inline_step.3, 0,
        "inline_step calls no step at all: {inline_step:?}",
    );
    assert!(
        baseline.3 >= 3,
        "the baseline pays a step call per arm: {baseline:?}",
    );
    assert_eq!(
        indexed.2, baseline.2,
        "indexing no longer sheds a slice the baseline pays: baseline {baseline:?}, indexed {indexed:?}",
    );
    for (label, wat) in [
        ("baseline", &baseline_wat),
        ("held_scan", &held_scan_wat),
        ("indexed", &indexed_wat),
    ] {
        assert_eq!(
            calls_inside_loops(wat, "call $bytes/slice"),
            0,
            "{label}'s per-character path slices nothing: the walk is a virtualized window",
        );
    }
}

/// Probe one of the four attributions: the returned scan state, tracked by where its construction lives — and the rung at which it stops having one.
///
/// M1a's interprocedural demand made `split_returns` eligible on the step component, so `/syn/Str/step` hands its `Scan` back as four results rather than a tuple; each caller's resume then rebuilt that tuple once per arm, which is the allocation this probe used to pin at three sites. Variant-width splitting removed them in two moves. The fold's loop carries the scan as a discriminant and three payload slots, the interned nullary constructors entering it as one field and filler — which left exactly one construction, the head materialization the known call to `step` kept alive, because `step` took the aggregate whole. Splitting `step`'s own parameter removed that boundary too.
///
/// **So the per-character path of an idiomatic UTF-8 walk allocates nothing.** `step` takes four field parameters beside its byte and hands back four results, constructing no scan at either end, and the fold's whole body carries no `struct.new` of any kind: not the accumulator, not the suffix view, not the scan. That last assertion is the strongest form this probe can take and is deliberately about *every* allocation rather than the tuple shapes the campaign named, because a rewrite that moved the cost into some other object would satisfy the narrow reading and fail this one.
#[test]
fn the_per_character_walk_carries_its_scan_without_allocating() {
    let wat = wat(PARSE_MULTIBYTE);
    let split = functions(&wat);
    let count_in = |body: &str, needle: &str| {
        body.lines()
            .map(str::trim)
            .filter(|line| line.starts_with(needle))
            .count()
    };

    let step = split
        .iter()
        .find(|function| function.name.contains("/syn/Str/step"))
        .expect("step survives as a function in this module");
    assert!(
        step.body.contains("(type $func/5/4)"),
        "step takes the scan as four field parameters beside its byte and hands back four results — both halves of the protocol live on its component: {}",
        step.body.lines().next().unwrap_or_default(),
    );
    assert_eq!(
        count_in(step.body, "struct.new"),
        0,
        "and step constructs no scan at either end",
    );

    // The fold's per-character shape. The accumulator travels as fields, the suffix view is virtualized — the walk carries `(base, offset, length)` through the loop, its slice an extent guard plus an offset sum — and the scan travels as a discriminant and three payload slots through the loop *and* through the call that consumes it.
    let fold = split
        .iter()
        .find(|function| function.name.contains("$/std/Str/fold"))
        .expect("the fold survives as a function in this module");
    let in_fold = |needle: &str| count_in(fold.body, needle);
    assert_eq!(
        in_fold("struct.new"),
        0,
        "the walk's body allocates nothing at all:\n{}",
        fold.body,
    );
    // Matched on the name rather than through `in_fold`, because the emitted call spells the callee's index before its hint and that index is not stable across passes.
    assert_eq!(
        fold.body.matches("$/syn/Str/step").count(),
        3,
        "the scan step is still a known call per arm — the fields travel through it rather than around it",
    );
    assert_eq!(
        in_fold("call $bytes/slice"),
        0,
        "the suffix view is a virtual window: no helper call, no view allocation",
    );
    // One site, not one per arm, since 2026-08-18. The dead-hypothesis peel in `into_ersd::eliminate` used to *substitute* `get(b, 0)` for the bound head, so the read was copied into every arm that mentioned it; it now emits the read once and binds a name. The per-character cost is unchanged — exactly one arm runs, so it was always one read per character — and the table above still reads correctly; what collapsed is the static site count.
    assert_eq!(
        in_fold("call $bytes/read"),
        1,
        "the byte is read once and shared by the arms, not re-read per arm",
    );
    assert_eq!(
        in_fold("call_indirect $clsr/2 $clsr/2"),
        2,
        "the user's step closure"
    );
}

/// The attribution measurement over the mirror family: static counts, raw-and-Binaryen agreement at a small input, and the recorded native timing protocol. Run explicitly:
///
/// ```sh
/// cargo test --package curios --lib --all-features -- --ignored --nocapture walk_mirror_attribution_measurements
/// ```
///
/// # The dynamic half, and how to retake it
///
/// Timings are taken over native binaries, same protocol as [`string_walk_ladder_measurements`]; the in-process route was tried and rejected, because the debug-built runtime executes the same modules fourteen to forty times slower than the native binaries — the GC libcalls compile at opt-level zero — which overweights exactly the allocation shares this family exists to divide.
///
/// ```sh
/// make curios
/// ./target/release/curios compile programs/walk_mirror_baseline.crs -o /tmp/wm-baseline    # and the other four
/// echo 300000 | /usr/bin/time -f "%U" /tmp/wm-baseline                                    # five runs each
/// ```
///
/// # What it last measured
///
/// Taken **2026-08-17**, native binaries, Linux, N = 300 000, five runs each, `user` seconds:
///
/// | Rung | `user` | Isolates | Reading |
/// | --- | --- | --- | --- |
/// | `baseline` | 0.76 0.76 0.84 0.73 0.71 | — | within noise of `parse_multibyte`'s 0.75–0.76, so the mirror calibrates against the real fold |
/// | `flat_acc` | 0.62 0.60 0.53 0.59 0.65 | the accumulator tuple | roughly a fifth of the walk |
/// | `held_scan` | 0.69 0.68 0.75 0.67 0.66 | the scan argument reconstruction | roughly 7% here; the real fold's own sweep measured ~2%, and the real figure is the sweep's — the mirror's smaller step inlines differently |
/// | `inline_step` | 0.57 0.58 0.74 0.64 0.59 | the returned scan state and its call | roughly a fifth, read as a bound: inlining also deduplicates a range test |
/// | `indexed` | 1.22 1.22 1.19 1.23 1.01 | nothing — a negative result | `Bytes/get`'s checked `Option` path costs more than the suffix view it replaces, so this rung cannot attribute the suffix view; M4's own transformation is that obligation's only honest instrument |
///
/// The two shares this family does measure — the accumulator and the returned scan — are each around twenty percent of the walk, which is the yield evidence M2 and M1a proceed on.
#[test]
#[ignore = "measurement: attributes the walk's obligations rather than asserting"]
fn walk_mirror_attribution_measurements() {
    let input = "2000";
    let mut agreed = None;
    for (label, source) in [
        ("baseline", WALK_MIRROR_BASELINE),
        ("flat_acc", WALK_MIRROR_FLAT_ACC),
        ("held_scan", WALK_MIRROR_HELD_SCAN),
        ("inline_step", WALK_MIRROR_INLINE_STEP),
        ("indexed", WALK_MIRROR_INDEXED),
    ] {
        let module = compile_raw(source);
        println!("{label:12} {:?} (tuple4, tuple2, slice, mstep calls)", {
            let printed = module.to_string();
            mirror_counts(&printed)
        });
        let raw = precompile(&to_bytes(&module)).expect("raw module precompiles");
        let optimized = crate::to_cwasm(&module).expect("binaryen path precompiles");
        for (kind, cwasm) in [("raw", &raw), ("binaryen", &optimized)] {
            let (system, io) = MockHost::builder().stdin_lines([input]).build();
            let start = Instant::now();
            run_bytes(cwasm, system, ForeignBindings::empty()).expect("mirror executes");
            let elapsed = start.elapsed().as_secs_f64();
            let printed = String::from_utf8_lossy(&io.output()).trim().to_string();
            println!("{label:12} {kind:9} {elapsed:.3}s prints {printed}");
            let agreed = agreed.get_or_insert_with(|| printed.clone());
            assert_eq!(*agreed, printed, "{label} disagrees with the family");
        }
    }
}
