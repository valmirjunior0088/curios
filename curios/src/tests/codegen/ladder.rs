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

use super::structural::{user_allocations, wat};

const PARSE_DIGITS: &str = include_str!("../../../../programs/parse_digits.crs");
const PARSE_BINDLESS: &str = include_str!("../../../../programs/parse_bindless.crs");
const PARSE_MANUAL: &str = include_str!("../../../../programs/parse_manual.crs");

/// Every static figure the string-walk specification's attribution leans on, taken over the three programs it names.
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
/// The residue — `bindless` 1.00 against `manual` 0.16 — is the UTF-8 scan **plus one rope-view allocation per character**, which is the successor specification's subject rather than this one's.
///
/// ## The static half, and why it divides almost nothing
///
/// ```text
/// parse_digits:   0 closure sites, 4 env sites, 0 shell sites, 18 slice calls, 344965 bytes of wat
/// parse_bindless: 0 closure sites, 5 env sites, 0 shell sites, 19 slice calls, 381463 bytes of wat
/// parse_manual:   0 closure sites, 4 env sites, 0 shell sites, 18 slice calls, 382690 bytes of wat
/// ```
///
/// Four, five and four env sites span a sevenfold spread in runtime, and the remaining ones belong to `/std/Str/utf8/check` and `/std/Nat/of_str` in all three programs — `parse_manual` included, because every rung parses its stdin the same way. **A site count cannot see a loop.** It is kept because it is the half that survives a machine change, and because a site appearing or vanishing is a real event — the `Str/fold` environments vanishing is how the walk's chain was confirmed gone. It is never the half that answers "what does this cost".
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
        let shells = user_allocations(&wat, "struct.new_default").len();
        // A rope view is allocated *inside* the shared `slice` helper, so no `struct.new` site in this module moves when a caller stops slicing — the call sites are what move. Counted directly rather than through [`user_allocations`], whose `$io/` separation exists for instructions that name their own definition and would report nothing here.
        let slices = wat
            .lines()
            .map(str::trim)
            .filter(|line| {
                line.starts_with("call $bytes/slice") || line.starts_with("call $list/slice")
            })
            .count();
        println!(
            "{label}: {closures} closure sites, {} env sites, {shells} shell sites, {slices} slice calls, {} bytes of wat",
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
