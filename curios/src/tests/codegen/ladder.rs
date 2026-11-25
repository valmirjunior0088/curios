//! What an idiomatic string walk emits, which the bench's ladder (`xbench/src/measurements/ladder.rs`) times: the walk mirrors each remove one obligation, a fold's step is copied into each width it is called at, and the per-character path allocates nothing.

use crate::test_support::*;

const PARSE_DIGITS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_digits.crs"
));
const PARSE_BINDLESS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/parse_bindless.crs"
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

// -- the mirror family -------------------------------------------------------
//
// `programs/walk_mirror_*.crs` are five user-level walks over the same multi-byte text that read one byte a turn and carry a scan state between bytes: a baseline, and one program per removed obligation — the accumulator tuple (`flat_acc`), the scan argument reconstruction (`held_scan`), the returned scan state (`inline_step`), and the suffix view (`indexed`). They exist because an obligation is attributed by removing it at the source, a program each, and they are bounds rather than equivalents: each removal necessarily reshapes the arms around it, and the mirrors carry no validity witness.

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

/// What the family guards: the allocation rungs were built to isolate obligations by *removing* them at the source, and continuation scalar replacement erases those obligations from the baseline itself, so `held_scan` compiles to counts identical to the baseline's and `flat_acc` saves no accumulator construction — naming the value costs nothing. Window virtualization reaches the walk in the baseline spelling too, so `indexed` sheds no slice the baseline pays, and the per-loop fact below is the stronger statement. The step rung keeps its spelling-structural fact: inlining the step removes its calls.
///
/// **The slice claim is per loop, because the module-wide count cannot see the event it guards.** The walk is inlined into the entry, so a slice leaving the walk's loop reads as one fewer in `mirror_counts` and nothing more, and a walk slicing a fresh rope per character passes that count. What keeps the walk's own region virtualized is order: `split_windows` takes the widest admissible region, and runs only once the continuations the walk flows into exist, so no one-continuation sub-region below the walk can claim its positions first.
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

/// A fold's step is copied into each width of a UTF-8 scalar `Str/fold` calls it at, so no character pays a call to it: `Nat/of_str`'s in `parse_digits`, and the step `parse_bindless` writes, whose range test of each byte weighs its comparisons at two apiece. A step past the multi-site limit is a function of its own the fold calls per character, which is what `curios-cps`'s `MULTI_SITE_INLINE_LIMIT` is read against.
#[test]
fn a_fold_step_is_copied_into_each_width_the_fold_calls_it_at() {
    for (label, program, step) in [
        ("parse_digits", PARSE_DIGITS, "/std/Nat/of_str/1"),
        ("parse_bindless", PARSE_BINDLESS, "/decode/1"),
    ] {
        let wat = wat(program);
        let standing = functions(&wat)
            .iter()
            .map(|function| function.name)
            .filter(|name| name.ends_with(step))
            .collect::<Vec<_>>();
        assert!(
            standing.is_empty(),
            "{label}'s fold step stands as a function the fold calls: {standing:?}"
        );
    }
}

/// The walk reads a character whole at its lead byte, so no scan state is carried from one turn to the next: the obligations the mirror family attributes to a per-byte walk — the returned scan state, the scan argument rebuilt — have nothing to stand on here.
///
/// The fold does not call `step`. `classify` names how many bytes continue a character and the range of the first, and the fold reads each in a line, its validity stepped through every byte as `step` would step it, so no call and no returned scan remain in the loop. Nothing else on this program's path calls `step` either: `Str/trim` reads forward through the walk under `Str/span`, which reads a character whole at its lead byte as the fold does, and back by position through `Str/At/prev`, which decides a character's width from its lead byte, so the module holds no copy of `step` at all.
///
/// **So the per-character path of an idiomatic UTF-8 walk allocates nothing.** A step of `Str/At` constructs nothing wherever it stands as a function of its own, and no loop that reads a byte carries a `struct.new` of its own: neither the accumulator nor the suffix view. That assertion is deliberately about *every* allocation but what a step answers rather than the tuple shapes named above, because a rewrite that moved the cost into some other object would satisfy the narrow reading and fail this one; the fold is copied into each call that hands it a known step, so a step's body runs in the walk's loop, and what it builds, as `Nat/of_str`'s `Option`, is the step's.
#[test]
fn the_per_character_walk_reads_each_character_whole_without_allocating() {
    let wat = wat(PARSE_MULTIBYTE);
    let split = functions(&wat);
    let count_in = |body: &str, needle: &str| {
        body.lines()
            .map(str::trim)
            .filter(|line| line.starts_with(needle))
            .count()
    };

    assert!(
        split
            .iter()
            .all(|function| !function.name.contains("/std/Str/utf8/step")),
        "nothing on this program's path calls `step`, so the module holds no copy of it",
    );
    // A step stands as a function where it is not copied into its caller, and wherever it stands it constructs nothing.
    for step in split.iter().filter(|function| {
        ["/std/Str/At/next", "/std/Str/At/prev", "/std/Str/At/skip"]
            .iter()
            .any(|name| function.name.contains(name))
    }) {
        assert_eq!(
            count_in(step.body, "struct.new"),
            0,
            "a step from one position to the next constructs nothing: {}",
            step.name,
        );
    }

    // The fold's per-character shape, in every loop that reads a byte: `Str/fold` is copied into each call that hands it a known step, the program's own and `Nat/of_str`'s, so its walk stands in no function of its own and the claims are stated of each such loop. The accumulator travels as fields, and the suffix view is virtualized — the walk carries `(base, offset, length)` through the loop, its slice an extent guard plus an offset sum.
    let walks = reading_loops(&wat);
    assert!(!walks.is_empty(), "the walk reads a byte in a loop");
    for walk in &walks {
        let in_walk = |needle: &str| walk.iter().filter(|line| line.starts_with(needle)).count();
        // What a step builds is the step's: `Nat/of_str`'s answers an `Option` a digit.
        assert!(
            walk.iter()
                .filter(|line| line.starts_with("struct.new"))
                .all(|line| line.contains("/sys/Option")),
            "the walk allocates nothing of its own, neither its state nor its suffix:\n{}",
            walk.join("\n"),
        );
        assert_eq!(
            in_walk("call $bytes/slice"),
            0,
            "the suffix view is a virtual window: no helper call, no view allocation",
        );
        // One site a byte, not one per arm: the dead-hypothesis peel in erasure's `eliminate` emits each read once and binds a name rather than substituting `get(b, 0)` into every arm that mentions it. A loop over bytes reads at one site, and a walk that reads a character whole at four, its lead and the three bytes that may continue it; only the arms a byte takes run, so this is a static site count, and the cost is one read a byte either way. The walk back `Str/trim` takes reads the character before at eight, two whole copies of the four: the optimizer emits the step back's decode once for each way its search for the boundary, `Str/At/floor`, ends, and one copy runs a step.
        assert!(
            matches!(in_walk("call $bytes/read"), 1 | 4 | 8),
            "each byte is read once and shared by the arms, not re-read per arm:\n{}",
            walk.join("\n"),
        );
        assert_eq!(
            in_walk("call_indirect $clsr/2"),
            0,
            "the step a call hands the fold runs in place",
        );
    }
    assert_eq!(
        walks
            .iter()
            .filter(|walk| walk
                .iter()
                .filter(|line| line.starts_with("call $bytes/read"))
                .count()
                == 4)
            .count(),
        3,
        "the fold's two walks, the program's and `Nat/of_str`'s, and the walk under the span `Str/trim` reads forward by each read a character whole",
    );
    // Matched on the name, because the emitted call spells the callee's index before its hint and that index is not stable across passes.
    assert_eq!(
        wat.matches("$/std/Str/utf8/step").count(),
        0,
        "the walk reads a character by the arm it is in rather than calling `step`",
    );
}

/// The lines each `loop` holds itself, its nested loops' left out, of every loop that reads a byte: the per-character path wherever a copy of a walk stands. Nesting is read off the printer's indentation, which the emitter keeps exact.
fn reading_loops(wat: &str) -> Vec<Vec<&str>> {
    let mut open = Vec::<(usize, usize)>::new();
    let mut loops = Vec::<Vec<&str>>::new();
    let mut in_heap = false;
    for line in wat.lines() {
        // A heap twin's copy of a walk runs past the frame budget alone, so it is no walk of the native code ([`in_heap_function`]).
        if line.starts_with("    (func ") {
            in_heap = line.contains("$heap/");
        }
        if in_heap {
            continue;
        }
        let indent = line.len() - line.trim_start().len();
        let line = line.trim();
        while open
            .last()
            .is_some_and(|&(loop_indent, _)| loop_indent >= indent)
        {
            open.pop();
        }
        if line.starts_with("loop ") {
            open.push((indent, loops.len()));
            loops.push(Vec::new());
        } else if let Some(&(_, current)) = open.last() {
            loops[current].push(line);
        }
    }
    loops
        .into_iter()
        .filter(|body| body.iter().any(|line| line.starts_with("call $bytes/read")))
        .collect()
}
