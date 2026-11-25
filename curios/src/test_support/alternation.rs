//! The three rope programs the alternation suite anchors and the bench's measurement of rope alternation reads, and the sizes its slope is read across.

use {super::*, curios_native::Compiled};

const PUSH_FIRST: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/rope_push_first.crs"
));

const PUSH_DROP: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/rope_push_drop.crs"
));

const PUSH_PEEK: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../programs/rope_push_peek.crs"
));

/// The sizes the slope is read across, each twice the one before; collections are counted at the last.
pub const SIZES: [u64; 3] = [16_000, 32_000, 64_000];

/// The three programs, each anchored at `N = 8` so a mistranslation fails before any figure is read.
pub fn alternation_programs() -> [(&'static str, Compiled); 3] {
    let programs = [
        ("push, then read the first", compiled_of(PUSH_FIRST)),
        ("push, then drop the first", compiled_of(PUSH_DROP)),
        ("push, then peek (control)", compiled_of(PUSH_PEEK)),
    ];
    for ((label, compiled), anchor) in programs.iter().zip([&b"600\n"[..], b"28\n", b"138773\n"]) {
        assert_eq!(run_at(compiled, 8), anchor, "{label} at N = 8");
    }
    programs
}
