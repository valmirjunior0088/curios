//! Freeing a tree.

use super::*;

/// Freeing a tree descends once per level, so this is what [`Term`]'s drop brackets: a tree as deep as a text can nest is freed on a test thread's stack. Deep enough that a regression is a stack overflow rather than a slow test, and held alone, so every level is freed here rather than shared away.
#[test]
fn a_deep_tree_is_freed_without_overflowing() {
    let mut term: Term = Subterm::Hole.into();
    for _ in 0..100_000 {
        term = Subterm::Apply(Apply {
            head: term,
            arguments: Vec::new(),
        })
        .into();
    }

    drop(term);
}
