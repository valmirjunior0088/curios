//! What the formatter's suites share: a source formatted to its canonical text, and the sources of the tree.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    super::Formatted,
    curios_utilities::Source,
    std::path::{Path, PathBuf},
};

/// The canonical text, either way: production callers match the variants directly, since changed-ness decides the exit path, and only these round-trip tests want the text alone.
pub(super) fn formatted(source: &str) -> String {
    match Formatted::from_source(&Source::inline(source)).expect("fixture formats") {
        Formatted::Unchanged(text) | Formatted::Changed(text) => text,
    }
}

/// Every `.crs` under `directory`, skipping the directories that hold generated or foreign trees.
pub(super) fn collect_sources(directory: &Path, sources: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(directory).expect("the tree is readable") {
        let path = entry.expect("a readable entry").path();
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        if path.is_dir() {
            // `editors/` holds highlighting fixtures for the editor grammars, which are not programs.
            if !matches!(
                name,
                "target" | ".git" | ".curios" | ".claude" | "node_modules" | "editors"
            ) {
                collect_sources(&path, sources);
            }
        } else if path.extension().is_some_and(|extension| extension == "crs") {
            sources.push(path);
        }
    }
}
