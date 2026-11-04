//! What the reader's suites share: task files of a test's own, read under a top holding nothing of its own.

use super::*;

pub(super) const TOP: &str = "description = \"The top\"\n";

/// The tree the files `(name, text)` of [`DIRECTORY`] make, under a top holding nothing of its own.
pub(super) fn tree(files: &[(&str, &str)]) -> Result<Tree, Refusal> {
    let paths = files
        .iter()
        .map(|(name, _)| format!("{DIRECTORY}/{name}.toml"))
        .collect::<Vec<_>>();
    let mut all = vec![(ROOT_FILE, TOP)];
    all.extend(
        paths
            .iter()
            .map(String::as_str)
            .zip(files.iter().map(|(_, text)| *text)),
    );

    Tree::parse(&all)
}

/// The message the reader refuses the files with.
pub(super) fn refused(files: &[(&str, &str)]) -> String {
    tree(files).unwrap_err().message
}

pub(super) fn name(text: &str) -> Name {
    text.split('.').map(str::to_string).collect()
}
