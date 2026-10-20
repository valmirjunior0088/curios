//! A browser build on disk: the directory a page is, and the name its program runs by.

use {
    curios_package::{Entry, Program},
    std::{collections::BTreeMap, fs, path::Path},
};

/// The name `program` runs by in a browser, which is its first argument and the page's title: the executable's name for one a package declares, a loose file's stem, and `stdin` for standard input, which a URL could not spell as `-`.
pub(crate) fn named(program: &Program) -> String {
    match (program.entry(), program.home()) {
        (_, Some(home)) => home.executable.clone(),
        (Entry::File(path), None) => path.file_stem().map_or_else(
            || "program".to_string(),
            |stem| stem.to_string_lossy().into_owned(),
        ),
        (Entry::Stdin, None) => "stdin".to_string(),
    }
}

/// Write `files`, each at its page path, under `directory`, creating what is missing. Where the directory is the store's own, `owned`, it is emptied first, so it holds the page and nothing an earlier build left; one `--output` names keeps whatever else it holds, a file of the page replacing the one of its name.
pub(crate) fn emit_page(
    directory: &Path,
    files: &BTreeMap<String, Vec<u8>>,
    owned: bool,
) -> Result<(), String> {
    if owned && directory.exists() {
        fs::remove_dir_all(directory)
            .map_err(|error| format!("failed to empty {}: {error}", directory.display()))?;
    }

    for (path, bytes) in files {
        let file = path
            .split('/')
            .fold(directory.to_path_buf(), |at, part| at.join(part));

        if let Some(parent) = file.parent() {
            fs::create_dir_all(parent)
                .map_err(|error| format!("failed to create {}: {error}", parent.display()))?;
        }

        fs::write(&file, bytes)
            .map_err(|error| format!("failed to write {}: {error}", file.display()))?;
    }

    Ok(())
}
