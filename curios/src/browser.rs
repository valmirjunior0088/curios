//! A browser build: the page a program is built into, shared by `compile`, which writes it, and `run`, which serves it, so the two cannot drift apart; the directory it is written as; and the name its program runs by.

use {
    crate::{Failure, linked_of, payload_of},
    curios_browser::page,
    curios_package::{Entry, Program, Target},
    curios_verdicts::Verdicts,
    std::{collections::BTreeMap, fs, path::Path},
};

/// What `program` is built into for a browser: the name it runs by, and every file of its page by its page path — the module through Binaryen alone, filed in `store` as `payload_of` files it, the rows it reaches linked to the gateways its packages declare, and the page laid out by `curios-browser`. Every file the page holds is read here, once, and what the page refuses is refused before anything is written or served.
pub(crate) fn built(
    budget: u64,
    program: Program,
    store: Option<Verdicts>,
) -> Result<(String, BTreeMap<String, Vec<u8>>), Failure> {
    // Kept before the program is consumed: the page names the program, stands at its package's root and links what the manifest declaring it names.
    let name = named(&program);
    let package = program.home().map(|home| home.package.clone());
    let manifest = program.home().map(|home| home.manifest.clone());

    let (module, foreigns) = payload_of(budget, program, store, Target::Browser)?;
    let linked = linked_of(manifest.as_deref(), &foreigns)?;
    let files = page(
        curios_browser::Program {
            name: &name,
            package: package.as_deref(),
            module,
            foreigns: &foreigns,
        },
        linked,
    )?;

    Ok((name, files))
}

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
