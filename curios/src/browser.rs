//! A browser build: the page a program is built into, shared by `compile`, which writes it, and `run`, which serves it, so the two cannot drift apart; the directory it is written as; and the name its program runs by.

use {
    crate::{Built, Failure, Then, Tree, payload_of},
    curios::{linked_of, page_name},
    curios_browser::page,
    curios_common::Report,
    curios_package::{Program, Refused, Target},
    curios_verdict::Verdicts,
    std::{collections::BTreeMap, fs, io, path::Path},
};

/// What `program` is built into for a browser: every file of its page by its page path — the module through Binaryen alone, filed in `store` as `payload_of` files it, the rows it reaches linked to the gateways its packages declare, and the page laid out by `curios-browser`, naming the program as its first argument and its title. Every file the page holds is read here, once, and what the page refuses is refused before anything is written or served.
///
/// The page's files come back with the subject's tree, in which its command goes on to say where the page stands: served, or written.
pub fn built(
    budget: u64,
    program: Program,
    store: Option<Verdicts>,
) -> Result<(BTreeMap<String, Vec<u8>>, Tree), Failure> {
    // Kept before the program is consumed: the page names the program, stands at its package's root and links what the manifest declaring it names.
    let name = page_name(&program.named());
    let package = program.home().map(|home| home.package.clone());
    let manifest = program.home().map(|home| home.manifest.clone());

    let Built {
        payload: module,
        foreigns,
        tree,
    } = payload_of(budget, program, store, Target::Browser, Then::More)?;
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

    Ok((files, tree))
}

/// Write `files`, each at its page path, under `directory`, creating what is missing. Where the directory is the store's own, `owned`, it is emptied first, so it holds the page and nothing an earlier build left; one `--output` names keeps whatever else it holds, a file of the page replacing the one of its name.
pub fn emit_page(
    directory: &Path,
    files: &BTreeMap<String, Vec<u8>>,
    owned: bool,
) -> Result<(), Box<Report>> {
    let unwritable =
        |tag: &str, path: &Path, error: io::Error| Refused::of(tag).stating(error).about_path(path);
    if owned && directory.exists() {
        fs::remove_dir_all(directory)
            .map_err(|error| unwritable("unwritable directory", directory, error))?;
    }

    for (path, bytes) in files {
        let file = path
            .split('/')
            .fold(directory.to_path_buf(), |at, part| at.join(part));

        if let Some(parent) = file.parent() {
            fs::create_dir_all(parent)
                .map_err(|error| unwritable("unwritable directory", parent, error))?;
        }

        fs::write(&file, bytes).map_err(|error| unwritable("unwritable file", &file, error))?;
    }

    Ok(())
}
