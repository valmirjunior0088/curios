//! How a built binary is filed in `<crate>/.artifacts/`: copied with the dep-info cargo wrote beside it, both keeping the times cargo gave them.
//!
//! **This is a contract with another crate, not a step of one recipe.** `curios/build.rs` embeds what is filed here, reads the dep-info beside it, and warns when a listed source is newer than the filed binary — so what this module files and what that build script reads have to agree.

use std::{fs, path::Path};

/// Copy `from` to `to`, giving the copy `from`'s modification time.
///
/// The time is cargo's, so it moves only when cargo built the binary again: a run that rebuilt nothing leaves the file `curios/build.rs` watches as it was, and the compiler that embeds it is not built again for nothing.
fn copy_keeping_time(from: &Path, to: &Path) -> Result<(), String> {
    let cannot = |error: std::io::Error| {
        format!(
            "cannot file {} as {}: {error}",
            from.display(),
            to.display()
        )
    };

    let built_at = fs::metadata(from)
        .and_then(|metadata| metadata.modified())
        .map_err(cannot)?;
    fs::copy(from, to).map_err(cannot)?;
    fs::File::options()
        .write(true)
        .open(to)
        .and_then(|file| file.set_modified(built_at))
        .map_err(cannot)?;

    eprintln!("filed {}", to.display());

    Ok(())
}

/// File the binary `built` at `filed` and the dep-info cargo wrote beside it at `dep_info`.
pub(crate) fn file_with_dep_info(
    built: &Path,
    filed: &Path,
    dep_info: &Path,
) -> Result<(), String> {
    // `.artifacts/` is a build product and is not committed, so on a clean checkout — CI's, and any fresh clone's — nothing has created it.
    let directory = filed.parent().expect("a filed artifact has a parent");
    fs::create_dir_all(directory)
        .map_err(|error| format!("cannot create {}: {error}", directory.display()))?;

    copy_keeping_time(built, filed)?;
    copy_keeping_time(&built.with_extension("d"), dep_info)
}
