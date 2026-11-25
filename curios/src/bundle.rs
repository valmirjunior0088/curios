//! Emitting a self-contained native executable: the embedded slim launcher with the program's payload, the modules answering its `foreign` declarations, and the record naming them appended. The trailing tail layout lives in `curios_native::bundle` (shared with the launcher that recovers it); this module only appends it to the launcher image the library embeds and writes the result to disk.

use {
    curios::{LAUNCHER, fault},
    curios_common::{OsError, Report, write_whole_prepared},
    curios_native::{Bundle, append_bundle},
    curios_package::Refused,
    std::{fs, io, os::unix::fs::PermissionsExt, path::Path},
};

/// Build a self-contained executable: the embedded launcher stub with `bundle` and its footer appended to the tail (see [`curios_native::append_bundle`]).
pub fn emit_exe(bundle: &Bundle, output: &Path) -> Result<(), Box<Report>> {
    let mut bytes = LAUNCHER.to_vec();
    let unwritable = |tag: &str, path: &Path, error: io::Error| {
        Refused::of(tag).stating(OsError(&error)).about_path(path)
    };

    append_bundle(&mut bytes, bundle)
        .map_err(|error| fault("the executable's bundle could not be archived", error))?;

    // A declared executable lands inside `.curios/`, nested under its package, so the directories below it are this write's to create — nothing else has had reason to.
    if let Some(directory) = output.parent()
        && !directory.as_os_str().is_empty()
    {
        fs::create_dir_all(directory)
            .map_err(|error| unwritable("unwritable directory", directory, error))?;
    }

    // Written whole and renamed into place, made executable before it takes the name: a program still running from an earlier build keeps the file it started from, where a write in place is refused as a busy text file.
    write_whole_prepared(output, &bytes, |staged| {
        let mut permissions = fs::metadata(staged)?.permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(staged, permissions)
    })
    .map_err(|error| unwritable("unwritable file", output, error))?;

    // On macOS we do NOT re-sign. The embedded launcher image carries an ad-hoc signature from the linker whose code-limit covers the original image; our payload is appended *past* that limit, so the signature stays valid and the loader ignores the trailing bytes (the launcher reads them via `current_exe`). `codesign --force` would in fact reject the result ("data after signature"), so leaving the original signature in place is both correct and necessary.

    Ok(())
}

#[cfg(test)]
mod tests;
