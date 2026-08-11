//! Starting a package.
//!
//! Last of the machinery rather than first, and deliberately: scaffolding writes what everything else reads, so it can only be written once there is something for it to be right *about*. What it produces is a package the rest of this crate already accepts — no template escapes the rules, and nothing here knows a rule the manifest parser does not.
//!
//! It writes every part a package has: a manifest, a library header, and one executable. A package can still be a program alone or a library alone — the manifest decides, and deleting either file says so — but *starting* one is not the moment to ask which, and a flag asking would only be answerable by somebody who already knows what the two are.

#[cfg(test)]
mod tests;

use {
    crate::{EXECUTABLE, LIBRARY, MANIFEST, canonical},
    std::{
        fs,
        path::{Path, PathBuf},
    },
};

/// Scaffold a package in `directory`, named after it.
///
/// The name comes from the directory rather than from a second argument, because a package's name is the one thing it must declare for itself and offering two places to say it invites them to disagree. It is checked before anything is written: a directory half-populated with a manifest nothing can parse is worse than no directory at all.
pub fn scaffold(directory: &Path) -> Result<Vec<PathBuf>, String> {
    let name = directory
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .ok_or_else(|| format!("{} names no package", directory.display()))?;

    // The same predicate the manifest parser applies, rather than a second opinion about what a name may be.
    canonical(&name, "package name")?;

    let manifest = directory.join(MANIFEST);
    if manifest.exists() {
        return Err(format!(
            "{} already holds a package; `new` starts one rather than adopting one",
            directory.display()
        ));
    }

    fs::create_dir_all(directory)
        .map_err(|error| format!("failed to create {}: {error}", directory.display()))?;

    let library = directory.join(LIBRARY);
    let program = directory.join(EXECUTABLE);

    write(&manifest, &format!("name = {name:?}\n"))?;
    // The program reads its own library rather than printing a literal, so the two files it starts with demonstrate the one thing a package *is*: a namespace mounted at its declared name. Neither file is named after the package, which is what keeps `use /hello/{message}` from reading `hello` twice for two different things.
    write(
        &library,
        "use /std/{Str};\n\npub let message: Str =\n    \"Hello, world!\";\n",
    )?;
    write(
        &program,
        &format!("use /std/{{Fmt}};\nuse /{name}/{{message}};\n\nFmt/print(\"%\\n\")(message)\n"),
    )?;

    // In the order they were written, which is the order a reader wants them: the directory that now exists, then what is in it.
    Ok(vec![directory.to_path_buf(), manifest, library, program])
}

/// One scaffolded file.
fn write(path: &Path, contents: &str) -> Result<(), String> {
    fs::write(path, contents)
        .map_err(|error| format!("failed to write {}: {error}", path.display()))
}
