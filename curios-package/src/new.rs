//! Starting a package.
//!
//! Last of the machinery rather than first, and deliberately: scaffolding writes what everything else reads, so it can only be written once there is something for it to be right *about*. What it produces is a package the rest of this crate already accepts — no template escapes the rules, and nothing here knows a rule the manifest parser does not.
//!
//! It writes the smallest thing that runs. A package of nothing but a program gets no `lib.crs`, because a library is not an artifact you opt into and an empty one would be ceremony; a library package gets no executable, for the same reason in the other direction.

#[cfg(test)]
mod tests;

use {
    crate::{EXTENSION, LIBRARY, MANIFEST, canonical},
    std::{fs, path::Path},
};

/// Scaffold a package in `directory`, named after it.
///
/// The name comes from the directory rather than from a second argument, because a package's name is the one thing it must declare for itself and offering two places to say it invites them to disagree. It is checked before anything is written: a directory half-populated with a manifest nothing can parse is worse than no directory at all.
pub fn scaffold(directory: &Path, library: bool) -> Result<(), String> {
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

    let (declaration, source, contents) = match library {
        true => (
            String::new(),
            LIBRARY.to_string(),
            format!("pub let greeting : /std/Str = \"Hello from {name}!\";\n"),
        ),
        false => (
            format!("\n[[executables]]\nname = {name:?}\n"),
            format!("{name}.{EXTENSION}"),
            format!("/std/print(\"Hello from {name}!\\n\")\n"),
        ),
    };

    write(&manifest, &format!("name = {name:?}\n{declaration}"))?;
    write(&directory.join(source), &contents)
}

/// One scaffolded file.
fn write(path: &Path, contents: &str) -> Result<(), String> {
    fs::write(path, contents)
        .map_err(|error| format!("failed to write {}: {error}", path.display()))
}
