//! The installer script a release attaches, rendered from `templates/install.sh` with the release's version baked in. The version is fixed when the script is rendered rather than discovered when it runs, so the script a release ships installs that release's binary and a pinned URL stays pinned; the release workflow renders it from the tag and attaches what this files. The template is rendered without an escaper, because the product is a shell script rather than markup, and the version is the one thing it substitutes: a placeholder the template names and the context does not supply fails the build, which is what the `sed` this replaced could not promise.

use {crate::helpers::root, askama::Template, std::fs};

/// The one value the script cannot know for itself.
#[derive(Template)]
#[template(path = "install.sh", escape = "none")]
pub(crate) struct Installer<'a> {
    pub(crate) version: &'a str,
}

/// Render the installer for `version` under `xtask/.artifacts/install.sh`.
pub(crate) fn installer(version: &str) -> Result<(), String> {
    let script = Installer {
        version: validated(version)?,
    }
    .render()
    .map_err(|error| format!("cannot render the installer: {error}"))?;

    let directory = root().join("xtask").join(".artifacts");
    fs::create_dir_all(&directory)
        .map_err(|error| format!("cannot create {}: {error}", directory.display()))?;
    let filed = directory.join("install.sh");
    // Askama drops a template's final newline, as Jinja does; a script ends in one.
    fs::write(&filed, format!("{script}\n"))
        .map_err(|error| format!("cannot write {}: {error}", filed.display()))?;

    eprintln!("filed {}", filed.display());

    Ok(())
}

/// The version as the release names it, or why the argument is not one. The tag is `release/<version>` and the workflow hands over the part after the slash, so a slash is the sign the whole tag arrived; a space or a quote would break the assignment the script bakes it into, and a brace would trip the guard that recognizes the unrendered template.
pub(crate) fn validated(version: &str) -> Result<&str, String> {
    let admitted = |char: char| char.is_ascii_alphanumeric() || matches!(char, '.' | '-' | '+');
    match !version.is_empty() && version.chars().all(admitted) {
        true => Ok(version),
        false => Err(format!(
            "not a release version: {version:?}; the tag is release/<version> and the recipe takes the part after the slash"
        )),
    }
}
