//! What every sitting is taken with, the contest's and a measurement's beside the code alike: the workspace it is taken in, what a tool answers of itself, the day, and the record it files.

#[cfg(test)]
mod tests;

use {
    crate::{Record, workspace_version},
    std::{
        fs::{self, OpenOptions},
        io::Write,
        path::{Path, PathBuf},
        process::Command,
    },
    toml::value::{Date, Datetime},
};

/// The workspace root, which is the crate's parent.
pub fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("the crate sits in the workspace's root")
        .to_path_buf()
}

/// What a program says of itself, read rather than shown.
pub fn asked(program: &str, arguments: &[&str], directory: &Path) -> Result<String, String> {
    let produced = Command::new(program)
        .args(arguments)
        .current_dir(directory)
        .output()
        .map_err(|error| format!("{program} did not start: {error}"))?;

    match produced.status.success() {
        true => Ok(String::from_utf8_lossy(&produced.stdout).trim().to_string()),
        false => Err(format!("{program} {} failed", arguments.join(" "))),
    }
}

/// The subject a record taken in `root` is about: the workspace's version and the commit the sitting is of, refused where the tree holds a change no commit does, before anything is built.
pub fn subject(root: &Path) -> Result<String, String> {
    clean(&asked("git", &["status", "--porcelain"], root)?)?;
    let commit = asked("git", &["rev-parse", "--short", "HEAD"], root)?;

    Ok(format!("curios {} ({commit})", workspace_version(root)?))
}

/// Refuse a tree whose `git status --porcelain` lists a change: a record names the commit that holds what it measured, and a change no commit holds would make it name one that does not.
fn clean(status: &str) -> Result<(), String> {
    match status.trim().is_empty() {
        true => Ok(()),
        false => Err(format!(
            "the tree holds changes no commit does, so a record could name no commit that holds what it measured; commit them first:\n{status}"
        )),
    }
}

/// The day `date -I` answers, as a record holds it.
pub fn today(said: &str) -> Result<Date, String> {
    match said.parse::<Datetime>() {
        Ok(Datetime {
            date: Some(date),
            time: None,
            offset: None,
        }) => Ok(date),
        _ => Err(format!("`date -I` answered {said:?}, which is no day")),
    }
}

/// File `record` as the record numbered `number` of `measurement`: written only once its sitting has ended, and never over a file that stands, so a sitting that fails files nothing.
pub fn file(root: &Path, measurement: &str, number: usize, record: &Record) -> Result<(), String> {
    let text = record.filed()?;
    let directory = root.join("xbench/records").join(measurement);
    fs::create_dir_all(&directory)
        .map_err(|error| format!("{} cannot be made: {error}", directory.display()))?;
    let path = directory.join(format!("{number:02}.toml"));
    OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .and_then(|mut file| file.write_all(text.as_bytes()))
        .map_err(|error| format!("{} cannot be filed: {error}", path.display()))?;
    eprintln!(">> filed as {}", path.display());

    Ok(())
}
