//! The tools a reading is taken with, pinned, and the check that refuses to measure under anything else.
//!
//! Neither version is written here. `rust-toolchain.toml` pins the compiler the workspace builds under and `Cargo.lock` resolves the engine the compiler embeds, so this module reads the file that owns each rather than keeping a second copy for the bench to fall behind.
//!
//! The engine pin is the load-bearing one: `rust-wasm` runs under the `wasmtime` on the path while `curios` runs under the one it embeds, so unless the two agree the wasm comparison is between two engines rather than between two compilers.
//!
//! What a pin buys is that a control cannot drift for a reason the record does not show. The control still catches everything else, and a pin moved deliberately is a visible commit; a toolchain that floats is the drift that left run 04's result unattributable in the bench this one replaces.

#[cfg(test)]
mod tests;

use std::{fs, path::Path, process::Command};

/// The channel `rust-toolchain.toml` pins.
fn channel(told: &str) -> Option<String> {
    told.lines()
        .find_map(|line| line.trim().strip_prefix("channel"))
        .and_then(|rest| rest.trim().strip_prefix('='))
        .map(|value| value.trim().trim_matches('"').to_string())
}

/// What `Cargo.lock` resolves `package` to.
fn locked(told: &str, package: &str) -> Option<String> {
    told.split("[[package]]")
        .find(|entry| entry.contains(&format!("\nname = \"{package}\"\n")))?
        .lines()
        .find_map(|line| line.strip_prefix("version = "))
        .map(|quoted| quoted.trim().trim_matches('"').to_string())
}

/// The version in what a tool reports of itself: the first word that reads like one, since each spells the rest of the line its own way.
pub fn version(reported: &str) -> String {
    reported
        .split_whitespace()
        .map(|word| word.trim_matches(|character: char| matches!(character, ',' | '(' | ')' | ';')))
        .find(|word| {
            let number = word.strip_prefix('v').unwrap_or(word);
            number.starts_with(|character: char| character.is_ascii_digit()) && number.contains('.')
        })
        .unwrap_or_default()
        .to_string()
}

/// What `program --version` reports of itself.
fn reported(program: &str) -> Result<String, String> {
    let produced = Command::new(program)
        .arg("--version")
        .output()
        .map_err(|error| format!("{program} did not answer: {error}"))?;

    match produced.status.success() {
        true => Ok(String::from_utf8_lossy(&produced.stdout).into_owned()),
        false => Err(format!("{program} --version failed: {}", produced.status)),
    }
}

/// Ask `program` what it is, refuse where it is not `pinned`, and hand back what it said.
///
/// What a reading records is this answer rather than the pin it was held to: the pin is what a file says the tool should be, and the answer is what the tool on the path reported of itself when the figures were taken.
fn hold(program: &str, pinned: &str) -> Result<String, String> {
    let found = version(&reported(program)?);

    match found == pinned {
        true => Ok(found),
        false => Err(format!(
            "{program} is pinned at {pinned}, and the one on the path is {}",
            match found.is_empty() {
                true => "of no version it states",
                false => &found,
            }
        )),
    }
}

/// Hold every tool a reading is taken with to its pin, and hand back the pins for the reading to record.
///
/// This runs before anything is built, since building with the wrong compiler and timing it afterwards spends the run to produce the wrong artefact.
pub fn held(root: &Path) -> Result<Vec<(&'static str, String)>, String> {
    let toolchain = fs::read_to_string(root.join("rust-toolchain.toml"))
        .map_err(|error| format!("rust-toolchain.toml cannot be read: {error}"))?;
    let rustc = channel(&toolchain).ok_or("rust-toolchain.toml pins no channel")?;

    let lock = fs::read_to_string(root.join("Cargo.lock"))
        .map_err(|error| format!("Cargo.lock cannot be read: {error}"))?;
    let wasmtime = locked(&lock, "wasmtime").ok_or("Cargo.lock resolves no wasmtime")?;

    // The engine is recorded twice over: `wasmtime` is the CLI that runs `rust-wasm`, and `wasmtime in Curios` the one the compiler embeds and runs its own module on. The check holds them equal, and the reading says so rather than leaving a reader to assume it.
    let rustc = hold("rustc", &rustc)?;
    let engine = hold("wasmtime", &wasmtime)?;

    Ok(vec![
        ("rustc", rustc),
        ("wasmtime", engine),
        ("wasmtime in Curios", wasmtime),
    ])
}
