//! Each recipe here is its steps and nothing else. What selects one is [`main`](crate)'s rule table, which names every recipe and dispatches it to a single call from this file.
//!
//! The vocabulary they are written in sits beside them: [`places`](crate::places) for where a thing lives, [`commands`](crate::commands) for how a tool is invoked, [`filing`](crate::filing) for how a build product is filed. A recipe leaves this file when it acquires decisions of its own, as [`installer`](mod@crate::installer) has.

use {
    crate::{
        commands::{bindgen_web, cargo, run},
        filing::file_with_inputs,
        places::{
            BROWSER_TRIPLE, HOST_TRIPLE, artifact, built, inputs, modified, root, target_directory,
        },
    },
    curios_profile::ProfileReport,
    std::{
        fs,
        io::{BufReader, Read, empty},
        path::{Path, PathBuf},
        process::Command,
    },
};

pub(crate) fn runtime() -> Result<(), String> {
    cargo(&[
        "build",
        "--release",
        "--package",
        "curios-runtime",
        "--target",
        HOST_TRIPLE,
    ])?;

    file_with_inputs(
        &built(HOST_TRIPLE, "curios-runtime"),
        &artifact("curios", HOST_TRIPLE),
        &inputs("curios", HOST_TRIPLE),
    )?;

    Ok(())
}

/// One cargo check over the whole workspace, or over the one package it was given, with `before` naming the subcommand and `after` what follows the scope.
///
/// Which packages exist is cargo's question and not a list kept here. A name the workspace does not hold is refused before anything is built, though not by name: `--all-features` is what cargo notices first, so it reports a feature selection outside the workspace.
pub(crate) fn scoped(package: Option<&str>, before: &[&str], after: &[&str]) -> Result<(), String> {
    let scope = match package {
        Some(package) => vec!["--package", package],
        None => vec!["--workspace"],
    };

    cargo(&[before, scope.as_slice(), after].concat())
}

pub(crate) fn build() -> Result<(), String> {
    runtime()?;

    cargo(&["build", "--release", "--package", "curios"])?;

    Ok(())
}

pub(crate) fn js() -> Result<(), String> {
    cargo(&[
        "build",
        "--release",
        "--package",
        "curios-js",
        "--target",
        BROWSER_TRIPLE,
    ])?;

    bindgen_web(
        &built(BROWSER_TRIPLE, "curios_js.wasm"),
        &artifact("curios-js", BROWSER_TRIPLE),
    )?;

    Ok(())
}

/// The one spelling of the rustdoc build: the gate's, the check workflow's and the release's, so a broken intra-doc link fails all three the same way. Private items are documented because these crates state their invariants on `pub(crate)` items, and the root redirect is what makes the tree a site: `target/doc/` has no landing page of its own.
pub(crate) fn rust_docs() -> Result<(), String> {
    runtime()?;

    cargo(&[
        "doc",
        "--workspace",
        "--no-deps",
        "--document-private-items",
    ])?;

    let landing = target_directory().join("doc").join("index.html");
    fs::write(
        &landing,
        "<!DOCTYPE html><meta http-equiv=\"refresh\" content=\"0; url=curios/index.html\">\n",
    )
    .map_err(|error| format!("{}: {error}", landing.display()))?;

    Ok(())
}

/// The standard library's pages, rendered by the compiler from the prelude image its own build filed — no sources are read and nothing is compiled twice, since the image already carries the library's record. `build` is what produces both, and costs nothing when nothing changed; the render is skipped the same way, when the landing page is newer than the image and the compiler that reads it, so a repeated run touches nothing. `cargo x clean` removes the pages with every other filed product.
pub(crate) fn std_docs() -> Result<(), String> {
    build()?;

    let artifacts = root().join("curios-prelude-archive").join(".artifacts");
    let image = artifacts.join("std.rkyv");
    let pages = artifacts.join("documentation");
    let compiler = target_directory().join("release").join("curios");

    if let (Some(rendered), Some(image_at), Some(compiler_at)) = (
        modified(&pages.join("index.html")),
        modified(&image),
        modified(&compiler),
    ) && rendered >= image_at
        && rendered >= compiler_at
    {
        println!(
            "{} is newer than the image and the compiler; nothing to render",
            pages.display()
        );
        return Ok(());
    }

    run(
        Command::new(&compiler),
        &[
            "document",
            "--archive",
            &image.to_string_lossy(),
            "--output",
            &pages.to_string_lossy(),
        ],
    )?;

    Ok(())
}

/// Run `source` under a profiling build, then fold the stream it filed.
///
/// **The compiler is not asked to profile anything.** A `profile` build files every span and event it makes, whatever subcommand ran, so this recipe selects a subject rather than a mode — and the summary is a recipe's job because a fold reads a file the compiler has already finished writing.
pub(crate) fn profile(source: &Path) -> Result<(), String> {
    runtime()?;

    // The one place the stream's location is spelled. The compiler takes it as an argument and keeps no default, so what is written and what is read back cannot drift — and it is derived here the way every other path in this crate is.
    let stream = root().join("curios/.artifacts/profile.tsv");

    cargo(&[
        "run",
        "--release",
        "--package",
        "curios",
        "--features",
        "profile",
        "--",
        "--profile",
        &stream.to_string_lossy(),
        "run",
        &source.to_string_lossy(),
    ])?;

    print!("{}", summarize(&stream)?.render());
    println!("\nstream: {}", stream.display());

    Ok(())
}

/// Fold a rotated stream back into its summaries: the discarded file's rows first, then the current file's.
///
/// Both are handed to one fold because each restates the callsite table at its head, so their concatenation is well defined. They are chained rather than concatenated in memory: the fold reads rows, and at the default cap the pair is a gigabyte. A missing `.prev` is the ordinary case of a run that never grew past one file.
pub(crate) fn summarize(path: &Path) -> Result<ProfileReport, String> {
    let mut previous = path.to_path_buf().into_os_string();
    previous.push(".prev");

    let discarded: Box<dyn Read> = match fs::File::open(PathBuf::from(previous)) {
        Ok(file) => Box::new(file),
        Err(_) => Box::new(empty()),
    };
    let current = fs::File::open(path).map_err(|error| named(path, error))?;

    curios_profile::fold(BufReader::new(discarded.chain(current)))
        .map_err(|error| named(path, error))
}

/// An IO failure with the file it happened to, which is the only thing a reader needs to act on one.
fn named(path: &Path, error: impl std::fmt::Display) -> String {
    format!("{}: {error}", path.display())
}

pub(crate) fn benchmarks(tag: &str) -> Result<(), String> {
    run(
        Command::new("docker"),
        &[
            "build",
            "--platform",
            "linux/arm64",
            "--file",
            "benchmarks/Dockerfile",
            "--tag",
            tag,
            ".",
        ],
    )?;

    run(
        Command::new("docker"),
        &["run", "--rm", "--cpuset-cpus", "0", tag],
    )?;

    Ok(())
}

pub(crate) fn clean() -> Result<(), String> {
    run(Command::new("git"), &["clean", "-xffd"])?;

    Ok(())
}
