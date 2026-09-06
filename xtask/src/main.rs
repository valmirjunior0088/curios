//! The workspace's build recipes, as `cargo x <recipe>`.
//!
//! **A recipe is cargo with flags, then one step cargo does not do.** Every recipe here spawns `cargo` as a separate process and then copies a file, generates the browser bindings, or runs a container. Nothing is a build script: a build script runs before its crate compiles and so cannot post-process that crate's output, and a nested `cargo` inside one contends for the target-directory lock. A process that `cargo run` has already launched holds no lock, so its nested builds are ordinary.
//!
//! **The launcher's isolation is the spawn.** `runtime` builds `curios-runtime` in its own `cargo` invocation, exactly as the recipe it replaced did, so workspace feature unification cannot reach it — `curios` enables `curios-runtime/cranelift`, and a launcher built beside it would carry a compiler. `curios/build.rs` embeds what this recipe copies to `curios/.artifacts/<triple>` and refuses to build without it.
//!
//! **A recipe that needs the launcher runs `runtime` first, unconditionally.** `build`, `profile`, `rust-docs` and `std-docs` all do, because the compiler they build or document embeds it. What makes that free to repeat is that `runtime` costs nothing when nothing changed: cargo decides whether the launcher needs rebuilding, and [`file_with_inputs()`](helpers::file_with_inputs) skips the copy when the filed bytes are already the built ones, so a repeated run neither rebuilds nor touches the file `curios/build.rs` watches. It files the launcher's inputs beside it — cargo's dep-info and the lock file — which is what that build script compares the launcher against, and it refreshes the launcher's timestamp when a listed input is newer while the bytes stayed the same, so the staleness warning never outlives the command it names.
//!
//! **The bindings generator is a dependency.** `js` calls `wasm-bindgen-cli-support`, the crate the `wasm-bindgen` command line wraps; why, and what keeps its version honest, is the README's decision.
//!
//! **The installer is a template.** `installer` renders `templates/install.sh` with a release's version through Askama and files the script under `xtask/.artifacts/`, the one recipe that runs no cargo at all: the release workflow calls it with the tag's version and attaches what it filed. What it is and why it is rendered here rather than by the workflow is [`installer`](mod@installer)'s own documentation.
//!
//! **A dependency of nothing.** This crate is reached only through the `x` alias in `.cargo/config.toml`, and no crate may depend on it: its dependency tree exists to build the workspace, not to be part of it.
//!
//! The command line is clap's, in `curios`'s own convention — a `Parser` root over a `Subcommand` of recipes — so the help is derived from the definitions and cannot fall out of step with them. Each recipe below is its steps and nothing else; the verbs they share live in [`helpers`].

mod helpers;
use helpers::*;

mod installer;
use installer::*;

#[cfg(test)]
mod tests;

use {
    clap::{Parser, Subcommand},
    std::{
        fs,
        path::{Path, PathBuf},
        process::{Command, ExitCode},
    },
};

/// The triple this tool was built for: the host, which is the one triple every recipe builds for.
const HOST_TRIPLE: &str = env!("CURIOS_HOST_TRIPLE");

/// The triple the browser bundle is built for: the bare Wasm target `wasm-bindgen` binds, which no host is.
const BROWSER_TRIPLE: &str = "wasm32-unknown-unknown";

#[derive(Debug, Parser)]
#[command(
    name = "cargo x",
    bin_name = "cargo x",
    version,
    about = "The workspace's build recipes",
    help_template = "\
{name} {version}
{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}"
)]
struct Cli {
    #[command(subcommand)]
    recipe: Recipe,
}

#[derive(Debug, Subcommand)]
enum Recipe {
    #[command(
        about = "Build the slim runtime launcher in its own cargo invocation and file it under curios/.artifacts/<triple>"
    )]
    Runtime,

    #[command(about = "Build the launcher, then the compiler that embeds it")]
    Build,

    #[command(
        about = "Build curios-js for wasm32-unknown-unknown and generate the browser bindings under curios-js/.artifacts/<triple>"
    )]
    Js,

    #[command(
        about = "Build the launcher, then the workspace's Rust documentation under target/doc, with a root redirect to the compiler's"
    )]
    RustDocs,

    #[command(
        about = "Build the compiler, then the standard library's pages under curios-prelude-archive/.artifacts/documentation from the prelude image it was built with"
    )]
    StdDocs,

    #[command(
        about = "Render the installer script for one release version under xtask/.artifacts/install.sh"
    )]
    Installer {
        #[arg(
            value_name = "VERSION",
            help = "The release's version: the part of its tag after release/"
        )]
        version: String,
    },

    #[command(about = "Run one compilation under the tracing profiler")]
    Profile {
        #[arg(
            value_name = "PATH",
            default_value = "programs/hello_world.crs",
            help = "Path to the .crs entrypoint file"
        )]
        source: PathBuf,
    },

    #[command(about = "Build the benchmark image and run it")]
    Benchmarks {
        #[arg(
            long,
            value_name = "TAG",
            default_value = "curios-benchmarks",
            help = "The image tag to build and run"
        )]
        tag: String,
    },

    #[command(about = "Run npm in editors/grammar")]
    Grammar {
        #[arg(
            trailing_var_arg = true,
            allow_hyphen_values = true,
            value_name = "ARGS",
            help = "Arguments passed to npm, such as `ci` or `test`"
        )]
        arguments: Vec<String>,
    },

    #[command(about = "Run npm in editors/vscode")]
    Vscode {
        #[arg(
            trailing_var_arg = true,
            allow_hyphen_values = true,
            value_name = "ARGS",
            help = "Arguments passed to npm, such as `ci`, `test` or `run package`"
        )]
        arguments: Vec<String>,
    },

    #[command(about = "Run cargo in editors/zed, the extension's own workspace")]
    Zed {
        #[arg(
            trailing_var_arg = true,
            allow_hyphen_values = true,
            value_name = "ARGS",
            help = "Arguments passed to cargo, such as `build --release --target wasm32-wasip2`"
        )]
        arguments: Vec<String>,
    },

    #[command(about = "Remove everything git does not track, including the build products")]
    Clean,
}

fn main() -> ExitCode {
    let outcome = match Cli::parse().recipe {
        Recipe::Runtime => runtime(),
        Recipe::Build => build(),
        Recipe::Js => js(),
        Recipe::RustDocs => rust_docs(),
        Recipe::StdDocs => std_docs(),
        Recipe::Installer { version } => installer(&version),
        Recipe::Profile { source } => profile(&source),
        Recipe::Benchmarks { tag } => benchmarks(&tag),
        Recipe::Grammar { arguments } => bridge("grammar", Command::new("npm"), &arguments),
        Recipe::Vscode { arguments } => bridge("vscode", Command::new("npm"), &arguments),
        Recipe::Zed { arguments } => bridge("zed", cargo(), &arguments),
        Recipe::Clean => clean(),
    };

    match outcome {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("{message}");

            ExitCode::FAILURE
        }
    }
}

fn runtime() -> Result<(), String> {
    run(
        cargo(),
        &[
            "build",
            "--release",
            "--package",
            "curios-runtime",
            "--target",
            HOST_TRIPLE,
        ],
    )?;

    file_with_inputs(
        &built(HOST_TRIPLE, "curios-runtime"),
        &artifact("curios", HOST_TRIPLE),
        &inputs("curios", HOST_TRIPLE),
    )?;

    Ok(())
}

fn build() -> Result<(), String> {
    runtime()?;

    run(cargo(), &["build", "--release", "--package", "curios"])?;

    Ok(())
}

fn js() -> Result<(), String> {
    run(
        cargo(),
        &[
            "build",
            "--release",
            "--package",
            "curios-js",
            "--target",
            BROWSER_TRIPLE,
        ],
    )?;

    bindgen_web(
        &built(BROWSER_TRIPLE, "curios_js.wasm"),
        &artifact("curios-js", BROWSER_TRIPLE),
    )?;

    Ok(())
}

/// The one spelling of the rustdoc build: the gate's, the check workflow's and the release's, so a broken intra-doc link fails all three the same way. Private items are documented because these crates state their invariants on `pub(crate)` items, and the root redirect is what makes the tree a site: `target/doc/` has no landing page of its own.
fn rust_docs() -> Result<(), String> {
    runtime()?;

    run(
        cargo(),
        &[
            "doc",
            "--workspace",
            "--no-deps",
            "--document-private-items",
        ],
    )?;

    let landing = target_directory().join("doc").join("index.html");
    fs::write(
        &landing,
        "<!DOCTYPE html><meta http-equiv=\"refresh\" content=\"0; url=curios/index.html\">\n",
    )
    .map_err(|error| format!("{}: {error}", landing.display()))?;

    Ok(())
}

/// The standard library's pages, rendered by the compiler from the prelude image its own build filed — no sources are read and nothing is compiled twice, since the image already carries the library's record. `build` is what produces both, and costs nothing when nothing changed; the render is skipped the same way, when the landing page is newer than the image and the compiler that reads it, so a repeated run touches nothing. `cargo x clean` removes the pages with every other filed product.
fn std_docs() -> Result<(), String> {
    build()?;

    let artifacts = root().join("curios-prelude-archive").join(".artifacts");
    let image = artifacts.join("archive.rkyv");
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
            &image.to_string_lossy(),
            "--output",
            &pages.to_string_lossy(),
        ],
    )?;

    Ok(())
}

fn profile(source: &Path) -> Result<(), String> {
    runtime()?;

    run(
        cargo(),
        &[
            "run",
            "--release",
            "--package",
            "curios",
            "--features",
            "profile",
            "--",
            "profile",
            &source.to_string_lossy(),
        ],
    )?;

    Ok(())
}

fn benchmarks(tag: &str) -> Result<(), String> {
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

fn bridge(tree: &str, command: Command, arguments: &[String]) -> Result<(), String> {
    run_in(
        &root().join("editors").join(tree),
        command,
        &arguments.iter().map(String::as_str).collect::<Vec<_>>(),
    )?;

    Ok(())
}

fn clean() -> Result<(), String> {
    run(Command::new("git"), &["clean", "-xffd"])?;

    Ok(())
}
