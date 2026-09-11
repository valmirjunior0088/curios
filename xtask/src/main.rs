//! The workspace's build recipes, as `cargo x <recipe>`.
//!
//! **A recipe is cargo with flags, and whatever step cargo does not do.** Every recipe here spawns `cargo` — or npm, in an editor tree — as a separate process, and then copies a file, generates the browser bindings or runs a container, where there is such a step to take. Nothing is a build script: a build script runs before its crate compiles and so cannot post-process that crate's output, and a nested `cargo` inside one contends for the target-directory lock. A process that `cargo run` has already launched holds no lock, so its nested builds are ordinary.
//!
//! **A recipe takes no arguments it passes on.** A recipe may take a parameter it places itself — a release's version, a program to profile, a package to narrow a check to — but never a tail it hands to the tool unread. Every recipe's command line is written here, so the gate in `CLAUDE.md`, the check workflow and a contributor run one spelling of each step and no two of them can drift. A recipe names a tool and a tree: `cargo` at the workspace root, `grammar` and `vscode` for their npm packages, `zed` for the extension's own workspace. Anything else in a tree is run from inside it, where that tree's README sends the reader.
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
//! The command line is clap's, in `curios`'s own convention — a `Parser` root over a `Subcommand` of recipes — so the help is derived from the definitions and cannot fall out of step with them.
//!
//! **This file is the rule table and nothing else.** It declares the recipes and dispatches each to one call, so what a recipe *is* can be read top to bottom without reading what it *does* — and a recipe's steps, like the verbs they are written in, live in [`helpers`]. The list here is therefore the same list `CLAUDE.md`'s gate and the check workflow name, in the same order, with nothing between a name and its meaning.

mod helpers;
use helpers::*;

mod installer;
use installer::*;

#[cfg(test)]
mod tests;

use {
    clap::{Parser, Subcommand},
    std::{path::PathBuf, process::ExitCode},
};

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

    #[command(about = "Format the workspace")]
    Fmt,

    #[command(about = "Check the workspace's formatting without writing")]
    FmtCheck,

    #[command(about = "Lint the workspace over every target and feature, warnings denied")]
    Clippy {
        #[arg(
            value_name = "PACKAGE",
            help = "The package to lint; the whole workspace when omitted"
        )]
        package: Option<String>,
    },

    #[command(about = "Run the workspace's tests over every target and feature")]
    Test {
        #[arg(
            value_name = "PACKAGE",
            help = "The package whose tests to run; the whole workspace when omitted"
        )]
        package: Option<String>,
    },

    #[command(
        about = "Run the workspace's documentation examples, which testing every target leaves out"
    )]
    Doctest {
        #[arg(
            value_name = "PACKAGE",
            help = "The package whose documentation examples to run; the whole workspace when omitted"
        )]
        package: Option<String>,
    },

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

    #[command(about = "Run one program under a profiling build, and fold what it filed")]
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

    #[command(about = "Install editors/grammar's dependencies from its lock file")]
    GrammarInstall,

    #[command(about = "Run the grammar's tests: regeneration drift, corpus and highlight queries")]
    GrammarTest,

    #[command(about = "Install editors/vscode's dependencies from its lock file")]
    VscodeInstall,

    #[command(about = "Run the VS Code extension's TextMate grammar snapshots")]
    VscodeTest,

    #[command(
        about = "Bundle the VS Code extension and write its .vsix under editors/vscode/.artifacts"
    )]
    VscodePackage,

    #[command(about = "Format the Zed extension")]
    ZedFmt,

    #[command(about = "Check the Zed extension's formatting without writing")]
    ZedFmtCheck,

    #[command(about = "Lint the Zed extension for wasm32-wasip2, warnings denied")]
    ZedClippy,

    #[command(about = "Build the Zed extension for wasm32-wasip2 in release")]
    ZedBuild,

    #[command(
        about = "Check that the grammar rev in editors/zed/extension.toml publishes this tree's grammar"
    )]
    ZedTest,

    #[command(about = "Remove everything git does not track, including the build products")]
    Clean,
}

fn main() -> ExitCode {
    let outcome = match Cli::parse().recipe {
        Recipe::Runtime => runtime(),
        Recipe::Build => build(),
        Recipe::Fmt => cargo(&["fmt", "--all"]),
        Recipe::FmtCheck => cargo(&["fmt", "--all", "--", "--check"]),
        Recipe::Clippy { package } => scoped(
            package.as_deref(),
            &["clippy"],
            &["--all-targets", "--all-features", "--", "-Dwarnings"],
        ),
        Recipe::Test { package } => scoped(
            package.as_deref(),
            &["test"],
            &["--all-targets", "--all-features", "--no-fail-fast"],
        ),
        Recipe::Doctest { package } => {
            scoped(package.as_deref(), &["test"], &["--doc", "--all-features"])
        }
        Recipe::Js => js(),
        Recipe::RustDocs => rust_docs(),
        Recipe::StdDocs => std_docs(),
        Recipe::Installer { version } => installer(&version),
        Recipe::Profile { source } => profile(&source),
        Recipe::Benchmarks { tag } => benchmarks(&tag),
        Recipe::GrammarInstall => grammar(&["clean-install"]),
        Recipe::GrammarTest => grammar(&["test"]),
        Recipe::VscodeInstall => vscode(&["clean-install"]),
        Recipe::VscodeTest => vscode(&["test"]),
        Recipe::VscodePackage => vscode(&["run", "package"]),
        Recipe::ZedFmt => zed(&["fmt", "--all"]),
        Recipe::ZedFmtCheck => zed(&["fmt", "--all", "--", "--check"]),
        Recipe::ZedClippy => zed(&["clippy", "--target", "wasm32-wasip2", "--", "-Dwarnings"]),
        Recipe::ZedBuild => zed(&["build", "--release", "--target", "wasm32-wasip2"]),
        Recipe::ZedTest => zed(&["test"]),
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
