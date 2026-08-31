//! The workspace's build recipes, as `cargo x <recipe>`.
//!
//! **A recipe is cargo with flags, then one step cargo does not do.** Every recipe here spawns `cargo` as a separate process and then copies a file, generates the browser bindings, or runs a container. Nothing is a build script: a build script runs before its crate compiles and so cannot post-process that crate's output, and a nested `cargo` inside one contends for the target-directory lock. A process that `cargo run` has already launched holds no lock, so its nested builds are ordinary.
//!
//! **The launcher's isolation is the spawn.** `runtime` builds `curios-runtime` in its own `cargo` invocation, exactly as the recipe it replaced did, so workspace feature unification cannot reach it — `curios` enables `curios-runtime/cranelift`, and a launcher built beside it would carry a compiler. `curios/build.rs` embeds what this recipe copies to `curios/.artifacts/<triple>` and refuses to build without it.
//!
//! **A recipe that needs the launcher runs `runtime` first, unconditionally.** `build` and `profile` both do, because the compiler they build embeds it. What makes that free to repeat is that `runtime` costs nothing when nothing changed: cargo decides whether the launcher needs rebuilding, and [`file`] skips the copy when the filed bytes are already the built ones, so a repeated run neither rebuilds nor touches the file `curios/build.rs` watches.
//!
//! **The bindings generator is a dependency.** `js` calls `wasm-bindgen-cli-support`, the crate the `wasm-bindgen` command line wraps; why, and what keeps its version honest, is the README's decision.
//!
//! **A dependency of nothing.** This crate is reached only through the `x` alias in `.cargo/config.toml`, and no crate may depend on it: its dependency tree exists to build the workspace, not to be part of it.
//!
//! The command line is clap's, in `curios`'s own convention — a `Parser` root over a `Subcommand` of recipes — so the help is derived from the definitions and cannot fall out of step with them. Each recipe below is its steps and nothing else; the verbs they share live in [`helpers`].

mod helpers;

use {
    clap::{Parser, Subcommand},
    helpers::{artifact, bindgen_web, built, cargo, file, root, run, run_in},
    std::{
        path::{Path, PathBuf},
        process::{Command, ExitCode},
    },
};

/// The triple this tool was built for: the host, which is the one triple every recipe builds for.
const HOST: &str = env!("CURIOS_HOST_TRIPLE");

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
            HOST,
        ],
    )?;

    file(&built(HOST, "curios-runtime"), &artifact("curios", HOST))?;

    Ok(())
}

fn build() -> Result<(), String> {
    runtime()?;

    run(cargo(), &["build", "--release", "--package", "curios"])?;

    Ok(())
}

fn js() -> Result<(), String> {
    const TARGET: &str = "wasm32-unknown-unknown";

    run(
        cargo(),
        &[
            "build",
            "--release",
            "--package",
            "curios-js",
            "--target",
            TARGET,
        ],
    )?;

    bindgen_web(
        &built(TARGET, "curios_js.wasm"),
        &artifact("curios-js", TARGET),
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
