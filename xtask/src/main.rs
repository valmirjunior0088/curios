//! The workspace's build recipes, as `cargo x <recipe>`.
//!
//! **A recipe is cargo with flags, then one step cargo does not do.** Every recipe here spawns `cargo` as a separate process and then copies a file, generates the browser bindings, or runs a container. Nothing is a build script: a build script runs before its crate compiles and so cannot post-process that crate's output, and a nested `cargo` inside one contends for the target-directory lock. A process that `cargo run` has already launched holds no lock, so its nested builds are ordinary.
//!
//! **The launcher's isolation is the spawn.** `runtime` builds `curios-runtime` in its own `cargo` invocation, exactly as the recipe it replaced did, so workspace feature unification cannot reach it — `curios` enables `curios-runtime/cranelift`, and a launcher built beside it would carry a compiler. `curios/build.rs` embeds what this recipe copies to `curios/.artifacts/<triple>` and refuses to build without it.
//!
//! **A recipe that needs the launcher runs `runtime` first, unconditionally.** `build` and `profile` both do, because the compiler they build embeds it. What makes that free to repeat is that `runtime` costs nothing when nothing changed: cargo decides whether the launcher needs rebuilding, and the copy is skipped when the filed bytes are already the built ones, so a repeated run neither rebuilds nor touches the file `curios/build.rs` watches.
//!
//! **The bindings generator is a dependency.** `js` calls `wasm-bindgen-cli-support`, the crate the `wasm-bindgen` command line wraps; why, and what keeps its version honest, is the README's decision.
//!
//! **A dependency of nothing.** This crate is reached only through the `xtask` alias in `.cargo/config.toml`, and no crate may depend on it: its dependency tree exists to build the workspace, not to be part of it.
//!
//! The command line is clap's, in `curios`'s own convention — a `Parser` root over a `Subcommand` of recipes — so the help is derived from the definitions and cannot fall out of step with them.

use {
    clap::{Parser, Subcommand},
    std::{
        env, fs,
        path::{Path, PathBuf},
        process::{Command, ExitCode},
    },
    wasm_bindgen_cli_support::Bindgen,
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

/// The workspace root: this crate's manifest directory is one level below it.
fn root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("xtask sits one level below the workspace root")
}

/// Where cargo puts what it builds, honoring the same override cargo itself honors.
fn target_directory() -> PathBuf {
    match env::var_os("CARGO_TARGET_DIR") {
        Some(directory) => root().join(directory),
        None => root().join("target"),
    }
}

/// The cargo that launched this tool, so a recipe builds with the toolchain the alias resolved to.
fn cargo() -> Command {
    Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into()))
}

/// Run one command from the workspace root, echoing it first as a recipe would, and fail with its status.
fn run(command: Command, arguments: &[&str]) -> Result<(), String> {
    run_in(root(), command, arguments)
}

/// [`run`] from `directory` instead of the workspace root — the editor recipes, whose trees are their own npm packages and cargo workspace.
fn run_in(directory: &Path, mut command: Command, arguments: &[&str]) -> Result<(), String> {
    command.args(arguments).current_dir(directory);

    eprintln!(
        "{} {}",
        command.get_program().to_string_lossy(),
        command
            .get_args()
            .map(|argument| argument.to_string_lossy())
            .collect::<Vec<_>>()
            .join(" ")
    );

    let status = command.status().map_err(|error| {
        format!(
            "cannot run {}: {error}",
            command.get_program().to_string_lossy()
        )
    })?;

    match status.success() {
        true => Ok(()),
        false => Err(format!(
            "{} exited with {status}",
            command.get_program().to_string_lossy()
        )),
    }
}

fn runtime() -> Result<(), String> {
    let target = HOST;

    run(
        cargo(),
        &[
            "build",
            "--release",
            "--package",
            "curios-runtime",
            "--target",
            target,
        ],
    )?;

    let built = target_directory()
        .join(target)
        .join("release")
        .join("curios-runtime");

    // The triple is the file name, as `curios/build.rs` expects.
    let filed = root().join("curios").join(".artifacts").join(target);

    // Every recipe that embeds the launcher runs this one first, so it must cost nothing when nothing changed: cargo already answers that for the build, and the copy is skipped when the filed bytes are the built ones — a copy would touch the file, and `curios/build.rs` reruns and relinks the compiler on that touch.
    let bytes =
        fs::read(&built).map_err(|error| format!("cannot read {}: {error}", built.display()))?;

    if fs::read(&filed).is_ok_and(|current| current == bytes) {
        eprintln!("up to date {}", filed.display());
        return Ok(());
    }

    fs::create_dir_all(
        filed
            .parent()
            .expect("the artifacts directory has a parent"),
    )
    .map_err(|error| format!("cannot create {}: {error}", filed.display()))?;

    fs::copy(&built, &filed).map_err(|error| {
        format!(
            "cannot copy {} to {}: {error}",
            built.display(),
            filed.display()
        )
    })?;

    eprintln!("filed {}", filed.display());

    Ok(())
}

fn build() -> Result<(), String> {
    runtime()?;
    run(cargo(), &["build", "--release", "--package", "curios"])
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

    let module = target_directory()
        .join(TARGET)
        .join("release")
        .join("curios_js.wasm");

    let bundle = root().join("curios-js").join(".artifacts").join(TARGET);

    eprintln!(
        "wasm-bindgen --target web --out-dir {} {}",
        bundle.display(),
        module.display()
    );

    // What `wasm-bindgen --target web --out-dir` does, called as the library it wraps. The command line emits the TypeScript declarations unless told not to, where the library does not unless told to; asking for them keeps the bundle's file set what the command line produced.
    let mut bindgen = Bindgen::new();

    bindgen
        .input_path(&module)
        .web(true)
        .map_err(|error| format!("{error:#}"))?
        .typescript(true)
        .generate(&bundle)
        .map_err(|error| format!("{error:#}"))?;

    Ok(())
}

fn profile(source: &Path) -> Result<(), String> {
    // The profiling build is `curios`, which embeds the launcher, so the launcher comes first here as it does in `build`.
    runtime()?;

    let source = source.to_string_lossy();

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
            &source,
        ],
    )
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
    )
}

/// The editor recipes: one tool run in one tree under `editors/`, with the arguments given. Each tree is its own npm package or cargo workspace, so the recipe adds nothing but the directory — what to run there is the caller's to say, as the workflows do step by step. The Zed workspace is excluded from the root one, and the pinned toolchain still governs it, since `rust-toolchain.toml` is found by walking up from its directory.
fn bridge(tree: &str, command: Command, arguments: &[String]) -> Result<(), String> {
    let arguments = arguments.iter().map(String::as_str).collect::<Vec<_>>();
    run_in(&root().join("editors").join(tree), command, &arguments)
}

fn clean() -> Result<(), String> {
    run(Command::new("git"), &["clean", "-xffd"])
}
