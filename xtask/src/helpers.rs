//! The vocabulary the recipes are written in: where the workspace and its build products live, how a command is run and echoed, and the copy discipline every filed artifact shares.

use {
    std::{
        env, fs,
        path::{Path, PathBuf},
        process::Command,
    },
    wasm_bindgen_cli_support::Bindgen,
};

/// The workspace root: this crate's manifest directory is one level below it.
pub(crate) fn root() -> &'static Path {
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

/// What a `--release --target <triple>` build left for `name` — the file a recipe files or feeds onward.
pub(crate) fn built(target: &str, name: &str) -> PathBuf {
    target_directory().join(target).join("release").join(name)
}

/// A crate's filed build product for one triple: `<crate>/.artifacts/<triple>`, the triple as the file name, as `curios/build.rs` expects.
pub(crate) fn artifact(crate_dir: &str, target: &str) -> PathBuf {
    root().join(crate_dir).join(".artifacts").join(target)
}

/// The cargo that launched this tool, so a recipe builds with the toolchain the alias resolved to.
pub(crate) fn cargo() -> Command {
    Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into()))
}

/// Run one command from the workspace root, echoing it first as a recipe would, and fail with its status.
pub(crate) fn run(command: Command, arguments: &[&str]) -> Result<(), String> {
    run_in(root(), command, arguments)
}

/// [`run`] from `directory` instead of the workspace root — the editor recipes, whose trees are their own npm packages and cargo workspace.
pub(crate) fn run_in(
    directory: &Path,
    mut command: Command,
    arguments: &[&str],
) -> Result<(), String> {
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

/// File `built` at `filed`, skipping the copy when the filed bytes are already the built ones. A recipe that embeds the launcher runs `runtime` first unconditionally, so filing must cost nothing when nothing changed: cargo already answers that for the build, and the skip is what keeps a repeated run from touching the file `curios/build.rs` watches.
pub(crate) fn file(built: &Path, filed: &Path) -> Result<(), String> {
    let bytes =
        fs::read(built).map_err(|error| format!("cannot read {}: {error}", built.display()))?;

    if fs::read(filed).is_ok_and(|current| current == bytes) {
        eprintln!("up to date {}", filed.display());
        return Ok(());
    }

    fs::create_dir_all(
        filed
            .parent()
            .expect("the artifacts directory has a parent"),
    )
    .map_err(|error| format!("cannot create {}: {error}", filed.display()))?;

    fs::copy(built, filed).map_err(|error| {
        format!(
            "cannot copy {} to {}: {error}",
            built.display(),
            filed.display()
        )
    })?;

    eprintln!("filed {}", filed.display());

    Ok(())
}

/// What `wasm-bindgen --target web --out-dir` does, called as the library it wraps and echoed as the command line it stands for. The command line emits the TypeScript declarations unless told not to, where the library does not unless told to; asking for them keeps the bundle's file set what the command line produced.
pub(crate) fn bindgen_web(module: &Path, bundle: &Path) -> Result<(), String> {
    eprintln!(
        "wasm-bindgen --target web --out-dir {} {}",
        bundle.display(),
        module.display()
    );

    let mut bindgen = Bindgen::new();

    bindgen
        .input_path(module)
        .web(true)
        .map_err(|error| format!("{error:#}"))?
        .typescript(true)
        .generate(bundle)
        .map_err(|error| format!("{error:#}"))?;

    Ok(())
}
