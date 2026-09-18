//! How a tool is invoked and echoed: cargo at the workspace root or in a tree of its own, npm in the two editor packages, any other program a recipe names, and the one generator called as the library it wraps rather than spawned.
//!
//! **A verb names a tool and a tree.** [`cargo`] is cargo at the workspace root, [`grammar`] and [`vscode`] npm in their own packages, [`zed`] cargo in the extension's own workspace; anything else is a [`Command`] the recipe builds and hands to [`run`], which defaults to the root. Every one of them echoes its command line before it runs, so the terminal reads as a transcript of the work.

use {
    crate::places::root,
    std::{env, path::Path, process::Command},
    wasm_bindgen_cli_support::Bindgen,
};

/// cargo at the workspace root, under the toolchain the alias resolved to.
pub(crate) fn cargo(arguments: &[&str]) -> Result<(), String> {
    cargo_in(root(), arguments)
}

/// [`cargo`] in `directory` instead of the workspace root — the Zed extension, whose tree is its own workspace.
///
/// The cargo that launched this tool is the one every recipe runs: `cargo x` is `cargo run`, and cargo sets `CARGO` to the binary performing the build — the toolchain's own, the rustup shim already out of the picture — so a recipe cannot resolve a second time and land somewhere else. The fallback is for the other way in, running the built binary directly.
pub(crate) fn cargo_in(directory: &Path, arguments: &[&str]) -> Result<(), String> {
    run_in(
        directory,
        Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into())),
        arguments,
    )
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

/// npm in `editors/grammar`, the tree-sitter grammar's own package.
pub(crate) fn grammar(arguments: &[&str]) -> Result<(), String> {
    run_in(
        &root().join("editors").join("grammar"),
        Command::new("npm"),
        arguments,
    )
}

/// npm in `editors/vscode`, the VS Code extension's own package.
pub(crate) fn vscode(arguments: &[&str]) -> Result<(), String> {
    run_in(
        &root().join("editors").join("vscode"),
        Command::new("npm"),
        arguments,
    )
}

/// cargo in `editors/zed`, the extension's own workspace.
pub(crate) fn zed(arguments: &[&str]) -> Result<(), String> {
    cargo_in(&root().join("editors").join("zed"), arguments)
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
