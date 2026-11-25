//! The runner `rust-wasm` runs under: a Rust twin built for `wasm32-wasip2`, a command component, precompiled once on the engine `Cargo.lock` resolves for the compiler's own and run from that precompile at every execution, handed its arguments and this process's standard output and error.
//!
//! **A process, as every contestant is.** The contest starts it as it starts the other two, so its wall clock covers what starting a process costs, and a count opened before it inherits into it. What an execution spends is a load and a run, as the CLI's cached runs were: the twin is compiled once, when the contest builds the contestants, by this binary, so the precompile and every run of it are of one engine.

use {
    clap::Parser,
    std::{
        fs,
        path::{Path, PathBuf},
        process::ExitCode,
    },
    wasmtime::{
        Engine, Store,
        component::{Component, Linker, ResourceTable},
    },
    wasmtime_wasi::{
        WasiCtx, WasiCtxView, WasiView,
        p2::{add_to_linker_sync, bindings::sync::Command},
    },
};

#[derive(Debug, Parser)]
#[command(
    name = "twin",
    about = "Run a Rust twin built for wasm32-wasip2 on the engine the compiler embeds"
)]
enum Runner {
    #[command(about = "Compile the twin's component, for every run after it")]
    Precompile {
        #[arg(value_name = "COMPONENT", help = "The component the twin was built as")]
        component: PathBuf,
        #[arg(value_name = "PRECOMPILED", help = "Where its precompile is written")]
        precompiled: PathBuf,
    },

    #[command(about = "Run the twin from its precompile, handing it the arguments that follow")]
    Run {
        #[arg(
            value_name = "PRECOMPILED",
            help = "The precompile this runner wrote of the twin"
        )]
        precompiled: PathBuf,
        #[arg(
            value_name = "ARGUMENT",
            help = "What the twin is handed after its own name",
            trailing_var_arg = true,
            allow_hyphen_values = true
        )]
        arguments: Vec<String>,
    },
}

/// What the twin's store holds: its WASI context and the resources it opens through it.
struct Twin {
    context: WasiCtx,
    table: ResourceTable,
}

impl WasiView for Twin {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.context,
            table: &mut self.table,
        }
    }
}

/// Compile the component at `component` and write its precompile at `precompiled`, on this runner's engine.
fn precompile(component: &Path, precompiled: &Path) -> Result<(), String> {
    let bytes = fs::read(component)
        .map_err(|error| format!("{} cannot be read: {error}", component.display()))?;
    let code = Engine::default()
        .precompile_component(&bytes)
        .map_err(|error| format!("{} does not compile: {error}", component.display()))?;

    fs::write(precompiled, code)
        .map_err(|error| format!("{} cannot be written: {error}", precompiled.display()))
}

/// Run the twin precompiled at `precompiled`, its name and then `arguments` as what it is handed: whether it reported success.
fn run(precompiled: &Path, arguments: &[String]) -> Result<bool, String> {
    let engine = Engine::default();
    // SAFETY: `precompiled` is what `precompile` above wrote, the contest running this binary's `precompile` over the twin it built in the same sitting and handing the file to no one else; wasmtime's own stamp in the file refuses one another engine's version or configuration wrote.
    let component = unsafe { Component::deserialize_file(&engine, precompiled) }
        .map_err(|error| format!("{} does not load: {error}", precompiled.display()))?;
    let mut linker = Linker::new(&engine);
    add_to_linker_sync(&mut linker).map_err(|error| format!("WASI does not link: {error}"))?;

    let name = precompiled.display().to_string();
    let mut context = WasiCtx::builder();
    context
        .inherit_stdout()
        .inherit_stderr()
        .arg(&name)
        .args(arguments);
    let mut store = Store::new(
        &engine,
        Twin {
            context: context.build(),
            table: ResourceTable::new(),
        },
    );
    let command = Command::instantiate(&mut store, &component, &linker)
        .map_err(|error| format!("{name} does not instantiate: {error}"))?;
    let ran = command
        .wasi_cli_run()
        .call_run(&mut store)
        .map_err(|error| format!("{name} trapped: {error}"))?;

    Ok(ran.is_ok())
}

fn main() -> ExitCode {
    let outcome = match Runner::parse() {
        Runner::Precompile {
            component,
            precompiled,
        } => precompile(&component, &precompiled).map(|()| true),
        Runner::Run {
            precompiled,
            arguments,
        } => run(&precompiled, &arguments),
    };

    match outcome {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::FAILURE,
        Err(why) => {
            eprintln!("{why}");
            ExitCode::FAILURE
        }
    }
}
