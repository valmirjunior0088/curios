use {
    super::{OsHost, Stage, compile_entrypoint, run_wasm, text, typecheck_entrypoint, wasm},
    clap::{Parser, Subcommand},
    std::{
        fs, iter,
        path::{Path, PathBuf},
        process,
        time::Duration,
    },
};

fn parse_timeout(input: &str) -> Result<Duration, String> {
    input
        .parse::<u64>()
        .map(Duration::from_millis)
        .map_err(|error| format!("invalid timeout in milliseconds: {error}"))
}

#[derive(Debug, Subcommand)]
enum Mode {
    #[command(about = "Execute the compiled WASM module")]
    Run {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,

        #[arg(
            trailing_var_arg = true,
            allow_hyphen_values = true,
            value_name = "ARGS",
            help = "Arguments passed to the program (read via /std/Proc/args)"
        )]
        args: Vec<String>,
    },

    #[command(about = "Type-check the entrypoint without executing")]
    Check {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,
    },

    #[command(about = "Emit the compiled WASM module")]
    Compile {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,

        #[arg(
            long,
            value_name = "PATH",
            help = "Write the compiled WebAssembly binary to PATH"
        )]
        output_path: Option<PathBuf>,
    },
}

#[derive(Debug, Parser)]
#[command(version, about)]
struct Cli {
    #[arg(long, default_value = "1000", value_name = "MILLIS", value_parser = parse_timeout, help = "Type-checker reduction timeout in milliseconds")]
    timeout: Duration,

    #[arg(
        long,
        value_name = "STAGES",
        num_args = 0..=1,
        default_missing_value = "text,core,ersd,cont,optm,wasm",
        help = "Print selected IRs to stderr (comma-separated: text,core,ersd,cont,optm,wasm; bare --print prints all)"
    )]
    print: Option<String>,

    #[command(subcommand)]
    mode: Mode,
}

fn compile_file(timeout: Duration, print: &str, input_path: &Path) -> Result<wasm::Module, String> {
    let entrypoint = text::Entrypoint::from_path(input_path).map_err(|error| error.format())?;

    let loader = text::FileLoader::new(input_path.parent().unwrap_or(Path::new(".")));

    let stages = print.split(',').collect::<Vec<_>>();

    compile_entrypoint(timeout, &entrypoint, &loader, |stage| match stage {
        Stage::Text(text) if stages.contains(&"text") => eprintln!("\n=== text ===\n{text}"),
        Stage::Core(core) if stages.contains(&"core") => eprintln!("\n=== core ===\n{core}"),
        Stage::Ersd(ersd) if stages.contains(&"ersd") => eprintln!("\n=== ersd ===\n{ersd}"),
        Stage::Cont(cont) if stages.contains(&"cont") => eprintln!("\n=== cont ===\n{cont}"),
        Stage::Optm(optm) if stages.contains(&"optm") => eprintln!("\n=== optm ===\n{optm}"),
        Stage::Wasm(wasm) if stages.contains(&"wasm") => eprintln!("\n=== wasm ===\n{wasm}"),
        _ => {}
    })
}

fn typecheck_file(timeout: Duration, print: &str, input_path: &Path) -> Result<(), String> {
    let entrypoint = text::Entrypoint::from_path(input_path).map_err(|error| error.format())?;

    let loader = text::FileLoader::new(input_path.parent().unwrap_or(Path::new(".")));

    let stages = print.split(',').collect::<Vec<_>>();

    typecheck_entrypoint(timeout, &entrypoint, &loader, |stage| match stage {
        Stage::Text(text) if stages.contains(&"text") => eprintln!("\n=== text ===\n{text}"),
        Stage::Core(core) if stages.contains(&"core") => eprintln!("\n=== core ===\n{core}"),
        _ => {}
    })
}

fn default_output_path(input_path: &Path) -> PathBuf {
    PathBuf::from(input_path.file_stem().unwrap_or(input_path.as_os_str())).with_extension("wasm")
}

fn emit_executable(module: &wasm::Module, output_path: &Path) -> Result<(), String> {
    let bytes = wasm::to_bytes(module);

    #[cfg(feature = "binaryen")]
    let bytes = super::binaryen::optimize(bytes);

    fs::write(output_path, bytes)
        .map_err(|error| format!("failed to write {}: {error}", output_path.display()))
}

pub fn cli() -> Result<(), String> {
    let Cli {
        timeout,
        print,
        mode,
    } = Cli::parse();

    let print = print.unwrap_or_default();

    match mode {
        Mode::Run { input_path, args } => {
            let module = compile_file(timeout, &print, &input_path)?;

            let code = run_wasm(
                &module,
                OsHost::with_args(
                    iter::once(input_path.to_string_lossy().into_owned().into_bytes())
                        .chain(args.into_iter().map(String::into_bytes))
                        .collect(),
                ),
            )?;

            if code != 0 {
                process::exit(code);
            }
        }
        Mode::Check { input_path } => {
            // Run the fast type-check-only path; fall through to the full pipeline
            // only when a post-core stage is requested for printing, since those
            // stages do not exist until lowering runs.
            let post_core = ["ersd", "cont", "optm", "wasm"];

            if print.split(',').any(|stage| post_core.contains(&stage)) {
                compile_file(timeout, &print, &input_path)?;
            } else {
                typecheck_file(timeout, &print, &input_path)?;
            }
        }
        Mode::Compile {
            input_path,
            output_path,
        } => {
            emit_executable(
                &compile_file(timeout, &print, &input_path)?,
                &output_path.unwrap_or_else(|| default_output_path(&input_path)),
            )?;
        }
    }

    Ok(())
}
