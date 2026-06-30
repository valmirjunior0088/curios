use {
    clap::{Parser, Subcommand},
    curios::{Stage, compile_entrypoint, text, typecheck_entrypoint, wasm},
    curios_compiler::{run_wasm, to_cwasm},
    curios_runtime::OsHost,
    std::{
        env, fs, iter,
        path::{Path, PathBuf},
        process::{self, ExitCode},
        time::Duration,
    },
};

/// Footer appended after the `.cwasm` payload: `payload ++ (len: u64 LE) ++ MAGIC`.
/// Kept in sync with the launcher in `curios-runtime`.
const MAGIC: &[u8; 8] = b"CRSEXEC1";

fn parse_timeout(input: &str) -> Result<Duration, String> {
    input
        .parse::<u64>()
        .map(Duration::from_millis)
        .map_err(|error| format!("invalid timeout in milliseconds: {error}"))
}

#[derive(Debug, Subcommand)]
enum Mode {
    #[command(about = "Compile and execute the entrypoint")]
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

    #[command(about = "Compile the entrypoint to a native executable")]
    Compile {
        #[arg(value_name = "PATH", help = "Path to the .crs entrypoint file")]
        input_path: PathBuf,

        #[arg(
            short = 'o',
            long = "output",
            value_name = "PATH",
            help = "Write the executable to PATH (default: the input file stem)"
        )]
        output_path: Option<PathBuf>,

        #[arg(
            long,
            value_name = "PATH",
            help = "Path to the curios-runtime launcher stub (default: a sibling of \
                    this binary, or $CURIOS_LAUNCHER)"
        )]
        launcher: Option<PathBuf>,
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
        default_missing_value = "text,core,ersd,ersd-optm,cont,cont-optm,wasm",
        help = "Print selected IRs to stderr (comma-separated: text,core,ersd,ersd-optm,cont,cont-optm,wasm; bare --print prints all)"
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
        Stage::ErsdOptm(ersd) if stages.contains(&"ersd-optm") => {
            eprintln!("\n=== ersd-optm ===\n{ersd}")
        }
        Stage::Cont(cont) if stages.contains(&"cont") => eprintln!("\n=== cont ===\n{cont}"),
        Stage::ContOptm(cont) if stages.contains(&"cont-optm") => {
            eprintln!("\n=== cont-optm ===\n{cont}")
        }
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

/// Default executable name: the input's file stem, no extension.
fn exe_output_path(input_path: &Path) -> PathBuf {
    PathBuf::from(input_path.file_stem().unwrap_or(input_path.as_os_str()))
}

/// Find the `curios-runtime` launcher stub to append the payload to: an explicit
/// `--launcher`, else `$CURIOS_LAUNCHER`, else a sibling of this binary.
fn locate_launcher(flag: Option<&Path>) -> Result<PathBuf, String> {
    if let Some(path) = flag {
        return Ok(path.to_path_buf());
    }

    if let Some(path) = env::var_os("CURIOS_LAUNCHER") {
        return Ok(PathBuf::from(path));
    }

    let here =
        env::current_exe().map_err(|error| format!("cannot locate own executable: {error}"))?;
    let candidate = here.with_file_name("curios-runtime");

    if candidate.exists() {
        return Ok(candidate);
    }

    Err("could not find the curios-runtime launcher (set --launcher or CURIOS_LAUNCHER)".into())
}

/// Build a self-contained executable: a copy of the launcher stub with the
/// `.cwasm` payload and a `len ++ MAGIC` footer appended to its tail.
fn emit_exe(launcher: &Path, cwasm: &[u8], output: &Path) -> Result<(), String> {
    let mut bytes = fs::read(launcher)
        .map_err(|error| format!("failed to read launcher {}: {error}", launcher.display()))?;

    bytes.extend_from_slice(cwasm);
    bytes.extend_from_slice(&(cwasm.len() as u64).to_le_bytes());
    bytes.extend_from_slice(MAGIC);

    fs::write(output, &bytes)
        .map_err(|error| format!("failed to write {}: {error}", output.display()))?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;

        let mut perms = fs::metadata(output)
            .map_err(|error| format!("failed to stat {}: {error}", output.display()))?
            .permissions();
        perms.set_mode(0o755);
        fs::set_permissions(output, perms)
            .map_err(|error| format!("failed to chmod {}: {error}", output.display()))?;
    }

    // On macOS we do NOT re-sign. The launcher stub carries an ad-hoc signature
    // from the linker whose code-limit covers the original image; our payload is
    // appended *past* that limit, so the signature stays valid and the loader
    // ignores the trailing bytes (the launcher reads them via `current_exe`).
    // `codesign --force` would in fact reject the result ("data after signature"),
    // so leaving the original signature in place is both correct and necessary.

    Ok(())
}

fn cli() -> Result<(), String> {
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
            let post_core = ["ersd", "ersd-optm", "cont", "cont-optm", "wasm"];

            if print.split(',').any(|stage| post_core.contains(&stage)) {
                compile_file(timeout, &print, &input_path)?;
            } else {
                typecheck_file(timeout, &print, &input_path)?;
            }
        }
        Mode::Compile {
            input_path,
            output_path,
            launcher,
        } => {
            let module = compile_file(timeout, &print, &input_path)?;
            let cwasm = to_cwasm(&module)?;
            let stub = locate_launcher(launcher.as_deref())?;
            let output = output_path.unwrap_or_else(|| exe_output_path(&input_path));

            emit_exe(&stub, &cwasm, &output)?;
        }
    }

    Ok(())
}

fn main() -> ExitCode {
    if let Err(error) = cli() {
        eprintln!("{error}");

        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
