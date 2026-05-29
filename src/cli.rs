use {
    clap::Parser,
    std::{
        path::{Path, PathBuf},
        time::Duration,
    },
};

fn parse_timeout(input: &str) -> Result<Duration, String> {
    input
        .parse::<u64>()
        .map(Duration::from_millis)
        .map_err(|error| format!("invalid timeout in milliseconds: {error}"))
}

#[derive(Debug, Parser)]
#[command(version, about)]
struct Cli {
    #[arg(long, default_value = "1000", value_name = "MILLIS", value_parser = parse_timeout, help = "Type-checker reduction timeout in milliseconds")]
    timeout: Duration,

    #[arg(long, help = "Run the full pipeline without executing the result")]
    check: bool,

    #[arg(long, help = "Print each intermediate representation to stdout")]
    print: bool,

    #[arg(help = "Path to the .crs entrypoint file")]
    path: PathBuf,
}

pub fn cli() -> Result<(), String> {
    let cli = Cli::parse();

    let source = std::fs::read_to_string(&cli.path)
        .map_err(|e| format!("failed to read {}: {e}", cli.path.display()))?;

    let base = cli.path.parent().unwrap_or(Path::new(".")).to_path_buf();
    let loader = crate::text::FileLoader::new(base);

    let module = crate::compile(cli.timeout, &loader, None, &source, |stage| {
        if !cli.print {
            return;
        }
        match stage {
            crate::Stage::Text(entrypoint) => {
                println!("=== text ===");
                println!("{entrypoint}");
            }
            crate::Stage::Core(term) => {
                println!();
                println!("=== core ===");
                println!("{term}");
            }
            crate::Stage::Ersd(term) => {
                println!();
                println!("=== ersd ===");
                println!("{term}");
            }
            crate::Stage::Cont(cont_module) => {
                println!();
                println!("=== cont ===");
                println!("{cont_module}");
            }
            crate::Stage::Wasm(wasm_module) => {
                println!();
                println!("=== wasm ===");
                println!("{wasm_module}");
                println!();
            }
        }
    })?;

    if !cli.check {
        crate::run_wasm(&module, crate::StdioHost)?;
    }

    Ok(())
}
