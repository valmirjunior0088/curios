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

    let term = crate::text::to_core(
        &source
            .parse()
            .map_err(|error| format!("failed to parse source: {error:?}"))?,
        &crate::text::FileLoader::new(base),
    );

    if cli.print {
        println!("=== core ===");
        println!("{term}");
    }

    let type_ = crate::core::infer(&mut crate::core::Context::new(cli.timeout), &term)
        .map_err(|error| format!("failed to infer type: {error:?}"))?;

    let term = crate::core::erase(&mut crate::core::Context::new(cli.timeout), &term, &type_)
        .map_err(|error| format!("failed to erase term: {error:?}"))?;

    if cli.print {
        println!();
        println!("=== ersd ===");
        println!("{term}");
    }

    let cont_module = crate::ersd::to_cont(&term);

    if cli.print {
        println!();
        println!("=== cont ===");
        println!("{cont_module}");
    }

    let wasm_module = crate::cont::to_wasm(&cont_module);

    if cli.print {
        println!();
        println!("=== wasm ===");
        println!("{wasm_module}");
        println!();
    }

    if !cli.check {
        crate::run_wasm(&wasm_module, crate::StdioProvider)?;
    }

    Ok(())
}
