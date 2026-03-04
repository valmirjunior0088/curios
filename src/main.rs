use {
    clap::Parser,
    curios::execute,
    std::{fs, path::PathBuf, time::Duration},
};

fn parse_timeout(input: &str) -> Result<Duration, String> {
    input
        .parse::<u64>()
        .map(Duration::from_millis)
        .map_err(|error| format!("invalid timeout in milliseconds: {error}"))
}

#[derive(Debug, Parser)]
struct Cli {
    #[arg(long, default_value = "1000", value_name = "MILLIS", value_parser = parse_timeout)]
    timeout: Duration,

    #[arg()]
    path: PathBuf,
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();

    let source = fs::read_to_string(&cli.path)
        .map_err(|error| format!("failed to read {}: {error}", cli.path.display()))?;

    println!("{}", execute(cli.timeout, &source)?);

    Ok(())
}
