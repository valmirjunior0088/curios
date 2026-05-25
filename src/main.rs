use std::process::ExitCode;

fn main() -> ExitCode {
    #[cfg(feature = "cli")]
    if let Err(error) = curios::cli() {
        eprintln!("{error}");

        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
