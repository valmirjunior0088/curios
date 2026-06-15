mod host;
pub use host::*;

mod lift;
pub use lift::*;

mod lower;
pub use lower::*;

mod engine;
pub use engine::*;

mod compile;
pub use compile::*;

use {
    crate::text,
    std::{path::Path, time::Duration},
};

pub fn run_entrypoint<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    entrypoint: &text::Entrypoint,
    loader: &dyn text::Loader,
    host: H,
) -> Result<(), String> {
    run_wasm(
        &compile_entrypoint(timeout, entrypoint, loader, |_| {})?,
        host,
    )
    .map(|_| ())
}

pub fn run<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    host: H,
) -> Result<(), String> {
    let entrypoint = source
        .parse::<text::Entrypoint>()
        .map_err(|error| error.format())?;

    run_entrypoint(timeout, &entrypoint, &text::NullLoader, host)
}

pub fn run_text<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    host: H,
) -> Result<(), String> {
    run(timeout, source, host)
}

pub fn run_file<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    path: &Path,
    host: H,
) -> Result<(), String> {
    let entrypoint = text::Entrypoint::from_path(path).map_err(|error| error.format())?;
    let loader = text::FileLoader::new(path.parent().unwrap_or(Path::new(".")));

    run_entrypoint(timeout, &entrypoint, &loader, host)
}
