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
    crate::{Source, text},
    std::{fs, path::Path, time::Duration},
};

pub fn run_entrypoint<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    entrypoint: &text::Entrypoint,
    store: &dyn text::Store,
    host: H,
) -> Result<(), String> {
    run_wasm(
        &compile_entrypoint(timeout, entrypoint, store, |_| {})?,
        host,
    )
}

pub fn run<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    host: H,
) -> Result<(), String> {
    let entrypoint = text::Entrypoint::parse(&Source::inline(source))
        .map_err(|error| error.format())?
        .with_prelude();

    run_entrypoint(timeout, &entrypoint, &text::EmptyStore, host)
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
    let source = fs::read_to_string(path)
        .map_err(|error| format!("failed to read {}: {error}", path.display()))?;

    let entrypoint = text::Entrypoint::parse(&Source::new(path, source))
        .map_err(|error| error.format())?
        .with_prelude();
    let store = text::FileStore::new(path.parent().unwrap_or(Path::new(".")), &entrypoint)
        .map_err(|error| error.format())?;

    run_entrypoint(timeout, &entrypoint, &store, host)
}
