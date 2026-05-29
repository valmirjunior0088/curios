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
    std::{fs, path::Path, time::Duration},
};

pub fn run<L: text::Loader, H: Host + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    loader: &L,
    host: H,
) -> Result<(), String> {
    run_wasm(&compile(timeout, loader, None, source, |_| {})?, host)
}

pub fn run_text<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    source: &str,
    host: H,
) -> Result<(), String> {
    run(timeout, source, &text::PanicLoader, host)
}

pub fn run_file<H: Host + Send + Sync + 'static>(
    timeout: Duration,
    path: &Path,
    host: H,
) -> Result<(), String> {
    let source = fs::read_to_string(path)
        .map_err(|error| format!("failed to read {}: {error}", path.display()))?;

    let base = path.parent().unwrap_or(Path::new(".")).to_path_buf();

    run(timeout, &source, &text::FileLoader::new(base), host)
}
