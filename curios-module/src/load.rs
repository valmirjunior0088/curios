//! Reading a module or an entrypoint from where it is held — a file, an editor's buffer, or text supplied whole — and pairing an entrypoint with the [`RootSource`] its unit resolves its file-backed modules through.

#[cfg(test)]
mod tests;

use {
    crate::{LoadError, RootSource, parsed},
    curios_common::Source,
    curios_parse::ParserError,
    curios_surface::{Entrypoint, Module},
    std::{path::Path, sync::Arc},
};

/// Read and parse a standalone module from the disk, through the thread's parse memo as every file-backed module is read ([`RootSource`]): `curios-package` reads a library's header with it to learn what the library declares, and the compilation that reads the same text afterwards parses nothing.
pub fn read_module(path: impl AsRef<Path>) -> Result<Module, LoadError> {
    parsed(path.as_ref(), None).map(|(module, _)| module)
}

/// Reads and parses `path` as an entrypoint (top-level items followed by a tail expression), handing back the text that was parsed as well as what it parsed to. The file-path counterpart of `Entrypoint`'s `FromStr`, distinguished by keeping the real path in the [`Source`] so diagnostics name the file; a parsed `Entrypoint` resolves its file-backed `mod` declarations separately, through whatever [`RootSource`] the caller pairs it with.
fn read_entrypoint(path: impl AsRef<Path>) -> Result<(Entrypoint, Arc<Source>), LoadError> {
    let path = path.as_ref();
    let source = Source::read(path).map_err(|error| LoadError::Read {
        path: path.into(),
        error,
    })?;

    Entrypoint::parse(&source)
        .map(|entrypoint| (entrypoint, source))
        .map_err(LoadError::Parse)
}

/// Reads `path` as an entrypoint, paired with the [`RootSource`] the directory it sits in anchors and the text that was parsed — the file is the root of its unit, so `mod util` in `main.crs` reads `util.crs` beside it.
///
/// The pairing is the point: `read_entrypoint` leaves a parsed entrypoint's file-backed `mod` declarations unresolved, and every caller that opens a file then has to know which `RootSource` goes with it. That is one answer, not a caller's choice, so it lives beside the two calls it makes.
///
/// **The loader logs the entry's own header as read, from the text parsed here.** A `RootSource` logs what it resolves, and the entry's header is deliberately never resolved through it — the caller already has the body — so it is logged where the two are paired (`RootSource::opened`), and a cache that verifies a compilation against what it read finds the entry among the rest. It must be *this* text rather than a re-read of the path: re-reading races an edit landing between the parse and the digest, and records newer text against an older artifact, which is the one direction that admits stale. The source comes back too, for a caller that files what the whole program compiled to.
pub fn open_entrypoint(path: &Path) -> Result<(Entrypoint, RootSource, Arc<Source>), LoadError> {
    let (entrypoint, source) = read_entrypoint(path)?;
    let loader = RootSource::opened(path, &source);

    Ok((entrypoint, loader, source))
}

/// Parses `text` as an entrypoint, paired with the [`RootSource`] that resolves nothing — the counterpart of [`open_entrypoint`] for a program that arrived as text rather than as a file. `label` names it in diagnostics, where a file's path would go.
///
/// **Nothing resolves because there is nowhere to resolve from.** A root's file-backed modules live in the directory it sits in, and text sits in none, so `mod util;` fails here as an unfound module rather than reading a directory somebody upstream had to invent. Inline modules are untouched, which leaves a supplied program able to declare everything it uses — it just cannot spread itself over files it has no name for.
///
/// The pairing lives here for the reason [`open_entrypoint`]'s does: which `RootSource` goes with an entrypoint is one answer, not a caller's choice. The source comes back for the same reason too, though nothing verifies a supplied program against it — text that was never on disk cannot go stale, so what a caller has here is the third element of one shape rather than a second one to handle.
pub fn supply_entrypoint(
    label: &str,
    text: &str,
) -> Result<(Entrypoint, RootSource, Arc<Source>), ParserError> {
    let source = Source::labelled(label, text);
    let entrypoint = Entrypoint::parse(&source)?;

    Ok((entrypoint, RootSource::none(), source))
}

/// [`open_entrypoint`] with `text` standing in for what `path` holds — an editor's unsaved buffer — paired with the same [`RootSource`] the file would get, so its file-backed `mod` declarations resolve where the saved file's would. The source names `path`, so a diagnostic about it reads as one about the file.
///
/// The overlay for the *rest* of the unit is the loader's to carry (`RootSource::with_overlay`); this is the one file the loader never reads, supplied the one way it can be.
pub fn overlay_entrypoint(
    path: &Path,
    text: &str,
) -> Result<(Entrypoint, RootSource, Arc<Source>), ParserError> {
    let source = Source::held(path, text);
    let entrypoint = Entrypoint::parse(&source)?;
    let loader = RootSource::opened(path, &source);

    Ok((entrypoint, loader, source))
}
