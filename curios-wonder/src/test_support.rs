//! What `curios`'s suites read of the engine that no transport reads: a library's documentation record made the way a question compiles, and the subject a text on standard input is asked about as.

use {
    crate::*,
    curios_document::Documentation,
    curios_module::{Overlay, RootSource},
    curios_package::{Selection, Spelling, Supplied},
    curios_pipeline::{Cache, CompileError, Fold},
    curios_surface::WrittenIn,
    curios_verdict::Verdicts,
    std::path::Path,
};

/// The subject `text`, on standard input and written in `written`, is asked about as, as `curios wonder -` admits it: a program where a term closes its declarations, and a module of its own at `/stdin` where none does.
pub fn stdin_subject(text: &str, written: WrittenIn) -> Subject {
    let selection = Selection::of(
        Spelling::Stdin,
        None,
        Path::new("."),
        &Overlay::default(),
        None,
        || {
            Ok(Supplied {
                text: text.to_string(),
                written,
            })
        },
    )
    .expect("standard input is selected wherever it is asked");

    Asked::every(selection)
        .expect("a text on standard input is asked about as itself")
        .pop()
        .expect("standard input is one subject")
        .subject
}

/// The subject the file `path` is asked about as, its text read through `overlay` before the disk, as `curios wonder <path>` admits it standing in `directory`: the unit that declares it, or, where none does, the program or the module it is written as.
pub fn file_subject(path: &Path, directory: &Path, overlay: &Overlay) -> Subject {
    let selection = Selection::of(
        Spelling::File(path.to_path_buf()),
        None,
        directory,
        overlay,
        None,
        || unreachable!("a file is asked about, and standard input is never drained"),
    )
    .expect("the file is selected");

    Asked::every(selection)
        .expect("the file is asked about as what selects it")
        .pop()
        .expect("a file selects one subject")
        .subject
}

/// The interface of the last of `units` — a package's library, compiled against everything before it — for its consumers. `overlay` and `cache` behave exactly as they do for [`diagnostics`](diagnostics()): unsaved text wins over the disk, and the cabinet is read, and filed into where a question may.
///
/// The compilation runs to completion first, the certifier included, so a library that does not check is not documented and reports what stopped it exactly as `run` would. The record itself is made after it, from the unit's stored tables and its sources, whether the unit was compiled now or reused from the cabinet.
pub fn documentation(
    budget: u64,
    units: Vec<RootSource>,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Result<Documentation, CompileError> {
    let reached = cache.map(|cache| Overlaid::over(cache, overlay));
    let cache = reached.as_ref().map(|cache| cache as &dyn Cache);
    let units = overlaid(units, overlay);

    Fold::new(budget, &units, cache).documentation(|_| {})
}
