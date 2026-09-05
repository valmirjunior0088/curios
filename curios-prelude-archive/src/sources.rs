//! The authored prelude as a root source: `/sys` synthesized from the host table, `/syn` and `/std` parsed from this crate's two trees. Shared by the build script, which lowers it into the archive, and by the tests, which hold the same sources to what the archive cannot check — that they lint clean.

use {
    curios_abi::host_ops,
    curios_text::{Module, RootSource, sys_module},
    curios_utilities::{Qualifier, RootKind},
    std::{
        fs,
        path::{Path, PathBuf},
    },
};

use crate::syntax::SYNTAX;

/// Every prelude source under `manifest`, the two indexes first, the rest in path order.
pub(crate) fn source_files(manifest: &Path) -> Vec<PathBuf> {
    let mut files = vec![manifest.join("syn.crs"), manifest.join("std.crs")];
    collect_crs(&manifest.join("syn"), &mut files);
    collect_crs(&manifest.join("std"), &mut files);
    files.sort();
    files
}

fn collect_crs(directory: &Path, files: &mut Vec<PathBuf>) {
    let mut entries = fs::read_dir(directory)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", directory.display()))
        .map(|entry| {
            entry
                .expect("failed to read prelude directory entry")
                .path()
        })
        .collect::<Vec<_>>();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            collect_crs(&path, files);
        } else if path.extension().is_some_and(|extension| extension == "crs") {
            files.push(path);
        }
    }
}

fn parse_module(path: impl AsRef<Path>) -> Module {
    let path = path.as_ref();
    Module::from_path(path)
        .unwrap_or_else(|error| panic!("failed to parse {}: {error:?}", path.display()))
}

fn source_qualifier(manifest: &Path, source: &Path) -> Qualifier {
    let relative = source
        .strip_prefix(manifest)
        .expect("prelude source lies below its crate");
    let mut segments = relative
        .components()
        .map(|component| component.as_os_str().to_string_lossy().into_owned())
        .collect::<Vec<_>>();
    let last = segments.last_mut().expect("prelude source has a file name");
    *last = last
        .strip_suffix(".crs")
        .expect("prelude source extension was filtered")
        .to_owned();
    Qualifier::from(segments)
}

/// The three roots the fixed prelude is lowered from, with every authored module under `manifest` filed at its qualifier.
pub(crate) fn authored_prelude(manifest: &Path) -> RootSource {
    let mut modules = RootSource::supplied();
    modules.insert_root("sys", RootKind::Internal, sys_module(&host_ops(), &SYNTAX));
    modules.insert_root(
        "syn",
        RootKind::Privileged,
        parse_module(manifest.join("syn.crs")),
    );
    modules.insert_root(
        "std",
        RootKind::Privileged,
        parse_module(manifest.join("std.crs")),
    );

    for source in source_files(manifest).iter().filter(|path| {
        path.starts_with(manifest.join("syn")) || path.starts_with(manifest.join("std"))
    }) {
        modules.insert_module(source_qualifier(manifest, source), parse_module(source));
    }

    modules
}
