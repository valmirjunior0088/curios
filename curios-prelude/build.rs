#[path = "src/archive.rs"]
mod archive;
use archive::*;

#[path = "src/syntax.rs"]
#[allow(unreachable_pub)]
mod syntax;
use syntax::SYNTAX;

use {
    curios_abi::{RootId, sys_io},
    curios_base::Qualifier,
    curios_text::{Module, PreludeModules},
    sha2::{Digest, Sha256},
    std::{
        collections::BTreeSet,
        env, fs,
        path::{Path, PathBuf},
        time::Duration,
    },
};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/archive.rs");
    println!("cargo:rerun-if-changed=src/syntax.rs");

    let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let sources = source_files(&manifest);
    for source in &sources {
        println!("cargo:rerun-if-changed={}", source.display());
    }

    let fingerprint = fingerprint(&manifest, &sources);
    println!(
        "cargo:rustc-env=CURIOS_PRELUDE_FINGERPRINT={}",
        hex(&fingerprint)
    );

    let mut modules = PreludeModules::new();
    modules.insert_root("sys", RootId::Sys, curios_text::sys_module(&sys_io()));
    modules.insert_root("syn", RootId::Syn, parse_module(manifest.join("syn.crs")));
    modules.insert_root("std", RootId::Std, parse_module(manifest.join("std.crs")));

    for source in sources.iter().filter(|path| {
        path.starts_with(manifest.join("syn")) || path.starts_with(manifest.join("std"))
    }) {
        modules.insert_module(source_qualifier(&manifest, source), parse_module(source));
    }

    let prepared = curios_text::prepare_prelude(&modules, &SYNTAX)
        .unwrap_or_else(|error| panic!("fixed prelude failed to lower: {}", error.format()));
    validate_syntax_targets(prepared.core());

    let lowered = prepared.core().clone();
    let mut context = curios_core::Context::new(Duration::from_secs(300));
    let (core, body_type) = curios_core::elaborate_and_zonk_module(
        &mut context,
        &lowered,
        prepared.metavariable_floor(),
        curios_core::Mode::Infer,
    )
    .unwrap_or_else(|error| {
        panic!(
            "fixed prelude failed to elaborate: {}",
            error.format_with(&lowered)
        )
    });

    let ersd = curios_core::erase_prelude_to_ir_prefix(
        &mut curios_core::Context::new(Duration::from_secs(300)),
        &core,
    )
    .unwrap_or_else(|error| {
        panic!(
            "fixed prelude failed to erase into the arena prefix: {}",
            error.format_with(&core)
        )
    });

    let image = PreludeArchive {
        schema: SCHEMA,
        fingerprint,
        prepared,
        core,
        body_type,
        ersd,
    };
    let first = rkyv::to_bytes::<rkyv::rancor::Error>(&image)
        .expect("fixed prelude archive serialization failed");
    let second = rkyv::to_bytes::<rkyv::rancor::Error>(&image)
        .expect("fixed prelude archive repeat serialization failed");
    assert_eq!(
        first.as_slice(),
        second.as_slice(),
        "fixed prelude archive is not deterministic"
    );
    println!(
        "cargo:rustc-env=CURIOS_PRELUDE_ARCHIVE_DIGEST={}",
        hex(&Sha256::digest(first.as_slice()))
    );

    let out = PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("prelude.rkyv");
    fs::write(out, first).expect("failed to write fixed prelude archive");
}

fn source_files(manifest: &Path) -> Vec<PathBuf> {
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

fn fingerprint(manifest: &Path, sources: &[PathBuf]) -> [u8; 32] {
    let mut digest = Sha256::new();
    digest.update(SCHEMA.to_le_bytes());
    for source in sources {
        let relative = source
            .strip_prefix(manifest)
            .expect("prelude source lies below its crate");
        let path = relative.to_string_lossy();
        let bytes = fs::read(source)
            .unwrap_or_else(|error| panic!("failed to read {}: {error}", source.display()));
        digest.update((path.len() as u64).to_le_bytes());
        digest.update(path.as_bytes());
        digest.update((bytes.len() as u64).to_le_bytes());
        digest.update(bytes);
    }
    digest.finalize().into()
}

fn validate_syntax_targets(module: &curios_core::Module) {
    let names = module
        .items
        .iter()
        .flat_map(curios_core::Item::declared_names)
        .collect::<BTreeSet<_>>();
    for target in SYNTAX.targets() {
        assert!(
            names.contains(target),
            "registered syntax target '{target}' is absent from the lowered prelude; nearby names: {:?}",
            names
                .iter()
                .filter(|name| name.contains(target.rsplit('/').next().unwrap_or(target)))
                .collect::<Vec<_>>()
        );
    }
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}
