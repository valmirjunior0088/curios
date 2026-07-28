#[path = "src/archive.rs"]
mod archive;
use archive::*;

#[path = "src/syntax.rs"]
#[allow(unreachable_pub)]
mod syntax;
use syntax::SYNTAX;

use {
    curios_abi::host_ops,
    curios_base::{Qualifier, RootId},
    curios_text::{Module, PreludeModules},
    sha2::{Digest, Sha256},
    std::{
        collections::BTreeSet,
        env, fs,
        path::{Path, PathBuf},
    },
};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/archive.rs");
    println!("cargo:rerun-if-changed=src/syntax.rs");
    println!("cargo:rerun-if-env-changed=CURIOS_PRELUDE_LOG");
    install_tracing();

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
    modules.insert_root("sys", RootId::Sys, curios_text::sys_module(&host_ops()));
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
    assert_eq!(
        prepared.core().universe_seeds.len(),
        prepared.universe_floor(),
        "lowered Text universe floor does not match its seed table"
    );
    curios_core::validate_lowered_universe_seeds(prepared.core(), prepared.universe_floor())
        .unwrap_or_else(|error| panic!("lowered Text universe seeds are invalid: {error}"));

    let lowered = prepared.core().clone();
    let mut context = curios_core::Context::with_default_budget();
    let (core, body_type) = curios_core::elaborate_and_zonk_module(
        &mut context,
        &lowered,
        prepared.metavariable_floor(),
        prepared.universe_floor(),
        curios_core::Mode::Infer,
    )
    .unwrap_or_else(|error| {
        panic!(
            "fixed prelude failed to elaborate: {}",
            error.format_with(&lowered)
        )
    });

    // Every universe invariant the archive is trusted to satisfy is asserted
    // here, on the value about to be serialized, and nowhere else. Restoration
    // establishes that the bytes it reads are exactly the bytes written from
    // this value — content digest, schema, source fingerprint, and bytecheck —
    // so re-deriving the invariants per compilation only re-answers a question
    // already settled. `erase_prelude_prefix` below happens to project
    // through the same check, but inheriting the guarantee from an unrelated
    // call is not the same as stating it.
    curios_core::validate_universes(&core)
        .unwrap_or_else(|error| panic!("elaborated fixed prelude universes are invalid: {error}"));

    let ersd =
        curios_core::erase_prelude_prefix(&mut curios_core::Context::with_default_budget(), &core)
            .unwrap_or_else(|error| {
                panic!(
                    "fixed prelude failed to erase into the arena prefix: {}",
                    error.format_with(&core)
                )
            });

    // Hash-cons every archived Core snapshot against one table, so structurally
    // equal subterms collapse onto a single allocation across the lowered and
    // elaborated views as well as within each. Elaboration builds the same
    // types, telescopes, and proof spines independently in definition after
    // definition and nothing deduplicates them, because `Rc` sharing only ever
    // arises from cloning: two definitions that build the same type build it
    // twice. rkyv shares by pointer address, so collapsing them here is also
    // what lets the archive store each distinct structure once.
    //
    // `ersd` is deliberately not included: it is a flat, index-addressed arena
    // with no shared pointers to collapse, and it already interns its constants
    // by value.
    let sharing = curios_core::Sharing::new();
    let prepared = prepared.shared(&sharing);
    let core = core.shared(&sharing);
    let body_type = sharing.share(&body_type);
    println!(
        "cargo:warning=fixed prelude hash-consed to {} distinct structures",
        sharing.structures()
    );

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

/// Install a `tracing` subscriber over the prelude elaboration when
/// `CURIOS_PRELUDE_LOG` names a filter, and do nothing otherwise.
///
/// Elaborating the fixed prelude is the compiler's largest single run, and it
/// happens here rather than under any binary a profiler or debugger can attach
/// to. Without a subscriber the spans and events the compiler crates already
/// emit go nowhere, which leaves recompiling with `eprintln!` as the only way to
/// ask a question — minutes per question, and the answer is gone when the probe
/// is removed. A filter read from the environment instead selects targets and
/// levels at *run* time, so successive questions cost a run rather than a build.
///
/// `FmtSpan::CLOSE` reports each span's duration as it closes, which is what
/// makes this double as the per-declaration breakdown of the prelude build.
/// `cargo:rerun-if-env-changed` above keeps a changed filter from being served
/// out of Cargo's cache as a no-op.
#[cfg(feature = "profile")]
fn install_tracing() {
    use tracing_subscriber::{EnvFilter, fmt, fmt::format::FmtSpan, prelude::*};

    let Some(filter) = env::var_os("CURIOS_PRELUDE_LOG") else {
        return;
    };

    tracing_subscriber::registry()
        .with(
            fmt::layer()
                .with_writer(std::io::stderr)
                .with_span_events(FmtSpan::CLOSE)
                // `without_time` would also suppress the busy/idle figures a
                // close event carries, which are the whole point of enabling it.
                .with_timer(fmt::time::uptime())
                .with_ansi(false),
        )
        .with(EnvFilter::new(filter.to_string_lossy()))
        .init();
}

/// Without the `profile` feature the compiler crates emit no spans or events at
/// all, so there is nothing for a subscriber to observe.
#[cfg(not(feature = "profile"))]
fn install_tracing() {}

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
        .cloned()
        .collect::<BTreeSet<_>>();
    for target in SYNTAX.targets() {
        let symbol = target.symbol();
        assert!(
            names.contains(&curios_core::Global::Authored(target.qualifier())),
            "registered syntax target '{symbol}' is absent from the lowered prelude; nearby names: {:?}",
            names
                .iter()
                .map(curios_core::Global::symbol)
                .filter(|name| name.contains(target.last()))
                .collect::<Vec<_>>()
        );
    }
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}
