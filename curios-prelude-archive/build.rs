#[path = "src/archive.rs"]
mod archive;
use archive::*;

#[path = "src/syntax.rs"]
#[allow(unreachable_pub)]
mod syntax;
use syntax::SYNTAX;

use {
    curios_abi::host_ops,
    curios_base::{Qualifier, RootKind},
    curios_core::Item,
    curios_core::{Global, Sharing, derived_binder_floor, validate_stored_identities},
    curios_elab::{
        Context, ErasedArena, Mode, Resumed, elaborate_and_zonk_module, erase_unit,
        validate_lowered_universe_seeds, validate_universes,
    },
    curios_text::{Module, RootSource, prepare_prelude, sys_module},
    sha2::{Digest, Sha256},
    std::{
        collections::BTreeSet,
        env, fs,
        path::{Path, PathBuf},
    },
};

// Installed for the whole build script so the capture's memory columns are populated; the counters are what make this build's own footprint measurable, which is the question the prelude build most often raises.
#[cfg(feature = "profile")]
#[global_allocator]
static ALLOCATOR: curios_profile::CountingAllocator = curios_profile::CountingAllocator;

fn main() {
    // Under the `profile` feature the whole build runs inside a programmatic capture — the report lands in `OUT_DIR/profile.tsv`, announced with one warning. There is deliberately no environment switch: the feature is the switch, and it is specified where every other build input is.
    #[cfg(feature = "profile")]
    {
        let ((), report) = curios_profile::capture(build);
        let out = PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("profile.tsv");
        let mut rendered = String::from(
            "total_ms\tcalls\tretained_mb\tallocated_mb\tallocs\ttarget\tname\tgroup\n",
        );
        for summary in &report.summaries {
            rendered.push_str(&format!(
                "{:.3}\t{}\t{:.1}\t{:.1}\t{}\t{}\t{}\t{}\n",
                summary.total.as_secs_f64() * 1_000.0,
                summary.calls,
                summary.retained as f64 / (1024.0 * 1024.0),
                summary.allocated as f64 / (1024.0 * 1024.0),
                summary.allocations,
                summary.target,
                summary.name,
                summary.group.as_deref().unwrap_or(""),
            ));
        }
        if !report.samples.is_empty() {
            rendered.push_str("\ncount\ttotal\tmin\tmean\tmax\ttarget\tname\n");
            for sample in &report.samples {
                rendered.push_str(&format!(
                    "{}\t{}\t{}\t{:.1}\t{}\t{}\t{}\n",
                    sample.count,
                    sample.total,
                    sample.min,
                    sample.mean(),
                    sample.max,
                    sample.target,
                    sample.name,
                ));
            }
        }
        fs::write(&out, rendered).expect("failed to write the build profile");
        println!(
            "cargo:warning=prelude build profile written to {} (peak {:.1} MiB)",
            out.display(),
            report.peak as f64 / (1024.0 * 1024.0),
        );
    }
    #[cfg(not(feature = "profile"))]
    build();
}

fn build() {
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

    let mut modules = RootSource::supplied();
    modules.insert_root("sys", RootKind::Internal, sys_module(&host_ops()));
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

    for source in sources.iter().filter(|path| {
        path.starts_with(manifest.join("syn")) || path.starts_with(manifest.join("std"))
    }) {
        modules.insert_module(source_qualifier(&manifest, source), parse_module(source));
    }

    let prepared = prepare_prelude(&modules, &SYNTAX)
        .unwrap_or_else(|error| panic!("fixed prelude failed to lower: {}", error.format()));
    validate_syntax_targets(prepared.core());
    assert_eq!(
        prepared.core().universe_seeds.len(),
        prepared.universe_floor(),
        "lowered Text universe floor does not match its seed table"
    );
    validate_lowered_universe_seeds(prepared.core(), prepared.universe_floor())
        .unwrap_or_else(|error| panic!("lowered Text universe seeds are invalid: {error}"));

    let lowered = prepared.core().clone();
    let mut context = Context::with_default_budget(SYNTAX);
    let (core, _body_type) = elaborate_and_zonk_module(
        &mut context,
        &lowered,
        prepared.metavariable_floor(),
        prepared.universe_floor(),
        Mode::Infer,
    )
    .unwrap_or_else(|error| {
        panic!(
            "fixed prelude failed to elaborate: {}",
            error.format_with(&lowered, &[])
        )
    });

    // Every universe invariant the archive is trusted to satisfy is asserted here, on the value about to be serialized, and nowhere else. Restoration establishes that the bytes it reads are exactly the bytes written from this value — schema, source fingerprint, and bytecheck — so re-deriving the invariants per compilation only re-answers a question already settled. `erase_prelude_prefix` below happens to project through the same check, but inheriting the guarantee from an unrelated call is not the same as stating it.
    validate_universes(&core)
        .unwrap_or_else(|error| panic!("elaborated fixed prelude universes are invalid: {error}"));

    // This is the seam a unit is stored at, so it is where the rule about what a stored unit may carry is enforced: no identity meaningful only in the compilation that assigned it. Two calls rather than one, because the universe half of that rule is what the line above already refuses — an unsolved universe metavariable, named as such — and a rule stated twice is a copy rather than a second opinion.
    validate_stored_identities(&core).unwrap_or_else(|found| {
        panic!("elaborated fixed prelude carries a positional identity: {found}")
    });

    // No entrypoint, so nothing to seal: this unit's arena stays open, which is what its successors resume over.
    let mut ersd = erase_unit(
        &mut Context::with_default_budget(SYNTAX),
        Resumed::of(&[], ErasedArena::default()),
        &core,
        None,
    )
    .unwrap_or_else(|error| {
        panic!(
            "fixed prelude failed to erase into the arena prefix: {}",
            error.format_with(&core, &[])
        )
    });

    // Erasure tombstones as it goes, and the image is restored and walked by every compilation that follows — so the dead slots are compacted out here rather than serialized and stepped over forever after.
    ersd.compact();

    // Hash-cons every archived Core snapshot against one table, so structurally equal subterms collapse onto a single allocation across the lowered and elaborated views as well as within each. Elaboration builds the same types, telescopes, and proof spines independently in definition after definition and nothing deduplicates them, because `Rc` sharing only ever arises from cloning: two definitions that build the same type build it twice. rkyv shares by pointer address, so collapsing them here is also what lets the archive store each distinct structure once.
    //
    // `ersd` is deliberately not included: it is a flat, index-addressed arena with no shared pointers to collapse, and it already interns its constants by value.
    let sharing = Sharing::new();
    let prepared = prepared.shared(&sharing);
    let core = core.shared(&sharing);
    println!(
        "cargo:warning=fixed prelude hash-consed to {} distinct structures",
        sharing.structures()
    );

    // Derived here, where the walk that establishes this image runs, so per-compile rechecking reads the bound instead of re-deriving it over every archived term.
    let binder_floor = derived_binder_floor(&core);
    let image = PreludeArchive {
        schema: SCHEMA,
        fingerprint,
        prepared,
        core,
        binder_floor,
        ersd,
    };
    let first = curios_archive::rkyv::to_bytes::<curios_archive::rkyv::rancor::Error>(&image)
        .expect("fixed prelude archive serialization failed");
    let second = curios_archive::rkyv::to_bytes::<curios_archive::rkyv::rancor::Error>(&image)
        .expect("fixed prelude archive repeat serialization failed");
    assert_eq!(
        first.as_slice(),
        second.as_slice(),
        "fixed prelude archive is not deterministic"
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
        .flat_map(Item::declared_names)
        .cloned()
        .collect::<BTreeSet<_>>();
    for target in SYNTAX.targets() {
        let symbol = target.symbol();
        assert!(
            names.contains(&Global::Authored(target.qualifier())),
            "registered syntax target '{symbol}' is absent from the lowered prelude; nearby names: {:?}",
            names
                .iter()
                .map(Global::symbol)
                .filter(|name| name.contains(target.last()))
                .collect::<Vec<_>>()
        );
    }

    // A concept can exist under the registered name and still not declare the method the elaborator projects, which the presence check above cannot see. `elaborate_infix` resolves that method *positionally* against the declaration, so a drifted label is not a compile error here but a panic on the first program that writes the operator.
    for target in SYNTAX.concept_fields() {
        let symbol = target.concept.symbol();
        let field = target.field;
        let concept = module
            .concepts
            .get(&Global::Authored(target.concept.qualifier()))
            .unwrap_or_else(|| {
                panic!(
                    "registered operator concept '{symbol}' is not a concept in the lowered prelude"
                )
            });
        assert!(
            concept.fields.iter().any(|declared| declared == field),
            "registered operator concept '{symbol}' does not declare the method '{field}'; it declares: {:?}",
            concept.fields
        );
    }
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}
