#[path = "src/syntax.rs"]
#[allow(unreachable_pub)]
mod syntax;
use syntax::SYNTAX;

#[path = "src/sources.rs"]
mod sources;
use sources::*;

use {
    curios_core::Item,
    curios_core::{Global, Sharing, Zonked, derived_binder_floor, validate_stored_identities},
    curios_elab::{
        Context, ErasedArena, Established, Mode, Resumed, Tail, elaborate_and_zonk_unit,
        erase_unit, validate_lowered_universe_seeds, validate_universes,
    },
    curios_text::{PreparedText, prepare_prelude},
    curios_unit::Unit,
    std::{collections::BTreeSet, env, fs, path::PathBuf},
};

/// What the standard library's landing page says it is — the one description no manifest supplies.
const STD_DESCRIPTION: &str = "The standard library: what every Curios program gets for free, compiled into the fixed prelude beside the syntax forms and the host's operations.";

// Installed for the whole build script so the capture's memory columns are populated; the counters are what make this build's own footprint measurable, which is the question the prelude build most often raises.
#[cfg(feature = "profile")]
#[global_allocator]
static ALLOCATOR: curios_profile::CountingAllocator = curios_profile::CountingAllocator;

fn main() {
    // Under the `profile` feature the whole build runs under a record stream, filed beside the archive it builds. There is deliberately no environment switch: the feature is the switch, and it is specified where every other build input is.
    #[cfg(feature = "profile")]
    {
        // Filed here rather than under `OUT_DIR` because it is read after the build that wrote it, which is `.artifacts`'s rule; a hung prelude build is the case it exists for, and that build never reaches the summary below. A build script has no caller to take a destination from — unlike the CLI, whose `--profile` names one — so this is the one place that chooses its own, and the directory is the destination's to make.
        let out = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap())
            .join(".artifacts")
            .join("profile.tsv");

        curios_profile::trace(
            curios_profile::Destination::Rotating {
                path: out.clone(),
                cap: 512 * 1024 * 1024,
            },
            build,
        )
        .expect("failed to open the build profile");

        let rows = fs::File::open(&out).expect("the build profile reopens");
        let report =
            curios_profile::fold(std::io::BufReader::new(rows)).expect("the build profile folds");
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
    println!("cargo:rerun-if-changed=src/sources.rs");
    println!("cargo:rerun-if-changed=src/syntax.rs");

    let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    // The directory rather than each discovered file: Cargo scans a watched directory recursively, so a *newly added* source triggers the rerun a per-file directive cannot — it matches no directive from the run that predates it. The index sits beside the directory, not inside it, so it is named on its own.
    for watched in ["std.crs", "std"] {
        println!(
            "cargo:rerun-if-changed={}",
            manifest.join(watched).display()
        );
    }

    let sys_modules = sys_source();
    // The standard library is the one prelude mount a program reaches for by name, so it is the one the image documents; `/sys` is the host's rows and the intrinsic carriers, which no consumer reads for.
    let std_modules = std_source(&manifest).documented("std", Some(STD_DESCRIPTION));

    // Both roots lowered before either is elaborated, because the registry spans them: a target check over `/sys` alone would miss every `/std` slot and one over `/std` alone every `/sys` slot, so the check runs once over the union and there is no half to pass by being asked the wrong question.
    let sys_text = lower("sys", &sys_modules, &[]);
    let std_text = lower("std", &std_modules, &[&sys_text]);
    validate_syntax_targets(&[sys_text.core(), std_text.core()]);

    // The fold: `/sys` against nothing, `/std` against what `/sys` established. Each image is a bare `Unit`, in the format every store slot files a unit in, so a restored prelude is a two-unit prefix and not a shape of its own.
    let sys = archive(
        "sys",
        sys_text,
        Established::nothing(),
        &[],
        ErasedArena::default(),
    );
    let std_scope = [sys.core()];
    let std = archive(
        "std",
        std_text,
        Established::over(&std_scope),
        &std_scope,
        sys.arena(),
    );

    // Filed beside this crate rather than under `OUT_DIR`, because the images are read outside the build: `curios document` renders the standard library's pages from `std.rkyv`, so it needs a path a recipe can name. The crate includes them from the same paths, so there is one image per unit in one place; the rule for a product that outlives its build is `.artifacts/`, which `cargo clean` leaves alone and `cargo x clean` removes.
    let artifacts = manifest.join(".artifacts");
    fs::create_dir_all(&artifacts).expect("failed to create the archive's .artifacts directory");
    write_image(&artifacts, "sys", &sys);
    write_image(&artifacts, "std", &std);
}

/// Resolve and lower one prelude root against the roots already lowered, with every invariant the lowered form is trusted to satisfy asserted here.
fn lower(root: &str, modules: &curios_text::RootSource, scope: &[&PreparedText]) -> PreparedText {
    let prepared = prepare_prelude(modules, scope, &SYNTAX)
        .unwrap_or_else(|error| panic!("/{root} failed to lower: {}", error.format()));
    assert_eq!(
        prepared.core().universe_seeds.len(),
        prepared.universe_floor(),
        "lowered Text universe floor of /{root} does not match its seed table"
    );
    validate_lowered_universe_seeds(prepared.core(), prepared.universe_floor()).unwrap_or_else(
        |error| panic!("lowered Text universe seeds of /{root} are invalid: {error}"),
    );

    prepared
}

/// Elaborate, validate, erase and hash-cons one lowered root into the unit its image is, against what the roots before it established.
///
/// `arena` is the previous root's, not a fresh one: each unit's erasure resumes over what the one before it produced, so the arena a unit carries is the whole prefix's and the split between images is a split of items rather than of operands.
fn archive(
    root: &str,
    prepared: PreparedText,
    established: Established<'_>,
    scope: &[&curios_core::Module],
    arena: ErasedArena,
) -> Unit {
    let lowered = prepared.core().clone();
    let mut context = Context::with_default_budget(SYNTAX);
    // Grown explicitly, where the whole-module spelling this replaced grew for its caller: a root is the deepest module the compiler ever elaborates, and a build script's thread is the smallest stack it is ever elaborated on.
    let (core, _body_type) = curios_utilities::grown(|| {
        elaborate_and_zonk_unit(
            &mut context,
            established,
            &lowered,
            prepared.metavariable_floor(),
            prepared.universe_floor(),
            Mode::Infer,
            Tail::Written,
        )
    })
    .unwrap_or_else(|error| {
        panic!(
            "/{root} failed to elaborate: {}",
            error.format_with(&lowered, scope, &SYNTAX)
        )
    });

    // Every universe invariant the archive is trusted to satisfy is asserted here, on the value about to be serialized, and nowhere else. Restoration reads exactly the bytes written from this value — a constant of the same build, whose structure bytecheck confirms — so re-deriving the invariants per compilation only re-answers a question already settled. `erase_unit` below happens to project through the same check, but inheriting the guarantee from an unrelated call is not the same as stating it.
    validate_universes(&core)
        .unwrap_or_else(|error| panic!("elaborated /{root} universes are invalid: {error}"));

    // This is the seam a unit is stored at, so it is where the rule about what a stored unit may carry is enforced: no identity meaningful only in the compilation that assigned it. Two calls rather than one, because the universe half of that rule is what the line above already refuses — an unsolved universe metavariable, named as such — and a rule stated twice is a copy rather than a second opinion.
    validate_stored_identities(&core).unwrap_or_else(|found| {
        panic!("elaborated /{root} carries a positional identity: {found}")
    });

    // The archive's own zonk evidence, taken where the module is final: erasure below consumes it, and it is the same claim `zonk_module` just enforced, restated as a checked value rather than inherited.
    let zonked = Zonked::project(&core)
        .unwrap_or_else(|refusal| panic!("elaborated /{root} is not zonked: {refusal}"));

    // No entrypoint, so nothing to seal: this unit's arena stays open, which is what its successors resume over.
    let mut ersd = erase_unit(
        &mut Context::with_default_budget(SYNTAX),
        Resumed::of(scope, arena),
        &zonked,
        None,
    )
    .unwrap_or_else(|error| {
        panic!(
            "/{root} failed to erase into the erased prefix: {}",
            error.format_with(&core, scope, &SYNTAX)
        )
    });

    // Erasure tombstones as it goes, and the image is restored and walked by every compilation that follows — so the dead slots are compacted out here rather than serialized and stepped over forever after.
    ersd.compact();

    // Hash-cons every archived Core snapshot against one table, so structurally equal subterms collapse onto a single allocation across the lowered and elaborated views as well as within each. Elaboration builds the same types, telescopes, and proof spines independently in definition after definition and nothing deduplicates them, because `Rc` sharing only ever arises from cloning: two definitions that build the same type build it twice. rkyv shares by pointer address, so collapsing them here is also what lets the archive store each distinct structure once.
    //
    // One table per image, not one across both: rkyv shares by pointer address *within* a single image, so a structure collapsed across the two would still be written into each of them — a shared table would buy nothing and would report a distinct-structure count no image actually has.
    //
    // `ersd` is deliberately not included: it is a flat, index-addressed arena with no shared pointers to collapse, and it already interns its constants by value.
    let sharing = Sharing::new();
    let prepared = prepared.shared(&sharing);
    let core = core.shared(&sharing);
    // Plain stdout, not `cargo:warning=`: this is a metric nobody acts on during a build, and a line that shouts on every build of every consumer teaches readers to skim `warning:` — which is the habit that loses a real one later. Cargo captures it to `target/<profile>/build/<pkg>-<hash>/output`, where `-vv` or a reader who went looking will find it.
    println!(
        "/{root} hash-consed to {} distinct structures",
        sharing.structures()
    );

    // Derived here, where the walk that establishes this image runs, so per-compile rechecking reads the bound instead of re-deriving it over every archived term.
    let binder_floor = derived_binder_floor(&core);

    Unit::new(prepared, core, ersd, binder_floor)
}

/// Serialize one unit to `<root>.rkyv`, twice, and refuse a serializer that does not agree with itself.
///
/// The image carries no version and is no stable interchange format: Cargo regenerates it whenever its inputs change — the sources, this script, or any crate whose representation it serializes — so two incompatible images can never meet, and a schema beside the bytes could only ever compare a build against itself.
fn write_image(artifacts: &std::path::Path, root: &str, image: &Unit) {
    let first = curios_archive::to_bytes(image)
        .unwrap_or_else(|error| panic!("/{root} archive serialization failed: {error}"));
    let second = curios_archive::to_bytes(image)
        .unwrap_or_else(|error| panic!("/{root} archive repeat serialization failed: {error}"));
    assert_eq!(&*first, &*second, "/{root} archive is not deterministic");

    let path = artifacts.join(format!("{root}.rkyv"));
    println!("/{root} archived to {} bytes", first.len());
    fs::write(&path, &*first)
        .unwrap_or_else(|error| panic!("failed to write {}: {error}", path.display()));
}

/// Check every registered syntax target against the lowered prelude, over the **union** of its roots.
///
/// The union rather than each root in turn, because the registry spans them: `/sys` holds the intrinsic preconditions and `/std` everything the compiler emits by name, so either half alone would report the other's slots as absent.
fn validate_syntax_targets(modules: &[&curios_core::Module]) {
    let names = modules
        .iter()
        .flat_map(|module| module.items.iter())
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
        let concept = modules
            .iter()
            .find_map(|module| {
                module
                    .concepts
                    .get(&Global::Authored(target.concept.qualifier()))
            })
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
