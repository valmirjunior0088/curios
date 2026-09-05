#[path = "src/archive.rs"]
mod archive;
use archive::*;

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
        Context, ErasedArena, Mode, Resumed, elaborate_and_zonk_module, erase_unit,
        validate_lowered_universe_seeds, validate_universes,
    },
    curios_text::prepare_prelude,
    std::{collections::BTreeSet, env, fs, path::PathBuf},
};

/// What the standard library's landing page says it is — the one description no manifest supplies.
const STD_DESCRIPTION: &str = "The standard library: what every Curios program gets for free, compiled into the fixed prelude beside the syntax forms and the host's operations.";

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
    println!("cargo:rerun-if-changed=src/sources.rs");
    println!("cargo:rerun-if-changed=src/syntax.rs");

    let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    // The two directories rather than each discovered file: Cargo scans a watched directory recursively, so a *newly added* source triggers the rerun a per-file directive cannot — it matches no directive from the run that predates it. The two indexes sit beside the directories, not inside them, so they are named on their own.
    for watched in ["syn.crs", "std.crs", "syn", "std"] {
        println!(
            "cargo:rerun-if-changed={}",
            manifest.join(watched).display()
        );
    }
    // The standard library is the one prelude mount a program reaches for by name, so it is the one the image documents; `/syn` is the syntax forms' concepts and `/sys` the host's rows, neither an interface anybody reads for.
    let modules = authored_prelude(&manifest).documented("std", Some(STD_DESCRIPTION));

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
            error.format_with(&lowered, &[], &SYNTAX)
        )
    });

    // Every universe invariant the archive is trusted to satisfy is asserted here, on the value about to be serialized, and nowhere else. Restoration reads exactly the bytes written from this value — a constant of the same build, whose structure bytecheck confirms — so re-deriving the invariants per compilation only re-answers a question already settled. `erase_unit` below happens to project through the same check, but inheriting the guarantee from an unrelated call is not the same as stating it.
    validate_universes(&core)
        .unwrap_or_else(|error| panic!("elaborated fixed prelude universes are invalid: {error}"));

    // This is the seam a unit is stored at, so it is where the rule about what a stored unit may carry is enforced: no identity meaningful only in the compilation that assigned it. Two calls rather than one, because the universe half of that rule is what the line above already refuses — an unsolved universe metavariable, named as such — and a rule stated twice is a copy rather than a second opinion.
    validate_stored_identities(&core).unwrap_or_else(|found| {
        panic!("elaborated fixed prelude carries a positional identity: {found}")
    });

    // The archive's own zonk evidence, taken where the module is final: erasure below consumes it, and it is the same claim `zonk_module` just enforced, restated as a checked value rather than inherited.
    let zonked = Zonked::project(&core)
        .unwrap_or_else(|refusal| panic!("elaborated fixed prelude is not zonked: {refusal}"));

    // No entrypoint, so nothing to seal: this unit's arena stays open, which is what its successors resume over.
    let mut ersd = erase_unit(
        &mut Context::with_default_budget(SYNTAX),
        Resumed::of(&[], ErasedArena::default()),
        &zonked,
        None,
    )
    .unwrap_or_else(|error| {
        panic!(
            "fixed prelude failed to erase into the erased prefix: {}",
            error.format_with(&core, &[], &SYNTAX)
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
    // Plain stdout, not `cargo:warning=`: this is a metric nobody acts on during a build, and a line that shouts on every build of every consumer teaches readers to skim `warning:` — which is the habit that loses a real one later. Cargo captures it to `target/<profile>/build/<pkg>-<hash>/output`, where `-vv` or a reader who went looking will find it.
    println!(
        "fixed prelude hash-consed to {} distinct structures",
        sharing.structures()
    );

    // Derived here, where the walk that establishes this image runs, so per-compile rechecking reads the bound instead of re-deriving it over every archived term.
    let binder_floor = derived_binder_floor(&core);
    let image = PreludeArchive {
        prepared,
        core,
        binder_floor,
        ersd,
    };
    let first =
        curios_archive::to_bytes(&image).expect("fixed prelude archive serialization failed");
    let second = curios_archive::to_bytes(&image)
        .expect("fixed prelude archive repeat serialization failed");
    assert_eq!(
        &*first, &*second,
        "fixed prelude archive is not deterministic"
    );

    // Filed beside this crate rather than under `OUT_DIR`, because the image is read outside the build: `curios document` renders the standard library's pages from it, so it needs a path a recipe can name. The crate includes it from the same path, so there is one image in one place; the rule for a product that outlives its build is `.artifacts/`, which `cargo clean` leaves alone and `cargo x clean` removes.
    let artifacts = manifest.join(".artifacts");
    fs::create_dir_all(&artifacts).expect("failed to create the archive's .artifacts directory");
    fs::write(artifacts.join("prelude.rkyv"), &*first)
        .expect("failed to write fixed prelude archive");
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
