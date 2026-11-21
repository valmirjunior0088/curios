//! The fixed prelude's build: `/sys` and `/std` lowered, elaborated, certified and erased through the certifier that certified them, each imaged as the certified unit the compiler restores.
//!
//! Two programs run it. `curios-prelude`'s build script is the one Cargo runs, into that crate's `OUT_DIR`; this crate's `profile` runs the same work under a recorder for whoever asks, into a directory of its own.

use {
    crate::{std_source, sys_source},
    curios_cert::{Accepted, Globals, Rechecked, certify_definitions},
    curios_common::{Qualifier, Report, Source, digest},
    curios_core::{
        Anchors, Definitions, Global, Item, Sharing, method_labels, validate_stored_identities,
    },
    curios_document::Documentation,
    curios_elab::{
        Context, DEFAULT_BUDGET, Established, Reading, elaborate_and_zonk_unit,
        validate_lowered_universe_seeds, validate_universes,
    },
    curios_erase::{ErasedUnit, Erasures, erase_unit},
    curios_module::{PreparedText, RootSource, SYNTAX, UnitSource, document_unit, prepare_prelude},
    curios_surface::Spelling,
    curios_unit::{Indexed, Record, Unit, framed, packed, recorded},
    std::{
        collections::BTreeSet,
        fs,
        path::{Path, PathBuf},
        sync::Arc,
    },
};

/// Compile the prelude's two roots and file their images, with the documentation's records, in `out`. Answers each source file the lowering of `/std` read: what a build that ran this is run again for.
pub fn compile(out: &Path) -> Vec<PathBuf> {
    let sys_modules = sys_source();
    let std_modules = std_source();

    // Both roots lowered before either is elaborated, because the registry spans them: a target check over `/sys` alone would miss every `/std` slot and one over `/std` alone every `/sys` slot, so the check runs once over the union and there is no half to pass by being asked the wrong question.
    let sys_text = lower("sys", &sys_modules, &[]);
    let std_text = lower("std", &std_modules, &[&sys_text]);
    validate_syntax_targets(&[sys_text.core(), std_text.core()]);

    // The fold: `/sys` against nothing, `/std` against what `/sys` established. Each root is certified as it is made and its image framed as every unit the cabinet keeps is, holding the certified unit, so the prelude the crate restores is a fold of two units and not a shape of its own.
    let sys = unit("sys", sys_text, &Established::nothing(), &[]);
    let std = unit(
        "std",
        std_text,
        // Every root before this one is the compiler's own, of which no read is recorded.
        &Established::placed(&[(sys.core(), sys.places(), sys.mounts(), sys.traces())])
            .fixed(&[true]),
        &[&sys],
    );

    // The records `document --std` renders, made once each root is checked, since a page says which declarations are proofs and which are proven total, and filed as an image of their own: nothing that compiles reads one, so neither unit holds one. `/sys` first, because `/std` adopts its intrinsic declarations out of that record.
    let sys_record = documented("sys", &sys_modules, &sys, &[], &[]);
    let std_record = documented("std", &std_modules, &std, &[&sys], &[&sys_record]);

    // Each image carries the record an object carries: what the root was compiled from, by the read log the lowering kept, and what came before it. `/sys` is supplied whole and reads nothing; `/std` reads its tree and follows `/sys`, whose image is what its record's one predecessor digests.
    let sys_digest = write_image(out, "sys", &sys, sys_modules.reads(), Vec::new());
    let sys_prefix = Qualifier::from(["sys"]).join();
    write_image(
        out,
        "std",
        &std,
        std_modules.reads(),
        vec![(sys_prefix, sys_digest)],
    );
    write_documentation(out, &vec![sys_record, std_record]);

    std_modules
        .reads()
        .into_iter()
        .map(|(path, _)| path)
        .collect()
}

/// Resolve and lower one prelude root against the roots already lowered, with every invariant the lowered form is trusted to satisfy asserted here.
fn lower(root: &str, modules: &RootSource, predecessors: &[&PreparedText]) -> PreparedText {
    let prepared = prepare_prelude(modules, predecessors, &SYNTAX)
        .unwrap_or_else(|error| panic!("/{root} failed to lower: {}", error.format()));
    validate_lowered_universe_seeds(prepared.core(), prepared.minted()).unwrap_or_else(|error| {
        panic!("lowered Text universe seeds of /{root} are invalid: {error}")
    });

    prepared
}

/// One prelude root's documentation record, from the source it was read from and the unit it was checked as, over the roots before it and their records.
fn documented(
    root: &str,
    modules: &RootSource,
    unit: &Unit,
    predecessors: &[&Unit],
    records: &[&Documentation],
) -> Documentation {
    let predecessors = predecessors
        .iter()
        .map(|unit| lowered(root, unit))
        .collect::<Vec<_>>();
    document_unit(
        &UnitSource::mounted(modules),
        lowered(root, unit),
        &predecessors,
        records,
        unit.core(),
        unit.certification(),
    )
    .unwrap_or_else(|error| panic!("/{root} failed to document: {}", error.format()))
    .unwrap_or_else(|| panic!("/{root} marks no mount as documented"))
}

/// The lowering the unit of a prelude root keeps: each is lowered from text.
fn lowered<'a>(root: &str, unit: &'a Unit) -> &'a PreparedText {
    unit.text()
        .unwrap_or_else(|| panic!("/{root} is documented from a root lowered from text"))
}

/// What a refusal says of `module`, a root's lowered or elaborated module, as `format` renders it, spelled against the roots before it: each one's lexicon is made here, where a root is refused, since a build that refuses stops.
fn reported(
    format: impl FnOnce(&Reading<'_>) -> String,
    definitions: &Definitions,
    predecessors: &[&Definitions],
    anchors: &Anchors,
) -> String {
    let lexicons = predecessors
        .iter()
        .map(|unit| unit.lexicon())
        .collect::<Vec<_>>();

    format(&Reading {
        definitions,
        kept: None,
        scope: &lexicons.iter().collect::<Vec<_>>(),
        syntax: &SYNTAX,
        spellings: None,
        minted: None,
        anchors: &[anchors],
    })
}

/// Elaborate, validate, hash-cons, certify and erase one lowered root into the unit its image is, against what the roots before it established.
///
/// `predecessors` are the roots before it, made: the certifier judges it against their modules, with the records their walks left, and an item of this root reads in their erased forms how a name of an earlier one is bound, its own erased form holding nothing of theirs.
fn unit(
    root: &str,
    prepared: PreparedText,
    established: &Established<'_>,
    predecessors: &[&Unit],
) -> Unit {
    let predecessors_core = predecessors
        .iter()
        .map(|unit| unit.core())
        .collect::<Vec<_>>();
    let lowered = prepared.core().clone();
    let mut context = Context::with_default_budget(SYNTAX);
    // An item the parser could not read is absent from `lowered`, so what names it is withheld as a refused item's dependent is, rather than reported unbound once per mention: the seeding the compile pipeline does before it elaborates a unit.
    context.set_broken(prepared.broken_names());
    let elaborated = elaborate_and_zonk_unit(
        &mut context,
        established,
        &lowered,
        prepared.mounts(),
        prepared.minted(),
    );
    // A broken item refuses the root whatever elaboration said of the rest, and its parse report comes first, with elaboration's beside it in the same build — the pipeline's `with_broken`, for the one unit it does not compile.
    let broken = prepared.broken();
    if !broken.is_empty() {
        let parsed = Report::render_all(
            &broken
                .iter()
                .map(|item| item.report.clone())
                .collect::<Vec<_>>(),
        );
        let elaboration = match &elaborated {
            Ok(_) => String::new(),
            Err(error) => reported(
                |reading| error.format(reading),
                &lowered,
                &predecessors_core,
                prepared.anchors(),
            ),
        };
        panic!("/{root} failed to parse: {parsed}{elaboration}");
    }
    let (core, traces) = elaborated.unwrap_or_else(|error| {
        panic!(
            "/{root} failed to elaborate: {}",
            reported(
                |reading| error.format(reading),
                &lowered,
                &predecessors_core,
                prepared.anchors(),
            )
        )
    });
    // What the elaboration observed of the sites no declaration's record holds, as the pipeline hands a unit it makes: the one caller that elaborates a unit itself.
    let observed = context.observed();

    // Every universe invariant the archive is trusted to satisfy is asserted here, on the value about to be serialized, and nowhere else. Restoration reads exactly the bytes written from this value — a constant of the same build, whose structure bytecheck confirms — so re-deriving the invariants per compilation only re-answers a question already settled.
    validate_universes(&core)
        .unwrap_or_else(|error| panic!("elaborated /{root} universes are invalid: {error}"));

    // This is the seam a unit is stored at, so it is where the rule about what a stored unit may carry is enforced: no identity meaningful only in the compilation that assigned it. Two calls rather than one, because the universe half of that rule is what the line above already refuses — an unsolved universe metavariable, named as such — and a rule stated twice is a copy rather than a second opinion.
    validate_stored_identities(&core, prepared.mounts()).unwrap_or_else(|found| {
        panic!("elaborated /{root} carries a positional identity: {found}")
    });

    // Hash-cons the elaborated module, so each structure it spells is one allocation. Elaboration builds the same types, telescopes, and proof spines independently in definition after definition and nothing deduplicates them, because `Rc` sharing only ever arises from cloning: two definitions that build the same type build it twice. rkyv shares by pointer address, so collapsing them here is also what lets the archive store each distinct structure once.
    //
    // The lowered module is left as it was built: a consed node sits under no position and a lint or a report is located in the lowered module, so there is nothing of it a table could share and keep.
    //
    // One table per image, not one across both: rkyv shares by pointer address *within* a single image, so a structure collapsed across the two would still be written into each of them — a shared table would buy nothing and would report a distinct-structure count no image actually has.
    //
    // `anf` is deliberately not included: an erased form is flat, index-addressed arenas with no shared pointers to collapse, and it already interns its constants by value.
    let sharing = Sharing::new();
    let core = core.shared(&sharing);
    // Plain stdout, not `cargo:warning=`: this is a metric nobody acts on during a build, and a line that shouts on every build of every consumer teaches readers to skim `warning:` — which is the habit that loses a real one later. Cargo captures it to `target/<profile>/build/<pkg>-<hash>/output`, where `-vv` or a reader who went looking will find it.
    println!(
        "/{root} hash-consed to {} distinct structures",
        sharing.structures()
    );

    let (accepted, anf) = certified(root, &core, predecessors, prepared.anchors());
    // What the certifier stated rebuilt the nodes whose marks it changed, which are consed with the rest: the table holds every node the module shared, so a node the certifier handed back as it was costs nothing.
    let accepted = accepted.shared(&sharing);

    Unit::new(prepared, accepted, anf, traces, &observed)
}

/// The certifier's walk over one root's module as it is imaged, against the roots before it with the records their walks left, and the root erased through the certifier that walked it: the module as the certifier stated it with the record of its definitions, and its items' erased forms, or a panic at the first refusal, since a prelude the certifier does not accept or erasure cannot erase is no prelude.
///
/// The module judged is the hash-consed one the image holds, and certified before it is imaged as a cabinet's unit is: what a compilation restores is these bytes, which the image's own determinism check holds to one serialization.
fn certified(
    root: &str,
    core: &Definitions,
    predecessors: &[&Unit],
    anchors: &Anchors,
) -> (Accepted<Definitions>, ErasedUnit) {
    // The roots before this one rather than `Globals::default()`: `/std` names `/sys`, so judging it against an empty environment would refuse every intrinsic carrier it wraps. Each with the record its walk made, so `/std`'s walk reads the certifier's classification of `/sys` rather than classifying it again. The assembler `curios-pipeline` offers is unreachable from here — a build script that reached the compiler boundary would pull the whole pipeline into the prelude's own build — so the environment is mounted by hand.
    let mut globals = Globals::default();
    for earlier in predecessors {
        globals.mount(earlier.filed());
    }
    // Each is the compiler's own, of which no read is recorded.
    globals.standing(
        &vec![true; predecessors.len()],
        &vec![false; predecessors.len()],
    );
    let (rechecked, mut certifier) = certify_definitions(core, DEFAULT_BUDGET, &globals, SYNTAX);

    // The root is the module the certifier stated, so every mark it carries is the walk's own.
    let stated = match rechecked {
        Rechecked::Accepted(stated) => stated,
        Rechecked::Refused(verdicts) => {
            let verdict = &verdicts[0];
            // The name, not only the error: a `curios_cert::Error` renders terms and sorts and never the top-level item it came from, so without this the one diagnostic this build exists to produce points nowhere in a prelude of a thousand items.
            let name = match &verdict.name {
                Some(name) => name.faithful().to_string(),
                None => "<entrypoint>".to_owned(),
            };

            panic!(
                // A refusal count over a corpus size, not a subset of it: the walk pushes a verdict per *failing pass*, and its passes run over declarations and over each definition of a `rec` group, so the count can exceed the item count outright.
                "/{root} failed the certifier: {} refusals over {} items, first: {name} — {}",
                verdicts.len(),
                core.items.len(),
                verdict.error.rendered(&Spelling::default()),
            );
        }
    };
    // Plain stdout rather than `cargo:warning=`, as for the hash-consing metric — and because the verdict is not this line. The panic above is: a refusal fails the build, so a build that finished has certified. What this adds is the count, which changes only when the prelude does.
    println!(
        "/{root} certified: {} items accepted by the certifier",
        core.items.len()
    );

    // No entrypoint, so nothing to link: each item's erased form is its own, which is what a successor reads and a program links.
    let cores = predecessors
        .iter()
        .map(|unit| unit.core())
        .collect::<Vec<_>>();
    let places = predecessors
        .iter()
        .map(|unit| unit.places())
        .collect::<Vec<_>>();
    let certifications = predecessors
        .iter()
        .map(|unit| unit.certification())
        .collect::<Vec<_>>();
    let erased = predecessors
        .iter()
        .map(|unit| unit.erased())
        .collect::<Vec<_>>();
    let anf = erase_unit(
        &mut certifier,
        Erasures::of(&cores, &places, &certifications, &erased)
            .fixed(&vec![true; predecessors.len()]),
        &stated,
    )
    .unwrap_or_else(|error| {
        panic!(
            "/{root} failed to erase: {}",
            reported(
                |reading| error.format(reading.spelling(), reading.anchors),
                core,
                &cores,
                anchors
            )
        )
    });

    (stated, anf)
}

/// Serialize one certified unit, as the fixed prelude images one ([`Unit::imaged`]), to `<root>.rkyv`, framed as a stored unit is — its record, of `reads` and of what each root before it contained, `after`, ahead of its index — and the blobs its index names to `<root>.pack`, one pack as a cabinet keeps beside an object's unit file, serializing the unit twice and refusing a serializer that does not agree with itself. Hands back the digest of the unit's index, which is what the next root's record names it by.
///
/// The image carries no version and is no stable interchange format: it is made again, or taken from the images the checkout keeps under the same key (`images.rs`), whenever what the build links changes — the sources, this script, or any crate whose representation it serializes — so two incompatible images can never meet, and a schema beside the bytes could only ever compare one state of the sources against itself.
///
/// The record is the compiler's own account of the tree the root was built from: each file by the name the package `std` gives it, with the digest of its text, the same bytes wherever the tree stood. A checkout claiming `/std` holds the tree the archive came from exactly when its files hold those texts.
fn write_image(
    directory: &Path,
    root: &str,
    image: &Unit,
    reads: Vec<(PathBuf, Arc<Source>)>,
    after: Vec<(String, String)>,
) -> String {
    let pack = |stored: &Indexed| {
        let blobs = stored
            .blobs(|_| true)
            .unwrap_or_else(|error| panic!("/{root} blobs serialization failed: {error}"));

        packed(&blobs)
    };
    let first = image
        .imaged()
        .unwrap_or_else(|error| panic!("/{root} archive serialization failed: {error}"));
    let second = image
        .imaged()
        .unwrap_or_else(|error| panic!("/{root} archive repeat serialization failed: {error}"));
    let blobs = pack(&first);
    assert!(
        first.index() == second.index() && blobs == pack(&second),
        "/{root} archive is not deterministic"
    );

    // The compiler's own library is no package's, and no manifest claims a declaration of it: its rows are the host's to answer.
    let reads = recorded(&reads).unwrap_or_else(|| panic!("/{root} reads a file for no package"));
    let record = Record::of(reads, after, Vec::new(), digest(first.index()));
    let recorded = curios_archive::to_bytes(&record)
        .unwrap_or_else(|error| panic!("/{root} record serialization failed: {error}"));

    let path = directory.join(format!("{root}.rkyv"));
    println!(
        "/{root} archived to {} bytes, its blobs to {}",
        first.index().len(),
        blobs.len()
    );
    fs::write(&path, framed(&recorded, first.index()))
        .unwrap_or_else(|error| panic!("failed to write {}: {error}", path.display()));
    let path = directory.join(format!("{root}.pack"));
    fs::write(&path, blobs)
        .unwrap_or_else(|error| panic!("failed to write {}: {error}", path.display()));

    record.unit
}

/// Serialize the roots' documentation records, in the fold's order, to `documentation.rkyv`, twice, refusing a serializer that does not agree with itself.
///
/// Unframed, where a root's image is framed as a unit the cabinet keeps is: a record is no unit, nothing compiles against one, and no object holds one.
fn write_documentation(directory: &Path, records: &Vec<Documentation>) {
    let first = curios_archive::to_bytes(records)
        .unwrap_or_else(|error| panic!("documentation serialization failed: {error}"));
    let second = curios_archive::to_bytes(records)
        .unwrap_or_else(|error| panic!("documentation repeat serialization failed: {error}"));
    assert_eq!(&*first, &*second, "the documentation is not deterministic");

    let path = directory.join("documentation.rkyv");
    println!("documentation archived to {} bytes", first.len());
    fs::write(&path, &first)
        .unwrap_or_else(|error| panic!("failed to write {}: {error}", path.display()));
}

/// Check every registered syntax target against the lowered prelude, over the **union** of its roots.
///
/// The union rather than each root in turn, because the registry spans them: `/sys` holds the intrinsic preconditions and `/std` everything the compiler emits by name, so either half alone would report the other's slots as absent.
fn validate_syntax_targets(modules: &[&curios_core::Definitions]) {
    let names = modules
        .iter()
        .flat_map(|definitions| definitions.items.iter())
        .flat_map(Item::declared_names)
        .collect::<BTreeSet<_>>();
    for target in SYNTAX.targets() {
        let symbol = target.symbol();
        assert!(
            names.contains(&Global::Authored(target.qualifier())),
            "registered syntax target '{symbol}' is absent from the lowered prelude; nearby names: {:?}",
            names
                .iter()
                .map(|name| name.faithful().to_string())
                .filter(|name| name.contains(target.last()))
                .collect::<Vec<_>>()
        );
    }

    // A concept can exist under the registered name and still not declare the method the elaborator projects, which the presence check above cannot see. `elaborate_infix` resolves that method *positionally* against the declaration, so a drifted label is not a compile error here but a panic on the first program that writes the operator.
    for target in SYNTAX.concept_fields() {
        let symbol = target.concept.symbol();
        let field = target.field;
        let name = Global::Authored(target.concept.qualifier());
        // A concept's methods are named by its record's fields.
        let record = modules
            .iter()
            .find_map(|definitions| {
                definitions.concepts().get(&name)?;
                definitions.struct_decls().get(&name).copied()
            })
            .unwrap_or_else(|| {
                panic!(
                    "registered operator concept '{symbol}' is not a concept in the lowered prelude"
                )
            });
        let declared = method_labels(record);
        assert!(
            declared.contains(&field),
            "registered operator concept '{symbol}' does not declare the method '{field}'; it declares: {declared:?}",
        );
    }
}
