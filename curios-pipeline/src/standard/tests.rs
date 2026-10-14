//! Which unit takes an archived root's place, what it is granted, and what it is offered; and the library compiled from its own terms as a report prints them.

use {
    super::{granted, withheld},
    crate::{
        Cache, DEFAULT_STEP_BUDGET, Earlier, Fold, compile_unit, compile_unit_over, recheck_over,
        tests::test_support::{assert_stored_alike, compile_with_units},
    },
    curios_core::{
        Anchors, Certification, Definition, DefinitionKind, Global, Imports, Item, Module, Nominal,
        ReaderPosition, Sharing, Term, Totality, Zonked, build_rename, display_names,
    },
    curios_elab::{
        Context, ErasedUnit, Error, Established, Reading, Recompile, Recompiled, Traces,
        elaborate_and_zonk_unit_over,
    },
    curios_ersd::FieldShape,
    curios_prelude::with_prelude,
    curios_std::std_directory,
    curios_text::{
        Label, LetSignature, Overlay, PreparedText, RootSource, SYNTAX, TopItem, TopLet, TopMod,
        Translation, UnitSource, into_core_unit, into_core_unit_over, spell,
    },
    curios_unit::{Predecessors, Unit, apart_parts, halves},
    curios_utilities::{Qualifier, RootKind, digest, test_support::Temporary},
    std::{
        cell::RefCell,
        cmp::Reverse,
        collections::{BTreeMap, BTreeSet},
        fs,
        path::{Path, PathBuf},
        rc::Rc,
        time::{Duration, Instant},
    },
};

/// The standard library's own tree, as the package claiming `/std` from it.
fn std_from_its_tree() -> RootSource {
    let directory = std_directory();

    RootSource::mounted(
        "std",
        RootKind::Ordinary,
        directory.join("lib.crs"),
        directory,
    )
    .declaring([Qualifier::from(["sys"])])
}

/// The same claim from a directory that is not the archive's tree — one nothing creates, so its guard has nothing to remove.
fn std_from_elsewhere() -> RootSource {
    let directory = Temporary::new("standard", "elsewhere");

    RootSource::mounted(
        "std",
        RootKind::Ordinary,
        directory.join("lib.crs"),
        directory.to_path_buf(),
    )
}

/// One edit the census measures: what it is, the file it touches, and the text it leaves there.
struct Edit {
    label: &'static str,
    file: &'static str,
    edit: fn(&str) -> String,
}

/// The two edits the standard library is measured over: a leaf, which nothing reads, and a hub, which most of the library reaches.
fn edits() -> [Edit; 2] {
    [
        Edit {
            label: "a leaf: a declaration added to /std/Nat",
            file: "Nat.crs",
            edit: |text| format!("{text}\npub let _census_probe(a: Nat) -> Nat =\n    a;\n"),
        },
        Edit {
            label: "a hub: the body of /std/Bool/not respelled",
            file: "Bool.crs",
            edit: |text| text.replacen("xor(b, true)", "xor(true, b)", 1),
        },
    ]
}

/// `lowered`, the library's sources lowered over the archived unit `std`, elaborated over it: every declaration of `kept` is offered to the walk as one the lowering kept, and the walk keeps each whose trace stands.
fn elaborated_over(
    predecessors: Predecessors<'_>,
    std: &Unit,
    lowered: &PreparedText,
    kept: &BTreeSet<Global>,
) -> Result<Recompiled, Error> {
    let established = Established::placed(&predecessors.placed()).keeping(std.core(), std.places());
    let mut context = Context::new(DEFAULT_STEP_BUDGET, SYNTAX);
    context.set_imports(lowered.imports().clone());
    context.set_broken(lowered.broken_names());

    elaborate_and_zonk_unit_over(
        &mut context,
        &established,
        Recompile {
            baseline: std.core(),
            places: std.places(),
            traces: std.traces(),
            lowered: kept,
        },
        lowered.core(),
        lowered.minted(),
    )
}

/// What a question about the standard library costs after an edit to one declaration: how many names are lowered again and how many elaborated again, and the time each phase takes, for a leaf and for a hub. A measurement, so it reports rather than asserts, and its timings are the profile it was built under.
#[test]
#[ignore = "measurement, mixed: lowers the standard library and recompiles it over the archive, reporting what is done again and per-phase timings"]
fn std_recompile_census() {
    let directory = std_directory();

    with_prelude(|prelude| {
        let [sys, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);

        println!("\n=== recompiling /std over the archive ===");
        for Edit { label, file, edit } in edits() {
            let path = directory.join(file);
            let text = edit(&fs::read_to_string(&path).expect("an authored source"));
            let source = std_from_its_tree().with_overlay(Overlay::of([(path, text)]));
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

            let start = Instant::now();
            let lowered = into_core_unit_over(&unit, &predecessors.text(), &SYNTAX, std.text())
                .expect("the edited library lowers");
            let lowered_in = start.elapsed();
            let kept = lowered.kept_from(std.text());
            let start = Instant::now();
            let elaborated = elaborated_over(predecessors, std, &lowered, &kept)
                .unwrap_or_else(|error| panic!("the edited library elaborates: {error}"));
            let elaborated_in = start.elapsed();
            let start = Instant::now();
            compile_unit_over(
                DEFAULT_STEP_BUDGET,
                predecessors,
                &SYNTAX,
                &unit,
                std,
                &[false],
            )
            .expect("the edited library recompiles");
            let recompiled_in = start.elapsed();

            let names = lowered
                .core()
                .items
                .iter()
                .flat_map(Item::declared_names)
                .count();
            println!("{label}");
            println!(
                "  lowered     {:>6} names again of {names}",
                names - kept.len()
            );
            println!(
                "  elaborated  {:>6} names again of {names}",
                names - elaborated.kept.len()
            );
            println!("  lower       {lowered_in:>10.1?}");
            println!("  elaborate   {elaborated_in:>10.1?}");
            println!("  recompile   {recompiled_in:>10.1?}   (lower, elaborate, judge, erase)");
        }
    });
}

/// Whether the standard library compiles to one unit: its tree compiled whole twice in one process, the two stored units compared byte for byte, and where they differ, the parts and the items that do. A measurement, since each compile is the whole library; counted, and it asserts, since a second unit is the fault it is here to name.
#[test]
#[ignore = "measurement, counted: compiles the standard library whole twice and compares the two stored units byte for byte"]
fn std_unit_reproduction() {
    with_prelude(|prelude| {
        let [sys, _] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);
        let compiled = || {
            let source = std_from_its_tree();
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

            compile_unit(DEFAULT_STEP_BUDGET, predecessors, &SYNTAX, &unit)
                .expect("the library compiles")
        };
        let (first, second) = (compiled(), compiled());

        macro_rules! stored {
            ($value:expr) => {
                curios_archive::to_bytes($value)
                    .expect("a stored part serializes")
                    .to_vec()
            };
        }
        let mut differing = Vec::new();
        if stored!(first.text()) != stored!(second.text()) {
            differing.push("the text stage's part".to_string());
        }
        if stored!(first.erased()) != stored!(second.erased()) {
            differing.push("the erased forms".to_string());
        }
        if stored!(first.certification()) != stored!(second.certification()) {
            differing.push("the certifier's record".to_string());
        }
        if stored!(first.traces()) != stored!(second.traces()) {
            differing.push("what its declarations asked".to_string());
        }
        let erasures = |unit: &Unit| {
            unit.erased()
                .baseline_bytes()
                .expect("an erased unit's traces serialize")
                .into_owned()
        };
        if erasures(&first) != erasures(&second) {
            differing.push("what its erasures asked".to_string());
        }
        for (one, other) in first.core().items.iter().zip(&second.core().items) {
            if stored!(one) != stored!(other) {
                differing.push(one.describe());
            }
        }

        println!("\n=== /std compiled whole twice ===");
        println!("  items       {}", first.core().items.len());
        println!("  differing   {differing:?}");
        assert!(
            first.stored().expect("a unit serializes")
                == second.stored().expect("a unit serializes")
                && stored!(&first) == stored!(&second),
            "two compilations of the standard library store two units, differing in {differing:?}"
        );
    });
}

/// Whether the standard library compiled over a baseline is the unit it is compiled whole: for a leaf edit and for a hub, its edited tree compiled whole and over the archived unit, the two compared as each is stored. A measurement, since each edit compiles the library whole; counted, and it asserts, since a second unit is the fault it is here to name.
#[test]
#[ignore = "measurement, counted: for a leaf edit and a hub, compiles the edited standard library whole and over the archive and compares the two stored units byte for byte"]
fn std_recompile_reproduction() {
    let directory = std_directory();

    with_prelude(|prelude| {
        let [sys, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);

        println!("\n=== /std compiled whole and over the archive ===");
        for Edit { label, file, edit } in edits() {
            let path = directory.join(file);
            let text = edit(&fs::read_to_string(&path).expect("an authored source"));
            let source = std_from_its_tree().with_overlay(Overlay::of([(path, text)]));
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

            let whole = compile_unit(DEFAULT_STEP_BUDGET, predecessors, &SYNTAX, &unit)
                .expect("the edited library compiles");
            let over = compile_unit_over(
                DEFAULT_STEP_BUDGET,
                predecessors,
                &SYNTAX,
                &unit,
                std,
                &[false],
            )
            .expect("the edited library recompiles");

            println!("{label}");
            assert_stored_alike(&whole, &over);
            println!(
                "  stored      {:>10} bytes either way",
                whole.stored().expect("a unit serializes").len()
            );
        }
    });
}

/// How the standard library's stored unit divides between its parts: the bytes of each archived alone, and how long each takes to restore out of them. What only a recompile of the unit reads is its baseline half, and the rest is what every compilation after the unit restores. A measurement, so it reports rather than asserts; mixed, its bytes counted and its times the median of five readings with their span, the profile it was built under.
#[test]
#[ignore = "measurement, mixed: archives each part of the standard library's unit alone and reports its bytes and how long it takes to restore"]
fn std_image_division() {
    with_prelude(|prelude| {
        let [_, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let row = |part: &str, bytes: usize, times: Option<[Duration; 3]>| match times {
            Some([least, median, most]) => println!(
                "  {part:<42}{bytes:>12} bytes   {median:>9.1?}  ({least:.1?} to {most:.1?})"
            ),
            None => println!("  {part:<42}{bytes:>12} bytes"),
        };
        // The least, the median and the most of five restorations of `$bytes` as a `$type`.
        macro_rules! restored {
            ($type:ty, $bytes:expr) => {{
                let mut readings = [Duration::ZERO; 5];
                for reading in &mut readings {
                    let start = Instant::now();
                    let value =
                        curios_archive::from_bytes::<$type>($bytes).expect("a part restores");
                    *reading = start.elapsed();
                    drop(value);
                }
                readings.sort();

                [readings[0], readings[2], readings[4]]
            }};
        }
        // What `$module`'s items are of it: archived as it holds them, and each alone, which stores a structure two of them share once for each.
        macro_rules! items {
            ($name:expr, $module:expr) => {{
                let module: &Module = $module;
                let whole = curios_archive::to_bytes(&module.items.to_vec())
                    .expect("a module's items serialize");
                let alone = module
                    .items
                    .iter()
                    .map(|item| {
                        curios_archive::to_bytes(item)
                            .expect("an item serializes")
                            .len()
                    })
                    .sum::<usize>();
                row($name, whole.len(), None);
                row(concat!($name, ", each archived alone"), alone, None);
            }};
        }
        macro_rules! part {
            ($name:expr, $type:ty, $value:expr) => {{
                let bytes = curios_archive::to_bytes($value).expect("a part serializes");
                let length = bytes.len();
                row($name, length, Some(restored!($type, &bytes)));

                length
            }};
        }

        let stored = std.stored().expect("the library's unit serializes");
        let (unit, apart) = halves(&stored).expect("a unit's artifact");
        let [apart, reads]: [curios_archive::Bytes; 2] =
            curios_archive::parted(&curios_archive::Bytes::from(apart.to_vec()))
                .and_then(|parts| parts.try_into().ok())
                .expect("what a certified unit stores apart");
        let reads = reads.as_slice().len();
        let [
            lowered,
            forms,
            module,
            anchors,
            imports,
            records,
            asked,
            erasures,
        ] = apart_parts(&apart)
            .expect("what a unit stores apart")
            .map(|part| part.as_slice().len());

        println!("\n=== how /std's stored unit divides ===");
        row("the artifact", stored.len(), None);
        row(
            "what a successor restores",
            unit.len(),
            Some(restored!(Unit, unit)),
        );
        part!("  the lowering's state", PreparedText, std.text());
        let consed = std.core().shared(&Sharing::new());
        part!("  the elaborated module, consed", Module, &consed);
        items!("    its items", &consed);
        part!("  its erased forms' index", ErasedUnit, std.erased());
        part!(
            "  the certifier's record",
            Certification,
            std.certification()
        );

        row(
            "what a reader of a declaration restores",
            lowered + forms + module,
            None,
        );
        row("  its lowered items, each stored alone", lowered, None);
        row("  its erased forms, each stored alone", forms, None);
        row("  its elaborated module, read by item", module, None);
        row("what a report restores", anchors, None);
        part!(
            "  where its declarations lie",
            Anchors,
            std.text().anchors()
        );
        row(
            "what a recompile alone restores",
            imports + records + asked + erasures + reads,
            None,
        );
        part!("  its import scopes", Imports, std.text().imports());
        row("  what its verdicts stand on", reads, None);
        row(
            "  the lowering's records and scans",
            std.text()
                .baseline_bytes()
                .expect("the lowering's half serializes")
                .len(),
            None,
        );
        part!("  what its declarations asked", Traces, std.traces());
        row(
            "  what its erasures asked",
            std.erased()
                .baseline_bytes()
                .expect("erasure's half serializes")
                .len(),
            None,
        );

        let together = Sharing::new();
        std.core().shared(&together);
        let alone = std
            .core()
            .items
            .iter()
            .map(|item| {
                let sharing = Sharing::new();
                let module = Module {
                    items: vec![item.clone()].into(),
                    ..Module::default()
                };
                module.shared(&sharing);

                sharing.structures()
            })
            .sum::<usize>();
        println!(
            "  structures  {} with the module consed in one table, {} with each item in its own",
            together.structures(),
            alone
        );

        // The least, the median and the most of five runs of `$step`.
        macro_rules! timed {
            ($name:expr, $step:expr) => {{
                let mut readings = [Duration::ZERO; 5];
                for reading in &mut readings {
                    let start = Instant::now();
                    let value = $step;
                    *reading = start.elapsed();
                    drop(value);
                }
                readings.sort();
                println!(
                    "  {:<42}{:>9.1?}  ({:.1?} to {:.1?})",
                    $name, readings[2], readings[0], readings[4]
                );
            }};
        }
        println!("  a filing's steps");
        timed!(
            "    consing the elaborated module",
            std.core().shared(&Sharing::new())
        );
        timed!(
            "    serializing the elaborated module alone",
            curios_archive::to_bytes(&consed).expect("a module serializes")
        );
        timed!(
            "    serializing the unit, consed",
            std.stored().expect("a unit serializes")
        );
        timed!("    digesting the artifact", digest(&stored));
        timed!(
            "    restoring the artifact",
            Unit::restored(&stored).expect("a unit restores")
        );
    });
}

/// Whether a declaration the prover's vocabulary comes to read is elaborated again with no lemma of the library on offer, as a whole compile elaborates it. `/std/Eq/sym`, a lemma of the vocabulary, is made to name `/std/Str/At/skip`, whose proofs the prover writes: compiled whole, `skip` is then asked for before the lemmas are offered. Compiled over the archive, `skip`'s text stands and its trace says the lemmas were on offer, so the walk elaborates it again where it now stands. The two compiles answer alike: with one stored unit, or with the same reports. A measurement, since each compile is the library's; it asserts, the two answers being one claim.
#[test]
#[ignore = "measurement, counted: makes a lemma of the prover's vocabulary read a declaration, compiles the standard library whole and over the archive, and compares the two answers"]
fn std_vocabulary_reach_reproduction() {
    let directory = std_directory();

    with_prelude(|prelude| {
        let [sys, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);
        let path = directory.join("Eq.crs");
        let written = fs::read_to_string(&path).expect("an authored source");
        let text = written.replacen(
            "    match p | refl() => Eq/refl() end;",
            "    let _ = /std/Str/At/skip;\n    match p | refl() => Eq/refl() end;",
            1,
        );
        assert_ne!(text, written, "the lemma is written as the edit expects");
        let source = std_from_its_tree().with_overlay(Overlay::of([(path, text)]));
        let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

        let whole = compile_unit(DEFAULT_STEP_BUDGET, predecessors, &SYNTAX, &unit);
        let over = compile_unit_over(
            DEFAULT_STEP_BUDGET,
            predecessors,
            &SYNTAX,
            &unit,
            std,
            &[false],
        );

        println!("\n=== /std with a lemma of the vocabulary reading /std/Str/At/skip ===");
        match (whole, over) {
            (Ok(whole), Ok(over)) => {
                println!("  compiles either way");
                assert_stored_alike(&whole, &over);
            }
            (Err(whole), Err(over)) => {
                let (whole, over) = (String::from(whole), String::from(over));
                println!(
                    "  refused either way: {}",
                    whole.lines().next().unwrap_or_default()
                );
                assert_eq!(
                    whole, over,
                    "a compile over the archive refuses the library in other words than a whole compile"
                );
            }
            (whole, over) => panic!(
                "a whole compile {} and a compile over the archive {}",
                if whole.is_ok() {
                    "compiles"
                } else {
                    "is refused"
                },
                if over.is_ok() {
                    "compiles"
                } else {
                    "is refused"
                },
            ),
        }
    });
}

/// The kind an item was introduced as: a group's is its first member's.
fn introduced(item: &Item) -> &DefinitionKind {
    match item {
        Item::Let(definition) => &definition.kind,
        Item::Rec(rec) => &rec.definitions[0].kind,
    }
}

/// The names of each declaration of `module`, in item order: an item's, with those of every item generated from it — a type former's constructors, a concept's method wrappers — which no elaboration takes apart from it.
fn declarations(module: &Module) -> Vec<BTreeSet<Global>> {
    let mut generated: BTreeMap<Global, BTreeSet<Global>> = BTreeMap::new();
    let mut declarations = Vec::new();
    for item in &module.items {
        let names = item.declared_names().into_iter().copied();
        match introduced(item) {
            DefinitionKind::InductiveConstructor { owner, .. }
            | DefinitionKind::ConceptMethod { owner } => generated
                .entry(Global::Authored(*owner))
                .or_default()
                .extend(names),
            _ => declarations.push(names.collect::<BTreeSet<_>>()),
        }
    }
    for declaration in &mut declarations {
        let members = declaration
            .iter()
            .filter_map(|name| generated.get(name))
            .flatten()
            .copied()
            .collect::<Vec<_>>();
        declaration.extend(members);
    }

    declarations
}

/// What differs between the items of `whole` declaring `names` and `replayed`'s, each as the definition and the part that differs.
fn differences(whole: &Module, replayed: &Module, names: &BTreeSet<Global>) -> Vec<String> {
    let first = |item: &Item| item.declared_names().first().map(|name| **name);
    let mut differences = Vec::new();
    for item in &whole.items {
        if !item
            .declared_names()
            .iter()
            .all(|name| names.contains(name))
        {
            continue;
        }
        let Some(other) = replayed
            .items
            .iter()
            .find(|other| first(other) == first(item))
        else {
            differences.push(format!("{}: no item", item.describe()));
            continue;
        };
        for (this, that) in item.definitions().iter().zip(other.definitions()) {
            let mut parts = Vec::new();
            if this.universe_context != that.universe_context {
                parts.push(format!(
                    "{} universe parameters for {}",
                    that.universe_context.parameter_count, this.universe_context.parameter_count
                ));
            }
            if this.type_ != that.type_ {
                parts.push("its type".to_string());
            }
            if this.body != that.body {
                parts.push("its body".to_string());
            }
            if this.totality != that.totality {
                parts.push("its totality".to_string());
            }
            if !parts.is_empty() {
                differences.push(format!("{}: {}", this.name.faithful(), parts.join(", ")));
            }
        }
    }
    for name in names {
        if whole.induct_decls().get(name) != replayed.induct_decls().get(name)
            || whole.struct_decls().get(name) != replayed.struct_decls().get(name)
            || whole.concepts().get(name) != replayed.concepts().get(name)
        {
            differences.push(format!("{}: its registry entry", name.faithful()));
        }
    }

    differences
}

/// How many unrelated definitions the replay elaborates ahead of a declaration, one pass each.
const PADDINGS: [usize; 2] = [0, 1];

/// What each declaration of the standard library elaborates to where it alone is elaborated again: the library as its sources lower it, compiled over the archived unit with every declaration but one offered as kept, so the walk elaborates that one and keeps the others by their traces. The declaration is compared with what the archive holds for it, and so is what it asked; what else the walk elaborated again is counted. And again with unrelated authored definitions elaborated beside it, which nothing it reads distinguishes. A declaration is a function of what it reads exactly when this reports nothing (`documentation/design/compilation/a-declaration-is-a-function-of-what-it-reads.md`). A measurement, so it reports rather than asserts.
#[test]
#[ignore = "measurement, counted: lowers the standard library and compiles it over the archive once for each of its declarations, that declaration alone elaborated again, reporting the ones that come out different"]
fn std_replay_census() {
    with_prelude(|prelude| {
        let [sys, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);
        let source = std_from_its_tree();
        let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);
        let lowered =
            into_core_unit(&unit, &predecessors.text(), &SYNTAX).expect("the library lowers");
        let (whole, lowered_core) = (std.core(), lowered.core());

        let position = lowered_core
            .items
            .iter()
            .enumerate()
            .flat_map(|(index, item)| {
                item.declared_names()
                    .into_iter()
                    .map(move |name| (*name, index))
            })
            .collect::<BTreeMap<_, _>>();
        let every = position.keys().copied().collect::<BTreeSet<_>>();

        println!("\n=== elaborating each declaration of /std again, alone, over the archive ===");
        let declarations = declarations(lowered_core);
        for padding in PADDINGS {
            let (mut refused, mut differing, mut retraced, mut beside) = (0, 0, 0, 0);
            for names in &declarations {
                let first = names
                    .iter()
                    .filter_map(|name| position.get(name))
                    .min()
                    .copied()
                    .expect("a declaration declares a lowered name");
                // The item the declaration is written as, wherever the lowering puts the items generated from it: a concept's method wrappers leave the lowering ahead of the concept.
                let own = names
                    .iter()
                    .filter_map(|name| position.get(name))
                    .map(|place| &lowered_core.items[*place])
                    .find(|item| {
                        !matches!(
                            introduced(item),
                            DefinitionKind::InductiveConstructor { .. }
                                | DefinitionKind::ConceptMethod { .. }
                        )
                    })
                    .expect("a declaration holds the item it is written as");
                let written = *own.declared_names()[0];
                let mut again = names.clone();
                let reach = lowered_core.reaches(own);
                again.extend(
                    lowered_core
                        .items
                        .iter()
                        .take(first)
                        .rev()
                        .filter_map(|item| match item {
                            Item::Let(definition)
                                if matches!(definition.kind, DefinitionKind::Authored)
                                    && !reach.contains(&definition.name) =>
                            {
                                Some(definition.name)
                            }
                            _ => None,
                        })
                        .take(padding),
                );

                let kept = every.difference(&again).copied().collect::<BTreeSet<_>>();
                match elaborated_over(predecessors, std, &lowered, &kept) {
                    Err(error) => {
                        refused += 1;
                        let refusal = error.to_string().replace('\n', " | ");
                        println!(
                            "  beside {padding}: {} is refused: {}",
                            own.describe(),
                            refusal.chars().take(600).collect::<String>()
                        );
                    }
                    Ok(replayed) => {
                        let differences = differences(whole, &replayed.module, names);
                        differing += usize::from(!differences.is_empty());
                        for difference in differences {
                            println!("  beside {padding}: {difference}");
                        }
                        if replayed.traces.of(&written) != std.traces().of(&written) {
                            retraced += 1;
                            println!("  beside {padding}: {}: what it asked", own.describe());
                        }
                        // What the walk elaborated again beyond what it was handed: a declaration that asked a part which came out another way.
                        let others = every.len() - replayed.kept.len() - again.len();
                        if others != 0 {
                            beside += 1;
                            println!(
                                "  beside {padding}: {}: {others} other names elaborated again",
                                own.describe()
                            );
                        }
                    }
                }
            }
            println!(
                "beside {padding} unrelated definitions: {differing} of {} declarations differ, {refused} are refused, {retraced} ask otherwise, {beside} take another declaration with them",
                declarations.len()
            );
        }
    });
}

/// What a type former of the prelude holds that its entry does not spell: each former of `/sys` and `/std` spelled again from the entry it carries ([`Definition::former`]) and compared, part by part, with the one the archive holds. What elaboration stamps on a definition is compared with what the entry gives for it: a former calls nothing, so it is total, and a former that is no member of a group holds each level as its entry does, a member of one storing no vector. A former is a function of its entry exactly where this reports nothing, and what it reports is what deriving every former would have to account for. A measurement, so it reports rather than asserts; counted.
#[test]
#[ignore = "measurement, counted: spells each type former of the prelude from the entry it carries and reports the parts that differ from the stored one"]
fn std_former_derivation() {
    /// How much of a differing term a line shows.
    const SHOWN: usize = 400;
    /// How many differing formers of one kind are shown in full.
    const EXAMPLES: usize = 4;

    with_prelude(|prelude| {
        println!("\n=== each type former of the prelude, spelled from its entry ===");
        let mut tally: BTreeMap<(&str, &str), usize> = BTreeMap::new();
        let mut shown: BTreeMap<&str, usize> = BTreeMap::new();
        for root in prelude {
            let definitions = root.core().items.iter().flat_map(|item| {
                let grouped = matches!(item, Item::Rec(_));
                item.definitions()
                    .into_iter()
                    .map(move |stored| (grouped, stored))
            });
            for (grouped, stored) in definitions {
                let Some(nominal) = stored.kind.nominal() else {
                    continue;
                };
                let (kind, variances) = match nominal {
                    Nominal::Inductive(declaration) => ("induct", &declaration.variances),
                    Nominal::Struct(record) => ("struct", &record.variances),
                    Nominal::Concept { record, .. } => ("concept", &record.variances),
                };
                let derived = Definition {
                    totality: Totality::Total,
                    variances: match grouped {
                        true => Vec::new(),
                        false => variances.clone(),
                    },
                    ..Definition::former(stored.name, stored.island, nominal.clone())
                };
                let parts = [
                    (
                        "universe context",
                        stored.universe_context != derived.universe_context,
                    ),
                    ("type", stored.type_ != derived.type_),
                    ("body", stored.body != derived.body),
                    ("totality", stored.totality != derived.totality),
                    ("variances", stored.variances != derived.variances),
                    ("proof", stored.proof != derived.proof),
                ]
                .into_iter()
                .filter_map(|(part, differs)| differs.then_some(part))
                .collect::<Vec<_>>();

                *tally.entry((kind, "formers")).or_default() += 1;
                if parts.is_empty() {
                    *tally.entry((kind, "alike")).or_default() += 1;
                    continue;
                }
                for part in &parts {
                    *tally.entry((kind, *part)).or_default() += 1;
                }
                let examples = shown.entry(kind).or_default();
                if *examples < EXAMPLES {
                    *examples += 1;
                    let cut = |term: &dyn ToString| {
                        term.to_string().chars().take(SHOWN).collect::<String>()
                    };
                    println!("{kind} {}: {}", stored.name.faithful(), parts.join(", "));
                    if stored.type_ != derived.type_ {
                        println!("  stored type   {}", cut(&stored.type_));
                        println!("  derived type  {}", cut(&derived.type_));
                    }
                    if stored.body != derived.body {
                        println!("  stored body   {}", cut(&stored.body));
                        println!("  derived body  {}", cut(&derived.body));
                    }
                    if stored.totality != derived.totality {
                        println!("  stored totality   {:?}", stored.totality);
                        println!("  derived totality  {:?}", derived.totality);
                    }
                    if stored.variances != derived.variances {
                        println!("  stored variances   {:?}", stored.variances);
                        println!("  derived variances  {:?}", derived.variances);
                    }
                }
            }
        }
        for ((kind, part), count) in tally {
            println!("{kind:<8} {part:<17} {count:>5}");
        }
    });
}

/// A unit supplied whole under `prefix`, holding one declaration.
fn supplied(prefix: &str) -> RootSource {
    let mut modules = RootSource::supplied();
    modules.insert_root(
        prefix,
        RootKind::Ordinary,
        "pub let a : /std/Nat = 1;".parse().unwrap(),
    );

    modules
}

/// The name is the claim: a package named `std` takes the archived root's place wherever it is read from, and however it arrived.
#[test]
fn a_package_named_std_takes_the_archived_roots_place() {
    with_prelude(|prelude| {
        for claim in [std_from_its_tree(), std_from_elsewhere(), supplied("std")] {
            let (index, root) = withheld(prelude, &[claim]).expect("a package named std");

            assert_eq!(index + 1, prelude.len(), "the last root, /std");
            assert_eq!(root.mounts()[0].prefix, Qualifier::from(["std"]));
        }
    });
}

#[test]
fn a_package_named_otherwise_takes_no_roots_place() {
    with_prelude(|prelude| {
        assert!(withheld(prelude, &[supplied("other")]).is_none());
        assert!(withheld(prelude, &[]).is_none());
    });
}

/// Only the last root can be taken, since the roots after a withheld one were compiled against it: a claim on the compiler's own root, which nothing could name anyway, collides with it as any claim does.
#[test]
fn a_package_named_sys_collides_with_the_compilers_own_root() {
    with_prelude(|prelude| {
        assert!(withheld(prelude, &[supplied("sys")]).is_none());
    });

    let error = compile_with_units(&[("sys", "pub let a : /std/Nat = 1;")], "0")
        .expect_err("the compiler's own root is in scope");

    assert!(error.contains("sys"), "unexpected error: {error}");
}

/// A unit after the first has predecessors the archived unit was never compiled against, so it is not the one that takes the root's place — and it collides, as any later claim does.
#[test]
fn only_the_first_unit_can_take_a_roots_place() {
    with_prelude(|prelude| {
        assert!(withheld(prelude, &[supplied("other"), std_from_its_tree()]).is_none());
    });

    let error = compile_with_units(
        &[
            ("other", "pub let a : /std/Nat = 1;"),
            ("std", "pub let b : /std/Nat = 2;"),
        ],
        "0",
    )
    .expect_err("a package named std after another unit claims a root still in scope");

    assert!(error.contains("std"), "unexpected error: {error}");
}

#[test]
fn a_withheld_root_is_granted_the_roots_before_it() {
    with_prelude(|prelude| {
        let prefixes = granted(prelude, prelude.len() - 1, &std_from_its_tree());

        assert!(prefixes.contains(&Qualifier::from(["sys"])));
    });
}

/// The archived unit is offered to the unit taking its root's place and to no other, and the offer reaches the cache, which decides what becomes of it.
///
/// Read off what the cache was offered rather than off a compile over it, so the standard library is never recompiled here: the offer is made before a unit compiles, and a one-declaration `/std`, which does not, is refused only after it.
#[test]
fn the_withheld_root_is_offered_to_the_unit_taking_its_place() {
    struct Recording(RefCell<Vec<bool>>);

    impl Cache for Recording {
        fn get(&self, _: &UnitSource<'_>) -> Option<Unit> {
            None
        }

        fn baseline(&self, _: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
            self.0.borrow_mut().push(offered.is_some());
            None
        }

        fn put(&self, _: &UnitSource<'_>, _: &Unit) {}
    }

    for (claim, offered) in [("std", true), ("other", false)] {
        let units = [supplied(claim)];
        let recording = Recording(RefCell::new(Vec::new()));
        let _ = Fold::new(DEFAULT_STEP_BUDGET, &units, Some(&recording)).check_units(|_| {});

        assert_eq!(*recording.0.borrow(), [offered], "a unit claiming /{claim}");
    }
}

/// Every authored source of the library with the module it is, by where it lies under the library's directory: `lib.crs` the root, `Nat/le.crs` the module `/std/Nat/le`. The library's own tests are units of their own and are left out.
fn library_files() -> Vec<(PathBuf, Qualifier, String)> {
    fn walk(directory: &Path, found: &mut Vec<PathBuf>) {
        let mut entries = fs::read_dir(directory)
            .expect("a directory of the library")
            .map(|entry| entry.expect("a directory entry").path())
            .collect::<Vec<_>>();
        entries.sort();
        for path in entries {
            if path.is_dir() {
                if path.file_name().is_some_and(|name| name != "tests") {
                    walk(&path, found);
                }
            } else if path.extension().is_some_and(|extension| extension == "crs") {
                found.push(path);
            }
        }
    }

    let directory = std_directory();
    let mut found = Vec::new();
    walk(&directory, &mut found);

    found
        .into_iter()
        .map(|path| {
            let relative = path
                .strip_prefix(&directory)
                .expect("a path under the library")
                .with_extension("");
            let segments = relative
                .components()
                .map(|component| component.as_os_str().to_string_lossy().into_owned())
                .collect::<Vec<_>>();
            let module = match segments.as_slice() {
                [root] if root == "lib" => Qualifier::from(["std"]),
                below => {
                    Qualifier::from(std::iter::once("std".to_string()).chain(below.iter().cloned()))
                }
            };
            let text = fs::read_to_string(&path).expect("an authored source");

            (path, module, text)
        })
        .collect()
}

/// Write after every `let` group of `items`, the items of `module`, the `let`s `added` answers for each of its members, the groups of a module written inline among them.
fn add_beside_each_let(
    items: &mut Vec<TopItem>,
    module: &Qualifier,
    added: &mut dyn FnMut(&Qualifier, &TopLet) -> Vec<TopLet>,
) {
    let mut index = 0;
    while index < items.len() {
        let beside = match &mut items[index] {
            TopItem::Let(members) => members
                .iter()
                .flat_map(|member| added(module, member))
                .collect::<Vec<_>>(),
            TopItem::Mod(TopMod {
                label,
                module: Some(inline),
                ..
            }) => {
                add_beside_each_let(&mut inline.items, &module.with(label.as_str()), added);
                Vec::new()
            }
            _ => Vec::new(),
        };
        let count = beside.len();
        items.splice(
            index + 1..index + 1,
            beside.into_iter().map(|member| TopItem::Let(vec![member])),
        );
        index += 1 + count;
    }
}

/// The declarations a refusal is of: each member's, wherever it is wrapped.
fn refused(error: &Error) -> BTreeSet<Global> {
    fn names(error: &Error, found: &mut BTreeSet<Global>) {
        match error {
            Error::InDeclaration { names, .. } => found.extend(names.iter().copied()),
            Error::Located { error, .. } | Error::InScope { error, .. } => names(error, found),
            _ => {}
        }
    }

    let mut found = BTreeSet::new();
    for member in error.each() {
        names(member, &mut found);
    }

    found
}

/// What a refusal says, to a length a failure is read at.
fn clipped(said: &str) -> String {
    said.chars().take(4000).collect()
}

/// The `let`s of the library the translation declines, each holding a form no program spells, in the order of their names. A report raised in one shows the term as the compiler holds it.
const DECLINED: [&str; 2] = [
    // Each holds a string past the length a report prints one whole.
    "/std/http/Url/part",
    "/std/http/syntax/status_line",
];

/// The `let`s of the library whose elaborated term names a declaration their module cannot: another module's own, reached by unfolding. No name writes it from there, and a report shows it by its full path.
const UNREACHED: [&str; 11] = [
    "/std/Nat/Coprime/cancel",
    "/std/Nat/Coprime/common",
    "/std/Nat/Coprime/quotients",
    "/std/Nat/Coprime/symmetric",
    "/std/Nat/Gcd/common",
    "/std/Nat/Gcd/euclid",
    "/std/Nat/Gcd/scaled",
    "/std/Nat/Gcd/unique",
    "/std/Tui/Field/draw",
    "/std/Tui/Listing/draw",
    "/std/Tui/border/edge",
];

/// The `let`s of the library that are refused as a report prints them: none.
const REFUSED: [&str; 0] = [];

/// The `let`s of the library whose printed form elaborates and is not proved to be the `let`.
const UNPROVED: [&str; 1] = [
    // Its printed form is generalized over nine universe levels the `let` holds at zero, and the proof asks that their maximum be zero, which elaboration does not solve for.
    "/std/Command/spawn",
];

/// The `let`s of the library whose printed form or whose proof elaboration accepts and the kernel refuses, each a finding of `documentation/roadmap/soundness/000-findings.md`.
const UNCERTIFIED: [&str; 16] = [
    // Each runs an effect: the kernel holds no two such definitions to be one, whatever they unfold to.
    "/std/Async/poll_ready",
    "/std/Command/Pipe/close",
    "/std/File/close",
    // Its printed body holds a bound's proof the kernel refuses: a comparison it decides under one order of its atoms and not under another.
    "/std/Flt/of_decimal_in",
    // The kernel's conversion stops short of elaboration's between the body and its printed form.
    "/std/Flt/signals/scaled_prod_diff",
    "/std/Flt/signals/scaled_prod_sum",
    "/std/Flt/significant",
    "/std/Flt/to_str",
    // Each runs an effect, as the first three do.
    "/std/Handle/close",
    "/std/Handle/watch",
    "/std/Serial/close",
    "/std/proc/args",
    "/std/rand/bytes",
    "/std/tcp/Listener/close",
    "/std/tcp/Socket/close",
    "/std/time/Instant/timer",
];

/// One `#[test]` per part the library is read back in, and the number of parts [`parts_of`] deals its files into, declared from one list so the two cannot disagree.
macro_rules! read_back_in_parts {
    ($($part:ident),* $(,)?) => {
        const PARTS: &[&str] = &[$(stringify!($part)),*];

        $(
            #[test]
            fn $part() {
                let part = PARTS
                    .iter()
                    .position(|name| *name == stringify!($part))
                    .expect("a part is one of the list it is declared in");

                every_let_of_the_library_reads_back_as_a_report_prints_it(part);
            }
        )*
    };
}

read_back_in_parts! {
    the_first_part_of_the_library_reads_back,
    the_second_part_of_the_library_reads_back,
    the_third_part_of_the_library_reads_back,
    the_fourth_part_of_the_library_reads_back,
    the_fifth_part_of_the_library_reads_back,
    the_sixth_part_of_the_library_reads_back,
    the_seventh_part_of_the_library_reads_back,
    the_eighth_part_of_the_library_reads_back,
}

/// The part each of `files` is read back in: the largest file first, each to the part holding the fewest bytes so far. Every file is in exactly one part by construction, so there is no roster to hold to the tree, and the parts stay level as the library grows.
fn parts_of(files: &[(PathBuf, Qualifier, String)]) -> Vec<usize> {
    let mut order = (0..files.len()).collect::<Vec<_>>();
    order.sort_by_key(|&index| (Reverse(files[index].2.len()), index));

    let mut held = vec![0usize; PARTS.len()];
    let mut parts = vec![0; files.len()];
    for index in order {
        let lightest = (0..held.len())
            .min_by_key(|&part| (held[part], part))
            .expect("the library is read back in at least one part");
        held[lightest] += files[index].2.len();
        parts[index] = lightest;
    }

    parts
}

/// **A report's term reads back.** Beside every `let` of the library two declarations are written: its type and its body as a report raised in it prints them, and a proof by `Eq/refl()` that what those declare is the `let`. The files are lowered and elaborated as the library, over the archive, and the kernel walks what comes of it. So each printed term is accepted where its reader stands, and is the term it was printed from by the conversion of each checker, at the universe levels that checker is handed: a printed term states none, and elaboration solves for them. A `let` not known to terminate is named by no type, so what is written for one is that it has its printed type.
///
/// A `let` the translation declines and one naming what its module cannot have nothing written beside them, and each is listed ([`DECLINED`], [`UNREACHED`]). One refused as printed, or whose proof is, is found by leaving it out and compiling again, so every `let` that does not read back is named, and not the first alone ([`REFUSED`], [`UNPROVED`]); and the proofs the kernel refuses are listed with the `let`s they are of ([`UNCERTIFIED`]).
///
/// **In parts, and once.** Read back whole the library is one test several times the length of any other, so it is read back a part at a time, a `#[test]` each ([`read_back_in_parts`]), and the parts run side by side; what a part writes beside the `let`s of its files is elaborated over the archive, which keeps every declaration the source wrote. And a `let` listed as refused or as unproved is left out before the first compile, so a library nothing has changed in is elaborated once: the listed ones are then written alone, where what is refused of them is what is still to be listed.
fn every_let_of_the_library_reads_back_as_a_report_prints_it(part: usize) {
    let files = library_files();
    let parts = parts_of(&files);
    // This part's files, parsed once: a round writes beside the `let`s of a copy.
    let trees = files
        .iter()
        .zip(&parts)
        .filter(|(_, at)| **at == part)
        .map(|((path, module, text), _)| {
            let tree = text
                .parse::<curios_text::Module>()
                .unwrap_or_else(|error| panic!("{} parses: {error:?}", path.display()));

            (path, module, tree)
        })
        .collect::<Vec<_>>();
    let mut own = BTreeSet::<Global>::new();
    for (_, module, tree) in &trees {
        add_beside_each_let(&mut tree.items.clone(), module, &mut |module, member| {
            own.insert(Global::Authored(module.with(member.label.as_str())));
            Vec::new()
        });
    }

    with_prelude(|prelude| {
        let [sys, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);
        let lexicons = predecessors.lexicons(&SYNTAX);
        let spellings = Rc::new(std.text().spellings(&predecessors.text()));
        let reading = Reading {
            module: std.core(),
            scope: &lexicons,
            syntax: &SYNTAX,
            spellings: Some(&spellings),
            anchors: &[],
            printer: Some(Rc::new(Translation)),
        };
        // Made once: the library's own lexicon is what every reader of it is spelled by.
        let unplaced = reading.spelling();
        let definitions = std
            .core()
            .items
            .iter()
            .flat_map(Item::definitions)
            .map(|definition| (definition.name, definition))
            .collect::<BTreeMap<_, _>>();

        let parsed = |text: &str, owner: Global| {
            text.parse::<curios_text::Term>().unwrap_or_else(|error| {
                panic!(
                    "a term written for {} parses:\n{text}\n{error:?}",
                    owner.faithful()
                )
            })
        };
        // A term as a report raised in `owner` prints it, read back as the tree it parses to: `None` where the translation declines it.
        let written = |term: &Term, owner: Global| {
            let spelling = unplaced.clone().for_reader(ReaderPosition {
                owner: Some(owner),
                witnesses: Rc::default(),
            });
            let rename = build_rename(&display_names(term), &spelling);
            let spelling = Rc::new(spelling.with_pretty_names(Rc::new(rename)));
            let printed = term.spelled(&spelling).to_string();

            (!spelling.declined()).then(|| parsed(&printed, owner))
        };

        // Every listed `let` is one the library defines, and this part answers for those of its own files.
        let listed = |list: &[&str]| {
            list.iter()
                .map(|name| {
                    *definitions
                        .keys()
                        .find(|defined| defined.faithful().to_string() == *name)
                        .unwrap_or_else(|| {
                            panic!("{name} is listed and is no `let` of the library")
                        })
                })
                .filter(|name| own.contains(name))
                .collect::<BTreeSet<_>>()
        };
        let seeded = listed(&REFUSED)
            .union(&listed(&UNPROVED))
            .copied()
            .collect::<BTreeSet<_>>();

        // One round: the library elaborated with the declarations written beside each `let` of this part that `taken` answers for.
        let round = |taken: &dyn Fn(&Global) -> bool| {
            // The `let` each added declaration stands beside, and whether it is the proof.
            let mut beside = BTreeMap::<Global, (Global, bool)>::new();
            let mut declined = BTreeSet::new();
            let mut unreached = BTreeSet::new();
            let overlay = trees
                .iter()
                .map(|(path, module, tree)| {
                    let mut tree = tree.clone();
                    add_beside_each_let(&mut tree.items, module, &mut |module, member| {
                        let label = member.label.as_str();
                        let name = Global::Authored(module.with(label));
                        let definition = definitions.get(&name).unwrap_or_else(|| {
                            panic!(
                                "the library defines {}, a `let` of its source",
                                name.faithful()
                            )
                        });
                        if !taken(&name) {
                            return Vec::new();
                        }
                        // A term elaboration built may name a declaration its reader cannot: another module's own, reached by unfolding.
                        let reaches = |term: &Term| {
                            term.free_vars()
                                .into_iter()
                                .filter_map(|free| free.as_global().copied())
                                .chain(term.construction_names())
                                .filter(|global| matches!(global, Global::Authored(_)))
                                .all(|global| {
                                    spell(&spellings, &definition.island, Some(&name), &global)
                                        .is_some()
                                })
                        };
                        if !(reaches(&definition.type_) && reaches(&definition.body)) {
                            unreached.insert(name);
                            return Vec::new();
                        }
                        let (Some(type_), Some(body)) = (
                            written(&definition.type_, name),
                            written(&definition.body, name),
                        ) else {
                            declined.insert(name);
                            return Vec::new();
                        };

                        let (printed, same) =
                            (format!("{label}_as_printed"), format!("{label}_reads_back"));
                        for (added, is_proof) in [(&printed, false), (&same, true)] {
                            let added = Global::Authored(module.with(added.as_str()));
                            assert!(
                                !definitions.contains_key(&added),
                                "the library declares {}, the name the sweep writes beside {}",
                                added.faithful(),
                                name.faithful()
                            );
                            beside.insert(added, (name, is_proof));
                        }
                        // A type names only what is known to terminate, so no proposition states that a `let` which is not is its printed form: what is written for one is that it has the printed type.
                        let (stated, by) = match definition.totality.is_total() {
                            true => (
                                parsed(&format!("/std/Eq/Eq()({label}, {printed})"), name),
                                parsed("/std/Eq/Eq/refl()", name),
                            ),
                            false => (type_.clone(), parsed(label, name)),
                        };
                        let declared = |label: String, type_, body| TopLet {
                            doc: None,
                            vis_pub: false,
                            label: Label::from(label),
                            signature: LetSignature::Name {
                                type_: Some(type_),
                                body,
                            },
                        };

                        vec![declared(printed, type_, body), declared(same, stated, by)]
                    });

                    ((*path).clone(), tree.to_string())
                })
                .collect::<Vec<_>>();

            let source = std_from_its_tree().with_overlay(Overlay::of(overlay));
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);
            let lowered = into_core_unit_over(&unit, &predecessors.text(), &SYNTAX, std.text())
                .unwrap_or_else(|error| {
                    panic!(
                        "the library lowers with its printed terms beside it:\n{}",
                        error.format()
                    )
                });
            let kept = lowered.kept_from(std.text());
            let elaborated = elaborated_over(predecessors, std, &lowered, &kept);

            (elaborated, beside, declined, unreached)
        };

        let mut refusals = Vec::new();
        let mut refused_as_printed = BTreeSet::<Global>::new();
        let mut unlike = BTreeSet::<Global>::new();
        // What a refused round says of the `let`s it wrote beside: those whose printed term is refused, and those whose proof alone is.
        let mut refused_of = |error: &Error, beside: &BTreeMap<Global, (Global, bool)>| {
            let failed = refused(error);
            let said = error
                .each()
                .map(|member| clipped(&member.to_string()))
                .collect::<Vec<_>>()
                .join("\n");
            let of = |proofs: bool| {
                failed
                    .iter()
                    .filter_map(|added| beside.get(added))
                    .filter(|(_, is_proof)| *is_proof == proofs)
                    .map(|(name, _)| *name)
                    .collect::<BTreeSet<_>>()
            };
            // A printed term that is refused takes its proof with it, and is no other term for that.
            let printed_refused = of(false);
            let proof_refused = of(true)
                .difference(&printed_refused)
                .copied()
                .collect::<BTreeSet<_>>();
            assert!(
                !(printed_refused.is_empty() && proof_refused.is_empty()),
                "the library is refused for nothing the sweep wrote:\n{said}"
            );
            refusals.push(said);
            refused_as_printed.extend(printed_refused.iter().copied());
            unlike.extend(proof_refused.iter().copied());

            printed_refused
                .union(&proof_refused)
                .copied()
                .collect::<BTreeSet<_>>()
        };

        // The listed `let`s are left out from the first round, and each round after it leaves out what the one before it was refused for, so the rounds end.
        let mut left_out = seeded.clone();
        let (recompiled, beside, declined, unreached) = loop {
            match round(&|name| !left_out.contains(name)) {
                (Ok(recompiled), beside, declined, unreached) => {
                    break (recompiled, beside, declined, unreached);
                }
                (Err(error), beside, ..) => left_out.extend(refused_of(&error, &beside)),
            }
        };
        // The listed ones, written alone: what is refused of them is what the lists still hold truly.
        if !seeded.is_empty()
            && let (Err(error), beside, ..) = round(&|name| seeded.contains(name))
        {
            refused_of(&error, &beside);
        }

        // The kernel's own walk: a declaration the source wrote stands on the verdict filed with the archive, and each one written beside it is judged.
        let finished = Zonked::project(&recompiled.module)
            .unwrap_or_else(|refusal| panic!("what elaboration finished is finished: {refusal}"));
        let judged = recheck_over(
            &finished,
            DEFAULT_STEP_BUDGET,
            predecessors,
            std,
            &[false],
            &SYNTAX,
        );

        let names = |names: &BTreeSet<Global>| {
            let mut names = names
                .iter()
                .map(|name| name.faithful().to_string())
                .collect::<Vec<_>>();
            names.sort();
            names
        };
        // A verdict is of a declaration written beside a `let`, the others standing on the archive's.
        let uncertified = judged
            .verdicts
            .iter()
            .filter_map(|verdict| beside.get(&verdict.name?))
            .map(|(name, _)| *name)
            .collect::<BTreeSet<_>>();
        // Everything the sweep found is said at once: a run costs the library over again.
        let mut found = Vec::new();
        let (known_refused, known_unproved, known_declined, known_unreached, known_uncertified) = (
            listed(&REFUSED),
            listed(&UNPROVED),
            listed(&DECLINED),
            listed(&UNREACHED),
            listed(&UNCERTIFIED),
        );
        if refused_as_printed != known_refused || unlike != known_unproved {
            found.push(format!(
                "as printed, {:?} are refused, where {:?} are the `let`s of this part known to be, and {:?} are not proved the `let` they were printed from, where {:?} are known not to be:\n{}",
                names(&refused_as_printed),
                names(&known_refused),
                names(&unlike),
                names(&known_unproved),
                refusals.join("\n")
            ));
        }
        if declined != known_declined {
            found.push(format!(
                "the translation declines {:?}, and {:?} are the `let`s of this part it is known to",
                names(&declined),
                names(&known_declined)
            ));
        }
        if unreached != known_unreached {
            found.push(format!(
                "{:?} name what their module cannot, and {:?} are the `let`s of this part known to",
                names(&unreached),
                names(&known_unreached)
            ));
        }
        if uncertified != known_uncertified || uncertified.len() != judged.verdicts.len() {
            found.push(format!(
                "of the {} declarations written beside a `let` the kernel refuses {}, those of {:?}, where {:?} are the `let`s of this part known to be refused theirs:\n{}",
                beside.len(),
                judged.verdicts.len(),
                names(&uncertified),
                names(&known_uncertified),
                judged
                    .verdicts
                    .iter()
                    .map(|verdict| {
                        format!(
                            "{}: {}",
                            verdict
                                .name
                                .map_or_else(String::new, |name| name.faithful().to_string()),
                            clipped(&verdict.error.to_string())
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            ));
        }
        assert!(found.is_empty(), "{}", found.join("\n\n"));
    });
}

/// The family-slot probe: what typing a tagged family's reference fields would cost, family by family, over every family the prelude's units declare.
///
/// Every other recorded shape is typed by the door, and each is strictly additive: the same width, fewer instructions, and for a list or a closure a deleted `is_subtype` libcall. A tagged family's reference fields are the exception on both counts. Slots are grouped by carrier, so a family-typed slot cannot share the uniform range, and a family whose constructors disagree widens to gain one; and what the widening buys is the cheap kind of cast, an exact compare against a final type. So the question is where to type them, and the criterion is exact: a family widens or it does not. This reports the split, each family read off the item that declares it (`curios_elab::test_support::declared_families`), so the rule admitting the free ones is held to the library's own families and not to those one program reaches.
///
/// # How to run it
///
/// ```sh
/// cargo test --package curios-pipeline --lib -- --ignored --nocapture family_slot_probe
/// ```
///
/// # What it last printed
///
/// Counted on top of `89b3697ab`:
///
/// ```text
/// families declared: 82, holding a family-typed field: 24
///   free: 16 families, 16 slots typed at no width cost
///   paid: 8 families, 11 slots typed for 10 slots of width
///   /std/Map/Node/Node                 4 slots -> 5 slots, 0 typed -> 2 typed   PAID
///   /std/Cli/Chosen                    3 slots -> 5 slots, 1 typed -> 3 typed   PAID
///   /std/Cli/Outcome                   2 slots -> 4 slots, 0 typed -> 2 typed   PAID
///   /std/Cli/Cluster                   4 slots -> 5 slots, 2 typed -> 3 typed   PAID
///   /std/Test/Test                     2 slots -> 3 slots, 0 typed -> 1 typed   PAID
///   /std/Tui/Wake                      2 slots -> 3 slots, 0 typed -> 1 typed   PAID
///   /std/http/read/Step                3 slots -> 4 slots, 0 typed -> 1 typed   PAID
///   /std/http/stream/Stop              2 slots -> 3 slots, 0 typed -> 1 typed   PAID
/// ```
///
/// **What the figures decide.** `/std/Map/Node/Node`, the family a typed fork's children would serve, is a paid row: four slots to five, a quarter again as much live memory on the corpus's hottest allocated structure, to replace two casts that are already exact compares against a final type. Against the `trees` finding that live bytes convert to time under an all-live collector, that is a trade to decline, and the door declines it. What the door does instead is admit the free column by the exact criterion: it types a family's reference slots iff the row's width is unchanged, which sixteen families meet, the command line's, the formatter's, the layout's and the TOML reader's among them. A product needs no such test: one writer can never widen a row, so its reference fields always type.
#[test]
#[ignore = "measurement, counted: reports the split rather than asserting"]
fn family_slot_probe() {
    with_prelude(|prelude| {
        let families = prelude
            .iter()
            .flat_map(|unit| curios_elab::test_support::declared_families(unit.erased()))
            .collect::<Vec<_>>();
        let named = families
            .iter()
            .map(|family| (family.name.as_str(), family))
            .collect::<BTreeMap<_, _>>();

        // Whether a value of the family is always the row struct. An immediate family's bare constructor rides the i31, or for a `Nat` or an `Int` the boxed magnitude, so no heap type names its population and a slot can never be declared at it. A family no unit of the prelude declares is judged by nothing, and a slot at it stays untyped.
        let always_a_row = |name: &str| -> bool {
            let Some(family) = named.get(name) else {
                return false;
            };
            let bare = family
                .constructors
                .iter()
                .filter(|fields| {
                    matches!(fields.as_slice(), [field] if matches!(field.shape, FieldShape::Immediate | FieldShape::Number))
                })
                .count();

            family.constructors.len() < 2 || bare != 1
        };

        // The carrier a field occupies, mirroring the door's own rule. `family_typed` is the question this probe asks: a family-typed reference field is the one shape whose typing can cost width.
        let class = |field: &curios_elab::test_support::ErasedField,
                     family_typed: bool|
         -> Option<String> {
            match field.shape {
                FieldShape::Immediate => Some("nat".into()),
                FieldShape::Flt => Some("flt".into()),
                FieldShape::List => Some("list".into()),
                FieldShape::Closure(arity) => Some(format!("closure/{arity}")),
                FieldShape::Product(schema) => Some(format!("product/{schema}")),
                FieldShape::Family(_) => field
                    .family
                    .as_deref()
                    .filter(|name| family_typed && always_a_row(name))
                    .map(|name| format!("family/{name}")),
                FieldShape::Number | FieldShape::Packed(_) | FieldShape::Opaque => None,
            }
        };

        // The classed layout's width and typed-slot count for one family's constructor rows.
        let layout = |rows: &[Vec<curios_elab::test_support::ErasedField>],
                      family_typed: bool|
         -> (usize, usize) {
            let mut classed = BTreeMap::<String, usize>::new();
            let mut opaque_slots = 0;
            for row in rows {
                let mut counts = BTreeMap::<String, usize>::new();
                let mut opaque = 0;
                for field in row {
                    match class(field, family_typed) {
                        Some(name) => *counts.entry(name).or_default() += 1,
                        None => opaque += 1,
                    }
                }
                for (name, count) in counts {
                    let slot = classed.entry(name).or_default();
                    *slot = (*slot).max(count);
                }
                opaque_slots = opaque_slots.max(opaque);
            }
            let typed: usize = classed.values().sum();

            (1 + typed + opaque_slots, typed)
        };

        let (mut free_families, mut paid_families) = (0, 0);
        let (mut free_slots, mut paid_slots, mut paid_width) = (0, 0, 0);
        let mut report = Vec::new();

        for family in &families {
            let rows = &family.constructors;
            // Collapsed and immediate encodings do not lay a tag out, so they are not what this asks about.
            if rows.len() < 2 || !always_a_row(&family.name) {
                continue;
            }
            if !rows
                .iter()
                .flatten()
                .any(|field| matches!(field.shape, FieldShape::Family(_)))
            {
                continue;
            }

            let (uniform_width, uniform_typed) = layout(rows, false);
            let (typed_width, typed_typed) = layout(rows, true);
            let gain = typed_typed - uniform_typed;
            let free = typed_width == uniform_width;
            match free {
                true => {
                    free_families += 1;
                    free_slots += gain;
                }
                false => {
                    paid_families += 1;
                    paid_slots += gain;
                    paid_width += typed_width - uniform_width;
                }
            }
            report.push(format!(
                "  {:<34} {} slots -> {} slots, {} typed -> {} typed   {}",
                family.name,
                uniform_width,
                typed_width,
                uniform_typed,
                typed_typed,
                match free {
                    true => "FREE",
                    false => "PAID",
                }
            ));
        }

        println!(
            "families declared: {}, holding a family-typed field: {}",
            families.len(),
            free_families + paid_families
        );
        println!("  free: {free_families} families, {free_slots} slots typed at no width cost");
        println!(
            "  paid: {paid_families} families, {paid_slots} slots typed for {paid_width} slots of width"
        );
        for line in report {
            println!("{line}");
        }
    });
}
