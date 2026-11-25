//! What the standard library's build does as it is built again: a recompile's census of what each edit reads, the images a build divides the library into, a replay's census, and the slots a family's declarations take.

use {
    curios_anf::FieldShape,
    curios_common::{Qualifier, digest},
    curios_core::{Certification, DefinitionKind, Definitions, Global, Item, Sharing},
    curios_elab::{DEFAULT_BUDGET, Traces},
    curios_erase::ErasedUnit,
    curios_module::{
        Overlay, PreparedText, SYNTAX, UnitSource, into_core_unit, into_core_unit_over,
    },
    curios_pipeline::test_support::*,
    curios_prelude::with_prelude,
    curios_prelude_build::std_directory,
    curios_unit::{Pack, Predecessors, Unit, apart_parts, halves, packed, test_support::*},
    std::{
        collections::{BTreeMap, BTreeSet},
        fs,
        time::Instant,
    },
    xbench::{Measure, read, read_elapsed},
};

/// What a question about the standard library costs after an edit to one declaration: how many names are lowered again and how many elaborated again, and the time each phase takes, for a leaf and for a hub. `cargo xbench measure std_recompile_census` takes it: for each edit, `<edit>/names`, `/lowered again` and `/elaborated again`, and `/lower`, `/elaborate` and `/recompile`, each phase's time, the last lowering, elaborating, judging and erasing.
pub fn std_recompile_census() {
    let directory = std_directory();

    with_prelude(|prelude| {
        let [sys, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);

        for Edit { label, file, edit } in edits() {
            let path = directory.join(file);
            let text = edit(&fs::read_to_string(&path).expect("an authored source"));
            let source = std_from_its_tree().with_overlay(Overlay::of([(path, text)]));
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

            let start = Instant::now();
            let lowered = into_core_unit_over(
                &unit,
                &predecessors.text(),
                &SYNTAX,
                std.text().expect("a program's unit holds its lowering"),
                true,
            )
            .expect("the edited library lowers");
            let lowered_in = start.elapsed();
            let relowered = lowered
                .lowered_anew()
                .expect("lowered over a baseline")
                .clone();
            let start = Instant::now();
            let elaborated = elaborated_over(predecessors, std, &lowered, &relowered)
                .unwrap_or_else(|error| panic!("the edited library elaborates: {error}"));
            let elaborated_in = start.elapsed();
            let start = Instant::now();
            recompiled_over(DEFAULT_BUDGET, predecessors, &unit, std)
                .expect("the edited library recompiles");
            let recompiled_in = start.elapsed();

            let names = lowered
                .core()
                .items
                .iter()
                .flat_map(Item::declared_names)
                .count();
            for (figure, count) in [
                ("names", names),
                ("lowered again", relowered.len()),
                ("elaborated again", elaborated.made.len()),
            ] {
                read(
                    &format!("{label}/{figure}"),
                    Measure::Count,
                    None,
                    count as u64,
                );
            }
            for (phase, took) in [
                ("lower", lowered_in),
                ("elaborate", elaborated_in),
                ("recompile", recompiled_in),
            ] {
                read_elapsed(&format!("{label}/{phase}"), None, took);
            }
        }
    });
}

/// How the standard library's stored unit divides between its parts: the bytes of each archived alone, and how long each takes to restore out of them. What only a recompile of the unit reads is its baseline half, and the rest is what every compilation after the unit restores. `cargo xbench measure std_image_division` takes it: each part's `<part>/bytes` and, where it is restored alone, five readings of `<part>/restore`, a part named under the whole it is of; `structures/…`, the structures the module conses into in one table and item by item; and five readings of each of a filing's steps, `a filing's steps/<step>`.
pub fn std_image_division() {
    with_prelude(|prelude| {
        let [_, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let sized = |part: &str, length: usize| {
            read(
                &format!("{part}/bytes"),
                Measure::Count,
                None,
                length as u64,
            );
        };
        // Five restorations of `$bytes` as a `$type`, each a reading of `$part`'s restore.
        macro_rules! restored {
            ($part:expr, $type:ty, $bytes:expr) => {{
                for _ in 0..5 {
                    let start = Instant::now();
                    let value =
                        curios_archive::from_bytes::<$type>($bytes).expect("a part restores");
                    read_elapsed(concat!($part, "/restore"), None, start.elapsed());
                    drop(value);
                }
            }};
        }
        // What `$definitions`' items are of it: archived as it holds them, and each alone, which stores a structure two of them share once for each.
        macro_rules! items {
            ($name:expr, $definitions:expr) => {{
                let definitions: &Definitions = $definitions;
                let whole = curios_archive::to_bytes(&definitions.items.to_vec())
                    .expect("a module's items serialize");
                let alone = definitions
                    .items
                    .iter()
                    .map(|item| {
                        curios_archive::to_bytes(item)
                            .expect("an item serializes")
                            .len()
                    })
                    .sum::<usize>();
                sized($name, whole.len());
                sized(concat!($name, ", each archived alone"), alone);
            }};
        }
        macro_rules! part {
            ($name:expr, $type:ty, $value:expr) => {{
                let encoded = curios_archive::to_bytes($value).expect("a part serializes");
                sized($name, encoded.len());
                restored!($name, $type, &encoded);
            }};
        }

        let stored = std.stored().expect("the library's unit serializes");
        let (unit, apart) = halves(stored.index()).expect("a unit's index");
        let [apart, _, _]: [curios_archive::Bytes; 3] =
            curios_archive::parted(&curios_archive::Bytes::from(apart.to_vec()))
                .and_then(|parts| parts.try_into().ok())
                .expect("what a certified unit stores apart");
        let reads = reads_filed(std.certification().recorded())
            .expect("the certifier's reads serialize")
            .len();
        let records =
            lowering_stored(std.text().expect("a program's unit holds its lowering")).len();
        let anchors = {
            let held = std
                .text()
                .expect("a program's unit holds its lowering")
                .anchors();
            curios_archive::to_bytes(held)
                .expect("the anchors serialize")
                .len()
                + held
                    .blobs(&|_| true)
                    .expect("the anchors serialize")
                    .iter()
                    .map(|blob| blob.bytes.as_slice().len())
                    .sum::<usize>()
        };
        let imports = {
            let held = std
                .text()
                .expect("a program's unit holds its lowering")
                .imports();
            curios_archive::to_bytes(held)
                .expect("the import scopes serialize")
                .len()
                + held
                    .blobs(&|_| true)
                    .expect("the import scopes serialize")
                    .iter()
                    .map(|blob| blob.bytes.as_slice().len())
                    .sum::<usize>()
        };
        let [lowered, forms, _, _, _, _, _, _, _, _, _, _, _] = apart_parts(&apart)
            .expect("what a unit stores apart")
            .map(|part| part.as_slice().len());
        // Each lowered item, then each erased form, is a blob stored alone, and the elaborated items are the members of one run.
        let blobs = stored
            .blobs(|_| true)
            .expect("the library's blobs serialize");
        let alone = |range: std::ops::Range<usize>| {
            blobs[range]
                .iter()
                .map(|blob| blob.bytes.as_slice().len())
                .sum::<usize>()
        };
        let (lowered, forms) = (lowered / 32, forms / 32);
        let module = blobs[lowered + forms..]
            .first()
            .map_or(0, |member| member.bytes.as_slice().len());
        let (lowered, forms) = (alone(0..lowered), alone(lowered..lowered + forms));
        let asked = traces_stored(std.traces()).len();
        let erasures = asked_stored(
            std.erased()
                .recorded()
                .get()
                .expect("erasure's half restores"),
        )
        .len();

        // Every blob once, a run its members share counted once.
        let blobs_held = blobs
            .iter()
            .map(|blob| {
                let bytes = blob.bytes.as_slice();
                ((bytes.as_ptr(), bytes.len()), bytes.len())
            })
            .collect::<BTreeMap<_, _>>()
            .values()
            .sum::<usize>();
        sized("the index", stored.index().len());
        sized("its blobs", blobs_held);
        sized("what a successor restores", unit.len());
        restored!("what a successor restores", Unit, unit);
        part!(
            "what a successor restores/the lowering's state",
            PreparedText,
            std.text().expect("a program's unit holds its lowering")
        );
        let consed = std.core().shared(&Sharing::new());
        part!(
            "what a successor restores/the elaborated module, consed",
            Definitions,
            &consed
        );
        items!(
            "what a successor restores/the elaborated module, consed/its items",
            &consed
        );
        part!(
            "what a successor restores/its erased forms' index",
            ErasedUnit,
            std.erased()
        );
        part!(
            "what a successor restores/the certifier's record",
            Certification,
            std.certification()
        );

        sized(
            "what a reader of a declaration restores",
            lowered + forms + module,
        );
        sized(
            "what a reader of a declaration restores/its lowered items, each stored alone",
            lowered,
        );
        sized(
            "what a reader of a declaration restores/its erased forms, each stored alone",
            forms,
        );
        sized(
            "what a reader of a declaration restores/its elaborated items, one run read by item",
            module,
        );
        sized("what a report restores", anchors);
        sized(
            "what a report restores/where its declarations lie, each source's texts alone",
            anchors,
        );
        sized(
            "what a recompile alone restores",
            imports + records + asked + erasures + reads,
        );
        sized("what a recompile alone restores/its import scopes", imports);
        sized(
            "what a recompile alone restores/what its verdicts stand on",
            reads,
        );
        sized(
            "what a recompile alone restores/the lowering's records and scans",
            records,
        );
        sized(
            "what a recompile alone restores/what its declarations asked",
            asked,
        );
        sized(
            "what a recompile alone restores/what its erasures asked",
            erasures,
        );

        let together = Sharing::new();
        std.core().shared(&together);
        let alone = std
            .core()
            .items
            .iter()
            .map(|item| {
                let sharing = Sharing::new();
                let definitions = Definitions {
                    items: vec![item.clone()].into(),
                };
                definitions.shared(&sharing);

                sharing.structures()
            })
            .sum::<usize>();
        read(
            "structures/the module consed in one table",
            Measure::Count,
            None,
            together.structures() as u64,
        );
        read(
            "structures/each item in its own",
            Measure::Count,
            None,
            alone as u64,
        );

        // Five runs of `$step`, each a reading of `a filing's steps/$name`.
        macro_rules! timed {
            ($name:expr, $step:expr) => {{
                for _ in 0..5 {
                    let start = Instant::now();
                    let value = $step;
                    read_elapsed(concat!("a filing's steps/", $name), None, start.elapsed());
                    drop(value);
                }
            }};
        }
        timed!(
            "consing the elaborated module",
            std.core().shared(&Sharing::new())
        );
        timed!(
            "serializing the elaborated module alone",
            curios_archive::to_bytes(&consed).expect("a module serializes")
        );
        timed!(
            "indexing the unit, its items named alone",
            std.stored().expect("a unit serializes")
        );
        timed!(
            "serializing its blobs, one run consed",
            stored.blobs(|_| true).expect("a unit's blobs serialize")
        );
        timed!("digesting the index", digest(stored.index()));
        let pack =
            Pack::read(curios_archive::Bytes::from(packed(&blobs))).expect("a unit's blobs pack");
        timed!(
            "restoring the unit",
            Unit::restored(stored.index(), |digest| pack.get(digest)).expect("a unit restores")
        );
    });
}

/// What differs between the items of `whole` declaring `names` and `replayed`'s, each beside the traces its elaboration filed, each as the definition and the part that differs.
fn differences(
    (whole, whole_traces): (&Definitions, &Traces),
    (replayed, replayed_traces): (&Definitions, &Traces),
    names: &BTreeSet<Global>,
) -> Vec<String> {
    let first = |item: &Item| item.declared_names().first().copied();
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
            let totality = |traces: &Traces| {
                traces
                    .classified(&this.name)
                    .map(|classified| classified.totality)
            };
            if totality(whole_traces) != totality(replayed_traces) {
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

/// What each declaration of the standard library elaborates to where it alone is elaborated again: the library as its sources lower it, compiled over the archived unit with every declaration but one offered as kept, so the walk elaborates that one and keeps the others by their traces. The declaration is compared with what the archive holds for it, and so is what it asked; what else the walk elaborated again is counted. And again with unrelated authored definitions elaborated beside it, which nothing it reads distinguishes. A declaration is a function of what it reads exactly when this reports nothing (`documentation/design/compilation/a-declaration-is-a-function-of-what-it-reads.md`). `cargo xbench measure std_replay_census` takes it: for each padding, `beside <padding>/declarations`, and a figure of one for each declaration of `beside <padding>/refused/<declaration>`, `/differing/<declaration>` and `/asked otherwise/<declaration>`, with `/others/<declaration>` the names the walk elaborated again beyond those it was handed; how a declaration differs, or why it is refused, is written to standard error beside them.
pub fn std_replay_census() {
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
                    .map(move |name| (name, index))
            })
            .collect::<BTreeMap<_, _>>();

        let declarations = declarations(lowered_core);
        for padding in PADDINGS {
            let beside = format!("beside {padding}");
            read(
                &format!("{beside}/declarations"),
                Measure::Count,
                None,
                declarations.len() as u64,
            );
            for names in &declarations {
                let first = names
                    .iter()
                    .filter_map(|name| position.get(name))
                    .min()
                    .copied()
                    .expect("a declaration declares a lowered name");
                let own = &lowered_core.items[first];
                let written = own.declared_names()[0];
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

                let declaration = own.describe();
                match elaborated_over(predecessors, std, &lowered, &again) {
                    Err(error) => {
                        read(
                            &format!("{beside}/refused/{declaration}"),
                            Measure::Count,
                            None,
                            1,
                        );
                        let refusal = error.to_string().replace('\n', " | ");
                        eprintln!(
                            "{beside}: {declaration} is refused: {}",
                            refusal.chars().take(600).collect::<String>()
                        );
                    }
                    Ok(replayed) => {
                        let differences = differences(
                            (whole, std.traces()),
                            (&replayed.definitions, &replayed.traces),
                            names,
                        );
                        if !differences.is_empty() {
                            read(
                                &format!("{beside}/differing/{declaration}"),
                                Measure::Count,
                                None,
                                1,
                            );
                        }
                        for difference in differences {
                            eprintln!("{beside}: {difference}");
                        }
                        if replayed.traces.of(&written) != std.traces().of(&written) {
                            read(
                                &format!("{beside}/asked otherwise/{declaration}"),
                                Measure::Count,
                                None,
                                1,
                            );
                        }
                        // What the walk elaborated again beyond what it was handed: a declaration that asked a part which came out another way.
                        let others = replayed.made.difference(&again).count();
                        if others != 0 {
                            read(
                                &format!("{beside}/others/{declaration}"),
                                Measure::Count,
                                None,
                                others as u64,
                            );
                        }
                    }
                }
            }
        }
    });
}

/// The family-slot probe: what typing a tagged family's reference fields would cost, family by family, over every family the prelude's units declare.
///
/// Every other recorded shape is typed by the door, and each is strictly additive: the same width, fewer instructions, and for a list a deleted test of its shape and for a closure a deleted `is_subtype` libcall. A tagged family's reference fields are the exception on both counts. Slots are grouped by carrier, so a family-typed slot cannot share the uniform range, and a family whose constructors disagree widens to gain one; and what the widening buys is the cheap kind of cast, an exact compare against a final type. So the question is where to type them, and the criterion is exact: a family widens or it does not. This reports the split, each family read off the item that declares it (`curios_erase::test_support::declared_families`), so the rule admitting the free ones is held to the library's own families and not to those one program reaches.
///
/// # How to run it
///
/// `cargo xbench measure family_slot_probe` takes it: `families declared` and `families holding a family-typed field`, `free/families` and `free/slots typed`, `paid/families`, `paid/slots typed` and `paid/width`, and for each family holding one, `<family>/slots uniform`, `/slots typed`, `/typed slots uniform`, `/typed slots typed` and `/paid`, one where typing its reference slots widens it.
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
pub fn family_slot_probe() {
    with_prelude(|prelude| {
        let families = prelude
            .iter()
            .flat_map(|unit| curios_erase::test_support::declared_families(unit.erased()))
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
        let class = |field: &curios_erase::test_support::ErasedField,
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
                FieldShape::Number | FieldShape::Bytes | FieldShape::Opaque => None,
            }
        };

        // The classed layout's width and typed-slot count for one family's constructor rows.
        let layout = |rows: &[Vec<curios_erase::test_support::ErasedField>],
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
            for (figure, count) in [
                ("slots uniform", uniform_width),
                ("slots typed", typed_width),
                ("typed slots uniform", uniform_typed),
                ("typed slots typed", typed_typed),
                ("paid", usize::from(!free)),
            ] {
                read(
                    &format!("{}/{figure}", family.name),
                    Measure::Count,
                    None,
                    count as u64,
                );
            }
        }

        for (figure, count) in [
            ("families declared", families.len()),
            (
                "families holding a family-typed field",
                free_families + paid_families,
            ),
            ("free/families", free_families),
            ("free/slots typed", free_slots),
            ("paid/families", paid_families),
            ("paid/slots typed", paid_slots),
            ("paid/width", paid_width),
        ] {
            read(figure, Measure::Count, None, count as u64);
        }
    });
}
