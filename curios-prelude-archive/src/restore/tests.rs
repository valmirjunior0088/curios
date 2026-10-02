//! What the restored images must be true of, asserted over the prelude as the fold sees it: two roots in dependency order, whose claims are about their union unless the claim is about one of them.

use {
    super::*,
    crate::SYNTAX,
    curios_cert::{
        Error, Globals, certify_module, recheck_module_measured, recheck_module_verdicts_uncached,
    },
    curios_core::{Bound, Cases, Global, Item, Match, Subterm, Term, Visit, Zonked},
    curios_elab::{Context, DEFAULT_STEP_BUDGET, ErasedArena, Resumed, erase_unit},
    curios_unit::{Record, Uncertified, segments},
    curios_utilities::digest,
    std::{
        cell::{Cell, RefCell},
        collections::{BTreeMap, BTreeSet},
        path::PathBuf,
        rc::Rc,
        thread,
        time::Instant,
    },
};

/// Every item the prelude declares, across its roots — what a claim about "the prelude" is a claim about.
fn items<'a>(prelude: &'a [&'a Uncertified]) -> impl Iterator<Item = &'a Item> {
    prelude.iter().flat_map(|root| root.core().items.iter())
}

#[test]
fn embedded_archives_validate() {
    for archived in validate_archives() {
        archived.unwrap();
    }
}

/// A prelude test would ride into every compilation and surface in every downstream `curios test` run, so the standard library shipping none is a contract, not an accident of today's sources.
#[test]
fn the_stored_prelude_declares_no_tests() {
    with_prelude(|prelude| {
        assert!(prelude.iter().all(|root| root.core().tests.is_empty()));
    });
}

/// The declarations a literal expands into, and those its proof is checked by running, are monomorphic, so no occurrence mints universe metavariables in the data's length.
///
/// A literal's type is `Str` at a single level, and its proof is `True/qed()` against a decided `Valid` — once, whatever the length, and `True` has no level to mint. What stays pinned is what checking that proof reduces: each carrier's `Valid`, the `Valid/from` a string's unfolds to, and the `scan_from` fold under it, on which a level would be minted once per literal or once per byte, and a declaration's level count would grow with literal *length* — which is what makes `long_str_literal_compiles_on_the_default_test_stack` a test.
#[test]
fn string_literal_machinery_is_monomorphic() {
    // `Str` and `Char` are the carriers a literal builds; the rest is what its proof is discharged by running. None of the reduced names is in `curios-prelude-archive/src/syntax.rs`: nothing in Rust emits them, they are reached through the carriers' field types.
    let pinned = [
        "/std/Str/Str",
        "/std/Str/Valid/Valid",
        "/std/Str/Valid/from",
        "/std/Str/scan_from",
        "/std/Char/Char",
        "/std/Char/Valid/Valid",
    ];

    with_prelude(|prelude| {
        let mut parameters = std::collections::BTreeMap::new();
        for item in items(prelude) {
            match item {
                Item::Let(definition) => {
                    parameters.insert(
                        definition.name.symbol(),
                        definition.universe_context.parameter_count,
                    );
                }
                Item::Rec(rec) => {
                    for definition in rec.definitions() {
                        parameters.insert(
                            definition.name.symbol(),
                            definition.universe_context.parameter_count,
                        );
                    }
                }
            }
        }

        let mut checked = 0;
        for target in pinned {
            let Some(count) = parameters.get(target) else {
                continue;
            };
            checked += 1;
            assert_eq!(
                *count, 0,
                "{target} is universe-polymorphic; every literal byte will mint levels"
            );
        }
        // Without this the test passes vacuously if a registered name is renamed.
        assert!(
            checked == pinned.len(),
            "found only {checked} of the pinned registered names; \
             the names this pins have moved"
        );
    });
}

#[test]
fn a_truncated_archive_is_rejected() {
    for (root, bytes) in ROOTS {
        assert!(validate_bytes(root, &bytes[..bytes.len() / 2]).is_err());
    }
}

/// `/sys` is supplied whole by the build script — no file is read and nothing precedes it — so its record says so, which is what keeps a source tree from ever claiming to be the one it came from.
#[test]
fn the_sys_image_records_no_reads_and_no_predecessor() {
    let [sys, _] = restore_archives();

    assert!(sys.record.reads.is_empty());
    assert!(sys.record.predecessors.is_empty());
}

/// The digest a record names its predecessor by is the one that predecessor's own record names itself by: the chain is checkable from the records alone.
#[test]
fn the_std_image_records_the_sys_image_as_its_predecessor() {
    let [sys, std] = restore_archives();

    assert_eq!(std.record.predecessors, [sys.record.unit]);
}

/// The read set is closed and every authored module is registered, so the record and the tree agree exactly: the record is the compiler's own account of which sources the image came from.
#[test]
fn the_std_record_names_every_authored_source_and_no_other() {
    let authored = crate::tests::authored()
        .into_iter()
        .map(|path| {
            path.canonicalize()
                .expect("an authored source canonicalizes")
        })
        .collect::<BTreeSet<_>>();

    let [_, std] = restore_archives();
    let recorded = std
        .record
        .reads
        .iter()
        .map(|(path, _)| PathBuf::from(path))
        .collect::<BTreeSet<_>>();

    assert_eq!(recorded, authored);
}

/// The record ahead of each image digests exactly the unit segment behind it, as a slot's record digests the unit it was filed with.
#[test]
fn each_record_digests_the_unit_segment_it_sits_ahead_of() {
    for (root, bytes) in ROOTS {
        let (record, unit) = segments(bytes).unwrap_or_else(|| panic!("/{root} is a stored unit"));
        let record = curios_archive::from_bytes::<Record>(record)
            .unwrap_or_else(|error| panic!("/{root} record restores: {error}"));

        assert_eq!(record.unit, digest(unit));
    }
}

/// The last root's, because the arena is cumulative: each unit's erasure resumes over the one before it, so the prefix's whole artifact is the arena the last root carries.
#[test]
fn ersd_clones_are_fresh() {
    with_prelude(|prelude| {
        let last = prelude.last().expect("the prelude has roots");
        let first = last.arena();
        assert!(!first.is_empty());
        drop(first);
        assert!(!last.arena().is_empty());
    });
}

#[test]
fn every_syntax_target_is_present_after_restore() {
    with_prelude(|prelude| {
        let names = items(prelude)
            .flat_map(Item::declared_names)
            .cloned()
            .collect::<BTreeSet<_>>();
        for target in SYNTAX.targets() {
            assert!(
                names.contains(&Global::Authored(target.qualifier())),
                "missing syntax target {}",
                target.symbol()
            );
        }
    });
}

#[test]
fn every_registered_concept_declares_its_method_after_restore() {
    with_prelude(|prelude| {
        for target in SYNTAX.concept_fields() {
            let concept = prelude
                .iter()
                .find_map(|root| {
                    root.core()
                        .concepts
                        .get(&Global::Authored(target.concept.qualifier()))
                })
                .unwrap_or_else(|| panic!("missing concept {}", target.concept.symbol()));
            assert!(
                concept.fields.iter().any(|field| field == target.field),
                "concept {} does not declare {}",
                target.concept.symbol(),
                target.field
            );
        }
    });
}

/// A term's printed head, clipped — enough to tell one refusal's shape from another's without pasting a standard-library type into a tally.
fn head(term: &Term) -> String {
    let rendered = format!("{term}");
    let rendered = rendered.split_whitespace().collect::<Vec<_>>().join(" ");

    match rendered.char_indices().nth(44) {
        Some((cut, _)) => format!("{}…", &rendered[..cut]),
        None => rendered,
    }
}

/// The class a refusal is tallied under.
///
/// Deliberately mechanical: the variant, plus for a mismatch the two sides' printed heads. Naming classes like "index inversion" here would be inventing categories from a heuristic, which is how this project's wrong answers get made — the point of the tally is to let the categories fall out of it.
fn class(error: &Error) -> String {
    match error {
        Error::Mismatch { inferred, expected } => {
            format!("Mismatch  {}  vs  {}", head(inferred), head(expected))
        }
        other => {
            let rendered = format!("{other:?}");

            rendered
                .split(['(', ' ', '{'])
                .next()
                .unwrap_or("?")
                .to_string()
        }
    }
}

/// Every item the kernel refuses across the whole fixed prelude, tallied by class.
///
/// Not an assertion — a measurement, run on demand. `recheck_module` stops at the first refusal and so says nothing about what lies past it; this walks to the end with each verdict independent of the others (see `recheck_module_verdicts`), which is what makes the classes countable rather than discovered one build at a time.
///
/// The restored image is the subject, named directly: a compiled module carries only its own items, so no fixture reaches the standard library by compiling. What asserts the prelude is acceptable is `curios-prelude`'s own build script, which runs this same walk and panics on the first refusal; this is the inventory beside it.
///
/// An abort rather than a tally is a finding, not noise: nothing here is wrapped in a catch, because a kernel that aborts is a kernel to fix, and raising `RUST_MIN_STACK` would conceal one rather than fix it.
#[test]
#[ignore = "inventory: measures where the kernel disagrees rather than asserting"]
fn kernel_disagreements() {
    with_prelude(|prelude| {
        let mut globals = Globals::default();
        let mut verdicts = Vec::new();

        // Each root against the roots before it, which is the environment it was elaborated in: walking `/std` from an empty one would tally a refusal per intrinsic carrier it wraps and say nothing about the kernel. Mounted with the record its own walk left, as `curios-prelude`'s build mounts it.
        for root in prelude {
            let core = root.core();
            let zonked = Zonked::project(core).expect("a restored prelude root is zonked");
            let rechecked = certify_module(&zonked, DEFAULT_STEP_BUDGET, &globals, SYNTAX);
            verdicts.extend(rechecked.verdicts);
            globals.mount(core, &rechecked.certification);
        }

        let mut tally: BTreeMap<String, usize> = BTreeMap::new();
        for verdict in &verdicts {
            *tally.entry(class(&verdict.error)).or_default() += 1;
        }

        println!(
            "\n=== {} refusals over {} prelude items ===",
            verdicts.len(),
            items(prelude).count()
        );
        for (class, count) in &tally {
            println!("  {count:>4}  {class}");
        }
        for verdict in &verdicts {
            let name = match &verdict.name {
                Some(name) => format!("{name}"),
                None => "<entrypoint>".to_string(),
            };
            println!("        {name}  —  {}", class(&verdict.error));
        }
    });
}

/// Memoization is an evaluation strategy exactly as long as switching it off changes no *semantic* verdict. This runs the whole-prelude walk both ways and requires the verdict lists identical.
///
/// **The semantic half is what this is for, rather than what it happens to cover.** A term-keyed memo hit spends nothing, so an uncached walk spends at least as much as a cached one and the two may reach different *exhaustion* points; `curios-cert`'s `spend` module argues why that is the whole of what the design gives up. Comparing verdicts at one budget, over a corpus where nothing exhausts, is therefore exactly the surviving property — acceptance, and refusals that are not exhaustion, are budget-independent and must agree. Asserting equal exhaustion points would assert what the design gives up.
///
/// The prelude is the subject because the property needs a large body of real terms to mean anything, and this is the only one the workspace has.
#[test]
#[ignore = "parity: runs the whole-prelude walk twice, the second time uncached"]
fn kernel_memo_parity() {
    with_prelude(|prelude| {
        let mut globals = Globals::default();

        for root in prelude {
            let core = root.core();
            let zonked = Zonked::project(core).expect("a restored prelude root is zonked");
            let cached = certify_module(&zonked, DEFAULT_STEP_BUDGET, &globals, SYNTAX);
            assert_eq!(
                cached.verdicts,
                recheck_module_verdicts_uncached(&zonked, DEFAULT_STEP_BUDGET, &globals, SYNTAX),
            );
            globals.mount(core, &cached.certification);
        }
    });
}

/// The costs of the stored prelude, taken over the image: the figures `curios-package`'s README and `curios-elab`'s budget floors cite.
///
/// Kept in-tree so a figure is retaken by running it rather than by writing a probe again: a figure nobody can cheaply reproduce decays into a claim about history.
///
/// # How to read it
///
/// Numbers are only comparable to the recorded ones in `--release`, because these are the figures taken over the *stored* image and a debug build measures a different program:
///
/// ```sh
/// cargo test --release --package curios-prelude-archive -- --ignored --nocapture stored_prelude_measurements
/// ```
///
/// It asserts nothing and cannot fail — a measurement that fails is a measurement with an opinion. What it does not cover is stated where it prints: the elaboration figures come from the build script's own `profile.tsv` and need no probe, and the witness inventory is a term walk this does not do.
///
/// A range is two runs, and the spread between them is part of the figure. The clone alone is averaged, over 100, because at a few milliseconds one sample is inside the noise band.
///
/// # What it last printed
///
/// The latest reading of each row, in release, with the run it came from; different runs may come from different hosts, so wall clocks compare only within one run. Kept here rather than in a document that cites this test, so a number cannot drift from the thing that would check it.
///
/// | What | Measured | Run |
/// | --- | --- | --- |
/// | Cold restore — bytecheck, then deserializing the prepared Text state, the Core and the erased prefix | 34.1–35.3 ms | A |
/// | Erased-prefix clone, taken once per compile | 2.0–2.1 ms, mean of 100 | A |
/// | Re-erasing one whole unit over the stored Core | 621 ms | B |
/// | Certifying one whole unit | 12.6 s, 0 refusals | C |
/// | Heaviest declaration that certification makes | 512 455 units, peak depth 6 | C |
///
/// The heaviest declaration's figure counts the closed machine re-deriving within a run — the trade that buys a closed fold's check back a thousandfold — and at sixty times under the default budget it decides nothing.
#[test]
#[ignore = "measurement: reports timings over the stored image rather than asserting"]
fn stored_prelude_measurements() {
    // A restore happens once per thread, so a *cold* one needs a thread that has not had one — measuring it on this thread would measure a thread-local read.
    let cold = thread::spawn(|| {
        let start = Instant::now();
        with_prelude(|_| ());

        start.elapsed()
    })
    .join()
    .expect("the restoring thread");

    with_prelude(|prelude| {
        // Averaged, because a single shot at this magnitude is not a measurement: the other three take long enough that one sample says something, and this one does not. The last root's arena is the whole prefix's — see `ersd_clones_are_fresh`.
        const CLONES: u32 = 100;
        let last = prelude.last().expect("the prelude has roots");
        let start = Instant::now();
        for _ in 0..CLONES {
            drop(last.arena());
        }
        let clone = start.elapsed() / CLONES;

        println!("\n=== timings, over the stored images ===");
        println!(
            "  cold restore                 {:>10.1?}   (both roots)",
            cold
        );
        println!(
            "  erased-prefix clone          {:>10.1?}   (mean of {CLONES})",
            clone
        );

        // Per root, against the roots before it: the figures are per *unit*, and the prelude is two of them, so summing them would report a cost no single operation has.
        let mut globals = Globals::default();
        let mut cores = Vec::new();
        let mut arena = ErasedArena::default();

        for root in prelude {
            let core = root.core();
            let name = core
                .mounts
                .first()
                .map(|mount| mount.prefix.join())
                .unwrap_or_else(|| "?".to_string());
            let zonked = Zonked::project(core).expect("a restored prelude root is zonked");

            let mut erasure_context = Context::new(DEFAULT_STEP_BUDGET, SYNTAX);
            let start = Instant::now();
            let erased = erase_unit(
                &mut erasure_context,
                Resumed::of(&cores, arena.clone()),
                &zonked,
            )
            .expect("a stored prelude root re-erases");
            let erasure = start.elapsed();

            let start = Instant::now();
            let (rechecked, kernel) =
                recheck_module_measured(&zonked, DEFAULT_STEP_BUDGET, &globals, SYNTAX);
            let certification = start.elapsed();
            let heaviest = kernel.heaviest_declaration();

            let definitions: usize = core
                .items
                .iter()
                .map(|item| match item {
                    Item::Let(_) => 1,
                    Item::Rec(rec) => rec.definitions().len(),
                })
                .sum();

            println!("\n=== /{name} ===");
            println!("  re-erasing the unit          {:>10.1?}", erasure);
            println!(
                "  certifying the unit          {:>10.1?}  ({} refusals)",
                certification,
                rechecked.verdicts.len()
            );
            println!(
                "  ...heaviest declaration      {:>10} units   (depth {}, costing {} of them)",
                heaviest.units(),
                heaviest.peak_depth(),
                heaviest.frame_units()
            );
            println!("  items                        {:>10}", core.items.len());
            println!("  definitions                  {definitions:>10}");
            println!(
                "  witnesses                    {:>10}",
                core.witnesses.len()
            );
            println!(
                "  inductives                   {:>10}",
                core.induct_decls.len()
            );
            println!(
                "  structures                   {:>10}",
                core.struct_decls.len()
            );
            println!("  concepts                     {:>10}", core.concepts.len());

            globals.mount(core, &rechecked.certification);
            cores.push(core);
            arena = erased;
        }

        println!(
            "\nNot measured here: the elaboration figures, which the build script already writes to `OUT_DIR/profile.tsv` (retake with `cargo build --release --package curios-prelude --features profile`), and the witness inventory, which is a term walk.\n"
        );
    });
}

/// Every mark vector in the restored standard library sits beside a telescope of the same length. The pairing is a construction invariant — `FuncType::new`, `Func::new`, `InductArm::new` and `InductParam::new` assert it — and the archive restores exactly the constructor-built value its build wrote, so this is not a defense the compiler runs but the one place the claim is checked against the real image, once per test run. `curios-cert`'s `sort.rs` slices marks with no guard of its own on the strength of it.
#[test]
fn the_restored_prelude_pairs_every_mark_with_its_binder() {
    with_prelude(|prelude| {
        let drifted = Rc::new(RefCell::new(Vec::new()));
        let inspected = Rc::new(Cell::new(0usize));
        let (sink, counter) = (Rc::clone(&drifted), Rc::clone(&inspected));
        let mut visit = Visit::rewriting(
            |_, _| None,
            Box::new(move |_, term: &Term| {
                let paired = match &**term {
                    Subterm::Func(func) => Some(func.plicities().len() == func.telescope.len()),
                    Subterm::FuncType(func_type) => {
                        Some(func_type.plicities().len() == func_type.telescope.len())
                    }
                    Subterm::Match(Match {
                        cases: Cases::Induct { cases, .. },
                        ..
                    }) => Some(
                        cases
                            .iter()
                            .all(|(_, arm)| arm.plicities().len() == arm.arity()),
                    ),
                    _ => None,
                };
                if let Some(paired) = paired {
                    counter.set(counter.get() + 1);
                    if !paired {
                        sink.borrow_mut().push(term.clone());
                    }
                }
                None
            }),
        );

        for definition in items(prelude).flat_map(Item::definitions) {
            definition.type_.traverse(&mut visit);
            definition.body.traverse(&mut visit);
        }
        for declaration in prelude
            .iter()
            .flat_map(|root| root.core().induct_decls.values())
        {
            for (tag, constructor) in &declaration.constructors {
                assert_eq!(
                    constructor.plicities().len(),
                    constructor.telescope.len(),
                    "constructor {tag} carries a drifted mark vector"
                );
            }
        }

        assert!(inspected.get() > 0, "the walk reached no function nodes");
        assert!(
            drifted.borrow().is_empty(),
            "drifted mark vectors survived into the archive: {:?}",
            drifted.borrow()
        );
    });
}

/// Every prelude definition's finalized universe parameter count, printed rather than asserted: a change to how conversion identifies levels can merge two of a declaration's parameters into one, and the archive build reports only refusals, so a drop is invisible unless it is measured. Take it before and after such a change and diff the two.
#[test]
#[ignore = "inventory: measures the prelude's universe polymorphism rather than asserting it"]
fn universe_parameter_census() {
    with_prelude(|prelude| {
        for item in items(prelude) {
            let described = item.describe();
            for definition in item.definitions() {
                println!(
                    "{described}\t{}",
                    definition.universe_context.parameter_count
                );
            }
        }
    });
}

/// A `/sys` former is levelled by its one argument alone. `List`, `Io`, `Cell` and `Channel` each take `T: Type u` to `Type u`, and the level its body's binder is written at, bounded only by `u`, would be a second parameter every occurrence of the former mints. `UniverseSolver::identify_bounded_choices` identifies a chosen level bounded only by one other chosen level with it, so each takes one.
#[test]
fn every_sys_former_takes_one_universe_parameter() {
    with_prelude(|prelude| {
        let formers = [
            "/sys/List/List",
            "/sys/Io/Io",
            "/sys/Cell/Cell",
            "/sys/Channel/Channel",
        ];
        let counts = items(prelude)
            .flat_map(|item| item.definitions())
            .map(|definition| (definition.name.to_string(), definition))
            .filter(|(name, _)| formers.contains(&name.as_str()))
            .map(|(name, definition)| (name, definition.universe_context.parameter_count))
            .collect::<BTreeMap<_, _>>();

        assert_eq!(
            counts.len(),
            formers.len(),
            "a former is missing: {counts:?}"
        );
        assert!(counts.values().all(|&count| count == 1), "{counts:?}");
    });
}
