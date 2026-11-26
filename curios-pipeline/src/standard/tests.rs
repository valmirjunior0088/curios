//! Which unit takes an archived root's place, what it is granted, and what it is offered; the library compiled from its own terms as a report prints them; and `/test`, the unit a test program's tail is closed over.

use {
    super::*,
    crate::{test_support::*, tests::test_support::*, *},
    curios_abi::{TEST_REPORT, WireReturn, WireType},
    curios_common::{Qualifier, RootKind, test_support::*},
    curios_core::{Definition, Global, Item, Places, Term},
    curios_elab::{DEFAULT_BUDGET, Error, Established, Reading, test_support::*},
    curios_erase::test_support::*,
    curios_module::{Overlay, RootSource, SYNTAX, UnitSource, into_core_unit, into_core_unit_over},
    curios_prelude::{root_prefixes, with_prelude},
    curios_prelude_build::{std_directory, std_source},
    curios_surface::{
        Label, LetSignature, ReaderPosition, Spellable, Spelling, TopItem, TopLet, TopMod, WordAt,
        build_rename, display_names,
    },
    curios_unit::{Predecessors, Unit},
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        fs,
        path::{Path, PathBuf},
        rc::Rc,
    },
};

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

/// Whether the standard library compiles to one unit: its tree compiled whole twice in one process, the two stored units compared byte for byte, and where they differ, the parts and the items that do. A check the suite ignores, since each compile is the whole library; a second unit is the fault it is here to name.
#[test]
#[ignore = "check: compiles the standard library whole twice and compares the two stored units byte for byte, minutes a run"]
fn std_unit_reproduction() {
    with_prelude(|prelude| {
        let [sys, _] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);
        let compiled = || {
            let source = std_source();
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

            compile_unit(
                DEFAULT_BUDGET,
                predecessors,
                &SYNTAX,
                &unit,
                &|_| {},
                Telling::default(),
            )
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
        if stored!(first.text().expect("a program's unit holds its lowering"))
            != stored!(second.text().expect("a program's unit holds its lowering"))
        {
            differing.push("the text stage's part".to_string());
        }
        if stored!(first.erased()) != stored!(second.erased()) {
            differing.push("the erased forms".to_string());
        }
        if stored!(first.certification()) != stored!(second.certification()) {
            differing.push("the certifier's record".to_string());
        }
        if traces_stored(first.traces()) != traces_stored(second.traces()) {
            differing.push("what its declarations asked".to_string());
        }
        let erasures = |unit: &Unit| {
            asked_stored(
                unit.erased()
                    .recorded()
                    .get()
                    .expect("what erasing a unit read restores"),
            )
        };
        if erasures(&first) != erasures(&second) {
            differing.push("what its erasures asked".to_string());
        }
        for (one, other) in first.core().items.iter().zip(&second.core().items) {
            if stored!(one) != stored!(other) {
                differing.push(one.describe());
            }
        }

        assert!(
            first.stored().expect("a unit serializes").index()
                == second.stored().expect("a unit serializes").index()
                && stored!(&first) == stored!(&second),
            "two compilations of the standard library's {} items store two units, differing in {differing:?}",
            first.core().items.len()
        );
    });
}

/// Whether the standard library elaborates to one module however far its declarations' binder counters had run: its tree lowered once and elaborated with every declaration's binders counted from its lowering's count and from a thousand above it, the two modules compared by value, and where they differ, the items that do. A check the suite ignores, since each elaboration is the whole library; a module that moves is the fault it is here to name: an order the elaborator puts atoms in reading an identity by its value.
#[test]
#[ignore = "check: elaborates the standard library from two floors of its binder counters and compares the two modules item by item, minutes a run"]
fn std_identity_census() {
    with_prelude(|prelude| {
        let [sys, _] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let source = std_source();
        let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);
        let elaborated = |floor| elaborated_above(&unit, Predecessors::over(&roots), floor);
        let (first, second) = (elaborated(0), elaborated(1_000));

        let differing = first
            .items
            .iter()
            .zip(second.items.iter())
            .filter(|(one, other)| one != other)
            .map(|(one, _)| one.describe())
            .collect::<Vec<_>>();
        assert!(
            first == second,
            "two floors elaborate two modules of {} items, differing in {differing:?}",
            first.items.len()
        );
    });
}

/// Whether the standard library elaborates to one module however long its tables keep an answer: its tree lowered once and elaborated with every table cleared at every boundary, and with each answer kept for as long as what it read stands, the two modules compared by value, and where they differ, the items that do. A check the suite ignores, since each elaboration is the whole library; a module that moves is an answer kept past something it read (`documentation/design/soundness/a-reduction-step-costs-what-it-builds.md`).
#[test]
#[ignore = "check: elaborates the standard library with every table cleared at every boundary and with each answer kept as long as what it read stands, and compares the two modules item by item, minutes a run"]
fn std_memo_census() {
    with_prelude(|prelude| {
        let [sys, _] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let source = std_source();
        let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);
        let wholesale = elaborated_wholesale(&unit, Predecessors::over(&roots));
        let kept = elaborated_above(&unit, Predecessors::over(&roots), 0);

        let differing = wholesale
            .items
            .iter()
            .zip(kept.items.iter())
            .filter(|(one, other)| one != other)
            .map(|(one, _)| one.describe())
            .collect::<Vec<_>>();
        assert!(
            wholesale == kept,
            "keeping an answer moves the module of {} items, in {differing:?}",
            wholesale.items.len()
        );
    });
}

/// Whether the standard library compiled over a baseline is the unit it is compiled whole: for a leaf edit and for a hub, its edited tree compiled whole and over the archived unit, the two compared as each is stored. A check the suite ignores, since each edit compiles the library whole; a second unit is the fault it is here to name.
#[test]
#[ignore = "check: for a leaf edit and a hub, compiles the edited standard library whole and over the archive and compares the two stored units byte for byte, minutes a run"]
fn std_recompile_reproduction() {
    let directory = std_directory();

    with_prelude(|prelude| {
        let [sys, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);

        for Edit { file, edit, .. } in edits() {
            let path = directory.join(file);
            let text = edit(&fs::read_to_string(&path).expect("an authored source"));
            let source = std_source().with_overlay(Overlay::of([(path, text)]));
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

            let whole = compile_unit(
                DEFAULT_BUDGET,
                predecessors,
                &SYNTAX,
                &unit,
                &|_| {},
                Telling::default(),
            )
            .expect("the edited library compiles");
            let over = compile_unit_over(
                DEFAULT_BUDGET,
                predecessors,
                &SYNTAX,
                &unit,
                Over {
                    baseline: std,
                    moved: &[false],
                    candidates: None,
                },
                &|_| {},
                Telling::default(),
            )
            .expect("the edited library recompiles");

            assert_stored_alike(&whole, &over);
        }
    });
}

/// Whether the standard library, an edit to it undone, takes each declaration the edit made again from what a cabinet filed of it before: for a leaf edit and for a hub, the library compiled whole and filed, the edit compiled over it and filed, and the library's own tree compiled over the edit with the cabinet's candidates, compared as stored with the first, and every verdict it holds one a filing completed, none judged again. A check the suite ignores, since each edit compiles the library whole; a unit other than the first, or a verdict judged again, is the fault it is here to name.
#[test]
#[ignore = "check: for a leaf edit and a hub, files the standard library and the edit, undoes the edit over the cabinet's candidates, and compares the result with the first unit byte for byte, minutes a run"]
fn std_undo_reproduction() {
    let directory = std_directory();

    with_prelude(|prelude| {
        let [sys, _] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);
        let source = std_source();
        let first = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);
        let original = compile_unit(
            DEFAULT_BUDGET,
            predecessors,
            &SYNTAX,
            &first,
            &|_| {},
            Telling::default(),
        )
        .expect("the library compiles");

        for Edit { label, file, edit } in edits() {
            let mut cabinet = Cabinet::default();
            cabinet.file(&original);
            let path = directory.join(file);
            let text = edit(&fs::read_to_string(&path).expect("an authored source"));
            let edited = std_source().with_overlay(Overlay::of([(path, text)]));
            let unit = UnitSource::mounted(&edited).seeing(vec![Qualifier::from(["sys"])]);
            let edited = compile_unit_over(
                DEFAULT_BUDGET,
                predecessors,
                &SYNTAX,
                &unit,
                Over {
                    baseline: &original,
                    moved: &[false],
                    candidates: None,
                },
                &|_| {},
                Telling::default(),
            )
            .expect("the edited library recompiles");
            cabinet.file(&edited);

            let undone = compile_unit_over(
                DEFAULT_BUDGET,
                predecessors,
                &SYNTAX,
                &first,
                Over {
                    baseline: &edited,
                    moved: &[false],
                    candidates: Some(&cabinet),
                },
                &|_| {},
                Telling::default(),
            )
            .expect("the library recompiles over the edit");

            let judged = undone
                .certification()
                .records()
                .filter(|(_, record)| !record.completed())
                .map(|(name, _)| *name)
                .collect::<Vec<_>>();
            assert_eq!(judged, Vec::new(), "{label}: judged again");
            assert_stored_alike(&original, &undone);
        }
    });
}

/// Whether every part of the standard library digests alike held in hand and restored from a cabinet: what a unit compiled over the library records of a read, of its own or of another unit, is asked again by its digest, and a unit restored is read alike only where the two agree ([`part_digests`]); and so does what the certifier completes each judgment's record with, its own digest of each part read ([`complete_records`]). A check the suite ignores, since it compiles the library whole.
#[test]
#[ignore = "check: compiles the standard library whole, stores and restores it, and compares the digest of every part, binding and form the two hold, and how each completes its certifier's records, minutes a run"]
fn std_part_digest_census() {
    with_prelude(|prelude| {
        let [sys, _] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let source = std_source();
        let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);
        let held = compile_unit(
            DEFAULT_BUDGET,
            Predecessors::over(&roots),
            &SYNTAX,
            &unit,
            &|_| {},
            Telling::default(),
        )
        .expect("the library compiles");
        let stored = restored(&held);
        let digests = |unit: &Unit| {
            part_digests(
                unit.core(),
                unit.places(),
                unit.certification(),
                unit.erased(),
            )
        };
        let (now, then) = (digests(&held), digests(&stored));

        let differing = now
            .iter()
            .zip(&then)
            .filter(|(now, then)| now != then)
            .map(|((question, _), _)| format!("{question:?}"))
            .collect::<Vec<_>>();
        // The certifier's own digests: each record completed over the unit held in hand and over the one restored, which was stored before either was completed.
        complete_records(&held);
        complete_records(&stored);
        let judgments = |unit: &Unit| {
            unit.certification()
                .records()
                .map(|(name, record)| (*name, record.made().cloned()))
                .collect::<Vec<_>>()
        };
        let (judged_now, judged_then) = (judgments(&held), judgments(&stored));
        let moved = judged_now
            .iter()
            .zip(&judged_then)
            .filter(|(now, then)| now != then)
            .map(|((name, _), _)| *name)
            .collect::<Vec<_>>();
        assert_eq!(now.len(), then.len(), "the two hold the same parts");
        assert!(
            differing.is_empty(),
            "a part digests otherwise restored: {differing:?}"
        );
        assert_eq!(
            judged_now.len(),
            judged_then.len(),
            "the two record the same judgments"
        );
        assert!(
            moved.is_empty(),
            "a judgment's reads digest otherwise restored: {moved:?}"
        );
    });
}

/// Whether a declaration the prover's vocabulary comes to read is elaborated again with no lemma of the library on offer, as a whole compile elaborates it. `/std/Eq/sym`, a lemma of the vocabulary, is made to name `/std/Str/At/skip`, whose proofs the prover writes: compiled whole, `skip` is then asked for before the lemmas are offered. Compiled over the archive, `skip`'s text stands and its trace says the lemmas were on offer, so the walk elaborates it again where it now stands. The two compiles answer alike: with one stored unit, or with the same reports. A check the suite ignores, since each compile is the library's; the two answers are one claim.
#[test]
#[ignore = "check: makes a lemma of the prover's vocabulary read a declaration, compiles the standard library whole and over the archive, and compares the two answers, minutes a run"]
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
        let source = std_source().with_overlay(Overlay::of([(path, text)]));
        let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);

        let whole = compile_unit(
            DEFAULT_BUDGET,
            predecessors,
            &SYNTAX,
            &unit,
            &|_| {},
            Telling::default(),
        );
        let over = compile_unit_over(
            DEFAULT_BUDGET,
            predecessors,
            &SYNTAX,
            &unit,
            Over {
                baseline: std,
                moved: &[false],
                candidates: None,
            },
            &|_| {},
            Telling::default(),
        );

        match (whole, over) {
            (Ok(whole), Ok(over)) => assert_stored_alike(&whole, &over),
            (Err(whole), Err(over)) => {
                let (whole, over) = (String::from(whole), String::from(over));
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

/// Each declaration of the standard library, lowered from its sources over the archived `/sys`, is held to its record and to its answers: asked again where only what its record names stands, every name it resolved answers as it did, and lowered where only what its doors answered stands, it lowers to what the library holds. What a recompile keeps of a declaration is what a whole lowering makes exactly where this holds (`PreparedText::census`).
#[test]
fn every_declaration_of_the_library_is_held_to_its_record_and_its_answers() {
    with_prelude(|prelude| {
        let [sys, _] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);
        let source = std_source();
        let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);
        let lowered =
            into_core_unit(&unit, &predecessors.text(), &SYNTAX).expect("the library lowers");

        let departures = lowered.census(&predecessors.text(), &SYNTAX);

        assert!(departures.is_empty(), "{}", departures.join("\n"));
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
    let roots = root_prefixes();
    for claim in [std_source(), std_from_elsewhere(), supplied("std")] {
        let index = stood_in_for(&roots, &[claim]).expect("a package named std");

        assert_eq!(index + 1, roots.len(), "the last root, /std");
        assert_eq!(roots[index], Qualifier::from(["std"]));
    }
}

#[test]
fn a_package_named_otherwise_takes_no_roots_place() {
    assert!(stood_in_for(&root_prefixes(), &[supplied("other")]).is_none());
    assert!(stood_in_for(&root_prefixes(), &[]).is_none());
}

/// Only the last root can be taken, since the roots after a withheld one were compiled against it: a claim on the compiler's own root, which nothing could name anyway, collides with it as any claim does.
#[test]
fn a_package_named_sys_collides_with_the_compilers_own_root() {
    assert!(stood_in_for(&root_prefixes(), &[supplied("sys")]).is_none());

    let error = compile_with_units(&[("sys", "pub let a : /std/Nat = 1;")], "0")
        .expect_err("the compiler's own root is in scope");

    assert!(error.contains("sys"), "unexpected error: {error}");
}

/// A unit after the first has predecessors the archived unit was never compiled against, so it is not the one that takes the root's place — and it collides, as any later claim does.
#[test]
fn only_the_first_unit_can_take_a_roots_place() {
    assert!(stood_in_for(&root_prefixes(), &[supplied("other"), std_source()]).is_none());

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
    let roots = root_prefixes();
    let prefixes = granted_in_place(&roots, roots.len() - 1, &std_source());

    assert!(prefixes.contains(&Qualifier::from(["sys"])));
}

/// Every unit of a fold whose first unit takes a root's place is compiled over the stand-in, and the first sees what the root could see: the sources a payload's probe and its filing name the units by are the ones the fold compiles, so a package named `std` is found where it was filed.
#[test]
fn a_folds_sources_carry_the_stand_in_its_first_unit_takes() {
    let units = [std_source(), supplied("other")];
    let sources = fold_sources(&units);
    let std = [Qualifier::from(["std"])];

    assert_eq!(sources[0].stand_in(), Some(&std[..]));
    assert_eq!(sources[1].stand_in(), Some(&std[..]));
    assert!(
        sources[0]
            .declared()
            .is_some_and(|declared| declared.contains(&Qualifier::from(["sys"])))
    );

    let units = [supplied("other")];
    assert_eq!(fold_sources(&units)[0].stand_in(), None);
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
        let _ = Fold::new(DEFAULT_BUDGET, &units, Some(&recording)).check_units(|_| {});

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

/// The `let`s of the library the translation declines, each holding a form its module cannot write, under their reason and in the order of their names. A report raised in one shows the term as the compiler holds it.
const DECLINED: [&str; 3] = [
    // Each holds a string past the length a report prints one whole.
    "/std/Cli/parse/switch_word",
    "/std/http/Url/part",
    "/std/http/syntax/status_line",
];

/// The `let`s of the library that are refused as a report prints them, under their reason: none.
const REFUSED: [&str; 0] = [];

/// The `let`s of the library whose printed form elaborates and is not proved to be the `let`: none.
const UNPROVED: [&str; 0] = [];

/// The `let`s of the library whose printed form or whose proof elaboration accepts and the certifier refuses: none.
const UNCERTIFIED: [&str; 0] = [];

/// One `#[test]` per part the library is read back in, and the number of parts [`lets_in_runs`] cuts the library's `let`s into, declared from one list so the two cannot disagree.
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
    the_ninth_part_of_the_library_reads_back,
    the_tenth_part_of_the_library_reads_back,
    the_eleventh_part_of_the_library_reads_back,
    the_twelfth_part_of_the_library_reads_back,
    the_thirteenth_part_of_the_library_reads_back,
    the_fourteenth_part_of_the_library_reads_back,
    the_fifteenth_part_of_the_library_reads_back,
    the_sixteenth_part_of_the_library_reads_back,
}

/// Each `let` of `trees` under the part it is read back in: the library's `let`s in the order of their files and of each file's items, cut into as many runs as there are parts, each as heavy as another to within one `let`. A `let` weighs what the read-back does with it, printing its elaborated type and body and elaborating them again, so it weighs the distinct nodes of those two, where a file's bytes say little of it: `/std/Map/internal/Node` is a tenth of the library's text and the most of its proofs. A run of neighbouring `let`s holds few files, so a part lowers, elaborates and walks over the archive little more than the files it writes beside; and the cut is made from the library as it stands, so the parts stay level as it grows.
fn lets_in_runs(
    trees: &[(&PathBuf, &Qualifier, curios_surface::Module)],
    definitions: &BTreeMap<Global, Definition>,
) -> BTreeMap<Global, usize> {
    let mut weighed = Vec::new();
    for (_, module, tree) in trees {
        add_beside_each_let(&mut tree.items.clone(), module, &mut |module, member| {
            let name = Global::Authored(module.with(member.label.as_str()));
            let weight = definitions.get(&name).map_or(1, |definition| {
                definition.type_.distinct_nodes() + definition.body.distinct_nodes()
            });
            weighed.push((name, weight));
            Vec::new()
        });
    }
    let total = weighed
        .iter()
        .map(|(_, weight)| weight)
        .sum::<usize>()
        .max(1);
    let mut before = 0;

    weighed
        .into_iter()
        .map(|(name, weight)| {
            let part = ((before + weight / 2) * PARTS.len() / total).min(PARTS.len() - 1);
            before += weight;
            (name, part)
        })
        .collect()
}

/// **A report's term reads back.** Beside every `let` of the library two declarations are written: its type and its body as a report raised in it prints them, and a proof by `Eq/refl()` that what those declare is the `let`. The files are lowered and elaborated as the library, over the archive, and the certifier walks what comes of it. So each printed term is accepted where its reader stands, and is the term it was printed from by the conversion of each checker, at the universe levels that checker is handed: a printed term states none, and elaboration solves for them. A `let` not known to terminate is named by no type, so what is written for one is that it has its printed type.
///
/// A `let` the translation declines, one naming what its module cannot among them, has nothing written beside it, and each is listed ([`DECLINED`]). One refused as printed, or whose proof is, is found by leaving it out and compiling again, so every `let` that does not read back is named, and not the first alone ([`REFUSED`], [`UNPROVED`]); and the proofs the certifier refuses are listed with the `let`s they are of ([`UNCERTIFIED`]).
///
/// **In parts, and once.** Read back whole the library is one test several times the length of any other, so it is read back a part at a time, its `let`s cut into runs of equal weight ([`lets_in_runs`]), a `#[test]` each ([`read_back_in_parts`]), and the parts run side by side; what a part writes beside its `let`s is elaborated over the archive, which keeps every declaration the source wrote. And a `let` listed as refused or as unproved is left out before the first compile, so a library nothing has changed in is elaborated once: the listed ones are then written alone, where what is refused of them is what is still to be listed.
fn every_let_of_the_library_reads_back_as_a_report_prints_it(part: usize) {
    let files = library_files();
    // Every file of the library, parsed once: a round writes beside the `let`s of a copy.
    let every = files
        .iter()
        .map(|(path, module, text)| {
            let tree = text
                .parse::<curios_surface::Module>()
                .unwrap_or_else(|error| panic!("{} parses: {error:?}", path.display()));

            (path, module, tree)
        })
        .collect::<Vec<_>>();

    with_prelude(|prelude| {
        let [sys, std] = prelude else {
            panic!("the prelude has two roots")
        };
        let roots = [*sys];
        let predecessors = Predecessors::over(&roots);
        let lexicons = predecessors.lexicons();
        let spellings = Rc::new(
            std.text()
                .expect("a program's unit holds its lowering")
                .spellings(&predecessors.text()),
        );
        let reading = Reading {
            definitions: std.core(),
            kept: None,
            scope: &lexicons,
            syntax: &SYNTAX,
            spellings: Some(&spellings),
            minted: None,
            anchors: &[],
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
        let own = lets_in_runs(&every, &definitions)
            .into_iter()
            .filter(|(_, at)| *at == part)
            .map(|(name, _)| name)
            .collect::<BTreeSet<Global>>();
        // This part's files: those holding a `let` of its own.
        let trees = every
            .iter()
            .filter(|(_, module, tree)| {
                let mut holds = false;
                add_beside_each_let(&mut tree.items.clone(), module, &mut |module, member| {
                    holds |= own.contains(&Global::Authored(module.with(member.label.as_str())));
                    Vec::new()
                });
                holds
            })
            .cloned()
            .collect::<Vec<_>>();

        let parsed = |text: &str, owner: Global| {
            text.parse::<curios_surface::Term>()
                .unwrap_or_else(|error| {
                    panic!(
                        "a term written for {} parses:\n{text}\n{error:?}",
                        owner.faithful()
                    )
                })
        };
        // A term as a report raised in `owner` prints it, settled where its reader stands and read back as the tree it parses to: `None` where the translation declines it.
        let established = Established::placed(&[
            (
                sys.core(),
                sys.places(),
                sys.mounts(),
                sys.classifications(),
            ),
            (
                std.core(),
                std.places(),
                std.mounts(),
                std.classifications(),
            ),
        ]);
        let written = |term: &Term, owner: Global| {
            let term = &written_for_reader(&established, SYNTAX, term);
            let spelling = unplaced.clone().for_reader(ReaderPosition {
                owner: Some(owner),
                witnesses: Rc::default(),
                locals: Rc::default(),
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
                        if !own.contains(&name) || !taken(&name) {
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
                        let total = std
                            .traces()
                            .classified(&name)
                            .is_some_and(|classified| classified.totality.is_total());
                        let (stated, by) = match total {
                            true => (
                                parsed(&format!("/std/Eq/Eq()({label}, {printed})"), name),
                                parsed("/std/Eq/Eq/refl()", name),
                            ),
                            false => (type_.clone(), parsed(label, name)),
                        };
                        let declared = |label: String, type_, body| TopLet {
                            word: WordAt::default(),
                            doc: None,
                            core: (),
                            vis_pub: false,
                            label: Label::from(label),
                            signature: LetSignature::Name {
                                type_: Some(type_),
                                body,
                            },
                        };

                        vec![declared(printed, type_, body), declared(same, stated, by)]
                    });

                    ((*path).clone(), tree.printed().to_string())
                })
                .collect::<Vec<_>>();

            let source = std_source().with_overlay(Overlay::of(overlay));
            let unit = UnitSource::mounted(&source).seeing(vec![Qualifier::from(["sys"])]);
            let lowered = into_core_unit_over(
                &unit,
                &predecessors.text(),
                &SYNTAX,
                std.text().expect("a program's unit holds its lowering"),
                true,
            )
            .unwrap_or_else(|error| {
                panic!(
                    "the library lowers with its printed terms beside it:\n{}",
                    error.format()
                )
            });
            let elaborated = elaborated_over(
                predecessors,
                std,
                &lowered,
                lowered.lowered_anew().expect("lowered over a baseline"),
            );

            (elaborated, beside, declined)
        };

        let mut refusals = Vec::new();
        let mut refused_as_printed = BTreeSet::<Global>::new();
        let mut unlike = BTreeSet::<Global>::new();
        // What a refused round says of the `let`s it wrote beside: those whose printed term is refused, and those whose proof alone is.
        let mut refused_of = |error: &Error, beside: &BTreeMap<Global, (Global, bool)>| {
            let failed = refused(error);
            let said = error
                .each()
                .map(|member| clipped(&member.faithful(&SYNTAX).to_string()))
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
        let (recompiled, beside, declined) = loop {
            match round(&|name| !left_out.contains(name)) {
                (Ok(recompiled), beside, declined) => break (recompiled, beside, declined),
                (Err(error), beside, ..) => left_out.extend(refused_of(&error, &beside)),
            }
        };
        // The listed ones, written alone: what is refused of them is what the lists still hold truly.
        if !seeded.is_empty()
            && let (Err(error), beside, ..) = round(&|name| seeded.contains(name))
        {
            refused_of(&error, &beside);
        }

        // The certifier's own walk: a declaration the source wrote stands on the verdict filed with the archive, and each one written beside it is judged.
        let finished = &recompiled.definitions;
        let places = recompiled
            .places
            .clone()
            .unwrap_or_else(|| Places::of(finished));
        let globals = crate::globals_after(predecessors, &[false]);
        let (judged, _) = recheck_over(
            finished,
            &places,
            DEFAULT_BUDGET,
            &globals,
            Over {
                baseline: std,
                moved: &[false],
                candidates: None,
            },
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
            .refusals()
            .iter()
            .filter_map(|verdict| beside.get(&verdict.name?))
            .map(|(name, _)| *name)
            .collect::<BTreeSet<_>>();
        // Everything the sweep found is said at once: a run costs the library over again.
        let mut found = Vec::new();
        let (known_refused, known_unproved, known_declined, known_uncertified) = (
            listed(&REFUSED),
            listed(&UNPROVED),
            listed(&DECLINED),
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
        if uncertified != known_uncertified || uncertified.len() != judged.refusals().len() {
            found.push(format!(
                "of the {} declarations written beside a `let` the certifier refuses {}, those of {:?}, where {:?} are the `let`s of this part known to be refused theirs:\n{}",
                beside.len(),
                judged.refusals().len(),
                names(&uncertified),
                names(&known_uncertified),
                judged
                    .refusals()
                    .iter()
                    .map(|verdict| {
                        format!(
                            "{}: {}",
                            verdict
                                .name
                                .map_or_else(String::new, |name| name.faithful().to_string()),
                            clipped(&verdict.error.rendered(&Spelling::new(&SYNTAX)))
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            ));
        }
        assert!(found.is_empty(), "{}", found.join("\n\n"));
    });
}

/// `/test`, compiled over the prelude as a test compilation compiles it, declares the scheduler the synthesized tail calls, [`SYNTAX`]'s `test.main`, and the row its runner answers, [`TEST_REPORT`], at the types each runner answers it at: one byte string a record, and nothing back.
#[test]
fn the_runner_declares_the_scheduler_a_test_tail_calls_and_the_row_its_runner_answers() {
    with_prelude(|prelude| {
        let unit = runner(DEFAULT_BUDGET, Predecessors::over(prelude)).expect("`/test` compiles");
        let declared = unit
            .core()
            .items
            .iter()
            .flat_map(Item::declared_names)
            .collect::<BTreeSet<_>>();
        assert!(declared.contains(&Global::Authored(SYNTAX.test.main.qualifier())));

        let row = unit
            .foreigns()
            .get(TEST_REPORT)
            .expect("`/test` declares the row its runner answers");
        let signature = row.signature();
        assert_eq!(signature.params, [WireType::Bytes]);
        assert!(
            matches!(&signature.returns, WireReturn::Answers(results) if results.types().is_empty())
        );
    });
}
