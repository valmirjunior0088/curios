//! Whether a unit is reused across two compilations: unchanged, edited, under other claims, damaged, its blobs gone, or reaching an edited unit through one stored as it was.

use {
    super::test_support::*,
    crate::verdicts::*,
    curios_common::{Qualifier, fill, test_support::*},
    curios_core::{Anchors, Certification, Global},
    curios_module::{identity, open_entrypoint},
    curios_package::{Governing, order},
    curios_pipeline::{Fold, Progress},
    std::{cell::RefCell, collections::BTreeMap, path::Path},
};

/// Three packages, each declaring the one below it and nothing further: `mid`'s `g` calls `base`'s `x`, and `top` proves that `g(0)` is `1`, which unfolds both.
fn chained(name: &str, body: &str) -> Temporary {
    let root = temporary(name);
    let manifest = |name: &str, below: Option<&str>| {
        let dependency = below.map_or(String::new(), |below| {
            fill!(
                "\n[dependencies]\n${below} = { path = \"../${below}\" }\n",
                below
            )
        });

        fill!(
            concat!(
                "compiler = \"",
                env!("CARGO_PKG_VERSION"),
                "\"\nname = \"${name}\"\n${dependency}"
            ),
            name,
            dependency
        )
    };

    write(&root, "base/curios.toml", &manifest("base", None));
    write(&root, "base/lib.crs", &base_library(body));
    write(&root, "mid/curios.toml", &manifest("mid", Some("base")));
    write(
        &root,
        "mid/lib.crs",
        "use /std/{Nat};\nuse /base/{x};\n\npub let g(n: Nat) -> Nat = x(n);\n",
    );
    write(&root, "top/curios.toml", &manifest("top", Some("mid")));
    write(
        &root,
        "top/lib.crs",
        "use /std/{Eq};\nuse /mid/{g};\n\npub let p: Eq()(g(0), 1) = Eq/refl();\n",
    );
    write(&root, "exe.crs", "use /std/{Io};\n\nIo/pure(())\n");

    root
}

/// The same, for a compilation reading through `overlay`, verified by [`Verdicts::get_overlaid`] as the `wonder` engine's is.
fn reused_overlaid(root: &Path, overlay: &Overlay) -> bool {
    let verdicts = Verdicts::at(own(root));

    reused_through(root, &root.join("shape"), &Overlaid(&verdicts, overlay))
}

/// The cabinet read and written through, noting the certifier's record of each mounted unit filed into it and of each one handed back. The unit of the entry's own declarations passes through unnoted: the cabinet is told of no executable here, so it files none.
struct Recorded<'a> {
    verdicts: &'a Verdicts,
    filed: RefCell<Vec<Certification>>,
    taken: RefCell<Vec<Certification>>,
}

impl Cache for Recorded<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        let unit = Cache::get(self.verdicts, source)?;
        if !source.is_entry() {
            self.taken.borrow_mut().push(unit.certification().clone());
        }
        Some(unit)
    }

    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        if !source.is_entry() {
            self.filed.borrow_mut().push(unit.certification().clone());
        }
        Cache::put(self.verdicts, source, unit);
    }
}

/// The cabinet, keeping the last unit it handed back.
struct Taking<'a> {
    verdicts: &'a Verdicts,
    taken: RefCell<Option<Unit>>,
}

impl Cache for Taking<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        let hit = Cache::get(self.verdicts, source);
        if let Some(unit) = &hit {
            *self.taken.borrow_mut() = Some(unit.clone());
        }

        hit
    }

    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        Cache::put(self.verdicts, source, unit);
    }
}

/// What a build of `root`'s `top` through the cabinet did to each unit, or what it was refused with.
fn folded_chain(root: &Path) -> Result<Vec<String>, String> {
    let verdicts = Verdicts::at(own(root));
    let governing = Governing::of(&root.join("top"), None).expect("a governed package");
    let library = order(&governing).expect("a resolvable library");
    let (entrypoint, loader, _source) =
        open_entrypoint(&root.join("exe.crs"), None).expect("an openable entrypoint");

    let mut events = Vec::new();
    let compiled = Fold::new(1_000_000, &library, Some(&verdicts)).compile(
        &entrypoint,
        &loader,
        |_| {},
        |progress| match progress {
            Progress::Compiling { unit: prefix, .. } => {
                events.push(format!("compiling {}", prefix.join()))
            }
            Progress::Recompiling { unit: prefix, .. } => {
                events.push(format!("recompiling {}", prefix.join()));
            }
            Progress::Reused(prefix) => events.push(format!("reused {}", prefix.join())),
            _ => {}
        },
    );

    compiled.map(|_| events).map_err(String::from)
}

/// A unit taken back from an object carries the record the certifier filed beside it, every entry whole — its totality and what judging it read — so a later walk reads the verdicts on its definitions from the cabinet rather than from what elaboration concluded, and the reads a recompile over it invalidates along.
#[test]
fn a_reused_unit_brings_its_certification_back() {
    let root = project("certified");
    let shape = root.join("shape");
    // A handle per compilation, as every caller opens one: the chain it places is the fold's that used it.
    let recorded = |verdicts| Recorded {
        verdicts,
        filed: Default::default(),
        taken: Default::default(),
    };
    let (first, second) = (Verdicts::at(own(&root)), Verdicts::at(own(&root)));
    let (filing, taking) = (recorded(&first), recorded(&second));

    assert!(
        !reused_through(&root, &shape, &filing),
        "nothing is stored for the first compile"
    );
    assert!(reused_through(&root, &shape, &taking));

    let (filed, taken) = (filing.filed.into_inner(), taking.taken.into_inner());
    assert_eq!(taken, filed);
    let message = Global::Authored(Qualifier::from(["shape", "message"]));
    let [record] = &taken[..] else {
        panic!("one unit taken back, not {}", taken.len());
    };
    assert!(
        record
            .reads(&message)
            .is_some_and(|reads| !reads.signatures.is_empty() || !reads.bodies.is_empty()),
        "the record came back without what judging `message` read"
    );
}

/// A unit taken back from an object lies in its package's files: what was stored of each source is its name, and the cabinet restores the unit for the source asking, whose package stands on this machine, so a place in it reads as a place in a unit compiled here does.
#[test]
fn a_reused_unit_lies_in_its_packages_files() {
    let root = project("filed");
    let shape = root.join("shape");
    let verdicts = Verdicts::at(own(&root));
    let taking = Taking {
        verdicts: &verdicts,
        taken: RefCell::new(None),
    };

    assert!(!reused(&root), "nothing is stored for the first compile");
    assert!(reused_through(&root, &shape, &taking));

    let unit = taking.taken.into_inner().expect("the unit was taken back");
    let anchors = unit.text().expect("a program's unit").anchors();
    let message = Global::Authored(Qualifier::from(["shape", "message"]));
    let label = Anchors::label(&[anchors], &message).expect("`message` is written");

    assert_eq!(
        label.source.name().map(|name| name.as_str()),
        Some("/shape/lib.crs")
    );
    assert_eq!(
        label.source.file().map(identity),
        Some(identity(&shape.join("lib.crs")))
    );
}

/// The point of the thing: source that has not changed is not compiled again.
#[test]
fn an_unchanged_unit_is_reused() {
    let root = project("unchanged");

    assert!(!reused(&root), "nothing is stored for the first compile");
    assert!(reused(&root), "and the second finds what the first filed");
}

/// Filing a unit writes into `.curios/`, which sits inside the directory a package's library reads from, so an address hashed over that tree would miss forever and grow the cabinet a directory per compile. What a unit was compiled from is recorded and verified rather than addressed, and a generated file is not something it was compiled from.
#[test]
fn writing_into_the_cabinet_does_not_invalidate_it() {
    let root = project("cabinet-writes");

    reused(&root);
    assert!(reused(&root), "the cabinet's own writes are not source");

    write(&root, ".curios/unrelated", "not a source file");
    assert!(reused(&root), "and neither is anything else under it");
}

/// A compilation reading through an overlay is verified against the text it would read: a file the unit was compiled from hits while the editor's text is the disk's and misses once it differs, and an open file the unit never read — the executable beside a package's library, in the directory the library reads from — is no reason to compile the library again. A containment guess, refusing the hit whenever any open document lies under the unit's directory, would cost the language server the whole library on every keystroke in a program file.
#[test]
fn an_overlaid_compilation_is_verified_by_the_text_it_would_read() {
    let root = project("overlaid");
    let held = |path: &str, text: String| Overlay::of(BTreeMap::from([(root.join(path), text)]));

    reused(&root);
    assert!(reused(&root), "stored by the first compile");

    assert!(
        reused_overlaid(&root, &held("shape/lib.crs", library("first"))),
        "an open file holding the text on disk is the text the unit was compiled from"
    );
    assert!(
        !reused_overlaid(&root, &held("shape/lib.crs", library("second"))),
        "and one the editor has changed is not"
    );
    assert!(
        reused_overlaid(
            &root,
            &held("shape/exe.crs", "-- a program beside the library".into())
        ),
        "a file the unit never read leaves the hit standing, wherever it lies"
    );
}

/// An object is addressed without its contents, so it is the *verification* that has to notice an edit. This is the half that would still pass if the record were never checked.
#[test]
fn an_edited_unit_is_not_reused() {
    let root = project("edited");

    reused(&root);
    assert!(reused(&root), "stored by the first compile");

    write(&root, "shape/lib.crs", &library("second"));
    assert!(!reused(&root), "and refused once its source differs");
}

/// A unit is reused only under the claims its package made when it was compiled. A manifest whose provider's `answers` table has changed since is a disagreement like an edited source, so what it claims now is held to the unit's declarations again, where an object that still answered would vouch for claims nobody checked.
#[test]
fn a_unit_is_not_reused_under_other_claims() {
    const ROW: &str = concat!(
        "compiler = \"",
        env!("CARGO_PKG_VERSION"),
        "\"\nname = \"shape\"\n\n[foreign.probe.native]\npath = \"probe.wasm\"\nhash = \"f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\"\n"
    );
    let root = project("claims");
    let library = format!(
        "{}\npub foreign one(Nat) -> Nat;\n\npub foreign two(Nat) -> Nat;\n",
        library("first")
    );

    write(&root, "shape/lib.crs", &library);
    write(
        &root,
        "shape/curios.toml",
        &format!("{ROW}\n[foreign.probe.answers]\none = \"one\"\n"),
    );
    reused(&root);
    assert!(reused(&root), "stored by the first compile");

    write(
        &root,
        "shape/curios.toml",
        &format!("{ROW}\n[foreign.probe.answers]\none = \"one\"\ntwo = \"two\"\n"),
    );
    assert!(
        !reused(&root),
        "and refused once its package claims otherwise"
    );
    assert!(reused(&root), "then stored under the claims it has now");
}

/// The unit's own digest is in its record, so a unit damaged after it was filed is a miss rather than a belief — bytecheck confirms its structure and nothing about its contents, and a flipped byte inside a string would otherwise read back as a different string.
#[test]
fn a_damaged_unit_is_not_reused() {
    let root = project("damaged");

    reused(&root);
    assert!(reused(&root), "filed by the first compilation");

    let unit = own(&root).unit_files().pop().expect("one object");
    let mut bytes = fs::read(&unit).unwrap();
    let last = bytes.len() - 1;
    bytes[last] ^= 0xff;
    fs::write(&unit, &bytes).unwrap();

    assert!(!reused(&root), "damaged bytes are not the bytes filed");
    assert!(reused(&root), "and the recompile repairs the object");
}

/// An entry's unit rests on the units it could have read, as any unit does: an edit to its dependency compiles it over the unit it filed.
#[test]
fn an_edited_dependency_recompiles_the_entry_over_its_unit() {
    let root = project("entry-after");
    write(&root, "exe.crs", DECLARING_ENTRY);

    assert_eq!(entered(&root, "exe.crs", Some("exe")), "compiled");
    write(&root, "shape/lib.crs", &library("second"));
    assert_eq!(entered(&root, "exe.crs", Some("exe")), "recompiled");
    assert_eq!(entered(&root, "exe.crs", Some("exe")), "reused");
}

/// An object names blobs it does not hold: one whose blobs are gone is not restored, and the compile that misses writes them again.
#[test]
fn a_unit_whose_blobs_are_gone_is_compiled_again() {
    let root = project("blobs-gone");
    reused(&root);
    assert!(reused(&root), "filed by the first compilation");

    let written = packs(&root);
    assert!(!written.is_empty(), "the packs a filing wrote");
    for pack in written {
        fs::remove_file(pack).expect("a pack a filing wrote");
    }

    assert!(!reused(&root), "an object naming blobs nowhere is no unit");
    assert!(reused(&root), "and the recompile writes them again");
}

/// A pack is held to its name where it is read: a damaged one is taken away, the unit it held blobs of is compiled again, and the filing that follows writes them again, as the very pack they were, since a pack is a function of what it holds.
#[test]
fn a_damaged_pack_is_taken_away_and_written_again() {
    let root = project("damaged-pack");
    reused(&root);
    let [pack] = &packs(&root)[..] else {
        panic!("one pack after one filing");
    };
    let filed = fs::read(pack).unwrap();
    let mut bytes = filed.clone();
    bytes[0] ^= 0xff;
    fs::write(pack, &bytes).unwrap();

    assert!(!reused(&root), "a damaged pack's blobs are not believed");
    assert_eq!(
        fs::read(pack).unwrap(),
        filed,
        "and the recompile writes the pack again whole"
    );
    assert!(reused(&root), "and what it wrote is restored");
}

/// A hit rests on every unit its unit could have read, the ones those could read among them. `base`'s `x` respelled to add two leaves `mid` stored as it was, `g` calling `x` by its signature alone; `top`, which declares `mid` and nothing of `base`, unfolded `x` through `g` to prove `g(0)` is `1`, so it is no hit, and is refused as a build with nothing stored refuses it.
///
/// Mutation-checked: with a unit's reach left without the units its own reach holds, `base` and `mid` are compiled again, `top` is reused, and the build admits `p` with `g(0)` at `2`.
#[test]
fn a_unit_reaching_an_edited_one_through_a_unit_stored_as_it_was_is_not_reused() {
    let root = chained("chained", "n + 1");
    folded_chain(&root).expect("the chain builds");
    assert!(
        folded_chain(&root)
            .expect("the chain builds")
            .contains(&"reused /top".to_string()),
        "the control stopped holding: an unchanged chain is reused"
    );

    write(&root, "base/lib.crs", &base_library("n + 2"));

    // Each refusal names the file it stands in, under a directory of its own.
    let refused = |root: &Path| {
        folded_chain(root)
            .expect_err("`g(0)` is `2`")
            .replace(&root.display().to_string(), "<root>")
    };
    let fresh = chained("chained-fresh", "n + 2");
    assert_eq!(
        refused(&root),
        refused(&fresh),
        "a build over the cabinet answers what a build with nothing stored answers"
    );
}
