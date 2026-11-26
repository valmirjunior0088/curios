//! What the stored images hold of themselves: each archive validating, a truncated one refused, each image's record of what it read and what precedes it, the digests a record sits ahead of, the record a root's establishing walk filed, and what the stored image costs.

use {
    super::*,
    curios_common::{Qualifier, SourceName, Style, digest},
    curios_core::{Anchors, Global, Item, Size},
    curios_prelude_build::std_directory,
    curios_unit::{Record, segments},
    std::collections::BTreeSet,
};

#[test]
fn embedded_archives_validate() {
    for archived in validate_archives() {
        archived.unwrap();
    }
}

#[test]
fn a_truncated_archive_is_rejected() {
    for (root, bytes, pack) in ROOTS {
        assert!(validate_bytes(root, &bytes[..bytes.len() / 2], pack).is_err());
        assert!(validate_bytes(root, bytes, &pack[..pack.len() / 2]).is_err());
    }
}

/// A place in the image is spelled by its source's name: the image was read in a checkout no compiler carrying it names, so its sources hold their names and no file, and a report placed in the library reads `/std/…` wherever its reader stands.
#[test]
fn a_place_in_the_image_is_spelled_by_its_sources_name() {
    with_prelude(|prelude| {
        let std = prelude.last().expect("the prelude ends in /std");
        let anchors = std.text().expect("/std is written as a program").anchors();
        let reverse = Global::Authored(Qualifier::from(["std", "List", "reverse"]));
        let label = Anchors::label(&[anchors], &reverse).expect("`reverse` is written");

        assert_eq!(label.source.file(), None);
        assert_eq!(label.place(&Style::plain()), "/std/List.crs:129:9");
    });
}

/// `/sys` is supplied whole by the build script — no file is read and nothing precedes it — so its record says so, which is what keeps a source tree from ever claiming to be the one it came from.
#[test]
fn the_sys_image_records_no_reads_and_no_predecessor() {
    let [(sys, _), _] = restore_archives();

    assert!(sys.reads.is_empty());
    assert!(sys.after.is_empty());
}

/// The digest a record names its predecessor by is the one that predecessor's own record names itself by: the chain is checkable from the records alone.
#[test]
fn the_std_image_records_the_sys_image_as_its_predecessor() {
    let [(sys, _), (std, _)] = restore_archives();

    assert_eq!(std.after, [(Qualifier::from(["sys"]).join(), sys.unit)]);
}

/// The read set is closed and every authored module is registered, so the record and the tree agree exactly: the record is the compiler's own account of which sources the image came from.
#[test]
fn the_std_record_names_every_authored_source_and_no_other() {
    let directory = std_directory();
    let authored = crate::tests::authored()
        .into_iter()
        .map(|path| {
            let below = path
                .strip_prefix(&directory)
                .expect("an authored source of `/std`");
            SourceName::of("std", below).expect("a plain path")
        })
        .collect::<BTreeSet<_>>();

    let [_, (std, _)] = restore_archives();
    let recorded = std
        .reads
        .iter()
        .map(|(name, _)| name.clone())
        .collect::<BTreeSet<_>>();

    assert_eq!(recorded, authored);
}

/// No byte of the prelude's images holds the directory they were built in: what an image keeps of a source is its name, so one tree built in two directories is one image, and a compiler carries no directory of the machine that built it.
#[test]
fn no_image_holds_the_directory_it_was_built_in() {
    let built_in = std_directory();
    let built_in = built_in.to_string_lossy();
    let images = ROOTS
        .into_iter()
        .flat_map(|(root, bytes, pack)| [(root, bytes), (root, pack)])
        .chain([("documentation", DOCUMENTATION_BYTES)]);

    for (image, bytes) in images {
        assert!(
            !bytes
                .windows(built_in.len())
                .any(|window| window == built_in.as_bytes()),
            "an image of /{image} holds {built_in}"
        );
    }
}

/// The record ahead of each image digests exactly the index behind it, as an object's record digests the index it was filed with.
#[test]
fn each_record_digests_the_index_it_sits_ahead_of() {
    for (root, bytes, _) in ROOTS {
        let (record, unit) = segments(bytes).unwrap_or_else(|| panic!("/{root} is a stored unit"));
        let record = curios_archive::from_bytes::<Record>(record)
            .unwrap_or_else(|error| panic!("/{root} record restores: {error}"));

        assert_eq!(record.unit, digest(unit));
    }
}

/// **What a compilation believes of the prelude is what the establishing walk concluded, asked of the restored bytes.** The build walks each root with the certifier and panics on the first refusal, then images the unit beside the record that walk filed (`curios-prelude-build/src/compile.rs`); every compilation mounts those bytes and never walks them again ([the soundness board](../../../documentation/design/soundness/the-soundness-board.md)'s second boundary), so the three things a mount reads off the record are asked here and nowhere else.
///
/// The record must **answer for every name** its unit holds: a mounted record is read per name (`curios_cert::Filed`), so a root whose record named fewer definitions than the root holds would fail the first compilation that reads one it misses. It must file a **vector for every former**: positivity composes through the vector the certifier filed, read per name the same way. And the unit must carry no **positional identity**, the rule the build asserts on the value before it is imaged (`validate_stored_identities`), asked again over what was restored, since an index two compilations can both hand out aliases silently rather than failing.
#[test]
fn every_root_restores_the_record_its_establishing_walk_filed() {
    with_prelude(|prelude| {
        for root in prelude {
            let (core, certification) = (root.core(), root.certification());
            curios_core::validate_stored_identities(core, root.mounts())
                .unwrap_or_else(|found| panic!("a restored root carries {found}"));
            for place in core.items.places() {
                for name in core.items.names(place) {
                    assert!(
                        certification.totality(&name).is_some(),
                        "a restored root's record classifies no {}",
                        name.faithful()
                    );
                }
            }
            for (name, _) in core.formers() {
                assert!(
                    certification.polarities(&name).is_some()
                        && certification.variances(&name).is_some(),
                    "a restored root's record files no vector for {}",
                    name.faithful()
                );
            }
        }
    });
}

/// **The record files how a list function applies its callback.** `/std/List/any` hands `pred` an element of `l`, and a recursion through a callback handed to it is graded by that: every application of `pred` its record files, its own and the one reached through its recursion, grades the element below `l`. Asked of the restored bytes, which every compilation reads it from.
#[test]
fn the_std_record_files_that_any_applies_its_predicate_below_its_list() {
    with_prelude(|prelude| {
        let (root, any) = prelude
            .iter()
            .find_map(|root| {
                let name = root
                    .core()
                    .items
                    .iter()
                    .flat_map(Item::declared_names)
                    .find(|name| name.faithful().to_string() == "/std/List/any")?;
                Some((root, name))
            })
            .expect("the prelude declares /std/List/any");
        let applied = root
            .certification()
            .applications(&any)
            .and_then(|applications| applications.part(&[])?.of(2))
            .expect("/std/List/any's record files how it applies `pred`");

        assert!(
            !applied.is_empty()
                && applied
                    .iter()
                    .all(|application| application.entry(1, 0) == Size::Less),
            "{applied:?}"
        );
    });
}
