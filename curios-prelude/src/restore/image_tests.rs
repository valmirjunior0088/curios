//! What the stored images hold of themselves: each archive validating, a truncated one refused, each image's record of what it read and what precedes it, the digests a record sits ahead of, the record a root's establishing walk filed, and what the stored image costs.

use {
    super::{test_support::*, *},
    curios_cert::certify_definitions,
    curios_common::{Qualifier, digest},
    curios_core::{Item, Size, Zonked},
    curios_elab::DEFAULT_BUDGET,
    curios_erase::{Erasures, erase_unit},
    curios_module::SYNTAX,
    curios_unit::{Record, segments},
    std::{collections::BTreeSet, path::PathBuf, thread, time::Instant},
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
    let authored = crate::tests::authored()
        .into_iter()
        .map(|path| {
            path.canonicalize()
                .expect("an authored source canonicalizes")
        })
        .collect::<BTreeSet<_>>();

    let [_, (std, _)] = restore_archives();
    let recorded = std
        .reads
        .iter()
        .map(|(path, _)| PathBuf::from(path))
        .collect::<BTreeSet<_>>();

    assert_eq!(recorded, authored);
}

/// The record ahead of each image digests exactly the index behind it, as a slot's record digests the index it was filed with.
#[test]
fn each_record_digests_the_index_it_sits_ahead_of() {
    for (root, bytes, _) in ROOTS {
        let (record, unit) = segments(bytes).unwrap_or_else(|| panic!("/{root} is a stored unit"));
        let record = curios_archive::from_bytes::<Record>(record)
            .unwrap_or_else(|error| panic!("/{root} record restores: {error}"));

        assert_eq!(record.unit, digest(unit));
    }
}

/// **What a compilation believes of the prelude is what the establishing walk concluded, asked of the restored bytes.** The build walks each root with the kernel and panics on the first refusal, then images the unit beside the record that walk filed (`curios-prelude/compile.rs`); every compilation mounts those bytes and never walks them again ([the soundness board](../../../documentation/design/soundness/the-soundness-board.md)'s second boundary), so the three things a mount reads off the record are asked here and nowhere else.
///
/// The record must **cover** its unit: `Globals::partial` reads a classification only where it does, so a root whose record named fewer definitions than the root holds would have every proof in scope of it classified afresh from its terms, every group among them read as one no walk typed. It must file a **vector for every former**: positivity composes through the vector the certifier filed, an entry with none answers `Mixed`, and `file_scope_polarities` would recompute the library's on every compile. And the unit must carry no **positional identity**, the rule the build asserts on the value before it is imaged (`validate_stored_identities`), asked again over what was restored, since an index two compilations can both hand out aliases silently rather than failing.
#[test]
fn every_root_restores_the_record_its_establishing_walk_filed() {
    with_prelude(|prelude| {
        for root in prelude {
            let (core, certification) = (root.core(), root.certification());
            curios_core::validate_stored_identities(core, root.mounts())
                .unwrap_or_else(|found| panic!("a restored root carries {found}"));
            assert!(
                certification.covers(core),
                "a restored root's record classifies fewer definitions than the root declares",
            );
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
                Some((root, *name))
            })
            .expect("the prelude declares /std/List/any");
        let applied = root
            .certification()
            .applications(&any)
            .and_then(|applications| applications.of(2))
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

/// The costs of the stored prelude, taken over the image: the figures `curios-package`'s README and `curios-elab`'s budget floors cite.
///
/// Kept in-tree so a figure is retaken by running it rather than by writing a probe again: a figure nobody can cheaply reproduce decays into a claim about history.
///
/// # How to read it
///
/// Numbers are only comparable to the recorded ones in `--release`, because these are the figures taken over the *stored* image and a debug build measures a different program:
///
/// ```sh
/// cargo test --release --package curios-prelude -- --ignored --nocapture stored_prelude_measurements
/// ```
///
/// It asserts nothing and cannot fail — a measurement that fails is a measurement with an opinion. What it does not cover is stated where it prints: the elaboration figures come from the stream `cargo xtask profile prelude` files and need no probe, and the witness inventory is a term walk this does not do.
///
/// A range is two runs, and the spread between them is part of the figure.
///
/// # What it last printed
///
/// The latest reading of each row, taken on top of `9c543476a` in release on `x86_64-unknown-linux-gnu`, at a load average under two, over two runs of one host. Kept here rather than in a document that cites this test, so a number cannot drift from the thing that would check it.
///
/// | What | Measured |
/// | --- | --- |
/// | Cold restore — bytecheck, then deserializing the prepared Text state, the Core, the items' erased forms and the certifier's record | 19.3–19.8 ms |
/// | Re-erasing `/std` whole over the stored Core | 2.6–2.7 s |
/// | Certifying `/std` whole | 6.7–6.8 s, 0 refusals |
/// | Heaviest declaration that certification makes | 980 868 units, peak depth 17 |
///
/// The heaviest declaration's figure counts the closed machine re-deriving within a run — the trade that buys a closed fold's check back a thousandfold — and at thirty times under the default budget it decides nothing.
#[test]
#[ignore = "measurement, mixed: reports timings and counts over the stored image rather than asserting"]
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
        println!("\n=== timings, over the stored images ===");
        println!(
            "  cold restore                 {:>10.1?}   (both roots)",
            cold
        );

        // Per root, against the roots before it: the figures are per *unit*, and the prelude is two of them, so summing them would report a cost no single operation has.
        let mut cores = Vec::new();
        let mut places = Vec::new();
        let mut records = Vec::new();
        let mut units = Vec::new();

        for (index, root) in prelude.iter().enumerate() {
            let core = root.core();
            let name = root
                .mounts()
                .first()
                .map(|mount| mount.prefix.join())
                .unwrap_or_else(|| "?".to_string());
            let zonked = Zonked::project(core).expect("a restored prelude root is zonked");

            // Certified, then erased through the kernel that certified it, as the build makes the root.
            let globals = certified(&prelude[..index]);
            let start = Instant::now();
            let (rechecked, mut kernel) =
                certify_definitions(&zonked, DEFAULT_BUDGET, &globals, SYNTAX);
            let certification = start.elapsed();
            let heaviest = kernel.heaviest_declaration();

            let start = Instant::now();
            erase_unit(
                &mut kernel,
                Erasures::of(&cores, &places, &records, &units),
                &zonked,
            )
            .expect("a stored prelude root re-erases");
            let erasure = start.elapsed();

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
                core.witnesses().len()
            );
            println!(
                "  inductives                   {:>10}",
                core.induct_decls().len()
            );
            println!(
                "  structures                   {:>10}",
                core.struct_decls().len()
            );
            println!(
                "  concepts                     {:>10}",
                core.concepts().len()
            );

            cores.push(core);
            places.push(root.places());
            records.push(root.certification());
            units.push(root.erased());
        }

        println!(
            "\nNot measured here: the elaboration figures, which `cargo xtask profile prelude` files, and the witness inventory, which is a term walk.\n"
        );
    });
}
