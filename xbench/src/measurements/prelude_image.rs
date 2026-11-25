//! What the stored prelude costs to restore and to certify again, and what its images hold.

use {
    crate::{Measure, phase, read},
    curios_cert::certify_definitions,
    curios_core::Item,
    curios_elab::DEFAULT_BUDGET,
    curios_erase::{Erasures, erase_unit},
    curios_module::SYNTAX,
    curios_prelude::{test_support::*, with_prelude},
    std::thread,
};

/// The costs of the stored prelude, taken over the image: the figures `curios-package`'s README and `curios-elab`'s budget floors cite.
///
/// # How to read it
///
/// `cargo xbench measure stored_prelude_measurements` takes it: `cold restore`, both roots restored on a thread that had restored neither, and for each root `<root>/re-erasing` and `<root>/certifying`, each step's time, `<root>/refusals`, `<root>/heaviest declaration/units`, `/depth` and `/frame units`, and `<root>/items`, `/definitions`, `/witnesses`, `/inductives`, `/structures` and `/concepts`.
///
/// It asserts nothing and cannot fail — a measurement that fails is a measurement with an opinion. What it does not cover: the elaboration figures come from the stream `cargo xtask profile prelude` files and need no probe, and the witness inventory is a term walk this does not do.
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
pub fn stored_prelude_measurements() {
    // A restore happens once per thread, so a *cold* one needs a thread that has not had one — measuring it on this thread would measure a thread-local read.
    let cold = thread::spawn(|| phase(&[], || with_prelude(|_| ())))
        .join()
        .expect("the restoring thread");

    with_prelude(|prelude| {
        cold.read("cold restore", None);

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
            // Certified, then erased through the certifier that accepted it, as the build makes the root.
            let globals = certified(&prelude[..index]);
            let certification = phase(&[], || {
                certify_definitions(core, DEFAULT_BUDGET, &globals, SYNTAX)
            });
            certification.read(&format!("{name}/certifying"), None);
            let (rechecked, mut certifier) = certification.answer;
            let heaviest = certifier.heaviest_declaration();
            let accepted = rechecked
                .accepted()
                .expect("a stored prelude root is certified");

            phase(&[], || {
                erase_unit(
                    &mut certifier,
                    Erasures::of(&cores, &places, &records, &units),
                    accepted,
                )
                .expect("a stored prelude root re-erases")
            })
            .read(&format!("{name}/re-erasing"), None);

            let definitions: usize = core
                .items
                .iter()
                .map(|item| match item {
                    Item::Let(_) => 1,
                    Item::Rec(rec) => rec.definitions().len(),
                    Item::Nominal(nominals) => nominals.entries().len(),
                })
                .sum();

            for (figure, count) in [
                ("refusals", rechecked.refusals().len() as u64),
                ("heaviest declaration/units", heaviest.units()),
                ("heaviest declaration/depth", heaviest.peak_depth() as u64),
                ("heaviest declaration/frame units", heaviest.frame_units()),
                ("items", core.items.len() as u64),
                ("definitions", definitions as u64),
                ("witnesses", core.witnesses().len() as u64),
                ("inductives", core.induct_decls().len() as u64),
                ("structures", core.struct_decls().len() as u64),
                ("concepts", core.concepts().len() as u64),
            ] {
                read(&format!("{name}/{figure}"), Measure::Count, None, count);
            }

            cores.push(core);
            places.push(root.places());
            records.push(root.certification());
            units.push(root.erased());
        }
    });
}
