//! What the prelude's printed trees hold: each declaration's tree against the graph it shares, the library's restored declarations read whole.

use {
    curios_core::{Enter, Term},
    curios_prelude::{test_support::*, with_prelude},
    std::collections::HashMap,
    xbench::{Measure, read},
};

/// The nodes of `term`'s tree, saturating, and the distinct nodes of its graph.
fn tree_and_graph(term: &Term) -> (u128, usize) {
    let mut sizes: HashMap<Term, u128> = HashMap::new();
    let tree = term.walk(
        &mut sizes,
        |sizes, term| match sizes.get(term) {
            Some(&size) => Enter::Skip(size),
            None => Enter::Descend,
        },
        |sizes, term, children| {
            let size = children.fold(1u128, u128::saturating_add);
            sizes.insert(term.clone(), size);
            size
        },
    );

    (tree, sizes.len())
}

/// How far a term's tree outgrows its graph across the prelude: what `curios-core`'s print allowance is set above, so that no declaration the standard library prints is elided.
///
/// # How to take it
///
/// `cargo xbench measure printed_tree_measurements` takes it: `terms` and `terms past the floor`, and for each of the eight terms whose tree outgrows its graph the most, over all terms and over those past the floor, `largest/<term>/tree` and `/graph`, or `largest past the floor/…`, the nodes of its tree and the distinct nodes of its graph, the first over the second its ratio. A print builds about one document per node of its term's tree, so an allowance of that many documents per distinct node, set well above the largest ratio here, elides none of them.
///
/// # What it last printed
///
/// At `84eff3c90`: 4 898 terms, none with a tree past 5 000 nodes. The largest ratios:
///
/// | Tree over graph | Tree | Graph | Term |
/// | --- | --- | --- | --- |
/// | 10.9 | 1 065 | 98 | `/std/Bytes/euclid`, body |
/// | 10.0 | 2 873 | 286 | `/std/Str/Valid/encoded`, body |
/// | 8.9 | 688 | 77 | `/std/Command/capture`, body |
/// | 8.7 | 2 449 | 281 | `/std/Str/advanced`, body |
/// | 8.4 | 652 | 78 | `/std/Str/At/next`, body |
pub fn printed_tree_measurements() {
    const FLOOR: u128 = 5_000;

    with_prelude(|prelude| {
        let mut rows = Vec::new();
        for item in items(prelude) {
            let described = item.describe();
            for definition in item.definitions() {
                for (part, term) in [("type", &definition.type_), ("body", &definition.body)] {
                    let (tree, graph) = tree_and_graph(term);
                    rows.push((tree, graph, described.clone(), part));
                }
            }
        }

        let past = rows.iter().filter(|(tree, ..)| *tree > FLOOR).count();
        read("terms", Measure::Count, None, rows.len() as u64);
        read("terms past the floor", Measure::Count, None, past as u64);

        // Tree over graph, descending, compared without dividing.
        rows.sort_by(|left, right| (right.0 * left.1 as u128).cmp(&(left.0 * right.1 as u128)));
        let largest = rows.iter().take(8).map(|row| ("largest", row));
        let largest_past = rows
            .iter()
            .filter(|(tree, ..)| *tree > FLOOR)
            .take(8)
            .map(|row| ("largest past the floor", row));
        for (set, (tree, graph, described, part)) in largest.chain(largest_past) {
            let term = format!("{set}/{described} ({part})");
            read(
                &format!("{term}/tree"),
                Measure::Count,
                None,
                u64::try_from(*tree).unwrap_or(u64::MAX),
            );
            read(
                &format!("{term}/graph"),
                Measure::Count,
                None,
                *graph as u64,
            );
        }
    });
}
