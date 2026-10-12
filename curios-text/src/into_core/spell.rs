//! Resolution in reverse: the name a reader writes to reach a declaration from where they stand.
//!
//! It stands beside the resolver because it is the resolver's inverse. Every spelling [`spell`] gives is one the lowering resolves to that declaration from the reader's position, and no shorter one is: a differential over the library puts each to the lowering's own doors (`curios-pipeline`'s `spelling_tests`), so a door resolution gains or loses is one this file is held to.

use curios_utilities::Qualifier;

/// The shortest spelling that resolves to `global` for a reader in module `island`, written inside `owner` (the entrypoint's final term when `None`): its bare label or a path through a child module, where the reader's own module declares the way there; an import in scope there, as it was written; a path through a module in scope there, from the label it was imported under down; or an absolute path whose audience includes the reader. `None` when the reader can reach it by none of those — a name only a refusal shows, spelled faithfully by the caller.
pub fn spell(
    spellings: &curios_core::Spellings,
    island: &Qualifier,
    owner: Option<&curios_core::Global>,
    global: &curios_core::Global,
) -> Option<String> {
    let seen = spellings
        .units
        .iter()
        .flat_map(|unit| {
            let paths = unit.paths.get(global).into_iter().flatten();

            paths.map(move |written| (written, unit.own))
        })
        .filter(|(written, _)| written.audience.iter().any(|root| island.is_within(root)))
        .collect::<Vec<_>>();
    // Through a module a `use` brought into scope: its label, then the path from that module down. A head an import put in scope is vetted where the `use` is written and nowhere after, so a path under a prefix the reader may not name is written this way and no other.
    let through = spellings
        .imports
        .modules_at(owner)
        .flat_map(|(module, label)| {
            seen.iter()
                .filter(move |(written, _)| {
                    written.path.is_within(module) && written.path != *module
                })
                .map(move |(written, _)| {
                    let below = &written.path.segments()[module.segments().len()..];

                    format!("{label}/{}", below.join("/"))
                })
        })
        .collect::<Vec<_>>();

    let mut reachable = seen
        .into_iter()
        .filter(|(written, _)| {
            !spellings
                .closed
                .iter()
                .any(|prefix| written.path.is_within(prefix))
        })
        .collect::<Vec<_>>();
    // A declaration's own path, then each path it is exported at, by path, whichever unit gives it: a reader takes the first of two spellings equally short.
    reachable.sort_by_key(|(written, _)| {
        (
            curios_core::Global::Authored(written.path) != *global,
            written.path,
        )
    });

    let mut candidates = Vec::new();
    for (written, own) in reachable {
        if own && written.path.is_within(island) && written.path != *island {
            candidates.push(written.path.segments()[island.segments().len()..].join("/"));
        }
        candidates.push(written.path.join());
    }
    candidates.extend(
        spellings
            .imports
            .in_scope_at(owner)
            .filter(|imported| imported.global == *global)
            .map(|imported| imported.spelling()),
    );
    candidates.extend(through);

    candidates
        .into_iter()
        .min_by_key(|spelling| (spelling.split('/').count(), spelling.len()))
}
