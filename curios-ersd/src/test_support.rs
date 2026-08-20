//! Assertions over the door's sequence-usage census, for tests that pin a field's settling.
//!
//! A namespace rather than a root export, for `curios-runtime`'s `test_support` reason: nothing here collides with anything, but `curios_ersd::test_support::census_settles_constructor_field` says at its use site that the caller reached for scaffolding rather than product API. The census's admission is spelling-sensitive by necessity — a spread of a field's value is structurally indistinguishable from the builder-accumulator cliff — so a respelling can silently unmark a field, and the cost surfaces only in a profile. This surface is the loud guard: a test states the verdict it relies on, by name, and a breaking respelling fails it instead of quietly regressing.

use {
    super::{Module, into_cont::sequence_census},
    curios_utilities::ArenaId,
};

/// Whether the census marks `constructor`'s field indexed-only — the verdict under which the door settles every store into it. Names are the schema's debug names — the family's is its qualified spelling (`/std/Map/Node`, `/Box` for an entrypoint's own) — and an unknown name panics with the known ones listed, since a silent `false` here would defeat the loudness this surface exists for.
pub fn census_settles_constructor_field(
    module: &Module,
    family: &str,
    constructor: &str,
    field: &str,
) -> bool {
    let family_id = module
        .families()
        .iter()
        .position(|candidate| candidate.debug_name.as_deref() == Some(family))
        .unwrap_or_else(|| {
            panic!(
                "no family named `{family}`; known: {:?}",
                module
                    .families()
                    .iter()
                    .filter_map(|f| f.debug_name.as_deref())
                    .collect::<Vec<_>>(),
            )
        });

    let (constructor_id, row) = module
        .constructors()
        .iter()
        .enumerate()
        .find(|(_, candidate)| {
            candidate.family.index() == family_id
                && candidate.debug_name.as_deref() == Some(constructor)
        })
        .unwrap_or_else(|| {
            panic!(
                "family `{family}` has no constructor named `{constructor}`; known: {:?}",
                module
                    .constructors()
                    .iter()
                    .filter(|c| c.family.index() == family_id)
                    .filter_map(|c| c.debug_name.as_deref())
                    .collect::<Vec<_>>(),
            )
        });

    let position = row
        .fields
        .iter()
        .position(|candidate| candidate.debug_name.as_deref() == Some(field))
        .unwrap_or_else(|| {
            panic!(
                "constructor `{constructor}` has no field named `{field}`; known: {:?}",
                row.fields
                    .iter()
                    .filter_map(|f| f.debug_name.as_deref())
                    .collect::<Vec<_>>(),
            )
        });

    sequence_census(module).indexed_only_constructor(ArenaId::from_index(constructor_id), position)
}
