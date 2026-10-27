//! Scaffolding for tests that read what erasure made: the families a unit's items declare, as each declaring item erased them, and the digest of every part a unit's items are read by.
//!
//! Behind the `test-support` feature, so no build that ships holds it. What it hands out is owned and names a family by its spelling, since a family's identity is its fragment's own and means nothing outside it.

use crate::{ErasedUnit, NamedPart, Part, declared_digest, erased_digest};

/// The digest of every part of every declaration `module` holds where `places` keeps it, under the record `certification` its walk filed: each signature, body and kind, how an item naming each name reads `erased`'s forms of it, and those forms whole, each beside the name and part it is of. What a unit records of a read: a unit held in hand and the same unit restored from a store are read alike by a unit compiled over either only where these agree.
pub fn part_digests(
    module: &curios_core::Module,
    places: &curios_core::Places,
    certification: &curios_core::Certification,
    erased: &ErasedUnit,
) -> Vec<(NamedPart, Result<Option<String>, String>)> {
    module
        .items
        .places()
        .flat_map(|place| module.items.names(place).into_iter().copied())
        .flat_map(|name| Part::ALL.map(|part| (name, part)))
        .map(|read| {
            let digest = match (read.1, erased.place(&read.0)) {
                (Part::Binding | Part::Forms, Some(at)) => erased_digest(erased, at, read),
                (Part::Binding | Part::Forms, None) => Ok(None),
                _ => declared_digest(module, places, certification, read),
            };

            (read, digest)
        })
        .collect()
}

/// A field of a constructor's row as its declaring item erased it: its shape, and for a field at a family, that family's name.
#[derive(Debug, Clone)]
pub struct ErasedField {
    pub shape: curios_anf::FieldShape,
    pub family: Option<String>,
}

/// A family a unit declares: its name, and each constructor's fields in the declaration's order.
#[derive(Debug, Clone)]
pub struct ErasedFamily {
    pub name: String,
    pub constructors: Vec<Vec<ErasedField>>,
}

/// The families `unit`'s items declare. Each is read off the fragment of the item that declares it, the one place its fields' shapes stand: a fragment that imports a family reads its name and each constructor's width and mask, and no shape.
pub fn declared_families(unit: &ErasedUnit) -> Vec<ErasedFamily> {
    unit.declared_families()
}
