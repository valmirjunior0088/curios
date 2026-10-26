//! Scaffolding for tests that read what erasure made: the families a unit's items declare, as each declaring item erased them, and the digest of every part a unit's items are read by.
//!
//! Behind the `test-support` feature, so no build that ships holds it. What it hands out is owned and names a family by its spelling, since a family's identity is its fragment's own and means nothing outside it.

use {
    crate::ErasedUnit,
    curios_elab::{Parts, Question, part_digest},
};

/// The digest of every part of every declaration `module` holds where `places` keeps it, of how an item naming each name reads `erased`'s form of it, and of that form whole, each beside the question it answers: what a unit records of a read. A unit held in hand and the same unit restored from a store are read alike by a unit compiled over either only where these agree.
pub fn part_digests(
    module: &curios_core::Module,
    places: &curios_core::Places,
    erased: &ErasedUnit,
) -> Vec<(Question, Result<Option<String>, String>)> {
    module
        .items
        .places()
        .flat_map(|place| module.items.names(place).into_iter().copied())
        .flat_map(|name| Parts::ALL.questions(name))
        .map(|question| {
            let digest = match (
                question,
                question.subject().and_then(|name| erased.place(name)),
            ) {
                (Question::Erased(name), Some(at)) => erased.item(at).read_digest(&name).map(Some),
                (Question::Ran(_), Some(at)) => erased.form_digest(at).map(Some),
                (Question::Erased(_) | Question::Ran(_), None) => Ok(None),
                (part, _) => part_digest(module, places, &part),
            };

            (question, digest)
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
