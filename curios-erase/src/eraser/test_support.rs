//! What a suite reaches of the eraser beyond its entry points: a unit's items erased through a reader the suite holds, so it can set the reader up first or read what the certifier answered after.

use {super::unit::erase_items, crate::*, curios_cert::Reader};

/// `module`'s items erased through `reader` with nothing in scope and no baseline: the walk `erase_unit` runs inside the certifier's read, with the reader handed to the suite. No record of the unit's verdicts is handed with it, so whether what an item's evaluation leaves is observable is decided with none of its functions marked total.
pub fn erase_through(
    reader: &mut Reader,
    definitions: &curios_core::Definitions,
    places: &curios_core::Places,
) -> Result<ErasedUnit, Error> {
    erase_items(
        reader,
        Erasures::nothing(),
        (definitions, places, &curios_core::Certification::default()),
        None,
    )
}
