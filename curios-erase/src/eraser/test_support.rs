//! What a suite reaches of the eraser beyond its entry points: a unit's items erased through a reader the suite holds, so it can set the reader up first or read what the kernel answered after.

use {super::unit::erase_items, crate::*, curios_cert::Reader};

/// `module`'s items erased through `reader` with nothing in scope and no baseline: the walk `erase_unit` runs inside the kernel's read, with the reader handed to the suite.
pub fn erase_through(
    reader: &mut Reader,
    module: &curios_core::Module,
    places: &curios_core::Places,
) -> Result<ErasedUnit, Error> {
    erase_items(reader, Erasures::nothing(), (module, places), None)
}
