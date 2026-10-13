//! Scaffolding for tests that read what this crate erased: the families a unit's items declare, as each declaring item erased them.
//!
//! Behind the `test-support` feature, so no build that ships holds it. What it hands out is owned and names a family by its spelling, since a family's identity is its fragment's own and means nothing beside another fragment's.

use crate::ErasedUnit;

/// A field of a constructor's row as its declaring item erased it: its shape, and for a field at a family, that family's name.
#[derive(Debug, Clone)]
pub struct ErasedField {
    pub shape: curios_ersd::FieldShape,
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
