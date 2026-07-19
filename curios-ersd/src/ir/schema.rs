//! Product schemas, variant families, and constructors.
//!
//! Structures and inductives register their post-erasure shape here exactly
//! once, from their declarations: a product schema is the ordered row of
//! relevant fields, a variant family is the ordered set of its constructors,
//! and a constructor is its payload row plus its position in the family — the
//! position *is* the discriminant. Construction, projection, and matching
//! reference these identities, which is what keeps the semantic shapes legible
//! to the transformations and hands the lowering everything a tag layout needs.

use super::{ConstructorId, FamilyId};

/// The post-erasure field row of a structure (or tuple). Field names are
/// debug metadata only; identity and layout are positional.
#[derive(Debug, Clone)]
pub struct ProductSchema {
    pub debug_name: Option<String>,
    /// One entry per relevant field, in declaration order; the entry is the
    /// field's optional debug name.
    pub fields: Vec<Option<String>>,
}

impl ProductSchema {
    /// The number of fields.
    pub fn width(&self) -> usize {
        self.fields.len()
    }
}

/// The post-erasure shape of an inductive type: its constructors in
/// declaration order. A constructor's index in this list is its discriminant.
#[derive(Debug, Clone)]
pub struct VariantFamily {
    pub debug_name: Option<String>,
    pub constructors: Vec<ConstructorId>,
}

/// One constructor of a [`VariantFamily`]: its payload row and a back-link to
/// its family. The discriminant is the constructor's position in the family's
/// list, not stored here.
#[derive(Debug, Clone)]
pub struct Constructor {
    pub debug_name: Option<String>,
    pub family: FamilyId,
    /// One entry per relevant payload field, in declaration order; the entry
    /// is the field's optional debug name.
    pub fields: Vec<Option<String>>,
}

impl Constructor {
    /// The number of payload fields.
    pub fn width(&self) -> usize {
        self.fields.len()
    }
}
