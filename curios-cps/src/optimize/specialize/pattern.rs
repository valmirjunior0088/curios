//! One constructor-pattern vocabulary shared by call and join specialization.

use {
    crate::{Atom, RowId, ValueId},
    std::collections::BTreeMap,
};

pub type TaggedRows = BTreeMap<ValueId, (u32, Vec<Atom>, RowId)>;

/// A known nominal constructor in one argument position; equal widths and tags from different rows never share a specialization.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstructorPattern {
    pub index: usize,
    pub tag: u32,
    pub width: usize,
    pub row: RowId,
}

impl ConstructorPattern {
    pub fn fields<'a>(&self, args: &[Atom], constructors: &'a TaggedRows) -> Option<&'a [Atom]> {
        let Atom::Value(value) = args.get(self.index)? else {
            return None;
        };
        let (tag, fields, row) = constructors.get(value)?;
        (*tag == self.tag && fields.len() == self.width && *row == self.row).then_some(fields)
    }
}
