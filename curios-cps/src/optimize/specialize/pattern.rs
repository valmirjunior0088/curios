//! One constructor-pattern vocabulary shared by call and join specialization.

use {
    crate::{Atom, NodeId, RowId, ValueId},
    std::collections::{BTreeMap, BTreeSet},
};

pub type TaggedRows = BTreeMap<ValueId, (u32, Vec<Atom>, RowId)>;

/// What a pattern specializer plans of one body it may copy, before it copies any: the binding node its definition joins, the extent a copy reproduces and what that weighs, and the parameters the body reads a row slot of.
pub struct PatternRoot {
    pub intro: NodeId,
    pub extent: BTreeSet<NodeId>,
    pub weight: usize,
    pub projected: BTreeSet<ValueId>,
}

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
