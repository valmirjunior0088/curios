//! A written site no written name reads: one value, recorded by the lowering, the one stage that resolves every written name, and read by elaboration, which decides the binder lints from it beside what the proofs it writes read, as an [`Unresolved`](crate::Unresolved) is made by the one stage and reported by the other.
//!
//! **Each declaration's own** ([`Mints::unread`](crate::Mints::unread)): a written binder's place means something within its declaration alone, so the lowering hands each declaration's sites beside what it minted, and a declaration a recompile keeps keeps them with what it minted.
//!
//! **A site stands in a text of its unit, as a lowered term's position does** ([`Extent`]): in the declaration it is written in. So a site holds no source, and one a recompile keeps is placed in the text its file is read as now.

use crate::{Extent, Global};

/// One written site no written name reads, with where it stands among its unit's texts: `None` for one written nowhere, as a name the compiler spelled is.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub enum Unread {
    /// A written binder nothing referenced, standing at its word: what a proof the elaborator writes may still read, by `binder`.
    Binder {
        name: String,
        at: Option<Extent>,
        binder: WrittenBinder,
    },
    /// A written `_`, standing at it: a slot no written name reads, which a proof the elaborator writes may still read, by `binder`, and which binds what its class says.
    Wildcard {
        at: Option<Extent>,
        binder: WrittenBinder,
    },
    /// A `let` whose value runs no action and none of whose binders a written name referenced, standing at its pattern: one that does nothing where no proof the elaborator writes reads a binder of it either, by `binders`. `name` is what its pattern names where it is one binder with a name.
    Let {
        name: Option<String>,
        at: Option<Extent>,
        binders: Vec<WrittenBinder>,
    },
}

impl Unread {
    /// Where it stands among its unit's texts.
    pub fn at(&self) -> Option<Extent> {
        match self {
            Unread::Binder { at, .. } | Unread::Wildcard { at, .. } | Unread::Let { at, .. } => *at,
        }
    }

    /// Every written binder the site is about.
    pub fn binders(&self) -> &[WrittenBinder] {
        match self {
            Unread::Binder { binder, .. } | Unread::Wildcard { binder, .. } => {
                std::slice::from_ref(binder)
            }
            Unread::Let { binders, .. } => binders,
        }
    }

    /// The site with every binder of the definition `name` re-keyed to `renamed`, as a witness is named anew by every lowering.
    pub fn renamed(&self, name: &Global, renamed: Global) -> Self {
        let mut site = self.clone();
        let binders = match &mut site {
            Unread::Binder { binder, .. } | Unread::Wildcard { binder, .. } => {
                std::slice::from_mut(binder)
            }
            Unread::Let { binders, .. } => binders.as_mut_slice(),
        };
        for binder in binders {
            if binder.declaration.as_ref() == Some(name) {
                binder.declaration = Some(renamed);
            }
        }
        site
    }
}

/// A written binder, by the definition it is written in and its place among that definition's written binders: what a local opened from it carries into elaboration (`Free::local_written`), and what a later lowering of the declaration unchanged gives it too — so one key names it to the elaborator's credits and to a recompile over a baseline. Neither half is a counter another compilation shares.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord))]
pub struct WrittenBinder {
    /// `None` for the term a program closes with, which declares nothing.
    pub declaration: Option<Global>,
    pub ordinal: u32,
}

/// What a written binder binds, read off the form that binds it: what a question about a place answers it as, where it is bound and where it is read.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Binds {
    /// A local, answered as one: a binder a term, a parameter list or a telescope's parameters bind.
    Local,
    /// A structure's field, answered as the field.
    Field,
    /// A tuple type's field, answered as a field of no structure.
    TupleField,
    /// A concept's method, answered as the method's declaration.
    Method,
}

impl WrittenBinder {
    /// This binder re-keyed to `renamed` where it is written in the definition `name`: a witness's, whose name is given anew by every lowering.
    pub fn renamed(&self, name: &Global, renamed: Global) -> Self {
        match self.declaration.as_ref() == Some(name) {
            true => Self {
                declaration: Some(renamed),
                ..*self
            },
            false => *self,
        }
    }
}
