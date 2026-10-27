//! What a test reads of the frames and no judgment does: every local, the base frame's among them.

use {
    super::Frames,
    curios_core::{Free, Term},
};

impl Frames<'_> {
    /// Every frame's locals in binding order (outermost first), the base frame's top-level names first: what a test reads of a declaration forgotten everywhere, where a judgment reads [`Frames::local_binders`].
    pub fn locals(&self) -> impl Iterator<Item = &(Free, Term)> {
        self.read_scope();
        self.base
            .locals
            .iter()
            .chain(self.top.iter().flat_map(|top| top.locals.iter()))
            .map(|(_, local)| local)
    }
}
