//! What a test reads of the context's scope and no judgment does: every local, the base frame's among them.

use {
    crate::Context,
    curios_core::{Free, Term},
};

impl Context<'_> {
    /// [`Frames::locals`](crate::context::Frames::locals).
    /// Every frame's locals, the base's among them: what a test reads of a declaration forgotten everywhere, where every judgment reads [`Context::local_binders`].
    pub fn locals(&self) -> impl Iterator<Item = &(Free, Term)> {
        self.typing.frames.locals()
    }
}
