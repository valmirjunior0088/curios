//! What one judgment read of other items — the kernel's half of the item graph, filed in each definition's record as [`Reads`].
//!
//! Like the erased positions and the calls, this is the kernel's *output*, collected where the walk consults its environment because that is the only place a read happens: a name's type or universe scheme, a declaration's registry entry, a definition's body. Every other path to another item goes through one of those, a memo included: every memo lives one declaration, so a remembered reduct was computed, and the bodies it unfolds were asked for, within the judgment that hits it.
//!
//! **Behind a cell.** The accessors that read answer through `&Kernel`, because the seams they serve do: the closed machine's `ClosedHost`, the shared analyses' `Env` and `Reducer` all borrow the kernel shared while handing back what they read. Noting a read is the one mutation such a borrow needs, and nothing reads the notes until the item they belong to has been judged.

use {
    curios_core::{Global, Reads},
    std::cell::RefCell,
};

#[derive(Default)]
pub(super) struct ReadRecorder {
    reads: RefCell<Reads>,
}

impl ReadRecorder {
    /// `name`'s type, universe scheme or registry entry was read.
    pub(super) fn signature(&self, name: &Global) {
        let mut reads = self.reads.borrow_mut();
        // Looked up before it is cloned: the same few names are read over and over within one judgment.
        if !reads.signatures.contains(name) {
            reads.signatures.insert(*name);
        }
    }

    /// `name`'s body was asked for.
    pub(super) fn body(&self, name: &Global) {
        let mut reads = self.reads.borrow_mut();
        if !reads.bodies.contains(name) {
            reads.bodies.insert(*name);
        }
    }

    /// Everything read since the last take, leaving nothing behind for the next item.
    pub(super) fn take(&self) -> Reads {
        self.reads.take()
    }
}
