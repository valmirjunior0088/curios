//! What a suite does to the answers and no host does: a row entered by a trampoline of its own.
//!
//! Behind the `test-support` feature, so no build that ships holds it.

use super::*;

impl Answers {
    /// Replace how the row declared as `name` is entered with a trampoline that marshals for itself — how a test puts a reply in front of the guest that no checked answer would give.
    pub(crate) fn replace_raw<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&mut Caller<'_, Guest>, &[Val], &mut [Val]) -> wasmtime::Result<()>
            + Send
            + Sync
            + 'static,
    {
        self.rows
            .get_mut(name)
            .unwrap_or_else(|| panic!("nothing here answers `{name}`"))
            .1 = Entered::now(Arc::new(f));
    }
}
