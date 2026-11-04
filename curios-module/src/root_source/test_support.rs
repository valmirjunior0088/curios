//! What a suite asks of a source that no product does: a unit written as Core text.

use super::*;

impl RootSource {
    /// A unit read from Core text: `header`, its one file, mounted at `prefix`, with `source`, the text read from it, logged as read, as an entry's header is (`RootSource::opened`). Nothing lowers the unit, so nothing else is read, and a store verifies it against its text as against any unit's files.
    pub fn core_text(prefix: &str, header: &Path, source: &Arc<Source>) -> Self {
        let opened = header
            .canonicalize()
            .unwrap_or_else(|_| header.to_path_buf());
        let unit = Self::mounted(
            prefix,
            RootKind::Ordinary,
            header,
            Self::namespace_of(header),
        );
        unit.reads.borrow_mut().insert(opened, Arc::clone(source));

        unit
    }
}
