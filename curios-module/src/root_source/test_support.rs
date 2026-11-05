//! What a suite asks of a source that no product does: a unit of one file whose text is in hand, written as a program or as Core text.

use super::*;

impl RootSource {
    /// A unit of one file: `header`, mounted at `prefix`, with `source`, the text read from it, logged as read, as an entry's header is (`RootSource::opened`), so a store verifies the unit against its text before anything is lowered, and a record filed for it names the file. A unit read from Core text is lowered by nothing, so nothing else is read; a program's lowering reads the same file again.
    pub fn one_file(prefix: &str, header: &Path, source: &Arc<Source>) -> Self {
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
