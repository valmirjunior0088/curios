//! A spelling as a copyable identity: interned once per process, so a name that carries one — a binder's display hint — is `Copy` and can cross a thread.

#[cfg(test)]
mod tests;

use {
    super::{Table, intern},
    std::{
        cmp::Ordering,
        collections::HashMap,
        fmt,
        hash::{Hash, Hasher},
        ptr,
        sync::{LazyLock, Mutex},
    },
};

/// A spelling interned once per process.
///
/// Equal spellings are one allocation, so equality is an address comparison; ordering and hashing read the spelling, so both come out the same in every process.
#[derive(Clone, Copy)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct Symbol {
    /// Archived as the bare spelling, and read back through the process table — see [`InternedText`].
    #[archived_with(InternedText)]
    text: &'static String,
}

/// Every spelling this process has interned, keyed by the spelling itself, so looking one up allocates nothing — which matters because every binder either checker mints with a hint asks. See the `interner` module for why nothing is freed.
static TEXTS: LazyLock<Table<str, String>> = LazyLock::new(|| Mutex::new(HashMap::new()));

/// The one allocation of `text`.
fn interned(text: &str) -> &'static String {
    intern(&TEXTS, text, || text.to_string(), String::as_str)
}

impl Symbol {
    /// The symbol spelling `text`.
    pub fn new(text: &str) -> Self {
        Self {
            text: interned(text),
        }
    }

    /// The spelling.
    pub fn as_str(&self) -> &'static str {
        self.text.as_str()
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.text, other.text)
    }
}

impl Eq for Symbol {}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> Ordering {
        match ptr::eq(self.text, other.text) {
            true => Ordering::Equal,
            false => self.text.cmp(other.text),
        }
    }
}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.text.hash(state);
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.text, formatter)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.text)
    }
}

/// Reads an archived spelling back into the process table.
#[cfg(feature = "archive")]
pub struct InterningText;

/// The field adapter: `#[archived_with(InternedText)]`.
#[cfg(feature = "archive")]
pub type InternedText = curios_archive::Via<InterningText>;

#[cfg(feature = "archive")]
impl curios_archive::Proxy<&'static String> for InterningText {
    type Archivable = String;

    /// Borrowed, never cloned: the archived form is the `String` the table already holds.
    fn to_archivable(text: &&'static String) -> impl std::borrow::Borrow<String> {
        *text
    }

    fn from_archivable(text: String) -> Result<&'static String, String> {
        Ok(interned(&text))
    }
}
