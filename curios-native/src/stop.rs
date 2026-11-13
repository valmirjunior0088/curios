//! How a program that could not go on stopped.

use {
    crate::Unlinked,
    std::{error::Error, fmt},
};

/// A program's stop, as whoever ran it says it on one line: the tag a reader knows the kind of stop by, and what there is to say of this one.
///
/// **One line, and no frame of the machine.** A stop is the program's own, said alike by the compiler's `run`, by a test and by a built executable with no compiler beside it. What was running when it stopped is machine code, whose frames name nothing the program wrote.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stop {
    pub tag: &'static str,
    pub said: String,
}

impl Stop {
    /// A stop of the kind `tag` names, of which `said` is said.
    pub fn of(tag: &'static str, said: impl fmt::Display) -> Self {
        Self {
            tag,
            said: said.to_string(),
        }
    }
}

impl fmt::Display for Stop {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.said.is_empty() {
            true => formatter.write_str(self.tag),
            false => write!(formatter, "{}: {}", self.tag, self.said),
        }
    }
}

impl From<Unlinked> for Stop {
    /// What could not be linked where a run begins stops the run before its first step, under the refusal's own tag.
    fn from(unlinked: Unlinked) -> Self {
        Self {
            tag: unlinked.tag,
            said: unlinked.facts.join("; "),
        }
    }
}

/// A stop a host operation raises while the program runs, carried out of the wasm call as a trap and read back where the run ends.
#[derive(Debug)]
pub struct Raised(pub Stop);

impl fmt::Display for Raised {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

impl Error for Raised {}

/// The trap that stops the run as the kind `tag` names, saying `said`.
pub fn raised(tag: &'static str, said: impl fmt::Display) -> wasmtime::Error {
    wasmtime::Error::from(Raised(Stop::of(tag, said)))
}
