mod lower;

#[cfg(test)]
mod tests;

pub use lower::lower as into_cont;

/// A `into_cont` lowering failure: a Curios language restriction earlier stages don't
/// reject syntactically — only closures may be mutually recursive, and a call/match-valued
/// term cannot be bound where a synchronous value is required — reported as an ordinary
/// compile error instead of panicking.
#[derive(Debug)]
pub enum Error {
    /// A term reaching `Apply`/`Match`/`NatMatch` on its construction path was bound where
    /// a synchronous value is required.
    UnsupportedSyncRecItem { term: String },
    /// Two or more non-closure `rec` bindings depend on each other's value.
    CyclicRecComputed { cycle: Vec<String> },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnsupportedSyncRecItem { term } => write!(
                f,
                "`into_cont` does not support a call-valued `rec` item in value position: \
                 the following term reaches `Apply`/`Match`/`NatMatch` on its construction path \
                 but is bound where a synchronous value is required: {term}",
            ),
            Error::CyclicRecComputed { cycle } => write!(
                f,
                "`into_cont` does not support value-level mutual recursion: \
                 {} would require runtime fixpoint cells; only closures may be \
                 mutually recursive (a cyclic tuple/array reaches this path too)",
                cycle.join(" -> "),
            ),
        }
    }
}

/// Shorthand for a lowering result: every fallible step in this module fails with the
/// same [`Error`].
type LowerResult<T> = Result<T, Error>;
