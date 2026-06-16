use {
    crate::{Span, parser::ParserError},
    std::{fmt, io, path::PathBuf},
};

#[derive(Debug)]
pub enum Error {
    UnresolvedQualifier {
        qualifier: String,
    },
    ModuleNotFound {
        path: String,
    },
    ChildModuleNotFound {
        segment: String,
    },
    PrivateChildModule {
        segment: String,
    },
    /// A root module reachable only from the standard library (e.g. `sys`) was
    /// referenced from user code. Such modules are the trusted primitive
    /// substrate; user code reaches them through their `/std` wrappers.
    InternalRootModule {
        segment: String,
    },
    BindingNotFound {
        binding: String,
    },
    PrivateBinding {
        binding: String,
    },
    QualifierConflict {
        qualifier: String,
    },
    BindingConflict {
        label: String,
    },
    NotAModule {
        label: String,
        parent: String,
    },
    NotABinding {
        label: String,
        parent: String,
    },
    NoSuchUseTarget {
        label: String,
        parent: String,
    },
    DuplicatePublicDeclaration {
        label: String,
    },
    ExportConflict {
        label: String,
    },
    CyclicReExport {
        label: String,
    },
    /// A postfix `!` appeared outside any `let !` body, so there is no bind
    /// function to sequence it with.
    BangWithoutBind,
    /// The annotated motive form `(x : T(...)) => P` is only meaningful on a
    /// union scrutinee — `Bln` and `Nat` matches take `: P` or `: (x) => P`.
    AnnotatedMotiveNotUnion,
    /// One match-arm column mixes constructor patterns with scalar literal
    /// patterns (`cons(…)` and `0` under the same scrutinee position) — the
    /// pattern-matrix compiler cannot dispatch on a column of inconsistent kind.
    PatternColumnConflict,
    /// A `_`/binder fallthrough sits at a union column whose unlisted
    /// constructors must be materialized, but `tag` (an explicit constructor in
    /// the same column) does not resolve to a known union — bring the
    /// constructors into scope (`use …`) or list every constructor explicitly.
    UnresolvedMatchConstructor {
        tag: String,
    },
    /// A constructor pattern `tag(…)`'s argument count disagrees with the
    /// constructor's declared payload arity.
    PatternArityMismatch {
        tag: String,
        expected: usize,
        found: usize,
    },
    /// A scalar (`Nat`/`Bln`) literal column is not total: a `Nat` column needs a
    /// `_`/binder catch-all for its default, and a `Bln` column needs both
    /// `true` and `false` or a catch-all.
    NonExhaustiveScalarMatch,
    /// An annotated (indexed) motive `(x : T(...)) => P` was written on a match
    /// whose arms nest refutable patterns. The matrix compiler synthesizes the
    /// inner matches with inferred motives, so it cannot thread an index-dependent
    /// motive through them — keep the dependent match single-level.
    DependentMatrixMotive,
    ModuleLoadFailed {
        label: String,
        cause: Box<LoadError>,
    },
    Located {
        span: Span,
        error: Box<Error>,
    },
}

impl Error {
    pub fn at(self, span: Span) -> Self {
        match self {
            Self::Located { .. } => self,
            error => Self::Located {
                span,
                error: Box::new(error),
            },
        }
    }

    pub fn format(&self) -> String {
        match self {
            Self::Located { span, error } => {
                format!("{error}\n\n{}", span.render_snippet())
            }
            error => error.to_string(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnresolvedQualifier { qualifier } => {
                write!(f, "unresolved qualifier: {qualifier}")
            }
            Error::ModuleNotFound { path } => write!(f, "module not found: {path}"),
            Error::ChildModuleNotFound { segment } => {
                write!(f, "child module not found: {segment}")
            }
            Error::PrivateChildModule { segment } => write!(f, "private child module: {segment}"),
            Error::InternalRootModule { segment } => write!(
                f,
                "`{segment}` is internal to the standard library; use the corresponding `/std` module"
            ),
            Error::BindingNotFound { binding } => write!(f, "binding not found: {binding}"),
            Error::PrivateBinding { binding } => write!(f, "private binding: {binding}"),
            Error::QualifierConflict { qualifier } => {
                write!(
                    f,
                    "qualifier conflicts with existing scope entry: {qualifier}"
                )
            }
            Error::BindingConflict { label } => {
                write!(f, "binding conflicts with existing scope entry: {label}")
            }
            Error::NotAModule { label, parent } => {
                write!(f, "not a module: {label} in {parent}")
            }
            Error::NotABinding { label, parent } => {
                write!(f, "not a binding: {label} in {parent}")
            }
            Error::NoSuchUseTarget { label, parent } => {
                write!(f, "no module or binding named {label} in {parent}")
            }
            Error::DuplicatePublicDeclaration { label } => {
                write!(f, "duplicate public declaration: {label}")
            }
            Error::ExportConflict { label } => {
                write!(f, "export conflict for label: {label}")
            }
            Error::CyclicReExport { label } => {
                write!(f, "cyclic re-export with no concrete target: {label}")
            }
            Error::BangWithoutBind => {
                write!(f, "postfix `!` used outside a `let !` block")
            }
            Error::AnnotatedMotiveNotUnion => {
                write!(
                    f,
                    "an annotated motive `(x : T(...)) => P` is only legal on a union match"
                )
            }
            Error::PatternColumnConflict => write!(
                f,
                "a match column mixes constructor patterns with scalar literal patterns"
            ),
            Error::UnresolvedMatchConstructor { tag } => write!(
                f,
                "cannot resolve the union of constructor `{tag}` to expand a `_` arm; \
                 bring the constructors into scope or list every constructor"
            ),
            Error::PatternArityMismatch {
                tag,
                expected,
                found,
            } => write!(
                f,
                "constructor pattern `{tag}` expects {expected} argument(s), found {found}"
            ),
            Error::NonExhaustiveScalarMatch => write!(
                f,
                "a scalar literal match column needs a `_`/binder catch-all (or, for `Bln`, both cases)"
            ),
            Error::DependentMatrixMotive => write!(
                f,
                "an annotated motive `(x : T(...)) => P` cannot be threaded through nested patterns; \
                 keep the dependent match single-level"
            ),
            Error::ModuleLoadFailed { label, cause } => {
                write!(f, "failed to load module {label}:\n{}", cause.format())
            }
            Error::Located { error, .. } => write!(f, "{error}"),
        }
    }
}

#[derive(Debug)]
pub enum LoadError {
    Read { path: PathBuf, error: io::Error },
    Parse(ParserError),
}

impl LoadError {
    pub fn format(&self) -> String {
        match self {
            LoadError::Read { path, error } => {
                format!("failed to read {}: {error}", path.display())
            }
            LoadError::Parse(error) => error.format(),
        }
    }
}
