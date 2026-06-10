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
    /// A postfix `!` appeared outside any `with` body, so there is no bind
    /// function to sequence it with.
    BangOutsideWith,
    /// The annotated motive form `(x : T(...)) => P` is only meaningful on a
    /// union scrutinee — `Bln` and `Nat` matches take `: P` or `: (x) => P`.
    AnnotatedMotiveNotUnion,
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
            Error::BangOutsideWith => {
                write!(f, "postfix `!` used outside a `with` block")
            }
            Error::AnnotatedMotiveNotUnion => {
                write!(
                    f,
                    "an annotated motive `(x : T(...)) => P` is only legal on a union match"
                )
            }
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
