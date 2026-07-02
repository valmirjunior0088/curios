use {
    curios_base::{Span, parser::ParserError},
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
    /// A `use`-marked concept field's type is not a concept application (a path,
    /// optionally applied). Only such a type names a superclass edge. Superclass
    /// fields are anonymous, so the enclosing concept identifies the offender.
    MalformedSuperField {
        concept: String,
    },
    /// A concept declaration marks every parameter `out`, leaving an empty
    /// witness key — at least one input position is required.
    ConceptWithoutInputs {
        label: String,
    },
    /// A `pub` item's declared signature references an item that is not itself
    /// publicly reachable. Cross-module references are vetted during
    /// resolution; this closes the two privately-resolvable paths (the item's
    /// own module and its own private child modules).
    PrivateItemInPublicInterface {
        item: String,
        referent: String,
    },
    /// A postfix `!` was reached through a *type* lowering (an annotation, a
    /// motive, a Π/Σ component): types have no region to hoist the action to.
    BangInTypePosition,
    /// The annotated motive form `(x : T(...)) => P` is only meaningful on a
    /// inductive scrutinee — `Bln` and `Nat` matches take `: P` or `: (x) => P`.
    AnnotatedMotiveNotInductive,
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
            Error::PrivateItemInPublicInterface { item, referent } => write!(
                f,
                "public item '{item}' exposes private item '{referent}' in its signature\n  mark '{referent}' pub, or make '{item}' private"
            ),
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
            Error::MalformedSuperField { concept } => {
                write!(
                    f,
                    "concept `{concept}` has a `use` field whose type is not a concept application"
                )
            }
            Error::ConceptWithoutInputs { label } => {
                write!(
                    f,
                    "concept `{label}` marks every parameter `out`; at least one input parameter is required to key witnesses on"
                )
            }
            Error::BangInTypePosition => {
                write!(f, "postfix `!` is not allowed inside a type")
            }
            Error::AnnotatedMotiveNotInductive => {
                write!(
                    f,
                    "an annotated motive `(x : T(...)) => P` is only legal on an inductive match"
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
