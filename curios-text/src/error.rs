use {
    curios_base::{Span, parser::ParserError},
    std::{fmt, io, path::PathBuf},
};

/// Everything that can go wrong between a parsed surface tree and a core module: module discovery and loading, `use`/name resolution and visibility, and the structural checks `into_core` lowering enforces. As an error propagates it is wrapped in `Located` with the *innermost* relevant span (`at` never overwrites an existing location), which [`Error::format`] renders as a source snippet.
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
    /// A `pub use` in an inductive's declaring module attempted to expose the
    /// constructors of a representation-private inductive.
    OpaqueConstructorsCannotBeReExported {
        inductive: String,
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
    /// A dependent motive (`(x) => P` or the annotated type-pattern form) was
    /// written on a match whose head does not dispatch on a single tag or
    /// literal shape directly — every arm matches a tuple/struct, is a
    /// plain binder, or arms disagree on which carrier (`Ctor`/`Bln`/`Nat`/
    /// `Lst`/`Bin`) they dispatch on. There is no core `Match` node for such
    /// a head to attach the motive to; only a match whose every top-level
    /// arm shares one dispatchable shape can carry a dependent motive.
    MatrixMotiveRequiresCtorHead,
    /// Two match-arm rows write incompatible shapes for the same column —
    /// mixing a plain binder with a concrete constructor/tuple/struct shape
    /// (a "Path A" full-enumeration violation: no wildcard/catch-all is
    /// allowed alongside a concrete case), or two concrete shapes that
    /// disagree (a tuple/struct of different arity or field labels, a
    /// struct with a different head name, or the same constructor tag
    /// applied with a different number of arguments).
    MatrixInconsistentShape,
    /// Two match-arm rows specify the exact same pattern in every column —
    /// including a flat, single-column match with a literally repeated
    /// constructor tag. Every arm must be reachable and distinct; "Path A"
    /// gives arms no priority order to break the tie with.
    MatrixDuplicateRow,
    /// A nested `Bln`/`Nat`/`Lst`/`Bin` leaf-pattern column split without
    /// both of its required cases present. Unlike an ordinary constructor
    /// tag (whose omission the matrix compiler defers entirely to
    /// `inductive_match`'s vacuity inversion), these four hardcoded
    /// carriers have no core-side exhaustiveness mechanism — the matrix
    /// compiler must enforce completeness itself.
    MatrixIncompleteCarrierMatch {
        carrier: &'static str,
    },
    /// A `Nat` match-arm column mixes successor-peeling (`n + 1; ih`) with
    /// literal dispatch (`5`, `0x90`). A literal case peels no successor, so the
    /// two select incompatible core forms (the `Nat` eliminator vs. a value
    /// `switch`) and cannot share one column — write one or the other.
    MatrixMixedNatDispatch,
    /// A binary-pattern column mixes bit and byte grains. A single scrutinee
    /// has one binary type, so every row in the column must use the same prefix.
    MatrixMixedBinGrain,
    /// A headless-ladder bind arm `| pattern = value =>` whose `pattern` is a
    /// bare binder — irrefutable, so it always fires and the rest of the ladder
    /// is dead. A bind is for *refutable* matching; use a `let` for an
    /// unconditional binding.
    BindArmIrrefutable,
    /// A headed match ended in a *named* catch-all (`| x =>`) among concrete
    /// constructor arms. Only a bare `| _ =>` is a catch-all default; a named
    /// binder there is almost certainly a mistake (a misspelled constructor, or
    /// a binder that silently swallows every remaining case).
    MatchNamedCatchAll,
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
    pub(crate) fn at(self, span: Span) -> Self {
        match self {
            Self::Located { .. } => self,
            error => Self::Located {
                span,
                error: Box::new(error),
            },
        }
    }

    /// Renders the error for the user: the `Display` message plus, when the error is `Located`, the source snippet its span points at. Callers should prefer this over `to_string()`, which prints the message alone.
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
            Error::OpaqueConstructorsCannotBeReExported { inductive } => write!(
                f,
                "constructors of opaque inductive '{inductive}' cannot be re-exported\n  mark its representation public (`: pub Type` or `: pub Prop`)"
            ),
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
            Error::BangInTypePosition => {
                write!(f, "postfix `!` is not allowed inside a type")
            }
            Error::AnnotatedMotiveNotInductive => {
                write!(
                    f,
                    "an annotated motive `(x : T(...)) => P` is only legal on an inductive match"
                )
            }
            Error::MatrixMotiveRequiresCtorHead => {
                write!(
                    f,
                    "a dependent motive is only legal when every arm dispatches on the same kind of tag/literal directly"
                )
            }
            Error::MatrixInconsistentShape => {
                write!(
                    f,
                    "match arm patterns disagree on shape for the same column"
                )
            }
            Error::MatrixDuplicateRow => {
                write!(
                    f,
                    "duplicate or overlapping match arm: every arm must be reachable and distinct"
                )
            }
            Error::MatrixIncompleteCarrierMatch { carrier } => {
                write!(
                    f,
                    "a nested `{carrier}` pattern column must cover both of its cases"
                )
            }
            Error::MatrixMixedNatDispatch => {
                write!(
                    f,
                    "a `Nat` match arm mixes successor-peeling (`n + 1; ih`) with literal dispatch (`5`) in one column; use one or the other"
                )
            }
            Error::MatrixMixedBinGrain => {
                write!(f, "binary match arms mix `b\\` and `x\\` patterns")
            }
            Error::BindArmIrrefutable => {
                write!(
                    f,
                    "a bind arm `| pattern = value =>` needs a refutable pattern; a bare binder is irrefutable — use a `let`"
                )
            }
            Error::MatchNamedCatchAll => {
                write!(
                    f,
                    "a named final arm cannot be a catch-all; write `| _ =>` for a default, or name the constructor"
                )
            }
            Error::ModuleLoadFailed { label, cause } => {
                write!(f, "failed to load module {label}:\n{}", cause.format())
            }
            Error::Located { error, .. } => write!(f, "{error}"),
        }
    }
}

/// Why a `.crs` file could not become a parsed module: unreadable, or read but failed to parse. Returned directly by [`Entrypoint::from_path`](crate::Entrypoint::from_path); for a `mod`-declared file it is wrapped in [`Error::ModuleLoadFailed`], which adds which module was being loaded.
#[derive(Debug)]
pub enum LoadError {
    Read { path: PathBuf, error: io::Error },
    Parse(ParserError),
}

impl LoadError {
    /// Renders the failure for the user: the offending path plus the io error, or the parser's own formatted diagnostic (which carries its source snippet).
    pub fn format(&self) -> String {
        match self {
            LoadError::Read { path, error } => {
                format!("failed to read {}: {error}", path.display())
            }
            LoadError::Parse(error) => error.format(),
        }
    }
}
