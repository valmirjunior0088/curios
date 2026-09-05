use {
    curios_parse::ParserError,
    curios_utilities::{Report, Span},
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
    /// A `pub use` in an inductive's declaring module attempted to expose the constructors of a representation-private inductive.
    OpaqueConstructorsCannotBeReExported {
        induct_decl: String,
    },
    /// A root module reachable only from the standard library (e.g. `sys`) was referenced from user code. Such modules are the trusted intrinsic substrate; user code reaches them through their `/std` wrappers.
    InternalRootModule {
        segment: String,
    },
    /// Two claimed prefixes are not disjoint: the same prefix twice, one lying inside the other's subtree, or a prefix the entry program already declares as a module. All are the same collision seen from a different side, and all name both parties — a reader has to know which two things to change, and only the mount table knows both.
    ///
    /// A prefix *is* a package's canonical name, so naming the prefixes is naming the packages.
    MountCollision {
        /// What the unit being compiled claims, and how it claims it.
        claim: String,
        /// The prefix already claimed that the claim is not disjoint from.
        claimed: String,
        /// Who claims it.
        claimant: String,
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
    /// A `use`-marked concept field's type is not a concept application (a path, optionally applied). Only such a type names a superclass edge. Superclass fields are anonymous, so the enclosing concept identifies the offender.
    MalformedSuperField {
        concept: String,
    },
    /// A `pub` item's declared signature references an item that is not itself publicly reachable. Cross-module references are vetted during resolution; this closes the two privately-resolvable paths (the item's own module and its own private child modules).
    PrivateItemInPublicInterface {
        item: String,
        referent: String,
    },
    /// A postfix `!` was reached through a *type* lowering (an annotation, a motive, a Π/Σ component): types have no region to hoist the action to.
    BangInTypePosition,
    /// A motive was written on a match whose head does not dispatch on a single tag or literal shape directly — every arm matches a tuple/struct, is a plain binder, or arms disagree on which carrier (`Ctor`/`Bool`/`Nat`/`List`/`Bits`/`Bytes`) they dispatch on. Such a head explodes into projections and builds no core `Match` node for the motive to attach to, so the motive would be silently discarded.
    MatrixMotiveRequiresCtorHead,
    /// Two match-arm rows write incompatible shapes for the same column — mixing a plain binder with a concrete constructor/tuple/struct shape (a "Path A" full-enumeration violation: no wildcard/catch-all is allowed alongside a concrete case), or two concrete shapes that disagree (a tuple/struct of different arity or field labels, a struct with a different head name, or the same constructor tag applied with a different number of arguments).
    MatrixInconsistentShape,
    /// Two match-arm rows specify the exact same pattern in every column — including a flat, single-column match with a literally repeated constructor tag. Every arm must be reachable and distinct; "Path A" gives arms no priority order to break the tie with.
    MatrixDuplicateRow,
    /// A `Bool`/`Nat`/`List`/`Bits`/`Bytes` leaf-pattern column split without both of its required cases present and no `| _ =>` to stand in — `missing` is the spelling of the case the rows lack. Unlike an ordinary constructor tag (whose omission the matrix compiler defers entirely to `induct_match`'s vacuity inversion), these hardcoded carriers have no core-side exhaustiveness mechanism — the matrix compiler must enforce completeness itself. Raised for a column at any depth, so the report names neither.
    MatrixIncompleteCarrierMatch {
        carrier: &'static str,
        missing: &'static str,
    },
    /// A `Nat` column dispatching on literals with no `| _ =>` default. A `switch` over `Nat` is never exhaustive, so `documentation/syntax.md` makes the default mandatory for the dispatch form — a rule of its own, which the two-case message above misstated as a missing successor arm.
    MatrixNatDispatchNeedsDefault,
    /// A `Nat` match-arm column mixes successor-peeling (`n + 1; ih`) with literal dispatch (`5`, `0x90`). A literal case peels no successor, so the two select incompatible core forms (the `Nat` eliminator vs. a value `switch`) and cannot share one column — write one or the other.
    MatrixMixedNatDispatch,
    /// A binary-pattern column mixes bit and byte grains. A single scrutinee has one binary type, so every row in the column must use the same prefix.
    MatrixMixedBinGrain,
    /// A `choose` bind arm `| pattern = value =>` whose `pattern` is a bare binder — irrefutable, so it always fires and the rest of the ladder is dead. A bind is for *refutable* matching; use a `let` for an unconditional binding.
    BindArmIrrefutable,
    /// A headed match ended in a *named* catch-all (`| x =>`) among concrete constructor arms. Only a bare `| _ =>` is a catch-all default; a named binder there is almost certainly a mistake (a misspelled constructor, or a binder that silently swallows every remaining case).
    MatchNamedCatchAll,
    /// A recursive local binding — one that names itself, or a member of a `let … and …;` group — whose binder is a pattern. A pattern binds projections, none of which its own value could name.
    RecursivePatternBinding,
    /// A recursive local binding with no type annotation. A body that mentions the binding cannot be the source of its type.
    RecursiveBindingNeedsType {
        label: String,
    },
    /// A recursive local binding whose value performs `!`. The action runs before the binding exists, so it cannot name the result it produces.
    RecursiveBangBinding {
        label: String,
    },
    /// Top-level definitions that reference one another without being declared as one `let … and …;` group. A lone definition may name itself, but a cycle between two has no order to declare them in.
    UndeclaredCycle {
        names: Vec<String>,
    },
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

    /// The error as data: the `Display` message, at the span `Located` wraps it in when one does — except a module that failed to parse, whose location is the parser's own rather than the `mod` line that asked for it, and whose message therefore carries the parser's message and no snippet of its own.
    pub fn report(&self) -> Report {
        match self {
            Self::Located { span, error } => error
                .parse_failure()
                .unwrap_or_else(|| Report::at(span.clone(), error.to_string())),
            error => error
                .parse_failure()
                .unwrap_or_else(|| Report::unlocated(error.to_string())),
        }
    }

    /// The nested parser's report, prefixed with which module was being loaded, when this is a module that failed to parse.
    fn parse_failure(&self) -> Option<Report> {
        let Self::ModuleLoadFailed { label, cause } = self else {
            return None;
        };
        let LoadError::Parse(error) = &**cause else {
            return None;
        };
        let report = error.report();

        Some(Report {
            span: report.span,
            message: format!("failed to load module {label}:\n{}", report.message),
        })
    }

    /// Renders the error for the user: the `Display` message plus, when the error is `Located`, the source snippet its span points at. Callers should prefer this over `to_string()`, which prints the message alone. [`report`](Self::report) rendered, so the two cannot disagree about where.
    pub fn format(&self) -> String {
        self.report().render()
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
            Error::OpaqueConstructorsCannotBeReExported { induct_decl } => write!(
                f,
                "constructors of opaque inductive '{induct_decl}' cannot be re-exported\n  mark its representation public (`: pub Type` or `: pub Prop`)"
            ),
            Error::InternalRootModule { segment } => write!(
                f,
                "`{segment}` is internal to the standard library; use the corresponding `/std` module"
            ),
            Error::MountCollision {
                claim,
                claimed,
                claimant,
            } => write!(
                f,
                "{claim} collides with `{claimed}`, claimed by {claimant}; a prefix belongs to exactly one unit, and no claimed prefix may lie within another's subtree"
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
            Error::MatrixMotiveRequiresCtorHead => {
                write!(
                    f,
                    "a written motive is only legal when every arm dispatches on the same kind of tag/literal directly"
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
            Error::MatrixIncompleteCarrierMatch { carrier, missing } => {
                write!(
                    f,
                    "a `{carrier}` match must also cover `{missing}`, or end in `| _ =>`"
                )
            }
            Error::MatrixNatDispatchNeedsDefault => {
                write!(
                    f,
                    "a `Nat` dispatch over literals must end in `| _ =>`: no set of literals covers every natural"
                )
            }
            Error::MatrixMixedNatDispatch => {
                write!(
                    f,
                    "a `Nat` match arm mixes successor-peeling (`n + 1; ih`) with literal dispatch (`5`) in one column; use one or the other"
                )
            }
            Error::MatrixMixedBinGrain => {
                write!(f, "binary match arms mix `b[…]` and `x[…]` patterns")
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
            Error::RecursivePatternBinding => {
                write!(
                    f,
                    "a recursive binding is a plain name: a pattern binds projections of a value, none of which the value itself can name; bind the value under a name and destructure it after"
                )
            }
            Error::RecursiveBindingNeedsType { label } => {
                write!(
                    f,
                    "`{label}` mentions itself and states no type; a recursive binding states its type, since a body that mentions the binding cannot be the source of it — and if an outer `{label}` was meant, the new binding shadows it: rename one of them"
                )
            }
            Error::RecursiveBangBinding { label } => {
                write!(
                    f,
                    "`{label}` is bound by an action that mentions it; the action runs before the binding exists, so it cannot name the result it produces"
                )
            }
            Error::UndeclaredCycle { names } => {
                let quoted = names
                    .iter()
                    .map(|name| format!("`{name}`"))
                    .collect::<Vec<_>>();
                let listed = match quoted.split_last() {
                    Some((last, rest)) if !rest.is_empty() => {
                        format!("{} and {last}", rest.join(", "))
                    }
                    _ => quoted.join(", "),
                };
                write!(
                    f,
                    "{listed} reference each other; a definition may name itself, but definitions that name one another are declared as one group — join them with `and`"
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
    /// The failure as data: the offending path plus the io error, located nowhere, or the parser's own report at the offset it stopped.
    pub fn report(&self) -> Report {
        match self {
            LoadError::Read { path, error } => {
                Report::unlocated(format!("failed to read {}: {error}", path.display()))
            }
            LoadError::Parse(error) => error.report(),
        }
    }

    /// Renders the failure for the user. [`report`](Self::report) rendered, so the two cannot disagree about where.
    pub fn format(&self) -> String {
        self.report().render()
    }
}
