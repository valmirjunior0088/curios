use {
    curios_common::{About, Report, Said, Span},
    curios_parse::ParserError,
    std::{fmt, io, path::PathBuf},
};

/// Everything that can go wrong between a parsed surface tree and a core module: module discovery and loading, `use`/name resolution and visibility, and the structural checks `into_core` lowering enforces. As an error propagates it is wrapped in `Located` with the *innermost* relevant span (`at` never overwrites an existing location), which [`Error::format`] renders as a source snippet.
#[derive(Debug)]
pub enum Error {
    /// A name that did not resolve: the one refusal of a step of a path, a `use` selector or a relative head that finds nothing it may use — see `curios_core::Unresolved`, which says what failed and renders it. A step into a root the unit may not name is one of these: a root only the compiler supplies (`sys`), whose roster is the compiler's own vocabulary with no compatibility promise (`curios_common::RootKind::Internal` says why), and a prefix the compilation mounts and the unit never declared a dependency on. Raised for a `use`, which refuses its unit; a name written in a declaration hands the same value to elaboration instead (`Error::into_unresolved`). `leaf` is whether the step that failed is the last segment of the name it was written in, which is what a report of a `use m/{a}` is placed by.
    Unresolved {
        failure: Box<curios_core::Unresolved>,
        leaf: bool,
    },
    ModuleNotFound {
        path: String,
    },
    /// A `pub use` in an inductive's declaring module attempted to expose the constructors of a representation-private inductive.
    OpaqueConstructorsCannotBeReExported {
        induct_decl: String,
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
    /// A scope is given one module under one label twice: by two `use` selectors, or by a `use` beside a declaration of the module's own. `first` is where the label was claimed by the claim that stood when this one was refused, where that claim was written.
    QualifierConflict {
        qualifier: String,
        first: Option<Span>,
    },
    /// [`Error::QualifierConflict`] for a binding.
    BindingConflict {
        label: String,
        first: Option<Span>,
    },
    /// A module declares one name twice. About the name occupying a slot rather than about `pub`, which is why neither declaration's visibility appears: two private `let`s collide exactly as two public ones do.
    DuplicateDeclaration {
        label: String,
    },
    /// A structure declares one field label twice. Distinct from [`Error::DuplicateDeclaration`] because a field is not a module declaration: it occupies a slot in the structure's telescope, which is what the message says and what a reader has to look at.
    DuplicateField {
        label: String,
    },
    ExportConflict {
        label: String,
    },
    CyclicReExport {
        label: String,
    },
    /// A concept declares a `use` parameter. A concept's premise is a superclass field; a parameter would put the premise's dictionary in the concept's identity.
    ConceptUseParameter {
        concept: String,
    },
    /// A `pub` item's declared signature references an item that is not itself publicly reachable. Cross-module references are vetted during resolution; this closes the two privately-resolvable paths (the item's own module and its own private child modules).
    PrivateItemInPublicInterface {
        item: String,
        referent: String,
        /// The module that keeps the referent from the item's readers, where the referent is exposed to all of that module's and the module is the declaration written without `pub`.
        behind: Option<String>,
    },
    /// A `Sort` written where no caller states it. A sort left open is a parameter of a top-level declaration, stated at each use of it, so the word stands in the signature of a top-level `let`, `induct` or `struct` and nowhere else.
    SortOutsideSignature {
        /// What it was written in instead.
        site: &'static str,
    },
    /// A motive was written on a match whose head does not dispatch on a single tag or literal shape directly — every arm matches a tuple/struct, is a plain binder, or arms disagree on which carrier (`Variant`/`Bool`/`Char`/`Nat`/`List`/`Bytes`) they dispatch on. Such a head explodes into projections and builds no core `Match` node for the motive to attach to, so the motive would be silently discarded.
    MatrixMotiveRequiresCtorHead,
    /// A `choose` bind arm `| pattern = value =>` whose `pattern` is a bare binder — irrefutable, so it always fires and the rest of the ladder is dead. A bind is for *refutable* matching; use a `let` for an unconditional binding.
    BindArmIrrefutable,
    /// A headed match ended in a *named* catch-all (`| x =>`) among concrete constructor arms. Only a bare `| _ =>` is a catch-all default; a named binder there is almost certainly a mistake (a misspelled constructor, or a binder that silently swallows every remaining case).
    MatchNamedCatchAll {
        /// The name the arm binds, which its author may have meant as a constructor.
        binder: String,
    },
    /// An arm whose alternatives do not all bind the same names: `name` is bound by one of them and not by the alternative the refusal stands at. The arm's body reads each name it binds, in each alternative.
    MatchAlternativeNames {
        name: String,
    },
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
    /// Whether the refusal is about the *last* segment of the path it was raised for, rather than about the path that reaches it.
    ///
    /// A `use m/{a, b}` member is written apart from its group's path, so the two halves have separate spans ([`Name::leaf_span`](curios_surface::Name::leaf_span)) and only the refusal knows which one it is talking about: `use /nope/{a}` cannot find `/nope` and `use /std/{nope}` cannot find `nope` in it.
    pub(crate) fn names_the_leaf(&self) -> bool {
        match self {
            Self::Located { error, .. } => error.names_the_leaf(),
            Self::Unresolved { leaf, .. } => *leaf,
            Self::QualifierConflict { .. } | Self::BindingConflict { .. } => true,
            _ => false,
        }
    }

    /// The refusal of a name that did not resolve — see [`Error::Unresolved`].
    pub(crate) fn unresolved(failure: curios_core::Unresolved, leaf: bool) -> Self {
        Self::Unresolved {
            failure: Box::new(failure),
            leaf,
        }
    }

    /// What failed, where this is the refusal of a name that did not resolve, and the error back where it is any other: a name written in a declaration hands the failure on to elaboration, and nothing else is the declaration's alone to answer for.
    pub(crate) fn into_unresolved(self) -> Result<curios_core::Unresolved, Self> {
        match self {
            Self::Unresolved { failure, .. } => Ok(*failure),
            Self::Located { span, error } => {
                error.into_unresolved().map_err(|error| Self::Located {
                    span,
                    error: Box::new(error),
                })
            }
            error => Err(error),
        }
    }

    pub(crate) fn at(self, span: Span) -> Self {
        match self {
            Self::Located { .. } => self,
            error => Self::Located {
                span,
                error: Box::new(error),
            },
        }
    }

    /// The error as data: what it says, at the span `Located` wraps it in when one does — except a module that failed to parse, whose report is the parser's own, in the module's file rather than at the `mod` line that asked for it, with the fact of which module the file was read as.
    pub fn report(&self) -> Report {
        let report = match self {
            Self::Located { span, error } => error
                .parse_failure()
                .unwrap_or_else(|| Report::of(Some(span.clone()), error.said())),
            error => error
                .parse_failure()
                .unwrap_or_else(|| Report::of(None, error.said())),
        };

        // "Also" and not "first": a module's declarations are in scope before any of its `use` lines is read, so the claim that stood first may be written below the one refused.
        match self.other_claim() {
            Some((label, other)) => report.noting(
                other.clone(),
                format!("`{label}` is also brought into scope here"),
            ),
            None => report,
        }
    }

    /// The label a conflict is about and where its other claim stands, where that claim was written: the second place its report is about.
    fn other_claim(&self) -> Option<(&str, &Span)> {
        match self {
            Self::Located { error, .. } => error.other_claim(),
            Self::QualifierConflict {
                qualifier: label,
                first,
            }
            | Self::BindingConflict { label, first } => Some((label, first.as_ref()?)),
            _ => None,
        }
    }

    /// The nested parser's report, in the module's own file, with the fact of which module the file was read as, when this is a module that failed to parse.
    fn parse_failure(&self) -> Option<Report> {
        let Self::ModuleLoadFailed { label, cause } = self else {
            return None;
        };
        let LoadError::Parse(error) = &**cause else {
            return None;
        };
        let mut report = error.report();
        report.facts.push(read_as(label));

        Some(report)
    }

    /// Renders the error for the user: its report as a block, with the pane its span points at when the error is `Located`. Callers should prefer this over `to_string()`, which prints the tag and the facts alone. [`report`](Self::report) rendered, so the two cannot disagree about where.
    pub fn format(&self) -> String {
        self.report().render()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.said().fmt(formatter)
    }
}

impl Error {
    /// What the error says: its sentence, and each fact beneath it.
    pub fn said(&self) -> Said {
        let mut said = Said::default();
        self.say(&mut said);

        said
    }

    /// Say the error onto `f`.
    fn say(&self, f: &mut Said) {
        match self {
            Error::Unresolved { failure, .. } => failure.say(f),
            Error::ModuleNotFound { path } => {
                f.text("unknown module");
                f.stating(format_args!("not found: {path}"));
            }
            Error::OpaqueConstructorsCannotBeReExported { induct_decl } => {
                f.text("private representation");
                f.stating(format_args!(
                    "the constructors of `{induct_decl}` cannot be re-exported"
                ));
                f.stating("mark its representation public, `: pub Type` or `: pub Prop`");
            }
            Error::MountCollision {
                claim,
                claimed,
                claimant,
            } => {
                f.text("prefix collision");
                f.stating(format_args!(
                    "{claim} collides with `{claimed}`, claimed by {claimant}"
                ));
                f.stating("a prefix belongs to exactly one unit");
                f.stating("no claimed prefix lies within another's subtree");
            }
            Error::PrivateItemInPublicInterface {
                item,
                referent,
                behind: None,
            } => {
                f.text("private item exposed");
                f.stating(format_args!(
                    "the public `{item}` names the private `{referent}` in its signature"
                ));
                f.stating(format_args!(
                    "mark `{referent}` `pub`, or make `{item}` private"
                ));
            }
            Error::PrivateItemInPublicInterface {
                item,
                referent,
                behind: Some(module),
            } => {
                f.text("private item exposed");
                f.stating(format_args!(
                    "the public `{item}` names `{referent}` in its signature"
                ));
                f.stating(format_args!(
                    "`{referent}` is in the private module `{module}`"
                ));
                f.stating(format_args!("mark `{module}` `pub`, re-export `{referent}` beside `{item}`, or make `{item}` private"));
            }
            Error::QualifierConflict { qualifier, .. } => {
                f.text("name conflict");
                f.stating(format_args!(
                    "`{qualifier}` already names a module in this scope"
                ));
            }
            Error::BindingConflict { label, .. } => {
                f.text("name conflict");
                f.stating(format_args!("`{label}` is already in scope here"));
            }
            Error::DuplicateDeclaration { label } => {
                f.text("duplicate declaration");
                f.stating(format_args!("`{label}` is already declared in this module"));
            }
            Error::DuplicateField { label } => {
                f.text("duplicate field");
                f.stating(format_args!(
                    "`{label}` is already a field of this structure"
                ));
            }
            Error::ExportConflict { label } => {
                f.text("name conflict");
                f.stating(format_args!("`{label}` is exported twice"));
            }
            Error::CyclicReExport { label } => {
                f.text("re-export cycle");
                f.stating(format_args!(
                    "`{label}` is re-exported in a cycle that reaches no declaration"
                ));
            }
            Error::ConceptUseParameter { concept } => {
                f.text("misplaced premise");
                f.stating(format_args!(
                    "the concept `{concept}` takes a `use` parameter"
                ));
                f.stating("a concept states a premise as a superclass field, `use C(args)` among its fields");
            }
            Error::SortOutsideSignature { site } => {
                f.text("misplaced sort");
                f.stating(format_args!("`Sort` stands in {site}"));
                f.stating("it is left to a caller, so it is written in the signature of a top-level `let`, `induct` or `struct`");
            }
            Error::MatrixMotiveRequiresCtorHead => {
                f.text("misplaced motive");
                f.stating("a motive is written where every arm dispatches directly on one kind of tag or literal");
            }
            Error::BindArmIrrefutable => {
                f.text("irrefutable pattern");
                f.stating("a bind arm, `| pattern = value =>`, takes a refutable pattern");
                f.stating("a bare binder always matches");
                f.stating("write a `let`");
            }
            Error::MatchNamedCatchAll { binder } => {
                f.text("named catch-all");
                f.stating("a named final arm is no catch-all");
                f.stating("write `| _ =>` for a default");
                f.stating(format_args!(
                    "or the constructor with its payload list, `| {binder}() =>`"
                ));
            }
            Error::MatchAlternativeNames { name } => {
                f.text("missing binder");
                f.stating(format_args!(
                    "`{name}` is bound by another alternative of this arm and not by this one"
                ));
                f.stating("every alternative of an arm binds the same names");
            }
            Error::RecursivePatternBinding => {
                f.text("recursive pattern");
                f.stating("a recursive binding is a plain name");
                f.stating(
                    "a pattern binds projections of a value, and the value can name none of them",
                );
                f.stating("bind the value under a name and destructure it after");
            }
            Error::RecursiveBindingNeedsType { label } => {
                f.text("missing type");
                f.stating(format_args!("`{label}` mentions itself and states no type"));
                f.stating("a body that mentions the binding cannot be the source of its type");
                f.stating(format_args!(
                    "if an outer `{label}` was meant, the new binding shadows it"
                ));
                f.stating("rename one of them");
            }
            Error::RecursiveBangBinding { label } => {
                f.text("recursive action");
                f.stating(format_args!(
                    "`{label}` is bound by an action that mentions it"
                ));
                f.stating("the action runs before the binding exists, so it cannot name the result it produces");
            }
            Error::ModuleLoadFailed { label, cause } => {
                let Report {
                    headline, facts, ..
                } = cause.report();
                f.text(headline);
                facts.into_iter().for_each(|fact| f.stating(fact));
                f.stating(read_as(label));
            }
            Error::Located { error, .. } => error.say(f),
        }
    }
}

/// The fact that says which module a file was read as, beneath the report of a file that could not be read or parsed.
fn read_as(label: &str) -> String {
    format!("read as the module `{label}`")
}

/// Why a `.crs` file could not become a parsed module: unreadable, or read but failed to parse. Returned directly by [`open_entrypoint`](crate::open_entrypoint) and [`read_module`](crate::read_module); for a `mod`-declared file it is wrapped in [`Error::ModuleLoadFailed`], which adds which module was being loaded.
#[derive(Debug)]
pub enum LoadError {
    Read { path: PathBuf, error: io::Error },
    Parse(ParserError),
}

impl LoadError {
    /// The failure as data: the io error, about the file that could not be read, or the parser's own report at the offset it stopped.
    pub fn report(&self) -> Report {
        match self {
            LoadError::Read { path, error } => {
                let mut said = Said::line("unreadable file");
                said.stating(error);

                Report::about(About::Path(path.clone()), said)
            }
            LoadError::Parse(error) => error.report(),
        }
    }

    /// Renders the failure for the user. [`report`](Self::report) rendered, so the two cannot disagree about where.
    pub fn format(&self) -> String {
        self.report().render()
    }
}
