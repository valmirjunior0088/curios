//! A name that did not resolve: one value, made by the stage that could not resolve it and read by whichever stage refuses for it.
//!
//! The lowering resolves every name a program writes and is the only stage that can say why one failed: which step of a path found nothing, whether what it wanted is absent or private there, and what the scope held that the reader may have meant. A name written in a declaration refuses that declaration alone, so the lowering hands the failure on rather than raising it — the name lowers to a binder nothing binds, this value is filed under that binder, and elaboration reports it where it meets the binder. A `use` that fails refuses its unit, and the lowering raises the same value itself. Either way one rendering says it, so what a reader is told of a misspelled name does not depend on where it was written.

#[cfg(test)]
mod tests;

use {curios_utilities::Qualifier, std::fmt};

/// What a step of a name was looked up as.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Sought {
    /// A module: a step of a path before its last, a `use` of a module, the path of a glob.
    Module,
    /// A binding: the last step of a name a term writes, a bare name.
    Binding,
    /// Either, as a `use` selector with no keyword takes whichever the module holds.
    Either,
    /// A structure, which a literal's head names.
    Structure,
}

/// Why a step found nothing it may use.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Missing {
    /// Nothing of that name stands there.
    Absent,
    /// It stands there and is not `pub`, and the name was written outside the module that declares it.
    Private,
    /// It is a root only the compiler's own library may name: the step is the root, and the name stands under it for no program to write.
    Internal,
    /// It is a root the compilation mounts and the unit declared no dependency on: the step is the root, and declaring the dependency is what reaches under it.
    Undeclared,
}

/// A name that did not resolve — see the module documentation.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Unresolved {
    /// The step that failed: the segment no lookup answered.
    pub segment: String,
    pub sought: Sought,
    /// The module the walk had reached where the step failed. `None` where the step was looked up in the scope the name was written in: a bare name, or the head of a relative path.
    pub within: Option<Qualifier>,
    pub missing: Missing,
    /// The name in scope there that the reader most likely meant, by `curios_utilities::nearest`.
    pub nearest: Option<String>,
    /// The shortest absolute paths the unit may write that carry the name the reader reached for, each a way to what may have been meant: the failed step itself where the scope it was written in holds nothing of that name, and the step after it where the failed step is a root the unit may not name. Found by the name alone, in the namespace it was sought in: a module and a declaration of one name are two things, and nothing here pairs a path with another that happens to export the same declaration.
    pub elsewhere: Vec<Qualifier>,
}

impl fmt::Display for Unresolved {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let segment = &self.segment;
        match (&self.within, self.missing, self.sought) {
            (_, Missing::Internal, _) => {
                write!(formatter, "`{segment}` is internal to the standard library")?;
            }
            (_, Missing::Undeclared, _) => write!(
                formatter,
                "`{segment}` is mounted by this compilation but is not a declared dependency of this unit; declare it to reach its names"
            )?,
            (None, _, Sought::Structure) => {
                write!(formatter, "`{segment}` names no structure in scope")?;
            }
            (None, _, _) => write!(formatter, "`{segment}` is not in scope")?,
            (Some(module), Missing::Private, _) => {
                write!(formatter, "`{segment}` is private to `{}`", module.join())?;
            }
            // The root has no name to say the step by, so the path it would have been is named.
            (Some(module), Missing::Absent, Sought::Module) if module.is_root() => {
                write!(formatter, "`/{segment}` names no module")?;
            }
            (Some(module), Missing::Absent, _) if module.is_root() => {
                write!(formatter, "`/{segment}` names nothing")?;
            }
            (Some(module), Missing::Absent, Sought::Module) => {
                write!(formatter, "`{}` has no module `{segment}`", module.join())?;
            }
            (Some(module), Missing::Absent, _) => {
                write!(formatter, "`{}` has no `{segment}`", module.join())?;
            }
        }
        if let Some(nearest) = &self.nearest {
            write!(formatter, "\n  did you mean `{nearest}`?")?;
        }
        // Each by its own last segment: the name a reader reached for is the failed step itself, or, under a root they may not name, the step after it.
        for candidate in &self.elsewhere {
            write!(formatter, "\n{}", candidate.reach_hint(candidate.last()))?;
        }

        Ok(())
    }
}
