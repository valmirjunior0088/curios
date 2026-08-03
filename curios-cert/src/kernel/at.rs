//! A declaration read *for a particular occurrence*, with everything that reading depends on already checked.
//!
//! # Why this is a type rather than a convention
//!
//! Consulting a declaration for an occurrence is not a lookup. The occurrence carries a universe instance and a parameter list, and every answer the declaration gives — its result sort, a constructor's signature, its index telescope, its fields — is meaningful only once those have been checked against what the declaration actually declares. Three things have to hold, and they are independent:
//!
//! 1. the instance's width and its constraints — `Kernel::check_instance`;
//! 2. the parameter count;
//! 3. for an inductive *type* occurrence, the index count.
//!
//! Spread over the call sites, that is three things to remember at each of them, and the record is that they get forgotten. `Sort::of` and `synth_neutral` checked the parameter count; `infer`'s projection rule, `check`'s record rule and `convert`'s structural eta did not, and each reached [`Telescope::open`] — which **asserts**. An occurrence at the wrong count therefore aborted the walk rather than refusing the item, losing every other verdict in the same run, which is what makes `recheck_module_verdicts`' count a count.
//!
//! So the checks move into the constructor and the answers move behind it. A rule that holds an [`InductAt`] or a [`StructAt`] holds proof that all three passed, and it cannot open an arity at parameters other than the ones they were checked against, because the handle carries them and never takes them again.
//!
//! # What this is *not* for
//!
//! Reading a declaration as **data** — positivity walking its telescopes, inversion reading its `result_sort`, `check_induct_decl` verifying the entry itself. No occurrence is involved there, so there is nothing to check against, and those go on reading `Globals::induct_decl` directly. The distinction is the whole of when to use which: *what does this occurrence mean* takes a handle, *what does this declaration say* does not.

use {
    super::{Kernel, KernelError, sort::arity_matches},
    curios_core::{
        Atom, Global, InductDecl, InductType, Level, StructDecl, Telescope, Term,
        instantiate_universe_levels_scoped,
    },
};

/// An `induct` declaration at one occurrence's universes and parameters.
pub(crate) struct InductAt {
    declaration: InductDecl,
    params: Vec<Term>,
}

impl InductAt {
    /// The declaration itself, instantiated at the occurrence's universes.
    ///
    /// The arm rules and the elimination guard need the whole entry — its constructor list, its tags — and having reached it through this handle is what says the occurrence it belongs to was checked.
    pub(crate) fn declaration(&self) -> &InductDecl {
        &self.declaration
    }

    /// The family's declared result sort, at these universes.
    pub(crate) fn result_sort(&self) -> &Term {
        &self.declaration.result_sort
    }

    /// `tag`'s signature at this occurrence's parameters, or `None` when the family has no such constructor.
    pub(crate) fn signature(&self, tag: &Atom) -> Option<Telescope<Vec<Term>>> {
        self.declaration.instantiate(tag, &self.params)
    }

    /// The family's index telescope at this occurrence's parameters.
    pub(crate) fn indices(&self) -> Telescope<()> {
        self.declaration.indices_at(&self.params)
    }
}

/// A `struct` declaration at one occurrence's universes and parameters.
pub(crate) struct StructAt {
    declaration: StructDecl,
    params: Vec<Term>,
}

impl StructAt {
    /// The family's declared result sort, at these universes.
    pub(crate) fn result_sort(&self) -> &Term {
        &self.declaration.result_sort
    }

    /// The structure's field telescope at this occurrence's parameters.
    pub(crate) fn fields(&self) -> Telescope<()> {
        self.declaration.fields_at(&self.params)
    }
}

impl Kernel {
    /// An `induct` *type* occurrence's declaration: universes checked, parameter and index counts checked.
    pub(crate) fn induct_at(&self, family: &InductType) -> Result<InductAt, KernelError> {
        let at = self.induct_at_params(&family.name, &family.universes, &family.params)?;
        arity_matches(at.declaration.index_count(), family.indices.len())?;

        Ok(at)
    }

    /// An `induct` occurrence that carries parameters but no indices — a constructor application, whose index targets are the declaration's rather than the occurrence's.
    pub(crate) fn induct_at_params(
        &self,
        name: &Global,
        universes: &[Level],
        params: &[Term],
    ) -> Result<InductAt, KernelError> {
        let declaration = self
            .induct_decl(name)
            .ok_or_else(|| KernelError::Undeclared(name.clone()))?;
        self.check_instance(&declaration.universe_context, universes)?;
        arity_matches(declaration.param_count(), params.len())?;

        Ok(InductAt {
            declaration: instantiate_induct_decl(declaration, universes)?,
            params: params.to_vec(),
        })
    }

    /// A `struct` occurrence's declaration: universes checked, parameter count checked.
    pub(crate) fn struct_at(
        &self,
        name: &Global,
        universes: &[Level],
        params: &[Term],
    ) -> Result<StructAt, KernelError> {
        let declaration = self
            .struct_decl(name)
            .ok_or_else(|| KernelError::Undeclared(name.clone()))?;
        self.check_instance(&declaration.universe_context, universes)?;
        arity_matches(declaration.param_count(), params.len())?;

        let mut declaration = declaration.clone();
        declaration.arity = instantiate_universe_levels_scoped(&declaration.arity, universes)?;
        declaration.result_sort =
            instantiate_universe_levels_scoped(&declaration.result_sort, universes)?;
        declaration.universe_context = curios_core::UniverseContext::empty();

        Ok(StructAt {
            declaration,
            params: params.to_vec(),
        })
    }
}

/// A declaration with its universe parameters replaced by this occurrence's instance, so its constructor signatures speak of the right levels.
fn instantiate_induct_decl(
    declaration: &InductDecl,
    levels: &[Level],
) -> Result<InductDecl, KernelError> {
    let mut instantiated = declaration.clone();

    instantiated.arity = instantiate_universe_levels_scoped(&instantiated.arity, levels)?;
    instantiated.result_sort =
        instantiate_universe_levels_scoped(&instantiated.result_sort, levels)?;
    for constructor in instantiated.signatures_mut() {
        constructor.telescope = instantiate_universe_levels_scoped(&constructor.telescope, levels)?;
    }
    instantiated.universe_context = curios_core::UniverseContext::empty();

    Ok(instantiated)
}
