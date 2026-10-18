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
//! Spread over the call sites, that is three things to remember at each of them, and a site that forgets one reaches [`Telescope::open`] — which **asserts** — so an occurrence at the wrong count aborts the walk rather than refusing the item, losing every other verdict in the same run, which is what makes `recheck_module_verdicts`' count a count.
//!
//! So the checks live in the constructor and the answers behind it. A rule that holds an [`InductAt`] or a [`StructAt`] holds proof that all three passed, and it cannot open an arity at parameters other than the ones they were checked against, because the handle carries them and never takes them again.
//!
//! # What this is *not* for
//!
//! Reading a declaration as **data** — positivity walking its telescopes, inversion reading its `result_sort`, `check_induct_decl` verifying the entry itself. No occurrence is involved there, so there is nothing to check against, and those go on reading `Kernel::induct_decl` directly. The distinction is the whole of when to use which: *what does this occurrence mean* takes a handle, *what does this declaration say* does not.

use {
    super::{Counted, Error, Kernel, arity_matches},
    curios_core::{
        Atom, Field, Global, InductDecl, InductType, StructDecl, Telescope, Term, Universes,
    },
};

/// An `induct` declaration at one occurrence's universes and parameters.
///
/// The arity and the sort are instantiated where the handle is made, for every rule reads them; a constructor is instantiated where one is read, so a family of many cases is read at an occurrence in its arity's size and not in all of its cases' ([`InductInstance`](curios_core::InductInstance)).
pub(crate) struct InductAt {
    name: Global,
    universes: Universes,
    params: Vec<Term>,
    arity: Telescope<Telescope<()>>,
    result_sort: Term,
}

impl InductAt {
    /// The family's declared result sort, at these universes.
    pub(crate) fn result_sort(&self) -> &Term {
        &self.result_sort
    }

    /// The declaration's own parameter telescope, at this occurrence's universes.
    ///
    /// What [`InductAt::indices`] and [`StructAt::fields`] hand back is the *terminal*, already opened at the parameters this handle holds. This is the telescope those parameters were declared at, which is what a rule needs to type the arguments the constructor above only counted.
    pub(crate) fn parameters(&self) -> Telescope<Telescope<()>> {
        self.arity.clone()
    }

    /// `tag`'s signature at this occurrence's universes and parameters, instantiated alone, or `None` when the family has no such constructor.
    pub(crate) fn signature(
        &self,
        kernel: &Kernel<'_>,
        tag: &Atom,
    ) -> Result<Option<Telescope<Vec<Term>>>, Error> {
        let Some(constructor) = self
            .declaration(kernel)
            .at(&self.universes)
            .constructor(tag)
        else {
            return Ok(None);
        };

        Ok(Some(constructor?.telescope.open_params(&self.params)))
    }

    /// The family's constructor tags, in declaration order: what the arm rules and the elimination guard walk, having reached the family through this handle, which is what says the occurrence they belong to was checked.
    pub(crate) fn tags(&self, kernel: &Kernel<'_>) -> Vec<Atom> {
        self.declaration(kernel)
            .constructor_order()
            .cloned()
            .collect()
    }

    /// The family's index telescope at this occurrence's parameters.
    pub(crate) fn indices(&self) -> Telescope<()> {
        self.arity.open(&self.params.iter().collect::<Vec<_>>())
    }

    /// The declaration as declared, read again without being noted: the read was noted where this handle was made.
    fn declaration<'k>(&self, kernel: &'k Kernel<'_>) -> &'k InductDecl {
        kernel
            .globals
            .induct_decl(&self.name)
            .expect("a handle is made only for a declared family")
    }
}

/// A `struct` declaration at one occurrence's universes and parameters.
///
/// The sort is instantiated where the handle is made, for every rule reads it; the parameters, the fields and one field's type are each instantiated where they are read, so a structure of many fields is read at an occurrence in what the rule asks of it ([`StructInstance`](curios_core::StructInstance)).
pub(crate) struct StructAt {
    name: Global,
    universes: Universes,
    params: Vec<Term>,
    result_sort: Term,
}

impl StructAt {
    /// The family's declared result sort, at these universes.
    pub(crate) fn result_sort(&self) -> &Term {
        &self.result_sort
    }

    /// The declaration's own parameter telescope, at this occurrence's universes, the fields beneath it left out — [`InductAt::parameters`] for a structure.
    pub(crate) fn parameters(&self, kernel: &Kernel<'_>) -> Result<Telescope<()>, Error> {
        Ok(self.declaration(kernel).at(&self.universes).parameters()?)
    }

    /// The structure's field telescope at this occurrence's universes and parameters: what a rule reading every field takes.
    pub(crate) fn fields(&self, kernel: &Kernel<'_>) -> Result<Telescope<()>, Error> {
        Ok(self
            .declaration(kernel)
            .at(&self.universes)
            .fields_at(&self.params)?)
    }

    /// The type of the slot `field` reads off `value`, this structure, with that one field instantiated and opened ([`StructInstance::field_type`](curios_core::StructInstance::field_type)), or `None` for a slot past its fields.
    pub(crate) fn field_type(
        &self,
        kernel: &Kernel<'_>,
        value: &Term,
        field: &Field,
    ) -> Result<Option<Term>, Error> {
        Ok(self
            .declaration(kernel)
            .at(&self.universes)
            .field_type(&self.params, value, field)?)
    }

    /// How many fields the structure declares.
    pub(crate) fn field_count(&self, kernel: &Kernel<'_>) -> usize {
        self.declaration(kernel).field_count()
    }

    /// The declaration as declared, read again without being noted: the read was noted where this handle was made.
    fn declaration<'k>(&self, kernel: &'k Kernel<'_>) -> &'k StructDecl {
        kernel
            .globals
            .struct_decl(&self.name)
            .expect("a handle is made only for a declared structure")
    }
}

impl Kernel<'_> {
    /// An `induct` *type* occurrence's declaration: universes checked, parameter and index counts checked.
    pub(crate) fn induct_at(&self, family: &InductType) -> Result<InductAt, Error> {
        let at = self.induct_at_params(&family.name, &family.universes, &family.params)?;
        arity_matches(
            Counted::Indices,
            self.globals
                .induct_decl(&family.name)
                .expect("a handle is made only for a declared family")
                .index_count(),
            family.indices.len(),
        )?;

        Ok(at)
    }

    /// An `induct` occurrence that carries parameters but no indices — a constructor application, whose index targets are the declaration's rather than the occurrence's.
    pub(crate) fn induct_at_params(
        &self,
        name: &Global,
        universes: &Universes,
        params: &[Term],
    ) -> Result<InductAt, Error> {
        let declaration = self
            .induct_decl(name)
            .ok_or_else(|| Error::Undeclared(*name))?;
        self.check_instance(&declaration.universe_context, universes)?;
        arity_matches(Counted::Parameters, declaration.param_count(), params.len())?;

        let instance = declaration.at(universes);

        Ok(InductAt {
            name: *name,
            universes: universes.clone(),
            params: params.to_vec(),
            arity: instance.arity()?,
            result_sort: instance.result_sort()?,
        })
    }

    /// A `struct` occurrence's declaration: universes checked, parameter count checked.
    pub(crate) fn struct_at(
        &self,
        name: &Global,
        universes: &Universes,
        params: &[Term],
    ) -> Result<StructAt, Error> {
        let declaration = self
            .struct_decl(name)
            .ok_or_else(|| Error::Undeclared(*name))?;
        self.check_instance(&declaration.universe_context, universes)?;
        arity_matches(Counted::Parameters, declaration.param_count(), params.len())?;

        Ok(StructAt {
            name: *name,
            universes: universes.clone(),
            params: params.to_vec(),
            result_sort: declaration.at(universes).result_sort()?,
        })
    }
}
