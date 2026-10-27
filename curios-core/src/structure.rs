#[cfg(test)]
mod tests;

use {
    super::{
        Field, Polarity, Quality, Telescope, Term, UniverseContext, UniverseError, Universes,
        Variance, instantiate_qualities, instantiate_universes_scoped, open_field,
    },
    crate::Sharing,
    curios_utilities::Qualifier,
};

/// One struct declaration's registry entry: the metadata a `struct` declaration produces alongside its type-former binding.
///
/// A struct is a nominal record — there is no value-constructor function and no tag — so this is an [`InductDecl`](super::InductDecl) minus the indices and the per-constructor map, plus the privacy metadata the representation boundary needs. Elaboration consults it to check a struct literal's fields and to type a projection; `erase` consults it to lower the fields.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct StructDecl {
    pub universe_context: UniverseContext,
    /// The declaration's parameters, terminating in its field telescope: `struct Pair(A : Type, B : Type) { fst : A, snd : B }` is `(A : Type, B : Type)` ending in `(fst : A, snd : B)` ending in `()`. Each parameter stands under the mark it binds as — the marks the type former's own function type carries, stated here for the elaborator, which opens the parameters over the fields and has no function type to read them from. A `use` parameter joins the witness scope the field types resolve in. The kernel reads no mark of either telescope.
    ///
    /// Nested rather than two parallel telescopes, for the reason [`InductDecl::arity`](super::InductDecl) gives: the fields are scoped under the parameters *by construction*, so there is no agreement between two encodings to state and no malformed pairing for a judgment to refuse.
    pub arity: Telescope<Telescope<()>>,
    /// The declared result sort — `Type` or `Prop` — the codomain of the type-former's kind. A fully-applied `StructType { name, .. }` has this sort, which `Sort::of` reads to decide propositional irrelevance.
    pub result_sort: Term,
    /// The declaring module's qualifier (e.g. `Foo/Bar`); the root module is the empty qualifier. Compared against the use-site module for the representation-privacy checks.
    pub module: Qualifier,
    /// The inner `pub`: whether the representation — construction and projection — is exported.
    pub rep_public: bool,
    /// How this struct uses each of its `params`, one entry per parameter in declaration order. See [`InductDecl::polarities`](super::InductDecl).
    pub polarities: Vec<Polarity>,
    /// How two instances of this struct compare in each of its universe parameters, one entry per parameter of `universe_context`. See [`InductDecl::variances`](super::InductDecl).
    pub variances: Vec<Variance>,
}

impl StructDecl {
    /// How many of this declaration's binders are uniform parameters.
    pub fn param_count(&self) -> usize {
        self.arity.len()
    }

    /// [`InductDecl::at_qualities`](super::InductDecl::at_qualities) for a `struct` registry entry.
    pub fn at_qualities(&self, qualities: &[Quality]) -> Self {
        let mut at = self.clone();
        at.arity = instantiate_qualities(&at.arity, qualities);
        at.result_sort = instantiate_qualities(&at.result_sort, qualities);
        at.universe_context.quality_count = 0;

        at
    }

    /// This structure's field telescope, still under its parameter binders. Use [`StructDecl::fields_at`] to read it at known parameters.
    pub fn fields(&self) -> &Telescope<()> {
        let mut telescope = &self.arity;
        loop {
            match telescope {
                Telescope::Cons(_, _, rest, ..) => telescope = rest.body(),
                Telescope::Done(fields) => return fields,
            }
        }
    }

    /// How many fields this structure declares, read without instantiating it.
    pub fn field_count(&self) -> usize {
        self.fields().len()
    }

    /// This declaration's polarity in its `i`th parameter, defaulting to [`Polarity::Mixed`] before the declaration is analyzed. See [`InductDecl::polarity`](super::InductDecl).
    pub fn polarity(&self, i: usize) -> Polarity {
        self.polarities.get(i).copied().unwrap_or(Polarity::Mixed)
    }

    /// This declaration's variance in its `i`th universe parameter, defaulting to [`Variance::Invariant`] before the declaration is analyzed. See [`InductDecl::variance`](super::InductDecl).
    pub fn variance(&self, i: usize) -> Variance {
        self.variances
            .get(i)
            .copied()
            .unwrap_or(Variance::Invariant)
    }

    /// This declaration with every term hash-consed against `sharing`. See [`Module::shared`](crate::Module::shared).
    pub fn shared(&self, sharing: &Sharing) -> Self {
        Self {
            universe_context: self.universe_context.unplaced(),
            arity: sharing.share(&self.arity),
            result_sort: sharing.share(&self.result_sort),
            module: self.module,
            rep_public: self.rep_public,
            polarities: self.polarities.clone(),
            variances: self.variances.clone(),
        }
    }

    /// This structure's field telescope at known parameters: `fields_at([Nat, Bytes])` for `Pair` is `(fst : Nat, snd : Bytes)`. The fields are the arity's terminal, so this opens it rather than peeling a repeated prefix.
    pub fn fields_at(&self, params: &[Term]) -> Telescope<()> {
        self.arity.open(&params.iter().collect::<Vec<_>>())
    }

    /// This declaration at `universes`, read a facet at a time.
    pub fn at<'d>(&'d self, universes: &'d Universes) -> StructInstance<'d> {
        StructInstance {
            declaration: self,
            universes,
        }
    }
}

/// A structure declaration read at one occurrence's universes: each facet is instantiated where it is read, so a read of the sort, the parameters or one field pays for that facet and never for every field the declaration has. What [`StructDecl::at`] hands back.
#[derive(Clone, Copy)]
pub struct StructInstance<'d> {
    declaration: &'d StructDecl,
    universes: &'d Universes,
}

impl<'d> StructInstance<'d> {
    /// The declaration as declared: what no universe instance changes — its module, its privacy, its counts, its vectors.
    pub fn declaration(&self) -> &'d StructDecl {
        self.declaration
    }

    /// The parameter telescope at these universes, the fields beneath it left out: what an occurrence's parameters are checked against.
    pub fn parameters(&self) -> Result<Telescope<()>, UniverseError> {
        instantiate_universes_scoped(&self.declaration.arity.prefix(), self.universes)
    }

    /// The declared result sort, at these universes.
    pub fn result_sort(&self) -> Result<Term, UniverseError> {
        instantiate_universes_scoped(&self.declaration.result_sort, self.universes)
    }

    /// The field telescope at these universes and `params` ([`StructDecl::fields_at`]): what a reader of every field takes.
    pub fn fields_at(&self, params: &[Term]) -> Result<Telescope<()>, UniverseError> {
        Ok(
            instantiate_universes_scoped(&self.declaration.arity, self.universes)?
                .open(&params.iter().collect::<Vec<_>>()),
        )
    }

    /// The type of the slot `field` reads off `value`, a structure at `params`, at these universes: what [`Telescope::field_type_from`] reads off [`Self::fields_at`], with that one field instantiated and opened — each field before it at its projection off `value`, each parameter at `params` — and no other. `None` for a slot past the fields, and for a written form.
    pub fn field_type(
        &self,
        params: &[Term],
        value: &Term,
        field: &Field,
    ) -> Result<Option<Term>, UniverseError> {
        assert_eq!(
            params.len(),
            self.declaration.param_count(),
            "a structure's fields are read at every parameter"
        );
        let Some(index) = field.slot() else {
            return Ok(None);
        };
        let Some(written) = self.declaration.fields().entry_type(index) else {
            return Ok(None);
        };
        // The levels first, of the declaration's own type: a parameter stands at the occurrence's.
        let instantiated = instantiate_universes_scoped(written, self.universes)?;

        Ok(Some(open_field(&instantiated, index, params, value, field)))
    }

    /// Every facet at these universes: what a reader of the whole declaration — a structure literal's fields, a derivation — takes.
    pub fn whole(&self) -> Result<StructDecl, UniverseError> {
        let mut instantiated = self.declaration.clone();
        instantiated.arity = instantiate_universes_scoped(&self.declaration.arity, self.universes)?;
        instantiated.result_sort = self.result_sort()?;
        instantiated.universe_context = UniverseContext::empty();
        Ok(instantiated)
    }
}
