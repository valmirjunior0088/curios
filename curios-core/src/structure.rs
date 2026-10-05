use {
    super::{Polarity, Telescope, Term, UniverseContext, Variance},
    crate::Sharing,
    curios_utilities::{Plicity, Qualifier},
};

/// One struct declaration's registry entry: the metadata a `struct` declaration produces alongside its type-former binding.
///
/// A struct is a nominal record — there is no value-constructor function and no tag — so this is an [`InductDecl`](super::InductDecl) minus the indices and the per-constructor map, plus the privacy metadata the representation boundary needs. Elaboration consults it to check a struct literal's fields and to type a projection; `erase` consults it to lower the fields.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct StructDecl {
    pub universe_context: UniverseContext,
    /// The declaration's parameters, terminating in its field telescope: `struct Pair(A : Type, B : Type) { fst : A, snd : B }` is `(A : Type, B : Type)` ending in `(fst : A, snd : B)` ending in `()`.
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
    /// What each of its `params` binds as, one mark per parameter in declaration order — the marks the type former's own function type carries, kept here for the elaborator, which opens the parameters over the fields and has no function type to read them from. A `use` parameter joins the witness scope the field types resolve in. Read through [`Self::plicity`], never indexed directly; the kernel reads none of it.
    pub plicities: Vec<Plicity>,
}

impl StructDecl {
    /// How many of this declaration's binders are uniform parameters.
    pub fn param_count(&self) -> usize {
        self.arity.len()
    }

    /// This structure's field telescope, still under its parameter binders. Use [`StructDecl::fields_at`] to read it at known parameters.
    pub fn fields(&self) -> &Telescope<()> {
        let mut telescope = &self.arity;
        loop {
            match telescope {
                Telescope::Cons(_, _, rest) => telescope = rest.body(),
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

    /// What this declaration's `i`th parameter binds as. A declaration that states no marks has plain parameters only.
    pub fn plicity(&self, i: usize) -> Plicity {
        self.plicities.get(i).copied().unwrap_or(Plicity::Explicit)
    }

    /// This declaration with every term hash-consed against `sharing`. See [`Module::shared`](crate::Module::shared).
    pub fn shared(&self, sharing: &Sharing) -> Self {
        Self {
            universe_context: self.universe_context.clone(),
            arity: sharing.share(&self.arity),
            result_sort: sharing.share(&self.result_sort),
            module: self.module,
            rep_public: self.rep_public,
            polarities: self.polarities.clone(),
            variances: self.variances.clone(),
            plicities: self.plicities.clone(),
        }
    }

    /// This structure's field telescope at known parameters: `fields_at([Nat, Bin])` for `Pair` is `(fst : Nat, snd : Bin)`. The fields are the arity's terminal, so this opens it rather than peeling a repeated prefix.
    pub fn fields_at(&self, params: &[Term]) -> Telescope<()> {
        self.arity.open(&params.iter().collect::<Vec<_>>())
    }
}
