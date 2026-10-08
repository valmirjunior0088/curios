//! What a context reads of the units in scope, where its own stores hold nothing under a name.
//!
//! A unit is judged against the units before it, and what it reads of them is read where they stand: the frames and the registries ask the scope on a miss ([`InScope`]) and are answered by reference into the unit that holds the declaration, so nothing of a scope is defined again, cloned or keyed again, and a compile pays for the declarations of its scope that it reads.
//!
//! **A scope is read and never written.** Its units are finished, so an answer holds for as long as the context does, and no cache is held against one. What a context writes — the unit's own declarations, a refused one's undoing — is its own stores', above the scope.
//!
//! **A trait, because two readers want two forms of one scope.** Elaboration reads the units as they were elaborated (`Established`), and erasure reads each declaration out of its universes, made the first time it is asked for.

use {
    curios_core::{
        ConceptDecl, DefinitionKind, FuncType, Global, InductDecl, RecGroup, StructDecl, Subterm,
        Term, Totality, UniverseContext, Variance, WitnessKey,
    },
    curios_utilities::Qualifier,
    std::fmt::Debug,
};

/// One definition of a unit in scope, as a declaration reads it: every part a reference into the unit that holds it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Defined<'a> {
    pub(crate) type_: &'a Term,
    /// What the name unfolds to: a definition's body, a group's member as the group's projection, and nothing where the definition is a proof ([`curios_core::Definition::proof`]).
    pub(crate) body: Option<&'a Term>,
    pub(crate) kind: &'a DefinitionKind,
    pub(crate) universe_context: &'a UniverseContext,
    pub(crate) totality: Totality,
    /// The vector its publication filed, empty where it filed none.
    pub(crate) variances: &'a [Variance],
    /// The module declaring it.
    pub(crate) island: Qualifier,
}

impl<'a> Defined<'a> {
    /// Its vector, with how many arguments one spine applies it in full by, where it is irrelevant in some level: what a context files for a definition of its own unit (`Context::record_definition_variances`), read here off the definition.
    pub(crate) fn irrelevance(&self) -> Option<(&'a [Variance], usize)> {
        let arity = match &**self.type_ {
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.len(),
            _ => 0,
        };

        self.variances
            .contains(&Variance::Irrelevant)
            .then_some((self.variances, arity))
    }
}

/// The units in scope, answering by name.
pub(crate) trait InScope: Debug {
    /// The definition a unit in scope declares under `name`.
    fn definition(&self, name: &Global) -> Option<Defined<'_>>;

    fn induct_decl(&self, name: &Global) -> Option<&InductDecl>;

    fn struct_decl(&self, name: &Global) -> Option<&StructDecl>;

    fn concept(&self, name: &Global) -> Option<&ConceptDecl>;

    /// The witness a unit in scope declares at `(concept, key)`, by its name.
    fn witness(&self, concept: &Global, key: &WitnessKey) -> Option<Global>;

    /// Every witness the units in scope declare, each with its concept and its key: the raw material of a report's survey, never of a resolution.
    fn witnesses(&self) -> Vec<(Global, WitnessKey, Global)>;

    /// Every recursive group the units in scope declare, each with its members' names in member order: what a report folds a group's unfolding back to.
    fn groups(&self) -> Vec<(RecGroup, Vec<Global>)>;

    /// The names a unit in scope defines `group`'s members under, in member order.
    fn member_names(&self, group: &RecGroup) -> Option<Vec<Global>>;
}
