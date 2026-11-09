//! What a context reads of the units in scope, where its own stores hold nothing under a name.
//!
//! A unit is judged against the units before it, and what it reads of them is read where they stand: the frames and the registries ask the scope on a miss ([`InScope`]) and are answered by reference into the unit that holds the declaration, so nothing of a scope is defined again, cloned or keyed again, and a compile pays for the declarations of its scope that it reads.
//!
//! **A scope is read and never written.** Its units are finished, so an answer holds for as long as the context does, and no cache is held against one. What a context writes — the unit's own declarations, a refused one's undoing — is its own stores', above the scope.
//!
//! **A trait, because two readers want two forms of one scope.** Elaboration reads the units as they were elaborated (`Established`), and erasure reads each declaration out of its universes, made the first time it is asked for.

use {
    curios_common::Qualifier,
    curios_core::{
        Applications, ConceptDecl, FuncType, Global, InductDecl, ParameterField, RecGroup,
        StructDecl, Subterm, Term, Totality, UniverseContext, Variance, WitnessKey,
    },
    std::fmt::Debug,
};

/// One recursive group a unit in scope declares: the group, the names its members are defined under in member order, and the universe context its declaration states.
///
/// The context is the item's and not the group's, because that is whose parameters a member's levels are ([`curios_core::RecGroup`]): a reader that opens a group at an assignment of sorts, or names its members again, asks the declaration.
#[derive(Debug, Clone)]
pub struct DefinedGroup {
    pub group: RecGroup,
    pub names: Vec<Global>,
    pub universe_context: UniverseContext,
}

/// One definition of a unit in scope, as a declaration reads it: every part a reference into the unit that holds it.
#[derive(Debug, Clone, Copy)]
pub struct Defined<'a> {
    pub type_: &'a Term,
    /// What the name unfolds to: a definition's body, a group's member as the group's projection, and nothing where the definition is a proof ([`curios_core::Definition::proof`]).
    pub body: Option<&'a Term>,
    pub universe_context: &'a UniverseContext,
    /// What its unit's elaboration concluded of it as it published it ([`crate::Classified::totality`]), `Partial` where it filed nothing: elaboration's own conclusion, and the only answer this checker has. The kernel reads the certifier's record of the unit instead, which is what makes its verdict a second one ([`curios-cert`'s README](../../../curios-cert/README.md)).
    pub totality: Totality,
    /// How it applies its parameters, where its publication filed that ([`crate::Classified::applications`]).
    pub applications: Option<&'a Applications>,
    /// The vector its publication filed, empty where it filed none ([`crate::Vectors::variances`]).
    pub variances: &'a [Variance],
    /// The vector of each field of its value its publication filed, empty where it filed none ([`crate::Vectors::field_variances`]).
    pub field_variances: &'a [Vec<Variance>],
    /// The field of one of its binders its value is, where it is one ([`crate::Vectors::parameter_field`]).
    pub parameter_field: Option<ParameterField>,
    /// The sort its value inhabits, where its value is a type ([`curios_core::Definition::value_sort`]), and nothing where the definition is a proof — the mark withholds this with the body, and both are read through [`curios_core::Definition::published`].
    pub value_sort: Option<&'a Term>,
    /// The module declaring it.
    pub island: Qualifier,
}

impl<'a> Defined<'a> {
    /// Its vector, with how many arguments one spine applies it in full by, where it is irrelevant in some level: what a context files for a definition of its own unit (`Context::record_vectors`), read here off what its unit filed.
    pub fn irrelevance(&self) -> Option<(&'a [Variance], usize)> {
        self.variances
            .contains(&Variance::Irrelevant)
            .then_some((self.variances, self.arity()))
    }

    /// The vector of field `slot` of its value, with its arity, where some field is irrelevant in some level: what a context files for a definition of its own unit (`Context::record_vectors`), read here off what its unit filed.
    pub fn field_irrelevance(&self, slot: usize) -> Option<(&'a [Variance], usize)> {
        if !self
            .field_variances
            .iter()
            .flatten()
            .any(|variance| *variance == Variance::Irrelevant)
        {
            return None;
        }

        Some((self.field_variances.get(slot)?.as_slice(), self.arity()))
    }

    /// The field of one of its binders its value is, with its arity: what a context files for a definition of its own unit (`Context::record_vectors`), read here off what its unit filed.
    pub fn parameter_field(&self) -> Option<(ParameterField, usize)> {
        Some((self.parameter_field?, self.arity()))
    }

    /// How many arguments one spine applies it in full by: its type's outer telescope.
    fn arity(&self) -> usize {
        match &**self.type_ {
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.len(),
            _ => 0,
        }
    }
}

/// The units in scope, answering by name.
pub trait InScope: Debug {
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
    fn groups(&self) -> Vec<DefinedGroup>;

    /// The names a unit in scope defines `group`'s members under, in member order.
    fn member_names(&self, group: &RecGroup) -> Option<Vec<Global>>;
}
