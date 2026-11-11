//! Algebraic universe levels and declaration-local schemes: the data a universe judgment is stated over. Solving it is `curios-elab`'s `universes`, and deciding whether a constraint set is satisfiable is each checker's own.
//!
//! Surface Curios has no level syntax. `curios-module` assigns a fresh [`UniverseMetaId`] and diagnostic [`UniverseSeed`] to every written `Type` and every written `Prop`; Core normalizes levels to finite maxima of a constant and checked-offset parameter/meta atoms. `Type u : Type (u + 1)` and `Prop u : Type (u + 1)`, and checking `Type u` or `Prop u` against `Type v`, or `Prop u` against `Prop v`, records `u ≤ v`.
//!
//! Reusable declarations bind surviving input levels in a [`UniverseContext`]. Flexible classifier levels take their principal least solution when one exists; genuinely non-principal choices remain residual constrained parameters. Every external occurrence instantiates the stored context freshly. Local schemes may temporarily refer to ambient metas and are capture-safely rewritten beneath the enclosing declaration's parameters; recursive members instead share one context and one internal instance.
//!
//! A constraint is its inequality alone, so its equality, its hash and its archived bytes are the inequality's, and a context's are its counts' and its constraints': where a constraint was raised is the elaborator's solver's, which holds it beside the constraint ([`UniverseConstraintOrigin`]). Zonked Core validates that contexts are closed and nominal instance arities agree, then erasure removes every level, context, and instance before ANF.

use {
    super::{Extent, Global},
    curios_common::Mint,
    std::{collections::BTreeMap, fmt, hash::Hash},
};

/// A lowering- or elaboration-minted universe metavariable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct UniverseMetaId(pub usize);

impl From<usize> for UniverseMetaId {
    fn from(raw: usize) -> Self {
        Self(raw)
    }
}

impl Mint for UniverseMetaId {
    fn mint(entropy: u64) -> Self {
        Self(usize::mint(entropy))
    }
}

impl fmt::Display for UniverseMetaId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "?u{}", self.0)
    }
}

/// A declaration-local, de Bruijn-indexed universe parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct UniverseParam(pub usize);

/// The head of one non-constant level atom.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub enum LevelHead {
    Param(UniverseParam),
    Meta(UniverseMetaId),
}

/// A canonical algebraic universe level.
///
/// The value denotes `max(constant, head₁ + offset₁, …)`. A `BTreeMap` provides deterministic atom ordering; construction coalesces duplicate heads at their greatest offset, making equality, hashing, and archival independent of the expression's original association and ordering.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub struct Level {
    pub constant: u32,
    pub atoms: BTreeMap<LevelHead, u32>,
}

impl Default for Level {
    fn default() -> Self {
        Self::zero()
    }
}

impl Level {
    fn normalized(mut self) -> Self {
        // Every parameter/meta ranges over naturals. Therefore `head + k` already dominates any constant `n ≤ k`.
        if self.atoms.values().any(|offset| *offset >= self.constant) {
            self.constant = 0;
        }
        self
    }

    pub fn zero() -> Self {
        Self {
            constant: 0,
            atoms: BTreeMap::new(),
        }
    }

    pub fn constant(value: u32) -> Self {
        Self {
            constant: value,
            atoms: BTreeMap::new(),
        }
    }

    pub fn param(param: UniverseParam) -> Self {
        Self::atom(LevelHead::Param(param), 0)
    }

    pub fn meta(meta: UniverseMetaId) -> Self {
        Self::atom(LevelHead::Meta(meta), 0)
    }

    pub(crate) fn atom(head: LevelHead, offset: u32) -> Self {
        Self {
            constant: 0,
            atoms: BTreeMap::from([(head, offset)]),
        }
    }

    pub fn constant_part(&self) -> u32 {
        self.constant
    }

    pub fn atoms(&self) -> impl Iterator<Item = (LevelHead, u32)> + '_ {
        self.atoms.iter().map(|(&head, &offset)| (head, offset))
    }

    pub fn is_zero(&self) -> bool {
        self.constant == 0 && self.atoms.is_empty()
    }

    /// Whether the level algebra alone proves `self ≤ upper`, without using any surrounding constraints.
    pub fn structurally_leq(&self, upper: &Self) -> bool {
        let constant_is_bounded = self.constant <= upper.constant
            || upper.atoms.values().any(|offset| *offset >= self.constant);
        constant_is_bounded
            && self.atoms.iter().all(|(head, offset)| {
                upper
                    .atoms
                    .get(head)
                    .is_some_and(|upper_offset| offset <= upper_offset)
            })
    }

    pub fn is_closed(&self, parameter_count: usize) -> bool {
        self.atoms.keys().all(|head| match head {
            LevelHead::Param(param) => param.0 < parameter_count,
            LevelHead::Meta(_) => false,
        })
    }

    pub fn metas(&self) -> impl Iterator<Item = UniverseMetaId> + '_ {
        self.atoms.keys().filter_map(|head| match head {
            LevelHead::Meta(meta) => Some(*meta),
            LevelHead::Param(_) => None,
        })
    }

    pub fn params(&self) -> impl Iterator<Item = UniverseParam> + '_ {
        self.atoms.keys().filter_map(|head| match head {
            LevelHead::Param(param) => Some(*param),
            LevelHead::Meta(_) => None,
        })
    }

    /// Add a successor offset, distributing it over the canonical maximum.
    pub fn checked_add(&self, offset: u32) -> Result<Self, UniverseError> {
        let constant = self
            .constant
            .checked_add(offset)
            .ok_or(UniverseError::OffsetOverflow)?;
        let atoms = self
            .atoms
            .iter()
            .map(|(&head, &old)| {
                old.checked_add(offset)
                    .map(|new| (head, new))
                    .ok_or(UniverseError::OffsetOverflow)
            })
            .collect::<Result<_, _>>()?;
        Ok(Self { constant, atoms }.normalized())
    }

    pub fn succ(&self) -> Result<Self, UniverseError> {
        self.checked_add(1)
    }

    /// Cancel a common successor offset from a lower bound. Algebraic levels have no predecessor former, so an atom below the cancelled offset has no principal expression in the level language.
    pub fn cancel_offset(&self, offset: u32) -> Option<Self> {
        let mut atoms = BTreeMap::new();
        for (&head, &old) in &self.atoms {
            atoms.insert(head, old.checked_sub(offset)?);
        }
        Some(
            Self {
                constant: self.constant.saturating_sub(offset),
                atoms,
            }
            .normalized(),
        )
    }

    /// Construct the canonical least upper bound.
    pub fn max(levels: impl IntoIterator<Item = Level>) -> Self {
        let mut result = Self::zero();
        for level in levels {
            result.constant = result.constant.max(level.constant);
            for (head, offset) in level.atoms {
                result
                    .atoms
                    .entry(head)
                    .and_modify(|old| *old = (*old).max(offset))
                    .or_insert(offset);
            }
        }
        result.normalized()
    }

    /// Substitute heads simultaneously and normalize the resulting maximum.
    ///
    /// This is [`Level::max`] over one part per atom, accumulated straight into the result rather than materialized as a vector of parts. The distinction is not stylistic: a part built for an atom *nothing replaces* is that atom back again, and building it cost two `BTreeMap` allocations.
    ///
    /// `Self::atom(head, 0)` is `{0, {head → 0}}`; `checked_add(offset)` raises both halves to `offset` and then normalizes, and normalization zeroes a constant no greater than some atom's offset — so the part is exactly `{0, {head → offset}}`, which is the entry already in `self.atoms`. Two consequences make the unreplaced arm below equivalent rather than merely close: it contributes `0` to the constant and so cannot raise it, and neither of those additions can overflow, so it cannot be the arm that fails.
    ///
    /// `replacement` is still called exactly once per atom, in the same order. That is deliberate, and it is why there is no "probe first, clone if nothing matched" fast path — [`FnMut`] promises nothing about purity, so calling it twice would change behaviour where this does not.
    pub fn substitute(
        &self,
        mut replacement: impl FnMut(LevelHead) -> Option<Level>,
    ) -> Result<Self, UniverseError> {
        fn raise(atoms: &mut BTreeMap<LevelHead, u32>, head: LevelHead, offset: u32) {
            atoms
                .entry(head)
                .and_modify(|old| *old = (*old).max(offset))
                .or_insert(offset);
        }

        let mut constant = self.constant;
        let mut atoms = BTreeMap::new();

        for (&head, &offset) in &self.atoms {
            match replacement(head) {
                None => raise(&mut atoms, head, offset),
                Some(base) => {
                    let part = base.checked_add(offset)?;
                    constant = constant.max(part.constant);
                    for (head, offset) in part.atoms {
                        raise(&mut atoms, head, offset);
                    }
                }
            }
        }

        Ok(Self { constant, atoms }.normalized())
    }

    pub fn instantiate(&self, arguments: &[Level]) -> Result<Self, UniverseError> {
        if self.params().any(|param| param.0 >= arguments.len()) {
            return Err(UniverseError::EscapingLevel);
        }
        self.substitute(|head| match head {
            LevelHead::Param(param) => arguments.get(param.0).cloned(),
            LevelHead::Meta(_) => None,
        })
    }
}

/// A declaration-local quality parameter: which of the sorts its top-level item leaves to a caller this is, counted from zero in that item's [`UniverseContext`].
///
/// Flat, where a [`UniverseParam`] is scoped: only a top-level item binds a quality, so one names the same parameter at every depth beneath it and nothing shifts it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct QualityParam(pub usize);

/// An elaboration-minted quality metavariable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct QualityMetaId(pub usize);

impl fmt::Display for QualityMetaId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "?q{}", self.0)
    }
}

/// What kind of universe a sort is, beside its level: of propositions, of types, or one not stated.
///
/// Any two inhabitants of a proposition are one to conversion and a type's are not, so every judgment that reads a sort reads its quality. An open quality is neither: a parameter is a sort its declaration leaves to each caller, and a metavariable one its elaboration has not solved. The certifier judges no term that holds one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub enum Quality {
    Prop,
    Type,
    Param(QualityParam),
    Meta(QualityMetaId),
}

impl Quality {
    /// Whether this is the quality of propositions. An open quality is not: what it will be is not what it is.
    pub fn is_prop(&self) -> bool {
        matches!(self, Quality::Prop)
    }

    /// Whether this states a sort: `Prop` or `Type`, and neither a parameter nor a metavariable.
    pub fn is_ground(&self) -> bool {
        matches!(self, Quality::Prop | Quality::Type)
    }

    /// Every assignment of `count` sorts, each `Prop` or `Type`, the one stating `Prop` throughout first: the one empty assignment for none.
    pub fn assignments(count: usize) -> impl Iterator<Item = Vec<Quality>> {
        (0..1usize << count).map(move |chosen| {
            (0..count)
                .map(|sort| match (chosen >> sort) & 1 {
                    0 => Quality::Prop,
                    _ => Quality::Type,
                })
                .collect()
        })
    }

    /// This quality at an instance: a parameter as the quality `arguments` states for it, and anything else as it stands. A parameter past the instance's width is left, for the reader that refuses an open sort to meet.
    pub fn instantiate(&self, arguments: &[Quality]) -> Quality {
        match self {
            Quality::Param(param) => arguments.get(param.0).copied().unwrap_or(*self),
            _ => *self,
        }
    }
}

/// The instance an occurrence of a universe-polymorphic declaration states: a level for each level parameter of its [`UniverseContext`], and a quality for each sort the declaration leaves to its caller.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[curios_archive::archived]
pub struct Universes {
    pub levels: Vec<Level>,
    pub qualities: Vec<Quality>,
}

impl Universes {
    /// The instance of a declaration that leaves no sort open: its levels, and no quality.
    pub fn levels(levels: Vec<Level>) -> Self {
        Self {
            levels,
            qualities: Vec::new(),
        }
    }

    /// Whether this states nothing: the instance of a declaration with no universe parameter of either kind.
    pub fn is_empty(&self) -> bool {
        self.levels.is_empty() && self.qualities.is_empty()
    }

    /// Whether elaboration has left something of this unsolved: a level that names a metavariable, or a quality that is one.
    pub fn has_meta(&self) -> bool {
        self.levels
            .iter()
            .any(|level| level.metas().next().is_some())
            || self
                .qualities
                .iter()
                .any(|quality| matches!(quality, Quality::Meta(_)))
    }
}

impl From<Vec<Level>> for Universes {
    fn from(levels: Vec<Level>) -> Self {
        Self::levels(levels)
    }
}

impl<const N: usize> From<[Level; N]> for Universes {
    fn from(levels: [Level; N]) -> Self {
        Self::levels(levels.into())
    }
}

impl FromIterator<Level> for Universes {
    /// An instance stating levels alone, which is every instance of a declaration that leaves no sort open.
    fn from_iter<I: IntoIterator<Item = Level>>(levels: I) -> Self {
        Self::levels(levels.into_iter().collect())
    }
}

/// Whether an unsolved level is an input eligible for generalization or an inferred output/classifier that should be minimized.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum UniverseRole {
    Generalizable,
    Flexible,
}

/// Lowering-time metadata for one densely numbered universe metavariable.
///
/// The role controls finalization, while where its `Type` was written survives the Text/Core boundary so a later constraint failure can still point at the written `Type` that introduced the level.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct UniverseSeed {
    pub role: UniverseRole,
    pub written: Option<WrittenSort>,
}

/// Where a written `Type`, `Prop` or `Sort` stands and which of them it is: what the lowering keeps of the level or the sort it minted for one, archived with the lowered module, which keeps its positions. The solver makes of it the origin of what that level or sort raises ([`WrittenSort::origin`]), and nothing else stored holds one.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct WrittenSort {
    pub extent: Option<Extent>,
    pub kind: UniverseConstraintKind,
    /// The path the declaration it was written in stands under, where that is not the root.
    pub declaration: Option<String>,
}

impl WrittenSort {
    /// Where a constraint raised on this sort's level or quality was raised, as the solver that raised it holds it.
    pub fn origin(&self) -> UniverseConstraintOrigin {
        UniverseConstraintOrigin {
            extent: self.extent,
            kind: self.kind.clone(),
            declaration: self.declaration.clone(),
            binder: None,
            instantiated: None,
        }
    }
}

/// The semantic reason a universe inequality was introduced.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum UniverseConstraintKind {
    WrittenType,
    WrittenProp,
    WrittenSort,
    Cumulativity,
    PropositionCumulativity,
    TypeSuccessor,
    FunctionFormation,
    TupleFormation,
    FieldSizing,
    ConstructorSizing,
    Conversion,
    SchemeInstantiation,
    Other(String),
}

/// Whether an inconsistency's path runs through a proposition: one written, or one put below a sort.
pub fn names_a_proposition(path: &[UniverseConstraintOrigin]) -> bool {
    path.iter().any(|origin| {
        matches!(
            origin.kind,
            UniverseConstraintKind::WrittenProp | UniverseConstraintKind::PropositionCumulativity
        )
    })
}

/// The declarations whose schemes an inconsistency's path runs through, each once and in the order the path meets them: what a report names beside the constraint, since a bound a scheme states is written in no term of the declaration that broke it.
pub fn instantiated_on(path: &[UniverseConstraintOrigin]) -> Vec<Global> {
    let mut named = Vec::new();
    for origin in path {
        if let Some(instantiated) = origin.instantiated
            && !named.contains(&instantiated)
        {
            named.push(instantiated);
        }
    }

    named
}

/// The sort an inconsistency is reported about, as its reader wrote it: `Prop` where its path runs through a proposition and through no type, `Type` otherwise.
pub fn inconsistent_sort(path: &[UniverseConstraintOrigin]) -> &'static str {
    let through_a_type = path.iter().any(|origin| {
        matches!(
            origin.kind,
            UniverseConstraintKind::WrittenType | UniverseConstraintKind::Cumulativity
        )
    });

    match names_a_proposition(path) && !through_a_type {
        true => "Prop",
        false => "Type",
    }
}

/// Where a universe constraint was raised, as the solver that raised it holds it beside the constraint, and as a conflict's path reports it: a scheme stores its constraints as their inequalities alone ([`UniverseConstraint`]), so nothing stored holds one.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UniverseConstraintOrigin {
    pub extent: Option<Extent>,
    pub kind: UniverseConstraintKind,
    pub declaration: Option<String>,
    pub binder: Option<String>,
    /// The declaration an occurrence instantiated, where the constraint is one that declaration's scheme states, restated at the occurrence's instance: whose bound it is, to a reader who wrote the occurrence and never the bound. A scheme stores none: it is an instance's to say.
    pub instantiated: Option<Global>,
}

impl UniverseConstraintOrigin {
    pub fn new(kind: UniverseConstraintKind) -> Self {
        Self {
            extent: None,
            kind,
            declaration: None,
            binder: None,
            instantiated: None,
        }
    }
}

/// One normalized inequality `lower ≤ upper`, and nothing of where it was raised: that is the solver's that raised it ([`UniverseConstraintOrigin`]), so a scheme says what it means and the same inequality raised twice is one constraint.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct UniverseConstraint {
    pub lower: Level,
    pub upper: Level,
}

/// A closed, declaration-local residual universe context.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
#[curios_archive::archived]
pub struct UniverseContext {
    pub parameter_count: usize,
    /// How many sorts the declaration leaves to its caller: its quality parameters, which only a top-level declaration binds — a definition, a recursive group, a nominal declaration. A group standing inside a term binds none: a quality parameter there is the enclosing declaration's, and a walk that states a declaration's qualities states them through every group it passes.
    pub quality_count: usize,
    pub constraints: Vec<UniverseConstraint>,
}

impl UniverseContext {
    /// The most sorts one declaration leaves to its caller. The certifier judges a declaration once at every assignment of them, so the count bounds what a declaration costs: two to its power judgments.
    pub const QUALITY_LIMIT: usize = 8;

    pub fn empty() -> Self {
        Self::default()
    }

    /// How many universe parameters of either kind the declaration binds, which is how much an instance of it states: nothing, for a monomorphic declaration.
    pub fn width(&self) -> usize {
        self.parameter_count + self.quality_count
    }

    /// Whether `universes` states this scheme whole: a level for each parameter and a sort for each one the declaration leaves. Only such an instance denotes one of the scheme's instances; one short of it, read at a body or a sort, would read the scheme's own parameters as its reader's.
    pub fn stated_by(&self, universes: &Universes) -> bool {
        universes.levels.len() == self.parameter_count
            && universes.qualities.len() == self.quality_count
    }

    /// Whether this context mentions only what it declares: no constraint level naming a parameter past `parameter_count`, and none holding a metavariable.
    ///
    /// A context is closed. Universe polymorphism belongs to declarations, so there is no enclosing scheme whose parameters a constraint could still name, and elaboration is over by the time anything asks. A constraint naming a parameter past the count is not a stronger hypothesis but a meaningless one — instantiation substitutes an argument vector of the declared length, and a reference past its end has nothing to become — while a constraint carrying a metavariable is elaboration residue a zonked module cannot contain. Both are refused rather than interpreted.
    ///
    /// **A method on the data, not a judgment.** A copy in each checker would be a second opinion worth nothing (see `documentation/design/soundness/an-independent-certifier-re-checks-what-the-elaborator-accepts.md`): the predicate is too simple to have two genuine implementations — any "independent" rewrite would agree by construction rather than by independence — so two copies would buy a diff test over a copy of themselves. It lives here for the reason [`Level::structurally_leq`] does: it decides a property of the representation by looking at it, runs no solver, and admits nothing on its own. Deciding *satisfiability* is the opposite case and stays written twice — see `curios-cert`'s `satisfiable` for why.
    pub fn is_closed(&self) -> bool {
        let within = |level: &Level| {
            level.params().all(|param| param.0 < self.parameter_count)
                && level.metas().next().is_none()
        };

        self.constraints
            .iter()
            .all(|constraint| within(&constraint.lower) && within(&constraint.upper))
    }

    /// This context's own parameters as an argument vector: the one instance that instantiates it to itself.
    ///
    /// A declaration denotes this instance at every occurrence inside its own signature, body, and registry entries, because a group is monomorphic in its own universes. External uses instead take a fresh instance from `curios-elab`'s `UniverseSolver::instantiate`.
    pub fn identity_instance(&self) -> Universes {
        Universes {
            levels: (0..self.parameter_count)
                .map(UniverseParam)
                .map(Level::param)
                .collect(),
            qualities: (0..self.quality_count)
                .map(QualityParam)
                .map(Quality::Param)
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UniverseError {
    OffsetOverflow,
    EscapingLevel,
    InstanceArity {
        expected: usize,
        got: usize,
    },
    /// An instance states `got` sorts where its scheme leaves `expected` to its caller.
    QualityArity {
        expected: usize,
        got: usize,
    },
    UnknownMeta(UniverseMetaId),
    MismatchedRecursiveContexts,
    /// The disjunctive consistency search exceeded its node budget.
    ///
    /// Deciding a set of inequalities whose right-hand sides are genuine maxima means choosing, for each left atom, which branch dominates it — a search exponential in the number of such clauses. The budget makes that blowup a reported diagnostic rather than an unbounded spin, and names the shape that caused it.
    SearchExhausted {
        constraints: usize,
        branches: usize,
        widths: Vec<usize>,
    },
    Inconsistency {
        lower: Level,
        upper: Level,
        path: Vec<UniverseConstraintOrigin>,
    },
}

#[cfg(test)]
mod tests;
