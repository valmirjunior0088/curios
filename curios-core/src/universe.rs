//! Algebraic universe levels, declaration-local schemes, and the transactional constraint solver used by elaboration.
//!
//! Surface Curios has no level syntax. `curios-text` assigns a fresh [`UniverseMetaId`] and diagnostic [`UniverseSeed`] to every written `Type`; Core normalizes levels to finite maxima of a constant and checked-offset parameter/meta atoms. `Prop` remains a separate impredicative sort, while `Type u : Type (u + 1)` and checking `Type u` against `Type v` records `u ≤ v`.
//!
//! Reusable declarations bind surviving input levels in a [`UniverseContext`]. Flexible classifier levels take their principal least solution when one exists; genuinely non-principal choices remain residual constrained parameters. Every external occurrence instantiates the stored context freshly. Local schemes may temporarily refer to ambient metas and are capture-safely rewritten beneath the enclosing declaration's parameters; recursive members instead share one context and one internal instance.
//!
//! Constraint provenance is diagnostic-only: semantic equality and hashing compare normalized inequalities but ignore their spans and explanations. Consistency reduces ordinary inequalities to one difference graph and branches only for genuine maxima on the right. Solver marks cover both assignments and constraints so failed speculative elaboration rolls them back together. Zonked Core validates that contexts are closed and nominal instance arities agree, then erasure removes every level, context, and instance before Ersd.

use {
    curios_base::{Mint, Span},
    std::{
        collections::BTreeMap,
        fmt,
        hash::{Hash, Hasher},
    },
};

/// A lowering- or elaboration-minted universe metavariable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(derive(PartialEq, Eq, PartialOrd, Ord, Hash))
)]
pub struct UniverseMetaId(pub usize);

impl From<usize> for UniverseMetaId {
    fn from(raw: usize) -> Self {
        Self(raw)
    }
}

impl Mint for UniverseMetaId {
    fn mint(entropy: usize) -> Self {
        Self(entropy)
    }
}

impl fmt::Display for UniverseMetaId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "?u{}", self.0)
    }
}

/// A declaration-local, de Bruijn-indexed universe parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(derive(PartialEq, Eq, PartialOrd, Ord, Hash))
)]
pub struct UniverseParam(pub usize);

/// The head of one non-constant level atom.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(derive(PartialEq, Eq, PartialOrd, Ord, Hash))
)]
pub enum LevelHead {
    Param(UniverseParam),
    Meta(UniverseMetaId),
}

/// A canonical algebraic universe level.
///
/// The value denotes `max(constant, head₁ + offset₁, …)`. A `BTreeMap` provides deterministic atom ordering; construction coalesces duplicate heads at their greatest offset, making equality, hashing, and archival independent of the expression's original association and ordering.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
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
    pub fn substitute(
        &self,
        mut replacement: impl FnMut(LevelHead) -> Option<Level>,
    ) -> Result<Self, UniverseError> {
        let mut parts = vec![Self::constant(self.constant)];
        for (&head, &offset) in &self.atoms {
            let base = replacement(head).unwrap_or_else(|| Self::atom(head, 0));
            parts.push(base.checked_add(offset)?);
        }
        Ok(Self::max(parts))
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

impl fmt::Display for Level {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.atoms.is_empty() {
            return write!(formatter, "{}", self.constant);
        }

        let mut parts = Vec::new();
        if self.constant != 0 {
            parts.push(self.constant.to_string());
        }
        for (head, offset) in &self.atoms {
            let name = match head {
                // `u` through `z`, then numbered cycles. Stepping 26 letters from `u` instead runs off the end of the alphabet at the seventh parameter and prints `{`, `|`, `}` as level names.
                LevelHead::Param(param) => {
                    const LETTERS: usize = (b'z' - b'u' + 1) as usize;
                    let letter = char::from(b'u' + u8::try_from(param.0 % LETTERS).unwrap());
                    match param.0 / LETTERS {
                        0 => letter.to_string(),
                        cycle => format!("{letter}{cycle}"),
                    }
                }
                LevelHead::Meta(meta) => meta.to_string(),
            };
            parts.push(if *offset == 0 {
                name
            } else {
                format!("{name}+{offset}")
            });
        }
        if parts.len() == 1 {
            formatter.write_str(&parts[0])
        } else {
            write!(formatter, "max({})", parts.join(","))
        }
    }
}

/// Whether an unsolved level is an input eligible for generalization or an inferred output/classifier that should be minimized.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum UniverseRole {
    Generalizable,
    Flexible,
}

/// Lowering-time metadata for one densely numbered universe metavariable.
///
/// The role controls finalization, while the origin survives the Text/Core boundary so a later constraint failure can still point at the written `Type` that introduced the level.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct UniverseSeed {
    pub role: UniverseRole,
    pub origin: Option<UniverseConstraintOrigin>,
}

/// The semantic reason a universe inequality was introduced.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum UniverseConstraintKind {
    WrittenType,
    Cumulativity,
    TypeSuccessor,
    FunctionFormation,
    TupleFormation,
    FieldSizing,
    ConstructorSizing,
    Conversion,
    SchemeInstantiation,
    Other(String),
}

/// User-facing provenance for one universe constraint.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct UniverseConstraintOrigin {
    pub span: Option<Span>,
    pub kind: UniverseConstraintKind,
    pub declaration: Option<String>,
    pub binder: Option<String>,
}

impl UniverseConstraintOrigin {
    pub fn new(kind: UniverseConstraintKind) -> Self {
        Self {
            span: None,
            kind,
            declaration: None,
            binder: None,
        }
    }
}

/// One normalized inequality `lower ≤ upper`.
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct UniverseConstraint {
    pub lower: Level,
    pub upper: Level,
    pub origin: UniverseConstraintOrigin,
}

// Provenance explains an inequality but is not part of its semantic identity. In particular, `Span` equality follows source-allocation identity, which must not make replayed or archive-restored schemes compare differently.
impl PartialEq for UniverseConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.lower == other.lower && self.upper == other.upper
    }
}

impl Eq for UniverseConstraint {}

impl Hash for UniverseConstraint {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lower.hash(state);
        self.upper.hash(state);
    }
}

impl UniverseConstraint {
    pub fn is_tautology(&self) -> bool {
        self.lower.structurally_leq(&self.upper)
    }
}

/// A closed, declaration-local residual universe context.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct UniverseContext {
    pub parameter_count: usize,
    pub constraints: Vec<UniverseConstraint>,
}

impl UniverseContext {
    pub fn empty() -> Self {
        Self::default()
    }

    /// This context's own parameters as an argument vector: the one instance that instantiates it to itself.
    ///
    /// A declaration denotes this instance at every occurrence inside its own signature, body, and registry entries, because a group is monomorphic in its own universes. External uses instead take a fresh instance from `curios-elab`'s `UniverseSolver::instantiate`.
    pub fn identity_instance(&self) -> Vec<Level> {
        (0..self.parameter_count)
            .map(UniverseParam)
            .map(Level::param)
            .collect()
    }

    pub(crate) fn map_levels(&self, mut map: impl FnMut(&Level) -> Level) -> Self {
        Self::from_constraints(
            self.parameter_count,
            self.constraints
                .iter()
                .map(|constraint| UniverseConstraint {
                    lower: map(&constraint.lower),
                    upper: map(&constraint.upper),
                    origin: constraint.origin.clone(),
                })
                .collect(),
        )
    }

    pub fn from_constraints(parameter_count: usize, constraints: Vec<UniverseConstraint>) -> Self {
        Self {
            parameter_count,
            constraints,
        }
    }
}

/// One value under a shared universe context.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub(crate) struct UniverseScheme<T> {
    pub(crate) context: UniverseContext,
    pub(crate) value: T,
}

impl<T> UniverseScheme<T> {
    pub(crate) fn monomorphic(value: T) -> Self {
        Self {
            context: UniverseContext::empty(),
            value,
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

impl fmt::Display for UniverseError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UniverseError::OffsetOverflow => formatter.write_str("universe level offset overflow"),
            UniverseError::EscapingLevel => {
                formatter.write_str("a universe metavariable or parameter escaped its scope")
            }
            UniverseError::InstanceArity { expected, got } => {
                write!(
                    formatter,
                    "universe instance has {got} arguments but its scheme expects {expected}"
                )
            }
            UniverseError::UnknownMeta(meta) => write!(formatter, "unknown universe meta {meta}"),
            UniverseError::MismatchedRecursiveContexts => formatter
                .write_str("members of one recursive group carry different universe contexts"),
            UniverseError::SearchExhausted {
                constraints,
                branches,
                widths,
            } => write!(
                formatter,
                "universe consistency search exceeded its budget: \
                 {constraints} constraints, {branches} disjunctive clauses, widths {widths:?}"
            ),
            UniverseError::Inconsistency { lower, upper, path } => {
                write!(
                    formatter,
                    "this Type would need to be strictly below itself ({lower} ≤ {upper})"
                )?;
                for origin in path {
                    write!(formatter, "\n  required by {:?}", origin.kind)?;
                    if let Some(declaration) = &origin.declaration {
                        write!(formatter, " in {declaration}")?;
                    }
                    if let Some(binder) = &origin.binder {
                        write!(formatter, " at binder {binder}")?;
                    }
                    if let Some(span) = &origin.span {
                        write!(formatter, "\n{}", span.render_snippet())?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for UniverseError {}
