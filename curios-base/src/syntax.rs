//! The shape of the compiler's `/syn` vocabulary — every name a stage emits, with the values supplied by the crate that owns the source declarations.
//!
//! The registry is *shape only*: it names slots, never spellings. `curios-prelude` fills them, because that is the crate holding both the authored `.crs` declarations and the archive that proves each one exists; the two stages that emit `/syn` names — `curios-text`'s lowering and `curios-elab`'s type-directed features — read the filled registry rather than spelling anything themselves. That inversion is why this file lives below both consumers instead of beside the sources: a consumer must see the type, and the prelude sits above every consumer in the crate graph.

use crate::{NumOp, Qualifier};

/// One compiler-known `/syn` name, stated as its module segments.
///
/// Segments rather than a path string, because a consumer needs the *identity*, and building one from `"/syn/Monad/bind"` would mean splitting a spelling — the coupling this registry exists to remove. The registry is the site that knows the structure, so the registry states it.
#[derive(Debug, Clone, Copy)]
pub struct SyntaxName {
    segments: &'static [&'static str],
}

impl SyntaxName {
    pub const fn new(segments: &'static [&'static str]) -> Self {
        Self { segments }
    }

    /// The resolved identity this name denotes — what a lowered or synthesized `Var` carries.
    pub fn qualifier(self) -> Qualifier {
        Qualifier::from(self.segments.iter().copied())
    }

    /// The flattened spelling, for the nominal registries `curios-elab` still keys by `String`, and for diagnostics. Rendering, not parsing: it goes out and never back in. Retired with those keys.
    pub fn symbol(self) -> String {
        self.qualifier().join()
    }

    /// The final segment — the declaration's own name.
    pub fn last(self) -> &'static str {
        self.segments.last().copied().unwrap_or_default()
    }
}

/// One concept method the compiler dispatches through: the concept's `/syn` name, and the label of the field within it.
///
/// The label is deliberately not a [`SyntaxName`]. A concept field is a structure field resolved positionally against its declaration, not a global anything can name, so it travels beside the concept it belongs to instead of pretending to be a name of its own — and it is checked differently: presence in the declaration's field list rather than presence in the module's declared names.
#[derive(Debug, Clone, Copy)]
pub struct ConceptField {
    concept: SyntaxName,
    field: &'static str,
}

impl ConceptField {
    pub const fn new(concept: SyntaxName, field: &'static str) -> Self {
        Self { concept, field }
    }

    pub const fn concept(self) -> SyntaxName {
        self.concept
    }

    pub const fn field(self) -> &'static str {
        self.field
    }
}

/// The compiler-known `/syn` names, grouped by the surface feature that emits them.
///
/// Fields are private so this crate owns the shape of the contract, while the crate that owns the corresponding source declarations chooses the canonical value. [`SyntaxRegistry::targets`] and [`SyntaxRegistry::concept_fields`] enumerate the whole obligation, which is what lets the prelude build check every slot against the sources rather than trusting them to agree.
#[derive(Debug, Clone, Copy)]
pub struct SyntaxRegistry {
    monad: MonadSyntax,
    lift: LiftSyntax,
    operator: OperatorSyntax,
    character: CharacterSyntax,
    string: StringSyntax,
    proof: ProofSyntax,
}

impl SyntaxRegistry {
    pub const fn new(
        monad: MonadSyntax,
        lift: LiftSyntax,
        operator: OperatorSyntax,
        character: CharacterSyntax,
        string: StringSyntax,
        proof: ProofSyntax,
    ) -> Self {
        Self {
            monad,
            lift,
            operator,
            character,
            string,
            proof,
        }
    }

    pub const fn monad(self) -> MonadSyntax {
        self.monad
    }

    pub const fn lift(self) -> LiftSyntax {
        self.lift
    }

    pub const fn operator(self) -> OperatorSyntax {
        self.operator
    }

    pub const fn character(self) -> CharacterSyntax {
        self.character
    }

    pub const fn string(self) -> StringSyntax {
        self.string
    }

    pub const fn proof(self) -> ProofSyntax {
        self.proof
    }

    /// Every registered name, for the prelude build's presence check. The operator concepts appear once per method that dispatches through them, so `/syn/Cmp` recurs — a duplicate costs a redundant assertion and nothing else, and enumerating per slot is what keeps a newly added slot impossible to omit here.
    pub fn targets(self) -> impl Iterator<Item = SyntaxName> {
        [
            self.monad.bind,
            self.character.character,
            self.character.scalar_below,
            self.character.scalar_above,
            self.string.string,
            self.string.of_scan_eq,
            self.string.refl_scan,
            self.proof.true_qed,
            self.proof.false_absurd,
        ]
        .into_iter()
        .chain(self.concept_fields().map(ConceptField::concept))
    }

    /// Every registered concept method, for the prelude build's field check. A concept can exist under the registered name and still not declare the field the compiler projects, which is the drift a presence check alone cannot see.
    pub fn concept_fields(self) -> impl Iterator<Item = ConceptField> {
        self.operator
            .concept_fields()
            .chain(std::iter::once(self.lift.lift))
    }
}

/// The target postfix `!` sequences with: `/syn/Monad`'s `bind`, projected from the witness the operand's type resolves.
#[derive(Debug, Clone, Copy)]
pub struct MonadSyntax {
    bind: SyntaxName,
}

impl MonadSyntax {
    pub const fn new(bind: SyntaxName) -> Self {
        Self { bind }
    }

    pub const fn bind(self) -> SyntaxName {
        self.bind
    }
}

/// The embedding concept auto-lift resolves at a postfix `!` whose action's monad differs from its region's: `/syn/Lift`'s `lift` method, projected from the witness keyed by the two monads. Consulted by `elaborate_bang` only — lowering never reads it.
#[derive(Debug, Clone, Copy)]
pub struct LiftSyntax {
    lift: ConceptField,
}

impl LiftSyntax {
    pub const fn new(lift: ConceptField) -> Self {
        Self { lift }
    }

    pub const fn lift(self) -> ConceptField {
        self.lift
    }
}

/// The operator→concept table backing `elaborate_infix`: one slot per method the fixed infix operators dispatch through.
///
/// One slot per *method* rather than per concept, because that is the granularity the elaborator asks at — `Cmp` answers four operators and `Eql` answers two, and a per-concept grouping would have to reintroduce the method as a positional index into a field list. Every operator, `&&`/`||` included, resolves through a witness projection of its concept; there is no carved-out exception, so there is no operator without a slot.
#[derive(Debug, Clone, Copy)]
pub struct OperatorSyntax {
    add: ConceptField,
    sub: ConceptField,
    mul: ConceptField,
    div: ConceptField,
    rem: ConceptField,
    eql: ConceptField,
    neq: ConceptField,
    lt: ConceptField,
    gt: ConceptField,
    lte: ConceptField,
    gte: ConceptField,
    and: ConceptField,
    or: ConceptField,
}

impl OperatorSyntax {
    #[expect(
        clippy::too_many_arguments,
        reason = "one argument per registered slot is the point: a slot cannot be forgotten, and grouping them into sub-structs would only move the same count behind another constructor"
    )]
    pub const fn new(
        add: ConceptField,
        sub: ConceptField,
        mul: ConceptField,
        div: ConceptField,
        rem: ConceptField,
        eql: ConceptField,
        neq: ConceptField,
        lt: ConceptField,
        gt: ConceptField,
        lte: ConceptField,
        gte: ConceptField,
        and: ConceptField,
        or: ConceptField,
    ) -> Self {
        Self {
            add,
            sub,
            mul,
            div,
            rem,
            eql,
            neq,
            lt,
            gt,
            lte,
            gte,
            and,
            or,
        }
    }

    /// The concept and method `op` dispatches through. `Neq` has its own slot rather than sharing `Eql`'s: it projects `neq`, so a carrier with a native disequality instruction names it instead of paying for an equality and a negation.
    pub const fn concept_field(self, op: NumOp) -> ConceptField {
        match op {
            NumOp::Add => self.add,
            NumOp::Sub => self.sub,
            NumOp::Mul => self.mul,
            NumOp::Div => self.div,
            NumOp::Rem => self.rem,
            NumOp::Eql => self.eql,
            NumOp::Neq => self.neq,
            NumOp::Lt => self.lt,
            NumOp::Gt => self.gt,
            NumOp::Lte => self.lte,
            NumOp::Gte => self.gte,
            NumOp::And => self.and,
            NumOp::Or => self.or,
        }
    }

    /// The operator dispatching through `concept`'s `field` — [`OperatorSyntax::concept_field`]'s reverse, for folding an elaborated projection back to operator syntax in a report. Exact rather than lossy: `Neq` has its own slot, so `!=` folds back to `!=` instead of to an equality the reader would have to un-negate.
    pub fn operator_for(self, concept: &Qualifier, field: &str) -> Option<NumOp> {
        NumOp::ALL.into_iter().find(|op| {
            let target = self.concept_field(*op);
            target.concept().qualifier() == *concept && target.field() == field
        })
    }

    fn concept_fields(self) -> impl Iterator<Item = ConceptField> {
        [
            self.add, self.sub, self.mul, self.div, self.rem, self.eql, self.neq, self.lt, self.gt,
            self.lte, self.gte, self.and, self.or,
        ]
        .into_iter()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CharacterSyntax {
    character: SyntaxName,
    scalar_below: SyntaxName,
    scalar_above: SyntaxName,
}

impl CharacterSyntax {
    pub const fn new(
        character: SyntaxName,
        scalar_below: SyntaxName,
        scalar_above: SyntaxName,
    ) -> Self {
        Self {
            character,
            scalar_below,
            scalar_above,
        }
    }

    pub const fn character(self) -> SyntaxName {
        self.character
    }

    pub const fn scalar_below(self) -> SyntaxName {
        self.scalar_below
    }

    pub const fn scalar_above(self) -> SyntaxName {
        self.scalar_above
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StringSyntax {
    string: SyntaxName,
    of_scan_eq: SyntaxName,
    refl_scan: SyntaxName,
}

impl StringSyntax {
    pub const fn new(string: SyntaxName, of_scan_eq: SyntaxName, refl_scan: SyntaxName) -> Self {
        Self {
            string,
            of_scan_eq,
            refl_scan,
        }
    }

    pub const fn of_scan_eq(self) -> SyntaxName {
        self.of_scan_eq
    }

    pub const fn refl_scan(self) -> SyntaxName {
        self.refl_scan
    }

    pub const fn string(self) -> SyntaxName {
        self.string
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ProofSyntax {
    true_qed: SyntaxName,
    false_absurd: SyntaxName,
}

impl ProofSyntax {
    pub const fn new(true_qed: SyntaxName, false_absurd: SyntaxName) -> Self {
        Self {
            true_qed,
            false_absurd,
        }
    }

    pub const fn true_qed(self) -> SyntaxName {
        self.true_qed
    }

    pub const fn false_absurd(self) -> SyntaxName {
        self.false_absurd
    }
}
