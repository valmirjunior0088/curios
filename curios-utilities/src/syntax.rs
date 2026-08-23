//! The shape of the compiler's `/syn` vocabulary — every name a stage emits, with the values supplied by the crate that owns the source declarations.
//!
//! Every enumeration below opens by destructuring the struct it enumerates, which is not a stylistic choice: a pattern naming fewer fields than the struct has does not compile, so a slot added to a group is a compile error until it is enumerated — exactly as it is a compile error at every fill site until it is filled. The lists were written out by hand before, and [`ProofSyntax::bytes_four`] sat in the registry unenumerated, and so unchecked, from the commit that added it.
//!
//! The registry is *shape only*: it names slots, never spellings. `curios-prelude-archive` fills them, because that is the crate holding both the authored `.crs` declarations and the archive that proves each one exists; the two stages that emit `/syn` names — `curios-text`'s lowering and `curios-elab`'s type-directed features — read the filled registry rather than spelling anything themselves. That inversion is why this file lives below both consumers instead of beside the sources: a consumer must see the type, and the prelude sits above every consumer in the crate graph.

use crate::{InfixOp, Qualifier};

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
    pub concept: SyntaxName,
    pub field: &'static str,
}

/// The compiler-known `/syn` names, grouped by the surface feature that emits them.
///
/// The crate that owns the corresponding source declarations fills the fields as an exhaustive named struct literal: a new slot is a compile error at every fill site until it is filled, and the fill names each slot — where a positional constructor once let two like-typed slots swap silently past every check. [`SyntaxRegistry::targets`] and [`SyntaxRegistry::concept_fields`] enumerate the whole obligation, which is what lets the prelude build check every slot against the sources rather than trusting them to agree.
#[derive(Debug, Clone, Copy)]
pub struct SyntaxRegistry {
    pub monad: MonadSyntax,
    pub lift: LiftSyntax,
    pub operator: OperatorSyntax,
    pub character: CharacterSyntax,
    pub string: StringSyntax,
    pub proof: ProofSyntax,
}

impl SyntaxRegistry {
    /// Every registered name, for the prelude build's presence check. The operator concepts appear once per method that dispatches through them, so `/syn/Cmp` recurs — a duplicate costs a redundant assertion and nothing else.
    ///
    /// Each group answers for its own slots instead of having them reached through from here, so that the exhaustive pattern sits in the same scope as the fields it has to keep up with.
    pub fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            monad,
            lift,
            operator,
            character,
            string,
            proof,
        } = self;

        monad
            .targets()
            .chain(lift.targets())
            .chain(operator.targets())
            .chain(character.targets())
            .chain(string.targets())
            .chain(proof.targets())
    }

    /// Every registered concept method, for the prelude build's field check. A concept can exist under the registered name and still not declare the field the compiler projects, which is the drift a presence check alone cannot see.
    ///
    /// Only two groups hold concept methods; the other four are bound and discarded rather than elided with `..`, so a group added with methods of its own cannot quietly miss this check.
    pub fn concept_fields(self) -> impl Iterator<Item = ConceptField> {
        let Self {
            monad: _,
            lift,
            operator,
            character: _,
            string: _,
            proof: _,
        } = self;

        operator.concept_fields().chain(std::iter::once(lift.lift))
    }
}

/// The target postfix `!` sequences with: `/syn/Monad`'s `bind`, projected from the witness the operand's type resolves.
#[derive(Debug, Clone, Copy)]
pub struct MonadSyntax {
    pub bind: SyntaxName,
}

impl MonadSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self { bind } = self;

        [bind].into_iter()
    }
}

/// The embedding concept auto-lift resolves at a postfix `!` whose action's monad differs from its region's: `/syn/Lift`'s `lift` method, projected from the witness keyed by the two monads. Consulted by `elaborate_bang` only — lowering never reads it.
#[derive(Debug, Clone, Copy)]
pub struct LiftSyntax {
    pub lift: ConceptField,
}

impl LiftSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self { lift } = self;

        [lift.concept].into_iter()
    }
}

/// The operator→concept table backing `elaborate_infix`: one slot per method the fixed infix operators dispatch through.
///
/// One slot per *method* rather than per concept, because that is the granularity the elaborator asks at — `Cmp` answers four operators and `Eql` answers two, and a per-concept grouping would have to reintroduce the method as a positional index into a field list. Every operator, `&&`/`||` included, resolves through a witness projection of its concept; there is no carved-out exception, so there is no operator without a slot.
#[derive(Debug, Clone, Copy)]
pub struct OperatorSyntax {
    pub add: ConceptField,
    pub sub: ConceptField,
    pub mul: ConceptField,
    pub div: ConceptField,
    pub rem: ConceptField,
    pub eql: ConceptField,
    pub neq: ConceptField,
    pub lt: ConceptField,
    pub gt: ConceptField,
    pub le: ConceptField,
    pub ge: ConceptField,
    pub and: ConceptField,
    pub or: ConceptField,
}

impl OperatorSyntax {
    /// The concept and method `op` dispatches through. `Neq` has its own slot rather than sharing `Eql`'s: it projects `neq`, so a carrier with a native disequality instruction names it instead of paying for an equality and a negation.
    pub const fn concept_field(self, op: InfixOp) -> ConceptField {
        match op {
            InfixOp::Add => self.add,
            InfixOp::Sub => self.sub,
            InfixOp::Mul => self.mul,
            InfixOp::Div => self.div,
            InfixOp::Rem => self.rem,
            InfixOp::Eql => self.eql,
            InfixOp::Neq => self.neq,
            InfixOp::Lt => self.lt,
            InfixOp::Gt => self.gt,
            InfixOp::Le => self.le,
            InfixOp::Ge => self.ge,
            InfixOp::And => self.and,
            InfixOp::Or => self.or,
        }
    }

    /// The operator dispatching through `concept`'s `field` — [`OperatorSyntax::concept_field`]'s reverse, for folding an elaborated projection back to operator syntax in a report. Exact rather than lossy: `Neq` has its own slot, so `!=` folds back to `!=` instead of to an equality the reader would have to un-negate.
    pub fn operator_for(self, concept: &Qualifier, field: &str) -> Option<InfixOp> {
        InfixOp::ALL.into_iter().find(|op| {
            let target = self.concept_field(*op);
            target.concept.qualifier() == *concept && target.field == field
        })
    }

    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        self.concept_fields().map(|target| target.concept)
    }

    fn concept_fields(self) -> impl Iterator<Item = ConceptField> {
        let Self {
            add,
            sub,
            mul,
            div,
            rem,
            eql,
            neq,
            lt,
            gt,
            le,
            ge,
            and,
            or,
        } = self;

        [add, sub, mul, div, rem, eql, neq, lt, gt, le, ge, and, or].into_iter()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CharacterSyntax {
    pub character: SyntaxName,
    pub scalar_below: SyntaxName,
    pub scalar_above: SyntaxName,
}

impl CharacterSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            character,
            scalar_below,
            scalar_above,
        } = self;

        [character, scalar_below, scalar_above].into_iter()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StringSyntax {
    pub string: SyntaxName,
    pub of_scan_eq: SyntaxName,
    pub refl_scan: SyntaxName,
}

impl StringSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            string,
            of_scan_eq,
            refl_scan,
        } = self;

        [string, of_scan_eq, refl_scan].into_iter()
    }
}

/// The proof vocabulary the compiler writes into terms it builds itself: the inhabitant it supplies for a discharged obligation, and the propositions it states as preconditions on `/sys`'s partial-looking operations.
#[derive(Debug, Clone, Copy)]
pub struct ProofSyntax {
    pub true_qed: SyntaxName,
    /// The trivially true proposition itself. Named beside its constructor because discharging an obligation needs both halves: this one recognises a goal worth discharging, `true_qed` inhabits it.
    pub true_type: SyntaxName,
    /// `a < b`, decided — the bound `/sys`'s checked accessors take as their index precondition. Decided rather than inductive is load-bearing: it reduces to `True` on a refined comparison, which is what lets the obligation be discharged without a written proof.
    pub lt: SyntaxName,
    /// `a <= b`, decided, by the same mechanism — the ordering half of a window bound. Distinct from `/std/Nat/Le/Ind`, which stays inductive so its laws can recurse over the relation's constructors.
    pub le: SyntaxName,
    /// `a != 0` over `Int`, the precondition a division states. `Nat` needs no counterpart: a natural is nonzero exactly when `0 < n`, which [`ProofSyntax::lt`] already states.
    pub int_non_zero: SyntaxName,
    /// `0 <= a` over `Int`, the precondition narrowing to `Nat` states.
    pub int_non_neg: SyntaxName,
    /// `len(b) = 4` over `Bytes`, the precondition reinterpreting one as a binary32 states. Decided on the length alone, which is the whole of what the reinterpretation needs.
    pub bytes_four: SyntaxName,
}

impl ProofSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            true_qed,
            true_type,
            lt,
            le,
            int_non_zero,
            int_non_neg,
            bytes_four,
        } = self;

        [
            true_qed,
            true_type,
            lt,
            le,
            int_non_zero,
            int_non_neg,
            bytes_four,
        ]
        .into_iter()
    }
}
