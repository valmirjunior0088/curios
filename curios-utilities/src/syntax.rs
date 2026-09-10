//! The shape of the compiler's emitted vocabulary — every name a stage writes into a term it builds, with the values supplied by the crate that owns the source declarations.
//!
//! Every enumeration below opens by destructuring the struct it enumerates: a pattern naming fewer fields than the struct has does not compile, so a slot added to a group is a compile error until it is enumerated — exactly as it is a compile error at every fill site until it is filled.
//!
//! The registry is *shape only*: it names slots, never spellings. `curios-prelude-archive` fills them, and the two stages that emit those names — `curios-text`'s lowering and `curios-elab`'s type-directed features — read the filled registry rather than spelling anything themselves. Why the shape lives below both consumers, and what the destructuring once caught, are `README.md`'s decisions.

use crate::{InfixOp, Qualifier};

/// One compiler-known name, stated as its module segments.
///
/// Segments rather than a path string, because a consumer needs the *identity*, and building one from `"/std/Monad/bind"` would mean splitting a spelling — the coupling this registry exists to remove. The registry is the site that knows the structure, so the registry states it.
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

/// One concept method the compiler dispatches through: the concept's name, and the label of the field within it.
///
/// The label is deliberately not a [`SyntaxName`]. A concept field is a structure field resolved positionally against its declaration, not a global anything can name, so it travels beside the concept it belongs to instead of pretending to be a name of its own — and it is checked differently: presence in the declaration's field list rather than presence in the module's declared names.
#[derive(Debug, Clone, Copy)]
pub struct ConceptField {
    pub concept: SyntaxName,
    pub field: &'static str,
}

/// The compiler-known names, grouped by the surface feature that emits them.
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
    pub test: TestSyntax,
    pub derivations: DerivationSyntax,
}

impl SyntaxRegistry {
    /// Every registered name, for the prelude build's presence check. The operator concepts appear once per method that dispatches through them, so `/std/Cmp` recurs — a duplicate costs a redundant assertion and nothing else.
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
            test,
            derivations,
        } = self;

        monad
            .targets()
            .chain(lift.targets())
            .chain(operator.targets())
            .chain(character.targets())
            .chain(string.targets())
            .chain(proof.targets())
            .chain(test.targets())
            .chain(derivations.targets())
    }

    /// Every registered concept method, for the prelude build's field check. A concept can exist under the registered name and still not declare the field the compiler projects, which is the drift a presence check alone cannot see.
    ///
    /// Only three groups hold concept methods; the other five are bound and discarded rather than elided with `..`, so a group added with methods of its own cannot quietly miss this check.
    pub fn concept_fields(self) -> impl Iterator<Item = ConceptField> {
        let Self {
            monad: _,
            lift,
            operator,
            character: _,
            string: _,
            proof: _,
            test: _,
            derivations,
        } = self;

        operator
            .concept_fields()
            .chain([lift.lift])
            .chain(derivations.concept_fields())
    }
}

/// The target postfix `!` sequences with: `/std/Monad`'s `bind`, projected from the witness the operand's type resolves.
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

/// The embedding concept auto-lift resolves at a postfix `!` whose action's monad differs from its region's: `/std/Lift`'s `lift` method, projected from the witness keyed by the two monads. Consulted by `elaborate_bang` only — lowering never reads it.
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
    /// The reflection of a decided comparison into a proposition — `Holds(b)`, which reduces to [`ProofSyntax::true_type`] on a refined scrutinee, and that is what lets an obligation be discharged without a written proof.
    ///
    /// **Every bound stated over an intrinsic comparison is built from this one rather than named.** A comparison is a term the table already holds the operands of, so naming five separate propositions — `Lt`, `Le`, `NonZero`, `NonNeg`, `EightBytes` — made the roster reach into a root above it for what it could spell itself. What survives beside this are the two `Flt` bounds, whose decision is a conjunction and so is authored rather than constructed.
    pub holds: SyntaxName,
    /// `a` is a number over `Flt` — finite, so neither infinity nor the NaN — the precondition truncating one to an `Int` states.
    pub flt_finite: SyntaxName,
    /// `0 <= a` and `a` is a number, the precondition truncating a `Flt` to a `Nat` states. An `Int`'s non-negativity needs no upper bound and so is built from [`ProofSyntax::holds`] over a single comparison; this one is not, which is why it is named.
    pub flt_non_neg: SyntaxName,
}

impl ProofSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            true_qed,
            true_type,
            holds,
            flt_finite,
            flt_non_neg,
        } = self;

        [true_qed, true_type, holds, flt_finite, flt_non_neg].into_iter()
    }
}

/// The names the `test` declaration form emits: `/std/Test`, the declared output type of every lowered test, and `/std/Test/main`, the scheduler the synthesized tail applies to the collected tests. Two slots, because the tail decides nothing — a test takes no parameters, so there is no discharge to choose and no description to compare a body against.
#[derive(Debug, Clone, Copy)]
pub struct TestSyntax {
    pub test_type: SyntaxName,
    pub main: SyntaxName,
}

impl TestSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self { test_type, main } = self;

        [test_type, main].into_iter()
    }
}

/// The blessed set: the concepts the compiler writes a witness body for, each row carrying the names the body it writes applies.
///
/// A struct of rows rather than a list of them, for the reason this whole registry is one — a row is a named slot, so a derivation added here is a compile error at every fill site until it is filled, where a list would have let a missing row pass every check. That is the drift the roster exists to end, and a list would have relocated it rather than removed it.
///
/// The tag a body writer dispatches on and the vocabulary `curios-text`'s scheduler needs are the same value, so the two can no longer disagree: before this, the writer read one registry group and the scheduler restated its names in a parallel `if`-chain that nothing checked.
#[derive(Debug, Clone, Copy)]
pub struct DerivationSyntax {
    pub spell: SpellDerivation,
    pub eql: EqlDerivation,
    pub ord: OrdDerivation,
}

impl DerivationSyntax {
    /// Every row, for the concept lookup and the scheduler's edges. Destructures `Self`, so a row added and not yielded here does not compile.
    pub fn rows(self) -> impl Iterator<Item = Derivation> {
        let Self { spell, eql, ord } = self;

        [
            Derivation::Spell(spell),
            Derivation::Eql(eql),
            Derivation::Ord(ord),
        ]
        .into_iter()
    }

    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        self.rows().flat_map(Derivation::targets)
    }

    fn concept_fields(self) -> impl Iterator<Item = ConceptField> {
        self.rows().map(Derivation::concept_field)
    }
}

/// The names a derived `Spell` witness body is written with: the concept's `spell` method, applied to each payload and resolved like any written call; the two renderers the body applies over the spelled pieces — `call` for a constructor over its explicit payloads, `record` for a struct over its labeled fields; and the string machinery every rendered piece is built out of. The re-parse grammar is spelled once, in `/std/Spell`, where the kernel re-certifies it on every prelude build; the derivation only ever emits an application of one of these.
///
/// The string machinery is carried by the row rather than reached out of the registry at the emitter, because it is part of what *this* body writes: `curios_elab::str_literal` names the scan certificate and constructs the carrier at every rendered piece. A row that emits a literal therefore cannot forget to order it, and one that emits none — as `Eql` does, building a `Bool` — does not carry it.
#[derive(Debug, Clone, Copy)]
pub struct SpellDerivation {
    pub spell: ConceptField,
    pub call: SyntaxName,
    pub record: SyntaxName,
    /// The whole group rather than the names out of it, because `str_literal` takes it whole — so the body writer and the scheduler read one value, and a spelling cannot drift between what is emitted and what is ordered.
    pub string: StringSyntax,
}

/// The names a derived `Eql` witness body is written with: its own method, applied to each payload pair, and nothing else. What the body builds beside that is an intrinsic or an infix operator, and the elaborator resolves an operator to a projection off a witness — neither names a global for anything to order against.
#[derive(Debug, Clone, Copy)]
pub struct EqlDerivation {
    pub eql: ConceptField,
}

/// The names a derived `Ord` witness body is written with: its own method, applied to each payload pair, and the two `/std/Ord` combinators the body folds their answers through — `lexicographic` within one constructor, `by_tag` across two, with `tied` filling the arm that cannot be reached because the tags already agreed.
///
/// No string machinery, and deliberately: `lexicographic` takes the payload answers alone, where `Spell`'s renderers take a constructor path. An `Ordering` has no text to re-parse and no report to read, so the path would be a literal nothing consumes — and carrying one would put every `/std/Str` name back into this row's vocabulary for a value that is never rendered.
#[derive(Debug, Clone, Copy)]
pub struct OrdDerivation {
    pub ord: ConceptField,
    pub lexicographic: SyntaxName,
    pub by_tag: SyntaxName,
    pub tied: SyntaxName,
}

/// One row of the roster as the lookup yields it: the tag a body writer dispatches on, carrying the names that writer applies.
#[derive(Debug, Clone, Copy)]
pub enum Derivation {
    Spell(SpellDerivation),
    Eql(EqlDerivation),
    Ord(OrdDerivation),
}

impl Derivation {
    /// The concept method a body-less `satisfy` of this derivation asks for — the key the lookup matches on, and the field the prelude build checks against the declaration.
    pub const fn concept_field(self) -> ConceptField {
        match self {
            Derivation::Spell(row) => row.spell,
            Derivation::Eql(row) => row.eql,
            Derivation::Ord(row) => row.ord,
        }
    }

    /// Every name a body written by this derivation references, as the identity a lowered `Var` carries — the hard edges `curios-text`'s scheduler cannot read off the `Derive` transient, since the body naming them does not exist yet.
    ///
    /// A [`Qualifier`] rather than a [`SyntaxName`]: a concept *method* is its concept's path extended by a field label, which is built rather than spelled, so no `&'static` segment list for it exists or could be made.
    pub fn vocabulary(self) -> Vec<Qualifier> {
        let field = self.concept_field();
        let method = field.concept.qualifier().with(field.field);

        match self {
            Derivation::Spell(SpellDerivation {
                spell: _,
                call,
                record,
                string,
            }) => [method]
                .into_iter()
                .chain(
                    [call, record]
                        .into_iter()
                        .chain(string.targets())
                        .map(SyntaxName::qualifier),
                )
                .collect(),
            Derivation::Eql(EqlDerivation { eql: _ }) => vec![method],
            Derivation::Ord(OrdDerivation {
                ord: _,
                lexicographic,
                by_tag,
                tied,
            }) => [method]
                .into_iter()
                .chain([lexicographic, by_tag, tied].map(SyntaxName::qualifier))
                .collect(),
        }
    }

    /// The names among this row's fields, for the prelude build's presence check.
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        match self {
            Derivation::Spell(SpellDerivation {
                spell,
                call,
                record,
                string,
            }) => [spell.concept, call, record]
                .into_iter()
                .chain(string.targets())
                .collect::<Vec<_>>(),
            Derivation::Eql(EqlDerivation { eql }) => vec![eql.concept],
            Derivation::Ord(OrdDerivation {
                ord,
                lexicographic,
                by_tag,
                tied,
            }) => vec![ord.concept, lexicographic, by_tag, tied],
        }
        .into_iter()
    }
}
