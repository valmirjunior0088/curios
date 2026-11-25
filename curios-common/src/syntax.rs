//! The shape of the compiler's emitted vocabulary — every name a stage writes into a term it builds, with the values supplied by the crate that owns the source declarations.
//!
//! Every enumeration below opens by destructuring the struct it enumerates: a pattern naming fewer fields than the struct has does not compile, so a slot added to a group is a compile error until it is enumerated — exactly as it is a compile error at every fill site until it is filled.
//!
//! The registry is *shape only*: it names slots, never spellings. `curios-module` fills them (`registry.rs`), and the two stages that emit those names — `curios-module`'s lowering and `curios-elab`'s type-directed features — read the filled registry rather than spelling anything themselves. Why the shape lives below both consumers, and why every enumeration destructures, are `README.md`'s decisions.

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

    /// The flattened spelling, for diagnostics. Rendering, not parsing: it goes out and never back in.
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

/// The compiler-known names, grouped by the surface feature that emits them, and a name more than one feature emits in a group they all read — `eql`, `string`, `proof`, `equality` and `conjunction` — so each name is one slot.
///
/// The crate that owns the corresponding source declarations fills the fields as an exhaustive named struct literal: a new slot is a compile error at every fill site until it is filled, and the fill names each slot, where a positional constructor would let two like-typed slots swap silently past every check. [`SyntaxRegistry::targets`] and [`SyntaxRegistry::concept_fields`] enumerate the whole obligation, which is what lets the prelude build check every slot against the sources rather than trusting them to agree.
#[derive(Debug, Clone, Copy)]
pub struct SyntaxRegistry {
    pub option: OptionSyntax,
    pub result: ResultSyntax,
    pub monad: MonadSyntax,
    pub lift: LiftSyntax,
    pub operator: OperatorSyntax,
    pub eql: EqlSyntax,
    pub character: CharacterSyntax,
    pub string: StringSyntax,
    pub proof: ProofSyntax,
    pub equality: EqualitySyntax,
    pub conjunction: ConjunctionSyntax,
    pub test: TestSyntax,
    pub program: ProgramSyntax,
    pub report: ReportSyntax,
    pub derivations: DerivationSyntax,
    pub entailment: EntailmentSyntax,
}

impl SyntaxRegistry {
    /// Every registered name, for the prelude build's presence check. The operator concepts appear once per method that dispatches through them, so `/std/Ord/Ord` recurs — a duplicate costs a redundant assertion and nothing else.
    ///
    /// Each group answers for its own slots instead of having them reached through from here, so that the exhaustive pattern sits in the same scope as the fields it has to keep up with.
    pub fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let rows = self
            .derivation_rows()
            .flat_map(Derivation::targets)
            .collect::<Vec<_>>();
        let Self {
            option,
            result,
            monad,
            lift,
            operator,
            eql,
            character,
            string,
            proof,
            equality,
            conjunction,
            test,
            program,
            report,
            derivations: _,
            entailment,
        } = self;

        option
            .targets()
            .chain(result.targets())
            .chain(monad.targets())
            .chain(lift.targets())
            .chain(operator.targets())
            .chain(eql.targets())
            .chain(character.targets())
            .chain(string.targets())
            .chain(proof.targets())
            .chain(equality.targets())
            .chain(conjunction.targets())
            .chain(test.targets())
            .chain(program.targets())
            .chain(report.targets())
            .chain(rows)
            .chain(entailment.targets(equality, conjunction))
    }

    /// Every registered concept method, for the prelude build's field check. A concept can exist under the registered name and still not declare the field the compiler projects, which is the drift a presence check alone cannot see.
    ///
    /// Only six groups hold concept methods; the other ten are bound and discarded rather than elided with `..`, so a group added with methods of its own cannot quietly miss this check.
    pub fn concept_fields(self) -> impl Iterator<Item = ConceptField> {
        let rows = self
            .derivation_rows()
            .flat_map(Derivation::concept_fields)
            .collect::<Vec<_>>();
        let Self {
            option: _,
            result: _,
            monad,
            lift,
            operator,
            eql,
            character: _,
            string: _,
            proof: _,
            equality: _,
            conjunction: _,
            test: _,
            program,
            report: _,
            derivations: _,
            entailment: _,
        } = self;

        operator
            .concept_fields()
            .chain(eql.concept_fields())
            .chain([monad.bind])
            .chain([lift.lift])
            .chain([program.execute])
            .chain(rows)
    }

    /// The method `op` dispatches through, and how it reads it: `!=` is the negation of `Eql`'s `eql`, so a witness states equality once and `a != b` cannot disagree with `a == b`; and `a > b` and `a >= b` are `b < a` and `b <= a`, so an order states `lt` and `le` alone.
    pub const fn operator_method(self, op: InfixOp) -> OperatorMethod {
        let operator = self.operator;
        let (method, form) = match op {
            InfixOp::Add => (operator.add, MethodForm::Direct),
            InfixOp::Sub => (operator.sub, MethodForm::Direct),
            InfixOp::Mul => (operator.mul, MethodForm::Direct),
            InfixOp::Div => (operator.div, MethodForm::Direct),
            InfixOp::Rem => (operator.rem, MethodForm::Direct),
            InfixOp::Eql => (self.eql.eql, MethodForm::Direct),
            InfixOp::Neq => (self.eql.eql, MethodForm::Negated),
            InfixOp::Lt => (operator.lt, MethodForm::Direct),
            InfixOp::Gt => (operator.lt, MethodForm::Swapped),
            InfixOp::Le => (operator.le, MethodForm::Direct),
            InfixOp::Ge => (operator.le, MethodForm::Swapped),
            InfixOp::And => (operator.and, MethodForm::Direct),
            InfixOp::Or => (operator.or, MethodForm::Direct),
        };
        OperatorMethod { method, form }
    }

    /// The operator that reads `concept`'s `field` as it stands, for folding an elaborated projection back to operator syntax in a report: [`SyntaxRegistry::operator_method`]'s reverse at [`MethodForm::Direct`], since a form that negates is folded from the negation around the projection.
    pub fn operator_for(self, concept: &Qualifier, field: &str) -> Option<InfixOp> {
        InfixOp::ALL.into_iter().find(|op| {
            let OperatorMethod { method, form } = self.operator_method(*op);
            form == MethodForm::Direct
                && method.concept.qualifier() == *concept
                && method.field == field
        })
    }

    /// Every derivation's row, for the concept lookup and the references a derived witness makes: `Eql`'s is the concept's own group, [`SyntaxRegistry::eql`], and the others are [`DerivationSyntax`]'s. Destructures it, so a row added and not yielded here does not compile. Each row is borrowed, so the tag is a reference whatever its row's size.
    pub fn derivation_rows(&self) -> impl Iterator<Item = Derivation<'_>> {
        let DerivationSyntax { spell, ord, hash } = &self.derivations;

        [
            Derivation::Spell(spell),
            Derivation::Eql(&self.eql),
            Derivation::Ord(ord),
            Derivation::Hash(hash),
        ]
        .into_iter()
    }
}

/// The ordinary optional family and its constructors, in declaration order.
#[derive(Debug, Clone, Copy)]
pub struct OptionSyntax {
    pub family: SyntaxName,
    pub some: SyntaxName,
    pub none: SyntaxName,
}

impl OptionSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self { family, some, none } = self;
        [family, some, none].into_iter()
    }
}

/// The ordinary success-or-failure family and its constructors, in declaration order: what a `/sys` host row's generated outcome is built from.
#[derive(Debug, Clone, Copy)]
pub struct ResultSyntax {
    pub family: SyntaxName,
    pub success: SyntaxName,
    pub failure: SyntaxName,
}

impl ResultSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            family,
            success,
            failure,
        } = self;
        [family, success, failure].into_iter()
    }
}

/// The method postfix `!` sequences with: `/std/Monad`'s `bind`, projected from the witness the region's monad resolves.
#[derive(Debug, Clone, Copy)]
pub struct MonadSyntax {
    pub bind: ConceptField,
}

impl MonadSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self { bind } = self;

        [bind.concept].into_iter()
    }
}

/// The embedding concept auto-lift resolves at a postfix `!` whose action's monad differs from its region's: `/std/Monad/Lift`'s `lift` method, projected from the witness keyed by the two monads. Consulted by `elaborate_bang` only — lowering never reads it.
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

/// The operator→concept table backing `elaborate_infix`: one slot per method the fixed infix operators dispatch through, but `Eql`'s `eql`, which [`SyntaxRegistry::eql`] holds beside the concept's laws; [`SyntaxRegistry::operator_method`] reads both.
///
/// One slot per *method* rather than per concept, because that is the granularity the elaborator asks at — `Ord`'s `lt` and `le` answer four operators, `>` and `>=` reading them swapped, and `Eql` answers two, and a per-concept grouping would have to reintroduce the method as a positional index into a field list. Every operator, `&&`/`||` included, resolves through a witness projection of its concept; there is no carved-out exception, so there is no operator without a slot.
#[derive(Debug, Clone, Copy)]
pub struct OperatorSyntax {
    pub add: ConceptField,
    pub sub: ConceptField,
    pub mul: ConceptField,
    pub div: ConceptField,
    pub rem: ConceptField,
    pub lt: ConceptField,
    pub le: ConceptField,
    pub and: ConceptField,
    pub or: ConceptField,
}

impl OperatorSyntax {
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
            lt,
            le,
            and,
            or,
        } = self;

        [add, sub, mul, div, rem, lt, le, and, or].into_iter()
    }
}

/// `/std/ops/Eql`: the method `==` and `!=` dispatch through, and the two laws that hold it to `Eq`. A group of its own because three features read it: the operators dispatch through `eql`, the `Eql` derivation writes a witness of all three — this group is its row — and the procedure that proves a bound crosses a decided equality to its equation by `eq_of_eql`. The law is no lemma of that procedure's vocabulary ([`EntailmentSyntax::targets`]): a proof names it where its witness resolves.
#[derive(Debug, Clone, Copy)]
pub struct EqlSyntax {
    /// Whether the two are equal.
    pub eql: ConceptField,
    /// The law that two values `eql` holds equal are one value.
    pub eq_of_eql: ConceptField,
    /// The law that a value is `eql` to itself.
    pub refl: ConceptField,
}

impl EqlSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        self.concept_fields().map(|target| target.concept)
    }

    fn concept_fields(self) -> impl Iterator<Item = ConceptField> {
        let Self {
            eql,
            eq_of_eql,
            refl,
        } = self;

        [eql, eq_of_eql, refl].into_iter()
    }
}

/// What an operator calls: the method it projects and how it reads its answer.
#[derive(Debug, Clone, Copy)]
pub struct OperatorMethod {
    pub method: ConceptField,
    pub form: MethodForm,
}

/// How an operator reads the method it dispatches through.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodForm {
    /// The method's answer over the operands as written: `a + b` is `add(a, b)`.
    Direct,
    /// The method's `Bool` answer negated: `a != b` is `eql(a, b)` negated.
    Negated,
    /// The method's answer over the operands swapped, which are elaborated in the order written: `a > b` is `lt(b, a)`.
    Swapped,
}

/// The name a character literal builds: `/std/Char`, whose decided `Valid` field the literal closes with [`ProofSyntax::true_qed`].
#[derive(Debug, Clone, Copy)]
pub struct CharacterSyntax {
    pub character: SyntaxName,
}

impl CharacterSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self { character } = self;

        [character].into_iter()
    }
}

/// The name a string literal builds: `/std/Str`, whose decided `Valid` field the literal closes with [`ProofSyntax::true_qed`].
#[derive(Debug, Clone, Copy)]
pub struct StringSyntax {
    pub string: SyntaxName,
}

impl StringSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self { string } = self;

        [string].into_iter()
    }
}

/// The proof vocabulary the compiler writes into terms it builds itself: the inhabitant it supplies for a discharged obligation, and the propositions it states as preconditions on `/sys`'s partial-looking operations.
#[derive(Debug, Clone, Copy)]
pub struct ProofSyntax {
    pub true_qed: SyntaxName,
    /// The trivially true proposition itself. Named beside its constructor because discharging an obligation needs both halves: this one recognises a goal worth discharging, `true_qed` inhabits it.
    pub true_type: SyntaxName,
    /// The proposition nothing inhabits: what an entry put as a proof is judged at, the claim being that no closed term has it.
    pub false_type: SyntaxName,
    /// The reflection of a decided comparison into a proposition — `Holds(b)`, which reduces to [`ProofSyntax::true_type`] on a refined scrutinee, and that is what lets an obligation be discharged without a written proof.
    ///
    /// **Every bound stated over an intrinsic comparison is built from this one rather than named.** A comparison is a term the table already holds the operands of, so naming five separate propositions — `Lt`, `Le`, `NonZero`, `NonNeg`, `EightBytes` — would make the roster reach into a root above it for what it can spell itself.
    pub holds: SyntaxName,
}

impl ProofSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            true_qed,
            true_type,
            false_type,
            holds,
        } = self;

        [true_qed, true_type, false_type, holds].into_iter()
    }
}

/// Propositional equality, `/std/Eq`: the family, its one constructor, and an equation read the other way round.
///
/// A group of its own because more than one feature writes these names: the procedure that proves a bound reads an equation and turns one round, and a derived witness's laws conclude one and close on its constructor. Each reads this group, so the names are filled once.
#[derive(Debug, Clone, Copy)]
pub struct EqualitySyntax {
    pub family: SyntaxName,
    pub refl: SyntaxName,
    pub sym: SyntaxName,
}

impl EqualitySyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self { family, refl, sym } = self;
        [family, refl, sym].into_iter()
    }
}

/// `/std/Bool/and`'s lemmas: a conjunction's first conjunct and its second from a proof of the whole, and two decisions that hold as their conjunction.
///
/// A group of its own because two features split and join conjunctions by them: the procedure that proves a bound reads a conjunction's guard a conjunct at a time and proves a conjunction conjunct by conjunct, and a derived `Eql` witness's laws take apart and put together the conjunction its comparison is.
#[derive(Debug, Clone, Copy)]
pub struct ConjunctionSyntax {
    pub first: SyntaxName,
    pub second: SyntaxName,
    pub both: SyntaxName,
}

impl ConjunctionSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            first,
            second,
            both,
        } = self;

        [first, second, both].into_iter()
    }
}

/// The names the procedure that proves a bound from the facts in scope writes into its proofs — `curios-elab`'s `entailment`. A proof form is written only where every name it applies is assumed in the context. Inside `/std`, where the vocabulary is elaborated beside its users, `curios-elab`'s walk elaborates the vocabulary first, with every declaration it reads, and offers it to the rest of the unit from then on ([`EntailmentSyntax::targets`]), since any item may omit a bound and no item says so ahead of elaboration.
#[derive(Debug, Clone, Copy)]
pub struct EntailmentSyntax {
    /// A range check's two bounds, as a pair: how a range check's guard is read, where a split on it would meet the guard's own key.
    pub range: SyntaxName,
    /// The range check `range` reads, which a guard's written spelling is opened to: its bounds are proved over the arguments the guard's key spells, the one spelling both checkers record.
    pub in_range: SyntaxName,
    /// A disjunction from its first side, which a bound that is a disjunction is proved by in the case the side holds.
    pub or_first: SyntaxName,
    /// A disjunction from its second side, in the case every side before it fails.
    pub or_second: SyntaxName,
    /// The library's door to the procedure, `/std/proved`: what a written goal's report offers where the bound it stands for holds, the call that asks the procedure again where it is pasted, in place of the proof the procedure wrote.
    pub proved: SyntaxName,
    /// The order vocabulary at `Nat`.
    pub nat: OrderSyntax,
    /// The order vocabulary at `Int`.
    pub int: OrderSyntax,
    /// What `Nat` alone needs: its subtraction truncates, and its quotients and remainders by a literal are read through their bounds.
    pub natural: NaturalSyntax,
    /// What `Int` alone needs: its `abs` and its `sign`, each read through the cases its operand's sign makes.
    pub integer: IntegerSyntax,
}

impl EntailmentSyntax {
    /// Every lemma the procedure writes or reads: its vocabulary, which `Eql`'s law it crosses to an equation by ([`SyntaxRegistry::eql`]) is no part of, a concept's method whose witness resolves where a proof names it. `equality` is [`SyntaxRegistry::equality`]: a hypothesis states an equation in the family, and the procedure reads one the other way round for its second bound. `conjunction` is [`SyntaxRegistry::conjunction`]: a conjunction's guard is read a conjunct at a time, where a split on the first conjunct would meet the guard's own key, and a bound that is a conjunction is proved conjunct by conjunct and joined.
    pub fn targets(
        self,
        equality: EqualitySyntax,
        conjunction: ConjunctionSyntax,
    ) -> impl Iterator<Item = SyntaxName> {
        let Self {
            range,
            in_range,
            or_first,
            or_second,
            proved,
            nat,
            int,
            natural,
            integer,
        } = self;

        [
            equality.family,
            equality.sym,
            range,
            in_range,
            or_first,
            or_second,
            proved,
        ]
        .into_iter()
        .chain(conjunction.targets())
        .chain(nat.targets())
        .chain(int.targets())
        .chain(natural.targets())
        .chain(integer.targets())
    }
}

/// The lemmas only `Nat` needs, each stated over its operands as arguments.
#[derive(Debug, Clone, Copy)]
pub struct NaturalSyntax {
    /// `a <= a + k`: a quotient's multiple is at most its dividend, once conversion reads `d * (x / d) + x % d` as `x`.
    pub below: SyntaxName,
    /// `a < m` and `b < n` give `a * n + b < m * n`: a dividend is below its quotient's successor's multiple, from `x % d < d`.
    pub above: SyntaxName,
    /// `a <= b` gives `n + a <= n + b`: a fact over a remainder raised to one over its dividend.
    pub shift: SyntaxName,
    /// `a <= b` gives `a + (b - a) = b`: the case of a truncated subtraction that does not truncate.
    pub difference: SyntaxName,
    /// `a <= b + (a - b)`: the half of the adjunction both cases of a truncated subtraction state, which conversion does not decide, read beside `a - b <= a` before any case is opened.
    pub residual: SyntaxName,
}

impl NaturalSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            below,
            above,
            shift,
            difference,
            residual,
        } = self;

        [below, above, shift, difference, residual].into_iter()
    }
}

/// The lemmas only `Int` needs, each stated over its operand as an argument.
#[derive(Debug, Clone, Copy)]
pub struct IntegerSyntax {
    /// `+0 <= a` gives `Nat/to_int(abs(a)) = a`: the case of a magnitude its operand is.
    pub abs_of_non_neg: SyntaxName,
    /// `a < +0` gives `Nat/to_int(abs(a)) = +0 - a`: the case it is the operand negated.
    pub abs_of_neg: SyntaxName,
    /// `+0 < a` gives `sign(a) = +1`.
    pub sign_of_pos: SyntaxName,
    /// `a <= +0` gives `sign(a) <= +0`: what is left of a sign where its operand is not above zero.
    pub sign_of_not_pos: SyntaxName,
    /// `a < +0` gives `sign(a) = -1`.
    pub sign_of_neg: SyntaxName,
    /// `+0 <= a` gives `+0 <= sign(a)`: what is left of a sign where its operand is not below zero.
    pub sign_of_not_neg: SyntaxName,
    /// `a <= Nat/to_int(abs(a))`: what both cases of a magnitude state of its operand.
    pub abs_at_least: SyntaxName,
    /// `+0 - a <= Nat/to_int(abs(a))`: what both cases of a magnitude state of its operand's negation.
    pub abs_at_least_negated: SyntaxName,
    /// `-1 <= sign(a)`: what every case of a sign states from below.
    pub sign_at_least: SyntaxName,
    /// `sign(a) <= +1`: what every case of a sign states from above.
    pub sign_at_most: SyntaxName,
    /// `a <= a + Nat/to_int(k)`: a quotient's multiple is at most its dividend, once conversion reads `d * (i / d) + i % d` as `i`.
    pub below: SyntaxName,
    /// `a < b` gives `n + a < n + b`: a dividend is below its quotient's multiple raised by its divisor's magnitude, from `i % d < |d|`.
    pub above: SyntaxName,
    /// `a <= b` gives `n + a <= n + b`: a fact over a remainder raised to one over its dividend.
    pub shift: SyntaxName,
}

impl IntegerSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            abs_of_non_neg,
            abs_of_neg,
            sign_of_pos,
            sign_of_not_pos,
            sign_of_neg,
            sign_of_not_neg,
            abs_at_least,
            abs_at_least_negated,
            sign_at_least,
            sign_at_most,
            below,
            above,
            shift,
        } = self;

        [
            abs_of_non_neg,
            abs_of_neg,
            sign_of_pos,
            sign_of_not_pos,
            sign_of_neg,
            sign_of_not_neg,
            abs_at_least,
            abs_at_least_negated,
            sign_at_least,
            sign_at_most,
            below,
            above,
            shift,
        ]
        .into_iter()
    }
}

/// One carrier's order lemmas, as the procedure applies them: each takes its operands as arguments, and the procedure passes them all, so nothing is left to inference.
#[derive(Debug, Clone, Copy)]
pub struct OrderSyntax {
    /// `a <= b` and `c <= d` give `a + c <= b + d`: how a certificate's facts are summed.
    pub add: SyntaxName,
    /// A bound scaled by a literal, `a * k <= b * k`: how a fact enters a certificate with a multiplier.
    pub scale: SyntaxName,
    /// Two bounds multiplied, `a * d + b * c <= a * c + b * d` from `a <= b` and `c <= d`: a product of two facts, for a multiplier that is no literal.
    pub mul: SyntaxName,
    /// An equation's bound: `a = b` gives `a <= b`.
    pub of_eq: SyntaxName,
    /// A decided equality as an equation: an `==` guard's true arm.
    pub eq_of_eql: SyntaxName,
    /// `a < b` gives `a <= b`: a failed `<=` guard's bound loosened to what a lemma over `<=` takes.
    pub of_lt: SyntaxName,
    /// `a <= b` and `a == b` failing give `a < b`: the case of a disequality its loose bound holds in.
    pub of_le_ne: SyntaxName,
    /// `a <= b` gives `min(a, b) = a`: the case of a `min` its first operand answers for.
    pub min_of_le: SyntaxName,
    /// `b <= a` gives `min(a, b) = b`: the case its second does.
    pub min_of_ge: SyntaxName,
    /// `a <= b` gives `max(a, b) = b`.
    pub max_of_le: SyntaxName,
    /// `b <= a` gives `max(a, b) = a`.
    pub max_of_ge: SyntaxName,
}

impl OrderSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            add,
            scale,
            mul,
            of_eq,
            eq_of_eql,
            of_lt,
            of_le_ne,
            min_of_le,
            min_of_ge,
            max_of_le,
            max_of_ge,
        } = self;

        [
            add, scale, mul, of_eq, eq_of_eql, of_lt, of_le_ne, min_of_le, min_of_ge, max_of_le,
            max_of_ge,
        ]
        .into_iter()
    }
}

/// What closes a program is executed through: `/std/Program`'s `execute` method, projected from the witness the closing term's type resolves. Written by `curios-pipeline` around each tail of an authored entry's final term, and read by the elaborator to say what a missing witness of it was needed for.
#[derive(Debug, Clone, Copy)]
pub struct ProgramSyntax {
    pub execute: ConceptField,
}

impl ProgramSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self { execute } = self;

        [execute.concept].into_iter()
    }
}

/// What a report names for its reader to write in place of a form that is refused: `/std/Try` and its `run`, where an action that may fail meets a region it has no edge into, and `/std/Str/concat` and `/std/Fmt/render`, where `+` meets a `Str`. Read by the elaborator's reports alone: no stage builds a term the program holds from them.
#[derive(Debug, Clone, Copy)]
pub struct ReportSyntax {
    pub try_type: SyntaxName,
    pub try_run: SyntaxName,
    pub str_concat: SyntaxName,
    pub fmt_render: SyntaxName,
}

impl ReportSyntax {
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        let Self {
            try_type,
            try_run,
            str_concat,
            fmt_render,
        } = self;

        [try_type, try_run, str_concat, fmt_render].into_iter()
    }
}

/// The names the `test` declaration form emits: `/std/Test`, the declared output type of every lowered test, and `/test/main`, the scheduler the synthesized tail applies to the collected tests, which a test compilation alone links. Two slots, because the tail decides nothing — a test takes no parameters, so there is no discharge to choose and no description to compare a body against.
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

/// The blessed set: the concepts the compiler writes a witness body for, each row carrying the names the body it writes applies — but `Eql`'s, whose row is the concept's own group, [`SyntaxRegistry::eql`], every name its body writes beside the concept's three being a shared group's. [`SyntaxRegistry::derivation_rows`] yields them all.
///
/// A struct of rows rather than a list of them, for the reason this whole registry is one — a row is a named slot, so a derivation added here is a compile error at every fill site until it is filled, where a list would have let a missing row pass every check. That is the drift the roster exists to end, and a list would have relocated it rather than removed it.
///
/// The tag a body writer dispatches on and the vocabulary `curios-module`'s unused-declaration lint reads are the same value, so the two cannot disagree.
#[derive(Debug, Clone, Copy)]
pub struct DerivationSyntax {
    pub spell: SpellDerivation,
    pub ord: OrdDerivation,
    pub hash: HashDerivation,
}

/// The names specific to a derived `Spell` witness body: the concept's `spell` method, applied to each payload and resolved like any written call; and the renderers the body applies over the spelled pieces — `call` for a constructor over its payloads, `record` for a struct over its fields, and `hidden` for a member written under its mark. The re-parse grammar is spelled once, in `/std/Spell`, where the certifier re-certifies it on every prelude build; the derivation only ever emits an application of one of these.
///
/// The body writer and [`Derivation::vocabulary`] both receive [`SyntaxRegistry::string`] and [`ProofSyntax::true_qed`]: `curios_core::str_literal` constructs the carrier and closes its scan certificate at every rendered piece, so a derived witness references those shared names too. They have one registry entry each, shared with ordinary string literals.
#[derive(Debug, Clone, Copy)]
pub struct SpellDerivation {
    pub spell: ConceptField,
    pub call: SyntaxName,
    pub record: SyntaxName,
    /// What a hidden member spells as, under its mark.
    pub hidden: SyntaxName,
}

/// The names a derived `Ord` witness body is written with: its own method, applied to each payload pair; the names its laws' proofs apply ([`OrdLaws`]); and the two `/std/Ord` combinators the body folds their answers through — `lexicographic` within one constructor, `by_tag` across two, with `Ordering/eq` filling the arm that cannot be reached because the tags already agreed — and `Ordering/is_lt` and `is_le`, which read `lt` and `le` off the same comparison.
///
/// No string machinery, and deliberately: `lexicographic` takes the payload answers alone, where `Spell`'s renderers take a constructor path. An `Ordering` has no text to re-parse and no report to read, so the path would be a literal nothing consumes — and carrying one would put every `/std/Str` name back into this row's vocabulary for a value that is never rendered.
#[derive(Debug, Clone, Copy)]
pub struct OrdDerivation {
    pub ord: ConceptField,
    pub lexicographic: SyntaxName,
    pub by_tag: SyntaxName,
    pub eq: SyntaxName,
    pub is_lt: SyntaxName,
    pub is_le: SyntaxName,
    pub laws: OrdLaws,
}

/// The names the proofs of a derived `Ord`'s laws apply, beside what the comparison itself is written with.
///
/// Two laws of the concept, each applied to a payload as the method is; the law every order's answers obey at three values, which is each payload's part of `transitive`; the laws of the step `lexicographic` folds by, which carry a proof from one payload to the next; and `by_tag`'s, which ask for the payloads' part where two constructors are one. The proofs also conclude equations and close on the inhabitant of `True`, which [`SyntaxRegistry::equality`] and [`SyntaxRegistry::proof`] name.
#[derive(Debug, Clone, Copy)]
pub struct OrdLaws {
    /// The law that `ord` answers the flipped answer with its operands swapped.
    pub flips: ConceptField,
    /// The law that two values `ord` answers `eq` at are one value.
    pub eq_of_ord: ConceptField,
    /// That an order's answers at three values chain, from its laws.
    pub chains: SyntaxName,
    /// A step flips to the step of the flipped answers.
    pub then_flips: SyntaxName,
    /// The first of two answers that step to `eq` is `eq`.
    pub then_first: SyntaxName,
    /// And so is the second.
    pub then_second: SyntaxName,
    /// Three steps chain where their answers do.
    pub chains_then: SyntaxName,
    /// The third of three answers that chain is `lt` or `eq` where the first two are, which closes `transitive`.
    pub chains_le: SyntaxName,
    /// `by_tag` flips where the answers it is handed do at one constructor.
    pub by_tag_flips: SyntaxName,
    /// `by_tag`'s answers chain where the ones it is handed do at one constructor.
    pub by_tag_chains: SyntaxName,
    /// Two positions `by_tag` answers `eq` at are one: the fact that closes two different constructors.
    pub by_tag_tied: SyntaxName,
}

impl OrdLaws {
    /// The lemmas among these names: every one but the concept's two fields.
    fn lemmas(self) -> [SyntaxName; 9] {
        let Self {
            flips: _,
            eq_of_ord: _,
            chains,
            then_flips,
            then_first,
            then_second,
            chains_then,
            chains_le,
            by_tag_flips,
            by_tag_chains,
            by_tag_tied,
        } = self;

        [
            chains,
            then_flips,
            then_first,
            then_second,
            chains_then,
            chains_le,
            by_tag_flips,
            by_tag_chains,
            by_tag_tied,
        ]
    }
}

/// The names a derived `Hash` witness body is written with: its own method, applied to each payload, and the one `/std/Hash` combinator the body folds their encodings through — `tagged`, which writes the constructor's ordinal and frames every part.
///
/// One combinator where `Ord` needs three, because framing is the whole of the structure: a tag and a list of already-encoded parts determine the answer, so there is no across-constructors case to spell separately and no tie to fill. No string machinery, for `Ord`'s reason — an encoding is `Bytes`, which the body builds as an intrinsic literal and needs no name for.
#[derive(Debug, Clone, Copy)]
pub struct HashDerivation {
    pub hash: ConceptField,
    pub tagged: SyntaxName,
}

/// One row of the roster as the lookup yields it: the tag a body writer dispatches on, carrying the names that writer applies.
#[derive(Debug, Clone, Copy)]
pub enum Derivation<'r> {
    Spell(&'r SpellDerivation),
    Eql(&'r EqlSyntax),
    Ord(&'r OrdDerivation),
    Hash(&'r HashDerivation),
}

impl Derivation<'_> {
    /// The concept method a derived `satisfy` of this derivation asks for — the key the lookup matches on, and the field the prelude build checks against the declaration.
    pub const fn concept_field(self) -> ConceptField {
        match self {
            Derivation::Spell(row) => row.spell,
            Derivation::Eql(row) => row.eql,
            Derivation::Ord(row) => row.ord,
            Derivation::Hash(row) => row.hash,
        }
    }

    /// Every concept method a body written by this derivation projects, for the prelude build's field check: the method the lookup keys on, and for `Eql` the two laws its body proves through each payload's own.
    fn concept_fields(self) -> Vec<ConceptField> {
        match self {
            Derivation::Eql(row) => row.concept_fields().collect(),
            Derivation::Ord(row) => vec![row.ord, row.laws.flips, row.laws.eq_of_ord],
            Derivation::Spell(_) | Derivation::Hash(_) => vec![self.concept_field()],
        }
    }

    /// Every name a body written by this derivation references, as the identity a lowered `Var` carries — the references `curios-module`'s unused-declaration lint cannot read off the `Derive` transient, since the body naming them does not exist yet.
    ///
    /// A [`Qualifier`] rather than a [`SyntaxName`], as the bodies' references are read.
    ///
    /// `registry` is the registry this row is of, for the groups a body shares with other features and the body writer reads there too: [`SyntaxRegistry::string`], for the string literals only `Spell` emits; [`SyntaxRegistry::equality`], for the equations `Eql`'s laws and `Ord`'s conclude; [`SyntaxRegistry::conjunction`], for the conjunction `Eql`'s laws take apart and join; and [`SyntaxRegistry::proof`], for the inhabitant of `True` a string literal's certificate is and an `Eql` or `Ord` law closes on where a constructor holds nothing.
    pub fn vocabulary(self, registry: &SyntaxRegistry) -> Vec<Qualifier> {
        let (string, equality, conjunction) =
            (registry.string, registry.equality, registry.conjunction);
        let qed = registry.proof.true_qed;
        // A method named by its path reads its concept: a method is its concept's field, and no declaration.
        let concept = self.concept_field().concept.qualifier();

        match self {
            Derivation::Spell(&SpellDerivation {
                spell: _,
                call,
                record,
                hidden,
            }) => [concept]
                .into_iter()
                .chain(
                    [call, record, hidden, qed]
                        .into_iter()
                        .chain(string.targets())
                        .map(SyntaxName::qualifier),
                )
                .collect(),
            Derivation::Eql(_) => [concept]
                .into_iter()
                .chain(
                    conjunction
                        .targets()
                        .chain([equality.family, equality.refl, qed])
                        .map(SyntaxName::qualifier),
                )
                .collect(),
            Derivation::Ord(&OrdDerivation {
                ord: _,
                lexicographic,
                by_tag,
                eq,
                is_lt,
                is_le,
                laws,
            }) => [concept]
                .into_iter()
                .chain(
                    [
                        lexicographic,
                        by_tag,
                        eq,
                        is_lt,
                        is_le,
                        equality.family,
                        equality.refl,
                        qed,
                    ]
                    .into_iter()
                    .chain(laws.lemmas())
                    .map(SyntaxName::qualifier),
                )
                .collect(),
            Derivation::Hash(&HashDerivation { hash: _, tagged }) => {
                vec![concept, tagged.qualifier()]
            }
        }
    }

    /// The names among this row's fields, for the prelude build's presence check.
    fn targets(self) -> impl Iterator<Item = SyntaxName> {
        match self {
            Derivation::Spell(&SpellDerivation {
                spell,
                call,
                record,
                hidden,
            }) => vec![spell.concept, call, record, hidden],
            Derivation::Eql(row) => row.targets().collect(),
            Derivation::Ord(&OrdDerivation {
                ord,
                lexicographic,
                by_tag,
                eq,
                is_lt,
                is_le,
                laws,
            }) => [ord.concept, lexicographic, by_tag, eq, is_lt, is_le]
                .into_iter()
                .chain(laws.lemmas())
                .collect(),
            Derivation::Hash(&HashDerivation { hash, tagged }) => vec![hash.concept, tagged],
        }
        .into_iter()
    }
}
