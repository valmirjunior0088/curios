//! Canonical compiler-known names, owned by the `/sys` and `/std` declarations this crate supplies, and by `/test`, the runner's half of a test program.

use curios_common::{
    CharacterSyntax, ConceptField, ConjunctionSyntax, DerivationSyntax, EntailmentSyntax,
    EqlSyntax, EqualitySyntax, HashDerivation, IntegerSyntax, LiftSyntax, MonadSyntax,
    NaturalSyntax, OperatorSyntax, OptionSyntax, OrdDerivation, OrdLaws, OrderSyntax,
    ProgramSyntax, ProofSyntax, ReportSyntax, SpellDerivation, StringSyntax, SyntaxName,
    SyntaxRegistry, TestSyntax,
};

/// Each target is stated as its module segments, so no stage has to split a path back apart to learn where the name lives.
const fn name(segments: &'static [&'static str]) -> SyntaxName {
    SyntaxName::new(segments)
}

/// One concept method: the concept's segments, and the label of the field the elaborator projects out of its witness.
const fn field(segments: &'static [&'static str], label: &'static str) -> ConceptField {
    ConceptField {
        concept: name(segments),
        field: label,
    }
}

/// The prefix `/test` mounts at: the runner's half of a test program, which declares the scheduler [`SYNTAX`]'s `test.main` names and which a test compilation alone links. The one spelling of it, which the registry's fill, the fold that compiles the unit and a manifest's refusal of a package claiming the prefix each read.
pub const TEST_UNIT: &str = "test";

pub const SYNTAX: SyntaxRegistry = SyntaxRegistry {
    option: OptionSyntax {
        family: name(&["sys", "Option"]),
        some: name(&["sys", "Option", "some"]),
        none: name(&["sys", "Option", "none"]),
    },
    monad: MonadSyntax {
        bind: field(&["std", "Monad", "Monad"], "bind"),
    },
    lift: LiftSyntax {
        lift: field(&["std", "Monad", "Lift"], "lift"),
    },
    operator: OperatorSyntax {
        add: field(&["std", "ops", "Add"], "add"),
        sub: field(&["std", "ops", "Sub"], "sub"),
        mul: field(&["std", "ops", "Mul"], "mul"),
        div: field(&["std", "ops", "Div"], "div"),
        rem: field(&["std", "ops", "Rem"], "rem"),
        lt: field(&["std", "Ord", "Ord"], "lt"),
        le: field(&["std", "Ord", "Ord"], "le"),
        and: field(&["std", "ops", "And"], "and"),
        or: field(&["std", "ops", "Or"], "or"),
    },
    eql: EqlSyntax {
        eql: field(&["std", "ops", "Eql"], "eql"),
        eq: field(&["std", "ops", "Eql"], "eq"),
        refl: field(&["std", "ops", "Eql"], "refl"),
    },
    character: CharacterSyntax {
        character: name(&["std", "Char", "Char"]),
    },
    string: StringSyntax {
        string: name(&["std", "Str", "Str"]),
    },
    proof: ProofSyntax {
        true_qed: name(&["sys", "Bool", "True", "qed"]),
        true_type: name(&["sys", "Bool", "True"]),
        false_type: name(&["sys", "Bool", "False"]),
        holds: name(&["sys", "Bool", "Holds"]),
    },
    equality: EqualitySyntax {
        family: name(&["std", "Eq", "Eq"]),
        refl: name(&["std", "Eq", "Eq", "refl"]),
        sym: name(&["std", "Eq", "sym"]),
    },
    conjunction: ConjunctionSyntax {
        first: name(&["std", "Bool", "and", "first"]),
        second: name(&["std", "Bool", "and", "second"]),
        both: name(&["std", "Bool", "and", "both"]),
    },
    test: TestSyntax {
        test_type: name(&["std", "Test", "Test"]),
        main: name(&[TEST_UNIT, "main"]),
    },
    program: ProgramSyntax {
        execute: field(&["std", "Program", "Program"], "execute"),
    },
    report: ReportSyntax {
        try_type: name(&["std", "Try", "Try"]),
        try_run: name(&["std", "Try", "run"]),
        str_concat: name(&["std", "Str", "concat"]),
        fmt_render: name(&["std", "Fmt", "render"]),
        result_type: name(&["std", "Result", "Result"]),
    },
    derivations: DerivationSyntax {
        spell: SpellDerivation {
            spell: field(&["std", "Spell", "Spell"], "spell"),
            call: name(&["std", "Spell", "call"]),
            record: name(&["std", "Spell", "record"]),
            hidden: name(&["std", "Spell", "hidden"]),
        },
        ord: OrdDerivation {
            ord: field(&["std", "Ord", "Ord"], "ord"),
            lexicographic: name(&["std", "Ord", "lexicographic"]),
            by_tag: name(&["std", "Ord", "by_tag", "by_tag"]),
            eq: name(&["std", "Ord", "Ordering", "Ordering", "eq"]),
            is_lt: name(&["std", "Ord", "Ordering", "is_lt"]),
            is_le: name(&["std", "Ord", "Ordering", "is_le"]),
            laws: OrdLaws {
                flips: field(&["std", "Ord", "Ord"], "flips"),
                eq: field(&["std", "Ord", "Ord"], "eq"),
                chains: name(&["std", "Ord", "chains"]),
                then_flips: name(&["std", "Ord", "Ordering", "then", "flips"]),
                then_first: name(&["std", "Ord", "Ordering", "then", "first"]),
                then_second: name(&["std", "Ord", "Ordering", "then", "second"]),
                chains_then: name(&["std", "Ord", "Ordering", "chains", "then"]),
                chains_le: name(&["std", "Ord", "Ordering", "chains", "le"]),
                by_tag_flips: name(&["std", "Ord", "by_tag", "flips"]),
                by_tag_chains: name(&["std", "Ord", "by_tag", "chains"]),
                by_tag_tied: name(&["std", "Ord", "by_tag", "tied"]),
            },
        },
        hash: HashDerivation {
            hash: field(&["std", "Hash", "Hash"], "hash"),
            position: name(&["std", "Nat", "hash", "hash"]),
            delimited: name(&["std", "Hash", "delimited", "delimited"]),
        },
    },
    entailment: EntailmentSyntax {
        range: name(&["std", "Nat", "le", "of_in_range"]),
        in_range: name(&["std", "Nat", "in_range"]),
        or_first: name(&["std", "Bool", "or", "of_first"]),
        or_second: name(&["std", "Bool", "or", "of_second"]),
        proved: name(&["std", "proved"]),
        nat: OrderSyntax {
            add: name(&["std", "Nat", "le", "add"]),
            scale: name(&["std", "Nat", "le", "mul_mono_r"]),
            mul: name(&["std", "Nat", "le", "mul"]),
            of_eq: name(&["std", "Nat", "le", "of_eq"]),
            of_lt: name(&["std", "Nat", "le", "of_lt"]),
            of_le_ne: name(&["std", "Nat", "lt", "of_le_ne"]),
            min_of_le: name(&["std", "Nat", "min", "of_le"]),
            min_of_ge: name(&["std", "Nat", "min", "of_ge"]),
            max_of_le: name(&["std", "Nat", "max", "of_le"]),
            max_of_ge: name(&["std", "Nat", "max", "of_ge"]),
        },
        int: OrderSyntax {
            add: name(&["std", "Int", "le", "add"]),
            scale: name(&["std", "Int", "le", "mul_mono_r"]),
            mul: name(&["std", "Int", "le", "mul"]),
            of_eq: name(&["std", "Int", "le", "of_eq"]),
            of_lt: name(&["std", "Int", "le", "of_lt"]),
            of_le_ne: name(&["std", "Int", "lt", "of_le_ne"]),
            min_of_le: name(&["std", "Int", "min", "of_le"]),
            min_of_ge: name(&["std", "Int", "min", "of_ge"]),
            max_of_le: name(&["std", "Int", "max", "of_le"]),
            max_of_ge: name(&["std", "Int", "max", "of_ge"]),
        },
        natural: NaturalSyntax {
            below: name(&["std", "Nat", "le", "add_r"]),
            above: name(&["std", "Nat", "lt", "add_mul_lt"]),
            shift: name(&["std", "Nat", "le", "add_mono_l"]),
            difference: name(&["std", "Nat", "le", "add_sub_cancel"]),
            residual: name(&["std", "Nat", "le", "add_sub"]),
        },
        integer: IntegerSyntax {
            abs_of_non_neg: name(&["std", "Int", "abs", "of_non_neg"]),
            abs_of_neg: name(&["std", "Int", "abs", "of_neg"]),
            sign_of_pos: name(&["std", "Int", "sign", "of_pos"]),
            sign_of_not_pos: name(&["std", "Int", "sign", "of_not_pos"]),
            sign_of_neg: name(&["std", "Int", "sign", "of_neg"]),
            sign_of_not_neg: name(&["std", "Int", "sign", "of_not_neg"]),
            abs_at_least: name(&["std", "Int", "abs", "at_least"]),
            abs_at_least_negated: name(&["std", "Int", "abs", "at_least_negated"]),
            sign_at_least: name(&["std", "Int", "sign", "at_least"]),
            sign_at_most: name(&["std", "Int", "sign", "at_most"]),
            below: name(&["std", "Int", "le", "add_r"]),
            above: name(&["std", "Int", "lt", "add_mono_l"]),
            shift: name(&["std", "Int", "le", "add_mono_l"]),
        },
    },
};
