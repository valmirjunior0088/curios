//! Canonical compiler-known names, owned by the `/sys` and `/std` declarations this crate supplies, and by `/test`, the runner's half of a test program.

use curios_common::{
    CharacterSyntax, ConceptField, DerivationSyntax, EntailmentSyntax, EqlDerivation,
    EqualitySyntax, HashDerivation, IntegerSyntax, LiftSyntax, MonadSyntax, NaturalSyntax,
    OperatorSyntax, OptionSyntax, OrdDerivation, OrderSyntax, ProgramSyntax, ProofSyntax,
    ReportSyntax, ResultSyntax, SpellDerivation, StringSyntax, SyntaxName, SyntaxRegistry,
    TestSyntax,
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
    result: ResultSyntax {
        family: name(&["sys", "Result"]),
        success: name(&["sys", "Result", "success"]),
        failure: name(&["sys", "Result", "failure"]),
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
        eql: field(&["std", "ops", "Eql"], "eql"),
        lt: field(&["std", "Ord", "Ord"], "lt"),
        le: field(&["std", "Ord", "Ord"], "le"),
        and: field(&["std", "ops", "And"], "and"),
        or: field(&["std", "ops", "Or"], "or"),
    },
    character: CharacterSyntax {
        character: name(&["std", "Char", "Char"]),
    },
    string: StringSyntax {
        string: name(&["std", "Str", "Str"]),
        qed: name(&["sys", "Bool", "True", "qed"]),
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
    },
    derivations: DerivationSyntax {
        spell: SpellDerivation {
            spell: field(&["std", "Spell", "Spell"], "spell"),
            call: name(&["std", "Spell", "call"]),
            record: name(&["std", "Spell", "record"]),
            hidden: name(&["std", "Spell", "hidden"]),
        },
        // `Eql`'s method is named here as well as in `OperatorSyntax`, deliberately: `==` dispatches through it and this derivation applies it, and the two are free to move apart. Sharing one slot would make them agree by coincidence rather than by decision.
        eql: EqlDerivation {
            eql: field(&["std", "ops", "Eql"], "eql"),
            eq_of_eql: field(&["std", "ops", "Eql"], "eq_of_eql"),
            refl: field(&["std", "ops", "Eql"], "refl"),
            first: name(&["std", "Bool", "and", "first"]),
            second: name(&["std", "Bool", "and", "second"]),
            both: name(&["std", "Bool", "and", "both"]),
            qed: name(&["sys", "Bool", "True", "qed"]),
        },
        ord: OrdDerivation {
            ord: field(&["std", "Ord", "Ord"], "ord"),
            lexicographic: name(&["std", "Ord", "lexicographic"]),
            by_tag: name(&["std", "Ord", "by_tag", "by_tag"]),
            eq: name(&["std", "Ord", "Ordering", "Ordering", "eq"]),
            is_lt: name(&["std", "Ord", "Ordering", "is_lt"]),
            is_le: name(&["std", "Ord", "Ordering", "is_le"]),
        },
        hash: HashDerivation {
            hash: field(&["std", "Hash", "Hash"], "hash"),
            tagged: name(&["std", "Hash", "tagged"]),
        },
    },
    entailment: EntailmentSyntax {
        decided: field(&["std", "ops", "Eql"], "eq_of_eql"),
        range: name(&["std", "Nat", "le", "of_in_range"]),
        in_range: name(&["std", "Nat", "in_range"]),
        and_first: name(&["std", "Bool", "and", "first"]),
        and_second: name(&["std", "Bool", "and", "second"]),
        and_both: name(&["std", "Bool", "and", "both"]),
        or_first: name(&["std", "Bool", "or", "of_first"]),
        or_second: name(&["std", "Bool", "or", "of_second"]),
        proved: name(&["std", "proved"]),
        nat: OrderSyntax {
            add: name(&["std", "Nat", "le", "add"]),
            scale: name(&["std", "Nat", "le", "mul_mono_r"]),
            mul: name(&["std", "Nat", "le", "mul"]),
            of_eq: name(&["std", "Nat", "le", "of_eq"]),
            eq_of_eql: name(&["std", "Nat", "eq_of_eql"]),
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
            eq_of_eql: name(&["std", "Int", "eq_of_eql"]),
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
