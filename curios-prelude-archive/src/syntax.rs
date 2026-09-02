//! Canonical compiler-known names owned by the authored `/syn` source tree.

use curios_utilities::{
    CharacterSyntax, ConceptField, LiftSyntax, MonadSyntax, OperatorSyntax, ProofSyntax,
    SpellSyntax, StringSyntax, SyntaxName, SyntaxRegistry, TestSyntax,
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

pub const SYNTAX: SyntaxRegistry = SyntaxRegistry {
    monad: MonadSyntax {
        bind: name(&["syn", "Monad", "bind"]),
    },
    lift: LiftSyntax {
        lift: field(&["syn", "Lift"], "lift"),
    },
    operator: OperatorSyntax {
        add: field(&["syn", "Add"], "add"),
        sub: field(&["syn", "Subtract"], "sub"),
        mul: field(&["syn", "Multiply"], "mul"),
        div: field(&["syn", "Divide"], "div"),
        rem: field(&["syn", "Remainder"], "rem"),
        eql: field(&["syn", "Equal"], "eql"),
        neq: field(&["syn", "Equal"], "neq"),
        lt: field(&["syn", "Compare"], "lt"),
        gt: field(&["syn", "Compare"], "gt"),
        le: field(&["syn", "Compare"], "le"),
        ge: field(&["syn", "Compare"], "ge"),
        and: field(&["syn", "And"], "and"),
        or: field(&["syn", "Or"], "or"),
    },
    character: CharacterSyntax {
        character: name(&["syn", "Char", "Char"]),
        scalar_below: name(&["syn", "Char", "Scalar", "below"]),
        scalar_above: name(&["syn", "Char", "Scalar", "above"]),
    },
    string: StringSyntax {
        string: name(&["syn", "Str", "Str"]),
        of_scan_eq: name(&["syn", "Str", "of_scan_eq"]),
        refl_scan: name(&["syn", "Str", "refl_scan"]),
    },
    proof: ProofSyntax {
        true_qed: name(&["syn", "True", "qed"]),
        true_type: name(&["syn", "True"]),
        lt: name(&["syn", "Nat", "Lt"]),
        le: name(&["syn", "Nat", "Le"]),
        int_non_zero: name(&["syn", "Int", "NonZero"]),
        int_non_neg: name(&["syn", "Int", "NonNeg"]),
        bytes_four: name(&["syn", "Flt", "FourBytes"]),
        flt_finite: name(&["syn", "Flt", "Finite"]),
        flt_non_neg: name(&["syn", "Flt", "NonNeg"]),
    },
    test: TestSyntax {
        test_type: name(&["syn", "Test", "Test"]),
        main: name(&["syn", "Test", "main"]),
        property: name(&["syn", "Test", "property"]),
        settled: name(&["syn", "Test", "settled"]),
        theorem: name(&["syn", "Test", "Test", "theorem"]),
    },
    spell: SpellSyntax {
        spell: field(&["syn", "Spell", "Spell"], "spell"),
        call: name(&["syn", "Spell", "call"]),
        record: name(&["syn", "Spell", "record"]),
    },
};
