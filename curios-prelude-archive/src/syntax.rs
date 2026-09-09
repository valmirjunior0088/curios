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
        bind: name(&["std", "Monad", "Monad", "bind"]),
    },
    lift: LiftSyntax {
        lift: field(&["std", "Lift", "Lift"], "lift"),
    },
    operator: OperatorSyntax {
        add: field(&["std", "Add", "Add"], "add"),
        sub: field(&["std", "Subtract", "Subtract"], "sub"),
        mul: field(&["std", "Multiply", "Multiply"], "mul"),
        div: field(&["std", "Divide", "Divide"], "div"),
        rem: field(&["std", "Remainder", "Remainder"], "rem"),
        eql: field(&["std", "Equal", "Equal"], "eql"),
        neq: field(&["std", "Equal", "Equal"], "neq"),
        lt: field(&["std", "Compare", "Compare"], "lt"),
        gt: field(&["std", "Compare", "Compare"], "gt"),
        le: field(&["std", "Compare", "Compare"], "le"),
        ge: field(&["std", "Compare", "Compare"], "ge"),
        and: field(&["std", "And", "And"], "and"),
        or: field(&["std", "Or", "Or"], "or"),
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
        true_qed: name(&["sys", "Bound", "True", "qed"]),
        true_type: name(&["sys", "Bound", "True"]),
        holds: name(&["sys", "Bound", "Holds"]),
        flt_finite: name(&["sys", "Bound", "Finite"]),
        flt_non_neg: name(&["sys", "Bound", "NonNeg"]),
    },
    test: TestSyntax {
        test_type: name(&["syn", "Test", "Test"]),
        main: name(&["syn", "Test", "main"]),
    },
    spell: SpellSyntax {
        spell: field(&["syn", "Spell", "Spell"], "spell"),
        call: name(&["syn", "Spell", "call"]),
        record: name(&["syn", "Spell", "record"]),
    },
};
