//! Canonical compiler-known names owned by the authored `/syn` source tree.

use curios_utilities::{
    CharacterSyntax, ConceptField, LiftSyntax, MonadSyntax, OperatorSyntax, ProofSyntax,
    StringSyntax, SyntaxName, SyntaxRegistry, TestSyntax,
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
        sub: field(&["syn", "Sub"], "sub"),
        mul: field(&["syn", "Mul"], "mul"),
        div: field(&["syn", "Div"], "div"),
        rem: field(&["syn", "Rem"], "rem"),
        eql: field(&["syn", "Eql"], "eql"),
        neq: field(&["syn", "Eql"], "neq"),
        lt: field(&["syn", "Cmp"], "lt"),
        gt: field(&["syn", "Cmp"], "gt"),
        le: field(&["syn", "Cmp"], "le"),
        ge: field(&["syn", "Cmp"], "ge"),
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
    },
};
