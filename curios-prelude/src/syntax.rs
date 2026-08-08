//! Canonical compiler-known names owned by the authored `/syn` source tree.

use curios_base::{
    CharacterSyntax, ConceptField, LiftSyntax, MonadSyntax, OperatorSyntax, ProofSyntax,
    StringSyntax, SyntaxName, SyntaxRegistry,
};

/// Each target is stated as its module segments, so no stage has to split a path back apart to learn where the name lives.
const fn name(segments: &'static [&'static str]) -> SyntaxName {
    SyntaxName::new(segments)
}

/// One concept method: the concept's segments, and the label of the field the elaborator projects out of its witness.
const fn field(segments: &'static [&'static str], label: &'static str) -> ConceptField {
    ConceptField::new(name(segments), label)
}

pub const SYNTAX: SyntaxRegistry = SyntaxRegistry::new(
    MonadSyntax::new(name(&["syn", "Monad", "bind"])),
    LiftSyntax::new(field(&["syn", "Lift"], "lift")),
    OperatorSyntax::new(
        field(&["syn", "Add"], "add"),
        field(&["syn", "Sub"], "sub"),
        field(&["syn", "Mul"], "mul"),
        field(&["syn", "Div"], "div"),
        field(&["syn", "Rem"], "rem"),
        field(&["syn", "Eql", "Eql"], "eql"),
        field(&["syn", "Eql", "Eql"], "neq"),
        field(&["syn", "Cmp"], "lt"),
        field(&["syn", "Cmp"], "gt"),
        field(&["syn", "Cmp"], "lte"),
        field(&["syn", "Cmp"], "gte"),
        field(&["syn", "And"], "and"),
        field(&["syn", "Or"], "or"),
    ),
    CharacterSyntax::new(
        name(&["syn", "Char", "Char"]),
        name(&["syn", "Char", "Scalar", "below"]),
        name(&["syn", "Char", "Scalar", "above"]),
    ),
    StringSyntax::new(
        name(&["syn", "Str", "Str"]),
        name(&["syn", "Str", "of_scan_eq"]),
        name(&["syn", "Str", "refl_scan"]),
    ),
    ProofSyntax::new(
        name(&["syn", "True", "True", "qed"]),
        name(&["syn", "False", "absurd"]),
    ),
);
