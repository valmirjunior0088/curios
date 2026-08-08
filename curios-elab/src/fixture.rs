//! Test-only stand-in for the registry `curios-prelude` fills in production.
//!
//! This crate cannot see `curios-prelude` — the dependency runs the other way, which is the whole reason the registry is supplied rather than spelled — so its unit tests need their own values, exactly as `curios-text`'s lowering tests do. The spellings match the real prelude's so a test that does reach a type-directed feature reaches the same declarations; nothing here is authoritative, and production compilation never constructs it.

use curios_base::{
    CharacterSyntax, ConceptField, LiftSyntax, MonadSyntax, OperatorSyntax, ProofSyntax,
    StringSyntax, SyntaxName, SyntaxRegistry,
};

const fn name(segments: &'static [&'static str]) -> SyntaxName {
    SyntaxName::new(segments)
}

const fn field(segments: &'static [&'static str], label: &'static str) -> ConceptField {
    ConceptField::new(name(segments), label)
}

pub(crate) const SYNTAX: SyntaxRegistry = SyntaxRegistry::new(
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
