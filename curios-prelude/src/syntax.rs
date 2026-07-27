//! Canonical compiler-known names owned by the authored `/syn` source tree.

use curios_text::{
    CharacterSyntax, MonadSyntax, ProofSyntax, StringSyntax, SyntaxName, SyntaxRegistry,
};

/// Each target is stated as its module segments, so no stage has to split a
/// path back apart to learn where the name lives.
const fn name(segments: &'static [&'static str]) -> SyntaxName {
    SyntaxName::new(segments)
}

pub const SYNTAX: SyntaxRegistry = SyntaxRegistry::new(
    MonadSyntax::new(name(&["syn", "Monad", "bind"])),
    CharacterSyntax::new(
        name(&["syn", "Char", "Char"]),
        name(&["syn", "Char", "Scalar", "below"]),
        name(&["syn", "Char", "Scalar", "above"]),
    ),
    StringSyntax::new(
        name(&["syn", "Str", "Str"]),
        name(&["syn", "Str", "Scan", "lead"]),
        name(&["syn", "Str", "Utf8", "stop"]),
        name(&["syn", "Str", "Utf8", "more"]),
        name(&["syn", "Str", "step"]),
    ),
    ProofSyntax::new(
        name(&["syn", "True", "True", "qed"]),
        name(&["syn", "False", "absurd"]),
    ),
);
