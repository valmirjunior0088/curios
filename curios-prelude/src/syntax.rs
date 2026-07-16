//! Canonical compiler-known names owned by the authored `/syn` source tree.

use curios_text::{CharacterSyntax, MonadSyntax, ProofSyntax, StringSyntax, SyntaxRegistry};

pub const SYNTAX: SyntaxRegistry = SyntaxRegistry::new(
    MonadSyntax::new("/syn/Monad/bind"),
    CharacterSyntax::new(
        "/syn/Char/Char",
        "/syn/Char/Scalar/below",
        "/syn/Char/Scalar/above",
    ),
    StringSyntax::new(
        "/syn/Str/Str",
        "/syn/Str/Scan/lead",
        "/syn/Str/Utf8/stop",
        "/syn/Str/Utf8/more",
        "/syn/Str/step",
    ),
    ProofSyntax::new("/syn/True/True/qed", "/syn/False/absurd"),
);
