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
    ConceptField {
        concept: name(segments),
        field: label,
    }
}

pub(crate) const SYNTAX: SyntaxRegistry = SyntaxRegistry {
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
        eql: field(&["syn", "Eql", "Eql"], "eql"),
        neq: field(&["syn", "Eql", "Eql"], "neq"),
        lt: field(&["syn", "Cmp"], "lt"),
        gt: field(&["syn", "Cmp"], "gt"),
        lte: field(&["syn", "Cmp"], "lte"),
        gte: field(&["syn", "Cmp"], "gte"),
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
        true_qed: name(&["syn", "True", "True", "qed"]),
        true_type: name(&["syn", "True", "True"]),
        lt: name(&["syn", "Nat", "Lt"]),
        le: name(&["syn", "Nat", "Le"]),
    },
};
