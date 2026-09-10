//! Canonical compiler-known names, owned by the authored `/std` source tree that declares them.

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
        add: field(&["std", "ops", "Add", "Add"], "add"),
        sub: field(&["std", "ops", "Sub", "Sub"], "sub"),
        mul: field(&["std", "ops", "Mul", "Mul"], "mul"),
        div: field(&["std", "ops", "Div", "Div"], "div"),
        rem: field(&["std", "ops", "Rem", "Rem"], "rem"),
        eql: field(&["std", "ops", "Eql", "Eql"], "eql"),
        neq: field(&["std", "ops", "Eql", "Eql"], "neq"),
        lt: field(&["std", "ops", "Cmp", "Cmp"], "lt"),
        gt: field(&["std", "ops", "Cmp", "Cmp"], "gt"),
        le: field(&["std", "ops", "Cmp", "Cmp"], "le"),
        ge: field(&["std", "ops", "Cmp", "Cmp"], "ge"),
        and: field(&["std", "ops", "And", "And"], "and"),
        or: field(&["std", "ops", "Or", "Or"], "or"),
    },
    character: CharacterSyntax {
        character: name(&["std", "Char", "Char"]),
        scalar_below: name(&["std", "Char", "Scalar", "below"]),
        scalar_above: name(&["std", "Char", "Scalar", "above"]),
    },
    string: StringSyntax {
        string: name(&["std", "Str", "Str"]),
        of_scan_eq: name(&["std", "Str", "of_scan_eq"]),
        refl_scan: name(&["std", "Str", "refl_scan"]),
    },
    proof: ProofSyntax {
        true_qed: name(&["sys", "True", "qed"]),
        true_type: name(&["sys", "True"]),
        holds: name(&["sys", "Holds"]),
        flt_finite: name(&["sys", "Flt", "Finite"]),
        flt_non_neg: name(&["sys", "Flt", "NonNeg"]),
    },
    test: TestSyntax {
        test_type: name(&["std", "Test", "Test"]),
        main: name(&["std", "Test", "main"]),
    },
    spell: SpellSyntax {
        spell: field(&["std", "Spell", "Spell"], "spell"),
        call: name(&["std", "Spell", "call"]),
        record: name(&["std", "Spell", "record"]),
    },
};
