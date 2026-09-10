//! Test-only stand-in for the registry `curios-prelude` fills in production.
//!
//! Neither checker can see `curios-prelude` — the dependency runs the other way, which is the whole reason the registry is handed to `Kernel::new` and `Context::new` rather than spelled inside them — so their tests need their own values. This crate holds the one copy because it is the lowest crate both checkers depend on normally, and because its own `tests/driven.rs` is a third consumer. Nothing here is authoritative: no test resolves one of these names, they exist so a checker can be built at all, and production compilation never constructs it.
//!
//! **Behind `test-support`, not `#[cfg(test)]`.** That cfg is set only while *this* crate is its own test harness, so a `cfg(test)` item is invisible to another crate's tests — which is exactly the case here, and the same reason `curios-runtime`'s `test_support` is a feature. The gate also keeps this module out of every normal build, which matters more than convenience: it spells prelude names, and keeping them out of every build that ships is what the gate is for.

use curios_utilities::{
    CharacterSyntax, ConceptField, LiftSyntax, MonadSyntax, OperatorSyntax, ProofSyntax,
    SpellSyntax, StringSyntax, SyntaxName, SyntaxRegistry, TestSyntax,
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

pub const SYNTAX: SyntaxRegistry = SyntaxRegistry {
    monad: MonadSyntax {
        bind: name(&["std", "Monad", "bind"]),
    },
    lift: LiftSyntax {
        lift: field(&["std", "Lift"], "lift"),
    },
    operator: OperatorSyntax {
        add: field(&["std", "Add"], "add"),
        sub: field(&["std", "Subtract"], "sub"),
        mul: field(&["std", "Multiply"], "mul"),
        div: field(&["std", "Divide"], "div"),
        rem: field(&["std", "Remainder"], "rem"),
        eql: field(&["std", "Equal", "Equal"], "eql"),
        neq: field(&["std", "Equal", "Equal"], "neq"),
        lt: field(&["std", "Compare"], "lt"),
        gt: field(&["std", "Compare"], "gt"),
        le: field(&["std", "Compare"], "le"),
        ge: field(&["std", "Compare"], "ge"),
        and: field(&["std", "And"], "and"),
        or: field(&["std", "Or"], "or"),
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
        true_qed: name(&["std", "True", "True", "qed"]),
        true_type: name(&["std", "True", "True"]),
        holds: name(&["std", "Bool", "Holds"]),
        flt_finite: name(&["std", "Flt", "Finite"]),
        flt_non_neg: name(&["std", "Flt", "NonNeg"]),
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
