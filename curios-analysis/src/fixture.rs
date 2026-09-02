//! Test-only stand-in for the registry `curios-prelude` fills in production.
//!
//! Neither checker can see `curios-prelude` — the dependency runs the other way, which is the whole reason the registry is handed to `Kernel::new` and `Context::new` rather than spelled inside them — so their tests need their own values. This crate holds the one copy because it is the lowest crate both checkers depend on normally, and because its own `tests/driven.rs` is a third consumer. Nothing here is authoritative: no test resolves one of these names, they exist so a checker can be built at all, and production compilation never constructs it.
//!
//! **Behind `test-support`, not `#[cfg(test)]`.** That cfg is set only while *this* crate is its own test harness, so a `cfg(test)` item is invisible to another crate's tests — which is exactly the case here, and the same reason `curios-runtime`'s `test_support` is a feature. The gate also keeps this module out of every normal build, which matters more than convenience: it spells `/syn` names, and `CLAUDE.md`'s rule that no crate below `curios-prelude-archive` may do so holds for everything that ships.

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
        eql: field(&["syn", "Equal", "Equal"], "eql"),
        neq: field(&["syn", "Equal", "Equal"], "neq"),
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
        true_qed: name(&["syn", "True", "True", "qed"]),
        true_type: name(&["syn", "True", "True"]),
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
    },
    spell: SpellSyntax {
        spell: field(&["syn", "Spell", "Spell"], "spell"),
        call: name(&["syn", "Spell", "call"]),
        record: name(&["syn", "Spell", "record"]),
    },
};
