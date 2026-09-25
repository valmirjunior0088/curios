//! Canonical compiler-known names, owned by the `/sys` and `/std` declarations this crate supplies.

use curios_utilities::{
    ChannelSyntax, CharacterSyntax, ConceptField, DerivationSyntax, EqlDerivation, HashDerivation,
    LiftSyntax, MonadSyntax, OperatorSyntax, OptionSyntax, OrdDerivation, ProofSyntax,
    ResultSyntax, SpellDerivation, StringSyntax, SyntaxName, SyntaxRegistry, TestSyntax,
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
    option: OptionSyntax {
        family: name(&["sys", "Option"]),
        some: name(&["sys", "Option", "some"]),
        none: name(&["sys", "Option", "none"]),
    },
    result: ResultSyntax {
        family: name(&["sys", "Result"]),
        success: name(&["sys", "Result", "success"]),
        failure: name(&["sys", "Result", "failure"]),
    },
    channel: ChannelSyntax {
        push: name(&["sys", "Channel", "Push"]),
        taken: name(&["sys", "Channel", "Push", "taken"]),
        full: name(&["sys", "Channel", "Push", "full"]),
        closed: name(&["sys", "Channel", "Push", "closed"]),
        take: name(&["sys", "Channel", "Take"]),
        item: name(&["sys", "Channel", "Take", "item"]),
        empty: name(&["sys", "Channel", "Take", "empty"]),
        ended: name(&["sys", "Channel", "Take", "ended"]),
    },
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
    },
    string: StringSyntax {
        string: name(&["std", "Str", "Str"]),
        qed: name(&["sys", "Bool", "True", "qed"]),
    },
    proof: ProofSyntax {
        true_qed: name(&["sys", "Bool", "True", "qed"]),
        true_type: name(&["sys", "Bool", "True"]),
        holds: name(&["sys", "Bool", "Holds"]),
    },
    test: TestSyntax {
        test_type: name(&["std", "Test", "Test"]),
        main: name(&["std", "Test", "main"]),
    },
    derivations: DerivationSyntax {
        spell: SpellDerivation {
            spell: field(&["std", "Spell", "Spell"], "spell"),
            call: name(&["std", "Spell", "call"]),
            record: name(&["std", "Spell", "record"]),
        },
        // `Eql`'s method is named here as well as in `OperatorSyntax`, deliberately: `==` dispatches through it and this derivation applies it, and the two are free to move apart. Sharing one slot made them agree by coincidence rather than by decision.
        eql: EqlDerivation {
            eql: field(&["std", "ops", "Eql", "Eql"], "eql"),
        },
        ord: OrdDerivation {
            ord: field(&["std", "Ord", "Ord"], "ord"),
            lexicographic: name(&["std", "Ord", "lexicographic"]),
            by_tag: name(&["std", "Ord", "by_tag"]),
            tied: name(&["std", "Ord", "tied"]),
        },
        hash: HashDerivation {
            hash: field(&["std", "Hash", "Hash"], "hash"),
            tagged: name(&["std", "Hash", "tagged"]),
        },
    },
};
