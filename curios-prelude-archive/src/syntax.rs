//! Canonical compiler-known names, owned by the `/sys` and `/std` declarations this crate supplies.

use curios_utilities::{
    ChannelSyntax, CharacterSyntax, ConceptField, DerivationSyntax, EntailmentSyntax,
    EqlDerivation, HashDerivation, LiftSyntax, MonadSyntax, NaturalSyntax, OperatorSyntax,
    OptionSyntax, OrdDerivation, OrderSyntax, ProofSyntax, ResultSyntax, SpellDerivation,
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
        lift: field(&["std", "Monad", "Lift"], "lift"),
    },
    operator: OperatorSyntax {
        add: field(&["std", "ops", "Add"], "add"),
        sub: field(&["std", "ops", "Sub"], "sub"),
        mul: field(&["std", "ops", "Mul"], "mul"),
        div: field(&["std", "ops", "Div"], "div"),
        rem: field(&["std", "ops", "Rem"], "rem"),
        eql: field(&["std", "ops", "Eql"], "eql"),
        neq: field(&["std", "ops", "Eql"], "neq"),
        lt: field(&["std", "ops", "Cmp"], "lt"),
        gt: field(&["std", "ops", "Cmp"], "gt"),
        le: field(&["std", "ops", "Cmp"], "le"),
        ge: field(&["std", "ops", "Cmp"], "ge"),
        and: field(&["std", "ops", "And"], "and"),
        or: field(&["std", "ops", "Or"], "or"),
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
        false_type: name(&["sys", "Bool", "False"]),
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
        // `Eql`'s method is named here as well as in `OperatorSyntax`, deliberately: `==` dispatches through it and this derivation applies it, and the two are free to move apart. Sharing one slot would make them agree by coincidence rather than by decision.
        eql: EqlDerivation {
            eql: field(&["std", "ops", "Eql"], "eql"),
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
    entailment: EntailmentSyntax {
        holds_of_eq: name(&["std", "Bool", "holds_of_eq"]),
        refl: name(&["std", "Eq", "Eq", "refl"]),
        equality: name(&["std", "Eq", "Eq"]),
        sym: name(&["std", "Eq", "sym"]),
        range: name(&["std", "Nat", "le", "of_in_range"]),
        in_range: name(&["std", "Nat", "in_range"]),
        nat: OrderSyntax {
            add: name(&["std", "Nat", "le", "add"]),
            scale: name(&["std", "Nat", "le", "mul_mono_r"]),
            mul: name(&["std", "Nat", "le", "mul"]),
            of_eq: name(&["std", "Nat", "le", "of_eq"]),
            eq_of_eql: name(&["std", "Nat", "eq_of_eql"]),
            of_not_lt: name(&["std", "Nat", "le", "of_not_lt"]),
            of_not_le: name(&["std", "Nat", "lt", "of_not_le"]),
        },
        int: OrderSyntax {
            add: name(&["std", "Int", "le", "add"]),
            scale: name(&["std", "Int", "le", "mul_mono_r"]),
            mul: name(&["std", "Int", "le", "mul"]),
            of_eq: name(&["std", "Int", "le", "of_eq"]),
            eq_of_eql: name(&["std", "Int", "eq_of_eql"]),
            of_not_lt: name(&["std", "Int", "le", "of_not_lt"]),
            of_not_le: name(&["std", "Int", "lt", "of_not_le"]),
        },
        natural: NaturalSyntax {
            below: name(&["std", "Nat", "le", "add_r"]),
            above: name(&["std", "Nat", "lt", "add_mul_lt"]),
            shift: name(&["std", "Nat", "le", "add_mono_l"]),
            difference: name(&["std", "Nat", "le", "add_sub_cancel"]),
            truncated: name(&["std", "Nat", "le", "sub_zero"]),
            loosened: name(&["std", "Nat", "le", "of_lt"]),
        },
    },
};
