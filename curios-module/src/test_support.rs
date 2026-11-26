//! The fixtures another crate's suites lower a source through: the registry of compiler-known names the lowering's suites declare as source, whose spellings are load-bearing where a suite writes the declarations they name — the lowering's `unused_declaration` among them.

use curios_common::{
    CharacterSyntax, ConceptField, ConjunctionSyntax, DerivationSyntax, EntailmentSyntax,
    EqlSyntax, EqualitySyntax, HashDerivation, IntegerSyntax, KeyDerivation, LiftSyntax,
    MonadSyntax, NaturalSyntax, OperatorSyntax, OptionSyntax, OrdDerivation, OrdLaws, OrderSyntax,
    ProgramSyntax, ProofSyntax, ReportSyntax, SpellDerivation, StringSyntax, SyntaxName,
    SyntaxRegistry, TestSyntax,
};

pub const fn registry_name(segments: &'static [&'static str]) -> SyntaxName {
    SyntaxName::new(segments)
}

pub const fn registry_field(
    segments: &'static [&'static str],
    label: &'static str,
) -> ConceptField {
    ConceptField {
        concept: registry_name(segments),
        field: label,
    }
}

pub const SYNTAX: SyntaxRegistry = SyntaxRegistry {
    option: OptionSyntax {
        family: registry_name(&["sys", "Option"]),
        some: registry_name(&["sys", "Option", "some"]),
        none: registry_name(&["sys", "Option", "none"]),
    },
    monad: MonadSyntax {
        bind: registry_field(&["std", "Monad"], "bind"),
    },
    lift: LiftSyntax {
        lift: registry_field(&["std", "Lift"], "lift"),
    },
    operator: OperatorSyntax {
        add: registry_field(&["std", "Add"], "add"),
        sub: registry_field(&["std", "Subtract"], "sub"),
        mul: registry_field(&["std", "Multiply"], "mul"),
        div: registry_field(&["std", "Divide"], "div"),
        rem: registry_field(&["std", "Remainder"], "rem"),
        lt: registry_field(&["std", "Compare"], "lt"),
        le: registry_field(&["std", "Compare"], "le"),
        and: registry_field(&["std", "And"], "and"),
        or: registry_field(&["std", "Or"], "or"),
    },
    eql: EqlSyntax {
        eql: registry_field(&["std", "Equal", "Equal"], "eql"),
        eq: registry_field(&["std", "Equal", "Equal"], "eq"),
        refl: registry_field(&["std", "Equal", "Equal"], "refl"),
    },
    character: CharacterSyntax {
        character: registry_name(&["std", "Char", "Char"]),
    },
    string: StringSyntax {
        string: registry_name(&["std", "Str", "Str"]),
    },
    proof: ProofSyntax {
        true_qed: registry_name(&["std", "Bool", "True", "qed"]),
        true_type: registry_name(&["std", "Bool", "True"]),
        false_type: registry_name(&["std", "Bool", "False"]),
        holds: registry_name(&["std", "Bool", "Holds"]),
    },
    equality: EqualitySyntax {
        family: registry_name(&["std", "Eq", "Eq"]),
        refl: registry_name(&["std", "Eq", "Eq", "refl"]),
        sym: registry_name(&["std", "Eq", "sym"]),
    },
    conjunction: ConjunctionSyntax {
        first: registry_name(&["std", "Bool", "and", "first"]),
        second: registry_name(&["std", "Bool", "and", "second"]),
        both: registry_name(&["std", "Bool", "and", "both"]),
    },
    test: TestSyntax {
        test_type: registry_name(&["std", "Test", "Test"]),
        main: registry_name(&["test", "main"]),
    },
    program: ProgramSyntax {
        execute: registry_field(&["std", "Program"], "execute"),
    },
    report: ReportSyntax {
        try_type: registry_name(&["std", "Try"]),
        try_run: registry_name(&["std", "Try", "run"]),
        str_concat: registry_name(&["std", "Str", "concat"]),
        fmt_render: registry_name(&["std", "Fmt", "render"]),
        result_type: registry_name(&["std", "Result"]),
    },
    // These spellings are load-bearing for the lowering's `unused_declaration` suite, which declares them as source and asserts the references that follow: a row whose concept does not match what the source declares yields no vocabulary at all, and that suite is what says so.
    derivations: DerivationSyntax {
        spell: SpellDerivation {
            spell: registry_field(&["std", "Spell", "Spell"], "spell"),
            call: registry_name(&["std", "Spell", "call"]),
            record: registry_name(&["std", "Spell", "record"]),
            hidden: registry_name(&["std", "Spell", "hidden"]),
        },
        ord: OrdDerivation {
            ord: registry_field(&["std", "Ord", "Ord"], "ord"),
            lexicographic: registry_name(&["std", "Ord", "lexicographic"]),
            by_tag: registry_name(&["std", "Ord", "by_tag", "by_tag"]),
            eq: registry_name(&["std", "Ord", "Ordering", "Ordering", "eq"]),
            is_lt: registry_name(&["std", "Ord", "Ordering", "is_lt"]),
            is_le: registry_name(&["std", "Ord", "Ordering", "is_le"]),
            laws: OrdLaws {
                flips: registry_field(&["std", "Ord", "Ord"], "flips"),
                eq: registry_field(&["std", "Ord", "Ord"], "eq"),
                chains: registry_name(&["std", "Ord", "chains"]),
                then_flips: registry_name(&["std", "Ord", "Ordering", "then", "flips"]),
                then_first: registry_name(&["std", "Ord", "Ordering", "then", "first"]),
                then_second: registry_name(&["std", "Ord", "Ordering", "then", "second"]),
                chains_then: registry_name(&["std", "Ord", "Ordering", "chains", "then"]),
                chains_le: registry_name(&["std", "Ord", "Ordering", "chains", "le"]),
                by_tag_flips: registry_name(&["std", "Ord", "by_tag", "flips"]),
                by_tag_chains: registry_name(&["std", "Ord", "by_tag", "chains"]),
                by_tag_tied: registry_name(&["std", "Ord", "by_tag", "tied"]),
            },
        },
        hash: HashDerivation {
            hash: registry_field(&["std", "Digest", "Digest"], "digest"),
            position: registry_name(&["std", "Nat", "hash", "hash"]),
            delimited: registry_name(&["std", "Digest", "delimited", "delimited"]),
        },
        key: KeyDerivation {
            mono: registry_field(&["std", "Digest", "Key", "Key"], "mono"),
            then: registry_name(&["std", "Digest", "Key", "then"]),
            last: registry_name(&["std", "Digest", "Key", "last"]),
        },
    },
    entailment: EntailmentSyntax {
        range: registry_name(&["std", "Nat", "le", "of_in_range"]),
        in_range: registry_name(&["std", "Nat", "in_range"]),
        or_first: registry_name(&["std", "Bool", "or", "of_first"]),
        or_second: registry_name(&["std", "Bool", "or", "of_second"]),
        proved: registry_name(&["std", "proved"]),
        nat: OrderSyntax {
            add: registry_name(&["std", "Nat", "le", "add"]),
            scale: registry_name(&["std", "Nat", "le", "mul_mono_r"]),
            mul: registry_name(&["std", "Nat", "le", "mul"]),
            of_eq: registry_name(&["std", "Nat", "le", "of_eq"]),
            of_lt: registry_name(&["std", "Nat", "le", "of_lt"]),
            of_le_ne: registry_name(&["std", "Nat", "lt", "of_le_ne"]),
            min_of_le: registry_name(&["std", "Nat", "min", "of_le"]),
            min_of_ge: registry_name(&["std", "Nat", "min", "of_ge"]),
            max_of_le: registry_name(&["std", "Nat", "max", "of_le"]),
            max_of_ge: registry_name(&["std", "Nat", "max", "of_ge"]),
        },
        int: OrderSyntax {
            add: registry_name(&["std", "Int", "le", "add"]),
            scale: registry_name(&["std", "Int", "le", "mul_mono_r"]),
            mul: registry_name(&["std", "Int", "le", "mul"]),
            of_eq: registry_name(&["std", "Int", "le", "of_eq"]),
            of_lt: registry_name(&["std", "Int", "le", "of_lt"]),
            of_le_ne: registry_name(&["std", "Int", "lt", "of_le_ne"]),
            min_of_le: registry_name(&["std", "Int", "min", "of_le"]),
            min_of_ge: registry_name(&["std", "Int", "min", "of_ge"]),
            max_of_le: registry_name(&["std", "Int", "max", "of_le"]),
            max_of_ge: registry_name(&["std", "Int", "max", "of_ge"]),
        },
        natural: NaturalSyntax {
            below: registry_name(&["std", "Nat", "le", "add_r"]),
            above: registry_name(&["std", "Nat", "lt", "add_mul_lt"]),
            shift: registry_name(&["std", "Nat", "le", "add_mono_l"]),
            difference: registry_name(&["std", "Nat", "le", "add_sub_cancel"]),
            residual: registry_name(&["std", "Nat", "le", "add_sub"]),
        },
        integer: IntegerSyntax {
            abs_of_non_neg: registry_name(&["std", "Int", "abs", "of_non_neg"]),
            abs_of_neg: registry_name(&["std", "Int", "abs", "of_neg"]),
            sign_of_pos: registry_name(&["std", "Int", "sign", "of_pos"]),
            sign_of_not_pos: registry_name(&["std", "Int", "sign", "of_not_pos"]),
            sign_of_neg: registry_name(&["std", "Int", "sign", "of_neg"]),
            sign_of_not_neg: registry_name(&["std", "Int", "sign", "of_not_neg"]),
            abs_at_least: registry_name(&["std", "Int", "abs", "at_least"]),
            abs_at_least_negated: registry_name(&["std", "Int", "abs", "at_least_negated"]),
            sign_at_least: registry_name(&["std", "Int", "sign", "at_least"]),
            sign_at_most: registry_name(&["std", "Int", "sign", "at_most"]),
            below: registry_name(&["std", "Int", "le", "add_r"]),
            above: registry_name(&["std", "Int", "lt", "add_mono_l"]),
            shift: registry_name(&["std", "Int", "le", "add_mono_l"]),
        },
    },
};
