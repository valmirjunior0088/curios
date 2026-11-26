//! Canonical compiler-known names, owned by the `/sys` and `/std` declarations this crate supplies, and by `/test`, the runner's half of a test program.

use curios_common::{
    CharacterSyntax, ConceptField, ConjunctionSyntax, DerivationSyntax, EntailmentSyntax,
    EqlSyntax, EqualitySyntax, HashDerivation, IntegerSyntax, KeyDerivation, LiftSyntax,
    MonadSyntax, NaturalSyntax, OperatorSyntax, OptionSyntax, OrdDerivation, OrdLaws, OrderSyntax,
    ProgramSyntax, ProofSyntax, ReportSyntax, RootSyntax, SpellDerivation, StringSyntax,
    SyntaxName, SyntaxRegistry, TestSyntax,
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

/// The word each root is mounted at, and the one spelling of each: every name below opens with one, and [`SYNTAX`]'s `roots` carries them to every reader asking where a root is — the prelude's build that mounts `/sys` and `/std`, the fold that compiles `/test` and the manifest's refusal of a package claiming it among them.
const SYS: &str = "sys";
const STD: &str = "std";
const TEST: &str = "test";

pub const SYNTAX: SyntaxRegistry = SyntaxRegistry {
    roots: RootSyntax {
        sys: SYS,
        std: STD,
        test: TEST,
    },
    option: OptionSyntax {
        family: name(&[SYS, "Option"]),
        some: name(&[SYS, "Option", "some"]),
        none: name(&[SYS, "Option", "none"]),
    },
    monad: MonadSyntax {
        bind: field(&[STD, "Monad", "Monad"], "bind"),
    },
    lift: LiftSyntax {
        lift: field(&[STD, "Monad", "Lift"], "lift"),
    },
    operator: OperatorSyntax {
        add: field(&[STD, "ops", "Add"], "add"),
        sub: field(&[STD, "ops", "Sub"], "sub"),
        mul: field(&[STD, "ops", "Mul"], "mul"),
        div: field(&[STD, "ops", "Div"], "div"),
        rem: field(&[STD, "ops", "Rem"], "rem"),
        lt: field(&[STD, "Ord", "Ord"], "lt"),
        le: field(&[STD, "Ord", "Ord"], "le"),
        and: field(&[STD, "ops", "And"], "and"),
        or: field(&[STD, "ops", "Or"], "or"),
    },
    eql: EqlSyntax {
        eql: field(&[STD, "ops", "Eql"], "eql"),
        eq: field(&[STD, "ops", "Eql"], "eq"),
        refl: field(&[STD, "ops", "Eql"], "refl"),
    },
    character: CharacterSyntax {
        character: name(&[STD, "Char", "Char"]),
    },
    string: StringSyntax {
        string: name(&[STD, "Str", "Str"]),
    },
    proof: ProofSyntax {
        true_qed: name(&[SYS, "Bool", "True", "qed"]),
        true_type: name(&[SYS, "Bool", "True"]),
        false_type: name(&[SYS, "Bool", "False"]),
        holds: name(&[SYS, "Bool", "Holds"]),
    },
    equality: EqualitySyntax {
        family: name(&[STD, "Eq", "Eq"]),
        refl: name(&[STD, "Eq", "Eq", "refl"]),
        sym: name(&[STD, "Eq", "sym"]),
    },
    conjunction: ConjunctionSyntax {
        first: name(&[STD, "Bool", "and", "first"]),
        second: name(&[STD, "Bool", "and", "second"]),
        both: name(&[STD, "Bool", "and", "both"]),
    },
    test: TestSyntax {
        test_type: name(&[STD, "Test", "Test"]),
        main: name(&[TEST, "main"]),
    },
    program: ProgramSyntax {
        execute: field(&[STD, "Program", "Program"], "execute"),
    },
    report: ReportSyntax {
        try_type: name(&[STD, "Try", "Try"]),
        try_run: name(&[STD, "Try", "run"]),
        str_concat: name(&[STD, "Str", "concat"]),
        fmt_render: name(&[STD, "Fmt", "render"]),
        result_type: name(&[STD, "Result", "Result"]),
    },
    derivations: DerivationSyntax {
        spell: SpellDerivation {
            spell: field(&[STD, "Spell", "Spell"], "spell"),
            call: name(&[STD, "Spell", "call"]),
            record: name(&[STD, "Spell", "record"]),
            hidden: name(&[STD, "Spell", "hidden"]),
        },
        ord: OrdDerivation {
            ord: field(&[STD, "Ord", "Ord"], "ord"),
            lexicographic: name(&[STD, "Ord", "lexicographic"]),
            by_tag: name(&[STD, "Ord", "by_tag", "by_tag"]),
            eq: name(&[STD, "Ord", "Ordering", "Ordering", "eq"]),
            is_lt: name(&[STD, "Ord", "Ordering", "is_lt"]),
            is_le: name(&[STD, "Ord", "Ordering", "is_le"]),
            laws: OrdLaws {
                flips: field(&[STD, "Ord", "Ord"], "flips"),
                eq: field(&[STD, "Ord", "Ord"], "eq"),
                chains: name(&[STD, "Ord", "chains"]),
                then_flips: name(&[STD, "Ord", "Ordering", "then", "flips"]),
                then_first: name(&[STD, "Ord", "Ordering", "then", "first"]),
                then_second: name(&[STD, "Ord", "Ordering", "then", "second"]),
                chains_then: name(&[STD, "Ord", "Ordering", "chains", "then"]),
                chains_le: name(&[STD, "Ord", "Ordering", "chains", "le"]),
                by_tag_flips: name(&[STD, "Ord", "by_tag", "flips"]),
                by_tag_chains: name(&[STD, "Ord", "by_tag", "chains"]),
                by_tag_tied: name(&[STD, "Ord", "by_tag", "tied"]),
            },
        },
        hash: HashDerivation {
            hash: field(&[STD, "Hash", "Hash"], "hash"),
            position: name(&[STD, "Nat", "hash", "hash"]),
            delimited: name(&[STD, "Hash", "delimited", "delimited"]),
        },
        key: KeyDerivation {
            mono: field(&[STD, "Hash", "Key", "Key"], "mono"),
            then: name(&[STD, "Hash", "Key", "then"]),
            last: name(&[STD, "Hash", "Key", "last"]),
        },
    },
    entailment: EntailmentSyntax {
        range: name(&[STD, "Nat", "le", "of_in_range"]),
        in_range: name(&[STD, "Nat", "in_range"]),
        or_first: name(&[STD, "Bool", "or", "of_first"]),
        or_second: name(&[STD, "Bool", "or", "of_second"]),
        proved: name(&[STD, "proved"]),
        nat: OrderSyntax {
            add: name(&[STD, "Nat", "le", "add"]),
            scale: name(&[STD, "Nat", "le", "mul_mono_r"]),
            mul: name(&[STD, "Nat", "le", "mul"]),
            of_eq: name(&[STD, "Nat", "le", "of_eq"]),
            of_lt: name(&[STD, "Nat", "le", "of_lt"]),
            of_le_ne: name(&[STD, "Nat", "lt", "of_le_ne"]),
            min_of_le: name(&[STD, "Nat", "min", "of_le"]),
            min_of_ge: name(&[STD, "Nat", "min", "of_ge"]),
            max_of_le: name(&[STD, "Nat", "max", "of_le"]),
            max_of_ge: name(&[STD, "Nat", "max", "of_ge"]),
        },
        int: OrderSyntax {
            add: name(&[STD, "Int", "le", "add"]),
            scale: name(&[STD, "Int", "le", "mul_mono_r"]),
            mul: name(&[STD, "Int", "le", "mul"]),
            of_eq: name(&[STD, "Int", "le", "of_eq"]),
            of_lt: name(&[STD, "Int", "le", "of_lt"]),
            of_le_ne: name(&[STD, "Int", "lt", "of_le_ne"]),
            min_of_le: name(&[STD, "Int", "min", "of_le"]),
            min_of_ge: name(&[STD, "Int", "min", "of_ge"]),
            max_of_le: name(&[STD, "Int", "max", "of_le"]),
            max_of_ge: name(&[STD, "Int", "max", "of_ge"]),
        },
        natural: NaturalSyntax {
            below: name(&[STD, "Nat", "le", "add_r"]),
            above: name(&[STD, "Nat", "lt", "add_mul_lt"]),
            shift: name(&[STD, "Nat", "le", "add_mono_l"]),
            difference: name(&[STD, "Nat", "le", "add_sub_cancel"]),
            residual: name(&[STD, "Nat", "le", "add_sub"]),
        },
        integer: IntegerSyntax {
            abs_of_non_neg: name(&[STD, "Int", "abs", "of_non_neg"]),
            abs_of_neg: name(&[STD, "Int", "abs", "of_neg"]),
            sign_of_pos: name(&[STD, "Int", "sign", "of_pos"]),
            sign_of_not_pos: name(&[STD, "Int", "sign", "of_not_pos"]),
            sign_of_neg: name(&[STD, "Int", "sign", "of_neg"]),
            sign_of_not_neg: name(&[STD, "Int", "sign", "of_not_neg"]),
            abs_at_least: name(&[STD, "Int", "abs", "at_least"]),
            abs_at_least_negated: name(&[STD, "Int", "abs", "at_least_negated"]),
            sign_at_least: name(&[STD, "Int", "sign", "at_least"]),
            sign_at_most: name(&[STD, "Int", "sign", "at_most"]),
            below: name(&[STD, "Int", "le", "add_r"]),
            above: name(&[STD, "Int", "lt", "add_mono_l"]),
            shift: name(&[STD, "Int", "le", "add_mono_l"]),
        },
    },
};
