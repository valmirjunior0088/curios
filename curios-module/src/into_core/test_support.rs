//! Lowering a source to core and reading the result back: the harness every case in these suites asserts through.

use {
    super::lower_unit,
    crate::*,
    curios_abi::ForeignStore,
    curios_common::{
        ChannelSyntax, CharacterSyntax, ConceptField, DerivationSyntax, EntailmentSyntax,
        EqlDerivation, HashDerivation, IntegerSyntax, LiftSyntax, MonadSyntax, Mount,
        NaturalSyntax, OperatorSyntax, OptionSyntax, OrdDerivation, OrderSyntax, ProgramSyntax,
        ProofSyntax, Qualifier, ReportSyntax, ResultSyntax, SpellDerivation, StringSyntax,
        SyntaxName, SyntaxRegistry, TestSyntax, test_support::*,
    },
    curios_core::test_support::*,
    curios_surface::Entrypoint,
    std::{collections::BTreeSet, fs, path::Path, rc::Rc},
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
    result: ResultSyntax {
        family: registry_name(&["sys", "Result"]),
        success: registry_name(&["sys", "Result", "success"]),
        failure: registry_name(&["sys", "Result", "failure"]),
    },
    channel: ChannelSyntax {
        push: registry_name(&["sys", "Channel", "Push"]),
        taken: registry_name(&["sys", "Channel", "Push", "taken"]),
        full: registry_name(&["sys", "Channel", "Push", "full"]),
        closed: registry_name(&["sys", "Channel", "Push", "closed"]),
        take: registry_name(&["sys", "Channel", "Take"]),
        item: registry_name(&["sys", "Channel", "Take", "item"]),
        empty: registry_name(&["sys", "Channel", "Take", "empty"]),
        ended: registry_name(&["sys", "Channel", "Take", "ended"]),
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
        eql: registry_field(&["std", "Equal", "Equal"], "eql"),
        neq: registry_field(&["std", "Equal", "Equal"], "neq"),
        lt: registry_field(&["std", "Compare"], "lt"),
        gt: registry_field(&["std", "Compare"], "gt"),
        le: registry_field(&["std", "Compare"], "le"),
        ge: registry_field(&["std", "Compare"], "ge"),
        and: registry_field(&["std", "And"], "and"),
        or: registry_field(&["std", "Or"], "or"),
    },
    character: CharacterSyntax {
        character: registry_name(&["std", "Char", "Char"]),
    },
    string: StringSyntax {
        string: registry_name(&["std", "Str", "Str"]),
        qed: registry_name(&["std", "Bool", "True", "qed"]),
    },
    proof: ProofSyntax {
        true_qed: registry_name(&["std", "Bool", "True", "qed"]),
        true_type: registry_name(&["std", "Bool", "True"]),
        false_type: registry_name(&["std", "Bool", "False"]),
        holds: registry_name(&["std", "Bool", "Holds"]),
    },
    test: TestSyntax {
        test_type: registry_name(&["std", "Test", "Test"]),
        main: registry_name(&["std", "Test", "main"]),
    },
    program: ProgramSyntax {
        execute: registry_field(&["std", "Program"], "execute"),
    },
    report: ReportSyntax {
        try_type: registry_name(&["std", "Try"]),
        try_run: registry_name(&["std", "Try", "run"]),
        str_concat: registry_name(&["std", "Str", "concat"]),
        fmt_render: registry_name(&["std", "Fmt", "render"]),
    },
    // These spellings are load-bearing for `tests::unused_declaration`, which declares them as source and asserts the references that follow: a row whose concept does not match what the source declares yields no vocabulary at all, and that suite is what says so.
    derivations: DerivationSyntax {
        spell: SpellDerivation {
            spell: registry_field(&["std", "Spell", "Spell"], "spell"),
            call: registry_name(&["std", "Spell", "call"]),
            record: registry_name(&["std", "Spell", "record"]),
            hidden: registry_name(&["std", "Spell", "hidden"]),
        },
        eql: EqlDerivation {
            eql: registry_field(&["std", "Equal", "Equal"], "eql"),
        },
        ord: OrdDerivation {
            ord: registry_field(&["std", "Ord", "Ord"], "ord"),
            lexicographic: registry_name(&["std", "Ord", "lexicographic"]),
            by_tag: registry_name(&["std", "Ord", "by_tag"]),
            eq: registry_name(&["std", "Ord", "Ordering", "Ordering", "eq"]),
        },
        hash: HashDerivation {
            hash: registry_field(&["std", "Digest", "Digest"], "digest"),
            tagged: registry_name(&["std", "Digest", "tagged"]),
        },
    },
    entailment: EntailmentSyntax {
        equality: registry_name(&["std", "Eq", "Eq"]),
        sym: registry_name(&["std", "Eq", "sym"]),
        range: registry_name(&["std", "Nat", "le", "of_in_range"]),
        in_range: registry_name(&["std", "Nat", "in_range"]),
        and_first: registry_name(&["std", "Bool", "and", "first"]),
        and_second: registry_name(&["std", "Bool", "and", "second"]),
        nat: OrderSyntax {
            add: registry_name(&["std", "Nat", "le", "add"]),
            scale: registry_name(&["std", "Nat", "le", "mul_mono_r"]),
            mul: registry_name(&["std", "Nat", "le", "mul"]),
            of_eq: registry_name(&["std", "Nat", "le", "of_eq"]),
            eq_of_eql: registry_name(&["std", "Nat", "eq_of_eql"]),
            of_not_lt: registry_name(&["std", "Nat", "le", "of_not_lt"]),
            of_not_le: registry_name(&["std", "Nat", "lt", "of_not_le"]),
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
            eq_of_eql: registry_name(&["std", "Int", "eq_of_eql"]),
            of_not_lt: registry_name(&["std", "Int", "le", "of_not_lt"]),
            of_not_le: registry_name(&["std", "Int", "lt", "of_not_le"]),
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
        },
    },
};

pub fn syntax() -> &'static SyntaxRegistry {
    &SYNTAX
}

/// A top-level definition's identity, from the path a test writes. Fixture-only — production code carries the `Qualifier` from resolution instead of recovering it from a spelling.
pub fn global_name(path: &str) -> curios_core::Global {
    curios_core::Global::Authored(Qualifier::from(path.trim_start_matches('/').split('/')))
}

/// What a fixture's entry resolves through: no filesystem, and the entry tested, so its tests are lowered as a question lowers a subject's. What a build passes over is held by a test of its own.
pub fn fixture_loader() -> RootSource {
    RootSource::none().tested()
}

/// The entry's body as the lowering writes it, the module it closes over left aside.
pub fn run(src: &str) -> curios_core::Term {
    let (program, _, _, _) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();

    program.entrypoint.body
}

/// What `src` lowers to as a suite reads it: each definition's name, type and body in the module's order, beside the entry's body.
pub fn lowered(src: &str) -> (Vec<Lowered>, curios_core::Term) {
    let (program, _, _, _) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();
    let definitions = program
        .definitions
        .items
        .iter()
        .flat_map(curios_core::Item::definitions)
        .map(|definition| (definition.name, definition.type_, definition.body))
        .collect();

    (definitions, program.entrypoint.body)
}

/// A definition as [`lowered`] reads it: its name, its type and its body.
pub type Lowered = (curios_core::Global, curios_core::Term, curios_core::Term);

pub fn lowered_definitions(src: &str) -> curios_core::Definitions {
    let (program, _, _, _) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();

    program.definitions
}

/// The members of the one item `src` lowers to, where it is a recursive group whose bodies name no declaration but its own members: `None` where it is anything else.
pub fn closed_group(src: &str) -> Option<Vec<String>> {
    let definitions = lowered_definitions(src);
    let [curios_core::Item::Rec(group)] = &definitions.items.iter().collect::<Vec<_>>()[..] else {
        return None;
    };
    let definitions = group.definitions();
    let members = definitions
        .iter()
        .map(|definition| definition.name)
        .collect::<BTreeSet<_>>();

    definitions
        .iter()
        .all(|definition| {
            definition.body.mentions().iter().all(|name| {
                name.as_declaration()
                    .is_some_and(|name| members.contains(name))
            })
        })
        .then(|| {
            definitions
                .iter()
                .map(|definition| definition.name.faithful().to_string())
                .collect()
        })
}

pub fn written_type(id: usize) -> curios_core::Term {
    curios_core::Term::type_at(curios_core::Level::meta(curios_core::UniverseMetaId(id)))
}

/// `src` lowered and elaborated as a program against `established`, its entry inferred.
fn elaborate_program(
    src: &str,
    established: &curios_elab::Established<'_>,
) -> curios_core::Program {
    let (program, minted, _, mounts) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();
    let (definitions, entry) = curios_elab::elaborate_and_zonk_program(
        &mut curios_elab::Context::with_default_budget(SYNTAX),
        established,
        &program.definitions,
        &mounts,
        &minted,
        curios_elab::Tail::Entry(&program.entrypoint),
    )
    .unwrap();

    curios_core::Program {
        definitions,
        entrypoint: entry.expect("nothing here fails to parse, so nothing withholds the entry"),
    }
}

/// `src` elaborated with nothing in scope, and the term it closes with.
pub fn elaborate_source_program(src: &str) -> curios_core::Program {
    elaborate_program(src, &curios_elab::Established::nothing())
}

/// The module `src` elaborates to with nothing in scope.
pub fn elaborate_source(src: &str) -> curios_core::Definitions {
    elaborate_source_program(src).definitions
}

pub fn elaboration_paths(src: &str) -> (curios_core::Program, curios_core::Program) {
    let (lowered, minted, _, mounts) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();
    assert!(lowered.definitions.items.len() >= 2);

    let mut lowered_prefix = lowered.definitions.clone();
    lowered_prefix.items = lowered_prefix.items.iter().take(1).cloned().collect();
    let nothing = curios_elab::Established::nothing();
    let (prelude, traces) = curios_elab::elaborate_and_zonk_unit(
        &mut curios_elab::Context::with_default_budget(SYNTAX),
        &nothing,
        &lowered_prefix,
        &mounts,
        &minted,
    )
    .unwrap();

    let full = elaborate_program(src, &nothing);
    let cached = elaborate_program(
        src,
        &curios_elab::Established::over(&[(&prelude, &mounts, &traces)]),
    );
    (full, cached)
}

/// The lints of `src` lowered alone, each as `kind: message`.
pub fn lints(src: &str) -> Vec<String> {
    let entrypoint = src.parse::<Entrypoint>().unwrap();
    let loader = fixture_loader();
    super::into_core_unit(
        &super::UnitSource::entry(&entrypoint, &loader),
        &[],
        syntax(),
    )
    .unwrap()
    .lints()
    .iter()
    .map(|lint| format!("{}: {}", lint.kind.name(), lint.report(&[]).message()))
    .collect()
}

/// What stops `src` compiling, as its lowering knows it: the refusal it raised, or, where it raised none, every name it filed as resolving to nothing, each as elaboration reports it of the declaration that wrote it. A name written in a declaration refuses that declaration and not the lowering, so a fixture about a name reads both.
pub fn run_err(src: &str) -> String {
    let entrypoint = src.parse::<Entrypoint>().unwrap();
    let loader = fixture_loader();

    match super::into_core_unit(
        &super::UnitSource::entry(&entrypoint, &loader),
        &[],
        syntax(),
    ) {
        Err(error) => error.to_string(),
        Ok(unit) => unresolved(unit.minted()).expect_err("the source lowers, every name resolved"),
    }
}

// A refusal the lowering raised, rendered as the reader sees it: the block, with its pane where the error was placed.
pub fn run_err_report(src: &str) -> String {
    into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap_err()
    .format()
}

/// A directory of its own, removed however the test that holds it ends.
pub fn temp_dir(name: &str) -> Temporary {
    Temporary::new("into-core", name)
}

pub fn write_module(base: &Path, path: &str, source: &str) {
    let path = base.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, source).unwrap();
}

pub fn universe_parameters(definitions: &curios_core::Definitions, name: &str) -> usize {
    definitions
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition)
                if definition.name.faithful().to_string() == name =>
            {
                Some(definition.universe_context.parameter_count)
            }
            // An inductive and its constructors are one recursive group, so a lookup restricted to `Let` would miss every one of them.
            curios_core::Item::Rec(rec) => rec
                .definitions()
                .iter()
                .find(|definition| definition.name.faithful().to_string() == name)
                .map(|definition| definition.universe_context.parameter_count),
            _ => None,
        })
        .unwrap_or_else(|| panic!("{name} is declared"))
}

/// Lower a whole [`Entrypoint`] with nothing in scope, as `curios-module`'s own stage tests do: the program, the counters its lowering minted from, its `foreign` rows and the prefixes its unit claims.
pub fn into_core(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    syntax: &SyntaxRegistry,
) -> Result<
    (
        curios_core::Program,
        Rc<curios_core::Minted>,
        ForeignStore,
        Vec<Mount>,
    ),
    Error,
> {
    let (unit, entry) = lower_unit(&UnitSource::entry(entrypoint, loader), &[], syntax, None)?;

    Ok((
        curios_core::Program {
            definitions: unit.core,
            entrypoint: entry.expect("an entry source's parse holds its final term"),
        },
        unit.minted,
        unit.foreigns,
        unit.mounts,
    ))
}
