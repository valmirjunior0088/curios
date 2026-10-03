//! Lowering a source to core and reading the result back: the harness every case in these suites asserts through.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::{Entrypoint, RootSource, sys_module},
    curios_abi::host_ops,
    curios_utilities::{
        ChannelSyntax, CharacterSyntax, ConceptField, DerivationSyntax, EntailmentSyntax,
        EqlDerivation, HashDerivation, LiftSyntax, MonadSyntax, NaturalSyntax, OperatorSyntax,
        OptionSyntax, OrdDerivation, OrderSyntax, ProofSyntax, Qualifier, ResultSyntax, RootKind,
        SpellDerivation, StringSyntax, SyntaxName, SyntaxRegistry, TestSyntax,
        test_support::Temporary,
    },
    std::{fs, path::Path},
};

pub(super) const fn registry_name(segments: &'static [&'static str]) -> SyntaxName {
    SyntaxName::new(segments)
}

pub(super) const fn registry_field(
    segments: &'static [&'static str],
    label: &'static str,
) -> ConceptField {
    ConceptField {
        concept: registry_name(segments),
        field: label,
    }
}

pub(super) const SYNTAX: SyntaxRegistry = SyntaxRegistry {
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
        bind: registry_name(&["std", "Monad", "bind"]),
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
    // These spellings are load-bearing for `ordering_tests`, which declares them as source and asserts the edges that follow: a row whose concept does not match what the source declares yields no vocabulary at all, and that suite is what says so.
    derivations: DerivationSyntax {
        spell: SpellDerivation {
            spell: registry_field(&["std", "Spell", "Spell"], "spell"),
            call: registry_name(&["std", "Spell", "call"]),
            record: registry_name(&["std", "Spell", "record"]),
        },
        eql: EqlDerivation {
            eql: registry_field(&["std", "Equal", "Equal"], "eql"),
        },
        ord: OrdDerivation {
            ord: registry_field(&["std", "Ord", "Ord"], "ord"),
            lexicographic: registry_name(&["std", "Ord", "lexicographic"]),
            by_tag: registry_name(&["std", "Ord", "by_tag"]),
            tied: registry_name(&["std", "Ord", "tied"]),
        },
        hash: HashDerivation {
            hash: registry_field(&["std", "Digest", "Digest"], "digest"),
            tagged: registry_name(&["std", "Digest", "tagged"]),
        },
    },
    entailment: EntailmentSyntax {
        holds_of_eq: registry_name(&["std", "Bool", "holds_of_eq"]),
        refl: registry_name(&["std", "Eq", "refl"]),
        equality: registry_name(&["std", "Eq", "Eq"]),
        sym: registry_name(&["std", "Eq", "sym"]),
        range: registry_name(&["std", "Nat", "le", "of_in_range"]),
        in_range: registry_name(&["std", "Nat", "in_range"]),
        nat: OrderSyntax {
            add: registry_name(&["std", "Nat", "le", "add"]),
            scale: registry_name(&["std", "Nat", "le", "mul_mono_r"]),
            mul: registry_name(&["std", "Nat", "le", "mul"]),
            of_eq: registry_name(&["std", "Nat", "le", "of_eq"]),
            eq_of_eql: registry_name(&["std", "Nat", "eq_of_eql"]),
            of_not_lt: registry_name(&["std", "Nat", "le", "of_not_lt"]),
            of_not_le: registry_name(&["std", "Nat", "lt", "of_not_le"]),
        },
        int: OrderSyntax {
            add: registry_name(&["std", "Int", "le", "add"]),
            scale: registry_name(&["std", "Int", "le", "mul_mono_r"]),
            mul: registry_name(&["std", "Int", "le", "mul"]),
            of_eq: registry_name(&["std", "Int", "le", "of_eq"]),
            eq_of_eql: registry_name(&["std", "Int", "eq_of_eql"]),
            of_not_lt: registry_name(&["std", "Int", "le", "of_not_lt"]),
            of_not_le: registry_name(&["std", "Int", "lt", "of_not_le"]),
        },
        natural: NaturalSyntax {
            below: registry_name(&["std", "Nat", "le", "add_r"]),
            above: registry_name(&["std", "Nat", "lt", "add_mul_lt"]),
            shift: registry_name(&["std", "Nat", "le", "add_mono_l"]),
            difference: registry_name(&["std", "Nat", "le", "add_sub_cancel"]),
            truncated: registry_name(&["std", "Nat", "le", "sub_zero"]),
            loosened: registry_name(&["std", "Nat", "le", "of_lt"]),
        },
    },
};

pub(super) fn syntax() -> &'static SyntaxRegistry {
    &SYNTAX
}

/// A top-level definition's identity, from the path a test writes. Fixture-only — production code carries the `Qualifier` from resolution instead of recovering it from a spelling.
pub(super) fn global(path: &str) -> curios_core::Free {
    curios_core::Free::global(Qualifier::from(path.trim_start_matches('/').split('/')))
}

pub(super) fn global_name(path: &str) -> curios_core::Global {
    curios_core::Global::Authored(Qualifier::from(path.trim_start_matches('/').split('/')))
}

pub(super) fn run(src: &str) -> curios_core::Term {
    let (program, _, _) = super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();

    curios_core::test_support::into_nested_term(program)
}

pub(super) fn lowered_module(src: &str) -> curios_core::Module {
    let (program, _, _) = super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();

    program.module
}

/// [`lowered_module`] over the fixture prelude, for what a unit's lowering reads off the units before it.
pub(super) fn lowered_module_over_prelude(src: &str) -> curios_core::Module {
    let modules = prelude_fixture();
    let prepared = super::prepare_prelude(&modules, &[], syntax()).unwrap();

    super::into_core_with_prelude(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        std::slice::from_ref(&&prepared),
        syntax(),
    )
    .unwrap()
    .program
    .module
}

pub(super) fn written_type(id: usize) -> curios_core::Term {
    curios_core::Term::type_at(curios_core::Level::meta(curios_core::UniverseMetaId(id)))
}

/// `src` lowered and elaborated as a program against `established`, its entry inferred.
fn elaborate_program(src: &str, established: curios_elab::Established<'_>) -> curios_core::Program {
    let (program, minted, _) = super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();
    let (module, entry) = curios_elab::elaborate_and_zonk_program(
        &mut curios_elab::Context::with_default_budget(SYNTAX),
        established,
        &program.module,
        &minted,
        curios_elab::Tail::Entry(&program.entry),
    )
    .unwrap();

    curios_core::Program {
        module,
        entry: entry.expect("nothing here fails to parse, so nothing withholds the entry"),
    }
}

/// `src` elaborated with nothing in scope, and the term it closes with.
pub(super) fn elaborate_source_program(src: &str) -> curios_core::Program {
    elaborate_program(src, curios_elab::Established::nothing())
}

/// The module `src` elaborates to with nothing in scope.
pub(super) fn elaborate_source(src: &str) -> curios_core::Module {
    elaborate_source_program(src).module
}

pub(super) fn elaboration_paths(src: &str) -> (curios_core::Program, curios_core::Program) {
    let (lowered, minted, _) = super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();
    assert!(lowered.module.items.len() >= 2);

    let mut lowered_prefix = lowered.module.clone();
    lowered_prefix.items.truncate(1);
    lowered_prefix.induct_decls.clear();
    lowered_prefix.struct_decls.clear();
    lowered_prefix.concepts.clear();
    lowered_prefix.witnesses.clear();
    lowered_prefix.tests.clear();
    let prelude = curios_elab::elaborate_and_zonk_module(
        &mut curios_elab::Context::with_default_budget(SYNTAX),
        &lowered_prefix,
        &minted,
    )
    .unwrap();

    let full = elaborate_program(src, curios_elab::Established::nothing());
    let cached = elaborate_program(
        src,
        curios_elab::Established::over(std::slice::from_ref(&&prelude)),
    );
    (full, cached)
}

/// The lints of `src` lowered alone, each as `kind: message`.
pub(super) fn lints(src: &str) -> Vec<String> {
    let entrypoint = src.parse::<Entrypoint>().unwrap();
    let loader = RootSource::none();
    super::into_core_unit(
        &super::UnitSource::entry(&entrypoint, &loader),
        &[],
        syntax(),
    )
    .unwrap()
    .lints()
    .iter()
    .map(|lint| format!("{}: {}", lint.kind.name(), lint.report.message))
    .collect()
}

pub(super) fn run_err(src: &str) -> String {
    super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap_err()
    .to_string()
}

// `run_err` rendered as the reader sees it: the message and, where the error was placed, its snippet.
pub(super) fn run_err_report(src: &str) -> String {
    super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap_err()
    .format()
}

// Lower against the real prelude (so `sys` and `std` are served and rooted), returning only success/error — the lens for the internal-root gate.
pub(super) fn lower_with_prelude(src: &str) -> Result<(), String> {
    let modules = prelude_fixture();
    let prepared =
        super::prepare_prelude(&modules, &[], syntax()).map_err(|error| error.to_string())?;
    super::into_core_with_prelude(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        std::slice::from_ref(&&prepared),
        syntax(),
    )
    .map(|_| ())
    .map_err(|error| error.to_string())
}

/// The prelude these tests lower against: the real `/sys` roster, and a `/std` of stubs deep enough for every name `/sys` reaches through the registry — with one concept beside them, for the order a unit takes around a row it registers into a concept declared before it.
fn prelude_fixture() -> RootSource {
    let mut modules = RootSource::supplied();
    modules.insert_root("sys", RootKind::Internal, sys_module(&host_ops(), &SYNTAX));
    modules.insert_root(
        "std",
        RootKind::Ordinary,
        r#"
            pub mod Str
                pub let Valid : Type = Type;
            end
            pub mod Nat
                pub let Nat : Type = Type;
                pub let add : Type = Type;
            end
            pub mod Bool
                pub let Holds : Type = Type;
                pub use /sys/Bool/{True};
            end
            pub mod Shape
                pub concept Shape(A : Type) : pub Type {
                    area(A, A) -> Type,
                }
            end
        "#
        .parse()
        .unwrap(),
    );
    // `/sys` states each decided precondition as `Holds` over one of its own comparisons, and names `Holds` through the registry — so the scope has to hold whatever this fixture's registry points it at. Stubs, not definitions: these tests lower and never elaborate, so a name that resolves is the whole requirement.
    modules
}

/// [`lower_with_prelude`], with the entry seeing only `declared` of what the scope mounts — the lens for the undeclared-dependency refusal.
pub(super) fn lower_declaring(declared: &[&str], src: &str) -> Result<(), String> {
    let modules = prelude_fixture();
    let prepared =
        super::prepare_prelude(&modules, &[], syntax()).map_err(|error| error.to_string())?;
    let entrypoint = src.parse::<Entrypoint>().unwrap();
    let loader = RootSource::none();

    super::into_core_unit(
        &super::UnitSource::entry(&entrypoint, &loader).seeing(
            declared
                .iter()
                .map(|prefix| Qualifier::from([*prefix]))
                .collect(),
        ),
        std::slice::from_ref(&&prepared),
        syntax(),
    )
    .map(|_| ())
    .map_err(|error| error.to_string())
}

/// A directory of its own, removed however the test that holds it ends.
pub(super) fn temp_dir(name: &str) -> Temporary {
    Temporary::new("into-core", name)
}

pub(super) fn write_module(base: &Path, path: &str, source: &str) {
    let path = base.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, source).unwrap();
}

pub(super) fn universe_parameters(module: &curios_core::Module, name: &str) -> usize {
    module
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition) if definition.name.symbol() == name => {
                Some(definition.universe_context.parameter_count)
            }
            // An inductive and its constructors are one recursive group, so a lookup restricted to `Let` would miss every one of them.
            curios_core::Item::Rec(rec) => rec
                .definitions()
                .iter()
                .find(|definition| definition.name.symbol() == name)
                .map(|definition| definition.universe_context.parameter_count),
            _ => None,
        })
        .unwrap_or_else(|| panic!("{name} is declared"))
}
