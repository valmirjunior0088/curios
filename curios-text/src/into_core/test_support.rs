//! Lowering a source to core and reading the result back: the harness every case in these suites asserts through.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use crate::{Entrypoint, RootSource, sys_module};
use curios_abi::host_ops;
use curios_utilities::{
    CharacterSyntax, ConceptField, LiftSyntax, MonadSyntax, OperatorSyntax, ProofSyntax, Qualifier,
    RootKind, SpellSyntax, StringSyntax, SyntaxName, SyntaxRegistry, TestSyntax,
};
use std::{
    fs,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
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
        scalar_below: registry_name(&["std", "Char", "Scalar", "below"]),
        scalar_above: registry_name(&["std", "Char", "Scalar", "above"]),
    },
    string: StringSyntax {
        string: registry_name(&["std", "Str", "Str"]),
        of_scan_eq: registry_name(&["std", "Str", "of_scan_eq"]),
        refl_scan: registry_name(&["std", "Str", "refl_scan"]),
    },
    proof: ProofSyntax {
        true_qed: registry_name(&["std", "True", "True", "qed"]),
        true_type: registry_name(&["std", "True", "True"]),
        holds: registry_name(&["std", "Bool", "Holds"]),
        flt_finite: registry_name(&["std", "Flt", "Finite"]),
        flt_non_neg: registry_name(&["std", "Flt", "NonNeg"]),
    },
    test: TestSyntax {
        test_type: registry_name(&["std", "Test", "Test"]),
        main: registry_name(&["std", "Test", "main"]),
    },
    spell: SpellSyntax {
        spell: registry_field(&["std", "Spell", "Spell"], "spell"),
        call: registry_name(&["std", "Spell", "call"]),
        record: registry_name(&["std", "Spell", "record"]),
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
    let (module, _, _, _) = super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();

    curios_core::test_support::into_nested_term(module)
}

pub(super) fn lowered_module(src: &str) -> curios_core::Module {
    let (module, _, _, _) = super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();

    module
}

pub(super) fn written_type(id: usize) -> curios_core::Term {
    curios_core::Term::type_at(curios_core::Level::meta(curios_core::UniverseMetaId(id)))
}

pub(super) fn elaborate_source(src: &str) -> curios_core::Module {
    let (module, metavar_floor, universe_floor, _) = super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();
    let mut context = curios_elab::Context::with_default_budget(SYNTAX);
    curios_elab::elaborate_and_zonk_module(
        &mut context,
        &module,
        metavar_floor,
        universe_floor,
        curios_elab::Mode::Infer,
    )
    .unwrap()
    .0
}

pub(super) fn elaboration_paths(src: &str) -> (curios_core::Module, curios_core::Module) {
    let (lowered, metavar_floor, universe_floor, _) = super::into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();
    assert!(lowered.items.len() >= 2);

    let mut lowered_prefix = lowered.clone();
    lowered_prefix.items.truncate(1);
    lowered_prefix.induct_decls.clear();
    lowered_prefix.struct_decls.clear();
    lowered_prefix.concepts.clear();
    lowered_prefix.witnesses.clear();
    lowered_prefix.tests.clear();
    lowered_prefix.entry = Some(curios_core::Entrypoint {
        body: curios_core::Term::intrinsic(curios_core::Intrinsic::Nat(curios_core::Nat::Zero)),
        type_: None,
    });
    let prelude = curios_elab::elaborate_and_zonk_module(
        &mut curios_elab::Context::with_default_budget(SYNTAX),
        &lowered_prefix,
        metavar_floor,
        universe_floor,
        curios_elab::Mode::Infer,
    )
    .unwrap()
    .0;

    let full = curios_elab::elaborate_and_zonk_module(
        &mut curios_elab::Context::with_default_budget(SYNTAX),
        &lowered,
        metavar_floor,
        universe_floor,
        curios_elab::Mode::Infer,
    )
    .unwrap()
    .0;
    let cached = curios_elab::elaborate_and_zonk_unit(
        &mut curios_elab::Context::with_default_budget(SYNTAX),
        curios_elab::Established::over(std::slice::from_ref(&&prelude)),
        &lowered,
        metavar_floor,
        universe_floor,
        curios_elab::Mode::Infer,
        curios_elab::Tail::Written,
    )
    .unwrap()
    .0;
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

/// The prelude these tests lower against: the real `/sys` roster, and a `/std` of stubs deep enough for every name `/sys` reaches through the registry.
fn prelude_fixture() -> RootSource {
    let mut modules = RootSource::supplied();
    modules.insert_root("sys", RootKind::Internal, sys_module(&host_ops(), &SYNTAX));
    modules.insert_root(
        "std",
        RootKind::Privileged,
        r#"
            pub mod Str
                pub let Valid : Type = Type;
            end
            pub mod Nat
                pub let Nat : Type = Type;
                pub let add : Type = Type;
                pub let Lt : Type = Type;
                pub let Le : Type = Type;
                pub induct Proof: pub Type
                | qed()
                end
            end
            pub mod Int
                pub let NonZero : Type = Type;
                pub let NonNeg : Type = Type;
            end
            pub mod Flt
                pub let FourBytes : Type = Type;
                pub let Finite : Type = Type;
                pub let NonNeg : Type = Type;
            end
            pub mod Bool
                pub let Holds : Type = Type;
            end
            pub use /sys/{True};
        "#
        .parse()
        .unwrap(),
    );
    // `/sys` states each decided precondition as `Holds` over one of its own comparisons, and names `Holds` and the two `Flt` bounds through the registry — so the scope has to hold whatever this fixture's registry points those at. Stubs, not definitions: these tests lower and never elaborate, so a name that resolves is the whole requirement.
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
                .map(|prefix| curios_utilities::Qualifier::from([*prefix]))
                .collect(),
        ),
        std::slice::from_ref(&&prepared),
        syntax(),
    )
    .map(|_| ())
    .map_err(|error| error.to_string())
}

pub(super) fn temp_dir(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()))
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
