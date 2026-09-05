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

pub(super) const fn syn_name(segments: &'static [&'static str]) -> SyntaxName {
    SyntaxName::new(segments)
}

pub(super) const fn syn_field(
    segments: &'static [&'static str],
    label: &'static str,
) -> ConceptField {
    ConceptField {
        concept: syn_name(segments),
        field: label,
    }
}

pub(super) const SYNTAX: SyntaxRegistry = SyntaxRegistry {
    monad: MonadSyntax {
        bind: syn_name(&["syn", "Monad", "bind"]),
    },
    lift: LiftSyntax {
        lift: syn_field(&["syn", "Lift"], "lift"),
    },
    operator: OperatorSyntax {
        add: syn_field(&["syn", "Add"], "add"),
        sub: syn_field(&["syn", "Subtract"], "sub"),
        mul: syn_field(&["syn", "Multiply"], "mul"),
        div: syn_field(&["syn", "Divide"], "div"),
        rem: syn_field(&["syn", "Remainder"], "rem"),
        eql: syn_field(&["syn", "Equal", "Equal"], "eql"),
        neq: syn_field(&["syn", "Equal", "Equal"], "neq"),
        lt: syn_field(&["syn", "Compare"], "lt"),
        gt: syn_field(&["syn", "Compare"], "gt"),
        le: syn_field(&["syn", "Compare"], "le"),
        ge: syn_field(&["syn", "Compare"], "ge"),
        and: syn_field(&["syn", "And"], "and"),
        or: syn_field(&["syn", "Or"], "or"),
    },
    character: CharacterSyntax {
        character: syn_name(&["syn", "Char", "Char"]),
        scalar_below: syn_name(&["syn", "Char", "Scalar", "below"]),
        scalar_above: syn_name(&["syn", "Char", "Scalar", "above"]),
    },
    string: StringSyntax {
        string: syn_name(&["syn", "Str", "Str"]),
        of_scan_eq: syn_name(&["syn", "Str", "of_scan_eq"]),
        refl_scan: syn_name(&["syn", "Str", "refl_scan"]),
    },
    proof: ProofSyntax {
        true_qed: syn_name(&["syn", "True", "True", "qed"]),
        true_type: syn_name(&["syn", "True", "True"]),
        lt: syn_name(&["syn", "Nat", "Lt"]),
        le: syn_name(&["syn", "Nat", "Le"]),
        int_non_zero: syn_name(&["syn", "Int", "NonZero"]),
        int_non_neg: syn_name(&["syn", "Int", "NonNeg"]),
        bytes_four: syn_name(&["syn", "Flt", "FourBytes"]),
        flt_finite: syn_name(&["syn", "Flt", "Finite"]),
        flt_non_neg: syn_name(&["syn", "Flt", "NonNeg"]),
    },
    test: TestSyntax {
        test_type: syn_name(&["syn", "Test", "Test"]),
        main: syn_name(&["syn", "Test", "main"]),
        property: syn_name(&["syn", "Test", "property"]),
        settled: syn_name(&["syn", "Test", "settled"]),
        theorem: syn_name(&["syn", "Test", "Test", "theorem"]),
    },
    spell: SpellSyntax {
        spell: syn_field(&["syn", "Spell", "Spell"], "spell"),
        call: syn_name(&["syn", "Spell", "call"]),
        record: syn_name(&["syn", "Spell", "record"]),
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
            end
        "#
        .parse()
        .unwrap(),
    );
    // `/sys` states its preconditions in `/syn`'s propositions, so the roster this fixture builds now carries references out of its own root and the scope has to hold their targets. Stubs, not definitions: these tests lower and never elaborate, so a name that resolves is the whole requirement — the same reason `/std` above is two modules of `Type`.
    modules.insert_root(
        "syn",
        RootKind::Privileged,
        r#"
            pub mod Nat
                pub let Lt : Type = Type;
                pub let Le : Type = Type;
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
        "#
        .parse()
        .unwrap(),
    );
    let prepared = super::prepare_prelude(&modules, syntax()).map_err(|error| error.to_string())?;
    super::into_core_with_prelude(
        &src.parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
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
