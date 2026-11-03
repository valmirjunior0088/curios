use {
    super::*,
    curios_common::Qualifier,
    curios_core::{
        Definition, DefinitionKind, Definitions, Global, Intrinsic, Item, Level, Minted, Subterm,
        Term, Totality, UniverseContext, UniverseMetaId, UniverseParam, test_support::*,
    },
};

fn nat() -> Term {
    Subterm::Intrinsic(Intrinsic::NatType).into()
}

/// A lowered module holding `body` as its one definition's value.
fn lowered_definitions(body: Term) -> Definitions {
    declaring(
        Definitions {
            mounts: Vec::new(),
            items: vec![Item::Let(Box::new(Definition {
                name: Global::Authored(Qualifier::from(["held"])),
                kind: DefinitionKind::Authored,
                universe_context: UniverseContext::empty(),
                island: Qualifier::empty(),
                totality: Totality::default(),
                proof: false,
                variances: Vec::new(),
                field_variances: Vec::new(),
                parameter_field: None,
                value_sort: None,
                type_: nat(),
                body,
            }))]
            .into(),
            witnesses: Default::default(),
            tests: Default::default(),
        },
        [],
        [],
        [],
    )
}

#[test]
fn lowered_module_validation_rejects_a_truncated_universe_seed_table() {
    let definitions = lowered_definitions(Term::type_at(Level::meta(UniverseMetaId(0))));

    assert!(matches!(
        validate_lowered_universe_seeds(&definitions, &Minted::default()),
        Err(Error::UniverseInvariant(message)) if message.contains("?u0")
    ));
}

#[test]
fn universes_are_validated_once_per_node() {
    let mut value = Term::type_at(Level::param(UniverseParam(0)));
    for _ in 0..60 {
        value = Term::intrinsic(Intrinsic::nat_add(value.clone(), value));
    }

    assert!(validate_bound_universes(&value, 1, "doubled").is_ok());
    assert!(validate_bound_universes(&value, 0, "doubled").is_err());
}
