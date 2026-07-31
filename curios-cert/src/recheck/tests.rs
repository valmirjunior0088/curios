//! What the walk derives for itself rather than reading off the module.

use {
    super::{derived_binder_floor, recheck_module_verdicts},
    curios_base::{Qualifier, RootId},
    curios_core::{
        Definition, DefinitionKind, Free, Global, Item, Level, Module, Prim, Term, Totality,
        UniverseConstraint, UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext,
        UniverseParam,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// The floor must clear every local a module's terms mention, whatever `Module::binder_floor` claims.
///
/// A binder the kernel mints while comparing under a telescope or eta-contracting aliases a free local the moment the floor is too low, and two terms that differ stop being distinguishable. The carried number is the elaborator's word and nothing checks it, so the walk derives its own and the caller takes the larger.
#[test]
fn the_floor_clears_every_local_a_term_mentions() {
    let mentioned = Free::local(4_242, Some("y"));
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        root: RootId::Entry,
        totality: Totality::Total,
        type_: Term::prim(Prim::NatType),
        body: Term::free_var(&mentioned),
    };

    let module = Module {
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        // The understated claim the walk must not believe.
        binder_floor: 0,
        type_: None,
        body: Term::prim(Prim::NatType),
    };

    assert_eq!(derived_binder_floor(&module), 4_243);
}

/// A declaration's universe context is *assumed* while checking it, so an unsatisfiable one is a hypothesis set that proves anything.
///
/// `Kernel::assume_universes` takes the item's own constraints as given, and `entails` answers `≤` questions under them — so a context containing `u + 1 ≤ u` lets every level relation through, and `check_instance` stops discharging anything. Deciding satisfiability runs a solver and lives in `curios-elab`; this asks whether the kernel notices regardless.
#[test]
fn an_unsatisfiable_universe_context_is_refused() {
    let contradiction = UniverseConstraint {
        lower: Level::param(UniverseParam(0))
            .succ()
            .expect("level has a successor"),
        upper: Level::param(UniverseParam(0)),
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    };
    let universe_context = UniverseContext {
        parameter_count: 1,
        constraints: vec![contradiction],
    };

    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        root: RootId::Entry,
        totality: Totality::Total,
        type_: Term::prim(Prim::NatType),
        body: Term::prim(Prim::Nat(curios_core::Nat::new(0usize))),
    };

    let module = Module {
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        type_: None,
        body: Term::prim(Prim::NatType),
    };

    assert!(
        !recheck_module_verdicts(&module, 1_000_000).is_empty(),
        "the kernel assumed a contradiction as a hypothesis without noticing",
    );
}
