//! What the walk derives for itself rather than reading off the module.

use {
    super::derived_binder_floor,
    curios_base::{Qualifier, RootId},
    curios_core::{
        Definition, DefinitionKind, Free, Global, Item, Module, Prim, Term, Totality,
        UniverseContext,
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
