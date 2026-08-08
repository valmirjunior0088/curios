use {
    super::*,
    curios_base::{Qualifier, RootId},
    curios_core::{DefinitionKind, Free, Nat, UniverseContext},
};

/// A qualified top-level name, from the path a test writes. Fixture-only.
fn name(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A `let` whose body is a bare reference to `mentions`. Fixture-only: the closure reads free variables, so one reference is the whole signal.
fn mentioning(path: &str, mentions: &str) -> Item {
    Item::Let(Definition {
        name: name(path),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        root: RootId::Entry,
        totality: Totality::default(),
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::free_var(&Free::from(&name(mentions))),
    })
}

fn module(items: Vec<Item>) -> Module {
    Module {
        items,
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        type_: None,
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(0u32))),
    }
}

#[test]
fn a_partial_name_from_outside_the_module_still_taints_what_mentions_it() {
    // The replay path: `caller` is user code and `/std/Async/bind` is a prelude definition this module does not contain. Without the inherited verdict the walk sees an unresolvable name and calls `caller` total, which is the hole that would let a user proof mention a divergent prelude function.
    let mut context = Context::with_default_budget(crate::SYNTAX);
    let module = module(vec![mentioning("caller", "prelude_partial")]);

    let inherited = BTreeMap::from([(name("prelude_partial"), Totality::Partial)]);
    let classified = classify_module(&mut context, &module, &inherited);
    assert_eq!(classified[&name("caller")], Totality::Partial);

    // And the verdict is inherited, not assumed: the same module against a total prelude name stays total.
    let inherited = BTreeMap::from([(name("prelude_partial"), Totality::Total)]);
    let classified = classify_module(&mut context, &module, &inherited);
    assert_eq!(classified[&name("caller")], Totality::Total);
}

#[test]
fn inherited_partiality_propagates_through_a_local_chain() {
    // `first → second → outside`. The taint has to cross the module boundary once and then travel the local closure, which is the fixpoint doing work a single ordered pass would miss.
    let mut context = Context::with_default_budget(crate::SYNTAX);
    let module = module(vec![
        mentioning("first", "second"),
        mentioning("second", "outside"),
    ]);

    let inherited = BTreeMap::from([(name("outside"), Totality::Partial)]);
    let classified = classify_module(&mut context, &module, &inherited);
    assert_eq!(classified[&name("first")], Totality::Partial);
    assert_eq!(classified[&name("second")], Totality::Partial);
}

#[test]
fn stamping_a_module_is_what_the_next_compilation_reads_back() {
    // `record_totality` writes the flag and `recorded_totality` reads it: the round trip the archive relies on to hand a user program the prelude's verdicts without re-analyzing `/std`.
    let mut context = Context::with_default_budget(crate::SYNTAX);
    let mut module = module(vec![mentioning("caller", "prelude_partial")]);
    let inherited = BTreeMap::from([(name("prelude_partial"), Totality::Partial)]);

    // Unstamped reads as partial, which is the fail-closed default rather than a verdict. Stamping against a clean prelude is what makes it total, so the flip is evidence the write landed.
    assert_eq!(
        recorded_totality(&module)[&name("caller")],
        Totality::Partial
    );
    record_totality(&mut context, &mut module, &BTreeMap::new());
    assert_eq!(recorded_totality(&module)[&name("caller")], Totality::Total);

    record_totality(&mut context, &mut module, &inherited);
    assert_eq!(
        recorded_totality(&module)[&name("caller")],
        Totality::Partial
    );
}
