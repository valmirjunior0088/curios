use {
    super::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{DefinitionKind, Free, Intrinsic, UniverseContext},
    curios_utilities::Qualifier,
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
        totality: Totality::default(),
        variances: Vec::new(),
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::free_var(&Free::from(&name(mentions))),
    })
}

fn module(items: Vec<Item>) -> Module {
    Module {
        mounts: Vec::new(),
        items,
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// The totality `item`'s one definition is stamped with.
fn stamped(item: &Item) -> Totality {
    item.definitions()[0].totality
}

#[test]
fn a_partial_name_from_another_unit_still_taints_what_mentions_it() {
    // The replay path: `caller` is user code and `/std/Async/bind` is a prelude definition this unit does not contain. Without the recorded verdict the classification sees a name with none and calls `caller` total, which is the hole that would let a user proof mention a divergent prelude function.
    let mut context = Context::with_default_budget(SYNTAX);
    context.seed_totality(&BTreeMap::from([(
        name("prelude_partial"),
        Totality::Partial,
    )]));
    let mut caller = mentioning("caller", "prelude_partial");
    stamp_totality(&mut context, &mut caller).expect("classified");
    assert_eq!(stamped(&caller), Totality::Partial);

    // And the verdict is read, not assumed: the same item against a total prelude name is total.
    let mut context = Context::with_default_budget(SYNTAX);
    context.seed_totality(&BTreeMap::from([(
        name("prelude_partial"),
        Totality::Total,
    )]));
    let mut caller = mentioning("caller", "prelude_partial");
    stamp_totality(&mut context, &mut caller).expect("classified");
    assert_eq!(stamped(&caller), Totality::Total);
}

#[test]
fn partiality_crosses_a_chain_of_declarations_one_verdict_at_a_time() {
    // `first → second → outside`. Each declaration is classified as it is published, after what it reads, and a verdict is a closure: `second` records that it reaches something partial, and `first` reads that of `second` without walking into it.
    let mut context = Context::with_default_budget(SYNTAX);
    context.seed_totality(&BTreeMap::from([(name("outside"), Totality::Partial)]));
    let mut second = mentioning("second", "outside");
    let mut first = mentioning("first", "second");

    stamp_totality(&mut context, &mut second).expect("classified");
    stamp_totality(&mut context, &mut first).expect("classified");

    assert_eq!(stamped(&second), Totality::Partial);
    assert_eq!(stamped(&first), Totality::Partial);
}

#[test]
fn a_stamp_is_what_the_next_compilation_reads_back() {
    // `stamp_totality` writes the flag and `recorded_totality` reads it: the round trip the archive relies on to hand a user program the prelude's verdicts without re-analyzing `/std`.
    let mut context = Context::with_default_budget(SYNTAX);
    let mut caller = mentioning("caller", "prelude_total");

    // Unstamped reads as partial, which is the fail-closed default rather than a verdict. Stamping against a clean prelude is what makes it total, so the flip is evidence the write landed.
    assert_eq!(
        recorded_totality(&module(vec![caller.clone()]))[&name("caller")],
        Totality::Partial
    );
    stamp_totality(&mut context, &mut caller).expect("stamped");
    assert_eq!(
        recorded_totality(&module(vec![caller]))[&name("caller")],
        Totality::Total
    );
}
