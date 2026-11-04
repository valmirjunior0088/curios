use {
    super::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{DefinitionKind, Intrinsic, UniverseContext, Universes},
};

/// A qualified top-level name, from the path a test writes. Fixture-only.
fn name(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A `let` whose body is a bare reference to `mentions`. Fixture-only: the closure reads free variables, so one reference is the whole signal.
fn mentioning(path: &str, mentions: &str) -> Item {
    Item::Let(Box::new(Definition {
        name: name(path),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        proof: false,
        value_sort: None,
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::instance(name(mentions), Universes::default()),
    }))
}

fn definitions(items: Vec<Item>) -> Definitions {
    Definitions {
        items: items.into(),
    }
}

/// The totality `context` concluded of `item`'s one definition.
fn classified(context: &Context, item: &Item) -> Totality {
    context.classified(&item.definitions()[0].name).totality
}

#[test]
fn a_partial_name_from_another_unit_still_taints_what_mentions_it() {
    // The replay path: `caller` is user code and `prelude_partial` a partial prelude definition this unit does not contain, as a parser of `/std/Json/decode` is. Without the recorded verdict the classification sees a name with none and calls `caller` total, which is the hole that would let a user proof mention a divergent prelude function.
    let mut context = Context::with_default_budget(SYNTAX);
    context.record_definition_totality(&name("prelude_partial"), Totality::Partial);
    let caller = mentioning("caller", "prelude_partial");
    classify_totality(&mut context, &caller).expect("classified");
    assert_eq!(classified(&context, &caller), Totality::Partial);

    // And the verdict is read, not assumed: the same item against a total prelude name is total.
    let mut context = Context::with_default_budget(SYNTAX);
    context.record_definition_totality(&name("prelude_partial"), Totality::Total);
    let caller = mentioning("caller", "prelude_partial");
    classify_totality(&mut context, &caller).expect("classified");
    assert_eq!(classified(&context, &caller), Totality::Total);
}

#[test]
fn partiality_crosses_a_chain_of_declarations_one_verdict_at_a_time() {
    // `first → second → outside`. Each declaration is classified as it is published, after what it reads, and a verdict is a closure: `second` records that it reaches something partial, and `first` reads that of `second` without walking into it.
    let mut context = Context::with_default_budget(SYNTAX);
    context.record_definition_totality(&name("outside"), Totality::Partial);
    let second = mentioning("second", "outside");
    let first = mentioning("first", "second");

    classify_totality(&mut context, &second).expect("classified");
    classify_totality(&mut context, &first).expect("classified");

    assert_eq!(classified(&context, &second), Totality::Partial);
    assert_eq!(classified(&context, &first), Totality::Partial);
}

#[test]
fn a_filed_verdict_is_what_a_later_unit_reads_back() {
    // `classify_totality` records the verdict and the unit's traces file it, and a unit in scope answers it from there: the round trip the archive relies on to hand a user program the prelude's verdicts without re-analyzing `/std`.
    let mut context = Context::with_default_budget(SYNTAX);
    let caller = mentioning("caller", "prelude_total");
    let module = definitions(vec![caller.clone()]);
    let mut traces = Traces::default();
    let read = |traces: &Traces| {
        Established::over(&[(&module, &[], traces)])
            .definition(&name("caller"))
            .expect("the unit declares it")
            .totality
    };

    // Unfiled reads as partial, which is the fail-closed default rather than a verdict. Classifying against a clean prelude is what makes it total, so the flip is evidence the verdict was filed.
    assert_eq!(read(&traces), Totality::Partial);
    classify_totality(&mut context, &caller).expect("classified");
    traces.classify(name("caller"), context.classified(&name("caller")));
    assert_eq!(read(&traces), Totality::Total);
}
