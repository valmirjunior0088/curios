//! What one run reports when an item is refused: every independent refusal in item order, nothing from an item that reaches a refused one, and a context with nothing of the refused item left in it.

use {
    crate::*,
    curios_core::*,
    curios_utilities::Qualifier,
    std::collections::{BTreeMap, BTreeSet},
};

fn context() -> Context {
    Context::with_default_budget(crate::SYNTAX)
}

fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn nat_lit(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

fn boolean(value: bool) -> Term {
    Term::intrinsic(Intrinsic::Bool(value))
}

fn mention(path: &str) -> Term {
    Term::free_var(&Free::from(&nominal(path)))
}

fn let_item(path: &str, type_: Term, body: Term) -> Item {
    Item::Let(Definition {
        name: nominal(path),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        type_,
        body,
    })
}

fn module(items: Vec<Item>) -> Module {
    Module {
        items,
        mounts: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 0,
        entry: None,
    }
}

/// Every refusal one elaboration reports, rendered.
fn refusals(context: &mut Context, module: &Module) -> Vec<String> {
    match elaborate_and_zonk_module(context, module, 0, 0, Mode::Infer) {
        Ok(_) => Vec::new(),
        Err(error) => error.each().map(Error::to_string).collect(),
    }
}

#[test]
fn two_independent_refusals_are_both_reported_in_item_order() {
    let module = module(vec![
        let_item("a", nat(), boolean(true)),
        let_item("b", nat(), nat_lit(1)),
        let_item("c", nat(), boolean(false)),
    ]);

    let reports = refusals(&mut context(), &module);

    assert_eq!(reports.len(), 2, "{reports:?}");
    assert!(
        reports[0].starts_with("while elaborating /a:"),
        "{reports:?}"
    );
    assert!(
        reports[1].starts_with("while elaborating /c:"),
        "{reports:?}"
    );
}

#[test]
fn a_dependent_of_a_refused_item_is_withheld_and_reports_nothing() {
    let module = module(vec![
        let_item("a", nat(), boolean(true)),
        let_item("b", nat(), mention("a")),
        let_item("c", nat(), mention("b")),
        let_item("d", nat(), nat_lit(1)),
    ]);

    let reports = refusals(&mut context(), &module);

    assert_eq!(reports.len(), 1, "{reports:?}");
    assert!(
        reports[0].starts_with("while elaborating /a:"),
        "{reports:?}"
    );
}

/// A name the lowering reports broken is poisoned before the first item: its own declaration and every dependent are withheld, and the survivors elaborate as if neither had been written.
#[test]
fn a_name_reported_broken_before_elaboration_withholds_its_dependents_from_the_start() {
    let mut context = context();
    context.set_broken(BTreeSet::from([nominal("a")]));
    let module = module(vec![
        let_item("a", nat(), nat_lit(1)),
        let_item("b", nat(), mention("a")),
        let_item("c", nat(), nat_lit(2)),
    ]);

    let (elaborated, _) = elaborate_and_zonk_module(&mut context, &module, 0, 0, Mode::Infer)
        .expect("nothing was refused");

    assert_eq!(
        elaborated
            .items
            .iter()
            .map(Item::describe)
            .collect::<Vec<_>>(),
        ["/c"]
    );
}

/// A withheld item's registry entry leaves the module with it — an entry whose declaring item is absent would reach zonk and erasure in its lowered form.
#[test]
fn a_withheld_declaring_item_takes_its_registry_entry_out_of_the_module() {
    let mut context = context();
    context.set_broken(BTreeSet::from([nominal("T")]));
    let mut module = module(vec![let_item(
        "T",
        Term::type_ground(),
        Term::induct_type(nominal("T"), Vec::<Term>::new(), Vec::<Term>::new()),
    )]);
    module.induct_decls.insert(
        nominal("T"),
        InductDecl {
            universe_context: UniverseContext::empty(),
            arity: Telescope::Done(Box::new(Telescope::Done(Box::new(())))),
            constructors: Vec::new(),
            result_sort: Term::type_ground(),
            module: Qualifier::empty(),
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    let (elaborated, _) = elaborate_and_zonk_module(&mut context, &module, 0, 0, Mode::Infer)
        .expect("nothing was refused");

    assert!(elaborated.items.is_empty());
    assert!(elaborated.induct_decls.is_empty());
    assert!(context.induct_decl(&nominal("T")).is_none());
}

#[test]
fn a_refused_item_leaves_no_binding_parked_work_deferred_goal_or_constraint_behind() {
    let mut context = context();
    let module = module(vec![let_item("a", nat(), boolean(true))]);

    elaborate_and_zonk_module(&mut context, &module, 0, 0, Mode::Infer).expect_err("refused");

    assert!(context.assumption(&Free::from(&nominal("a"))).is_none());
    assert_eq!(context.parked_len(), 0);
    assert!(context.take_deferred_witnesses().is_empty());
    assert_eq!(context.universes().constraint_count(), 0);
}
