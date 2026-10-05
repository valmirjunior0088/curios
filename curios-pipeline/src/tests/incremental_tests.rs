//! One unit compiled over a baseline: what the closure covers, what is reused untouched, and that the result agrees with a whole compile of the same text — its lints included, which a reused item's credits are carried into.
//!
//! Reuse is observed by allocation identity — a reused item carries the very terms the baseline holds, which no elaboration could produce twice — and agreement by the differential predicate in `test_support`. Resource verdicts are deliberately outside the predicate: a partial walk runs in a different cache state, so a budget-marginal declaration can move either way, as `documentation/design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md` states.

use {
    super::test_support::{
        assert_modules_agree, assert_stored_alike, carried_variances, compile_modules,
        recompile_modules, recompile_over, reuses_body, unit_of, unused_binders, written,
    },
    curios_core::Variance,
};

/// Three items: `twice` reaches `double`, and `unrelated` reaches neither.
const BASE: &str = "use /std/{Nat};

pub let double(n: Nat) -> Nat = n + n;

pub let twice: Nat = double(2);

pub let unrelated: Nat = 7;
";

#[test]
fn an_unchanged_text_reuses_every_item() {
    let baseline = unit_of(BASE);

    let again = recompile_over(BASE, &baseline).unwrap();

    for name in ["double", "twice", "unrelated"] {
        assert!(
            reuses_body(&baseline, &again, name),
            "{name} was re-elaborated"
        );
    }
    assert_modules_agree(baseline.core(), again.core());
    assert_stored_alike(&baseline, &again);
}

#[test]
fn an_edited_body_recompiles_the_item_and_its_dependents_and_agrees_with_the_whole_compile() {
    let edited = BASE.replace("n + n", "n + n + 0");
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "double"));
    assert!(
        !reuses_body(&baseline, &incremental, "twice"),
        "a dependent of a changed item is re-elaborated"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// A renamed parameter is a change, to its item and to every item that reaches it: a binder's name is in what is stored, the item's own and a dependent's that was handed its signature.
///
/// Mutation-checked: with the diff reading terms up to their binders' names, nothing is elaborated again and the unit is stored under the old ones.
#[test]
fn a_renamed_parameter_is_elaborated_again_with_its_dependents() {
    let edited = BASE.replace(
        "double(n: Nat) -> Nat = n + n",
        "double(m: Nat) -> Nat = m + m",
    );
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "double"));
    assert!(
        !reuses_body(&baseline, &incremental, "twice"),
        "a dependent of a renamed item is re-elaborated"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// A signature handed to a dependent carries its binders' names into the dependent's own term — `same(f)` is elaborated to `same(@(base: Nat, exp: Nat) -> Nat, f)` — so the dependent is stored anew when they are renamed.
#[test]
fn a_renamed_parameter_reaches_the_dependent_that_was_handed_its_signature() {
    let base = "use /std/{Nat};

pub let f(base: Nat, exp: Nat) -> Nat = base;

pub let same(@A: Type, x: A) -> A = x;

pub let g(n: Nat) -> Nat = same(f)(n, n);
";
    let edited = base.replace(
        "f(base: Nat, exp: Nat) -> Nat = base",
        "f(b: Nat, e: Nat) -> Nat = b",
    );
    let baseline = unit_of(base);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "g"));
    assert_stored_alike(&unit_of(&edited), &incremental);
}

#[test]
fn a_removed_item_is_gone_and_the_rest_is_reused() {
    let edited = BASE.replace("pub let unrelated: Nat = 7;\n", "");
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert_eq!(incremental.core().items.len(), 2);
    assert!(reuses_body(&baseline, &incremental, "double"));
    assert!(reuses_body(&baseline, &incremental, "twice"));
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
    assert_stored_alike(&unit_of(&edited), &incremental);
    assert!(
        incremental.certification() == unit_of(&edited).certification(),
        "the record keeps an entry for a declaration the text no longer holds"
    );
}

/// A declaration renamed is one removed and one added: the record a recompile joins holds an entry under the new name and none under the old.
#[test]
fn a_renamed_item_leaves_no_entry_under_its_old_name() {
    let edited = BASE.replace("pub let unrelated: Nat = 7;", "pub let apart: Nat = 7;");
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(reuses_body(&baseline, &incremental, "double"));
    assert!(
        incremental.certification() == unit_of(&edited).certification(),
        "the record differs from a whole compile's"
    );
}

/// A moved declaration changes the lowering's order and nothing about any item, so everything is reused and only reassembled.
#[test]
fn an_item_moved_past_another_reuses_every_item() {
    let moved = "use /std/{Nat};

pub let unrelated: Nat = 7;

pub let double(n: Nat) -> Nat = n + n;

pub let twice: Nat = double(2);
";
    let baseline = unit_of(BASE);

    let incremental = recompile_over(moved, &baseline).unwrap();

    for name in ["double", "twice", "unrelated"] {
        assert!(
            reuses_body(&baseline, &incremental, name),
            "{name} was re-elaborated"
        );
    }
    assert_modules_agree(unit_of(moved).core(), incremental.core());
    assert_stored_alike(&unit_of(moved), &incremental);
}

/// A lowering numbers its universe metavariables and holes in lowering order, so an item inserted ahead renumbers every polymorphic item after it; the diff identifies them by position and leaves those items reused.
#[test]
fn inserting_a_polymorphic_item_leaves_the_items_after_it_unchanged() {
    let base = "use /std/{Nat};

pub let apply(@A: Type, f: (A) -> A, a: A) -> A = f(a);

pub let double(n: Nat) -> Nat = n + n;

pub let twice: Nat = apply(double, 2);
";
    let inserted = base.replace(
        "pub let apply",
        "pub let id(@A: Type, a: A) -> A = a;\n\npub let apply",
    );
    let baseline = unit_of(base);

    let incremental = recompile_over(&inserted, &baseline).unwrap();

    for name in ["apply", "double", "twice"] {
        assert!(
            reuses_body(&baseline, &incremental, name),
            "{name} was re-elaborated"
        );
    }
    assert_eq!(incremental.core().items.len(), 4);
    assert_modules_agree(unit_of(&inserted).core(), incremental.core());
}

/// A struct literal names its declaration in the registry rather than the variable graph, so an item building one depends on the declaration through the reach edge `mentions` cannot see — and a changed registry entry marks its declaring item changed.
#[test]
fn an_edited_struct_recompiles_the_items_that_construct_it() {
    let base = "use /std/{Nat};

pub struct Box: pub Type { value: Nat }

pub let unbox(b: Box) -> Nat = b.value;

pub let value_of_boxed: Nat = (Box { value = 1 }).value;

pub let unrelated: Nat = 7;
";
    let sealed = base.replace("Box: pub Type", "Box: Type");
    let baseline = unit_of(base);

    let incremental = recompile_over(&sealed, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "unbox"));
    assert!(
        !reuses_body(&baseline, &incremental, "value_of_boxed"),
        "an item constructing the struct is in the closure"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&sealed).core(), incremental.core());
}

/// A field's mark is part of its declaration: hiding a field changes what a literal of the structure writes and what a position counts. An edit that moves the mark and nothing else therefore recompiles the items that read the structure, against the declaration as it now stands.
#[test]
fn an_edit_that_moves_a_fields_mark_alone_recompiles_the_items_that_read_the_struct() {
    let base = "use /std/{Nat};
use /std/Bool/{Holds};

pub struct Positive: pub Type { n: Nat, ok: Holds(0 < n) }

pub let n_of(p: Positive) -> Nat = p.n;

pub let unrelated: Nat = 7;
";
    let hidden = base.replace("ok: Holds", "@ok: Holds");
    let baseline = unit_of(base);

    let incremental = recompile_over(&hidden, &baseline).unwrap();

    assert!(
        !reuses_body(&baseline, &incremental, "n_of"),
        "an item reading the struct is in the closure"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&hidden).core(), incremental.core());
}

// A family's variance composes through the families it holds, so an edit that turns one of their levels invariant moves the vector of every family that reaches it. Such a family mentions the edited declaration, so it is elaborated again with the items that read its entry, and the recompile carries the vector a whole compile does.
#[test]
fn an_edit_that_moves_a_familys_variance_recompiles_the_families_that_hold_it() {
    let base = "use /std/{Nat, Eq};

pub induct Wrap(A: Type): pub Type
| wrap(a: A)
end

pub induct Held(A: Type): pub Type
| held(w: Wrap(A))
end

pub let reader(@A: Type, h: Held(A)) -> Held(A) = h;

pub let unrelated: Nat = 7;
";
    // `Eq()(A, A)` holds `A` as a value of its own `Type`, so `A`'s level reaches a `Type` and is invariant in `Wrap`.
    let pinned = base.replace("| wrap(a: A)", "| wrap(a: A, same: Eq()(A, A))");
    let baseline = unit_of(base);

    let incremental = recompile_over(&pinned, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "reader"));
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_eq!(carried_variances(&baseline, "Held"), [Variance::Irrelevant]);
    assert_eq!(
        carried_variances(&incremental, "Held"),
        [Variance::Invariant]
    );
    assert_modules_agree(unit_of(&pinned).core(), incremental.core());
}

/// A baseline from unrelated text invalidates everything, and the recompile is then a whole compile by another route.
#[test]
fn an_all_changed_closure_equals_the_whole_compile() {
    let baseline = unit_of("use /std/{Nat};\n\npub let other: Nat = 1;\n");

    let incremental = recompile_over(BASE, &baseline).unwrap();

    assert_eq!(incremental.core().items.len(), 3);
    assert_modules_agree(unit_of(BASE).core(), incremental.core());
    assert_stored_alike(&unit_of(BASE), &incremental);
}

/// An item the parser could not read withholds its dependents, which report nothing and leave no refusal behind them — so a recompile reassembling the lowered order has items in it that the closure's elaboration never produced. It leaves them out, exactly as a whole compile of the same text does, and answers with the parse error either way.
///
/// This is where the two features cross: recovery lets a withheld item fall out of a whole unit's module, and the recompile reassembles a run that either kept everything or refused something.
#[test]
fn a_broken_item_withholds_its_dependents_and_reports_as_the_whole_compile_does() {
    let baseline = unit_of(BASE);
    let (_guard, edited) = written("lib", &BASE.replace("n + n", ""));

    let Err(incremental) = recompile_modules(&edited, &baseline) else {
        panic!("a unit holding a broken item does not compile");
    };
    let Err(whole) = compile_modules(&edited) else {
        panic!("a unit holding a broken item does not compile");
    };

    assert!(
        incremental.contains("expected a term"),
        "the parse error is the answer: {incremental}"
    );
    assert_eq!(
        incremental, whole,
        "a recompile answers what the whole compile of the same text answers"
    );
}

/// A hypothesis only a proof the elaborator writes reads, in `below`, beside one nothing reads, in `idle`.
const CREDITED: &str = "use /std/{Nat};
use /std/Bool/{True, Holds};

pub let below(i: Nat, n: Nat, p: Holds(i < n)) -> Holds(i <= n) = True/proved();

pub let idle(i: Nat, q: Holds(i < 3)) -> Nat = i;

pub let unrelated: Nat = 7;
";

#[test]
fn a_binder_a_reused_items_proof_reads_stays_credited_as_in_a_whole_compile() {
    let baseline = unit_of(CREDITED);
    assert_eq!(
        unused_binders(&baseline),
        ["unused binder `q`; name it `_q` to keep it"],
        "`p` is read by the proof `proved` stands for, and `q` by nothing"
    );

    let edited = CREDITED.replace("= 7", "= 8");
    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(reuses_body(&baseline, &incremental, "below"));
    assert_eq!(
        unused_binders(&incremental),
        unused_binders(&unit_of(&edited))
    );
}

/// A hypothesis a proof reads in `below`, and one in `above` under it, whose statement reaches `limit`.
const CREDITED_TWICE: &str = "use /std/{Nat};
use /std/Bool/{True, Holds};

pub let limit: Nat = 7;

pub let below(i: Nat, n: Nat, p: Holds(i < n)) -> Holds(i <= n) = True/proved();

pub let above(i: Nat, q: Holds(limit < i)) -> Holds(limit <= i) = True/proved();
";

/// A unit's credited binders are stored alike whichever pass credited them.
///
/// A recompile credits the closure's binders and then the ones the baseline's proofs read in what it reused, where a whole compile credits in reading order: `above` is elaborated again and `below`, over it in the text, is reused, so the two passes run in the other order than the text. Mutation-checked: kept as a list, the two units' text parts differ.
#[test]
fn credited_binders_are_stored_alike_whichever_pass_credited_them() {
    let baseline = unit_of(CREDITED_TWICE);
    let edited = CREDITED_TWICE.replace("= 7", "= 8");

    let incremental = recompile_over(&edited, &baseline).unwrap();
    let whole = unit_of(&edited);

    assert!(reuses_body(&baseline, &incremental, "below"));
    assert!(!reuses_body(&baseline, &incremental, "above"));
    assert!(unused_binders(&incremental).is_empty());
    assert!(
        curios_archive::to_bytes(incremental.text()).unwrap()[..]
            == curios_archive::to_bytes(whole.text()).unwrap()[..],
        "the text stage's part is stored differently over a baseline"
    );
}
