//! One unit compiled over a baseline answers as a whole compile of the same text does: its refusals, its totality, and its lints, which a reused item's credits are carried into.
//!
//! Agreement is the differential predicate in `test_support`. Resource verdicts are deliberately outside it: a reused declaration stands on the verdict its baseline reached, under whatever budget that was, as `documentation/design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md` states.

use {
    super::test_support::{BASE, UNTAKEN},
    super::test_support::{
        assert_modules_agree, assert_stored_alike, compile_modules, mounted, recompile_modules,
        recompile_over, reuses_body, unit_of, unused_binders, written,
    },
    curios_unit::Unit,
};

/// A baseline from unrelated text keeps nothing, and the recompile is then a whole compile by another route.
#[test]
fn a_baseline_of_another_text_keeps_nothing_and_equals_the_whole_compile() {
    let baseline = unit_of("use /std/{Nat};\n\npub let other: Nat = 1;\n");

    let incremental = recompile_over(BASE, &baseline).unwrap();

    assert_eq!(incremental.core().items.len(), 3);
    assert_modules_agree(unit_of(BASE).core(), incremental.core());
    assert_stored_alike(&unit_of(BASE), &incremental);
}

/// An item the parser could not read withholds its dependents, which report nothing and leave no refusal behind them — so a unit compiled over a baseline holds declarations its walk made no item for. It leaves them out, exactly as a whole compile of the same text does, and answers with the parse error either way.
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
const CREDITED: &str = "use /std/{Nat, proved};
use /std/Bool/{Holds};

pub let below(i: Nat, n: Nat, p: Holds(i < n)) -> Holds(i <= n) = proved();

pub let idle(i: Nat, q: Holds(i < 3)) -> Nat = i;

pub let unrelated: Nat = 7;
";

#[test]
fn a_binder_a_reused_items_proof_reads_stays_credited_as_in_a_whole_compile() {
    let baseline = unit_of(CREDITED);
    assert_eq!(
        unused_binders(&baseline),
        ["unused binder `q`; write `_` in its name's place"],
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
const CREDITED_TWICE: &str = "use /std/{Nat, proved};
use /std/Bool/{Holds};

pub let limit: Nat = 7;

pub let below(i: Nat, n: Nat, p: Holds(i < n)) -> Holds(i <= n) = proved();

pub let above(i: Nat, q: Holds(limit < i)) -> Holds(limit <= i) = proved();
";

/// A unit's credited binders are stored alike whichever pass credited them.
///
/// A recompile credits the binders of what it elaborates again and then the ones the baseline's proofs read in what it reused, where a whole compile credits in reading order: `above` is elaborated again and `below`, over it in the text, is reused, so the two passes run in the other order than the text. Mutation-checked: kept as a list, the two units' text parts differ.
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

/// `edited` compiled over `baseline` answers what compiling it whole answers: the same unit, stored alike, or the same refusal.
fn answers_as_the_whole_compile_does(edited: &str, baseline: &Unit) {
    let whole = compile_modules(&mounted("lib", edited));
    let incremental = recompile_over(edited, baseline);

    match (whole, incremental) {
        (Ok(whole), Ok(incremental)) => {
            assert_modules_agree(whole.core(), incremental.core());
            assert_stored_alike(&whole, &incremental);
        }
        (Err(whole), Err(incremental)) => assert_eq!(
            incremental, whole,
            "a recompile refuses as the whole compile of the same text refuses"
        ),
        (whole, incremental) => panic!(
            "one compiles and the other does not: whole {:?}, over the baseline {:?}",
            whole.map(|_| ()),
            incremental.map(|_| ())
        ),
    }
}

/// A definition partial only through a name in an arm no reduction takes turns total with that name: `spin` made total leaves `f` total in a whole compile, and the recompile answers the same.
#[test]
fn a_definition_partial_only_through_an_untaken_arm_turns_total_with_it_as_the_whole_compile_does()
{
    let baseline = unit_of(UNTAKEN);

    answers_as_the_whole_compile_does(
        &UNTAKEN.replace("spin(n: Nat) -> Nat = spin(n);", "spin(n: Nat) -> Nat = n;"),
        &baseline,
    );
}

/// The converse: `spin` made partial makes `f` partial in a whole compile, and the recompile records it so.
#[test]
fn a_definition_total_through_an_untaken_arm_turns_partial_with_it_as_the_whole_compile_does() {
    let total = UNTAKEN.replace("spin(n: Nat) -> Nat = spin(n);", "spin(n: Nat) -> Nat = n;");
    let baseline = unit_of(&total);

    answers_as_the_whole_compile_does(UNTAKEN, &baseline);
}

/// A module a glob imports gains a member under a name the scope already holds: `y` wrote that name and no character of it changed, and what the recompile makes of it is what the whole compile does.
#[test]
fn a_globbed_module_gaining_a_member_is_answered_as_the_whole_compile_answers() {
    let base = "use /std/{Nat};

pub mod a
    pub let one: /std/Nat = 1;
end

pub mod b
    pub let two: /std/Nat = 2;
end

use /lib/b/{two};
use /lib/a/*;

pub let y: Nat = one + two;
";
    let edited = base.replace(
        "    pub let one: /std/Nat = 1;\n",
        "    pub let one: /std/Nat = 1;\n    pub let two: /std/Nat = 22;\n",
    );
    let baseline = unit_of(base);

    answers_as_the_whole_compile_does(&edited, &baseline);
}

/// An export moves from one module to another, and a declaration that wrote the path through the first is untouched: its lowering is not kept, and the refusal is the whole compile's.
#[test]
fn a_path_through_an_export_that_moved_is_refused_as_the_whole_compile_refuses_it() {
    let base = "pub mod a
    pub let x: /std/Nat = 1;
end

pub mod b
    pub use /lib/a/{x};

    pub let w: /std/Nat = 0;
end

pub mod c
    pub let v: /std/Nat = 0;
end

pub let y: /std/Nat = /lib/b/x;
";
    let edited = base.replace("    pub use /lib/a/{x};\n\n", "").replace(
        "    pub let v: /std/Nat = 0;\n",
        "    pub use /lib/a/{x};\n\n    pub let v: /std/Nat = 0;\n",
    );
    let baseline = unit_of(base);

    assert!(
        recompile_over(&edited, &baseline).is_err(),
        "a path through an export that is gone still resolved"
    );
    answers_as_the_whole_compile_does(&edited, &baseline);
}

/// An edit closes a cycle through a declaration its baseline trace read nothing of: `f` is made first, as nothing it read is marked, and asks for `g`, which asks for `f` again. The members are refused and `h`, which reads `g`, is withheld, as the whole compile refuses and withholds them.
#[test]
fn an_edit_closing_a_cycle_refuses_its_members_as_the_whole_compile_does() {
    let base = "use /std/{Nat};

pub let f(n: Nat) -> Nat = n;

pub let g(n: Nat) -> Nat = f(n);

pub let h: Nat = g(1);

pub let unrelated: Nat = 7;
";
    let edited = base.replace("f(n: Nat) -> Nat = n;", "f(n: Nat) -> Nat = g(n);");
    let baseline = unit_of(base);

    assert!(
        recompile_over(&edited, &baseline).is_err(),
        "a cycle the edit closed was compiled"
    );
    answers_as_the_whole_compile_does(&edited, &baseline);
}
