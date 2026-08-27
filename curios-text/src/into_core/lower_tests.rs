//! Goals, `!`, `choose`, match compilation, and the packed runs constant atoms fold into.

use curios_elab::TermBuilders;

use super::test_support::*;

#[test]
fn goal_lowers_to_marked_metavar() {
    // A written `?` lowers to the same fresh metavariable a desugared hole does, but marked `MetavarOrigin::Goal` so zonk reports it.
    assert_eq!(run("?"), curios_core::Term::goal(0));
}

#[test]
fn distinct_goals_get_distinct_ids() {
    // Two goals in one program draw distinct, monotonic ids from the shared counter.
    let term = run("(?, ?)");
    assert_eq!(
        term,
        curios_core::Term::tuple([curios_core::Term::goal(0), curios_core::Term::goal(1)]),
    );
}

#[test]
fn bang_lowers_to_the_bang_transient() {
    // Every value body is a region root: `x!` hoists to it as a `Bang` transient holding the action and the continuation over a gensym'd binder. `elaborate_bang` later replaces the node with the `/syn/Monad/bind` application, inserting the witness slot and implicits during core elaboration.
    let expected = curios_core::Term::bang(
        // `x` resolves to nothing, so it lowers to a binder identity that core will report as unbound — never to a global that a same-named root-level definition could satisfy.
        curios_core::Term::var(curios_core::Var::free(curios_core::Free::local(
            0,
            Some("x"),
        ))),
        curios_core::Term::func(
            [(
                curios_core::Free::local(1, None),
                curios_core::Term::hole(0),
            )],
            curios_core::Term::var(curios_core::Var::free(curios_core::Free::local(1, None))),
        ),
    );
    assert_eq!(run("x!"), expected);
}

#[test]
fn choose_lowers_to_nested_bool_matches() {
    // `choose | p => a | q => b | _ => ? end` right-folds into two nested `Bool` matches: the first condition's false branch holds the second, whose own false branch is the `_` default (a plain hole here).
    let term = run("choose | p => a | q => b | _ => ? end");

    let curios_core::Subterm::Match(outer) = &*term else {
        panic!("expected a Match at the top, got {term:?}");
    };
    let curios_core::Cases::Bool { false_case, .. } = &outer.cases else {
        panic!("expected the outer Cases::Bool, got {:?}", outer.cases);
    };
    let curios_core::Subterm::Match(inner) = &**false_case else {
        panic!("expected a nested Match in the outer false branch, got {false_case:?}");
    };
    let curios_core::Cases::Bool {
        false_case: inner_false,
        ..
    } = &inner.cases
    else {
        panic!("expected the inner Cases::Bool, got {:?}", inner.cases);
    };
    assert!(
        matches!(&**inner_false, curios_core::Subterm::Metavar(_)),
        "the `_` default should sit at the innermost false branch, got {inner_false:?}"
    );
}

#[test]
fn bind_arm_bare_binder_is_rejected() {
    // `| x = n =>` binds irrefutably — always fires, so the rest of the ladder is dead. Rejected in favor of a `let`.
    let error = run_err("choose | x = n => x | _ => 0 end");
    assert!(
        error.contains("refutable") && error.contains("let"),
        "unexpected error: {error}"
    );
}

#[test]
fn named_catch_all_is_rejected() {
    // A named final arm among concrete constructor arms is not a catch-all.
    let error = run_err("match m | some(x) => x | rest => 0 end");
    assert!(
        error.contains("named final arm") && error.contains("_"),
        "unexpected error: {error}"
    );
}

#[test]
fn nested_underscore_mixed_with_concrete_stays_inconsistent_shape() {
    // A `_` *nested* inside a constructor payload (not a final top-level arm) still mixes a binder with a concrete shape in the same column — the pre-existing full-enumeration boundary, not a catch-all.
    let error = run_err("match m | some(some(x)) => x | some(_) => 0 | none() => 1 end");
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

#[test]
fn nested_nat_literal_lowers_to_switch() {
    // A literal `5` inside a constructor payload, with a `_` fallthrough, is value dispatch — it lowers through `compile_nat`'s switch mode to a `Cases::Switch`, not the `Nat` eliminator. (`wrap`/`b` need not resolve: lowering precedes name resolution.)
    let term = run("match b | wrap(5) => 1 | _ => 0 end");
    assert!(
        format!("{term:?}").contains("Switch"),
        "expected a Cases::Switch, got {term:?}"
    );
}

#[test]
fn nat_literal_mixed_with_succ_is_rejected() {
    // A literal case and a `n + 1; ih` successor arm in the same `Nat` column select incompatible core forms (a value `switch` vs. the eliminator).
    let error = run_err("match b | wrap(5) => 1 | wrap(n + 1; ih) => n | _ => 0 end");
    assert!(
        error.contains("mixes successor-peeling"),
        "unexpected error: {error}"
    );
}

#[test]
fn constant_atoms_fold_into_the_packed_run() {
    // A constant atom folds into the neighbouring run whether it is spelled as a numeral or as a `true`/`false` literal, so the literal stays one `Intrinsic::Bin` rather than an append onto one. Conversion equates the spellings either way — `core::spine` decodes a concrete appended atom as a length-1 literal run — so this pins the compaction, not the meaning. (Names need not resolve: lowering precedes name resolution.)
    assert_eq!(run(r"x[0x48, 0x69]"), run(r"x[72, 105]"));
    assert_eq!(run(r"b[1, false]"), run(r"b[1, 0]"));
    assert!(!format!("{:?}", run(r"x[0x48, 0x69]")).contains("BinAppend"));

    // A symbolic atom cannot fold, and keeps the append the fold exists to avoid.
    assert!(format!("{:?}", run(r"x[0x48, b]")).contains("BinAppend"));

    // Past `Byte` range — or past a bit, or carrying a written sign — an entry stays an atom: elaboration reports it against the expected element type instead of the fold silently truncating it.
    assert!(format!("{:?}", run(r"x[300]")).contains("BinAppend"));
    assert!(format!("{:?}", run(r"x[+1]")).contains("BinAppend"));
    assert!(format!("{:?}", run(r"b[2]")).contains("BinAppend"));
}

#[test]
fn a_lone_non_sequence_spread_keeps_its_concat_wrapper() {
    // `[..true]` once collapsed to the bare operand: the literal lowered to `true` and typechecked as `Bool`, never having been a list at all. Only the family the literal itself builds may collapse; anything else keeps its `ListConcat`/`BinConcat` wrapper so elaboration checks the spread against the sequence type — grain included, since a bits value spread into a bytes literal is not a bytes value. (Names need not resolve: lowering precedes name resolution.)
    assert!(format!("{:?}", run("[..true]")).contains("ListConcat"));
    assert!(format!("{:?}", run(r"x[..true]")).contains("BinConcat"));
    assert!(format!("{:?}", run(r"x[..b[1]]")).contains("BinConcat"));

    // A lone sequence-shaped spread still collapses to the value it already is.
    assert_eq!(run("[..[1, 2]]"), run("[1, 2]"));
    assert!(!format!("{:?}", run(r"x[..x[0x48]]")).contains("BinConcat"));
}

#[test]
fn bang_in_a_type_is_rejected() {
    // Types have no region to hoist an action to, so a `!` in an annotation is rejected during desugaring.
    assert!(run_err("let a : e! = x; a").contains("not allowed inside a type"));
}
