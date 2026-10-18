//! Goals, `!`, `choose`, a match as the matrix it is written as, and the packed runs constant atoms fold into.

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
    // A written function body is a region root: `x!` hoists to it as a `Bang` transient holding the action and the continuation over a gensym'd binder. `elaborate_bang` later replaces the node with the `/std/Monad/bind` application, inserting the witness slot and implicits during core elaboration.
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
    let function = run("() => x!");
    let curios_core::Subterm::Func(function) = &*function else {
        panic!("expected a function");
    };
    assert_eq!(*function.telescope.terminal(), expected);
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
    // A named final arm among concrete constructor arms is not a catch-all, and the name may have been meant as a constructor, which is written with its payload list.
    let error = run_err("match m | some(x) => x | none => 0 end");
    assert!(
        error.contains("named final arm")
            && error.contains("`| _ =>`")
            && error.contains("`| none() =>`"),
        "unexpected error: {error}"
    );
}

#[test]
fn a_match_lowers_to_the_matrix_it_is_written_as() {
    // A match is handed on as its arms, each a pattern and its body as a function over the names the pattern binds, in the order it writes them: elaboration compiles the patterns, where the scrutinee's type is known. A binder beside a concrete pattern is nothing lowering refuses. (Names need not resolve: lowering precedes name resolution.)
    let term = run("match m | some(some(x)) => x | some(_) => 0 | none() => 1 end");

    let curios_core::Subterm::Transient(curios_core::Transient::Matrix(matrix)) = &*term else {
        panic!("expected a matrix at the top, got {term:?}");
    };
    assert!(matrix.motive.is_none());
    assert_eq!(
        matrix
            .rows
            .iter()
            .map(|row| row.pattern.binders())
            .collect::<Vec<_>>(),
        [1, 1, 0],
    );
    for row in &matrix.rows {
        let curios_core::Subterm::Func(body) = &*row.body else {
            panic!("expected a row's body to be a function, got {:?}", row.body);
        };
        assert_eq!(body.telescope.len(), row.pattern.binders());
    }
}

#[test]
fn a_bind_arm_lowers_to_the_two_rows_it_stands_for() {
    // `| pattern = value => body` is the matrix of two rows over `value`: its own, and a `_` that takes the rest of the ladder.
    let term = run("choose | some(x) = m => x | _ => 0 end");

    let curios_core::Subterm::Transient(curios_core::Transient::Matrix(matrix)) = &*term else {
        panic!("expected a matrix at the top, got {term:?}");
    };
    assert_eq!(matrix.rows.len(), 2);
    assert_eq!(matrix.rows[1].pattern, curios_core::Pattern::Binder);
}

#[test]
fn a_motive_on_a_head_that_does_not_dispatch_is_rejected() {
    // A motive attaches to the scrutinee's own dispatch, which a tuple pattern never builds: it projects.
    let error = run_err("match p : (q) => T | (x, y) => x end");
    assert!(
        error.contains("written motive"),
        "unexpected error: {error}"
    );
}

#[test]
fn constant_atoms_fold_into_the_packed_run() {
    // A constant atom folds into the neighbouring run whatever radix spells it, so the literal stays one `Intrinsic::Bytes` rather than an append onto one. Conversion equates the spellings either way — `core::spine` decodes a concrete appended atom as a length-1 literal run — so this pins the compaction, not the meaning. (Names need not resolve: lowering precedes name resolution.)
    assert_eq!(run(r"x[0x48, 0x69]"), run(r"x[72, 105]"));
    assert!(!format!("{:?}", run(r"x[0x48, 0x69]")).contains("BytesAppend"));

    // A symbolic atom cannot fold, and keeps the append the fold exists to avoid.
    assert!(format!("{:?}", run(r"x[0x48, b]")).contains("BytesAppend"));

    // Past `Byte` range, or carrying a written sign, an entry stays an atom: elaboration reports it against the expected element type instead of the fold silently truncating it.
    assert!(format!("{:?}", run(r"x[300]")).contains("BytesAppend"));
    assert!(format!("{:?}", run(r"x[+1]")).contains("BytesAppend"));
}

#[test]
fn a_lone_non_sequence_spread_keeps_its_concat_wrapper() {
    // `[..true]` must not collapse to the bare operand, which would lower the literal to `true` and typecheck it as `Bool`, never having been a list at all. Only the family the literal itself builds may collapse; anything else keeps its `ListConcat`/`BytesConcat` wrapper so elaboration checks the spread against the sequence type. (Names need not resolve: lowering precedes name resolution.)
    assert!(format!("{:?}", run("[..true]")).contains("ListConcat"));
    assert!(format!("{:?}", run(r"x[..true]")).contains("BytesConcat"));

    // A lone sequence-shaped spread still collapses to the value it already is.
    assert_eq!(run("[..[1, 2]]"), run("[1, 2]"));
    assert!(!format!("{:?}", run(r"x[..x[0x48]]")).contains("BytesConcat"));
}

/// The match lowering's refusals stand at what they are about: a named final arm and an irrefutable bind arm at the binder, and a motive no arm dispatches under at the motive.
#[test]
fn a_match_refusal_stands_at_the_binder_or_the_motive_it_is_about() {
    for (source, underlined) in [
        ("match m | some(x) => x | rest => 0 end", "rest"),
        ("choose | some(x) = m => x | y = m => 0 | _ => 1 end", "y"),
        (
            "match (a, b) : (p) => Nat | (0, _) => 0 | (_, k) => k end",
            "(p) => Nat",
        ),
    ] {
        let report = run_err_report(source);
        // The caret line under the source's one line: blanks to where the part begins, then a caret for each of its characters.
        let at = source.find(underlined).expect("the part is written");
        let carets = format!("| {}{}", " ".repeat(at), "^".repeat(underlined.len()));
        assert!(
            report.lines().any(|line| line.trim_start() == carets),
            "{source}\n{report}"
        );
    }
}
