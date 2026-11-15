//! Goals, `!`, a match as the matrix it is written as, its guards, and the packed runs constant atoms fold into.

use crate::into_core::test_support::*;

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
fn a_bind_guard_that_cannot_fail_is_rejected() {
    // `| x = n =>` binds irrefutably — its arm takes every value, so the arms after it are dead. Rejected in favor of a `let`.
    let error = run_err("match | x = n => x | _ => 0 end");
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
    // A match is handed on as its arms, each a pattern and its body as a function over the names the pattern binds, in the order it writes them, a `_` binding none: elaboration compiles the patterns, where the scrutinee's type is known. A binder beside a concrete pattern is nothing lowering refuses. (Names need not resolve: lowering precedes name resolution.)
    let term = run("match m | some(some(x)) => x | some(_) => 0 | none() => 1 end");

    let curios_core::Subterm::Transient(curios_core::Transient::Matrix(matrix)) = &*term else {
        panic!("expected a matrix at the top, got {term:?}");
    };
    assert_eq!(
        matrix
            .rows()
            .map(|row| row.pattern.binders())
            .collect::<Vec<_>>(),
        [1, 0, 0],
    );
    for row in matrix.rows() {
        let curios_core::Subterm::Func(body) = &*row.body else {
            panic!("expected a row's body to be a function, got {:?}", row.body);
        };
        assert_eq!(body.telescope.len(), row.pattern.binders());
    }
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

/// The match lowering's refusals stand at the binder they are about: a named final arm, and an irrefutable bind guard.
#[test]
fn a_match_refusal_stands_at_the_binder_it_is_about() {
    for (source, underlined) in [
        ("match m | some(x) => x | rest => 0 end", "rest"),
        ("match | some(x) = m => x | y = m => 0 | _ => 1 end", "y"),
    ] {
        let report = run_err_report(source);
        // The caret line under the source's one line: blanks to where the part begins, then a caret for each of its characters.
        let at = source.find(underlined).expect("the part is written");
        let carets = format!("│ {}{}", " ".repeat(at), "^".repeat(underlined.len()));
        assert!(
            report.lines().any(|line| line.ends_with(&carets)),
            "{source}\n{report}"
        );
    }
}

#[test]
fn an_arm_lowers_to_a_row_for_each_alternative() {
    // Each alternative is a row of its own arm, the body lowered again under the names it binds, so each is typed apart: `x` alone, the `_` beside it binding nothing.
    let term = run("match m | left(x, _) | right(_, x) => x | none() => 0 end");

    let curios_core::Subterm::Transient(curios_core::Transient::Matrix(matrix)) = &*term else {
        panic!("expected a matrix at the top, got {term:?}");
    };
    assert_eq!(
        matrix
            .arms
            .iter()
            .map(|arm| arm.alternatives.len())
            .collect::<Vec<_>>(),
        [2, 1]
    );
    let binders = matrix.arms[0]
        .alternatives
        .iter()
        .map(|row| {
            let curios_core::Subterm::Func(body) = &*row.body else {
                panic!("expected a row's body to be a function, got {:?}", row.body);
            };
            assert_eq!(body.telescope.len(), row.pattern.binders());
            body.telescope.len()
        })
        .collect::<Vec<_>>();
    assert_eq!(binders, [1, 1]);
}

#[test]
fn alternatives_that_bind_different_names_are_refused() {
    // The body reads each name the arm binds in every alternative, and one an alternative lacks would read whatever else it names there.
    let error = run_err("match m | some(x) | none() => x | _ => 0 end");
    assert!(
        error.contains("`x` is bound by another alternative of this arm and not by this one"),
        "unexpected error: {error}"
    );
}

#[test]
fn a_guard_lowers_to_a_function_over_the_names_written_before_it() {
    // Each guard is a function over the names the pattern and the guards before it bind, and the body one over all of them: a bind guard adds its pattern's names for what follows it.
    let term = run("match m | some(x), x > 1, some(y) = f(x), y > x => y | _ => 0 end");

    let curios_core::Subterm::Transient(curios_core::Transient::Matrix(matrix)) = &*term else {
        panic!("expected a matrix at the top, got {term:?}");
    };
    let row = &matrix.arms[0].alternatives[0];
    let telescope = |function: &curios_core::Term| {
        let curios_core::Subterm::Func(function) = &**function else {
            panic!("expected a function, got {function:?}");
        };
        function.telescope.len()
    };
    assert_eq!(
        row.guards
            .iter()
            .map(|guard| telescope(guard.term()))
            .collect::<Vec<_>>(),
        [1, 1, 2],
    );
    assert!(matches!(row.guards[1], curios_core::Guard::Binds { .. }));
    assert_eq!(telescope(&row.body), row.binders());
    assert_eq!(row.binders(), 2);
}

#[test]
fn a_match_with_no_head_lowers_to_a_matrix_over_the_unit_value() {
    // A match with no head is the match of no columns: its head is `()`, and every row writes `_`, a wildcard, which binds nothing, its guards being all it tests.
    let term = run("match | p => a | some(x) = o => x | _ => 0 end");

    let curios_core::Subterm::Transient(curios_core::Transient::Matrix(matrix)) = &*term else {
        panic!("expected a matrix at the top, got {term:?}");
    };
    assert_eq!(
        matrix.head,
        curios_core::Term::tuple(Vec::<curios_core::Term>::new())
    );
    assert!(
        matrix
            .rows()
            .all(|row| row.pattern == curios_core::Pattern::Wildcard)
    );
    assert_eq!(
        matrix
            .rows()
            .map(|row| row.guards.len())
            .collect::<Vec<_>>(),
        [1, 1, 0],
    );
}
