//! A fold's arms learn nothing of its scrutinee: what an arm reads of it, and what a motive that names it states.

use {super::test_support::*, crate::tests::test_support::*, curios_elab::Error};

// The induction hypothesis of a fold is typed at the motive opened at the tail, and no arm of a fold refines the scrutinee. A motive that names the scrutinee instead of binding it therefore states one proposition at every tail, `Eq()(n, 0)` here, and the identity arm's `refl` does not inhabit it. Were the arms typed with the scrutinee at their case, every program below would compile, run, and print, certifying a closed inhabitant of `Eq()(1, 0)`. The control's identity arm inhabits what the motive states, from a hypothesis.
#[test]
fn a_fold_motive_that_names_its_scrutinee_states_it_at_every_tail() {
    control(
        r#"
        use /std/{Nat, Eq};

        let all_zero(n : Nat, h : Eq()(n, 0)) -> Eq()(n, 0) =
            match n : (_) => Eq()(n, 0)
            | 0 => h
            | k + 1; ih => ih
            end;

        let boom : Eq()(0, 0) = all_zero(0, Eq/refl());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("all_zero(n : Nat, h : Eq()(n, 0))", "all_zero(n : Nat)"),
        ("| 0 => h", "| 0 => Eq/refl()"),
        ("let boom : Eq()(0, 0) = all_zero(0, Eq/refl());", "let boom : Eq()(1, 0) = all_zero(1);"),
    ])
    .by_elaborator_in(
        "/all_zero",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Nat)(n, 0)"),
    );
}

// The same over the `List` carrier, whose cons arm binds the element as well: the carriers share one arm rule. The control's identity arm inhabits what the motive states, from a hypothesis.
#[test]
fn a_list_fold_motive_that_names_its_scrutinee_states_it_at_every_tail() {
    control(
        r#"
        use /std/{Nat, Eq, List};

        let all_empty(l : List(Nat), h : Eq()(List/len(l), 0)) -> Eq()(List/len(l), 0) =
            match l : (_) => Eq()(List/len(l), 0)
            | [] => h
            | [h, ..t]; ih => ih
            end;

        let boom : Eq()(0, 0) = all_empty([], Eq/refl());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("all_empty(l : List(Nat), h : Eq()(List/len(l), 0))", "all_empty(l : List(Nat))"),
        ("| [] => h", "| [] => Eq/refl()"),
        ("let boom : Eq()(0, 0) = all_empty([], Eq/refl());", "let boom : Eq()(1, 0) = all_empty([7]);"),
    ])
    .by_elaborator_in(
        "/all_empty",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Nat)(List/len(@Nat, l), 0)"),
    );
}

// An expression scrutinee reaches an arm taken once through a case equation rather than a substitution, and an arm of a fold records none: the occurrence the motive names stays the expression. The control's identity arm inhabits what the motive states, from a hypothesis.
#[test]
fn a_fold_motive_that_names_its_scrutinee_expression_states_it_at_every_tail() {
    control(
        r#"
        use /std/{Nat, Eq};

        let twice(n : Nat) -> Nat = n + n;

        let all_zero(n : Nat, h : Eq()(twice(n), 0)) -> Eq()(twice(n), 0) =
            match twice(n) : (_) => Eq()(twice(n), 0)
            | 0 => h
            | k + 1; ih => ih
            end;

        let boom : Eq()(0, 0) = all_zero(0, Eq/refl());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("all_zero(n : Nat, h : Eq()(twice(n), 0))", "all_zero(n : Nat)"),
        ("| 0 => h", "| 0 => Eq/refl()"),
        ("let boom : Eq()(0, 0) = all_zero(0, Eq/refl());", "let boom : Eq()(2, 0) = all_zero(1);"),
    ])
    .by_elaborator_in(
        "/all_zero",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Nat)(2 * n, 0)"),
    );
}

// A `let` alias of the scrutinee is a definition the reducer reads through, and the motive that names the alias states the scrutinee as one that names it does. The control's identity arm inhabits what the motive states, from a hypothesis.
#[test]
fn a_fold_motive_that_names_its_scrutinee_through_an_alias_states_it_at_every_tail() {
    control(
        r#"
        use /std/{Nat, Eq};

        let all_zero(n : Nat, h : Eq()(n, 0)) -> Eq()(n, 0) =
            let y = n;
            match n : (_) => Eq()(y, 0)
            | 0 => h
            | k + 1; ih => ih
            end;

        let boom : Eq()(0, 0) = all_zero(0, Eq/refl());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("all_zero(n : Nat, h : Eq()(n, 0))", "all_zero(n : Nat)"),
        ("| 0 => h", "| 0 => Eq/refl()"),
        ("let boom : Eq()(0, 0) = all_zero(0, Eq/refl());", "let boom : Eq()(1, 0) = all_zero(1);"),
    ])
    .by_elaborator_in(
        "/all_zero",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Nat)(n, 0)"),
    );
}

// A motive that names its scrutinee is no mistake where the arms inhabit what it states: the scrutinee is then a parameter of the fold, fixed at every tail.
#[test]
fn a_fold_whose_arms_inhabit_a_motive_that_names_its_scrutinee_is_accepted() {
    assert_eq!(
        run(A_FOLD_WHOSE_ARMS_INHABIT_A_MOTIVE_THAT_NAMES_ITS_SCRUTINEE_IS_ACCEPTED),
        b"held"
    );
}

// The control: a motive that binds the scrutinee types the hypothesis at the tail, and the ordinary induction goes through.
#[test]
fn a_fold_motive_that_binds_its_scrutinee_still_folds() {
    assert_eq!(
        run(A_FOLD_MOTIVE_THAT_BINDS_ITS_SCRUTINEE_STILL_FOLDS),
        b"3"
    );
}

// A fold takes its identity arm at the end of every value, where the scrutinee is whatever it was. Typed with `n` at `0`, the arm reads `h` as a proof of `Holds(0 == 2)`, and the fold at `2` is a proof of `False`. The arm learns nothing of `n`, so `h` stands at the type it was given. The control states what `h` holds at every tail.
#[test]
fn the_identity_arm_of_a_fold_reads_its_scrutinee_at_no_case() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, False, Holds};

        let f(n : Nat, h : Holds(n == 2)) -> Holds(n == 2) =
            match n : (m) => Holds(n == 2)
            | 0 => h
            | k + 1; ih => ih
            end;

        let kept : Holds(2 == 2) = f(2, True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Holds(n == 2) =", "-> False ="),
        ("(m) => Holds(n == 2)", "(m) => False"),
        ("let kept : Holds(2 == 2)", "let boom : False"),
    ])
    .by_elaborator_in(
        "/f",
        |error| matches!(error, Error::TypeMismatch { inferred, .. } if pinned(inferred) == "/sys/Bool/Holds(n == 2)"),
    );
}

// The cons arm is taken at every tail, and `n` is `k + 1` at the outermost alone: `h` beside the hypothesis at the tail would refute each other at every other. The control states what `h` holds at every tail.
#[test]
fn the_cons_arm_of_a_fold_reads_its_scrutinee_at_no_case() {
    control(
        r#"
        use /std/{Nat, proved};
        use /std/Bool/{True, False, Holds};

        let step(k : Nat, a : Holds(k + 1 == 2), b : Holds(k == 0)) -> Holds(k + 1 == 0) =
            proved();

        let f(n : Nat, h : Holds(n == 2)) -> Holds(n == 2) =
            match n : (m) => Holds(n == 2)
            | 0 => h
            | k + 1; ih => h
            end;

        let kept : Holds(2 == 2) = f(2, True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Holds(n == 2) =", "-> Holds(n == 0) ="),
        ("(m) => Holds(n == 2)", "(m) => Holds(m == 0)"),
        ("| 0 => h", "| 0 => True/qed()"),
        ("| k + 1; ih => h", "| k + 1; ih => step(k, h, ih)"),
        ("let kept : Holds(2 == 2)", "let boom : False"),
    ])
    .by_elaborator_in(
        "/f",
        |error| matches!(error, Error::TypeMismatch { inferred, .. } if pinned(inferred) == "/sys/Bool/Holds(n == 2)"),
    );
}

// An expression scrutinee is refined through a recorded equation where a variable is substituted, and an arm of a fold records none. The control states what `h` holds at every tail.
#[test]
fn an_arm_of_a_fold_over_a_call_records_no_equation() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, False, Holds};

        let f(g : (Nat) -> Nat, x : Nat, h : Holds(g(x) == 2)) -> Holds(g(x) == 2) =
            match g(x) : (m) => Holds(g(x) == 2)
            | 0 => h
            | k + 1; ih => ih
            end;

        let kept : Holds(2 == 2) = f((y : Nat) => y, 2, True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Holds(g(x) == 2) =", "-> False ="),
        ("(m) => Holds(g(x) == 2)", "(m) => False"),
        ("let kept : Holds(2 == 2)", "let boom : False"),
    ])
    .by_elaborator_in(
        "/f",
        |error| matches!(error, Error::TypeMismatch { inferred, .. } if pinned(inferred) == "/sys/Bool/Holds(g(x) == 2)"),
    );
}

// The carriers share one arm rule: a list's fold reads its scrutinee at no case either. The control states what `h` holds at every tail.
#[test]
fn an_arm_of_a_list_fold_reads_its_scrutinee_at_no_case() {
    control(
        r#"
        use /std/{Nat, List};
        use /std/Bool/{True, False, Holds};

        let f(xs : List(Nat), h : Holds(List/len(xs) == 2)) -> Holds(List/len(xs) == 2) =
            match xs : (m) => Holds(List/len(xs) == 2)
            | [] => h
            | [x, ..t]; ih => ih
            end;

        let kept : Holds(2 == 2) = f([1, 2], True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Holds(List/len(xs) == 2) =", "-> False ="),
        ("(m) => Holds(List/len(xs) == 2)", "(m) => False"),
        ("let kept : Holds(2 == 2)", "let boom : False"),
    ])
    .by_elaborator_in(
        "/f",
        |error| matches!(error, Error::TypeMismatch { inferred, .. } if pinned(inferred) == "/sys/Bool/Holds(List/len(@Nat, xs) == 2)"),
    );
}

// The control: a hypothesis about the scrutinee rides in the motive, which restates it at each tail, and the arm hands the hypothesis at the tail the proof it asks for.
#[test]
fn a_hypothesis_about_a_folds_scrutinee_rides_in_its_motive() {
    assert_eq!(
        run(A_HYPOTHESIS_ABOUT_A_FOLDS_SCRUTINEE_RIDES_IN_ITS_MOTIVE),
        b"3"
    );
}

// The fold's rule reached through a proposition: under `e : Eq()(n, 2)`, an identity arm typed with `n` at `0` would see `e` at `Eq()(0, 2)` and refute it by a match with no arm, and at `2` the fold takes that arm at its last tail with `e` the proof it was handed. The arm is typed at the motive alone, so `e` keeps its type there and its match with no arm rules out nothing. The control is the same match as a case split, whose `0` arm is taken at `0` and refutes `e` there.
#[test]
fn a_folds_identity_arm_refutes_no_proof_about_its_scrutinee() {
    control(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{False};

        let f(n: Nat, e: Eq()(n, 2)) -> Nat =
            match n | 0 => match e end | k + 1 => k end;

        let _forged: Nat = f(2, Eq/refl());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Nat =", "-> False ="),
        (
            "match n | 0 => match e end | k + 1 => k end",
            "match n: (m) => False | 0 => match e end | k + 1; ih => ih end",
        ),
        ("let _forged: Nat", "let _forged: False"),
    ])
    .by_elaborator_in("/f", |error| {
        matches!(error, Error::MatrixUncovered { indexed: true, .. })
    });
}

// A fold takes its identity arm at the last tail, where the scrutinee is no case of that arm, so a match inside the arm meets `v` at the scrutinee itself and not at the arm's case: over each carrier `s`'s target may meet it there, and each empty match must name `s`. Specialized as a case split's arm is, the identity arm would excuse `s`, and `forged`, a fold at `1` whose last tail reaches the empty match with `v` a `V(1)` that `V/s(0)` inhabits, would be a closed inhabitant of `False`. The control is the same match as a case split, whose identity arm is taken once, at the scrutinee itself, where the case excuses `s`. `curios_cert`'s `recheck::elimination_tests` puts the module to the certifier under this name. Each control is that case split.
#[test]
fn a_match_inside_a_folds_identity_arm_learns_nothing_of_the_scrutinee() {
    let refusals = control(
        r#"
        use /std/{Nat, Byte, Bytes, List};
        use /std/Bool/{False};

        induct V: (n: Nat) -> Type
        | s(m: Nat): (m + 1)
        end

        induct L: (l: List(Nat)) -> Type
        | s(m: Nat, t: List(Nat)): ([m, ..t])
        end

        induct B: (l: Bytes) -> Type
        | s(m: Byte, t: Bytes): (x[m, ..t])
        end

        pub let nat(n: Nat, v: V(n)) -> Nat =
            match n | 0 => (match v end) | k + 1 => k end;
        pub let list(xs: List(Nat), v: L(xs)) -> Nat =
            match xs | [] => (match v end) | [h, .._] => h end;
        pub let bytes(xs: Bytes, v: B(xs)) -> Nat =
            match xs | x[] => (match v end) | x[_, .._] => 0 end;
        pub let split(n: Nat, v: V(n)) -> Nat =
            match n | 0 => (match v end) | k + 1 => k end;

        let forged = nat;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("nat(n: Nat, v: V(n)) -> Nat =\n            match n | 0 => (match v end) | k + 1 => k end;", "nat(n: Nat, v: V(n)) -> False =\n            match n: (_) => False | 0 => (match v end) | _ + 1; ih => ih end;"),
        ("list(xs: List(Nat), v: L(xs)) -> Nat =\n            match xs | [] => (match v end) | [h, .._] => h end;", "list(xs: List(Nat), v: L(xs)) -> False =\n            match xs: (_) => False | [] => (match v end) | [_, .._]; ih => ih end;"),
        ("bytes(xs: Bytes, v: B(xs)) -> Nat =\n            match xs | x[] => (match v end) | x[_, .._] => 0 end;", "bytes(xs: Bytes, v: B(xs)) -> False =\n            match xs: (_) => False | x[] => (match v end) | x[_, .._]; ih => ih end;"),
        ("let forged = nat;", "let forged: False = nat(1, V/s(0));"),
    ]);

    for at in ["/nat", "/list", "/bytes"] {
        refusals.by_elaborator_in(at, |error| {
            matches!(error, Error::MatrixUncovered { indexed: true, .. })
        });
    }
}

// The fold ticket's identity arm over `Bytes`, whose witnesses state it for `Nat` and `List`: typed with `xs` at `x[]`, `h` would stand at `Holds(0 == 2)`, which is `False`, while reduction takes the arm at the last tail with `xs` still `x[1, 2]`. The arm learns nothing of `xs`, so `h` stands at the type it was given. `curios_cert`'s `recheck::elimination_tests` puts the term to the certifier under this name. The control states what `h` holds at every tail.
#[test]
fn the_identity_arm_of_a_bytes_fold_reads_its_scrutinee_at_no_case() {
    control(
        r#"
        use /std/{Bytes};
        use /std/Bool/{True, False, Holds};

        let f(xs: Bytes, h: Holds(Bytes/len(xs) == 2)) -> Holds(Bytes/len(xs) == 2) =
            match xs: (_) => Holds(Bytes/len(xs) == 2) | x[] => h | x[_, .._]; ih => ih end;

        let kept: Holds(2 == 2) = f(x[1, 2], True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Holds(Bytes/len(xs) == 2) =", "-> False ="),
        ("(_) => Holds(Bytes/len(xs) == 2)", "(_) => False"),
        ("let kept: Holds(2 == 2)", "let forged: False"),
    ])
    .by_elaborator_in("/f", |error| {
        matches!(error, Error::TypeMismatch { inferred, .. } if pinned(inferred) == "/sys/Bool/Holds(Bytes/len(xs) == 2)")
    });
}
