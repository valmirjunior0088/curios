//! How a mismatch is spelled: universe instances withheld, implicit parameters marked, witnesses folded back to their operators.

use super::test_support::*;

#[test]
fn a_proposition_where_a_proof_belongs_is_named_and_its_metavariables_are_not_numbered() {
    // The beginner's collision: the statement `Eq()(n + n, n * 2)` handed to `Eq/cong` where the proof `ih` belonged. The mismatch is `Prop` against an `Eq` whose indices never solved; the report says so in a sentence, and spells the unsolved indices `?` rather than as elaboration counters.
    let source = r#"
        use /std/{Nat, Eq, Io};

        let step(n: Nat, ih: Eq()(n + n, n * 2)) -> Eq()((n + 1) + (n + 1), (n + 1) * 2) =
            Eq/cong((x) => x + 2, Eq()(n + n, n * 2));

        Io/pure(())
    "#;
    let error = compile(source).map(|_| ()).unwrap_err();
    assert!(
        error.contains("inferred: Prop") && error.contains("expected: Eq(@?)(?, ?)"),
        "unexpected report: {error}"
    );
    assert!(
        error.contains("a proposition was given where a proof of it was expected"),
        "unexpected report: {error}"
    );
}

#[test]
fn a_proof_handed_to_an_implicit_type_is_reported_as_a_proposition() {
    // `A` is declared at `Type` and the one candidate the argument offers it is the proposition `True`, which no `Type` admits: the implicit stays unsolved, and a mismatch against it would say `expected: ?`.
    let source = r#"
        use /std/{Io};
        use /std/Bool/{True};

        let keep(@A: Type, a: A) -> A = a;
        let proof: True = keep(True/qed());

        Io/pure(())
    "#;
    let error = compile(source).map(|_| ()).unwrap_err();
    assert!(
        error.contains(
            "a proposition was given where the implicit argument 'A' of 'keep' takes a type"
        ) && error.contains("True is a proposition"),
        "unexpected report: {error}"
    );
    assert!(!error.contains("expected: ?"), "unexpected report: {error}");

    // A type handed to the same implicit is no such report.
    let source = r#"
        use /std/{Nat, Io};

        let keep(@A: Type, a: A) -> A = a;
        let data: Nat = keep(3);

        Io/pure(())
    "#;
    assert!(compile(source).is_ok());
}

#[test]
fn a_value_handed_to_an_implicit_proposition_is_reported_as_a_type() {
    // The mirror of the report above: `P` is declared at `Prop` and the one candidate the argument offers it is the type `Nat`, which no `Prop` admits.
    let source = r#"
        use /std/{Nat, Io};

        let proves(@P: Prop, p: P) -> Nat = 0;
        let count: Nat = proves(5);

        Io/pure(())
    "#;
    let error = compile(source).map(|_| ()).unwrap_err();
    assert!(
        error.contains(
            "a type was given where the implicit argument 'P' of 'proves' takes a proposition"
        ) && error.contains("Nat is a type"),
        "unexpected report: {error}"
    );
    assert!(!error.contains("expected: ?"), "unexpected report: {error}");
}

#[test]
fn a_refusal_at_the_tail_of_a_let_is_placed_at_the_tail() {
    // A `let` block's tail is a term of its own: a mismatch at its root is reported where the tail is written, as one at a lambda's body is, and not under the block's first binding.
    let source = r#"
        use /std/{Nat, Bool, Io};

        let pick(x: Nat) -> Bool =
            let n = 5;
            Nat/add(n, x);

        Io/pure(())
    "#;
    let error = compile(source).map(|_| ()).unwrap_err();
    assert!(
        error.contains("Nat/add(n, x);") && !error.contains("let n = 5;"),
        "unexpected report: {error}"
    );
}

#[test]
fn an_unbound_name_is_offered_its_reachable_spelling_or_its_import() {
    // The transcript's two stalls, reported by the text stage, which is the one that can say what was meant: `cong` is a member of `Eq`, so the route through an imported `Eq` is offered beside the direct import; `Bool` is a root's child, so the import is the route. The prelude build elaborates every `/std` module through this same path, so a false report here would fail the workspace build before it reached this test.
    let source = r#"
        use /std/{Nat, Eq, Io};

        let step(n: Nat, ih: Eq()(n + n, n * 2)) -> Eq()((n + 1) + (n + 1), (n + 1) * 2) =
            cong((x) => x + 2, ih);

        Io/pure(())
    "#;
    let error = compile(source).map(|_| ()).unwrap_err();
    assert!(
        error.contains("`cong` is not in scope")
            && error.contains(
                "`cong` is `/std/Eq/cong`: write `Eq/cong` if `Eq` is imported, or `use /std/Eq/{cong};`"
            ),
        "unexpected report: {error}"
    );

    let source = r#"
        use /std/{Io};

        let f(b: Bool) -> Bool = b;

        Io/pure(())
    "#;
    let error = compile(source).map(|_| ()).unwrap_err();
    assert!(
        error.contains("`Bool` is not in scope")
            && error.contains("`Bool` is `/std/Bool`: write it absolute, or `use /std/{Bool};`"),
        "unexpected report: {error}"
    );
    assert!(
        !error.contains("/sys/Bool/Bool"),
        "a deeper route to the same name is not offered: {error}"
    );
    assert!(
        !error.contains("/sys/Bool"),
        "a route this program may not write is not offered: {error}"
    );
}

#[test]
fn a_rigid_mismatch_still_reports_as_a_mismatch() {
    let source = r#"
        use /std/{Nat, Io};

        let bad: Nat = true;

        Io/pure(())
    "#;
    let error = compile(source).map(|_| ()).unwrap_err();
    assert!(
        error.contains("type mismatch"),
        "unexpected report: {error}"
    );
    assert!(
        !error.contains("postponed conversion"),
        "unexpected report: {error}"
    );
}

#[test]
fn goal_reports_spell_no_universe_instances() {
    // A goal under a match motive over a universe-polymorphic family must not report an instance such as `Eq.{?u311}(…)` — a spelling the surface language does not even have. Report display erases universe instances, so no `.{` (and no `?u`) ever appears.
    let source = r#"
        use /std/{Nat, Eq};
        let double(n : Nat) -> Nat = n + n;
        let double_correct(n : Nat) -> Eq()(double(n), n * 2) =
            match n : (m) => Eq()(m + m, m * 2)
            | 0 => ?
            | p + 1; ih => ?
            end;
        let subject : /std/Nat = double(21);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert_eq!(
        error.matches("goal `?`").count(),
        2,
        "unexpected error: {error}"
    );
    assert!(!error.contains(".{"), "universe instance leaked: {error}");
    assert!(!error.contains("?u"), "universe meta leaked: {error}");
}

#[test]
fn mismatch_reports_spell_no_universe_instances() {
    // The goal path erases instances structurally; a mismatch carries raw terms to the formatter, which suppresses them at the printer. Same unspellable `.{…}`, same absence — over a nominal family, which is the only thing that carries an instance (`Bool` and `Nat` never did).
    let source = r#"
        use /std/{Nat, Str, Option};
        let bad : Str = Option/some(1);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
    assert!(
        error.contains("inferred: Option(Nat)"),
        "unexpected error: {error}"
    );
    assert!(!error.contains(".{"), "universe instance leaked: {error}");
    assert!(!error.contains("?u"), "universe meta leaked: {error}");
}

#[test]
fn a_mismatch_over_a_polymorphic_head_spells_no_universe_metas() {
    // The instance suffixes are gone by the sibling above, but a `Type`'s *own* level is a separate node, and a diagnostic over a polymorphic head is where unsolved ones cluster: unsuppressed, this fixture would read `(A: Type.{?u263}) -> Nat` against `(#…: (#…: Type.{?u261}) -> Type.{?u262}) -> Nat`. Three placeholders, none of them the disagreement. A concrete level still prints — only a metavariable-headed one is suppressed, which `curios-core`'s `a_concrete_level_prints_where_a_metavariable_headed_one_is_suppressed` holds over terms built by hand: a declaration's levels are its callers', so a program's report meets none above zero that is decided.
    let source = r#"
        use /std/{Nat};
        let g(A : Type) -> Nat = 0;
        let f : ((Type) -> Type) -> Nat = g;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
    assert!(
        error.contains("(A: Type) -> Nat"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("?u"), "universe meta leaked: {error}");
}

#[test]
fn a_mismatch_marks_an_implicit_nominal_parameter() {
    // `Eq(@A : Type) : (A, A) -> pub Prop` has one implicit parameter and two indices, so a use site writes `Eq()(5, 6)`. The mismatch normalizes to `InductType`, which carries no plicities — leaving `Eq(Nat)(5, 5)`, an explicit argument where the surface takes an implicit one. The marks come from the type constructor's own definition, which is where lowering left them.
    let source = r#"
        use /std/{Nat, Eq};
        let claim : Eq()(2 + 3, 6) = Eq/refl();
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("inferred: Eq(@Nat)(5, 5)"),
        "implicit parameter not marked: {error}"
    );
    assert!(
        error.contains("expected: Eq(@Nat)(5, 6)"),
        "implicit parameter not marked: {error}"
    );
}

#[test]
fn a_mismatch_leaves_an_explicit_nominal_parameter_unmarked() {
    // The other side of the same rule: `Option(A : Type)` declares its parameter explicit, so `Option(Nat)` is already what a use site writes and gains no mark. A blanket "parameters are implicit" rule would have spelled this `Option(@Nat)`.
    let source = r#"
        use /std/{Nat, Option, Bool};
        let o : Option(Nat) = Option/some(true);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("inferred: Option(Bool)") && error.contains("expected: Option(Nat)"),
        "explicit parameter wrongly marked: {error}"
    );
}

#[test]
fn a_mismatch_marks_an_implicit_struct_parameter() {
    // Structures take the same marks through the same table: their type constructors are `let` items where an inductive's is a `rec`, which is why the table walks both.
    let source = r#"
        use /std/{Nat, Str};
        pub struct Box(@A : Type) : pub Type { it : A }
        let f : Str = Box { it = 1 };
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("inferred: Box(@Nat)"),
        "implicit struct parameter not marked: {error}"
    );
}

#[test]
fn a_stuck_call_under_a_constructor_is_spelled_by_its_name() {
    // The first induction over a declared type: reducing `add(N/s(k), N/z())` fires the successor arm, and the call the arm built stands in the constructor's payload, headed by the group's projection. It is the call `add(k, N/z())` wherever it stands, where a head normalized alone spelled it as the member's body applied on one side and as that body's stuck `match` on the other, beside the name the author wrote.
    let source = r#"
        use /std/{Eq, Io};

        induct N: Type
        | z()
        | s(N)
        end

        let add(a: N, b: N) -> N =
            match a | z() => b | s(k) => N/s(add(k, b)) end;

        let step(k: N) -> Eq()(add(N/s(k), N/z()), N/s(add(k, N/z()))) = Eq/refl();

        let wrong(k: N) -> Eq()(add(N/s(k), N/z()), N/s(k)) = step(k);

        Io/pure(())
    "#;
    let error = compile(source).map(|_| ()).unwrap_err();
    assert!(
        error.contains("inferred: Eq(@N)(N/s(add(k, N/z())), N/s(add(k, N/z())))")
            && error.contains("expected: Eq(@N)(N/s(add(k, N/z())), N/s(k))"),
        "unexpected report: {error}"
    );
}

#[test]
fn a_mismatch_over_an_applied_head_is_located() {
    // A value body's spine forms are rebuilt by the `!`-hoisting walk rather than routed through the span-stamping lowering entry, so an applied head can reach elaboration unspanned and report with no snippet at all, where the same mismatch over a bare variable reports one; this holds it to the snippet.
    let source = r#"
        use /std/{Nat, Str, Option};
        let bad : Str = Option/some(1);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("Option/some(1)"),
        "no source snippet: {error}"
    );
}

#[test]
fn a_term_that_is_no_type_where_one_is_wanted_is_located_at_itself() {
    // The refusal is raised once the term's own elaboration has returned, so no frame of that term is left to place it: unplaced there, it takes the place of the nearest term around it, the first statement of the chain the annotation stands in, and none at all where the annotation opens the chain.
    let source = r#"
        use /std/{Nat, Byte};
        let first = 1;
        let pair: (Byte, Byte) = (1, 2);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("inferred: {Type, Type}")
            && error.contains("let pair: (Byte, Byte) = (1, 2);"),
        "located elsewhere: {error}"
    );
}

#[test]
fn an_abstract_witness_folds_back_to_its_operator() {
    // Under a `use Add(A)` parameter the projection is stuck on a witness that is a *binderso no amount of reduction reaches the operator: the fold must resolve it through the concept the binder's declared type names, or the report spells `a + a` as `(#6561).0(a, a)`.
    let source = r#"
        use /std/ops/{Add};
        use /std/{Nat, Eq};
        let bad(@A : Type, use Add(A), a : A) -> Eq()(a + a, a + a) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("a + a"), "operator not folded: {error}");
    assert!(
        !error.contains(").0("),
        "witness projection leaked: {error}"
    );
}

#[test]
fn an_abstract_witness_folds_back_in_a_mismatch_too() {
    // The mismatch path denoises by normalizing, which suffices only while the operand type is concrete. The abstract case needs the same structural fold the goal path runs.
    let source = r#"
        use /std/ops/{Add};
        use /std/{Nat, Eq};
        let bad(@A : Type, use Add(A), a : A) -> Eq()(a + a, a) = Eq/refl();
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
    assert!(error.contains("a + a"), "operator not folded: {error}");
    assert!(
        !error.contains(").0("),
        "witness projection leaked: {error}"
    );
}

#[test]
fn goal_types_spell_negated_equality_as_neq() {
    // `a != b` elaborates as a projection of `Eql`'s `neq` off the operand type's witness; the report folds it back to `!=`.
    let source = r#"
        use /std/{Nat, Bool, Eq};
        let claim : Eq()(1 != 2, true) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("1 != 2"), "unexpected error: {error}");
    assert!(!error.contains("witness"), "unexpected error: {error}");
}

#[test]
fn a_wide_goal_type_breaks_across_lines_in_the_report() {
    // The report renders terms within a fixed width, so a function type too wide for one line breaks one binder per line, while short goal types (every other goal test) stay on one.
    let source = r#"
        use /std/{Nat};
        let f : (first_argument : Nat, second_argument : Nat, third_argument : Nat, fourth_argument : Nat, fifth_argument : Nat) -> Nat = ?;
        let subject : /std/Nat = f(1, 2, 3, 4, 5);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(
        error.contains("first_argument: Nat,\n"),
        "expected a broken binder list: {error}"
    );
}

#[test]
fn a_hard_error_beside_a_goal_is_reported_with_it() {
    // A goal already registered does not soften a later hard failure, and the failure does not hide the goal: elaboration recovers past the refused item, so the run reports both — the refusal first, then the goal batch — and classifies as the refusal's (`goal_tests::a_goal_beside_a_refusal_classifies_as_mixed`).
    let source = r#"
        use /std/{Nat};
        let m : Nat = ?;
        let bad : Nat = true;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(
        error.find("type mismatch") < error.find("goal `?`"),
        "the refusal comes first: {error}"
    );
}

/// The reference's own span survives the instance wrapper elaboration mints around a polymorphic occurrence: the typed head is a bare `Var` with no span of its own, so the wrapper carries the occurrence's, and the mismatch renders with its source snippet and a caret under the reference rather than arriving unlocated.
#[test]
fn a_mismatch_at_a_polymorphic_reference_keeps_its_caret() {
    let source = r#"
        use /std/{Nat};
        let g(A : Type) -> Nat = 0;
        let f : ((Type) -> Type) -> Nat = g;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    let snippet = error
        .lines()
        .position(|line| line.ends_with("let f : ((Type) -> Type) -> Nat = g;"))
        .unwrap_or_else(|| panic!("the mismatch lost its source snippet: {error}"));
    let caret = error.lines().nth(snippet + 1).unwrap_or_default();
    assert_eq!(
        caret.chars().filter(|c| *c == '^').count(),
        1,
        "the caret under the reference is gone: {error}"
    );
    assert!(
        caret.ends_with('^'),
        "the caret drifted off the reference: {error}"
    );
}

/// A mistake decided where types are known names what is written instead, and only where that is what it is: `()` where a type is expected, and arguments past a call's list where what the call answers is itself a function.
#[test]
fn a_typed_mistake_names_what_is_written_instead() {
    let report = |source: &str| compile(source).map(|_| ()).unwrap_err();

    let unit = report(
        r#"
        use /std/{Io};

        let go() -> Io(()) = Io/pure(());

        go()
    "#,
    );
    assert!(
        unit.contains("the unit type is written `{}`"),
        "unexpected report: {unit}"
    );

    // A tuple that is no unit, where no tuple type is expected, is reported as it was.
    let pair = report(
        r#"
        use /std/{Nat, Io};

        let n: Nat = (1, 2);

        Io/pure(())
    "#,
    );
    assert!(
        pair.contains("introduced a tuple") && !pair.contains("unit type"),
        "unexpected report: {pair}"
    );

    let curried = report(
        r#"
        use /std/{Nat, Fmt};

        Fmt/print("% + % = %\n")(1, 2)
    "#,
    );
    assert!(
        curried.contains("wrong number of arguments: expected 1, got 2")
            && curried.contains("`f(a)(b)`"),
        "unexpected report: {curried}"
    );

    // Where the call answers no function, its arguments are only too many.
    let surplus = report(
        r#"
        use /std/{Nat, Io};

        let double(n: Nat) -> Nat = n + n;
        let four: Nat = double(1, 2);

        Io/pure(())
    "#,
    );
    assert!(
        surplus.contains("wrong number of arguments: expected 1, got 2")
            && !surplus.contains("`f(a)(b)`"),
        "unexpected report: {surplus}"
    );
}
