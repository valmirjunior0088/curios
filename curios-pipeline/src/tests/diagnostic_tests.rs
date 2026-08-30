//! How a mismatch is spelled: universe instances withheld, implicit parameters marked, witnesses folded back to their operators.

use super::test_support::*;

#[test]
fn a_proposition_where_a_proof_belongs_is_named_and_its_metavariables_are_not_numbered() {
    // The beginner's collision: the statement `Eq(n + n, n * 2)` handed to `Eq/cong` where the proof `ih` belonged. The mismatch is `Prop` against an `Eq` whose indices never solved; the report says so in a sentence, and spells the unsolved indices `?` rather than as elaboration counters.
    let source = r#"
        use /std/{Nat, Eq, Io};

        let step(n: Nat, ih: Eq(n + n, n * 2)) -> Eq((n + 1) + (n + 1), (n + 1) * 2) =
            Eq/cong((x) => x + 2, Eq(n + n, n * 2));

        Io/pure(())
    "#;
    let error = compile(source, None).map(|_| ()).unwrap_err();
    assert!(
        error.contains("inferred: Prop") && error.contains("expected: Eq(@?, ?, ?)"),
        "unexpected report: {error}"
    );
    assert!(
        error.contains("a proposition was given where a proof of it was expected"),
        "unexpected report: {error}"
    );
}

#[test]
fn an_unbound_name_is_offered_its_reachable_spelling_or_its_import() {
    // The transcript's two stalls, reported by the text stage, which is the one that can say what was meant: `cong` is a member of `Eq`, so the route through an imported `Eq` is offered beside the direct import; `Bool` is a root's child, so the import is the route. The prelude build elaborates every `/std` module through this same path, so a false report here would fail the workspace build before it reached this test.
    let source = r#"
        use /std/{Nat, Eq, Io};

        let step(n: Nat, ih: Eq(n + n, n * 2)) -> Eq((n + 1) + (n + 1), (n + 1) * 2) =
            cong((x) => x + 2, ih);

        Io/pure(())
    "#;
    let error = compile(source, None).map(|_| ()).unwrap_err();
    assert!(
        error.contains("unbound variable: cong")
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
    let error = compile(source, None).map(|_| ()).unwrap_err();
    assert!(
        error.contains("unbound variable: Bool")
            && error.contains("`Bool` is `/std/Bool`: write it absolute, or `use /std/{Bool};`"),
        "unexpected report: {error}"
    );
    assert!(
        !error.contains("/sys/Bool/Bool"),
        "a deeper route to the same name is not offered: {error}"
    );
}

#[test]
fn a_rigid_mismatch_still_reports_as_a_mismatch() {
    let source = r#"
        use /std/{Nat, Io};

        let bad: Nat = true;

        Io/pure(())
    "#;
    let error = compile(source, None).map(|_| ()).unwrap_err();
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
    // A goal under a match motive over a universe-polymorphic family used to report `Eq.{?u311}(…)` — an instance spelling the surface language does not even have. Report display erases universe instances, so no `.{` (and no `?u`) ever appears.
    let source = r#"
        use /std/{Nat, Eq};
        let double(n : Nat) -> Nat = n + n;
        let double_correct(n : Nat) -> Eq(double(n), n * 2) =
            match n : (m) => Eq(m + m, m * 2)
            | 0 => ?
            | p + 1; ih => ?
            end;
        double(21)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

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
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

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
    // The instance suffixes are gone by the sibling above, but a `Type`'s *own* level is a separate node, and a diagnostic over a polymorphic head is where unsolved ones cluster: this fixture once read `(A: Type.{?u263}) -> Nat` against `(#…: (#…: Type.{?u261}) -> Type.{?u262}) -> Nat`. Three placeholders, none of them the disagreement. A concrete level still prints — only a metavariable-headed one is suppressed.
    let source = r#"
        use /std/{Nat};
        let g(A : Type) -> Nat = 0;
        let f : ((Type) -> Type) -> Nat = g;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
    assert!(
        error.contains("(A: Type) -> Nat"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("?u"), "universe meta leaked: {error}");
}

#[test]
fn a_mismatch_keeps_a_concrete_universe_level() {
    // The other half of the rule, and the reason it is not "suppress every level": `Type` inhabits the sort one above it, so this reports a genuine `Type.{1}`. Erasing that would render two distinct sorts identically, which is the one thing a mismatch may never do.
    let source = r#"
        use /std/{Str};
        let f : Type = Type;
        let g : Str = f;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
    assert!(
        error.contains("inferred: Type.{1}"),
        "concrete level suppressed: {error}"
    );
}

#[test]
fn a_mismatch_marks_an_implicit_nominal_parameter() {
    // `Eq(@A : Type) : (A, A) -> pub Prop` has one implicit parameter and two indices, so a use site writes `Eq(5, 6)`. The mismatch normalizes to `InductType`, which carries no plicities — leaving `Eq(Nat, 5, 5)`, three positional arguments the surface would reject. The marks come from the type constructor's own definition, which is where lowering left them.
    let source = r#"
        use /std/{Nat, Eq};
        let claim : Eq(2 + 3, 6) = Eq/refl();
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("inferred: Eq(@Nat, 5, 5)"),
        "implicit parameter not marked: {error}"
    );
    assert!(
        error.contains("expected: Eq(@Nat, 5, 6)"),
        "implicit parameter not marked: {error}"
    );
}

#[test]
fn a_mismatch_leaves_an_explicit_nominal_parameter_unmarked() {
    // The other side of the same rule: `Option(A : Type)` declares its parameter explicit, so `Option(Nat)` is already what a use site writes and gains no mark. A blanket "parameters are implicit" rule would have spelled this `Option(@Nat)`.
    let source = r#"
        use /std/{Nat, Option};
        let o : Option(Nat) = Option/some(true);
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

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
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("inferred: Box(@Nat)"),
        "implicit struct parameter not marked: {error}"
    );
}

#[test]
fn a_mismatch_over_an_applied_head_is_located() {
    // A value body's spine forms are rebuilt by the `!`-hoisting walk rather than routed through the span-stamping lowering entry, so an applied head once reached elaboration unspanned and reported with no snippet at all — while the same mismatch over a bare variable reported one.
    let source = r#"
        use /std/{Nat, Str, Option};
        let bad : Str = Option/some(1);
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("Option/some(1)"),
        "no source snippet: {error}"
    );
}

#[test]
fn an_abstract_witness_folds_back_to_its_operator() {
    // Under a `use Add(A)` parameter the projection is stuck on a witness that is a *binderso no amount of reduction reaches the operator: the fold must resolve it through the concept the binder's declared type names, or the report spells `a + a` as `(#6561).0(a, a)`.
    let source = r#"
        use /syn/{Add};
        use /std/{Nat, Eq};
        let bad(@A : Type, use Add(A), a : A) -> Eq(a + a, a + a) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

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
        use /syn/{Add};
        use /std/{Nat, Eq};
        let bad(@A : Type, use Add(A), a : A) -> Eq(a + a, a) = Eq/refl();
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
    assert!(error.contains("a + a"), "operator not folded: {error}");
    assert!(
        !error.contains(").0("),
        "witness projection leaked: {error}"
    );
}

#[test]
fn goal_types_spell_negated_equality_as_neq() {
    // `a != b` elaborates as an xor-negated equality call (no `BoolNot` intrinsic exists); the report folds the pair back to `!=`.
    let source = r#"
        use /std/{Nat, Bool, Eq};
        let claim : Eq(1 != 2, true) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

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
        f(1, 2, 3, 4, 5)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(
        error.contains("first_argument: Nat,\n"),
        "expected a broken binder list: {error}"
    );
}

#[test]
fn a_hard_error_preempts_the_goal_batch() {
    // A goal already registered does not soften a later hard failure: the interrupted elaboration established no complete batch, so only the hard error reports.
    let source = r#"
        use /std/{Nat};
        let m : Nat = ?;
        let bad : Nat = true;
        m
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
    assert!(!error.contains("goal `?`"), "unexpected error: {error}");
}

/// The reference's own span survives the instance wrapper elaboration mints around a polymorphic occurrence: the typed head is a bare `Var` with no span of its own, so the wrapper carries the occurrence's, and the mismatch renders with its source snippet and a caret under the reference rather than arriving unlocated.
#[test]
fn a_mismatch_at_a_polymorphic_reference_keeps_its_caret() {
    let source = r#"
        use /std/{Nat};
        let g(A : Type) -> Nat = 0;
        let f : ((Type) -> Type) -> Nat = g;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

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
