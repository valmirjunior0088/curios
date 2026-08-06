//! The pipeline's test suite, kept beside `lib.rs`.

use {
    super::*,
    curios_elab::{Context, erase_module},
    curios_ersd::Analysis,
    curios_prelude::SYNTAX,
    curios_text::{Entrypoint, RootSource},
    curios_wasm::to_bytes,
};

#[test]
fn entrypoint_type_is_used_as_expected_type() {
    let entrypoint = "0"
        .parse::<Entrypoint>()
        .unwrap()
        .with_type("/std/Bool".parse().unwrap());

    let error = compile_entrypoint(DEFAULT_STEP_BUDGET, &entrypoint, RootSource::none(), |_| {})
        .map_err(String::from)
        .unwrap_err();

    assert!(error.contains("type mismatch"));
}

fn compile(source: &str, type_: Option<&str>) -> Result<curios_wasm::Module, String> {
    let entrypoint = source.parse::<Entrypoint>().unwrap();

    let entrypoint = match type_ {
        Some(type_) => entrypoint.with_type(type_.parse().unwrap()),
        None => entrypoint,
    };

    compile_entrypoint(DEFAULT_STEP_BUDGET, &entrypoint, RootSource::none(), |_| {})
        .map(|(module, _foreigns)| module)
        .map_err(String::from)
}

#[test]
fn a_goal_batch_classifies_as_incomplete_and_a_hard_error_as_failure() {
    // The typed split the CLI's exit codes rest on: a written-goal batch is incomplete development state, a type mismatch a hard failure.
    let goals = "let m : /std/Nat = ?; m".parse::<Entrypoint>().unwrap();
    assert!(matches!(
        compile_entrypoint(DEFAULT_STEP_BUDGET, &goals, RootSource::none(), |_| {}),
        Err(CompileError::Incomplete(_))
    ));

    let mismatch = "let bad : /std/Nat = true; bad"
        .parse::<Entrypoint>()
        .unwrap();
    assert!(matches!(
        compile_entrypoint(DEFAULT_STEP_BUDGET, &mismatch, RootSource::none(), |_| {}),
        Err(CompileError::Failure(_))
    ));
}

#[test]
fn repeated_compilation_restores_an_unmutated_ersd_prefix() {
    let source = "/std/Nat/add(20, 22)";
    let first = compile(source, None).unwrap();
    let second = compile(source, None).unwrap();
    assert_eq!(to_bytes(&first), to_bytes(&second));
}

#[test]
fn foreign_declaration_produces_a_wasm_import() {
    // Must actually call `frobnicate` — an unreferenced declaration is pruned by `curios_ersd::optimize` before codegen ever sees it.
    let module = compile(
        r#"
            foreign frobnicate : (Nat, Bytes) -> Nat;
            frobnicate(5, x[\00, \01])
        "#,
        None,
    )
    .unwrap();

    assert!(
        module
            .imports()
            .iter()
            .any(|(namespace, name, _)| namespace == "ffi" && name == "/frobnicate"),
        "expected an ffi./frobnicate import, got {:?}",
        module.imports()
    );
}

#[test]
fn sys_and_foreign_calls_import_under_separate_namespaces() {
    // Must actually call both — an unreferenced declaration is pruned before codegen ever sees it (see the note above).
    let module = compile(
        r#"
            foreign frobnicate : (Nat) -> Nat;
            let _ = /std/Handle/write(/std/Handle/stdout, x[\00]);
            frobnicate(5)
        "#,
        None,
    )
    .unwrap();

    let imports = module.imports();

    assert!(
        imports
            .iter()
            .any(|(namespace, name, _)| namespace == "sys" && name == "write"),
        "expected a sys.write import, got {imports:?}"
    );
    assert!(
        imports
            .iter()
            .any(|(namespace, name, _)| namespace == "ffi" && name == "/frobnicate"),
        "expected an ffi./frobnicate import, got {imports:?}"
    );
}

fn compile_printed_stages(source: &str) -> Result<(String, String), String> {
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let mut ersd = String::new();
    let mut cont = String::new();

    compile_entrypoint(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        |stage| match stage {
            Stage::Ersd(stage) => ersd = format!("{stage}"),
            Stage::Cont(stage) => cont = format!("{stage}"),
            _ => {}
        },
    )?;

    Ok((ersd, cont))
}

#[test]
fn let_bound_tuple_with_an_effectful_field_lowers() {
    // A `let` bound to a tuple one of whose fields is an opaque foreign call: the field cannot be lowered in a pure-name position, so the binding must take the CPS join-block path in `into_cont`. Head-only purity classification used to route the whole `let` through `lower_pure_name` and panic the compiler on the field's host primitive. End-to-end guard for `is_pure_term`.
    let source = r#"
        foreign frobnicate : (Nat) -> Nat;
        let t = (frobnicate(5), 2);
        t.0
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn meta_free_prelude_program_compiles_without_overflow() {
    // The exact case that used to overflow: a meta-free entrypoint (no holes) that still pulls in the whole std/std prelude. Assembling and traversing the old N-deep nested term overflowed the stack during construction and in every pass; the flat `curios_core::Module`/`curios_ersd::Module` representation lowers it end-to-end to wasm without overflow.
    let source = r#"
        let id(A : Type, a : A) -> A = a;
        id(/std/Nat, 5)
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn solved_goal_reports_its_solution() {
    // `id ? 5`: the type argument `?` is solved to `Nat` from the value `5` (`id ? x`) — but a written goal never compiles: the module still elaborates fully, then zonk reports what it determined.
    let source = r#"
        let id(A : Type, a : A) -> A = a;
        id(?, 5)
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("? : Type"), "unexpected error: {error}");
    assert!(
        error.contains("? =") && error.contains("Nat"),
        "unexpected error: {error}"
    );
}

#[test]
fn goal_pinned_through_the_expected_type_reports_the_pin() {
    // `id ? true` checked against `/std/Bool`: the turnaround pins the type argument `?` to `Bool` through the expected type (a type-level pin), and the goal report names that solution.
    let source = r#"
        use /std/{Bool};
        let id(A : Type, a : A) -> A = a;
        id(?, true)
    "#;

    let error = compile(source, Some("/std/Bool")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(
        error.contains("? =") && error.contains("Bool"),
        "unexpected error: {error}"
    );
}

#[test]
fn unconstrained_goal_reports_undetermined() {
    // `let m : Nat = ? in m`: nothing constrains the value of `?`, so the goal report shows its type but no solution.
    let source = r#"
        use /std/{Nat};
        let m : Nat = ?;
        m
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("? : Nat"), "unexpected error: {error}");
    // No `? =` clause: nothing determined the goal.
    assert!(!error.contains("? ="), "unexpected error: {error}");
}

#[test]
fn goal_report_includes_the_local_scope() {
    // The goal sits under `x`'s binder, so the Γ frozen at its birth — just that binder — appears in the report.
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = ?;
        f(1)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("x : Nat"), "unexpected error: {error}");
}

#[test]
fn goal_in_synthesis_position_reports_a_meta_type() {
    // A bare `?` with nothing to check against: a fresh metavariable stands in as its type, so the goal still reaches zonk's report (instead of dying with `CannotInfer` during elaboration) and shows the undetermined stand-in.
    let error = compile("?", None).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("? : ?"), "unexpected error: {error}");
    // No `? =` clause: nothing determined the goal.
    assert!(!error.contains("? ="), "unexpected error: {error}");
}

#[test]
fn several_goals_report_together_in_declaration_order() {
    // Two written goals in different declarations: one elaboration reports both — in declaration order — instead of stopping at the first.
    let source = r#"
        use /std/{Nat, Bool};
        let m : Nat = ?;
        let b : Bool = ?;
        m
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert_eq!(
        error.matches("goal `?`").count(),
        2,
        "unexpected error: {error}"
    );
    let nat = error.find("? : Nat").expect("the Nat goal is reported");
    let bool_ = error.find("? : Bool").expect("the Bool goal is reported");
    assert!(nat < bool_, "goals out of declaration order: {error}");
}

#[test]
fn item_and_entrypoint_tail_goals_share_one_batch() {
    // A goal in a declaration and a goal in the entrypoint tail: both land in the same report.
    let source = r#"
        use /std/{Nat};
        let m : Nat = ?;
        /std/Nat/add(m, ?)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert_eq!(
        error.matches("goal `?`").count(),
        2,
        "unexpected error: {error}"
    );
}

#[test]
fn solved_and_unsolved_goals_share_one_batch() {
    // An unconstrained goal and a solved one: the batch keeps each entry's own verdict — no solution line for the first, `? = Nat` for the second.
    let source = r#"
        use /std/{Nat};
        let id(A : Type, a : A) -> A = a;
        let m : Nat = ?;
        id(?, 5)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert_eq!(
        error.matches("goal `?`").count(),
        2,
        "unexpected error: {error}"
    );
    assert!(error.contains("? : Nat"), "unexpected error: {error}");
    assert!(error.contains("? : Type"), "unexpected error: {error}");
    assert!(error.contains("? ="), "unexpected error: {error}");
}

#[test]
fn goal_types_spell_operators_as_infix_not_witness_projections() {
    // The concept-dispatch rebuild (`a + b` ≙ a witness projection call — `elaborate_infix`) folds back to its source spelling in reports, nested operands parenthesized, and no anonymous witness name leaks.
    let source = r#"
        use /std/{Nat, Eq};
        let claim : Eq((1 + 2) * 3, 9) = ?;
        claim
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
    assert!(error.contains("(1 + 2) * 3"), "unexpected error: {error}");
    assert!(!error.contains("witness"), "unexpected error: {error}");
}

#[test]
fn a_computed_equality_goal_suggests_refl() {
    // The motivating base case: `? : Eq(0 + 0, 0 * 2)` — the indices unify through reduction, so the report suggests the complete candidate. The step case's indices are distinct stuck terms, so `refl` is filtered there and the step goal gets no constructor suggestion.
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

    assert!(
        error.contains("? \u{2248} Eq/refl()"),
        "unexpected error: {error}"
    );
    // The step case gets no suggestion: its sides are distinct stuck terms, so `refl` fails index validation there.
    assert_eq!(
        error.matches('\u{2248}').count(),
        1,
        "unexpected error: {error}"
    );
}

#[test]
fn impossible_constructors_are_not_suggested() {
    // At `Vec(Nat, 0)` inversion refutes `cons` (a successor target clashes with `0`) and admits `nil` completely.
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (m + 1)
        end
        let v : Vec(Nat, 0) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("? \u{2248} /Vec/nil()"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("cons"), "unexpected error: {error}");
}

#[test]
fn a_scope_binder_fitting_the_goal_is_suggested() {
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = ?;
        f(1)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("? \u{2248} x"), "unexpected error: {error}");
}

#[test]
fn a_solved_goal_gets_no_suggestions() {
    // A suggestion beside a `? =` answer is noise; solved goals carry none.
    let source = r#"
        let id(A : Type, a : A) -> A = a;
        id(?, 5)
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(error.contains("? ="), "unexpected error: {error}");
    assert!(!error.contains('\u{2248}'), "unexpected error: {error}");
}

#[test]
fn a_module_function_fitting_the_goal_is_suggested_with_pinned_arguments() {
    // The application-fit pool: `mk`'s output `Eq(n, n)` unifies with the goal `Eq(3, 3)`, pinning `n := 3` — and the pinned argument displays filled because the candidate is materialized before the transaction rolls back. Complete fits rank by pool order, so the constructor fit leads.
    let source = r#"
        use /std/{Nat, Eq};
        let mk(n : Nat) -> Eq(n, n) = Eq/refl();
        let claim : Eq(3, 3) = ?;
        0
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/refl()"),
        "unexpected error: {error}"
    );
    assert!(error.contains("mk(3)"), "unexpected error: {error}");
    let refl = error.find("\u{2248} Eq/refl()").expect("refl suggested");
    let mk = error.find("mk(3)").expect("mk suggested");
    assert!(refl < mk, "complete pool order broken: {error}");
}

#[test]
fn the_goals_own_definition_is_not_suggested() {
    // Suggesting the definition a goal sits inside would be circular for a plain `let`; the pools exclude the owner. The scope binder still fits.
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = ?;
        f(1)
    "#;

    let error = compile(source, Some("/std/Nat")).unwrap_err();

    assert!(error.contains("? \u{2248} x"), "unexpected error: {error}");
    assert!(!error.contains("/f("), "own definition suggested: {error}");
}

#[test]
fn a_suggested_complete_candidate_compiles_when_pasted() {
    // The paste-and-recheck contract: the candidate suggested for the fixture in `a_computed_equality_goal_suggests_refl`'s base shape compiles.
    let source = r#"
        use /std/{Nat, Eq};
        let claim : Eq(1 + 2, 3) = Eq/refl();
        0
    "#;

    assert!(compile(source, Some("/std/Nat")).is_ok());
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
fn goal_types_spell_negated_equality_as_neq() {
    // `a != b` elaborates as an xor-negated equality call (no `BoolNot` prim exists); the report folds the pair back to `!=`.
    let source = r#"
        use /std/{Nat, Bool, Eq};
        let claim : Eq(1 != 2, true) = ?;
        claim
    "#;

    let error = compile(source, None).unwrap_err();

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

#[test]
fn omitted_motive_mentioning_a_type_param_lowers() {
    // `pick` is polymorphic in `A`, and the `match c` omits its motive. The motive metavar is solved to `A` — a binder local to `pick`'s telescope. zonk must realign that solution to the enclosing binders when it splices it back in; otherwise `A` dangles as a free var after the module is re-closed and `erase` rejects it with `unbound variable`. Guards the zonk binder-realignment fix.
    let source = r#"
        use /std/{Bool};
        let pick(A : Type, a : A, b : A, c : Bool) -> A =
            match c
            | false => a
            | true => b
            end;
        pick(/std/Nat, 1, 2, true)
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn projection_through_a_stuck_inductive_payload_lowers() {
    // `Fmt/print`'s return type is `format_type_with({}, parse(s))`, so erasing `print` evaluates `parse(s)` at compile time with a *symbolic* `s`. The `Parse` combinator's result is a `Result` inductive whose discriminant is therefore stuck, and the inlined `success` payload is reached by a projection. `erase` must lower that projection through the neutral payload `match` (every variant carries the field at the same index) instead of demanding a literal `TupleType`. Guards `projectable_at`; without it this panics `erase: projected a non-tuple`.
    let source = r#"
        use /std/{Fmt, Bytes};
        Fmt/print("% is %")
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn checked_constructor_postpones_a_tuple_under_a_holed_type_arg() {
    // `Result/success((a, a))` checked against a known `Result(...)`. The tuple is an introduction form whose parameter type is the inserted implicit `?A`, so it can't be checked until `?A` is known. Elaboration postpones it, unifies the result against the expected `Result` — solving `?A` (the success type, which the tuple's own result witnesses) and the *phantom* `?E` (the failure type, carried only by the expected type) — then re-checks the tuple. Guards the result-directed argument order in `elaborate_apply`; without it this fails "introduced a tuple where the expected type is not a tuple type".
    let source = r#"
        use /std/{Result};
        use /std/{Nat};
        let f(a : Nat) -> Result({ Nat, Nat }, Nat) =
            Result/success((a, a));
        f(7)
    "#;

    assert!(compile(source, None).is_ok());

    // In infer position nothing pins the holes, so the postponed tuple is re-checked against a still-unsolved metavar and rejected — graceful degradation, no new acceptance of un-annotated constructors.
    let unpinned = r#"
        use /std/{Result};
        Result/success((1, 1))
    "#;

    assert!(compile(unpinned, None).is_err());
}

#[test]
fn inductive_match_arm_arity_is_checked_statically() {
    // Each arm's binder count is checked against the constructor's registry telescope at elaboration time. Under the legacy tagged-tuple desugar this mismatch was silent (the extra binder became an out-of-range payload projection).
    let source = r#"
        use /std/{Result};
        use /std/{Nat, Bytes};
        let f(r : Result(Nat, Bytes)) -> Nat =
            match r : (_) => Nat
            | success(value, extra) => value
            | failure(_) => 0
            end;
        f(Result/success(7))
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(
        error.contains("constructor 'success' takes 1 argument(s) but the match arm binds 2"),
        "unexpected error: {error}"
    );
}

#[test]
fn implicit_arguments_can_all_be_supplied_explicitly() {
    // Every implicit slot can be overridden positionally with a call-site `@` — including an inductive constructor's parameters, which are implicit by default — and the fully-supplied call compiles end-to-end.
    let source = r#"
        use /std/{Nat};
        induct Opt(A : Type) : Type
        | some(A)
        | none()
        end
        let id(@T : Type, x : T) -> T = x;
        match Opt/some(@Nat, id(@Nat, 1)) : (_) => Nat
        | some(value) => value
        | none() => 0
        end
    "#;

    compile(source, None).unwrap();
}

#[test]
fn implicit_argument_is_inserted_and_inferred() {
    // An `@`-marked inductive parameter makes the constructor's type argument implicit, so the call site writes no holes at all.
    let source = r#"
        use /std/{Nat};
        induct Opt(A : Type) : Type
        | some(A)
        | none()
        end
        match Opt/some(1) : (_) => Nat
        | some(value) => value
        | none() => 0
        end
    "#;

    compile(source, None).unwrap();
}

#[test]
fn interleaved_implicit_with_partial_override() {
    // `T` is overridden positionally with `@`, `U` (interleaved after an explicit binder) is inferred from `y`.
    let source = r#"
        use /std/{Nat, Bytes};
        let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
        std/Bytes/len(second(@Nat, 1, /std/Str/to_bytes("abc")))
    "#;

    compile(source, None).unwrap();
}

#[test]
fn implicit_argument_queues_are_order_insensitive() {
    // The two queues are matched independently: an `@`-argument fills the first unfilled implicit binder no matter where it sits among the plain arguments.
    let at_first = r#"
        use /std/{Nat, Bytes};
        let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
        std/Bytes/len(second(@Nat, 1, /std/Str/to_bytes("abc")))
    "#;
    let at_last = r#"
        use /std/{Nat, Bytes};
        let second(@T : Type, x : T, @U : Type, y : U) -> U = y;
        std/Bytes/len(second(1, /std/Str/to_bytes("abc"), @Nat))
    "#;

    compile(at_first, None).unwrap();
    compile(at_last, None).unwrap();
}

#[test]
fn trailing_implicit_is_pinned_by_the_expected_type() {
    // The proof-argument shape: the implicit trails every explicit binder and is mentioned only in the result type, so nothing but the result-directed turnaround can pin it.
    let source = r#"
        use /std/{Nat};
        induct Opt(A : Type) : Type
        | some(A)
        | none()
        end
        let nothing(n : Nat, @T : Type) -> Opt(T) = Opt/none(@T);
        let r : Opt(Nat) = nothing(0);
        match r : (_) => Nat
        | some(value) => value
        | none() => 9
        end
    "#;

    compile(source, None).unwrap();
}

#[test]
fn all_implicit_telescope_saturates_and_retargets() {
    // The curried `bind` shape: `(@A, @B) -> (M A, A -> M B) -> M B`. Applying it directly to plain arguments saturates the all-implicit telescope with fresh metavariables and re-targets the arguments at the next telescope — both through a direct call and the `!` sugar (which sequences through the user's `Monad(Id)` witness).
    let source = r#"
        use /std/{Nat, Monad};
        induct Id(A : Type) : Type
        | wrap(A)
        end
        let bind : (@A : Type, @B : Type) -> (Id(A), (A) -> Id(B)) -> Id(B) =
            (@A, @B) => (m, f) =>
                match m : (_) => Id(B)
                | wrap(x) => f(x)
                end;
        satisfy Monad(Id) {
            pure(@A, x) = Id/wrap(x),
            bind(@A, @B, m, f) = bind(@A, @B)(m, f)
        }
        let direct = bind(Id/wrap(1), (x) => Id/wrap(Nat/succ(x)));
        -- The lambda body is its own region root: the `!` sequences inside
        -- it instead of hoisting into the entrypoint tail (which returns a
        -- bare `Nat`, not an `Id`).
        let sugared_block = () =>
            let v = Id/wrap(3)!;
            Id/wrap(v);
        let sugared = sugared_block();
        match sugared : (_) => Nat
        | wrap(value) =>
            match direct : (_) => Nat
            | wrap(other) => Nat/add(value, other)
            end
        end
    "#;

    compile(source, None).unwrap();
}

#[test]
fn uninferred_implicit_names_the_binder_and_function() {
    // Nothing mentions `T` outside the binder itself, so unification can never pin it; the report must name the hole, not a bare metavar id.
    let source = r#"
        use /std/{Nat};
        let cast(x : Nat, @T : Type) -> Nat = x;
        cast(5)
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(
        error.contains("implicit argument 'T' of '/cast' was not inferred"),
        "unexpected error: {error}"
    );
}

#[test]
fn surplus_implicit_arguments_are_rejected() {
    let source = r#"
        use /std/{Nat};
        let id(@T : Type, x : T) -> T = x;
        id(@Nat, @Nat, 1)
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(
        error.contains("2 '@' argument(s) but the function has only 1 implicit parameter(s)"),
        "unexpected error: {error}"
    );
}

#[test]
fn non_pub_inductive_constructors_are_usable_in_the_declaring_module() {
    // Constructors are exactly as visible as their inductive: a non-`pub` inductive is module-local but fully usable where it is declared.
    let source = r#"
        use /std/{Nat};
        induct Opt : Type
        | none()
        | some(Nat)
        end
        match Opt/some(7) : (_) => Nat
        | none() => 0
        | some(n) => n
        end
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn non_pub_inductive_constructors_stay_private_across_modules() {
    // The same inductive declared inside a submodule is not reachable from the parent: the inductive's own visibility still gates the outside.
    let source = r#"
        pub mod m
            induct Secret : Type
            | hide(/std/Nat)
            end
        end
        m/Secret/hide(7)
    "#;

    assert!(compile(source, None).is_err());
}

#[test]
fn inductive_match_on_a_non_inductive_scrutinee_is_rejected_directly() {
    // With the legacy fallback gone, matching inductive constructors on a non-inductive value reports the real problem instead of a downstream projection error.
    let source = r#"
        use /std/{Nat};
        match 7 : (_) => Nat
        | success(value) => value
        end
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(
        error.contains("matched inductive constructors on a non-inductive type"),
        "unexpected error: {error}"
    );
}

#[test]
fn new_style_inductive_match_lowers_end_to_end() {
    // The same program with correct arities compiles through to wasm: the `Result` declaration takes the primitive-inductive path (InductiveType / Variant / InductiveMatch) and erases back to the legacy tagged-tuple runtime shape.
    let source = r#"
        use /std/{Result};
        use /std/{Nat, Bytes};
        let f(r : Result(Nat, Bytes)) -> Nat =
            match r : (_) => Nat
            | success(value) => value
            | failure(_) => 0
            end;
        f(Result/success(7))
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn indexed_inductive_declares_constructs_and_matches() {
    // Indexed inductives, end to end: an indexed `Vec` declares (head index telescope, named/`@` payload binders, per-case targets), constructs with `@T`/`@m` inferred — `Nat/succ(?m)` unifies against the annotation's `2` — and matches under a constant motive (Rung 0: arms are typed from the constructor telescopes; indices ride along), lowering through to wasm.
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        rec len(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
            match v : (_, _) => Nat
            | nil() => 0
            | cons(@m, x, xs) => Nat/add(len(xs), 1)
            end;
        let v : Vec(Nat, 2) = Vec/cons(10, Vec/cons(20, Vec/nil()));
        len(v)
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn indexed_inductive_without_params_and_unnamed_index_lowers() {
    // The head's index names are optional (`: (Nat)`), and an inductive can be indexed without being parameterized. Targets are arbitrary index expressions — here distinct literals — and conversion compares them pointwise: `Tag(7)` accepts `Tag/b` and the match dispatches on the tag as ever.
    let source = r#"
        use /std/{Nat, Bytes};
        induct Tag : (Nat) -> Type
        | a() : (0)
        | b() : (7)
        end
        let t : Tag(7) = Tag/b();
        match t : (_, _) => Bytes
        | a() => /std/Str/to_bytes("a")
        | b() => /std/Str/to_bytes("b")
        end
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn indexed_inductive_motive_binds_the_index() {
    // The motive `(k, v) => Vec(T, Nat/add(k, m))` binds the length index ahead of the scrutinee; each arm checks against the motive at that case's target index (`0` for nil, `Nat/succ(j)` for cons), and the whole match at the scrutinee's actual index. The cons arm converges via `Nat/add`'s definitional successor peeling.
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        rec append(@T : Type, @n : Nat, @m : Nat, v : Vec(T, n), w : Vec(T, m)) -> Vec(T, Nat/add(n, m)) =
            match v : (k, v) => Vec(T, Nat/add(k, m))
            | nil() => w
            | cons(@j, x, xs) => Vec/cons(x, append(xs, w))
            end;
        let a : Vec(Nat, 2) = Vec/cons(1, Vec/cons(2, Vec/nil()));
        let b : Vec(Nat, 1) = Vec/cons(3, Vec/nil());
        let c : Vec(Nat, 3) = append(a, b);
        0
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn motive_binder_count_is_checked_against_the_index_telescope() {
    // A motive binds the scrutinee's indices and then the scrutinee — two names for a one-index `Vec`. Binding too few or too many is reported as itself, at the motive, rather than as a domain mismatch downstream.
    let inductive_decl = r#"
        use /std/{Nat, Bytes};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
    "#;

    let under = format!(
        r#"{inductive_decl}
        let f(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
            match v : (_) => Nat
            | nil() => 0
            | cons(@m, x, xs) => 1
            end;
        0
    "#
    );
    let error = compile(&under, None).unwrap_err();
    assert!(
        error.contains("motive binds 1 name(s)") && error.contains("needs 2"),
        "unexpected error: {error}"
    );

    let over = format!(
        r#"{inductive_decl}
        let f(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
            match v : (_, _, _) => Nat
            | nil() => 0
            | cons(@m, x, xs) => 1
            end;
        0
    "#
    );
    let error = compile(&over, None).unwrap_err();
    assert!(
        error.contains("motive binds 3 name(s)") && error.contains("needs 2"),
        "unexpected error: {error}"
    );

    // Parameters are not motive binders at all, so the family a written scrutinee-binder annotation names is checked by ordinary conversion: annotating at the wrong parameter is a plain type mismatch.
    let wrong_annotation = format!(
        r#"{inductive_decl}
        let f(@n : Nat, v : Vec(Nat, n)) -> Nat =
            match v : (k, w : Vec(Bytes, k)) => Nat
            | nil() => 0
            | cons(@m, x, xs) => 1
            end;
        0
    "#
    );
    let error = compile(&wrong_annotation, None).unwrap_err();
    assert!(error.contains("mismatch"), "unexpected error: {error}");
}

#[test]
fn index_refinement_learns_inside_the_arm() {
    // Rung B: a scrutinee index that is a stable key is refined to the case's target inside the arm. Three faces of it:
    // - `subst` casts `Vec(Bytes, n)` to `Vec(Bytes, m)` through an `Eq(Nat, n, m)` under a *constant* motive — the equality is learned (`n := z`, `m := z`), not eliminated;
    // - `sym` is J-style elimination from the pattern motive alone;
    // - `f`'s nil arm uses a hypothesis demanding `Vec(T, 0)` — legal because the arm refines `n := 0`.
    let source = r#"
        use /std/{Nat, Bytes};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        induct Eq(A : Type) : (x : A, y : A) -> Type
        | refl(z : A) : (z, z)
        end
        let subst(@n : Nat, @m : Nat, p : Eq(Nat, n, m), v : Vec(Bytes, n)) -> Vec(Bytes, m) =
            match p : (_, _, _) => Vec(Bytes, m)
            | refl(z) => v
            end;
        let sym(@A : Type, @x : A, @y : A, p : Eq(A, x, y)) -> Eq(A, y, x) =
            match p : (s, t, q) => Eq(A, t, s)
            | refl(z) => Eq/refl(z)
            end;
        let zonly(@T : Type, v : Vec(T, 0)) -> Nat = 9;
        let f(@T : Type, @n : Nat, v : Vec(T, n), w : Vec(T, n)) -> Nat =
            match v : (_, _) => Nat
            | nil() => zonly(w)
            | cons(@j, x, xs) => 1
            end;
        let a : Vec(Bytes, 0) = Vec/nil();
        let p : Eq(Nat, 0, 0) = Eq/refl(0);
        let b : Vec(Bytes, 0) = subst(p, a);
        let q : Eq(Nat, 3, 3) = sym(Eq/refl(3));
        f(Vec/nil(@Bytes), Vec/nil())
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn empty_inductive_lowers_and_vacuous_match_eliminates_it() {
    // An inductive may declare zero cases — `False`. Its eliminator is a match with zero arms: every omission is vacuously justified, so the match checks at any motive and lowers through erasure and codegen.
    let source = r#"
        induct False : Type
        end
        let absurd(A : Type, v : False) -> A =
            match v : (_) => A
            end;
        5
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn inversion_prunes_impossible_arms_and_solves_binders() {
    // Rung C: at `Vec(T, Nat/succ(n))` the nil arm's target `0` clashes definitely with the successor spine, so the arm is omitted — checker-verified, no `impossible` keyword — and erase fills its dispatch slot with an unreachable body. In the cons arm the unifier decomposes `Nat/succ(n) ~ Nat/succ(j)` and pins `j := n`, which is what types `xs : Vec(T, j)` at the declared `Vec(T, n)`.
    let source = r#"
        use /std/{Nat, Bytes};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        let first(@T : Type, @n : Nat, v : Vec(T, Nat/succ(n))) -> T =
            match v : (_, _) => T
            | cons(@j, x, xs) => x
            end;
        let rest(@T : Type, @n : Nat, v : Vec(T, Nat/succ(n))) -> Vec(T, n) =
            match v : (_, _) => Vec(T, n)
            | cons(@j, x, xs) => xs
            end;
        let v : Vec(Bytes, 2) = Vec/cons(/std/Str/to_bytes("a"), Vec/cons(/std/Str/to_bytes("b"), Vec/nil()));
        let w : Vec(Bytes, 1) = rest(v);
        first(w)
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn impossible_inductive_arm_lowers_to_unreachable() {
    // The element is a lambda parameter so the scrutinee stays runtime — a fully-constant vector would be folded whole by ersd's `evaluate` pass, and the pruned arm would never reach the lowering this pins.
    let source = r#"
        use /std/{Nat, Bytes};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        let first(@T : Type, @n : Nat, v : Vec(T, Nat/succ(n))) -> T =
            match v : (_, _) => T
            | cons(@j, x, xs) => x
            end;
        (b : Bytes) => first(Vec/cons(b, Vec/nil()))
    "#;

    let (ersd, cont) = compile_printed_stages(source).unwrap();

    assert!(
        ersd.contains("unreachable"),
        "expected Ersd output to contain unreachable, got {ersd}",
    );
    assert!(
        cont.contains("unreachable"),
        "expected Cont output to contain unreachable, got {cont}",
    );
}

#[test]
fn omission_requires_a_definite_clash() {
    // An opaque index proves nothing: omitting nil at `Vec(T, n)` is rejected with the explanation as the error.
    let opaque = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        let f(@T : Type, @n : Nat, v : Vec(T, n)) -> Nat =
            match v : (_, _) => Nat
            | cons(@j, x, xs) => 1
            end;
        0
    "#;
    let error = compile(opaque, None).unwrap_err();
    assert!(
        error.contains("not provably impossible"),
        "unexpected error: {error}"
    );

    // The non-linear refusal — no K through the back door: `same`'s target `(z, z)` constrains two positions with one binder, which the unifier refuses, so the arm stays mandatory even at the plainly-uninhabited `Foo(3, 4)`. The flip side: `diff`'s target `(0, 1)` clashes against literals `(5, 5)` and prunes.
    let nonlinear = r#"
        use /std/{Nat, Bytes};
        induct Foo : (x : Nat, y : Nat) -> Type
        | same(z : Nat) : (z, z)
        | diff() : (0, 1)
        end
        let f(q : Foo(3, 4)) -> Bytes =
            match q : (_, _, _) => Bytes
            | diff() => /std/Str/to_bytes("d")
            end;
        0
    "#;
    let error = compile(nonlinear, None).unwrap_err();
    assert!(
        error.contains("missing arm 'same'"),
        "unexpected error: {error}"
    );

    let prunes = r#"
        use /std/{Nat, Bytes};
        induct Foo : (x : Nat, y : Nat) -> Type
        | same(z : Nat) : (z, z)
        | diff() : (0, 1)
        end
        let g(q : Foo(5, 5)) -> Bytes =
            match q : (_, _, _) => Bytes
            | same(z) => /std/Str/to_bytes("s")
            end;
        g(Foo/same(5))
    "#;
    assert!(compile(prunes, None).is_ok());
}

#[test]
fn indexed_inductive_index_mismatch_is_rejected() {
    // A two-element vector annotated at length 3: the per-case target `Nat/succ(m)` propagates through conversion until the index clash surfaces as an ordinary type mismatch.
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        let v : Vec(Nat, 3) = Vec/cons(10, Vec/cons(20, Vec/nil()));
        0
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
}

#[test]
fn indexed_inductive_targets_are_required_and_arity_checked() {
    // A case of an indexed inductive without its `: (...)` target is a parse error, as is a target whose arity differs from the head's index telescope, or a target on an unindexed inductive.
    let missing = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil()
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        0
    "#;
    let error = missing.parse::<Entrypoint>().unwrap_err();
    assert!(
        format!("{error:?}").contains("must state its index target"),
        "unexpected error: {error:?}"
    );

    let surplus = r#"
        use /std/{Nat};
        induct Pair(A : Type) : Type
        | pair(A, A) : (0)
        end
        0
    "#;
    let error = surplus.parse::<Entrypoint>().unwrap_err();
    assert!(
        format!("{error:?}").contains("declares no indices"),
        "unexpected error: {error:?}"
    );

    let arity = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0, 1)
        | cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
        end
        0
    "#;
    let error = arity.parse::<Entrypoint>().unwrap_err();
    assert!(
        format!("{error:?}").contains("but the head declares 1"),
        "unexpected error: {error:?}"
    );
}

#[test]
fn lambda_argument_postpones_until_a_sibling_pins_its_domain() {
    // `with((pair) => pair.0, xs)`: the inserted implicit `?A` is the lambda's domain *and* `xs`'s element type, but `xs : Lst(?A)` is checked after the lambda. Elaboration must postpone the lambda (its domain is an unsolved metavar, and its body projects `pair.0`) until `xs` pins `?A`, then re-check it. Guards the lambda-domain arm of `blocked_on_metavar`; without it this fails "projected from a non-tuple". Checked at the type-check level — the inference is the point, not lowering. (`with` is local: the std maps take their collection first, which would pin `?A` before the lambda and vacate the scenario.)
    let source = r#"
        use /std/{Lst};
        use /std/{Nat};
        let with(@A : Type, @B : Type, f : (A) -> B, xs : Lst(A)) -> Lst(B) =
            Lst/map(xs, f);
        let first(xs : Lst({ Nat, Nat })) -> Lst(Nat) =
            with((pair) => pair.0, xs);
        first
    "#;

    assert!(typecheck(source).is_ok());
}

#[test]
fn empty_array_postpones_until_a_sibling_pins_its_element_type() {
    // `pick([], cat)`: the inserted implicit `?A` is the empty array's type *and* `combine`'s domain. The empty-array literal `[]` borrows its element type from the expected (check-only intro), so against the bare metavar `?A` it cannot elaborate. Elaboration must postpone it until the sibling `cat` grounds `?A := Lst(?T)`, then re-check — at which point the `Lst(Nat)` result pins `?T`. Exercises the array arm of `blocked_on_metavar`; without it this fails "type mismatch" eagerly. `cat` is declared here rather than taken from `/std` because concatenation is literal syntax, which cannot be passed as a value.
    let source = r#"
        use /std/{Lst};
        use /std/{Nat};
        let cat(@T : Type, a : Lst(T), b : Lst(T)) -> Lst(T) = [..a, ..b];
        let pick(@A : Type, fallback : A, combine : (A, A) -> A) -> A =
            combine(fallback, fallback);
        let go : Lst(Nat) =
            pick([], cat);
        go
    "#;

    assert!(typecheck(source).is_ok());

    // With no sibling to ground the element type and no result type to pin it, the postponed `[]` re-checks against a bare metavar and is rejected — graceful degradation, no new acceptance.
    let unpinned = r#"
        use /std/{Lst};
        let id(@A : Type, x : A) -> A = x;
        let bad = id([]);
        bad
    "#;

    assert!(typecheck(unpinned).is_err());
}

#[test]
fn continuation_postpones_until_the_result_type_pins_its_codomain() {
    // A `!` region whose tail is `Parse/pure((x, x))` — a *bare tuple*, checkable only against a known tuple type. The expected type reaches the tail solely through each bind's result metavar `?B`, which the turnaround solves *after* the continuation is checked. Elaboration must postpone the continuation lambda (its codomain `M(?B)` carries a result metavar) until `expect` grounds `?B` against the concrete `Parse({ Byte, Byte })`, then re-check it. Guards the codomain arm of `blocked_on_metavar`; without it the tail fails "introduced a tuple where the expected type is not a tuple type".
    let source = r#"
        use /std/{Parse};
        use /std/{Byte};
        let pair : Parse({ Byte, Byte }) =
            let x = Parse/any_byte!;
            Parse/pure((x, x));
        pair
    "#;

    assert!(typecheck(source).is_ok());

    // The `expected_ground` gate: with no concrete result type to pin `?B`, the codomain stays a metavar, the continuation is *not* postponed, and the bare tuple is rejected — graceful degradation, no new acceptance.
    let unpinned = r#"
        use /std/{Parse};
        let x = Parse/any_byte!;
        Parse/pure((x, x))
    "#;

    assert!(typecheck(unpinned).is_err());
}

#[test]
fn closure_returning_a_bare_projection_lowers() {
    // A closure whose body *is* a tuple projection (`(pair) => pair.0`), handed to a higher-order function over an empty array, never constructs a tuple anywhere in the module — yet lowering must still emit the arity-1 tuple type the projection reads through. The wasm `Table` sizes its tuple types from the max arity it sees; scanning only tuple *constructions* missed this projection-only arity and panicked "`Table` lacks tuple type for arity `1`". Guards folding projection (`index + 1`) and prealloc arities into that scan.
    let source = r#"
        use /std/{Lst};
        use /std/{Nat};
        Lst/map(@{ Nat, Nat }, @Nat, [], (pair) => pair.0)
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn bare_polymorphic_function_inserts_implicits_in_value_position() {
    // Passing a bare `cat : (@T, Lst T, Lst T) -> Lst T` where an explicit `(Lst Nat, Lst Nat) -> Lst Nat` is expected: the check turnaround (`insert_implicits_on_check`) inserts the implicit `@T` and eta-expands over the explicit binders, so no hand-written `(l, r) => cat(l, r)` wrapper is needed. Lowers end-to-end — the eta-expansion is an ordinary closure over a saturated call.
    let source = r#"
        use /std/{Lst};
        use /std/{Nat};
        let cat(@T : Type, a : Lst(T), b : Lst(T)) -> Lst(T) = [..a, ..b];
        let pairwise(f : (Lst(Nat), Lst(Nat)) -> Lst(Nat), a : Lst(Nat)) -> Lst(Nat) =
            f(a, a);
        pairwise(cat, [1])
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn polymorphic_value_assignment_keeps_its_implicit() {
    // The guard arm: when the *expected* type also leads with an implicit binder, implicit-eta must not fire — the polymorphic function is assigned as-is, implicit intact, and stays applicable at a chosen instance. Without the expected-not-implicit gate this would wrongly eta-expand and fail to convert against the implicit-leading annotation.
    let source = r#"
        use /std/{Lst};
        use /std/{Nat};
        let cat(@T : Type, a : Lst(T), b : Lst(T)) -> Lst(T) = [..a, ..b];
        let g : (@T : Type, Lst(T), Lst(T)) -> Lst(T) = cat;
        g(@Nat, [1], [2])
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn typeless_let_infers_a_literal_body() {
    // A local `let` with no type annotation infers the body's type (`Nat` here) and lowers end-to-end.
    let source = r#"
        let n = 5;
        n
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn typeless_let_binds_an_annotated_closure() {
    // The composite feature: a typeless local `let` binds an annotated closure. The closure's type is synthesized from its annotation (Infer-mode `elaborate_func`), the let's type is inferred from it, and `f(5)` checks and lowers all the way to wasm.
    let source = r#"
        use /std/{Nat};
        let f = (x : Nat) => x;
        f(5)
    "#;

    assert!(compile(source, None).is_ok());
}

#[test]
fn closure_annotation_must_match_the_expected_domain() {
    // In checking position the param annotation is verified against the expected function type's domain — a wrong annotation is a type mismatch.
    let source = r#"
        use /std/{Nat, Bool};
        let f : (Nat) -> Nat = (x : Bool) => x;
        f(5)
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(error.contains("mismatch"), "unexpected error: {error}");
}

#[test]
fn bare_typeless_let_closure_cannot_be_inferred() {
    // Without an annotation there is nothing to infer the domain from, so a typeless `let` binding a bare closure is a `cannot`-infer error.
    let source = r#"
        let f = (x) => x;
        f
    "#;

    let error = compile(source, None).unwrap_err();

    assert!(error.contains("cannot"), "unexpected error: {error}");
}

// --- A: typecheck-only (stop after zonk, no lowering) ---------------------

fn typecheck(source: &str) -> Result<(), String> {
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    super::elaborate_and_zonk(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        &mut |_| {},
    )
    .map(|_| ())
    .map_err(String::from)
}

#[test]
fn typecheck_accepts_a_well_typed_program() {
    // The fast path stops after `elaborate → zonk`; a well-typed program passes without running erase/cont/optimize/wasm.
    assert!(
        typecheck("/std/Handle/write(/std/Handle/stdout, /std/Str/to_bytes(/std/Nat/to_str(0)))")
            .is_ok()
    );
}

#[test]
fn typecheck_rejects_a_goal() {
    // `zonk` is included in the fast path, so a written goal is still reported — type-checking is fully validated even though lowering is skipped.
    let error = typecheck(
        r#"
        use /std/{Nat};
        let m : Nat = ?;
        m
        "#,
    )
    .unwrap_err();

    assert!(error.contains("goal `?`"), "unexpected error: {error}");
}

// --- B2: named tuple fields ----------------------------------------------

#[test]
fn proj_by_label_resolves_to_its_position() {
    // `.label` is elaboration-time sugar for the positional projection, so both spellings typecheck identically.
    let source = r#"
        use /std/{Nat, Bytes, Handle};
        let r : { status : Nat, payload : Bytes } = (0, /std/Str/to_bytes("ok"));
        let by_label : Bytes = r.payload;
        let by_index : Bytes = r.1;
        Handle/write(Handle/stdout, by_label)
    "#;

    assert!(typecheck(source).is_ok());
}

#[test]
fn proj_unknown_label_names_the_available_fields() {
    let source = r#"
        use /std/{Nat, Bytes};
        let r : { status : Nat, payload : Bytes } = (0, /std/Str/to_bytes("ok"));
        r.body
    "#;

    let error = typecheck(source).unwrap_err();
    assert!(
        error.contains("no field named 'body'") && error.contains("status"),
        "unexpected error: {error}"
    );
}

#[test]
fn duplicate_tuple_label_is_rejected() {
    let source = r#"
        use /std/{Nat};
        let r : { x : Nat, x : Nat } = (0, 1);
        r.x
    "#;

    let error = typecheck(source).unwrap_err();
    assert!(
        error.contains("duplicate field label 'x'"),
        "unexpected error: {error}"
    );
}

#[test]
fn tuple_labels_are_part_of_type_identity() {
    // Same positional types, different label order: not convertible — this is what makes `.label` re-indexing impossible.
    let reordered = r#"
        use /std/{Nat};
        let p : { width : Nat, height : Nat } = (640, 480);
        let q : { height : Nat, width : Nat } = p;
        q.width
    "#;
    assert!(typecheck(reordered).is_err());

    // Labeled and unlabeled spellings are distinct types too.
    let unlabeled = r#"
        use /std/{Nat};
        let p : { width : Nat, height : Nat } = (640, 480);
        let q : { Nat, Nat } = p;
        q.0
    "#;
    assert!(typecheck(unlabeled).is_err());
}

#[test]
fn named_construction_checks_against_the_labels() {
    // Written names must match the expected type's labels positionally; bare fields are always accepted.
    let source = r#"
        use /std/{Nat, Bytes};
        let r : { status : Nat, payload : Bytes } = (status = 0, payload = /std/Str/to_bytes("ok"));
        let mixed : { status : Nat, payload : Bytes } = (status = 0, /std/Str/to_bytes("ok"));
        r.status
    "#;
    assert!(typecheck(source).is_ok());

    let wrong_name = r#"
        use /std/{Nat, Bytes};
        let r : { status : Nat, payload : Bytes } = (code = 0, payload = /std/Str/to_bytes("ok"));
        r.status
    "#;
    let error = typecheck(wrong_name).unwrap_err();
    assert!(
        error.contains("'code'") && error.contains("'status'"),
        "unexpected error: {error}"
    );

    let unlabeled_type = r#"
        use /std/{Nat, Bytes};
        let r : { Nat, Bytes } = (status = 0, /std/Str/to_bytes("ok"));
        r.0
    "#;
    assert!(typecheck(unlabeled_type).is_err());
}

#[test]
fn dependent_record_projects_by_label() {
    // Labels bind dependently: a later field's type mentions an earlier label, and label projection re-types through the dependency.
    let source = r#"
        let p : { T : Type, x : T } = (T = /std/Nat, x = 3);
        let v : p.T = p.x;
        /std/Handle/write(/std/Handle/stdout, /std/Str/to_bytes(/std/Nat/to_str(v)))
    "#;

    assert!(typecheck(source).is_ok());
}

#[test]
fn inductive_payload_relying_on_implicit_insertion_is_rebuilt() {
    // The inductive registry used to keep `into_core`'s *lowered* payload and index types, so a type relying on implicit-argument insertion — `Eq(0, 1)` against `Eq`'s 3-ary type constructor — survived under-applied and panicked the `Telescope::open` arity assert the first time reduction met the registry copy. The registry telescopes are now rebuilt during `elaborate_module` (indices while the inductive group's signatures are assumed, constructors once its bodies are defined), so the payload elaborates like any other type.
    let payload = r#"
        induct Eq(@A : Type) : (x : A, y : A) -> Type
        | refl(z : A) : (z, z)
        end
        induct Box : Type
        | mk(p : Eq(0, 1))
        end
        0
    "#;
    assert!(typecheck(payload).is_ok());

    // Index types take the same path — and previously panicked even earlier, while the type-constructor binding itself elaborated (its body's `InductiveType` node checks against the index telescope).
    let index = r#"
        induct Eq(@A : Type) : (x : A, y : A) -> Type
        | refl(z : A) : (z, z)
        end
        induct Tag : (p : Eq(0, 0)) -> Type
        | mk() : (Eq/refl(0))
        end
        0
    "#;
    assert!(typecheck(index).is_ok());

    // End to end: construct and eliminate through the rebuilt registry — the match arm's binder is typed from the rebuilt payload type, and the whole program lowers to wasm.
    let through = r#"
        use /std/{Nat};
        induct Eq(@A : Type) : (x : A, y : A) -> Type
        | refl(z : A) : (z, z)
        end
        induct Box : Type
        | mk(p : Eq(0, 0))
        end
        let b : Box = Box/mk(Eq/refl(0));
        match b : (_) => Nat
        | mk(p) => 7
        end
    "#;
    assert!(compile(through, None).is_ok());
}

#[test]
fn dead_user_definition_is_still_typechecked() {
    // A user-authored top-level binding the body never references is still type-checked (every item is, before any reachability is considered), so its error is reported. (`write` returns its `Nat` status, `Bytes` mismatches.)
    let error = typecheck(
        r#"
        let dead : /std/Bytes = /std/Handle/write(/std/Handle/stdout, /std/Str/to_bytes("x"));
        /std/Handle/write(/std/Handle/stdout, /std/Str/to_bytes("ok"))
        "#,
    )
    .unwrap_err();

    assert!(error.contains("mismatch"), "unexpected error: {error}");
}

/// Elaborate `source` to its meta-free Core module and erase it through the arena path, prelude erased fresh — the Phase-2 erasure vertical.
fn erase_to_ir(source: &str) -> curios_ersd::Module {
    let entrypoint = source.parse::<Entrypoint>().unwrap();
    let (module, core_type, _foreigns) = super::elaborate_and_zonk(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        &mut |_| {},
    )
    .unwrap();
    erase_module(
        &mut Context::with_default_budget(SYNTAX),
        &module,
        &core_type,
    )
    .expect("the elaborated module erases into a verified arena module")
}

#[test]
fn arena_erasure_covers_the_fixed_prelude() {
    // The entrypoint pulls in string formatting, so the erased module carries the whole fixed prelude — every construct the corpus uses — through the arena path, fresh, into one verified module.
    let module = erase_to_ir(r#"/std/Fmt/print("hello")"#);
    assert!(
        module.functions().len() > 100,
        "the fixed prelude erased with the program: {} functions",
        module.functions().len()
    );
}

#[test]
fn arena_erasure_is_deterministic_across_compiles() {
    let source = "/std/Nat/add(20, 22)";
    let first = erase_to_ir(source).to_string();
    let second = erase_to_ir(source).to_string();
    assert_eq!(first, second);
}

#[test]
fn arena_erasure_stores_no_captures_for_the_prelude() {
    // Functions carry no capture lists anywhere in the erased prelude; free values are derived on demand. The analysis on the full module is the witness that derivation covers every function.
    let module = erase_to_ir("/std/Nat/to_str(7)");
    let analysis = Analysis::analyze(&module);
    let counted = module.function_ids().count();
    assert!(counted > 0);
    for function in module.function_ids() {
        let _ = analysis.free_values(function);
    }
}

#[test]
fn arena_erasure_handles_deep_input_on_the_default_stack() {
    // A wide flat block (the shape whose N-deep nesting once overflowed the legacy pipeline); erasure, verification, and printing all stay on the default test-thread stack. Sized so quadratic *elaboration* cost — shared by both paths and out of erasure's scope — stays testable.
    let mut source = String::new();
    for index in 0..500 {
        source.push_str(&format!("let x{index} = {index} + 1;\n"));
    }
    source.push_str("x0");
    let module = erase_to_ir(&source);
    let printed = module.to_string();
    assert!(printed.contains("NatAdd"));
}
