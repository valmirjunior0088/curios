//! The candidates suggested to fill a goal: which pools they come from, which are withheld, and that a complete one compiles when pasted.

use super::test_support::*;

#[test]
fn a_computed_equality_goal_suggests_refl() {
    // The motivating base case: `? : Eq()(0 + 0, 0 * 2)` — the indices unify through reduction, so the report suggests the complete candidate. The step case gets it too: a sum is a linear combination, so `(p + 1) + (p + 1)` and `(p + 1) * 2` both reduce to `2 · p + 2` — the whole theorem is computation, which makes the fixture a probe of the suggestion rather than of its filtering.
    let source = r#"
        use /std/{Nat, Eq};
        let double(n : Nat) -> Nat = n + n;
        let double_correct(n : Nat) -> Eq()(double(n), n * 2) =
            match n
            | 0 => ?
            | p + 1 => ?
            end;
        let subject : /std/Nat = double(21);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/refl()"),
        "unexpected error: {error}"
    );
    // One refl line per arm, which is why the count is of refl lines and not of every candidate line.
    assert_eq!(
        error.matches("\u{2248} Eq/refl()").count(),
        2,
        "unexpected error: {error}"
    );
}

#[test]
fn impossible_constructors_are_not_suggested() {
    // At `Vec(Nat)(0)` inversion refutes `cons` (a successor target clashes with `0`) and admits `nil` completely.
    let source = r#"
        use /std/{Nat};
        induct Vec(T : Type) : (n : Nat) -> Type
        | nil() : (0)
        | cons(@m : Nat, x : T, xs : Vec(T)(m)) : (m + 1)
        end
        let v : Vec(Nat)(0) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("? \u{2248} Vec/nil()"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("cons"), "unexpected error: {error}");
}

#[test]
fn a_scope_binder_fitting_the_goal_is_suggested() {
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = ?;
        let subject : /std/Nat = f(1);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("? \u{2248} x"), "unexpected error: {error}");
}

#[test]
fn a_solved_goal_gets_no_suggestions() {
    // A suggestion beside a `? =` answer is noise; solved goals carry none.
    let source = r#"
        let id(A : Type, a : A) -> A = a;
        let subject : /std/Nat = id(?, 5);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("` inferred"), "unexpected error: {error}");
    assert!(!error.contains('\u{2248}'), "unexpected error: {error}");
}

#[test]
fn a_module_function_fitting_the_goal_is_suggested_with_pinned_arguments() {
    // The application-fit pool: `mk`'s output `Eq()(n, n)` unifies with the goal `Eq()(3, 3)`, pinning `n := 3` — and the pinned argument displays filled because the candidate is materialized before the transaction rolls back. Complete fits rank by pool order, so the constructor fit leads.
    let source = r#"
        use /std/{Nat, Eq};
        let mk(n : Nat) -> Eq()(n, n) = Eq/refl();
        let claim : Eq()(3, 3) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

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
fn an_application_fit_mentioning_a_scope_binder_is_suggested() {
    // The same fit as above, but the goal sits inside a function body and mentions its binder: `mk`'s output `Eq()(n, n)` unifies with `Eq()(k, k)` by `n := k`. The suggestion pass assumes the goal's telescope into a frame first: run on the bare context, `n`'s metavariable would be born closed and the solution `k` would fail the solver's scope check, so every application fit inside a function body would silently vanish.
    let source = r#"
        use /std/{Nat, Eq};
        let mk(n : Nat) -> Eq()(n, n) = Eq/refl();
        let claim(k : Nat) -> Eq()(k, k) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("mk(k)"), "unexpected error: {error}");
}

#[test]
fn a_module_function_written_below_the_goal_is_suggested() {
    // A goal is reported once every declaration of the unit has elaborated, so what fits it is drawn from the unit as it is written, wherever the goal stands in it: `claim` elaborates before `mk`, which nothing above it reads.
    let source = r#"
        use /std/{Nat, Eq};
        let claim : Eq()(3, 3) = ?;
        let mk(n : Nat) -> Eq()(n, n) = Eq/refl();
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("mk(3)"), "unexpected error: {error}");
}

#[test]
fn two_goals_are_each_offered_the_others_definition() {
    // A definition that holds a goal is read by its type alone, so it fits another goal as any definition does, and neither report waits on the other goal being filled.
    let source = r#"
        use /std/{Nat, Eq};
        let left(k : Nat) -> Eq()(k, k) = ?;
        let right(k : Nat) -> Eq()(k, k) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();
    let reports: Vec<&str> = error.split("\n\n").collect();
    let left = reports
        .iter()
        .find(|report| report.contains("let left("))
        .expect("the first goal reports");
    let right = reports
        .iter()
        .find(|report| report.contains("let right("))
        .expect("the second goal reports");

    assert!(left.contains("right(k)"), "unexpected error: {error}");
    assert!(right.contains("left(k)"), "unexpected error: {error}");
}

#[test]
fn a_goal_in_a_witness_is_offered_a_module_function() {
    // A witness that holds a goal is taken out of the unit, since it is read by more than its type, and its goal is still reported over the unit: `render` is a definition the method's body never mentions.
    let source = r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        let render(n : Nat) -> Str = Nat/to_str(n);
        satisfy Show(Nat) {
            show(n) = ?
        }
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("render(n)"), "unexpected error: {error}");
}

#[test]
fn an_imported_lemma_is_suggested_with_its_proof_slot_filled_from_the_scope() {
    // Pool 5 and the scope fill together. `Eq/sym` is never mentioned by the program — it arrives through `use /std/{Eq}` — and its explicit slot is a proof the goal cannot pin; the output pins `x := k, y := c`, and `h : Eq()(k, c)` is the one binder whose type then fits the slot. The complete fit leads; `Eq/cong(?, h)` follows as the refinement whose function is open. Spelled `Eq/sym`, the path the import resolves under, not the `/std/Eq/sym` Core holds. The type is a parameter, which decides no equality: at `Nat` the equation follows from `h` by arithmetic, and the report leads with `proved()` in the place of a fit.
    let source = r#"
        use /std/{Eq};
        let flip(@T : Type, k : T, c : T, h : Eq()(k, c)) -> Eq()(c, k) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/sym(h)"),
        "unexpected error: {error}"
    );
    assert!(
        error.contains("? \u{2248} Eq/cong(?, h)"),
        "unexpected error: {error}"
    );
    let sym = error.find("Eq/sym(h)").expect("sym suggested");
    let cong = error.find("Eq/cong(?, h)").expect("cong suggested");
    assert!(sym < cong, "complete fit should lead: {error}");
}

#[test]
fn a_hypothesis_fills_an_imported_lemmas_proof_slot_under_an_open_function() {
    // The step case of a proof about two functions the normalizer cannot unfold on a variable: `double(s(p))` against `twice(z(), s(p))` is two successors over each call at `p`, two functions of two shapes conversion holds apart, which is not refl, and `Eq/cong`'s output `Eq()(f(x), f(y))` is undecided on `f` alone once `ih`, the call at the predecessor, pins `x` and `y` — undecided on exactly the open slot, which is the advisory the report keeps. The vacuous `Eq/sym(?)` and `Eq/trans(?, ?)`, true of every equation, are not offered. The naturals are the program's own, which decide no equality: at `Nat` the step follows from `ih` by arithmetic, and the report leads with `proved()`.
    let source = r#"
        use /std/{Eq};
        induct N : Type | z() | s(N) end
        let double(n : N) -> N = match n | z() => N/z() | s(p) => N/s(N/s(double(p))) end;
        let twice(from : N, n : N) -> N = match n | z() => from | s(p) => N/s(N/s(twice(from, p))) end;
        let double_correct(n : N) -> Eq()(double(n), twice(N/z(), n)) =
            match n
            | z() => Eq/refl()
            | s(p) =>
                let ih = double_correct(p);
                ?
            end;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/cong(?, ih)"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("Eq/sym("), "vacuous fit offered: {error}");
    assert!(!error.contains("Eq/trans("), "vacuous fit offered: {error}");
    assert!(
        !error.contains("Eq/refl()"),
        "refl cannot close this: {error}"
    );
}

#[test]
fn an_application_fit_nothing_pinned_is_not_suggested() {
    // `touch`'s output `Eq()(y + 1, x + 1)` converts with `Eq()(3, 3)` by pinning only hidden slots; its one explicit slot stays a hole with nothing in scope to fill it. `touch(?)` says no more than `touch` has an argument, so it is dropped — where `mk(3)`, whose explicit slot the goal pinned, is kept (see `a_module_function_fitting_the_goal_is_suggested_with_pinned_arguments`).
    let source = r#"
        use /std/{Nat, Eq};
        let touch(@x : Nat, @y : Nat, e : Eq()(x, y)) -> Eq()(y + 1, x + 1) = Eq/sym(Eq/cong((z) => z + 1, e));
        let claim : Eq()(3, 3) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/refl()"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("touch("), "unpinned fit offered: {error}");
}

#[test]
fn a_suggested_imported_candidate_compiles_when_pasted() {
    // The paste-and-recheck contract for a pool-5 candidate: `Eq/sym(h)` as suggested in `an_imported_lemma_is_suggested_with_its_proof_slot_filled_from_the_scope`.
    let source = r#"
        use /std/{Eq};
        let flip(@T : Type, k : T, c : T, h : Eq()(k, c)) -> Eq()(c, k) = Eq/sym(h);
        /std/Io/pure(())
    "#;

    assert!(compile(source).is_ok());
}

#[test]
fn an_import_is_offered_only_below_its_use() {
    // `use` binds from its own position to the end of its body. The goal above `use /std/{Eq}` cannot paste `Eq/sym(h)` — `Eq` is not a name there — so it is not offered; the same goal below it is.
    let source = r#"
        use /std/{Nat};
        let above(k : Nat, h : /std/Eq/Eq()(k, 7)) -> /std/Eq/Eq()(7, k) = ?;
        use /std/{Eq};
        let below(k : Nat, h : Eq()(k, 7)) -> Eq()(7, k) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();
    let reports: Vec<&str> = error.split("\n\n").collect();
    let above = reports
        .iter()
        .find(|report| report.contains("let above("))
        .expect("the goal above its import reports");
    let below = reports
        .iter()
        .find(|report| report.contains("let below("))
        .expect("the goal below its import reports");

    assert!(
        !above.contains("Eq/sym("),
        "offered above its import: {error}"
    );
    assert!(
        below.contains("? \u{2248} Eq/sym(h)"),
        "unexpected error: {error}"
    );
}

#[test]
fn a_nested_modules_import_stays_in_its_body() {
    // A nested module body starts with no imports and its own `use` binds nothing outside it: the goal inside `M` is offered `Eq/sym(h)`, the goal in the root body — which never imported `Eq` — is not.
    let source = r#"
        use /std/{Nat};
        pub mod M
            use /std/{Nat, Eq};
            pub let inner(k : Nat, h : Eq()(k, 7)) -> Eq()(7, k) = ?;
        end
        let outer(k : Nat, h : /std/Eq/Eq()(k, 7)) -> /std/Eq/Eq()(7, k) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();
    let reports: Vec<&str> = error.split("\n\n").collect();
    let inner = reports
        .iter()
        .find(|report| report.contains("let inner("))
        .expect("the goal inside the module reports");
    let outer = reports
        .iter()
        .find(|report| report.contains("let outer("))
        .expect("the goal in the root body reports");

    assert!(
        inner.contains("? \u{2248} Eq/sym(h)"),
        "unexpected error: {error}"
    );
    assert!(
        !outer.contains("Eq/sym("),
        "leaked out of the module: {error}"
    );
}

#[test]
fn the_goals_own_definition_is_not_suggested() {
    // Suggesting the definition a goal sits inside would be circular for a plain `let`; the pools exclude the owner. The scope binder still fits.
    let source = r#"
        use /std/{Nat};
        let f(x : Nat) -> Nat = ?;
        let subject : /std/Nat = f(1);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("? \u{2248} x"), "unexpected error: {error}");
    assert!(!error.contains("/f("), "own definition suggested: {error}");
}

#[test]
fn a_suggested_complete_candidate_compiles_when_pasted() {
    // The paste-and-recheck contract: the candidate suggested for the fixture in `a_computed_equality_goal_suggests_refl`'s base shape compiles.
    let source = r#"
        use /std/{Nat, Eq};
        let claim : Eq()(1 + 2, 3) = Eq/refl();
        /std/Io/pure(())
    "#;

    assert!(compile(source).is_ok());
}

/// A goal inside an arm is offered what fits it under the arm's guard, as a paste there would be checked: `Eq/refl()` fits `Eq()(b, true)` only where the arm has `b` as `true`. Suggestions are computed after elaboration, with the arm long closed, so they reinstall the refinements the goal was born under beside its telescope; without them the fit would never be offered.
#[test]
fn a_goal_in_an_arm_is_offered_what_fits_under_its_guard() {
    let source = r#"
        use /std/{Bool, Nat, Eq};
        let f(b : Bool) -> Nat =
            match b
            | true =>
                let _p : Eq()(b, true) = ?;
                0
            | false => 1
            end;
        let subject : /std/Nat = f(true);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("? \u{2248} Eq/refl()"),
        "unexpected error: {error}"
    );
}

#[test]
fn a_candidate_of_the_other_sort_is_not_suggested() {
    // A goal's candidates are what `check` admits there, and `check` admits no proposition where a type is wanted: a family into `Type` and a type are offered, a predicate and a proposition are not.
    let source = r#"
        use /std/{Bool};
        let pick(
            is: (b: Bool) -> Prop,
            family: (b: Bool) -> Type,
            p: Prop,
            t: Type,
        ) -> {(Bool) -> Type, Type} = (?, ?);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("? \u{2248} family"),
        "unexpected error: {error}"
    );
    assert!(error.contains("? \u{2248} t"), "unexpected error: {error}");
    assert!(
        !error.contains("? \u{2248} is"),
        "unexpected error: {error}"
    );
    assert!(!error.contains("? \u{2248} p"), "unexpected error: {error}");
}

/// A goal written under two guards holds by its inner guard under the outer, and is offered `proved()`. `q(p(x), y) <= z` is `y <= z` once the outer guard has made `p(x)` true, and the goal is answered standing on the frames it was born under, the inner guard's inside the outer's. With the two registered side by side in one frame the inner one said nothing, and the report read `a proof was found and did not check, which is the compiler's fault`, where a proof written in the goal's place compiled.
#[test]
fn a_goal_under_two_guards_is_offered_the_proof_the_inner_one_gives() {
    let source = r#"
        use /std/{Bool, Nat};
        use /std/Bool/{Holds, True};

        let p(x: Nat) -> Bool = x % 2 == 0;
        let q(c: Bool, y: Nat) -> Nat = match c | true => y | false => 0 end;

        let f(x: Nat, y: Nat, z: Nat) -> Nat =
            match p(x)
            | true =>
                match q(p(x), y) <= z
                | true => let h: Holds(y <= z) = ?; y
                | false => 0
                end
            | false => 0
            end;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("? \u{2248} /std/proved()") && !error.contains("True/qed()"),
        "unexpected error: {error}"
    );
    assert!(
        !error.contains("did not check"),
        "unexpected error: {error}"
    );
}

/// A goal its guard decides is answered as the bound it stands for would be filled had it been left out: the report names the guard it follows from and offers `proved()`, never `True/qed()`, the term the fill writes. Pasted, the call asks the same fill, so it compiles.
#[test]
fn a_goal_its_guard_decides_names_the_guard_and_offers_proved() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{Holds};
        let share(total: Nat, people: Nat, @_: Holds(people > 0)) -> Nat = total / people;
        let tab(total: Nat, guests: Nat) -> Nat =
            match guests > 0 | true => share(total, guests, @?) | false => 0 end;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("it follows from the facts in scope:")
            && error.contains("a guard, `0 < guests`")
            && error.contains("? \u{2248} /std/proved()")
            && !error.contains("True/qed()"),
        "unexpected error: {error}"
    );
    assert!(compile(&source.replace("@?", "@/std/proved()")).is_ok());
}

/// A goal a hypothesis written `@_` proves is answered too: the proof the procedure writes reads a binder no program can name, so offered as the fit it left the report silent, and the call that proves it again names none.
#[test]
fn a_goal_an_unnamed_hypothesis_proves_is_offered_proved() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{Holds};
        let share(total: Nat, people: Nat, @_: Holds(people > 0)) -> Nat = total / people;
        let tab(total: Nat, guests: Nat, @_: Holds(guests >= 2)) -> Nat =
            share(total, guests, @?);
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        error.contains("it follows from the facts in scope:")
            && error.contains("? \u{2248} /std/proved()"),
        "unexpected error: {error}"
    );
    assert!(compile(&source.replace("@?", "@/std/proved()")).is_ok());
}

/// A head whose result is one of its own parameters meets every goal once that parameter is solved to it, so it is offered only with every explicit argument written: at a bound, with a list and an index in scope, `List/get(?, i)` and `List/fold(xs, ?, ?)` say nothing of it.
#[test]
fn a_head_whose_result_is_its_own_parameter_is_offered_only_with_its_arguments_written() {
    let source = r#"
        use /std/{Nat, List};
        let row(xs: List(Nat), i: Nat) -> Nat =
            match i < List/length(xs) | true => List/get(xs, i, @?) | false => 0 end;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(
        !error.contains("List/get(?") && !error.contains("List/fold("),
        "unexpected error: {error}"
    );
}

/// A fit whose hidden bound nothing proves where the goal stands is offered with the bound open, a hole of its own, so the fit offered complete is one whose paste compiles: `List/get(names, i)` is complete under a guard that proves its bound, and written `List/get(names, i, @?)` where nothing does.
#[test]
fn a_fit_whose_bound_nothing_proves_writes_the_bound_open() {
    let open = r#"
        use /std/{Nat, List};
        struct Name: Type { Nat }
        let pick(names: List(Name), i: Nat) -> Name = ?;
        /std/Io/pure(())
    "#;
    let guarded = r#"
        use /std/{Nat, List};
        struct Name: Type { Nat }
        let pick(names: List(Name), i: Nat) -> Name =
            match i < List/length(names) | true => ? | false => Name { 0 } end;
        /std/Io/pure(())
    "#;

    let error = compile(open).unwrap_err();
    assert!(
        error.contains("? \u{2248} List/get(names, i, @?)"),
        "unexpected error: {error}"
    );
    let error = compile(guarded).unwrap_err();
    assert!(
        error.contains("? \u{2248} List/get(names, i)\n") && !error.contains("@?"),
        "unexpected error: {error}"
    );
}
