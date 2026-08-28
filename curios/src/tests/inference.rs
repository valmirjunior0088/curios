use super::{error, run, typecheck};

#[test]
fn an_implicit_solves_against_a_reduction_through_a_let() {
    // `Eq/refl()`'s implicit must be solved against `through(x)`, whose weak-head form is a match stuck on `0 < x` with arms mentioning the `let`-bound `y` — which the reducer splays into a context definition rather than substituting. The scope check once hard-failed that spelling as an out-of-scope name, so this program refused with a type mismatch; the reification loop in `solve` now unfolds the definition back into the candidate.
    let source = r#"
        use /std/{Nat, Eq, Handle, Str};

        let through(x : Nat) -> Nat =
            let y = x + 1;
            match 0 < x
            | true => y
            | false => y + 1
            end;

        let probe(x : Nat) -> Eq(through(x), through(x)) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"ok");
}

#[test]
fn match_omitted_motive_infers() {
    // The same induction as `triangular_sum`, but with the motive omitted. It is non-dependent (every arm has type `std/Nat`), so the synthesized metavar motive is solved by the arms — no explicit `: std/Nat` needed.
    let source = r#"
        let result : std/Nat =
            match 5
            | 0 => 0
            | pred + 1; ih => std/Nat/add(ih, pred)
            end;
        let _ = std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Nat/to_str(result)))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"10");
}

#[test]
fn implicit_inductive_type_param_executes() {
    // A `@`-marked inductive parameter is implicit at the type constructor too: `Eq2(2, 2)` infers `A` from the indices, `Eq2(@Nat, 3, 3)` pins it, and the eliminator's motive type-pattern still spells every slot. Running (not just checking) also guards metavariable spines through the Π-domain close/reopen round trip: a solved implicit type-arg's solution names a sibling binder, and without the delayed substitution the two spellings of the same domain compare as distinct.
    let source = r#"
        use /std/{Nat, Bytes, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (s, t, q) => Eq2(t, s)
            | refl(@z) => Eq2/refl()
            end;
        let pinned : Eq2(@Nat, 3, 3) = Eq2/refl();
        let proof : Eq2(2, 2) = Eq2/refl();
        let inferred : Eq2(2, 2) = sym2(proof);
        match inferred : (_, _, _) => /std/Io({})
        | refl(@z) => let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(z)))!; /std/Io/pure(())
        end
        "#;

    assert_eq!(run(source), b"2");
}

#[test]
fn implicit_inductive_type_param_rejects_explicit_spelling() {
    // With `@A` implicit, the old explicit spelling queues `Nat` into the explicit slots — one argument too many, an error rather than a silent reinterpretation. (`Eq2(@Nat, 2, 2)` is the pinned spelling.)
    let source = r#"
        use /std/{Nat, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(Nat, 2, 2) = Eq2/refl();
        let _ = Handle/write(Handle/stdout, /std/Str/to_bytes("no"))!;
        /std/Io/pure(())
        "#;

    error(source);
}

#[test]
fn parked_constraints_let_nested_constructor_metas_resolve() {
    // `sym2(Eq2/refl())` — the argument's fresh metas meet the domain's fresh metas as flex–flex pairs embedded under the inductive type. Before the constraint store, the argument's `expect` failed at quiescence, seconds before the result-type unification would have pinned everything. Now the pairs park, the output `expect` solves the domain metas against the annotation, and the wake retries the parked pairs.
    let source = r#"
        use /std/{Nat, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (s, t, q) => Eq2(t, s)
            | refl(@z) => Eq2/refl()
            end;
        let direct : Eq2(2, 2) = sym2(Eq2/refl());
        let chained : Eq2(3, 3) = sym2(sym2(Eq2/refl()));
        match chained : (_, _, _) => /std/Io({})
        | refl(@z) => let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(z)))!; /std/Io/pure(())
        end
        "#;

    assert_eq!(run(source), b"3");
}

#[test]
fn parked_constraints_still_reject_the_unsolvable() {
    // An undecidable-at-first constraint that never resolves must still fail — at the item drain, attributed to its origin. `refl` forces both indices equal; `2` and `3` are not.
    let source = r#"
        use /std/{Nat, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(2, 3) = Eq2/refl();
        let _ = Handle/write(Handle/stdout, /std/Str/to_bytes("no"))!;
        /std/Io/pure(())
        "#;

    error(source);
}

#[test]
fn omitted_motive_infers_over_a_compound_scrutinee() {
    // The motive hole's scope is opened with the scrutinee — a non-pattern spine entry when the scrutinee is compound. Occurrence abstraction in `solve` rewrites the scrutinee's occurrences in the expected type to the motive binder, so the dependent motive infers where it previously had to be spelled.
    let source = r#"
        use /std/{Nat, Vec, Handle};
        rec build(n : Nat) -> Vec(Nat, n) =
            match n : (m) => Vec(Nat, m)
            | 0 => Vec/nil()
            | pred + 1; ih => Vec/cons(0, ih)
            end;
        let d(k : Nat) -> Vec(Nat, Nat/add(k, k)) =
            match Nat/add(k, k)
            | 0 => Vec/nil()
            | pred + 1; ih => build(Nat/succ(pred))
            end;
        /std/print(Nat/to_str(Vec/len(d(2))))
        "#;

    assert_eq!(run(source), b"4");
}

#[test]
fn bare_tuple_continuation_tail_infers() {
    // The recorded dead-end from the result-directed elaboration work: a bare tuple in a monadic continuation's tail, its expected type a metavariable pinned only by the *outer* apply's result unification. The in-apply postponement defers the tuple, the constraint store parks the flex–flex codomain pair across the inner apply, and the outer pin wakes both.
    let source = r#"
        use /std/{Parse, Byte, Nat, Bytes, Handle};
        let pairer : Parse({ Byte, Byte }) =
            Parse/bind(Parse/any_byte, (a) => Parse/pure((a, a)));
        rec with_sugar : Parse({ Byte, Byte }) =
            let a = Parse/any_byte!;
            Parse/pure((a, 0));
        match Parse/run(pairer, /std/Str/to_bytes("hi"))
        | success(pair) => /std/print(Nat/to_str(Byte/to_nat(pair.0)))
        | failure(_) => /std/print("error")
        end
        "#;

    assert_eq!(run(source), b"104");
}

#[test]
fn checking_problem_parks_until_an_outer_pin_lands() {
    // The constraint store's own window: the inner apply's output expect parks (provisional success), so the postponed tuple re-check meets a still-unsolved expected type — it now parks as a *checking problem* behind a placeholder metavariable, and the outer annotation's pin wakes it. Before ParkedWork::Checking this was a NotATupleType error.
    let source = r#"
        use /std/{Nat, List, Handle};
        let mk(@A : Type, a : A) -> List(A) = [a];
        let use_(@B : Type, l : List(B)) -> List(B) = l;
        let v : List({ Nat, Nat }) = use_(mk((1, 2)));
        match v : (_) => /std/Io({})
        | [] => /std/Io/pure(())
        | [p, ..rest] => let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(p.1)))!; /std/Io/pure(())
        end
        "#;

    assert_eq!(run(source), b"2");
}

// A tuple argument that is the *only* thing determining a call's type variable, where the result type does not mention it — so the apply's turnaround pins nothing and the literal is left holding its own expectation. The force tier settles it there, inside the call, rather than leaving it for the item's drain.
#[test]
fn a_tuple_argument_no_caller_pins_settles_to_its_product() {
    let source = r#"
        use /std/{Nat, Handle};
        let swallow(@A : Type, a : A) -> Nat = 0;
        let n : Nat = swallow((1, 2));
        let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(n)))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"0");
}

// Regression: an `Eq/subst` whose motive contains `Eq(_, _)` — whose `@A` is implicit — must insert that implicit when the motive is instantiated. It used to drop it, leaving `Eq` (a 3-telescope `@A, x, y`) applied to 2 args, which panicked `reduce_apply` with "telescope arity mismatch".
#[test]
fn subst_motive_inserts_implicit_in_eq() {
    let source = r#"
        use /std/{Eq, Nat, Handle};
        let g(n : Nat) -> Nat = n;
        let lemma(@a : Nat, @b : Nat, p : Eq(a, b)) -> Eq(g(a), g(b)) =
            Eq/subst((x) => Eq(g(a), g(x)), p, Eq/refl());
        let _ = lemma;
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// A `rec` that never reduces (`go : Bool = go`) forces forever when demanded in type position — same infinite-spin behavior as a top-level `rec` — so a step budget stops it with an error rather than hanging.
#[test]
fn nonproductive_inner_rec_in_type_position_exhausts_its_budget() {
    let source = r#"
        use /std/{Bool};
        let spin : Bool =
            rec go : Bool = go;
            go;
        let bad : Type =
            match spin : (_) => Type
            | true => {}
            | false => {}
            end;
        let x : bad = ();
        0
        "#;

    error(source);
}

// The flex-apply imitation rule: an implicit higher-kinded binder `@M` is inferred from an argument's concrete type — `?M(?A) ≡ List(Nat)` commits `?M := (A) => List(A)` and `?A := Nat` — where previously only the explicit `apply_m(@List, l)` spelling checked.
#[test]
fn higher_kinded_implicit_infers_by_imitation() {
    let source = r#"
        use /std/{Nat, List, Handle, Str};
        pub let apply_m(@M : (Type) -> Type, @A : Type, x : M(A)) -> M(A) = x;
        let l : List(Nat) = [1, 2];
        let k : List(Nat) = apply_m(l);
        /std/print(Nat/to_str(List/len(k)))
        "#;

    assert_eq!(run(source), b"2");
}

// A postponed argument keeps its *raw* surface spelling when `elaborate_apply` opens the rest of the telescope, and that spelling is load-bearing: reducing through it is what lets the result `expect` pin the metavariables the slot is waiting on. But `elaborate_proj` only resolves a label projection on the *checked* form, so beta-reducing a raw lambda body through the result type manufactures `head.label` where the settled spelling is `head.index` — a term `reduce_proj` once declared `unreachable!`. The result `expect` is now two-phase: best-effort through the raw spelling, then authoritative through the settled arguments.
#[test]
fn postponed_lambda_projecting_by_label_elaborates() {
    let source = r#"
        use /std/{Nat, Eq};
        struct Boxed : pub Type {
            value : Nat
        }
        let cong_value(@s : Boxed, @t : Boxed, p : Eq(s, t)) -> Eq(s.value, t.value) =
            Eq/cong((b : Boxed) => b.value, p);
        let boxed : Boxed = Boxed { value = 7 };
        let same : Eq(boxed, boxed) = Eq/refl();
        let lifted : Eq(boxed.value, boxed.value) = cong_value(same);
        /std/print(Nat/to_str(boxed.value))
        "#;

    assert_eq!(run(source), b"7");
}

// Scrutinee refinement keys on the applied head's *label* (the reducer's Rung-B probe in `reduce`). A concept-dispatched comparison reduces past the `Cmp` wrapper to an intrinsic normal form, which is not an application — so before `head_label` covered intrinsics, `match a <= hi` registered a refinement key the probe could never look up and the arm silently failed to refine, while the equivalent `Nat/le(a, hi)` spelling worked. Operators must be usable in a proof-carrying position, not just the intrinsic spelling.
#[test]
fn operator_scrutinee_refines_a_proof_carrying_arm() {
    let source = r#"
        use /std/{Nat, Option, True, False};
        let AtMost(a : Nat, hi : Nat) -> Prop =
            match a <= hi : (_) => Prop
            | false => False
            | true => True
            end;
        let certify(a : Nat, hi : Nat) -> Option(AtMost(a, hi)) =
            match a <= hi
            | false => Option/none()
            | true => Option/some(True/qed())
            end;
        match certify(3, 9)
        | some(_) => /std/print("refined")
        | none() => /std/print("no")
        end
        "#;

    assert_eq!(run(source), b"refined");
}

// A parked checking problem that survives every retry is reported by the item drain at the expression's own span, naming the expected type it waited on rather than the bare `cannot infer` it used to raise.
//
// The subject is a lambda rather than a tuple, and that is the whole distinction: a tuple has a product to fall back on and settles, a lambda has no domain to invent and cannot. Both once produced this message, which is what made them look like one problem.
#[test]
fn unresolvable_parked_check_reports_its_expected_type() {
    let source = r#"
        use /std/{Nat, Str};
        let use_it(@A : Type, a : A) -> Nat = 0;
        let z : Nat = use_it((x) => x);
        /std/print(Nat/to_str(z))
        "#;
    let error = error(source);
    assert!(error.contains("never gained structure"), "{error}");
}

/// A guard's refinement discharges a window bound whose spelling sits one definitional step away: the slice obligation states its end as `0 + n`, the guard can only spell `n <= List/len(l)`, and the probe-time canonicalization brings the two together. The regression this pins: the refinement store records a universes-erased key, and erasure strips the `UniverseInst` a polymorphic global (`List/len`) unfolds through — so canonicalizing the *erased* key stalled where the goal side reduced, and the bound reported as an uninferred implicit against a caller who had established it. The canonicalization now reduces the unerased original stored beside the key.
#[test]
fn a_guard_discharges_a_bound_spelled_one_reduction_away() {
    let source = r#"
        use /std/{Nat, List, Str, Handle};
        let take(l: List(Nat), n: Nat) -> List(Nat) =
            match n <= List/len(l) | true => List/slice(l, 0, n) | false => [] end;
        /std/print(Str/concat(Nat/to_str(List/len(take([1, 2, 3], 2))), "\n"))
        "#;
    assert_eq!(run(source), b"2\n");
}

/// The map-wall coda's elaboration-runaway record, resolved. The pathology — a `rec` over a packed accumulator called at a *literal* depth, scrutinised by any comparison under a `match`, spinning elaboration past twenty minutes at ×4 per +2 of depth with flat RSS — was named by stack sampling on 2026-08-20: every sample sat in `Term::any_metavar` under `Context::reduce`'s cache-write gate. Each unfolding substitutes the accumulator into two positions, so reduction results are linear DAGs with exponential tree expansions, and the walk's only prune — the cached `has_metavar` bit — was defeated by the metavariables the results name, so every cache write re-paid the full expansion, uncharged by the budget. The cure is the visited set in `Term::any_metavar`, pinned structurally by curios-core's `any_metavar_visits_a_shared_subterm_once` and end to end by the sibling test below. What this test pins is the other repair the hunt made: `Context::within_allowance` swallowed a *declaration's* exhaustion as an ordinary allowance bail whenever the remainder was below the cap, letting elaboration continue at zero budget — re-raised now, so a budget too small to finish the depth-30 chain refuses loudly instead of spinning.
#[test]
fn a_literal_depth_packed_recursion_refuses_within_a_small_budget() {
    let source = r#"
        use /std/{Nat, List, Bits, Bool, Str, Handle, proc};
        let taint = List/len(proc/args!);
        let t: Bool = taint == 0;
        let grown = b[t, 1];
        rec widen(n: Nat, acc: Bits) -> Bits =
            match n | 0 => acc | _ => widen(n - 1, b[..acc, t]) end;
        let wide = widen(30, grown);
        match Bits/len(wide) == 32
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;
    assert!(super::typecheck_within(1_000, source).is_err());
}

/// The runaway pathology end to end, at the depth that used to spin past twenty minutes: with the deduped metavariable walk the chain elaborates, compiles, and runs within the default budget. Kept beside the small-budget probe above so the pair states both directions — a budget too small refuses loudly, the default one finishes.
#[test]
fn a_literal_depth_packed_recursion_compiles_within_the_default_budget() {
    let source = r#"
        use /std/{Nat, List, Bits, Bool, Str, Handle, proc};
        let taint = List/len(proc/args!);
        let t: Bool = taint == 0;
        let grown = b[t, 1];
        rec widen(n: Nat, acc: Bits) -> Bits =
            match n | 0 => acc | _ => widen(n - 1, b[..acc, t]) end;
        let wide = widen(30, grown);
        match Bits/len(wide) == 32
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;
    assert_eq!(run(source), b"ok\n");
}

#[test]
fn a_goal_in_a_local_let_annotation_is_reported() {
    // `let y : ? = e` inside a body lowers to the same bare metavariable the typeless `let y = e` does, marked as a goal. `elaborate_let` used to take the typeless let's inference path for both, which discarded the goal unelaborated — nothing was left for zonk to report, and a program with a `?` in it compiled. The goal must take the annotation path, where the check solves it and the report carries the solution, exactly as a top-level `let y : ? = e` is reported.
    let source = r#"
        use /std/{Nat};

        let g(x : Nat) -> Nat =
            let y : ? = x + 1;
            y;

        /std/print("unreachable\n")
        "#;
    let error = typecheck(source).expect_err("a program with a written goal never compiles");
    assert!(error.contains("goal `?`"), "{error}");
    assert!(error.contains("? = Nat"), "{error}");
}

#[test]
fn a_typeless_local_let_still_infers_its_body() {
    // The positive control for the fix above: an absent annotation is the origin-less hole, and keeps the inference path — a lambda body needs it, since checking a lambda against an unsolved hole would park and never resolve.
    let source = r#"
        use /std/{Nat};

        let g(x : Nat) -> Nat =
            let f = (n : Nat) => n + 1;
            f(x);

        match g(1) == 2
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;
    assert_eq!(run(source), b"ok\n");
}

#[test]
fn a_goal_as_a_lambda_domain_is_reported() {
    // `elaborate_func_infer` refuses a lambda whose domain nothing pins, and used to refuse a written `?` domain the same way — "cannot infer" where the author had asked a question. Only a silent hole is refused; the goal rides on, is solved by the application, and is reported with its solution.
    let source = r#"
        use /std/{Nat};

        let h = (x : ?) => x;
        let r = h(1);

        /std/print("unreachable\n")
        "#;
    let error = typecheck(source).expect_err("a program with a written goal never compiles");
    assert!(error.contains("goal `?`"), "{error}");
    assert!(error.contains("? = Nat"), "{error}");
}

#[test]
fn a_goal_as_a_match_motive_is_reported() {
    // An elided motive is synthesized from the arms, and a written `?` motive used to count as elided — synthesized over, never elaborated, never reported, and the program compiled. It is a user-written motive the author is asking about: checked against the eliminator's motive type and reported.
    let source = r#"
        use /std/{Nat, Bool};

        let g(b : Bool) -> Nat =
            match b : ? | true => 1 | false => 0 end;

        /std/print("unreachable\n")
        "#;
    let error = typecheck(source).expect_err("a program with a written goal never compiles");
    assert!(error.contains("goal `?`"), "{error}");
}

// A non-empty tuple literal parks against a bare expected metavariable, and rightly: a dependent telescope can only ever arrive from the expectation, so committing to the non-dependent product while one could still arrive would be a guess. When the drain has established that nothing is left to send one, the guess is no longer a guess — and `?` must answer, as it already does for `()` and for a list literal.
#[test]
fn a_tuple_literal_synthesizes_when_its_expected_type_never_gains_structure() {
    let source = r#"
        let y : ? = (1, true);
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("? = {Nat, Bool}"),
        "expected the goal to report the synthesized product:\n{report}"
    );
}

// The one-field literal takes the same route. Its trailing comma is all that separates it from a parenthesized term, so a reader has no other way to learn which one the elaborator saw.
#[test]
fn a_one_field_tuple_literal_synthesizes_against_a_written_goal() {
    let source = r#"
        let y : ? = (1,);
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("? = {Nat}"),
        "expected a one-field product:\n{report}"
    );
}

// Settling the literal wakes whatever was parked on the metavariable it solved, and a woken obligation reports for itself. Here that is the missing tuple witness — the answer the program deserves, where before the same program said only that some type never gained structure.
#[test]
fn a_settled_tuple_reports_the_obligation_it_unblocked() {
    let source = r#"
        use /std/{Bool, Show, Str};
        let s : Str = Show/show((true, false));
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("no witness of Show({Bool, Bool})"),
        "expected the witness goal the tuple unblocked:\n{report}"
    );
}

// An expectation that *does* arrive must still win: the product here is written, and the literal is checked against it rather than synthesized.
#[test]
fn a_written_tuple_type_still_checks_the_literal_against_itself() {
    let source = r#"
        use /std/{Nat, Bool, Handle};
        let id(@A : Type, a : A) -> A = a;
        let z : {Nat, Bool} = id((1, true));
        /std/print(Nat/to_str(z.0))
        "#;

    assert_eq!(run(source), b"1");
}
