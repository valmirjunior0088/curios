use super::{error, run};

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

#[test]
fn checking_problem_without_a_pin_still_rejects() {
    // A checking problem whose expected type is never pinned drains as a cannot-infer at the tuple's own span — parked, not silently accepted.
    let source = r#"
        use /std/{Nat, Handle};
        let swallow(@A : Type, a : A) -> Nat = 0;
        let n : Nat = swallow((1, 2));
        let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(n)))!;
        /std/Io/pure(())
        "#;

    error(source);
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

// Scrutinee refinement keys on the applied head's *label* (the reducer's Rung-B probe in `reduce`). A concept-dispatched comparison reduces past the `Cmp` wrapper to an intrinsic normal form, which is not an application — so before `head_label` covered intrinsics, `match a <= hi` registered a refinement key the probe could never look up and the arm silently failed to refine, while the equivalent `Nat/lte(a, hi)` spelling worked. Operators must be usable in a proof-carrying position, not just the intrinsic spelling.
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

// A parked checking problem that survives every retry — a tuple against an implicit nothing ever pins — is reported by the item drain at the expression's own span, naming the expected type it waited on rather than the bare `cannot infer` it used to raise.
#[test]
fn unresolvable_parked_check_reports_its_expected_type() {
    let source = r#"
        use /std/{Nat, Str};
        let use_it(@A : Type, a : A) -> Nat = 0;
        let z : Nat = use_it((1, true));
        /std/print(Nat/to_str(z))
        "#;
    let error = error(source);
    assert!(error.contains("never gained structure"), "{error}");
}
