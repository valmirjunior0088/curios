use {curios_runtime::MockHost, std::time::Duration};

#[test]
fn match_omitted_motive_infers() {
    // The same induction as `triangular_sum`, but with the motive omitted. It is
    // non-dependent (every arm has type `std/Nat`), so the synthesized metavar
    // motive is solved by the arms — no explicit `: std/Nat` needed.
    let source = r#"
        let result : std/Nat =
            match 5
            | 0 => 0
            | pred + 1; ih => std/Nat/add(ih, pred)
            end;
        std/Handle/write(std/Handle/stdout, /std/Str/to_bytes(std/Nat/to_str(result)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"10");
}

#[test]
fn implicit_inductive_type_param_executes() {
    // A `@`-marked inductive parameter is implicit at the type constructor too:
    // `Eq2(2, 2)` infers `A` from the indices, `Eq2(@Nat, 3, 3)` pins it, and
    // the eliminator's motive type-pattern still spells every slot. Running
    // (not just checking) also guards metavariable spines through the
    // Π-domain close/reopen round trip: a solved implicit type-arg's solution
    // names a sibling binder, and without the delayed substitution the two
    // spellings of the same domain compare as distinct.
    let source = r#"
        use /std/{Nat, Bytes, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (q : Eq2(A, s, t)) => Eq2(t, s)
            | refl(@z) => Eq2/refl()
            end;
        let pinned : Eq2(@Nat, 3, 3) = Eq2/refl();
        let proof : Eq2(2, 2) = Eq2/refl();
        let inferred : Eq2(2, 2) = sym2(proof);
        match inferred : {}
        | refl(@z) => let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(z))); ()
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

#[test]
fn implicit_inductive_type_param_rejects_explicit_spelling() {
    // With `@A` implicit, the old explicit spelling queues `Nat` into the
    // explicit slots — one argument too many, an error rather than a silent
    // reinterpretation. (`Eq2(@Nat, 2, 2)` is the pinned spelling.)
    let source = r#"
        use /std/{Nat, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(Nat, 2, 2) = Eq2/refl();
        Handle/write(Handle/stdout, /std/Str/to_bytes("no"))
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

#[test]
fn parked_constraints_let_nested_constructor_metas_resolve() {
    // `sym2(Eq2/refl())` — the argument's fresh metas meet the domain's fresh
    // metas as flex–flex pairs embedded under the inductive type. Before the
    // constraint store (§8) the argument's `expect` failed at quiescence,
    // seconds before the result-type unification would have pinned
    // everything. Now the pairs park, the output `expect` solves the domain
    // metas against the annotation, and the wake retries the parked pairs.
    let source = r#"
        use /std/{Nat, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (q : Eq2(A, s, t)) => Eq2(t, s)
            | refl(@z) => Eq2/refl()
            end;
        let direct : Eq2(2, 2) = sym2(Eq2/refl());
        let chained : Eq2(3, 3) = sym2(sym2(Eq2/refl()));
        match chained : {}
        | refl(@z) => let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(z))); ()
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"3");
}

#[test]
fn parked_constraints_still_reject_the_unsolvable() {
    // An undecidable-at-first constraint that never resolves must still fail —
    // at the item drain, attributed to its origin. `refl` forces both indices
    // equal; `2` and `3` are not.
    let source = r#"
        use /std/{Nat, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(2, 3) = Eq2/refl();
        Handle/write(Handle/stdout, /std/Str/to_bytes("no"))
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

#[test]
fn omitted_motive_infers_over_a_compound_scrutinee() {
    // The motive hole's scope is opened with the scrutinee — a non-pattern
    // spine entry when the scrutinee is compound. Occurrence abstraction in
    // `solve` rewrites the scrutinee's occurrences in the expected type to
    // the motive binder, so the dependent motive infers where it previously
    // had to be spelled.
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"4");
}

#[test]
fn bare_tuple_continuation_tail_infers() {
    // The recorded dead-end from the result-directed elaboration work: a bare
    // tuple in a monadic continuation's tail, its expected type a metavariable
    // pinned only by the *outer* apply's result unification. The in-apply
    // postponement defers the tuple, the constraint store parks the flex–flex
    // codomain pair across the inner apply, and the outer pin wakes both.
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"104");
}

#[test]
fn checking_problem_parks_until_an_outer_pin_lands() {
    // The constraint store's own window: the inner apply's output expect
    // parks (provisional success), so the postponed tuple re-check meets a
    // still-unsolved expected type — it now parks as a *checking problem*
    // behind a placeholder metavariable, and the outer annotation's pin wakes
    // it. Before ParkedWork::Checking this was a NotATupleType error.
    let source = r#"
        use /std/{Nat, Lst, Handle};
        let mk(@A : Type, a : A) -> Lst(A) = [a];
        let use_(@B : Type, l : Lst(B)) -> Lst(B) = l;
        let v : Lst({ Nat, Nat }) = use_(mk((1, 2)));
        match v : {}
        | [] => ()
        | [p, ..rest] => let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(p.1))); ()
        end
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}

#[test]
fn checking_problem_without_a_pin_still_rejects() {
    // A checking problem whose expected type is never pinned drains as a
    // cannot-infer at the tuple's own span — parked, not silently accepted.
    let source = r#"
        use /std/{Nat, Handle};
        let swallow(@A : Type, a : A) -> Nat = 0;
        let n : Nat = swallow((1, 2));
        Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(n)))
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

// Regression: an `Eq/subst` whose motive contains `Eq(_, _)` — whose `@A` is
// implicit — must insert that implicit when the motive is instantiated. It used
// to drop it, leaving `Eq` (a 3-telescope `@A, x, y`) applied to 2 args, which
// panicked `reduce_apply` with "telescope arity mismatch".
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

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
}

// A `rec` that never reduces (`go : Bool = go`) forces forever when demanded in
// type position — same infinite-spin behavior as a top-level `rec` — so a
// short reduce deadline preempts it with a timeout error rather than hanging.
#[test]
fn nonproductive_inner_rec_in_type_position_is_preempted() {
    let source = r#"
        use /std/{Bool};
        let spin : Bool =
            rec go : Bool = go;
            go;
        let bad : Type =
            match spin : Type
            | true => {}
            | false => {}
            end;
        let x : bad = ();
        0
        "#;

    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(1), source, system).is_err());
}

// The flex-apply imitation rule: an implicit higher-kinded binder `@M` is
// inferred from an argument's concrete type — `?M(?A) ≡ Lst(Nat)` commits
// `?M := (A) => Lst(A)` and `?A := Nat` — where previously only the explicit
// `apply_m(@Lst, l)` spelling checked.
#[test]
fn higher_kinded_implicit_infers_by_imitation() {
    let source = r#"
        use /std/{Nat, Lst, Handle, Str};
        pub let apply_m(@M : (Type) -> Type, @A : Type, x : M(A)) -> M(A) = x;
        let l : Lst(Nat) = [1, 2];
        let k : Lst(Nat) = apply_m(l);
        /std/print(Nat/to_str(Lst/len(k)))
        "#;

    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"2");
}
