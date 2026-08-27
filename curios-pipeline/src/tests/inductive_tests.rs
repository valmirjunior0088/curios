//! Declaring, constructing and matching an inductive family, indexed or not, and the arms inversion prunes.

use curios_text::Entrypoint;

use super::test_support::*;

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

    assert!(compile(source, Some("/std/Nat")).is_ok());
}

#[test]
fn projection_through_a_stuck_inductive_payload_lowers() {
    // `Fmt/print`'s return type is `format_type_with({}, parse(s))`, so erasing `print` evaluates `parse(s)` at compile time with a *symbolic* `s`. The `Parse` combinator's result is a `Result` inductive whose discriminant is therefore stuck, and the inlined `success` payload is reached by a projection. `erase` must lower that projection through the neutral payload `match` (every variant carries the field at the same index) instead of demanding a literal `TupleType`. Guards `projectable_at`; without it this panics `erase: projected a non-tuple`.
    let source = r#"
        use /std/{Fmt, Bytes};
        Fmt/print("% is %")("a")(1)
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
        let r : Result({ Nat, Nat }, Nat) = f(7);
        0
    "#;

    assert!(compile(source, Some("/std/Nat")).is_ok());

    // In infer position nothing pins the holes, so the postponed tuple is re-checked against a still-unsolved metavar and rejected — graceful degradation, no new acceptance of un-annotated constructors. The infer position is a typeless local `let`: the entrypoint tail is always checked now.
    let unpinned = r#"
        use /std/{Result};
        let bad = Result/success((1, 1));
        0
    "#;

    assert!(compile(unpinned, Some("/std/Nat")).is_err());
}

#[test]
fn match_arm_arity_is_checked_statically() {
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

    assert!(compile(source, Some("/std/Nat")).is_ok());
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
fn match_on_a_non_inductive_scrutinee_is_rejected_directly() {
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
    // The same program with correct arities compiles through to wasm: the `Result` declaration takes the intrinsic-inductive path (InductiveType / Variant / InductiveMatch) and erases back to the legacy tagged-tuple runtime shape.
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

    assert!(compile(source, Some("/std/Nat")).is_ok());
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

    assert!(compile(source, Some("/std/Nat")).is_ok());
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

    assert!(compile(source, Some("/std/Bytes")).is_ok());
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

    assert!(compile(source, Some("/std/Nat")).is_ok());
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
    let error = compile(&wrong_annotation, Some("/std/Nat")).unwrap_err();
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

    assert!(compile(source, Some("/std/Nat")).is_ok());
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

    assert!(compile(source, Some("/std/Nat")).is_ok());
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

    assert!(compile(source, Some("/std/Bytes")).is_ok());
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

    let (ersd, cont) = compile_printed_stages(source, Some("(/std/Bytes) -> /std/Bytes")).unwrap();

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
    let error = compile(opaque, Some("/std/Nat")).unwrap_err();
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
    let error = compile(nonlinear, Some("/std/Nat")).unwrap_err();
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
    assert!(compile(prunes, Some("/std/Bytes")).is_ok());
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

    let error = compile(source, Some("/std/Nat")).unwrap_err();

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
fn payload_relying_on_implicit_insertion_is_rebuilt() {
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
    assert!(typecheck(payload, Some("/std/Nat")).is_ok());

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
    assert!(typecheck(index, Some("/std/Nat")).is_ok());

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
    assert!(compile(through, Some("/std/Nat")).is_ok());
}
