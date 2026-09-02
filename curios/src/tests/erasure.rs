use {
    super::{error, run},
    curios_pipeline::Stage,
    curios_pipeline::compile_with_prelude,
    curios_text::{Entrypoint, RootSource},
};

#[test]
fn prop_irrelevance_equates_distinct_proofs() {
    // Definitional proof irrelevance: `Le` is a strict proposition, so any two proofs of `Le(a, b)` are convertible — `refl` checks at `Eq(p, q)` even though `p` and `q` are distinct binders. Without the `Prop` short-circuit in `convert`, `refl : Eq(p, p)` would not check against `Eq(p, q)`.
    let source = r#"
        use /std/{Handle, Str, Eq, Nat};
        let irrelevant(a : Nat, b : Nat, p : Nat/Le/Ind(a, b), q : Nat/Le/Ind(a, b))
            -> Eq(p, q) =
            Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn data_is_not_proof_irrelevant() {
    // The bound on irrelevance: `Nat` is data, not a proposition, so distinct values are never equated — `refl` at `Eq(x, y)` for unequal `x`, `y` is rejected. Guards the `Prop` short-circuit against over-firing on non-props.
    let source = r#"
        use /std/{Handle, Str, Eq, Nat};
        let bad(x : Nat, y : Nat) -> Eq(x, y) = Eq/refl();
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn let_bound_unit_effect_survives_erasure() {
    // `{}` is unit, not a prop, so it is kept — a unit-typed effect must run after erasure. `/std/print` returns `{}`; binding its first call to an unused `let` must not drop the "a" write. Guards that the empty tuple stays runtime content (the `Sort::of` empty-tuple-is-`Type` rule).
    let source = r#"
        use /std/{Handle};
        let first = /std/print("a")!;
        /std/print("b")
        "#;
    assert_eq!(run(source), b"ab");
}

#[test]
fn large_elimination_of_a_prop_is_rejected() {
    // The large-elimination guard: `Le` is a multi-constructor proposition, so matching it into `Nat` (data) would observe which constructor it was, breaking irrelevance — rejected. The permitted cases (empty `False` via `absurd`, singleton `Eq` via `subst`, and prop→prop) are exercised by std.
    let source = r#"
        use /std/{Handle, Str, Nat};
        let bad(a : Nat, b : Nat, p : Nat/Le/Ind(a, b)) -> Nat =
            match p : (_) => Nat
            | z(_) => 0
            | s(_, _, _) => 1
            end;
        let _ = Handle/write(Handle/stdout, Str/to_bytes("ok"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn erased_param_unused_is_accepted() {
    // A type-valued parameter erases (sort-driven), yet at runtime the call still behaves normally — the dropped argument never affects the relevant result.
    let source = r#"
        use /std/{Handle, Str, Nat};
        let f : (T : Type, m : Nat) -> Nat = (T, m) => m;
        let r : Nat = f(Nat, 3);
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(r)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"3");
}

#[test]
fn erased_param_is_dropped_at_runtime() {
    // Sort-driven erasure drops type/proof binders and their arguments. `g`'s type-valued `n` does not exist at runtime, yet `g` passes it to `h`'s type-valued `m`. This runs ONLY if both the parameter and the argument are dropped — otherwise `h(n)` would reference a variable that erase removed from `g`, a dangling runtime reference.
    let source = r#"
        use /std/{Handle, Str, Nat};
        let h : (m : Type) -> Nat = (m) => 0;
        let g : (n : Type) -> Nat = (n) => h(n);
        let r : Nat = g(Nat);
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(r)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"0");
}

#[test]
fn erased_struct_field_collapses_to_bare_value() {
    // A record with a type-valued field is a newtype — the erasable field is dropped and the struct collapses to its single relevant field (bare `Nat` here). `make` fills the erasable `ghost` with its own type-valued `n`, which only works if both are dropped. Projecting `.val` off the collapsed record must still yield `val`, not `ghost`.
    let source = r#"
        use /std/{Handle, Str, Nat};
        struct Wrap : pub Type { val : Nat, ghost : Type }
        let make : (n : Type) -> Wrap = (n) => Wrap { val = 5, ghost = n };
        let r : Nat = make(Nat).val;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(r)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"5");
}

#[test]
fn char_and_str_certificates_erase_to_their_existing_carriers() {
    let source = r#"
        use /std/{Char, Str};
        let certificates = (Char/to_nat('😀'), Str/to_bytes("é😀"));
        /std/Io/pure(())
        "#;
    let entrypoint = source.parse::<Entrypoint>().expect("source parses");
    let mut ersd = None;

    compile_with_prelude(
        curios_pipeline::DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::Ersd(module) = stage {
                ersd = Some(format!("{module}"));
            }
        },
    )
    .expect("source compiles");

    let ersd = ersd.expect("ersd stage observed");
    assert!(
        ersd.contains("$/std/Char/to_nat(128512)"),
        "Char literal did not collapse to its Nat carrier:\n{ersd}"
    );
    assert!(
        ersd.contains("$/std/Str/to_bytes(x\"c3a9f09f9880\")"),
        "Str literal did not collapse to its packed Bytes carrier:\n{ersd}"
    );
}

#[test]
fn erased_tuple_field_is_a_subset_type() {
    // The anonymous Σ form of the same idea: `(val : Nat, Type)` is a subset type whose type-valued witness is dropped, collapsing to the bare relevant field. Here `make` puts its type-valued `n` in the erasable second field.
    let source = r#"
        use /std/{Handle, Str, Nat};
        let make : (n : Type) -> { val : Nat, Type } = (n) => (5, n);
        let r : Nat = make(Nat).0;
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(r)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"5");
}

#[test]
fn erased_definition_param_is_dropped_at_runtime() {
    // The def-form sugar `let f(n : Type) -> R = body`: `g`'s type-valued `n` is dropped and passed to `h`'s type-valued `m` (runs only if both are dropped).
    let source = r#"
        use /std/{Handle, Str, Nat};
        let h(m : Type) -> Nat = 0;
        let g(n : Type) -> Nat = h(n);
        let r : Nat = g(Nat);
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(r)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"0");
}

#[test]
fn erased_inductive_payload_is_dropped_at_runtime() {
    // A type-valued constructor payload field erases — dropped from the runtime variant tuple. `make` fills the erasable `ghost` with its own type-valued `n` (runs only if both are dropped); the match binds only the relevant `val`, so its projection must skip the absent field.
    let source = r#"
        use /std/{Handle, Str, Nat};
        induct Boxed : Type
        | box(ghost : Type, val : Nat)
        end
        let make : (n : Type) -> Boxed = (n) => Boxed/box(n, 5);
        let get : (b : Boxed) -> Nat = (b) =>
            match b : (_) => Nat
            | box(ghost, val) => val
            end;
        let r : Nat = get(make(Nat));
        let _ = Handle/write(Handle/stdout, Str/to_bytes(Nat/to_str(r)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"5");
}

#[test]
fn erased_void_discharges_to_relevant_result() {
    let source = r#"
        use /std/{False, Handle};
        let absurd(@A : Type, c : False) -> A = match c end;
        let direct(@A : Type, c : False) -> A = match c : (_) => A end;
        let via_absurd(@A : Type, c : False) -> A = absurd(c);
        let proofs = (direct, via_absurd);
        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn erased_indexed_relevant_repro() {
    // A type-indexed inductive whose payload is type-valued (sort-erased): the index binder `m : Type` is dropped, yet `Box(m)` stays a well-formed type.
    let source = r#"
        use /std/{Nat, Handle};
        induct Box : (n : Type) -> Type
        | mk(x : Type) : (x)
        end
        let f(m : Type, k : Nat) -> Box(m) =
            let go =
                match k : (k) => (d : Nat) -> Box(m)
                | 0 => (d) => Box/mk(m)
                | kp + 1; ih => (d) => Box/mk(m)
                end;
            go(0);
        let g = f;
        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// Regression: a type-valued *application* (`Box(m)`, here `absurd`'s explicit `@A`) is indexed by an erased binder `m`. It must erase to a unit like any type; erasing it structurally used to leave `m` in the runtime term, so codegen demanded a value for an erased binder ("`into_cont` lacks value").
#[test]
fn erased_index_in_type_valued_arg() {
    let source = r#"
        use /std/{Nat, False, Handle};
        induct Box : (n : Type) -> Type
        | mk(x : Type) : (x)
        end
        let absurd(@A : Type, c : False) -> A = match c end;
        let id_void(w : False) -> False = w;
        let f(m : Type) -> (False) -> Box(m) = (v) => absurd(@Box(m), id_void(v));
        let g = f;
        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// Regression: a `Prop` family is proof-irrelevant, so erasure drops its inhabitants wholesale. Classifying `Eq`'s `refl(@z : A)` payload on its own abstract `A` used to keep it, so rebuilding the constructor computed the field from binders the same erasure had dropped — `Eq/cong` erased to `apply f(unit)`, and a proof bound in a statement position (which the design runs regardless of sort) fed that unit to a `Bits` fold and trapped.
#[test]
fn proof_bound_as_a_statement_does_not_run_its_certificate() {
    let source = r#"
        use /std/{BigNat, Nat, Str, Eq, Handle};
        let a : BigNat = BigNat/of_nat(6);
        let b : BigNat = BigNat/of_nat(7);
        let p : Eq(BigNat/add(a, b), BigNat/add(b, a)) = BigNat/add/comm(a, b);
        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// The same law in an erased position stays non-strict: it is never evaluated, and the consumer still typechecks against it.
#[test]
fn proof_in_an_erased_position_is_not_evaluated() {
    let source = r#"
        use /std/{BigNat, Nat, Str, Eq, Handle};
        let a : BigNat = BigNat/of_nat(6);
        let b : BigNat = BigNat/of_nat(7);
        let consume(x : BigNat, y : BigNat, p : Eq(BigNat/add(x, y), BigNat/add(y, x))) -> Nat = 42;
        /std/print(Nat/to_str(consume(a, b, BigNat/add/comm(a, b))))
        "#;
    assert_eq!(run(source), b"42");
}

#[test]
fn a_dependent_proof_payload_leaves_its_constructor_a_bare_tag() {
    // The declaration mask and the application walk agree that a `(P: Prop, proof: P)` payload pair drops: the construction carries no arguments, and the emitted module verifies.
    assert_eq!(
        run(r#"
        use /std/{Nat, Eq, Io};
        pub induct Box: pub Type
        | theorem(P: Prop, proof: P)
        | other(Nat)
        end
        let t: Box = Box/theorem(Eq(1 + 1, 2), Eq/refl());
        match t
        | theorem(P, p) => /std/print("proved\n")
        | other(n) => /std/print("other\n")
        end
        "#),
        b"proved\n"
    );
}

#[test]
fn a_kept_field_survives_beside_a_dependent_proof_payload() {
    // Declaration arity one, application arity one: the proposition and its proof drop on both sides of the seam while the `Nat` rides through.
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, True, Io};
        pub induct Rec: pub Type
        | mixed(n: Nat, @P: Prop, proof: P)
        end
        let r: Rec = Rec/mixed(7, True/qed());
        match r | mixed(n, @P, p) => /std/print(Str/concat(Nat/to_str(n), "\n")) end
        "#),
        b"7\n"
    );
}

#[test]
fn a_function_of_only_proofs_is_called_with_nothing() {
    // A signature every parameter of which erases has runtime arity zero, and its call sites agree.
    assert_eq!(
        run(r#"
        use /std/{Nat, Eq, Io};
        pub induct Box: pub Type
        | tag()
        end
        let prove(P: Prop, proof: P) -> Box = Box/tag();
        let b: Box = prove(Eq(2 * 21, 42), Eq/refl());
        match b | tag() => /std/print("called\n") end
        "#),
        b"called\n"
    );
}

// The seam in the other direction, found on 2026-09-01 while landing `Nat/Lt/strong`. `app`'s declaration keeps `f`, whose declared type is `Type`-valued, but the call in `pf` instantiates `P` at a proposition, so the application side erases the lambda to `unit` as the proof it has become — and `app` applies `unit`. A proof bound in a statement position runs regardless of sort, so the trap is reachable from ordinary code. Ignored until the two sides agree on a `Prop` instantiation of a `Type`-valued parameter; the compiler-side fix is the item, and this is its acceptance check.
#[test]
#[ignore]
fn a_prop_instantiation_of_a_type_valued_parameter_is_erased_on_both_sides() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, True};
        let app(P: (Nat) -> Type, f: (k: Nat) -> P(k), n: Nat) -> P(n) = f(n);
        let pf(n: Nat) -> Nat/Le(n, n) = app((k) => Nat/Le(k, k), (k) => True/qed(), n);
        let _: Nat/Le(3, 3) = pf(3);
        /std/print("ok")
        "#),
        b"ok"
    );
}
