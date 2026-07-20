use {super::run, curios_runtime::MockHost, std::time::Duration};

#[test]
fn prop_irrelevance_equates_distinct_proofs() {
    // Definitional proof irrelevance: `Lte` is a strict proposition, so any two
    // proofs of `Lte(a, b)` are convertible — `refl` checks at `Eq(p, q)` even
    // though `p` and `q` are distinct binders. Without the `Prop` short-circuit
    // in `convert`, `refl : Eq(p, p)` would not check against `Eq(p, q)`.
    let source = r#"
        use /std/{Io, Str, Eq, Nat};
        let irrelevant(a : Nat, b : Nat, p : Nat/Lte(a, b), q : Nat/Lte(a, b))
            -> Eq(p, q) =
            Eq/refl();
        Io/write(Io/stdout, Str/to_bytes("ok"))
        "#;
    assert_eq!(run(source), b"ok");
}

#[test]
fn data_is_not_proof_irrelevant() {
    // The bound on irrelevance: `Nat` is data, not a proposition, so distinct
    // values are never equated — `refl` at `Eq(x, y)` for unequal `x`, `y` is
    // rejected. Guards the `Prop` short-circuit against over-firing on non-props.
    let source = r#"
        use /std/{Io, Str, Eq, Nat};
        let bad(x : Nat, y : Nat) -> Eq(x, y) = Eq/refl();
        Io/write(Io/stdout, Str/to_bytes("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

#[test]
fn let_bound_unit_effect_survives_erasure() {
    // `{}` is unit, not a prop, so it is kept — a unit-typed effect must run
    // after erasure. `Io/print` returns `{}`; binding its first call to an unused
    // `let` must not drop the "a" write. Guards that the empty tuple stays
    // runtime content (the `Sort::of` empty-tuple-is-`Type` rule).
    let source = r#"
        use /std/{Io};
        let first = Io/print("a");
        Io/print("b")
        "#;
    assert_eq!(run(source), b"ab");
}

#[test]
fn large_elimination_of_a_prop_is_rejected() {
    // The large-elimination guard: `Lte` is a multi-constructor proposition, so
    // matching it into `Nat` (data) would observe which constructor it was,
    // breaking irrelevance — rejected. The permitted cases (empty `False` via
    // `absurd`, singleton `Eq` via `subst`, and prop→prop) are exercised by std.
    let source = r#"
        use /std/{Io, Str, Nat};
        let bad(a : Nat, b : Nat, p : Nat/Lte(a, b)) -> Nat =
            match p : Nat
            | z(_) => 0
            | s(_, _, _) => 1
            end;
        Io/write(Io/stdout, Str/to_bytes("ok"))
        "#;
    let (system, _io) = MockHost::builder().build();
    assert!(crate::run_text(Duration::from_secs(10), source, system).is_err());
}

#[test]
fn erased_param_unused_is_accepted() {
    // A type-valued parameter erases (sort-driven), yet at runtime the call still
    // behaves normally — the dropped argument never affects the relevant result.
    let source = r#"
        use /std/{Io, Str, Nat};
        let f : (T : Type, m : Nat) -> Nat = (T, m) => m;
        let r : Nat = f(Nat, 3);
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"3");
}

#[test]
fn erased_param_is_dropped_at_runtime() {
    // Sort-driven erasure drops type/proof binders and their arguments. `g`'s
    // type-valued `n` does not exist at runtime, yet `g` passes it to `h`'s
    // type-valued `m`. This runs ONLY if both the parameter and the argument are
    // dropped — otherwise `h(n)` would reference a variable that erase removed
    // from `g`, a dangling runtime reference.
    let source = r#"
        use /std/{Io, Str, Nat};
        let h : (m : Type) -> Nat = (m) => 0;
        let g : (n : Type) -> Nat = (n) => h(n);
        let r : Nat = g(Nat);
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"0");
}

#[test]
fn erased_struct_field_collapses_to_bare_value() {
    // A record with a type-valued field is a newtype — the erasable field is
    // dropped and the struct collapses to its single relevant field (bare `Nat`
    // here). `make` fills the erasable `ghost` with its own type-valued `n`,
    // which only works if both are dropped. Projecting `.val` off the collapsed
    // record must still yield `val`, not `ghost`.
    let source = r#"
        use /std/{Io, Str, Nat};
        struct Wrap : pub Type { val : Nat, ghost : Type }
        let make : (n : Type) -> Wrap = (n) => Wrap { val = 5, ghost = n };
        let r : Nat = make(Nat).val;
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"5");
}

#[test]
fn char_and_str_certificates_erase_to_their_existing_carriers() {
    let source = r#"
        use /std/{Char, Str};
        (Char/to_nat('😀'), Str/to_bytes("é😀"))
        "#;
    let entrypoint = source
        .parse::<curios_text::Entrypoint>()
        .expect("source parses");
    let mut ersd = None;

    curios_pipeline::compile_entrypoint(
        Duration::from_secs(15),
        &entrypoint,
        curios_text::RootSource::none(),
        |stage| {
            if let curios_pipeline::Stage::Ersd(module) = stage {
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
    // The anonymous Σ form of the same idea: `(val : Nat, Type)` is a subset type
    // whose type-valued witness is dropped, collapsing to the bare relevant
    // field. Here `make` puts its type-valued `n` in the erasable second field.
    let source = r#"
        use /std/{Io, Str, Nat};
        let make : (n : Type) -> { val : Nat, Type } = (n) => (5, n);
        let r : Nat = make(Nat).0;
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"5");
}

#[test]
fn erased_definition_param_is_dropped_at_runtime() {
    // The def-form sugar `let f(n : Type) -> R = body`: `g`'s type-valued `n` is
    // dropped and passed to `h`'s type-valued `m` (runs only if both are dropped).
    let source = r#"
        use /std/{Io, Str, Nat};
        let h(m : Type) -> Nat = 0;
        let g(n : Type) -> Nat = h(n);
        let r : Nat = g(Nat);
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"0");
}

#[test]
fn erased_inductive_payload_is_dropped_at_runtime() {
    // A type-valued constructor payload field erases — dropped from the runtime
    // variant tuple. `make` fills the erasable `ghost` with its own type-valued
    // `n` (runs only if both are dropped); the match binds only the relevant
    // `val`, so its projection must skip the absent field.
    let source = r#"
        use /std/{Io, Str, Nat};
        induct Boxed : Type
        | box(ghost : Type, val : Nat)
        end
        let make : (n : Type) -> Boxed = (n) => Boxed/box(n, 5);
        let get : (b : Boxed) -> Nat = (b) =>
            match b : Nat
            | box(ghost, val) => val
            end;
        let r : Nat = get(make(Nat));
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(r)))
        "#;
    assert_eq!(run(source), b"5");
}

#[test]
fn erased_void_discharges_to_relevant_result() {
    let source = r#"
        use /std/{False, Io};
        let direct(@A : Type, c : False) -> A = match c : A end;
        let via_absurd(@A : Type, c : False) -> A = False/absurd(c);
        let proofs = (direct, via_absurd);
        Io/print("ok")
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
}

#[test]
fn erased_indexed_relevant_repro() {
    // A type-indexed inductive whose payload is type-valued (sort-erased): the
    // index binder `m : Type` is dropped, yet `Box(m)` stays a well-formed type.
    let source = r#"
        use /std/{Nat, Io};
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
        Io/print("ok")
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
}

// Regression: a type-valued *application* (`Box(m)`, here `False/absurd`'s inferred
// `@A`) is indexed by an erased binder `m`. It must erase to a unit like any type;
// erasing it structurally used to leave `m` in the runtime term, so codegen
// demanded a value for an erased binder ("`into_cont` lacks value").
#[test]
fn erased_index_in_type_valued_arg() {
    let source = r#"
        use /std/{Nat, False, Io};
        induct Box : (n : Type) -> Type
        | mk(x : Type) : (x)
        end
        let id_void(w : False) -> False = w;
        let f(m : Type) -> (False) -> Box(m) = (v) => False/absurd(@Box(m), id_void(v));
        let g = f;
        Io/print("ok")
        "#;
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    assert_eq!(io.output(), b"ok");
}
