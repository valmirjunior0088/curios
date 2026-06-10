use {
    curios::{Stage, compile_entrypoint, core, text},
    std::time::{Duration, Instant},
};

fn main() {
    let timeout = Duration::from_secs(15);

    // The complete program from PROOFS_101.md: propositions as types (`Void`,
    // `Not`), the `/std/Eq` kit, induction over `Nat` (`add_zero`), negation
    // by discriminate-and-transport (`zero_is_not_one`), and `subst` casting
    // a vector's length index. The proofs all erase — at runtime the program
    // just prints "ok".
    let source = r#"
        use /std/{Nat, Bin, Eq, Vec, Io};

        union Void
        end

        let absurd(@A : Type, contradiction : Void) -> A =
            match contradiction : A
            end;

        let Not(P : Type) -> Type = P -> Void;

        let trivially_true : {} = ();

        let two_is_two : Eq(Nat, 2, 2) = Eq/refl();
        let flipped : Eq(Nat, 2, 2) = Eq/sym(two_is_two);
        let chained : Eq(Nat, 2, 2) = Eq/trans(two_is_two, flipped);

        let succ_f(n : Nat) -> Nat = Nat/succ(n);

        let add_zero(n : Nat) -> Eq(Nat, Nat/add(n, 0), n) =
            match n : (m) => Eq(Nat, Nat/add(m, 0), m)
            | 0 => Eq/refl()
            | pred + 1, ih => Eq/cong(succ_f, ih)
            end;

        let IsZero(n : Nat) -> Type =
            match n : Type
            | 0 => {}
            | pred + 1, _ => Void
            end;

        let zero_is_not_one : Not(Eq(Nat, 0, 1)) =
            (p) => Eq/subst(IsZero, p, ());

        let BinVec(k : Nat) -> Type = Vec(Bin, k);

        let cast(@n : Nat, @m : Nat, p : Eq(Nat, n, m), v : Vec(Bin, n)) -> Vec(Bin, m) =
            Eq/subst(BinVec, p, v);

        let single : Vec(Bin, 1) = Vec/cons("hi", Vec/nil());
        let recast : Vec(Bin, Nat/add(1, 0)) = cast(Eq/sym(add_zero(1)), single);

        Io/print("ok")
        "#;

    let mut last = Instant::now();

    let entrypoint = source
        .parse::<text::Entrypoint>()
        .expect("failed to parse source")
        .with_type("()".parse().unwrap());

    let wasm_module = compile_entrypoint(timeout, &entrypoint, &text::NullLoader, |stage| {
        let now = Instant::now();
        let elapsed = now - last;
        last = now;

        println!(
            "{}: {elapsed:?}",
            match stage {
                Stage::Text(_) => "text",
                Stage::Core(_) => "core",
                Stage::Ersd(_) => "ersd",
                Stage::Cont(_) => "cont",
                Stage::Optm(_) => "optm",
                Stage::Wasm(_) => "wasm",
            }
        );
    })
    .expect("expected wasm module");

    let (system, receiver) = curios::ChannelHost::out();
    let t = Instant::now();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    println!("run:  {:?}", t.elapsed());
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"ok".to_vec()]
    );

    // PROOFS_101.md's two rejections. First: a zero-arm match on
    // `Eq(Nat, 0, 1)` — `refl`'s binder pins both index positions, which is
    // beyond the one-position-per-binder inverter, so the arm omission is
    // refused rather than verified.
    let zero_arm_match = r#"
        use /std/{Nat, Eq};

        union Void
        end
        let zero_is_not_one(p : Eq(Nat, 0, 1)) -> Void =
            match p : Void
            end;
        zero_is_not_one
        "#;

    assert_rejected(timeout, zero_arm_match, |error| {
        matches!(error, core::Error::MissingArmNotImpossible { .. })
    });

    // Second: `refl` only inhabits `Eq` at equal indices, so claiming
    // `Eq(Nat, 2, 3)` is a `TypeMismatch`.
    let unequal_refl = r#"
        use /std/{Nat, Eq};

        let bad : Eq(Nat, 2, 3) = Eq/refl();
        bad
        "#;

    assert_rejected(timeout, unequal_refl, |error| {
        matches!(error, core::Error::TypeMismatch { .. })
    });
}

fn assert_rejected(timeout: Duration, source: &str, expected: fn(&core::Error) -> bool) {
    let entrypoint = source
        .parse::<text::Entrypoint>()
        .expect("failed to parse ill-typed source");

    let module = text::to_core(&entrypoint, &text::prelude(&text::NullLoader))
        .expect("expected lowered module");
    let result =
        core::elaborate_module(&mut core::Context::new(timeout), &module, core::Mode::Infer);

    assert!(matches!(
        &result,
        Err(core::Error::Located { error, .. }) if expected(error)
    ));
}
