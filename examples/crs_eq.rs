use {
    curios::{Stage, compile_entrypoint, core, text},
    std::time::{Duration, Instant},
};

fn main() {
    let timeout = Duration::from_secs(15);

    // Propositional equality from `/std/Eq`: build a proof with `Eq/refl`,
    // flip and chain it with `sym`/`trans`, lift it through a function with
    // `cong`, and transport a value under a predicate with `subst`. The
    // proofs do all their work at compile time — at run time they are erased
    // and the program just prints "ok".
    let source = r#"
        use /std/{Nat, Eq, Io};

        let p : Eq(2, 2) = Eq/refl();
        let s : Eq(2, 2) = Eq/sym(p);
        let t : Eq(2, 2) = Eq/trans(p, s);

        let bump(n : Nat) -> Nat = Nat/add(n, 1);
        let c : Eq(bump(2), bump(2)) = Eq/cong(bump, p);

        let K(n : Nat) -> Type = Nat;
        let v : Nat = Eq/subst(K, p, 7);

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

    let (system, receiver) = curios::MockHost::out();
    let t = Instant::now();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    println!("run:  {:?}", t.elapsed());
    assert_eq!(
        receiver.try_iter().collect::<Vec<_>>(),
        vec![b"ok".to_vec()]
    );

    // `refl` only inhabits `Eq` at equal indices: claiming `Eq(2, 3)`
    // is a compile-time `TypeMismatch`.
    let ill_typed = r#"
        use /std/{Nat, Eq};

        let bad : Eq(2, 3) = Eq/refl();
        bad
        "#;

    let entrypoint = ill_typed
        .parse::<text::Entrypoint>()
        .expect("failed to parse ill-typed source");

    let (module, metavars) = text::to_core(&entrypoint, &text::prelude(&text::NullLoader))
        .expect("expected lowered module");
    let result = core::elaborate_module(
        &mut core::Context::new(timeout),
        &module,
        metavars,
        core::Mode::Infer,
    );

    assert!(matches!(
        &result,
        Err(core::Error::Located { error, .. })
            if matches!(error.as_ref(), core::Error::TypeMismatch { .. })
    ));
}
