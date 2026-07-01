use {
    curios::{Stage, compile_entrypoint, core, text},
    std::time::{Duration, Instant},
};

fn main() {
    let timeout = Duration::from_secs(15);

    // The complete program from PROOFS_101.md: propositions as types (`Void`,
    // `Not`), the `/std/Eq` kit, induction over `Nat` (`add_zero`), negation
    // by discriminate-and-transport (`zero_is_not_one`), `subst` casting a
    // vector's length index, and a sortedness invariant (`IsSorted`) guarding
    // `search`. The proofs all erase — at runtime the program runs `search`
    // and prints "ok".
    let source = r#"
        use /std/{Nat, Str, Bln, Eq, Lst, Vec, Io};

        induct Void : Type
        end

        let absurd(@A : Type, contradiction : Void) -> A =
            match contradiction : A
            end;

        let Not(P : Type) -> Type = (P) -> Void;

        let trivially_true : {} = ();

        let two_is_two : Eq(2, 2) = Eq/refl();
        let flipped : Eq(2, 2) = Eq/sym(two_is_two);
        let chained : Eq(2, 2) = Eq/trans(two_is_two, flipped);

        let succ_f(n : Nat) -> Nat = Nat/succ(n);

        let add_zero(n : Nat) -> Eq(Nat/add(n, 0), n) =
            match n : (m) => Eq(Nat/add(m, 0), m)
            | 0 => Eq/refl()
            | pred + 1; ih => Eq/cong(succ_f, ih)
            end;

        let IsZero(n : Nat) -> Type =
            match n : Type
            | 0 => {}
            | pred + 1; _ => Void
            end;

        let zero_is_not_one : Not(Eq(0, 1)) =
            (p) => Eq/subst(IsZero, p, ());

        let StrVec(k : Nat) -> Type = Vec(Str, k);

        let cast(@n : Nat, @m : Nat, p : Eq(n, m), v : Vec(Str, n)) -> Vec(Str, m) =
            Eq/subst(StrVec, p, v);

        let single : Vec(Str, 1) = Vec/cons("hi", Vec/nil());
        let recast : Vec(Str, Nat/add(1, 0)) = cast(Eq/sym(add_zero(1)), single);

        let Lte(a : Nat, b : Nat) -> Type =
            match Nat/lte(a, b) : Type
            | true => {}
            | false => Void
            end;

        rec IsSorted(l : Lst(Nat)) -> Type =
            match l : Type
            | nil() => {}
            | cons(x, rest) =>
                match rest : Type
                | nil() => {}
                | cons(y, _) => { Lte(x, y), IsSorted(rest) }
                end
            end;

        let one_two_three : Lst(Nat) = Lst/cons(1, Lst/cons(2, Lst/cons(3, Lst/nil())));
        let sorted_proof : IsSorted(one_two_three) = ((), ((), ()));

        let tail_sorted(@x : Nat, @rest : Lst(Nat), p : IsSorted(Lst/cons(x, rest))) -> IsSorted(rest) =
            match rest : (r) => IsSorted(r)
            | nil() => ()
            | cons(y, tail) => p.1
            end;

        rec search(target : Nat, l : Lst(Nat), sorted : IsSorted(l)) -> Bln =
            match l : Bln
            | nil() => false
            | cons(x, rest) =>
                match Nat/eql(x, target) : Bln
                | true => true
                | false =>
                    match Nat/gt(x, target) : Bln
                    | true => false
                    | false => search(target, rest, tail_sorted(sorted))
                    end
                end
            end;

        let found : Bln = search(2, one_two_three, sorted_proof);

        match found : {}
        | true => Io/print("ok")
        | false => Io/print("unreachable")
        end
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
                Stage::ErsdOptm(_) => "ersd-optm",
                Stage::Cont(_) => "cont",
                Stage::ContOptm(_) => "cont-optm",
                Stage::Wasm(_) => "wasm",
            }
        );
    })
    .expect("expected wasm module");

    let (system, io) = curios_rt::MockHost::builder().build();
    let t = Instant::now();
    curios::run_wasm(&wasm_module, system).expect("expected result");
    println!("run:  {:?}", t.elapsed());
    assert_eq!(io.output(), b"ok");

    // PROOFS_101.md's two rejections. First: a zero-arm match on
    // `Eq(0, 1)` — `refl`'s binder pins both index positions, which is
    // beyond the one-position-per-binder inverter, so the arm omission is
    // refused rather than verified.
    let zero_arm_match = r#"
        use /std/{Nat, Eq};

        induct Void : Type
        end
        let zero_is_not_one(p : Eq(0, 1)) -> Void =
            match p : Void
            end;
        zero_is_not_one
        "#;

    assert_rejected(timeout, zero_arm_match, |error| {
        matches!(error, core::Error::MissingArmNotImpossible { .. })
    });

    // Second: `refl` only inhabits `Eq` at equal indices, so claiming
    // `Eq(2, 3)` is a `TypeMismatch`.
    let unequal_refl = r#"
        use /std/{Nat, Eq};

        let bad : Eq(2, 3) = Eq/refl();
        bad
        "#;

    assert_rejected(timeout, unequal_refl, |error| {
        matches!(error, core::Error::TypeMismatch { .. })
    });

    // Third: claiming `IsSorted` of an unsorted list. The proposition computes
    // to a tuple whose first field is `Lte(3, 1)` — which is `False`, not a
    // tuple type, so the `()` written there is refused.
    let unsorted_claim = r#"
        use /std/{Nat, Lst, False};

        let Lte(a : Nat, b : Nat) -> Type =
            match Nat/lte(a, b) : Type
            | true => {}
            | false => False
            end;

        rec IsSorted(l : Lst(Nat)) -> Type =
            match l : Type
            | nil() => {}
            | cons(x, rest) =>
                match rest : Type
                | nil() => {}
                | cons(y, _) => { Lte(x, y), IsSorted(rest) }
                end
            end;

        let three_one : Lst(Nat) = Lst/cons(3, Lst/cons(1, Lst/nil()));
        let unsorted_proof : IsSorted(three_one) = ((), ());
        unsorted_proof
        "#;

    assert_rejected(timeout, unsorted_claim, |error| {
        matches!(error, core::Error::NotATupleType { .. })
    });
}

fn assert_rejected(timeout: Duration, source: &str, expected: fn(&core::Error) -> bool) {
    let entrypoint = source
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
        Err(core::Error::Located { error, .. }) if expected(error)
    ));
}
