//! The programs the reduction suites build a sequence or a literal at the type level in, and what a program costs each checker, which the bench's measurements of reduction read too.

use {
    curios_common::fill, curios_core::Consumption, curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource, curios_pipeline::test_support::*, curios_surface::Entrypoint,
};

/// Both: the same transitions as the arm above, over an accumulator that grows by ten bytes a step. Its excess over that arm is constructed payload and nothing else.
pub fn bytes_growing(n: usize) -> String {
    fill!(
        r#"
        use /std/{Bytes, Nat, Str};
        let go(i : Nat, acc : Bytes) -> Bytes =
            match i | 0 => acc | k + 1 => go(k, 0x[..acc, ..Str/to_bytes("0123456789")]) end;
        let built = go(${n}, 0x[]);
        let head = Bytes/slice(built, 0, 10);
        /std/print("ok")
        "#,
        n
    )
}

/// [`bytes_growing`] over the `List` carrier.
pub fn list_growing(n: usize) -> String {
    fill!(
        r#"
        use /std/{List, Nat, Str};
        let go(i : Nat, acc : List(Nat)) -> List(Nat) =
            match i | 0 => acc | k + 1 => go(k, [..acc, ..[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]) end;
        let built = go(${n}, []);
        let head = List/slice(built, 0, 10);
        /std/print("ok")
        "#,
        n
    )
}

/// A `Str` literal of `n` identical ASCII characters, bound and used `uses` times.
///
/// The literal lowers to `Str { bytes = <Bytes>, @valid = True/qed() }`, and checking that proof makes conversion decide `True ≡ Valid(b)` — `Valid`'s unfolding once, then a `rec` unfold, a `Bytes` peel, a `Byte/to_nat`, `classify`'s ladder and an inductive match, per byte. Nothing else in the program costs anything, so what a floor over this reports is the check.
pub fn str_literal(n: usize, uses: usize) -> String {
    let literal = "0123456789".repeat(n.div_ceil(10))[..n].to_string();
    let used = (0..uses)
        .map(|index| format!("let use{index} = Str/to_bytes(s);\n"))
        .collect::<String>();

    fill!(
        r#"
        use /std/{Str, Bytes};
        let s : Str = "${literal}";
        ${used}
        /std/print("ok")
        "#,
        literal,
        used
    )
}

/// What both checkers spend on `source`, each reporting its own heaviest declaration.
///
/// Reported rather than bisected. A budget floor found from outside costs one whole compile per probe, reports only the larger of the two checkers, and cannot separate depth from the rest at all — which is the separation that matters, because depth is the one row whose size is set by the reduction *strategy* rather than by the term.
pub fn declaration_cost(source: &str) -> (Consumption, Consumption) {
    let entrypoint = source.parse::<Entrypoint>().expect("the program parses");
    let checked = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .expect("the program elaborates within the default budget");
    let elaborator = checked.consumption;
    let module = &checked.program;
    let (verdicts, heaviest) = recheck_with_prelude_measured(module, DEFAULT_BUDGET, |certifier| {
        certifier.heaviest_declaration()
    });

    assert!(
        verdicts.is_empty(),
        "the certifier accepts it: {verdicts:?}"
    );

    (elaborator, heaviest)
}
