//! What is folded back to the operator a reader writes: an abstract witness, an intrinsic and the proof it holds, and a call that unfolds to an operation no reader writes.

use {
    crate::{EntryTail, Refused, test_support::examine_with_prelude, tests::test_support::*},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
};

/// The report of the one mismatch `source` is refused with.
fn refused(source: &str) -> String {
    compile(source).map(|_| ()).unwrap_err()
}

/// A statement `Flt/rounded/add` makes at its rounding: its dispatch reaches an addition `/sys` declares at a path no program writes.
const ROUNDED: &str = r#"
    use /std/{Flt, Eq};
    use /std/Flt/{Rounding};

    pub let f(a: Flt, b: Flt) -> Eq()(Flt/rounded/add(Rounding/toward_zero(), a, b), a) =
        Eq/refl();
"#;

#[test]
fn an_abstract_witness_folds_back_to_its_operator() {
    // Under a `use Add(A)` parameter the projection is stuck on a witness that is a *binderso no amount of reduction reaches the operator: the fold must resolve it through the concept the binder's declared type names, or the report spells `a + a` as `(#6561).0(a, a)`.
    let source = r#"
        use /std/ops/{Add};
        use /std/{Nat, Eq};
        let bad(@A : Type, use Add(A), a : A) -> Eq()(a + a, a + a) = ?;
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("└─ [goal]"), "unexpected error: {error}");
    assert!(error.contains("a + a"), "operator not folded: {error}");
    assert!(
        !error.contains(").0("),
        "witness projection leaked: {error}"
    );
}

#[test]
fn an_abstract_witness_folds_back_in_a_mismatch_too() {
    // The mismatch path denoises by normalizing, which suffices only while the operand type is concrete. The abstract case needs the same structural fold the goal path runs.
    let source = r#"
        use /std/ops/{Add};
        use /std/{Nat, Eq};
        let bad(@A : Type, use Add(A), a : A) -> Eq()(a + a, a) = Eq/refl();
        /std/Io/pure(())
    "#;

    let error = compile(source).unwrap_err();

    assert!(error.contains("type mismatch"), "unexpected error: {error}");
    assert!(error.contains("a + a"), "operator not folded: {error}");
    assert!(
        !error.contains(").0("),
        "witness projection leaked: {error}"
    );
}

/// A report writes an operation that holds the proof of its bound as its reader must write it. Where nothing proves the proof again where the reader stands, the report states it: a division is the call of its function, an operator having no place for a proof, and a narrowing takes it after its operand.
#[test]
fn an_intrinsic_keeps_the_proof_its_reader_cannot_prove_again() {
    let divided = refused(
        r#"
        use /std/{Nat, Eq, Io};

        pub let quot(n: Nat, d: Nat, @ok: Nat/NonZero(d)) -> Nat =
            n / d;

        pub let claim(g: (Nat) -> Nat, h: (m: Nat) -> Nat/NonZero(g(m)), n: Nat, m: Nat) -> Eq()(quot(n, g(m), @h(m)), 7) =
            Eq/refl();

        Io/pure(())
    "#,
    );
    assert!(
        divided.contains("expected: `Eq(@Nat)(Nat/div(n, g(m), @h(m)), 7)`"),
        "unexpected report: {divided}"
    );

    let narrowed = refused(
        r#"
        use /std/{Nat, Byte, Bool, Eq, Io};

        pub let narrow(n: Nat, @ok: Bool/Holds(n < 256)) -> Byte =
            Nat/to_byte(n);

        pub let claim(g: (Nat) -> Nat, h: (m: Nat) -> Bool/Holds(g(m) < 256), m: Nat) -> Eq()(narrow(g(m), @h(m)), Nat/to_byte(7)) =
            Eq/refl();

        Io/pure(())
    "#,
    );
    assert!(
        narrowed.contains("expected: `Eq(@Byte)(Nat/to_byte(g(m), @h(m)), 0x07)`"),
        "unexpected report: {narrowed}"
    );
}

/// Where the bound is a fact in the reader's scope, elaboration proves it again there, so the report writes the division by its operator and the narrowing by its bare call, as a program does.
#[test]
fn an_intrinsic_leaves_out_the_proof_its_reader_proves_again() {
    let divided = refused(
        r#"
        use /std/{Nat, Bool, Eq, Io};

        pub let claim(n: Nat, d: Nat, @positive: Bool/Holds(0 < d)) -> Eq()(n / d, 7) =
            Eq/refl();

        Io/pure(())
    "#,
    );
    assert!(
        divided.contains("expected: `Eq(@Nat)(n / d, 7)`"),
        "unexpected report: {divided}"
    );

    let narrowed = refused(
        r#"
        use /std/{Nat, Byte, Bool, Eq, Io};

        pub let claim(n: Nat, @small: Bool/Holds(n < 256)) -> Eq()(Nat/to_byte(n), Nat/to_byte(7)) =
            Eq/refl();

        Io/pure(())
    "#,
    );
    assert!(
        narrowed.contains("expected: `Eq(@Byte)(Nat/to_byte(n), 0x07)`"),
        "unexpected report: {narrowed}"
    );
}

/// A proof elaboration never found is a place the program left to it, as the operator does: the report writes the division as it was written, beside the line that says what was never solved.
#[test]
fn an_intrinsic_whose_proof_was_never_found_is_written_as_its_operator() {
    let open = refused(
        r#"
        use /std/{Nat, Eq, Io};

        pub let claim(a: Nat, b: Nat) -> Eq()(a / b, 7) =
            Eq/refl();

        Io/pure(())
    "#,
    );
    assert!(
        open.contains("and: `a / b`")
            && open.contains("never solved: the implicit argument of the `/` operator"),
        "unexpected report: {open}"
    );
}

/// Every operation that holds the proof of its bound, each in a statement whose scope proves the bound through `h` alone, over an operand `g` makes: the side its mismatch prints states the proof, and reads back as a statement over the same parameters. The division written by its operator there, as a report that drops the proof prints it, is refused.
#[test]
fn every_intrinsic_holding_a_proof_reads_back_with_it() {
    const IMPORTS: &str = "use /std/{Nat, Int, Flt, Byte, Bytes, List, Bool, Eq, Io};";
    const DIVISOR: &str =
        "g: (Nat) -> Nat, h: (m: Nat) -> Bool/Holds(0 < g(m)), n: Nat, m: Nat, z: Nat";
    const FINITE: &str =
        "g: (Nat) -> Flt, h: (m: Nat) -> Bool/Holds(-inf.0 < g(m) && g(m) < +inf.0), m: Nat";
    const SAME: &str = "g: (Nat) -> Bytes, k: (Nat) -> Bytes, h: (m: Nat) -> Bool/Holds(Bytes/length(g(m)) == Bytes/length(k(m))), m: Nat, z: Bytes";
    // A statement's parameters, and the operation as a program writes it over them.
    let cases = [
        (DIVISOR.to_string(), "Nat/div(n, g(m), @h(m))"),
        (DIVISOR.to_string(), "Nat/rem(n, g(m), @h(m))"),
        (
            "g: (Nat) -> Int, h: (m: Nat) -> Bool/Holds(g(m) != +0), n: Int, m: Nat, z: Int".to_string(),
            "Int/div(n, g(m), @h(m))",
        ),
        (
            "g: (Nat) -> Int, h: (m: Nat) -> Bool/Holds(g(m) != +0), n: Int, m: Nat, z: Nat".to_string(),
            "Int/rem(n, g(m), @h(m))",
        ),
        // The remainder widened, which `%` at `Int` is.
        (
            "g: (Nat) -> Int, h: (m: Nat) -> Bool/Holds(g(m) != +0), n: Int, m: Nat, z: Int".to_string(),
            "Nat/to_int(Int/rem(n, g(m), @h(m)))",
        ),
        (
            "g: (Nat) -> Nat, h: (m: Nat) -> Bool/Holds(g(m) < 256), m: Nat, z: Byte".to_string(),
            "Nat/to_byte(g(m), @h(m))",
        ),
        (
            "g: (Nat) -> Int, h: (m: Nat) -> Bool/Holds(+0 <= g(m)), m: Nat, z: Nat".to_string(),
            "Int/to_nat(g(m), @h(m))",
        ),
        (
            "g: (Nat) -> Flt, h: (m: Nat) -> Bool/Holds(+0.0 <= g(m) && g(m) < +inf.0), m: Nat, z: Nat".to_string(),
            "Flt/to_nat(g(m), @h(m))",
        ),
        (format!("{FINITE}, z: Int"), "Flt/to_int(g(m), @h(m))"),
        (format!("{FINITE}, z: Int"), "Flt/mantissa(g(m), @h(m))"),
        (format!("{FINITE}, z: Int"), "Flt/exponent(g(m), @h(m))"),
        (
            "g: (Nat) -> Bytes, h: (m: Nat) -> Bool/Holds(Bytes/length(g(m)) == 8), m: Nat, z: Flt".to_string(),
            "Flt/of_le_bytes(g(m), @h(m))",
        ),
        (
            "g: (Nat) -> Bytes, h: (m: Nat) -> Bool/Holds(m < Bytes/length(g(m))), m: Nat, z: Byte".to_string(),
            "Bytes/get(g(m), m, @h(m))",
        ),
        (
            "g: (Nat) -> Bytes, h: (m: Nat) -> Bool/Holds(m + m <= Bytes/length(g(m))), m: Nat, z: Bytes".to_string(),
            "Bytes/slice(g(m), m, m, @h(m))",
        ),
        (SAME.to_string(), "Bytes/and(g(m), k(m), @h(m))"),
        (SAME.to_string(), "Bytes/or(g(m), k(m), @h(m))"),
        (SAME.to_string(), "Bytes/xor(g(m), k(m), @h(m))"),
        (
            "g: (Nat) -> List(Nat), h: (m: Nat) -> Bool/Holds(m < List/length(g(m))), m: Nat, z: Nat".to_string(),
            "List/get(g(m), m, @h(m))",
        ),
        (
            "g: (Nat) -> List(Nat), h: (m: Nat) -> Bool/Holds(m + m <= List/length(g(m))), m: Nat, z: List(Nat)".to_string(),
            "List/slice(g(m), m, m, @h(m))",
        ),
        (
            "g: (Nat) -> List(Nat), h: (m: Nat) -> Bool/Holds(m < List/length(g(m))), m: Nat, z: List(Nat)".to_string(),
            "List/set(g(m), m, 7, @h(m))",
        ),
    ];

    let claims = cases
        .iter()
        .enumerate()
        .map(|(index, (parameters, call))| {
            format!("pub let claim{index}({parameters}) -> Eq()({call}, z) =\n    Eq/refl();\n")
        })
        .collect::<String>();
    let report = refused(&format!("{IMPORTS}\n{claims}\nIo/pure(())\n"));
    let printed = report
        .lines()
        .filter_map(|line| line.split_once("expected: `")?.1.strip_suffix('`'))
        .collect::<Vec<_>>();
    assert_eq!(printed.len(), cases.len(), "unexpected report: {report}");
    let unstated = cases
        .iter()
        .zip(&printed)
        .filter(|(_, printed)| !printed.contains("@h(m)"))
        .map(|((_, call), printed)| format!("`{call}` as `{printed}`"))
        .collect::<Vec<_>>();
    assert!(
        unstated.is_empty(),
        "printed without their proof: {unstated:#?}"
    );

    let pasted = cases
        .iter()
        .zip(&printed)
        .enumerate()
        .map(|(index, ((parameters, _), printed))| {
            format!("pub let pasted{index}({parameters}) -> Prop =\n    {printed};\n")
        })
        .collect::<String>();
    if let Err(error) = compile(&format!("{IMPORTS}\n{pasted}\nIo/pure(())\n")) {
        panic!("a printed statement does not read back:\n{pasted}\n{error}");
    }

    let bare = refused(&format!(
        "use /std/{{Nat, Bool, Eq, Io}};\npub let bare({DIVISOR}) -> Prop =\n    Eq(@Nat)(n / g(m), z);\n\nIo/pure(())\n"
    ));
    assert!(
        bare.contains("the bound of the `/` operator was not discharged"),
        "unexpected report: {bare}"
    );
}

/// A side's call whose unfolding reaches an operation of `/sys` its reader writes by no name keeps the call as it was written, and the report says nothing of a term shown as the compiler holds it: in a program, in a library unit, in a library recompiled over a baseline, and in a program put whole as the soundness board puts one, each driver handing the elaborator what the unit's reader writes.
#[test]
fn a_call_that_unfolds_to_an_operation_no_reader_writes_keeps_its_name() {
    let kept = |report: &str| {
        assert!(
            report
                .contains("expected: `Eq(@Flt)(Flt/rounded/add(Rounding/toward_zero(), a, b), a)`"),
            "unexpected report: {report}"
        );
        assert!(
            !report.contains("as the compiler holds it") && !report.contains("/sys"),
            "unexpected report: {report}"
        );
    };

    let program = format!("{ROUNDED}\nIo/pure(())\n").replace("Eq};", "Eq, Io};");
    kept(&refused(&program));
    kept(
        &compile_modules(&mounted("lib", ROUNDED))
            .map(|_| ())
            .unwrap_err(),
    );
    let baseline = compile_modules(&mounted(
        "lib",
        "use /std/{Flt};\n\npub let g(a: Flt) -> Flt = a;\n",
    ))
    .expect("the baseline compiles");
    kept(
        &recompile_modules(&mounted("lib", ROUNDED), &baseline)
            .map(|_| ())
            .unwrap_err(),
    );
    match examine_with_prelude(
        DEFAULT_BUDGET,
        &entrypoint_of(&program),
        &RootSource::none(),
        EntryTail::Program,
    ) {
        Err(Refused { reported, .. }) => kept(&String::from(reported)),
        Ok(_) => panic!("the program put whole is refused"),
    }
}
