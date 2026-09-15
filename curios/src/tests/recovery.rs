//! What a run reports when more than one declaration is refused: every independent refusal, nothing from a declaration that reaches a refused one, and both erasure obligations when both fail.

use {
    super::{error, run_entrypoint},
    crate::to_cwasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, EntryTail, compile_tests_with_units},
    curios_runtime::{ForeignBindings, MockHost, run_bytes},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::RootKind,
};

/// Run `source` as standard input is run — the reading that recovers past a broken item — expecting a refusal, and return its report.
fn supplied_error(source: &str) -> String {
    let (entrypoint, loader, _) =
        Entrypoint::supplied("<stdin>", source).expect("the program parses, holes and all");
    let (system, _io) = MockHost::builder().build();
    run_entrypoint(&entrypoint, &loader, system).expect_err("the fixture is refused")
}

/// Compile `source` as its own test program, expecting a refusal, and return its report.
fn tests_error(source: &str) -> String {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");
    compile_tests_with_units(
        DEFAULT_STEP_BUDGET,
        &[],
        &entrypoint,
        &RootSource::none(),
        None,
        EntryTail::Tests,
        |_| {},
        |_| {},
    )
    .map(|_| ())
    .expect_err("the fixture is refused")
    .to_string()
}

#[test]
fn two_independent_refusals_report_both() {
    let report = error(
        r#"
        use /std/{Nat};

        let _a : Nat = true;
        let _b : Nat = 1;
        let _c : Nat = false;

        /std/print("unreachable")
        "#,
    );

    assert!(report.contains("while elaborating /_a:"), "{report}");
    assert!(report.contains("while elaborating /_c:"), "{report}");
    assert!(!report.contains("_b"), "{report}");
}

#[test]
fn a_dependent_of_a_refused_declaration_reports_nothing_of_its_own() {
    let report = error(
        r#"
        use /std/{Nat};

        let _a : Nat = true;
        let _b : Nat = _a + 1;

        /std/print("unreachable")
        "#,
    );

    assert!(report.contains("while elaborating /_a:"), "{report}");
    assert!(!report.contains("_b"), "{report}");
}

/// The dependency runs through the registry: `_open` never mentions `Box` by name in a way its variables show, it matches a constructor of it.
#[test]
fn a_dependent_through_a_constructed_type_reports_nothing_of_its_own() {
    let report = error(
        r#"
        use /std/{Nat};

        induct Box : pub Type
        | boxed(value : true)
        end

        let _open(b : Box) -> Nat =
            match b
            | boxed(v) => v
            end;

        /std/print("unreachable")
        "#,
    );

    assert!(report.contains("Box"), "{report}");
    assert!(!report.contains("_open"), "{report}");
}

/// A witness registers before its body elaborates, so a refused body leaves the key it stood under known: a goal on that key is the refusal's dependent, and says nothing where a plain miss would report `no witness`.
#[test]
fn a_user_of_a_refused_witness_reports_nothing_of_its_own() {
    let report = error(
        r#"
        use /std/{Nat, Str, Show};

        induct Color : pub Type
        | red()
        end

        satisfy Show(Color) {
            show(c) = 1,
        }

        let _shown : Str = Show/show(Color/red());

        /std/print("unreachable")
        "#,
    );

    assert!(
        report.contains("the witness in the entry module"),
        "{report}"
    );
    assert!(!report.contains("_shown"), "{report}");
    assert!(!report.contains("no witness"), "{report}");
}

/// The limit of that silence: a witness refused in its signature never reached the table, so nothing records the key it would have held, and its user reports the miss it can see.
#[test]
fn a_witness_refused_before_it_registered_leaves_its_user_reporting_no_witness() {
    let report = error(
        r#"
        use /std/{Nat, Str, Show};

        induct Color : pub Type
        | red()
        end

        satisfy Show(1) {
            show(c) = "",
        }

        let _shown : Str = Show/show(Color/red());

        /std/print("unreachable")
        "#,
    );

    assert!(
        report.contains("the witness in the entry module"),
        "{report}"
    );
    assert!(report.contains("no witness"), "{report}");
}

/// A test is an item like any other: one naming a refused declaration is withheld, and the synthesized tail leaves out what the refusal poisoned rather than naming a test the module no longer holds.
#[test]
fn a_test_of_a_refused_declaration_is_left_out_of_the_synthesized_tail() {
    let report = tests_error(
        r#"
        use /std/{Nat, Test};

        let _a : Nat = true;

        test uses_a =
            Test/equal(_a, 1);

        /std/print("unreachable")
        "#,
    );

    assert!(report.contains("while elaborating /_a:"), "{report}");
    assert!(!report.contains("uses_a"), "{report}");
}

/// The boundary of that omission: a unit's tests are scheduled into the tail of the program compiled after it, whose own module declares none of them and refuses nothing, so every test the unit declares is in the tail and runs. A filter over the entry module's survivors once dropped them all, and each test exited without a word.
#[test]
fn a_units_tests_run_from_the_tail_of_the_program_compiled_after_it() {
    let mut unit = RootSource::supplied();
    unit.insert_root(
        "lib",
        RootKind::Ordinary,
        "use /std/{Nat, Test};\n\ntest addition_passes =\n    Test/assert(1 + 1 == 2);\n"
            .parse()
            .expect("the unit parses"),
    );

    let (module, _foreigns, records) = compile_tests_with_units(
        DEFAULT_STEP_BUDGET,
        &[unit],
        &Entrypoint::trivial(),
        &RootSource::none(),
        None,
        EntryTail::LastUnitTests,
        |_| {},
        |_| {},
    )
    .expect("the unit's test program compiles");
    let cwasm = to_cwasm(&module).expect("the test program precompiles");
    assert_eq!(records.len(), 1);

    let (system, io) = MockHost::builder().args(["test", "0"]).build();
    // SAFETY: `cwasm` was precompiled in this process, immediately above.
    let outcome = unsafe { run_bytes(&cwasm, system, ForeignBindings::empty()) };

    assert!(matches!(outcome, Ok(0)), "the test ran to {outcome:?}");
    assert_eq!(
        String::from_utf8_lossy(&io.output()).trim_end(),
        format!("{}: passed", records[0].path)
    );
}

/// The two obligations are decided independently and reported together, so a reader fixing one is told about the other in the same run.
#[test]
fn both_erasure_obligations_are_reported_together() {
    let report = error(
        r#"
        use /std/{Nat};

        let Bad(n : Nat) -> Type =
            match n < 10
            | true => Bad(n - 1)
            | false => {}
            end;

        let loop(P : Prop, n : Nat, again : (m : Nat) -> Nat) -> P =
            loop(P, again(n), again);

        /std/print("unreachable")
        "#,
    );

    assert!(report.contains("is a type position"), "{report}");
    assert!(report.contains("is a proof position"), "{report}");
}

/// A declaration the parser could not read is a refusal like any other: reported once, with its dependents withheld.
#[test]
fn a_dependent_of_a_broken_declaration_reports_nothing_of_its_own() {
    let report = supplied_error(
        "use /std/{Nat};\n\nlet _a : Nat = ;\nlet _b : Nat = _a;\n\n/std/print(\"\")\n",
    );

    assert!(report.contains("expected a term"), "{report}");
    assert!(!report.contains("_b"), "{report}");
    assert!(!report.contains("unbound"), "{report}");
}

#[test]
fn a_broken_declaration_is_reported_beside_a_refusal_after_it() {
    let report = supplied_error(
        "use /std/{Nat};\n\nlet _a : Nat = ;\nlet _c : Nat = true;\n\n/std/print(\"\")\n",
    );

    assert!(report.contains("expected a term"), "{report}");
    assert!(report.contains("while elaborating /_c:"), "{report}");
    assert!(
        report.find("expected a term") < report.find("while elaborating"),
        "the parse failure first: {report}"
    );
}
