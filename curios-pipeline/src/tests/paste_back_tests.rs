//! A type a goal reports reads back: written where the goal stood, it compiles. That is the promise every reader-facing rendering makes — a report spells what its reader could have written at that position — and these are the programs that hold it to that, each written the first time a rendering broke it.

use {crate::*, curios_text::RootSource};

use super::test_support::*;

/// Compile `source`, whose every `?` a goal report determines, write each goal's reported solution over the goal's own span, and compile the result. An error names the program as rewritten, so a failure shows the spelling that did not read back.
fn reads_back(source: &str) -> Result<(), String> {
    let reports = match compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &with_entrypoint_type(source, None),
        &RootSource::none(),
        |_| {},
    ) {
        Err(CompileError::Incomplete(reports)) => reports,
        Err(error) => return Err(format!("the program is refused:\n{error}")),
        Ok(_) => return Err("the program has no goal to read back".to_string()),
    };

    let mut answers = reports
        .iter()
        .map(|report| {
            let span = report.span.as_ref().expect("a goal report is located");
            assert_eq!(
                span.source.text, source,
                "a goal's span points into the program"
            );
            let answer = solution(&report.message)
                .unwrap_or_else(|| panic!("the goal is determined:\n{}", report.message));
            (span.start, span.end, answer)
        })
        .collect::<Vec<_>>();

    // Last first, so each splice leaves the offsets of the ones still to come where they were.
    answers.sort_by_key(|(start, _, _)| std::cmp::Reverse(*start));
    let mut rewritten = source.to_string();
    for (start, end, answer) in answers {
        rewritten.replace_range(start..end, &answer);
    }

    compile(&rewritten, None)
        .map(|_| ())
        .map_err(|error| format!("{rewritten}\ndoes not compile:\n{error}"))
}

/// The `? = …` line of a goal report, with the lines a long solution breaks onto: those are indented past the two spaces every other line of the report takes.
fn solution(message: &str) -> Option<String> {
    let mut lines = message.lines();
    let first = lines
        .by_ref()
        .find_map(|line| line.strip_prefix("  ? = "))?;
    let rest = lines.take_while(|line| line.starts_with("   "));
    Some(
        std::iter::once(first)
            .chain(rest)
            .collect::<Vec<_>>()
            .join("\n"),
    )
}

#[test]
fn a_polymorphic_signature_over_imported_names_reads_back() {
    let source = r#"
        use /std/{List};
        pub let t: ? = List/map;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_local_function_type_reads_back() {
    let source = r#"
        use /std/{Nat};
        let double(n: Nat) -> Nat = n + n;
        pub let t: ? = double;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_tuple_type_reads_back() {
    let source = r#"
        use /std/{Nat, Bool};
        pub let t: ? = (1, true);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn an_applied_family_reads_back() {
    let source = r#"
        use /std/{Nat, Option};
        pub let t: ? = Option/some(1);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_proposition_over_an_implicit_parameter_reads_back() {
    let source = r#"
        use /std/{Nat, Eq};
        pub let t: ? = Eq/refl(@Nat, @3);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_type_holding_a_lambda_reads_back() {
    let source = r#"
        use /std/{Nat, Eq};
        pub let t: ? = Eq/refl(@(Nat) -> Nat, @(n: Nat) => n);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn an_indexed_family_and_its_applications_read_back() {
    let source = r#"
        use /std/{Nat};
        induct Vec(T: Type): (n: Nat) -> Type
        | nil(): (0)
        | cons(@m: Nat, x: T, xs: Vec(T)(m)): (m + 1)
        end
        pub let family: ? = Vec;
        pub let at_nat: ? = Vec(Nat);
        pub let value: ? = Vec/cons(7, Vec/nil());
        /std/print("")
    "#;

    reads_back(source).unwrap();
}
