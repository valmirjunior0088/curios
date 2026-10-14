//! What closes a program: each type `/std/Program` performs, by what the program wrote and the status it ended with, and what a term nothing performs is told.

use {
    crate::{run_wasm, tests::typecheck},
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_runtime::{ForeignBindings, MockHost, MockHostBuilder},
    curios_text::{Entrypoint, RootSource},
};

/// What a program came to under `host`: the status it ended with, everything it wrote, and the standard error half of that.
fn ended_under(source: &str, host: MockHostBuilder) -> (u8, Vec<u8>, Vec<u8>) {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");
    let (module, foreigns) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .expect("compile succeeded");

    let (system, io) = host.build();
    let code =
        run_wasm(&module, foreigns, system, ForeignBindings::empty()).expect("execution succeeded");

    (code, io.output(), io.errors())
}

/// [`ended_under`] a host that holds `files` and nothing else.
fn ended(source: &str, files: &[(&str, &str)]) -> (u8, Vec<u8>, Vec<u8>) {
    ended_under(source, MockHost::builder().files(files.iter().copied()))
}

/// A program over a file it reads and prints the length of, closed by a description that may fail.
const READS_A_FILE: &str = r#"
    use /std/{Nat, Bytes, Io, Try, File, Path, print};
    let main: Try(Io, Io/Error, {}) =
        let held = File/read_all(Path/of_str("notes.txt"))!;
        print(Nat/to_str(Bytes/len(held)));
    let _ = print("before|")!;
    main
    "#;

#[test]
fn a_failure_that_closes_a_program_is_written_to_standard_error_and_ends_it_with_status_one() {
    let (code, output, errors) = ended(READS_A_FILE, &[]);

    assert_eq!(code, 1);
    assert_eq!(errors, b"not_found\n");
    assert_eq!(output, b"before|not_found\n");
}

#[test]
fn a_description_that_may_fail_and_does_not_ends_its_program_with_status_zero() {
    let (code, output, errors) = ended(READS_A_FILE, &[("notes.txt", "hello")]);

    assert_eq!(code, 0);
    assert_eq!(errors, b"");
    assert_eq!(output, b"before|5");
}

#[test]
fn a_failure_of_the_programs_own_type_is_written_as_that_type_is_shown() {
    let (code, _, errors) = ended(
        r#"
        use /std/{Str, Show, Io, Try};
        induct Broke: Type
        | broke()
        end
        satisfy Show(Broke) {
            show(_) = "it broke",
        }
        let main: Try(Io, Broke, {}) = Try/raise(Broke/broke());
        main
        "#,
        &[],
    );

    assert_eq!(code, 1);
    assert_eq!(errors, b"it broke\n");
}

#[test]
fn a_test_that_closes_a_program_is_reported_and_decides_its_status() {
    for (claim, status, report) in [
        ("1 + 1 == 2", 0, "program: passed\n"),
        (
            "1 + 1 == 3",
            1,
            "program: failed\n  the condition was false\n",
        ),
    ] {
        let source = format!("use /std/{{Nat, Test}};\nTest/assert({claim})\n");
        let (code, output, errors) = ended(&source, &[]);

        assert_eq!(code, status, "{claim}");
        assert_eq!(output, report.as_bytes(), "{claim}");
        assert_eq!(errors, b"", "{claim}");
    }
}

/// Each way a program closes is performed at its own type: an arm that prints and an arm that may fail stand in one `match`, which states no type for both.
#[test]
fn the_arms_of_a_match_that_closes_a_program_are_each_performed() {
    for (choice, status, written) in [(0, 0, "usage"), (1, 1, "not_found\n")] {
        let source = format!(
            r#"
            use /std/{{Nat, Str, Io, Try, File, Path, print}};
            let read(name: Str) -> Try(Io, Io/Error, {{}}) =
                let _ = File/read_all(Path/of_str(name))!;
                print("read");
            let choice: Nat = {choice};
            match choice
            | 0 => print("usage")
            | _ =>
                let name = Io/pure("notes.txt")!;
                read(name)
            end
            "#
        );
        let (code, output, _) = ended(&source, &[]);

        assert_eq!(code, status, "{choice}");
        assert_eq!(output, written.as_bytes(), "{choice}");
    }
}

/// The top level is an `Io` region whatever closes it: a step that may fail is refused there, as it is in any `Io` region.
#[test]
fn a_step_that_may_fail_is_refused_at_the_top_level_of_a_program_that_closes_in_a_try() {
    let error = typecheck(
        r#"
        use /std/{Io, Try, File, Path, print};
        let main: Try(Io, Io/Error, {}) = print("read");
        let _ = File/read_all(Path/of_str("notes.txt"))!;
        main
        "#,
    )
    .expect_err("a `Try` has no edge into `Io`");

    assert!(
        error.contains("no witness of") && error.contains("Lift"),
        "expected the missing edge, got: {error}"
    );
}

/// A term of a type nothing performs is told so as what closes the program, not as the premise of a call its author never wrote.
#[test]
fn a_term_nothing_performs_is_refused_as_what_closes_the_program() {
    for (closing, type_) in [("7", "Nat"), ("() => print(\"x\")", "() -> Io({})")] {
        let source = format!("use /std/{{Nat, Io, print}};\n{closing}\n");
        let error = typecheck(&source).expect_err("nothing performs it");

        assert!(
            error.contains(&format!(
                "a program closes with a term whose type has one, and this one's type is {type_}"
            )),
            "{closing}: {error}"
        );
        assert!(!error.contains("needed by"), "{closing}: {error}");
    }
}

/// A description that yields a value closes no program: `Io` is performed at `{}` alone, and the refusal is the payload's.
#[test]
fn a_description_that_yields_a_value_closes_no_program() {
    let error = typecheck("use /std/{Nat, Io};\nIo/pure(3)\n").expect_err("an `Io(Nat)`");

    assert!(
        error.contains("Nat") && error.contains("{}"),
        "expected the payload's mismatch, got: {error}"
    );
}

/// A goal that closes a program is asked at the program's own type, which is what most programs close with.
#[test]
fn a_goal_that_closes_a_program_is_asked_at_io() {
    let error = typecheck("use /std/{Io};\n?\n").expect_err("a goal never compiles");

    assert!(error.contains("? : Io({})"), "{error}");
}

/// The first program past printing: a line read as text, a number made of it and its double printed, under one `match`, with a line that is not text ending the program by itself.
#[test]
fn a_program_reads_a_number_and_prints_its_double() {
    const DOUBLES: &str = r#"
        use /std/{Nat, Str, Option, Io, Try, print};
        use /std/Io/{Reader};
        let main: Try(Io, Io/Error, {}) =
            let input = Reader/new(Io/stdin)!;
            let line = Reader/line_text(input)!;
            match Option/bind(line, Nat/of_str)
            | some(n) => print(Nat/to_str(n * 2))
            | none() => print("not a number")
            end;
        main
        "#;

    for (typed, status, written) in [
        (vec![&b"21\n"[..]], 0, "42"),
        (vec![&b"many\n"[..]], 0, "not a number"),
        (vec![], 0, "not a number"),
        (vec![&b"2\xff1\n"[..]], 1, "not_text(1)\n"),
    ] {
        let host = MockHost::builder().stdin_chunks(typed.clone());
        let (code, output, _) = ended_under(DOUBLES, host);

        assert_eq!(code, status, "{typed:?}");
        assert_eq!(String::from_utf8_lossy(&output), written, "{typed:?}");
    }
}
