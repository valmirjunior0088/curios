//! What closes a program: each type `/std/Program` executes, by what the program wrote and the status it ended with, and what a term nothing executes is told.

use {
    crate::test_support::*,
    curios_common::fill,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{ForeignBindings, test_support::*},
    curios_pipeline::compile_with_prelude,
    curios_surface::Entrypoint,
};

/// What a program came to under `host`: the status it ended with, everything it wrote, and the standard error half of that.
fn ended_under(source: &str, host: MockHostBuilder) -> (u8, Vec<u8>, Vec<u8>) {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");
    let (module, foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
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
    let main() -> Try(Io, Io/Error, {}) =>
        let _ = print("before|")!;
        let held = File/read_all(Path/of_str("notes.txt"))!;
        print(Nat/to_str(Bytes/length(held)));
    main()
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
            show(_) => "it broke",
        }
        let main: Try(Io, Broke, {}) = Try/raise(Broke/broke());
        main
        "#,
        &[],
    );

    assert_eq!(code, 1);
    assert_eq!(errors, b"it broke\n");
}

/// A description of a test closes no program: its runner is its one presenter, so the refusal says where a test is declared and what runs it.
#[test]
fn a_test_closes_no_program_and_is_told_where_one_is_declared() {
    let error =
        typecheck("use /std/{Nat, Test};\nTest/assert(\"one and one are two\", 1 + 1 == 2)\n")
            .expect_err("nothing executes a test");

    assert!(
        error.contains(
            "this one's type is `Test({})`\n│  a description of a test is declared, `test name => ...;`, and run by `curios test`"
        ),
        "{error}"
    );
}

/// Each way a program closes is executed at its own type: an arm that prints and an arm that may fail stand in one `match`, which states no type for both.
#[test]
fn the_arms_of_a_match_that_closes_a_program_are_each_executed() {
    for (choice, status, written) in [(0, 0, "usage"), (1, 1, "not_found\n")] {
        let source = fill!(
            r#"
            use /std/{Nat, Str, Io, Try, File, Path, print};
            let read(name: Str) -> Try(Io, Io/Error, {}) =>
                let _ = File/read_all(Path/of_str(name))!;
                print("read");
            let choice: Nat = ${choice};
            let named() -> Try(Io, Io/Error, {}) =>
                let name = Io/pure("notes.txt")!;
                read(name);
            match choice
            | 0 => print("usage")
            | _ => named()
            end
            "#,
            choice
        );
        let (code, output, _) = ended(&source, &[]);

        assert_eq!(code, status, "{choice}");
        assert_eq!(output, written.as_bytes(), "{choice}");
    }
}

/// An `Io` function cannot sequence a fallible action: no reverse edge from `Try` to `Io` discards its error.
#[test]
fn a_step_that_may_fail_is_refused_in_an_io_function() {
    let error = typecheck(
        r#"
        use /std/{Io, Try, File, Path};
        let step() -> Io({}) =>
            let _ = File/read_all(Path/of_str("notes.txt"))!;
            Io/pure(());
        step()
        "#,
    )
    .expect_err("a `Try` has no edge into `Io`");

    assert!(
        error.contains("no witness") && error.contains("Lift"),
        "expected the missing edge, got: {error}"
    );
}

/// A term of a type nothing executes is told so as what closes the program, not as the premise of a call its author never wrote; its type is shown as it reduces where that is a function, so a format given too few arguments reads as the function it still is.
#[test]
fn a_term_nothing_executes_is_refused_as_what_closes_the_program() {
    for (closing, type_) in [
        ("7", "Nat"),
        ("() => print(\"x\")", "() -> Io({})"),
        (
            "Fmt/print(\"% and %\\n\")(1)",
            "(@A: Type, use Show(A), a: A) -> Io({})",
        ),
    ] {
        let source = fill!(
            "use /std/{Nat, Io, Fmt, Show, print};\n${closing}\n",
            closing
        );
        let error = typecheck(&source).expect_err("nothing executes it");

        assert!(
            error.contains(&format!(
                "a program closes with an action a `Program` witness runs, such as an `Io({{}})`\n│  this one's type is `{type_}`"
            )),
            "{closing}: {error}"
        );
        assert!(!error.contains("needed by"), "{closing}: {error}");
        assert!(!error.contains("format_type_with"), "{closing}: {error}");
    }
}

/// A description that yields a value closes no program: `Io` is executed at `{}` alone, and the refusal is the payload's.
#[test]
fn a_description_that_yields_a_value_closes_no_program() {
    let error = typecheck("use /std/{Nat, Io};\nIo/pure(3)\n").expect_err("an `Io(Nat)`");

    assert!(
        error.contains("Nat") && error.contains("{}"),
        "expected the payload's mismatch, got: {error}"
    );
}

/// A function ends with an explicit pure description after its last action.
#[test]
fn a_program_that_closes_with_a_pure_description_ends_with_what_its_actions_did() {
    let (code, output, errors) = ended(
        "use /std/{Io, print};\nlet main() -> Io({}) => let _ = print(\"done\")!; Io/pure(());\nmain()\n",
        &[],
    );

    assert_eq!(code, 0);
    assert_eq!(errors, b"");
    assert_eq!(output, b"done");
}

/// A goal that closes a program is asked at the program's own type, which is what most programs close with.
#[test]
fn a_goal_that_closes_a_program_is_asked_at_io() {
    let error = typecheck("use /std/{Io};\n?\n").expect_err("a goal never compiles");

    assert!(error.contains("`Io({})` expected"), "{error}");
}

/// The first program past printing: a line read as text, a number made of it and its double printed, under one `match`, with a line that is not text ending the program by itself.
#[test]
fn a_program_reads_a_number_and_prints_its_double() {
    const DOUBLES: &str = r#"
        use /std/{Nat, Str, Option, Io, Try, print, Proc};
        use /std/Io/{Reader};
        let main() -> Try(Io, Io/Error, {}) =>
            let input = Reader/new(Proc/stdin)!;
            let line = Reader/line_text(input)!;
            match Option/bind(line, Nat/of_str)
            | some(n) => print(Nat/to_str(n * 2))
            | none() => print("not a number")
            end;
        main()
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
