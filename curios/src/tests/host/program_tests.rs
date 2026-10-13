//! What closes a program: each type `/std/Program` performs, by what the program wrote and the status it ended with.

use {
    crate::run_wasm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
    curios_runtime::{ForeignBindings, MockHost},
    curios_text::{Entrypoint, RootSource},
};

/// What a program came to under a host that holds `files`: the status it ended with, everything it wrote, and the standard error half of that.
fn ended(source: &str, files: &[(&str, &str)]) -> (u8, Vec<u8>, Vec<u8>) {
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

    let (system, io) = MockHost::builder().files(files.iter().copied()).build();
    let code =
        run_wasm(&module, foreigns, system, ForeignBindings::empty()).expect("execution succeeded");

    (code, io.output(), io.errors())
}

/// A program over a file it reads and prints the length of, closed by a description that may fail.
const READS_A_FILE: &str = r#"
    use /std/{Nat, Bytes, Io, Try, File, Path, Program, print};
    let work: Try(Io, Io/Error, {}) =
        let held = File/read_all(Path/of_str("notes.txt"))!;
        print(Nat/to_str(Bytes/len(held)));
    let _ = print("before|")!;
    Program/perform(work)
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
        use /std/{Str, Show, Io, Try, Program};
        induct Broke: Type
        | broke()
        end
        satisfy Show(Broke) {
            show(_) = "it broke",
        }
        let work: Try(Io, Broke, {}) = Try/raise(Broke/broke());
        Program/perform(work)
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
        let source =
            format!("use /std/{{Nat, Test, Program}};\nProgram/perform(Test/assert({claim}))\n");
        let (code, output, errors) = ended(&source, &[]);

        assert_eq!(code, status, "{claim}");
        assert_eq!(output, report.as_bytes(), "{claim}");
        assert_eq!(errors, b"", "{claim}");
    }
}
