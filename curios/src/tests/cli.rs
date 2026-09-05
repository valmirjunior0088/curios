//! What `/std/Cli` refuses at compile time, and what `Cli/main` writes where. The surface `parse` and `help` *accept* is the corpus unit's; neither a refusal nor a stream nor an exit status is a `Test` a program can declare, so those stay here.

use {
    super::{Compiled, compile, error},
    curios_runtime::MockHost,
};

/// A name the specification does not contain is refused where it is written: `Has` walks to `False` and the bound holding `get` up cannot be discharged. The binding is unannotated so the bound is what fails — an expected type would report `Lookup`'s `{}` first and say nothing about why.
#[test]
fn a_misspelled_name_is_refused_rather_than_answered() {
    let refused = error(
        r#"
        use /std/{Cli, Str, Nat, List, print};

        let spec: List(Cli/Arg) = [Cli/default("port", Cli/nat, 8080, "Port to listen on")];
        let port: Nat = 1;
        let sample: Cli/Values(spec) = Cli/Values/cons(port, Cli/Values/nil());

        let read = Cli/get(sample, "prot");
        print("unreachable")
        "#,
    );

    assert!(
        refused.contains("Has"),
        "the refusal names the bound nothing discharged:\n{refused}"
    );
}

/// `WellFormed` is decided by reduction, so a specification naming one long name twice is refused at the entry that demands it rather than at the first line it would misparse.
#[test]
fn a_duplicate_long_name_is_refused_at_well_formed() {
    let refused = error(
        r#"
        use /std/{Cli, Str, Option, Io, print};

        let twice: Cli =
            Cli {
                name = "dup",
                about = "Two entries share a long name",
                version = Option/none(),
                args = [Cli/flag("verbose", "once"), Cli/flag("verbose", "twice")],
                run(_) = print("never"),
                commands = [],
            };

        let demand(c: Cli, @ok: Cli/WellFormed(c)) -> Io({}) = print(c.name);

        demand(twice)
        "#,
    );

    assert!(
        refused.contains("WellFormed"),
        "the refusal names the bound nothing discharged:\n{refused}"
    );
}

/// A program built once and run under several scripted command lines. The compile is what a fixture pays for; a run of the precompiled module is milliseconds, so one compile serves every row below.
fn serve() -> Compiled {
    compile(
        r#"
        use /std/{Cli, Nat, Str, Option, Io, print};

        let serve: Cli =
            Cli {
                name = "serve",
                about = "Serve a directory over HTTP",
                version = Option/some("0.1.0"),
                args = [
                    Cli/Arg { ..Cli/flag("verbose", "Log every request"), short = Option/some('v') },
                    Cli/default("port", Cli/nat, 8080, "Port to listen on"),
                    Cli/positional("root", Cli/str, "Directory to serve"),
                ],
                run(v) =
                    let port = Cli/get(v, "port");
                    let root = Cli/get(v, "root");
                    print(Str/flatten(["serving ", root, " on ", Nat/to_str(port), "\n"])),
                commands = [],
            };

        Cli/main(serve)
        "#,
    )
    .expect("the entry compiles")
}

/// A line the specification accepts runs the handler, writes nothing to standard error, and exits 0.
#[test]
fn a_good_line_runs_the_handler_and_exits_zero() {
    let (system, io) = MockHost::builder()
        .args(["serve", "--port", "9090", "/srv"])
        .build();

    assert_eq!(serve().run(system), Ok(0));
    assert_eq!(io.output(), b"serving /srv on 9090\n");
    assert!(
        io.errors().is_empty(),
        "a good line writes nothing to stderr"
    );
}

/// Help is a request, not a mistake: standard output and exit 0, even over a line that would otherwise have parsed.
#[test]
fn help_answers_on_stdout_and_exits_zero() {
    let (system, io) = MockHost::builder()
        .args(["serve", "--port", "1", "--help"])
        .build();

    assert_eq!(serve().run(system), Ok(0));
    assert!(io.errors().is_empty(), "help is not a failure");
    assert!(
        String::from_utf8_lossy(&io.output())
            .starts_with("Serve a directory over HTTP\n\nUsage: serve [OPTIONS] <ROOT>\n"),
        "the help screen goes to stdout:\n{}",
        String::from_utf8_lossy(&io.output())
    );
}

/// A line the specification refuses goes to standard *error* and exits 2 — the distinction the scripted host learned to make for this, since a concatenation of both streams cannot show it.
#[test]
fn a_refused_line_reports_on_stderr_and_exits_two() {
    let (system, io) = MockHost::builder()
        .args(["serve", "--prot", "/srv"])
        .build();

    assert_eq!(serve().run(system), Ok(2));
    // `output` is both streams in write order, so "nothing reached stdout" is "everything written was the stderr half".
    assert_eq!(
        io.output(),
        io.errors(),
        "a refusal writes nothing to stdout:\n{}",
        String::from_utf8_lossy(&io.output())
    );
    assert_eq!(
        String::from_utf8(io.errors()).expect("the refusal is text"),
        "error: unexpected argument '--prot'\n\nUsage: serve [OPTIONS] <ROOT>\n\nFor more information, try '--help'.\n"
    );
}

/// An argument the host hands over as bytes that are not text is refused by its position rather than converted lossily, and takes the same stream and status as any other refusal.
#[test]
fn a_non_utf8_argument_is_refused_by_its_position() {
    let (system, io) = MockHost::builder()
        .args([
            b"serve".as_slice(),
            b"\xff\xfe".as_slice(),
            b"/srv".as_slice(),
        ])
        .build();

    assert_eq!(serve().run(system), Ok(2));
    assert!(
        String::from_utf8_lossy(&io.errors()).contains("argument 1 is not valid UTF-8"),
        "the refusal names which argument:\n{}",
        String::from_utf8_lossy(&io.errors())
    );
}
