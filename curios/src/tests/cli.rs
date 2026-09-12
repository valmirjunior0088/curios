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
    assert!(
        refused.contains("which reduces to False"),
        "and says what the bound came to:\n{refused}"
    );
}

/// A program built once and run under several scripted command lines. The compile is what a fixture pays for; a run of the precompiled module is milliseconds, so one compile serves every row below.
fn serve() -> Compiled {
    compile(
        r#"
        use /std/{Cli, Nat, Str, Option, Io, print};

        let serve: Cli =
            Cli/leaf(
                "serve",
                "Serve a directory over HTTP",
                Option/some("0.1.0"),
                [
                    Cli/Arg { ..Cli/flag("verbose", "Log every request"), short = Option/some('v') },
                    Cli/default("port", Cli/nat, 8080, "Port to listen on"),
                    Cli/positional("root", Cli/str, "Directory to serve"),
                ],
                (v) =>
                    let port = Cli/get(v, "port");
                    let root = Cli/get(v, "root");
                    print(Str/flatten(["serving ", root, " on ", Nat/to_str(port), "\n"])));

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

/// A group inside a group, and two leaves that each name one long name twice — the shapes `serve` has none of.
fn deps() -> Compiled {
    compile(
        r#"
        use /std/{Cli, Str, Option, print};

        let add: Cli =
            Cli/leaf(
                "add",
                "Add a dependency",
                Option/none(),
                [Cli/positional("name", Cli/str, "What to add")],
                (v) => print(Str/flatten(["adding ", Cli/get(v, "name"), "\n"])));

        let registry: Cli =
            Cli/group("registry", "Manage registry dependencies", Option/none(), [add]);

        let flags: Cli =
            Cli/leaf(
                "flags",
                "One flag named twice",
                Option/none(),
                [Cli/flag("verbose", "once"), Cli/flag("verbose", "twice")],
                (_) => print("ran\n"));

        let ports: Cli =
            Cli/leaf(
                "ports",
                "One required option named twice",
                Option/none(),
                [Cli/option("port", Cli/nat, "first"), Cli/option("port", Cli/nat, "second")],
                (_) => print("ran\n"));

        let deps: Cli =
            Cli/group("deps", "Manage dependencies", Option/some("0.1.0"), [registry, flags, ports]);

        Cli/main(deps)
        "#,
    )
    .expect("the entry compiles")
}

/// A group descends into the subcommand its first token names, to any depth, so a handler two groups down runs under `Cli/main` alone.
#[test]
fn a_subcommand_two_groups_down_runs_its_handler() {
    let (system, io) = MockHost::builder()
        .args(["deps", "registry", "add", "curios"])
        .build();

    assert_eq!(deps().run(system), Ok(0));
    assert_eq!(io.output(), b"adding curios\n");
}

/// Help answered inside a subcommand names the commands that lead to it, so its usage line is the line that reaches the command.
#[test]
fn help_inside_a_subcommand_names_the_path_that_reaches_it() {
    let (system, io) = MockHost::builder()
        .args(["deps", "registry", "add", "--help"])
        .build();

    assert_eq!(deps().run(system), Ok(0));
    let help = String::from_utf8_lossy(&io.output()).into_owned();
    assert!(
        help.contains("\n\nUsage: deps registry add <NAME>\n\n"),
        "help names the path:\n{help}"
    );
}

/// A refusal inside a subcommand carries the same path, on standard error with exit 2 like any other refusal.
#[test]
fn a_refusal_inside_a_subcommand_names_the_path_that_reaches_it() {
    let (system, io) = MockHost::builder()
        .args(["deps", "registry", "add"])
        .build();

    assert_eq!(deps().run(system), Ok(2));
    let refusal = String::from_utf8(io.errors()).expect("the refusal is text");
    assert!(
        refusal.contains(
            "error: the argument 'NAME' is required\n\nUsage: deps registry add <NAME>\n\n"
        ),
        "the refusal names the path:\n{refusal}"
    );
}

/// Nothing refuses a specification that names one long name twice, so what it does is the fact worth pinning: the first entry takes every occurrence, and a flag's second entry reads as absent, so the line parses and the handler runs.
#[test]
fn a_repeated_flag_name_parses_with_its_second_entry_absent() {
    let (system, io) = MockHost::builder()
        .args(["deps", "flags", "--verbose"])
        .build();

    assert_eq!(deps().run(system), Ok(0));
    assert_eq!(io.output(), b"ran\n");
}

/// A required option's second entry is never filled either, so the line is refused for an argument it did supply.
#[test]
fn a_repeated_option_name_reports_the_supplied_argument_as_missing() {
    let (system, io) = MockHost::builder()
        .args(["deps", "ports", "--port", "9090"])
        .build();

    assert_eq!(deps().run(system), Ok(2));
    let refusal = String::from_utf8(io.errors()).expect("the refusal is text");
    assert!(
        refusal.contains("error: the argument 'PORT' is required"),
        "the second entry reports as missing:\n{refusal}"
    );
}
