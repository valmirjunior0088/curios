//! What `/std/Cli` refuses at compile time, and what `Cli/main` writes where. The surface `parse` and `help` *accept* is the corpus unit's; neither a refusal nor a stream nor an exit status is a `Test` a program can declare, so those stay here.

mod grammar_tests;

mod prepared_tests;

mod validation_tests;

mod command_tests;

mod declaration_tests;

use {
    super::{compile, error},
    curios_runtime::{Compiled, ForeignBindings, MockHost},
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
        refused.contains("which reduces to /std/Bool/False"),
        "and says what the bound came to:\n{refused}"
    );
}

/// A program built once and run under several scripted command lines. The compile is what a fixture pays for; a run of the precompiled module is milliseconds, so one compile serves every row below.
fn serve() -> Compiled {
    compile(
        r#"
        use /std/{Cli, Nat, Str, Option, Io, print};

        let serve: Cli/Declaration =
            Cli/leaf(
                "serve",
                "Serve a directory over HTTP",
                Option/some("0.1.0"),
                Cli/record([
                    Cli/Arg { ..Cli/flag("verbose", "Log every request"), short = Option/some('v') },
                    Cli/default("port", Cli/nat, 8080, "Port to listen on"),
                    Cli/positional("root", Cli/str, "Directory to serve"),
                ]),
                (v) =>
                    let port = Cli/get(v, "port");
                    let root = Cli/get(v, "root");
                    print(Str/flatten(["serving ", root, " on ", Nat/to_str(port), "\n"])));

        Cli/main(Cli/Declaration/declare(serve))
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

    assert_eq!(serve().run(system, ForeignBindings::empty()), Ok(0));
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

    assert_eq!(serve().run(system, ForeignBindings::empty()), Ok(0));
    assert!(io.errors().is_empty(), "help is not a failure");
    assert!(
        String::from_utf8_lossy(&io.output()).starts_with(
            "Serve a directory over HTTP\n\nUsage: serve [--verbose] [--port <N>] <ROOT>\n"
        ),
        "the help screen goes to stdout:\n{}",
        String::from_utf8_lossy(&io.output())
    );
}

/// A line the specification refuses goes to standard *error* and exits 2 — the distinction the scripted host's separate record of standard error makes visible, since a concatenation of both streams cannot show it.
#[test]
fn a_refused_line_reports_on_stderr_and_exits_two() {
    let (system, io) = MockHost::builder()
        .args(["serve", "--prot", "/srv"])
        .build();

    assert_eq!(serve().run(system, ForeignBindings::empty()), Ok(2));
    // `output` is both streams in write order, so "nothing reached stdout" is "everything written was the stderr half".
    assert_eq!(
        io.output(),
        io.errors(),
        "a refusal writes nothing to stdout:\n{}",
        String::from_utf8_lossy(&io.output())
    );
    assert_eq!(
        String::from_utf8(io.errors()).expect("the refusal is text"),
        "error: unexpected argument '--prot'\n\nUsage: serve [--verbose] [--port <N>] <ROOT>\n\nFor more information, try '--help'.\n"
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

    assert_eq!(serve().run(system, ForeignBindings::empty()), Ok(2));
    assert!(
        String::from_utf8_lossy(&io.errors()).contains("argument 1 is not valid UTF-8"),
        "the refusal names which argument:\n{}",
        String::from_utf8_lossy(&io.errors())
    );
}

/// A valid nested command tree, kept separate from invalid-specification fixtures.
fn deps() -> Compiled {
    compile(
        r#"
        use /std/{Cli, Str, Option, print};

        let add: Cli/Declaration =
            Cli/leaf(
                "add",
                "Add a dependency",
                Option/none(),
                Cli/record([Cli/positional("name", Cli/str, "What to add")]),
                (v) => print(Str/flatten(["adding ", Cli/get(v, "name"), "\n"])));

        let registry: Cli/Declaration =
            Cli/Declaration/group("registry", "Manage registry dependencies", Option/none(), [add]);

        let deps: Cli/Declaration =
            Cli/Declaration/group("deps", "Manage dependencies", Option/some("0.1.0"), [registry]);

        Cli/main(Cli/Declaration/declare(deps))
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

    assert_eq!(deps().run(system, ForeignBindings::empty()), Ok(0));
    assert_eq!(io.output(), b"adding curios\n");
}

/// Help answered inside a subcommand names the commands that lead to it, so its usage line is the line that reaches the command.
#[test]
fn help_inside_a_subcommand_names_the_path_that_reaches_it() {
    let (system, io) = MockHost::builder()
        .args(["deps", "registry", "add", "--help"])
        .build();

    assert_eq!(deps().run(system, ForeignBindings::empty()), Ok(0));
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

    assert_eq!(deps().run(system, ForeignBindings::empty()), Ok(2));
    let refusal = String::from_utf8(io.errors()).expect("the refusal is text");
    assert!(
        refusal.contains(
            "error: the argument 'NAME' is required\n\nUsage: deps registry add <NAME>\n\n"
        ),
        "the refusal names the path:\n{refusal}"
    );
}
