//! What `/std/Cli` refuses at compile time, and what `Cli/main` writes where. The surface `parse` and `help` accept is the corpus unit's; neither a refusal nor a stream nor an exit status is a `Test` a program can declare, so those stay here.

use {
    super::{compile, error},
    curios_runtime::{Compiled, ForeignBindings, MockHost},
};

/// The declaration every refusal below is written against, beside the line `body` declares after it.
fn against(body: &str) -> String {
    error(&format!(
        r#"
        use /std/{{Cli, Bool, Option, print}};

        let deps: Cli =
            Cli/declare(
                Cli/group(
                    "deps",
                    "Manage dependencies",
                    [],
                    [Cli/leaf("add", "Add one", [Cli/default("port", Cli/nat, 8080, "Port")], Cli/positionals([]))]),
                Option/none());

        {body}
        print("unreachable")
        "#
    ))
}

/// A name the declaration lacks is refused where it is written: `Has` reduces to `False`, and the bound holding `get` up is discharged by nothing. The binding is unannotated so the bound is what fails — an expected type would report `Lookup`'s `{}` first and say nothing about why.
#[test]
fn a_misspelled_name_is_refused_rather_than_answered() {
    let refused = against(
        r#"pub let read(p: Cli/Parsed(deps.command)) -> Bool =
            match Cli/command(p, "add") | some(a) => let _port = Cli/get(a, "prot"); true | none() => false end;"#,
    );

    assert!(
        refused.contains("Cli/Has(") && refused.contains("which reduces to Bool/False"),
        "the refusal names the bound nothing discharged and what it came to:\n{refused}"
    );
}

/// A command the group lacks is refused the same way, by `HasCommand`.
#[test]
fn a_misspelled_command_is_refused_rather_than_answered() {
    let refused = against(
        r#"pub let read(p: Cli/Parsed(deps.command)) -> Bool =
            match Cli/command(p, "ad") | some(_) => true | none() => false end;"#,
    );

    assert!(
        refused.contains("Cli/HasCommand(") && refused.contains("which reduces to Bool/False"),
        "{refused}"
    );
}

/// A fixed declaration with two options answering to `--port` is refused where it is written, by the problem `checked` would report, so the reason is the declaration's and not a guard's.
#[test]
fn a_duplicate_name_is_refused_where_it_is_written_by_its_problem() {
    let refused = error(
        r#"
        use /std/{Cli, Option, print};

        let twice: Cli =
            Cli/declare(
                Cli/leaf(
                    "serve",
                    "Serve",
                    [Cli/default("port", Cli/nat, 8080, "Port"), Cli/Field { ..Cli/flag("verbose", "Loud"), aliases = ["port"] }],
                    Cli/positionals([])),
                Option/none());

        Cli/main(twice, (_) => print("unreachable"))
        "#,
    );

    assert!(
        refused.contains(r#"which reduces to Cli/Refused([Cli/Problem/duplicate_name(Cli/Spelled/long("port"))])"#),
        "{refused}"
    );
}

/// A problem beneath a group refuses the group where it is written, within the command that holds it: a command beside the one a line chooses is checked as much as that one.
#[test]
fn a_problem_beneath_a_group_refuses_the_group_where_it_is_written() {
    let refused = error(
        r#"
        use /std/{Cli, Option, print};

        let deps: Cli =
            Cli/declare(
                Cli/group(
                    "deps",
                    "Manage dependencies",
                    [],
                    [
                        Cli/leaf("add", "Add one", [], Cli/positionals([])),
                        Cli/leaf("rm", "Remove one", [Cli/flag("help", "Mine")], Cli/positionals([]))]),
                Option/none());

        Cli/main(deps, (_) => print("unreachable"))
        "#,
    );

    assert!(
        refused.contains(
            r#"Cli/Problem/within("rm", Cli/Problem/duplicate_name(Cli/Spelled/long("help")))"#
        ),
        "{refused}"
    );
}

/// A literal states the evidence as `declare` does, and an update of a declared command line states it again: a command line changed into one with problems keeps none of the evidence it was built with.
#[test]
fn a_command_line_changed_into_one_with_problems_is_refused() {
    let accepted = compile(
        r#"
        use /std/{Cli, Option, print};

        let serve: Cli = Cli { command = Cli/leaf("serve", "Serve", [], Cli/positionals([])), version = Option/none() };

        Cli/main(serve, (_) => print("ran"))
        "#,
    );
    assert!(accepted.is_ok(), "{:?}", accepted.err());

    let refused = error(
        r#"
        use /std/{Cli, Option, print};

        let serve: Cli = Cli { command = Cli/leaf("serve", "Serve", [], Cli/positionals([])), version = Option/none() };
        let changed: Cli = Cli { ..serve, command = Cli/leaf("serve", "Serve", [Cli/flag("help", "Mine")], Cli/positionals([])) };

        Cli/main(changed, (_) => print("unreachable"))
        "#,
    );
    assert!(refused.contains("Cli/Refused("), "{refused}");
}

/// A program built once and run under several scripted command lines: the compile is what a fixture pays for, and a run of the compiled module is milliseconds.
fn serve() -> Compiled {
    compile(
        r#"
        use /std/{Cli, Nat, Str, Option, print};

        let serve: Cli =
            Cli/declare(
                Cli/leaf(
                    "serve",
                    "Serve a directory over HTTP",
                    [
                        Cli/Field { ..Cli/flag("verbose", "Log every request"), short = Option/some('v') },
                        Cli/Field { ..Cli/default("port", Cli/nat, 8080, "Port to listen on"), env = Option/some("PORT") }],
                    Cli/positionals([Cli/positional("root", Cli/str, "Directory to serve")])),
                Option/some("0.1.0"));

        Cli/main(serve, (p) =>
            let port = Cli/get(p, "port");
            let root = Cli/get(p, "root");
            print(Str/flatten(["serving ", root, " on ", Nat/to_str(port), "\n"])))
        "#,
    )
    .expect("the entry compiles")
}

/// A line the declaration accepts runs `run`, writes nothing to standard error, and exits 0.
#[test]
fn a_good_line_runs_the_program_and_exits_zero() {
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

/// A variable the declaration names is read from the host where the line leaves its option out.
#[test]
fn a_declared_variable_is_read_from_the_host() {
    let (system, io) = MockHost::builder()
        .args(["serve", "/srv"])
        .env([("PORT", "7070")])
        .build();

    assert_eq!(serve().run(system, ForeignBindings::empty()), Ok(0));
    assert_eq!(io.output(), b"serving /srv on 7070\n");
}

/// Help and the version are requests, not mistakes: standard output and exit 0, even over a line that would otherwise have parsed.
#[test]
fn help_and_the_version_answer_on_stdout_and_exit_zero() {
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

    let (system, io) = MockHost::builder().args(["serve", "--version"]).build();

    assert_eq!(serve().run(system, ForeignBindings::empty()), Ok(0));
    assert_eq!(io.output(), b"0.1.0\n");
}

/// A line the declaration refuses goes to standard *error* and exits 2 — the distinction the scripted host's separate record of standard error makes visible, since a concatenation of both streams cannot show it.
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
        "a refusal writes nothing to stdout"
    );
    assert_eq!(
        String::from_utf8(io.errors()).expect("the refusal is text"),
        "error: unexpected argument '--prot'\n\n  tip: a similar argument exists: '--port'\n\nUsage: serve [--verbose] [--port <N>] <ROOT>\n\nFor more information, try '--help'.\n"
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

/// A command line of groups, read by a program that reaches a command two groups down through `command`.
fn deps() -> Compiled {
    compile(
        r#"
        use /std/{Cli, Str, Option, print};

        let deps: Cli =
            Cli/declare(
                Cli/group(
                    "deps",
                    "Manage dependencies",
                    [],
                    [
                        Cli/group(
                            "registry",
                            "Manage registry dependencies",
                            [],
                            [Cli/leaf("add", "Add a dependency", [], Cli/positionals([Cli/positional("name", Cli/str, "What to add")]))])]),
                Option/some("0.1.0"));

        Cli/main(deps, (p) =>
            match Cli/command(p, "registry")
            | some(r) =>
                match Cli/command(r, "add")
                | some(a) => print(Str/flatten(["adding ", Cli/get(a, "name"), "\n"]))
                | none() => print("nothing\n")
                end
            | none() => print("nothing\n")
            end)
        "#,
    )
    .expect("the entry compiles")
}

/// A group hands the line to the command its next word names, to any depth, so a command two groups down is read under `Cli/main` alone.
#[test]
fn a_command_two_groups_down_is_read() {
    let (system, io) = MockHost::builder()
        .args(["deps", "registry", "add", "curios"])
        .build();

    assert_eq!(deps().run(system, ForeignBindings::empty()), Ok(0));
    assert_eq!(io.output(), b"adding curios\n");
}

/// Help asked inside a command names the commands that lead to it, so its usage line is the line that reaches the command.
#[test]
fn help_inside_a_command_names_the_path_that_reaches_it() {
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

/// A refusal inside a command carries the same path, on standard error with exit 2 like any other refusal.
#[test]
fn a_refusal_inside_a_command_names_the_path_that_reaches_it() {
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

/// A declaration checked as the program runs and found to have problems is reported on standard error and ends the program with 1, before anything on the line is read: help, the version and an argument that is not text alike.
#[test]
fn a_declaration_with_problems_ends_the_program_with_one_before_the_line_is_read() {
    let compiled = compile(
        r#"
        use /std/{Cli, Option, print};

        Cli/main_checked(
            Cli/leaf("serve", "Serve", [Cli/flag("help", "Mine")], Cli/positionals([])),
            Option/some("0.1.0"),
            (_) => print("unreachable"))
        "#,
    )
    .expect("the entry compiles");

    for args in [
        vec![b"serve".to_vec()],
        vec![b"serve".to_vec(), b"--help".to_vec()],
        vec![b"serve".to_vec(), b"--version".to_vec()],
        vec![b"serve".to_vec(), b"\xff".to_vec()],
    ] {
        let (system, io) = MockHost::builder().args(args).build();

        assert_eq!(compiled.run(system, ForeignBindings::empty()), Ok(1));
        assert_eq!(
            String::from_utf8(io.errors()).expect("the report is text"),
            "error: invalid command line declaration: duplicate name --help\n"
        );
        assert_eq!(io.output(), io.errors(), "nothing reaches stdout");
    }
}
