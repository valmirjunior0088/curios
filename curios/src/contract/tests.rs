//! What every command admits: each way its argument can be spelled, from each place an invocation can stand, parsed as the command line spells it and admitted through the contract the command has. The table is written by hand from what the commands do, so a change to what any command accepts is a change to it.

use {
    super::*,
    crate::Cli,
    clap::{CommandFactory, Parser},
    curios_utilities::test_support::Temporary,
    std::{collections::BTreeSet, fmt::Write, fs, iter},
};

/// Where a command line spells its target.
const TARGET: &str = "TARGET";

/// Every command, as the command line that invokes it.
///
/// A function rather than a `const`, because the set the tree actually holds is feature-dependent — `profile` carries a command of its own — and `#[cfg]` does not reach an element of an array literal. What [`every_command_is_listed`] compares this against is the tree clap built for *this* build, so the table has to vary exactly as that tree does or one of the two builds would be checked against the other's surface.
fn commands() -> Vec<&'static [&'static str]> {
    #[allow(unused_mut)]
    let mut lines: Vec<&'static [&'static str]> = vec![
        &["run", TARGET],
        &["compile", TARGET],
        &["compile", TARGET, "--output", "out"],
        &["document", TARGET],
        &["document", "--archive", "unit.rkyv", "--output", "site"],
        &["test", TARGET],
        &["curate"],
        &["pin", "foreign", "probe", "--path", "probe.wasm"],
        &[
            "pin",
            "foreign",
            "probe",
            "--url",
            "https://example.invalid/probe.wasm",
        ],
        &["pin", "foreign", "probe", "--refresh"],
        &["pin", "dependency", "json", "--path", "../json"],
        &[
            "pin",
            "dependency",
            "json",
            "--url",
            "https://example.invalid/json",
            "--rev",
            "abc",
        ],
        &["pin", "dependency", "json", "--rev", "abc"],
        &["pin", "dependency", "json", "--refresh"],
        &["new", "fresh"],
        &["lint", TARGET],
        &["format", TARGET],
        &["wonder", "diagnostics", TARGET],
        &["wonder", "tests", TARGET],
        &["wonder", "cost", TARGET],
        &["wonder", "stage", "core", TARGET],
        &["wonder", "server"],
    ];

    #[cfg(feature = "profile")]
    lines.push(&["profile", "stream.tsv"]);

    lines
}

/// Where an invocation stands under the tree's root, and the argument it spells there — `None` for none.
const ROWS: &[(&str, Option<&str>)] = &[
    ("work/app", None),
    ("work/app", Some("serve")),
    ("work/app", Some("absent")),
    ("work/app", Some("-")),
    ("work/app", Some("serve.crs")),
    ("work/app", Some("serve/helper.crs")),
    ("work/app", Some("bench.crs")),
    ("work/app", Some("lib.crs")),
    ("work/app", Some("util.crs")),
    ("work/app", Some("stray.crs")),
    ("work/app", Some("nested/lib.crs")),
    ("work/app", Some("<root>/scratch.crs")),
    ("work/app", Some("missing.crs")),
    ("work", None),
    ("work", Some("serve")),
    ("work/app/serve", None),
    ("work/app/serve", Some("bench")),
    ("work/app/nested", None),
    (".", None),
];

/// An umbrella enumerating one package, and beside the umbrella a file no manifest governs. The package declares a library with a module, two executables — the `default` one with a module its entry declares — and holds a file nothing declares and a package nothing enumerates.
fn tree() -> Temporary {
    let root = Temporary::new("contract", "table");

    for (path, text) in [
        ("work/curios.toml", "members = [\"app\"]\n"),
        (
            "work/app/curios.toml",
            "name = \"app\"\ndefault = \"serve\"\n\n[[executables]]\nname = \"serve\"\n\n[[executables]]\nname = \"bench\"\n",
        ),
        ("work/app/lib.crs", "pub mod util;\n"),
        ("work/app/util.crs", ""),
        ("work/app/stray.crs", ""),
        ("work/app/serve.crs", "mod helper;\n/std/print(\"\")\n"),
        ("work/app/serve/helper.crs", ""),
        ("work/app/bench.crs", "/std/print(\"\")\n"),
        ("work/app/nested/curios.toml", "name = \"nested\"\n"),
        ("work/app/nested/lib.crs", ""),
        ("scratch.crs", ""),
    ] {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }

    root
}

/// `line` as the parser receives it, `TARGET` replaced by `argument` or dropped when there is none.
fn parsed(line: &[&str], argument: Option<&str>) -> Mode {
    let words = iter::once("curios").chain(line.iter().filter_map(|word| match *word == TARGET {
        true => argument,
        false => Some(*word),
    }));

    Cli::try_parse_from(words)
        .unwrap_or_else(|error| panic!("{}: {error}", line.join(" ")))
        .mode
}

/// The subcommand `line` names, walked from the root for as long as its words name subcommands.
fn subcommand(line: &[&str]) -> clap::Command {
    let mut command = Cli::command();

    for word in line {
        let Some(subcommand) = command.find_subcommand(word).cloned() else {
            break;
        };
        command = subcommand;
    }

    command
}

/// The long options the command `line` names takes, pinned beside its contract, so a command that elaborates without a budget, or takes a flag it never reads, is a row written by hand.
fn options(line: &[&str]) -> String {
    let options = subcommand(line)
        .get_arguments()
        .filter(|argument| !argument.is_global_set())
        .filter_map(|argument| argument.get_long())
        .map(|long| format!("--{long}"))
        .collect::<Vec<_>>();

    match options.is_empty() {
        true => "none".to_string(),
        false => options.join(" "),
    }
}

/// Where a row stands: the tree's root, and the directory under it the invocation starts in.
struct Standing<'a> {
    root: &'a Path,
    directory: PathBuf,
}

impl Standing<'_> {
    /// `written` as the parser receives it: `<root>` spelled out, and a path from where the invocation stands made whole, since a test cannot move the process to stand there.
    fn argument(&self, written: &str) -> String {
        let written = written.replace("<root>", &self.root.display().to_string());

        match Spelling::of(Some(&written)) {
            Spelling::File(path) => self.directory.join(path).display().to_string(),
            Spelling::Stdin | Spelling::Name(_) | Spelling::Nothing => written,
        }
    }

    /// `text` with the tree's root written as `<root>`, so a row reads the same whichever directory the test was given.
    fn rooted(&self, text: &str) -> String {
        text.replace(&self.root.display().to_string(), "<root>")
    }

    /// `path` as a row writes it: from where the invocation stands when it lies under there, and from the tree's root otherwise.
    fn path(&self, path: &Path) -> String {
        match path.strip_prefix(&self.directory) {
            Ok(relative) => relative.display().to_string(),
            Err(_) => self.rooted(&path.display().to_string()),
        }
    }

    fn through(&self, file: Option<&Path>) -> String {
        file.map(|file| format!(" through {}", self.path(file)))
            .unwrap_or_default()
    }

    /// A program as a row writes it: declared by its executable's name, or loose by its entry — marked when a package holds the file and declares it nowhere.
    fn program(&self, program: &Program) -> String {
        let unlinked = match program.unlinked() {
            Some(_) => " unlinked",
            None => "",
        };

        match (program.home(), program.entry()) {
            (Some(home), _) => format!(
                "program {}{}",
                home.executable,
                self.through(program.through())
            ),
            (None, Entry::Stdin) => "loose -".to_string(),
            (None, Entry::File(path)) => format!("loose {}{unlinked}", self.path(path)),
        }
    }

    fn library(&self, library: &Library) -> String {
        format!(
            "library {}{}",
            library.package,
            self.through(library.through.as_deref())
        )
    }

    fn rewritten(&self, rewritten: &Rewritten) -> String {
        match rewritten {
            Rewritten::Files(paths) => format!(
                "files {}",
                paths
                    .iter()
                    .map(|path| self.path(path))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Rewritten::Stdin => "standard input".to_string(),
        }
    }

    /// What `mode` admits its argument as, standing here, or the refusal it earns.
    fn admitted(&self, mode: &Mode) -> String {
        let contract = mode.contract();
        let target = mode.target();
        let directory = self.directory.as_path();

        let admitted = match contract.accepts {
            Accepts::Program => contract
                .admit_program(target, None, mode.output(), directory)
                .map(|program| self.program(&program)),
            Accepts::Library => contract
                .admit_library(target, None, directory)
                .map(|library| self.library(&library)),
            Accepts::Any => contract
                .admit_any(target, None, directory)
                .map(|selection| match selection {
                    Selection::Program(program) => self.program(&program),
                    Selection::Library(library) => self.library(&library),
                    Selection::Entire(entire) => package_entire(&entire),
                }),
            Accepts::Entire => contract
                .admit_entire(target, None, directory)
                .map(|entire| package_entire(&entire)),
            Accepts::Files => contract
                .admit_files(target, None, directory)
                .map(|rewritten| self.rewritten(&rewritten)),
            Accepts::Nothing => Ok("takes no subject".to_string()),
        };

        match admitted {
            Ok(subject) => subject,
            Err(refusal) => format!("refused: {}", self.rooted(&refusal)),
        }
    }
}

fn package_entire(entire: &Entire) -> String {
    format!("entire {}", entire.governing.package.name)
}

/// Every command's contract and every row it admits, as [`EXPECTED`] writes them.
fn table(root: &Path) -> String {
    let mut table = String::new();

    for line in commands() {
        let contract = parsed(line, None).contract();
        let own = match contract.own_file_only {
            true => " (its own file)",
            false => "",
        };
        writeln!(
            table,
            "{} — {:?}{own}, store {:?}, leaves {:?}, options {}",
            line.join(" "),
            contract.accepts,
            contract.access,
            contract.product,
            options(line)
        )
        .unwrap();

        // A command with no subject is the same from everywhere, so it is one row.
        if contract.accepts == Accepts::Nothing {
            writeln!(table, "  takes no subject").unwrap();
            continue;
        }

        let takes_target = line.contains(&TARGET);
        for &(standpoint, written) in ROWS {
            // A command taking no target reads no argument, so only the rows spelling none are its.
            if !takes_target && written.is_some() {
                continue;
            }

            let standing = Standing {
                root,
                directory: root.join(standpoint),
            };
            let argument = written.map(|written| standing.argument(written));
            let admitted = standing.admitted(&parsed(line, argument.as_deref()));

            writeln!(
                table,
                "  {standpoint}: {} → {admitted}",
                written.unwrap_or("(none)")
            )
            .unwrap();
        }
    }

    table
}

/// The section a profiling build adds, written the way [`table`] writes an [`Accepts::Nothing`] command: one header and the single row that says it resolves nothing.
///
/// Separate from [`EXPECTED`] for the reason [`commands`] is a function — the surface varies with the feature, so the expectation must vary with it, or one build would be held to the other's surface.
#[cfg(feature = "profile")]
const PROFILE_EXPECTED: &str =
    "profile stream.tsv — Nothing, store None, leaves Nothing, options none\n  takes no subject\n";

/// What [`table`] must read for the tree *this* build holds.
fn expected() -> String {
    #[allow(unused_mut)]
    let mut expected = String::from(EXPECTED);

    #[cfg(feature = "profile")]
    expected.push_str(PROFILE_EXPECTED);

    expected
}

const EXPECTED: &str = r#"run TARGET — Program (its own file), store Write, leaves Nothing, options --budget --manifest
  work/app: (none) → program serve
  work/app: serve → program serve
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → loose -
  work/app: serve.crs → program serve
  work/app: serve/helper.crs → refused: `run` takes a program's own file, and <root>/work/app/serve/helper.crs is a module of `serve`: name `serve` instead
  work/app: bench.crs → program bench
  work/app: lib.crs → refused: `run` takes a program, and a library is not one: name an executable or a program file
  work/app: util.crs → refused: `run` takes a program, and a library is not one: name an executable or a program file
  work/app: stray.crs → loose stray.crs unlinked
  work/app: nested/lib.crs → refused: `run` takes a program, and a library is not one: name an executable or a program file
  work/app: <root>/scratch.crs → loose <root>/scratch.crs
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → program serve
  work/app/serve: bench → program bench
  work/app/nested: (none) → refused: "nested" declares no executable: add `exe.crs`, or declare one with `[[executables]]`
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
compile TARGET — Program (its own file), store Write, leaves Executable, options --output --budget --manifest
  work/app: (none) → program serve
  work/app: serve → program serve
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → refused: `compile` files what it builds under the package that declares it, and standard input has none: say where, as in `curios compile - --output program`
  work/app: serve.crs → program serve
  work/app: serve/helper.crs → refused: `compile` takes a program's own file, and <root>/work/app/serve/helper.crs is a module of `serve`: name `serve` instead
  work/app: bench.crs → program bench
  work/app: lib.crs → refused: `compile` takes a program, and a library is not one: name an executable or a program file
  work/app: util.crs → refused: `compile` takes a program, and a library is not one: name an executable or a program file
  work/app: stray.crs → refused: `compile` files what it builds under the package that declares it, and <root>/work/app/stray.crs has none: say where, as in `curios compile <root>/work/app/stray.crs --output stray`
  work/app: nested/lib.crs → refused: `compile` takes a program, and a library is not one: name an executable or a program file
  work/app: <root>/scratch.crs → refused: `compile` files what it builds under the package that declares it, and <root>/scratch.crs has none: say where, as in `curios compile <root>/scratch.crs --output scratch`
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → program serve
  work/app/serve: bench → program bench
  work/app/nested: (none) → refused: "nested" declares no executable: add `exe.crs`, or declare one with `[[executables]]`
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
compile TARGET --output out — Program (its own file), store Write, leaves Executable, options --output --budget --manifest
  work/app: (none) → program serve
  work/app: serve → program serve
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → loose -
  work/app: serve.crs → program serve
  work/app: serve/helper.crs → refused: `compile` takes a program's own file, and <root>/work/app/serve/helper.crs is a module of `serve`: name `serve` instead
  work/app: bench.crs → program bench
  work/app: lib.crs → refused: `compile` takes a program, and a library is not one: name an executable or a program file
  work/app: util.crs → refused: `compile` takes a program, and a library is not one: name an executable or a program file
  work/app: stray.crs → loose stray.crs unlinked
  work/app: nested/lib.crs → refused: `compile` takes a program, and a library is not one: name an executable or a program file
  work/app: <root>/scratch.crs → loose <root>/scratch.crs
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → program serve
  work/app/serve: bench → program bench
  work/app/nested: (none) → refused: "nested" declares no executable: add `exe.crs`, or declare one with `[[executables]]`
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
document TARGET — Library, store Write, leaves Pages, options --archive --output --budget --manifest
  work/app: (none) → library app
  work/app: serve → refused: `document` takes a library, and a program is not one
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → refused: `document` takes a library, and a program is not one
  work/app: serve.crs → refused: `document` takes a library, and a program is not one
  work/app: serve/helper.crs → refused: `document` takes a library, and a program is not one
  work/app: bench.crs → refused: `document` takes a library, and a program is not one
  work/app: lib.crs → library app through lib.crs
  work/app: util.crs → library app through util.crs
  work/app: stray.crs → refused: `document` takes a library, and a program is not one
  work/app: nested/lib.crs → library nested through nested/lib.crs
  work/app: <root>/scratch.crs → refused: `document` takes a library, and a program is not one
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → library app
  work/app/serve: bench → refused: `document` takes a library, and a program is not one
  work/app/nested: (none) → library nested
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
document --archive unit.rkyv --output site — Nothing, store None, leaves Pages, options --archive --output --budget --manifest
  takes no subject
test TARGET — Any, store Write, leaves Nothing, options --filter --budget --manifest
  work/app: (none) → entire app
  work/app: serve → program serve
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → loose -
  work/app: serve.crs → program serve
  work/app: serve/helper.crs → program serve through serve/helper.crs
  work/app: bench.crs → program bench
  work/app: lib.crs → library app through lib.crs
  work/app: util.crs → library app through util.crs
  work/app: stray.crs → loose stray.crs unlinked
  work/app: nested/lib.crs → library nested through nested/lib.crs
  work/app: <root>/scratch.crs → loose <root>/scratch.crs
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → entire app
  work/app/serve: bench → program bench
  work/app/nested: (none) → entire nested
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
curate — Entire, store None, leaves Sources, options --manifest
  work/app: (none) → entire app
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → entire app
  work/app/nested: (none) → entire nested
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
pin foreign probe --path probe.wasm — Nothing, store None, leaves Manifest, options --path --url --refresh --check --manifest
  takes no subject
pin foreign probe --url https://example.invalid/probe.wasm — Nothing, store None, leaves Manifest, options --path --url --refresh --check --manifest
  takes no subject
pin foreign probe --refresh — Nothing, store None, leaves Manifest, options --path --url --refresh --check --manifest
  takes no subject
pin dependency json --path ../json — Nothing, store None, leaves Manifest, options --path --url --rev --refresh --check --manifest
  takes no subject
pin dependency json --url https://example.invalid/json --rev abc — Nothing, store None, leaves Manifest, options --path --url --rev --refresh --check --manifest
  takes no subject
pin dependency json --rev abc — Nothing, store None, leaves Manifest, options --path --url --rev --refresh --check --manifest
  takes no subject
pin dependency json --refresh — Nothing, store None, leaves Manifest, options --path --url --rev --refresh --check --manifest
  takes no subject
new fresh — Nothing, store None, leaves Package, options none
  takes no subject
lint TARGET — Any, store Read, leaves Nothing, options --budget --manifest
  work/app: (none) → entire app
  work/app: serve → program serve
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → loose -
  work/app: serve.crs → program serve
  work/app: serve/helper.crs → program serve through serve/helper.crs
  work/app: bench.crs → program bench
  work/app: lib.crs → library app through lib.crs
  work/app: util.crs → library app through util.crs
  work/app: stray.crs → loose stray.crs unlinked
  work/app: nested/lib.crs → library nested through nested/lib.crs
  work/app: <root>/scratch.crs → loose <root>/scratch.crs
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → entire app
  work/app/serve: bench → program bench
  work/app/nested: (none) → entire nested
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
format TARGET — Files, store None, leaves Rewritten, options --check --manifest
  work/app: (none) → files lib.crs util.crs serve.crs serve/helper.crs bench.crs
  work/app: serve → files serve.crs serve/helper.crs
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → standard input
  work/app: serve.crs → files serve.crs
  work/app: serve/helper.crs → files serve/helper.crs
  work/app: bench.crs → files bench.crs
  work/app: lib.crs → files lib.crs
  work/app: util.crs → files util.crs
  work/app: stray.crs → files stray.crs
  work/app: nested/lib.crs → files nested/lib.crs
  work/app: <root>/scratch.crs → files <root>/scratch.crs
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → files <root>/work/app/lib.crs <root>/work/app/util.crs <root>/work/app/serve.crs helper.crs <root>/work/app/bench.crs
  work/app/serve: bench → files <root>/work/app/bench.crs
  work/app/nested: (none) → files lib.crs
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
wonder diagnostics TARGET — Any, store Read, leaves Nothing, options --budget --manifest
  work/app: (none) → entire app
  work/app: serve → program serve
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → loose -
  work/app: serve.crs → program serve
  work/app: serve/helper.crs → program serve through serve/helper.crs
  work/app: bench.crs → program bench
  work/app: lib.crs → library app through lib.crs
  work/app: util.crs → library app through util.crs
  work/app: stray.crs → loose stray.crs unlinked
  work/app: nested/lib.crs → library nested through nested/lib.crs
  work/app: <root>/scratch.crs → loose <root>/scratch.crs
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → entire app
  work/app/serve: bench → program bench
  work/app/nested: (none) → entire nested
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
wonder tests TARGET — Any, store Read, leaves Nothing, options --budget --manifest
  work/app: (none) → entire app
  work/app: serve → program serve
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → loose -
  work/app: serve.crs → program serve
  work/app: serve/helper.crs → program serve through serve/helper.crs
  work/app: bench.crs → program bench
  work/app: lib.crs → library app through lib.crs
  work/app: util.crs → library app through util.crs
  work/app: stray.crs → loose stray.crs unlinked
  work/app: nested/lib.crs → library nested through nested/lib.crs
  work/app: <root>/scratch.crs → loose <root>/scratch.crs
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → entire app
  work/app/serve: bench → program bench
  work/app/nested: (none) → entire nested
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
wonder cost TARGET — Program, store Read, leaves Nothing, options --budget --manifest
  work/app: (none) → program serve
  work/app: serve → program serve
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → loose -
  work/app: serve.crs → program serve
  work/app: serve/helper.crs → program serve through serve/helper.crs
  work/app: bench.crs → program bench
  work/app: lib.crs → refused: `wonder cost` takes a program, and a library is not one: name an executable or a program file
  work/app: util.crs → refused: `wonder cost` takes a program, and a library is not one: name an executable or a program file
  work/app: stray.crs → loose stray.crs unlinked
  work/app: nested/lib.crs → refused: `wonder cost` takes a program, and a library is not one: name an executable or a program file
  work/app: <root>/scratch.crs → loose <root>/scratch.crs
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → program serve
  work/app/serve: bench → program bench
  work/app/nested: (none) → refused: "nested" declares no executable: add `exe.crs`, or declare one with `[[executables]]`
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
wonder stage core TARGET — Program, store Read, leaves Nothing, options --budget --manifest
  work/app: (none) → program serve
  work/app: serve → program serve
  work/app: absent → refused: "app" declares no executable named "absent"; it declares the executable "serve", the executable "bench"
  work/app: - → loose -
  work/app: serve.crs → program serve
  work/app: serve/helper.crs → program serve through serve/helper.crs
  work/app: bench.crs → program bench
  work/app: lib.crs → refused: `wonder stage` takes a program, and a library is not one: name an executable or a program file
  work/app: util.crs → refused: `wonder stage` takes a program, and a library is not one: name an executable or a program file
  work/app: stray.crs → loose stray.crs unlinked
  work/app: nested/lib.crs → refused: `wonder stage` takes a program, and a library is not one: name an executable or a program file
  work/app: <root>/scratch.crs → loose <root>/scratch.crs
  work/app: missing.crs → refused: failed to read <root>/work/app/missing.crs: No such file or directory (os error 2)
  work: (none) → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work: serve → refused: <root>/work/curios.toml declares an umbrella, and an umbrella compiles nothing of its own: work in one of its members instead
  work/app/serve: (none) → program serve
  work/app/serve: bench → program bench
  work/app/nested: (none) → refused: "nested" declares no executable: add `exe.crs`, or declare one with `[[executables]]`
  .: (none) → refused: no `curios.toml` in <root> or any directory above it; run a `.crs` file by name, or work inside a package
wonder server — Nothing, store Read, leaves Nothing, options --budget --manifest
  takes no subject
"#;

/// **The table.** Every command, admitted from every row it reads.
#[test]
fn every_command_admits_what_the_table_says() {
    let root = tree();
    let table = table(&root);

    assert!(table == expected(), "the table now reads:\n{table}");
}

/// A flag a command reads follows the command, and written before it is refused with the spelling that works — or, for a command that takes no such flag, with why.
#[test]
fn a_flag_written_before_its_command_is_refused_with_where_it_goes() {
    let refusal = |words: &[&str]| {
        Cli::try_parse_from(iter::once("curios").chain(words.iter().copied()))
            .unwrap_or_else(|error| panic!("{}: {error}", words.join(" ")))
            .misplaced()
    };

    assert_eq!(
        refusal(&["wonder", "diagnostics", "--manifest", "curios.toml"]),
        None
    );
    assert_eq!(refusal(&["run", "--budget", "5", "serve"]), None);
    assert_eq!(
        refusal(&["--manifest", "curios.toml", "wonder", "diagnostics"]).as_deref(),
        Some(
            "`--manifest` belongs to the command, so it follows it: `curios wonder diagnostics --manifest curios.toml`"
        )
    );
    assert_eq!(
        refusal(&["--budget", "5", "run", "serve"]).as_deref(),
        Some("`--budget` belongs to the command, so it follows it: `curios run --budget 5`")
    );
    assert_eq!(
        refusal(&["--budget", "5", "new", "fresh"]).as_deref(),
        Some("`new` elaborates nothing, so it takes no `--budget`")
    );
    assert_eq!(
        refusal(&["--manifest", "curios.toml", "new", "fresh"]).as_deref(),
        Some("`new` reads no manifest, so it takes no `--manifest`")
    );
}

/// Every command the parser knows has a line in the table, so a command added without one fails here rather than going unexamined.
#[test]
fn every_command_the_parser_knows_is_in_the_table() {
    fn leaves(command: &clap::Command, path: &[&str], known: &mut BTreeSet<String>) {
        let subcommands = command
            .get_subcommands()
            .filter(|subcommand| subcommand.get_name() != "help")
            .collect::<Vec<_>>();

        if subcommands.is_empty() {
            known.insert(path.join(" "));
        }

        for subcommand in subcommands {
            let path = [path, &[subcommand.get_name()]].concat();
            leaves(subcommand, &path, known);
        }
    }

    let mut known = BTreeSet::new();
    leaves(&Cli::command(), &[], &mut known);

    let listed = commands()
        .iter()
        .map(|line| {
            let mut command = Cli::command();
            let mut path = Vec::new();
            for word in *line {
                let Some(subcommand) = command.find_subcommand(word).cloned() else {
                    break;
                };
                path.push(*word);
                command = subcommand;
            }
            path.join(" ")
        })
        .collect::<BTreeSet<_>>();

    assert_eq!(listed, known);
}
