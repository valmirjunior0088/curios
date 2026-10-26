//! What the CLI does across two invocations of one target — the half of payload reuse that is a command-line decision rather than a store mechanism.
//!
//! The store's own behaviour is covered where it lives, in `curios-verdicts`'s payload tests: those decide when a slot may be believed. These decide what the *subcommands* do with the answer — that one slot serves `run` and `compile` alike, that a hit reports itself, that a program's output and exit code do not depend on whether it was compiled just now, that a stage query files nothing, and that a bare file has nothing to do with any of it.
//!
//! Not `#[ignore]`d, unlike `bundle`: nothing here execs a produced executable. `curios run` runs its program in-process.

use {
    curios_package::Target,
    curios_utilities::test_support::Temporary,
    std::{
        fs,
        path::Path,
        process::{Command, Output},
    },
};

/// A directory of its own, shared with no other test and gone with it.
fn temporary(name: &str) -> Temporary {
    Temporary::new("cli-payload", name)
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// A package declaring one executable that prints and then exits with a code of its own.
fn project(name: &str) -> Temporary {
    let root = temporary(name);

    write(
        &root,
        "curios.toml",
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(&root, "lib.crs", "pub let word : /std/Str = \"reused\";\n");
    write(
        &root,
        "app.crs",
        "let main() -> /std/Io({}) = let _ = /std/print(/app/word)!;\n/std/Process/exit(7);\nmain()\n",
    );

    root
}

/// Run the compiler in `root`, with the arguments given.
fn curios(root: &Path, arguments: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_curios"))
        .current_dir(root)
        .args(arguments)
        .output()
        .expect("run the compiler")
}

/// Whether an invocation was served from the store, read off the report it wrote — which is the same question, asked the way a person at a terminal asks it.
fn reused(output: &Output) -> bool {
    String::from_utf8_lossy(&output.stderr).contains("; reused")
}

/// The native payload slots the project at `root` holds.
fn slots(root: &Path) -> usize {
    fs::read_dir(
        root.join(".curios")
            .join("payloads")
            .join(Target::Native.spelled()),
    )
    .map_or(0, Iterator::count)
}

/// The point of the thing, end to end: what the program does must not depend on whether it was compiled just now.
#[test]
fn running_twice_reuses_and_behaves_identically() {
    let root = project("twice");

    let first = curios(&root, &["run"]);
    let second = curios(&root, &["run"]);

    assert!(
        !reused(&first),
        "nothing is stored for the first invocation"
    );
    assert!(
        reused(&second),
        "and the second says so where a reader sees it"
    );

    assert_eq!(first.stdout, b"reused", "the program ran");
    assert_eq!(second.stdout, first.stdout, "and printed the same thing");
    assert_eq!(
        second.status.code(),
        first.status.code(),
        "and exited the same way — a payload from the store is the payload a compile produced"
    );
    assert_eq!(
        first.status.code(),
        Some(7),
        "which is the program's own code"
    );
}

/// One slot serves both subcommands, which is what makes `compile` after `run` a file write.
#[test]
fn one_slot_serves_run_and_compile() {
    let root = project("both");

    assert!(!reused(&curios(&root, &["run"])));
    assert!(
        reused(&curios(&root, &["compile"])),
        "`compile` wraps what `run` filed rather than compiling it again"
    );
    assert!(
        reused(&curios(&root, &["run"])),
        "and `run` afterwards finds the same slot"
    );
    assert_eq!(slots(&root), 1, "one program, one slot, whoever asked");

    assert!(
        root.join(".curios/native/app/app").exists(),
        "and the bundle was written from it"
    );
}

/// A stage is a question, and a question files the units it compiled whole and no payload: after `wonder stage`, the plain invocation finds the library in the store and compiles the program over it.
#[test]
fn asking_for_a_stage_files_its_units_and_no_payload() {
    let root = project("asking");

    let shown = curios(&root, &["wonder", "stage", "wasm"]);
    assert!(
        shown.status.success(),
        "{}",
        String::from_utf8_lossy(&shown.stderr)
    );
    assert!(
        String::from_utf8_lossy(&shown.stdout).starts_with("(module"),
        "the answer is the rung, on stdout"
    );
    assert!(shown.stderr.is_empty(), "and nothing was narrated");
    assert_eq!(slots(&root), 0, "a question files no payload");

    let run = curios(&root, &["run"]);
    let narrated = String::from_utf8_lossy(&run.stderr);
    assert!(
        narrated.contains("/app; reused"),
        "the library is the unit the question filed: {narrated}"
    );
    assert_eq!(
        slots(&root),
        1,
        "and the program was compiled over it, and its payload filed"
    );
}

/// A bare file has no project, hence no store and no slot — the declared-versus-bare split.
#[test]
fn a_bare_file_files_nothing() {
    let root = temporary("bare");
    write(&root, "scratch.crs", "/std/print(\"standalone\")\n");

    let first = curios(&root, &["run", "scratch.crs"]);
    assert_eq!(first.stdout, b"standalone");
    assert!(!reused(&first));

    let second = curios(&root, &["run", "scratch.crs"]);
    assert!(
        !reused(&second),
        "a file argument brings no project, so there is nothing to reuse"
    );

    assert!(
        !root.join(".curios").exists(),
        "and nothing was written beside it"
    );
}

/// A program no package declares has no store to be filed under, so it builds only where `--output` says — and files nothing in a store either way.
#[test]
fn a_loose_file_compiles_only_where_output_says() {
    let root = temporary("loose-compile");
    write(&root, "scratch.crs", "/std/print(\"loose\")\n");

    let refused = curios(&root, &["compile", "scratch.crs"]);
    assert!(!refused.status.success());
    let stderr = String::from_utf8_lossy(&refused.stderr);
    assert!(
        stderr.contains("`curios compile scratch.crs --output scratch`"),
        "{stderr}"
    );

    let built = curios(&root, &["compile", "scratch.crs", "--output", "scratch"]);
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    assert!(root.join("scratch").is_file());
    assert!(
        !root.join(".curios").exists(),
        "a loose program has no store to file into"
    );
}

/// A build and a test run of a file no unit declares name what its form is declared by, as a question does: one written as a module is told its `mod` line, though a build goes on to refuse it for the term it lacks, and one written as a program its row.
#[test]
fn a_build_and_a_test_run_name_what_the_files_form_is_declared_by() {
    let root = project("unlinked-forms");
    write(&root, "items.crs", "pub let w : /std/Str = \"a\";\n");
    write(&root, "scratch.crs", "/std/print(\"on its own\")\n");
    let module = "declare it with `mod items;` in lib.crs, or with `mod items;` in app.crs";
    let program = "an `[[executables]]` row in its manifest declares it";
    let stderr =
        |arguments: &[&str]| String::from_utf8_lossy(&curios(&root, arguments).stderr).into_owned();

    let built = stderr(&["run", "items.crs"]);
    assert!(
        built.starts_with(&format!(
            "note: items.crs is in no unit of `/app`, so it is compiled on its own against `/std`: {module}\n"
        )),
        "{built}"
    );

    for (file, remedy) in [("items.crs", module), ("scratch.crs", program)] {
        let tested = stderr(&["test", file]);
        assert!(
            tested.starts_with(&format!(
                "note: {file} is in no unit of `/app`, so it is tested on its own against `/std`: {remedy}\n"
            )),
            "{tested}"
        );
    }
}

/// A file its package's directory holds and no unit declares is built against `/std` alone, and a build says so ahead of its header, naming the row that would declare it as a program — where a file no package holds is built with nothing said.
#[test]
fn a_file_no_unit_declares_says_so_when_built() {
    let root = project("unlinked-build");
    write(&root, "scratch.crs", "/std/print(\"on its own\")\n");

    let ran = curios(&root, &["run", "scratch.crs"]);
    assert_eq!(ran.stdout, b"on its own");
    let stderr = String::from_utf8_lossy(&ran.stderr);
    assert!(
        stderr.starts_with(
            "note: scratch.crs is in no unit of `/app`, so it is compiled on its own against `/std`: an `[[executables]]` row in its manifest declares it\n"
        ),
        "{stderr}"
    );

    let built = curios(&root, &["compile", "scratch.crs", "--output", "scratch"]);
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    assert!(
        String::from_utf8_lossy(&built.stderr)
            .starts_with("note: scratch.crs is in no unit of `/app`"),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );

    let bare = temporary("unlinked-bare");
    write(&bare, "scratch.crs", "/std/print(\"on its own\")\n");
    let ran = curios(&bare, &["run", "scratch.crs"]);
    assert_eq!(ran.stdout, b"on its own");
    assert!(
        !String::from_utf8_lossy(&ran.stderr).contains("note:"),
        "{}",
        String::from_utf8_lossy(&ran.stderr)
    );
}

/// The package `dep`'s library under `index`, the type its family `Code`'s one constructor fixes the index at.
fn code(index: &str) -> String {
    format!("pub induct Code: (Type) -> pub Type\n| big(): ({index})\nend\n")
}

/// `app`'s library: `f` matches on `dep`'s `Code(X)` and adds to `x`, which checks while `big` fixes the index at `/std/Nat` and is refused once it fixes it at `/std/Bool`, which adds to nothing. A test beside it, so a test run has one to run.
const ON_CODE: &str = "use /dep/{Code};

pub let f(X: Type, c: Code(X), x: X) -> X =
    match c
    | big() => x + 0
    end;

test f_is_declared = /std/Test/assert(true);
";

/// Every command that runs, builds, tests, documents, checks or asks about the package, and whether it exits 0 on a refusal, as a question answered with a list of refusals does.
const COMMANDS: &[(&[&str], bool)] = &[
    (&["run"], false),
    (&["compile"], false),
    (&["compile", "--target", "browser"], false),
    (&["test"], false),
    (&["lint"], false),
    (&["document"], false),
    (&["wonder", "diagnostics"], true),
    (&["wonder", "tests"], false),
    (&["wonder", "cost"], false),
    (&["wonder", "stage", "wasm"], false),
];

/// A store holds what every command filed of a package while its dependency let `f` check, and the dependency's edit leaves `f`'s text as it was and refuses it: every command is refused, none answered from what the store holds for the text it was filed for.
#[test]
fn a_dependency_edit_that_refuses_a_declaration_stops_every_command_over_a_warm_store() {
    let dep = temporary("refused-dep");
    write(
        &dep,
        "curios.toml",
        "compiler = \"0.0.0\"\nname = \"dep\"\n",
    );
    write(&dep, "lib.crs", &code("/std/Nat"));
    let root = temporary("refused");
    write(
        &root,
        "curios.toml",
        &format!(
            "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n\n[dependencies]\ndep = {{ source = \"path\", path = {:?} }}\n",
            &*dep
        ),
    );
    write(&root, "lib.crs", ON_CODE);
    write(&root, "app.crs", "/std/print(\"ran\")\n");

    for (command, _) in COMMANDS {
        let warmed = curios(&root, command);
        assert!(
            warmed.status.success(),
            "{command:?}: {}{}",
            String::from_utf8_lossy(&warmed.stdout),
            String::from_utf8_lossy(&warmed.stderr)
        );
    }
    assert!(
        reused(&curios(&root, &["run"])),
        "the run's payload is filed"
    );

    write(&dep, "lib.crs", &code("/std/Bool"));
    for (command, answers) in COMMANDS {
        let refused = curios(&root, command);
        let said = format!(
            "{}{}",
            String::from_utf8_lossy(&refused.stdout),
            String::from_utf8_lossy(&refused.stderr)
        );
        assert_eq!(refused.status.success(), *answers, "{command:?}: {said}");
        // `dep` is reused once the first command after the edit has filed it; `app`'s library and its payload never are.
        assert!(
            said.contains("no witness of /std/ops/Add(/std/Bool) found")
                && !said.contains("app; reused"),
            "{command:?}: {said}"
        );
    }
}

/// Packages `a`, and `b` standing on it, beside the root of a package `app` whose executable's entry is `entry`: `a` is in the fold of every manifest `app` writes that declares `b`, and spellable from the entry only where `app` declares it too.
fn layered(name: &str, entry: &str) -> (Temporary, Temporary, Temporary) {
    let a = temporary(&format!("{name}-a"));
    write(&a, "curios.toml", "compiler = \"0.0.0\"\nname = \"a\"\n");
    write(&a, "lib.crs", "pub let word : /std/Str = \"from a\";\n");

    let b = temporary(&format!("{name}-b"));
    write(&b, "curios.toml", &app_manifest("b", &[("a", &a)]));
    write(&b, "lib.crs", "pub let side : /std/Str = /a/word;\n");

    let root = temporary(name);
    write(&root, "app.crs", entry);

    (root, a, b)
}

/// A manifest for the package `name`, declaring the executable `app` where `name` is `app`, and a path dependency on each of `dependencies`.
fn app_manifest(name: &str, dependencies: &[(&str, &Path)]) -> String {
    let executables = match name {
        "app" => "\n[[executables]]\nname = \"app\"\n",
        _ => "",
    };
    let rows = dependencies
        .iter()
        .map(|(name, path)| format!("{name} = {{ source = \"path\", path = {path:?} }}\n"))
        .collect::<String>();

    format!("compiler = \"0.0.0\"\nname = \"{name}\"\n{executables}\n[dependencies]\n{rows}")
}

/// What an executable's entry may name is what its manifest declares, whichever command compiles it: `test` refuses a declaration naming a dependency that `run` refuses it for, though another dependency puts that one in the fold.
#[test]
fn a_test_run_names_only_what_the_executable_declares() {
    let (root, _a, b) = layered(
        "tested-undeclared",
        "let shown : /std/Str = /a/word;\n\ntest shown_is_named = /std/Test/assert(true);\n\n/std/print(shown)\n",
    );
    write(&root, "curios.toml", &app_manifest("app", &[("b", &b)]));

    let ran = curios(&root, &["run"]);
    assert!(
        !ran.status.success(),
        "a run refuses the name the manifest does not declare: {}",
        String::from_utf8_lossy(&ran.stderr)
    );

    let tested = curios(&root, &["test"]);
    assert!(
        !tested.status.success(),
        "and so does a test run: {}{}",
        String::from_utf8_lossy(&tested.stdout),
        String::from_utf8_lossy(&tested.stderr)
    );
}

/// An entry may name only what its manifest declares, so a manifest that stops declaring a dependency the entry names makes a compile of the same text a refusal, the dependency still in the fold beneath another: the payload filed before the edit answers for a program that no longer checks, and is not reused.
#[test]
fn a_payload_is_not_reused_once_its_entry_no_longer_declares_what_it_names() {
    let (root, a, b) = layered("undeclared", "/std/print(/a/word)\n");
    write(
        &root,
        "curios.toml",
        &app_manifest("app", &[("a", &a), ("b", &b)]),
    );

    let first = curios(&root, &["run"]);
    assert_eq!(
        first.stdout,
        b"from a",
        "{}",
        String::from_utf8_lossy(&first.stderr)
    );
    assert!(reused(&curios(&root, &["run"])), "filed by the first run");

    write(&root, "curios.toml", &app_manifest("app", &[("b", &b)]));
    let fresh = temporary("undeclared-fresh");
    write(&fresh, "curios.toml", &app_manifest("app", &[("b", &b)]));
    write(&fresh, "app.crs", "/std/print(/a/word)\n");
    let compiled = curios(&fresh, &["run"]);
    assert!(
        !compiled.status.success(),
        "a compile of the entry under the narrowed manifest is refused: {}",
        String::from_utf8_lossy(&compiled.stderr)
    );

    // The units beneath the entry are reused, as nothing they read moved; the payload is not.
    let asked = curios(&root, &["run"]);
    let said = String::from_utf8_lossy(&asked.stderr);
    assert!(
        !said.contains("app; reused") && !asked.status.success(),
        "and the payload filed under the wider one does not answer for it: {said}"
    );
}
