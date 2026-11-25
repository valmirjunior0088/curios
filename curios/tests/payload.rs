//! What the CLI does across two invocations of one target — the half of payload reuse that is a command-line decision rather than a cabinet mechanism.
//!
//! The cabinet's own behaviour is covered where it lives, in `curios-verdict`'s payload tests: those decide when a payload may be believed. These decide what the *subcommands* do with the answer — that one object serves `run` and `compile` alike, that a hit reports itself, that a program's output and exit code do not depend on whether it was compiled just now, that a stage query files nothing, that a bare file has nothing to do with any of it, and that a filing command keeps of its project's own cabinet what the project reaches and nothing more.
//!
//! Not `#[ignore]`d, unlike `bundle`: nothing here execs a produced executable. `curios run` runs its program in-process.

use {
    curios::CABINET,
    curios_common::{fill, test_support::*},
    curios_package::{Cabinet, Target},
    std::{
        collections::BTreeSet,
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
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"app\"\n\n[executables]\napp = {}\n"
        ),
    );
    write(&root, "lib.crs", "pub let word : /std/Str = \"reused\";\n");
    write(
        &root,
        "app.crs",
        "let main() -> /std/Io({}) = let _ = /std/print(/app/word)!;\n/std/Proc/exit(7);\nmain()\n",
    );

    root
}

/// Run the compiler in `root`, with the arguments given.
fn curios(root: &Path, arguments: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_curios"))
        .env_remove(CABINET)
        .current_dir(root)
        .args(arguments)
        .output()
        .expect("run the compiler")
}

/// Whether an invocation was served from the cabinet, read off the report it wrote — which is the same question, asked the way a person at a terminal asks it: a program the cabinet held narrates no build, so nothing is emitted.
fn reused(output: &Output) -> bool {
    !String::from_utf8_lossy(&output.stderr).contains("Emitting ")
}

/// The native payloads the project at `root` holds.
fn payloads(root: &Path) -> usize {
    fs::read_dir(Cabinet::of(root, None).payloads(Target::Native)).map_or(0, Iterator::count)
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
        "and exited the same way — a payload from the cabinet is the payload a compile produced"
    );
    assert_eq!(
        first.status.code(),
        Some(7),
        "which is the program's own code"
    );
}

/// One payload serves both subcommands, which is what makes `compile` after `run` a file write.
#[test]
fn one_payload_serves_run_and_compile() {
    let root = project("both");

    assert!(!reused(&curios(&root, &["run"])));
    assert!(
        reused(&curios(&root, &["compile"])),
        "`compile` wraps what `run` filed rather than compiling it again"
    );
    assert!(
        reused(&curios(&root, &["run"])),
        "and `run` afterwards finds the same payload"
    );
    assert_eq!(
        payloads(&root),
        1,
        "one program, one payload, whoever asked"
    );

    assert!(
        root.join(".curios/native/app/app").exists(),
        "and the bundle was written from it"
    );
}

/// A stage is a question, and a question files the units it compiled whole and no payload: after `wonder --stage`, the plain invocation finds the library in the cabinet and compiles the program over it.
#[test]
fn asking_for_a_stage_files_its_units_and_no_payload() {
    let root = project("asking");

    let shown = curios(&root, &["wonder", "--stage", "wasm"]);
    assert!(
        shown.status.success(),
        "{}",
        String::from_utf8_lossy(&shown.stderr)
    );
    assert!(
        String::from_utf8_lossy(&shown.stdout).starts_with("(module"),
        "the answer is the rung, on stdout"
    );
    // A rung reprints the program, so what is narrated is the unit it stands on and never the program.
    let narrated = String::from_utf8_lossy(&shown.stderr);
    assert!(
        narrated.contains("Checking `/app`\n") && narrated.contains("Stored `/app` in "),
        "{narrated}"
    );
    assert!(
        !narrated.contains("Building ") && !narrated.contains("Emitting "),
        "{narrated}"
    );
    assert_eq!(payloads(&root), 0, "a question files no payload");

    let run = curios(&root, &["run"]);
    let narrated = String::from_utf8_lossy(&run.stderr);
    assert!(
        !narrated.contains("Storing ") && narrated.contains("Emitting "),
        "the library is the unit the question filed, so no unit is checked and filed again: {narrated}"
    );
    assert_eq!(
        payloads(&root),
        1,
        "and the program was compiled over it, and its payload filed"
    );
}

/// A bare file has no project, hence no cabinet and no object — the declared-versus-bare split.
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

/// A program no package declares has no `.curios/` to be built under, so it builds only where `--output` says — and files nothing in a cabinet either way.
#[test]
fn a_loose_file_compiles_only_where_output_says() {
    let root = temporary("loose-compile");
    write(&root, "scratch.crs", "/std/print(\"loose\")\n");

    let refused = curios(&root, &["compile", "scratch.crs"]);
    assert!(!refused.status.success());
    let stderr = String::from_utf8_lossy(&refused.stderr);
    assert!(
        stderr.contains(
            "├─ try; curios compile scratch.crs --output scratch\n└─ [error] scratch.crs"
        ),
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
        "a loose program has no cabinet to file into"
    );
}

/// A file its package's directory holds and no unit declares is built against `/std` alone, as a file no package holds is, with nothing said of it.
#[test]
fn a_file_no_unit_declares_is_built_on_its_own() {
    let root = project("unreached-build");
    write(&root, "scratch.crs", "/std/print(\"on its own\")\n");

    let ran = curios(&root, &["run", "scratch.crs"]);
    assert_eq!(ran.stdout, b"on its own");
    let stderr = String::from_utf8_lossy(&ran.stderr);
    assert!(
        stderr.starts_with("Checking `scratch.crs`\nChecked `scratch.crs` in "),
        "{stderr}"
    );

    let built = curios(&root, &["compile", "scratch.crs", "--output", "scratch"]);
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    assert!(root.join("scratch").is_file());
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

test f_is_declared = /std/Test/assert(\"it holds\", true);
";

/// Every command that runs, builds, tests, documents, checks or asks about the package, and whether it exits 0 on a refusal, as a question answered with a list of refusals does.
const COMMANDS: &[(&[&str], bool)] = &[
    (&["run"], false),
    (&["compile"], false),
    (&["compile", "--target", "browser"], false),
    (&["test"], false),
    (&["lint"], false),
    (&["document"], false),
    (&["wonder"], true),
    (&["wonder", "--tests"], false),
    (&["wonder", "--name", "/app/f", "--footprint"], false),
    (&["wonder", "--stage", "wasm"], false),
];

/// A cabinet holds what every command filed of a package while its dependency let `f` check, and the dependency's edit leaves `f`'s text as it was and refuses it: every command is refused, none answered from what the cabinet holds for the text it was filed for.
#[test]
fn a_dependency_edit_that_refuses_a_declaration_stops_every_command_over_a_warm_cabinet() {
    let dep = temporary("refused-dep");
    write(
        &dep,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"dep\"\n"
        ),
    );
    write(&dep, "lib.crs", &code("/std/Nat"));
    let root = temporary("refused");
    write(
        &root,
        "curios.toml",
        &format!(
            "compiler = \"{compiler}\"\nname = \"app\"\n\n[executables]\napp = {{}}\n\n[dependencies]\ndep = {{ path = {:?} }}\n",
            &*dep,
            compiler = env!("CARGO_PKG_VERSION")
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
        // `dep` is reused once the first command after the edit has filed it; `app`'s library and its payload never are, which is what refuses each command again.
        assert!(
            said.contains("wanted: `/std/ops/Add(/std/Bool)`"),
            "{command:?}: {said}"
        );
    }
}

/// Packages `a`, and `b` standing on it, beside the root of a package `app` whose executable's entry is `entry`: `a` is in the fold of every manifest `app` writes that declares `b`, and spellable from the entry only where `app` declares it too.
fn layered(name: &str, entry: &str) -> (Temporary, Temporary, Temporary) {
    let a = temporary(&format!("{name}-a"));
    write(
        &a,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"a\"\n"
        ),
    );
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
        "app" => "\n[executables]\napp = {}\n",
        _ => "",
    };
    let rows = dependencies
        .iter()
        .map(|(name, path)| format!("{name} = {{ path = {path:?} }}\n"))
        .collect::<String>();

    format!(
        "compiler = \"{compiler}\"\nname = \"{name}\"\n{executables}\n[dependencies]\n{rows}",
        compiler = env!("CARGO_PKG_VERSION")
    )
}

/// What an executable's entry may name is what its manifest declares, whichever command compiles it: `test` refuses a declaration naming a dependency that `run` refuses it for, though another dependency puts that one in the fold.
#[test]
fn a_test_run_names_only_what_the_executable_declares() {
    let (root, _a, b) = layered(
        "tested-undeclared",
        "let shown : /std/Str = /a/word;\n\ntest shown_is_named = /std/Test/assert(\"it holds\", true);\n\n/std/print(shown)\n",
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

    // The units beneath the entry are reused, as nothing they read moved; the payload is not, or the run would have succeeded.
    let asked = curios(&root, &["run"]);
    let said = String::from_utf8_lossy(&asked.stderr);
    assert!(
        !asked.status.success(),
        "and the payload filed under the wider one does not answer for it: {said}"
    );
}

/// A cabinet named for the run takes whole what a command keeps for reuse and leaves the project's `.curios/` with what the command emits: `compile` under `CURIOS_CABINET` writes its executable beside the project, and files its units, its payload and the compiler's memo in the directory named.
#[test]
fn a_named_cabinet_takes_what_is_kept_and_leaves_what_is_emitted() {
    let root = project("named");
    let named = temporary("named-cabinet");

    let compiled = Command::new(env!("CARGO_BIN_EXE_curios"))
        .env(CABINET, named.as_os_str())
        .current_dir(&root)
        .arg("compile")
        .output()
        .expect("run the compiler");
    assert!(
        compiled.status.success(),
        "{}",
        String::from_utf8_lossy(&compiled.stderr)
    );

    let generated = fs::read_dir(root.join(".curios"))
        .expect("what `compile` emits")
        .map(|entry| entry.unwrap().file_name().into_string().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(
        generated,
        ["native"],
        "`.curios/` holds what was emitted alone"
    );

    let cabinet = Cabinet::of(&root, Some(&named));
    assert!(
        cabinet.objects().is_dir(),
        "the units are filed in the cabinet named"
    );
    assert!(cabinet.payloads(Target::Native).is_dir(), "and the payload");
    assert!(cabinet.compiler().is_file(), "and the compiler's memo");
}

/// The manifest of a package `app` declaring each of `executables`.
fn declaring(executables: &[&str]) -> String {
    let compiler = env!("CARGO_PKG_VERSION");
    let rows = executables
        .iter()
        .map(|executable| fill!("${executable} = {}\n", executable))
        .collect::<String>();

    fill!(
        "compiler = \"${compiler}\"\nname = \"app\"\n\n[executables]\n${rows}",
        compiler,
        rows
    )
}

/// A package `app` whose library and each of whose `executables` declares a test.
fn tested(name: &str, executables: &[&str]) -> Temporary {
    let root = temporary(name);
    write(&root, "curios.toml", &declaring(executables));
    write(
        &root,
        "lib.crs",
        "pub let word : /std/Str = \"kept\";\n\ntest holds = /std/Test/assert(\"it holds\", true);\n",
    );
    for executable in executables {
        write(
            &root,
            &format!("{executable}.crs"),
            "test holds = /std/Test/assert(\"it holds\", true);\n\n/std/print(/app/word)\n",
        );
    }

    root
}

/// The entries of `directory`, by name: none where there is no such directory.
fn listed(directory: &Path) -> BTreeSet<String> {
    fs::read_dir(directory).map_or_else(
        |_| BTreeSet::new(),
        |entries| {
            entries
                .map(|entry| entry.unwrap().file_name().into_string().unwrap())
                .collect()
        },
    )
}

/// A filing command keeps what its project reaches and takes the rest: after a test run and a run of each executable, a run takes nothing they filed but what no family of the cabinet holds, and once the manifest drops an executable the next run takes its entry and its payloads.
#[test]
fn a_filing_command_keeps_what_its_project_reaches_and_takes_the_rest() {
    let root = tested("collected", &["app", "other"]);
    for arguments in [&["test"][..], &["run", "app"], &["run", "other"]] {
        let ran = curios(&root, arguments);
        assert!(
            ran.status.success(),
            "{arguments:?}: {}",
            String::from_utf8_lossy(&ran.stderr)
        );
    }
    let cabinet = Cabinet::of(&root, None);
    let objects = cabinet.addresses();
    let payloads = listed(&cabinet.payloads(Target::Native));
    assert_eq!(
        objects.len(),
        6,
        "the library and each entry, built and tested"
    );
    assert_eq!(
        payloads.len(),
        5,
        "each executable's, and the tests' of each and of the library"
    );

    let left = cabinet.directory().join("verdicts/left");
    fs::create_dir_all(left.parent().unwrap()).unwrap();
    fs::write(&left, "left behind").unwrap();
    assert!(curios(&root, &["run", "app"]).status.success());
    assert_eq!(cabinet.addresses(), objects, "nothing it reaches");
    assert_eq!(listed(&cabinet.payloads(Target::Native)), payloads);
    assert!(
        !cabinet.directory().join("verdicts").exists(),
        "but what no family holds"
    );

    write(&root, "curios.toml", &declaring(&["app"]));
    assert!(curios(&root, &["run", "app"]).status.success());
    let kept = cabinet.addresses();
    assert!(kept.is_subset(&objects) && kept.len() == 4, "{kept:?}");
    let kept = listed(&cabinet.payloads(Target::Native));
    assert!(kept.is_subset(&payloads) && kept.len() == 3, "{kept:?}");
}

/// A run filing into a cabinet named for every project collects nothing: the cabinet named keeps what no project reaches, since a run knows its own project alone, and an executable the manifest drops leaves its entry in it; and the project's own cabinet, which the run filed nothing into, is left as it stands.
#[test]
fn a_run_filing_into_a_named_cabinet_collects_nothing() {
    let root = tested("collected-named", &["app", "other"]);
    let named = temporary("collected-named-cabinet");
    let left = Cabinet::of(&root, None).directory().join("verdicts/left");
    fs::create_dir_all(left.parent().unwrap()).unwrap();
    fs::write(&left, "left behind").unwrap();
    let run = |executable: &str| {
        let ran = Command::new(env!("CARGO_BIN_EXE_curios"))
            .env(CABINET, named.as_os_str())
            .current_dir(&root)
            .args(["run", executable])
            .output()
            .expect("run the compiler");
        assert!(
            ran.status.success(),
            "{}",
            String::from_utf8_lossy(&ran.stderr)
        );
    };
    run("app");
    run("other");
    let cabinet = Cabinet::of(&root, Some(&named));
    let objects = cabinet.addresses();
    write(&root, "curios.toml", &declaring(&["app"]));
    run("app");

    assert_eq!(objects.len(), 3, "the library, and each entry");
    assert_eq!(cabinet.addresses(), objects);
    assert!(left.exists(), "and the project's own cabinet stands");
}
