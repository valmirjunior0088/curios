//! End-to-end tests of the `compile` subcommand's bundler: compile a package's program to a native executable, run it, and check what it produced.
//!
//! Those that exec what they produced run with the ordinary suite all the same: the compiler embeds its launcher, so the produced executable is self-contained, and a build of this crate already has the launcher it embeds.

use {
    curios_utilities::{digest, test_support::Temporary},
    curios_wasm::{
        AddressType, CompType, Export, Expr, Func, FuncName, FuncType, Instr, Limits, LocalName,
        MemName, MemType, Module, NumType, ResultType, SubType, TypeName, ValType, to_bytes,
    },
    std::{
        fs,
        io::Write,
        path::Path,
        process::{Command, Stdio},
    },
};

/// `i32`, the carrier the allocator's size and offset cross as.
fn i32_type() -> ValType {
    ValType::Num(NumType::I32)
}

/// `i64`, the carrier a `Nat` crosses to a plugin as.
fn i64_type() -> ValType {
    ValType::Num(NumType::I64)
}

/// One export: its name, its signature, and its whole body.
fn export(
    module: &mut Module,
    name: &str,
    inputs: Vec<ValType>,
    outputs: Vec<ValType>,
    body: Vec<Instr>,
) {
    let type_name = TypeName::from(name);
    let func_name = FuncName::from(name);

    module.add_type(
        type_name.clone(),
        SubType {
            is_final: true,
            super_types: vec![],
            comp_type: CompType::Func(FuncType {
                inputs: ResultType::from(inputs.clone()),
                outputs: ResultType::from(outputs),
            }),
        },
    );
    module.add_func(
        func_name.clone(),
        Func {
            type_name,
            params: (0..inputs.len())
                .map(|index| LocalName::from(format!("a{index}")))
                .collect(),
            locals: vec![],
            expr: Expr::from(body),
        },
    );
    module.add_export(name, Export::Func(func_name));
}

/// The fixture plugin: the memory and allocator conformance demands, and one function to answer a declaration with.
///
/// `alloc` answers a constant offset. Nothing here crosses a byte string, so there is nothing to allocate for — the export exists because a plugin is refused without one, and a bump allocator would only be a fixture that could run out.
fn plugin() -> Vec<u8> {
    let mut module = Module::new("probe");

    let memory = MemName::from("memory");
    module.add_memory(
        memory.clone(),
        MemType {
            address_type: AddressType::I32,
            limits: Limits { min: 1, max: None },
        },
    );
    module.add_export("memory", Export::Memory(memory));

    export(
        &mut module,
        "alloc",
        vec![i32_type()],
        vec![i32_type()],
        vec![Instr::I32Const { value: 0 }],
    );

    // Doubled, so the answer could not be the argument passed through.
    export(
        &mut module,
        "double",
        vec![i64_type()],
        vec![i64_type()],
        vec![
            Instr::LocalGet {
                local_name: LocalName::from("a0"),
            },
            Instr::I64Const { value: 2 },
            Instr::I64Mul,
        ],
    );

    // A word where a `Nat` is declared: an `i32`, which no `Nat` crosses to a module as.
    export(
        &mut module,
        "truthy",
        vec![],
        vec![i32_type()],
        vec![Instr::I32Const { value: 1 }],
    );

    to_bytes(&module)
}

/// A package `name` under `root` whose library declares `pub foreign double(Nat) -> Nat;`, whose program calls it, and whose manifest names the fixture plugin in each of `rows`, a row's name beside the export it says answers `double`.
fn calling_double(root: &Path, name: &str, rows: &[(&str, &str)]) -> std::path::PathBuf {
    let package = root.join(name);
    fs::create_dir_all(&package).expect("create the temp package");

    let wasm = plugin();
    fs::write(package.join("plugin.wasm"), &wasm).expect("write the plugin");

    // Each row pins the module by the digest of the bytes just written, which is what a `path` delivery is accepted against.
    let rows = rows
        .iter()
        .map(|(row, export)| {
            format!(
                "\n[[foreign]]\nname = \"{row}\"\npath = \"plugin.wasm\"\nhash = \"f1:{}\"\n\n[foreign.answers]\ndouble = \"{export}\"\n",
                digest(&wasm)
            )
        })
        .collect::<String>();
    fs::write(
        package.join("curios.toml"),
        format!("compiler = \"0.0.0\"\nname = \"{name}\"\n{rows}"),
    )
    .expect("write the manifest");

    fs::write(package.join("lib.crs"), "pub foreign double(Nat) -> Nat;\n")
        .expect("write the library");
    fs::write(
        package.join("exe.crs"),
        format!("let main() -> /std/Io({{}}) = let doubled = /{name}/double(3)!;\nlet _ = /std/Process/exit(@{{}}, /std/Nat/to_byte(doubled % 256))!;\n/std/Io/pure(());\nmain()\n"),
    )
    .expect("write the temp source");

    package
}

/// What `compile` says where it refuses to write `output`, run in `directory` with `arguments` after the destination; nothing is written.
fn refused(directory: &Path, output: &Path, arguments: &[&str]) -> String {
    let compiled = Command::new(env!("CARGO_BIN_EXE_curios"))
        .arg("compile")
        .arg("-o")
        .arg(output)
        .args(arguments)
        .current_dir(directory)
        .output()
        .expect("run the compiler");

    assert!(
        !compiled.status.success(),
        "an executable its launcher would refuse was written"
    );
    assert!(
        !output.exists(),
        "a refused compile left an executable behind"
    );

    String::from_utf8_lossy(&compiled.stderr).into_owned()
}

/// A program that calls a `foreign` of its own, which no `[[foreign]]` row answers.
const UNANSWERED: &str = "foreign frobnicate(Nat) -> Nat;\nlet main() -> /std/Io({}) = let n = frobnicate(3)!;\n/std/print(/std/Nat/to_str(n));\nmain()\n";

/// `compile` links what it bundles before it writes, as `run` links before it runs: a reached row nothing answers is refused by its name and by what would answer it, a `[[foreign]]` row, and no executable is written that its launcher would refuse at every start.
#[test]
fn compile_refuses_a_row_nothing_answers_and_says_what_would() {
    let root = Temporary::new("cli-bundle", "unanswered");
    let package = root.join("unanswered");
    fs::create_dir_all(&package).expect("create the temp package");
    fs::write(
        package.join("curios.toml"),
        "compiler = \"0.0.0\"\nname = \"unanswered\"\n",
    )
    .expect("write the manifest");
    fs::write(package.join("exe.crs"), UNANSWERED).expect("write the temp source");

    let refusal = refused(&package, &root.join("unanswered.out"), &[]);

    assert!(
        refusal.contains(
            "`/frobnicate` has no implementation: no `[[foreign]]` row of the package's manifests names an export that answers it"
        ),
        "{refusal}"
    );
}

/// A loose program has no manifest to write a `[[foreign]]` row in, and `compile` and `run` both say so of a row of its own, where the link would say only that nothing answered.
#[test]
fn a_loose_program_is_told_it_has_no_manifest_to_answer_its_foreign_in() {
    let root = Temporary::new("cli-bundle", "loose");
    fs::create_dir_all(&*root).expect("create the temp directory");
    fs::write(root.join("loose.crs"), UNANSWERED).expect("write the temp source");

    let expected = "`/frobnicate` has no implementation: a `foreign` is answered by a module that a `[[foreign]]` row of a manifest names, and a loose program has no manifest to write one in";

    let refusal = refused(&root, &root.join("loose.out"), &["loose.crs"]);
    assert!(refusal.contains(expected), "{refusal}");

    let mut run = Command::new(env!("CARGO_BIN_EXE_curios"))
        .args(["run", "-"])
        .current_dir(&*root)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run the compiler");
    run.stdin
        .take()
        .expect("standard input is piped")
        .write_all(UNANSWERED.as_bytes())
        .expect("the program is written");
    let run = run.wait_with_output().expect("the compiler exits");

    assert_eq!(run.status.code(), Some(1));
    assert!(
        String::from_utf8_lossy(&run.stderr).contains(expected),
        "{}",
        String::from_utf8_lossy(&run.stderr)
    );
}

/// What a run refuses of a package's modules is refused by `compile` too: two rows that claim one declaration, where the manifest is read, and an export that answers another type than its declaration crosses as, where the two are linked.
#[test]
fn compile_refuses_the_modules_a_run_would() {
    let root = Temporary::new("cli-bundle", "refused");

    let twice = calling_double(&root, "twice", &[("one", "double"), ("two", "double")]);
    let refusal = refused(&twice, &root.join("twice.out"), &[]);
    assert!(
        refusal.contains(
            "the foreign rows \"one\" and \"two\" both name an export that answers `/twice/double`"
        ),
        "{refusal}"
    );

    let mistyped = calling_double(&root, "mistyped", &[("probe", "truthy")]);
    let refusal = refused(&mistyped, &root.join("mistyped.out"), &[]);
    assert!(
        refusal.contains("the module \"probe\" of mistyped exports `truthy` as () -> (i32), and `/mistyped/double` crosses to a module as (i64) -> (i64)"),
        "{refusal}"
    );
}

/// A claim is held to a declaration however the program is reached: a run that reuses the payload an earlier run filed compiles no unit, so the claims a manifest has gained since are held by what the stored units record. Without that, a claim on no declaration is admitted for as long as the store answers and refused the moment it is emptied.
#[test]
fn a_claim_written_after_a_run_is_refused_by_the_next() {
    let root = Temporary::new("cli-bundle", "claimed");
    let package = calling_double(&root, "claimed", &[("probe", "double")]);
    let run = || {
        Command::new(env!("CARGO_BIN_EXE_curios"))
            .arg("run")
            .current_dir(&package)
            .output()
            .expect("run the compiler")
    };

    assert_eq!(run().status.code(), Some(6), "the module answers");
    assert_eq!(
        run().status.code(),
        Some(6),
        "and answers again from the store"
    );

    let manifest = package.join("curios.toml");
    let claiming = format!(
        "{}nowhere = \"double\"\n",
        fs::read_to_string(&manifest).expect("read the manifest")
    );
    fs::write(&manifest, claiming).expect("write the manifest");

    let refused = run();
    assert_eq!(refused.status.code(), Some(1));
    assert!(
        String::from_utf8_lossy(&refused.stderr).contains(
            "the foreign row \"probe\" says an export answers `/claimed/nowhere`, and /claimed declares no such `foreign`"
        ),
        "{}",
        String::from_utf8_lossy(&refused.stderr)
    );
}

/// Compile the package at `package` to `output`, failing the test if the compiler refuses.
fn compile(package: &Path, output: &Path) {
    let compiled = Command::new(env!("CARGO_BIN_EXE_curios"))
        .arg("compile")
        .arg("-o")
        .arg(output)
        .current_dir(package)
        .output()
        .expect("run the compiler");

    assert!(
        compiled.status.success(),
        "compile failed: {}",
        String::from_utf8_lossy(&compiled.stderr)
    );
}

#[test]
fn compile_produces_a_runnable_executable() {
    // `compile` builds a declared executable, so the program is a package: a one-line manifest beside its `exe.crs`, and the executable beside the package — both in a directory of their own that goes away with the test.
    let root = Temporary::new("cli-bundle", "e2e");
    let package = root.join("bundle");
    let output = root.join("bundle.out");
    fs::create_dir_all(&package).expect("create the temp package");
    fs::write(
        package.join("curios.toml"),
        "compiler = \"0.0.0\"\nname = \"bundle\"\n",
    )
    .expect("write the manifest");
    fs::write(package.join("exe.crs"), r#"/std/print("hello")"#).expect("write the temp source");

    compile(&package, &output);

    let run = Command::new(&output).output().expect("exec the bundle");
    assert!(
        run.status.success(),
        "bundle exited with failure: {:?}",
        run.status
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "hello");
}

/// A program that writes a line and then a second on the other stream: to standard output first with no argument, and to standard error first with one.
fn printer(root: &Path) -> std::path::PathBuf {
    let package = root.join("printer");
    let output = root.join("printer.out");
    fs::create_dir_all(&package).expect("create the temp package");
    fs::write(
        package.join("curios.toml"),
        "compiler = \"0.0.0\"\nname = \"printer\"\n",
    )
    .expect("write the manifest");
    fs::write(
        package.join("exe.crs"),
        r#"use /std/{List, print, print_err};
let entry() -> /std/Io({}) =
    let arguments = /std/Process/args()!;
    match List/try_get(arguments, 1)
    | some(_) =>
        let _ = print_err("written\n")!;
        print("after\n")
    | none() =>
        let _ = print("written\n")!;
        print_err("after\n")
    end;
entry()
"#,
    )
    .expect("write the temp source");

    compile(&package, &output);

    output
}

/// The writing end of a pipe whose reading end is already closed.
fn deaf_pipe() -> std::io::PipeWriter {
    let (reader, writer) = std::io::pipe().expect("open a pipe");
    drop(reader);
    writer
}

/// A `print` whose reader has gone ends the program with 141, the status a shell reports for a process `SIGPIPE` ended, and says nothing.
#[test]
fn a_print_whose_reader_has_gone_ends_the_program_with_141_and_says_nothing() {
    let root = Temporary::new("cli-bundle", "print-reader-gone");
    let printer = printer(&root);

    let run = Command::new(&printer)
        .stdout(deaf_pipe())
        .output()
        .expect("exec the bundle");

    assert_eq!(run.status.code(), Some(141), "{run:?}");
    assert_eq!(String::from_utf8_lossy(&run.stderr), "");
}

/// A `print` that cannot write for any other reason ends the program with 1 once standard error says which stream failed and why: here a standard output open only for reading, which both release targets refuse with `EBADF`.
#[test]
fn a_print_that_cannot_write_ends_the_program_with_1_and_says_why() {
    let root = Temporary::new("cli-bundle", "print-refused");
    let printer = printer(&root);
    let file = root.join("read-only");
    fs::write(&file, "").expect("write the file standard output reads");

    let run = Command::new(&printer)
        .stdout(fs::File::open(&file).expect("open the file for reading"))
        .output()
        .expect("exec the bundle");

    assert_eq!(run.status.code(), Some(1), "{run:?}");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "standard output could not be written: other(9)\n"
    );
}

/// A `print_err` whose reader has gone ends the program as `print`'s does, before the line it would have printed next.
#[test]
fn a_print_err_whose_reader_has_gone_ends_the_program_with_141() {
    let root = Temporary::new("cli-bundle", "print-err-reader-gone");
    let printer = printer(&root);

    let run = Command::new(&printer)
        .arg("stderr first")
        .stderr(deaf_pipe())
        .output()
        .expect("exec the bundle");

    assert_eq!(run.status.code(), Some(141), "{run:?}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "");
}

/// A branch reading standard input waits alone: the other branch ticks meanwhile, and the input is written only once a tick has been read off the output, so the read can end only where the ticker ran while it waited. Input written anyway after a bound ends a run whose read held the whole program, which then answers with no tick before it.
#[test]
fn a_branch_ticks_while_another_reads_standard_input() {
    let root = Temporary::new("cli-bundle", "stdin-ticker");
    let package = root.join("ticker");
    let output = root.join("ticker.out");
    fs::create_dir_all(&package).expect("create the temp package");
    fs::write(
        package.join("curios.toml"),
        "compiler = \"0.0.0\"\nname = \"ticker\"\n",
    )
    .expect("write the manifest");
    fs::write(
        package.join("exe.crs"),
        r#"use /std/{Io, Nat, Str, Bytes, Try, Process};
use /std/time/{Duration};
let ticks(n: Nat) -> Io({}) =
    match n
    | 0 => Io/pure(())
    | m + 1 =>
        let _ = Io/sleep(Duration/of_millis(10))!;
        let _ = /std/print("tick\n")!;
        ticks(m)
    end;
let reading() -> Io({}) =
    let read = Try/run(Io/read_all(Process/stdin))!;
    match read
    | success(bytes) => /std/print(Str/concat("read ", Nat/to_str(Bytes/len(bytes))))
    | failure(_) => /std/print("failed")
    end;
Io/race([reading(), ticks(3000)])
"#,
    )
    .expect("write the temp source");

    compile(&package, &output);

    let mut run = Command::new(&output)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("exec the bundle");
    let mut input = run.stdin.take().expect("the input was piped");
    let mut ticked = run.stdout.take().expect("the output was piped");

    let (tick, ticking) = std::sync::mpsc::channel::<()>();
    let writer = std::thread::spawn(move || {
        let _ = ticking.recv_timeout(std::time::Duration::from_secs(20));
        input
            .write_all(b"line\n")
            .expect("the bundle takes its input");
    });

    let mut seen = Vec::new();
    let mut byte = [0];
    while std::io::Read::read(&mut ticked, &mut byte).expect("the output reads") == 1 {
        seen.push(byte[0]);
        if seen.ends_with(b"tick\n") {
            let _ = tick.send(());
        }
    }
    writer.join().expect("the writer ends");

    assert!(run.wait().expect("the bundle ends").success());
    let seen = String::from_utf8_lossy(&seen);
    assert!(
        seen.starts_with("tick\n") && seen.ends_with("read 5"),
        "a tick before the read's answer: {seen:?}"
    );
}

/// A program keeps the modules answering its `foreign` declarations when it leaves the machine that built it.
///
/// **This is what the bundle format is for.** `curios run` links a plugin by reading the manifest, hashing the `.wasm` beside it and compiling it; none of those three things exist where a compiled executable runs. So the module is precompiled into the tail and the launcher deserializes it — and the executable is run from a directory holding nothing else, which is the only way to show that no part of the package was still being reached.
#[test]
fn a_compiled_executable_carries_the_modules_answering_its_foreign_declarations() {
    let root = Temporary::new("cli-bundle", "ffi");
    let package = calling_double(&root, "ffi", &[("probe", "double")]);

    // Somewhere else entirely, holding nothing but the executable: no manifest to read a row from, no `.wasm` to hash, no store to look in.
    let elsewhere = root.join("elsewhere");
    fs::create_dir_all(&elsewhere).expect("create the run directory");
    let output = elsewhere.join("ffi.out");

    compile(&package, &output);

    let run = Command::new(&output)
        .current_dir(&elsewhere)
        .output()
        .expect("exec the bundle");

    assert_eq!(
        run.status.code(),
        Some(6),
        "the bundled plugin did not answer: {}",
        String::from_utf8_lossy(&run.stderr)
    );
}
