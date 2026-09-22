//! End-to-end tests of the `compile` subcommand's bundler: compile a package's program to a native executable, run it, and check what it produced.
//!
//! Gated with `#[ignore]` because they exec a produced binary. The compiler embeds its launcher, so the produced executable is self-contained — but the compiler itself only builds once `cargo x runtime` has generated its target-scoped runtime launcher. Run them with:
//!
//! ```sh
//! cargo x runtime
//! cargo test -p curios --test bundle -- --ignored
//! ```

use {
    curios_utilities::{digest, test_support::Temporary},
    curios_wasm::{
        AddressType, CompType, Export, Expr, Func, FuncName, FuncType, Instr, Limits, LocalName,
        MemName, MemType, Module, NumType, ResultType, SubType, TypeName, ValType, to_bytes,
    },
    std::{fs, path::Path, process::Command},
};

/// `i32`, the carrier every raw-ABI scalar crosses as.
fn i32_type() -> ValType {
    ValType::Num(NumType::I32)
}

/// One export: its name, its signature, and its whole body.
fn export(module: &mut Module, name: &str, inputs: Vec<ValType>, body: Vec<Instr>) {
    let type_name = TypeName::from(name);
    let func_name = FuncName::from(name);

    module.add_type(
        type_name.clone(),
        SubType {
            is_final: true,
            super_types: vec![],
            comp_type: CompType::Func(FuncType {
                inputs: ResultType::from(inputs.clone()),
                outputs: ResultType::from(vec![i32_type()]),
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
        vec![Instr::I32Const { value: 0 }],
    );

    // Doubled, so the answer could not be the argument passed through.
    export(
        &mut module,
        "double",
        vec![i32_type()],
        vec![
            Instr::LocalGet {
                local_name: LocalName::from("a0"),
            },
            Instr::I32Const { value: 2 },
            Instr::I32Mul,
        ],
    );

    to_bytes(&module)
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
#[ignore = "execs a produced executable; build the compiler with `cargo x runtime` first"]
fn compile_produces_a_runnable_executable() {
    // `compile` builds a declared executable, so the program is a package: a one-line manifest beside its `exe.crs`, and the executable beside the package — both in a directory of their own that goes away with the test.
    let root = Temporary::new("cli-bundle", "e2e");
    let package = root.join("bundle");
    let output = root.join("bundle.out");
    fs::create_dir_all(&package).expect("create the temp package");
    fs::write(package.join("curios.toml"), "name = \"bundle\"\n").expect("write the manifest");
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

/// A program keeps the modules answering its `foreign` declarations when it leaves the machine that built it.
///
/// **This is what the bundle format is for.** `curios run` links a plugin by reading the manifest, hashing the `.wasm` beside it and compiling it; none of those three things exist where a compiled executable runs. So the module is precompiled into the tail and the launcher deserializes it — and the executable is run from a directory holding nothing else, which is the only way to show that no part of the package was still being reached.
#[test]
#[ignore = "execs a produced executable; build the compiler with `cargo x runtime` first"]
fn a_compiled_executable_carries_the_modules_answering_its_foreign_declarations() {
    let root = Temporary::new("cli-bundle", "ffi");
    let package = root.join("ffi");
    fs::create_dir_all(&package).expect("create the temp package");

    let wasm = plugin();
    fs::write(package.join("plugin.wasm"), &wasm).expect("write the plugin");

    // The row pins the module by the digest of the bytes just written, which is what a `path` delivery is accepted against.
    fs::write(
        package.join("curios.toml"),
        format!(
            "name = \"ffi\"\n\n[[foreign]]\nname = \"probe\"\npath = \"plugin.wasm\"\nhash = \"f1:{}\"\n\n[foreign.exports]\ndouble = \"/ffi/double\"\n",
            digest(&wasm)
        ),
    )
    .expect("write the manifest");

    fs::write(
        package.join("lib.crs"),
        "pub foreign double: (Nat) -> Nat;\n",
    )
    .expect("write the library");
    fs::write(
        package.join("exe.crs"),
        "let doubled = /ffi/double(3)!;\nlet _ = /std/proc/exit(@{}, /std/Nat/to_byte(doubled % 256))!;\n/std/Io/pure(())\n",
    )
    .expect("write the temp source");

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
