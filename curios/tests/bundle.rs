//! End-to-end test of the `compile` subcommand's bundler: compile a program to a native executable, run it, and check its output.
//!
//! Gated with `#[ignore]` because it execs a produced binary. The compiler embeds its launcher, so the produced executable is self-contained — but the compiler itself only builds once `make` has generated its target-scoped runtime launcher. Run it with:
//!
//! ```sh
//! make curios/runtime
//! cargo test -p curios --test bundle -- --ignored
//! ```

use std::{fs, process::Command};

#[test]
#[ignore = "execs a produced executable; build the compiler with `make` first"]
fn compile_produces_a_runnable_executable() {
    let compiler = env!("CARGO_BIN_EXE_curios");

    let dir = std::env::temp_dir();
    let source = dir.join("curios_bundle_e2e.crs");
    let output = dir.join("curios_bundle_e2e.out");
    fs::write(
        &source,
        r#"std/Handle/write(std/Handle/stdout, /std/Str/to_bytes("hello"))"#,
    )
    .expect("write the temp source");

    let compiled = Command::new(compiler)
        .arg("compile")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .status()
        .expect("run the compiler");
    assert!(compiled.success(), "compile failed");

    let run = Command::new(&output).output().expect("exec the bundle");
    assert!(
        run.status.success(),
        "bundle exited with failure: {:?}",
        run.status
    );
    assert_eq!(String::from_utf8_lossy(&run.stdout), "hello");
}
