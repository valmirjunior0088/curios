//! What the one authored part of the `/sys` roster must be true of, beyond parsing.

use {crate::Formatted, std::path::PathBuf};

/// **`/sys`'s authored source is written in the canonical form `curios format` produces.**
///
/// `curios-prelude-archive` holds its own two trees to this, and this file is the third authored `.crs` in the workspace — it lives here rather than there because `sys_module` includes it, so nothing over there walks it. Formatting is syntax-only, so this belongs in the ordinary suite; a failure names the file, and `curios format <file>` is the fix.
#[test]
fn the_authored_sys_source_is_canonically_formatted() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src")
        .join("prelude")
        .join("Bound.crs");

    match Formatted::from_path(&path) {
        Ok(Formatted::Unchanged(_)) => {}
        Ok(Formatted::Changed(_)) => panic!(
            "{} is not as `curios format` writes it; run `cargo run --package curios -- format {}`",
            path.display(),
            path.display()
        ),
        Err(refusal) => panic!("{}: {refusal}", path.display()),
    }
}
