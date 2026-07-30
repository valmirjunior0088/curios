//! Emitting a self-contained native executable: the embedded slim launcher with
//! the program's `.cwasm` payload appended. The trailing footer layout lives in
//! `curios_runtime::bundle` (shared with the launcher that recovers it); this
//! module only embeds the launcher image and writes the result to disk.

use {
    curios_runtime::append_payload,
    std::{
        fs,
        os::unix::fs::PermissionsExt,
        path::{Path, PathBuf},
    },
};

/// The slim `curios-runtime` launcher stub, embedded at build time. Produced by
/// `make curios/runtime` (an isolated `--package curios-runtime` build,
/// kept Cranelift/Binaryen-free) under Cargo's target directory.
/// If the file is absent this `include_bytes!` fails the build — run `make`. So
/// `compile` needs no launcher lookup at runtime.
const LAUNCHER: &[u8] = include_bytes!(env!("CURIOS_RUNTIME_BIN"));

/// Default executable name: the input's file stem, no extension.
pub(crate) fn exe_output_path(input_path: &Path) -> PathBuf {
    PathBuf::from(input_path.file_stem().unwrap_or(input_path.as_os_str()))
}

/// Build a self-contained executable: the embedded launcher stub with the
/// `.cwasm` payload and its footer appended to the tail (see
/// [`curios_runtime::append_payload`]).
pub(crate) fn emit_exe(cwasm: &[u8], output: &Path) -> Result<(), String> {
    let mut bytes = LAUNCHER.to_vec();

    append_payload(&mut bytes, cwasm);

    fs::write(output, &bytes)
        .map_err(|error| format!("failed to write {}: {error}", output.display()))?;

    let mut perms = fs::metadata(output)
        .map_err(|error| format!("failed to stat {}: {error}", output.display()))?
        .permissions();
    perms.set_mode(0o755);
    fs::set_permissions(output, perms)
        .map_err(|error| format!("failed to chmod {}: {error}", output.display()))?;

    // On macOS we do NOT re-sign. The embedded launcher image carries an ad-hoc
    // signature from the linker whose code-limit covers the original image; our
    // payload is appended *past* that limit, so the signature stays valid and the loader
    // ignores the trailing bytes (the launcher reads them via `current_exe`).
    // `codesign --force` would in fact reject the result ("data after signature"),
    // so leaving the original signature in place is both correct and necessary.

    Ok(())
}
