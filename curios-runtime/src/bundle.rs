//! The bundled-executable tail format, shared by the `curios` bundler (which appends a program's payload, the modules answering its `foreign` declarations, and what names them) and the launcher stub (which recovers all three at startup). Defining the layout once here is what keeps the two sides in lockstep — neither crate hand-rolls the byte layout.
//!
//! **One archive, not a frame of counted segments.** The tail is a single [`Bundle`], so the payloads, the wire signatures and the export mapping are one value behind one length rather than an arithmetic both sides would have to agree on separately. rkyv is built `unaligned` for this workspace, so the archive is read where it lands: a bundled executable's tail begins wherever the launcher image happens to end, and nothing pads to move it.
//!
//! **The store travels because a `.cwasm` cannot answer for it.** The rows a program's `ffi` imports are typed by live in the compilation, not in the artifact: `Nat`, `Int` and `Bool` all cross as an i31 ref, so re-deriving a signature from the module's import types would lose the signedness a result is boxed by. `curios-verdicts` files the store beside a payload for exactly that reason, and this carries the same value the rest of the way — to a machine that has no store, no manifest and no sources.

use {curios_abi::ForeignStore, std::collections::BTreeMap};

/// Magic trailing the footer, marking an image as a bundled Curios executable.
///
/// Bumped from `CRSEXEC1` when the tail became a bundle rather than a bare payload. This is a format move rather than a compatibility surface: `curios` embeds the launcher with `include_bytes!`, so the launcher that reads a tail is always the one appended to it, and no build ever meets the other's layout.
const MAGIC: &[u8; 8] = b"CRSEXEC2";

/// Footer length: the 8-byte little-endian archive length plus [`MAGIC`].
const FOOTER_LEN: usize = 16;

/// One module travelling in a bundle: what it is, which declarations its exports answer, and the precompiled payload implementing them.
// `always`: the launcher reads this archive unconditionally, so there is no `archive` feature for a `cfg_attr` to gate on.
#[curios_archive::archived(always)]
pub struct BundledPlugin {
    /// The package whose manifest declared the row, for a refusal to name.
    pub package: String,
    /// The row's own name, likewise.
    pub name: String,
    /// Which declaration each export answers, as `export -> fully qualified name`.
    pub exports: BTreeMap<String, String>,
    /// The module, already compiled. `curios` owns Cranelift and compiles it while bundling; the launcher only deserializes, which is what lets a plugin travel without a backend travelling with it.
    pub payload: Vec<u8>,
}

/// Everything a bundled executable carries past its launcher image.
// `always`: as [`BundledPlugin`].
#[curios_archive::archived(always)]
pub struct Bundle {
    /// The program itself, precompiled.
    pub program: Vec<u8>,
    /// The `ffi` rows it imports, with the signatures each binding's marshalling is typed by.
    pub foreigns: ForeignStore,
    /// The modules answering those rows. Empty for the overwhelming majority of programs, which declare no `foreign`.
    pub plugins: Vec<BundledPlugin>,
}

/// Append `bundle` and a `(len: u64 LE) ++ MAGIC` footer to `image`, forming the tail [`extract_bundle`] recovers.
pub fn append_bundle(image: &mut Vec<u8>, bundle: &Bundle) -> Result<(), String> {
    let archived = curios_archive::to_bytes(bundle)
        .map_err(|error| format!("failed to archive the bundle: {error}"))?;

    image.extend_from_slice(&archived);
    image.extend_from_slice(&(archived.len() as u64).to_le_bytes());
    image.extend_from_slice(MAGIC);

    Ok(())
}

/// Recover the [`Bundle`] that [`append_bundle`] appended to `image`.
///
/// Errors if the footer is absent, carries the wrong magic, claims a length larger than the image body, or does not validate as a bundle. Validation is bytecheck's, which is what makes reading bytes off a file on disk sound rather than a reinterpretation of whatever is there.
pub fn extract_bundle(image: &[u8]) -> Result<Bundle, String> {
    if image.len() < FOOTER_LEN {
        return Err("not a bundled Curios executable (no payload)".into());
    }

    let (body, footer) = image.split_at(image.len() - FOOTER_LEN);
    let (len_bytes, magic) = footer.split_at(8);

    if magic != MAGIC {
        return Err("not a bundled Curios executable (bad footer)".into());
    }

    let len = u64::from_le_bytes(len_bytes.try_into().unwrap()) as usize;

    if len > body.len() {
        return Err("corrupt Curios executable (payload length exceeds image)".into());
    }

    curios_archive::from_bytes::<Bundle>(&body[body.len() - len..])
        .map_err(|error| format!("corrupt Curios executable ({error})"))
}
