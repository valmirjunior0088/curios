//! The bundled-executable footer format, shared by the `curios` bundler
//! (which appends a `.cwasm` payload to the launcher image) and the launcher stub
//! (which recovers it at startup). Defining the layout once here is what keeps the
//! two sides in lockstep — neither crate hand-rolls the byte layout.

/// Magic trailing the footer, marking an image as a bundled curios executable.
const MAGIC: &[u8; 8] = b"CRSEXEC1";

/// Footer length: the 8-byte little-endian payload length plus [`MAGIC`].
const FOOTER_LEN: usize = 16;

/// Append `cwasm` and a `(len: u64 LE) ++ MAGIC` footer to `image`, forming the
/// `payload ++ footer` tail that [`extract_payload`] recovers.
pub fn append_payload(image: &mut Vec<u8>, cwasm: &[u8]) {
    image.extend_from_slice(cwasm);
    image.extend_from_slice(&(cwasm.len() as u64).to_le_bytes());
    image.extend_from_slice(MAGIC);
}

/// Recover the `.cwasm` payload that [`append_payload`] appended to `image`.
/// Errors if the footer is absent, carries the wrong magic, or claims a length
/// larger than the image body.
pub fn extract_payload(image: &[u8]) -> Result<Vec<u8>, String> {
    if image.len() < FOOTER_LEN {
        return Err("not a bundled curios executable (no payload)".into());
    }

    let (body, footer) = image.split_at(image.len() - FOOTER_LEN);
    let (len_bytes, magic) = footer.split_at(8);

    if magic != MAGIC {
        return Err("not a bundled curios executable (bad footer)".into());
    }

    let len = u64::from_le_bytes(len_bytes.try_into().unwrap()) as usize;

    if len > body.len() {
        return Err("corrupt curios executable (payload length exceeds image)".into());
    }

    Ok(body[body.len() - len..].to_vec())
}
