# One naming scheme for compiler identities

**Decision.** Compiler-minted identities are spelled by one scheme from the erased stage through Wasm emission — `curios-ersd`, `curios-cont`, and `curios-wasm` alike: `~{kind}{index}`, with the stored debug name appended after `$` at definition sites; Wasm symbols derive theirs the same way, with `$` as the only hint separator. Because surface names are alphanumeric-plus-underscore, the scheme cannot collide with a source spelling, and a hint never affects identity.

**Rationale.** A printed identity must read back unambiguously and must never collide with a user's name; reserving the separator makes clash-freedom structural rather than probabilistic, and hints stay display-only so behavior cannot grow back onto spellings. The scheme spans three crates, so it is stated once here rather than independently in each.
