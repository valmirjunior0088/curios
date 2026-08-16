# One naming scheme for compiler identities

**Decision.** Compiler-minted identities share one hint convention from the erased stage through Wasm emission — `curios-ersd`, `curios-cont`, and `curios-wasm` alike: `curios-ersd` and `curios-cont` spell theirs `~{kind}{index}`, `curios-wasm` spells theirs `kind/uniquifier`, and all three append the stored debug name after `$` at definition sites, with `$` as the only hint separator. Because surface names are alphanumeric-plus-underscore, the scheme cannot collide with a source spelling, and a hint never affects identity.

**Rationale.** A printed identity must read back unambiguously and must never collide with a user's name; reserving the separator makes clash-freedom structural rather than probabilistic, and hints stay display-only so behavior cannot grow back onto spellings. The scheme spans three crates, so it is stated once here rather than independently in each.
