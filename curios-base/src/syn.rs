//! Shared path registry for the small `/syn` surface referenced directly by Rust lowering and elaboration code. This is intentionally not a mirror of the full syntax library: it contains only compiler-emitted names, while the crate facade preserves the existing `curios_base::{…}` imports.

/// `/syn/Monad/bind` — the desugaring target for each `!`-collected bang.
pub const MONAD_BIND: &str = "/syn/Monad/bind";

/// `/syn/Char`'s character-literal construction targets.
pub const CHAR_CHAR: &str = "/syn/Char/Char";
pub const CHAR_SCALAR_BELOW: &str = "/syn/Char/Scalar/below";
pub const CHAR_SCALAR_ABOVE: &str = "/syn/Char/Scalar/above";

/// `/syn/Str`'s string-literal construction targets.
pub const STR_STR: &str = "/syn/Str/Str";
pub const STR_SCAN_LEAD: &str = "/syn/Str/Scan/lead";
pub const STR_UTF8_STOP: &str = "/syn/Str/Utf8/stop";
pub const STR_UTF8_MORE: &str = "/syn/Str/Utf8/more";
pub const STR_STEP: &str = "/syn/Str/step";

/// `/syn`'s foundational reflected-proof constructors.
pub const TRUE_QED: &str = "/syn/True/True/qed";
pub const FALSE_ABSURD: &str = "/syn/False/absurd";
