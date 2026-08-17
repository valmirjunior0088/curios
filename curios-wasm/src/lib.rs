//! The compiler's wasm-GC target representation: an in-memory module builder and its binary encoder, the final stage of the pipeline. `curios-cont` lowers continuation IR into a [`Module`], and [`to_bytes`] produces the binary that wasmtime (`curios-runtime`), the browser (`curios-js`), and `wasm-opt` (`curios-binaryen`) consume.
//!
//! The organizing idea is that everything is symbolic: items and their cross-references use the `name!` newtypes from `names` ([`TypeName`], [`FuncName`], …), and the numeric index spaces of the binary format exist only inside the encoder, derived from declaration order at encoding time. `types` mirrors the wasm-GC type grammar, `expr` holds the instruction set, and `module` the builder API. [`Module`] also implements `Display` and `FromStr` for a WAT-style text form, used for stage dumps and round-trip tests.
//!
//! The modeled surface is the whole envelope the pipeline pins — every declared item, segment mode, and instruction family the feature set `curios-binaryen` masks and Wasmtime's engine enables admits, memories and tables plural and each 32- or 64-bit addressed. What is *not* modeled is what that envelope excludes: threads, SIMD, exceptions, extended constant expressions, custom page sizes, stack switching. Nothing here is emitted on a module's behalf, and nothing here enforces how the surface is used — see `README.md` for both decisions.

mod names;
pub use names::*;

mod types;
pub use types::*;

mod expr;
pub use expr::*;

mod module;
pub use module::*;

mod writer;
pub use writer::*;

mod parse;

mod print;
use print::*;
