//! The compiler's wasm-GC target representation: an in-memory module builder and its binary encoder, the final stage of the pipeline. `curios-cont` lowers continuation IR into a [`Module`], and [`to_bytes`] produces the binary that wasmtime (`curios-rt`), the browser (`curios-js`), and `wasm-opt` (`curios-binaryen`) consume.
//!
//! The organizing idea is that everything is symbolic: items and their cross-references use the `name!` newtypes from `names` ([`TypeName`], [`FuncName`], …), and the numeric index spaces of the binary format exist only inside the encoder, derived from declaration order at encoding time. `types` mirrors the wasm-GC type grammar, `expr` holds the (GC-only, memory-less) instruction set, and `module` the builder API. [`Module`] also implements `Display` and `FromStr` for a WAT-style text form, used for stage dumps and round-trip tests.

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
