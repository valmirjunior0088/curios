//! The browser's host: what runs a program the compiler wrote under a JavaScript engine. `harness.mjs` instantiates the program against the browser's answers to the host rows and the hooks its embedder supplies, holding each `foreign` hook to its row; the bridge module gives it accessors over the program's GC values; and it is configured with the code tables `curios-abi` states and the rows the program reaches, each rendered here once, as JSON. [`run()`] is the wasm-bindgen door the playground calls, through the bundle `curios-js` is built into, and [`page()`] writes the files a browser build of a program is: the page, its loader carrying the harness, and the program.

mod bridge;
use bridge::*;

mod configuration;
pub use configuration::*;

mod page;
pub use page::*;

mod rows;
pub use rows::*;

mod run;
pub use run::*;

#[cfg(test)]
mod tests;
