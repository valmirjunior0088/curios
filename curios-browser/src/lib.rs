//! The browser's host: what runs a program the compiler wrote under a JavaScript engine. `harness.mjs` instantiates the program against the browser's answers to the host rows and the hooks its embedder supplies, holding each `foreign` hook to its row; the bridge module gives it accessors over the program's GC values; and it is configured with the code tables `curios-abi` states and the rows the program reaches, each rendered here once, as JSON. [`run()`] is the wasm-bindgen door the playground calls, through the bundle `curios-js` is built into, [`page()`] writes the files a browser build of a program is — the page, its loader carrying the harness, and the program — and [`runner()`] the document a test serve answers at its root, with the wire its reports cross on.

mod bridge;
use bridge::*;

mod configuration;
use configuration::*;

mod page;
use page::*;
pub use page::{ARGUMENT, INDEX_HTML, Linked, PAGE_FILES, Placed, Program, content_type, page};

mod rows;
pub use rows::rows;

mod run;
pub use run::run;

mod runner;
pub use runner::{Posted, Reply, Step, runner};

#[cfg(test)]
mod tests;
