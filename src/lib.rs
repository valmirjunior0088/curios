mod macros;

mod entropy;
pub use entropy::*;

mod span;
pub use span::*;

mod monads;
pub use monads::*;

pub mod wire;

pub mod text;

pub mod core;

pub mod ersd;

pub mod cont;

pub mod optm;

pub mod wasm;

#[cfg(feature = "binaryen")]
pub mod binaryen;

#[cfg(feature = "run")]
mod run;
#[cfg(feature = "run")]
pub use run::*;

#[cfg(feature = "cli")]
mod cli;
#[cfg(feature = "cli")]
pub use cli::*;

#[cfg(test)]
mod tests;
