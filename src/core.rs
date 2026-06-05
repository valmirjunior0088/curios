mod arity;
pub use arity::*;

mod flt;
pub use flt::*;

mod int;
pub use int::*;

mod nat;
pub use nat::*;

mod prim;
pub use prim::*;

mod names;
pub use names::*;

mod term;
pub use term::*;

#[cfg(test)]
mod term_tests;

mod print;

mod reduce_prim;
use reduce_prim::*;

mod reduce;
pub use reduce::*;

#[cfg(test)]
mod reduce_tests;

mod context;
pub use context::*;

mod convert_prim;
use convert_prim::*;

mod convert;
pub use convert::*;

#[cfg(test)]
mod convert_tests;

mod error;
pub use error::*;

mod typing;
pub use typing::*;

#[cfg(test)]
mod typing_tests;

mod infer_prim;
use infer_prim::*;

mod infer;
pub use infer::*;

mod erase_prim;
use erase_prim::*;

mod erase;
pub use erase::*;

#[cfg(test)]
mod erase_tests;
