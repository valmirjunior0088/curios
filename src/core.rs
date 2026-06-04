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

mod print;

mod reduce_prim;
use reduce_prim::*;

mod reduce;
pub use reduce::*;

mod context;
pub use context::*;

mod convert_prim;
use convert_prim::*;

mod convert;
pub use convert::*;

mod typing;
pub use typing::*;

mod infer;
pub use infer::*;

mod erase_prim;
use erase_prim::*;

mod erase;
pub use erase::*;

#[cfg(test)]
mod reduce_tests;

#[cfg(test)]
mod convert_tests;

#[cfg(test)]
mod erase_tests;
