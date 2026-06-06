mod scope;

mod int;
pub use int::*;

mod flt;
pub use flt::*;

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

mod module;
pub use module::*;

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

mod elaborate_prim;
use elaborate_prim::*;

mod elaborate;
pub use elaborate::*;

#[cfg(test)]
mod elaborate_tests;

mod erase_prim;
use erase_prim::*;

mod erase;
pub use erase::*;

#[cfg(test)]
mod erase_tests;

mod zonk;
pub use zonk::*;

#[cfg(test)]
mod zonk_tests;
