//! Foundational utilities shared across every Curios pipeline stage: source
//! spans, the fresh-name `Entropy`/`Mint` supply, the `name!` newtype macro, the
//! parser/printer monad combinators, and the slice `suffix_view` re-base laws.

mod macros;

mod entropy;
pub use entropy::*;

mod span;
pub use span::*;

mod monads;
pub use monads::*;

pub mod suffix_view;
