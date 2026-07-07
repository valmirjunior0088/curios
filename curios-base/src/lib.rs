//! Foundational utilities shared across every Curios pipeline stage: source
//! spans, the fresh-name `Entropy`/`Mint` supply, the `name!` newtype macro, the
//! parser/printer monad combinators, the slice `suffix_view` re-base laws, and
//! the resolved-module-path `Qualifier` identity.

mod macros;

mod entropy;
pub use entropy::*;

mod span;
pub use span::*;

mod qualifier;
pub use qualifier::*;

#[cfg(test)]
mod qualifier_tests;

mod monads;
pub use monads::*;

pub mod suffix_view;
