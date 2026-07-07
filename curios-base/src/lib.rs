//! Foundational utilities shared across every Curios pipeline stage: source
//! spans, the fresh-name `Entropy`/`Mint` supply, the `name!` newtype macro, the
//! parser/printer monad combinators, the slice `suffix_view` re-base laws, the
//! resolved-module-path `Qualifier` identity, and the value types the surface
//! (`curios-text`) and core (`curios-core`) `Term` representations share
//! verbatim: `Plicity`, `NumOp`, `Int`, `Flt`.

mod macros;

mod entropy;
pub use entropy::*;

mod span;
pub use span::*;

mod qualifier;
pub use qualifier::*;

#[cfg(test)]
mod qualifier_tests;

mod plicity;
pub use plicity::*;

mod num_op;
pub use num_op::*;

mod int;
pub use int::*;

mod flt;
pub use flt::*;

mod monads;
pub use monads::*;

pub mod suffix_view;
