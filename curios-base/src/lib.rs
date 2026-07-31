//! Foundational utilities shared across every Curios pipeline stage: source spans, the fresh-name `Entropy`/`Mint` supply, the `name!` and `id!` newtype macros, the parser/printer monad combinators, the resolved-module-path `Qualifier` identity, the value types the surface (`curios-text`) and core (`curios-elab`) `Term` representations share verbatim (`Plicity`, `NumOp`, `Int`, `Flt`). Compiler-known `/syn` names belong to `curios-prelude`, alongside the source declarations they name.

mod macros;

mod arena;
pub use arena::*;

#[cfg(feature = "archive")]
mod archive;
#[cfg(feature = "archive")]
pub use archive::*;

mod entropy;
pub use entropy::*;

mod span;
pub use span::*;

mod qualifier;
pub use qualifier::*;

mod plicity;
pub use plicity::*;

mod root_id;
pub use root_id::*;

mod num_op;
pub use num_op::*;

mod int;
pub use int::*;

mod flt;
pub use flt::*;

mod monads;
pub use monads::*;

mod packed;
pub use packed::*;

mod scalar;
pub use scalar::*;
