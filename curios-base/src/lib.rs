//! Foundational utilities shared across every Curios pipeline stage: source
//! spans, the fresh-name `Entropy`/`Mint` supply, the `name!` newtype macro, the
//! parser/printer monad combinators, the slice `suffix_view` re-base laws, the
//! resolved-module-path `Qualifier` identity, the value types the surface
//! (`curios-text`) and core (`curios-core`) `Term` representations share
//! verbatim (`Plicity`, `NumOp`, `Int`, `Flt`), and the `/syn` path-literal
//! registry both crates reference (`syn.rs`, kept separate from `NumOp` in `num_op.rs`).

mod macros;

mod entropy;
pub use entropy::*;

mod span;
pub use span::*;

mod qualifier;
pub use qualifier::*;

mod plicity;
pub use plicity::*;

mod num_op;
pub use num_op::*;

mod syn;
pub use syn::*;

mod int;
pub use int::*;

mod flt;
pub use flt::*;

mod monads;
pub use monads::*;

mod suffix_view;
pub use suffix_view::*;

mod packed;
pub use packed::*;
