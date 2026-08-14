//! Foundational utilities shared across every Curios pipeline stage: source spans, the fresh-name `Entropy`/`Mint` supply, the `name!` and `id!` newtype macros, the typed identity-addressed [`Arena`], the resolved-module-path `Qualifier` identity, the value types the surface (`curios-text`) and core (`curios-elab`) `Term` representations share verbatim (`Plicity`, `NumOp`), and the [`SyntaxRegistry`] shape those two stages read their `/syn` vocabulary from. Compiler-known `/syn` names themselves belong to `curios-prelude`, alongside the source declarations they name: this crate states the slots, never the spellings.
//!
//! Two neighbours hold what this crate used to. The numeric half of the shared vocabulary — `Natural`, `Integer`, `Flt`, and the erased carriers' scalar semantics — is `curios-num`, the one crate that names `num-bigint`. The two combinator DSLs are `curios-parse` and `curios-print`, split apart because both name their unit `pure` and a crate name disambiguates them where a module namespace had to.
//!
//! [`NumOp`] stays here because it is not numeric: it is the surface grammar's whole infix roster, `&&` and `||` included, and [`OperatorSyntax`] reads it to pick the `/syn` concept field an operator dispatches through.

mod macros;

mod arena;
pub use arena::*;

mod entropy;
pub use entropy::*;

mod span;
pub use span::*;

mod qualifier;
pub use qualifier::*;

mod plicity;
pub use plicity::*;

mod mount;
pub use mount::*;

mod num_op;
pub use num_op::*;

mod syntax;
pub use syntax::*;

mod packed;
pub use packed::*;

mod recurse;
pub use recurse::*;
