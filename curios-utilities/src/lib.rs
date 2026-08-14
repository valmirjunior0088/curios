//! Foundational utilities shared across every Curios pipeline stage: source spans, the fresh-name `Entropy`/`Mint` supply, the `name!` and `id!` newtype macros, the typed identity-addressed [`Arena`], the resolved-module-path `Qualifier` identity, the value types the surface (`curios-text`) and core (`curios-elab`) `Term` representations share verbatim (`Plicity`, `InfixOp`), and the [`SyntaxRegistry`] shape those two stages read their `/syn` vocabulary from. Compiler-known `/syn` names themselves belong to `curios-prelude`, alongside the source declarations they name: this crate states the slots, never the spellings.
//!
//! Two neighbours hold what this crate used to. The numeric half of the shared vocabulary — `Natural`, `Integer`, `Flt`, and the erased carriers' scalar semantics — is `curios-num`, the one crate that names `num-bigint`. The two combinator DSLs are `curios-parse` and `curios-print`, split apart because both name their unit `pure` and a crate name disambiguates them where a module namespace had to.
//!
//! [`InfixOp`] was `NumOp` and stayed behind when the numerics left, which for a while made it look misplaced. It was not: the roster is the surface grammar's *whole* infix set — `&&`, `||`, and the comparisons alongside the five arithmetic ones — and every match on it in the workspace is either [`OperatorSyntax::concept_field`] picking the `/syn` concept an operator dispatches through, or `curios-text`'s precedence table. Nothing computes with one; elaboration replaces it with an `Intrinsic` before anything does. The name was the only numeric thing about it.

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

mod infix_op;
pub use infix_op::*;

mod syntax;
pub use syntax::*;

mod packed;
pub use packed::*;

mod recurse;
pub use recurse::*;
