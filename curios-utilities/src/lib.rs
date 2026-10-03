//! Foundational utilities shared across every Curios pipeline stage: source spans, the fresh-name `Entropy`/`Mint` supply, the `name!` and `id!` newtype macros, the typed identity-addressed [`Arena`], the SHA-256 content [`digest()`] and the [`Fingerprint`] every store key, record and tree hash is spelled with, the resolved-module-path `Qualifier` identity, the value types the surface (`curios-text`) and core (`curios-core`) `Term` representations share verbatim (`Plicity`, `InfixOp`), and the [`SyntaxRegistry`] shape those two stages read their emitted vocabulary from. Compiler-known names themselves belong to `curios-text`, alongside the source declarations they name: this crate states the slots, never the spellings.
//!
//! Why names are never ordered, why every identity space has one source, why qualifiers and symbols are interned once per process, why a segment's legality is decided here, why a mount is a prefix, why the syntax registry states slots and never spellings, and why depth is bought with stack are `README.md`'s decisions.
//!
//! Two neighbours hold the rest of the shared foundations. The numeric half of the shared vocabulary — `Natural`, `Integer`, `Flt`, and the erased carriers' scalar semantics — is `curios-num`, the one crate that names `num-bigint`. The two combinator DSLs are `curios-parse` and `curios-print`, two crates because both name their unit `pure`.

mod macros;

mod arena;
pub use arena::*;

mod entropy;
pub use entropy::*;

mod span;
pub use span::*;

mod interner;
use interner::*;

mod qualifier;
pub use qualifier::*;

mod symbol;
pub use symbol::*;

mod plicity;
pub use plicity::*;

mod sign;
pub use sign::*;

mod mount;
pub use mount::*;

mod infix_op;
pub use infix_op::*;

mod syntax;
pub use syntax::*;

mod recurse;
pub use recurse::*;

mod digest;
pub use digest::*;

// A namespace rather than a root export, for `curios-runtime`'s `test_support` reason: `curios_utilities::test_support::Temporary` says at its use site that the caller reached for scaffolding rather than product API.
#[cfg(feature = "test-support")]
pub mod test_support;
