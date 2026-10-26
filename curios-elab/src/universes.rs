//! The elaborator's universe levels and qualities: the solver that settles them, how a declaration's levels close into its scheme, the gate a finished module's universes pass, their substitution into terms, and the variance a definition's levels are compared at.
//!
//! What a universe judgment is stated over is `curios-core`'s `universe`, which knows nothing of how it is solved.

mod solver;
pub use solver::*;

mod closing;
pub use closing::*;

mod gate;
pub use gate::*;

mod mutation;
pub use mutation::*;

mod substitution;
pub use substitution::*;

mod variance;
pub use variance::*;
