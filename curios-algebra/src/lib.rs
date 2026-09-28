//! The carriers' algebra over abstract atoms: what conversion decides about `Nat` and `Int`, stated without the terms it is decided over.
//!
//! An [`Atom`] is a handle its caller hands out, one per value the caller's identity decides is one, and a [`Monomial`] is a product of them. A [`Combination`] is a constant beside a linear combination of monomials, collected so each monomial is held once, and this crate cancels two of them at the strength each carrier admits: a cancellative commutative monoid at `Nat`, a group at `Int`. What a comparison concludes is a [`Deduction`] where inversion may read it and a [`Conclusion`] where only conversion may, so a residual that is merely sufficient cannot be read as an equation to deduce.
//!
//! **No term is named here.** A summand carries an origin of its caller's choosing, handed back untouched so the caller rebuilds a result from the terms it came from; this crate never inspects one. The crate depends on `curios-num` alone, which is what lets it be tested over bare atoms and concrete values, and why its README is the record of what it may and may not grow into.

mod atom;
pub use atom::*;

mod bitwise;
pub use bitwise::*;

mod combination;
pub use combination::*;

mod declaration;
pub use declaration::*;

mod division;
pub use division::*;

mod euclid;
pub use euclid::*;

mod order;
pub use order::*;

mod outcome;
pub use outcome::*;

mod product;
pub use product::*;

mod view;
pub use view::*;
