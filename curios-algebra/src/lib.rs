//! The carriers' algebra over abstract atoms: what conversion decides about the carriers, stated without the terms it is decided over.
//!
//! An [`Atom`] is a handle its caller hands out, one per value the caller's identity decides is one, and a [`Monomial`] is a product of them. A [`Combination`] is a constant beside a linear combination of monomials, collected so each monomial is held once, and this crate cancels two of them at the strength each carrier admits: a cancellative commutative monoid at `Nat`, a group at `Int`. What a comparison concludes is a [`Deduction`] where inversion may read it and a [`Conclusion`] where only conversion may, so a residual that is merely sufficient cannot be read as an equation to deduce. A [`Word`] is the free monoid a sequence carrier is, over its caller's [`Alphabet`]: runs of known elements beside stretches whose contents are unknown, compared by stripping what two words certainly share. An [`Operation`] is what an intrinsic is declared to be, [`TABLE`] names the law families conversion decides for each operation at each carrier, and a [`LinearForm`] is a comparison read as the difference of its sides.
//!
//! No term is named here, and the crate depends on `curios-num` alone, which `README.md` decides and states what the crate may and may not grow into.

mod atom;
pub use atom::{Atom, Monomial, Rank, Tie};

mod bitwise;
pub use bitwise::{Pair, Reduct, power_of_two};

mod boolean;
pub use boolean::{
    Agreement, BooleanPair, Connected, Formula, Nested, Node, Valuation, same_leaves,
};

mod combination;
pub use combination::{Cancelled, Coefficient, Combination, Progress, Summand};

mod declaration;
pub use declaration::{Carrier, Observed, Operation};

mod division;
pub use division::{Divided, FloorSplit, Half, cofactor, euclid_split, floor_law, signed_cofactor};

mod euclid;
pub use euclid::{Recombination, Wanted};

mod lattice;
pub use lattice::{Kept, Lattice};

mod law;
pub use law::{Constant, Declared, Expr, Family, Law, Position, TABLE, declares};

mod magnitude;
pub use magnitude::Scale;

mod measure;
pub use measure::{
    Concatenated, Cut, Joining, Seam, Span, Split, join, locate, seam_window, split, total,
};

mod order;
pub use order::{Comparison, Modular, Side};

mod outcome;
pub use outcome::{Conclusion, Deduction};

mod product;
pub use product::{distribute, distribution_size, pair_factors};

mod sign;
pub use sign::{SignBit, SignForm, SignMagnitude};

mod view;
pub use view::{LinearForm, Part};

mod word;
pub use word::{Alphabet, Packed, Run, Segment, Stripped, Window, Word, same_position};

#[cfg(test)]
mod test_support;
