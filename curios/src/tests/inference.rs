//! What the elaborator works out rather than being told.
//!
//! Inference is not one mechanism, and this is deliberately not one file. An implicit is *solved*; a checked-only form is *postponed* until its expectation has structure, or synthesized once nothing is left to give it any; a motive is *inferred* from the arms it eliminates; a `?` is *reported*. A test belongs here when what it pins is the elaborator supplying something the program left out — and belongs in one of the modules below, never beside them, which is what keeps a file named for a whole phase from becoming the place anything type-directed lands.

mod budget_tests;
mod elimination_tests;
mod goal_tests;
mod implicit_tests;
mod postponement_tests;
mod tuple_tests;
