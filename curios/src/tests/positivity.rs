//! End-to-end coverage for strict positivity modulo polarity.
//!
//! The check exists to make `induct` and `struct` sound: an inductive declaration claims its functor has an initial algebra, and without the gate `induct Bad | c(f : (Bad) -> False) end` inhabits `False`. The lattice itself is unit-tested in `curios-analysis/src/positivity/tests.rs`; these check what a *user* can observe.
//!
//! Every shape here is one the standard library already relies on. The prelude build exercises them through the from-scratch elaboration path, so it fails loudly on a regression; these run the same shapes through the prelude-replay path a user program actually takes, where the analysis sees only the user suffix and reads the prelude's polarity vectors back from the archive.

mod composition_tests;
mod computed_type_tests;
mod index_tests;
mod refusal_tests;
mod test_support;
